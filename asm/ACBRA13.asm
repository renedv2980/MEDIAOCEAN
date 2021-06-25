*          DATA SET ACBRA13    AT LEVEL 124 AS OF 12/02/20                      
*PHASE T62413B                                                                  
*                                                                               
ACBRA13  TITLE '- BRA eBuyer upload server'                                     
*                                                                               
* Level change comments                                                         
* ---------------------                                                         
* UK Levels                                                                     
* ---------                                                                     
* TKLU 001 28APR05 New DDLink upload server for eBuyer                          
* TKLU 002 19MAY05 Order identification number                                  
* TKLU 003 08AUG05 eTime preparations                                           
* TKLU 004 17OCT05 Update Text cells - bug fixes as in test now                 
* TKLU 005 09NOV05 Genie UKCR00004554 - IO bug fix for text/change              
* TKLU 006 14NOV05 Genie UKCR00004554 - bug fix when all data removed           
* TKLU 007 15NOV05 Genie UKCR00004549 - element length adjustment fix           
* TKLU 008 30NOV05 Some DDLink enhancements                                     
* TKLU 009 31JAN06 Genie order extra text upload bug fix                        
* TKLU 010 10FEB06 Upload return mapping bug fix                                
* TKLU 012 18MAR06 Genie UKCR00005820 - complete ID number feature              
* TKLU 013 20APR06 <LO01-5379> PROGRAM NUMBER FROM 22 TO 24                     
* TKLU 014 31AUG06 <UKCR00007253> allow for empty text in text upload           
* TKLU 015 28SEP06 Preparations for MCS -> BRA                                  
* TKLU 016 10OCT06 MERGE IN US CHANGES FROM JIM SHEA                            
*          11OCT06 <LO01-5871> Order printed description extension              
* TKLU 017 23OCT06 FINAL RENAME FROM MCS TO BRA                                 
* TKLU 018 31OCT06 Maximum text size check typo                                 
* TKLU 019 08JAN07 US Merger                                                    
* TKLU 020 01APR07 <DU01-5756> New order upload server * phase 1                
* TKLU 021 23APR07 <DU01-5756> New order upload server * phase 2                
*                  New 'display all order' request                              
* TKLU 022 03MAY07 <DU01-5756> New order upload server * phase 3                
* TKLU 023 15MAY07 <BR12144L> Bug fix to BLDEXT                                 
*          25MAY07 <UKCR00012795> Office different on exp. orders (US)          
* TKLU 024 01JUN07 <DU01-5756> More adjustments for 'change approved'           
*          04JUN07 <DU01-6519> Items (articles) on order preparations           
*          11JUN07 US merger (JSHA)                                             
* TKLU 025 15JUN07 Change approved order - account change bug fix               
* TKLU 028 21JUN07 Change approved order - fix on change accounts               
*          03JUL07 Fix 'serial number missing' bug (bad branch), skip           
*                  PROASA on resubmit, a fix for LO01-6192 exp. orders          
*          05JUL07 <UKCR00013226> Goods received status bug fix                 
* TKLU 029 13JUL07 <UKCR00013405> 'Change approved' versus 'auto appr.'         
*          16JUL07 <UKCR00013406> 'Change approved' versus 'self appr.'         
* TKLU 030 24JUL07 Another fix to audit of 'change approved'                    
* TKLU 031 07AUG08 Article/Item validation bug fix + US merger                  
*          09AUG07 <UKCR00013719> estimate check on submit                      
* TKLU 032 22AUG07 <UKCR00013885> W/C seq. must match receiving seq.            
* TKLU 033 04OCT07 <UKCR00014529> Seq. appr. bug fix (PIDEL sequence)           
* NSHE 034 08NOV07 <LO01-6995> Set FACPAK code for analysis                     
* TKLU 035 10NOV07 <DU01-6289> Allow for header and footer text                 
*          13NOV07 <UKCR00014972> Foreign currency AFCELD bug fix               
* TKLU 036 24NOV07 <DU01-6519> Final fixes pre release 'articles'               
* TKLU 037 04DEC07 <DU01-5916> Enable 'copy order' basic support in old         
*                  download for the time being                                  
*          19DEC07 <BR11758D> BLDMOR/HELLO bug fix (element sequence)           
* TKLU 038 03JAN08 <BR10063Y> UPDOSD/XDF call - use workcode, too               
*          07JAN08 <UKCR00015625> Self approval on resubmit - bug fix           
*          14JAN08 <UKCR00014625> 'No Price Items' allowed on orders            
*          21JAN08 <BR11874D> Support code in VALEST (ACBRA03)                  
*          29JAN08 Order Reopen adjustments and fixes                           
* SMAN 039 18FEB08 <LO01-6788> Include estimate check for workcodes             
* TKLU             <LO01-6456> EType application locks                          
*          19FEB08 <DU01-7333> Order profiles PO1/PO2 split                     
*          05MAR08 OAMELD W/Cs NOT sorted anymore, stay in input seq.           
*          06MAR08 INTERMEDIATE VERSION (SMAN changes deactivated)              
* TKLU 040 08MAR08 <BR12107D> Order single display non eBuyer bug fix           
* TKLU 041 01APR08 <UKCR00010225> Check closed account status                   
* TKLU 042 17APR08 <UKCR00016445> Improve audit on reopen action                
* NSHE 043 19APR08 Extend order name to 100 characters                          
* SMAN 044 26JUN08 <UKCR00017361> Ignore amount of order being edited           
* SMAN             <UKCR00017561> No amount check if Prof#6&7=N in PO1          
* SMAN             <UKCR00017569> Err msg when estimate does not exist          
* SMAN             <UKCR00017668> Change DEFEBY07 to N                          
* SMAN reactivated <LO01-6788> Include estimate check for workcodes             
* TKLU   (10JUN08) Goods received on reopen bug fix                             
* TKLU   (17JUN08) <BR18206L> Reopen - clear ORDSMNUP from ORDSTAT              
* TKLU   (24JUN08) New 'All 1F' call parameter of combined d/l                  
* TKLU 045 30JUN08 <UKCR00017814> Fix ORDSTAT bug: reset print flags            
* NSHE 046 13AUG08 Remove old code no longer in use                             
* TKLU     21AUG08 <NY02-0010> All orders profile in CHKEST                     
* SMAN 047 11NOV08 <UKCR00019846> Bug fixes to CHKEST                           
* TKLU     03NOV08 <DU01-8304> Expanded job locks implemented                   
* NSHE 048 20JAN09 Merge US and UK code                                         
* TKLU     27NOV08 <DU01-7817> MQ data export on full approval                  
*          08DEC08 <DU01-8048> Save order report format to order                
*          09DEC08 <UKCR00020323> Article flags onto order's ARTELD             
*          17FEB09 <BR23188L> Improve VALSTA for Submit -> Submit case          
* TKLU     29APR09 <DU01-7817> MQ data export bug fix for XData group           
* MPEN     29APR09 <BR24651L> FIX BUG WHERE COULDN'T UPDATE AUDIT WHEN          
*                             BLANK PTAELDS EXIST                               
* NSHE 052 08MAY09 UKCR00022019 Send emails in embedded call                    
* TKLU     14MAY09 <LO01-7636> New ORDASTA set for self/auto approval           
* TKLU 053 14JUL09 <UKCR00023071> Remove surplus approvers if changed           
* MPEN     06APR09 <LO01-8463> CONTROL NOTIFICATIONS ON SUBMIT                  
* MPEN     12JUL09 <LO01-9013> CHANGES TO EXPENDITURE TYPE CODE                 
* NSHE 054 19SEP09 Clearer equates for status element                           
* NSHE 055 30SEP09 Deal with new extra data type download list                  
* SMAN 056 12OCT09 <BR28033L> Ignore current order if PROPO302=Y                
* NSHE 057 22OCT09 <UKCR25075> Ensure profile for analysis obeyed               
* NSHE 058 25NOV09 <UKCR25221> Self approval should approve multi levls         
* TKLU     03DEC09 <LO01-9580> Resubm on change appr order - part appr          
*                  logic implemented                                            
* SMAN     10DEC09 <BR15094D> Skip delete of order trx if fully matched         
* SMAN 059 11DEC09 Disable changes for BR15094D at level 58                     
* NSHE 060 04JAN10 LO01-9579 Allow status to submitted from part apprvd         
* NSHE 061 10JAN10 Do estimate checking for artist orders                       
* NSHE 062 13JAN10 US fix to zero value orders                                  
* NSHE 063 19JAN10 Store ledger ST for artist orders                            
* TKLU 064 10MAR10 <LO01-9712> XData from orders to order trx and gene-         
*                  rate order transactions in here (not ACBRA01)                
*                  <LO01-9992> Estimate checks against single estimate          
*                  <PR000004> Order owner implemented                           
* SMAN 065 12MAY10 <BR33103L> Check TRNKDATE prior to FLTTIM calls              
* NRAK 067 26MAR10 <BR32128L> error *before* overrunning textbox area           
* NSHE 068 13JUL10 Change to TSJPAS structure                                   
* NRAK     28JUL10 <BR34818L> BRANDOCEAN, MEET ORDOJOB                          
* NRAK 069 06SEP10 <UKCR00028939> CLEAR PIDAPPQ ON PART-MATCH->REJECT           
* TKLU 070 09NOV10 IN RESUBMIT MODE CLEAR XSVPAPP                               
* NSHE 071 09NOV10 Remove VALEST and tidy up for invoices                       
* NSHE 072 01MAR11 Remove draft status when deleting orders trx                 
* MPEN 073 04MAR11 Fix for part matched status                                  
* MPEN 074 03SEP10 <PR000716> Mainframe changes for GAP Orders                  
* MPEN 075 13MAY11 <UKCR00032176> US show business email address                
* SMAN 075 01JUN11 <BR42045L> Skip CHKEST for certain scenarios                 
* MPEN     27MAY11 <BR18008D> allow part-matched-closed status change           
* NRAK     13JUN11 <BR41964L> CHKEST TO ALLOW FOR INVOICED AMOUNTS              
* MPEN 077 29JUN11 <UKCR00032449> POINT TO CORRECT STATUS BIT                   
* NSHE     15JUN11 Fix memo invoices for US                                     
* NRAK     14JUL11 <BR42913L> Fix OADDTX, ensure has extension rec              
* NSHE 078 11APR12 <BR19592D> Don't allow orders to be opened with              
* NSHE             pending invoices                                             
* NSHE 079 15MAY12 US merge                                                     
* NSHE 081 03SEP12 Change IO routines to deal with auto switching               
* NSHE     16OCT12 PR003305 - Force and M&C Saatchi to enter matching           
*                  description when adding an order                             
* NRAK     19OCT12 <BR52404L> fix previous - not if status change               
* NRAK     04APR13 <BR54902L> filter order recs if GOECE=5                      
* JFOS     18APR13 <BR11315Y> Clear ORDRQBD if not set on upload                
* MPEN     27MAR13 <BR54819L> increase COLTAB size for jobber                   
* NSHE 082 29MAY13 <DSBO-22/23> Self approval issue and text not removd         
* NSHE 083 30MAY13 <DSBO-31> Change of currency causing dump                    
* NSHE 084 13JUN13 <DSBO-63> Allow multiple items with same WC                  
* NSHE 085 25JUN13 <DSBO-26> Allow sub sequence to be bigger for text           
* NRAK 086 27JUN13 <br18137d> include subseq in old buff key                    
* NSHE 087 10JUL13 <DSBO-64> fix to estimate checking                           
* JFOS     18JUL13 <PR003402>New JOBBER:skip 1R-lvl ents on Ordr Estchk         
* NSHE 088 23AUG13 US fix for office filters                                    
* YNGX 089 25NOV13 <DSBO-532> SET MAXLEN TO 240 FOR DELIVERY ADDRESS            
*NRAK 090 10JAN14 <DSBO-609> STCCOM breaks on empty comment fields.             
*NRAK 091 18NOV14 <DSBO-1150> fix order transaction rebuild                     
*JFOS 092 14JAN15 <DSBO-1331> Fix TSAR dup ARTELs in Audit                      
*MPEN 093 28NOV14 <DSRD-5096> Save STCELD on main record                        
*MPEN     04FEB15 <DSRD-5958> Return estimate check/warn in new format          
*MPEN     12FEB15 <DSRD-6153> If aura always return order number                
*                 <DSRD-6278> Aura ensure order no. is correct                  
*                 <DSRD-6295> Clear workcode block between calls                
*                 <DSRD-6296> Fix bad branch and exit if chkest error           
*                 <DSRD-6297> Don't show WC level error if job error            
*                 <DSRD-6371> Changes for aura estimate checking                
*                 <DSRD-6390> Save application type to order audit              
*         09MAR15 <DSRD-6486> Fix CHKEST bug                                    
*         10MAR15 <DSRD-6497> Fix CHKEST bug                                    
*         11MAR15 <RD006456> New error message for highest revision             
*         11MAR15 <RD006387> Fix for no current estimate                        
*NSHE 094 25MAR15 <DSRD-6679> New error message when order changed              
*NSHE     31MAR15 <DSRD-6739> New error message when deleting order             
*MPEN 095 07APR15 <RD006813> Fix bug reading estimate number                    
*NSHE 096 13APR15 <DSRD-6875> Fix audit bug with printed description            
*MPEN     16APR15 <RD006927> Fix uncommitted amount if high rev est             
*JFOS     22APR15 <DSBO-1208 Update DIR audit status after MST put              
*TKLU 097 06May15 <RD007021> Update ORDAMDT on status changes, too              
*NRAK             <DSRD-7008> Add transition for sub'd -> auto approved         
*NRAK 098 06May15 <DSRD-7028> Add transitions for self appr from sub            
*JFOS 099 21May15 <DSBO-1208>Further fix to sync IS/DA AUDREC statuses          
*NSHE 100 12Jun15 <DSRD-7563> Set print texts and amounts for Aura              
*NSHE 101 09Jul15 <DSRD-7891> Fix bug where order goes to approved              
*MPEN 102 23Nov15 <DSBO-1635> Fix for order pending allocation check            
*JFOS 103 09oct15 <PCA-1989> Support Limit Account access                       
*NSHE 104 04Feb16 <DSRD-10305> Ensure Aura uses only FPR opt maint              
*MPEN     15Feb16 <DSRD-10423> Allow part app order resubt auto app             
*MPEN 105 07Mar16 <DSRD-10678> Don't create ty12 if self app submit             
*NSHE 106 20Jun16 <DSRD-11826> Save printed description to type 12              
*YNGX 107 27Jun16 <DSBO-1756>  Bug fixed for level 106                          
*YNGX 108 28Jun16 <ITMF-8229>  Bug fixed for DSBO-1756                          
*MPEN 109 15Nov16 <DSRD-13990> Fix for US office check                          
*MPEN 110 22Mar17 <DSRD-14959> Allow show amts/txt on PDF for aura              
*MPEN 111 11Jul17 <DSRD-16160> Set error/warning if zero est amt left           
*MPEN 112 24Aug17 <DSRD-16693> Fix for change of approvers in audit             
*NSHE 113 17Aug17 <DSRD-16357> Cancelled orders                                 
*NSHE     29Sep17 <DSRD-17100> Set status update to aid bulk extract            
*TKLU 114 07May18 DSPCA-2844   RNSPASD Adjustments                              
*MPEN 115 29Oct18 <ITMF-30550> Fix for order numbering                          
*MPEN 116 29Nov18 <DSRD-20896> Remove QA specific dates                         
*MPEN 117 30Apr19 <DSRD-22426> Send order to supplier                           
*NSHE 118 30Aug19 <DSRD-23314> Ensure audit is created when attention           
*NSHE             field is changed                                              
*NSHE 119 15Oct19 DSRD-24048 Ensure updates always update the audit rec         
*MPEN 120 31Mar20 DSRD-25344 Extend rejection comments                          
*SGAV 121 26May20 DSRD-25716 Save the overridden work code desc to MF           
*NSHE     24Jun20 DSRD-26484 Mobile order approval - allow no type              
*MPEN 122 26Aug20 DSRD-27018 New application bit for mobile                     
*NSHE 124 18Sep20 DSRD-27618 Reset GAP status is supplier changes               
*NSHE     24Sep20 DSRD-27632 Deal with cancelled orders reopened by             
*NSHE                        electronic invoicing                               
*MPEN 124 28Sep20 DSRD-27189 Support SI a/c                                     
*MPEN 125 01Dec20 DSRD-28127 Fix for mobile orders                              
*                                                                               
* US Levels                                                                     
* ---------                                                                     
* JSHA 004 28Sep07 All UK Levels up to 32                                       
* JSHA 005 27Jan08 US/UK Merger from Levels 33 - 37                             
*                                                                               
* ATIA used for DDLink/Upload purposes                                          
*                                                                               
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=2000,REQUEST=*,WORKERKEY=ACBO,   *        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=ACCSYSQ                   
                                                                                
         PRINT NOGEN                                                            
         EJECT                                                                  
                                                                                
ENTRY    NMOD1 0,**BO13**,RR=RE                                                 
         LR    R5,R1                                                            
         USING LP_D,R5             R1=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         ICM   R8,15,RSVRSAVE      R8=A(4K SAVE AREA)                           
         ST    R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
***********************************************************************         
* Initialise Upload                                                   *         
***********************************************************************         
                                                                                
         CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         JE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JE    INPUT                                                            
         J     EXITY                                                            
                                                                                
INIT     LA    R0,SAVED            CLEAR SAVE STORAGE                           
         LHI   R1,SAVEL                                                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   GIND2,GI2EBUY                                                    
         GOTOR (#CPYINI,ACPYINI)   INITIALISE COMPANY VALUES                    
         GOTOR (#ORDPRF,AORDPRF),DMCB,XL#ORPF                                   
T        USING ORDPRFD,XL#ORPF                                                  
                                                                                
         MVC   AALIOB,LP_ALIOB     EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   AALINKIO,CLINKIO-COMFACSD(RF)                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Process and Upload Record                                           *         
***********************************************************************         
                                                                                
INPUT    LA    RF,RECTAB                                                        
         USING RECTABD,RF                                                       
         LHI   R0,RECTABN                                                       
         BASR  RE,0                                                             
         CLC   RECTMAP#,LP_QMAPN   Look up record map code                      
         JE    INPUT02                                                          
         AHI   RF,RECTABL                                                       
         BCTR  R0,RE                                                            
         DC    H'0'                -> unknown record type                       
                                                                                
INPUT02  MVC   RECTYPE,RECTTYPE    -> set known record type                     
                                                                                
         XR    R0,R0               BUILD DOWNLOAD MAP ELEMENT                   
         ICM   R0,3,LP_QMAPN                                                    
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
                                                                                
         GOTOR (#SYSCHK,ASYSCHK)                                                
         JE    INPUT04                                                          
         GOTOR PUTERR,AE$FLRD                                                   
         J     EXITN                                                            
                                                                                
INPUT04  CLI   RECTYPE,RECTTOUP                                                 
         JE    UPDORD                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* New BrandOcean Orders upload module                                 *         
***********************************************************************         
                                                                                
UPDORD   DS    0H                                                               
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(1,OR_TODP)                                   
         GOTOR (RF),(R1),(5,0),(2,OR_TODC)                                      
         GOTOR (RF),(R1),(5,0),(0,OR_TODF)                                      
*                                                                               
* TWAMERP is used to indicate one of the sub-calls has errored.                 
*     OULAST will terminate early in that case. Clear all other                 
*     errors here, if not done already, for next subcall.                       
UPDORD2  MVC   XERRTXT,SPACES                                                   
         XC    ROUERRV,ROUERRV                                                  
*                                                                               
         CLI   RQOURTY,RQOURT1     Are we processing main header                
         JE    OUMAIN              Yes                                          
         CLI   RQOURTY,RQOURT2     Are we processing approvers                  
         JE    OUAPPR              Yes                                          
         CLI   RQOURTY,RQOURT3     Are we processing workcodes                  
         JE    OUWCOD              Yes                                          
         CLI   RQOURTY,RQOURT4     Are we processing items                      
         JE    OUITEM              Yes                                          
         CLI   RQOURTY,RQOURT5     Are we processing texts                      
         JE    OUTEXT              Yes                                          
         CLI   RQOURTY,RQOURT6     Are we processing extra data items           
         JE    OUXDAT              Yes                                          
         CLI   RQOURTY,RQOURT9     Are we processing last for update            
         JE    OULAST              Yes                                          
         DC    H'0'                Fail if unrecognised                         
         EJECT                                                                  
***********************************************************************         
* order upload: main data processing                                  *         
***********************************************************************         
         SPACE 1                                                                
OUMAIN   OC    CCTPID,CCTPID       ensure PID is set                            
         JNZ   OUMAIN05                                                         
         MVC   ROUERRV,=AL2(AE$NCPID)                                           
         J     OUMAINER                                                         
                                                                                
OUMAIN05 GOTOR INIOUV              initialise values                            
         MVC   XOMACT,RQOMACT      Copy action, to status and change            
         MVC   XOMSTA,RQOMSTA              to approved order                    
         MVC   XOMTYP,RQOMTYP              to approved order                    
         MVC   XOMCAO,RQOMCAO                                                   
         MVC   XGAPSTAT,RQOGAPST                                                
         MVC   XGAPEXPD,RQOGAPED                                                
                                                                                
         USING LW_D,R2                                                          
OUMAIN10 L     R3,AGENAREA                                                      
         LA    R3,(L'STCGEML*10)(R3)                                            
         XR    R2,R2                                                            
         ICM   R2,B'0111',RQOGAPEM                                              
         JZ    OUMAIN18                                                         
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN                                                     
         STC   R0,XGAPEMLN                                                      
         LA    R4,LW_DATA2                                                      
                                                                                
OUMAIN12 MVC   1(L'STCGEML,R3),0(R4)                                            
         MVI   0(R3),YESQ                                                       
         LA    R4,L'STCGEML(R4)                                                 
         LA    R3,(L'STCGEML+1)(R3)                                             
         JCT   R0,OUMAIN12                                                      
                                                                                
OUMAIN18 ZAP   XJOBLE,PZERO                                                     
         GOTOR GETORD              Get order                                    
         JNE   OUMAINER                                                         
         GOTOR SETSTA              Set original workflow status                 
                                                                                
         CLI   RQOMACT,RQOMA3Q     If status change only no data passed         
         JNE   OUMAIN20                                                         
                                                                                
         GOTOR SETVAL              Get data from order                          
                                                                                
* Validate main data now                                                        
                                                                                
OUMAIN20 GOTOR VALOFF              Validate office input                        
         JNE   OUMAINER                                                         
                                                                                
         GOTOR VALETY              Validate expenditure type                    
         JE    OUMAIN25                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN25 GOTOR VALSUP,0            Validate supplier account                    
         JE    OUMAIN30                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN30 GOTOR VALJOB              Validate client product job                  
         JE    OUMAIN35                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN35 GOTOR CHKOFF              Check office is allowed and matches          
         JE    OUMAIN40                supplier office                          
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN40 GOTOR VALEXP              Validate expense account                     
         JE    OUMAIN45                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN45 GOTOR VALANA              Validate analysis accounts                   
         JE    OUMAIN50                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN50 GOTOR VALCUR              Validate currency                            
         JE    OUMAIN55                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN55 GOTOR VALEST              Validate estimate number                     
         JE    OUMAIN60                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN60 GOTOR SETTXT                                                           
         JE    OUMAIN62                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN62 GOTOR SETODT                                                           
                                                                                
         GOTOR VALOWN                                                           
         JNE   OUMAINER                                                         
                                                                                
         GOTOR VALRFM                                                           
         JE    OUMAIN65                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN65 GOTOR SETTOT                                                           
                                                                                
         GOTOR VALEWC                                                           
         JE    OUMAIN70                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN70 CLI   RQOMACT,RQOMA3Q     Status change only                           
         JNE   OUMAIN75            No                                           
                                                                                
         GOTOR CHKWCS              Yes - validate workcodes                     
         JE    OUMAIN75                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OUMAINER                                                         
                                                                                
OUMAIN75 J     UPDORDX                                                          
                                                                                
OUMAINER OI    TWAMODE,TWAMERP     Main error exits                             
         J     UPDORDX                                                          
         EJECT                                                                  
***********************************************************************         
* order upload: approver data processing                              *         
***********************************************************************         
                                                                                
OUAPPR   MVC   BYTE1,RQOALVL       convert level to binary                      
         NI    BYTE1,X'0F'         and ensure between 1 and 4                   
         CLI   BYTE1,1                                                          
         JNL   *+6                                                              
         DC    H'0'                (must not be)                                
         CLI   BYTE1,4                                                          
         JNH   OUAPPR10                                                         
         DC    H'0'                                                             
                                                                                
OUAPPR10 LLC   R4,BYTE1            determine table entry                        
         SHI   R4,1                                                             
         MHI   R4,XPIDLQ                                                        
         LA    R4,XPVALS(R4)                                                    
         USING XPIDSD,R4                                                        
                                                                                
         OI    XPIDS1,PIDPROQ      set processed                                
         MVI   XPFLAG,YESQ                                                      
                                                                                
         CLI   RQOAKEP,YESQ        keep existing entry?                         
         JE    UPDORDX                                                          
                                                                                
         XC    XPIDC1,XPIDC1       clear 1st entry                              
         MVI   XPIDS1,PIDPROQ                                                   
                                                                                
         OC    RQOAPP1,SPACES      and process first PID                        
         CLC   RQOAPP1,SPACES                                                   
         JNH   OUAPPR20                                                         
                                                                                
         MVC   TEMP2(8),RQOAPP1                                                 
         GOTOR (#GETPIN,AGETPIN)                                                
         OC    XPIDC1,TEMP2+50                                                  
         JNZ   OUAPPR15                                                         
         MVC   ROUERRV,=AL2(AE$INPID)                                           
         J     OUAPPRN                                                          
                                                                                
OUAPPR15 LA    R1,XPIDS1           any approvals before?                        
         GOTOR PROASA                                                           
         JNE   OUAPPRN                                                          
                                                                                
OUAPPR20 XC    XPIDC2,XPIDC2       clear 2st entry                              
         MVI   XPIDS2,0                                                         
                                                                                
         OC    RQOAPP2,SPACES      and process first PID                        
         CLC   RQOAPP2,SPACES                                                   
         JNH   UPDORDX                                                          
                                                                                
         MVC   TEMP2(8),RQOAPP2                                                 
         GOTOR (#GETPIN,AGETPIN)                                                
         OC    XPIDC2,TEMP2+50                                                  
         JNZ   OUAPPR25                                                         
         MVC   ROUERRV,=AL2(AE$INPID)                                           
         J     OUAPPRN                                                          
                                                                                
OUAPPR25 LA    R1,XPIDS2           any approvals before?                        
         GOTOR PROASA                                                           
         JE    UPDORDX                                                          
                                                                                
OUAPPRN  OI    TWAMODE,TWAMERP                                                  
         J     UPDORDX                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* order upload: work code processing                                  *         
***********************************************************************         
                                                                                
OUWCOD   XR    RE,RE               set work code counter                        
         IC    RE,XWCNTR                                                        
         LR    R2,RE                                                            
         AHI   RE,1                                                             
         STC   RE,XWCNTR                                                        
         MHI   R2,XWCLNQ                                                        
                                                                                
         USING XWDSCT,R4                                                        
         LAY   R4,XWVALS                                                        
         AR    R4,R2                                                            
         MVI   XWCSTAT,0                                                        
                                                                                
         CLI   XWCNTR,XMAXWCS      and check for overall maximum                
         JNH   OUWCOD02                                                         
         MVC   ROUERRV,=AL2(AE$MAXWC)                                           
         J     OUWCODN                                                          
                                                                                
OUWCOD02 MVC   XWCODE,RQOWCOD      convert values                               
         OC    XWCODE,SPACES                                                    
         ZAP   XWCAMT,PZERO        Initialise all value fields                  
         ZAP   XWCFCA,PZERO                                                     
         ZAP   XWCINUM,PZERO                                                    
         ZAP   XWCIVAL,PZERO                                                    
         ZAP   XWCIFCA,PZERO                                                    
         XC    XWCIPND,XWCIPND                                                  
         CLC   RQOWAMT,SPACES                                                   
         JNH   OUWCOD04                                                         
         MVC   TEMP2(16),RQOWAMT                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   XWCAMT,TEMP2+16(8)                                               
         AP    XTOTAMT,XWCAMT                                                   
                                                                                
OUWCOD04 CLC   RQOWFCA,SPACES                                                   
         JNH   OUWCOD06                                                         
         MVC   TEMP2(16),RQOWFCA                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   XWCFCA,TEMP2+16(8)                                               
         AP    XTOTFCA,XWCFCA                                                   
                                                                                
OUWCOD06 CLI   ORDTYPE,EXPOQ       validate work code                           
         JNE   OUWCOD08                                                         
         DC    H'0'                no w/c on expense order                      
                                                                                
OUWCOD08 CLI   ORDTYPE,INTOQ                                                    
         JNE   OUWCOD09                                                         
         CLI   XWCNTR,1                                                         
         JE    OUWCOD09                                                         
         DC    H'0'                only 1 w/c on iternal orders                 
                                                                                
OUWCOD09 CLI   ORDTYPE,ARTOQ                                                    
         JNE   OUWCOD10                                                         
         CLI   XWCNTR,2                                                         
         JNH   OUWCOD10                                                         
         DC    H'0'                only 2 w/c on artist orders                  
                                                                                
OUWCOD10 DS    0H                                                               
*&&UK                                                                           
         CLI   CUCTRY,CTRYGER      For Germany check w/c type                   
         JNE   OUWCOD14                                                         
         CLI   ORDTYPE,INTOQ                                                    
         JNE   OUWCOD12                                                         
         CLI   XWCODE,C'0'                                                      
         JL    OUWCOD14                                                         
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     OUWCODN                                                          
                                                                                
OUWCOD12 CLI   XWCODE,C'0'                                                      
         JNL   OUWCOD14                                                         
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     OUWCODN                                                          
*&&                                                                             
         USING WCORECD,R2                                                       
OUWCOD14 DS    0H                  (duplicates checked in web appl)             
                                                                                
         LA    R2,IOKEY            Read for work code                           
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUXCPY                                                   
         MVC   WCOKUNT(L'PRODUL),PRODUL                                         
         MVC   WCOKWRK,XWCODE                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    OUWCOD16                                                         
         MVC   ROUERRV,=AL2(AE$WRKNF)                                           
         J     OUWCODN                                                          
                                                                                
OUWCOD16 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R3,WCORFST                                                       
         USING WCOELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
OUWCOD18 CLI   WCOEL,WCOELQ        Look for WCO element                         
         JE    OUWCOD22                                                         
         CLI   WCOEL,0                                                          
         JNE   OUWCOD20                                                         
         DC    H'0'                (must exist)                                 
                                                                                
OUWCOD20 IC    R0,WCOLN                                                         
         AR    R3,R0                                                            
         J     OUWCOD18                                                         
                                                                                
OUWCOD22 TM    WCOSTAT2,WCOSLORD   W/C locked for orders?                       
         JZ    OUWCOD24                                                         
         MVC   ROUERRV,=AL2(AE$WCLCK)                                           
         J     OUWCODN                                                          
                                                                                
OUWCOD24 DS    0H                                                               
                                                                                
*&&UK                                                                           
         CLI   T.PROPO202,C'Y'                                                  
         JNE   UPDORDX                                                          
         TM    WCOSTAT3,WCOSESCK                                                
         JZ    UPDORDX                                                          
         OI    XWCSTAT,XWCEXWC                                                  
         J     UPDORDX                                                          
*&&                                                                             
                                                                                
*&&US                                                                           
         TM    WCOSTAT,WCOSNONC    Is W/C non-commissionable                    
         JZ    OUWCOD26                                                         
         OI    XWCSTAT,XWCNCOM     mark w/c as such                             
         CLI   XBYTEN,0            Is this the major non comm. w/c?             
         JNE   *+8                                                              
         OI    XWCSTAT,XWCMAJ      mark w/c as such                             
         AP    XTOTNWC,PONE        keep non comm. count                         
         MVI   XBYTEN,1                                                         
         J     UPDORDX                                                          
                                                                                
OUWCOD26 AP    XTOTCWC,PONE        keep commissionable count                    
         CLI   XBYTEC,0            Is this the major comm. w/c?                 
         JNE   *+8                                                              
         OI    XWCSTAT,XWCMAJ      mark w/c as such                             
         MVI   XBYTEC,1                                                         
*&&                                                                             
         J     UPDORDX             more validation here                         
                                                                                
OUWCODN  OI    TWAMODE,TWAMERP                                                  
         J     UPDORDX                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* order upload: xtra data processing                                  *         
***********************************************************************         
         SPACE 1                                                                
OUXDAT   LAY   R6,I_NEW                                                         
         USING EL_RECD,R6                                                       
         XC    EL_REC(EL_KEYL),EL_REC                                           
         MVI   EL_ELCDE,XDFELQ         and build element                        
EL       USING XDFELD,EL_ELEM                                                   
         MVI   EL.XDFEL,XDFELQ         and build element                        
         GOTO1 VHEXIN,DMCB,RQOXCOD,EL.XDFOCOD,6                                 
         MVC   EL_ELWRK(L'XDFOCOD),EL.XDFOCOD                                   
         MVC   EL.XDFOTYP,RQOXTYP                                               
         CLI   RQOXTYP,XDFEDDQ      date?                                       
         JE    OUXDF20                                                          
         CLI   RQOXTYP,XDFEDAQ      amount?                                     
         JE    OUXDF30                                                          
         CLI   RQOXTYP,XDFEDCQ      character                                   
         JE    OUXDF10                                                          
         CLI   RQOXTYP,XDFEDNQ      or number string?                           
         JE    OUXDF10                                                          
         CLI   RQOXTYP,XDFEDXQ      dropdown list                               
         JE    OUXDF10                                                          
         CLI   RQOXTYP,XDFEDYQ      yes/no                                      
         JE    *+6                                                              
         DC    H'0'                 (unknown)                                   
                                                                                
         LA    RE,XDFOLNQ+1                                                     
         STC   RE,EL.XDFLN                                                      
         MVC   EL.XDFODTA(1),RQOXDTA                                            
         J     OUXDF80                                                          
                                                                                
OUXDF10  LA    RE,XDFOLNQ                                                       
         LLC   R1,RQOXDTAL                                                      
         AR    RE,R1                                                            
         STC   RE,EL.XDFLN                                                      
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   EL.XDFODTA(0),RQOXDTA                                            
         EX    R1,0(RF)                                                         
         J     OUXDF80                                                          
                                                                                
OUXDF20  LA    RE,XDFOLNQ+3                                                     
         STC   RE,EL.XDFLN                                                      
         GOTOR VDATCON,DMCB,(0,RQOXDTA+2),(1,EL.XDFODTA)                        
         J     OUXDF80                                                          
                                                                                
OUXDF30  LA    RE,XDFOLNQ+6                                                     
         STC   RE,EL.XDFLN                                                      
         MVC   TEMP2(16),RQOXDTA                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   EL.XDFODTA(6),TEMP2+16(8)                                        
                                                                                
OUXDF80  GOTOR BUFELE,DMCB,('TSAADD',TSARELEN),('BENEWQ',EL_REC)                
         JE    UPDORDX                                                          
         DC    H'0'                                                             
         DROP  EL,R6                                                            
         EJECT                                                                  
***********************************************************************         
* order upload: text processing                                       *         
***********************************************************************         
                                                                                
         SPACE 1                                                                
OUTEXT   LAY   R6,I_NEW                                                         
         USING EL_RECD,R6                                                       
         XC    EL_REC(EL_KEYL),EL_REC                                           
         MVI   EL_ELCDE,SCMELQ                                                  
EL       USING SCMELD,EL_ELEM                                                   
         CLI   RQOTTYP,RQOTTIQ     Item text?                                   
         JE    OUTEXT10                                                         
         CLI   RQOTTYP,RQOTTTQ     Text box?                                    
         JE    OUTEXT30                                                         
         CLI   RQOTTYP,RQOTTHQ     Header text box?                             
         JE    OUTEXT30                                                         
         CLI   RQOTTYP,RQOTTFQ     Footer text box?                             
         JE    OUTEXT30                                                         
         DC    H'0'                                                             
                                                                                
OUTEXT10 MVC   XSEQNO,RQOTSEQ      Item text processing                         
         MVC   XSUBNO,RQOTSSQ                                                   
                                                                                
OUTEXT18 OC    RQOTWCD,SPACES                                                   
                                                                                
         ZAP   XAMOUNT,PZERO                                                    
         CLC   RQOTAMT,SPACES                                                   
         JNH   OUTEXT20                                                         
         MVC   TEMP2(16),RQOTAMT   convert amount                               
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   XAMOUNT,TEMP2+16(8)                                              
                                                                                
OUTEXT20 MVI   XTXTLN,0            if no text passed element will have          
         OC    RQOTTXTL,RQOTTXTL   one space as text                            
         JZ    OUTEXT26                                                         
         MVC   XTXTLN,RQOTTXTL                                                  
                                                                                
OUTEXT26 MVI   EL.SCMEL,SCMELQ                                                  
         MVC   EL.SCMSEQ,XSEQNO                                                 
         MVI   EL.SCMTYPE,SCMTSANP                                              
         ZAP   EL.SCMAMNT,XAMOUNT                                               
         MVC   EL.SCMSSEQ,XSUBNO                                                
         MVC   EL.SCMWCOD,RQOTWCD                                               
         MVC   EL.SCMWODS,RQOWCNM  Work-code description                        
         MVC   EL_ELWRK(L'SCMSEQ+L'SCMTYPE+L'SCMSSEQ),EL.SCMSEQ                 
         LLC   RE,XTXTLN                                                        
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   EL.SCMTEXT(0),RQOTTXT                                            
         EX    RE,0(RF)                                                         
         AHI   RE,SCMLN2Q+1                                                     
         STC   RE,EL.SCMLN                                                      
         GOTOR BUFELE,DMCB,('TSAADD',TSARELEN),('BENEWQ',EL_REC)                
         JE    UPDORDX                                                          
         DC    H'0'                                                             
         DROP  R3                                                               
                                                                                
OUTEXT30 DS    0H                  Text box processing                          
                                                                                
         CLI   RQOTTYP,RQOTTHQ     Header or footer then skip                   
         JE    OUTEXT40                                                         
         CLI   RQOTTYP,RQOTTFQ                                                  
         JE    OUTEXT40                                                         
         CLI   XPRILEN,FF          Use first text box as PRINTED DESCR          
         JNE   OUTEXT40            if not set in main data upload               
         MVI   XPRILEN,0                                                        
         MVI   XPRITXT,C' '                                                     
         OC    RQOTBOXL,RQOTBOXL   If empty make it a single dot                
         JZ    UPDORDX                                                          
                                                                                
         MVC   XPRITXT,RQOTBOX                                                  
         MVC   XPRILEN,RQOTBOXL    set length of printing description           
         J     UPDORDX                                                          
                                                                                
OUTEXT40 MVC   XSEQNO,RQOTSEQ                                                   
                                                                                
         MVI   XTXTLN,0            if no text passed element will have          
         OC    RQOTBOXL,RQOTBOXL   one space as text                            
         JZ    OUTEXT50                                                         
         MVC   XTXTLN,RQOTBOXL                                                  
                                                                                
OUTEXT50 MVI   EL.SCMEL,SCMELQ                                                  
         MVC   EL.SCMSEQ,XSEQNO                                                 
         MVI   EL.SCMTYPE,SCMTPRBD                                              
         CLI   RQOTTYP,RQOTTHQ                                                  
         JE    OUTEXT52                                                         
         MVI   EL.SCMTYPE,SCMTPRAD                                              
         CLI   RQOTTYP,RQOTTFQ                                                  
         JE    OUTEXT52                                                         
         MVI   EL.SCMTYPE,SCMTOPDX                                              
                                                                                
OUTEXT52 MVC   EL_ELWRK(L'SCMSEQ+L'SCMTYPE),EL.SCMSEQ                           
         LLC   RE,XTXTLN                                                        
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   EL.SCMTBOX(0),RQOTBOX                                            
         EX    RE,0(RF)                                                         
         AHI   RE,SCMLN1Q+1                                                     
         STC   RE,EL.SCMLN                                                      
         GOTOR BUFELE,DMCB,('TSAADD',TSARELEN),('BENEWQ',EL_REC)                
         JE    UPDORDX                                                          
         DC    H'0'                                                             
                                                                                
OUTEXTN  OI    TWAMODE,TWAMERP                                                  
                                                                                
OUTEXTY  J     UPDORDX                                                          
         DROP  EL,R6                                                            
         EJECT                                                                  
***********************************************************************         
* order upload: item/article processing                               *         
***********************************************************************         
         SPACE 1                                                                
OUITEM   LAY   R6,I_NEW                                                         
         USING EL_RECD,R6                                                       
         XC    EL_REC(EL_KEYL),EL_REC                                           
         MVI   EL_ELCDE,ARTELQ                                                  
EL       USING ARTELD,EL_ELEM                                                   
         CLI   T.PROEBY16,YESQ                                                  
         JE    OUITEM00                                                         
         MVC   ROUERRV,=AL2(AE$NOUPD)                                           
         J     OUITEMN                                                          
                                                                                
OUITEM00 MVI   XSEQNO,0                                                         
         MVI   XSUBNO,0                                                         
         XC    XISEQN,XISEQN                                                    
         OC    RQOICOD,SPACES                                                   
                                                                                
         CLC   RQOISEQ,SPACES      Get item via seq # unless itemless           
         JNH   OUITEM02                                                         
                                                                                
         GOTO1 VHEXIN,DMCB,RQOISEQ,XISEQN,6                                     
                                                                                
OUITEM02 MVC   XSEQNO,RQOIWSN                                                   
         MVC   XSUBNO,RQOIWSS                                                   
                                                                                
         USING PASRECD,R2                                                       
OUITEM10 OC    XISEQN,XISEQN       itemless?                                    
         JZ    OUITEM32                                                         
         LA    R2,IOKEY                                                         
         XC    PASKEY,PASKEY                                                    
         MVI   PASKTYP,PASKTYPQ                                                 
         MVI   PASKSUB,PASKSQ                                                   
         MVC   PASKCPY,CUXCPY                                                   
         MVC   PASKSEQ,XISEQN                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    OUITEM20                                                         
         DC    H'0'                                                             
                                                                                
OUITEM20 TM    PASKSTAT,ARTKLOCK                                                
         JZ    OUITEM25                                                         
         MVC   ROUERRV,=AL2(AE$ITLOC)                                           
         J     OUITEMN                                                          
                                                                                
OUITEM25 TM    PASKSTAT,ARTKNOPQ                                                
         JZ    OUITEM30                                                         
***      MVC   ROUERRV,=AL2(AE$ITINV)                                           
***      J     OUITEMN             ('no price items' allowed)                   
                                                                                
OUITEM30 CLC   PASKSWC,SPACES                                                   
         JNH   OUITEM32                                                         
         CLC   PASKSWC,RQOIWCD                                                  
         JE    OUITEM32                                                         
         MVC   ROUERRV,=AL2(AE$ITWCI)                                           
         J     OUITEMN                                                          
                                                                                
OUITEM32 MVI   EL.ARTEL,ARTELQ                                                  
         MVC   EL.ARTWC,RQOIWCD                                                 
         MVC   EL.ARTSEQ,XISEQN                                                 
         MVC   EL.ARTWSQN,XSEQNO                                                
         MVC   EL.ARTWSSN,XSUBNO                                                
         MVC   EL_ELWRK(L'ARTSEQ+L'ARTWC),EL.ARTWC                              
         MVC   EL_ELWRK+L'ARTSEQ+L'ARTWC(L'ARTWSQN+L'ARTWSSN),EL.ARTWSQ+        
               N                                                                
         MVC   EL.ARTNUM,RQOICOD                                                
         MVC   TEMP2(16),RQOIPRI                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   EL.ARTPRICE,TEMP2+16(8)                                          
         MVC   TEMP2(16),RQOIMUL                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   EL.ARTMULT,TEMP2+16(8)                                           
                                                                                
         MVI   EL.ARTSTAT,0                                                     
         TM    PASKSTAT,ARTKFLXQ                                                
         JZ    OUITEM43                                                         
         OI    EL.ARTSTAT,ARTSTFQ                                               
                                                                                
OUITEM43 CLI   RQOIOVR,YESQ                                                     
         JNE   OUITEM44                                                         
         OI    EL.ARTSTAT,ARTSTPQ                                               
                                                                                
OUITEM44 TM    PASKSTAT,ARTKNOPQ                                                
         JZ    OUITEM45                                                         
         OI    EL.ARTSTAT,ARTSTXQ                                               
                                                                                
OUITEM45 TM    PASKSTAT,ARTKDAMQ                                                
         JZ    OUITEM46                                                         
         OI    EL.ARTSTAT,ARTSTDQ                                               
                                                                                
OUITEM46 TM    PASKSTAT,ARTKNICQ                                                
         JZ    OUITEM47                                                         
         OI    EL.ARTSTAT,ARTSTNQ                                               
                                                                                
OUITEM47 LA    R3,ARTLN3Q                                                       
                                                                                
         OC    RQOIDSCL,RQOIDSCL     Any description?                           
         JZ    OUITEM60                                                         
         LLC   RF,RQOIDSCL                                                      
         AR    R3,RF                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   EL.ARTDESC(0),RQOIDSC                                            
         EX    RF,0(R1)                                                         
                                                                                
OUITEM60 STC   R3,EL.ARTLN                                                      
         GOTOR BUFELE,DMCB,('TSAADD',TSARELEN),('BENEWQ',EL_REC)                
         JE    OUITEM65                                                         
         MVC   ROUERRV,=AL2(AE$MAX#)                                            
         J     OUITEMN                                                          
                                                                                
OUITEM65 OC    RQOITXTL,RQOITXTL      Any text?                                 
         JZ    EXITY                                                            
         LLC   RF,RQOITXTL                                                      
                                                                                
EL       USING ATXELD,EL_ELEM                                                   
OUITEM75 XC    EL_REC(EL_KEYL),EL_REC                                           
         MVI   EL.ATXEL,ATXELQ                                                  
         MVI   EL_ELCDE,ATXELQ                                                  
         LA    R3,ATXLNQ                                                        
         AR    R3,RF                                                            
         STC   R3,EL.ATXLN                                                      
         MVC   EL.ATXWCOD,RQOIWCD                                               
         MVC   EL.ATXWSQN,XSEQNO                                                
         MVC   EL.ATXWSSN,XSUBNO                                                
         MVC   EL_ELWRK(L'ATXWCOD+L'ATXWSQN+L'ATXWSSN),EL.ATXWCOD               
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   EL.ATXTXT(0),RQOITXT                                             
         EX    RF,0(R1)                                                         
         GOTOR BUFELE,DMCB,('TSAADD',TSARELEN),('BENEWQ',EL_REC)                
         JE    UPDORDX                                                          
         MVC   ROUERRV,=AL2(AE$MAX#)                                            
OUITEMN  OI    TWAMODE,TWAMERP                                                  
         J     UPDORDX                                                          
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* order upload: last for update processing                            *         
***********************************************************************         
                                                                                
OULAST   TM    TWAMODE,TWAMERP                                                  
         JNZ   OULAST90            exit ok on any error                         
         GOTOR GETOPT                                                           
         GOTOR VALINT              internal order validation                    
         JE    OULAST06                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OULASTER                                                         
                                                                                
OULAST06 GOTOR VALEXT              Expense order validation                     
         JE    OULAST08                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OULASTER                                                         
                                                                                
OULAST08 GOTOR PROAPP              Process approvers - check self               
         JE    OULAST10                           approval                      
         JH    OULASTER                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OULASTER                                                         
                                                                                
OULAST10 GOTOR VALSTA              Status validation                            
         JNE   OULASTER                                                         
                                                                                
         GOTOR VCROSS              cross validation                             
         JE    OULAST14                                                         
         TM    XORDIND,XOACCHK                                                  
         JNZ   OULASTER                                                         
         CLI   XSKPVAL,YESQ                                                     
         JNE   OULASTER                                                         
                                                                                
OULAST14 TM    XORDIND,XOTXCHK     Check for pending invoices                   
         JZ    OULAST16                                                         
         GOTOR TRXCHK                                                           
         JNE   OULASTER                                                         
*                                                                               
OULAST16 TM    XORDIND,XCHKMAT     Check for matching                           
         JZ    OULAST18                                                         
         GOTOR CHKMAT                                                           
         JNE   OULASTER                                                         
*                                                                               
OULAST18 TM    XORDIND,XOAUESK     Do we need to check estimate                 
         JNZ   *+12                                                             
         TM    XORDIND,XOESCHK     Do we need to check estimate                 
*&&UK*&& JZ    OULAST35                                                         
*&&US*&& JZ    OULAST30                                                         
         GOTOR CHKEST              estimate checking                            
*&&UK*&& JE    OULAST35                                                         
*&&US*&& JE    OULAST30                                                         
         JH    OULASTER            Exit if throwing an estcheck error           
         J     OULAST85                                                         
*                                                                               
*&&US                                                                           
OULAST30 CLI   XOMACT,RQOMA1Q      Always check when adding order               
         JNE   OULAST35                                                         
         GOTOR CHKBDO                                                           
         JNE   OULASTER                                                         
*&&                                                                             
OULAST35 GOTOR VALIDN              Validate ID number                           
         JNE   OULASTER                                                         
                                                                                
*** UPDATE OF RECORDS STARTS HERE - UNWIND FROM THIS POINT ***                  
                                                                                
OULAST40 CLI   XOMACT,RQOMA1Q      skip for ADD                                 
         JE    OULAST45                                                         
                                                                                
         USING CPTRBLK,R4                                                       
         LA    R4,ELEMENT          Delete passives                              
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'D',AIO3),(C'K',CPTRBLK),0,0,ACOMFACS             
         J     OULAST50                                                         
         DROP  R4                                                               
                                                                                
OULAST45 GOTOR NEWORD              Get order number                             
         JNE   OULASTER                                                         
         MVC   XORDNM,XPREORD      Store order number                           
                                                                                
*  update order records                                                         
                                                                                
OULAST50 CLI   XOMACT,RQOMA3Q      Are we in status only change mode            
         JNE   OULAST55            No                                           
         GOTOR UPDMOR              Yes - update order status                    
         J     OULAST60                                                         
                                                                                
OULAST55 GOTOR BLDMOR              Build order - add and maintain               
         GOTOR BLDEXT              Build order extension                        
         GOTOR BLDSEQ              Build sequence                               
*                                                                               
*                                  Order transactions maintenance               
OULAST60 TM    XORDIND,XOTXDEL     Are we to delete order transaction           
         JZ    OULAST62                                                         
         GOTOR ODELTX              Delete order transaction                     
                                                                                
OULAST62 TM    XORDIND,XOTXADD     Are we to add order transaction              
         JZ    OULAST64                                                         
         GOTOR OADDCA              Add contra account record                    
                                                                                
         GOTOR OADDTX              Add order transaction                        
                                                                                
OULAST64 GOTOR BLDAUD              Build audit record                           
         GOTOR UPGTRN              update transactions with GAP status          
*                                                                               
OULAST85 GOTOR RETURN              Process return data                          
*&&UK                                                                           
         GOTOR MQROUT              MQ routines                                  
*&&                                                                             
OULAST90 DS    0H                                                               
         J     UPDORDX                                                          
                                                                                
OULASTER OI    TWAMODE,TWAMUWD+TWAMERP                                          
                                                                                
***********************************************************************         
* COMMON EXIT                                                         *         
***********************************************************************         
                                                                                
UPDORDX  TM    TWAMODE,TWAMERP                                                  
         JZ    UPDORDY             exit ok on any error                         
         OC    ROUERRV,ROUERRV                                                  
         JZ    EXITY               TWAMODE FROM PREVIOUS CALL, THIS ONE         
*                                                       WAS ALRIGHT             
         XR    R0,R0               BUILD DOWNLOAD MAP ELEMENT                   
         ICM   R0,3,LP_QMAPN                                                    
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
         XR    R1,R1                                                            
         ICM   R1,3,ROUERRV                                                     
         GOTOR PUTERR                                                           
         J     UPDORDN                                                          
                                                                                
UPDORDY  LA    R0,RQUPVAL          Order upload exit GOOD                       
         LA    R1,RQUPLNQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
                                                                                
UPDORDN  LA    R0,RQUPVAL          Order upload exit ERROR                      
         LA    R1,RQUPLNQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         TM    TWAMODE,TWAMUWD     unwind required?                             
         JZ    EXITY                                                            
         NI    TWAMODE,FF-TWAMUWD                                               
         OI    GIND1,GIUNWIND      DDLINK will unwind/abend                     
         J     EXITY                                                            
                                                                                
***********************************************************************         
* CALL LINKIO TO BUILD ERROR RETURN ELEMENT                           *         
***********************************************************************         
                                                                                
PUTERR   NTR1  LABEL=NO                                                         
         STCM  R1,3,WORK                                                        
                                                                                
         CLC   XERRTXT,SPACES                                                   
         JNH   PUTERR2                                                          
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTERR',D#UPLERR),  *        
               WORK,(L'XERRTXT,XERRTXT)                                         
         J     PUTERR4                                                          
                                                                                
PUTERR2  GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTERR',D#UPLERR),  *        
               WORK,0                                                           
                                                                                
PUTERR4  MVC   XERRTXT,SPACES                                                   
                                                                                
PUTERRX  J     EXITY                                                            
                                                                                
***********************************************************************         
* Validate ID number                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALIDN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALIDN*'                                                      
                                                                                
         MVC   TEMP(L'XERRTXT),SPACES                                           
         XC    XIDNEW,XIDNEW                                                    
         CLI   XOMACT,RQOMA1Q      skip for ADD                                 
         JE    EXITY                                                            
                                                                                
         USING ORDELD,R2                                                        
         L     R2,AIO3             get current ID number                        
         AHI   R2,ORDRFST-ORDRECD                                               
         XR    R0,R0                                                            
                                                                                
VALIDN1  CLI   ORDEL,ORDELQ                                                     
         JE    VALIDN2                                                          
         CLI   ORDEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         IC    R0,ORDLN                                                         
         AR    R2,R0                                                            
         J     VALIDN1                                                          
                                                                                
VALIDN2  XOUT  ORDIDNO,FULL1,2                                                  
         CLC   RQOLIDN,SPACES                                                   
         JNH   VALIDN6                                                          
         LA    RE,3                                                             
         CLC   RQOLIDN+1(3),SPACES                                              
         JNH   VALIDN3                                                          
         LA    RE,2                                                             
         CLC   RQOLIDN+2(2),SPACES                                              
         JNH   VALIDN3                                                          
         LA    RE,1                                                             
         CLC   RQOLIDN+3(1),SPACES                                              
         JH    VALIDN5                                                          
                                                                                
VALIDN3  MVC   FULL2,RQOLIDN                                                    
                                                                                
VALIDN4  MVC   RQOLIDN,=C'0000'                                                 
         LA    RF,3                                                             
         SR    RF,RE                                                            
         LA    RE,RQOLIDN(RE)                                                   
         BASR  R1,0                                                             
         MVC   0(0,RE),FULL2                                                    
         EX    RF,0(R1)                                                         
                                                                                
VALIDN5  CLC   RQOLIDN,FULL1                                                    
         JE    VALIDN6                                                          
         MVC   ROUERRV,=AL2(AE$CFUPD)                                           
         MVC   TEMP(4),FULL1                                                    
         MVI   TEMP+4,C'/'                                                      
         MVC   TEMP+5(4),RQOLIDN                                                
         J     EXITN                                                            
                                                                                
VALIDN6  MVC   XIDOLD,ORDIDNO                                                   
         XR    RE,RE                                                            
         ICM   RE,3,ORDIDNO                                                     
         AHI   RE,1                                                             
         STCM  RE,3,XIDNEW                                                      
         J     EXITY                                                            
                                                                                
         DROP  R2                                                               
***********************************************************************         
* REQUEST MAP FOR NEW ORDER UPLOAD CALL                               *         
***********************************************************************         
                                                                                
ORDUPD   LKREQ H,A#OUPL,NEWREC=Y                                                
ORTyp    LKREQ F,1,(D,B#SAVED,RQOURTY),CHAR,TEXT=AC#TYPE                        
                                                                                
OMAct    LKREQ F,10,(D,B#SAVED,RQOMACT),CHAR,TEXT=AC#ACT                        
OMOrd    LKREQ F,11,(D,B#SAVED,RQOMORD),CHAR,TEXT=AC#NUM                        
OMTyp    LKREQ F,12,(D,B#SAVED,RQOMTYP),CHAR,TEXT=AC#TYPE                       
OMOff    LKREQ F,13,(D,B#SAVED,RQOMOFF),CHAR,TEXT=AC#OFFC                       
OMETy    LKREQ F,14,(D,B#SAVED,RQOMETY),CHAR,TEXT=AC#ETYPE                      
OMSup    LKREQ F,15,(D,B#SAVED,RQOMSUP),CHAR,TEXT=AC#SUPC                       
OMExp    LKREQ F,16,(D,B#SAVED,RQOMEXP),CHAR,TEXT=AC#EXPA                       
OMJob    LKREQ F,17,(D,B#SAVED,RQOMSJA),CHAR,TEXT=AC#CLPJO                      
OMDep    LKREQ F,18,(D,B#SAVED,RQOMDEP),CHAR,TEXT=AC#DEPC                       
OMPer    LKREQ F,19,(D,B#SAVED,RQOMPER),CHAR,TEXT=AC#STFC                       
OMAut    LKREQ F,20,(D,B#SAVED,RQOMAUT),CHAR,TEXT=AC#ATHR                       
OMAtT    LKREQ F,21,(D,B#SAVED,RQOMATO),CHAR,TEXT=AC#OATTF                      
OMRbD    LKREQ F,22,(D,B#SAVED,RQOMRBD),PDAT,TEXT=AC#RQRDB                      
OMCur    LKREQ F,23,(D,B#SAVED,RQOMCUR),CHAR,TEXT=AC#CURRY                      
OMTot    LKREQ F,24,(D,B#SAVED,RQOMTOT),CHAR,TEXT=AC#TOTAL                      
OMExc    LKREQ F,25,(D,B#SAVED,RQOMEXC),CHAR,TEXT=AC#EXCHR                      
OMMTx    LKREQ F,26,(D,B#SAVED,RQOMMTXL),(R,VALTXT),TEXT=AC#NRTV,      +        
               LOWERCASE=Y,OLEN=L'RQOMMTX+L'RQOMMTXL                            
OMPTx    LKREQ F,27,(D,B#SAVED,RQOMCTXL),(R,VALTXT),TEXT=AC#TEXT,      +        
               LOWERCASE=Y,OLEN=L'RQOMCTX+L'RQOMCTXL                            
OMDat    LKREQ F,28,(D,B#SAVED,RQOMDAT),PDAT,TEXT=AC#ORDDT                      
OMPrA    LKREQ F,29,(D,B#SAVED,RQOMPRA),CHAR,TEXT=AC#PRTCL                      
OMPrT    LKREQ F,30,(D,B#SAVED,RQOMPRT),CHAR,TEXT=AC#PRTCL                      
OMEWC    LKREQ F,31,(D,B#SAVED,RQOMEOW),CHAR,TEXT=AC#WC                         
OMSta    LKREQ F,32,(D,B#SAVED,RQOMSTA),CHAR,TEXT=AC#TO                         
OMSCC    LKREQ F,33,(D,B#SAVED,RQOMCOML),(R,VALTXT),TEXT=AC#CMTS,      +        
               LOWERCASE=Y,OLEN=L'RQOMCOM+L'RQOMCOML                            
OMNam    LKREQ F,34,(D,B#SAVED,RQOMNAML),(R,VALTXT),TEXT=AC#NAME,      +        
               LOWERCASE=Y,OLEN=L'RQOMNAM+L'RQOMNAML                            
OMDAd    LKREQ F,35,(D,B#SAVED,RQOMDADL),(R,VALTXT),TEXT=AC#ADR,       +        
               LOWERCASE=Y,OLEN=L'RQOMDAD+L'RQOMDADL,MAXLEN=L'RQOMDAD           
OMCha    LKREQ F,36,(D,B#SAVED,RQOMCAO),CHAR,TEXT=AC#CHNGS                      
OMEst    LKREQ F,37,(D,B#SAVED,RQOMEST),CHAR,TEXT=AC#FLES                       
OMTFC    LKREQ F,38,(D,B#SAVED,RQOMTFC),CHAR,TEXT=AC#TOTAL                      
OMTFC    LKREQ F,39,(D,B#SAVED,RQOMIOL),CHAR,TEXT=AC#SUACC                      
*&&UK                                                                           
OMRFm    LKREQ F,40,(D,B#SAVED,RQOMRFM),CHAR,TEXT=AC#REPFO                      
*&&                                                                             
*&&US                                                                           
OMRFm    LKREQ F,40,(D,B#SAVED,RQOMRFM),CHAR,TEXT=AC#FRMAT                      
*&&                                                                             
Owner    LKREQ F,41,(D,B#SAVED,RQOWNER),CHAR,TEXT=AC#OWNER                      
GAPST    LKREQ F,42,(D,B#SAVED,RQOGAPST),HEXD,TEXT=AC#GAPST                     
GAPED    LKREQ F,43,(D,B#SAVED,RQOGAPED),PDAT,TEXT=AC#GAPED                     
GAPEM    LKREQ F,44,(I,B#SAVED,RQOGAPEI),CHAR,TEXT=AC#GAPEM,           +        
               LIST=F,OLEN=L'STCGEML,SORT=NO                                    
*                                                                               
OWCod    LKREQ F,60,(D,B#SAVED,RQOWCOD),CHAR,TEXT=AC#WC                         
OWAmt    LKREQ F,61,(D,B#SAVED,RQOWAMT),CHAR,TEXT=AC#AMT                        
OWFCA    LKREQ F,62,(D,B#SAVED,RQOWFCA),CHAR,TEXT=AC#FCAMT                      
                                                                                
OTTyp    LKREQ F,70,(D,B#SAVED,RQOTTYP),CHAR,TEXT=AC#DTATP                      
OTSeq    LKREQ F,71,(D,B#SAVED,RQOTSEQ),LBIN,TEXT=AC#SEQ                        
OTSub    LKREQ F,72,(D,B#SAVED,RQOTSSQ),LBIN,TEXT=AC#SEQNO                      
OTWCd    LKREQ F,73,(D,B#SAVED,RQOTWCD),CHAR,TEXT=AC#WC                         
OTAmt    LKREQ F,74,(D,B#SAVED,RQOTAMT),CHAR,TEXT=AC#AMT                        
OTTXT    LKREQ F,75,(D,B#SAVED,RQOTTXTL),(R,VALTXT),TEXT=AC#TEXT,      +        
               LOWERCASE=Y,OLEN=L'RQOTTXT+L'RQOTTXTL                            
OTBox    LKREQ F,76,(D,B#SAVED,RQOTBOXL),(R,VALTXT),TEXT=AC#BOX,       +        
               LOWERCASE=Y,OLEN=L'RQOTBOX+L'RQOTBOXL                            
                                                                                
OIWCd    LKREQ F,80,(D,B#SAVED,RQOIWCD),CHAR,TEXT=AC#WC                         
OIWSq    LKREQ F,81,(D,B#SAVED,RQOIWSN),LBIN,TEXT=AC#WC                         
OIWSS    LKREQ F,82,(D,B#SAVED,RQOIWSS),LBIN,TEXT=AC#WC                         
OICod    LKREQ F,83,(D,B#SAVED,RQOICOD),CHAR,TEXT=AC#ARTCD                      
OISeq    LKREQ F,84,(D,B#SAVED,RQOISEQ),CHAR,TEXT=AC#SEQNO                      
OIPri    LKREQ F,85,(D,B#SAVED,RQOIPRI),CHAR,TEXT=AC#PRICE                      
OIMul    LKREQ F,86,(D,B#SAVED,RQOIMUL),CHAR,TEXT=AC#MULT                       
OIOvr    LKREQ F,87,(D,B#SAVED,RQOIOVR),CHAR,TEXT=AC#OVRI                       
OIDSC    LKREQ F,88,(D,B#SAVED,RQOIDSCL),(R,VALTXT),TEXT=AC#PLDSC,     +        
               LOWERCASE=Y,OLEN=L'RQOIDSC+L'RQOIDSCL                            
OITxt    LKREQ F,89,(D,B#SAVED,RQOITXTL),(R,VALTXT),TEXT=AC#TEXT,      +        
               LOWERCASE=Y,OLEN=L'RQOITXT+L'RQOITXTL                            
                                                                                
OXCod    LKREQ F,90,(D,B#SAVED,RQOXCOD),CHAR,TEXT=AC#CODE                       
OXTyp    LKREQ F,91,(D,B#SAVED,RQOXTYP),CHAR,TEXT=AC#TYPE                       
OXDat    LKREQ F,92,(D,B#SAVED,RQOXDTAL),(R,VALTXT),TEXT=AC#DATA,      +        
               LOWERCASE=Y,OLEN=L'RQOXDTA+L'RQOXDTAL                            
                                                                                
OALvl    LKREQ F,100,(D,B#SAVED,RQOALVL),CHAR,TEXT=AC#LEVEL                     
OAKSt    LKREQ F,101,(D,B#SAVED,RQOAKEP),CHAR,TEXT=AC#KEEP                      
OAAp1    LKREQ F,102,(D,B#SAVED,RQOAPP1),CHAR,TEXT=AC#APLST                     
OAAp2    LKREQ F,103,(D,B#SAVED,RQOAPP2),CHAR,TEXT=AC#APLST                     
                                                                                
OLIDN    LKREQ F,120,(D,B#SAVED,RQOLIDN),CHAR,TEXT=AC#ID                        
OLCCR    LKREQ F,121,(D,B#SAVED,RQOLCCR),CHAR,TEXT=AC#VALDT                     
OLEst    LKREQ F,122,(D,B#SAVED,RQOLEST),CHAR,TEXT=AC#SKIP                      
OLWCn    LKREQ F,123,(D,B#SAVED,RQOWCNM),CHAR,TEXT=AC#WCNAM                     
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* END OF REQUEST MAP TABLES                                          *          
***********************************************************************         
                                                                                
         LKMAP X                                                                
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Validate variable text                                              *         
***********************************************************************         
                                                                                
VALTXT   LM    R2,R4,LP_AINP-LP_D(R5)                                           
         STC   R3,0(R4)            Returns L'Text,Text string                   
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         MVC   1(0,R4),0(R2)                                                    
         EX    R3,0(RE)                                                         
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* Local routines for this Server                                      *         
***********************************************************************         
                                                                                
* Initialise values for order upload                                            
INIOUV   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13INIV*'                                                      
                                                                                
         ZAP   XTOTAMT,PZERO       clear totals                                 
         ZAP   XOTOTAMT,PZERO                                                   
         ZAP   XTOTFCA,PZERO                                                    
         ZAP   XOTOTFCA,PZERO                                                   
                                                                                
         MVI   XMQSTAT,0                                                        
         CLI   T.PROPO303,YESQ                                                  
         JNE   *+8                                                              
         OI    XMQSTAT,XMQSMQQ                                                  
                                                                                
         MVI   XSUPLLVL,NOQ                                                     
         MVI   XEXPLLVL,NOQ                                                     
         MVI   XESTWYN,NOQ                                                      
         MVI   XPFLAG,NOQ                                                       
         MVC   XESTWNO,SPACES                                                   
                                                                                
*&&US                                                                           
         ZAP   XTOTCWC,PZERO       Comm. and Non-Comm. W/C counter              
         ZAP   XTOTNWC,PZERO                                                    
         MVI   XBYTEN,0            Clear Minor/Major for Non-Comm               
         MVI   XBYTEC,0            and Comm                                     
*&&                                                                             
                                                                                
         USING EL_RECD,R6                                                       
         LAY   R6,I_NEW                                                         
         GOTOR BUFELE,DMCB,('TSAINI',TSARELEN),('BENEWQ',EL_REC)                
         LAY   R6,I_OLD                                                         
         GOTOR BUFELE,DMCB,('TSAINI',TSARELEO),('BEOLDQ',EL_REC)                
         LAY   R6,I_AUD                                                         
         GOTOR BUFELE,DMCB,('TSAINI',TSARSTCL),('BEAUDQ',EL_REC)                
         J     EXITY                                                            
         DROP  R6                                                               
***********************************************************************         
* Interface to TSAR for general use buffer                            *         
*                                                                     *         
* Note that when running off-line the TSAR buffers are acquired only  *         
* once - the XC(TSPNEWL) below is intentional as the first time       *         
* through the code TSAR will issue the GETMAIN for the buffer         *         
***********************************************************************         
                                                                                
BUFELE   NTR1  LABEL=NO,WORK=(RC,BEWORKL)                                       
         J     *+12                                                             
         DC    C'*BUFELE*'                                                      
         USING BEWORKD,RC                                                       
                                                                                
         MVC   BEACTN(BEWORKL),0(R1)                                            
         L     R3,BEATSBLK                                                      
TR       USING TSARD,R3            R3=A(TSAR block)                             
         USING EL_RECD,R4                                                       
         L     R4,BEABUFAR         R0=A(Record)                                 
         ST    R4,TR.TSAREC        Set A(Record)                                
         CLI   BEACTN,TSAINI       Test initialisation call                     
         JNE   BUFELE04                                                         
         XC    TR.TSARD(TSPNEWL),TR.TSARD                                       
         MVC   TR.TSACTN,BEACTN    Action                                       
         MVC   TR.TSACOM,ACOMFACS  A(COMFACS)                                   
         LHI   R0,ONEK                                                          
         STCM  R0,3,TR.TSBUFFL     Set require 1MB off-line                     
         MVI   TR.TSRECI,TSRXTN+TSRTSAB2                                        
         CLI   BETYPE,BEAUDQ       audit elements                               
         JNE   *+8                                                              
         MVI   TR.TSRECI,TSRXTN+TSRTSAB1                                        
         CLI   BETYPE,BENEWQ       new elements                                 
         JNE   *+8                                                              
         MVI   TR.TSRECI,TSRXTN+TSRMINB1                                        
         MVI   TR.TSKEYL,EL_KEYL1  Set key length                               
         LHI   R0,EL_LENQ                                                       
         STCM  R0,3,TR.TSRECL      Set max record length                        
         MVI   TR.TSINDS,TSINODSK  Set no disk writes (save/restore)            
         GOTOR VTSAR,TR.TSARD                                                   
         TM    TR.TSINDS,TSIINIOK                                               
         JNZ   EXITY                                                            
         DC    H'0'                Initialisation failure                       
                                                                                
BUFELE04 TM    TR.TSINDS,TSIINIOK  Test initialised                             
         JZ    BUFELE08                                                         
                                                                                
         MVC   TR.TSACTN,BEACTN    Set action                                   
         CLI   TR.TSACTN,TSAADD    Are we adding records to buffer              
         JNE   BUFELE06            No                                           
         CLI   BETYPE,BEAUDQ       Are we adding audit elements                 
         JNE   BUFELE06            No                                           
         LH    RF,ELSEQ#           Set sequence number on TSAR key              
         AHI   RF,1                                                             
         STH   RF,EL_KSEQ                                                       
         STH   RF,ELSEQ#                                                        
                                                                                
BUFELE06 GOTOR VTSAR,TR.TSARD      Call TSAR                                    
         MVC   TSAERR,TR.TSERRS    Return TSERRS in INVERR                      
         J     BUFELEX                                                          
                                                                                
BUFELE08 MVI   TSAERR,TSEEOF       Set EOF if not initialised                   
                                                                                
BUFELEX  CLI   TSAERR,0            Set condition code for caller                
         J     EXIT                                                             
         DROP  TR,R4,RC                                                         
         EJECT                                                                  
BEWORKD  DSECT                     ** BUFELE local working storage **           
BEACTN   DS    0XL1                Action                                       
BEATSBLK DS    A                   A(TSAR block to use)                         
BETYPE   DS    0XL1                Type of element buffer                       
BEOLDQ   EQU   1                   Old xdata/items/text buffer                  
BENEWQ   EQU   2                   New xdata/items/text buffer                  
BEAUDQ   EQU   3                   Audit element buffer                         
BEABUFAR DS    A                   A(Buffer area to use)                        
BEWORKL  EQU   *-BEWORKD                                                        
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
                                                                                
PROASA   DS    0H                  copy previous approvals to xpids1/2          
*                                                                               
PROASA0  LA    RF,XSVPAPP          List of approvers who have approved          
*                                                                               
PROASA2  OC    0(2,RF),0(RF)       Any PIDs that approved                       
         BZR   RE                                                               
         CLC   0(2,RF),1(R1)       Yes - Does it match any PID present          
         JE    PROASA4                                                          
         AHI   RF,2                Bump to next entry                           
         J     PROASA2                                                          
*                                  approved previously                          
PROASA4  TM    0(R1),PIDAPPQ       approving again?                             
         JZ    PROASAY                                                          
         MVC   ROUERRV,=AL2(AE$APAPA)     yes, why?                             
         LTR   RE,RE                                                            
         BR    RE                  CCNEQ IF ERROR                               
*                                                                               
PROASAY  OI    0(R1),PIDAPPQ       copy previous approval                       
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Get order record into AIO3                                          *         
***********************************************************************         
         SPACE 1                                                                
GETORD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETORD*'                                                      
                                                                                
         CLI   RQOMACT,RQOMA1Q     Read order record unless ADD action          
         JE    GETORD60                                                         
                                                                                
         USING ORDRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUXCPY                                                   
         MVC   ORDKORD,RQOMORD                                                  
         MVC   XORDNM,RQOMORD                                                   
         MVC   NEWONUM,RQOMORD                                                  
         L     R1,=AL4(IORDUP+IODIR+IO3)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    GETORD04                                                         
         MVC   ROUERRV,=AL2(AE$ORDNF)                                           
         J     EXITN                                                            
                                                                                
GETORD04 TM    ORDKSTA2,ORDSEXEX   ensure BrandOcean order                      
         JNZ   GETORD06                                                         
         MVC   ROUERRV,=AL2(AE$INEBD)                                           
         J     EXITN                                                            
                                                                                
GETORD06 TM    ORDKSTAT,ORDSLDEL   ensure not logically deleted                 
         JZ    GETORD08                                                         
         MVC   ROUERRV,=AL2(AE$ORDEL)                                           
         J     EXITN                                                            
                                                                                
GETORD08 MVC   XORDDA,ORDKDA       Save D/A                                     
         MVC   CSVKEY1,ORDKEY      Save key                                     
         MVC   XOOSTA1,ORDKSTAT    Save order status                            
         MVC   XOOSTA2,ORDKSTA2                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING PIDELD,R3                                                        
         L     R2,AIO3             save PID information                         
         XR    R3,R3                                                            
         LA    R3,ORDRFST                                                       
         XR    R0,R0                                                            
         LA    R4,XSVPAPP          saved previous approvers                     
         XC    ELSEQ#,ELSEQ#       Clear element sequence for OAMELD            
         L     R6,AGENAREA         R6=A(email addresses)                        
         XR    R0,R0                                                            
         ICM   R0,1,XGAPEMLN       # of array elements                          
                                                                                
GETORD12 CLI   PIDEL,0                                                          
         JE    GETORD60                                                         
         CLI   PIDEL,PIDELQ                                                     
         JE    GETORD16                                                         
         CLI   PIDEL,OAMELQ                                                     
         JE    GETORD22                                                         
         CLI   PIDEL,ORDELQ                                                     
         JE    GETORD24                                                         
         CLI   PIDEL,GDAELQ        General date element                         
         JE    GETORD20                                                         
         CLI   PIDEL,FFTELQ        Free form text element                       
         JE    GETORD26                                                         
         CLI   PIDEL,SORELQ        Client/prod/job for expense orders           
         JE    GETORD30                                                         
         CLI   PIDEL,SPAELQ        Staff and department account codes           
         JE    GETORD30                                                         
         CLI   PIDEL,AFCELQ        Foreign currency                             
         JE    GETORD30                                                         
         CLI   PIDEL,SCMELQ        Matching and printing description            
         JE    GETORD30                                                         
         CLI   PIDEL,ENMELQ        Order name element                           
         JE    GETORD30                                                         
         CLI   PIDEL,OATELQ        Order address element                        
         JE    GETORD30                                                         
                                                                                
GETORD14 IC    R0,PIDLN                                                         
         AR    R3,R0                                                            
         J     GETORD12                                                         
                                                                                
GETORD16 CLI   PIDTYPE,PIDT1Q                                                   
         JL    GETORD30                                                         
         CLI   PIDTYPE,PIDT4Q                                                   
         JH    GETORD30                                                         
         XR    RF,RF               will set XPIDS1 etc                          
         IC    RF,PIDTYPE                                                       
         SHI   RF,1                                                             
         MHI   RF,XPIDLQ                                                        
         LA    RF,XPVALS(RF)       determine PID entry                          
         LA    RE,(L'PIDNTRS)-1                                                 
         CLI   PIDNTR#,2                                                        
         JL    *+8                                                              
         LA    RE,2*(L'PIDNTRS)-1                                               
         BASR  R1,0                                                             
         MVC   0(0,RF),PIDNTRS                                                  
         EX    RE,0(R1)                                                         
         TM    PIDNTRS,PIDAPPQ                                                  
         JZ    GETORD18                                                         
         MVC   0(2,R4),PIDNTRS+1   Save PIDs that approved previously           
         AHI   R4,2                                                             
                                                                                
GETORD18 CLI   PIDNTR#,2                                                        
         JL    GETORD30                                                         
         TM    PIDNTRS+L'PIDNTRS,PIDAPPQ                                        
         JZ    GETORD30                                                         
         MVC   0(2,R4),PIDNTRS+1+L'PIDNTRS                                      
         AHI   R4,2                                                             
         J     GETORD30                                                         
                                                                                
         USING GDAELD,R3                                                        
GETORD20 CLI   GDATYPE,GDAGAPEX                                                 
         JNE   GETORD14                                                         
         MVC   XOLGAPEX,GDADATE                                                 
         J     GETORD14                                                         
                                                                                
         USING OAMELD,R3                                                        
GETORD22 AP    XOTOTAMT,OAMAMNT                                                 
         CLI   OAMLN,OAMLN3Q                                                    
         JL    GETORD30                                                         
         AP    XOTOTFCA,OAMFCAMT                                                
         J     GETORD30                                                         
                                                                                
         USING ORDELD,R3                                                        
GETORD24 MVC   XORDOWN,ORDCPID                                                  
         MVC   XORDSTA,ORDSTAT                                                  
         MVC   XOLGSTA,ORDGSTAT                                                 
         MVI   RQOMTYP,PROOQ                                                    
         CLC   PRODUL,ORDACCU      Is net a/c production                        
         JE    *+8                 Yes                                          
         MVI   RQOMTYP,EXPOQ       No - expense order                           
         CLI   ORDLN,ORDLN4Q       Check el is long enough                      
         JL    GETORD30                                                         
         MVC   XORDSUB,ORDCSUBM                                                 
         J     GETORD30                                                         
*                                                                               
         USING FFTELD,R3                                                        
GETORD26 DS    0H                                                               
*&&UK*&& CLI   FFTTYPE,FFTTPEML                                                 
*&&US*&& CLI   FFTTYPE,FFTTEML     Have we got GAP emails                       
         JNE   GETORD30            No                                           
         LA    R4,FFTEML1          R4=A(first emal address)                     
         LLC   R1,FFTLN                                                         
         AR    R1,R3               R1=A(end of element)                         
*                                                                               
GETORD28 CR    R4,R1               Hit end of element?                          
         JNL   GETORD14            Yes - exit                                   
         MVC   0(L'FFTEML1,R6),0(R4)  No - extract email addresses into         
         AHI   R4,L'FFTEML1                                                     
         AHI   R6,L'FFTEML1                                                     
         LLC   R0,XOLGEMLN                                                      
         AHI   R0,1                                                             
         STC   R0,XOLGEMLN                                                      
         J     GETORD28                                                         
                                                                                
         USING ORDELD,R3                                                        
GETORD30 GOTOR AUDOLD,DMCB,ORDELD                                               
         J     GETORD14                                                         
                                                                                
GETORD60 CLI   RQOMTYP,ARTOQ       Ensure order type is allowed                 
         JE    GETORD61                                                         
         CLI   RQOMTYP,PROOQ                                                    
         JE    GETORD61                                                         
         CLI   RQOMTYP,INTOQ                                                    
         JE    GETORD61                                                         
         CLI   RQOMTYP,EXPOQ                                                    
         JE    GETORD61                                                         
         DC    H'0'                invalid order type passed                    
                                                                                
GETORD61 MVC   ORDTYPE,RQOMTYP                                                  
         MVC   XOMTYP,RQOMTYP                                                   
*                                                                               
         CLI   RQOMACT,RQOMA2Q     Maintain                                     
         JE    *+12                                                             
         CLI   RQOMACT,RQOMA4Q                                                  
         JNE   GETORDX                                                          
         XC    BYTE1,BYTE1                                                      
         XC    IOKEY,IOKEY                                                      
*                                                                               
         LA    R2,IOKEY                                                         
         MVC   ORDKEY,CSVKEY1                                                   
         LLC   RE,BYTE1            increment sequence #                         
         AHI   RE,1                                                             
         STC   RE,BYTE1                                                         
         MVC   ORDKSEQ,BYTE1       Read sequential records                      
         L     R1,=AL4(IOHI+IODIR+IO2)                                          
         GOTOR (#IOEXEC,AIOEXEC)                                                
         J     GETORD64                                                         
*                                                                               
GETORD62 LA    R2,IOKEY                                                         
         L     R1,=AL4(IOSQ+IODIR+IO2)                                          
         GOTOR (#IOEXEC,AIOEXEC)                                                
*                                                                               
GETORD64 CLC   ORDKEY(ORDKSEQ-ORDKEY),CSVKEY1                                   
         JNE   GETORD80                                                         
                                                                                
         L     R1,=AL4(IOGET+IOMST+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         USING SCMELD,R3                                                        
         LA    R3,ORDRFST          Look for items/text                          
GETORD66 CLI   SCMEL,0                                                          
         JE    GETORD62            read next record                             
         CLI   SCMEL,SCMELQ        Standard comment element                     
         JE    GETORD70                                                         
         CLI   SCMEL,ARTELQ        Article/item element                         
         JE    GETORD70                                                         
         CLI   SCMEL,ATXELQ        Article/item text element                    
         JE    GETORD70                                                         
GETORD68 LLC   R0,SCMLN                                                         
         AR    R3,R0                                                            
         J     GETORD66                                                         
                                                                                
GETORD70 GOTOR AUDOLD,DMCB,SCMELD                                               
         J     GETORD68                                                         
*                                                                               
GETORD80 LA    R2,IOKEY            read for extension record                    
         MVC   ORDKEY,CSVKEY1                                                   
         MVI   ORDKSEQ,ORDKEXTN                                                 
         L     R1,=AL4(IORD+IODIR+IO2)                                          
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JNE   GETORDX                                                          
*                                                                               
         L     R1,=AL4(IOGET+IOMST+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO2                                                          
         USING XDFELD,R3                                                        
         LA    R3,ORDRFST          Extract extra data elements                  
                                                                                
GETORD82 CLI   XDFEL,0             End of record                                
         JE    GETORDX                                                          
         CLI   XDFEL,XDFELQ        Extra data element                           
         JE    GETORD86                                                         
GETORD84 LLC   R0,XDFLN                                                         
         AR    R3,R0                                                            
         J     GETORD82                                                         
*                                                                               
GETORD86 GOTOR AUDOLD,DMCB,XDFELD                                               
         J     GETORD84                                                         
*                                                                               
GETORDX  J     EXITY                                                            
         DROP  R2,R3                                                            
**********************************************************************          
* Validate Office for order upload                                   *          
**********************************************************************          
         SPACE 1                                                                
VALOFF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13VOFF*'                                                      
                                                                                
         OC    RQOMOFF,SPACES                                                   
         MVC   XORDOFF,RQOMOFF                                                  
         CLC   XORDOFF,SPACES                                                   
         JE    EXITY                                                            
                                                                                
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JNZ   VALOFF10            2CO = read office record                     
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY            1CO = read 2D department lvl 1               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVI   ACTKUNT,C'2'                                                     
         MVI   ACTKLDG,C'D'                                                     
         MVC   ACTKACT(L'XORDOFF),XORDOFF                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALOFF02                                                         
         MVC   ROUERRV,=AL2(AE$OFCNF)                                           
         J     VALOFFER                                                         
                                                                                
VALOFF02 LA    R2,IOKEY                                                         
         TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VALOFF04                                                         
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     VALOFFER                                                         
                                                                                
VALOFF04 TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VALOFF20                                                         
         MVC   ROUERRV,=AL2(AE$ACTCL)                                           
         J     VALOFFER                                                         
         DROP  R2                                                               
                                                                                
         USING OFFRECD,R2                                                       
VALOFF10 LA    R2,IOKEY                                                         
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUXCPY                                                   
         MVC   OFFKOFF,XORDOFF                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALOFF12                                                         
         MVC   ROUERRV,=AL2(AE$OFCNF)                                           
         J     VALOFFER                                                         
                                                                                
VALOFF12 LA    R2,IOKEY                                                         
         TM    OFFKSTAT,OFFSLIST                                                
         JZ    VALOFF20                                                         
         MVC   ROUERRV,=AL2(AE$INVOF)                                           
         J     VALOFFER                                                         
         DROP  R2                                                               
                                                                                
         USING OFFALD,R2                                                        
VALOFF20 L     R2,AOFFAREA                                                      
         MVC   OFFAOFFC,XORDOFF    OFFAL validation                             
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD                                                    
         JE    EXITY                                                            
         DROP  R2                                                               
                                                                                
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
                                                                                
VALOFFER MVC   XERRTXT(2),XORDOFF                                               
         J     EXITN                                                            
                                                                                
**********************************************************************          
* Final office checks for order upload                               *          
**********************************************************************          
         SPACE 1                                                                
CHKOFF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKOFF*'                                                      
                                                                                
         CLC   XORDOFF,SPACES      any office set?                              
         JH    CHKOFF05                                                         
         CLI   OFFIND,FULLYQ       office mandatory?                            
         JNE   EXITY                                                            
         MVC   ROUERRV,=AL2(AE$IOFCQ)                                           
         J     CHKOFFER                                                         
                                                                                
         USING OFFALD,R2                                                        
CHKOFF05 L     R2,AOFFAREA                                                      
         MVC   OFFAOFFC,XORDOFF    OFFAL validation                             
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD                                                    
         JE    CHKOFF10                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     CHKOFFER                                                         
         DROP  R2                                                               
                                                                                
CHKOFF10 CLC   XSUPOFF,SPACES                                                   
         JNH   EXITY                                                            
         CLC   XSUPOFF,XORDOFF                                                  
         JE    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$ACADO)                                           
         MVC   XERRTXT(2),XSUPOFF                                               
         J     EXITN                                                            
                                                                                
CHKOFFER MVC   XERRTXT(2),XORDOFF                                               
         J     EXITN                                                            
                                                                                
**********************************************************************          
* Validate expense account for order upload                          *          
**********************************************************************          
         SPACE 1                                                                
VALEXP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13VEXP*'                                                      
                                                                                
         XC    XEXPCULA,XEXPCULA                                                
         CLI   ORDTYPE,EXPOQ                                                    
         JNE   EXITY                                                            
                                                                                
         OC    RQOMEXP,SPACES            set U/L only if not passed             
         CLC   RQOMEXP,SPACES                                                   
         JNE   VALEXP05                                                         
         MVC   XEXPCULA(1),CUXCPY                                               
         MVC   XEXPCULA+L'ACTKCPY(2),EXPEU       (SE)                           
         MVC   XEXPCULA+L'ACTKCPY+L'ACTKUNT+L'ACTKLDG(L'ACTKACT),SPACES         
         J     EXITY                                                            
                                                                                
VALEXP05 MVC   XEXPCULA(1),CUXCPY                                               
         MVC   XEXPCULA+L'ACTKCPY(L'ACTKULA),RQOMEXP                            
         MVI   XANAIND,0                                                        
         MVI   XCSTCDE,C' '                                                     
                                                                                
*&&US*&& LA    RE,EXPUS                                                         
                                                                                
*&&UK                                                                           
         LA    RE,EXPEU                                                         
                                                                                
VALEXP10 CLI   0(RE),FF                                                         
         JNE   VALEXP15                                                         
         MVC   ROUERRV,=AL2(AE$INLDG)                                           
         J     VALEXPER                                                         
*&&                                                                             
*&&US                                                                           
VALEXP10 CLI   0(RE),FF                                                         
         JNE   *+14                                                             
         MVC   FULL2(2),=AL2(AE$INLDG)                                          
         J     VALEXPER                                                         
         CLC   XEXPCULA+1(L'ACTKUNT+L'ACTKLDG),0(RE)                            
         JE    VALEXP15                                                         
         AHI   RE,3                                                             
         J     VALEXP10                                                         
*&&                                                                             
                                                                                
VALEXP15 MVC   TEMP(L'ACTKULA),XEXPCULA+1                                       
         GOTOR VALLDG                                                           
         JE    VALEXP20                                                         
         MVC   ROUERRV,=AL2(AE$ACADO)                                           
         J     VALEXPER                                                         
                                                                                
VALEXP20 CLI   BYTE4,YESQ                                                       
         JNE   VALEXP21                                                         
         MVC   ROUERRV,=AL2(AE$ACADO)                                           
         J     VALEXPER                                                         
                                                                                
         USING ACTRECD,R2                                                       
VALEXP21 LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,XEXPCULA                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALEXP25                                                         
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     VALEXPER                                                         
                                                                                
VALEXP25 TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VALEXP27                                                         
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     VALEXPER                                                         
                                                                                
VALEXP27 TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VALEXP30                                                         
         MVC   ROUERRV,=AL2(AE$ACTCL)                                           
         J     VALEXPER                                                         
                                                                                
VALEXP30 TM    ACTKSTAT,ACTSABLP                                                
         JNZ   VALEXP35                                                         
         MVC   ROUERRV,=AL2(AE$INACP)                                           
         J     VALEXPER                                                         
                                                                                
VALEXP35 DS    0H                                                               
*&&UK                                                                           
         TM    ACTKSTAT,ACTSDRFT                                                
         JZ    VALEXP40                                                         
         MVC   ROUERRV,=AL2(AE$ACCNA)                                           
         J     VALEXPER                                                         
*&&                                                                             
VALEXP40 MVI   XEXPLLVL,YESQ                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         XR    R0,R0                                                            
                                                                                
*&&US                                                                           
         CLI   T.PROEBY10,SUBQ                                                  
         JE    VALEXP45                                                         
         CLI   T.PROEBY10,YESQ                                                  
         JNE   EXITY                                                            
                                                                                
SUBQ     EQU   C'S'                                                             
*&&                                                                             
                                                                                
VALEXP45 CLI   RSTEL,RSTELQ                                                     
         JE    VALEXP55                                                         
         CLI   RSTEL,0                                                          
*&&UK*&& JE    EXITY                                                            
*&&US*&& JE    VALEXP70                                                         
                                                                                
VALEXP50 IC    R0,RSTLN                                                         
         AR    R3,R0                                                            
         J     VALEXP45                                                         
                                                                                
VALEXP55 TM    RSTSTAT1,RSTSEADD   department analysis                          
         JZ    *+8                                                              
         OI    XANAIND,XANADEP                                                  
         TM    RSTSTAT1,RSTSGPEI   staff analysis                               
         JZ    *+8                                                              
*&&UK                                                                           
         OI    XANAIND,XANAPER                                                  
         MVC   XCSTCDE,RSTCOSTG                                                 
         CLI   XCSTCDE,C' '                                                     
         JNH   VALEXP60                                                         
*&&                                                                             
*&&US                                                                           
         OI    XANAIND,XANAPER+XANADEP                                          
         TM    RSTSTAT2,RSTSYCST   cost postings for expenses ledger            
         JZ    VALEXP60                                                         
*&&                                                                             
         OI    XANAIND,XANACST                                                  
                                                                                
VALEXP60 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALEXP50                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     VALEXPER                                                         
                                                                                
*&&US                                                                           
         USING CATD,R1                                                          
VALEXP70 TM    XANAIND,XANACST     Is this alreay set?                          
         JO    EXITY                                                            
         LA    R1,CATBLK           GET COST SETTING FROM CATCALL                
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,VDATAMGR                                                 
         MVC   CATSEAC,XEXPCULA                                                 
         CLC   XORDOFF,SPACES      Any office passed?                           
         JNH   *+10                                                             
         MVC   CATOFF,XORDOFF                                                   
         GOTO1 VCATCALL                                                         
         CLI   CATERR,0            if any problems - die                        
         JNE   VALEXP80                                                         
         CLI   CATPST,C'Y'         Do they require Cost postings?               
         JNE   EXITY                                                            
         OI    XANAIND,XANACST                                                  
         J     EXITY                                                            
                                                                                
VALEXP80 MVC   ROUERRV,=AL2(AE$SINAC)                                           
         MVC   XERRTXT(L'CATACC3-1),CATACC3+1                                   
         J     VALEXPR2            Skip next MVC into XERRTXT                   
         DROP  R1                                                               
*&&                                                                             
*&&UK                                                                           
         CLI   CUACCS,0            Test any user limit access                   
         JE    EXITY                                                            
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         TM    OFFACST4,CPYSOFF2                                                
         JZ    EXITY                                                            
         TM    OFFAXSTA,CPXLACAC   Test account limit access in use             
         JZ    EXITY                                                            
         MVC   OFFAOPOS,BYTE1      Set by VALLDG                                
         ST    R2,OFFAREC                                                       
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         JE    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$SECLK)  Security lock out                        
         J     EXITN                                                            
         DROP  R1                                                               
*&&                                                                             
                                                                                
VALEXPER MVC   XERRTXT(L'RQOMEXP),RQOMEXP                                       
         J     EXITN                                                            
VALEXPR2 J     EXITN                                                            
         DROP  R2,R3                                                            
                                                                                
* Validate Expenditure type for order upload                                    
VALETY   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13VETY*'                                                      
                                                                                
         MVC   XEXPTYP,RQOMETY     save and read for expenditure type           
         OC    XEXPTYP,SPACES                                                   
                                                                                
         USING ETYRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    ETYKEY,ETYKEY                                                    
         MVI   ETYKTYP,ETYKTYPQ                                                 
         MVI   ETYKSUB,ETYKSUBQ                                                 
         MVC   ETYKCPY,CUXCPY                                                   
         MVC   ETYKCODE,XEXPTYP                                                 
         MVC   IOKEYSAV,IOKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         CLC   IOKEYSAV(ETYKOFFC-ETYRECD),IOKEY                                 
         JE    VALETY05                                                         
         MVC   ROUERRV,=AL2(AE$INETY)                                           
         J     VALETYER                                                         
                                                                                
VALETY05 LA    R2,IOKEY                                                         
         TM    ETYKSTAT,ETYSLOCK                                                
         JNZ   VALETY10                                                         
                                                                                
         TM    ETYKSAPP,ETYKSAOQ                                                
         JZ    EXITY                                                            
                                                                                
VALETY10 MVC   ROUERRV,=AL2(AE$ETYLO)                                           
                                                                                
VALETYER MVC   XERRTXT(3),XEXPTYP                                               
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
                                                                                
* Validate ledger of account for order upload                                   
VALLDG   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13VLDG*'                                                      
                                                                                
         USING LDGRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUXCPY                                                   
         MVC   LDGKUNT(2),TEMP                                                  
         MVI   BYTE1,0                                                          
         MVI   BYTE2,0                                                          
         MVI   BYTE4,NOQ                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING LDGELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,LDGRFST                                                       
         MVI   XARTOFS,0                                                        
         XR    R0,R0                                                            
                                                                                
VALLDG02 CLI   LDGEL,0                                                          
         JE    VALLDG20                                                         
         CLI   LDGEL,LDGELQ                                                     
         JE    VALLDG06                                                         
         CLI   LDGEL,RSTELQ                                                     
         JE    VALLDG08                                                         
         CLI   LDGEL,ACLELQ                                                     
         JE    VALLDG10                                                         
                                                                                
VALLDG04 IC    R0,LDGLN                                                         
         AR    R3,R0                                                            
         J     VALLDG02                                                         
                                                                                
VALLDG06 MVC   BYTE1,LDGOPOS                                                    
         J     VALLDG04                                                         
                                                                                
         USING RSTELD,R3                                                        
VALLDG08 MVC   BYTE2,RSTSECY+1                                                  
         J     VALLDG04                                                         
                                                                                
         USING ACLELD,R3                                                        
VALLDG10 MVC   LDGAL1,ACLELLVA                                                  
         MVC   LDGAL2,ACLELLVB                                                  
         MVC   LDGAL3,ACLELLVC                                                  
         MVC   LDGAL4,ACLELLVD                                                  
         CLI   TEMP+1,STLDGQ       Artist account?                              
         JNE   VALLDG04                                                         
         MVC   XARTOFS,ACLELLVC    determine agent offset                       
         CLI   ACLELLVD,L'ACTKACT                                               
         JE    VALLDG04                                                         
         MVC   XARTOFS,ACLELLVB                                                 
         CLI   ACLELLVC,L'ACTKACT                                               
         JE    VALLDG04                                                         
         MVC   XARTOFS,ACLELLVA                                                 
         J     VALLDG04                                                         
                                                                                
VALLDG20 CLC   BYTE2,CUAUTH+1      Test security level of ledger                
         JH    EXITN                                                            
                                                                                
VALLDG22 CLI   OFFIND,NONEQ        check only if office agency                  
         JE    EXITY                                                            
         CLI   BYTE1,0             offpos = T is OK                             
         JE    EXITY                                                            
         CLI   BYTE1,LDGOTRAN                                                   
         JE    EXITY                                                            
*&&US                                                                           
         CLI   BYTE1,LDGONKHI      is Office in the KEY?                        
         JH    EXITY               No - Skip                                    
*&&                                                                             
                                                                                
         XR    R1,R1                                                            
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JZ    VALLDG26                                                         
         AHI   R1,1                                                             
         NI    BYTE1,FF-LDGOKEY2                                                
                                                                                
VALLDG26 XR    RE,RE                                                            
         IC    RE,BYTE1                                                         
         SHI   RE,1                                                             
         LA    RE,TEMP+2(RE)                                                    
         BASR  RF,0                                                             
         CLC   0(0,RE),XORDOFF                                                  
         EX    R1,0(RF)                                                         
         JE    EXITY                                                            
         CLC   XORDOFF,SPACES                                                   
         JH    EXITN                                                            
         MVI   BYTE4,YESQ                                                       
         MVC   HALF1,SPACES                                                     
         BASR  RF,0                                                             
         MVC   HALF1(0),0(RE)                                                   
         EX    R1,0(RF)                                                         
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
* Validate Supplier type for order upload                                       
VALSUP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13VSUP*'                                                      
                                                                                
         MVI   XGAPYN,NOQ                                                       
         MVI   XINTLED,SKLDGQ                                                   
         CLI   CUCTRY,CTRYGER                                                   
         JE    VALSUP01                                                         
         CLI   RQOURTY,RQOURT1                                                  
         JNE   VALSUP01                                                         
         MVC   XINTLED,RQOMIOL                                                  
                                                                                
VALSUP01 STC   R1,BYTE1            save call mode                               
         MVC   XSUPOFF,SPACES                                                   
         CLI   BYTE1,1                                                          
         JE    VALSUP10                                                         
                                                                                
         OC    RQOMSUP,SPACES                                                   
                                                                                
*&&UK*&& MVC   TEMP(14),RQOMSUP                                                 
                                                                                
*&&US                                                                           
         LA    RE,TEMP                                                          
         LA    R1,L'RQOMSUP                                                     
         LA    RF,SUPLDGS                                                       
                                                                                
VALSUP02 CLI   0(RF),FF                                                         
         JE    VALSUP04                                                         
         CLC   RQOMSUP(2),0(RF)    did they enter vendor u/l?                   
         JE    VALSUP06            yes - continue as is                         
         AHI   RF,3                                                             
         J     VALSUP02                                                         
                                                                                
VALSUP04 MVC   TEMP(2),=C'SV'      no - put sv in manually                      
         LA    RE,TEMP+2                                                        
         LA    R1,L'RQOMSUP-2                                                   
                                                                                
VALSUP06 SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,RE),RQOMSUP                                                  
         EX    R1,0(RF)                                                         
*&&                                                                             
                                                                                
         CLC   RQOMSUP,SPACES                                                   
         JH    VALSUP10                                                         
         MVC   XSUPCURR,AGYCURR                                                 
         CLI   ORDTYPE,INTOQ       for internals do post W/C                    
         JE    EXITY                                                            
         MVC   XSUPCULA,SPACES                                                  
         MVC   XSUPCULA(1),CUXCPY                                               
         MVI   XSUPCULA+1,C'S'                                                  
         MVC   XSUPCULA+2(1),SCPYEL+CPYSUPX-CPYELD                              
         MVI   XSUPCURR,ASTCANY    default to all currencies                    
         CLI   ORDTYPE,ARTOQ       Is this an artist order                      
         JNE   VALSUP11            No                                           
         MVI   XSUPCULA+2,C'T'     Artist ledger ST                             
         J     VALSUP11                                                         
                                                                                
VALSUP10 MVC   XSUPCULA(1),CUXCPY                                               
         MVC   XSUPCULA+1(14),TEMP                                              
         MVC   XSUPNAME,SPACES                                                  
         MVC   XSUPFNAM,SPACES                                                  
         MVI   XSUPCURR,ASTCANY    preset to all currencies                     
                                                                                
VALSUP11 MVC   TEMP(14),XSUPCULA+1                                              
         GOTOR VALLDG                                                           
         JE    VALSUP12                                                         
         MVC   ROUERRV,=AL2(AE$ACADO)                                           
         J     VALSUPER                                                         
                                                                                
VALSUP12 CLI   BYTE4,YESQ                                                       
         JNE   VALSUP13                                                         
         MVC   XSUPOFF,HALF1                                                    
                                                                                
VALSUP13 LA    RE,INTLDGS                                                       
         CLI   ORDTYPE,INTOQ                                                    
         JE    VALSUP14                                                         
         LA    RE,ARTLDGS                                                       
         CLI   ORDTYPE,ARTOQ                                                    
         JE    VALSUP14                                                         
         LA    RE,SUPLDGS                                                       
         CLI   ORDTYPE,PROOQ                                                    
         JE    VALSUP14                                                         
         LA    RE,SUELDGS                                                       
                                                                                
VALSUP14 CLI   0(RE),FF                                                         
         JNE   VALSUP16                                                         
         MVC   ROUERRV,=AL2(AE$INLDG)                                           
         J     VALSUPER                                                         
                                                                                
VALSUP16 CLC   XSUPCULA+1(2),0(RE)                                              
         JE    VALSUP18                                                         
         AHI   RE,3                                                             
         J     VALSUP14                                                         
                                                                                
VALSUP18 CLC   TEMP+2(12),SPACES   Skip if no account passed                    
         JE    EXITY                                                            
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,XSUPCULA                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALSUP20                                                         
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     VALSUPER                                                         
                                                                                
VALSUP20 TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VALSUP21                                                         
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     VALSUPER                                                         
                                                                                
VALSUP21 TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VALSUP22                                                         
         MVC   ROUERRV,=AL2(AE$ACTCL)                                           
         J     VALSUPER                                                         
                                                                                
VALSUP22 DS    0H                                                               
*&&UK                                                                           
         TM    ACTKSTAT,ACTSDRFT                                                
         JZ    VALSUP24                                                         
         MVC   ROUERRV,=AL2(AE$ACCNA)                                           
         J     VALSUPER                                                         
*&&                                                                             
                                                                                
VALSUP24 TM    ACTKSTAT,ACTSABLP                                                
         JNZ   VALSUP26                                                         
         MVC   ROUERRV,=AL2(AE$INACP)                                           
         J     VALSUPER                                                         
                                                                                
VALSUP26 CLC   RQOMSUP,SPACES      If supplier automatically populated          
         JNH   *+8                 then skip check later on                     
         MVI   XSUPLLVL,YESQ                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         XR    R0,R0                                                            
                                                                                
VALSUP28 CLI   RSTEL,0                                                          
         JE    VALSUP50                                                         
         CLI   RSTEL,RSTELQ                                                     
         JE    VALSUP32                                                         
         CLI   RSTEL,ASTELQ                                                     
         JE    VALSUP36                                                         
         CLI   RSTEL,NAMELQ                                                     
         JE    VALSUP38                                                         
         CLI   RSTEL,XNMELQ                                                     
         JE    VALSUP40                                                         
         CLI   RSTEL,XNMELQ                                                     
         JE    VALSUP40                                                         
         CLI   RSTEL,FFTELQ                                                     
         JE    VALSUP42                                                         
                                                                                
VALSUP30 IC    R0,RSTLN                                                         
         AR    R3,R0                                                            
         J     VALSUP28                                                         
                                                                                
VALSUP32 CLI   RSTEL,RSTLN2Q                                                    
         JNH   VALSUP33                                                         
*&&US                                                                           
         TM    RSTSTAT7,RSTGAPYN                                                
         JZ    *+8                                                              
         MVI   XGAPYN,YESQ                                                      
*&&                                                                             
*&&UK                                                                           
         TM    RSTSTAT7,RSTGAPQY                                                
         JZ    *+8                                                              
         MVI   XGAPYN,YESQ                                                      
         TM    RSTSTAT7,RSTGAPQN                                                
         JZ    *+8                                                              
         MVI   XGAPYN,NOQ                                                       
*&&                                                                             
VALSUP33 CLI   ORDTYPE,INTOQ                                                    
         JE    VALSUP34                                                         
         CLI   RSTLN,RSTLN3Q                                                    
         JL    VALSUP34                                                         
         TM    RSTSTAT5,RSTSPROV                                                
         JZ    VALSUP34                                                         
         MVC   ROUERRV,=AL2(AE$PROVV)                                           
         J     VALSUPER                                                         
                                                                                
VALSUP34 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALSUP30                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     VALSUPER                                                         
                                                                                
         USING ASTELD,R3                                                        
VALSUP36 MVC   XSUPCURR,ASTCUR                                                  
         CLC   XSUPCURR,SPACES                                                  
         JH    VALSUP30                                                         
         MVC   XSUPCURR,AGYCURR                                                 
         J     VALSUP30                                                         
                                                                                
         USING NAMELD,R3                                                        
VALSUP38 XR    RE,RE               save supplier name                           
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         BASR  RF,0                                                             
         MVC   XSUPNAME(0),NAMEREC                                              
         EX    RE,0(RF)                                                         
         J     VALSUP30                                                         
                                                                                
         USING XNMELD,R3                                                        
VALSUP40 XR    RE,RE               save supplier foreign name                   
         ICM   RE,1,XNMSUBL                                                     
         JZ    VALSUP30                                                         
         SHI   RE,2                                                             
         BASR  RF,0                                                             
         MVC   XSUPFNAM(0),XNMSUBN                                              
         EX    RE,0(RF)                                                         
         J     VALSUP30                                                         
*                                                                               
         USING FFTELD,R3                                                        
VALSUP42 DS    0H                                                               
*&&UK*&& CLI   FFTTYPE,FFTTPEML    EMAIL?                                       
*&&US                                                                           
         CLI   FFTTYPE,FFTTBEML    BUSINESS EMAIL                               
         JE    *+12                                                             
         CLI   FFTTYPE,FFTTEML                                                  
*&&                                                                             
         JNE   VALSUP30                                                         
         MVC   XSUPEM,SPACES                                                    
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   XSUPEM(0),FFTDATA                                                
         EX    RF,0(R1)                                                         
         J     VALSUP30                                                         
                                                                                
VALSUP50 DS    0H                                                               
*&&US                                                                           
         CLI   XGAPYN,YESQ        Set to Yes?                                   
         JE    EXITY                                                            
*&&                                                                             
*&&UK                                                                           
         CLI   XGAPYN,C' '        Set yet?                                      
         JE    EXITY                                                            
*&&                                                                             
         CLI   LDGAL1,L'ACTKACT   Only 1 level?                                 
         JE    EXITY                                                            
*                                                                               
         LA    R0,4                                                             
         LA    R6,LDGAL4                                                        
VALSUP52 CLI   0(R6),0                                                          
         JE    VALSUP54                                                         
         CLI   0(R6),L'ACTKACT                                                  
         JE    VALSUP54                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,1                                                             
         LA    RF,ACTKACT(RF)      Find length of account                       
         CLI   0(RF),C' '                                                       
         JH    VALSUP56                                                         
VALSUP54 SHI   R6,1                                                             
         JCT   R0,VALSUP52                                                      
         J     EXITY                                                            
*                                                                               
K        USING ACTRECD,IOKEY                                                    
VALSUP56 MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         MVC   K.ACTKUNT(L'ACTKUNT+L'ACTKLDG),ACTKUNT                           
         LLC   RF,0(R6)                                                         
         SHI   RF,1                                                             
         LTR   RF,RF                                                            
         JNM   *+6                                                              
         DC    H'0'                                                             
         BASR  RE,0                                                             
         MVC   K.ACTKACT(0),ACTKACT                                             
         EX    RF,0(RE)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('RSTELQ',ACTRECD),0                   
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R1                                                        
         L     R1,12(R1)                                                        
         CLI   RSTLN,RSTLN2Q       Is element long enough                       
         JNH   VALSUP58                                                         
*&&US                                                                           
         TM    RSTSTAT7,RSTGAPYN   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   XGAPYN,YESQ                                                      
*&&                                                                             
*&&UK                                                                           
         TM    RSTSTAT7,RSTGAPQY   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   XGAPYN,YESQ                                                      
         TM    RSTSTAT7,RSTGAPQN                                                
         JZ    *+8                                                              
         MVI   XGAPYN,NOQ                                                       
*&&                                                                             
*                                                                               
VALSUP58 DS    0H                                                               
*&&US                                                                           
         CLI   XGAPYN,YESQ         BOTH SETTINGS SET TO YES YET?                
         JE    EXITY                                                            
*&&                                                                             
*&&UK                                                                           
         CLI   XGAPYN,C' '         SETTING SET YET?                             
         JH    EXITY                                                            
*&&                                                                             
         J     VALSUP54                                                         
*                                                                               
VALSUPER MVC   XERRTXT(14),TEMP                                                 
         J     EXITN                                                            
                                                                                
         DROP  R1,R2,R3,K                                                       
                                                                                
                                                                                
* Validate SJ account for order upload                                          
VALJOB   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13VJOB*'                                                      
                                                                                
         OC    RQOMSJA,SPACES      initialise values                            
         MVC   XCLICULA,SPACES                                                  
         MVC   XPROCULA,SPACES                                                  
         MVC   XJOBCULA,SPACES                                                  
         MVC   XCLIC,SPACES                                                     
         MVC   XPROC,SPACES                                                     
         MVC   XJOBC,SPACES                                                     
                                                                                
         CLI   ORDTYPE,EXPOQ       skip if no SJ account data passed            
         JNE   VALJOB02                                                         
         CLC   RQOMSJA,SPACES                                                   
         JE    EXITY                                                            
                                                                                
VALJOB02 MVC   XJOBCULA(1),CUXCPY                                               
         MVC   XJOBCULA+1(2),PRODUL                                             
         MVC   XJOBCULA+3(12),RQOMSJA                                           
                                                                                
         CLC   PLSEC,CUAUTH+1      check ledger security                        
         JNH   VALJOB04                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB04 XR    R1,R1               set client/prod/job codes and a/cs           
         IC    R1,PCLILEN                                                       
         AHI   R1,2                                                             
         BASR  RF,0                                                             
         MVC   XCLICULA(0),XJOBCULA                                             
         EX    R1,0(RF)                                                         
         MVC   XCLIC,XCLICULA+3                                                 
                                                                                
         XR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         AHI   RF,2                                                             
         BASR  RE,0                                                             
         MVC   XPROCULA(0),XJOBCULA                                             
         EX    RF,0(RE)                                                         
         SR    RF,R1                                                            
         SHI   R1,2                                                             
         LA    R1,XJOBCULA+3(R1)                                                
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   XPROC(0),0(R1)                                                   
         EX    RF,0(RE)                                                         
                                                                                
         AHI   R1,2                                                             
         IC    R1,PJOBLEN                                                       
         IC    RF,PPROLEN                                                       
         SR    R1,RF                                                            
         SHI   R1,1                                                             
         LA    RF,XJOBCULA+3(RF)                                                
         BASR  RE,0                                                             
         MVC   XJOBC(0),0(RF)                                                   
         EX    R1,0(RE)                                                         
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY            Check client record                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,XCLICULA                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALJOB06                                                         
         MVC   ROUERRV,=AL2(AE$INCLI)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB06 TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VALJOB08                                                         
         MVC   ROUERRV,=AL2(AE$CLILK)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB08 TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VALJOB10                                                         
         MVC   ROUERRV,=AL2(AE$ACTCL)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB10 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING PPRELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         XR    R0,R0                                                            
         MVC   XCPJOFF,SPACES                                                   
                                                                                
VALJOB12 CLI   PPREL,0                                                          
         JE    VALJOB20                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    VALJOB16                                                         
         CLI   PPREL,RSTELQ                                                     
         JE    VALJOB18                                                         
                                                                                
VALJOB14 IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     VALJOB12                                                         
                                                                                
VALJOB16 MVC   XCPJOFF,PPRGAOFF                                                 
         OC    XCPJOFF,SPACES                                                   
         J     VALJOB14                                                         
                                                                                
         USING RSTELD,R3                                                        
VALJOB18 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALJOB14                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     VALJOBER                                                         
         DROP  R3                                                               
                                                                                
VALJOB20 CLC   XPROC,SPACES        Check product record                         
         JNH   VALJOB60                                                         
                                                                                
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,XPROCULA                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALJOB22                                                         
         MVC   ROUERRV,=AL2(AE$INPRO)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB22 TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VALJOB24                                                         
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB24 TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VALJOB26                                                         
         MVC   ROUERRV,=AL2(AE$ACTCL)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB26 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING PPRELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         XR    R0,R0                                                            
                                                                                
VALJOB28 CLI   PPREL,0                                                          
         JE    VALJOB36                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    VALJOB32                                                         
         CLI   PPREL,RSTELQ                                                     
         JE    VALJOB34                                                         
                                                                                
VALJOB30 IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     VALJOB28                                                         
                                                                                
VALJOB32 CLC   PPRGAOFF,SPACES                                                  
         JNH   VALJOB30                                                         
         MVC   XCPJOFF,PPRGAOFF                                                 
         OC    XCPJOFF,SPACES                                                   
         J     VALJOB30                                                         
                                                                                
         USING RSTELD,R3                                                        
VALJOB34 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALJOB30                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     VALJOBER                                                         
         DROP  R3                                                               
                                                                                
VALJOB36 CLC   XJOBC,SPACES        Check job record                             
         JNH   VALJOB60                                                         
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,XJOBCULA                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALJOB38                                                         
         MVC   ROUERRV,=AL2(AE$INJOB)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB38 TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VALJOB40                                                         
         MVC   ROUERRV,=AL2(AE$JOBLK)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB40 TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VALJOB42                                                         
         MVC   ROUERRV,=AL2(AE$JOBCL)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB42 TM    ACTKSTAT,ACTSDRFT                                                
         JZ    VALJOB44                                                         
         MVC   ROUERRV,=AL2(AE$ACCNA)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB44 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R0,AIO4             copy AIO1 into AIO4                          
         LA    R1,IOLENQ           keep a copy of the job record for            
         L     RE,AIO1                    check estimate routine                
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING PPRELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         XR    R0,R0                                                            
                                                                                
VALJOB46 CLI   PPREL,0                                                          
         JE    VALJOB60                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    VALJOB50                                                         
         CLI   PPREL,RSTELQ                                                     
         JE    VALJOB52                                                         
*&&US*&& CLI   PPREL,JOBELQ                                                     
*&&US*&& JE    VALJOB56                                                         
                                                                                
VALJOB48 IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     VALJOB46                                                         
                                                                                
VALJOB50 CLC   PPRGAOFF,SPACES                                                  
         JNH   VALJOB48                                                         
         MVC   XCPJOFF,PPRGAOFF                                                 
         OC    XCPJOFF,SPACES                                                   
         J     VALJOB48                                                         
                                                                                
         USING RSTELD,R3                                                        
VALJOB52 CLI   RSTLN,RSTLN3Q                                                    
         JL    VALJOB54                                                         
         TM    RSTLSTAT,RSTLSORQ   locked from orders?                          
         JZ    VALJOB54                                                         
         MVC   ROUERRV,=AL2(AE$JOBLK)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB54 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALJOB48                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     VALJOBER                                                         
         DROP  R3                                                               
                                                                                
*&&US                                                                           
         USING JOBELD,R3                                                        
VALJOB56 NI    BYTE3,X'FF'-JOBSMCSE   Start off fresh                           
         TM    JOBSTA1,JOBSMCSE       Is this a EST TYP=B?                      
         JZ    VALJOB48                                                         
         OI    BYTE3,JOBSMCSE                                                   
         J     VALJOB48                                                         
         DROP  R3                                                               
*&&                                                                             
                                                                                
VALJOB60 CLC   XORDOFF,SPACES      If order office not set then                 
         JH    VALJOB62            set it from production office                
         MVC   XORDOFF,XCPJOFF                                                  
                                                                                
VALJOB62 CLI   OFFIND,FULLYQ       - Office checking                            
         JNE   VALJOB66                                                         
         CLC   XCPJOFF,SPACES                                                   
         JNE   VALJOB64                                                         
         MVC   ROUERRV,=AL2(AE$INVPO)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB64 DS    0H                                                               
*&&US*&& CLI   ORDTYPE,EXPOQ                                                    
*&&US*&& JE    VALJOB68                                                         
         CLC   XORDOFF,XCPJOFF                                                  
         JE    VALJOB70                                                         
         MVC   ROUERRV,=AL2(AE$MIXOF)                                           
         J     VALJOBER                                                         
                                                                                
VALJOB66 DS    0H                                                               
*&&US*&& CLI   ORDTYPE,EXPOQ                                                    
*&&US*&& JE    VALJOB68                                                         
         CLC   XCPJOFF,SPACES                                                   
         JE    VALJOB70                                                         
         CLC   XORDOFF,SPACES                                                   
         JE    VALJOB68                                                         
         CLC   XORDOFF,XCPJOFF                                                  
         JE    VALJOB70                                                         
         MVC   ROUERRV,=AL2(AE$MIXOF)                                           
         J     VALJOBER                                                         
*&&UK                                                                           
VALJOB68 MVC   XORDOFF,XCPJOFF                                                  
                                                                                
VALJOB70 CLC   XORDOFF,SPACES                                                   
         JE    EXITY                                                            
                                                                                
         USING OFFALD,R2                                                        
         L     R2,AOFFAREA                                                      
         MVC   OFFAOFFC,XORDOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
*&&                                                                             
*&&US                                                                           
VALJOB68 DS    0H                                                               
                                                                                
VALJOB70 CLC   XCPJOFF,SPACES                                                   
         JE    EXITY                                                            
                                                                                
         USING OFFALD,R2                                                        
         L     R2,AOFFAREA                                                      
         MVC   OFFAOFFC,XCPJOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
*&&                                                                             
         GOTO1 VOFFAL,OFFALD       validate office against security             
         JE    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     VALJOBER                                                         
         DROP  R2                                                               
                                                                                
VALJOBER MVC   XERRTXT(14),XJOBCULA+1                                           
         J     EXITN                                                            
***********************************************************************         
* Call getopt to get option maintain settings                         *         
***********************************************************************         
         SPACE 1                                                                
GETOPT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETOPT*'                                                      
                                                                                
         L     RE,AGOBLOCB        CALL GETOPT FOR SCHEME CODE                   
         LA    RF,GOBLOCKX-GOBLOCK                                              
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         L     RE,AGOXBLCK                                                      
         LA    RF,GOXBLKX-GOXBLOCK                                              
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         L     R0,AGOBBLCK        EXTENSION BLOCK                               
         LH    R1,=Y(GOBBLKXX-GOBBLOCK)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,AGOBLOCB        CALL GETOPT FOR DEFAULT EXPIRATION            
         USING GOBLOCKD,RF        PERIOD                                        
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK                                                  
         MVC   GOABEXT,AGOBBLCK                                                 
         MVC   GOSELOFC,XORDOFF   FILL OUT OFFICE CODE                          
         MVI   GOWHICH,0                                                        
                                                                                
         MVC   GOCTRY,CUCTRY                                                    
         MVC   GOSELCUL(1),CUXCPY                                               
         MVC   GOSELCUL+1(2),PRODUL                                             
         MVI   GOWHICH,0                                                        
         GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
                                                                                
         USING GOXBLKD,RE                                                       
         L     RE,AGOXBLCK                                                      
         LLC   RF,GOGDEO          GET EXPIRATION PERIOD                         
         CVD   RF,DUB                                                           
         MVC   XDFEXPO,DUB+6                                                    
         CLC   XCLIC,SPACES                                                     
         JNH   EXITY                                                            
                                                                                
         L     RE,AGOBLOCB        CALL GETOPT FOR SCHEME CODE                   
         LA    RF,GOBLOCKX-GOBLOCK                                              
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         L     RE,AGOXBLCK                                                      
         LA    RF,GOXBLKX-GOXBLOCK                                              
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         L     R0,AGOBBLCK        EXTENSION BLOCK                               
         LH    R1,=Y(GOBBLKXX-GOBBLOCK)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,AGOBLOCB                                                      
         USING GOBLOCKD,RF                                                      
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK                                                  
         MVC   GOABEXT,AGOBBLCK                                                 
                                                                                
         MVC   GOCTRY,CUCTRY                                                    
         MVC   GOSELCUL(1),CUXCPY                                               
         MVC   GOSELCUL+1(2),PRODUL                                             
         MVC   GOSELCLI,XCLIC                                                   
         MVC   GOSELPRO,XPROC                                                   
         MVC   GOSELJOB,XJOBC                                                   
         MVI   GOWHICH,0                                                        
         USING XWDSCT,R1                                                        
         LAY   R1,XWVALS                                                        
         MVC   GOSELWC,XWCODE                                                   
         DROP  R1                                                               
         GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
         J     EXITY                                                            
         DROP  RE,RF                                                            
                                                                                
* Validate department and staff for analysis for order upload                   
VALANA   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13VANA*'                                                      
                                                                                
         OC    RQOMDEP,SPACES                                                   
         OC    RQOMPER,SPACES                                                   
         XC    XDEPCULA,XDEPCULA                                                
         XC    XSTACULA,XSTACULA                                                
                                                                                
         CLC   RQOMDEP,SPACES                                                   
         JE    VALANA05                                                         
*&&US                                                                           
*                                                                               
* Temporary fix to see if U/L 2D is being passed down incorrectly as            
*           as part of the account based on CPYSTAT5 setting.                   
*           If it is, remove it from RQOMDEP                                    
*                                                                               
         TM    SCPYEL+CPYSTAT5-CPYELD,CPYSOFPL                                  
         JZ    VALANA01                                                         
         CLC   =C'2D',RQOMDEP                                                   
         JNE   VALANA01                                                         
         MVC   TEMP(L'RQOMDEP),SPACES                                           
         MVC   TEMP(L'RQOMDEP-2),RQOMDEP+2                                      
         MVC   RQOMDEP,TEMP                                                     
VALANA01 DS    0H                                                               
*&&                                                                             
         MVC   XDEPCULA(1),CUXCPY                                               
         MVC   XDEPCULA+1(2),=C'2D'                                             
         MVC   XDEPCULA+3(12),RQOMDEP                                           
         MVI   XSWAPQ,YESQ                                                      
         LA    R4,XDEPCULA                                                      
         J     VALANA10                                                         
                                                                                
VALANA05 CLC   RQOMPER,SPACES                                                   
         JE    EXITY                                                            
         MVC   XSTACULA(1),CUXCPY                                               
         MVC   XSTACULA+1(2),=C'2P'                                             
         MVC   XSTACULA+3(12),RQOMPER                                           
         MVI   XSWAPQ,NOQ                                                       
         LA    R4,XSTACULA                                                      
                                                                                
VALANA10 MVC   TEMP(14),1(R4)                                                   
         GOTOR VALLDG                                                           
*&&UK*&& JE    VALANA15                                                         
*&&US*&& JE    VALANA12                                                         
         MVC   ROUERRV,=AL2(AE$ACADO)                                           
         J     VALANAER                                                         
*&&US                                                                           
VALANA12 XR    RF,RF                                                            
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JZ    *+8                 2CO = read office record                     
         LHI   RF,1                                                             
         CLC   RQOMDEP,SPACES      Final Acct match before proceeding           
         JNH   VALANA1A                                                         
         BASR  RE,0                                                             
         CLC   RQOMOFF(0),RQOMDEP                                               
         EX    RF,0(RE)                                                         
         JNE   VALANA13                                                         
*                                                                               
VALANA1A CLC   RQOMPER,SPACES                                                   
         JNH   VALANA15                                                         
         BASR  RE,0                                                             
         CLC   RQOMOFF(0),RQOMPER                                               
         EX    RF,0(RE)                                                         
         JE    VALANA14                                                         
*                                                                               
VALANA13 MVC   ROUERRV,=AL2(AE$ACADO)                                           
         J     VALANAER                                                         
*                                                                               
VALANA14 LLC   RF,ONERL2L                                                       
         SHI   RF,1                                                             
         CLC   RQOMDEP,SPACES                                                   
         JNH   VALANA15                                                         
         BASR  RE,0                                                             
         CLC   RQOMDEP(0),RQOMPER  Check departments match as well              
         EX    RF,0(RE)                                                         
         JE    VALANA15                                                         
         MVC   ROUERRV,=AL2(AE$ACSDD)                                           
         J     VALANAER                                                         
*&&                                                                             
*                                                                               
VALANA15 CLI   BYTE4,YESQ                                                       
         JNE   VALANA17                                                         
         MVC   ROUERRV,=AL2(AE$ACADO)                                           
         J     VALANAER                                                         
                                                                                
         USING ACTRECD,R2                                                       
VALANA17 LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,0(R4)                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALANA20                                                         
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     VALANAER                                                         
                                                                                
VALANA20 TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VALANA22                                                         
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     VALANAER                                                         
                                                                                
VALANA22 TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VALANA25                                                         
         MVC   ROUERRV,=AL2(AE$ACTCL)                                           
         J     VALANAER                                                         
                                                                                
VALANA25 TM    ACTKSTAT,ACTSABLP                                                
         JNZ   VALANA30                                                         
         MVC   ROUERRV,=AL2(AE$INACP)                                           
         J     VALANAER                                                         
                                                                                
VALANA30 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         XR    R0,R0                                                            
                                                                                
VALANA35 CLI   RSTEL,RSTELQ                                                     
         JE    VALANA45                                                         
         CLI   RSTEL,0                                                          
         JE    VALANA60                                                         
                                                                                
VALANA40 IC    R0,RSTLN                                                         
         AR    R3,R0                                                            
         J     VALANA35                                                         
                                                                                
VALANA45 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALANA40                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     VALANAER                                                         
                                                                                
VALANA60 CLI   XSWAPQ,YESQ                                                      
         JE    VALANA05                                                         
         J     EXITY                                                            
                                                                                
VALANAER MVC   XERRTXT(14),1(R4)                                                
         J     EXITN                                                            
         DROP  R2,R3                                                            
                                                                                
                                                                                
* Validate currency and supplier currency for order upload                      
VALCUR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13VCUR*'                                                      
                                                                                
         OC    RQOMCUR,SPACES                                                   
         MVC   XORDCUR,RQOMCUR                                                  
         CLC   RQOMCUR,SPACES                                                   
         JNE   VALCUR10                                                         
         MVC   XORDCUR,AGYCURR                                                  
                                                                                
VALCUR10 CLI   XSUPCURR,ASTCANY    OK if all currncies allowed                  
         JE    EXITY                                                            
         CLC   XSUPCURR,XORDCUR    else it must match                           
         JE    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$FCODE)                                           
                                                                                
VALCURER MVC   XERRTXT(3),XORDCUR                                               
         MVI   XERRTXT+3,C'/'                                                   
         MVC   XERRTXT+4(3),XSUPCURR                                            
         CLI   XSUPCURR,ASTCANY                                                 
         JNE   EXITN                                                            
         MVC   XERRTXT+4(3),=C'***'                                             
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Validate estimate no. for order upload                              *         
***********************************************************************         
         SPACE 1                                                                
VALEST   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALEST*'                                                      
                                                                                
         OC    RQOMEST,SPACES                                                   
         MVC   CESTNUM,RQOMEST                                                  
                                                                                
         CLI   ORDTYPE,EXPOQ                                                    
         JNE   VALEST05                                                         
         CLC   RQOMEST,SPACES                                                   
         JE    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$INVOT)                                           
         J     VALESTN                                                          
                                                                                
VALEST05 CLC   RQOMEST,SPACES                                                   
         JH    VALEST10                                                         
         CLI   T.PROEBY12,YESQ                                                  
         JNE   EXITY                                                            
         CLI   RQOMSTA,RQDRAFT                                                  
         JE    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$ENONS)                                           
         J     VALESTN                                                          
                                                                                
VALEST10 MVC   TEMP2+00(06),XCLIC                                               
         MVC   TEMP2+10(06),XPROC                                               
         MVC   TEMP2+20(07),XJOBC                                               
                                                                                
*&&UK                                                                           
         USING EGNPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,CUXCPY                                                   
         MVC   EGNPNUM,RQOMEST                                                  
         MVC   CSVKEY4,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   VALESTN1                                                         
         CLC   EGNPAS(EGNPCLI-EGNPASD),CSVKEY4                                  
         JNE   VALESTN1                                                         
         CLC   EGNPCLI,TEMP2+00                                                 
         JNE   VALESTN1                                                         
         CLC   EGNPPRO,TEMP2+10                                                 
         JNE   VALESTN1                                                         
         CLC   EGNPJOB,TEMP2+20                                                 
         JNE   VALESTN1                                                         
*                                  If Aura only valid estimates                 
*                                  are shown on the front end                   
*                                  therefore no need to validate                
*&&                                status                                       
                                                                                
VALESTY  J     EXITY                                                            
                                                                                
VALESTN1 MVC   ROUERRV,=AL2(AE$INEST)                                           
         J     *+10                                                             
VALESTN2 MVC   ROUERRV,=AL2(AE$INVES)                                           
VALESTN  MVC   XERRTXT(6),RQOMEST                                               
         J     EXITN                                                            
***********************************************************************         
* Validate expense order w/c for order upload                         *         
***********************************************************************         
         SPACE 1                                                                
VALEWC   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13VEWC*'                                                      
                                                                                
         OC    RQOMEOW,SPACES                                                   
         MVC   XEXCPWC,RQOMEOW                                                  
                                                                                
         CLI   ORDTYPE,EXPOQ                                                    
         JNE   EXITY                                                            
                                                                                
         CLC   XEXCPWC,SPACES                                                   
         JH    VALEWC10                                                         
         CLI   T.PROEBY14,C'C'     Is w/c compulsory?                           
         JNE   EXITY                                                            
         MVC   ROUERRV,=AL2(AE$CPJWR)                                           
         J     VALEWCER                                                         
                                                                                
VALEWC10 CLC   XJOBC,SPACES        If set cli/pro/job must be set, too          
         JH    VALEWC15                                                         
         MVC   ROUERRV,=AL2(AE$JBEXA)                                           
         J     VALEWCER                                                         
                                                                                
VALEWC15 CLI   CUCTRY,CTRYGER                                                   
         JNE   VALEWC20                                                         
         CLI   XEXCPWC,C'0'                                                     
         JNL   VALEWC20                                                         
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     VALEWCER                                                         
                                                                                
         USING WCORECD,R2                                                       
VALEWC20 LA    R2,IOKEY            read for work code                           
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUXCPY                                                   
         MVC   WCOKUNT(L'PRODUL),PRODUL                                         
         MVC   WCOKWRK,XEXCPWC                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$WRKNF)                                           
                                                                                
VALEWCER MVC   XERRTXT(2),XEXCPWC                                               
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Cross validation for order upload                                   *         
***********************************************************************         
         SPACE 1                                                                
VCROSS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VCROSS*'                                                      
                                                                                
         CLI   XNWRKST,XFLAPPRD                                                 
         JNE   VCROSS10                                                         
                                                                                
VCROSS05 OI    XORDIND,XOACCHK                                                  
         J     VCROSS30                                                         
                                                                                
VCROSS10 CLI   XNWRKST,XSBMITTD                                                 
         JNE   VCROSS20                                                         
                                                                                
         LA    RE,T.MINPAMT                                                     
         CLI   ORDTYPE,PROOQ                                                    
         JE    VCROSS16                                                         
         LA    RE,T.MINEAMT                                                     
         CLI   ORDTYPE,EXPOQ                                                    
         JE    VCROSS16                                                         
         LA    RE,T.MINIAMT                                                     
         CLI   ORDTYPE,INTOQ                                                    
         JE    VCROSS16                                                         
         LA    RE,T.MINAAMT                                                     
                                                                                
VCROSS16 CP    0(L'MINOAMT,RE),XTOTAMT                                          
         JNH   VCROSS20                                                         
         OI    XORDIND,XOACCHK                                                  
         J     VCROSS30                                                         
                                                                                
VCROSS20 CLI   RQOLCCR,YESQ                                                     
         JNE   VCROSS30                                                         
         OI    XORDIND,XOACCHK+XOESCHK                                          
                                                                                
VCROSS30 TM    XORDIND,XOACCHK     Any code check required?                     
         JZ    VCROSS50                                                         
                                                                                
         CLI   XSUPLLVL,YESQ                                                    
         JE    VCROSS32                                                         
         MVC   ROUERRV,=AL2(AE$MISSU) missing supplier                          
         MVC   XERRTXT(L'ACTKULA),XSUPCULA+1                                    
         J     EXITN                                                            
                                                                                
VCROSS32 CLI   XEXPLLVL,YESQ                                                    
         JE    VCROSS34                                                         
         CLI   ORDTYPE,EXPOQ                                                    
         JNE   VCROSS34                                                         
         MVC   ROUERRV,=AL2(AE$INACP)                                           
         MVC   XERRTXT(L'ACTKULA),XEXPCULA+1                                    
         J     EXITN                                                            
                                                                                
VCROSS34 CLI   ORDTYPE,EXPOQ       Expense order                                
         JE    VCROSS50            Yes                                          
         CLC   XJOBC,SPACES        No - must quote a job code                   
         JH    VCROSS50                                                         
         MVC   ROUERRV,=AL2(AE$MSJOB)                                           
         MVC   XERRTXT(L'ACTKULA),XJOBCULA+1                                    
         J     EXITN                                                            
                                                                                
VCROSS50 CLI   XANAIND,0           Analysis checks                              
         JE    VCROSS70                                                         
         CLI   T.PROEBY10,NOQ      Is analysis checked on order                 
         JE    VCROSS70            No                                           
         CLI   XNWRKST,XFLAPPRD    Are we approving                             
         JE    VCROSS52            Yes                                          
         CLI   XNWRKST,XPRTAPRD                                                 
         JE    VCROSS52            Yes                                          
         CLI   XNWRKST,XSBMITTD    Are we submitting                            
         JNE   VCROSS70            No                                           
         CLI   T.PROEBY10,C'S'     Do we check analysis at submit stage         
         JE    VCROSS54            Yes                                          
         J     VCROSS70            No                                           
                                                                                
VCROSS52 CLI   T.PROEBY10,YESQ     Do we check analysis at approval             
         JNE   VCROSS70            No                                           
                                                                                
VCROSS54 TM    XANAIND,XANAPER     Staff analysis                               
         JZ    VCROSS56                                                         
         OC    XSTACULA,XSTACULA                                                
         JNZ   VCROSS56                                                         
         MVC   ROUERRV,=AL2(AE$ISTFQ)                                           
         J     EXITN                                                            
                                                                                
VCROSS56 TM    XANAIND,XANADEP     Department analysis                          
         JZ    VCROSS58                                                         
         OC    XDEPCULA,XDEPCULA                                                
         JNZ   VCROSS58                                                         
         MVC   ROUERRV,=AL2(AE$IDPTQ)                                           
         J     EXITN                                                            
                                                                                
VCROSS58 TM    XANAIND,XANACST     Costing analysis                             
         JZ    VCROSS70                                                         
         CLC   XCLIC,SPACES        Any client code                              
         JH    VCROSS60                                                         
         MVC   ROUERRV,=AL2(AE$CLEXA)                                           
         J     EXITN                                                            
                                                                                
VCROSS60 DS    0H                                                               
*&&UK*&& J     VCROSS70                                                         
*&&US                                                                           
         TM    SCPYEL+CPYSTAT5-CPYELD,CPYSEXPP                                  
         JZ    VCROSS70                                                         
         CLC   XPROC,SPACES        Any product code                             
         JH    VCROSS70                                                         
         MVC   ROUERRV,=AL2(AE$ICLPQ)                                           
         J     EXITN                                                            
*&&                                                                             
                                                                                
                                                                                
VCROSS70 J     EXITY                                                            
                                                                                
*&&US                                                                           
**********************************************************************          
* Check order date is not earlier than today                         *          
**********************************************************************          
         SPACE 1                                                                
CHKBDO   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKBDO*'                                                      
         L     RF,AGOBLOCB                                                      
         USING GOBLOCKD,RF                                                      
         CLI   GOORDER,C'N'                                                     
         JNE   EXITY               no need to check further                     
         CLC   XORDDT,OR_TODP      Make sure orddate is valid                   
         JNL   EXITY                                                            
         MVC   ROUERRV,=AL2(AE$BDONA)                                           
         J     EXITN                                                            
         DROP  RF                                                               
                                                                                
*&&                                                                             
***********************************************************************         
* Transaction check on reject approved and change approved            *         
***********************************************************************         
         SPACE 1                                                                
TRXCHK   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*TRXCHK*'                                                      
                                                                                
         MVC   TEMP(L'XERRTXT),SPACES                                           
                                                                                
         L     R1,AIO3             get current ID number                        
         LA    R2,ORDRFST-ORDRECD(R1)                                           
         XR    R0,R0                                                            
         XC    FULL1,FULL1                                                      
                                                                                
         USING ORDELD,R2                                                        
TRXCHK02 CLI   ORDEL,ORDELQ                                                     
         JNE   *+8                                                              
         ST    R2,FULL1                                                         
         CLI   ORDEL,OAMELQ                                                     
         JE    TRXCHK06                                                         
         CLI   ORDEL,0                                                          
         JE    TRXCHK10                                                         
TRXCHK04 IC    R0,ORDLN                                                         
         AR    R2,R0                                                            
         J     TRXCHK02                                                         
                                                                                
         USING OAMELD,R2                                                        
TRXCHK06 OC    OAMIPND,OAMIPND     Any pending invoices                         
         JZ    TRXCHK04            No                                           
         MVC   ROUERRV,=AL2(AE$APDIV)                                           
         CLI   XOMSTA,RQCLOSED     Different error for closing                  
         JNE   TRXCHK08                                                         
         MVC   ROUERRV,=AL2(AE$PNDIN)  Yes - display error                      
TRXCHK08 CLI   XOMSTA,RQOPEN       Different error for reopening                
         JNE   EXITN                                                            
         MVC   ROUERRV,=AL2(AE$OCRPI)                                           
         J     EXITN                                                            
                                                                                
         USING TRNRECD,R3                                                       
         USING ORDELD,R2                                                        
TRXCHK10 LA    R3,IOKEY                                                         
         L     R2,FULL1                                                         
         MVC   TRNKEY,SPACES       MAKE KEY SPACES                              
         MVC   TRNKCULA,ORDJOB     ACCOUNT = JOB OR EXPENSE                     
*&&UK                                                                           
         CLC   ORDOJOB,SPACES                                                   
         JNH   *+10                                                             
         MVC   TRNKCULA,ORDOJOB    ALWAYS USE ORIGINAL FOR ORDER TX             
*&&                                                                             
                                                                                
TRXCHK11 CLC   TRNKACT,SPACES                                                   
         JH    *+6                                                              
         DC    H'0'                (BAD ACCOUNT CODE)                           
*&&UK*&& MVC   TRNKCULC,ORDSUP     CONTRA=SUPPLIER                              
*&&US                                                                           
         CLC   PRODUL,ORDACCU      IS IT PRODUCTION LEDGER                      
         JNE   *+10                                                             
         MVC   TRNKCCPY,ORDSUPC    COMPANY ONLY ON PRODUCTION ORDERS            
         MVC   TRNKULC,ORDSUPU     CONTRA=SUPPLIER                              
*&&                                                                             
                                                                                
TRXCHK12 CLC   PRODUL,ORDACCU      IS IT PRODUCTION LEDGER                      
         JNE   *+10                                                             
         MVC   TRNKWORK,=C'**'                                                  
         MVC   TRNKDATE,ORDDATE    TRANSACTION DATE=ORDER DATE                  
         MVC   TRNKREF,ORDKORD-ORDRECD(R1) TRX REFERENCE=ORDER NUMBER           
         MVI   TRNKSBR,0                                                        
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    TRXCHK15                                                         
         TM    XORDIND,XOTXADD                                                  
         JNZ   EXITY                                                            
         MVC   ROUERRV,=AL2(AE$ORTNF)                                           
         J     EXITN                                                            
                                                                                
TRXCHK15 LHI   R1,IOGET+IOMST+IO1                                               
*&&UK                                                                           
         TM    TRNKSTAT,TRNSARCH                                                
         JZ    *+8                                                              
         LHI   R1,IOGET+IOARC+IO1                                               
*&&                                                                             
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                (ABEND HERE)                                 
                                                                                
         USING PTAELD,R4                                                        
         L     R3,AIO1                                                          
         LA    R4,TRNRFST                                                       
         XR    R0,R0                                                            
         ZAP   XTOTNET,PZERO                                                    
         ZAP   XTOTNETF,PZERO                                                   
         ZAP   XTOTCDSC,PZERO                                                   
         ZAP   XTOTCOM,PZERO                                                    
                                                                                
TRXCHK20 CLI   PTAEL,0                                                          
         JE    TRXCHK35                                                         
         CLI   PTAEL,PTAELQ                                                     
         JE    TRXCHK30                                                         
                                                                                
TRXCHK25 IC    R0,PTALN                                                         
         AR    R4,R0                                                            
         J     TRXCHK20                                                         
                                                                                
TRXCHK30 AP    XTOTNET,PTANET      Total up amounts                             
         AP    XTOTNETF,PTANETF                                                 
         AP    XTOTCDSC,PTACDSC                                                 
         AP    XTOTCOM,PTARCOM                                                  
         J     TRXCHK25                                                         
*                                                                               
TRXCHK35 CP    XTOTNET,PZERO       Any amounts?                                 
         JH    TRXCHK40                                                         
         CP    XTOTNETF,PZERO                                                   
         JH    TRXCHK40                                                         
         CP    XTOTCDSC,PZERO                                                   
         JH    TRXCHK40                                                         
         CP    XTOTCOM,PZERO                                                    
         JH    TRXCHK40                                                         
         J     EXITY                                                            
*                                                                               
TRXCHK40 MVC   ROUERRV,=AL2(AE$ISAPA)                                           
         J     EXITN                                                            
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* Check whether any matching exists                                   *         
***********************************************************************         
         SPACE 1                                                                
CHKMAT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKMAT*'                                                      
         USING OAMELD,R2                                                        
         L     R2,AIO3             R2=A(order record)                           
         AHI   R2,ORDRFST-ORDRECD  R2=A(first element)                          
         XR    R0,R0                                                            
                                                                                
CHKMAT02 CLI   OAMEL,OAMELQ        Find order amount element                    
         JE    CHKMAT04                                                         
         CLI   OAMEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,OAMLN            Bump to next element                         
         AR    R2,R0                                                            
         J     CHKMAT02                                                         
                                                                                
         USING OAMELD,R2                                                        
CHKMAT04 OC    OAMIPND,OAMIPND     Any pending invoices                         
         JZ    CHKMAT06            No                                           
         MVC   ROUERRV,=AL2(AE$DPDIV)                                           
         J     EXITN                                                            
CHKMAT06 CP    OAMINUM,PZERO       Any invoicing occurred?                      
         JE    EXITY               No                                           
         MVC   ROUERRV,=AL2(AE$MTDEL)  Yes - error as not allowed               
         J     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Check estimate on order upload                                      *         
***********************************************************************         
         SPACE 1                                                                
CHKEST   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKEST*'                                                      
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
                                                                                
         CLI   GOEWE,GOEERR        Error (no Warning)                           
         JE    CHKES006                                                         
         CLI   GOEWE,GOEWARN       Warning only                                 
         JE    CHKES006                                                         
         CLI   GOEWE,GONONE        estimate checking required?                  
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,GOUESOA                                                       
         LA    R1,4                                                             
CHKES002 CLC   ORDTYPE,0(R2)       Was estimate check done when we              
         JE    CHKES004            got approvers?                               
         LA    R2,1(R2)                                                         
         JCT   R1,CHKES002                                                      
         J     CHKESTY                                                          
                                                                                
CHKES004 CLI   RQOLEST,YESQ        Have we already done this check              
         JE    CHKESTY             Yes - don't repeat it                        
         MVI   GOEWE,GOEERR                                                     
         MVI   GOECE,GOECETA                                                    
         J     CHKES010                                                         
                                                                                
CHKES006 CLI   ORDTYPE,INTOQ       only for internal orders                     
         JE    CHKES010                                                         
         CLI   ORDTYPE,PROOQ       only for production orders                   
         JE    CHKES010                                                         
         CLI   ORDTYPE,ARTOQ       and artist orders                            
         JNE   CHKESTY                                                          
                                                                                
CHKES010 CLI   GOEWE,GOEERR        Error (no Warning)                           
         JE    CHKES012                                                         
         CLI   RQOLEST,YESQ        skip estimate checking?                      
         JE    CHKESTY                                                          
                                                                                
CHKES012 GOTOR GETTUP                                                           
         ZAP   XTOTOE,PZERO                                                     
         ZAP   XTOTCE,PZERO                                                     
         ZAP   XTOTHR,PZERO                                                     
         ZAP   XTOTAM,PZERO                                                     
         ZAP   XTOTOR,PZERO                                                     
                                                                                
                                                                                
         USING ESTCHKD,RF                                                       
         LAY   RF,SVESTWK          set w/c values to estimate table             
         XR    RE,RE                                                            
         IC    RE,XWCNTR                                                        
                                                                                
         USING XWDSCT,R1                                                        
         LAY   R1,XWVALS                                                        
                                                                                
CHKES014 TM    XWCSTAT,XWCEXWC     exclude this workcode?                       
         JNZ   CHKES016                                                         
         MVC   ESTCHWC,XWCODE                                                   
         ZAP   ESTCHOE,PZERO                                                    
         ZAP   ESTCHCE,PZERO                                                    
         ZAP   ESTCHHR,PZERO                                                    
         ZAP   ESTRAMT,PZERO                                                    
         ZAP   ESTCHAM,XWCAMT                                                   
         ZAP   ESTCHOR,PZERO                                                    
         AP    XTOTAM,XWCAMT                                                    
         AHI   RF,ESTCHLQ                                                       
                                                                                
CHKES016 XC    ESTCHWC,ESTCHWC     end of table                                 
         AHI   R1,XWCLNQ                                                        
         JCT   RE,CHKES014                                                      
         DROP  R1,RF                                                            
                                                                                
CHKES018 CLI   GOEORN,GONONE       Check all non memo orders?                   
         JE    CHKES038                                                         
                                                                                
         USING OSJPASD,R2                                                       
         LA    R2,IOKEY            read for other orders to this job            
         XC    OSJPAS,OSJPAS       (doesn't take est # on order into            
         MVI   OSJPTYP,OSJPTYPQ    account)                                     
         MVI   OSJPSUB,OSJPSUBQ    and add amounts                              
         MVC   OSJPCPY,CUXCPY                                                   
         MVC   OSJPACT,XJOBCULA+L'ACTKCPY+L'ACTKUNT+L'ACTKLDG                   
         MVI   OSJPMEM,0                                                        
         MVC   CSVKEY2,OSJPAS                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CHKES022                                                         
         DC    H'0'                                                             
                                                                                
CHKES020 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JE    CHKES022                                                         
         DC    H'0'                                                             
                                                                                
CHKES022 CLC   CSVKEY2(OSJPORD-OSJPASD),OSJPAS                                  
         JNE   CHKES038                                                         
         CLC   OSJPORD,XORDNM                                                   
         JE    CHKES023                                                         
         GOTOR FLTORD,DMCB,OSJPAS  filter orders                                
         JNE   CHKES020                                                         
CHKES023 ZAP   DUB,PZERO                                                        
         MVC   CSVKEY2,OSJPAS                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         USING ORDRECD,R2                                                       
         L     R2,AIO2                                                          
         LA    R4,ORDRFST                                                       
* estimate number filtering                                                     
         CLI   GOECE,GOECSTA                                                    
         JNE   CHKES025                                                         
         CLC   CESTNUM,SPACES                                                   
         JNH   CHKES025                                                         
         USING FFTELD,R4                                                        
CHKES024 LLC   R0,FFTLN                                                         
         AR    R4,R0                                                            
         CLI   FFTEL,0                                                          
         JE    CHKES025                                                         
         CLI   FFTEL,FFTELQ                                                     
         JNE   CHKES024                                                         
         CLI   FFTTYPE,FFTTESTN                                                 
         JNE   CHKES024                                                         
         CLC   CESTNUM,SPACES                                                   
         JNH   CHKES024                                                         
*                                                                               
         LA    RF,FFTESTN                                                       
         CLI   FFTLN,FFTESLNQ      Any original estimate number?                
         JL    CHKES24A            No - OK                                      
         CLC   FFTOESTN,SPACES                                                  
         JNH   CHKES24A                                                         
         LA    RF,FFTOESTN                                                      
*                                                                               
CHKES24A CLC   CESTNUM,0(RF)                                                    
         JNE   CHKES020                                                         
         J     CHKES024                                                         
*                                                                               
* Go round again for costs                                                      
CHKES025 LA    R4,ORDRFST                                                       
         USING OAMELD,R4                                                        
CHKES026 LLC   R0,OAMLN                                                         
         AR    R4,R0                                                            
         CLI   OAMEL,0                                                          
         JE    CHKES032                                                         
         CLI   OAMEL,OAMELQ                                                     
         JNE   CHKES026                                                         
         CLI   CSVKEY2+(OSJPMEM-OSJPASD),0                                      
         JNE   CHKES027                                                         
         CLI   GOWCF,YESQ                                                       
         JNE   CHKES027                                                         
         CLC   OAMWORK,SPACES                                                   
         JNH   CHKES026                                                         
         GOTOR CHKWCEC,DMCB,OAMWORK                                             
         JL    CHKES026                                                         
         JH    CHKESTWN                                                         
                                                                                
CHKES027 CLC   ORDKORD,XORDNM                                                   
         JE    *+10                                                             
         AP    DUB,OAMAMNT                                                      
         SP    DUB,OAMIVAL                                                      
         CLI   CSVKEY2+(OSJPMEM-OSJPASD),0                                      
         JNE   CHKES026                                                         
                                                                                
         USING ESTCHKD,RF                                                       
         LAY   RF,SVESTWK                                                       
                                                                                
CHKES028 OC    ESTCHWC,ESTCHWC     EOT?                                         
         JZ    CHKES026                                                         
         CLC   ESTCHWC,OAMWORK                                                  
         JNE   CHKES030                                                         
         CLC   ORDKORD,XORDNM                                                   
         JE    *+10                                                             
         AP    ESTCHOR,OAMAMNT                                                  
         SP    ESTCHOR,OAMIVAL                                                  
         J     CHKES026                                                         
                                                                                
CHKES030 AHI   RF,ESTCHLQ                                                       
         J     CHKES028                                                         
                                                                                
CHKES032 CLI   CSVKEY2+(OSJPMEM-OSJPASD),0                                      
         JE    CHKES037                                                         
         LA    R4,ORDRFST                                                       
         USING FFTELD,R4                                                        
                                                                                
CHKES034 LLC   R0,FFTLN                                                         
         AR    R4,R0                                                            
         CLI   FFTEL,0                                                          
         JE    CHKES020                                                         
         CLI   FFTEL,FFTELQ                                                     
         JNE   CHKES034                                                         
         CLI   FFTTYPE,FFTTWRKC                                                 
         JNE   CHKES034                                                         
         CLI   GOWCF,YESQ                                                       
         JNE   CHKES035                                                         
         CLC   FFTWORK,SPACES                                                   
         JNH   CHKES020                                                         
         GOTOR CHKWCEC,DMCB,FFTWORK                                             
         JL    CHKES020                                                         
         JH    CHKESTWN                                                         
                                                                                
         USING ESTCHKD,RF                                                       
CHKES035 LAY   RF,SVESTWK                                                       
                                                                                
CHKES036 OC    ESTCHWC,ESTCHWC     EOT?                                         
         JZ    CHKES037                                                         
         CLC   ESTCHWC,FFTWORK                                                  
         JNE   CHKES03A                                                         
         AP    ESTCHOR,DUB                                                      
         J     CHKES037                                                         
         DROP  R4                                                               
                                                                                
CHKES03A AHI   RF,ESTCHLQ                                                       
         J     CHKES036                                                         
                                                                                
CHKES037 AP    XTOTOR,DUB                                                       
         J     CHKES020                                                         
                                                                                
CHKES038 CLI   GOEORM,GONONE       Check all memo orders?                       
         JE    CHKES040                                                         
         CLI   CSVKEY2+(OSJPMEM-OSJPASD),X'FF'                                  
         JE    CHKES040                                                         
         USING OSJPASD,R2                                                       
         LA    R2,IOKEY            read for other orders to this job            
         XC    OSJPAS,OSJPAS       (doesn't take est # on order into            
         MVI   OSJPTYP,OSJPTYPQ    account)                                     
         MVI   OSJPSUB,OSJPSUBQ    and add amounts                              
         MVC   OSJPCPY,CUXCPY                                                   
         MVC   OSJPACT,XJOBCULA+L'ACTKCPY+L'ACTKUNT+L'ACTKLDG                   
         MVI   OSJPMEM,X'FF'                                                    
         MVC   CSVKEY2,OSJPAS                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CHKES022                                                         
         DC    H'0'                                                             
                                                                                
         USING TRNRECD,R2                                                       
CHKES040 LA    R2,IOKEY            read for charges transactions                
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,XJOBCULA                                                
         MVC   TRNKWORK,=C'**'                                                  
         MVI   TRNKCCPY,FF                                                      
         MVC   CSVKEY2,TRNKEY                                                   
                                                                                
CHKES042 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CHKES046                                                         
         DC    H'0'                                                             
                                                                                
CHKES044 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JE    CHKES046                                                         
         DC    H'0'                                                             
                                                                                
CHKES046 CLC   TRNKCULA,CSVKEY2                                                 
         JNE   CHKES068                                                         
         CLC   TRNKWORK,=C'99'                                                  
         JE    CHKES068                                                         
                                                                                
         CLC   TRNKREF,SPACES      Skip other records                           
         JNH   CHKES044                                                         
         TM    TRNKSTAT,TRNSREVS+TRNSDRFT                                       
         JNZ   CHKES044                                                         
                                                                                
         CLI   GOWCF,YESQ                                                       
         JNE   CHKES048                                                         
         MVC   CSVKEY2(L'TRNKEY),TRNKEY                                         
         GOTOR CHKWCEC,DMCB,CSVKEY2+(TRNKWORK-TRNRECD)                          
         JL    CHKES044                                                         
         JH    CHKESTWN                                                         
                                                                                
CHKES048 LHI   R1,IOGET+IOMST+IO2                                               
         TM    TRNKSTAT,TRNSARCH                                                
         JZ    *+8                                                              
         LHI   R1,IOGET+IOARC+IO2                                               
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO2                                                          
         CLC   TRNKULC(L'TRNKCUNT+L'TRNKCLDG),=C'1R'  IS IT TIMESHEETS?         
         JNE   CHKES049                                                         
         CLC   TRNKDATE,SPACES                                                  
         JNH   CHKES049                                                         
         GOTOR FLTTIM,DMCB,AIO2    FILTER TRANSACTIONS                          
         JNE   CHKES044                                                         
                                                                                
CHKES049 LA    R4,TRNRFST                                                       
         USING TRNELD,R4                                                        
         XC    BYTE1,BYTE1                                                      
         ZAP   DUB1,PZERO                                                       
CHKES050 CLI   TRNEL,0                                                          
         JE    CHKES062                                                         
         CLI   TRNEL,TRNELQ                                                     
         JE    CHKES054                                                         
         CLI   TRNEL,SCIELQ                                                     
         JE    CHKES056                                                         
         CLI   TRNEL,FFTELQ                                                     
         JE    CHKES061                                                         
CHKES052 LLC   R0,TRNLN                                                         
         AR    R4,R0                                                            
         J     CHKES050                                                         
                                                                                
CHKES054 TM    TRNSTAT,TRNSDR                                                   
         JZ    CHKES044                                                         
         ZAP   DUB1,TRNAMNT                                                     
         CP    TRNAMNT,PZERO                                                    
         JNE   CHKES052                                                         
         MVI   BYTE1,1             Set a zero transaction                       
         J     CHKES052                                                         
                                                                                
         USING SCIELD,R4                                                        
CHKES056 CLI   SCITYPE,SCITMCRT    Cost amount                                  
         JNE   CHKES058                                                         
         CLI   BYTE1,1                                                          
         JNE   CHKES052                                                         
         CLI   GOCSAT,GOCSCOST                                                  
         JNE   CHKES052                                                         
         ZAP   DUB1,SCIAMNT                                                     
         J     CHKES052                                                         
                                                                                
CHKES058 CLI   SCITYPE,SCITMSRT    Sales amount                                 
         JNE   CHKES060                                                         
         CLI   BYTE1,1             Check it's R time                            
         JNE   CHKES052            otherwise it's billable                      
         CLI   GOCSAT,GOCSSALE                                                  
         JNE   CHKES052                                                         
         AP    DUB1,SCIAMNT                                                     
         J     CHKES052                                                         
*&&UK                                                                           
CHKES060 CLI   SCITYPE,SCITMEXP    Memo expenses (expense invoice)              
*&&                                                                             
*&&US                                                                           
CHKES060 CLI   SCITYPE,SCITSJXP    Memo expenses (expense invoice)              
*&&                                                                             
         JNE   CHKES052                                                         
         CLI   BYTE1,1                                                          
         JNE   CHKES052                                                         
         CLI   GOEMIN,YESQ                                                      
         JNE   CHKES052                                                         
         AP    DUB1,SCIAMNT                                                     
         J     CHKES052                                                         
*                                                                               
         USING FFTELD,R4                                                        
CHKES061 CLI   GOECE,GOECSTA       SINGLE EST FILTER?                           
         JNE   CHKES052                                                         
         CLI   FFTTYPE,FFTTESTN                                                 
         JNE   CHKES052                                                         
         CLC   CESTNUM,SPACES                                                   
         JNH   CHKES052                                                         
*                                                                               
         LA    RF,FFTESTN                                                       
         CLI   FFTLN,FFTESLNQ      Any original estimate number?                
         JL    CHKES61A            No - OK                                      
         CLC   FFTOESTN,SPACES                                                  
         JNH   CHKES61A                                                         
         LA    RF,FFTOESTN                                                      
*                                                                               
CHKES61A CLC   CESTNUM,0(RF)                                                    
         JE    CHKES052            MATCH, CARRY ON                              
         J     CHKES044            SKIP THIS T/X                                
*                                                                               
         USING ESTCHKD,RF                                                       
CHKES062 LAY   RF,SVESTWK                                                       
         AP    XTOTOR,DUB1                                                      
CHKES064 OC    ESTCHWC,ESTCHWC     EOT?                                         
         JZ    CHKES044                                                         
         CLC   ESTCHWC,TRNKWORK                                                 
         JNE   CHKES066                                                         
         AP    ESTCHOR,DUB1                                                     
         J     CHKES044                                                         
                                                                                
CHKES066 AHI   RF,ESTCHLQ                                                       
         J     CHKES064                                                         
         DROP  R2,R4,RF                                                         
                                                                                
CHKES068 LA    R1,X#RTTRX                                                       
         USING TMSTTABD,R1                                                      
         LA    R0,TMSTNUM                                                       
CHKES069 CLI   TMSTMPTY,YESQ       Are interested in in progress TS             
         JE    CHKES06A            Yes                                          
         OC    TMSTSTAT,TMSTSTAT   Are we interested in other statuses          
         JNZ   CHKES06A            Yes                                          
         LA    R1,TMSTTABL(R1)     No - look at next entry                      
         JCT   R0,CHKES069                                                      
         J     CHKES094                                                         
         DROP  R1                                                               
                                                                                
TP       USING TSJPASD,IOKEY                                                    
CHKES06A XC    TP.TSJPAS,TP.TSJPAS                                              
         MVI   TP.TSJPTYP,TSJPTYPQ                                              
         MVI   TP.TSJPSUB,TSJPSUBQ                                              
         MVI   TP.TSJPVIEW,TSJPSJAQ                                             
         MVC   TP.TSJPCPY,CUXCPY                                                
         LLC   RE,PPROLEN                                                       
         LA    R1,XJOBCULA+L'ACTKCPY+L'ACTKLDG+L'ACTKUNT                        
         AR    R1,RE                                                            
         MVC   TP.TSJPMED,0(R1)                                                 
         MVC   TP.TSJPACT,XJOBCULA+L'ACTKCPY+L'ACTKUNT+L'ACTKLDG                
         MVC   TP.TSJPCOFF,XCPJOFF                                              
         MVC   CSVKEY2,TP.TSJPAS                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CHKES072                                                         
                                                                                
CHKES070 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JNE   CHKES094                                                         
                                                                                
CHKES072 CLC   TP.TSJPAS(TSJPPEDT-TSJPASD),CSVKEY2    Match on key?             
         JNE   CHKES094                                                         
                                                                                
         GOTOR FLTTMK,DMCB,TP.TSJPASD                                           
         JNE   CHKES070                                                         
         MVC   CSVKEY2,TP.TSJPASD                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING TIMRECD,R2                                                       
         L     R2,AIO2                                                          
         USING TIMELD,R4                                                        
         LA    R4,TIMRFST                                                       
CHKES074 CLI   TIMEL,0                                                          
         JE    CHKES070                                                         
         CLI   TIMEL,TIMELQ                                                     
         JNE   CHKES076                                                         
         CLI   TIMETYP,TIMEITMS                                                 
         JE    CHKES090                                                         
         CLI   TIMETYP,TIMEINP                                                  
         JE    CHKES078                                                         
                                                                                
CHKES076 LLC   R0,TIMLN            find appropriate TIMEL cluster(s)            
         AR    R4,R0                                                            
         J     CHKES074                                                         
                                                                                
CHKES078 CLC   TIMACC,XJOBCULA+L'ACTKCPY                                        
         JNE   CHKES076                                                         
         CLC   TIMTSK,SPACES       Have we got a workcode                       
         JNH   CHKES076                                                         
         CLI   GOWCF,YESQ                                                       
         JNE   CHKES080                                                         
         GOTOR CHKWCEC,DMCB,TIMTSK Include in estimate checking?                
         JL    CHKES076            No                                           
         JH    CHKESTWN                                                         
CHKES080 GOTOR FLTTMP,DMCB,TIMELD,TP.TSJPASD                                    
         JNE   CHKES076                                                         
         LR    R6,R4                                                            
         ZAP   DUB1,PZERO                                                       
         CLI   TIMTTYP,TIMTCR                                                   
         JNE   CHKES084                                                         
         CLI   GOCSAT,GOCSSALE                                                  
         JNE   CHKES082                                                         
         ZAP   DUB1,TIMRATE        Amount = Rate * Hours                        
         MP    DUB1,TIMHRS                                                      
         SRP   DUB1,64-2,5         / 100 (hours is 2 dec places)                
         J     CHKES086                                                         
                                                                                
CHKES082 DS    0H                                                               
*&&UK                                                                           
         ZAP   DUB1,TIMCRATE       Amount = Rate * Hours                        
         MP    DUB1,TIMHRS                                                      
         SRP   DUB1,64-2,5         / 100 (hours is 2 dec places)                
*&&                                                                             
         J     CHKES086                                                         
                                                                                
CHKES084 CLI   TIMTTYP,TIMTCB                                                   
         JNE   CHKES076                                                         
         ZAP   DUB1,TIMAMNT                                                     
                                                                                
CHKES086 AP    XTOTOR,DUB1                                                      
         USING ESTCHKD,RF                                                       
         LAY   RF,SVESTWK                                                       
                                                                                
         OC    ESTCHWC,ESTCHWC     EOT?                                         
         JZ    CHKES076                                                         
         CLC   ESTCHWC,TIMTSK                                                   
         JNE   CHKES088                                                         
         AP    ESTCHOR,DUB1                                                     
         J     CHKES076                                                         
                                                                                
CHKES088 AHI   RF,ESTCHLQ                                                       
         J     CHKES076                                                         
         DROP  RF                                                               
                                                                                
CHKES090 CLC   TIMIULA,XJOBCULA+L'ACTKCPY                                       
         JNE   CHKES076                                                         
         CLC   TIMITSK,SPACES      Have we got a workcode                       
         JNH   CHKES076                                                         
         CLI   GOWCF,YESQ                                                       
         JNE   CHKES092                                                         
         GOTOR CHKWCEC,DMCB,TIMITSK Include in estimate checking?               
         JL    CHKES076            No                                           
         JH    CHKESTWN                                                         
         GOTOR FLTTMP,DMCB,(R6),TP.TSJPASD                                      
         JNE   CHKES076                                                         
CHKES092 ZAP   DUB1,TIMITOT                                                     
         J     CHKES086                                                         
         DROP  TP,R2                                                            
                                                                                
CHKES094 LA    R6,EXPTYTAB                                                      
         USING EXJPASD,R2                                                       
CHKES096 LA    R2,IOKEY                                                         
         XC    EXJPAS,EXJPAS                                                    
         MVI   EXJPTYP,EXJPTYPQ                                                 
         MVI   EXJPSUB,EXJPSUBQ                                                 
         MVC   EXJPCPY,CUXCPY                                                   
         MVC   EXJPVIEW,0(R6)                                                   
         MVC   EXJPCOFF,XCPJOFF                                                 
         MVC   EXJPCPJ,XJOBCULA+L'ACTKCPY+L'ACTKUNT+L'ACTKLDG                   
         MVC   CSVKEY2,EXJPAS                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CHKES100                                                         
         J     CHKES114                                                         
                                                                                
CHKES098 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JNE   CHKES114                                                         
                                                                                
CHKES100 CLC   EXJPAS(EXJPMED-EXJPASD),CSVKEY2    Match on key?                 
         JNE   CHKES112                                                         
         MVC   CSVKEY2,EXJPASD                                                  
         GOTOR FLTEXK,DMCB,EXJPAS                 filter expense claims         
         JNE   CHKES098                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING EXCRECD,R2                                                       
         L     R2,AIO2                                                          
         LA    R4,EXCRFST                                                       
         USING CIDELD,R4                                                        
         J     CHKES104                                                         
                                                                                
CHKES102 LLC   R0,CIDLN            find appropriate CIDEL cluster(s)            
         AR    R4,R0                                                            
                                                                                
CHKES104 CLI   CIDEL,0                                                          
         JE    CHKES098                                                         
         CLI   CIDEL,CIDELQ                                                     
         JNE   CHKES102                                                         
         CLI   CIDTYPE,CIDTYMQ                                                  
         JNE   CHKES102                                                         
                                                                                
CUR      USING CIDELD,RF                                                        
         LA    RF,CIDELD           filter CIDEL and its cluster                 
         ST    RF,SAVERF                                                        
         CLC   CIDMWCD,SPACES      Have we got a workcode                       
         JNH   CHKES102                                                         
         CLI   GOWCF,YESQ                                                       
         JNE   CHKES106                                                         
         GOTOR CHKWCEC,DMCB,CIDMWCD Include in estimate checking?               
         JL    CHKES102            No                                           
         JH    CHKESTWN                                                         
*                                                                               
CHKES106 L     RF,SAVERF                                                        
         GOTOR FLTEXL,DMCB,CUR.CIDELD,EXCRECD filter expense claims             
         JNE   CHKES102                                                         
         L     RF,SAVERF                                                        
                                                                                
CHKES108 LLC   R0,CUR.CIDLN                                                     
         AR    RF,R0                                                            
         CLI   CUR.CIDEL,CIDELQ                                                 
         JNE   CHKES102                                                         
         CLC   CUR.CIDSEQ,CIDSEQ                                                
         JNE   CHKES102                                                         
         CLI   CUR.CIDTYPE,CIDTYCQ                                              
         JNE   CHKES108                                                         
                                                                                
         CLC   CUR.CIDCCPJ,XJOBCULA+L'ACTKCPY+L'ACTKUNT+L'ACTKLDG               
         JNE   CHKES102            look for this job's items                    
         AP    XTOTOR,CIDMAMT                                                   
         USING ESTCHKD,RF                                                       
         LAY   RF,SVESTWK                                                       
                                                                                
         OC    ESTCHWC,ESTCHWC     EOT?                                         
         JZ    CHKES102                                                         
         CLC   ESTCHWC,CIDMWCD                                                  
         JNE   CHKES110                                                         
         AP    ESTCHOR,CIDMAMT                                                  
         J     CHKES102                                                         
                                                                                
CHKES110 AHI   RF,ESTCHLQ                                                       
         J     CHKES102                                                         
         DROP  R2,CUR                                                           
                                                                                
CHKES112 LA    R6,1(R6)                                                         
         CLI   0(R6),X'FF'                                                      
         JNE   CHKES096                                                         
                                                                                
CHKES114 L     RF,AACCFACS                                                      
         USING X_ACCFACSD,RF                                                    
         GOTO1 X_AACCEMU,DMCB,=C'NEWO',,AIO4,AIO4                               
         ORG   *-2                                                              
         LR    R2,R1               (ACCEMU requires R2=A(DMCB))                 
         BASR  RE,RF               convert job record to emulated               
         DROP  RF                                                               
                                                                                
*&&UK*&& GOTO1 VJOBCOL,DMCB,(X'FF',JOBFLDS),ACOLIST,ACOMFACS                    
*&&US*&& GOTO1 VJOBCOL,DMCB,LOOKFLDH,ACOLIST,ACOMFACS                           
                                                                                
         CLI   4(R1),0                                                          
*&&UK*&& JE    *+6                                                              
*&&US*&& JNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,AJOBLOCK                                                      
         USING JBLOCKD,R3                                                       
         MVC   JBAJOB,AIO4                                                      
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
*&&UK*&& MVC   JBCSHVAL,VCASHVAL                                                
*&&UK*&& MVC   JBTOBACO,VTOBACCO                                                
         MVC   JBAGOBLK,AGOBLOCB                                                
         MVC   JBAIO,AIO1                                                       
         MVC   JBGETOPT,VGETOPT                                                 
         MVC   JBAOPVTB,AELEAREA   use ELEAREA here                             
         LHI   RE,L'ELEAREA                                                     
         ST    RE,JBLOPVTB                                                      
         MVC   JBACOLTB,AGENAXTN                                                
         LHI   RE,L'GENAEXTN                                                    
         ST    RE,JBLCOLTB                                                      
         MVI   JBSELFUN,JBGETDE     MCS: GET DETAILS AND SET ORIGINAL           
*&&UK*&& LA    RE,JOBFLDS                COLUMN LIST  ADDRESS (OE/CE)           
*&&US*&& LA    RE,LOOKFLDH               COLUMN LIST  ADDRESS (OE/CE)           
         ST    RE,JBORICLI                                                      
                                                                                
         USING GOXBLKD,R6                                                       
         L     R6,AGOXBLCK                                                      
                                                                                
         CLI   GOECE,GOECSWC        SINGLE ESTIMATE CHECKING?                   
         JE    CHKES115                                                         
         CLI   GOECE,GOECSTA                                                    
         JNE   CHKES116                                                         
                                                                                
         USING EGNPASD,R1                                                       
CHKES115 LA    R1,IOKEY                                                         
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,CUXCPY                                                   
         MVC   EGNPNUM,CESTNUM                                                  
         MVC   CSVKEY2,IOKEY                                                    
         CLC   CESTNUM,SPACES      Do we have an estimate number                
         JH    *+14                Yes                                          
         MVC   ROUERRV,=AL2(AE$ENONS) No - give error                           
         J     CHKESTWN                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOKEY                                                         
         CLC   EGNPAS(EGNPCLI-EGNPASD),CSVKEY2                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    JBSELFUN,JBGETSS                                                 
         MVC   JBSELEST,EGNPLNO                                                 
         DROP  R1                                                               
                                                                                
CHKES116 GOTO1 VJOBBER,DMCB,AJOBLOCK                                            
         CLI   JBERROR,0                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
CHKES117 CLI   JBNEWEST,JBMCSQ     FOR MCS ESTIMATES                            
         JE    CHKES128                                                         
                                                                                
         L     R4,JBACOLTB         R4=A(COLUMN TABLE)                           
         USING JBCOLD,R4                                                        
         XR    R2,R2                                                            
         ICM   R2,3,JBNROWS        R2=(LOOP COUNTER)                            
         JZ    CHKES140                                                         
         DS    0H                  PUT VALUES TO EST. CHECK TABLE               
                                                                                
CHKES118 CLI   JBCOLTYP,JBCOLTWC   TEST FOR WORK CODE ENTRY                     
         JNE   CHKES126            NO-SKIP IT                                   
*                                                                               
         CLI   GOWCF,YESQ                                                       
         JNE   CHKES120                                                         
         GOTOR CHKWCEC,DMCB,JBCOLWC                                             
         JL    CHKES126                                                         
         JH    CHKESTWN                                                         
*                                                                               
CHKES120 AP    XTOTOE,JBCOLVAL                                                  
         AP    XTOTCE,JBCOLVAL+L'JBCOLVAL(L'JBCOLVAL)                           
         AP    XTOTHR,JBCOLVAL+(2*L'JBCOLVAL)(L'JBCOLVAL)                       
         USING ESTCHKD,R1                                                       
         LAY   R1,SVESTWK                                                       
                                                                                
CHKES122 OC    ESTCHWC,ESTCHWC     TEST FOR END OF TABLE                        
         JZ    CHKES126                                                         
         CLC   JBCOLWC,ESTCHWC     IS WORK CODE ALREADY IN THE LIST?            
         JE    CHKES124            YES - ADD UP SUB WORK CODE AMOUNTS           
         AHI   R1,ESTCHLQ                                                       
         J     CHKES122                                                         
                                                                                
CHKES124 AP    ESTCHOE,JBCOLVAL                                                 
         AP    ESTCHCE,JBCOLVAL+L'JBCOLVAL(L'JBCOLVAL)                          
         AP    ESTCHHR,JBCOLVAL+(2*L'JBCOLVAL)(L'JBCOLVAL)                      
         J     CHKES126                                                         
                                                                                
CHKES126 AH    R4,JBLCOL           Next table entry                             
         JCT   R2,CHKES118                                                      
         J     CHKES140                                                         
         DROP  R1,R4                                                            
*                                                                               
* MCS estimate checking here                                                    
*                                                                               
         USING MJETABD,R4                                                       
CHKES128 L     R4,JBACOLTB         R4=A(COLUMN TABLE)                           
                                                                                
         CLI   GOWCF,C'Y'                                                       
         JNE   CHKES130                                                         
         ZAP   MJETVAL,PZERO                                                    
         ZAP   MJETVAL+L'MJETVAL(L'MJETVAL),PZERO                               
                                                                                
CHKES130 XR    R0,R0               first entry is totals                        
         IC    R0,MJETLEN                                                       
         AR    R4,R0                                                            
         CLI   MJETTYP,MJETTEQ     end of table?                                
         JE    CHKES138                                                         
         CLI   MJETTYP,MJETTWQ     look for work code entries                   
         JNE   CHKES130                                                         
         OC    MJET1RA,MJET1RA     ...but not 1R-lvl entries                    
         JNZ   CHKES130                                                         
                                                                                
         CLI   GOWCF,YESQ                                                       
         JNE   CHKES132                                                         
         GOTOR CHKWCEC,DMCB,MJETWCD Workcode to be included                     
         JE    CHKES132             No                                          
         JH    CHKESTWN                                                         
                                                                                
         ZAP   MJETVAL,PZERO                                                    
         ZAP   MJETVAL+L'MJETVAL(L'MJETVAL),PZERO                               
         ZAP   MJETVAL+(2*L'MJETVAL)(L'MJETVAL),PZERO                           
         J     CHKES130                                                         
                                                                                
CHKES132 AP    XTOTOE,MJETVAL                                                   
         AP    XTOTCE,MJETVAL+L'MJETVAL(L'MJETVAL)                              
         AP    XTOTHR,MJETVAL+(2*L'MJETVAL)(L'MJETVAL)                          
         USING ESTCHKD,R1                                                       
         LAY   R1,SVESTWK                                                       
                                                                                
CHKES134 OC    ESTCHWC,ESTCHWC     TEST FOR END OF TABLE                        
         JZ    CHKES130                                                         
         CLC   MJETWCD,ESTCHWC     IS WORK CODE ALREADY IN THE LIST?            
         JE    CHKES136            YES - ADD UP SUB WORK CODE AMOUNTS           
         AHI   R1,ESTCHLQ                                                       
         J     CHKES134                                                         
                                                                                
CHKES136 AP    ESTCHOE,MJETVAL                                                  
         AP    ESTCHCE,MJETVAL+L'MJETVAL(L'MJETVAL)                             
         AP    ESTCHHR,MJETVAL+(2*L'MJETVAL)(L'MJETVAL)                         
         J     CHKES130                                                         
                                                                                
CHKES138 CLI   GOWCF,YESQ                                                       
         JNE   CHKES140                                                         
         L     R4,JBACOLTB         R4=A(COLUMN TABLE)                           
         ZAP   MJETVAL,XTOTOE                                                   
         ZAP   MJETVAL+L'MJETVAL(L'MJETVAL),XTOTCE                              
         ZAP   MJETVAL+(2*L'MJETVAL)(L'MJETVAL),XTOTHR                          
         DROP  R4                                                               
                                                                                
CHKES140 CLI   GOECE,GOECEESA      Do we want an approved estimate              
         JE    CHKES142            Yes                                          
         CLI   GOECE,GOECEWCA                                                   
         JE    CHKES142            Yes                                          
         CLI   GOECE,GOECETAA                                                   
         JNE   CHKES144            No                                           
                                                                                
CHKES142 CLI   JBHIAPP,0           does approved estimate exist?                
         JNE   CHKES144                                                         
         LHI   R0,OA_NAEQ          No approved estimate                         
         J     CHKESTNX                                                         
*                                                                               
CHKES144 CLI   GOECE,GOECEWC       w/c test?                                    
         JE    CHKES170                                                         
         CLI   GOECE,GOECSWC                                                    
         JE    CHKES170                                                         
         CLI   GOECE,GOECEWCA                                                   
         JE    CHKES170            Yes                                          
                                                                                
         CLI   GOECE,GOECETA       Total test?                                  
         JE    CHKES150                                                         
         CLI   GOECE,GOECSTA                                                    
         JE    CHKES168                                                         
         CLI   GOECE,GOECETAA                                                   
         JE    CHKES168                                                         
         CP    XTOTCE,PZERO        Any estimate figure                          
         JE    CHKESTN             No - error                                   
         J     CHKESTY                                                          
                                                                                
CHKES150 LA    R2,GOUESOA                                                       
         LA    R0,4                                                             
CHKES152 CLC   ORDTYPE,0(R2)       Are we doing new estimate check              
         JE    CHKES154            Yes                                          
         LA    R2,1(R2)                                                         
         JCT   R0,CHKES152                                                      
         J     CHKES168                                                         
                                                                                
CHKES154 CP    XTOTCE,PZERO        Any estimate figure                          
         JH    CHKES156                                                         
         LHI   R0,OA_NCEQ          No current estimate                          
         J     CHKESTNX            No - error                                   
*                                                                               
CHKES156 L     RF,AGOBLOCB                                                      
         USING GOBLOCKD,RF                                                      
         ZAP   X#PL16,XTOTCE                                                    
         MP    X#PL16,GOOVRPER                                                  
         SRP   X#PL16,64-4,5       Calculate current est*maxper                 
         ZAP   XTOTCE,X#PL16       Current estimate incl contingency            
         SP    XTOTCE,XTOTOR       Minus charges                                
         ZAP   XJOBLE,XTOTCE       Remaining amount left to spend               
         SP    XTOTCE,XTOTAM       Minus order value                            
         CP    XTOTCE,PZERO        Do we have any money left                    
         JNL   CHKESTY             Yes                                          
         CLC   JBHIREV,JBCURVER    No - so do we have a higher revision         
         JNH   CHKESTN             No - error order over estimate               
         ZAP   X#PL16,XTOTHR                                                    
         MP    X#PL16,GOOVRPER                                                  
         SRP   X#PL16,64-4,5       Calculate highest rev est*maxper             
         ZAP   XTOTHR,X#PL16       Highest rev estimate incl contingcy          
         SP    XTOTHR,XTOTOR       Minus charges                                
         ZAP   XJOBLE,XTOTHR       Remaining amount left to spend               
         SP    XTOTHR,XTOTAM       Minus order value                            
         CP    XTOTHR,PZERO        Do we have any money left                    
         JNL   CHKESTY                                                          
*                                                                               
         LHI   R0,OA_OHSQ          Order exceeds highest sub est                
         CLI   ORDTYPE,PROOQ                                                    
         JE    CHKESTNX                                                         
         J     CHKESTN             No - error order over estimate               
                                                                                
CHKES168 ZAP   DUB,XTOTCE                                                       
         SP    DUB,XTOTOR                                                       
         ZAP   XJOBLE,DUB          Remaining amount left                        
         CP    XTOTAM,DUB          check totals                                 
         JH    CHKESTN                                                          
         J     CHKESTY                                                          
         DROP  R1                                                               
                                                                                
         USING ESTCHKD,R5                                                       
CHKES170 LAY   R5,SVESTWK                                                       
         OC    ESTCHWC,ESTCHWC     check w/c                                    
         JZ    CHKESTY                                                          
*                                                                               
CHKES171 OC    ESTCHWC,ESTCHWC     check w/c                                    
         JZ    CHKES176                                                         
*                                                                               
         LA    R2,ESTCHKE                                                       
         CLI   GOEWE,GOEWARN       Warn or error                                
         JNE   *+8                                                              
         LA    R2,ESTCHKW                                                       
*                                                                               
         CP    ESTCHCE,PZERO       any estimates                                
         JNE   CHKES172                                                         
         GOTOR SETWCE,(R2)                                                      
         MVI   XESTWYN,YESQ                                                     
*                                                                               
CHKES172 ZAP   DUB,ESTCHCE                                                      
         SP    DUB,ESTCHOR                                                      
         CP    ESTCHAM,DUB         Any amount left?                             
         JNH   CHKES174                                                         
         MVI   XESTWYN,YESQ                                                     
         ZAP   ESTRAMT,DUB         Remaining amount                             
         GOTOR SETWCE,(R2)                                                      
*                                                                               
CHKES174 AHI   R5,ESTCHLQ                                                       
         J     CHKES171                                                         
                                                                                
CHKES176 CLI   XESTWYN,YESQ        Do we have error/warning                     
         JE    EXITL               Yes                                          
                                                                                
CHKESTY  J     EXITY                                                            
                                                                                
         USING EAETABD,RF                                                       
CHKESTN  LA    RF,EAETAB           Lookup correct error code                    
         MVI   XESTWYN,YESQ                                                     
*                                                                               
CHKESTN2 CLI   EAETGOV,FF                                                       
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   GOECE,EAETGOV                                                    
         JE    CHKESTN4                                                         
         AHI   RF,EAETLNQ                                                       
         J     CHKESTN2                                                         
*                                                                               
CHKESTN4 CLI   GOEWE,GOEERR        <warning no error but response>              
         JE    CHKESTN6                                                         
         MVC   XESCWM,EAETERR                                                   
         J     EXITL                                                            
*                                                                               
CHKESTN6 MVC   XESCKE,EAETERR                                                   
         J     EXITL                                                            
         DROP  RF                                                               
                                                                                
CHKESTNX MVI   XESTWYN,YESQ                                                     
         CLI   GOEWE,GOEERR        <warning no error but response>              
         JNE   CHKESTX2                                                         
         STC   R0,XESCKE           Error now                                    
         J     EXITL                                                            
*                                                                               
CHKESTX2 STC   R0,XESCWM           Return warning message                       
         J     EXITL                                                            
                                                                                
CHKESTWN J     EXITH                                                            
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* CHECK IF WC INCLUDED FOR ESTIMATES                                  *         
***********************************************************************         
         USING LP_D,R5                                                          
CHKWCEC  NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*CHKWCEC'                                                      
*                                                                               
         L     R2,0(R1)            R2=A(WORKCODE)                               
         LA    R1,IOKEY                                                         
         USING WCORECD,R1                                                       
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUXCPY                                                   
         MVC   WCOKUNT(L'PRODUL),PRODUL                                         
         MVC   WCOKWRK,0(R2)                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    CHKWC02                                                          
         MVC   ROUERRV,=AL2(AE$WRKNF)                                           
         MVC   XERRTXT(L'WCOKWRK),0(R2)                                         
         J     EXITH                                                            
                                                                                
CHKWC02  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   IOKEY,CSVKEY2                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2' RE-ESTABLISH SEQUENCE         
                                                                                
         L     R1,AIO1                                                          
         LA    R1,WCORFST                                                       
         USING WCOELD,R1                                                        
         XR    R0,R0                                                            
                                                                                
CHKWC04  CLI   WCOEL,WCOELQ        Look for WCO element                         
         JE    CHKWC06                                                          
         CLI   WCOEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                (must exist)                                 
         IC    R0,WCOLN                                                         
         AR    R1,R0                                                            
         J     CHKWC04                                                          
                                                                                
CHKWC06  DS    0H                                                               
*&&UK*&& TM    WCOSTAT3,WCOSESCK                                                
*&&UK*&& JNZ   EXITL                                                            
         J     EXITY                                                            
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
* Get TUP profile and 'translate' filter out based on GOETMB and      *         
* GOETMR (must be called after getopt call)                           *         
***********************************************************************         
GETTUP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETTUP*'                                                      
         USING COBLOCKD,R2                                                      
         USING GOXBLKD,R4                                                       
         L     R2,ACOBLOCK         Get cost allocation profiles                 
         L     R4,AGOXBLCK                                                      
         MVC   ORACCNT,SPACES                                                   
         GOTOR (#CSTPRF,ACSTPRF),DMCB,ORACCNT                                   
*                                                                               
         LA    R3,GOETMR                                                        
         LA    R1,X#RTTRX                                                       
         LA    R0,TMSTNUM          Number of settings                           
         USING COGOTABD,R4                                                      
GETTUP2  LA    R4,COGOTAB                                                       
*                                                                               
GETTUP4  CLC   COGOCAP,COTUP       Find match on COTUP                          
         JNE   GETTUP6             Get next entry in table if no match          
         CLC   COGOGOPT,0(R3)      Find match on getopt setting                 
         JE    GETTUP8                                                          
GETTUP6  LA    R4,COGOTABL(R4)                                                  
         J     GETTUP4                                                          
*                                                                               
GETTUP8  MVC   0(TMSTTABL,R1),COGOMPTY Set whether to read passives             
         LA    R3,1(R3)                                                         
         LA    R1,TMSTTABL(R1)     Point R1 to next type of time                
         JCT   R0,GETTUP2                                                       
*                                                                               
GETTUPX  J     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* Filter order passives according to GOEORN and GOEORM                *         
* On Ntry P1=A(order passive)                                         *         
***********************************************************************         
                                                                                
FLTORD   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*FLTORD*'                                                      
                                                                                
         L     R2,0(R1)                                                         
         USING OSJPASD,R2                                                       
         TM    OSJPSTAT,ORDCLOSE+ORDSFMCH+ORDSLDEL                              
         JNZ   EXITN                                                            
         TM    OSJPSTA2,ORDSOREJ   ignore rejected                              
         JNZ   EXITN                                                            
*                                                                               
         L     R3,AGOXBLCK                                                      
         USING GOXBLKD,R3                                                       
         LA    RF,GOEORN           Order option                                 
         CLI   OSJPMEM,0           Is it memo?                                  
         JE    FLTORD02                                                         
         LA    RF,GOEORM           Yes - use memo order option                  
*                                                                               
FLTORD02 CLI   0(RF),GONONE        none                                         
         JE    EXITN                                                            
         CLI   0(RF),GOINPR        in progress                                  
         JE    EXITY                                                            
         TM    OSJPSTA2,ORDSDRFT   ignore in progress                           
         JNZ   EXITN                                                            
         CLI   0(RF),GOSUB         submitted                                    
         JE    EXITY                                                            
         TM    OSJPSTA2,ORDSSUBM   ignore submitted                             
         JNZ   EXITN                                                            
         CLI   0(RF),GOPAP         part approved                                
         JE    EXITY                                                            
         TM    OSJPSTA2,ORDSPAPP   ignore part approved                         
         JNZ   EXITN                                                            
         CLI   0(RF),GOAPPR        approved                                     
         JE    EXITY                                                            
         DC    H'0'                bad option record                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Filter time records according to lowest Getopt setting              *         
* ON NTRY P1=TSJPASD                                                  *         
***********************************************************************         
                                                                                
FLTTMK   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*FLTTMK*'                                                      
*                                                                               
         USING TSJPASD,R3                                                       
         L     R3,0(R1)            R3=A(Time passive key)                       
         USING TMSTTABD,R2                                                      
         LA    R2,X#RTTRX                                                       
         LA    R1,TMSTNUM                                                       
FLTTMK02 CLI   TMSTMPTY,YESQ       Are we interested in saved timesheet         
         JNE   FLTTMK04            No                                           
         OC    TSJPSTAT,TSJPSTAT   Yes - have we got one                        
         JZ    EXITY               Yes - want this time record                  
FLTTMK04 OC    TMSTSTAT,TMSTSTAT   Are we interested in other statuses          
         JZ    FLTTMK10            No - get next time type                      
         MVC   BYTE1,TMSTSTAT      Yes - do we have a match                     
         NC    BYTE1,TSJPSTAT                                                   
         OC    BYTE1,BYTE1         Any match on the statuses                    
         JZ    FLTTMK10            No                                           
         OC    TMSTXSTA,TMSTXSTA   Yes - do we have an exception status         
         JZ    FLTTMK06            No                                           
         MVC   BYTE1,TMSTXSTA      Yes - check for a match                      
         NC    BYTE1,TSJPSTAT                                                   
         OC    BYTE1,BYTE1         Any match                                    
         JNZ   FLTTMK10            Yes - get next time type                     
FLTTMK06 OC    TMSTCLST,TMSTCLST   Are we interested in client approval         
         JZ    EXITY               No - we want time record                     
         MVC   BYTE1,TMSTCLST      Yes - check it matches                       
         NC    BYTE1,TSJPKSTA                                                   
         OC    BYTE1,BYTE1                                                      
         JNZ   EXITY               Match found accept timel                     
                                                                                
FLTTMK10 LA    R2,TMSTTABL(R2)                                                  
         JCT   R1,FLTTMK02                                                      
         J     EXITN                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER TIME RECORDS ACCORDING TO COTUP AND GOETMB, GOETMR           *         
* ROUTINE #1-FILTER TSJPASD                                           *         
* ON NTRY P1=A(TIMELD)                                                *         
*         P2=A(TSJPASD)                                               *         
***********************************************************************         
                                                                                
FLTTMP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*FLTTMP*'                                                      
                                                                                
         LM    R3,R4,0(R1)         R4=A(TIMELD),R5=A(TSJPAS)                    
                                                                                
         USING TSJPASD,R4                                                       
         USING TIMELD,R3                                                        
*                                                                               
         USING TMSTTABD,R1                                                      
         CLI   TIMTTYP,TIMTCB      Billable time?                               
         JNE   FLTTMP02                                                         
         LA    R1,X#BTTRX                                                       
         J     FLTTMP06                                                         
                                                                                
FLTTMP02 CLI   TIMTTYP,TIMTCR      R type time?                                 
         JNE   FLTTMP04                                                         
         LA    R1,X#RTTRX                                                       
         J     FLTTMP06                                                         
                                                                                
FLTTMP04 CLI   TIMTTYP,TIMTCN      non billable time?                           
         JE    EXITN                                                            
         DC    H'0'                                                             
                                                                                
FLTTMP06 OC    TMSTSTAT,TMSTSTAT   Are we interested in any statuses            
         JNZ   FLTTMP08            Yes                                          
         CLI   TMSTMPTY,NOQ        Are we interested in saved TS                
         JE    EXITN                                                            
*                                                                               
FLTTMP08 CLI   TMSTMPTY,YESQ       Are we interested in saved TS                
         JNE   FLTTMP10            No                                           
         OC    TSJPSTAT,TSJPSTAT   Yes - do we have a saved timesheet           
         JZ    EXITY               Yes - accept time line                       
         OC    TMSTSTAT,TMSTSTAT   No - are we interested in any other          
         JZ    EXITN                            statuses                        
FLTTMP10 MVC   BYTE1,TMSTSTAT                                                   
         NC    BYTE1,TSJPSTAT                                                   
         OC    BYTE1,BYTE1         Any match on the statuses                    
         JZ    EXITN               No - don't want this timel                   
         OC    TMSTXSTA,TMSTXSTA   Yes - do we have an exception status         
         JZ    FLTTMP12            No                                           
         MVC   BYTE1,TMSTXSTA      Yes - check for a match                      
         NC    BYTE1,TSJPSTAT                                                   
         OC    BYTE1,BYTE1                                                      
         JNZ   EXITN               Reject timel if found                        
FLTTMP12 OC    TMSTCLST,TMSTCLST   Are we interested in client approval         
         JZ    EXITY               No                                           
         MVC   BYTE1,TMSTCLST      Yes - check it matches                       
         NC    BYTE1,TSJPKSTA                                                   
         OC    BYTE1,BYTE1                                                      
         JNZ   EXITY               Match found accept timel                     
         J     EXITN               Reject timel                                 
                                                                                
         DROP  R1,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* FILTER TIME RECORDS ACCORDING TO  GOETMB AND GOETMR                 *         
* ROUTINE #2-FILTER TRNRECD                                           *         
* ON NTRY P1=A(TRNRECD)                                               *         
***********************************************************************         
FLTTIM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*FLTTIM*'                                                      
                                                                                
         L     R2,0(R1)                                                         
         USING TRNRECD,R2                                                       
         LA    R4,TRNRFST                                                       
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
*                                                                               
         XR    RF,RF                                                            
         USING TRNELD,R4                                                        
FLTTIM02 CLI   TRNEL,0                                                          
         JE    FLTTIM10                                                         
         CLI   TRNEL,TRNELQ        find time type                               
         JE    FLTTIM06                                                         
         CLI   TRNEL,PRTELQ        check time type                              
         JE    FLTTIM08                                                         
         CLI   TRNEL,TRSELQ                                                     
         JNE   FLTTIM04                                                         
         ST    R4,FULL1                                                         
FLTTIM04 LLC   R0,TRNLN                                                         
         AR    R4,R0                                                            
         J     FLTTIM02                                                         
*                                                                               
FLTTIM06 CP    TRNAMNT,PZERO       Is it billable                               
         JE    FLTTIM04            No                                           
         LA    RF,GOETMB           Billable                                     
         J     FLTTIM04                                                         
*                                                                               
         USING PRTELD,R4                                                        
FLTTIM08 OR    RF,RF                                                            
         JNZ   FLTTIM04                                                         
         LA    RF,GOETMR                                                        
         CP    PRTRATE,PZERO       R type has a rate                            
         JNE   FLTTIM04            Must be R type                               
         J     EXITN               Not interested in N time                     
*                                                                               
         USING TRSELD,R4                                                        
FLTTIM10 L     R4,FULL1                                                         
         CLI   0(RF),GONONE                 want none included?                 
         JE    EXITN                                                            
         CLI   0(RF),GOINPR                 in progress or higher               
         JE    EXITY                                                            
         TM    TRSSTAT4,TRSSSAVT            ignore in progress                  
         JNZ   EXITN                                                            
         TM    TRSSTAT4,TRSSREJT            and rejected                        
         JNZ   EXITN                                                            
         CLI   0(RF),GOSUB                  submitted or higher                 
         JE    EXITY                                                            
         TM    TRSSTAT4,TRSSSUBT            ignore submitted                    
         JNZ   EXITN                                                            
         CLI   0(RF),GOMANPAP               line manager approved               
         JNE   FLTTIM12                                                         
         TM    TRSSTAT4,TRSSMAAP            ignore part approved                
         JNZ   EXITY                                                            
         OC    TRSSTAT4,TRSSTAT4                                                
         JZ    EXITY                                                            
         J     EXITN                                                            
*                                                                               
FLTTIM12 CLI   0(RF),GOCLIPAP      Client approved                              
         JNE   FLTTIM14            No                                           
         TM    TRSSTAT4,TRSSSJAT   Is the time client approved                  
         JNZ   EXITY               Yes                                          
         OC    TRSSTAT4,TRSSTAT4   Or fully approved                            
         JZ    EXITY                                                            
         J     EXITN                                                            
                                                                                
FLTTIM14 TM    TRSSTAT4,TRSSSJAT+TRSSMAAP   ignore part approved                
         JNZ   EXITN                                                            
         CLI   0(RF),GOAPPR                 approved                            
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Filter expenses according to GOEXPN and GOEXPB                      *         
* On ntry P1=EXJPASD                                                  *         
***********************************************************************         
                                                                                
FLTEXK   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*FLTEXK*'                                                      
         L     R2,0(R1)                                                         
         L     R3,AGOXBLCK                                                      
         USING EXJPASD,R2                                                       
         USING GOXBLKD,R3                                                       
*                                                                               
         TM    EXJPSTAT,EXCSLOGD   ignore logically deleted                     
         JNZ   EXITN                                                            
*                                                                               
         CLI   GOEXPN,GOINPR       find lowest level of both settings           
         JE    EXITY                                                            
         CLI   GOEXPB,GOINPR                                                    
         JE    EXITY                                                            
         OC    EXJPSTAT,EXJPSTAT   ignore in progress                           
         JZ    EXITN                                                            
         TM    EXJPSTAT,EXCSREJE   ignore and rejected                          
         JNZ   EXITN                                                            
         CLI   GOEXPN,GOSUB        submitted?                                   
         JE    EXITY                                                            
         CLI   GOEXPB,GOSUB                                                     
         JE    EXITY                                                            
         TM    EXJPSTAT,EXCSSUBM   ignore submitted                             
         JNZ   EXITN                                                            
         CLI   GOEXPN,GOPAP        part approved                                
         JE    EXITY                                                            
         CLI   GOEXPB,GOPAP                                                     
         JE    EXITY                                                            
         TM    EXJPSTAT,EXCSPAPP+EXCSFNTA                                       
         JNZ   EXITN                                                            
         TM    EXJPSTAT,EXCSCOMP                                                
         JZ    EXITN                                                            
         CLI   GOEXPN,GOAPPR       approved                                     
         JE    EXITY                                                            
         CLI   GOEXPB,GOAPPR                                                    
         JE    EXITY                                                            
         DC    H'0'                                                             
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Filter expenses according to GOEXPN AND GOEXPB                      *         
* Filter CIDELDS                                                     *          
* On ntry P1=A(CIDELD)                                                *         
*         P2=A(EXCRECD)                                               *         
***********************************************************************         
FLTEXL   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*FLTEXL*'                                                      
         LM    R2,R3,0(R1)        R2=A(CIDELD),R3=A(EXCRECD)                    
                                                                                
         L     R4,AGOXBLCK                                                      
         USING CIDELD,R2                                                        
         USING GOXBLKD,R4                                                       
         USING EXCRECD,R3                                                       
                                                                                
         CLI   CIDMTYP,C'B'        billable                                     
         JNE   FLTEXL02                                                         
         LA    RF,GOEXPB                                                        
         TM    EXCRSTAT,EXCSCOMP   ignore fully approved - double count         
         JNZ   EXITN                                                            
FLTEXL02 CLI   CIDMTYP,C'N'        non-billable                                 
         JNE   FLTEXL04                                                         
         LA    RF,GOEXPN                                                        
         CLI   GOEMIN,YESQ         Have we already got memo invoice             
         JNE   FLTEXL04            No                                           
         TM    EXCRSTAT,EXCSCOMP   Yes ignore fully approved to avoid           
         JNZ   EXITN                               double counting              
*                                                                               
FLTEXL04 CLI   0(RF),GONONE        none?                                        
         JE    EXITN                                                            
         CLI   0(RF),GOINPR        in progress?                                 
         JE    EXITY                                                            
         OC    EXCRSTAT,EXCRSTAT   ignore in progress                           
         JZ    EXITN                                                            
         TM    EXCRSTAT,EXCSREJE   ignore rejected                              
         JNZ   EXITN                                                            
         CLI   0(RF),GOSUB         submitted?                                   
         JE    EXITY                                                            
         TM    EXCRSTAT,EXCSSUBM   ignore submitted                             
         JNZ   EXITN                                                            
         CLI   0(RF),GOPAP         part approved                                
         JE    EXITY                                                            
         TM    EXCRSTAT,EXCSPAPP+EXCSFNTA  ignore part appr                     
         JNZ   EXITN                                                            
         TM    EXCRSTAT,EXCSCOMP                                                
         JZ    EXITN                                                            
         CLI   0(RF),GOAPPR        approved                                     
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* (Re-)Build main record in AIO3 (Maintenance)                        *         
***********************************************************************         
         SPACE 1                                                                
         USING ORDRECD,R2                                                       
         USING ORDELD,R3                                                        
BLDMOR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDMOR*'                                                      
                                                                                
         XC    XSVORDEL,XSVORDEL   Clear save order element                     
         CLI   XOMACT,RQOMA1Q      On ADD:                                      
         JNE   BLDOR004                                                         
                                                                                
         L     R2,AIO3             point to IO area                             
         LR    RE,R2               clear IO area and build new key              
         LA    RF,IOLENQ                                                        
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUXCPY                                                   
         MVC   ORDKORD,NEWONUM                                                  
         MVC   ORDRLEN,=Y(ORDRFST-ORDKEY+1)                                     
         LA    R3,XSVORDEL                                                      
         MVI   ORDEL,ORDELQ                                                     
         J     BLDOR026                                                         
                                                                                
BLDOR004 DS    0H                  On MAINTENANCE:                              
                                                                                
         MVC   IOKEY,CSVKEY1       reread order record for update               
         L     R1,=AL4(IORDUP+IODIR+IO3)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(IOGETRUP+IOMST+IO3)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO3             point to IO area                             
         XC    ORDRSTA,ORDRSTA     clear status and remove unwanted             
         XR    R0,R0               elements                                     
         LA    R3,ORDRFST                                                       
                                                                                
BLDOR006 CLI   ORDEL,0                                                          
         JE    BLDOR024                                                         
         CLI   ORDEL,RACELQ                                                     
         JNE   BLDOR008                                                         
         CLI   ORDEL+RACTYPE-RACELD,RACTADD                                     
         JE    BLDOR022            keep element                                 
         J     BLDOR020            delete element                               
                                                                                
BLDOR008 CLI   ORDEL,FFTELQ                                                     
         JNE   BLDOR010                                                         
         CLI   ORDEL+FFTTYPE-FFTELD,FFTTORNO                                    
         JE    BLDOR022            keep element                                 
         J     BLDOR020            delete element                               
                                                                                
BLDOR010 DS    0H                                                               
*&&US*&& CLI   ORDEL,TRSELQ                                                     
*&&US*&& JE    BLDOR022            keep element                                 
                                                                                
         CLI   ORDEL,ORDELQ                                                     
         JNE   BLDOR012                                                         
                                                                                
         CLC   XIDOLD,ORDIDNO      record changed in between                    
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XR    R1,R1               save and delete element                      
         IC    R1,ORDLN                                                         
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   XSVORDEL(0),ORDELD                                               
         EX    R1,0(RE)                                                         
         J     BLDOR020                                                         
                                                                                
BLDOR012 CLI   ORDEL,OAMELQ        Order amount element                         
         JNE   BLDOR018                                                         
         USING OAMELD,R3                                                        
         LAY   RE,XWVALS                                                        
         USING XWDSCT,RE                                                        
         XR    R4,R4               Loop counter for w/cs                        
         IC    R4,XWCNTR                                                        
BLDOR014 CLC   OAMWORK,XWCODE      Find match on work code                      
         JE    BLDOR016                                                         
         LA    RE,XWCLNQ(RE)                                                    
         JCT   R4,BLDOR014                                                      
*&&UK*&& J     BLDOR020            Didn't find match - delete                   
*&&US                                                                           
         TM    OAMSTAT,OAMSXTRA    Preserve extra workcodes on order            
         JZ    BLDOR020                                                         
         OI    XWCSTAT,XWCXTRA     In US add this as an addtional               
         MVC   XWCODE,OAMWORK        not to be seen workcode                    
         ZAP   XWCAMT,OAMAMNT                                                   
         LLC   R4,XWCNTR           Increment counter                            
         AHI   R4,1                                                             
         STC   R4,XWCNTR                                                        
*&&                                                                             
BLDOR016 ZAP   XWCINUM,OAMINUM     Extract invoicing to date                    
         ZAP   XWCIVAL,OAMIVAL                                                  
         MVC   XWCIPND,OAMIPND                                                  
         CLC   XORDCUR,AGYCURR     Foreign currency?                            
         JE    BLDOR020                                                         
         CLI   OAMLN,OAMLN3Q       Will be short element if currency            
         JL    BLDOR020                  was previously agency                  
         ZAP   XWCIFCA,OAMFCIVL    Yes - extract currency invoiced              
         J     BLDOR020                                                         
         DROP  R3,RE                                                            
                                                                                
         USING ORDELD,R3                                                        
BLDOR018 CLI   ORDEL,GDAELQ        keep all GDAELDs from order GAP              
         JE    BLDOR022                                                         
                                                                                
BLDOR020 MVI   ORDEL,FF            Delete all other elements                    
                                                                                
BLDOR022 IC    R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     BLDOR006                                                         
                                                                                
BLDOR024 GOTO1 VHELLO,DMCB,(C'D',ACCMST),('FF',ORDRECD),0                       
                                                                                
BLDOR026 LA    R3,XSVORDEL         build status area and add elements           
         MVI   ORDLN,ORDLN4Q                                                    
                                                                                
         DS    0H                  Set ORDEL values:                            
         MVC   ORDJOB,XJOBCULA     Job or Expense account                       
*&&UK                                                                           
         CLI   XOMSTA,RQOPEN                                                    
         JNE   *+10                                                             
         XC    ORDOJOB,ORDOJOB     Forget 'original' a/c on reopen              
*&&                                                                             
         CLI   ORDTYPE,EXPOQ                                                    
         JNE   BLDOR028                                                         
         MVC   ORDEXP,XEXPCULA                                                  
                                                                                
BLDOR028 CLC   ORDSUP,XSUPCULA     Has supplier changed                         
         JE    *+8                                                              
         OI    XORDIND2,XORDISUP   Yes                                          
         MVC   ORDSUP,XSUPCULA     Supplier                                     
         MVC   ORDGSTAT,XGAPSTAT   set GAP status                               
*                                                                               
BLDOR030 MVC   ORDDATE,XORDDT                                                   
*&&UK*&& MVC   ORDAUTH,XOSVAUT                                                  
*&&US*&& MVC   ORDAUTH,REQRPID                                                  
         MVC   ORDAMDT,OR_TODP                                                  
         MVC   ORDRFMT,XORDRFM                                                  
         MVC   ORDOWNER,XORDOWNR                                                
         CLI   XOMACT,RQOMA1Q      Are we adding order                          
         JNE   BLDOR032            No                                           
         MVC   ORDCPID,CCTPID      Yes - creator on add only                    
BLDOR032 OC    ORDCSUBM,ORDCSUBM   Do we have submitted PID                     
         JNZ   BLDOR034            Yes                                          
         TM    XORDIND,XOGTSUB     Are we submitting the order now              
         JZ    BLDOR034            No                                           
         MVC   ORDCSUBM,CCTPID     Yes - save submitter PID                     
                                                                                
BLDOR034 XC    ORDRQBD,ORDRQBD     Clear 'required by' date                     
         OC    XOSVRBD,XOSVRBD                                                  
         JZ    BLDOR036                                                         
         MVC   ORDRQBD,XOSVRBD                                                  
*&&US*&& MVC   ORDDDTE,XOSVRBD                                                  
                                                                                
BLDOR036 MVC   ORDSTAT2,XNOSTA2                                                 
         TM    XNOSTA1,ORDGDRCV                                                 
         JZ    *+8                                                              
         OI    ORDSTAT2,ORDGDRCV                                                
                                                                                
         MVC   ORDAMDT,OR_TODP     Alwqys update order amended date             
                                                                                
         CLI   XOMSTA,RQOPEN       Reopen - then clear matching bit             
         JNE   BLDOR038                                                         
         NI    ORDSTAT,FF-ORDSMNUP                                              
         TM    XOMATST,XFULMAT+XPRTMAT Any matching activity?                   
         JZ    BLDOR038            No                                           
         OI    ORDSTAT,ORDSPART    Mark as part matched                         
                                                                                
BLDOR038 CLI   XOMACT,RQOMA2Q                                                   
         JE    *+12                                                             
         CLI   XOMACT,RQOMA4Q                                                   
         JNE   *+8                                                              
*&&UK*&& NI    ORDSTAT,FF-(ORDSPRTA+ORDSPRTT)                                   
*&&US*&& NI    ORDSTAT3,FF-(ORDSPRTA+ORDSPRTT)                                  
                                                                                
         CLI   XOMACT,RQOMA1Q      If add build status from scratch             
         JNE   BLDOR044                                                         
         MVI   ORDSTAT,ORDSEBUY                                                 
*&&US*&& OI    ORDSTAT,ORDSPOP     All BO orders must use POP                   
*&&UK*&& NI    ORDSTAT,FF-(ORDSPRTA+ORDSPRTT)                                   
*&&US                                                                           
         NI    ORDSTAT3,FF-(ORDSPRTA+ORDSPRTT)                                  
         CLI   ORDTYPE,EXPOQ       Only 1 w/c for expense order                 
         JE    BLDOR042                                                         
         CP    XTOTNWC,=P'5'       > 4 noncommissionable w/cs                   
         JL    BLDOR040                                                         
         OI    ORDSTAT,ORDSTY10    Must use type 10                             
                                                                                
BLDOR040 CP    XTOTCWC,=P'5'       > 4 commissionable w/cs                      
         JL    BLDOR042                                                         
         OI    ORDSTAT,ORDSTY10    Must use type 10                             
                                                                                
BLDOR042 DS    0H                                                               
*&&                                                                             
BLDOR044 XR    RF,RF               If Aura check for N, default is Y            
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ                                                      
         JNE   BLDOR045                                                         
         CLI   XOSVPRA,NOQ                                                      
         JE    BLDOR046                                                         
         J     *+12                                                             
*                                                                               
BLDOR045 CLI   XOSVPRA,YESQ                                                     
         JNE   BLDOR046                                                         
*&&UK*&& OI    ORDSTAT,ORDSPRTA                                                 
*&&US*&& OI    ORDSTAT3,ORDSPRTA                                                
                                                                                
BLDOR046 XR    RF,RF               If Aura default is Y, check for N            
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ                                                      
         JNE   BLDOR047                                                         
         CLI   XOSVPRT,NOQ                                                      
         JE    BLDOR048                                                         
         J     *+12                                                             
*                                                                               
BLDOR047 CLI   XOSVPRT,YESQ                                                     
         JNE   BLDOR048                                                         
*&&UK*&& OI    ORDSTAT,ORDSPRTT                                                 
*&&US*&& OI    ORDSTAT3,ORDSPRTT                                                
                                                                                
BLDOR048 DS    0H                                                               
*&&US                                                                           
         CLI   ORDTYPE,EXPOQ             is this an expense order?              
         JNE   BLDOR050                                                         
         OI    ORDSTAT,ORDSTY10+ORDSPOP  Always Use type 10/POP                 
         CLI   XANAIND,0                 any analysis?                          
         JE    BLDOR050                                                         
         OI    ORDSTAT,ORDSANL           set Analyzed status byte               
                                                                                
BLDOR050 CLC   XATOTXT,SPACES      attention set?                               
         JNH   BLDOR052                                                         
         MVC   ORDATTN,XATOTXT                                                  
                                                                                
BLDOR052 DS    0H                                                               
*&&                                                                             
         MVC   ORDIDNO,XIDNEW                                                   
                                                                                
         CLI   XOMACT,RQOMA2Q      Can't have self approval on maintain         
         JE    BLDOR054                          only change                    
         TM    XAPPIND,XASLFAP     Have we self approval?                       
         JZ    BLDOR054                                                         
         OI    ORDASTA,ORDSTSQ     Set self approval                            
BLDOR054 CLI   XOMSTA,RQAUTO                                                    
         JNE   *+8                                                              
         OI    ORDASTA,ORDSTAQ     Set auto approval                            
                                                                                
         GOTOR ADDELM,DMCB,ORDELD                                               
         GOTOR AUDNEW,DMCB,ORDELD  Store new elements for audit                 
*&&US                                                                           
         CLI   ORDTYPE,EXPOQ       is this an expense order?                    
         JNE   BLDOR062                                                         
         CLC   XORDOFF,SPACES      (if present)                                 
         JE    BLDOR062                                                         
*                                                                               
         USING EXOELD,R3                                                        
         LA    R3,ELEMENT          Expense order analysis element               
         XC    ELEMENT,ELEMENT                                                  
         MVI   EXOEL,EXOELQ                                                     
         MVI   EXOLN,EXOLNQ                                                     
         MVC   EXODOF,XORDOFF      Office is always required                    
*                                                                               
         CLI   XANAIND,0           any analysis checking required?              
         JE    BLDOR060                                                         
         TM    XANAIND,XANACST     are we analyzing by cli/pro?                 
         JZ    BLDOR056                                                         
*                                                                               
         MVC   EXOCLI,XJOBCULA+3                                                
         MVC   EXOPRO,SPACES                                                    
         LA    RE,XJOBCULA+3       point to cli/prod                            
         XR    R1,R1                                                            
         XR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         IC    R1,PPROLEN                                                       
         SR    R1,RF               get level 2 individual length                
         AR    RE,RF                                                            
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   EXOPRO(0),0(RE)     move in dept code                            
         EX    R1,0(RF)                                                         
                                                                                
BLDOR056 TM    XANAIND,XANADEP     are we analyzing by dept?                    
         JZ    BLDOR058                                                         
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,=CL2'2D'                                                  
         GOTOR (#SETLDG,ASETLDG)                                                
         MVC   EXODEP,SPACES       init dept field                              
         LA    RE,XDEPCULA+3       point to off/dept                            
         XR    R1,R1                                                            
         XR    RF,RF                                                            
         IC    RF,LDGAL1                                                        
         IC    R1,LDGAL2                                                        
         SR    R1,RF               get level 2 individual length                
         AR    RE,RF                                                            
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   EXODEP(0),0(RE)     move in dept code                            
         EX    R1,0(RF)                                                         
                                                                                
BLDOR058 TM    XANAIND,XANAPER     are we analyzing by staff?                   
         JZ    BLDOR060                                                         
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,=CL2'2P'                                                  
         GOTOR (#SETLDG,ASETLDG)                                                
         MVC   EXOPER,SPACES       init person field                            
         LA    RE,XSTACULA+3       point to off/dept/staff                      
         XR    R1,R1                                                            
         XR    RF,RF                                                            
         IC    RF,LDGAL2                                                        
         IC    R1,LDGAL3                                                        
         SR    R1,RF               get level 2 individual length                
         AR    RE,RF                                                            
         AHI   R1,-1                                                            
         BASR  RF,0                                                             
         MVC   EXOPER(0),0(RE)     move in person code                          
         EX    R1,0(RF)                                                         
                                                                                
BLDOR060 GOTOR ADDELM,DMCB,EXOELD                                               
                                                                                
BLDOR062 DS    0H                                                               
*&&                                                                             
         CLC   XORDOFF,SPACES                                                   
         JE    BLDOR064                                                         
                                                                                
         USING FFTELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'XORDOFF+1                                        
         MVI   FFTTYPE,FFTTOFFC                                                 
         MVI   FFTDLEN,L'XORDOFF                                                
         MVC   FFTDATA(2),XORDOFF                                               
                                                                                
         GOTOR ADDELM,DMCB,FFTELD  Add new element to main order record         
         GOTOR AUDNEW,DMCB,FFTELD  Store new elements for audit                 
                                                                                
BLDOR064 CLC   CESTNUM,SPACES                                                   
         JE    BLDOR066                                                         
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTESLNQ                                                   
         MVI   FFTTYPE,FFTTESTN                                                 
         MVI   FFTDLEN,12                                                       
*&&UK*&& MVC   FFTESTN,SPACES                                                   
*&&US*&& MVC   FFTCESTN,SPACES                                                  
         MVC   FFTOESTN,CESTNUM                                                 
                                                                                
         GOTOR ADDELM,DMCB,FFTELD  Add new element to main order record         
         GOTOR AUDNEW,DMCB,FFTELD  Store new elements for audit                 
*                                                                               
BLDOR066 DS    0H                                                               
*&&UK                                                                           
         CLC   XEXCPWC,SPACES                                                   
         JNH   BLDOR068                                                         
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'XEXCPWC+1                                        
         MVI   FFTTYPE,FFTTWRKC                                                 
         MVI   FFTDLEN,L'XEXCPWC                                                
         MVC   FFTWORK,XEXCPWC                                                  
*                                                                               
         GOTOR ADDELM,DMCB,FFTELD  Add new element to main order rec            
         GOTOR AUDNEW,DMCB,FFTELD  Store new elements for audit                 
*                                                                               
*&&                                                                             
BLDOR068 OC    NEWRNUM,NEWRNUM                                                  
         JZ    BLDOR070                                                         
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'NEWRNUM+1                                        
         MVI   FFTTYPE,FFTTORNO                                                 
         MVI   FFTDLEN,L'NEWRNUM                                                
         MVC   FFTDATA(L'NEWRNUM),NEWRNUM                                       
                                                                                
         GOTOR ADDELM,DMCB,FFTELD  Add new element to main order rec            
         GOTOR AUDNEW,DMCB,FFTELD  Store new elements for audit                 
                                                                                
BLDOR070 LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'XEXPTYP+1                                        
         MVI   FFTTYPE,FFTTEXTY                                                 
         MVI   FFTDLEN,L'XEXPTYP                                                
         MVC   FFTDATA(L'XEXPTYP),XEXPTYP                                       
*                                                                               
         GOTOR ADDELM,DMCB,FFTELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,FFTELD  Store new elements for audit                 
*                                                                               
         CLC   XATOTXT,SPACES                                                   
         JNH   BLDOR072                                                         
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ                                                     
         XR    RF,RF                                                            
         IC    RF,XATOLEN                                                       
         LA    R1,1(RF)                                                         
         STC   R1,FFTDLEN                                                       
         AHI   R1,FFTLN1Q+1                                                     
         STC   R1,FFTLN                                                         
         MVI   FFTTYPE,FFTTSATN                                                 
         BASR  RE,0                                                             
         MVC   FFTDATA(0),XATOTXT                                               
         EX    R1,0(RE)                                                         
*                                                                               
         GOTOR ADDELM,DMCB,FFTELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,FFTELD  Store new elements for audit                 
*                                                                               
         USING SORELD,R3                                                        
BLDOR072 CLI   ORDTYPE,EXPOQ                                                    
         JNE   BLDOR074                                                         
         CLC   XJOBCULA,SPACES                                                  
         JNH   BLDOR074                                                         
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SOREL,SORELQ                                                     
         MVI   SORLN,SORALNQ                                                    
         MVI   SORSYS,SORSACC                                                   
         MVC   SORAULA,XJOBCULA+1                                               
*                                                                               
         GOTOR ADDELM,DMCB,SORELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,SORELD  Store new elements for audit                 
*                                                                               
         USING SPAELD,R3                                                        
BLDOR074 OC    XDEPCULA,XDEPCULA                                                
         JZ    BLDOR076                                                         
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATDEPT                                                 
         MVC   SPAAULA,XDEPCULA+1                                               
*                                                                               
         GOTOR ADDELM,DMCB,SPAELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,SPAELD  Store new elements for audit                 
*                                                                               
BLDOR076 OC    XSTACULA,XSTACULA                                                
         JZ    BLDOR078                                                         
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATPERS                                                 
         MVC   SPAAULA,XSTACULA+1                                               
*                                                                               
         GOTOR ADDELM,DMCB,SPAELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,SPAELD  Store new elements for audit                 
*                                                                               
BLDOR078 CLC   XORDCUR,AGYCURR     Foreign currency?                            
         JE    BLDOR080                                                         
                                                                                
         USING AFCELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   AFCEL,AFCELQ                                                     
         MVI   AFCLN,AFCLNQ                                                     
         MVC   AFCCURR,XORDCUR                                                  
         ZAP   AFCAMNT,XTOTFCA                                                  
         GOTO1 VHEXIN,DMCB,XOSVEXC,AFCX,14                                      
*                                                                               
         GOTOR ADDELM,DMCB,AFCELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,AFCELD  Store new elements for audit                 
*                                                                               
         USING OAMELD,R3                                                        
BLDOR080 LA    R3,ELEMENT                                                       
         XC    ELSEQ#,ELSEQ#       Sequence the OAMELD elements                 
         CLI   ORDTYPE,EXPOQ                                                    
         JNE   BLDOR084                                                         
         LAY   RE,XWVALS                                                        
         USING XWDSCT,RE                                                        
         XC    ELEMENT,ELEMENT     Expense orders here, else see below          
         MVI   OAMEL,OAMELQ                                                     
*&&UK*&& MVI   OAMLN,OAMLN1Q                                                    
*&&US*&& MVI   OAMLN,OAMLN2Q                                                    
         MVC   OAMWORK,SPACES                                                   
         ZAP   OAMAMNT,XWCAMT                                                   
         ZAP   OAMINUM,XWCINUM                                                  
         ZAP   OAMIVAL,XWCIVAL                                                  
         MVC   OAMIPND,XWCIPND                                                  
*&&US*&& ZAP   OAMTVAL,PZERO                                                    
         CLC   XORDCUR,AGYCURR     Foreign currency?                            
         JE    BLDOR082                                                         
         MVI   OAMLN,OAMLN3Q                                                    
         OI    OAMSTAT,OAMSFCUR                                                 
         ZAP   OAMFCAMT,XWCFCA                                                  
         ZAP   OAMFCIVL,XWCIFCA                                                 
*                                                                               
BLDOR082 GOTOR ADDELM,DMCB,OAMELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,OAMELD  Store new elements for audit                 
         J     BLDOR090                                                         
*                                                                               
BLDOR084 XR    R4,R4               Loop counter for w/cs                        
         IC    R4,XWCNTR                                                        
                                                                                
BLDOR086 XR    R1,R1                                                            
         IC    R1,XWCNTR                                                        
         SR    R1,R4                                                            
         MHI   R1,XWCLNQ                                                        
         LAY   RE,XWVALS                                                        
         AR    RE,R1                                                            
         USING XWDSCT,RE                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   OAMEL,OAMELQ                                                     
*&&UK*&& MVI   OAMLN,OAMLN1Q                                                    
*&&US*&& MVI   OAMLN,OAMLN2Q                                                    
         MVC   OAMWORK,XWCODE                                                   
         ZAP   OAMAMNT,XWCAMT                                                   
         ZAP   OAMINUM,XWCINUM                                                  
         ZAP   OAMIVAL,XWCIVAL                                                  
         MVC   OAMIPND,XWCIPND                                                  
*&&US                                                                           
         ZAP   OAMTVAL,PZERO                                                    
         TM    XWCSTAT,XWCNCOM     Is this w/c non-commissionable?              
         JZ    *+8                                                              
         OI    OAMSTAT,OAMSNOCM    If so, mark it as such                       
         TM    XWCSTAT,XWCMAJ      Is this a major?                             
         JO    *+8                                                              
         OI    OAMSTAT,OAMSMAMI    Off means it's a major - On=minor            
         TM    XWCSTAT,XWCXTRA     Is this an extra workcode?                   
         JZ    *+8                                                              
         OI    OAMSTAT,OAMSXTRA    Set this as an extra workcode                
*&&                                                                             
         CLC   XORDCUR,AGYCURR     Foreign currency?                            
         JE    BLDOR088                                                         
         MVI   OAMLN,OAMLN3Q                                                    
         OI    OAMSTAT,OAMSFCUR                                                 
         ZAP   OAMFCAMT,XWCFCA                                                  
         ZAP   OAMFCIVL,XWCIFCA                                                 
         DROP  RE                                                               
*                                                                               
BLDOR088 GOTOR ADDELM,DMCB,OAMELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,OAMELD  Store new elements for audit                 
*                                                                               
         JCT   R4,BLDOR086         next w/c                                     
                                                                                
BLDOR090 GOTOR ADDPID              PIDEL processing                             
                                                                                
         CLI   XMATLEN,FF          matching description?                        
         JE    BLDOR092                                                         
                                                                                
         USING SCMELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SCMEL,SCMELQ                                                     
         MVI   SCMTYPE,SCMTOMOC                                                 
         MVI   SCMSEQ,0                                                         
         LLC   RF,XMATLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SCMNARR(0),XMATTXT                                               
         EX    RF,0(RE)                                                         
         AHI   RF,SCMLN1Q+1                                                     
         STC   RF,SCMLN                                                         
         GOTOR ADDELM,DMCB,SCMELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,SCMELD  Store new elements for audit                 
*                                                                               
BLDOR092 CLI   XPRILEN,FF          printing description?                        
         JE    BLDOR094                                                         
         CLI   XPRILEN,0                                                        
         JE    BLDOR094                                                         
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SCMEL,SCMELQ                                                     
         MVI   SCMTYPE,SCMTSTND                                                 
         MVI   SCMSEQ,0                                                         
         LLC   RF,XPRILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SCMNARR(0),XPRITXT                                               
         EX    RF,0(RE)                                                         
         AHI   RF,SCMLN1Q+1                                                     
         STC   RF,SCMLN                                                         
         GOTOR ADDELM,DMCB,SCMELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,SCMELD  Store new elements for audit                 
*                                                                               
BLDOR094 CLI   XNAMLN,FF           Order name?                                  
         JE    BLDOR096                                                         
                                                                                
         USING ENMELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ENMEL,ENMELQ                                                     
         LLC   RF,XNAMLN                                                        
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ENMNAME(0),XONAME                                                
         EX    RF,0(RE)                                                         
         AHI   RF,ENMLNQ+1                                                      
         STC   RF,ENMLN                                                         
         GOTOR ADDELM,DMCB,ENMELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,ENMELD  Store new elements for audit                 
*                                                                               
BLDOR096 CLI   XDALNN,FF          Delivery address?                             
         JE    BLDOR098                                                         
         XR    RF,RF                                                            
         IC    RF,XDALNN                                                        
         USING OATELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   OATEL,OATELQ                                                     
         MVI   OATSUB,OATSUB5Q                                                  
         SHI   RF,1                                                             
         STC   RF,OATNUM                                                        
         BASR  RE,0                                                             
         MVC   OATDABOX(0),XDABOX                                               
         EX    RF,0(RE)                                                         
         AHI   RF,OATADD1-OATELD+1                                              
         STC   RF,OATLN                                                         
         GOTOR ADDELM,DMCB,OATELD  Add element to main order record             
         GOTOR AUDNEW,DMCB,OATELD  Store new elements for audit                 
*                                                                               
         USING RACELD,R3                                                        
BLDOR098 LA    R3,ELEMENT          Activity elements                            
         XC    ELEMENT,ELEMENT                                                  
         MVI   RACEL,RACELQ                                                     
         MVI   RACLN,RACLNQ                                                     
         MVI   RACTYPE,RACTCHA                                                  
         CLI   XOMACT,RQOMA1Q                                                   
         JNE   BLDOR100                                                         
         MVI   RACTYPE,RACTADD                                                  
                                                                                
BLDOR100 MVC   RACUSER,CUUSER                                                   
         MVC   RACPERS,CUPASS                                                   
         MVC   RACTERM,CUTERM                                                   
         MVC   RACDATE,OR_TODP                                                  
         MVC   RACTIME,CTIME                                                    
                                                                                
         GOTOR ADDELM,DMCB,RACELD                                               
         GOTOR UPDREJ              Add reject stceld                            
                                                                                
*&&US                                                                           
         CLI   XOMACT,RQOMA1Q      ADD action                                   
         JNE   BLDOR102                                                         
                                                                                
         USING TRSELD,R3                                                        
         LA    R3,ELEMENT          add record activity for the US               
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSDATE,OR_TODC                                                  
                                                                                
         GOTOR ADDELM,DMCB,TRSELD                                               
                                                                                
*&&                                                                             
*        no need to add record activity and pointer element                     
                                                                                
BLDOR102 MVC   ORDRSTAT,XNOSTA1    set order key status area                    
         MVC   ORDROFF,XORDOFF                                                  
         MVC   ORDRSTA2,XNOSTA2                                                 
         MVC   ORDREXTY,XEXPTYP                                                 
         GOTOR SETGAP                                                           
         GOTOR WRTORD                                                           
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Update main record in AIO3 (Status change)                          *         
***********************************************************************         
         SPACE 1                                                                
WRTORD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*WRTORD*'                                                      
         USING ORDRECD,R2                                                       
         CLI   XOMACT,RQOMA1Q      ADD action                                   
         JNE   WRTORD02                                                         
         MVC   CSVKEY1,ORDKEY      save key                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   XORDDA,IODA         save D/A                                     
         J     WRTORD04                                                         
*                                                                               
WRTORD02 GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO3'   Write back rec          
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
DIR      USING ORDRECD,R1                                                       
         LA    R1,IOKEY                                                         
         MVC   IOKEY,CSVKEY1                                                    
         MVC   DIR.ORDKSTA,ORDRSTA                                              
         DROP  DIR                                                              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO3'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING CPTRBLK,R4                                                       
WRTORD04 LA    R4,ELEMENT          add passives                                 
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'A',AIO3),CPTRBLK,XORDDA,0,ACOMFACS               
         J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Update stcel on main order record                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING EL_RECD,R2                                                       
EL       USING STCELD,EL_ELEM                                                   
UPDREJ   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDREJ*'                                                      
*                                                                               
         CLI   XNWRKST,XRJECTED    Has the order been rejected?                 
         JNE   EXITY                                                            
*                                                                               
         LAY   R2,I_AUD                                                         
         XC    EL_REC(EL_KEYL),EL_REC                                           
         GOTOR BLDBST                                                           
         MVC   EL.STCOTOST,XNWRKST Workflow status                              
         MVC   EL.STCOFRST,XOWRKST                                              
         MVI   EL.STCLN,STCOLN3Q   Standard length                              
         CLI   XSTCLN,FF                                                        
         JE    UPDREJ02            Empty                                        
         LLC   RF,XSTCLN                                                        
         SHI   RF,1                                                             
         MVC   EL.STCOCOM(0),XSTCCO  Add comment                                
         EXRL  RF,*-6                                                           
         AHI   RF,STCOLN3Q+1       Set extended length                          
         STC   RF,EL.STCLN                                                      
UPDREJ02 GOTOR ADDELM,DMCB,EL.STCELD                                            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Update main record in AIO3 (Status change)                          *         
***********************************************************************         
         SPACE 1                                                                
UPDMOR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDMOR*'                                                      
                                                                                
         XC    XSVORDEL,XSVORDEL   Clear save order element                     
         MVC   IOKEY,CSVKEY1       reread order record for update               
         L     R1,=AL4(IORDUP+IODIR+IO3)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(IOGETRUP+IOMST+IO3)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING ORDRECD,R2                                                       
         L     R2,AIO3             point to record and get element              
                                                                                
         MVC   ORDRSTAT,XNOSTA1    set new key status values                    
         MVC   ORDRSTA2,XNOSTA2                                                 
         OI    ORDRSTA2,ORDSSTAT   set this is a status change                  
                                                                                
         USING ORDELD,R3                                                        
         LA    R3,ORDRFST                                                       
         XR    R0,R0                                                            
                                                                                
UPDMOR05 CLI   ORDEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ORDEL,ORDELQ                                                     
         JE    UPDMOR10                                                         
         IC    R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     UPDMOR05                                                         
                                                                                
UPDMOR10 XR    R1,R1               Save and delete element                      
         IC    R1,ORDLN                                                         
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   XSVORDEL(0),ORDELD                                               
         EX    R1,0(RE)                                                         
         MVI   ORDEL,FF            Delete all other elements                    
                                                                                
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('FF',ORDRECD),0                       
                                                                                
         LA    R3,XSVORDEL         build status area and add elements           
         MVI   ORDLN,ORDLN4Q                                                    
         MVC   ORDSTAT2,XNOSTA2    set new status on element                    
         OI    ORDSTAT2,ORDSSTAT   set this is a status change                  
                                                                                
         MVC   ORDAMDT,OR_TODP     Alwqys update order amended date             
*&&UK                                                                           
         CLI   XOMSTA,RQOPEN                                                    
         JNE   *+10                                                             
         XC    ORDOJOB,ORDOJOB     Forget 'original' a/c on reopen              
*&&                                                                             
         TM    XNOSTA1,ORDGDRCV                                                 
         JZ    UPDMOR12                                                         
         OI    ORDSTAT2,ORDGDRCV                                                
                                                                                
UPDMOR12 CLI   XOMSTA,RQOPEN       Reopen - then clear matching bit             
         JNE   UPDMOR14                                                         
         NI    ORDSTAT,FF-ORDSMNUP                                              
         TM    XOMATST,XFULMAT+XPRTMAT Any matching activity?                   
         JZ    UPDMOR14            No                                           
         OI    ORDSTAT,ORDSPART    Mark as part matched                         
                                                                                
UPDMOR14 CLC   ORDIDNO,XIDOLD                                                   
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   ORDIDNO,XIDNEW                                                   
                                                                                
         CLI   XOMACT,RQOMA2Q      Can't have self approval on maintain         
         JE    UPDMOR16                          only change                    
         TM    XAPPIND,XASLFAP     Have we self approval?                       
         JZ    UPDMOR16                                                         
         OI    ORDASTA,ORDSTSQ     Set self approval                            
                                                                                
UPDMOR16 CLI   XOMSTA,RQAUTO       Is it auto approval                          
         JNE   *+8                                                              
         OI    ORDASTA,ORDSTAQ     Set auto approval                            
                                                                                
         OC    ORDCSUBM,ORDCSUBM   Do we have submitted PID                     
         JNZ   UPDMOR20            Yes                                          
         TM    XORDIND,XOGTSUB     Are we submitting the order now              
         JZ    UPDMOR20            No                                           
         MVC   ORDCSUBM,CCTPID     Yes - save submitter PID                     
                                                                                
UPDMOR20 GOTOR ADDELM,DMCB,ORDELD                                               
         GOTOR AUDNEW,DMCB,ORDELD                                               
         USING PIDELD,R3                                                        
         XC    ELSEQ#,ELSEQ#       Clear element sequence for OAMELD            
         LA    R3,ORDRFST          remove and add PID elements                  
         XR    R0,R0                                                            
         XC    CESTNUM,CESTNUM                                                  
                                                                                
UPDMOR22 CLI   PIDEL,0                                                          
         JE    UPDMOR46                                                         
         CLI   PIDEL,PIDELQ                                                     
         JE    UPDMOR26                                                         
         CLI   PIDEL,OAMELQ                                                     
         JE    UPDMOR28                                                         
         CLI   PIDEL,FFTELQ                                                     
         JE    UPDMOR30                                                         
         CLI   PIDEL,SORELQ                                                     
         JE    UPDMOR32                                                         
         CLI   PIDEL,SPAELQ                                                     
         JE    UPDMOR32                                                         
         CLI   PIDEL,AFCELQ                                                     
         JE    UPDMOR32                                                         
         CLI   PIDEL,STCELQ                                                     
         JE    UPDMOR26                                                         
UPDMOR24 LLC   RF,PIDLN                                                         
         AR    R3,RF                                                            
         J     UPDMOR22                                                         
                                                                                
UPDMOR26 MVI   PIDEL,FF                                                         
         J     UPDMOR24                                                         
                                                                                
         USING OAMELD,R3                                                        
UPDMOR28 AP    XTOTAMT,OAMAMNT                                                  
         CLI   OAMLN,OAMLN3Q                                                    
         JL    UPDMOR32                                                         
         AP    XTOTFCA,OAMFCAMT                                                 
         J     UPDMOR32                                                         
                                                                                
         USING FFTELD,R3                                                        
UPDMOR30 CLI   FFTTYPE,FFTTESTN                                                 
         JNE   UPDMOR32                                                         
         MVC   CESTNUM,FFTOESTN                                                 
                                                                                
UPDMOR32 GOTOR AUDNEW,DMCB,FFTELD                                               
         J     UPDMOR24                                                         
                                                                                
UPDMOR46 GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',ORDRECD),0                      
         CLI   12(R1),0                                                         
         JE    UPDMOR48                                                         
         DC    H'0'                                                             
                                                                                
UPDMOR48 GOTOR ADDPID                                                           
         GOTOR UPDREJ              Add reject stceld                            
         GOTOR SETGAP              Add GAP status to record                     
                                                                                
*  write back record                                                            
                                                                                
UPDMOR50 GOTOR WRTORD                                                           
         J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Update extension record in AIO2                                     *         
***********************************************************************         
         SPACE 1                                                                
BLDEXT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDEXT*'                                                      
         MVI   BYTE1,NOQ                                                        
         MVI   BYTE2,NOQ                                                        
         LAY   R0,I_NEW            Clear buffer                                 
         LHI   R1,I_NEWL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   XOMACT,RQOMA1Q      ADD action                                   
         JNE   BLDEXT04                                                         
                                                                                
         USING ORDRECD,R2                                                       
BLDEXT02 L     R2,AIO2             clear IO area and build key                  
         LR    RE,R2                                                            
         LHI   RF,IOLENQ                                                        
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   BYTE2,YESQ         Set add extension record                      
*                                                                               
         MVC   ORDKEY,CSVKEY1                                                   
         MVI   ORDKSEQ,ORDKEXTN                                                 
         MVC   ORDRLEN,=Y(ORDRFST-ORDKEY+1)                                     
         J     BLDEXT12                                                         
                                                                                
BLDEXT04 CLI   XOMACT,RQOMA2Q                                                   
         JE    *+12                                                             
         CLI   XOMACT,RQOMA4Q                                                   
         JNE   EXITY                                                            
                                                                                
BLDEXT06 LA    R2,IOKEY            read for extension record                    
         MVC   ORDKEY,CSVKEY1                                                   
         MVI   ORDKSEQ,ORDKEXTN                                                 
                                                                                
         L     R1,=AL4(IORDD+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JE    *+12                                                             
         CLI   IOERR,IOEDEL                                                     
         JNE   BLDEXT02            if not found add it                          
*                                                                               
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JE    *+14                                                             
         CLI   IOERR,IOEDEL                                                     
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO2                                                          
         NI    ORDRSTA,X'FF'-ORDSDEL undelete if deleted                        
         CLI   XOMACT,RQOMA4Q                                                   
         JE    *+12                                                             
         CLI   XOMACT,RQOMA2Q                                                   
         JNE   BLDEXT12                                                         
                                                                                
         USING XDFELD,R3                                                        
         LA    R3,ORDRFST          delete xtra data elements on maint           
                                                                                
BLDEXT08 CLI   XDFEL,0                                                          
         JE    BLDEXT10                                                         
         CLI   XDFEL,XDFELQ                                                     
         JNE   *+8                                                              
         MVI   XDFEL,FF                                                         
*                                                                               
         LLC   R0,XDFLN                                                         
         AR    R3,R0                                                            
         J     BLDEXT08                                                         
                                                                                
BLDEXT10 GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',ORDRECD),0                      
         CLI   12(R1),0                                                         
         JE    BLDEXT12                                                         
         DC    H'0'                                                             
*                                                                               
BLDEXT12 LAY   R6,I_NEW                                                         
         USING EL_RECD,R6                                                       
         MVI   EL_ELCDE,XDFELQ                                                  
         LA    R3,EL_ELEM                                                       
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEN),('BENEWQ',EL_REC)                
         TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   BLDEXT24            Yes - delete any following recs              
         CLI   XDFEL,XDFELQ        any extra data elements?                     
         JNE   BLDEXT24                                                         
*                                                                               
BLDEXT14 MVI   BYTE1,YESQ                                                       
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ORDRECD,XDFELD,(RF)                    
         CLI   12(R1),0                                                         
         JE    BLDEXT18                                                         
         DC    H'0'                                                             
*                                                                               
BLDEXT18 GOTOR BUFELE,DMCB,('TSANXT',TSARELEN),('BENEWQ',EL_REC)                
         TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   BLDEXT20            Yes                                          
         CLI   XDFEL,XDFELQ        any extra data elements?                     
         JE    BLDEXT14            Yes - continue to add to record              
                                                                                
BLDEXT20 CLI   BYTE2,YESQ          ADD action                                   
         JNE   BLDEXT22                                                         
         CLI   BYTE1,YESQ          Any elements added                           
         JNE   EXITY                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO2'                           
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
* write back DA record only                                                     
                                                                                
BLDEXT22 GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         NI    ORDKSTA,X'FF'-ORDSDEL                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
BLDEXT24 CLI   BYTE2,YESQ          ADD action                                   
         JE    EXITY               Nothing to do as no record exists            
         L     R2,AIO2             Delete extension record                      
         OI    ORDRSTA,ORDSDEL                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         OI    ORDKSTAT,ORDSDEL    Set to be deleted                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
* Build order sequential records                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING ORDRECD,R2                                                       
BLDSEQ   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDSEQ*'                                                      
         MVI   BYTE1,0             set initial sequence number                  
         MVI   BYTE4,0                                                          
         USING ORDRECD,R2                                                       
         LAY   R0,I_NEW            Clear buffer                                 
         LHI   R1,I_NEWL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R2,AIO1             build record into clean AIO1                 
         LR    R0,R2                                                            
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   ORDKEY,CSVKEY1                                                   
         XR    RE,RE               increment sequence #                         
         IC    RE,BYTE1                                                         
         AHI   RE,1                                                             
         STC   RE,BYTE1                                                         
         MVC   ORDKSEQ,BYTE1                                                    
         MVC   ORDRLEN,=Y(ORDRFST-ORDKEY+1)                                     
                                                                                
         LAY   R6,I_NEW                                                         
         USING EL_RECD,R6                                                       
         MVI   EL_ELCDE,SCMELQ                                                  
         USING SCMELD,R4                                                        
         LA    R4,EL_ELEM                                                       
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEN),('BENEWQ',EL_REC)                
         J     BLDSEQ04                                                         
                                                                                
BLDSEQ02 GOTOR BUFELE,DMCB,('TSANXT',TSARELEN),('BENEWQ',EL_REC)                
BLDSEQ04 TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   BLDSEQ12            Yes - delete any following recs              
         CLI   SCMEL,SCMELQ        any standard comment elements?               
         JNE   BLDSEQ06                                                         
         CLI   SCMTYPE,SCMTOMOC    Matching description                         
         JE    BLDSEQ02            Added to main order so skip here             
         CLI   SCMTYPE,SCMTSTND    Printing description                         
         JE    BLDSEQ02            Added to main order so skip here             
         J     BLDSEQ08                                                         
                                                                                
BLDSEQ06 CLI   SCMEL,ARTELQ        any article/item elements?                   
         JE    BLDSEQ08                                                         
         CLI   SCMEL,ATXELQ        any article/item text elements?              
         JNE   BLDSEQ02            No - get next element from buffer            
                                                                                
BLDSEQ08 LLC   RF,SCMLN                                                         
         XR    R0,R0                                                            
         ICM   R0,B'0011',ORDRLEN  L'order record                               
         AR    R0,RF               New rec length                               
         CHI   R0,MAXRECLN                                                      
         JH    BLDSEQ14                                                         
BLDSEQ10 LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ORDRECD,SCMELD,(RF)                    
         CLI   12(R1),0                                                         
         JE    BLDSEQ02            Get next element                             
         DC    H'0'                                                             
                                                                                
BLDSEQ12 CLC   ORDRLEN,=Y(ORDRFST-ORDKEY+1)                                     
         JE    BLDSEQ22            No data on record                            
         MVI   BYTE4,YESQ          Set last time                                
                                                                                
BLDSEQ14 XC    IOKEY,IOKEY         Read to see if record is on file             
         MVC   IOKEY(L'ORDKEY),ORDRECD                                          
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    BLDSEQ16                                                         
         CLI   IOERR,IOEDEL        If deleted it still exists                   
         JE    BLDSEQ16                                                         
                                                                                
* Not found need to add                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO1'   Add record              
         JE    BLDSEQ18                                                         
         DC    H'0'                                                             
                                                                                
BLDSEQ16 L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R0,AIO2             Copy record to IO2                           
         L     RE,AIO1                                                          
         LA    R1,IODDWQ                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         NI    IOKEY+ORDKSTAT-ORDRECD,FF-ORDSDEL   ensure not deleted           
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
BLDSEQ18 L     R2,AIO1             build record into clean AIO1                 
         LR    R0,R2                                                            
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   ORDKEY,CSVKEY1                                                   
         XR    RE,RE               increment sequence #                         
         IC    RE,BYTE1                                                         
         AHI   RE,1                                                             
         STC   RE,BYTE1                                                         
         MVC   ORDKSEQ,BYTE1                                                    
         MVC   ORDRLEN,=Y(ORDRFST-ORDKEY+1)                                     
*                                                                               
BLDSEQ20 CLI   BYTE4,YESQ          Was it last time?                            
         JNE   BLDSEQ10            No                                           
*                                                                               
BLDSEQ22 LA    R2,IOKEY                                                         
         MVC   ORDKEY,CSVKEY1      get key of main record                       
*        XR    RE,RE               increment sequence #                         
*        IC    RE,BYTE1                                                         
*        AHI   RE,1                                                             
*        STC   RE,BYTE1                                                         
         MVC   ORDKSEQ,BYTE1                                                    
                                                                                
         L     R1,=AL4(IOHIUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         J     BLDSEQ26                                                         
                                                                                
BLDSEQ24 L     R1,=AL4(IOSQUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
                                                                                
BLDSEQ26 CLC   ORDKEY(ORDKSEQ-ORDRECD),CSVKEY1                                  
         JNE   EXITY                                                            
         CLI   ORDKSEQ,ORDKEXTN    skip extension record                        
         JE    EXITY                                                            
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         OI    ORDRSTAT,ORDSDEL    delete DA file record                        
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,IOKEY                                                         
         OI    ORDKSTAT,ORDSDEL    delete IS file record                        
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         JE    BLDSEQ24                                                         
         DC    H'0'                                                             
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* Build audit record in AIO2                                          *         
* AIO3 has main order record                                          *         
* BYTE1 holds sequence number                                         *         
* BYTE2 says whether building new records or not                      *         
***********************************************************************         
         SPACE 1                                                                
BLDAUD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDAUD*'                                                      
         USING ORDRECD,R3                                                       
         L     R3,AIO3             R3=A(Order record)                           
         LAY   R4,I_AUD                                                         
         USING EL_RECD,R4                                                       
         XC    EL_REC(EL_KEYL),EL_REC                                           
EL       USING STCELD,EL_ELEM                                                   
         XC    ELSEQ#,ELSEQ#                                                    
         GOTOR BLGSTC              build status elements for GAP                
         GOTOR BLDSTC              build status elements in element             
                                                                                
BLDAUD04 XC    EL_REC(EL_KEYL),EL_REC                                           
         GOTOR BUFELE,DMCB,('TSARDH',TSARSTCL),('BEAUDQ',EL_REC)                
         TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   BLDAUD30            Yes - nothing to update on audit             
         CLI   EL.STCEL,STCELQ     Check element is as expected                 
         JNE   BLDAUD30            No - nothing to update on audit              
         XC    BYTE1,BYTE1                                                      
         MVI   BYTE2,NOQ                                                        
         USING AUDRECD,R2                                                       
         LA    R2,IOKEY            build key of audit record                    
         XC    IOKEY,IOKEY                                                      
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,CUXCPY                                                   
         MVI   AUDKAUDT,AUDKORD                                                 
         MVC   AUDKORDN,ORDKORD                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    BLDAUD08                                                         
*                                                                               
BLDAUD06 L     R2,AIO2             clear aio area and add new audit             
         LR    R0,R2                                                            
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,CUXCPY                                                   
         MVI   AUDKAUDT,AUDKORD                                                 
         MVC   AUDKORDN,ORDKORD                                                 
         MVC   AUDRSTAT,ORDRSTAT                                                
         MVC   AUDRSTA2,ORDRSTA2                                                
         MVC   AUDKSEQ,BYTE1                                                    
         LHI   R0,AUDRFST-AUDRECD                                               
         STCM  R0,3,AUDRLEN                                                     
         MVI   BYTE2,YESQ                                                       
         J     BLDAUD10                                                         
*                                                                               
BLDAUD08 MVC   BYTE1,AUDKSEQ       Save current sequence number                 
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         MVC   AUDRSTAT,ORDRSTAT   Set new order status                         
         MVC   AUDRSTA2,ORDRSTA2                                                
*                                                                               
BLDAUD10 GOTOR TSTFIT              will it fit?                                 
         JH    BLDAUD12                                                         
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,EL.STCELD,(RF)                 
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOR BUFELE,DMCB,('TSANXT',TSARSTCL),('BEAUDQ',EL_REC)                
         TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   BLDAUD18            Yes - no more elements to add                
         CLI   EL.STCEL,STCELQ     Is it an audit element                       
         JE    BLDAUD10            No - no more element to add                  
         J     BLDAUD18                                                         
*                                                                               
BLDAUD12 GOTOR SETDTE              Set low and high date on audit rec           
         CLI   BYTE2,YESQ          Are we adding a new record?                  
         JNE   BLDAUD14                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO2'                           
         JE    BLDAUD16                                                         
         DC    H'0'                                                             
*                                                                               
BLDAUD14 GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO2             Update directory status too                  
         LA    R2,IOKEY                                                         
         MVC   AUDKSTA,AUDRSTA-AUDRECD(RF)                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    *+6                                                              
         DC    H'0'                                                             
*                                  Read again for sequential                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    BLDAUD16                                                         
         DC    H'0'                                                             
*                                                                               
BLDAUD16 LLC   RF,BYTE1            Read next audit record                       
         AHI   RF,1                                                             
         STC   RF,BYTE1                                                         
         MVI   BYTE2,NOQ                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         LA    R2,IOKEY                                                         
         CLC   AUDKEY(AUDKSEQ-AUDRECD),IOKEYSAV                                 
         JE    BLDAUD08            Process next record                          
         J     BLDAUD06                                                         
*                                                                               
BLDAUD18 CLI   BYTE2,YESQ          Are we adding a new record?                  
         JNE   BLDAUD20                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO2'                           
         JE    BLDAUDX                                                          
         DC    H'0'                                                             
*                                                                               
BLDAUD20 GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO2             Update directory status too                  
         LA    R2,IOKEY                                                         
         MVC   AUDKSTA,AUDRSTA-AUDRECD(RF)                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    BLDAUDX                                                          
         DC    H'0'                                                             
*                                                                               
* No audit elements but record is updated as Aura warehouse extract             
* process relies on the audit record being present on recovery                  
*                                                                               
         USING AUDRECD,R2                                                       
BLDAUD30 LA    R2,IOKEY            build key of audit record                    
         XC    IOKEY,IOKEY                                                      
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,CUXCPY                                                   
         MVI   AUDKAUDT,AUDKORD                                                 
         MVC   AUDKORDN,ORDKORD                                                 
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         MVC   AUDRSTAT,ORDRSTAT   Set new order status                         
         MVC   AUDRSTA2,ORDRSTA2                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO2             Update directory status too                  
         LA    R2,IOKEY                                                         
         MVC   AUDKSTA,AUDRSTA-AUDRECD(RF)                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
BLDAUDX  J     EXITY                                                            
         DROP  EL,R2,R3,R4                                                      
         EJECT                                                                  
***********************************************************************         
* Test element will fit on audit record                               *         
***********************************************************************         
         SPACE 1                                                                
         USING AUDRECD,R2                                                       
TSTFIT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*TSTFIT*'                                                      
         LAY   R4,I_AUD                                                         
         USING EL_RECD,R4                                                       
         XC    EL_REC(EL_KEYL),EL_REC                                           
EL       USING STCELD,EL_ELEM                                                   
         XR    R0,R0                                                            
         ICM   R0,3,AUDRLEN                                                     
         LLC   RF,EL.STCLN                                                      
         AR    R0,RF                                                            
*                                                                               
TSTFITX  CHI   R0,MAXRECLN         Set condition code                           
         J     EXIT                                                             
         DROP  EL,R2,R4                                                         
         EJECT                                                                  
***********************************************************************         
* Set date on audit record                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING AUDRECD,R2                                                       
SETDTE   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETDTE*'                                                      
         XC    FULL2,FULL2                                                      
         MVC   FULL1,EFFS                                                       
         LA    RF,AUDRFST                                                       
                                                                                
         USING STCELD,RF                                                        
SETDTE02 CLI   STCEL,0                                                          
         JE    SETDTE08                                                         
         CLI   STCEL,STCELQ                                                     
         JE    SETDTE06                                                         
SETDTE04 LLC   R0,STCLN                                                         
         AR    RF,R0                                                            
         J     SETDTE02                                                         
                                                                                
SETDTE06 CLC   FULL2,STCDATE                                                    
         JNL   *+10                                                             
         MVC   FULL2,STCDATE                                                    
         CLC   FULL1,STCDATE                                                    
         JNH   *+10                                                             
         MVC   FULL1,STCDATE                                                    
         J     SETDTE04                                                         
                                                                                
SETDTE08 GOTOR VDATCON,DMCB,(1,FULL1),(2,AUDRSTDT)                              
         GOTOR VDATCON,DMCB,(1,FULL2),(2,AUDRENDT)                              
         J     EXITY                                                            
         DROP  RF,R2                                                            
         EJECT                                                                  
***********************************************************************         
* Compare old and new element buffers and create audit elements       *         
* Audit elements are then added to buffer                             *         
***********************************************************************         
         SPACE 1                                                                
BLDSTC   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDSTC*'                                                      
                                                                                
         LAY   R6,I_NEW                                                         
NEW      USING EL_RECD,R6                                                       
         XC    NEW.EL_REC(EL_KEYL),NEW.EL_REC                                   
*                                                                               
         LAY   R2,I_AUD                                                         
         USING EL_RECD,R2                                                       
         XC    EL_REC(EL_KEYL),EL_REC                                           
EL       USING STCELD,EL_ELEM                                                   
         GOTOR BLDBST              Build base STCELD                            
         CLI   XOMACT,RQOMA3Q      Status change                                
         JNE   BLDST002                                                         
         CLI   XOMSTA,RQRECVD      Goods received needs to build audit          
         JE    BLDST002                            for order change             
         CLC   XOWRKST,XNWRKST     Any difference in workflow status            
         JE    BLDST092            No - don't build change order STCELD         
                                                                                
BLDST002 MVI   EL.STCOTYP,STCOADDQ                                              
         CLI   XOMACT,RQOMA1Q      Are we adding a new order                    
         JE    *+8                 Yes                                          
         MVI   EL.STCOTYP,STCOCHGQ                                              
*                                                                               
         XR    RF,RF               If Aura check aura flag                      
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ         Set BrandOcean or Aura application           
         JNE   *+8                                                              
         OI    EL.STCOTYP,STCOAURQ                                              
*                                                                               
         CHI   RF,XPMOBILQ         Set Mobile application                       
         JNE   *+8                                                              
         OI    EL.STCOTYP,STCOMOBQ                                              
*                                                                               
         MVC   EL.STCOTOST,XNWRKST Workflow status                              
         MVC   EL.STCOFRST,XOWRKST                                              
         MVC   EL.STCOCURC,AGYCURR                                              
         ZAP   EL.STCOAMT,XTOTAMT  Store total order amount                     
         ZAP   EL.STCOWCAM,XTOTAMT Store workcode amount - for exp ords         
         ZAP   EL.STCOFCAM,XTOTFCA                                              
                                                                                
         CP    XTOTAMT,XOTOTAMT    Are old and new amounts different            
         JE    BLDST004                                                         
         OI    EL.STCOIND2,STCOAMTQ Set amount has changed                      
BLDST004 MVC   BYTE1,XOOSTA1       Old order status byte 1                      
         MVC   BYTE2,XNOSTA1       New order status byte 1                      
         NI    BYTE1,ORDGDRCV      n to goods received                          
         NI    BYTE2,ORDGDRCV                                                   
         CLC   BYTE2,BYTE1         If different it's changed                    
         JE    BLDST012                                                         
         OI    EL.STCOIND4,STCOGDRQ Set good received has changed               
BLDST012 TM    BYTE2,ORDGDRCV      Does new order status have it                
         JZ    BLDST014            No                                           
         OI    EL.STCOSTAT,STCOGDRD Yes - Set goods received                    
                                                                                
BLDST014 LA    R3,AUDTAB1                                                       
         USING AUDTABD,R3                                                       
                                                                                
BLDST016 TM    AUDDTIND,AUDNSTON   Audit not required on status only            
         JZ    BLDST018                                                         
         CLI   XOMACT,RQOMA3Q      Status only change                           
         JE    BLDST044            Skip this entry                              
BLDST018 LHI   RF,L'AUDORDTY       Number of possible order types               
         LA    RE,AUDORDTY                                                      
BLDST020 CLC   XOMTYP,0(RE)        Does audit type match order type             
         JE    BLDST022            Yes                                          
         LA    RE,1(RE)            Try next entry                               
         JCT   RF,BLDST020                                                      
         J     BLDST044            No match skip to next audit entry            
                                                                                
BLDST022 LAY   R6,I_NEW                                                         
NEW      USING EL_RECD,R6                                                       
         XC    NEW.EL_REC(EL_KEYL),NEW.EL_REC                                   
NEL      USING ORDELD,NEW.EL_ELEM                                               
         MVC   NEW.EL_ELCDE,AUDELCDE  Set element code to read                  
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEN),('BENEWQ',NEW.EL_REC)            
         J     BLDST026                                                         
*                                                                               
BLDST024 GOTOR BUFELE,DMCB,('TSANXT',TSARELEN),('BENEWQ',NEW.EL_REC)            
BLDST026 TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   BLDST044            Yes - get next element code from tab         
         CLC   NEL.ORDEL,AUDELCDE  Does it match the code                       
         JNE   BLDST044            No - read next entry in table                
BLDST028 OC    AUDELDTY,AUDELDTY   Do we have to check the sub type             
         JZ    BLDST030            No                                           
         LLC   RF,AUDELDTY         Yes - RF=displacement to sub type            
         LA    RE,NEL.ORDEL                                                     
         AR    RE,RF               RE=A(element type field)                     
         CLC   AUDELTYP,0(RE)                                                   
         JNE   BLDST024            No match - read for next element             
                                                                                
BLDST030 XR    RF,RF                                                            
         ICM   RF,1,AUDDSSTC       Any data to be on audit                      
         JZ    BLDST032            No                                           
         LLC   R1,AUDSTCLN         R1=length of data to be extracted            
         LA    RE,EL.STCEL(RF)     RE=A(audit data field)                       
         LLC   RF,AUDDSELM                                                      
         SHI   R1,1                                                             
         TM    AUDDTIND,AUDPREV    Check for previous data                      
         JZ    BLDST031                                                         
         CLC   0(0,RE),SPACES      Any present?                                 
         EXRL  R1,*-6                                                           
         JH    BLDST044            Yes - Skip to next audit entry               
                                                                                
BLDST031 LA    R5,NEL.ORDEL(RF)    R5=A(data field on element)                  
         MVC   0(0,RE),0(R5)       Extract data to audit                        
         EXRL  R1,*-6                                                           
         TM    AUDDTIND,AUDOCHRS                                                
         JZ    BLDST032                                                         
         OC    0(0,RE),SPACES      Ensure empty bytes space filled              
         EXRL  R1,*-6                                                           
                                                                                
BLDST032 LAY   R4,I_OLD                                                         
OLD      USING EL_RECD,R4                                                       
         XC    OLD.EL_REC(EL_KEYL),OLD.EL_REC                                   
OEL      USING ORDELD,OLD.EL_ELEM                                               
         MVC   OLD.EL_REC(EL_KEYL),NEW.EL_REC                                   
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEO),('BEOLDQ',OLD.EL_REC)            
         CLI   TSAERR,0                                                         
         JNE   BLDST036            Not found must be a new element              
         CLC   NEW.EL_REC(EL_KEYL1),OLD.EL_REC Is it the same element           
         JNE   BLDST036            No                                           
         OI    OLD.EL_ELSTA,EL_EXSTS  Set we found record on old buffer         
         GOTOR BUFELE,DMCB,('TSAWRT',TSARELEO),('BEOLDQ',OLD.EL_REC)            
         LLC   RF,OEL.ORDLN                                                     
         LA    RF,OEL.ORDELD(RF)   RF=A(End of old element)                     
         ST    RF,XOENDELE                                                      
         LLC   RF,NEL.ORDLN                                                     
         LA    RF,NEL.ORDELD(RF)   RF=A(End of new element)                     
         ST    RF,XNENDELE                                                      
         TM    AUDDTIND,AUDDTVAR+AUDSUBEL Is data variable or sub els           
         JZ    BLDST034            No                                           
         CLC   NEL.ORDLN,OEL.ORDLN Yes - check if length same                   
         JNE   BLDST036            No - data must be different                  
         TM    AUDDTIND,AUDDTVAR   Is the data variable length                  
         JZ    BLDST034            No                                           
         LLC   RF,NEL.ORDLN                                                     
         SHI   RF,1                                                             
         CLC   NEL.ORDEL(0),OEL.ORDEL                                           
         EXRL  RF,*-6                                                           
         JNE   BLDST036                                                         
         J     BLDST038                                                         
                                                                                
BLDST034 LLC   R1,AUDSTCLN                                                      
         LLC   RF,AUDDSELM                                                      
         LA    RE,NEL.ORDEL(RF)    R5=A(data field on new element)              
         CLM   RE,15,XNENDELE                                                   
         JNL   BLDST040                                                         
         LA    R5,OEL.ORDEL(RF)    R5=A(data field on old element)              
         SHI   R1,1                R1=length of data                            
         CLC   0(0,RE),0(R5)       Compare data between old and new             
         EXRL  R1,*-6                                                           
         JE    BLDST038            Data the same                                
*                                                                               
* Special treatment for SJ account                                              
*                                                                               
         CLI   AUDSTCIN,STCOCLIQ+STCOPROQ+STCOJOBQ                              
         JNE   BLDST036                                                         
         LLC   R1,PCLILEN          R1=length of client                          
         SHI   R1,1                                                             
         CLC   0(0,RE),0(R5)       Compare old and new client codes             
         EXRL  R1,*-6                                                           
         JNE   BLDST036            Not the same all must be changed             
         AHI   R1,1                R1=Displacement to product                   
         AR    R5,R1               R5=A(old product code)                       
         AR    RE,R1               RE=A(new product code)                       
         LLC   RF,PPROLEN          RF=length of client product                  
         SR    RF,R1               RF=length of product                         
         LLC   R1,AUDDSTCI         Displacement to suitable indicator           
         LA    R1,EL.STCEL(R1)     R1=A(indicator byte on STCELD)               
         SHI   RF,1                                                             
         CLC   0(0,RE),0(R5)       Compare old and new product                  
         EXRL  RF,*-6                                                           
         JE    *+8                 No change so must be job only                
         OI    0(R1),STCOPROQ      Set product code as changed                  
         OI    0(R1),STCOJOBQ      Set job code as changed                      
         J     BLDST038                                                         
*                                                                               
BLDST036 LLC   RF,AUDDSTCI         Displacement to suitable indicator           
         LA    R5,EL.STCEL(RF)     R5=A(indicator byte on STCELD)               
         OC    0(1,R5),AUDSTCIN    Set indicator bit for data change            
         TM    AUDDTIND,AUD1ERR    Is one error enough                          
         JNZ   BLDST044            Yes - read next table entry                  
                                                                                
BLDST038 TM    AUDDTIND,AUDSUBEL   Are there sub elements                       
         JZ    BLDST040            Read along sub elements                      
         LLC   RF,AUDSUBLN                                                      
         AR    R6,RF                                                            
         AR    R4,RF                                                            
         J     BLDST034                                                         
                                                                                
BLDST040 LAY   R6,I_NEW                                                         
         LAY   R4,I_OLD                                                         
                                                                                
BLDST042 TM    AUDDTIND,AUDMULT    Are there multiple elements                  
         JNZ   BLDST024            Read for other elements                      
                                                                                
BLDST044 LA    R3,AUDTABL(R3)      Get next entry in table                      
         CLI   0(R3),X'FF'         End of table                                 
         JE    BLDST050            Yes                                          
         TM    TSAERR,TSEEOF       Did we get to end of the buffer?             
         JNZ   BLDST016            Yes - read buffer for this element           
*        CLC   =X'DB0269',AUDELCDE                                              
*        JE    *+2                                                              
         CLC   NEL.ORDEL,AUDELCDE  Does it match the code of el read            
         JNE   BLDST016            No - read element from buffer                
         TM    AUDDTIND,AUDNSTON   Audit not required on status only            
         JZ    BLDST046                                                         
         CLI   XOMACT,RQOMA3Q      Status only change                           
         JE    BLDST044            Skip this entry                              
BLDST046 LHI   RF,L'AUDORDTY       Number of possible order types               
         LA    RE,AUDORDTY                                                      
BLDST048 CLC   XOMTYP,0(RE)        Does audit type match order type             
         JE    BLDST028            Yes                                          
         LA    RE,1(RE)            Try next entry                               
         JCT   RF,BLDST048                                                      
         J     BLDST044                                                         
                                                                                
* Process element deletions                                                     
                                                                                
BLDST050 LA    R3,AUDTAB1                                                       
         USING AUDTABD,R3                                                       
                                                                                
BLDST052 TM    AUDDTIND,AUDNSTON   Audit not required on status only            
         JZ    BLDST054                                                         
         CLI   XOMACT,RQOMA3Q      Status only change                           
         JE    BLDST068            Skip this entry                              
BLDST054 LHI   RF,L'AUDORDTY       Number of possible order types               
         LA    RE,AUDORDTY                                                      
BLDST056 CLC   XOMTYP,0(RE)        Does audit type match order type             
         JE    BLDST058            Yes                                          
         LA    RE,1(RE)            Try next entry                               
         JCT   RF,BLDST056                                                      
         J     BLDST068            No match skip to next audit entry            
                                                                                
BLDST058 XC    OLD.EL_REC(EL_KEYL),OLD.EL_REC                                   
         MVC   OLD.EL_ELCDE,AUDELCDE  Set element code to read                  
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEO),('BEOLDQ',OLD.EL_REC)            
         J     BLDST062                                                         
*                                                                               
BLDST060 GOTOR BUFELE,DMCB,('TSANXT',TSARELEO),('BEOLDQ',OLD.EL_REC)            
BLDST062 TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   BLDST068            Yes - get next element code from tab         
         CLC   OEL.ORDEL,AUDELCDE  Does it match the code                       
         JNE   BLDST068            No - read next entry in table                
BLDST064 OC    AUDELDTY,AUDELDTY   Do we have to check the sub type             
         JZ    BLDST066            No                                           
         LLC   RF,AUDELDTY         Yes - RF=displacement to sub type            
         LA    RE,OEL.ORDEL                                                     
         AR    RE,RF               RE=A(element type field)                     
         CLC   AUDELTYP,0(RE)                                                   
         JNE   BLDST060            No match - read for next element             
                                                                                
BLDST066 TM    OLD.EL_ELSTA,EL_EXSTS Does data exist on new buffer              
         JNZ   BLDST068            Yes - ok                                     
                                                                                
         XR    RF,RF                                                            
         ICM   RF,1,AUDDSSTC       Any data to be on audit                      
         JZ    BLDST067            No                                           
         LLC   R1,AUDSTCLN         R1=length of data to be extracted            
         LA    RE,EL.STCEL(RF)     RE=A(audit data field)                       
         LLC   RF,AUDDSELM                                                      
         SHI   R1,1                                                             
         TM    AUDDTIND,AUDPREV    Check for previous data                      
         JZ    BLDST067                                                         
         CLC   0(0,RE),SPACES      Any present?                                 
         EXRL  R1,*-6                                                           
         JH    BLDST068            Yes - Skip to next audit entry               
                                                                                
BLDST067 LLC   RF,AUDDSTCI         Displacement to suitable indicator           
         LA    R5,EL.STCEL(RF)     R5=A(indicator byte on STCELD)               
         OC    0(1,R5),AUDSTCIN    Set indicator bit for data change            
         TM    AUDDTIND,AUDMULT    Are there multiple elements                  
         JNZ   BLDST060            Read for other elements                      
                                                                                
BLDST068 LA    R3,AUDTABL(R3)      Get next entry in table                      
         CLI   0(R3),X'FF'         End of table                                 
         JE    BLDST074            Yes                                          
         CLC   OEL.ORDEL,AUDELCDE  Does it match the code of el read            
         JNE   BLDST052            No - read element from buffer                
         TM    AUDDTIND,AUDNSTON   Audit not required on status only            
         JZ    BLDST070                                                         
         CLI   XOMACT,RQOMA3Q      Status only change                           
         JE    BLDST068            Skip this entry                              
BLDST070 LHI   RF,L'AUDORDTY       Number of possible order types               
         LA    RE,AUDORDTY                                                      
BLDST072 CLC   XOMTYP,0(RE)        Does audit type match order type             
         JE    BLDST064            Yes                                          
         LA    RE,1(RE)            Try next entry                               
         JCT   RF,BLDST072                                                      
         J     BLDST068                                                         
                                                                                
* Build end of first element if expense order                                   
                                                                                
BLDST074 XC    ELEMENT,ELEMENT                                                  
         CLI   XOMTYP,EXPOQ                                                     
         JE    BLDST098                                                         
                                                                                
* Build additional workcode change element                                      
                                                                                
BLDST076 XC    BYTE1,BYTE1         Clear indicator                              
         LA    R3,ELEMENT                                                       
         USING STCELD,R3                                                        
         MVC   STCEL(STCOLN1Q),EL.STCEL                                         
         MVI   STCOTYP,STCOWCCQ                                                 
*                                                                               
         XR    RF,RF               If Aura check aura flag                      
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ          Set BrandOcean or Aura application          
         JNE   *+8                                                              
         OI    STCOTYP,STCOAURQ                                                 
*                                                                               
         MVI   STCLN,STCOLN1Q                                                   
         XC    NEW.EL_REC(EL_KEYL),NEW.EL_REC                                   
         MVI   NEW.EL_ELCDE,OAMELQ  Set element code to read                    
NEL      USING OAMELD,NEW.EL_ELEM                                               
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEN),('BENEWQ',NEW.EL_REC)            
         J     BLDST080                                                         
*                                                                               
BLDST078 GOTOR BUFELE,DMCB,('TSANXT',TSARELEN),('BENEWQ',NEW.EL_REC)            
BLDST080 TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   BLDST090            Yes - get next element code from tab         
         CLI   NEL.OAMEL,OAMELQ    Does it match the code                       
         JNE   BLDST090            No - read next entry in table                
                                                                                
BLDST082 CLI   BYTE1,0             Are we building first workcode               
         JNE   BLDST084            No                                           
         MVC   EL.STCOWC,NEL.OAMWORK                                            
         ZAP   EL.STCOWCAM,NEL.OAMAMNT                                          
         MVI   BYTE1,1                                                          
         J     BLDST086                                                         
                                                                                
BLDST084 MVC   STCOWK,NEL.OAMWORK On second audit element                       
         ZAP   STCOWKAM,NEL.OAMAMNT                                             
         LLC   RF,ELEMENT+1                                                     
         AHI   RF,L'STCOWKNT                                                    
         STC   RF,ELEMENT+1       Increment length of element                   
         LA    R3,L'STCOWKNT(R3)  Bump R3 so it points to next entry            
                                                                                
BLDST086 XC    OLD.EL_REC(EL_KEYL),OLD.EL_REC                                   
OEL      USING OAMELD,OLD.EL_ELEM                                               
         MVC   OLD.EL_REC(EL_KEYL),NEW.EL_REC                                   
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEO),('BEOLDQ',OLD.EL_REC)            
         CLI   TSAERR,0                                                         
         JNE   BLDST088            Not found must be a new element              
         CLC   NEW.EL_REC(EL_KEYL1),OLD.EL_REC Is it the same element           
         JNE   BLDST088            No                                           
         OI    OLD.EL_ELSTA,EL_EXSTS  Set we found record on old buffer         
         GOTOR BUFELE,DMCB,('TSAWRT',TSARELEO),('BEOLDQ',OLD.EL_REC)            
         CLI   TSAERR,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   NEL.OAMWORK,OEL.OAMWORK Does old workcode match                  
         JE    *+8                                                              
         OI    EL.STCOIND1,STCOWCQ                                              
         CLC   NEL.OAMAMNT,OEL.OAMAMNT Does old workcode amount match           
         JE    *+8                                                              
         OI    EL.STCOIND1,STCOWCAQ                                             
         J     *+8                                                              
*                                                                               
BLDST088 OI    EL.STCOIND1,STCOWCQ+STCOWCAQ                                     
         J     BLDST078                                                         
                                                                                
* Read for deletions and report them                                            
                                                                                
BLDST090 XC    OLD.EL_REC(EL_KEYL),OLD.EL_REC                                   
         MVI   OLD.EL_ELCDE,OAMELQ  Set element code to read                    
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEO),('BEOLDQ',OLD.EL_REC)            
         J     BLDST094                                                         
*                                                                               
BLDST092 GOTOR BUFELE,DMCB,('TSANXT',TSARELEO),('BEOLDQ',OLD.EL_REC)            
BLDST094 TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   BLDST096            Yes - get next element code from tab         
         CLI   OEL.OAMEL,OAMELQ    Does it match the code                       
         JNE   BLDST096            No - read next entry in table                
         TM    OLD.EL_ELSTA,EL_EXSTS Does data exist on new buffer              
         JNZ   BLDST092            Yes - ok                                     
         OI    EL.STCOIND1,STCOWCQ+STCOWCAQ                                     
                                                                                
* Build end of element and send to buffer                                       
                                                                                
BLDST096 TM    EL.STCOIND1,STCOWCQ+STCOWCAQ Don't add work codes                
         JNZ   BLDST098            if no change                                 
         MVC   EL.STCOWC,SPACES                                                 
         ZAP   EL.STCOWCAM,PZERO                                                
BLDST098 GOTOR STCCOM              Add comment to end of element                
         TM    EL.STCOIND1,STCOWCQ+STCOWCAQ Don't add work codes                
         JZ    BLDST100            if no change                                 
         LA    R3,ELEMENT                                                       
         CLI   STCLN,STCLN1Q       Have we got a second element                 
         JNH   BLDST100            No                                           
         LLC   RF,STCLN            Yes - copy to buffer area and add            
         SHI   RF,1                                                             
         MVC   EL.STCEL(0),STCEL                                                
         EXRL  RF,*-6                                                           
         GOTOR BUFELE,DMCB,('TSAADD',TSARSTCL),('BEAUDQ',EL_REC)                
         JE    BLDST100            Save element to buffer                       
         DC    H'0'                                                             
                                                                                
BLDST100 CLI   XOMSTA,RQPRTAPP     Were we part approving                       
         JNE   BLDST102                                                         
         CLC   XOWRKST,XNWRKST     New and old workflow match                   
         JNE   BLDST102                                                         
         CLI   XOWRKST,XPRTAPRD    Are we part approved                         
         JNE   BLDST102                                                         
         GOTOR BLDBST              Build base STCELD                            
         MVI   EL.STCOTYP,STCOAPPQ Set approval audit                           
         MVI   EL.STCLN,STCOLN1Q   Set length of element                        
*                                                                               
         XR    RF,RF               If Aura check aura flag                      
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ         Set BrandOcean or Aura application           
         JNE   BLDST101                                                         
         OI    EL.STCOTYP,STCOAURQ Aura                                         
         MVC   EL.STCOTOST,XNWRKST Workflow status                              
         MVC   EL.STCOFRST,XOWRKST                                              
         MVI   EL.STCLN,STCOLN3Q   Standard length                              
         CLI   XSTCLN,FF                                                        
         JE    BLDST101            Empty                                        
         LLC   RF,XSTCLN                                                        
         SHI   RF,1                                                             
         MVC   EL.STCOCOM(0),XSTCCO  Add comment                                
         EXRL  RF,*-6                                                           
         AHI   RF,STCOLN3Q+1       Set extended length                          
         STC   RF,EL.STCLN                                                      
*                                                                               
BLDST101 GOTOR BUFELE,DMCB,('TSAADD',TSARSTCL),('BEAUDQ',EL_REC)                
         JE    BLDST102            Save element to buffer                       
         DC    H'0'                                                             
                                                                                
* Build extra data fields additions, changes or deletions                       
                                                                                
BLDST102 XC    NEW.EL_REC(EL_KEYL),NEW.EL_REC                                   
         MVI   NEW.EL_ELCDE,XDFELQ  Set element code to read                    
NEL      USING XDFELD,NEW.EL_ELEM                                               
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEN),('BENEWQ',NEW.EL_REC)            
         J     BLDST106                                                         
*                                                                               
BLDST104 GOTOR BUFELE,DMCB,('TSANXT',TSARELEN),('BENEWQ',NEW.EL_REC)            
BLDST106 TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   BLDST114            Yes - get next element code from tab         
         CLI   NEL.XDFEL,XDFELQ    Does it match the code                       
         JNE   BLDST114            No - read next entry in table                
         GOTOR BLDBST              Build base STCELD                            
                                                                                
BLDST108 XC    OLD.EL_REC(EL_KEYL),OLD.EL_REC                                   
OEL      USING XDFELD,OLD.EL_ELEM                                               
         MVC   OLD.EL_REC(EL_KEYL),NEW.EL_REC                                   
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEO),('BEOLDQ',OLD.EL_REC)            
         CLI   TSAERR,0                                                         
         JNE   BLDST112            Not found must be a new addition             
         CLC   NEW.EL_REC(EL_KEYL1),OLD.EL_REC Is it the same element           
         JNE   BLDST112            Not same code so new addition                
         OI    OLD.EL_ELSTA,EL_EXSTS  Set we found record on old buffer         
         GOTOR BUFELE,DMCB,('TSAWRT',TSARELEO),('BEOLDQ',OLD.EL_REC)            
         CLI   TSAERR,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   OEL.XDFLN,NEL.XDFLN If the element length different              
         JNE   BLDST110            Yes - then data must be different            
         LLC   RF,OEL.XDFLN        No - compare data                            
         SHI   RF,1                                                             
         CLC   OEL.XDFEL(0),NEL.XDFEL                                           
         EXRL  RF,*-6                                                           
         JE    BLDST104            Data is the same - get next element          
                                                                                
BLDST110 MVI   EL.STCOTYP,STCOXDCQ Set type as extra data change                
         J     *+8                                                              
BLDST112 MVI   EL.STCOTYP,STCOXDAQ Set type as extra data addition              
*                                                                               
         XR    RF,RF               If Aura check aura flag                      
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ         Set BrandOcean or Aura application           
         JNE   *+8                                                              
         OI    EL.STCOTYP,STCOAURQ                                              
*                                                                               
         LLC   RF,NEL.XDFLN        Copy extra data to audit element             
         SHI   RF,XDFSEQL+1                                                     
         MVC   EL.STCOXDTC(0),NEL.XDFOCOD                                       
         EXRL  RF,*-6                                                           
         AHI   RF,STCOLN1Q+1                                                    
         STC   RF,EL.STCLN                                                      
         GOTOR BUFELE,DMCB,('TSAADD',TSARSTCL),('BEAUDQ',EL_REC)                
         JE    BLDST104            Save element to buffer                       
         DC    H'0'                                                             
                                                                                
* Read for deletions and report them                                            
                                                                                
BLDST114 XC    OLD.EL_REC(EL_KEYL),OLD.EL_REC                                   
         MVI   OLD.EL_ELCDE,XDFELQ  Set element code to read                    
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEO),('BEOLDQ',OLD.EL_REC)            
         J     BLDST118                                                         
*                                                                               
BLDST116 GOTOR BUFELE,DMCB,('TSANXT',TSARELEO),('BEOLDQ',OLD.EL_REC)            
BLDST118 TM    TSAERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   EXITY               Yes - get next element code from tab         
         CLI   OEL.XDFEL,XDFELQ    Does it match the extra data element         
         JNE   EXITY               No - read next entry in table                
         TM    OLD.EL_ELSTA,EL_EXSTS Does data exist on new buffer              
         JNZ   BLDST116            Yes - ok                                     
         GOTOR BLDBST              Build base STCELD                            
         MVI   EL.STCOTYP,STCOXDDQ Set type as deletion of extra data           
*                                                                               
         XR    RF,RF               If Aura check aura flag                      
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ         Set BrandOcean or Aura application           
         JNE   *+8                                                              
         OI    EL.STCOTYP,STCOAURQ                                              
*                                                                               
         LLC   RF,OEL.XDFLN        Copy extra data to audit element             
         SHI   RF,XDFSEQL+1                                                     
         MVC   EL.STCOXDTC(0),OEL.XDFOCOD                                       
         EXRL  RF,*-6                                                           
         AHI   RF,STCOLN1Q+1                                                    
         STC   RF,EL.STCLN                                                      
         GOTOR BUFELE,DMCB,('TSAADD',TSARSTCL),('BEAUDQ',EL_REC)                
         JE    BLDST116            Save element to buffer                       
         DC    H'0'                                                             
         DROP  EL,OEL,NEL,OLD,NEW,R2,R3                                         
***********************************************************************         
* Build comment at end of STCELD element                              *         
***********************************************************************         
         SPACE 1                                                                
STCCOM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*STCCOM*'                                                      
                                                                                
         LAY   R2,I_AUD                                                         
         USING EL_RECD,R2                                                       
EL       USING STCELD,EL_ELEM                                                   
         MVI   EL.STCLN,STCOLN3Q   Standard length                              
         CLI   XSTCLN,FF                                                        
         JE    STCCOM10            Empty                                        
*                                                                               
         XR    RF,RF               If Aura check aura flag                      
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ         Set BrandOcean or Aura application           
         JNE   STCCOM08                                                         
         CLI   XOMSTA,RQPRTAPP     Were we part approving                       
         JNE   STCCOM08                                                         
         CLC   XOWRKST,XNWRKST     New and old workflow match                   
         JNE   STCCOM08                                                         
         CLI   XOWRKST,XPRTAPRD    Are we part approved                         
         JE    STCCOM10                                                         
                                                                                
STCCOM08 LLC   RF,XSTCLN                                                        
         SHI   RF,1                                                             
         MVC   EL.STCOCOM(0),XSTCCO  Add comment                                
         EXRL  RF,*-6                                                           
         AHI   RF,STCOLN3Q+1       Set extended length                          
         STC   RF,EL.STCLN                                                      
STCCOM10 CLI   EL.STCOTYP,STCOCHGQ+STCOAURQ for change check whether            
         JNE   STCCOM20                     any changes to data                 
         OC    EL.STCOIND1(L'STCOIND1*4),EL.STCOIND1  or workflow               
         JNZ   STCCOM20                                   status                
         CLC   EL.STCOFRST,EL.STCOTOST                                          
         JE    EXITY                                                            
STCCOM20 GOTOR BUFELE,DMCB,('TSAADD',TSARSTCL),('BEAUDQ',EL_REC)                
         JE    EXITY               Save element to buffer                       
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* Build all status elements in AIO4                                   *         
* Uses AIO4 to hold stcelds                                           *         
***********************************************************************         
         SPACE 1                                                                
BLGSTC   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLGSTC*'                                                      
         LAY   R2,I_AUD                                                         
         USING EL_RECD,R2                                                       
         XC    EL_REC(EL_KEYL),EL_REC                                           
         XC    BYTE2,BYTE2                                                      
         OC    XGAPSTAT,XGAPSTAT   Any GAP status change?                       
         JZ    BLGSTCX             No                                           
         CLC   XOLGSTA,XGAPSTAT    Any change to GAP status?                    
         JNE   BLGSTC08            Yes                                          
         CLC   XOLGAPEX,XGAPEXPD   Any change to GAP expiry date                
         JNE   BLGSTC08            Yes                                          
                                                                                
* check for changes to email addresses                                          
                                                                                
         XR    R1,R1                                                            
         ICM   R1,1,XGAPEMLN       Number of email addresses                    
         JZ    BLGSTCX             None - nothing to do                         
         L     R3,AGENAREA         R3=A(old email addresses)                    
         LA    R4,(L'STCGEML*10)(R3) R4=A(new email addresses)                  
BLGSTC02 XR    R5,R5                                                            
         ICM   R5,1,XOLGEMLN       Number of email addresses previously         
         JZ    BLGSTC08                                                         
         L     R3,AGENAREA                                                      
BLGSTC04 CLC   0(L'STCGEML,R3),1(R4)  See if they match                         
         JE    BLGSTC06                if so no need to send to audit           
         LA    R3,L'STCGEML(R3)                                                 
         JCT   R5,BLGSTC04                                                      
         MVI   BYTE2,YESQ          Set we found a different email               
         J     *+8                                                              
BLGSTC06 MVI   0(R4),NOQ           Set no so we skip sending this               
         LA    R4,(L'STCGEML+1)(R4)                                             
         JCT   R1,BLGSTC02                                                      
         CLI   BYTE2,YESQ          Do we have email address to process          
         JNE   BLGSTCX             No                                           
                                                                                
* Build element of GAP status email changes                                     
                                                                                
*LGSTC08 CLI   XGAPSTAT,ORDGNSNT     Don't capture not sent                     
*        JE    BLGSTCX                                                          
BLGSTC08 OC    XOLGSTA,XOLGSTA       Do we have an old status                   
         JNZ   *+8                   Yes                                        
         MVI   XOLGSTA,ORDGNSNT      No - default to not sent                   
         L     R4,AGENAREA                                                      
         LA    R4,(L'STCGEML*10)(R4)                                            
         XR    R5,R5                                                            
         ICM   R5,1,XGAPEMLN         # of array elements                        
EL       USING STCELD,EL_ELEM                                                   
*                                                                               
BLGSTC10 GOTOR BLDBST                                                           
         MVI   EL.STCOTYP,STCOGAPQ    Gap status change                         
         OI    EL.STCOTYP,STCOAURQ    and Aura                                  
         MVC   EL.STCGAPFR,XOLGSTA                                              
         MVC   EL.STCGAPTO,XGAPSTAT                                             
         MVC   EL.STCGEXDT,XGAPEXPD                                             
         MVI   EL.STCLN,STCGAPLQ                                                
         LTR   R5,R5                                                            
         JZ    BLGSTC12                                                         
         CLI   0(R4),YESQ                                                       
         JNE   BLGSTC14                                                         
         MVC   EL.STCGEML,1(R4)                                                 
         MVI   EL.STCLN,STCGLNQ                                                 
                                                                                
BLGSTC12 GOTOR BUFELE,DMCB,('TSAADD',TSARSTCL),('BEAUDQ',EL_REC)                
         JE    *+6                 Save element to buffer                       
         DC    H'0'                                                             
         LTR   R5,R5                                                            
         JZ    BLGSTCX                                                          
BLGSTC14 LA    R4,(L'STCGEML+1)(R4)                                             
         JCT   R5,BLGSTC10                                                      
*                                                                               
BLGSTCX  J     EXITY                                                            
         DROP  EL,R2                                                            
         EJECT                                                                  
***********************************************************************         
* Build a basic stceld                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING STCELD,R4                                                        
BLDBST   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDBST*'                                                      
         LAY   R1,I_AUD                                                         
         USING EL_RECD,R1                                                       
         XC    EL_REC(EL_KEYL),EL_REC                                           
EL       USING STCELD,EL_ELEM                                                   
         XC    EL.STCEL(L'ELEMENT),EL.STCEL                                     
         MVI   EL.STCEL,STCELQ                                                  
         MVI   EL.STCIND,STCIORD2                                               
         MVI   EL.STCLN,STCOLN2Q                                                
         MVC   EL.STCOUSR,CUUSER                                                
         MVC   EL.STCOPID,CCTPID                                                
         MVC   EL.STCODTE,OR_TODP                                               
         MVC   EL.STCOTIM,CTIME                                                 
         J     EXITY                                                            
         DROP  EL,R1                                                            
         EJECT                                                                  
***********************************************************************         
* Add element to main order record                                    *         
***********************************************************************         
         SPACE 1                                                                
ADDELM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13ADDE*'                                                      
         L     R3,0(R1)                                                         
         USING ORDELD,R3                                                        
                                                                                
*        LA    RF,=CL8'ADD=END'    all elements to the end                      
         LA    RF,=CL8'ADD=CODE'   all elements added in SEQ                    
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AIO3,ORDELD,0(RF)                      
         CLI   12(R1),0                                                         
         JE    EXITY                                                            
         DC    H'0'                record too big                               
         EJECT                                                                  
***********************************************************************         
* Add element to new buffer so we can audit changes                   *         
***********************************************************************         
         SPACE 1                                                                
AUDNEW   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*AUDNEW*'                                                      
         L     R3,0(R1)                                                         
         USING ORDELD,R3                                                        
         LAY   R6,I_NEW                                                         
         USING EL_RECD,R6                                                       
         XC    EL_REC(EL_KEYL),EL_REC                                           
EL       USING ORDELD,EL_ELEM                                                   
         XC    EL.ORDEL(L'ELEMENT),EL.ORDEL                                     
         LLC   RF,ORDLN                                                         
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   EL.ORDEL(0),ORDEL                                                
         EX    RF,0(R1)                                                         
         MVC   EL_ELCDE,ORDEL                                                   
*                                                                               
         USING PIDELD,R3                                                        
         CLI   PIDEL,PIDELQ                                                     
         JNE   AUDNEW02                                                         
         MVC   EL_ELWRK(L'PIDTYPE),PIDTYPE                                      
         USING OAMELD,R3                                                        
AUDNEW02 CLI   OAMEL,OAMELQ                                                     
         JNE   AUDNEW04                                                         
         LLH   RF,ELSEQ#           Set sequence no. and update the              
         STC   RF,EL_ELWRK         sequence no.                                 
         AHI   RF,1                                                             
         STH   RF,ELSEQ#                                                        
                                                                                
         USING FFTELD,R3                                                        
AUDNEW04 CLI   FFTEL,FFTELQ        Free form text element                       
         JNE   AUDNEW06                                                         
         MVC   EL_ELWRK(L'FFTTYPE),FFTTYPE                                      
         USING SPAELD,R3                                                        
AUDNEW06 CLI   SPAEL,SPAELQ        Staff and department account codes           
         JNE   AUDNEW08                                                         
         MVC   EL_ELWRK(L'SPATYPE),SPATYPE                                      
         USING SCMELD,R3                                                        
AUDNEW08 CLI   SCMEL,SCMELQ        Matching and printing description            
         JNE   AUDNEW10                                                         
         MVC   EL_ELWRK(L'SCMSEQ+L'SCMTYPE),SCMSEQ                              
         CLI   SCMTYPE,SCMTSANP                                                 
         JNE   *+10                                                             
         MVC   EL_ELWRK(L'SCMSEQ+L'SCMTYPE+L'SCMSSEQ),SCMSEQ                    
AUDNEW10 CLI   SCMEL,ARTELQ                                                     
         JNE   AUDNEW12                                                         
         USING ARTELD,R3                                                        
         MVC   EL_ELWRK(L'ARTSEQ+L'ARTWC),ARTWC                                 
AUDNEW12 CLI   ARTEL,ATXELQ                                                     
         JNE   AUDNEW14                                                         
         USING ATXELD,R3                                                        
         MVC   EL_ELWRK(L'ATXWCOD+L'ATXWSQN+L'ATXWSSN),ATXWCOD                  
*                                                                               
AUDNEW14 GOTOR BUFELE,DMCB,('TSAADD',TSARELEN),('BENEWQ',EL_REC)                
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  EL,R6                                                            
         EJECT                                                                  
***********************************************************************         
* Add element to old buffer so we can audit changes                   *         
***********************************************************************         
         SPACE 1                                                                
AUDOLD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*AUDOLD*'                                                      
         L     R3,0(R1)                                                         
         USING ORDELD,R3                                                        
         LAY   R6,I_OLD                                                         
         USING EL_RECD,R6                                                       
         XC    EL_REC(EL_KEYL),EL_REC                                           
EL       USING ORDELD,EL_ELEM                                                   
         XC    EL.ORDEL(L'ELEMENT),EL.ORDEL                                     
         LLC   RF,ORDLN                                                         
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   EL.ORDEL(0),ORDEL                                                
         EX    RF,0(R1)                                                         
         MVC   EL_ELCDE,ORDEL                                                   
*                                                                               
         USING PIDELD,R3                                                        
         CLI   PIDEL,PIDELQ                                                     
         JNE   AUDOLD02                                                         
         MVC   EL_ELWRK(L'PIDTYPE),PIDTYPE                                      
         USING OAMELD,R3                                                        
AUDOLD02 CLI   OAMEL,OAMELQ                                                     
         JNE   AUDOLD04                                                         
         LLH   RF,ELSEQ#           Set sequence no. and update the              
         STC   RF,EL_ELWRK         sequence no.                                 
         AHI   RF,1                                                             
         STH   RF,ELSEQ#                                                        
                                                                                
         USING FFTELD,R3                                                        
AUDOLD04 CLI   FFTEL,FFTELQ        Free form text element                       
         JNE   AUDOLD06                                                         
         MVC   EL_ELWRK(L'FFTTYPE),FFTTYPE                                      
         USING SPAELD,R3                                                        
AUDOLD06 CLI   SPAEL,SPAELQ        Staff and department account codes           
         JNE   AUDOLD08                                                         
         MVC   EL_ELWRK(L'SPATYPE),SPATYPE                                      
         USING SCMELD,R3                                                        
AUDOLD08 CLI   SCMEL,SCMELQ        Matching and printing description            
         JNE   AUDOLD10                                                         
         MVC   EL_ELWRK(L'SCMSEQ+L'SCMTYPE),SCMSEQ                              
         CLI   SCMTYPE,SCMTSANP                                                 
         JNE   *+10                                                             
         MVC   EL_ELWRK(L'SCMSEQ+L'SCMTYPE+L'SCMSSEQ),SCMSEQ                    
AUDOLD10 CLI   SCMEL,ARTELQ                                                     
         JNE   AUDOLD12                                                         
         USING ARTELD,R3                                                        
         MVC   EL_ELWRK(L'ARTSEQ+L'ARTWC),ARTWC                                 
         MVC   EL_ELWRK+L'ARTSEQ+L'ARTWC(L'ARTWSQN+L'ARTWSSN),ARTWSQN           
AUDOLD12 CLI   ARTEL,ATXELQ                                                     
         JNE   AUDOLD14                                                         
         USING ATXELD,R3                                                        
         MVC   EL_ELWRK(L'ATXWCOD+L'ATXWSQN+L'ATXWSSN),ATXWCOD                  
*                                                                               
AUDOLD14 CLI   ATXEL,XDFELQ                                                     
         JNE   AUDOLD16                                                         
         USING XDFELD,R3                                                        
         MVC   EL_ELWRK(L'XDFOCOD),XDFOCOD                                      
*                                                                               
AUDOLD16 GOTOR BUFELE,DMCB,('TSAADD',TSARELEO),('BEOLDQ',EL_REC)                
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  EL,R6                                                            
         EJECT                                                                  
***********************************************************************         
* Add PIDEL to main order record                                      *         
***********************************************************************         
         SPACE 1                                                                
ADDPID   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ADDPID*'                                                      
                                                                                
                                                                                
         USING PIDELD,R3                                                        
         USING XPIDSD,R2                                                        
ADDPID06 LA    R3,ELEMENT                                                       
         LA    R2,XPVALS                                                        
         LHI   R4,1                                                             
                                                                                
ADDPID10 OC    XPIDC1,XPIDC1       any PID present                              
         JZ    ADDPID16                                                         
         LA    RE,1                one or two PIDs used on this level?          
         OC    XPIDC2,XPIDC2                                                    
         JZ    ADDPID12                                                         
         LA    RE,2                                                             
                                                                                
ADDPID12 XC    ELEMENT,ELEMENT                                                  
         MVI   PIDEL,PIDELQ                                                     
         STC   R4,PIDTYPE                                                       
         STC   RE,PIDNTR#                                                       
         MHI   RE,L'PIDNTRS                                                     
         AHI   RE,PIDLNQ                                                        
         STC   RE,PIDLN                                                         
         MVC   PIDNTRS,XPIDS1                                                   
         CLI   PIDNTR#,2                                                        
         JL    ADDPID14                                                         
         MVC   PIDNTRS+L'PIDNTRS,XPIDS2                                         
                                                                                
ADDPID14 GOTOR ADDELM,DMCB,PIDELD                                               
         GOTOR AUDNEW,DMCB,PIDELD                                               
                                                                                
ADDPID16 AHI   R2,XPIDLQ           Bump to next entry in storage                
         AHI   R4,1                Increment type                               
         CHI   R4,4                Only allow a max of 4 entries                
         JNH   ADDPID10            Get next entry                               
         J     EXITY               Exit                                         
         EJECT                                                                  
***********************************************************************         
* Return data on order upload                                         *         
***********************************************************************         
         SPACE 1                                                                
RETURN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13RETU*'                                                      
                                                                                
         USING LIOBD,R4                                                         
         L     R4,AALIOB                                                        
         XR    R0,R0               BUILD DOWNLOAD MAP ELEMENT                   
         ICM   R0,3,LP_QMAPN                                                    
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
                                                                                
         CLI   XESTWYN,YESQ        Order not added as warning/error             
         JE    RETURN10            Yes                                          
                                                                                
RETURN02 XR    RF,RF               If Aura always return order number           
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ                                                      
         JE    *+12                                                             
         CLI   XOMACT,RQOMA1Q      return order number                          
         JNE   RETURN04                                                         
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#ORD),   *        
               ('LD_CHARQ',NEWONUM),(L'NEWONUM,0)                               
                                                                                
         CLC   NEWRNUM,SPACES      return requisition number                    
         JNH   RETURN04                                                         
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#REQ),   *        
               ('LD_CHARQ',NEWRNUM),(L'NEWRNUM,0)                               
                                                                                
RETURN04 CLI   XESTWYN,YESQ        return ID number                             
         JE    RETURN06                                                         
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#IDN),   *        
               ('LD_HEXDQ',XIDNEW),(L'XIDNEW,0)                                 
                                                                                
RETURN06 DS    0H                  return new status                            
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#STA),   *        
               ('LD_CHARQ',XOUTSTA),(L'XOUTSTA,0)                               
                                                                                
         CLI   ORDTYPE,INTOQ       return internal supplier code                
         JNE   RETURN10                                                         
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#ISC),   *        
               ('LD_CHARQ',XSUPCULA+1),(L'XSUPCULA-1,0)                         
                                                                                
         DS    0H                  return internal supplier name                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#ISN),   *        
               ('LD_CHARQ',XSUPNAME),(L'XSUPNAME,0)                             
                                                                                
RETURN10 OC    XESCKE,XESCKE       Error message                                
         JZ    RETURN12                                                         
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#ESE),   *        
               ('LD_HEXDQ',XESCKE),(L'XESCKE,0)                                 
                                                                                
RETURN12 OC    XESCWM,XESCWM       Warning message                              
         JZ    RETURN14                                                         
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#ESW),   *        
               ('LD_HEXDQ',XESCWM),(L'XESCWM,0)                                 
                                                                                
RETURN14 GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#DXO),   *        
               ('LD_SPAKQ',XDFEXPO),(L'XDFEXPO,0)                               
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#GEM),   *        
               ('LD_CHARQ',XSUPEM),(L'XSUPEM,0)                                 
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#PRS),   *        
               ('LD_CHARQ',GOPPORES),(L'GOPPORES,0)                             
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#FRS),   *        
               ('LD_CHARQ',GOFPORES),(L'GOFPORES,0)                             
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#MRS),   *        
               ('LD_CHARQ',GOMPORES),(L'GOMPORES,0)                             
         DROP  R3                                                               
*                                                                               
         USING ESTCHKD,R2                                                       
RETURN16 GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#JRA),   *        
               ('LD_SPAKQ',XJOBLE),(L'XJOBLE,0)                                 
RETURN18 LAY   R2,SVESTWK                                                       
         OC    ESTCHWC,ESTCHWC     EOT?                                         
         JZ    RETURN28                                                         
*                                                                               
RETURN20 GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',1)                   
*                                  Workcode                                     
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#WRK),   *        
               ('LD_CHARQ',ESTCHWC),(L'ESTCHWC,0)                               
*                                  Remaining amount                             
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#ESR),   *        
               ('LD_SPAKQ',ESTRAMT),(L'ESTRAMT,0)                               
*                                  Estimate check error                         
RETURN22 OC    ESTCHKE,ESTCHKE                                                  
         JZ    RETURN24                                                         
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#WLE),   *        
               ('LD_HEXDQ',ESTCHKE),(L'ESTCHKE,0)                               
*                                  Estimate check warning                       
RETURN24 OC    ESTCHKW,ESTCHKW                                                  
         JZ    RETURN26                                                         
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OU#WLW),   *        
               ('LD_HEXDQ',ESTCHKW),(L'ESTCHKW,0)                               
*                                                                               
RETURN26 LA    R2,ESTCHLQ(R2)                                                   
         OC    ESTCHWC,ESTCHWC     any entry in the table                       
         JNZ   RETURN20                                                         
*                                                                               
         OC    XESCKE,XESCKE       Order not added - error                      
         JNZ   RETURN40            Yes                                          
                                                                                
RETURN28 GOTOR (#CLRIO,ACLRIO),DMCB,AIO6                                        
         L     R3,AIO6                                                          
         USING XPIDSD,R2                                                        
         LA    R2,XPVALS                                                        
         LA    R4,XPMAXQ                                                        
         XC    BYTE1,BYTE1                                                      
RETURN30 OC    XPIDC1,XPIDC1       Any pid in storage                           
         JZ    RETURN32            No                                           
         TM    XPIDS1,PIDAPPQ      Yes - is it approved                         
         JNZ   RETURN32            Yes - no need for email                      
         MVC   0(L'PIDNO,R3),XPIDC1  No - need to send email                    
         LA    R3,3(R3)            Move to next free entry                      
         MVI   BYTE1,1             Set we found a person to email               
RETURN32 OC    XPIDC2,XPIDC2       Any pid in storage                           
         JZ    RETURN34            No                                           
         TM    XPIDS2,PIDAPPQ      Is it approved                               
         JNZ   RETURN34            Yes - no need for email                      
         MVC   0(L'PIDNO,R3),XPIDC2 No - need to send email                     
         LA    R3,3(R3)            Move to next free space in AIO6              
         MVI   BYTE1,1             Set we found a person to email               
RETURN34 CLI   BYTE1,0             Have we found anyone                         
         JE    RETURN36            No                                           
         CLI   T.PROEBY04,YESQ     Yes - sequential approval?                   
         JE    RETURN38            Yes - so when found 1 level exit             
RETURN36 LA    R2,XPIDLQ(R2)       Bump to next entry                           
         JCT   R4,RETURN30         repeat check                                 
         CLI   BYTE1,0             Any emails to send                           
         JE    RETURN40            No - skip embedded call                      
RETURN38 MVI   BYTE4,SE#ORDS                                                    
         GOTOR (#SEMAIL,ASEMAIL),DMCB,XORDOFF,BYTE4                             
         JNE   RETURN40                       CHECK WHETHER TO SEND             
         SR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN       Send approver run sequence                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRUN',A#TEAP)                
                                                                                
         MVI   BYTE1,C'6'          orders                                       
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',D#CAE),       +        
               ('LD_CHARQ',BYTE1),(L'BYTE1,0)                                   
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTERU',0),0,0                 
                                                                                
RETURN40 DS    0H                                                               
                                                                                
         J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* MQ routines - uses AGENAREA as data buffer                          *         
***********************************************************************         
         SPACE 1                                                                
MQROUT   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*13MQRT*'                                                      
                                                                                
         TM    XMQSTAT,XMQSMQQ+XMQSAPQ                                          
         JNO   EXIT                skip if no applicable                        
                                                                                
         USING ORDRECD,R2                                                       
         L     R2,AIO3                                                          
                                                                                
         MVC   MQHALPH,CUAALF                                                   
         MVC   MQHXORD,ORDKORD                                                  
         MVC   NEWONUM,ORDKORD                                                  
                                                                                
         L     R3,AGENAREA         clear data output area                       
         LR    R0,R3                                                            
         LHI   R1,GENAREAX-GENAREA-1                                            
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   0(MQHLNQ,R3),MQH                                                 
         AHI   R3,MQHLNQ                                                        
                                                                                
* set XML file header                                                           
                                                                                
         GOTOR XMLFHEAD                                                         
                                                                                
* set XML order download header                                                 
                                                                                
         GOTOR XMLGROUP,XO_DLODQ                                                
                                                                                
         GOTOR XMLGROUP,XO_GLOBQ   * set XML global header                      
                                                                                
* get order data and pass to XML (AIO1 for data in, AIO2 for data out)          
                                                                                
         LA    R4,CUAALF           -> agency alpha ID                           
         LHI   R1,L'CUAALF                                                      
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'AGID'                                               
         GOTOR MQSETXML                                                         
                                                                                
         LA    R4,NEWONUM          -> order number                              
         LHI   R1,L'NEWONUM                                                     
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'ONum'                                               
         GOTOR MQSETXML                                                         
                                                                                
         LA    R4,ORDROFF          -> order office                              
         LHI   R1,L'ORDROFF                                                     
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'OffC'                                               
         GOTOR MQSETXML                                                         
                                                                                
         LA    R4,ORDTYPE          -> order type                                
         LHI   R1,L'ORDTYPE                                                     
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'Type'                                               
         GOTOR MQSETXML                                                         
                                                                                
         LA    R4,ORDREXTY         -> expenditure type                          
         LHI   R1,L'ORDREXTY                                                    
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'ETyp'                                               
         GOTOR MQSETXML                                                         
                                                                                
         MVI   XELECOD,ORDELQ                                                   
         MVI   XELESUB,0                                                        
         GOTOR MQGELM,0                                                         
         JE    MQR02                                                            
         DC    H'0'                                                             
                                                                                
         USING ORDELD,R2                                                        
MQR02    LA    R4,TEMP2            -> order date                                
         GOTO1 VDATCON,DMCB,(1,ORDDATE),(13,TEMP2)                              
         LHI   R1,8                                                             
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'ODat'                                               
         GOTOR MQSETXML                                                         
                                                                                
         MVC   TEMP2(2),ORDCPID    -> order raiser                              
         GOTOR (#GETPID,AGETPID)                                                
         JNE   MQR06                                                            
         MVC   XONAME,SPACES                                                    
         MVC   XONAME(8),TEMP2                                                  
                                                                                
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   MQR04                                                            
         MVC   XONAME+9(16),TEMP2                                               
         MVC   XONAME+26(16),TEMP2+16                                           
                                                                                
MQR04    GOTO1 VSQUASH,DMCB,XONAME,L'XONAME                                     
         LA    R4,XONAME                                                        
         LLC   R1,DMCB+7                                                        
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'Rais'                                               
         GOTOR MQSETXML                                                         
                                                                                
MQR06    LA    R4,ORDSUPU          -> supplier account                          
         LHI   R1,L'ACTKULA                                                     
         LA    RF,0(R1,R4)                                                      
         SHI   RF,1                                                             
                                                                                
         CLI   0(RF),C' '                                                       
         JH    *+16                                                             
         SHI   RF,1                                                             
         JCT   R1,*-12                                                          
         AHI   R1,1                                                             
                                                                                
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'Supp'                                               
         GOTOR MQSETXML                                                         
                                                                                
         CLI   ORDTYPE,EXPOQ                                                    
         JNE   MQR08                                                            
         LA    R4,ORDACCU          -> expense account                           
         LHI   R1,L'ACTKULA                                                     
         LA    RF,0(R1,R4)                                                      
         SHI   RF,1                                                             
                                                                                
         CLI   0(RF),C' '                                                       
         JH    *+16                                                             
         SHI   RF,1                                                             
         JCT   R1,*-12                                                          
         AHI   R1,1                                                             
                                                                                
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'ExpA'                                               
         GOTOR MQSETXML                                                         
         J     MQR10                                                            
                                                                                
MQR08    LLC   R1,PCLILEN          -> client                                    
         LA    R4,ORDACCA                                                       
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'CliC'                                               
         GOTOR MQSETXML                                                         
                                                                                
         LLC   RF,PCLILEN          -> product                                   
         LA    R4,ORDACCA(RF)                                                   
         LLC   R1,PPROLEN                                                       
         SR    R1,RF                                                            
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'ProC'                                               
         GOTOR MQSETXML                                                         
                                                                                
         LLC   RF,PPROLEN          -> job                                       
         LA    R4,ORDACCA(RF)                                                   
         LLC   R1,PJOBLEN                                                       
         SR    R1,RF                                                            
         LA    RF,0(R1,R4)                                                      
         SHI   RF,1                                                             
                                                                                
         CLI   0(RF),C' '                                                       
         JH    *+16                                                             
         SHI   RF,1                                                             
         JCT   R1,*-12                                                          
         AHI   R1,1                                                             
                                                                                
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'JobC'                                               
         GOTOR MQSETXML                                                         
                                                                                
MQR10    OC    ORDRQBD,ORDRQBD     -> required by date                          
         JZ    MQR12                                                            
         LA    R4,TEMP2                                                         
         GOTO1 VDATCON,DMCB,(1,ORDRQBD),(13,TEMP2)                              
         LHI   R1,8                                                             
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'RqBy'                                               
         GOTOR MQSETXML                                                         
                                                                                
MQR12    MVI   XELECOD,ENMELQ                                                   
         MVI   XELESUB,0                                                        
         GOTOR MQGELM,0                                                         
         JNE   MQR14                                                            
                                                                                
         USING ENMELD,R2                                                        
         LA    R4,ENMNAME          -> order name                                
         LLC   R1,ENMLN                                                         
         SHI   R1,ENMLNQ                                                        
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'ONam'                                               
         GOTOR MQSETXML                                                         
                                                                                
MQR14    MVI   XELECOD,AFCELQ                                                   
         MVI   XELESUB,0                                                        
         GOTOR MQGELM,0                                                         
         JE    MQR16                                                            
         LA    R4,AGYCURR          -> currency code                             
         LHI   R1,L'AGYCURR                                                     
         NI    XMQSTAT,FF-XMQSFCQ                                               
         J     MQR18                                                            
                                                                                
MQR16    OI    XMQSTAT,XMQSFCQ                                                  
                                                                                
         USING AFCELD,R2                                                        
         LA    R4,AFCCURR                                                       
         LA    R1,L'AFCCURR                                                     
                                                                                
MQR18    MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'Curr'                                               
         GOTOR MQSETXML                                                         
                                                                                
         MVI   XELECOD,FFTELQ                                                   
         MVI   XELESUB,FFTTSATN                                                 
         GOTOR MQGELM,FFTTYPE-FFTELD                                            
         JNE   MQR20                                                            
                                                                                
         USING FFTELD,R2                                                        
         LA    R4,FFTDATA          -> attention to                              
         LLC   R1,FFTDLEN                                                       
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'Attn'                                               
         GOTOR MQSETXML                                                         
                                                                                
MQR20    MVI   XELECOD,SPAELQ                                                   
         MVI   XELESUB,SPATDEPT                                                 
         GOTOR MQGELM,SPATYPE-SPAELD                                            
         JNE   MQR22                                                            
                                                                                
         USING SPAELD,R2                                                        
         LA    R4,SPAAULA          -> department account                        
         LHI   R1,L'SPAAULA                                                     
         LA    RF,0(R1,R4)                                                      
         SHI   RF,1                                                             
                                                                                
         CLI   0(RF),C' '                                                       
         JH    *+16                                                             
         SHI   RF,1                                                             
         JCT   R1,*-12                                                          
         AHI   R1,1                                                             
                                                                                
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'Dept'                                               
         GOTOR MQSETXML                                                         
                                                                                
MQR22    MVI   XELECOD,SPAELQ                                                   
         MVI   XELESUB,SPATPERS                                                 
         GOTOR MQGELM,SPATYPE-SPAELD                                            
         JNE   MQR24                                                            
                                                                                
         USING SPAELD,R2                                                        
         LA    R4,SPAAULA          -> person/staff account                      
         LHI   R1,L'SPAAULA                                                     
         LA    RF,0(R1,R4)                                                      
         SHI   RF,1                                                             
                                                                                
         CLI   0(RF),C' '                                                       
         JH    *+16                                                             
         SHI   RF,1                                                             
         JCT   R1,*-12                                                          
         AHI   R1,1                                                             
                                                                                
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'Prsn'                                               
         GOTOR MQSETXML                                                         
                                                                                
MQR24    MVI   XELECOD,OATELQ                                                   
         MVI   XELESUB,OATSUB5Q                                                 
         GOTOR MQGELM,OATSUB-OATELD                                             
         JNE   MQR26                                                            
                                                                                
         USING OATELD,R2                                                        
         LA    R4,OATDABOX         -> delivery address                          
         LLC   R1,OATNUM                                                        
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'Dlvr'                                               
         GOTOR MQSETXML                                                         
                                                                                
MQR26    MVI   XELECOD,SCMELQ                                                   
         MVI   XELESUB,SCMTSTND                                                 
         GOTOR MQGELM,SCMTYPE-SCMELD                                            
         JNE   MQR28                                                            
                                                                                
         USING SCMELD,R2                                                        
         LA    R4,SCMNARR          -> printed description                       
         LLC   R1,SCMLN                                                         
         SHI   R1,SCMLN1Q          (<NL> and <T> remain)                        
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'Prnt'                                               
         GOTOR MQSETXML                                                         
                                                                                
MQR28    DS    0H                                                               
                                                                                
         GOTOR XMLGROUP,XC_GLOBQ   * close order global data                    
                                                                                
         GOTOR XMLGROUP,XO_WGRPQ   * set XML work code grouping header          
                                                                                
         ZAP   XCOUNT,PZERO                                                     
         MVI   XELECOD,OAMELQ                                                   
         MVI   XELESUB,0                                                        
         GOTOR MQGELM,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING OAMELD,R2                                                        
MQR40    GOTOR XMLGROUP,XO_WCODQ   set work code entry header                   
                                                                                
         AP    XCOUNT,PONE                                                      
         EDIT  (P4,XCOUNT),(5,TEMP2),0,ALIGN=LEFT                               
         LA    R4,TEMP2                                                         
         LR    R1,R0                                                            
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'WCnt'  -> counter                                   
         GOTOR MQSETXML                                                         
                                                                                
         CLI   ORDTYPE,EXPOQ                                                    
         JE    MQR42                                                            
         LA    R4,OAMWORK                                                       
         LHI   R1,L'OAMWORK                                                     
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'WCod'  -> work code                                 
         GOTOR MQSETXML                                                         
                                                                                
MQR42    EDIT  (P6,OAMAMNT),(12,TEMP2),0,ALIGN=LEFT,ZERO=NOBLANK                
         LA    R4,TEMP2                                                         
         LR    R1,R0                                                            
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'WAmt'  -> amount                                    
         GOTOR MQSETXML                                                         
                                                                                
         TM    XMQSTAT,XMQSFCQ                                                  
         JZ    MQR44                                                            
                                                                                
         EDIT  (P6,OAMFCAMT),(12,TEMP2),0,ALIGN=LEFT,ZERO=NOBLANK               
         LA    R4,TEMP2                                                         
         LR    R1,R0                                                            
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'WCur'  -> currency amount                           
         GOTOR MQSETXML                                                         
                                                                                
MQR44    DS    0H                                                               
         GOTOR XMLGROUP,XC_WCODQ   close work code entry                        
                                                                                
         CLI   ORDTYPE,EXPOQ                                                    
         JE    MQR48                                                            
                                                                                
MQR46    LLC   R0,OAMLN            look for other work code entries             
         AR    R2,R0                                                            
         CLI   OAMEL,0                                                          
         JE    MQR48                                                            
         CLI   OAMEL,OAMELQ                                                     
         JNE   MQR46                                                            
         J     MQR40                                                            
                                                                                
MQR48    GOTOR XMLGROUP,XC_WGRPQ   * close work code grouping                   
                                                                                
         USING ORDRECD,R2                                                       
         L     R1,AIO3             extension record for XDATA                   
         MVC   IOKEY,0(R1)                                                      
         LA    R2,IOKEY                                                         
         MVI   ORDKSEQ,ORDKEXTN                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   XELECOD,XDFELQ                                                   
         MVI   XELESUB,0                                                        
         GOTOR MQGELM,0                                                         
         JNE   MQR70                                                            
         ZAP   XCOUNT,PZERO                                                     
                                                                                
         GOTOR XMLGROUP,XO_XDGRQ   * set XML XData grouping header              
                                                                                
MQR50    GOTOR XMLGROUP,XO_XDATQ   * set XML XData item header                  
                                                                                
         USING XDFELD,R2                                                        
         AP    XCOUNT,PONE                                                      
         EDIT  (P4,XCOUNT),(5,TEMP2),0,ALIGN=LEFT                               
         LA    R4,TEMP2                                                         
         LR    R1,R0                                                            
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'XCnt'  -> counter                                   
         GOTOR MQSETXML                                                         
                                                                                
         CLI   XDFOTYP,XDFEDNQ     N=numeric                                    
         JE    MQR52                                                            
         CLI   XDFOTYP,XDFEDCQ     C=char                                       
         JNE   MQR54                                                            
                                                                                
MQR52    LLC   RE,XDFLN                                                         
         SHI   RE,XDFOLNQ+1                                                     
         BASR  R1,0                                                             
         MVC   XONAME(0),XDFODTA                                                
         EX    RE,0(R1)                                                         
         AHI   RE,1                                                             
         LR    R1,RE                                                            
         J     MQR60                                                            
                                                                                
MQR54    CLI   XDFOTYP,XDFEDDQ     D=DATE                                       
         JNE   MQR56                                                            
         GOTOR VDATCON,DMCB,(1,XDFODTA),(13,XONAME)                             
         LHI   R1,8                                                             
         J     MQR60                                                            
                                                                                
MQR56    CLI   XDFOTYP,XDFEDYQ     Y=YES/NO                                     
         JNE   MQR58                                                            
         MVC   XONAME(1),XDFODTA                                                
         LHI   R1,1                                                             
         J     MQR60                                                            
                                                                                
MQR58    CLI   XDFOTYP,XDFEDAQ     A=AMOUNT                                     
         JNE   MQR64                                                            
         CURED (P6,XDFODTA),(20,XONAME),0,ZERO=YES,ALIGN=LEFT                   
         LR    R1,R0                                                            
                                                                                
MQR60    LA    R4,XONAME                                                        
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'XVal'  -> XData value                               
         GOTOR MQSETXML                                                         
                                                                                
         GOTOR MQGETXDF                                                         
                                                                                
         LA    R4,TEMP2+1                                                       
         LLC   R1,TEMP2                                                         
         MVI   TEMP,4                                                           
         MVC   TEMP+1(4),=C'XDsc'  -> description                               
         GOTOR MQSETXML                                                         
                                                                                
MQR62    DS    0H                                                               
                                                                                
MQR64    GOTOR XMLGROUP,XC_XDATQ   * close XData item                           
                                                                                
MQR66    LLC   R0,XDFLN                                                         
         AR    R2,R0                                                            
         CLI   XDFEL,0                                                          
         JE    MQR68                                                            
         CLI   XDFEL,XDFELQ                                                     
         JE    MQR50                                                            
         J     MQR66                                                            
                                                                                
MQR68    GOTOR XMLGROUP,XC_XDGRQ   * close XData grouping                       
                                                                                
MQR70    DS    0H                                                               
                                                                                
* close order download                                                          
                                                                                
         GOTOR XMLGROUP,XC_DLODQ                                                
                                                                                
         LR    RF,R3               determine overall length                     
         L     R3,AGENAREA                                                      
         SR    RF,R3                                                            
                                                                                
* send to MQ                                                                    
                                                                                
         GOTO1 VMQIO,DMCB,MQPUT,(R3),(RF)                                       
         CLI   0(R1),0                                                          
         JE    MQR80               0 if OK, 1 if any error                      
         DC    H'0'                                                             
                                                                                
MQR80    DS    0H                                                               
                                                                                
*** For testing purposes prepare EMail                                          
                                                                                
         USING SMTPD,R3                                                         
         LA    R3,DMCB                                                          
                                                                                
         LA    R1,CSVKEY1          60/FF - send to                              
         ST    R1,SMTPTO                                                        
         MVC   CSVKEY1,SPACES                                                   
         MVC   CSVKEY1(25),=CL25'DATEN@DDS.DE'                                  
         MVI   CSVKEY1+60,FF                                                    
                                                                                
         XC    SMTPCC,SMTPCC       60/FF - send to cc                           
*        LA    R1,CSVKEY2                                                       
*        ST    R1,SMTPCC                                                        
*        MVC   CSVKEY2(25),=CL25'THOMAS.KLUTH@DDS.DE'                           
*        MVI   CSVKEY2+60,FF                                                    
                                                                                
         XC    SMTPBCC,SMTPBCC     60/FF - send to bcc                          
                                                                                
*        SMTPFROM defaults to DoNotReply@donovandata.com                        
         XC    SMTPFROM,SMTPFROM                                                
                                                                                
         L     R1,AIO1             70 - subject                                 
         ST    R1,SMTPSUB                                                       
         MVC   0(70,R1),SPACES                                                  
         MVC   0(36,R1),=C'Order ...... has been fully approved'                
         MVC   6(6,R1),NEWONUM                                                  
                                                                                
         L     R1,AIO1             80/FF                                        
         AHI   R1,250                                                           
         ST    R1,SMTPDATA                                                      
         MVC   0(80,R1),SPACES                                                  
         MVC   0(21,R1),=C'MQ processed, header:'                               
         MVC   22(MQHLNQ,R1),MQH                                                
         AHI   R1,80                                                            
         L     RF,AGENAREA         (send some of the output/XML data)           
         AHI   RF,MQHLNQ                                                        
         MVC   0(80,R1),0(RF)                                                   
                                                                                
         LHI   RE,10               (send more lines of data)                    
                                                                                
MQR82    AHI   R1,80                                                            
         AHI   RF,80                                                            
         MVC   0(80,R1),0(RF)                                                   
         JCT   RE,MQR82                                                         
                                                                                
         AHI   R1,80                                                            
         MVI   0(R1),FF            (set end marker)                             
                                                                                
         GOTO1 VJESMAIL,SMTPD                                                   
         DROP  R3                                                               
                                                                                
*** EMail send                                                                  
                                                                                
MQRX     DS    0H                                                               
         J     EXIT                                                             
         DROP  R2                                                               
***********************************************************************         
* Get an element from order record                                    *         
***********************************************************************         
         SPACE 1                                                                
MQGELM   L     R2,AIO3                                                          
         AHI   R2,ORDRFST-ORDRECD                                               
         XR    R0,R0                                                            
         LR    RF,R1                                                            
                                                                                
MQGELM2  CLI   0(R2),0                                                          
         JE    MQGELMN                                                          
         CLC   XELECOD,0(R2)                                                    
         JNE   MQGELM4                                                          
         CLI   XELESUB,0                                                        
         BER   RE                                                               
         LA    R1,0(RF,R2)                                                      
         CLC   XELESUB,0(R1)                                                    
         BER   RE                                                               
                                                                                
MQGELM4  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         J     MQGELM2                                                          
                                                                                
MQGELMN  LTR   R2,R2                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* set XML call parameters                                             *         
***********************************************************************         
         SPACE 1                                                                
MQSETXML ST    RE,SAVERE           pass to XMLITEM                              
                                                                                
         L     RF,AIO1                                                          
         STH   R1,0(RF)                                                         
         AHI   RF,2                                                             
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,RF),0(R4)                                                    
         EX    R1,0(RE)                                                         
         L     RF,AIO2                                                          
         LHI   R1,IOLENQ                                                        
         STH   R1,0(RF)                                                         
         GOTOR XMLITEM,DMCB,AIO1,AIO2,(TEMP,TEMP+1)                             
         L     RF,AIO2                                                          
         LH    R1,0(RF)                                                         
         AHI   RF,2                                                             
                                                                                
         LR    RE,RF               add to overall buffer                        
         LR    R0,R3                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     RF,AIO2             and point to next data area                  
         LH    R1,0(RF)                                                         
         AR    R3,R1                                                            
                                                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
* get description of XData field into TEMP2                                     
                                                                                
MQGETXDF ST    RE,SAVERE                                                        
                                                                                
         MVI   TEMP2,FF                                                         
         MVC   TEMP2+1(L'XDFNAME),SPACES                                        
                                                                                
         USING XDFPASD,RF                                                       
         L     RF,AIO1                                                          
         LA    RF,IOKEY                   READ                                  
         XC    IOKEY,IOKEY                                                      
         MVI   XDFPTYP,XDFPTYPQ                                                 
         MVI   XDFPSUB,XDFPSUBQ                                                 
         MVC   XDFPCPY,CUXCPY                                                   
         MVC   XDFPPTR(L'XDFOCOD),XDFOCOD-XDFELD(R2)                            
         MVC   CSVKEY1,IOKEY                                                    
                                                                                
MQGETX02 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
                                                                                
         CLC   CSVKEY1(XDFPSEQ-XDFPASD),IOKEY                                   
         JNE   MQGETXX                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
                                                                                
         USING XDFRECD,RF                                                       
         L     RF,AIO1                                                          
         LA    RF,XDFRFST                                                       
         XR    R0,R0                                                            
                                                                                
         USING XDFELD,RF                                                        
MQGETX04 CLI   XDFEL,0                                                          
         JE    MQGETXX                                                          
         CLI   XDFEL,XDFELQ                                                     
         JE    MQGETX08                                                         
                                                                                
MQGETX06 IC    R0,XDFLN                                                         
         AR    RF,R0                                                            
         J     MQGETX04                                                         
                                                                                
MQGETX08 CLC   XDFSEQ,XDFOCOD+2-XDFELD(R2)                                      
         JNE   MQGETX06                                                         
         LLC   R1,XDFLN                                                         
         SHI   R1,XDFLN1Q+1                                                     
         BASR  RE,0                                                             
         MVC   TEMP2+1(0),XDFNAME                                               
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         STC   R1,TEMP2                                                         
         DROP  RF                                                               
                                                                                
MQGETXX  L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
* set XML file header                                                           
                                                                                
XMLFHEAD NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13XMLFH'                                                      
                                                                                
         MVI   0(R3),XML_LEFT                                                   
         AHI   R3,1                                                             
         MVC   0(XMLFHLBQ,R3),XMLFHLBL                                          
         AHI   R3,XMLFHLBQ                                                      
         MVI   0(R3),XML_RIGT                                                   
         AHI   R3,1                                                             
         XIT1  REGS=(R3)                                                        
                                                                                
                                                                                
* set XML grouping tags                                                         
                                                                                
XMLGROUP NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13XMLGR'                                                      
                                                                                
         STC   R1,BYTE1                                                         
         STC   R1,BYTE2                                                         
         NI    BYTE2,FF-XO_XC_Q                                                 
                                                                                
         LA    R1,XOCTAB                                                        
                                                                                
XMLGR02  CLI   0(R1),0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE2,0(R1)                                                      
         JE    XMLGR04                                                          
         AHI   R1,XOCTABL                                                       
         J     XMLGR02                                                          
                                                                                
XMLGR04  MVI   0(R3),XML_LEFT                                                   
         AHI   R3,1                                                             
         TM    BYTE1,XO_XC_Q                                                    
         JZ    XMLGR06                                                          
         MVI   0(R3),XML_SLSH                                                   
         AHI   R3,1                                                             
                                                                                
XMLGR06  LLC   RF,1(R1)                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R3),2(R1)                                                    
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         AR    R3,RF                                                            
                                                                                
XMLGR08  MVI   0(R3),XML_RIGT                                                   
         AHI   R3,1                                                             
         XIT1  REGS=(R3)                                                        
                                                                                
* add XML coded data item to buffer                                             
                                                                                
XMLITEM  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13XMLIT'                                                      
                                                                                
* Para 1 = A(data in, starts with two byte data length)                         
* Para 2 = A(data out, in: area size, out: data length in bytes 1/2)            
* Para 3 = Length of tag, A(tag)                                                
                                                                                
         LM    R2,R4,0(R1)         get parameters                               
         MVC   XML_PARM(XML_PARL),0(R1)                                         
                                                                                
         LR    R0,R3               clear output area                            
         XR    R1,R1                                                            
         ICM   R1,B'0011',0(R3)                                                 
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         AHI   R3,2                (reserved for length)                        
                                                                                
         MVI   0(R3),XML_LEFT      create <TAG>                                 
         AHI   R3,1                                                             
         LR    R1,R4                                                            
         SRL   R1,24                                                            
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,R3),0(R4)                                                    
         EX    R1,0(RF)                                                         
         AHI   R1,1                                                             
         AR    R3,R1                                                            
         MVI   0(R3),XML_RIGT                                                   
         AHI   R3,1                                                             
                                                                                
         XR    R4,R4               set input length for loop                    
         ICM   R4,B'0011',0(R2)                                                 
         AHI   R2,2                                                             
                                                                                
XMLIT02  LA    RE,XML_SCT          move byte by byte + replace specials         
                                                                                
XMLIT04  CLI   0(RE),FF                                                         
         JE    XMLIT08                                                          
         CLC   0(1,R2),0(RE)                                                    
         JE    XMLIT06                                                          
         AHI   RE,XML_SCTL                                                      
         J     XMLIT04                                                          
                                                                                
XMLIT06  LLC   RF,1(RE)                                                         
         BASR  R1,0                                                             
         MVC   0(0,R3),2(RE)                                                    
         EX    RF,0(R1)                                                         
         AHI   RF,1                                                             
         AR    R3,RF                                                            
         J     XMLIT10                                                          
                                                                                
XMLIT08  MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
                                                                                
XMLIT10  AHI   R2,1                                                             
         JCT   R4,XMLIT02          next byte                                    
                                                                                
         L     R4,XML_PARM+8       create </TAG>                                
         MVI   0(R3),XML_LEFT                                                   
         AHI   R3,1                                                             
         MVI   0(R3),XML_SLSH                                                   
         AHI   R3,1                                                             
         LR    R1,R4                                                            
         SRL   R1,24                                                            
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,R3),0(R4)                                                    
         EX    R1,0(RF)                                                         
         AHI   R1,1                                                             
         AR    R3,R1                                                            
         MVI   0(R3),XML_RIGT                                                   
         AHI   R3,1                                                             
                                                                                
         L     R1,XML_PARM+4       set output length                            
         SR    R3,R1                                                            
         SHI   R3,2                                                             
         STCM  R3,B'0011',0(R1)                                                 
                                                                                
         J     EXIT                                                             
         DROP  RB                                                               
*&&                                                                             
***********************************************************************         
* Validate status' for order upload                                   *         
***********************************************************************         
         SPACE 1                                                                
VALSTA   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALSTA*'                                                      
                                                                                
         L     RE,AGOXBLCK         Yes - see whether need to resubmit           
         LA    R2,RESBTAB                                                       
         USING RESBTABD,R2                                                      
                                                                                
         MVC   BYTE1,XOMATST                                                    
         NI    BYTE1,X'FF'-XGDRCVD                                              
VALSTA02 CLC   ROWKFLST,XOWRKST    Find match on workflow status                
         JNE   VALSTA20                                                         
         CLC   RMATSTAT,BYTE1      Find match on matching status                
         JNE   VALSTA20                                                         
         LH    RF,=AL2(GOFPORES-GOXBLKD)                                        
         XR    R1,R1               In Aura always use FPR                       
         ICM   R1,3,CUXPNUM                                                     
         CHI   R1,XPRODIKQ                                                      
         JE    *+8                                                              
         LH    RF,ROPTMAIT         RF=displacement to opt maint setting         
         AR    RF,RE                                                            
         CLC   ROPTVAL,0(RF)       Check opt maint value                        
         JNE   VALSTA20                                                         
         CLC   ROMCAO,XOMCAO       Check against change made                    
         JNE   VALSTA20                                                         
VALSTA04 OI    XAPPIND,XARESUB     Set we need to resubmit order                
         NI    XAPPIND,X'FF'-XAPRSNT Remove some approvals present              
         USING XPIDSD,R3                                                        
         LA    R3,XPVALS           Is connected user also approver?             
         LA    RF,XPMAXQ                                                        
VALSTA06 TM    XPIDS1,PIDAPPQ      Is the approver entry approved               
         JZ    VALSTA12            No                                           
         CLC   CCTPID,XPIDC1       Yes - check PID                              
         JE    VALSTA10            If match don't remove approval               
VALSTA08 NI    XPIDS1,X'FF'-PIDAPPQ                                             
         NI    XAPPIND,X'FF'-XAFULAP                                            
         J     *+8                                                              
VALSTA10 OI    XAPPIND,XAPRSNT                                                  
VALSTA12 TM    XPIDS2,PIDAPPQ      Is the approver entry approved               
         JZ    VALSTA18            No                                           
         CLC   CCTPID,XPIDC2       Yes - check PID                              
         JE    VALSTA16            If match don't remove approval               
VALSTA14 NI    XPIDS2,X'FF'-PIDAPPQ                                             
         NI    XAPPIND,X'FF'-XAFULAP                                            
         J     *+8                                                              
VALSTA16 OI    XAPPIND,XAPRSNT                                                  
VALSTA18 AHI   R3,XPIDLQ                                                        
         JCT   RF,VALSTA06                                                      
         J     VALSTA24                                                         
                                                                                
VALSTA20 LA    R2,RESBTABL(R2)                                                  
         CLI   0(R2),X'FF'                                                      
         JNE   VALSTA02                                                         
*                                                                               
VALSTA22 OI    XAPPIND,XANOCHG     Set no change                                
                                                                                
VALSTA24 LA    R2,NSTTAB                                                        
         USING NSTTABD,R2                                                       
         XR    R1,R1                                                            
VALSTA26 CLC   NOWKFLST,XOWRKST    Find match on old workflow status            
         JNE   VALSTA28                                                         
         CLC   NREQSTAT,XOMSTA     Find match on request staus                  
         JNE   VALSTA28                                                         
         CLC   NREQACTN,XOMACT     Find match on request action                 
         JNE   VALSTA28                                                         
         ICM   R1,1,NAPPROVL       Extract bit setting needed to test           
         JZ    VALSTA32            No bit to test assume good                   
         EXRL  R1,*+10                                                          
         JNZ   VALSTA32                                                         
         TM    XAPPIND,0                                                        
                                                                                
VALSTA28 LA    R2,NSTTABDL(R2)                                                  
         CLI   0(R2),X'FF'         End of table                                 
         JNE   VALSTA26            No                                           
                                                                                
         CLI   XOMSTA,RQDELETD     Are we trying to delete the order            
         JNE   VALSTA30                                                         
         MVC   ROUERRV,=AL2(AE$IVODL) Invalid order for deletion                
         J     VALSTAN                                                          
                                                                                
VALSTA30 MVC   ROUERRV,=AL2(AE$INTST) Yes invalid entry                         
         J     VALSTAN                                                          
                                                                                
VALSTA32 MVC   XNOSTA1,NORDST1     Set order rec status byte 1                  
         TM    XOMATST,XGDRCVD     Test whether goods received                  
         JZ    *+8                                                              
         OI    XNOSTA1,ORDGDRCV    Set goods received                           
         MVC   XNOSTA2,NORDST2     Set order rec status byte 2                  
                                                                                
         MVC   XNWRKST,NNWKFLST    Set audit workflow staus                     
         CLI   XNWRKST,XCMPLETE    Is the new workflow complete                 
         JNE   VALSTA34            No                                           
         TM    XORDSTA,ORDSMNUP    Test if fully matched                        
         JNZ   VALSTA34            Don't need to set closed                     
         TM    XORDSTA,ORDSPART                                                 
         JZ    VALSTA33                                                         
         OI    XNOSTA1,ORDCLOSE    Set closed for part matched                  
         J     VALSTA34                                                         
                                                                                
VALSTA33 MVI   XNWRKST,XCANCELD    if unmatched set as cancelled                
                                                                                
VALSTA34 OC    XORDIND,NBEHAVOR    Set behaviour we need to follow              
         XR    RF,RF                                                            
         MVC   XOUTSTA,NOUTSTAT                                                 
         CLI   XOUTSTA,RQFULAPP                                                 
         JNE   EXITY                                                            
         TM    XOMATST,XGDRCVD     Test whether goods received                  
         JZ    EXITY                                                            
         MVI   XOUTSTA,RQRECVD     Set output as received                       
         J     EXITY                                                            
                                                                                
VALSTAY  LHI   RE,1                                                             
         J     VALSTAC                                                          
VALSTAN  LHI   RE,0                                                             
VALSTAC  CHI   RE,1                                                             
VALSTAX  XIT1  ,                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Routines for adding a new order for order upload                    *         
***********************************************************************         
         SPACE 1                                                                
NEWORD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*NEWORD*'                                                      
                                                                                
         XC    NEWRNUM,NEWRNUM                                                  
         MVC   TEMP(L'XERRTXT),SPACES                                           
                                                                                
         MVI   XREQNUM,NOQ         Requisition number by order type             
         LA    RE,RNSAREQ                                                       
         CLI   ORDTYPE,EXPOQ                                                    
         JE    NEWOR000                                                         
         LA    RE,RNSARPQ                                                       
         CLI   ORDTYPE,PROOQ                                                    
         JE    NEWOR000                                                         
         LA    RE,RNSARIQ                                                       
         CLI   ORDTYPE,INTOQ                                                    
         JE    NEWOR000                                                         
         LA    RE,RNSARAQ                                                       
                                                                                
NEWOR000 BASR  RF,0                                                             
         TM    T.RNSAIND,0                                                      
         EX    RE,0(RF)                                                         
         JZ    NEWOR001                                                         
         MVI   XREQNUM,YESQ        Requisition number in use                    
                                                                                
         USING ORDRECD,R2                                                       
NEWOR001 LA    R2,IOKEY            read for order control record                
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUXCPY                                                   
         MVC   ORDKORD,=C'000000'                                               
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    NEWOR010                                                         
                                                                                
NEWOR005 MVC   ROUERRV,=AL2(AE$OCMIS)                                           
         J     EXITN                                                            
                                                                                
NEWOR010 L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING ONCELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,ORDRFST                                                       
         XR    R0,R0                                                            
                                                                                
NEWOR015 CLI   ONCEL,0                                                          
         JE    NEWOR005                                                         
         CLI   ONCEL,ONCELQ                                                     
         JE    NEWOR020                                                         
         IC    R0,ONCLN                                                         
         AR    R3,R0                                                            
         J     NEWOR015                                                         
                                                                                
NEWOR020 MVI   XONSRCE,C'C'        order number source is control               
         MVC   LSTONUM,ONCNUM      increment ONCNUM on update                   
         MVC   NEWONUM,ONCNUM                                                   
*        CLI   PROEBY09,YESQ       Req. # in use?                               
         CLI   XREQNUM,YESQ                                                     
         JNE   NEWOR025                                                         
         MVC   LSTRNUM,ONCLREQ                                                  
         CLI   ONCLN,ONCLNRQ                                                    
         JNL   NEWOR025                                                         
         MVC   LSTRNUM,=C'000000'                                               
         MVI   ONCEL,FF                                                         
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ONCEL,ONCELQ                                                     
         MVI   ONCLN,ONCLNRQ                                                    
         MVC   ONCNUM,LSTONUM                                                   
         MVC   ONCLREQ,LSTRNUM                                                  
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ORDRECD,ONCELD                         
         CLI   12(R1),0                                                         
         JNE   NEWOR005                                                         
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('FF',ORDRECD),0                       
         CLI   12(R1),0                                                         
         JNE   NEWOR005                                                         
                                                                                
         USING ONARECD,R2                                                       
NEWOR025 LA    R2,IOKEY            ** READ FOR ORDER # ALLOCATION **            
         XC    ONAKEY,ONAKEY                                                    
         MVI   ONAKTYP,ONAKTYPQ                                                 
         MVI   ONAKSUB,ONAKSUBQ                                                 
         MVC   ONAKCPY,CUXCPY                                                   
         MVI   ONAKAPP,ONAKAEQ                                                  
         MVC   ONAKCLS,ORDTYPE                                                  
         MVC   ONAKOFF,XORDOFF     office level first (if set)                  
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    NEWOR030                                                         
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   ONAKCLS,ONAKCDF                                                  
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    NEWOR030                                                         
         CLC   XORDOFF,SPACES                                                   
         JE    NEWOR060                                                         
         MVC   IOKEY,IOKEYSAV                                                   
         MVC   ONAKCLS,ORDTYPE                                                  
         MVC   ONAKOFF,SPACES                                                   
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    NEWOR030                                                         
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   ONAKCLS,ONAKCDF                                                  
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   NEWOR060                                                         
                                                                                
NEWOR030 MVI   XONSRCE,C'O'        order number source is office                
         CLC   ONAKOFF,SPACES                                                   
         JNE   *+8                                                              
         MVI   XONSRCE,C'A'        order number source is agency                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   NEWOR030                                                         
                                                                                
         USING ONCELD,R3                                                        
         L     R2,AIO2                                                          
         LA    R3,ONARFST                                                       
         XR    R0,R0                                                            
                                                                                
NEWOR035 CLI   ONCEL,ONCELQ                                                     
         JE    NEWOR040                                                         
         CLI   ONCEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,ONCLN                                                         
         AR    R3,R0                                                            
         J     NEWOR035                                                         
                                                                                
NEWOR040 MVC   XPREORD,ONCNXT#     use INCORD to get higher number              
         OC    ONCNXT#,ONCNXT#                                                  
         JNZ   *+10                                                             
         MVC   XPREORD,ONCSTA#                                                  
         MVC   NEWONUM,XPREORD                                                  
         GOTOR INCORD                                                           
         JE    NEWOR045                                                         
         MVC   ROUERRV,=AL2(AE$INAGN)                                           
         J     EXITN                                                            
                                                                                
NEWOR045 MVC   ONCNXT#,XNEWORD                                                  
         CLC   ONCNXT#,ONCEND#                                                  
         JNH   NEWOR090                                                         
         MVC   ROUERRV,=AL2(AE$INAGN)                                           
         J     EXITN                                                            
                                                                                
** numbers from control record - check for any allocation **                    
                                                                                
         USING ONAPASD,R2                                                       
NEWOR060 LA    R2,IOKEY                                                         
         XC    ONAPKEY,ONAPKEY                                                  
         MVI   ONAPTYP,ONAPTYPQ                                                 
         MVI   ONAPSUB,ONAPSUBQ                                                 
         MVC   ONAPCPY,CUXCPY                                                   
                                                                                
NEWOR065 MVC   ONAPEND#,NEWONUM    set new number into passive                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   ONAPKEY,IOKEYSAV                                                 
         JNE   NEWOR070                                                         
         TM    ONAPSTAT,ONAPSDLT                                                
         JNZ   NEWOR070                                                         
         CLC   ONAPSTA#,NEWONUM                                                 
         JH    NEWOR070                                                         
         MVC   NEWONUM,ONAPEND#    if in allocated region then                  
         MVC   XPREORD,ONAPEND#    get next higher number                       
         GOTOR INCORD                                                           
         JE    NEWOR065                                                         
         MVC   ROUERRV,=AL2(AE$INAGN)                                           
         J     EXITN                                                            
                                                                                
         USING ORDRECD,R2                                                       
NEWOR070 L     R2,AIO1                                                          
         LA    R3,ORDRFST                                                       
         XR    R0,R0                                                            
                                                                                
NEWOR075 CLI   ONCEL,ONCELQ                                                     
         JE    NEWOR080                                                         
         IC    R0,ONCLN                                                         
         AR    R2,R0                                                            
         J     NEWOR075                                                         
                                                                                
NEWOR080 MVC   XPREORD,NEWONUM                                                  
         GOTOR INCORD                                                           
         JE    NEWOR085                                                         
         MVC   ROUERRV,=AL2(AE$INAGN)                                           
         J     EXITN                                                            
                                                                                
NEWOR085 MVC   ONCNUM,XNEWORD      save new number in control record            
         CLI   XREQNUM,YESQ        Req number in use                            
         JE    NEWOR100                                                         
         J     NEWOR110                                                         
                                                                                
NEWOR090 CLI   XREQNUM,YESQ        Req number is use                            
         JNE   NEWOR110                                                         
         L     R2,AIO1                                                          
         LA    R3,ORDRFST                                                       
         XR    R0,R0                                                            
                                                                                
NEWOR095 CLI   ONCEL,ONCELQ                                                     
         JE    NEWOR100                                                         
         IC    R0,ONCLN                                                         
         AR    R3,R0                                                            
         J     NEWOR095                                                         
                                                                                
NEWOR100 MVC   XPREORD,LSTRNUM                                                  
         GOTOR INCORD                                                           
         JE    NEWOR105                                                         
         MVC   ROUERRV,=AL2(AE$INAGN)                                           
         J     EXITN                                                            
                                                                                
NEWOR105 MVC   NEWRNUM,XNEWORD     save new req no in control record            
         MVC   ONCLREQ,NEWRNUM                                                  
                                                                                
NEWOR110 ZAP   XCOUNT,PZERO        do max of 50 ios in this loop                
                                                                                
NEWOR115 LA    R2,IOKEY            order number already used?                   
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUXCPY                                                   
         MVC   ORDKORD,NEWONUM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   NEWOR140                                                         
         AP    XCOUNT,PONE                                                      
         CP    XCOUNT,PMAX                                                      
         JNH   NEWOR120                                                         
         MVC   ROUERRV,=AL2(AE$INAGN)                                           
         J     EXITN                                                            
                                                                                
NEWOR120 MVC   XPREORD,NEWONUM     get next one                                 
         GOTOR INCORD                                                           
         JE    NEWOR125                                                         
         MVC   ROUERRV,=AL2(AE$INAGN)                                           
         J     EXITN                                                            
                                                                                
NEWOR125 MVC   NEWONUM,XNEWORD                                                  
         CLI   XONSRCE,C'C'        number from control?                         
         JE    NEWOR060                                                         
                                                                                
         USING ONARECD,R2                                                       
         L     R2,AIO2             number from order # allocation               
         LA    R3,ONARFST                                                       
         XR    R0,R0                                                            
                                                                                
NEWOR130 CLI   ONCEL,ONCELQ                                                     
         JE    NEWOR135                                                         
         IC    R0,ONCLN                                                         
         AR    R3,R0                                                            
         J     NEWOR130                                                         
                                                                                
NEWOR135 MVC   ONCNXT#,NEWONUM                                                  
         CLC   ONCNXT#,ONCEND#                                                  
         JNH   NEWOR115                                                         
         MVC   ROUERRV,=AL2(AE$INAGN)                                           
         J     EXITN                                                            
                                                                                
NEWOR140 OC    NEWRNUM,NEWRNUM     write back order control?                    
         JZ    NEWOR145                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    NEWOR145                                                         
         DC    H'0'                                                             
                                                                                
NEWOR145 CLI   XONSRCE,C'C'                                                     
         JE    EXITY                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    EXITY                                                            
         DC    H'0'                                                             
***********************************************************************         
* Increment (order) number by 1 (numeric/alphanumeric)                *         
***********************************************************************         
         SPACE 1                                                                
INCORD   ST    RE,SAVERE                                                        
         MVC   XNEWORD,XPREORD                                                  
         J     INCORD50            use numeric only, alphanum. code:            
*        order # = 1 char office plus 5 char alphanumeric = 70 million          
*        available no.s per off., if numeric only then 100K per off.)           
         LA    RE,XPREORD+L'XPREORD-1                                           
         LA    RF,XNEWORD+L'XNEWORD-1                                           
         LA    R0,L'XNEWORD-1                                                   
         CLI   XONSRCE,C'A'        office related?                              
         JE    *+8                                                              
         AHI   R0,1                                                             
         XR    R1,R1                                                            
                                                                                
INCORD10 IC    R1,0(RE)                                                         
         AHI   R1,1                                                             
         CHI   R1,X'CA'                                                         
         JE    INCORD20                                                         
         CHI   R1,X'DA'                                                         
         JE    INCORD20                                                         
         CHI   R1,X'EA'                                                         
         JE    INCORD20                                                         
         CHI   R1,X'FA'                                                         
         JNE   INCORD30                                                         
         LA    R1,X'C1'                                                         
         STC   R1,0(RF)                                                         
         SHI   RE,1                                                             
         SHI   RF,1                                                             
         JCT   R0,INCORD10                                                      
         J     INCORDN             overflow                                     
                                                                                
INCORD20 SHI   R1,X'DA'-X'E1'                                                   
                                                                                
INCORD30 STC   R1,0(RF)                                                         
         J     INCORDY                                                          
                                                                                
INCORD50 XR    R0,R0               set to naught                                
         CLI   XONSRCE,C'A'        office related?                              
         JE    INCORD60                                                         
         CLI   XNEWORD,C'0'        Is first char a number?                      
         JNL   INCORD70                                                         
                                                                                
INCORD60 IC    R0,XNEWORD          save off first digit                         
         MVI   XNEWORD,C'0'                                                     
                                                                                
INCORD70 PACK  DUB,XNEWORD                                                      
         CVB   R1,DUB                                                           
         AHI   R1,1                                                             
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XNEWORD,DUB                                                      
         LTR   R0,R0                                                            
         JZ    INCORDY                                                          
         STC   R0,XNEWORD                                                       
         CLC   XNEWORD(1),XPREORD                                               
         JNE   INCORDN                                                          
                                                                                
INCORDY  L     RE,SAVERE                                                        
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
INCORDN  L     RE,SAVERE                                                        
         LTR   RE,RE                                                            
         BR    RE                                                               
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Validate internal order details                                     *         
***********************************************************************         
         SPACE 1                                                                
VALINT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALINT*'                                                      
                                                                                
         CLI   ORDTYPE,INTOQ       only for internal orders                     
         JNE   EXITY                                                            
         CLC   XSUPCULA+3(12),SPACES                                            
         JH    EXITY               and unless supplier passed                   
                                                                                
         MVC   XSUPCULA(1),CUXCPY  preset fro if not found                      
         MVI   XSUPCULA+1,C'S'                                                  
         MVC   XSUPCULA+2(1),XINTLED                                            
         MVC   TEMP(L'XERRTXT),SPACES                                           
                                                                                
         CLI   T.PROEBY11,C'W'     A/C from Work Code?                          
         JNE   VALINT10                                                         
                                                                                
         USING GOBBLKD,R2                                                       
         L     R2,AGOBBLCK                                                      
         MVC   XSUPCULA,GOINCAC    get and save income account                  
         J     VALINT50                                                         
         DROP  R2                                                               
                                                                                
VALINT10 CLC   XJOBC,SPACES        A/C from Media Code                          
         JH    VALINT15                                                         
         MVC   ROUERRV,=AL2(AE$MCMIS)                                           
         J     EXITN                                                            
                                                                                
         USING PMDRECD,R2                                                       
VALINT15 LA    R2,IOKEY                                                         
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUXCPY                                                   
         MVC   PMDKMED,XJOBC                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALINT20                                                         
         MVC   ROUERRV,=AL2(AE$MCMIS)                                           
         J     EXITN                                                            
                                                                                
VALINT20 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING PMDELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,PMDRFST                                                       
         XR    R0,R0                                                            
                                                                                
VALINT25 CLI   PMDEL,PMDELQ                                                     
         JE    VALINT30                                                         
         CLI   PMDEL,0                                                          
         JE    VALINT50                                                         
         IC    R0,PMDLN                                                         
         AR    R3,R0                                                            
         J     VALINT25                                                         
                                                                                
VALINT30 CLI   PMDLN,PMDLN2Q                                                    
         JL    VALINT50                                                         
         MVC   XSUPCULA,PMDCOM2                                                 
         DROP  R2,R3                                                            
                                                                                
VALINT50 CLC   XSUPCULA+3(12),SPACES   set and validate SI/SK supplier          
         JH    VALINT55                                                         
         MVC   ROUERRV,=AL2(AE$MIACC)                                           
         MVC   TEMP(5),=C'SI/SK'                                                
         J     EXITN                                                            
                                                                                
VALINT55 MVC   XSUPCULA+2(1),XINTLED   ensure correct ledger                    
                                                                                
VALINT60 MVC   XSUPCURR,AGYCURR                                                 
         MVC   TEMP(14),XSUPCULA+1                                              
         GOTOR VALSUP,1                                                         
         JNE   EXITN               on error TEMP is set to error text           
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Validate expense order details                                      *         
***********************************************************************         
         SPACE 1                                                                
VALEXT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALEXT*'                                                      
                                                                                
         CLI   ORDTYPE,EXPOQ       only for internal orders                     
         JNE   EXITY                                                            
         USING GOXBLKD,RF                                                       
         L     RF,AGOXBLCK                                                      
         CLI   GONJLE,C'Y'         Do we allow job entry to expense             
         JE    EXITY               Yes                                          
         DROP  RF                                                               
                                                                                
         CLC   XJOBC,SPACES        Do we have a job input                       
         JNH   EXITY                                                            
         MVC   ROUERRV,=AL2(AE$JBNAL)                                           
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Set 'attention to', matching and printing description, etc          *         
***********************************************************************         
         SPACE 1                                                                
SETTXT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETTXT*'                                                      
                                                                                
         MVC   XOSVAUT,RQOMAUT     save off certain values                      
         MVC   XOSVRBD,RQOMRBD                                                  
         MVC   XOSVEXC,RQOMEXC                                                  
         MVC   XOSVPRA,RQOMPRA                                                  
         MVC   XOSVPRT,RQOMPRT                                                  
*  initialise header text save area to 'not supplied in header'                 
         MVI   XPRILEN,FF          printed description                          
         MVI   XSTCLN,FF                                                        
         MVI   XNAMLN,FF           order name                                   
         MVI   XDALNN,FF           Delivery address                             
*                                                                               
         MVI   XATOLEN,FF                                                       
         CLC   RQOMATO,SPACES                                                   
         JNH   SETTXT06                                                         
                                                                                
         MVC   XATOTXT,RQOMATO                                                  
         LA    R2,RQOMATO+L'RQOMATO-1                                           
         LA    R3,L'RQOMATO                                                     
                                                                                
SETTXT02 CLI   0(R2),X'40'                                                      
         JH    SETTXT04                                                         
         SHI   R2,1                                                             
         JCT   R3,SETTXT02                                                      
         DC    H'0'                (cannot be empty string)                     
                                                                                
SETTXT04 SHI   R3,1                                                             
         STC   R3,XATOLEN          set length of attention                      
                                                                                
SETTXT06 MVI   XMATLEN,FF                                                       
         OC    RQOMMTXL,RQOMMTXL                                                
         JNZ   SETTXT08                                                         
*&&US*&& J     SETTXT12                                                         
*&&UK                                                                           
         CLC   CUAALF,=C'NS'       For M&C Saatchi check matching desc          
         JNE   SETTXT12                 is present                              
         CLI   RQOMACT,RQOMA3Q     unless status change only                    
         JE    SETTXT12                (cannot save unless present)             
*                                                                               
         MVC   ROUERRV,=AL2(AE$MDSCC)                                           
         J     SETTXTN                                                          
*&&                                                                             
SETTXT08 MVC   XMATTXT,RQOMMTX                                                  
         MVC   XMATLEN,RQOMMTXL                                                 
                                                                                
SETTXT12 DS    0H                                                               
         OC    RQOMCTXL,RQOMCTXL                                                
         JZ    SETTXT18                                                         
                                                                                
         MVC   XPRITXT,RQOMCTX                                                  
         MVC   XPRILEN,RQOMCTXL                                                 
                                                                                
SETTXT18 DS    0H                                                               
         OC    RQOMCOML,RQOMCOML                                                
         JZ    SETTXT24                                                         
                                                                                
         MVC   XSTCCO,RQOMCOM                                                   
         MVC   XSTCLN,RQOMCOML                                                  
                                                                                
SETTXT24 DS    0H                                                               
         OC    RQOMNAML,RQOMNAML   Order Name                                   
         JZ    SETTXT30                                                         
                                                                                
         MVC   XONAME,RQOMNAM                                                   
         MVC   XNAMLN,RQOMNAML     set length of order name                     
                                                                                
SETTXT30 DS    0H                  Delivery Address                             
         OC    RQOMDADL,RQOMDADL                                                
         JZ    SETTXT36                                                         
                                                                                
         MVC   XDABOX,RQOMDAD                                                   
         MVC   XDALNN,RQOMDADL     set length of delivery address               
                                                                                
SETTXT36 J     EXITY                                                            
SETTXTN  J     EXITN                                                            
                                                                                
***********************************************************************         
* Set order date for order upload                                     *         
***********************************************************************         
         SPACE 1                                                                
SETODT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETODT*'                                                      
                                                                                
         OC    XORDDT,XORDDT                                                    
         JNZ   EXITY                                                            
         MVC   XORDDT,OR_TODP                                                   
         OC    RQOMDAT,RQOMDAT                                                  
         JZ    EXITY                                                            
         MVC   XORDDT,RQOMDAT                                                   
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Validate order report format for order upload                       *         
***********************************************************************         
         SPACE 1                                                                
VALRFM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALRFM*'                                                      
                                                                                
         CLC   RQOMRFM,SPACES                                                   
         JNH   EXITY                                                            
         MVC   XORDRFM,RQOMRFM                                                  
         OC    XORDRFM,SPACES                                                   
         USING ERFRECD,R2                                                       
         LA    R2,IOKEY            read for work code                           
         XC    ERFKEY,ERFKEY                                                    
         MVI   ERFKTYP,ERFKTYPQ                                                 
         MVI   ERFKSUB,ERFKSUBQ                                                 
         MVC   ERFKCPY,CUXCPY                                                   
         MVI   ERFKAPPL,ERFKORDQ                                                
         MVC   ERFKFRMT,XORDRFM                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    EXITY               (no test for locked)                         
         MVC   ROUERRV,=AL2(AE$INVFM)                                           
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Validate order 'owner' for order upload                             *         
***********************************************************************         
         SPACE 1                                                                
VALOWN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALOWN*'                                                      
                                                                                
         CLC   RQOWNER,SPACES      Have we got an order owner                   
         JNH   EXITY                                                            
                                                                                
         MVC   TEMP2(8),RQOWNER    8 byte character PID                         
         GOTOR (#GETPIN,AGETPIN)                                                
         OC    XORDOWNR,TEMP2+50   2 byte PIN from character PID                
         JNZ   EXITY                                                            
         MVC   ROUERRV,=AL2(AE$INPID)                                           
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Set total amount on expense order for order upload                  *         
***********************************************************************         
SETTOT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13STOT*'                                                      
                                                                                
         CLI   ORDTYPE,EXPOQ                                                    
         JNE   EXITY                                                            
         CLC   RQOMTOT,SPACES                                                   
         JNH   SETTOT2                                                          
                                                                                
         MVC   TEMP2(16),RQOMTOT                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   XTOTAMT,TEMP2+16(8)                                              
                                                                                
SETTOT2  CLC   RQOMTFC,SPACES                                                   
         JNH   SETTOT4                                                          
                                                                                
         MVC   TEMP2(16),RQOMTFC                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   XTOTFCA,TEMP2+16(8)                                              
                                                                                
         USING XWDSCT,R2                                                        
SETTOT4  LAY   R2,XWVALS                                                        
         MVC   XWCODE,SPACES                                                    
         ZAP   XWCAMT,XTOTAMT                                                   
         ZAP   XWCFCA,XTOTFCA                                                   
         ZAP   XWCIFCA,PZERO                                                    
         ZAP   XWCINUM,PZERO                                                    
         ZAP   XWCIVAL,PZERO                                                    
         XC    XWCIPND,XWCIPND                                                  
         MVI   XWCNTR,1                                                         
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Process approval and rejection                                      *         
* Set whether we have self approval and/or any outstanding approvals  *         
***********************************************************************         
         SPACE 1                                                                
PROAPP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PROAPP*'                                                      
         MVC   TEMP,SPACES                                                      
         CLI   XPFLAG,YESQ         any approver entry processed at all?         
         JNE   PROAPP06                                                         
                                                                                
         USING XPIDSD,R3                                                        
         LA    R3,XPVALS                                                        
         LA    RF,XPMAXQ                                                        
                                                                                
PROAPP02 TM    XPIDS1,PIDPROQ      was this entry processed                     
         JNZ   PROAPP04                                                         
         XC    XPIDS1(XPIDLQ),XPIDS1     (clear it if not)                      
                                                                                
PROAPP04 NI    XPIDS1,FF-PIDPROQ                                                
         AHI   R3,XPIDLQ                                                        
         JCT   RF,PROAPP02                                                      
*                                                                               
PROAPP06 OC    XOMSTA,XOMSTA       Self approval scanario?                      
         JZ    *+12                                                             
         CLI   XOMSTA,RQSUBMIT                                                  
         JNE   PROAPP30                                                         
         LA    RE,RNSASEQ          Self approval by order type                  
         CLI   ORDTYPE,EXPOQ                                                    
         JE    PROAPP12                                                         
         LA    RE,RNSASPQ                                                       
         CLI   ORDTYPE,PROOQ                                                    
         JE    PROAPP12                                                         
         LA    RE,RNSASIQ                                                       
         CLI   ORDTYPE,INTOQ                                                    
         JE    PROAPP12                                                         
         LA    RE,RNSASAQ                                                       
                                                                                
PROAPP12 BASR  RF,0                                                             
         TM    T.RNSAIND,0                                                      
         EX    RE,0(RF)                                                         
         JZ    PROAPP30            no self approval - skip                      
         OC    XORDSUB,XORDSUB                                                  
         JZ    PROAPP14                                                         
         XR    RF,RF               In Aura, approver can 'self approve'         
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ                                                      
         JE    PROAPP14                                                         
         CLC   XORDSUB,CCTPID      Is the submitter the connected               
         JNE   PROAPP30                                    user?                
PROAPP14 LA    R3,XPVALS           Is connected user also approver?             
         LA    RF,XPMAXQ                                                        
         MVI   BYTE1,NOQ                                                        
         MVI   BYTE2,NOQ                                                        
                                                                                
PROAPP16 TM    XPIDS1,PIDAPPQ                                                   
         JZ    PROAPP18                                                         
         OI    XAPPIND,XAPRSNT     Set some approvals present                   
         J     PROAPP22                                                         
PROAPP18 OC    XPIDC1,XPIDC1                                                    
         JZ    PROAPP22                                                         
         CLC   CCTPID,XPIDC1                                                    
         JNE   PROAPP20                                                         
         OI    XAPPIND,XASLFAP     self approval active on submit               
         OI    XPIDS1,PIDAPPQ                                                   
         MVI   BYTE2,YESQ                                                       
         J     PROAPP22                                                         
                                                                                
PROAPP20 MVI   BYTE1,YESQ                                                       
                                                                                
PROAPP22 TM    XPIDS2,PIDAPPQ                                                   
         JZ    PROAPP24                                                         
         OI    XAPPIND,XAPRSNT     Set some approvals present                   
         J     PROAPP28                                                         
PROAPP24 OC    XPIDC2,XPIDC2                                                    
         JZ    PROAPP28                                                         
         CLC   CCTPID,XPIDC2                                                    
         JNE   PROAPP26                                                         
         OI    XAPPIND,XASLFAP     self approval active on submit               
         OI    XPIDS2,PIDAPPQ                                                   
         MVI   BYTE2,YESQ                                                       
         J     PROAPP28                                                         
                                                                                
PROAPP26 MVI   BYTE1,YESQ                                                       
                                                                                
PROAPP28 AHI   R3,XPIDLQ                                                        
         JCT   RF,PROAPP16                                                      
         CLI   BYTE1,YESQ          Do we have a missing approval                
         JE    PROAPP30            Yes                                          
         OI    XAPPIND,XAFULAP     No - set status as fully approved            
         DROP  R3                                                               
                                                                                
PROAPP30 CLI   XOMACT,RQOMA3Q      Only if STATUS change                        
         JE    *+12                                                             
         CLI   XOMACT,RQOMA4Q      or status and maintain change                
         JNE   PROAPP60                                                         
                                                                                
         CLI   XOMSTA,RQREJECT     reject                                       
         JE    PROAPP32                                                         
         CLI   XOMSTA,RQPRTAPP     part approved                                
         JNE   PROAPP60                                                         
*        JE    PROAPP32                                                         
*        CLI   XOMSTA,RQFULAPP     fully approved                               
*        JE    PROAPP32                                                         
*        CLI   XOMSTA,RQAUTO       auto/no approval                             
*        JNE   PROAPP60                                                         
                                                                                
         USING XPIDSD,R2                                                        
PROAPP32 LA    R2,XPVALS           Check connected user against PIDs            
         LA    R3,XPMAXQ                                                        
         MVI   BYTE2,NOQ                                                        
                                                                                
PROAPP34 MVI   BYTE1,NOQ                                                        
         OC    XPIDC1,XPIDC1       approver set?                                
         JZ    PROAPP52                                                         
         TM    XPIDS1,PIDAPPQ      approved already?                            
         JZ    PROAPP36                                                         
         CLC   CCTPID,XPIDC1                                                    
         JNE   PROAPP40                                                         
         MVI   BYTE2,YESQ                                                       
         J     PROAPP40                                                         
                                                                                
PROAPP36 CLC   CCTPID,XPIDC1       current user?                                
         JNE   PROAPP38                                                         
         CLI   XOMSTA,RQREJECT                                                  
         JE    PROAPP54                                                         
         OI    XPIDS1,PIDAPPQ      mark as approved                             
         J     PROAPP54                                                         
                                                                                
PROAPP38 MVI   BYTE1,YESQ                                                       
                                                                                
PROAPP40 OC    XPIDC2,XPIDC2       approver set?                                
         JZ    PROAPP46                                                         
         TM    XPIDS2,PIDAPPQ      approved already?                            
         JZ    PROAPP42                                                         
         CLC   CCTPID,XPIDC2                                                    
         JNE   PROAPP46                                                         
         MVI   BYTE2,YESQ                                                       
         J     PROAPP46                                                         
                                                                                
PROAPP42 CLC   CCTPID,XPIDC2       current user?                                
         JNE   PROAPP44                                                         
         CLI   XOMSTA,RQREJECT     Are we rejecting                             
         JE    PROAPP54            Yes                                          
         OI    XPIDS2,PIDAPPQ      No - must be approving                       
         J     PROAPP54                                                         
                                                                                
PROAPP44 MVI   BYTE1,YESQ          missing approval in between                  
                                                                                
PROAPP46 CLI   BYTE1,YESQ          any gap in approvals?                        
         JNE   PROAPP52                                                         
         CLI   XOMSTA,RQREJECT     Are we rejecting                             
         JE    PROAPP52                                                         
         CLI   T.PROEBY04,YESQ     sequential approval?                         
         JNE   PROAPP52                                                         
         LA    RE,RNSASEQ          Self approval by order type                  
         CLI   ORDTYPE,EXPOQ                                                    
         JE    PROAPP48                                                         
         LA    RE,RNSASPQ                                                       
         CLI   ORDTYPE,PROOQ                                                    
         JE    PROAPP48                                                         
         LA    RE,RNSASIQ                                                       
         CLI   ORDTYPE,INTOQ                                                    
         JE    PROAPP48                                                         
         LA    RE,RNSASAQ                                                       
                                                                                
PROAPP48 BASR  RF,0                                                             
         TM    T.RNSAIND,0                                                      
         EX    RE,0(RF)                                                         
         JNZ   PROAPP50            self approval OK                             
         MVC   ROUERRV,=AL2(AE$OCREF)                                           
         J     EXITH               Error                                        
                                                                                
PROAPP50 CLC   CCTPID,XORDOWN      for 'added by' only                          
         JE    PROAPP52                                                         
         MVC   ROUERRV,=AL2(AE$OCREF)                                           
         J     EXITH               Error                                        
                                                                                
PROAPP52 AHI   R2,XPIDLQ           next entry                                   
         JCT   R3,PROAPP34                                                      
         MVC   ROUERRV,=AL2(AE$INAPP)    User is not approver                   
         CLI   BYTE2,YESQ                                                       
         JNE   EXITH                                                            
         MVC   ROUERRV,=AL2(AE$APAPA)    User approved already                  
         J     EXITH                                                            
                                                                                
PROAPP54 CLI   XOMSTA,RQREJECT     Check for any o/s approvals                  
         JE    PROAPP60                                                         
         LA    R2,XPVALS                                                        
         LA    R3,XPMAXQ                                                        
                                                                                
PROAPP56 OC    XPIDC1,XPIDC1       approver set?                                
         JZ    PROAPP58                                                         
         TM    XPIDS1,PIDAPPQ      o/s approval?                                
         JZ    PROAPP60                                                         
         OC    XPIDC2,XPIDC2       approver set?                                
         JZ    PROAPP58                                                         
         TM    XPIDS2,PIDAPPQ      o/s approval?   ?                            
         JZ    PROAPP60                                                         
                                                                                
PROAPP58 AHI   R2,XPIDLQ           next entry                                   
         JCT   R3,PROAPP56                                                      
         OI    XAPPIND,XAFULAP     set to fully approved                        
         DROP  R2                                                               
                                                                                
PROAPP60 J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Set status of order for order upload                                *         
***********************************************************************         
         SPACE 1                                                                
SETSTA   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETSTA*'                                                      
         MVI   XOWRKST,0                                                        
         CLI   RQOMACT,RQOMA1Q     Status depends on action & record            
         JE    SETSTA40            Add                                          
         CLI   RQOMACT,RQOMA2Q                                                  
         JE    SETSTA20            Maintain only                                
         CLI   RQOMACT,RQOMA3Q                                                  
         JE    SETSTA20            Status only                                  
         CLI   RQOMACT,RQOMA4Q                                                  
         JE    SETSTA20            Maintain and status                          
         DC    H'0'                                                             
                                                                                
         USING ORDRECD,R2                                                       
SETSTA20 L     R2,AIO3             maintenance/status change                    
                                                                                
SETSTA22 LA    RE,ORDRSTAT         determine old status                         
         LA    RF,STATAB                                                        
                                                                                
SETSTA24 CLI   0(RF),FF                                                         
         JE    SETSTA32                                                         
         CLC   0(1,RE),2(RF)                                                    
         JNE   SETSTA28                                                         
         XR    R1,R1                                                            
         ICM   R1,1,3(RF)                                                       
         JZ    SETSTA30                                                         
         EXRL  R1,*+10                                                          
         JNZ   SETSTA30                                                         
         TM    XORDSTA,0                                                        
SETSTA28 AHI   RF,4                                                             
         J     SETSTA24                                                         
                                                                                
SETSTA30 MVC   XOWRKST,0(RF)       set workflow status                          
         MVC   XOMATST,1(RF)       set matching status                          
SETSTA32 OC    XOWRKST,XOWRKST     anything set for workflow                    
         JNZ   SETSTA38            Yes                                          
         NI    ORDRSTA2,X'FF'-ORDSSTAT Remove last update status change         
         LA    RE,ORDRSTA2                                                      
         LA    RF,ST2TAB                                                        
SETSTA34 CLI   0(RF),FF                                                         
         JNE   *+6                                                              
         DC    H'0'                (unknown scenario)                           
         CLC   0(1,RE),2(RF)                                                    
         JE    SETSTA36                                                         
         AHI   RF,4                                                             
         J     SETSTA34                                                         
                                                                                
SETSTA36 MVC   XOWRKST,0(RF)       set workflow status                          
                                                                                
SETSTA38 CLI   RQOMACT,RQOMA2Q     if maint. only set new from old              
         JNE   SETSTA40                                                         
         MVC   XNWRKST,XOWRKST                                                  
                                                                                
SETSTA40 MVI   XSKPVAL,NOQ                                                      
         CLI   RQOMSTA,RQCLOSED    Closed deleted and rejected can              
         JE    SETSTA50             skip validation in other routines           
         CLI   RQOMSTA,RQDELETD                                                 
         JE    SETSTA50                                                         
         CLI   RQOMSTA,RQREJECT                                                 
         JNE   EXITY                                                            
                                                                                
SETSTA50 MVI   XSKPVAL,YESQ                                                     
                                                                                
         J     EXITY                                                            
         DROP  R2                                                               
***********************************************************************         
* Set values on status change only for order upload                   *         
***********************************************************************         
         SPACE 1                                                                
SETVAL   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETVAL*'                                                      
                                                                                
         USING ORDRECD,R2                                                       
         L     R2,AIO3                                                          
         USING ORDELD,R3                                                        
         LA    R3,ORDRFST                                                       
                                                                                
         MVC   RQOMOFF,ORDROFF                                                  
         MVC   RQOMETY,ORDREXTY                                                 
         MVC   RQOMCUR,AGYCURR                                                  
                                                                                
         CLI   RQOMTYP,C' '                                                     
         JH    SETVAL05                                                         
                                                                                
         DC    H'0'                order type must be passed                    
                                                                                
SETVAL05 CLI   ORDEL,0                                                          
         JE    EXITY                                                            
                                                                                
         CLI   ORDEL,ORDELQ                                                     
         JNE   SETVAL20                                                         
                                                                                
*&&UK*&& MVC   RQOMAUT,ORDAUTH                                                  
         MVC   RQOMRBD,ORDRQBD                                                  
         MVC   RQOMDAT,ORDDATE                                                  
         MVC   RQOMSUP,ORDSUPU                                                  
         CLI   ORDTYPE,EXPOQ                                                    
         JE    SETVAL10                                                         
         MVC   RQOMSJA,ORDACCA                                                  
         J     SETVAL15                                                         
                                                                                
SETVAL10 MVC   RQOMEXP,ORDACCU                                                  
                                                                                
SETVAL15 DS    0H                                                               
         J     SETVAL80                                                         
                                                                                
         USING SPAELD,R3                                                        
SETVAL20 CLI   SPAEL,SPAELQ                                                     
         JNE   SETVAL30                                                         
         LA    RE,RQOMDEP                                                       
         CLI   SPATYPE,SPATDEPT                                                 
         JE    SETVAL25                                                         
         LA    RE,RQOMPER                                                       
         CLI   SPATYPE,SPATPERS                                                 
         JNE   SETVAL30                                                         
                                                                                
SETVAL25 MVC   0(12,RE),SPAAACT                                                 
         J     SETVAL80                                                         
                                                                                
         USING SORELD,R3                                                        
SETVAL30 CLI   SOREL,SORELQ                                                     
         JNE   SETVAL35                                                         
         MVC   RQOMSJA,SORAACT                                                  
         J     SETVAL80                                                         
                                                                                
         USING AFCELD,R3                                                        
SETVAL35 CLI   AFCEL,AFCELQ                                                     
         JNE   SETVAL40                                                         
         MVC   RQOMCUR,AFCCURR                                                  
         J     SETVAL80                                                         
                                                                                
         USING FFTELD,R3                                                        
SETVAL40 CLI   FFTEL,FFTELQ                                                     
         JNE   SETVAL60                                                         
         CLI   FFTTYPE,FFTTESTN                                                 
         JNE   SETVAL45                                                         
         MVC   RQOMEST,FFTOESTN                                                 
         J     SETVAL80                                                         
                                                                                
SETVAL45 CLI   FFTTYPE,FFTTWRKC                                                 
         JNE   SETVAL50                                                         
         MVC   RQOMEOW,FFTWORK                                                  
         J     SETVAL80                                                         
                                                                                
SETVAL50 CLI   FFTTYPE,FFTTSATN                                                 
         JNE   SETVAL55                                                         
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   RQOMATO(0),FFTDATA                                               
         EX    RE,0(RF)                                                         
         J     SETVAL80                                                         
                                                                                
SETVAL55 DS    0H                                                               
         J     SETVAL80                                                         
                                                                                
SETVAL60 DS    0H                                                               
                                                                                
         USING ORDELD,R3                                                        
SETVAL80 XR    R0,R0                                                            
         IC    R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     SETVAL05                                                         
                                                                                
                                                                                
         DROP  R2,R3                                                            
***********************************************************************         
* Add or update order transaction contra account                      *         
***********************************************************************         
         SPACE 1                                                                
OADDCA   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*OADDCA*'                                                      
         USING ORDRECD,R2                                                       
         L     R2,AIO3             R2=A(order record)                           
         LA    R2,ORDRFST                                                       
         USING ORDELD,R2                                                        
         XR    R0,R0                                                            
OADDCA10 CLI   ORDEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                Should always have an order element          
         CLI   ORDEL,ORDELQ        R2=A(order element)                          
         JE    OADDCA20                                                         
         IC    R0,ORDLN                                                         
         AR    R2,R0                                                            
         J     OADDCA10                                                         
                                                                                
         USING CHDRECD,R3                                                       
OADDCA20 LA    R3,IOKEY            Build key - check if record is there         
         MVC   CHDKEY,SPACES       R3=A(contra record)                          
         MVC   CHDKCULA,ORDJOB                                                  
         CLC   PRODUL,ORDACCU                                                   
         JNE   *+10                                                             
         MVC   CHDKWRK,=C'**'      Workcode is ** on production ledger          
*&&UK*&& MVC   CHDKCULC,ORDSUP                                                  
*&&US                                                                           
         CLC   PRODUL,ORDACCU      Is it production ledger                      
         JNE   *+10                                                             
         MVC   CHDKCCPY,ORDSUPC    Company only on production orders            
         MVC   CHDKULC,ORDSUPU     Contra=supplier                              
*&&                                                                             
         XC    CHDKNULL,CHDKNULL   Zero last 6 bytes of key                     
         MVC   CSVKEY4,IOKEY                                                    
         L     R1,=AL4(IOHIUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   CHDKEY,CSVKEY4      Is it the same key                           
         JNE   OADDCA30            No                                           
         TM    CHDKSTAT,CHDSDELT   Ensure not marked for deletion               
         JZ    EXITY               No - ok can exit                             
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
         NI    CHDRSTAT,FF-CHDSDELT                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,IOKEY                                                         
         NI    CHDKSTAT,FF-CHDSDELT                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         JE    EXITY                                                            
         DC    H'0'                                                             
*                                                                               
OADDCA30 L     R3,AIO1             Build new contra record in IO1               
         LR    RE,R3                                                            
         LA    RF,IOLENQ                                                        
         XR    R1,R1                                                            
         MVCL  RE,R0               Clear IO area                                
*                                                                               
         MVC   CHDKEY,CSVKEY4      Set key                                      
         MVC   CHDRLEN,=Y(CHDRFST-CHDKEY+1) and record length                   
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         USING CACELD,R4                                                        
         LA    R4,ELEMENT                                                       
         MVI   CACEL,CACELQ                                                     
         MVC   CACCNT,CHDKCULC                                                  
         MVC   CACNAME,XSUPNAME                                                 
         LA    RE,L'CACNAME        Compute name length                          
         LA    R1,CACNAME+L'CACNAME-1                                           
         CLI   0(R1),C' '          Test for significant byte                    
         JH    *+10                                                             
         BCTR  R1,0                                                             
         JCT   RE,*-10                                                          
         AHI   RE,CACLN1Q          Set correct element length                   
         STC   RE,CACLN                                                         
         GOTO1 VHELLO,DMCB,(C'P',=CL8'ACCMST'),CHDRECD,CACELD                   
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                Die if Hello fails                           
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO1'                           
         JE    EXITY                                                            
         DC    H'0'                Die if unable to add record                  
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* Add an order transaction record                                     *         
* Needs order extension in AIO2                                       *         
***********************************************************************         
         SPACE 1                                                                
OADDTX   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*OADDTX*'                                                      
         USING TRNRECD,R5                                                       
         L     R5,AIO1             R5=A(transaction record)                     
         LR    RE,R5                                                            
         LA    RF,IOLENQ                                                        
         XR    R1,R1                                                            
         MVCL  RE,R0               Clear IO area 1                              
         USING ORDRECD,R6                                                       
         L     R6,AIO3             R6=A(order record)                           
         LA    R2,ORDRFST          Get order element                            
         USING ORDELD,R2                                                        
         XR    R0,R0                                                            
OADDTX04 CLI   ORDEL,ORDELQ        R2=A(order element)                          
         JE    OADDTX06                                                         
         CLI   ORDEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                Order element should be exist                
         IC    R0,ORDLN                                                         
         AR    R2,R0                                                            
         J     OADDTX04                                                         
                                                                                
OADDTX06 MVC   TRNKEY,SPACES       Make key spaces                              
         MVC   TRNKCULA,ORDJOB     Account = job or expense                     
*&&UK                                                                           
         CLI   XOMSTA,RQOPEN       Unless re-opening                            
         JE    OADDTX07                                                         
         CLC   ORDOJOB,SPACES      use original for order trx                   
         JNH   OADDTX07                  (if exists)                            
         MVC   TRNKCULA,ORDOJOB                                                 
OADDTX07 MVC   TRNKCULC,ORDSUP     Contra=supplier                              
*&&                                                                             
         CLC   TRNKACT,SPACES      Check account code present                   
         JH    *+6                                                              
         DC    H'0'                If no account present die                    
*&&US                                                                           
         CLC   PRODUL,ORDACCU      Is it production ledger                      
         JNE   *+10                                                             
         MVC   TRNKCCPY,ORDSUPC    Company only on production orders            
         MVC   TRNKULC,ORDSUPU     Contra=supplier                              
*&&                                                                             
         CLC   TRNKCACT,SPACES     Check contra account is present              
         JH    *+6                                                              
         DC    H'0'                No - should always exist                     
         CLC   PRODUL,ORDACCU      Is it production ledger                      
         JNE   *+10                                                             
         MVC   TRNKWORK,=C'**'                                                  
         MVC   TRNKDATE,ORDDATE    Transaction date=order date                  
         MVC   TRNKREF,ORDKORD     Transaction reference=order number           
         MVI   TRNKSBR,0                                                        
         MVC   TRNRLEN,=Y(TRNRFST-TRNRECD+1)                                    
         XC    ELEMENT,ELEMENT                                                  
         USING TRNELD,R4                                                        
         LA    R4,ELEMENT                                                       
         MVI   TRNEL,TRNELQ                                                     
*&&UK*&& MVI   TRNLN,TRNLN1Q                                                    
*&&US                                                                           
         MVI   TRNLN,TRNLN1Q+1     Length is default + 1 for narrative          
         MVI   TRNNARR,X'40'       Add 1 byte of spaces to narrative            
*&&                                                                             
         MVC   TRNDATE,TRNKDATE                                                 
         MVC   TRNREF,TRNKREF                                                   
         MVI   TRNTYPE,TRNTORD     Type 12                                      
         OI    TRNSTAT,TRNSDR                                                   
                                                                                
         MVC   TRNMOS(1),OR_TODF+1                                              
         MVC   TRNMOS+1(1),OR_TODF+3 Convert month into DDS format              
         CLI   OR_TODF+2,C'1'                                                   
         JNE   OADDTX08                                                         
         NI    TRNMOS+1,X'C3'                                                   
         XR    R1,R1                                                            
         IC    R1,TRNMOS+1                                                      
         AHI   R1,1                (10=A, 11=B, 12=C)                           
         STC   R1,TRNMOS+1                                                      
OADDTX08 MVC   TRNBREF,SPACES                                                   
         ZAP   TRNAMNT,=P'0'                                                    
         MVC   TRNANAL,=C'**'                                                   
         MVC   TRNRSMOS,TRNDATE    SET ALWAYS TODAY'S MONTH                     
         MVC   TRNRSTYP,TRNTYPE    TRANSACTION TYPE                             
*&&UK                                                                           
         USING SCMELD,R1                                                        
         XR    R0,R0               Look for matching desciption first           
         LA    R1,ORDRFST                                                       
OADDTX10 CLI   SCMEL,0                                                          
         JE    OADDTX13                                                         
         CLI   SCMEL,SCMELQ                                                     
         JNE   OADDTX12                                                         
         CLI   SCMTYPE,SCMTOMOC    Pass matching text for UK                    
         JE    OADDTX14                                                         
OADDTX12 IC    R0,SCMLN                                                         
         AR    R1,R0                                                            
         J     OADDTX10                                                         
*                                                                               
OADDTX13 LA    R1,ORDRFST                                                       
OADDTX1A CLI   SCMEL,0                                                          
         JE    OADDTX16                                                         
         CLI   SCMEL,SCMELQ                                                     
         JNE   OADDTX1B                                                         
         CLI   SCMTYPE,SCMTSTND    Pass printed descp for UK                    
         JE    OADDTX14                                                         
OADDTX1B IC    R0,SCMLN                                                         
         AR    R1,R0                                                            
         J     OADDTX1A                                                         
*                                                                               
OADDTX14 XR    RE,RE                                                            
         IC    RE,SCMLN                                                         
         SHI   RE,SCMLN1Q                                                       
         CHI   RE,L'TRNNARR                                                     
         JNH   *+8                                                              
         LA    RE,L'TRNNARR                                                     
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   TRNNARR(0),SCMNARR   Copy text to transaction narrative          
         AHI   RE,TRNLN1Q+1                                                     
         STC   RE,TRNLN                                                         
         DROP  R1                                                               
*&&                                                                             
OADDTX16 GOTO1 VHELLO,DMCB,(C'P',=CL8'ACCMST'),TRNRECD,TRNELD                   
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRSELD,R4                                                        
         LA    R4,ELEMENT          Status element                               
         XC    ELEMENT,ELEMENT                                                  
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLN2Q                                                    
         CLI   XGAPYN,YESQ         GAP in use?                                  
         JNE   OADDTX17                                                         
         TM    ORDRSTAT,ORDSFMCH+ORDGDRCV                                       
         JNZ   OADDTX17            only if old status is not fully mat          
         TM    ORDRSTA2,ORDSAPPR   If order is being approved                   
         JZ    OADDTX17                                                         
         MVI   TRSGSTAT,ORDGNSNT   set GAP status to not sent                   
*                                                                               
OADDTX17 GOTO1 VDATCON,DMCB,(1,OR_TODP),(2,TRSDATE)                             
         GOTO1 VHELLO,DMCB,(C'P',=CL8'ACCMST'),TRNRECD,TRSELD,0                 
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PIDELD,R4                                                        
         LA    R4,ELEMENT          Pid element                                  
         XC    PIDEL(PIDLNQ),PIDEL                                              
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,ORDCPID                                                    
         GOTO1 VHELLO,DMCB,(C'P',=CL8'ACCMST'),TRNRECD,PIDELD,0                 
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING FFTELD,R4                                                        
         LA    R4,ORDRFST                                                       
OADDTX18 CLI   FFTEL,0                                                          
         JE    OADDTX30                                                         
         CLI   FFTEL,AFCELQ                                                     
         JE    OADDTX22                                                         
         CLI   FFTEL,SORELQ                                                     
         JE    OADDTX22                                                         
         CLI   FFTEL,OAMELQ                                                     
         JE    OADDTX22                                                         
*&&US                                                                           
         CLI   FFTEL,EXOELQ                                                     
         JE    OADDTX22                                                         
*&&                                                                             
         CLI   FFTEL,FFTELQ                                                     
         JNE   OADDTX20                                                         
         CLI   FFTTYPE,FFTTOFFC                                                 
         JE    OADDTX22                                                         
         CLI   FFTTYPE,FFTTEXTY                                                 
         JE    OADDTX22                                                         
         CLI   FFTTYPE,FFTTESTN                                                 
         JE    OADDTX24                                                         
OADDTX20 LLC   R0,FFTLN                                                         
         AR    R4,R0                                                            
         J     OADDTX18                                                         
                                                                                
         USING AFCELD,R4                                                        
OADDTX22 LLC   RF,AFCLN                                                         
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   ELEMENT(0),AFCEL    Copy element                                 
         EX    RF,0(R1)                                                         
         CLI   AFCEL,AFCELQ                                                     
         JNE   OADDTX26                                                         
         ZAP   ELEMENT+AFCAMNT-AFCELD(L'AFCAMNT),=P'0'                          
         J     OADDTX26                                                         
                                                                                
         USING FFTELD,R4                                                        
NEW      USING FFTELD,RF                                                        
OADDTX24 XC    ELEMENT,ELEMENT                Copy estimate number              
         LA    RF,ELEMENT                                                       
         MVI   NEW.FFTEL,FFTELQ                                                 
         MVI   NEW.FFTLN,FFTLN1Q+7                                              
         MVI   NEW.FFTTYPE,FFTTESTN                                             
         MVI   NEW.FFTDLEN,6                                                    
         MVC   NEW.FFTESTN,FFTESTN                                              
         CLC   FFTESTN,SPACES                                                   
         JH    OADDTX26                                                         
         CLI   FFTLN,FFTESLNQ                                                   
         JL    OADDTX26                                                         
         MVC   NEW.FFTESTN,FFTOESTN                                             
         DROP  NEW                                                              
                                                                                
OADDTX26 LA    RF,=CL8'ADD=END'    all elements to the end                      
         CLI   FFTEL,OAMELQ        Ensure order amount elements added           
         JNE   OADDTX28                           in sequence                   
         LA    RF,=CL8'ADD=CODE'   all elements added in SEQ                    
OADDTX28 GOTO1 VHELLO,DMCB,(C'P',ACCMST),TRNRECD,ELEMENT                        
         CLI   12(R1),0                                                         
         JE    OADDTX20                                                         
         DC    H'0'                                                             
         DROP  R2,R4                                                            
                                                                                
EXT      USING XDFELD,R2                                                        
OADDTX30 L     R2,AIO2             Copy extra data elements to trx              
         CLC   ORDKEY(ORDKSEQ-ORDRECD),0(R2) from order extension rec           
         JE    OADDTX32                                                         
*                                                                               
         MVC   IOKEY,ORDKEY                                                     
         MVI   IOKEY+ORDKSEQ-ORDKEY,ORDKEXTN                                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         CLI   IOERR,0                                                          
         JNE   OADDTX46     skip if not found, or some other problem            
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         CLI   IOERR,0                                                          
         JE    OADDTX32                                                         
         DC    H'0'                                                             
*                                                                               
OADDTX32 AHI   R2,ORDRFST-ORDRECD                                               
                                                                                
OADDTX34 CLI   EXT.XDFEL,0                                                      
         JE    OADDTX46                                                         
         CLI   EXT.XDFEL,XDFELQ                                                 
         JE    OADDTX38                                                         
                                                                                
OADDTX36 LLC   R0,EXT.XDFLN                                                     
         AR    R2,R0                                                            
         J     OADDTX34                                                         
                                                                                
         USING XDFPASD,R4                                                       
OADDTX38 LA    R4,IOKEY            Read XDFRECD using passives                  
         XC    IOKEY,IOKEY                                                      
         MVI   XDFPTYP,XDFPTYPQ                                                 
         MVI   XDFPSUB,XDFPSUBQ                                                 
         MVC   XDFPCPY,CUXCPY                                                   
         MVC   XDFPPTR(L'XDFXPTX),EXT.XDFOCOD                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO5'                               
         CLC   IOKEYSAV(XDFPSEQ-XDFPASD),IOKEY                                  
         JE    *+6                                                              
         DC    H'0'                Die if we can't find records                 
         DROP  R4                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO5'                              
                                                                                
         USING XDFRECD,R4                                                       
         L     R4,AIO5                                                          
         LA    R4,XDFRFST                                                       
XDF      USING XDFELD,R4                                                        
OADDTX40 CLI   XDF.XDFEL,0         If element not found die                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   XDF.XDFEL,XDFELQ                                                 
         JE    OADDTX44                                                         
                                                                                
OADDTX42 LLC   R0,XDF.XDFLN        Bump to next element                         
         AR    R4,R0                                                            
         J     OADDTX40                                                         
                                                                                
OADDTX44 CLC   XDF.XDFSEQ,EXT.XDFOCOD+L'XDFPPTR                                 
         JNE   OADDTX42            Skip if seq number doesn't match             
                                                                                
TRN      USING XDFELD,R3                                                        
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         MVI   TRN.XDFEL,XDFELQ                                                 
         LLC   R1,XDF.XDFLN                                                     
         SHI   R1,XDFLN1Q                                                       
         LTR   R1,R1                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SHI   R1,1                                                             
         MVC   TRN.XDFXEDIT,XDF.XDFEDIT                                         
         BASR  RE,0                Store header name                            
         EX    R1,4(RE)                                                         
         MVC   TRN.XDFXDATA(0),XDF.XDFNAME                                      
         AHI   R1,1                                                             
         STC   R1,TRN.XDFXDALN     Store length                                 
         AHI   R1,2                                                             
         LLC   RE,EXT.XDFLN                                                     
         SHI   RE,XDFOLNQ                                                       
         LTR   RE,RE                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AHI   RE,1                                                             
         LR    R0,R1                                                            
         AR    R0,RE                                                            
         AHI   R0,XDFSEQL                                                       
         STC   R0,TRN.XDFLN                                                     
         SHI   RE,2                                                             
         AR    R3,R1                                                            
         SHI   R3,1                                                             
         BASR  RF,0                Move data in                                 
         EX    RE,4(RF)                                                         
         MVC   TRN.XDFXDATA(0),EXT.XDFODTA                                      
         AHI   RE,1                                                             
         STC   RE,TRN.XDFXDALN     Store length of data                         
         SR    R3,R1                                                            
         AHI   R3,1                                                             
         GOTO1 VHELLO,DMCB,(C'P',=CL8'ACCMST'),TRNRECD,TRN.XDFELD,     *        
               =CL8'ADD=CODE'                                                   
         CLI   12(R1),0                                                         
         JE    OADDTX36                                                         
         DC    H'0'                                                             
         DROP  EXT,XDF,TRN                                                      
                                                                                
OADDTX46 TM    SCPYEL+CPYSTAT9-CPYELD,CPYSSRNM  Accent cashflow in use          
         JZ    OADDTX48            No                                           
         LA    R4,ELEMENT                                                       
         USING SERELD,R4           Build serial unique id element               
         XC    ELEMENT,ELEMENT                                                  
         MVI   SEREL,SERELQ                                                     
         MVI   SERLN,SERLNQ                                                     
         LA    R3,IOKEY            Build key for serial passive pointer         
         USING TRSPASD,R3                                                       
         XC    TRSPKEY,TRSPKEY                                                  
         MVI   TRSPTYP,TRSPTYPQ                                                 
         MVI   TRSPSUB,TRSPSUBQ                                                 
         MVC   TRSPCPY,TRNKCPY                                                  
         L     R1,=AL4(IOHIUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         LA    R3,IOKEY                                                         
         ICM   RE,15,TRSPSER                                                    
         LPR   RE,RE                                                            
         AHI   RE,1                Increment by 1                               
         STCM  RE,15,SERNM         Store new number in element                  
         LNR   RE,RE                                                            
         STCM  RE,15,TRSPSER                                                    
         MVC   TRSPSTA,TRNRSTA                                                  
         MVC   CSVKEY3,IOKEY                                                    
         GOTO1 VHELLO,DMCB,(C'P',=CL8'ACCMST'),TRNRECD,SERELD                   
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
OADDTX48 L     RE,AIO1             See if tranaction exists                     
         MVC   IOKEY,0(RE)                                                      
         L     R1,=AL4(IORDUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    OADDTX50                                                         
         CLI   IOERR,IOEDEL                                                     
         JNE   OADDTX54                                                         
                                                                                
OADDTX50 L     R0,AIO2             COPY NEW RECORD TO AIO2                      
         LA    R1,IOLENQ                                                        
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
*&&UK*&& MVC   XTRNDA,IOKEY+(TRNKDA-TRNRECD)                                    
                                                                                
         LA    R5,IOKEY                                                         
         MVC   CSVKEY2,IOKEY                                                    
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         TM    TRNKSTAT,TRNSARCH   Is record archived                           
         JZ    *+8                                                              
         L     R1,=AL4(IOGET+IOARC+IO1)  Yes - read archive file                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    TRNKSTAT,TRNSARCH   Is record archived                           
         JZ    OADDTX52                                                         
*&&UK                                                                           
         GOTOR VPROMOTE,DMCB,AIO1,ACOMFACS Promote rec to master file           
         MVC   IOKEY,CSVKEY2                and then update                     
         L     R1,=AL4(IORDUPD+IODIR+IO1)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JE    *+14                                                             
         CLI   IOERR,IOEDEL                                                     
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
                                                                                
OADDTX52 L     R0,AIO1             Copy back and write back                     
         LA    R1,IOLENQ                                                        
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*&&UK                                                                           
         USING CPTRBLK,ELEMENT                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'D',AIO1),(C'K',CPTRBLK),0,0,ACOMFACS             
*&&                                                                             
                                                                                
         L     R5,AIO1                                                          
         NI    TRNRSTAT,FF-(TRNSDELT+TRNSDRFT)                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*&&UK                                                                           
         USING CPTRBLK,ELEMENT                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,XTRNDA,0,ACOMFACS               
*&&                                                                             
                                                                                
         LA    R5,IOKEY            Undelete directory and write back            
         NI    TRNKSTAT,FF-(TRNSDELT+TRNSDRFT)                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         JE    OADDTX56                                                         
         DC    H'0'                                                             
                                                                                
OADDTX54 GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                Die if we can't add record                   
*&&UK                                                                           
         MVC   XTRNDA,IODA                                                      
         USING CPTRBLK,ELEMENT                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,DMCB,(C'A',AIO1),CPTRBLK,XTRNDA,0,ACOMFACS               
*&&                                                                             
                                                                                
OADDTX56 MVC   DUB2,IODA           Save new disk address for serial             
*                                               passive                         
         TM    SCPYEL+CPYSTAT9-CPYELD,CPYSSRNM                                  
         JZ    EXITY                                                            
         MVC   IOKEY,CSVKEY3                                                    
         L     R1,=AL4(IORDD+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    EXITY                                                            
         TM    IOERR,IOEDEL                                                     
         JNZ   EXITY                                                            
                                                                                
OADDTX58 MVC   IOKEY,CSVKEY3                                                    
         MVC   IOKEY+TRSPDA-TRSPASD(L'TRSPDA),DUB2                              
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IODIR+IO2'                              
         JE    EXITY                                                            
         DC    H'0'                Die if we can't add new passive              
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
***********************************************************************         
* Delete an order transaction record                                  *         
***********************************************************************         
         SPACE 1                                                                
ODELTX   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ODELTX*'                                                      
         LA    R5,IOKEY            R5=A(transaction record)                     
         USING TRNRECD,R5                                                       
         L     R3,AIO3             R3=A(order record)                           
         USING ORDRECD,R3                                                       
         LAY   R6,I_OLD            Get order element from old buffer            
         USING EL_RECD,R6                                                       
         XC    EL_KEY(EL_KEYL),EL_KEY                                           
         MVI   EL_ELCDE,ORDELQ                                                  
         USING ORDELD,R2                                                        
         LA    R2,EL_ELEM                                                       
         GOTOR BUFELE,DMCB,('TSARDH',TSARELEO),('BEOLDQ',EL_REC)                
         CLI   TSAERR,0            Is it the end of the buffer?                 
         JE    *+6                                                              
         DC    H'0'                Yes - die as should find record              
         CLI   ORDEL,ORDELQ        Check element is order element               
         JE    *+6                                                              
         DC    H'0'                No - die as should be                        
*                                                                               
         MVC   TRNKEY,SPACES       Make key spaces                              
         MVC   TRNKCULA,ORDJOB     Account = job or expense                     
*&&UK                                                                           
         CLC   ORDOJOB,SPACES                                                   
         JNH   *+10                                                             
         MVC   TRNKCULA,ORDOJOB    Always use original for order trx            
         MVC   TRNKCULC,ORDSUP     Contra=supplier                              
*&&                                                                             
*&&US                                                                           
         CLC   PRODUL,ORDACCU      Is it production ledger                      
         JNE   *+10                                                             
         MVC   TRNKCCPY,ORDSUPC    Company only on production orders            
         MVC   TRNKULC,ORDSUPU     Contra=supplier                              
*&&                                                                             
         CLC   PRODUL,ORDACCU      Is it production ledger                      
         JNE   *+10                                                             
         MVC   TRNKWORK,=C'**'                                                  
         MVC   TRNKDATE,ORDDATE    Transaction date=order date                  
         MVC   TRNKREF,ORDKORD     Transaction reference=order number           
         MVI   TRNKSBR,0           Read for order transaction                   
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    ODELTX02            Found                                        
         TM    ORDRSTAT,ORDSFMCH   Is Order fully match then                    
         JO    ODELTXX             transaction could be not present             
         DC    H'0'                If not we have a serious error               
*                                  as order transaction should exist            
ODELTX02 MVC   CSVKEY2,IOKEY                                                    
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         TM    TRNKSTAT,TRNSARCH   Is this on archive file                      
         JZ    *+8                                                              
         L     R1,=AL4(IOGET+IOARC+IO1)  Yes                                    
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                Die if not found                             
         TM    TRNKSTAT,TRNSARCH                                                
         JZ    ODELTX04                                                         
*&&UK                                                                           
         GOTOR VPROMOTE,DMCB,AIO1,ACOMFACS                                      
         MVC   IOKEY,CSVKEY2                                                    
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
                                                                                
ODELTX04 L     R5,AIO1                                                          
         OI    TRNRSTAT,TRNSDELT   Set transaction to deleted                   
         NI    TRNRSTAT,FF-TRNSDRFT   Remove draft status                       
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                Die if we can't put record back              
         LA    R5,IOKEY                                                         
         OI    TRNKSTAT,TRNSDELT   Set transaction to deleted                   
         NI    TRNKSTAT,FF-TRNSDRFT   Remove draft status                       
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         JE    ODELTXX                                                          
         DC    H'0'                Die if we can't write back directory         
                                                                                
ODELTXX  J     EXITY                                                            
         DROP  R2,R3,R5,R6                                                      
         EJECT                                                                  
***********************************************************************         
* Check w/cs on status change only for order upload                   *         
***********************************************************************         
         SPACE 1                                                                
CHKWCS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*13CWCS*'                                                      
                                                                                
         USING ORDRECD,R2                                                       
         L     R2,AIO3                                                          
                                                                                
         USING OAMELD,R3                                                        
         LA    R3,ORDRFST                                                       
         XR    R0,R0                                                            
         MVC   TEMP,SPACES                                                      
         USING XWDSCT,R4                                                        
         LAY   R4,XWVALS                                                        
         MVI   XWCNTR,0                                                         
         DROP  R2                                                               
                                                                                
CHKWCS05 CLI   OAMEL,0                                                          
         JE    CHKWCS80                                                         
         CLI   OAMEL,OAMELQ                                                     
         JNE   CHKWCS40                                                         
         CLC   OAMWORK,SPACES                                                   
         JNH   CHKWCS40                                                         
         MVC   TEMP(2),OAMWORK                                                  
                                                                                
         USING WCORECD,R2                                                       
         LA    R2,IOKEY            Read for work code                           
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUXCPY                                                   
         MVC   WCOKUNT(L'PRODUL),PRODUL                                         
         MVC   WCOKWRK,TEMP                                                     
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    CHKWCS10                                                         
         MVC   ROUERRV,=AL2(AE$WRKNF)                                           
         J     CHKWCSN                                                          
                                                                                
CHKWCS10 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         LA    R2,WCORFST                                                       
         USING WCOELD,R2                                                        
         XR    R0,R0                                                            
                                                                                
CHKWCS15 CLI   WCOEL,WCOELQ        Look for WCO element                         
         JE    CHKWCS25                                                         
         CLI   WCOEL,0                                                          
         JNE   CHKWCS20                                                         
         DC    H'0'                (must exist)                                 
                                                                                
CHKWCS20 IC    R0,WCOLN                                                         
         AR    R3,R0                                                            
         J     CHKWCS15                                                         
                                                                                
CHKWCS25 TM    WCOSTAT2,WCOSLORD   W/C locked for orders?                       
         JZ    CHKWCS30                                                         
         MVC   ROUERRV,=AL2(AE$WCLCK)                                           
         J     CHKWCSN                                                          
                                                                                
CHKWCS30 DS    0H                                                               
*&&UK                                                                           
         CLI   T.PROPO202,C'Y'                                                  
         JNE   CHKWCS32                                                         
         TM    WCOSTAT3,WCOSESCK                                                
         JZ    CHKWCS32                                                         
         OI    XWCSTAT,XWCEXWC                                                  
         J     CHKWCS32                                                         
*&&                                                                             
CHKWCS32 MVC   XWCODE,OAMWORK                                                   
         ZAP   XWCAMT,OAMAMNT                                                   
         ZAP   XWCFCA,PZERO                                                     
                                                                                
         AHI   R4,XWCLNQ                                                        
                                                                                
         XR    RE,RE                                                            
         IC    RE,XWCNTR                                                        
         AHI   RE,1                                                             
         STC   RE,XWCNTR                                                        
                                                                                
CHKWCS40 XR    R0,R0                                                            
         IC    R0,OAMLN                                                         
         AR    R3,R0                                                            
         J     CHKWCS05                                                         
                                                                                
CHKWCS80 DS    0H                                                               
         J     EXITY                                                            
                                                                                
CHKWCSN  MVC   XERRTXT,TEMP                                                     
         J     EXITN                                                            
                                                                                
         DROP  R2,R3,R4                                                         
         SPACE 1                                                                
***********************************************************************         
* Set GAP status/dates                                                *         
* AIO3 has main order record                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING ORDRECD,R2                                                       
         USING ORDELD,R3                                                        
SETGAP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETGAP*'                                                      
         OC    XGAPSTAT,XGAPSTAT   Any GAP status change?                       
         JNZ   SETGAP00                                                         
         TM    XORDIND2,XORDISUP   Has supplier changed                         
         JNZ   SETGAP00            Yes - reset GAP status                       
         MVC   XGAPSTAT,XOLGSTA    Use what was on the order originally         
SETGAP00 CLI   XGAPSTAT,ORDGNSNT   If it's not sent has the order               
         JNE   SETGAP01             now been approved                           
         MVC   BYTE1,XOOSTA2         - if so set to sent                        
         MVC   BYTE2,XNOSTA2                                                    
         NI    BYTE1,ORDSAPPR                                                   
         NI    BYTE2,ORDSAPPR                                                   
         CLC   BYTE2,BYTE1                                                      
         JE    SETGAP01                                                         
         CLI   BYTE2,ORDSAPPR                                                   
         JNE   SETGAP01                                                         
         MVI   XGAPSTAT,ORDGSENT   Set sent as order is now approved            
*                                                                               
SETGAP01 L     R2,AIO3                                                          
         OC    XGAPEXPD,XGAPEXPD   Any date passed?                             
         JZ    SETGAP08                                                         
         CLC   XGAPEXPD,XOLGAPEX   Has date changed                             
         JE    SETGAP08                                                         
*                                                                               
         USING GDAELD,R3                                                        
         LA    R3,ORDRFST                                                       
*                                                                               
SETGAP02 CLI   GDAEL,0                                                          
         JE    SETGAP06                                                         
         CLI   GDAEL,GDAELQ                                                     
         JNE   *+12                                                             
         CLI   GDATYPE,GDAGAPEX    check if el exists..........                 
         JE    SETGAP04                                                         
         LLC   R0,GDALN                                                         
         AR    R3,R0                                                            
         J     SETGAP02                                                         
*                                                                               
SETGAP04 MVC   GDADATE,XGAPEXPD                                                 
         J     SETGAP08                                                         
*                                                                               
SETGAP06 LA    R3,ELEMENT          if not add it                                
         XC    ELEMENT,ELEMENT                                                  
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALN2Q                                                    
         MVI   GDATYPE,GDAGAPEX                                                 
         MVC   GDADATE,XGAPEXPD                                                 
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ORDRECD,GDAELD,(RF)                    
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SETGAP08 CLI   XGAPSTAT,ORDGSENT   if status=sent build sent date               
         JNE   SETGAP16                                                         
         LA    R3,ORDRFST                                                       
*                                                                               
SETGAP10 CLI   GDAEL,0                                                          
         JE    SETGAP14                                                         
         CLI   GDAEL,GDAELQ                                                     
         JNE   *+12                                                             
         CLI   GDATYPE,GDAGAPST    check if el exists..........                 
         JE    SETGAP12                                                         
         LLC   R0,GDALN                                                         
         AR    R3,R0                                                            
         J     SETGAP10                                                         
*                                                                               
SETGAP12 L     RE,ATWA                                                          
         MVC   GDADATE,OR_TODP                                                  
         J     SETGAP16                                                         
*                                                                               
SETGAP14 LA    R3,ELEMENT          if not add it                                
         XC    ELEMENT,ELEMENT                                                  
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALN2Q                                                    
         MVI   GDATYPE,GDAGAPST                                                 
         L     RE,ATWA                                                          
         MVC   GDADATE,OR_TODP                                                  
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ORDRECD,GDAELD,(RF)                    
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SETGAP16 LA    R3,ORDRFST                                                       
         USING ORDELD,R3                                                        
SETGAP18 CLI   ORDEL,ORDELQ            Look for order element                   
         JE    SETGAP20                                                         
         CLI   ORDEL,0                 Should never reach end of rec            
         JE    *+2                      so die if you do                        
         LLC   R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     SETGAP18                                                         
                                                                                
SETGAP20 MVC   ORDGSTAT,XGAPSTAT       Amend GAP status                         
                                                                                
SETGAP26 XR    R0,R0                                                            
         ICM   R0,1,XGAPEMLN           # of array elements                      
         JZ    SETGAPX                                                          
         LA    R3,ORDRFST                                                       
         XC    BYTE1,BYTE1                                                      
*                                                                               
         USING FFTELD,R3                                                        
SETGAP28 CLI   FFTEL,0                                                          
         JE    SETGAP34            if not found add one                         
         CLI   FFTEL,FFTELQ                                                     
         JNE   *+12                                                             
*&&UK*&& CLI   FFTTYPE,FFTTPEML    check if el exists..........                 
*&&US*&& CLI   FFTTYPE,FFTTEML     check if el exists..........                 
         JE    SETGAP32                                                         
SETGAP30 LLC   R0,FFTLN                                                         
         AR    R3,R0                                                            
         J     SETGAP28                                                         
*                                                                               
SETGAP32 MVI   FFTEL,X'FF'         delete it                                    
         J     SETGAP30                                                         
*                                                                               
SETGAP34 GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',ORDRECD),0                      
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,ELEMENT          rebuild element                              
         L     R6,AGENAREA                                                      
         LA    R6,(L'STCGEML*10)(R6)                                            
         XR    R0,R0                                                            
         ICM   R0,1,XGAPEMLN       # of array elements                          
         MVI   BYTE1,0                                                          
*                                                                               
SETGAP36 XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ                                                     
*&&UK*&& MVI   FFTTYPE,FFTTPEML                                                 
*&&US*&& MVI   FFTTYPE,FFTTEML                                                  
         LHI   RF,L'STCGEML                                                     
         STC   RF,FFTDLEN                                                       
         LA    R4,FFTEML1                                                       
         LA    R5,FFTEML5                                                       
         MVC   FFTSEQ,BYTE1                                                     
*                                                                               
SETGAP38 CR    R4,R5               Hit end of element?                          
         JH    SETGAP40                                                         
         MVC   0(L'FFTEML1,R4),1(R6)                                            
         AHI   R4,L'FFTEML1                                                     
         AHI   R6,L'FFTEML1+1                                                   
         JCT   R0,SETGAP38                                                      
*                                                                               
SETGAP40 ST    R0,SAVER0                                                        
         SR    R4,R3                                                            
         STC   R4,FFTLN                                                         
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ORDRECD,FFTELD,(RF)                    
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LLC   RF,BYTE1            bump sequence no.                            
         AHI   RF,1                                                             
         STC   RF,BYTE1                                                         
         L     R0,SAVER0                                                        
         LTR   R0,R0                                                            
         JNZ   SETGAP36            Any more elements to do?                     
*                                                                               
SETGAPX  J     EXITY                                                            
         EJECT                                                                  
         DROP  R2,R3                                                            
***********************************************************************         
* Update order transactions with new GAP status                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING ORDELD,R4                                                        
         USING ORDRECD,R3                                                       
UPGTRN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPGTRN*'                                                      
         CLC   XGAPSTAT,XOLGSTA    Anything to update?                          
         JE    EXITY                                                            
         L     R3,AIO3                                                          
         LA    R4,ORDRFST                                                       
*&&US                                                                           
*                                                                               
UPGT02   CLI   ORDEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ORDEL,ORDELQ                                                     
         JE    UPGT04                                                           
         LLC   R0,ORDLN                                                         
         AR    R4,R0                                                            
         J     UPGT02                                                           
*                                                                               
UPGT04   LA    R2,IOKEY                                                         
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,ORDJOB     ACCOUNT = JOB OR EXPENSE                     
         CLC   TRNKACT,SPACES                                                   
         JH    *+6                                                              
         DC    H'0'                (BAD ACCOUNT CODE)                           
*                                                                               
         CLC   PRODUL,ORDACCU      IS IT PRODUCTION LEDGER                      
         JNE   *+10                                                             
         MVC   TRNKCCPY,ORDSUPC    COMPANY ONLY ON PRODUCTION ORDERS            
         MVC   TRNKULC,ORDSUPU     CONTRA=SUPPLIER                              
*                                                                               
         MVC   FULL1,ORDCPID                                                    
         CLC   PRODUL,ORDACCU      IS IT PRODUCTION LEDGER                      
         JNE   *+10                                                             
         MVC   TRNKWORK,=C'**'                                                  
         MVC   TRNKDATE,ORDDATE    TRANSACTION DATE=ORDER DATE                  
         MVC   TRNKREF,ORDKORD     TRANSACTION REFERENCE=ORDER NUMBER           
         MVI   TRNKSBR,0                                                        
         L     R1,=AL4(IORDD+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   UPGTX                                                            
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                IF WE DIE HERE TRANSACTION DOESN'T           
         MVC   CSVKEY2,IOKEY                                    EXIST           
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         TM    TRNKSTAT,TRNSARCH                                                
         JZ    *+8                                                              
         L     R1,=AL4(IOGET+IOARC+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                MUST HAVE ORDER TRANSACTION HERE!            
         TM    TRNKSTAT,TRNSARCH                                                
         JZ    UPGT05                                                           
*                                                                               
         USING TRSELD,R3                                                        
UPGT05   L     R2,AIO2                                                          
         NI    TRNRSTAT,FF-(TRNSDELT+TRNSDRFT)                                  
         XC    ELEMENT,ELEMENT     LOOK FOR TRSELD                              
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('TRSELQ',TRNRECD),0                   
         CLI   12(R1),0                                                         
         JNE   UPGT06                                                           
*                                                                               
         L     R3,12(R1)           EXTRACT TO ELEMENT                           
         LLC   RF,TRSLN                                                         
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   ELEMENT(0),TRSELD                                                
         EX    RF,0(R1)                                                         
         MVI   0(R3),X'FF'                                                      
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',TRNRECD),0                      
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,ELEMENT          INCREASE LENGTH                              
         CLI   TRSLN,TRSLNQ                                                     
         JH    *+8                                                              
         MVI   TRSLN,TRSLN2Q                                                    
         MVC   TRSGSTAT,XGAPSTAT                                                
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TRNRECD,TRSELD,(RF)                    
         CLI   12(R1),0                                                         
         JE    UPGT08                                                           
         DC    H'0'                                                             
*                                                                               
UPGT06   LA    R3,ELEMENT          ADD A TRSELD                                 
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLN2Q                                                    
         MVC   TRSGSTAT,XGAPSTAT                                                
*                                                                               
TRAP     OC    TRNKEY(4),TRNKEY    CRAP TRAP                                    
         JNZ   *+6                 SOMETHING IS ADDING TRSELS TO                
         DC    H'0'                            HEADER RECS                      
*                                                                               
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TRNRECD,TRSELD,(RF)                    
         CLI   12(R1),0                                                         
         JE    UPGT08                                                           
         DC    H'0'                                                             
*                                                                               
UPGT08   GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    UPGTX                                                            
         DC    H'0'                                                             
*&&                                                                             
*                                                                               
*&&UK                                                                           
UPGT10   DS    0H                                                               
         L     R3,AIO3                                                          
         USING ORDRECD,R3                                                       
         USING RNSPASD,R2          READ PASSIVES TO FIND TRANSACTIONS           
         XC    IOKEY,IOKEY         WHICH NEED UPDATING                          
         LA    R2,IOKEY                                                         
         MVI   RNSPTYP,RNSPTYPQ                                                 
         MVI   RNSPSUB,RNSPSUBQ                                                 
         MVC   RNSPCPY,ORDKCPY                                                  
         MVI   RNSPIND,RNSPORQ                                                  
         MVC   RNSPREF,ORDKORD                                                  
         MVC   CSVKEY2,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JNE   *+2                                                              
         J     UPGT14                                                           
*                                                                               
UPGT12   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JNE   *+2                                                              
*                                                                               
UPGT14   CLC   CSVKEY2(RNSPBTY-RNSPASD),IOKEY                                   
         JNE   UPGTX               REACHED END OF PASSIVES?                     
         MVC   CSVKEY2,IOKEY                                                    
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         TM    RNSPSTA,TRNSARCH                                                 
         JZ    *+8                                                              
         L     R1,=AL4(IOGET+IOARC+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
*                                                                               
         LA    R2,IOKEY                                                         
         TM    RNSPSTA,TRNSARCH    IF ARCHIVED PROMOTE TO ACCMST                
         JZ    UPGT16                                                           
         GOTOR VPROMOTE,DMCB,AIO2,ACOMFACS                                      
         MVC   IOKEY,CSVKEY2                                                    
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JE    UPGT15                                                           
         CLI   IOERR,IOEDEL                                                     
         JNE   *+2                 TRANSACTION DOESN'T EXIST?|                  
*                                                                               
UPGT15   L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
*                                                                               
         USING TRNRECD,R2                                                       
         USING TRSELD,R4                                                        
UPGT16   L     R2,AIO2                                                          
         XC    ELEMENT,ELEMENT     LOOK FOR TRSELD                              
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('TRSELQ',TRNRECD),0                   
         CLI   12(R1),0                                                         
         JNE   UPGT18                                                           
*                                                                               
         L     R4,12(R1)           EXTRACT TO ELEMENT                           
         LLC   RF,TRSLN                                                         
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   ELEMENT(0),TRSELD                                                
         EX    RF,0(R1)                                                         
         MVI   0(R4),X'FF'                                                      
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',TRNRECD),0                      
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
*                                                                               
         LA    R4,ELEMENT          INCREASE LENGTH                              
         CLI   TRSLN,TRSLNQ                                                     
         JH    *+8                                                              
         MVI   TRSLN,TRSLN2Q                                                    
         MVC   TRSGSTAT,XGAPSTAT                                                
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TRNRECD,TRSELD,(RF)                    
         CLI   12(R1),0                                                         
         JE    UPGT20                                                           
         DC    H'0'                                                             
*                                                                               
UPGT18   LA    R4,ELEMENT          ADD A TRSELD                                 
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLN2Q                                                    
         MVC   TRSGSTAT,XGAPSTAT                                                
*                                                                               
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TRNRECD,TRSELD,(RF)                    
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
*                                                                               
UPGT20   GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    UPGT12              UPDATE IT                                    
         DC    H'0'                                                             
*&&                                                                             
UPGTX    J     EXITY                                                            
         EJECT                                                                  
         DROP  R2,R3,R4                                                         
**********************************************************************          
* Set workcode error                                                 *          
**********************************************************************          
         SPACE 1                                                                
         USING EWCTABD,RF                                                       
         USING GOXBLKD,RE                                                       
SETWCE   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETWCE*'                                                      
         LR    R2,R1                                                            
         LA    RF,EWCTAB                                                        
         L     RE,AGOXBLCK                                                      
*                                                                               
SETWC02  CLI   EWCTGOV,X'FF'                                                    
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   EWCTGOV,GOECE                                                    
         JE    SETWC04                                                          
         LHI   R0,EWCTLNQ                                                       
         AR    RF,R0                                                            
         J     SETWC02                                                          
*                                                                               
SETWC04  MVC   0(L'EWCTERR,R2),EWCTERR                                          
         J     EXITY                                                            
         EJECT                                                                  
*                                                                               
* Next routine here ...                                                         
***********************************************************************         
* GENERAL EXITS HERE                                                  *         
***********************************************************************         
EXITH    LHI   RE,2                                                             
         J     EXITCC                                                           
                                                                                
EXITL    DS    0H                                                               
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
                                                                                
MAXRECLN EQU   IOLENQ-(L'IODA+L'IOWORK)                                         
                                                                                
D#CAE    EQU   1                   Type field in TEAPP call                     
*                                  XData upload                                 
D#XD#IDN EQU   01                  - ID number return                           
                                                                                
*                                  Text upload                                  
D#TX#IDN EQU   01                  - ID number return                           
                                                                                
*                                  Order upload                                 
D#OU#ORD EQU   01                  - order number                               
D#OU#REQ EQU   02                  - requisition number                         
D#OU#IDN EQU   03                  - ID number                                  
D#OU#STA EQU   04                  - new status                                 
D#OU#ISC EQU   05                  - internal supplier code                     
D#OU#ISN EQU   06                  - internal supplier name                     
D#OU#ESY EQU   07                  - estimate warning flag                      
D#OU#ESE EQU   07                  - estimate check error message               
D#OU#ESN EQU   08                  - estimate warning number                    
D#OU#ESW EQU   08                  - estimate check warning message             
D#OU#DXO EQU   09                  - default expiration period orders           
D#OU#GEM EQU   10                  - GAP email address                          
D#OU#PRS EQU   11                  - Part approved resubmit rule                
D#OU#FRS EQU   12                  - Fully approved resubmit rule               
D#OU#MRS EQU   13                  - Matched order resubmit rule                
D#OU#JRA EQU   14                  - Job level check remaining amount           
*                                                                               
D#OU#WRK EQU   01                  - Workcode                                   
D#OU#ESR EQU   02                  - Estimate check remaining amount            
D#OU#WLE EQU   03                  - Workcode level error                       
D#OU#WLW EQU   04                  - Workcode level warning                     
                                                                                
D#OD#TYP EQU   01                                                               
D#OD#NUM EQU   02                                                               
D#OD#MOD EQU   03                                                               
                                                                                
D#OD#XOC EQU   01                                                               
D#OD#XOT EQU   02                                                               
D#OD#XCC EQU   03                                                               
D#OD#XET EQU   04                                                               
D#OD#XWC EQU   05                                                               
D#OD#XMC EQU   06                                                               
                                                                                
D#OD#ONO EQU   01                                                               
                                                                                
D#OD#1FN EQU   01                                                               
D#OD#1FS EQU   02                                                               
                                                                                
D#UPLERR EQU   255                                                              
D#UPLNOD EQU   254                                                              
                                                                                
XO_XC_Q  EQU   X'80'                                                            
XO_DLODQ EQU   01                                                               
XC_DLODQ EQU   XO_DLODQ+XO_XC_Q                                                 
XO_GLOBQ EQU   02                                                               
XC_GLOBQ EQU   XO_GLOBQ+XO_XC_Q                                                 
XO_WGRPQ EQU   03                                                               
XC_WGRPQ EQU   XO_WGRPQ+XO_XC_Q                                                 
XO_WCODQ EQU   04                                                               
XC_WCODQ EQU   XO_WCODQ+XO_XC_Q                                                 
XO_XDGRQ EQU   05                                                               
XC_XDGRQ EQU   XO_XDGRQ+XO_XC_Q                                                 
XO_XDATQ EQU   06                                                               
XC_XDATQ EQU   XO_XDATQ+XO_XC_Q                                                 
                                                                                
XML_LEFT EQU   C'<'                                                             
XML_RIGT EQU   C'>'                                                             
XML_SLSH EQU   C'/'                                                             
*&&UK                                                                           
XMAXWCS  EQU   6                                                                
*&&                                                                             
*&&US                                                                           
XMAXWCS  EQU   20                                                               
*&&                                                                             
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
RECTAB   DS    0XL(RECTABL)        ** RECORD TABLE **                           
         DC    AL2(A#OUPL),AL1(RECTTOUP)                                        
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
                                                                                
ACCMST   DC    C'ACCMST  '                                                      
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
PMAX     DC    P'50'                                                            
EFFS     DC    X'FFFFFFFF'                                                      
                                                                                
EXPTYTAB DS    0X                                                               
         DC    AL1(EXJPCLI1)                                                    
         DC    AL1(EXJPCBL1)                                                    
         DC    AL1(EXJPCNB1)                                                    
         DC    X'FF'                                                            
                                                                                
COGOTAB  DS    0X                                                               
         DC    AL1(COSAVED,GONONE,NOQ,0,0,0)                                    
         DC    AL1(COSAVED,GOAPPR,NOQ,0,0,0)                                    
         DC    AL1(COSAVED,GOINPR,NOQ,0,0,0)                                    
         DC    AL1(COSAVED,GOSUB,NOQ,0,0,0)                                     
         DC    AL1(COSAVED,GOMANPAP,NOQ,0,0,0)                                  
         DC    AL1(COSAVED,GOCLIPAP,NOQ,0,0,0)                                  
         DC    AL1(COSUBMD,GONONE,NOQ,0,0,0)                                    
         DC    AL1(COSUBMD,GOAPPR,NOQ,0,0,0)                                    
         DC    AL1(COSUBMD,GOINPR,YESQ,0,0,0)                                   
         DC    AL1(COSUBMD,GOSUB,NOQ,0,0,0)                                     
         DC    AL1(COSUBMD,GOCLIPAP,NOQ,0,0,0)                                  
         DC    AL1(COSUBMD,GOMANPAP,NOQ,0,0,0)                                  
         DC    AL1(COCLAPR,GONONE,NOQ,0,0,0)                                    
         DC    AL1(COCLAPR,GOAPPR,NOQ,0,0,0)                                    
         DC    AL1(COCLAPR,GOINPR,YESQ,TIMSSUBM+TIMSREJE+TIMSPAPP)              
         DC    AL1(0,TIMSAWAP)                                                  
         DC    AL1(COCLAPR,GOSUB,NOQ,TIMSSUBM+TIMSPAPP,0,TIMSAWAP)              
         DC    AL1(COCLAPR,GOMANPAP,NOQ,TIMSMAAP,0,TIMSAWAP)                    
         DC    AL1(COCLAPR,GOCLIPAP,NOQ,0,0,0)                                  
         DC    AL1(COLMAPR,GONONE,NOQ,0,0,0)                                    
         DC    AL1(COLMAPR,GOAPPR,NOQ,0,0,0)                                    
         DC    AL1(COLMAPR,GOINPR,YESQ,TIMSSUBM+TIMSREJE+TIMSPAPP)              
         DC    AL1(TIMSMAAP,0)                                                  
         DC    AL1(COLMAPR,GOSUB,NOQ,TIMSSUBM+TIMSPAPP,TIMSMAAP,0)              
         DC    AL1(COLMAPR,GOMANPAP,NOQ,0,0,0)                                  
         DC    AL1(COLMAPR,GOCLIPAP,NOQ,TIMSPAPP,0,TIMSAWAP)                    
         DC    AL1(COFUAPR,GONONE,NOQ,0,0,0)                                    
         DC    AL1(COFUAPR,GOAPPR,NOQ,0,0,0)                                    
         DC    AL1(COFUAPR,GOINPR,YESQ,TIMSSUBM+TIMSREJE+TIMSPAPP,0,0)          
         DC    AL1(COFUAPR,GOSUB,NOQ,TIMSSUBM+TIMSPAPP,0,0)                     
         DC    AL1(COFUAPR,GOMANPAP,NOQ,TIMSMAAP,0,0)                           
         DC    AL1(COFUAPR,GOCLIPAP,NOQ,TIMSPAPP,0,TIMSFAPP)                    
         DC    X'FF'                                                            
                                                                                
         DS    0H                                                               
                                                                                
EXPEU    DC    C'SE SQ SA SI '                                                  
         DC    X'FF'                                                            
EXPUS    DC    C'SE SA SB '                                                     
         DC    X'FF'                                                            
*&&UK                                                                           
SUELDGS  DC    C'SV SX SQ SA '                                                  
         DC    X'FF'                                                            
SUPLDGS  DC    C'SV SX '                                                        
         DC    X'FF'                                                            
*&&                                                                             
*&&US                                                                           
SUELDGS  DC    C'SV SW SX SY '                                                  
         DC    X'FF'                                                            
SUPLDGS  DC    C'SV SW SX SY '                                                  
         DC    X'FF'                                                            
*&&                                                                             
ARTLDGS  DC    C'ST '                                                           
         DC    X'FF'                                                            
INTLDGS  DC    C'SI SK '                                                        
         DC    X'FF'                                                            
                                                                                
JOBFLDS  DC    AL2(AC#OE,AC#CE,AC#RSHR,0)                                       
                                                                                
EAWTAB   DS    0X                                                               
         DC    AL1(GOECEES),AL2(AE$ECONN)                                       
         DC    AL1(GOECEWC),AL2(AE$ECONW)                                       
         DC    AL1(GOECETA),AL2(AE$ECONT)                                       
         DC    AL1(GOECSWC),AL2(AE$ECONW)                                       
         DC    AL1(GOECSTA),AL2(AE$ECONT)                                       
         DC    AL1(GOECEESA),AL2(AE$ECOYN)                                      
         DC    AL1(GOECEWCA),AL2(AE$ECOYW)                                      
         DC    AL1(GOECETAA),AL2(AE$ECOYT)                                      
         DC    X'FF'                                                            
                                                                                
EAETAB   DS    0X                                                               
         DC    AL1(GOECEES),AL1(OA_NCEQ)                                        
         DC    AL1(GOECEWCA),AL1(OA_NCEQ)                                       
         DC    AL1(GOECEESA),AL1(OA_NAEQ)                                       
         DC    AL1(GOECETA),AL1(OA_ORJQ)                                        
         DC    AL1(GOECETAA),AL1(OA_ORJQ)                                       
         DC    AL1(GOECSTA),AL1(OA_OESQ)                                        
         DC    X'FF'                                                            
                                                                                
EWCTAB   DS    0X                                                               
         DC    AL1(GOECEES),AL1(ESTNCESQ)                                       
         DC    AL1(GOECEWC),AL1(ESTREJBQ)                                       
         DC    AL1(GOECEWCA),AL1(ESTREJBQ)                                      
         DC    AL1(GOECSWC),AL1(ESTREESQ)                                       
         DC    X'FF'                                                            
                                                                                
*&&US                                                                           
LOOKFLDH DC    AL1(8+L'LOOKFLD),4X'00',AL1(L'LOOKFLD),AL2(0)                    
LOOKFLD  DC    C'OE,CE,HR'                                                      
         DS    0H                                                               
*&&                                                                             
                                                                                
MQH      DC    16C'*'              MQ HEADER                                    
         ORG   MQH                                                              
MQHDR    DS    0CL16                                                            
MQHTYPE  DC    CL2'PO'             FILE TYPE: PURCHASE ORDERS                   
         DS    CL2                 SPARE                                        
MQHALPH  DS    CL2                 AGENCY ALPHA                                 
         DS    CL10                SPARE                                        
         ORG                                                                    
MQHX     DC    16C'+'              EXTRA HEADER INFO (OPTIONAL)                 
         ORG   MQHX                                                             
MQHXTRA  DS    0CL16                                                            
MQHXORD  DS    CL6                                                              
         DS    CL10                SPARE                                        
MQHXLNQ  EQU   *-MQHXTRA                                                        
MQHLNQ   EQU   *-MQH                                                            
         ORG                                                                    
                                                                                
MQPUT    DC    C'PUT'                                                           
         DS    0H                                                               
                                                                                
XOCTAB   DC    AL1(XO_DLODQ),AL1(14),CL14'Order_Download'                       
XOCTABL  EQU   *-XOCTAB                                                         
         DC    AL1(XO_GLOBQ),AL1(11),CL14'Global_Data'                          
         DC    AL1(XO_WGRPQ),AL1(14),CL14'Workcode_Group'                       
         DC    AL1(XO_WCODQ),AL1(13),CL14'Workcode_Data'                        
         DC    AL1(XO_XDGRQ),AL1(11),CL14'XData_Group'                          
         DC    AL1(XO_XDATQ),AL1(10),CL14'XData_Item'                           
         DC    AL1(0)                                                           
                                                                                
XMLFHLBL DC    C'?xml version="1.0" standalone="yes"?'                          
XMLFHLBQ EQU   *-XMLFHLBL                                                       
         DS    0H                                                               
                                                                                
* special characters table, equates and parameter save area                     
                                                                                
XML_SCT  DC    CL1'&&',AL1(5),CL8'&&amp;'                                       
XML_SCTL EQU   *-XML_SCT                                                        
         DC    CL1'<',AL1(4),CL8'&&lt;'                                         
         DC    CL1'>',AL1(4),CL8'&&gt;'                                         
         DC    CL1'''',AL1(6),CL8'&&apos;'                                      
         DC    CL1'"',AL1(6),CL8'&&quot;'                                       
         DC    X'FF'                                                            
                                                                                
XML_PARM DC    3F'0'                                                            
XML_PARL EQU   *-XML_PARM                                                       
         DS    0H                                                               
                                                                                
***********************************************************************         
* Status translation table - translate status bytes to workflow status*         
* and matching status                                                 *         
*                                                                     *         
* 1st byte old workflow status, 2nd byte matched status, 3rd order    *         
* status byte, 4th byte order element status byte                     *         
***********************************************************************         
         SPACE 1                                                                
STATAB   DC    AL1(XCMPLETE,XFULMAT+XGDRCVD,ORDGDRCV,ORDSMNUP)                  
         DC    AL1(0,XPRTMAT+XGDRCVD,ORDGDRCV,ORDSPART)                         
         DC    AL1(0,XGDRCVD,ORDGDRCV,0)                                        
         DC    AL1(0,XPRTMAT,0,ORDSPART)                                        
         DC    AL1(0,XFULMAT,0,ORDSMNUP)                                        
         DC    AL1(XDLETED,0,ORDSLDEL,0)                                        
         DC    AL1(XCMPLETE,XPRTMAT,ORDCLOSE+ORDSFMCH,ORDSPART)                 
         DC    AL1(XCMPLETE,XPRTMAT,ORDCLOSE,ORDSPART)                          
         DC    AL1(XCMPLETE,XFULMAT,ORDSFMCH,ORDSMNUP)                          
         DC    AL1(XCANCELD,0,ORDCLOSE+ORDSFMCH,0)                              
         DC    AL1(XCMPLETE,XPRTMAT+XGDRCVD,ORDGDRCV+ORDCLOSE+ORDSFMCH)         
         DC    AL1(ORDSPART)                                                    
         DC    AL1(XCMPLETE,XPRTMAT+XGDRCVD,ORDGDRCV+ORDCLOSE)                  
         DC    AL1(ORDSPART)                                                    
         DC    AL1(XCMPLETE,XFULMAT+XGDRCVD,ORDGDRCV+ORDSFMCH)                  
         DC    AL1(ORDSMNUP)                                                    
         DC    AL1(XCANCELD,XGDRCVD,ORDGDRCV+ORDCLOSE+ORDSFMCH,0)               
         DC    X'FF'                                                            
* Status translation table #2 - looks at 2nd status byte on order rec           
ST2TAB   DC    AL1(XIPROGRS,0,ORDSDRFT+ORDSEXEX,0)                              
         DC    AL1(XSBMITTD,0,ORDSSUBM+ORDSEXEX,0)                              
         DC    AL1(XPRTAPRD,0,ORDSPAPP+ORDSEXEX,0)                              
         DC    AL1(XFLAPPRD,0,ORDSAPPR+ORDSEXEX,0)                              
         DC    AL1(XRJECTED,0,ORDSOREJ+ORDSEXEX,0)                              
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* Resubmit table for change to approved, part approved, matched orders*         
***********************************************************************         
         SPACE 1                                                                
RESBTAB  DC    AL1(XFLAPPRD,0),AL2(GOFPORES-GOXBLKD),AL1(GORKEYQ)               
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XFLAPPRD,0),AL2(GOFPORES-GOXBLKD),AL1(GORKEYQ)               
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XFLAPPRD,0),AL2(GOFPORES-GOXBLKD),AL1(GORALLQ)               
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XFLAPPRD,0),AL2(GOFPORES-GOXBLKD),AL1(GORALLQ)               
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XFLAPPRD,0),AL2(GOFPORES-GOXBLKD),AL1(GORALLQ)               
         DC    AL1(RQOMCTQ)                                                     
         DC    AL1(XPRTAPRD,0),AL2(GOPPORES-GOXBLKD),AL1(GORKEYQ)               
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XPRTAPRD,0),AL2(GOPPORES-GOXBLKD),AL1(GORKEYQ)               
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XPRTAPRD,0),AL2(GOPPORES-GOXBLKD),AL1(GORALLQ)               
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XPRTAPRD,0),AL2(GOPPORES-GOXBLKD),AL1(GORALLQ)               
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XPRTAPRD,0),AL2(GOPPORES-GOXBLKD),AL1(GORALLQ)               
         DC    AL1(RQOMCTQ)                                                     
         DC    AL1(XFLAPPRD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XFLAPPRD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XFLAPPRD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XFLAPPRD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XFLAPPRD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCTQ)                                                     
         DC    AL1(XFLAPPRD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XFLAPPRD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XFLAPPRD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XFLAPPRD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XFLAPPRD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCTQ)                                                     
                                                                                
         DC    AL1(XPRTAPRD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XPRTAPRD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XPRTAPRD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XPRTAPRD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XPRTAPRD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCTQ)                                                     
         DC    AL1(XPRTAPRD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XPRTAPRD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XPRTAPRD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XPRTAPRD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XPRTAPRD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCTQ)                                                     
                                                                                
         DC    AL1(XRJECTED,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XRJECTED,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XRJECTED,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XRJECTED,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XRJECTED,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCTQ)                                                     
         DC    AL1(XRJECTED,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XRJECTED,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XRJECTED,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XRJECTED,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XRJECTED,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCTQ)                                                     
                                                                                
         DC    AL1(XRJECTED,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(0)                                                           
         DC    AL1(XRJECTED,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(0)                                                           
         DC    AL1(XRJECTED,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(0)                                                           
         DC    AL1(XRJECTED,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORNOQ)          
         DC    AL1(0)                                                           
         DC    AL1(XRJECTED,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(0)                                                           
         DC    AL1(XRJECTED,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(0)                                                           
         DC    AL1(XRJECTED,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(0)                                                           
         DC    AL1(XRJECTED,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORNOQ)          
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XRJECTED,0),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)               
         DC    AL1(0)                                                           
         DC    AL1(XRJECTED,0),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)               
         DC    AL1(0)                                                           
         DC    AL1(XRJECTED,0),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)               
         DC    AL1(0)                                                           
         DC    AL1(XRJECTED,0),AL2(GOMPORES-GOXBLKD),AL1(GORNOQ)                
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XSBMITTD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XSBMITTD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XSBMITTD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XSBMITTD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XSBMITTD,XPRTMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCTQ)                                                     
         DC    AL1(XSBMITTD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XSBMITTD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORKEYQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XSBMITTD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCYQ)                                                     
         DC    AL1(XSBMITTD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCBQ)                                                     
         DC    AL1(XSBMITTD,XFULMAT),AL2(GOMPORES-GOXBLKD),AL1(GORALLQ)         
         DC    AL1(RQOMCTQ)                                                     
                                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* New order workflow status                                           *         
* 1st byte new workflow status, 2nd byte original workflow status     *         
* 3rd byte request status,                                            *         
* 4th byte request action,                                            *         
* 5th byte approval indicator                                         *         
* 6th byte behaviour to follow                                        *         
* 7th byte order record status byte 1                                 *         
* 8th byte order record status byte 2                                 *         
*                                                                               
* Entries sorted by request Action (add/status/maint etc)                       
***********************************************************************         
* Fully approved from fully approved - Goods received status only chnge         
NSTTAB   DC    AL1(XFLAPPRD,XFLAPPRD,RQRECVD,RQOMA3Q,0,0)                       
         DC    AL1(ORDGDRCV,ORDSAPPR+ORDSEXEX,RQRECVD)                          
* Completed from completed - Goods received status only change                  
         DC    AL1(XCMPLETE,XCMPLETE,RQRECVD,RQOMA3Q,0,0)                       
         DC    AL1(ORDGDRCV+ORDSFMCH,ORDSAPPR+ORDSEXEX,RQRECVD)                 
* Fully approved from complete - Reopen status only change                      
         DC    AL1(XFLAPPRD,XCMPLETE,RQOPEN,RQOMA3Q,0)                          
         DC    AL1(XOTXCHK+XOESCHK+XOTXADD)                                     
         DC    AL1(0,ORDSAPPR+ORDSEXEX,RQFULAPP)                                
* Cancelled from cancelled - Goods received status only change                  
         DC    AL1(XCANCELD,XCANCELD,RQRECVD,RQOMA3Q,0,0)                       
         DC    AL1(ORDGDRCV+ORDSFMCH,ORDSAPPR+ORDSEXEX,RQRECVD)                 
* Fully approved from cancelled - Reopen status only change                     
         DC    AL1(XFLAPPRD,XCANCELD,RQOPEN,RQOMA3Q,0)                          
         DC    AL1(XOTXCHK+XOESCHK+XOTXADD)                                     
         DC    AL1(0,ORDSAPPR+ORDSEXEX,RQFULAPP)                                
* Deleted from in progress - status only change                                 
         DC    AL1(XDLETED,XIPROGRS,RQDELETD,RQOMA3Q,0)                         
         DC    AL1(XCHKMAT)                                                     
         DC    AL1(ORDSLDEL,ORDSEXEX,RQDELETD)                                  
* fully approved from in progress - status only change                          
         DC    AL1(XFLAPPRD,XIPROGRS,RQSUBMIT,RQOMA3Q,XAFULAP)                  
         DC    AL1(XOGTSUB+XOESCHK+XOTXADD)                                     
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Part approved from in progress - status only change                           
         DC    AL1(XPRTAPRD,XIPROGRS,RQSUBMIT,RQOMA3Q,XASLFAP)                  
         DC    AL1(XOGTSUB+XOESCHK)                                             
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Submitted from in progress - status only change                               
         DC    AL1(XSBMITTD,XIPROGRS,RQSUBMIT,RQOMA3Q,0)                        
         DC    AL1(XOGTSUB+XOESCHK)                                             
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Deleted from submitted - status only change                                   
         DC    AL1(XDLETED,XSBMITTD,RQDELETD,RQOMA3Q,0)                         
         DC    AL1(XCHKMAT)                                                     
         DC    AL1(ORDSLDEL,ORDSEXEX,RQDELETD)                                  
* Deleted from part approved - status only change                               
         DC    AL1(XDLETED,XPRTAPRD,RQDELETD,RQOMA3Q,0)                         
         DC    AL1(XCHKMAT)                                                     
         DC    AL1(ORDSLDEL,ORDSEXEX,RQDELETD)                                  
* Deleted from rejected  - status only change                                   
         DC    AL1(XDLETED,XRJECTED,RQDELETD,RQOMA3Q,0)                         
         DC    AL1(XCHKMAT)                                                     
         DC    AL1(ORDSLDEL,ORDSEXEX,RQDELETD)                                  
* Completed from fully approved - status only change                            
         DC    AL1(XCMPLETE,XFLAPPRD,RQCLOSED,RQOMA3Q,0)                        
         DC    AL1(XOTXDEL+XOTXCHK)                                             
         DC    AL1(ORDSFMCH+ORDCLOSE,ORDSEXEX+ORDSAPPR,RQCLOSED)                
* Completed from completed - status only change                                 
         DC    AL1(XCMPLETE,XCMPLETE,RQCLOSED,RQOMA3Q,0)                        
         DC    AL1(XOTXDEL+XOTXCHK)                                             
         DC    AL1(ORDSFMCH+ORDCLOSE,ORDSEXEX+ORDSAPPR,RQCLOSED)                
* Cancelled from cancelled - status only change                                 
         DC    AL1(XCANCELD,XCANCELD,RQCLOSED,RQOMA3Q,0)                        
         DC    AL1(XOTXDEL+XOTXCHK)                                             
         DC    AL1(ORDSFMCH+ORDCLOSE,ORDSEXEX+ORDSAPPR,RQCLOSED)                
* Approved from submitted - status only change                                  
         DC    AL1(XFLAPPRD,XSBMITTD,RQPRTAPP,RQOMA3Q,XAFULAP)                  
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Approved from part approved - status only change                              
         DC    AL1(XFLAPPRD,XPRTAPRD,RQPRTAPP,RQOMA3Q,XAFULAP)                  
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Fully approved auto approval of an in progress order                          
         DC    AL1(XFLAPPRD,XIPROGRS,RQAUTO,RQOMA3Q,0)                          
         DC    AL1(XOESCHK+XOTXADD+XOGTSUB)                                     
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Part approved from submitted - status only change                             
         DC    AL1(XPRTAPRD,XSBMITTD,RQPRTAPP,RQOMA3Q,0)                        
         DC    AL1(XOAUESK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Part approved from part approved - status only change                         
         DC    AL1(XPRTAPRD,XPRTAPRD,RQPRTAPP,RQOMA3Q,0)                        
         DC    AL1(XOAUESK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Fully approved from rejected - status only change                             
         DC    AL1(XFLAPPRD,XRJECTED,RQPRTAPP,RQOMA3Q,XAFULAP)                  
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Part approved from rejected - status only change                              
         DC    AL1(XPRTAPRD,XRJECTED,RQPRTAPP,RQOMA3Q,0)                        
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Submitted from rejected - status only change                                  
         DC    AL1(XSBMITTD,XRJECTED,RQSUBMIT,RQOMA3Q,0)                        
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Rejected from submitted - status only change                                  
         DC    AL1(XRJECTED,XSBMITTD,RQREJECT,RQOMA3Q,0,0)                      
         DC    AL1(0,ORDSEXEX+ORDSOREJ,RQREJECT)                                
* Rejected from part approved - status only change                              
         DC    AL1(XRJECTED,XPRTAPRD,RQREJECT,RQOMA3Q,0,0)                      
         DC    AL1(0,ORDSEXEX+ORDSOREJ,RQREJECT)                                
* Rejected from fully approved - status only change                             
         DC    AL1(XRJECTED,XFLAPPRD,RQREJECT,RQOMA3Q,0,0)                      
         DC    AL1(0,ORDSEXEX+ORDSOREJ,RQREJECT)                                
* Rejected from rejected - maintain change                                      
         DC    AL1(XRJECTED,XRJECTED,0,RQOMA2Q,0,0)                             
         DC    AL1(0,ORDSEXEX+ORDSOREJ,RQREJECT)                                
* In progress from in progress - maintain change                                
         DC    AL1(XIPROGRS,XIPROGRS,RQDRAFT,RQOMA2Q,0,0)                       
         DC    AL1(0,ORDSEXEX+ORDSDRFT,RQDRAFT)                                 
* In progress from in progress - maintain change                                
         DC    AL1(XIPROGRS,XIPROGRS,0,RQOMA2Q,0,0)                             
         DC    AL1(0,ORDSEXEX+ORDSDRFT,RQDRAFT)                                 
* Submitted from in progress - maintain change                                  
         DC    AL1(XSBMITTD,XIPROGRS,RQSUBMIT,RQOMA2Q,0,XOGTSUB)                
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Part approved from submitted - maintain change                                
         DC    AL1(XPRTAPRD,XSBMITTD,RQSUBMIT,RQOMA2Q,XAPRSNT,0)                
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Part approved from submitted - maintain change                                
         DC    AL1(XPRTAPRD,XSBMITTD,0,RQOMA2Q,XAPRSNT,0)                       
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Submitted from submitted - maintain change                                    
         DC    AL1(XSBMITTD,XSBMITTD,RQSUBMIT,RQOMA2Q,0,0)                      
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Submitted from submitted - maintain change                                    
         DC    AL1(XSBMITTD,XSBMITTD,0,RQOMA2Q,0,0)                             
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Fully approved from fully approved - maintain change                          
         DC    AL1(XFLAPPRD,XFLAPPRD,0,RQOMA2Q,XANOCHG)                         
         DC    AL1(XOTXCHK+XOESCHK+XOTXDEL+XOTXADD)                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Fully approved from fully approved - maintain change                          
         DC    AL1(XFLAPPRD,XFLAPPRD,RQSUBMIT,RQOMA2Q,XAFULAP)                  
         DC    AL1(XOTXCHK+XOESCHK+XOTXDEL+XOTXADD)                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Fully approved from fully approved - maintain change                          
         DC    AL1(XFLAPPRD,XFLAPPRD,0,RQOMA2Q,XAFULAP)                         
         DC    AL1(XOTXCHK+XOESCHK+XOTXDEL+XOTXADD)                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Part approved from fully approved - maintain change/self approval             
         DC    AL1(XPRTAPRD,XFLAPPRD,RQSUBMIT,RQOMA2Q,XASLFAP)                  
         DC    AL1(XOTXCHK+XOESCHK+XOTXDEL)                                     
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Part approved from fully approved - maintain change/approvel present          
         DC    AL1(XPRTAPRD,XFLAPPRD,RQSUBMIT,RQOMA2Q,XAPRSNT)                  
         DC    AL1(XOESCHK+XOTXCHK+XOTXDEL)                                     
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Submitted from fully approved - maintain change/resubmit                      
         DC    AL1(XSBMITTD,XFLAPPRD,RQSUBMIT,RQOMA2Q,XARESUB)                  
         DC    AL1(XOTXCHK+XOESCHK+XOTXDEL)                                     
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Submitted from fully approved - maintain change/resubmit                      
         DC    AL1(XSBMITTD,XFLAPPRD,0,RQOMA2Q,XARESUB)                         
         DC    AL1(XOTXCHK+XOESCHK+XOTXDEL)                                     
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Fully approved from fully approved - maintain change                          
         DC    AL1(XFLAPPRD,XFLAPPRD,RQSUBMIT,RQOMA2Q,0)                        
         DC    AL1(XOTXCHK+XOESCHK+XOTXDEL+XOTXADD)                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Part approved from part approved - maintain change/no changes                 
         DC    AL1(XPRTAPRD,XPRTAPRD,RQSUBMIT,RQOMA2Q,XANOCHG,0)                
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQPRTAPP)                                
* Part approved from part approved - maintain change/no changes                 
         DC    AL1(XPRTAPRD,XPRTAPRD,0,RQOMA2Q,XANOCHG,0)                       
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Fully approved from part approved - maintain change                           
         DC    AL1(XFLAPPRD,XPRTAPRD,RQSUBMIT,RQOMA2Q,XAFULAP)                  
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Part approved from part approved - maintain change                            
         DC    AL1(XPRTAPRD,XPRTAPRD,RQSUBMIT,RQOMA2Q,XASLFAP)                  
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Completed from completed - maintain change                                    
         DC    AL1(XCMPLETE,XCMPLETE,0,RQOMA2Q,0,0)                             
         DC    AL1(ORDSFMCH,ORDSAPPR+ORDSEXEX,RQCLOSED)                         
* Cancelled from cancelled - maintain change                                    
         DC    AL1(XCANCELD,XCANCELD,0,RQOMA2Q,0,0)                             
         DC    AL1(ORDSFMCH,ORDSAPPR+ORDSEXEX,RQCLOSED)                         
* Submitted from Part approved when order changed and resubmit required         
         DC    AL1(XSBMITTD,XPRTAPRD,RQSUBMIT,RQOMA2Q,XARESUB)                  
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Submitted from Part approved when order changed and resubmit required         
         DC    AL1(XSBMITTD,XPRTAPRD,0,RQOMA2Q,XARESUB)                         
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Fully approved from fully approved - auto approval of changed order           
         DC    AL1(XFLAPPRD,XFLAPPRD,RQAUTO,RQOMA2Q,0)                          
         DC    AL1(XOTXCHK+XOESCHK+XOTXDEL+XOTXADD)                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Part approved to Part approved when order changed                             
         DC    AL1(XPRTAPRD,XPRTAPRD,RQSUBMIT,RQOMA2Q,0)                        
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Fully approved from submitted - status and maintain change                    
         DC    AL1(XFLAPPRD,XSBMITTD,RQPRTAPP,RQOMA4Q,XAFULAP)                  
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Fully approved from part approved - status and maintain change                
         DC    AL1(XFLAPPRD,XPRTAPRD,RQPRTAPP,RQOMA4Q,XAFULAP)                  
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Fully approved from part approved - status and maintain change                
         DC    AL1(XFLAPPRD,XPRTAPRD,RQSUBMIT,RQOMA4Q,XAFULAP)                  
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Self Fully approved from Submitted - status and maintain change               
         DC    AL1(XFLAPPRD,XSBMITTD,RQSUBMIT,RQOMA4Q,XAFULAP)                  
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Self Part approved from Submitted - status and maintain change                
         DC    AL1(XPRTAPRD,XSBMITTD,RQSUBMIT,RQOMA4Q,XASLFAP)                  
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Auto-approved from part approved - status and maintain change                 
         DC    AL1(XFLAPPRD,XPRTAPRD,RQAUTO,RQOMA4Q,0)                          
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Auto-approved from submitted - status and maintain change                     
         DC    AL1(XFLAPPRD,XSBMITTD,RQAUTO,RQOMA4Q,0)                          
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Part approved from submitted - status and maintain change                     
         DC    AL1(XPRTAPRD,XSBMITTD,RQPRTAPP,RQOMA4Q,0,0)                      
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Part approved from rejected - status and maintain change                      
         DC    AL1(XPRTAPRD,XRJECTED,RQPRTAPP,RQOMA4Q,XAPRSNT)                  
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Part approved from part approved - status and maintain change                 
         DC    AL1(XPRTAPRD,XPRTAPRD,RQPRTAPP,RQOMA4Q,0,0)                      
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Part approved from part approved - status and maintain change                 
         DC    AL1(XPRTAPRD,XPRTAPRD,RQSUBMIT,RQOMA4Q,XAPRSNT,0)                
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Part approved from part approved - status and maintain change                 
         DC    AL1(XPRTAPRD,XPRTAPRD,RQSUBMIT,RQOMA4Q,XANOCHG,0)                
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Submitted from part approved - status and maintain change                     
         DC    AL1(XSBMITTD,XPRTAPRD,RQSUBMIT,RQOMA4Q,XARESUB)                  
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Fully approved from rejected - status and maintain change                     
         DC    AL1(XFLAPPRD,XRJECTED,RQSUBMIT,RQOMA4Q,XAFULAP)                  
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Part approved from rejected - status and maintain change                      
         DC    AL1(XPRTAPRD,XRJECTED,RQSUBMIT,RQOMA4Q,XASLFAP)                  
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Part approved from rejected - status and maintain change                      
         DC    AL1(XPRTAPRD,XRJECTED,RQSUBMIT,RQOMA4Q,XAPRSNT)                  
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Submitted from rejected - status and maintain change                          
         DC    AL1(XSBMITTD,XRJECTED,RQSUBMIT,RQOMA4Q,XARESUB)                  
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Submitted from rejected - status and maintain change                          
         DC    AL1(XSBMITTD,XRJECTED,RQSUBMIT,RQOMA4Q,0)                        
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* fully approved from in progress - status and maintain change                  
         DC    AL1(XFLAPPRD,XIPROGRS,RQSUBMIT,RQOMA4Q,XAFULAP)                  
         DC    AL1(XOGTSUB+XOESCHK+XOTXADD)                                     
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Part approved from in progress - status and maintain change                   
         DC    AL1(XPRTAPRD,XIPROGRS,RQSUBMIT,RQOMA4Q,XASLFAP)                  
         DC    AL1(XOGTSUB+XOESCHK)                                             
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Submitted from in progress - status and maintain change                       
         DC    AL1(XSBMITTD,XIPROGRS,RQSUBMIT,RQOMA4Q,0)                        
         DC    AL1(XOGTSUB+XOESCHK)                                             
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Submitted from submitted - status and maintain change                         
         DC    AL1(XSBMITTD,XSBMITTD,RQSUBMIT,RQOMA4Q,0)                        
         DC    AL1(XOESCHK)                                                     
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* Rejected from submitted - status and maintain change                          
         DC    AL1(XRJECTED,XSBMITTD,RQREJECT,RQOMA4Q,0,0)                      
         DC    AL1(0,ORDSEXEX+ORDSOREJ,RQREJECT)                                
* Rejected from part approved - status and maintain change                      
         DC    AL1(XRJECTED,XPRTAPRD,RQREJECT,RQOMA4Q,0,0)                      
         DC    AL1(0,ORDSEXEX+ORDSOREJ,RQREJECT)                                
* Rejected from fully approved - status and maintain change                     
         DC    AL1(XRJECTED,XFLAPPRD,RQREJECT,RQOMA4Q,0,0)                      
         DC    AL1(0,ORDSEXEX+ORDSOREJ,RQREJECT)                                
* Deleted from in progress - status and maintain change                         
         DC    AL1(XDLETED,XIPROGRS,RQDELETD,RQOMA4Q,0)                         
         DC    AL1(XCHKMAT)                                                     
         DC    AL1(ORDSLDEL,ORDSEXEX,RQDELETD)                                  
* Deleted from submitted - status and maintain change                           
         DC    AL1(XDLETED,XSBMITTD,RQDELETD,RQOMA4Q,0)                         
         DC    AL1(XCHKMAT)                                                     
         DC    AL1(ORDSLDEL,ORDSEXEX,RQDELETD)                                  
* Deleted from part approved - status and maintain change                       
         DC    AL1(XDLETED,XPRTAPRD,RQDELETD,RQOMA4Q,0)                         
         DC    AL1(XCHKMAT)                                                     
         DC    AL1(ORDSLDEL,ORDSEXEX,RQDELETD)                                  
* Deleted from rejected - status and maintain change                            
         DC    AL1(XDLETED,XRJECTED,RQDELETD,RQOMA4Q,0)                         
         DC    AL1(XCHKMAT)                                                     
         DC    AL1(ORDSLDEL,ORDSEXEX,RQDELETD)                                  
* Fully approved auto approval of a rejected order                              
         DC    AL1(XFLAPPRD,XRJECTED,RQAUTO,RQOMA4Q,0)                          
         DC    AL1(XOESCHK+XOTXADD)                                             
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Fully approved auto approval of an in progress order                          
         DC    AL1(XFLAPPRD,XIPROGRS,RQAUTO,RQOMA4Q,0)                          
         DC    AL1(XOESCHK+XOTXADD+XOGTSUB)                                     
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Fully approved auto approval of new order                                     
         DC    AL1(XFLAPPRD,0,RQAUTO,RQOMA1Q,0)                                 
         DC    AL1(XOESCHK+XOTXADD+XOGTSUB)                                     
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Self approval on submit - add                                                 
         DC    AL1(XFLAPPRD,0,RQSUBMIT,RQOMA1Q,XAFULAP)                         
         DC    AL1(XOESCHK+XOTXADD+XOGTSUB)                                     
         DC    AL1(0,ORDSEXEX+ORDSAPPR,RQFULAPP)                                
* Part approval on submit - add                                                 
         DC    AL1(XPRTAPRD,0,RQSUBMIT,RQOMA1Q,XASLFAP)                         
         DC    AL1(XOESCHK+XOGTSUB)                                             
         DC    AL1(0,ORDSEXEX+ORDSPAPP,RQPRTAPP)                                
* Submitted from new - add                                                      
         DC    AL1(XSBMITTD,0,RQSUBMIT,RQOMA1Q,0)                               
         DC    AL1(XOESCHK+XOGTSUB)                                             
         DC    AL1(0,ORDSEXEX+ORDSSUBM,RQSUBMIT)                                
* In progress added new                                                         
         DC    AL1(XIPROGRS,0,RQDRAFT,RQOMA1Q,0,0)                              
         DC    AL1(0,ORDSEXEX+ORDSDRFT,RQDRAFT)                                 
* In progress added new, draft not sent by web front end                        
         DC    AL1(XIPROGRS,0,0,RQOMA1Q,0,0)                                    
         DC    AL1(0,ORDSEXEX+ORDSDRFT,RQDRAFT)                                 
         DC    X'FF'                                                            
*                                                                               
                                                                                
AUDTAB1  DS    0X                 For added and amended orders                  
* Order date                                                                    
         DC    AL1(ORDELQ,0,0,STCOIND1-STCELD)                                  
         DC    AL1(STCODTEQ,STCODATE-STCELD,L'STCODATE,ORDDATE-ORDELD)          
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,0,0)                                 
* Required date                                                                 
         DC    AL1(ORDELQ,0,0,STCOIND1-STCELD)                                  
         DC    AL1(STCORQDQ,STCORQDT-STCELD,L'STCORQDT,ORDRQBD-ORDELD)          
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,0,0)                                 
* Expense account                                                               
         DC    AL1(ORDELQ,0,0,STCOIND3-STCELD)                                  
         DC    AL1(STCOXACQ,STCOXULA-STCELD,L'STCOXULA,ORDACCU-ORDELD)          
         DC    AL1(EXPOQ,0,0,0,AUDOCHRS,0)                                      
* Client Product job for all orders apart from expense                          
         DC    AL1(ORDELQ,0,0,STCOIND1-STCELD)                                  
         DC    AL1(STCOCLIQ+STCOPROQ+STCOJOBQ)                                  
         DC    AL1(STCOSJAC-STCELD,L'STCOSJAC,ORDACCA-ORDELD)                   
         DC    AL1(PROOQ,INTOQ,ARTOQ,0,AUDOCHRS,0)                              
* Supplier                                                                      
         DC    AL1(ORDELQ,0,0,STCOIND2-STCELD)                                  
         DC    AL1(STCOSUPQ,STCOSULA-STCELD,L'STCOSULA,ORDSUPU-ORDELD)          
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDOCHRS,0)                          
* Department                                                                    
         DC    AL1(SPAELQ,SPATYPE-SPAELD,SPATDEPT)                              
         DC    AL1(STCOIND2-STCELD)                                             
         DC    AL1(STCODPTQ,STCO2DAC-STCELD,L'STCO2DAC,SPAAACT-SPAELD)          
         DC    AL1(EXPOQ,0,0,0,AUDOCHRS,0)                                      
* Staff                                                                         
         DC    AL1(SPAELQ,SPATYPE-SPAELD,SPATPERS)                              
         DC    AL1(STCOIND2-STCELD)                                             
         DC    AL1(STCOSTFQ,STCO2PAC-STCELD,L'STCO2PAC,SPAAACT-SPAELD)          
         DC    AL1(EXPOQ,0,0,0,AUDOCHRS,0)                                      
* Foreign currency                                                              
         DC    AL1(AFCELQ,0,0,STCOIND2-STCELD)                                  
         DC    AL1(STCOFCQ,STCOCURC-STCELD,L'STCOCURC,AFCCURR-AFCELD)           
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,0,0)                                 
* Foreign currency amount                                                       
         DC    AL1(AFCELQ,0,0,STCOIND2-STCELD)                                  
         DC    AL1(STCOFCAQ,STCOFCAM-STCELD,L'STCOFCAM,AFCAMNT-AFCELD)          
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,0,0)                                 
* For attention                                                                 
         DC    AL1(FFTELQ,FFTTYPE-FFTELD,FFTTSATN)                              
         DC    AL1(STCOIND4-STCELD)                                             
         DC    AL1(STCOATNQ,0,0,FFTDATA-FFTELD)                                 
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDOCHRS+AUDDTVAR,0)                 
* Workcode for expense order                                                    
         DC    AL1(FFTELQ,FFTTYPE-FFTELD,FFTTWRKC)                              
         DC    AL1(STCOIND1-STCELD)                                             
         DC    AL1(STCOWCQ,STCOWC-STCELD,L'STCOWC,FFTWORK-FFTELD)               
         DC    AL1(EXPOQ,0,0,0,0,0)                                             
* Etype                                                                         
         DC    AL1(FFTELQ,FFTTYPE-FFTELD,FFTTEXTY)                              
         DC    AL1(STCOIND1-STCELD)                                             
         DC    AL1(STCOETYQ,STCOETYP-STCELD,L'STCOETYP,FFTDATA-FFTELD)          
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDOCHRS,0)                          
* Estimate number                                                               
         DC    AL1(FFTELQ,FFTTYPE-FFTELD,FFTTESTN)                              
         DC    AL1(STCOIND4-STCELD)                                             
         DC    AL1(STCOESNQ,0,L'FFTESTN,FFTDATA-FFTELD)                         
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDOCHRS,0)                          
* Office                                                                        
         DC    AL1(FFTELQ,FFTTYPE-FFTELD,FFTTOFFC)                              
         DC    AL1(STCOIND2-STCELD)                                             
         DC    AL1(STCOOFFQ,STCO2DAC-STCELD,L'STCO2DAC,FFTDATA-FFTELD)          
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDOCHRS+AUDPREV,0)                  
* Client product job for expense orders                                         
         DC    AL1(SORELQ,0,0,STCOIND1-STCELD)                                  
         DC    AL1(STCOCLIQ+STCOPROQ+STCOJOBQ)                                  
         DC    AL1(STCOSJAC-STCELD,L'STCOSJAC,SORAACT-SORELD)                   
         DC    AL1(EXPOQ,0,0,0,AUDOCHRS,0)                                      
* Order name                                                                    
         DC    AL1(ENMELQ,0,0,STCOIND2-STCELD)                                  
         DC    AL1(STCONAMQ,0,0,ENMNAME-ENMELD)                                 
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDDTVAR+AUDNSTON,0)                 
* Order address                                                                 
         DC    AL1(OATELQ,0,0,STCOIND3-STCELD)                                  
         DC    AL1(STCODADQ,0,0,OATADD1-OATELD)                                 
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDDTVAR+AUDNSTON,0)                 
* Approvers                                                                     
         DC    AL1(PIDELQ,0,0,STCOIND3-STCELD)                                  
         DC    AL1(STCOAPRQ,0,L'PIDNO,PIDNTRS+1-PIDELD)                         
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDSUBEL+AUD1ERR+AUDMULT)            
         DC    AL1(L'PIDNTRS)                                                   
* Printing description - initial element                                        
         DC    AL1(SCMELQ,SCMTYPE-SCMELD,SCMTSTND)                              
         DC    AL1(STCOIND3-STCELD)                                             
         DC    AL1(STCOPDSQ,0,0,SCMTBOX-SCMELD)                                 
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDDTVAR+AUDNSTON,0)                 
* Printing description - extension elements                                     
         DC    AL1(SCMELQ,SCMTYPE-SCMELD,SCMTOPDX)                              
         DC    AL1(STCOIND3-STCELD)                                             
         DC    AL1(STCOPDSQ,0,0,SCMTBOX-SCMELD)                                 
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDDTVAR+AUDMULT+AUDNSTON,0)         
* Header text                                                                   
         DC    AL1(SCMELQ,SCMTYPE-SCMELD,SCMTPRBD)                              
         DC    AL1(STCOIND3-STCELD)                                             
         DC    AL1(STCOHEDQ,0,0,SCMTBOX-SCMELD)                                 
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDDTVAR+AUDMULT+AUDNSTON,0)         
* Footer text                                                                   
         DC    AL1(SCMELQ,SCMTYPE-SCMELD,SCMTPRAD)                              
         DC    AL1(STCOIND3-STCELD)                                             
         DC    AL1(STCOFOTQ,0,0,SCMTBOX-SCMELD)                                 
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDDTVAR+AUDMULT+AUDNSTON,0)         
* Matching text                                                                 
         DC    AL1(SCMELQ,SCMTYPE-SCMELD,SCMTOMOC)                              
         DC    AL1(STCOIND3-STCELD)                                             
         DC    AL1(STCOMDSQ,0,0,SCMTBOX-SCMELD)                                 
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDDTVAR+AUDNSTON,0)                 
* Item text                                                                     
         DC    AL1(SCMELQ,SCMTYPE-SCMELD,SCMTSANP)                              
         DC    AL1(STCOIND4-STCELD)                                             
         DC    AL1(STCOITXQ,0,0,SCMTEXT-SCMELD)                                 
         DC    AL1(PROOQ,EXPOQ,ARTOQ,INTOQ,AUDDTVAR+AUDMULT+AUDNSTON,0)         
                                                                                
AUDTAB1X DC    X'FF'                                                            
                                                                                
                                                                                
         EJECT                                                                  
SAVED    DSECT                                                                  
XNENDELE DS    F                   End of new element                           
XOENDELE DS    F                   End of old element                           
SAVER0   DS    XL4                 saved register R0                            
SAVERF   DS    F                   Temp save area for RF                        
TSARELEN DS    XL(TSPXTNL)         TSAR block for new text/items/xdata          
TSARELEO DS    XL(TSPXTNL)         TSAR block for old text/items/xdata          
TSARSTCL DS    XL(TSPXTNL)         TSAR block for audit elements                
ELSEQ#   DS    XL2                 Element sequence for BUFELE                  
TSAERR   DS    XL1                 TSAR buffer error                            
                                                                                
XMQSTAT  DS    XL1                 MQ indicator                                 
XMQSMQQ  EQU   X'80'               - company uses this feature                  
XMQSAPQ  EQU   X'40'               - full approval scenario                     
XMQSFCQ  EQU   X'20'               - FC order                                   
                                                                                
OR_TODP  DS    PL3                 Today's date packed                          
OR_TODC  DS    XL2                 Today's date compressed                      
OR_TODF  DS    CL6                 Today's date character                       
NEWONUM  DS    CL6                 Order number                                 
NEWRNUM  DS    CL6                 Requisition number                           
LSTONUM  DS    CL6                 Last order number                            
LSTRNUM  DS    CL6                 Last requisistion number                     
ORDTYPE  DS    CL1                 Order type                                   
XREQNUM  DS    CL1                 Y/N Requisition number in use                
XSKPVAL  DS    CL1                 Skip validation - reject close delte         
XOWRKST  DS    XL1                 Old workflow status                          
XIPROGRS EQU   STCOINPR            In progress                                  
XSBMITTD EQU   STCOSUBD            Submitted                                    
XPRTAPRD EQU   STCOPAPR            Part approved                                
XRJECTED EQU   STCOREJT            Rejected                                     
XFLAPPRD EQU   STCOAPPD            Fully approved                               
XCMPLETE EQU   STCOCOMP            Completed                                    
XDLETED  EQU   STCODELT            Deleted                                      
XCANCELD EQU   STCOCAND            Cancelled                                    
XNWRKST  DS    XL1                 New workflow status                          
XOMATST  DS    XL1                 Matching + good received status              
XPRTMAT  EQU   X'80'               Part matched                                 
XFULMAT  EQU   X'40'               Fully matched                                
XGDRCVD  EQU   X'20'               Goods received                               
XPREORD  DS    CL6                 Previous order number                        
XNEWORD  DS    CL6                 Next order number                            
XORDSTA  DS    XL1                 ORDSTAT from order element                   
XOMACT   DS    CL1                 Copy of RQOMACT - Action                     
XOMSTA   DS    CL1                 Copy of RQOMSTA - 'To' status                
XOMCAO   DS    CL1                 Copy of RQOMCAO - Change to apprvd           
XOMTYP   DS    CL1                 Copy of RQOMTYP - Type of order              
XOUTSTA  DS    CL1                 Output - 'To' status                         
XORDIND  DS    XL1                 Order indicator                              
XOESCHK  EQU   X'80'               Estimate amount check                        
XOACCHK  EQU   X'40'               Account code check                           
XOTXCHK  EQU   X'20'               Trx for pending activity check               
XOTXDEL  EQU   X'10'               Delete order transction                      
XOTXADD  EQU   X'08'               Add order transction                         
XOAUESK  EQU   X'04'               Aura only estimate check                     
XCHKMAT  EQU   X'02'               Check matching status                        
XOGTSUB  EQU   X'01'               Get submitter on to order element            
                                                                                
XAPPIND  DS    XL1                 Approval indicator                           
XASLFAP  EQU   X'80'               Self approved                                
XAFULAP  EQU   X'40'               Order has no outstanding approvals           
XARESUB  EQU   X'20'               Resubmit order for approval                  
XANOCHG  EQU   X'10'               No change made to order                      
XAPRSNT  EQU   X'08'               Some approvals are present                   
                                                                                
XNOSTA1  DS    XL1                 New order record status byte 1               
XOOSTA1  DS    XL1                 Old order record status byte 1               
XNOSTA2  DS    XL1                 New order record status byte 2               
XOOSTA2  DS    XL1                 Old order record status byte 2               
XOLGSTA  DS    XL1                 Previous GAP status                          
XOLGAPEX DS    XL3                 Previous GAP expiry date                     
XOLGEMLN DS    XL1                 Previous GAP number of emails                
XGAPYN   DS    XL1                 RSTACST1 from supplier record                
XDFEXPO  DS    PL2                 GAP default expiration period                
XSUPEM   DS    CL50                Supplier email address                       
XORDNM   DS    CL6                 Order number                                 
CESTNUM  DS    CL6                 Current estimate number                      
XL#ORPF  DS    XL(ORDPRFL)         Order profiles                               
                                                                                
XSTCLN   DS    XL1                 Status comment change length                 
XSTCCO   DS    CL(L'STCOCOM)       Status comment change                        
XNAMLN   DS    XL1                 Order name length                            
XONAME   DS    CL100               Order name                                   
XDALNN   DS    XL1                 Delivery address length                      
XDABOX   DS    CL(L'OATDABOX)      Delivery address                             
                                                                                
XORDIND2 DS    XL1                 Order indicator 2                            
XORDISUP EQU   X'80'               Supplier has changed                         
XGAPSTAT DS    CL1                 GAP status                                   
XGAPEXPD DS    PL3                 GAP expiry date                              
*                                                                               
XGAPEMLN DS    XL1                                                              
                                                                                
XORDOWN  DS    XL2                                                              
XORDSUB  DS    XL2                                                              
XONSRCE  DS    CL1                                                              
XINTLED  DS    CL1                                                              
XSUPOFF  DS    CL2                                                              
XCOUNT   DS    PL4                                                              
XSWAPQ   DS    CL1                                                              
XRETIDN  DS    CL4                                                              
XSEQNO   DS    XL1                                                              
XSUBNO   DS    XL1                                                              
XTXTLN   DS    XL1                                                              
XAMOUNT  DS    PL8                                                              
XIDNUM   DS    0XL4                                                             
XIDNEW   DS    XL2            New ID number                                     
XIDOLD   DS    XL2            Old ID number                                     
XARTOFS  DS    XL1                                                              
XCSTCDE  DS    CL1            Cost code                                         
XANAIND  DS    XL1            Analysis indicator                                
XANAPER  EQU   X'08'          person                                            
XANADEP  EQU   X'80'          department                                        
XANACST  EQU   X'10'          cost postings for expense ledger                  
XATOLEN  DS    XL1                                                              
XATOTXT  DS    CL36                                                             
XMATLEN  DS    XL1                                                              
XMATTXT  DS    CL120                                                            
XPRILEN  DS    XL1                                                              
XPRITXT  DS    CL240                                                            
XISUBS   DS    XL1                                                              
XORDDT   DS    XL3            Order date                                        
XORDDA   DS    XL4            Order record disk address                         
XTRNDA   DS    XL4            Transaction record disk address                   
XORDRFM  DS    CL8            Report format for PDFs                            
XORDOWNR DS    XL2            PIN of order owner                                
XISEQN   DS    XL3            Item sequence number                              
XELECOD  DS    XL1            Work area for element code                        
XELESUB  DS    XL1            Work area for element sub type                    
                                                                                
XESTWYN  DS    CL1            Y/N going over estimate warning                   
XESTWNO  DS    XL2            Error message for estimate warning                
XTOTOE   DS    PL6            Total original estimated amount                   
XTOTCE   DS    PL6            Total current estimated amount                    
XTOTHR   DS    PL6            Total highest revision estimated amount           
XTOTAM   DS    PL6            Total ord amt less tot invoiced amt               
XTOTOR   DS    PL6            uninv amt+posted amt incl time, exp etc           
XJOBLE   DS    PL6            Job level estimate check remaining amount         
X#PL16   DS    PL16                                                             
XTOTNET  DS    PL6            Temporary net amount                              
XTOTNETF DS    PL6            Temporary foreign net amount                      
XTOTCDSC DS    PL6            Temporary cash discount amount                    
XTOTCOM  DS    PL6            Temporary commission amount                       
                                                                                
XOSVAUT  DS    CL15                Authoriser - Not in use                      
XOSVRBD  DS    XL3                 Order required by date                       
XOSVEXC  DS    CL14                Foreign exchange rate rule                   
XOSVPRA  DS    CL1                 Print amounts on PDF Y/N                     
XOSVPRT  DS    CL1                 Print texts on PDF Y/N                       
                                                                                
XSVPAPP  DS    XL(((XPMAXQ*2)+1)*L'PIDNO)                                       
                                                                                
XORDOFF  DS    CL2                 Order office                                 
XCPJOFF  DS    CL2                 Client/product office                        
XEXPTYP  DS    CL3                 Expenditure type                             
XORDCUR  DS    CL3                 Order currency                               
XEXCPWC  DS    CL2                 Expense order workcode                       
                                                                                
XSUPCULA DS    XL15                Supplier company unit ledger account         
XSUPNAME DS    CL36                Supplier name                                
XSUPFNAM DS    CL36                Supplier foreign name                        
XSUPCURR DS    CL3                 Supplier currency                            
XSUPLLVL DS    CL1                 Supplier present Y/N                         
                                                                                
XCLICULA DS    XL15                Client - company unit ledger account         
XPROCULA DS    XL15                Prod - company unit ledger account           
XJOBCULA DS    XL15                Job - company unit ledger account            
XCLIC    DS    CL6                 Client code                                  
XPROC    DS    CL6                 Product code                                 
XJOBC    DS    CL7                 Job code                                     
                                                                                
XEXPCULA DS    XL15                Expense - comp unit ledger account           
XEXPLLVL DS    CL1                 Expense account present Y/N                  
                                                                                
XDEPCULA DS    XL15                Dept - company unit ledger account           
XSTACULA DS    XL15                Staff - company unit ledger account          
                                                                                
*&&US                                                                           
XTOTCWC  DS    PL3                 Total of commissionable W/C                  
XTOTNWC  DS    PL3                 Total of noncommissionable W/C               
XBYTEN   DS    XL1                                                              
XBYTEC   DS    XL1                                                              
*&&                                                                             
XESJLR   DS    PL6                 Job level remaining amount                   
XESCKE   DS    XL1                 Estimate check error message                 
OA_NCEQ  EQU   1                   No current estimate                          
OA_NAEQ  EQU   2                   No approved estimate                         
OA_ORJQ  EQU   3                   Order exceeds job total                      
OA_OESQ  EQU   4                   Order exceeds est total                      
OA_OHSQ  EQU   5                   Order exceeds amt on highest sub est         
XESCWM   DS    XL1                 Estimate check warning message               
                                                                                
XSVORDEL DS    XL(ORDLN4Q)         Saved order element                          
                                                                                
XPFLAG   DS    XL1                                                              
XPVALS   DS    (XPMAXQ)XL(XPIDLQ)                                               
                                                                                
XOTOTAMT DS    PL6                 Total old order amount                       
XTOTAMT  DS    PL6                 Total new order amount                       
XOTOTFCA DS    PL6                 Total old order foreign curr amount          
XTOTFCA  DS    PL6                 Total new order foreign curr amount          
                                                                                
X#RTTRX  DS    XL(TMSTTABL)        R time type settings                         
X#BTTRX  DS    XL(TMSTTABL)        B time type settings                         
TMSTNUM  EQU   (*-X#RTTRX)/TMSTTABL                                             
                                                                                
XWCNTR   DS    XL1                 Workcode counter                             
                                                                                
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      Record type                                  
                                                                                
RQUPVAL  DS    2000X               see SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
                                                                                
         ORG   RQUPVAL                                                          
RQORDER  DS    0H                  Order upload data                            
                                                                                
RQOURTY  DS    CL1                 Order upload record type                     
RQOURT1  EQU   C'1'                - main header                                
RQOURT2  EQU   C'2'                - approvers                                  
RQOURT3  EQU   C'3'                - work codes                                 
RQOURT4  EQU   C'4'                - article/items                              
RQOURT5  EQU   C'5'                - texts                                      
RQOURT6  EQU   C'6'                - xtra data                                  
RQOURT9  EQU   C'9'                - last for update                            
                                                                                
RQOMACT  DS    CL1                 Action                                       
RQOMA1Q  EQU   C'1'                - add                                        
RQOMA2Q  EQU   C'2'                - maintain only  GAP                         
RQOMA3Q  EQU   C'3'                - status change only                         
RQOMA4Q  EQU   C'4'                - maintain and status change                 
RQOMORD  DS    CL6                 Order number                                 
RQOMTYP  DS    CL1                 Order type (see ORDTYPE in WRKD)             
RQOMOFF  DS    CL2                 Office code                                  
RQOMETY  DS    CL3                 Expenditure type                             
RQOMSUP  DS    CL14                Supplier account                             
RQOMEXP  DS    CL14                Expense account                              
RQOMSJA  DS    CL12                SJ account (no U/L)                          
RQOMDEP  DS    CL12                Department account (no U/L)                  
RQOMPER  DS    CL12                Person account (no U/L)                      
RQOMAUT  DS    CL15                Authoriser ***NOT IN USE***                  
RQOMATO  DS    CL36                Attention of                                 
RQOMRBD  DS    XL3                 Required by date (PDAT)                      
RQOMCUR  DS    CL3                 Currency code                                
RQOMTOT  DS    CL16                Total amount (agency curr)                   
RQOMTFC  DS    CL16                Total amount (foreign curr)                  
RQOMEXC  DS    CL14                Exchange rate block                          
RQOMMTXL DS    XL1                 Matching text length                         
RQOMMTX  DS    CL120               Matching text                                
RQOMCTXL DS    XL1                 Printing text length                         
RQOMCTX  DS    CL240               Printing text                                
RQOMDADL DS    XL1                 Delivery address length                      
RQOMDAD  DS    CL240               Delivery address                             
RQOMEST  DS    CL6                 Estimate number                              
RQOMDAT  DS    XL3                 Order date (PDAT)                            
RQOMPRA  DS    CL1                 Print amounts on PDF Y/N                     
RQOMPRT  DS    CL1                 Print texts on PDF Y/N                       
RQOMEOW  DS    CL2                 Expense order w/c                            
RQOMSTA  DS    CL1                 'To' status                                  
RQDRAFT  EQU   STCODRFT            D Draft                                      
RQSUBMIT EQU   STCOSUBM            S Submit                                     
RQPRTAPP EQU   STCOPAPP            P Part approved                              
RQFULAPP EQU   STCOFAPP            A Fully approved                             
RQRECVD  EQU   STCOGRCV            G Goods received                             
RQREJECT EQU   STCOREJ             R Rejected                                   
RQDELETD EQU   STCOLDLT            L Logically deleted                          
RQCLOSED EQU   STCOCLOS            C Closed                                     
RQOPEN   EQU   STCOOPEN            O Open                                       
RQAUTO   EQU   STCOANO             N Auto or no approval                        
RQOMCOML DS    XL1                 Status change comment length                 
RQOMCOM  DS    CL(L'STCOCOM)       Status change comment                        
RQOMNAML DS    XL1                 Order name length                            
RQOMNAM  DS    CL100               Order name                                   
RQOMCAO  DS    CL1                 Change approved order?                       
RQOMCTQ  EQU   C'T'                text change only                             
RQOMCYQ  EQU   C'Y'                for account amount wc changes                
RQOMCBQ  EQU   C'B'                both of the above                            
RQOMCNQ  EQU   X'00'               None                                         
RQOMIOL  DS    CL1                 Internal order ledger (I/K)                  
RQOMRFM  DS    CL8                 Report format                                
RQOWNER  DS    CL8                 Order 'owner'                                
RQOGAPST DS    CL1                 GAP status                                   
RQOGAPED DS    PL3                 GAP expiry date                              
*                                                                               
RQOGAPEI DS    X                                                                
RQOGAPEM DS    AL3                 email address                                
*                                                                               
RQOGACII DS    AL4                 GAP comment array                            
                                                                                
RQOALVL  DS    CL1                 Approver level 1-4                           
RQOAKEP  DS    CL1                 Approver keep Y/N                            
RQOAPP1  DS    CL8                 First approver on level                      
RQOAPP2  DS    CL8                 Second approver on level                     
                                                                                
RQOWCOD  DS    CL2                 Work code                                    
RQOWAMT  DS    CL16                Amount                                       
RQOWFCA  DS    CL16                Foreign currency amount                      
                                                                                
RQOTTYP  DS    CL1                 Text type                                    
RQOTTIQ  EQU   C'I'                - item text                                  
RQOTTTQ  EQU   C'T'                - text box                                   
RQOTTHQ  EQU   C'H'                - header text                                
RQOTTFQ  EQU   C'F'                - footer text                                
RQOTSEQ  DS    CL1                 Sequence (item text)                         
RQOTSSQ  DS    CL1                 Sub sequence (item text)                     
RQOTWCD  DS    CL2                 Work code (item text)                        
RQOTAMT  DS    CL16                Amount (item text)                           
RQOTTXTL DS    XL1                 Text length (item text)                      
RQOTTXT  DS    CL200               Text (item text)                             
RQOTBOXL DS    XL1                 Text length (text box)                       
RQOTBOX  DS    CL240               Text (text box)                              
                                                                                
RQOIWCD  DS    CL2                 W/C cell work code                           
RQOIWSN  DS    CL1                 W/C cell sequence number                     
RQOIWSS  DS    CL1                 W/C cell sub sequence number                 
RQOICOD  DS    CL4                 Item code                                    
RQOISEQ  DS    CL6                 Item sequence                                
RQOIPRI  DS    CL16                Item price selected                          
RQOIMUL  DS    CL16                Item multiplier                              
RQOIOVR  DS    CL1                 Item price overridden?                       
RQOIDSCL EQU   RQOMNAML            Item description (50)                        
RQOIDSC  EQU   RQOMNAM             Item description (50)                        
RQOITXTL EQU   RQOTBOXL            Item Text        (240)                       
RQOITXT  EQU   RQOTBOX             Item Text        (240)                       
                                                                                
RQOXCOD  DS    CL6                 Xtra data code                               
RQOXTYP  DS    CL1                 Xtra data type                               
RQOXDTAL DS    XL1                 Xtra data length                             
RQOXDTA  DS    CL90                Xtra data data                               
                                                                                
RQOLIDN  DS    CL4                 ID Number (on last for update)               
RQOLCCR  DS    CL1                 Code check required                          
RQOLEST  DS    CL1                 Skip estimate check                          
RQOWCNM  DS    CL(L'SCMWODS)       Work Code name/description                   
                                                                                
         DS    XL((RQUPLNQ)-(*-RQORDER))                                        
                                                                                
XWVALS   DS    25XL(XWCLNQ)        Workcodes and amounts                        
XWVALQ   EQU   *-XWVALS                                                         
SVESTWK  DS    200CL(L'JBCOLWC+L'JBCOLVAL)  (1600)                              
SVESTWKL EQU   *-SVESTWK                                                        
*                                                                               
I_OLD    DS    0X                                                               
         DS    (EL_LENQ)X                                                       
I_OLDL   EQU   *-I_OLD                                                          
*                                                                               
I_NEW    DS    0X                                                               
         DS    (EL_LENQ)X                                                       
I_NEWL   EQU   *-I_NEW                                                          
*                                                                               
I_AUD    DS    0X                                                               
         DS    (EL_LENQ)X                                                       
I_AUDL   EQU   *-I_AUD                                                          
*                                                                               
SAVEL    EQU   *-SAVED                                                          
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
         EJECT                                                                  
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
*&&UK                                                                           
       ++INCLUDE FAJESMAILD                                                     
*&&                                                                             
*&&US                                                                           
       ++INCLUDE ACCATCALLD                                                     
*&&                                                                             
         PRINT ON                                                               
***********************************************************************         
* Audit element table DSECT - used to build status change element     *         
* from looking at the old and new elements that make up the order     *         
***********************************************************************         
                                                                                
AUDTABD  DSECT                                                                  
AUDELCDE DS    XL1                 Element code                                 
AUDELDTY DS    XL1                 Displacement to element type                 
AUDELTYP DS    XL1                 Element type equate                          
AUDDSTCI DS    XL1                 Displacement to indicator on STCELD          
AUDSTCIN DS    XL1                 Indicator value                              
AUDDSSTC DS    XL1                 Displacement in STCELD                       
AUDSTCLN DS    XL1                 Length of STCELD data                        
AUDDSELM DS    XL1                 Displacement in element                      
AUDORDTY DS    0XL4                Order types applicable                       
AUDORDT1 DS    CL1                                                              
AUDORDT2 DS    CL1                                                              
AUDORDT3 DS    CL1                                                              
AUDORDT4 DS    CL1                                                              
AUDDTIND DS    XL1                 Date indicator                               
AUDDTVAR EQU   X'80'               Data is variable length                      
AUDMULT  EQU   X'40'               Multiple elements for this data              
AUDOCHRS EQU   X'20'               Or characters with spaces                    
AUDSUBEL EQU   X'10'               Element has sub elements                     
AUD1ERR  EQU   X'08'               One error is enough                          
AUDNSTON EQU   X'04'               Not to be read on status only change         
AUDPREV  EQU   X'02'               Check for previous data                      
AUDSUBLN DS    XL1                 Sub element length                           
AUDTABL  EQU   *-AUDTABD           Audit table length                           
                                                                                
***********************************************************************         
* Resubmit table DSECT - used to work out whether approved, part      *         
* approved or matched orders need to be resubmitted                   *         
***********************************************************************         
                                                                                
RESBTABD DSECT                                                                  
ROWKFLST DS    XL1                 Old workflow status                          
RMATSTAT DS    XL1                 Matching status                              
ROPTMAIT DS    XL2                 Displacement to opt maint setting            
ROPTVAL  DS    XL1                 Option maintain value                        
ROMCAO   DS    XL1                 Change approved order                        
RESBTABL EQU   *-RESBTABD          Length of table entry                        
                                                                                
***********************************************************************         
* New status table DSECT - used to work out new workflow status       *         
* and order status on updated record                                  *         
***********************************************************************         
                                                                                
NSTTABD  DSECT                                                                  
NNWKFLST DS    XL1                 New workflow status                          
NOWKFLST DS    XL1                 Old workflow status                          
NREQSTAT DS    XL1                 Requent status                               
NREQACTN DS    XL1                 Request action                               
NAPPROVL DS    XL1                 Approval indicator                           
NBEHAVOR DS    XL1                 Behaviour to follow                          
NORDST1  DS    XL1                 Order record status byte 1                   
NORDST2  DS    XL1                 Order record status byte 2                   
NOUTSTAT DS    XL1                 Output status for return                     
NSTTABDL EQU   *-NSTTABD           Length of table entry                        
***********************************************************************         
* Audit element buffer DSECT                                          *         
***********************************************************************         
                                                                                
EL_RECD  DSECT ,                                                                
EL_REC   DS    0X                                                               
EL_KEY   DS    0X                                                               
EL_ELCDE DS    XL1                 Element code                                 
EL_ELWRK DS    XL7                 Work area for sorting                        
EL_KSEQ  DS    XL2                 Unique serial number                         
EL_KEYL1 EQU   *-EL_KEY            Key length for compare                       
EL_ELSTA DS    XL1                 Status                                       
EL_EXSTS EQU   X'80'               Exists on the other buffer                   
EL_KEYL  EQU   *-EL_KEY                                                         
                                                                                
EL_ELEM  DS    XL255               STCELQ element                               
EL_LENQ  EQU   *-EL_REC                                                         
                                                                                
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 Record map number                            
RECTTYPE DS    AL1                 ** Record type **                            
RECTTOUP EQU   1                   New order upload                             
RECTABL  EQU   *-RECTABD                                                        
*                                                                               
*  dsect for estimate values-by-workcode table                                  
ESTCHKD  DSECT                                                                  
ESTCHWC  DS    CL2              WORKCODE                                        
ESTCHOE  DS    PL6                                                              
ESTCHCE  DS    PL6              SEE XTOTCE                                      
ESTCHHR  DS    PL6                                                              
ESTCHAM  DS    PL6              see XTOTAM                                      
ESTCHOR  DS    PL6              soo XTOTOR                                      
ESTRAMT  DS    PL6              Remaining amount                                
ESTCHKE  DS    XL1              estimate check error message                    
ESTNCESQ EQU   1                No current estimate                             
ESTNAPEQ EQU   2                No approved estimate                            
ESTREJBQ EQU   3                Work code exceeds remaining job amount          
ESTREESQ EQU   4                Work code exceeds remaining est amount          
ESTCHKW  DS    XL1              estimate check warning message                  
ESTCHLQ  EQU   *-ESTCHKD                                                        
                                                                                
EAWTABD  DSECT                                                                  
EAWTGOV  DS    CL1                                                              
EAWTERR  DS    XL2                                                              
EAWTLNQ  EQU   *-EAWTABD                                                        
                                                                                
EAETABD  DSECT                                                                  
EAETGOV  DS    CL1                 ECE SETTING                                  
EAETERR  DS    XL1                 ECE ERROR                                    
EAETLNQ  EQU   *-EAETABD                                                        
                                                                                
EWCTABD  DSECT                                                                  
EWCTGOV  DS    CL1                 ECE SETTING                                  
EWCTERR  DS    XL1                 ECE ERROR                                    
EWCTLNQ  EQU   *-EWCTABD                                                        
                                                                                
XPIDSD   DSECT                                                                  
XPIDS1   DS    XL1                                                              
PIDPROQ  EQU   X'01'               (level processed)                            
XPIDC1   DS    XL2                                                              
XPIDS2   DS    XL1                                                              
XPIDC2   DS    XL2                                                              
XPIDLQ   EQU   *-XPIDSD                                                         
XPMAXQ   EQU   4                                                                
                                                                                
XWDSCT   DSECT                                                                  
XWCODE   DS    CL2                 Work code                                    
XWCAMT   DS    PL6                 Order agency amount                          
XWCFCA   DS    PL6                 Order currency amount                        
XWCINUM  DS    PL6                 Number of invoices                           
XWCIVAL  DS    PL6                 Invoiced agency amount                       
XWCIFCA  DS    PL6                 Invoiced currency amount                     
XWCIPND  DS    XL1                 Pending invoice log records                  
XWCSTAT  DS    XL1                 Status for estimates                         
XWCNCOM  EQU   X'80'               (US) w/c is commissionable                   
XWCMAJ   EQU   X'40'               (US) w/c is major                            
XWCEXWC  EQU   X'20'               (UK) exclude this w/c from est check         
XWCXTRA  EQU   X'04'               (US) extra work code to order                
XWCLNQ   EQU   *-XWCODE                                                         
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'124ACBRA13   12/02/20'                                      
         END                                                                    
