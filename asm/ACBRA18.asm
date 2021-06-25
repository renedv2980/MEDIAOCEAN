*          DATA SET ACBRA18    AT LEVEL 098 AS OF 12/15/20                      
*PHASE T62418A                                                                  
                                                                                
ACBRA18  TITLE '- BRA Estimates upload server'                                  
                                                                                
* Level change comments                                                         
* ---------------------                                                         
* UK Levels                                                                     
* ---------                                                                     
* TKLU 001 09MAY06 DDLink upload server for Estimates                           
* TKLU 002 24MAY06 Bug fix on submit action (IO area) and to PROPID             
* TKLU     26MAY06 Allow approver update on SUBMIT, too                         
* TKLU 003 30MAY06 EMDSCA needs to be saved to PIDELD, too                      
* TKLU     31MAY06 Items - flag for price override                              
* TKLU     31MAY06 Return current Audit entry on upload                         
* TKLU 004 14JUN06 Ballpark adjustments                                         
* TKLU     16JUN06 Error missing from LOOKUP routine                            
* TKLU     16JUN06 Save supposed approver in PROPID routine, too                
* TKLU     19JUN06 Date in upload return - set from char to pdat                
* TKLU 005 22JUN06 Duplicate PIDEL entries - bug fix (OVN summary)              
* TKLU 006 03JUL06 One more PIDEL bug fix (OVN summary)                         
* TKLU 007 25JUL06 <DU01-5577> Draft account status on non draft est's          
* TKLU 008 26JUL06 PROPID loop - bug fix, also use new AE$EABOU                 
* TKLU 009 11AUG06 Client approver name/date in STCELD                          
* TKLU 010 23AUG06 <UKCR00007708> - Embedded call parameter change              
* TKLU 011 12OCT06 Implement category text box upload                           
*          12OCT06 New Estimate row copy upload (logical download)              
* TKLU 012 23OCT06 FINAL RENAME FROM MCS TO BRA                                 
* TKLU 013 31OCT06 Recognise any error before update estimate                   
*          02NOV06 Tidy up logical versus physical deletion                     
* TKLU 014 09NOV06 <UKCR00009761> allow for itemless items                      
*                  <UKCR00009933> Logical delete status table change            
* TKLU 015 22NOV06 Additions to estimate row copy upload                        
* TKLU 016 04DEC06 Row copy upload bug fix and additions                        
*                  Ensure 'EX   Rn,*+4' is not used anymore (assembler)         
* TKLU 015 08JAN07 <UKCR00010566> RQRCISN - check for zeroes                    
* TKLU 016 29JAN07 <DU01-5937> Suppress printing flags                          
* TKLU 017 16FEB07 <UKCR00011350> Disallow Local Est. Number to be FF           
* TKLU 018 01MAR07 <UKCR00010828> Job lock/close error message fix              
* TKLU 019 08MAR07 <UKCR00010879> VALITM error message improvement              
* TKLU 020 15MAR07 <UKCR00011691> Prevent from appr if not approver             
* TKLU 021 20MAR07 Estimate approver check improvement                          
* TKLU 022 23MAR07 Additions to GETITM call                                     
* TKLU 023 28MAR07 VSTR macvro needs MAXLEN=L'RQ... protection                  
* TKLU 024 23APR07 <UKCR00012125> Error message and handling changed            
*                  plus US merger                                               
* TKLU 025 10MAY07 Work code error return bug fix                               
* TKLU 026 23MAY07 <DU01-6486> Preparations for Estimate/Merge                  
* TKLU 027 11JUN07 US merger (JSHA)                                             
* TKLU 028 20JUL07 <DU01-6665> set ERDIITQ for time items                       
* TKLU 029 23JUL07 <UKCR00013309> Estimate category foreign names               
* TKLU 030 08OCT07 US merger (for JSHA)                                         
* TKLU 031 19OCT07 Use EMSELD/EMSICQ to save 'copy from' estimate #             
* NSHE 032 13NOV07 <LO01-6995> Set FACPAK code for analysis                     
* TKLU 033 07NOV07 <UKCR00012444> Client name/date on reject BUG FIX +          
*          08NOV07 Allow status change comment (auto generated) to be           
*                  120 characters                                               
* TKLU 034 11DEC07 US merger (JSHA)                                             
* TKLU 035 18DEC07 <UKCR00015118> Estimate percentage & w/c behaviour           
* TKLU 036 15JAN08 <BR15521L> XDFELD is not main data element                   
*          19JAN08 <DU01-5718> Contingency redo (w/c level with DVIL)           
* TKLU 037 08MAR08 <UKCR00016524> GETOPT call init bug fix                      
* TKLU 038 11AUG08 <NY02-0005> Estimate name change to audit                    
* NSHE 039 12AUG08 change to passives used                                      
* YNGX 040 17SEP08 <LO01-815> Add wc seq# which will be used by =CBill          
* NSHE 041 08OCT08 <BR20591L> Change estimate approver look up                  
* TKLU 042 16OCT08 <LO01-8207> 'Internal Approvals' implementation              
* TKLU     03NOV08 <DU01-8304> Expanded job locks implemented                   
* TKLU     07JAN09 <LO01-8495> Estimate name to be optional field               
* NSHE 043 11MAY09 Change to approver record                                    
* YNGX 044 17FBE09 <LO01-2599> BILL FROM ESTIMATE (VSN>=1.13+)                  
* SMAN 044 18JUN09 <LO01-8421> Internal Approvals in Estimates                  
* SMAN     18JUN09 <LO01-8422> Self approvals in Estimates                      
* SMAN     18JUN09 <LO01-8642> Merge estimates - remove duplicate rows          
* MPEN     20MAR09 <LO01-8463> CONTROL NOTIFICATIONS ON SUBMIT BY OFF           
* SMAN     16JUL09 <LO01-8950> Override Item text on WC row                     
* TKLU 045 23AUG09 <DU01-8003> Implement XDFEDXQ                                
* SMAN 046 21SEP09 BR27483L Fix to CHKAPP validation of global appvrs           
* SMAN 047 09OCT09 <UKCR00024810> X#SAPPR can be blank if int apr only          
* YNGX 048 29OCT09 <LO01-2599> Bug fix for estimate billing                     
* SMAN     04NOV09 <UKCR00025404> Fix to passives for Merge estimates           
*          04NOV09 Fixes to audits for Internal Apprs and Merge Ests            
* SMAN 049 10NOV09 <UKCR00025431> Fix to email notifications                    
* YNGX     10NOV09 <LO01-2599> Allow copy if there's billing activity           
* SMAN 050 16NOV09 <UKCR00025671> Don't mark cli aprvd ests as merged           
* SMAN     16NOV09 <UKCR00024698> Send emails on approve and reject etc         
* SMAN 051 18NOV09 <UKCR00025873> Use M instead of G for merged ests            
* SMAN 052 26NOV09 <UKCR00025947> Handle X#SC2NVL correctly                     
* SMAN 053 03DEC09 <UKCR00025550> Add 'Merged to estimate' in audit             
* SMAN     09DEC09 <UKCR00025832> Error msg for job locked from ests            
* SMAN     10DEC09 <UKCR00026168> Send whole STCESCO in upload rtn              
* SMAN     11DEC09 <UKCR00026184> Save approvers from base estimate             
* SMAN     12DEC09 <UKCR00026343> Bring back comm rate for ballparks            
* SMAN     17FEB10 <UKCR00026712> Send email when est. is cli apprvd            
* NSHE 055 15MAR10 Allow approval to look up client without office              
* SMAN 056 13MAY10 <BR33169L> Small fix to CHKAPP                               
* SMAN 057 14JUN10 <UKCR00028124> Softcode APRTAB                               
* NSHE 058 21JUL10 Merge US Fix                                                 
* NSHE 059 05AUG10 Remove CONPIDC and replace with CCTPID                       
* SMAN     07OCT10 <UKCR00029148> Find next available estimate number           
* MPEN 060 11MAR11 <BR17786D> 2 char office status check fix                    
* MPEN 061 12MAR11 <BR41141L> Additional fix for the above                      
* MPEN 062 16FEB11 <PR001523> Store actuals w/c calc info                       
*                  <PR001545> Change sort order of xdfelds                      
* MPEN 063 09AUG11 <PR002045> Improvements to foreign language ests             
* SMAN     06SEP11 <UKCR00033245> use ESTRSTA2, not ESTKSTA2                    
* SMAN     20SEP11 <UKCR00033357> Turn off ESTKSINA when merging ests           
* SMAN     21SEP11 Make billing activity checks UK only                         
* JFOS 065 16SEP11 <PR002047> Hourly rates                                      
* JFOS             <PR001992> Handle 24*250chars text array                     
* JFOS 066 18JUN12 <UJCR00034342> Save 'Print rates' setting on ests            
* YNGX 067 04JUL12 <UKCR00033779> Change error message for merged Est.          
* NSHE 068 03SEP12 Change to IO routine for auto switching system               
* NSHE     23JAN13 Chunk up elements so the time information follows WC         
* JFOS 069 30MAY13 <DSBO-30> Handle >2Kb text on a W/C                          
* YNGX 070 10JUN13 <BR21372D> FIX ALL W/C DESCRIPTIONS MOVE TO LAST W/C         
* MPEN 071 17JUL14 <DSRD3303> Changes to move audit to new record               
*                  <DSRD2733> Estimate audit improvements                       
* MPEN     01Dec14 <DSBO1266> Fix internal approver lookup                      
* MPEN     02Dec14 <DSBO1267> Fix dump when approving and name < 2 cha          
* TKLU     17Sep14 <RD004333> EMail notification 4 subm2cli 2 reject            
*                  <RD004393> Delete Audit Records on Estimate Delete           
*                  <RD004388> QA special dates not for Aura                     
*                  <RD004406> Error if adding 'no details' estimates            
*          23Sep14 <RD004461> An approve check for any locked w/cs              
*          25Sep14 <RD004505> Allow status change on maintenance call           
*          01Dec14 <DSBO1266> Fix internal approver lookup                      
*          02Dec14 <DSBO1267> Fix dump when approving and name < 2 char         
*          03Dec14 <DSRD5368> Fix dump when approving/rejecting                 
* NRAK 072 12Dec14 <DSRD5427> Fix status checking                               
* NSHE 073 08Jan15 <DSRD5424> Fix audit for printing in Aura                    
* MPEN 074 31Mar15 <SUP04900> Fix for BLDAPP routine and copy US chges          
* MPEN 075 11Mar15 <DSBO1168> Copy elements added by FlexiBill                  
* TKLU 076 06May15 <RD006455> US only support for HR/OE/CE on Job               
* NSHE 077 16Jun15 <DSRD-7555> Concurrent error messages fix for Aura           
* NRAK 078 09JuL15 <DSBO-1517> Rebuilt client approval data on maint.           
* NSHE 079 03Jul15 <DSSUP-5496> US fix to check job level                       
* MPEN     22Jul15 <DSRD-7979> Save off w/c and cat for auditing delets         
* NSHE 080 21Aug15 <DSRD-8352> Allow additional text to be saved                
* NSHE 081 09Oct15 <DSRD-9043> Fix time wc status on estimate rec               
* NSHE 082 22Oct15 <DSRD-9161> Don't remove internally approved commnts         
* NSHE 083 10Dec15 <DSRD-9740> Fix audit process to stop dump                   
* TKLU 084 27Jan15 <RD010160>  Use AGOBLOCB not AGOBLOCK                        
* TKLU 085 14Feb17 <RD007510>  Support alphanumeric estimate numbers            
*NRAK 086 22Feb17 <DSRD-14994> status STCEL on maint+status                     
*NRAK 087 22Feb17 (restore code lost on previous commit)                        
*NSHE 088 31May17 DSRD-14935 Allow delete status for Aura with maintain         
*NSHE     05Jun17 DSRD-14949 Fix email response when approved after rej         
*MPEN     19Jun17 DSRD-15985 Return record changed elsewhere                    
*NSHE 089 30Jan18 DSRD-18005 Set electronic signature status, dates etc         
*MPEN     21Feb18 DSRD-18308 Fix for client approved date                       
*NSHE 090 23Mar18 DSRD-18624 Cancel electronic if estimate rejected             
*TKLU 091 03May18 DSRD-19029 XOAPID/XFREMS/XELSIG error return fix              
*NSHE 092 27Sep18 DSRD-20165 Make WC sequence both UK and US                    
*SGAV 093 08MAR19 DSRD-21926 Allow Estimate nos to be in HEX format             
*MPEN 094 08MAR19 DSRD-21956 Fix for approve rejected estimate                  
*MPEN 095 18JUL19 DSRD-23156 Fix for electronic signature                       
*MPEN 096 13Jan20 DSRD-25133 Preserve foreign language setting                  
*MPEN 097 17Feb20 DSRD-25417 Fix for rejecting client app estimate              
*SGAV     17Feb20 DSPCA-3080 Extend RSTLN                                       
*MPEN 098 12Mar20 DSRD-25729 Store internal/external cost breakdown             
*ABID 099 04JUL20 DSRD-26178 AMEND THE ESTIMATE UPLOAD TO MARK IT AS            
*                            A CURRENT ESTIMATE AND REMOVE CE FROM OLD          
*MPEN 099 27JUL20 DSRD-25991 FIX FOR REJECT NON ELEC EST                        
*MPEN 100 10JUL20 DSRD-25261 FIX FOR ELECTRONIC SIGNATURE RESUBMIT              
*MPEN 101 15Dec20 ITMF-52095 Check for deleted estimates                        
*                                                                               
                                                                                
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=2000,REQUEST=*,WORKERKEY=ACBO,   *        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=ACCSYSQ                   
                                                                                
ENTRY    NMOD1 0,**BO18**,RR=RE,R7                                              
         LR    RC,R1                                                            
         USING LP_D,RC                                                          
         L     RF,LP_ARUNP                                                      
         USING RUNPARMD,RF         R7=A(RUNPARMS)                               
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         BASR  RA,0                                                             
         AHI   RA,GLOBALS-*                                                     
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
                                                                                
INITUL   CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         JE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JE    INPUT                                                            
         CLI   RUNPMODE,RRUNENDQ   TEST 'LAST TIME' MODE                        
         JNE   EXITY                                                            
         GOTOR GOIO                inactive - for pending action                
         J     EXITY                                                            
                                                                                
INIT     LA    R0,SAVED            CLEAR SAVE STORAGE                           
         LHI   R1,SAVEL                                                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   GIND2,GI2EEST                                                    
         GOTOR (#CPYINI,ACPYINI)   INITIALISE COMPANY VALUES                    
                                                                                
         L     R0,AIO4                                                          
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR I/O AREA 4 (RECORD ID TABLE)           
                                                                                
         LA    R0,LP_D                                                          
         ST    R0,ALP              SAVE A(LP_D) IN GLOBAL STORAGE               
                                                                                
         MVC   AALIOB,LP_ALIOB     EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   AALINKIO,CLINKIO-COMFACSD(RF)                                    
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Process and Upload Record                                           *         
***********************************************************************         
                                                                                
INPUT    BASR  RF,0                                                             
         AHI   RF,RECTAB-*                                                      
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
*        AHI   R0,1                                                             
                                                                                
         GOTOR UPDREC              PROCESS THE INPUT RECORD                     
                                                                                
         J     EXITY               EXIT BACK TO DDLINK                          
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* Estimate upload return data                                         *         
***********************************************************************         
         USING LIOBD,R4                                                         
ESTUPR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ESTUPR*'                                                      
                                                                                
         L     R4,AALIOB                                                        
                                                                                
         XR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN                                                    
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
                                                                                
         DS    0H                  return global estimate number                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#GNO),   *        
               ('LD_CHARQ',ES#ESTN),(L'ES#ESTN,0)                               
                                                                                
         DS    0H                  return local estimate number                 
         EDIT  (B1,X#LOCNO),(3,FULL2),0,ALIGN=LEFT                              
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#LNO),   *        
               ('LD_CHARQ',FULL2),(L'FULL2,0)                                   
                                                                                
         XOUT  X#NEWIDN,FULL2,2    return new ID number                         
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#IDN),   *        
               ('LD_CHARQ',FULL2),(L'FULL2,0)                                   
                                                                                
         MVI   BYTE1,0                                                          
         LA    R3,X#STCEL1                                                      
         OC    X#STCEL1,X#STCEL1     return audit entry?                        
         JZ    ESTUPR40                                                         
                                                                                
         USING STCELD,R3                                                        
ESTUPR10 CLI   STCDFR,FF                                                        
         JNE   *+8                                                              
         MVI   STCDFR,STCEADDE                                                  
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#ASF),   *        
               ('LD_CHARQ',X#OSCHAR),(L'X#OSCHAR,0)                             
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#AST),   *        
               ('LD_CHARQ',X#NSCHAR),(L'X#NSCHAR,0)                             
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#ADA),   *        
               ('LD_PDATQ',SVTODAYP),(L'SVTODAYP,0)                             
         UNPK  DUB2,CTIME                                                       
         OI    DUB2+7,C'0'                                                      
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#ATI),   *        
               ('LD_CHARQ',DUB2+2),(L'DUB2-2,0)                                 
         MVC   TEMP2(2),CUUSER                                                  
         GOTOR (#GETUSR,AGETUSR)                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#AUS),   *        
               ('LD_CHARQ',TEMP2),(10,0)                                        
         MVC   TEMP2(2),CCTPID                                                  
         J     ESTUPR30                                                         
                                                                                
ESTUPR20 GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#ASF),   *        
               ('LD_CHARQ',STCDFR),(L'STCDFR,0)                                 
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#AST),   *        
               ('LD_CHARQ',STCDTO),(L'STCDTO,0)                                 
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#ADA),   *        
               ('LD_PDATQ',STCDATE),(L'STCDATE,0)                               
         UNPK  DUB2,STCTIME                                                     
         OI    DUB2+7,C'0'                                                      
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#ATI),   *        
               ('LD_CHARQ',DUB2+2),(L'DUB2-2,0)                                 
         MVC   TEMP2(2),STCUSER                                                 
         GOTOR (#GETUSR,AGETUSR)                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#AUS),   *        
               ('LD_CHARQ',TEMP2),(10,0)                                        
         MVC   TEMP2(2),STCPERS                                                 
ESTUPR30 GOTOR (#GETPID,AGETPID)                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#APC),   *        
               ('LD_CHARQ',TEMP2),(8,0)                                         
         GOTOR (#GETPIN,AGETPIN)                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#AFN),   *        
               ('LD_CHARQ',TEMP2),(16,0)                                        
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#ALN),   *        
               ('LD_CHARQ',TEMP2+16),(16,0)                                     
                                                                                
         CLI   STCETY2,STCSTATE     Is it electronic signature stat?            
         JNE   ESTUPR35                                                         
         XR    RF,RF                                                            
         IC    RF,STCLN                                                         
         SHI   RF,STCECOMT-STCEL                                                
         LTR   RF,RF                                                            
         JNP   ESTUPR50                                                         
*                                                                               
         LA    R3,STCECOMT                                                      
         J     ESTUPR36                                                         
                                                                                
ESTUPR35 XR    RF,RF                                                            
         IC    RF,STCLN                                                         
         SHI   RF,STCLN1Q                                                       
         LTR   RF,RF                                                            
         JNP   ESTUPR50                                                         
*                                                                               
         LA    R3,STCESCO                                                       
*                                                                               
ESTUPR36 GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UE#ACC),   *        
               ('LD_CHARQ',0(R3)),((RF),0)                                      
                                                                                
ESTUPR40 CLI   BYTE1,1                                                          
         JE    ESTUPR50                                                         
         OC    X#STCEL2,X#STCEL2                                                
         JZ    ESTUPR50                                                         
         LA    R3,X#STCEL2                                                      
         MVI   BYTE1,1                                                          
         J     ESTUPR10                                                         
         DROP  R3                                                               
                                                                                
ESTUPR50 DS    0H                                                               
                                                                                
         J     EXIT                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* Go to Upload Record handling routine                                *         
***********************************************************************         
                                                                                
UPDREC   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDREC*'                                                      
                                                                                
         GOTOR (#SYSCHK,ASYSCHK)                                                
         JE    UPDREC2                                                          
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$FLRD                                                   
         J     EXITN                                                            
                                                                                
UPDREC2  GOTOR VDATCON,DMCB,(5,0),(1,SVTODAYP)                                  
                                                                                
UPDREC4  LLC   RF,RECTYPE                                                       
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         CHI   RF,UPDTABL                                                       
         JNL   *+2                                                              
         B     UPDTAB(RF)                                                       
                                                                                
UPDTAB   DS    0XL4                                                             
         J     UPDEST              Main upload for estimates                    
UPDTABL  EQU   *-UPDTAB                                                         
                                                                                
UPDRECY  J     EXITY                                                            
                                                                                
UPDRECN  J     EXITN                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Estimate Main Upload                                                *         
***********************************************************************         
                                                                                
UPDEST   TM    TWAMODE,TWAMEDP     exit on all errors set                       
         JNZ   UPDESTN                                                          
                                                                                
         DS    0H                  Data record received for:                    
                                                                                
         CLI   RQUPTYP,RQUPTGQ     - Global values                              
         JE    UPDE100                                                          
         CLI   RQUPTYP,RQUPTHQ     - Header text values                         
         JE    UPDE300                                                          
         CLI   RQUPTYP,RQUPTEQ     - Estimate additional text                   
         JE    UPDE302                                                          
         CLI   RQUPTYP,RQUPTFQ     - Footer text values                         
         JE    UPDE304                                                          
         CLI   RQUPTYP,RQUPTYQ     - W/C text values                            
         JE    UPDE310                                                          
         CLI   RQUPTYP,RQUPTZQ     - Item text values                           
         JE    UPDE315                                                          
         CLI   RQUPTYP,RQUPTVQ     - Category text values                       
         JE    UPDE320                                                          
         CLI   RQUPTYP,RQUPTWQ     - Work code values                           
         JE    UPDE400                                                          
         CLI   RQUPTYP,RQUPTIQ     - Item values                                
         JE    UPDE500                                                          
         CLI   RQUPTYP,RQUPTXQ     - Xtra data values                           
         JE    UPDE600                                                          
         CLI   RQUPTYP,RQUPTCQ     - Category values                            
         JE    UPDE700                                                          
         CLI   RQUPTYP,RQUPTSQ     - Source (merge/copy) values                 
         JE    UPDE750                                                          
         CLI   RQUPTYP,RQUPTUQ     - Update values                              
         JE    UPDE800                                                          
         DC    H'0'                - Unknown type                               
                                                                                
*** Global values (first time for estimate) ***************************         
                                                                                
UPDE100  MVC   X#ACTION,RQUGACT                                                 
         MVI   X#NODATA,YESQ                                                    
         MVI   X#MODE,0                                                         
         MVI   X#SJUPD,NOQ                                                      
         MVI   X#SVNAL,FF                                                       
         XC    NXWCSEQ,NXWCSEQ     CLEAR WC SEQUENCE #                          
                                                                                
         L     R1,AFREEST          init source buffer                           
         MVI   0(R1),0                                                          
                                                                                
         XC    X#ELSRT(X#ELSRL),X#ELSRT                                         
         GOTOR BUFELM,DMCB,('TSAINI',NEWBUF)                                    
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR BUFELM,DMCB,('TSAINI',NSRBUF)                                    
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR BUFELM,DMCB,('TSAINI',OLDBUF)                                    
         JE    *+6                 2nd buffer for old estimate rec              
         DC    H'0'                to build aura audit                          
                                                                                
         XC    X#STCEL1,X#STCEL1                                                
         XC    X#STCEL2,X#STCEL2                                                
         XC    X#SAPPR,X#SAPPR                                                  
         XC    X#SIAPP,X#SIAPP                                                  
         XC    X#ERAIS,X#ERAIS                                                  
         XC    X#ECLAP,X#ECLAP                                                  
                                                                                
         OC    CCTPID,CCTPID                                                    
         JNZ   UPDE101                                                          
                                                                                
         OI    TWAMODE,TWAMEDP                                                  
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$NCPID                                                  
         J     UPDESTN                                                          
                                                                                
UPDE101  MVI   X#STATUS,X#NOSTAQ   no status change involved                    
         CLI   X#ACTION,RQUGACQ    create (from merge) or maintenance           
         JE    UPDE102             call?                                        
         CLI   X#ACTION,RQUGAMQ                                                 
         JE    UPDE102                                                          
         MVC   X#STATUS,X#ACTION   move action to status                        
         MVI   X#ACTION,X#ACTISQ   action is a status only change               
                                                                                
UPDE102  CLI   RQUGMASC,YESQ       skip if not set                              
         JNE   UPDE104                                                          
         CLI   X#STATUS,X#NOSTAQ   (bad data received via API)                  
         JE    *+2                                                              
         MVI   X#ACTION,RQUGAMQ    set to maintenance action                    
         J     UPDE115                                                          
                                                                                
UPDE104  CLI   X#ACTION,RQUGACQ    Create action?                               
         JNE   UPDE115             no - must be status or maintenance           
                                                                                
         MVI   X#SJUPD,YESQ                                                     
         MVI   X#SCNVAL,ESTKCREA   set status                                   
                                                                                
         GOTOR VEKADD              validate estimate key values for add         
         JE    UPDE160                                                          
                                                                                
         OI    TWAMODE,TWAMEDP                                                  
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
* existing estimate - maintenance or status only change                         
                                                                                
UPDE115  GOTOR VALCPJ                                                           
         JE    UPDE116                                                          
         OI    TWAMODE,TWAMEDP                                                  
         ICM   R1,3,ROUERRV                                                     
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE116  MVC   ES#ESTN,RQUGGNO     read for estimate record                     
                                                                                
         USING EGNPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,CUXCPY                                                   
         MVC   EGNPNUM,ES#ESTN                                                  
                                                                                
         MVC   CSVKEY1,EGNPAS                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   CSVKEY1(EGNPCLI-EGNPASD),EGNPAS                                  
         JE    UPDE117                                                          
                                                                                
         MVC   ROUERRV,=AL2(AE$ESTNF)                                           
         J     EXITN                                                            
                                                                                
UPDE117  MVC   X#SC2OVL,EGNPSTA2   save status byte 2                           
         MVC   X#SC3OVL,EGNPSTA3   save status byte 3                           
         MVC   CSVKEY1,EGNPAS      save estimate passive                        
         MVC   X#LOCNO,EGNPLNO     save local number                            
                                                                                
         CLC   EGNPSOFF,SPACES     office set?                                  
         JNH   UPDE118                                                          
                                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,EGNPSOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         DROP  R1                                                               
         GOTO1 VOFFAL              validate office                              
         JE    UPDE118                                                          
                                                                                
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
                                                                                
UPDE118  CLI   X#STATUS,RQUGAAQ    If approve ensure user is approver           
         JE    *+12                                                             
         CLI   X#STATUS,RQUGAIQ    If int appr ensure user is int appr          
         JNE   UPDE120                                                          
         GOTOR CHKAPP,X#STATUS                                                  
         JE    UPDE120                                                          
                                                                                
         OI    TWAMODE,TWAMEDP                                                  
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE120  CLI   X#ACTION,RQUGAMQ    Maintenance action?                          
         JNE   UPDE230             No - must be status only change              
                                                                                
         MVC   EGNPAS,CSVKEY1                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING ESTRECD,R2                                                       
         L     R2,AIO2             check for client approver set                
         MVC   CSVKEY2,ESTKEY      save estimate main key                       
         USING EMDELD,R3                                                        
         LA    R3,ESTRFST-ESTRECD(R2)                                           
         CLI   EMDEL,EMDELQ                                                     
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   X#ERAIS,EMDAPI                                                   
         MVC   X#ECLAP,EMDSCA                                                   
         MVC   X#EINAP,EMDSIA                                                   
         MVC   X#FLANG,EMDLAN      Save foreign language                        
         MVC   X#SCNVAL,ESTRSTA1   save status byte 1                           
         MVC   X#SC2NVL,ESTRSTA2   save status byte 2                           
         MVC   X#SC3NVL,ESTRSTA3   save status byte 3                           
         NI    X#SC3NVL,FF-(ESTSWCTM+ESTSITTM+ESTSITMS)                         
*                                                                               
UPDE125  OC    RQUGCLI(L'RQUGCLI+L'RQUGPRO+L'RQUGJOB),SPACES                    
         CLC   RQUGCLI,ESTKCLI     C/P/J must match                             
         JNE   UPDE135                                                          
         CLC   RQUGPRO,ESTKPRO                                                  
         JNE   UPDE135                                                          
         CLC   RQUGJOB(6),ESTKJOB                                               
         JE    UPDE135A                                                         
UPDE135  OI    TWAMODE,TWAMEDP                                                  
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$00445                                                  
         J     UPDESTN                                                          
*                                                                               
UPDE135A MVC   IOKEY,CSVKEY2       Read estimate main record for read           
         L     R0,AIO4             copy record to AIO4 for restoring            
         LA    R1,IOLENQ           later                                        
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         J     UPDE140             Then buffer old elements                     
*                                                                               
UPDE136  LA    R2,IOKEY                                                         
         LLC   RF,ESTKSEQ                                                       
         AHI   RF,1                                                             
         STC   RF,ESTKSEQ                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    UPDE138                                                          
*                                                                               
         L     R0,AIO2             Restore main record AIO2                     
         LA    R1,IOLENQ                                                        
         L     RE,AIO4                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     UPDE149                                                          
*                                                                               
UPDE138  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    UPDE140                                                          
         DC    H'0'                                                             
*                                                                               
UPDE140  L     R2,AIO2                                                          
         LA    R3,ESTRFST                                                       
*                                                                               
UPDE142  CLI   EMDEL,0                                                          
         JE    UPDE136                                                          
                                                                                
         GOTOR PUTSAR,OLDBUF       Add to old buffer                            
         LLC   R0,EMDLN                                                         
         AR    R3,R0                                                            
         J     UPDE142                                                          
*                                                                               
UPDE149  LA    R2,IOKEY            read main IS key for lock                    
         MVC   ESTKEY,CSVKEY2                                                   
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
*        add/maintain code here                                                 
                                                                                
         USING EMDELD,R3                                                        
UPDE160  LA    R3,ELEMENT          validate main data now                       
         XC    ELEMENT,ELEMENT                                                  
         XC    X#ELSRT(X#ELSRL),X#ELSRT                                         
                                                                                
         MVI   EMDEL,EMDELQ                                                     
         MVI   EMDLN,EMDL2Q                                                     
                                                                                
         CLC   RQUGDAT,SPACES                                                   
         JH    UPDE165                                                          
                                                                                
         OI    TWAMODE,TWAMEDP                                                  
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$MISDA                                                  
         J     UPDESTN                                                          
                                                                                
UPDE165  GOTOR VDATCON,DMCB,(0,RQUGDAT+2),(1,EMDDAT)                            
                                                                                
         CLC   RQUGCUR,SPACES                                                   
         JH    *+10                                                             
         MVC   RQUGCUR,AGYCURR                                                  
                                                                                
         CLC   RQUGCUR,AGYCURR                                                  
         JNE   UPDE170                                                          
                                                                                
         MVC   EMDCUR,RQUGCUR                                                   
         MVI   EMDNDP,2                                                         
         XC    EMDRAT,EMDRAT                                                    
         J     UPDE185                                                          
                                                                                
UPDE170  XC    X#CURTAB,X#CURTAB                                                
         GOTO1 VBLDCUR,DMCB,RQUGCUR,X#CURTAB,ACOMFACS                           
         CLI   0(R1),0                                                          
         JE    UPDE180                                                          
                                                                                
UPDE175  OI    TWAMODE,TWAMEDP                                                  
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$INCUR                                                  
         J     UPDESTN                                                          
                                                                                
         USING GCREL,RF                                                         
UPDE180  L     RF,0(R1)            A(currency record)                           
         LA    RF,GCFIRST(RF)                                                   
         CLI   GCREL,GCRELQ                                                     
         JNE   UPDE175                                                          
         MVC   EMDNDP,GCRDECP      save no. of decimal places                   
         DROP  RF                                                               
                                                                                
         MVC   EMDCUR,RQUGCUR      rate not validated                           
         GOTO1 VHEXIN,DMCB,RQUGEXC,EMDRAT,14                                    
                                                                                
UPDE185  GOTOR VALAPP                                                           
         JNE   UPDE195                                                          
                                                                                
         MVC   X#BPMED,RQUGMED                                                  
         GOTOR VALBPM                                                           
         JNE   UPDE195                                                          
                                                                                
         MVC   X#SCHCD,RQUGSCH                                                  
         GOTOR VALSCH                                                           
         JE    UPDE200                                                          
                                                                                
UPDE195  OI    TWAMODE,TWAMEDP                                                  
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
*                                  Not allowed to change scheme/lan if          
UPDE200  MVC   EMDSCH,X#SCHCD      not create                                   
         CLI   RQUGACT,RQUGACQ     Only for create estimate                     
         JE    UPDE20A             not allowed to change                        
         MVC   EMDLAN,X#FLANG      preserve existing setting                    
         J     *+10                                                             
*                                                                               
UPDE20A  MVC   EMDLAN,RQUGLAN                                                   
         MVC   EMDGNO,SPACES                                                    
         MVC   EMDBMC,X#BPMED                                                   
         ZAP   EMDAMT,PZERO                                                     
         ZAP   EMDFCA,PZERO                                                     
                                                                                
         MVI   EMDSTA,0                                                         
         CLI   RQUGSEP,YESQ        printing flags                               
         JNE   *+8                                                              
         OI    EMDSTA,EMDSPQ       Don't print lines where zero amount          
         CLI   RQUGSDP,YESQ              and no description                     
         JNE   *+8                                                              
         OI    EMDSTA,EMDSDQ       Don't print lines where zero amount          
         CLI   RQUPRAT,YESQ        Test 'print rates' flag                      
         JE    *+8                                                              
         OI    EMDSTA,EMDSRQ       Suppress rates                               
                                                                                
         MVC   EMDFMT,RQUGFMT      Format code                                  
         OC    EMDFMT,SPACES                                                    
                                                                                
         MVC   EMDGSTAT,RQGAPST    Electronic signature status                  
                                                                                
         MVC   TEMP2(16),RQUGTVR                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   EMDTVR,TEMP2+16(8)                                               
                                                                                
         MVC   EMDIUS,SPACES                                                    
         CLC   RQUINUS,SPACES                                                   
         JNH   UPDE201                                                          
         MVC   EMDIUS,RQUINUS                                                   
                                                                                
UPDE201  MVC   EMDLDT,SVTODAYP                                                  
         MVO   TEMP(L'CTIME),CTIME                                              
         MVC   EMDLTI,TEMP                                                      
         MVC   EMDLPI,CCTPID                                                    
                                                                                
         ZAP   X#TOTAL,PZERO                                                    
         ZAP   X#TOTFC,PZERO                                                    
         ZAP   X#TOTCA,PZERO                                                    
         ZAP   X#TOTCF,PZERO                                                    
         ZAP   X#TOTVA,PZERO                                                    
         ZAP   X#TOTVF,PZERO                                                    
         ZAP   X#TOTIW,PZERO                                                    
         ZAP   X#TOTXW,PZERO                                                    
         ZAP   X#TOHRS,PZERO     INITALIZE TOTAL ESTIMATE HRS                   
*                                                                               
         CLI   X#ACTION,RQUGACQ    set values for create                        
         JNE   UPDE205                                                          
                                                                                
UPDE203  XC    EMDIDN,EMDIDN                                                    
         MVC   EMDADT,SVTODAYP                                                  
         MVO   TEMP(L'CTIME),CTIME                                              
         MVC   EMDATI,TEMP                                                      
         MVC   EMDAPI,CCTPID                                                    
                                                                                
         J     UPDE210                                                          
                                                                                
         USING ESTRECD,R2                                                       
OLD      USING EMDELD,R4                                                        
UPDE205  L     R2,AIO2             transfer from existing record                
         MVC   X#SCOVAL,ESTRSTA1   set old status value                         
         MVC   X#SC2OVL,ESTRSTA2   old status value 2                           
         MVC   X#SC3OVL,ESTRSTA3   old status value 3                           
         LA    R4,ESTRFST                                                       
                                                                                
         MVC   EMDIDN,OLD.EMDIDN                                                
         MVC   EMDADT,OLD.EMDADT                                                
         MVC   EMDATI,OLD.EMDATI                                                
         MVC   EMDAPI,OLD.EMDAPI                                                
         MVC   EMDSCA,OLD.EMDSCA                                                
         MVC   EMDSIA,OLD.EMDSIA                                                
                                                                                
         OC    X#SAPPR,X#SAPPR     if passed this time use it                   
         JZ    *+10                                                             
         MVC   EMDSCA,X#SAPPR      EMDSCB not used anymore                      
                                                                                
         OC    X#SIAPP,X#SIAPP     if passed this time use it                   
         JZ    *+10                                                             
         MVC   EMDSIA,X#SIAPP                                                   
                                                                                
         MVC   X#SAPPR,EMDSCA                                                   
         MVC   X#SIAPP,EMDSIA                                                   
         DROP  OLD                                                              
         DROP  R2                                                               
                                                                                
UPDE210  DS    0H                  post EMD element to TSAR                     
         GOTOR PUTSAR,NEWBUF                                                    
         JNE   UPDE195                                                          
                                                                                
         CLC   RQUGDES,SPACES      validate name                                
         JNH   UPDE212                                                          
         GOTOR BLDNAME             build name element                           
                                                                                
         GOTOR PUTSAR,NEWBUF                                                    
         JNE   UPDE195                                                          
*                                  save status change comment etc.              
UPDE212  CLI   X#STATUS,X#NOSTAQ   status change?                               
         JE    UPDE214             - create or maintain                         
         MVC   X#SCCOMM,RQUGCOM                                                 
         MVC   X#SCCCNA,RQUGCCN                                                 
         MVC   X#SCCCDA,RQUGCCD                                                 
*                                                                               
         CLI   X#STATUS,RQUGA1Q    Electronic signature to send                 
         JNE   UPDE214                                                          
         GOTOR BLDGDAE             Set expiry date                              
         JNE   UPDE195                                                          
         GOTOR BLDGDAS             Set sent date                                
         JNE   UPDE195                                                          
         GOTOR BLDEMAL             Set email addresses                          
         JNE   UPDE195                                                          
*                                  Read for PTAELs and FADELs and copy          
         USING ZTSARECD,R3         to new buffer                                
UPDE214  L     R3,AIO1                                                          
         XC    ZTRLEN,ZTRLEN                                                    
         XC    ZTRKEY(ZTRKEYL),ZTRKEY                                           
         XC    ZTRDELM,ZTRDELM                                                  
         GOTOR BUFELM,DMCB,('TSARDH',OLDBUF),AIO1                               
         J     UPDE214B                                                         
*                                                                               
UPDE214A GOTOR BUFELM,DMCB,('TSANXT',OLDBUF),AIO1                               
*                                                                               
         USING PTAELD,ZTRDELM                                                   
UPDE214B TM    TSARERRS,TSEEOF     End of file?                                 
         JNZ   UPDE216                                                          
*&&UK                                                                           
         CLI   ZTRDELM,FADELQ                                                   
         JE    UPDE214D                                                         
*&&                                                                             
         CLI   ZTRDELM,PTAELQ                                                   
         JE    UPDE214D                                                         
* copy old BrO client approval details                                          
         CLI   ZTRDELM,STCELQ                                                   
         JNE   UPDE214A                                                         
         CLI   ZTRDELM+STCIND-STCEL,STCIEST2   Preserve internal                
         JNE   UPDE214A                          approver comments              
         CLI   ZTRDELM+STCETY2-STCEL,STCAPCMI                                   
         JNE   UPDE214A                                                         
*                                                                               
UPDE214D DS    0H                                                               
         XC    ELEMENT,ELEMENT                                                  
         LLC   RF,PTALN                                                         
         BCTR  RF,0                                                             
         MVC   ELEMENT(0),PTAELD                                                
         EX    RF,*-6                                                           
         GOTOR PUTSAR,NEWBUF                                                    
         JE    UPDE214A            next array entry                             
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         OI    TWAMODE,TWAMERP                                                  
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE216  GOTOR BLDAPCM             Build approver comments element              
                                                                                
UPDE218  GOTOR BLDAUTH             Build authoriser name element                
                                                                                
UPDE220  GOTOR BLDAPDT             Build approver date elemenmt                 
                                                                                
         GOTOR XOAPID              add/transfer PID element                     
         JE    UPDE222                                                          
         OC    ROUERRV,ROUERRV     (skip PUTSAR error as already out)           
         JZ    UPDESTN                                                          
         J     UPDE195                                                          
                                                                                
UPDE222  GOTOR XFREMS              transfer EMS element                         
         JE    UPDE224                                                          
         OC    ROUERRV,ROUERRV     (skip PUTSAR error as already out)           
         JZ    UPDESTN                                                          
         J     UPDE195                                                          
                                                                                
UPDE224  GOTOR XELSIG              transfer electronic signature els            
         JE    UPDE226                                                          
         OC    ROUERRV,ROUERRV     (skip PUTSAR error as already out)           
         JZ    UPDESTN                                                          
         J     UPDE195                                                          
                                                                                
UPDE226  CLI   X#STATUS,X#NOSTAQ   combined maint and status call?              
         JE    UPDESTY             change                                       
         J     UPDE231                                                          
                                                                                
*        status only change code here                                           
                                                                                
UPDE230  MVI   X#MODE,FF           set mode to 'update only'                    
                                                                                
*        may be maint and status change combined                                
                                                                                
UPDE231  MVC   X#NSCHAR,X#STATUS   save new status character value              
         MVC   X#SC2NVL,X#SC2OVL                                                
         MVC   X#SC3NVL,X#SC3OVL                                                
         NI    X#SC2NVL,FF-(ESTKMERG+ESTKSINA)                                  
         CLI   X#MODE,FF           update only mode                             
         JE    *+8                  don't remove status 3 values                
         NI    X#SC3NVL,FF-(ESTSWCTM+ESTSITTM+ESTSITMS)                         
         MVI   X#SCNVAL,ESTKSUBM   status change code here                      
         CLI   X#STATUS,RQUGABQ    - submit to internal approver                
         JNE   UPDE232                                                          
         OI    X#SC2NVL,ESTKSINA                                                
         J     UPDE235                                                          
*                                                                               
UPDE232  CLI   X#STATUS,RQUGA1Q    - submit for electronic signature            
         JE    UPDE235                                                          
         CLI   X#STATUS,RQUGA2Q    - recall electronic signature                
         JE    UPDE235                                                          
         CLI   X#STATUS,RQUGASQ    - submit to client approver                  
         JNE   UPDE233                                                          
         TM    SCPXEL+CPXSTAT2-CPXELD,CPXSFAPE self approve?                    
         JZ    UPDE235                                                          
         MVI   X#NSCHAR,RQUGAAQ                                                 
         MVI   X#SCNVAL,ESTKCAPP                                                
         J     UPDE235                                                          
UPDE233  MVI   X#SCNVAL,ESTKCAPP                                                
         CLI   X#STATUS,RQUGAAQ    - approve                                    
         JE    UPDE235                                                          
         MVI   X#SCNVAL,ESTKINTA                                                
         CLI   X#STATUS,RQUGAIQ    - internally approve                         
         JE    UPDE235                                                          
         MVI   X#SCNVAL,ESTKREJE                                                
         CLI   X#STATUS,RQUGARQ    - reject                                     
         JE    UPDE235                                                          
         MVI   X#SCNVAL,ESTKLOGD                                                
         CLI   X#STATUS,RQUGADQ    - logical delete                             
         JE    UPDE235                                                          
         MVI   X#SCNVAL,ESTKDELT                                                
         CLI   X#STATUS,RQUGA#Q    - physical delete                            
         JE    UPDE235                                                          
*                                                                               
         OI    TWAMODE,TWAMEDP                                                  
         MVC   XERRTXT(1),X#STATUS                                              
         GOTOR PUTERR,AE$NAWTT                                                  
         J     UPDESTN                                                          
                                                                                
UPDE235  MVC   ES#ESTN,RQUGGNO     read estimate record                         
         GOTOR GETEST,2                                                         
         JE    UPDE236                                                          
                                                                                
         OI    TWAMODE,TWAMEDP                                                  
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE236  CLI   X#STATUS,RQUGASQ    on submit and (internal) approve             
         JE    UPDE237             check for work codes not locked              
         CLI   X#STATUS,RQUGA1Q                                                 
         JE    UPDE237                                                          
         CLI   X#STATUS,RQUGAAQ                                                 
         JE    UPDE237                                                          
         CLI   X#STATUS,RQUGAIQ                                                 
         JNE   UPDE237A                                                         
                                                                                
UPDE237  GOTOR TSTWCS                                                           
         JE    UPDE237A                                                         
         OI    TWAMODE,TWAMEDP                                                  
         ICM   R1,3,ROUERRV        (XERRTXT set by TSTWCS)                      
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
         USING ESTRECD,R2                                                       
UPDE237A LA    R2,CSVKEY2                                                       
*&&US*&& J     UPDE240                                                          
*&&UK                                                                           
         TM    ESTRSTA2,ESTKBILP+ESTKBILA                                       
         JZ    UPDE240             OK - No billing activities                   
         CLI   X#STATUS,RQUGADQ    - logical delete                             
         JE    UPDE238                                                          
         CLI   X#STATUS,RQUGA#Q    - physical delete                            
         JE    UPDE238                                                          
         CLI   X#STATUS,RQUGARQ    - reject                                     
         JNE   UPDE240                                                          
UPDE238  OI    TWAMODE,TWAMEDP                                                  
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$ESHBA                                                  
         J     UPDESTN                                                          
*&&                                                                             
UPDE240  CLI   X#ACTION,X#ACTISQ   Status change only                           
         JNE   UPDE241             No                                           
         CLC   RQUGCOM,SPACES                                                   
         JH    UPDE241                                                          
         MVC   RQUGCOM,RQUGDES                                                  
                                                                                
UPDE241  MVC   X#SCCOMM,RQUGCOM    save status change comment                   
         MVC   X#SCCCNA,RQUGCCN                                                 
         MVC   X#SCCCDA,RQUGCCD                                                 
*                                                                               
         MVC   X#SCOVAL,ESTRSTA1   set old status value                         
         MVC   X#SC2OVL,ESTRSTA2   old status value 2                           
         MVC   X#SC3OVL,ESTRSTA3   old status value 3                           
                                                                                
         TM    X#SCOVAL,ESTKCREA   if more than created                         
         JNZ   UPDE245                                                          
         CLI   X#SCNVAL,ESTKDELT   and to be deleted                            
         JNE   UPDE245                                                          
                                                                                
         MVI   X#SCNVAL,ESTKLOGD   then delete logically only                   
         MVI   X#SJUPD,YESQ                                                     
         J     UPDE260             (skip validation in that case)               
                                                                                
UPDE245  LA    R1,SCVTAB           test valid status change                     
         TM    G#OFSTA2,OFFSIAEQ                                                
         JZ    UPDE250                                                          
         LA    R1,SC2TAB           test valid status change                     
                                                                                
UPDE250  CLI   0(R1),FF            EOT                                          
         JNE   *+6                                                              
         DC    H'0'                (how could that happen?)                     
         CLC   X#SCNVAL,0(R1)      found new status entry?                      
         JE    UPDE255                                                          
         AHI   R1,SCVTABQ                                                       
         J     UPDE250                                                          
                                                                                
UPDE255  CLC   X#SCOVAL,1(R1)      ensure old status valid for new              
         JE    UPDE260                                                          
         CLC   X#SCOVAL,2(R1)                                                   
         JE    UPDE260                                                          
         CLC   X#SCOVAL,3(R1)                                                   
         JE    UPDE260                                                          
         CLC   X#SCOVAL,4(R1)                                                   
         JE    UPDE260                                                          
         OI    TWAMODE,TWAMEDP                                                  
         MVC   XERRTXT,SPACES                                                   
*                                  Checking old=new status to check for         
*                                  concurrency  if status the same then         
*                                  record must be changed elsewhere             
         CLC   X#SCOVAL,X#SCNVAL   Old=new status                               
         JNE   UPDE258                                                          
         GOTOR PUTERR,AE$FATAL     Record changed elsewhere                     
         J     UPDESTN                                                          
*                                                                               
UPDE258  GOTOR PUTERR,AE$INTST     Invalid too status                           
         J     UPDESTN                                                          
                                                                                
UPDE260  LA    R1,STATAB           set old status character value               
         MVC   BYTE1,X#SC2OVL                                                   
         NI    BYTE1,ESTKSINA                                                   
                                                                                
UPDE265  CLI   0(R1),FF                                                         
         JNE   *+6                                                              
         DC    H'0'                (must not happen)                            
         CLC   X#SCOVAL,0(R1)                                                   
         JNE   *+14                                                             
         CLC   BYTE1,1(R1)                                                      
         JE    UPDE270                                                          
         AHI   R1,STATABQ                                                       
         J     UPDE265                                                          
                                                                                
UPDE270  MVC   X#OSCHAR,2(R1)      anything else to check?                      
         MVC   X#OSENUM,3(R1)                                                   
         CLI   X#OSCHAR,C'S'                                                    
         JNE   UPDE271                                                          
         TM    X#SC2OVL,ESTKSINA   submitted internally?                        
         JZ    UPDE271                                                          
         MVI   X#OSCHAR,C'B'                                                    
                                                                                
UPDE271  LA    R1,STATAB           set old status character value               
         MVC   BYTE1,X#SC2NVL                                                   
         NI    BYTE1,ESTKSINA                                                   
                                                                                
UPDE271A CLI   0(R1),FF                                                         
         JNE   *+6                                                              
         DC    H'0'                (must not happen)                            
         CLC   X#SCNVAL,0(R1)                                                   
         JNE   *+14                                                             
         CLC   BYTE1,1(R1)                                                      
         JE    UPDE271B                                                         
         AHI   R1,STATABQ                                                       
         J     UPDE271A                                                         
                                                                                
UPDE271B MVC   X#NSENUM,3(R1)                                                   
                                                                                
         CLI   X#SCNVAL,ESTKSUBM   submit?                                      
         JNE   UPDE290                                                          
         CLI   X#STATUS,RQUGA2Q    if electronic signatures                     
         JE    UPDE290              skip client approver validation             
         CLI   X#STATUS,RQUGA1Q                                                 
         JE    UPDE290                                                          
         GOTOR VALAPP              validate approver (passed this time)         
         JE    UPDE275                                                          
                                                                                
         OI    TWAMODE,TWAMEDP                                                  
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE275  L     R3,AIO2             check for client approver set                
         USING EMDELD,R3                                                        
         AHI   R3,ESTRFST-ESTRECD                                               
         CLI   EMDEL,EMDELQ                                                     
         JNE   *+2                                                              
                                                                                
         OC    X#SAPPR,X#SAPPR     approver passed this time?                   
         JZ    UPDE280                                                          
         MVC   EMDSCA,X#SAPPR      save it                                      
                                                                                
UPDE280  MVC   X#SAPPR,EMDSCA                                                   
                                                                                
         OC    X#SIAPP,X#SIAPP     internal approver passed this time?          
         JZ    UPDE282                                                          
         MVC   EMDSIA,X#SIAPP      save it                                      
                                                                                
UPDE282  MVC   X#SIAPP,EMDSIA                                                   
                                                                                
UPDE283  DS    0H                  (no data changes until UPDE290 if            
*                                   in combined maint & status mode)            
         CLI   X#STATUS,RQUGABQ                                                 
         JE    UPDE284                                                          
         OC    X#SAPPR,X#SAPPR                                                  
         JNZ   UPDE284                                                          
                                                                                
         OI    TWAMODE,TWAMEDP                                                  
         MVC   XERRTXT,SPACES                                                   
         DC    H'0'                                                             
         GOTOR PUTERR,AE$NOCLA                                                  
         J     UPDESTN                                                          
                                                                                
UPDE284  OC    X#SIAPP,X#SIAPP                                                  
         JNZ   UPDE285                                                          
         TM    G#OFSTA2,OFFSIAEQ                                                
         JZ    UPDE285                                                          
                                                                                
         OI    TWAMODE,TWAMEDP                                                  
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$MIIAP                                                  
         J     UPDESTN                                                          
                                                                                
UPDE285  GOTOR CHKJOB              check job                                    
         JE    UPDE290                                                          
                                                                                
         OI    TWAMODE,TWAMEDP                                                  
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE290  CLI   X#ACTION,X#ACTISQ   action is a status only change               
         JNE   UPDE298                                                          
         CLI   X#STATUS,RQUGA1Q    Electronic signature to send                 
         JNE   UPDE294                                                          
         GOTOR BLDGDAE             Set expiry date                              
         JNE   UPDE292                                                          
         GOTOR BLDGDAS             Set sent date                                
         JNE   UPDE292                                                          
         GOTOR BLDEMAL             Set email addresses                          
         JE    UPDE298                                                          
                                                                                
UPDE292  OI    TWAMODE,TWAMEDP                                                  
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE294  GOTOR BLDAPCM             Build approver comments element              
                                                                                
UPDE296  GOTOR BLDAUTH             Build authoriser name element                
                                                                                
UPDE297  GOTOR BLDAPDT             Build approver date elemenmt                 
                                                                                
UPDE298  J     UPDESTY                                                          
         DROP  R2,R3                                                            
                                                                                
*** Common text values ************************************************         
                                                                                
UPDE300  LA    R2,ERDTHTQ          *** Header                                   
         J     UPDE350                                                          
                                                                                
UPDE302  LA    R2,ERDTETQ          *** Estimate additional text                 
         J     UPDE350                                                          
                                                                                
UPDE304  LA    R2,ERDTFTQ          *** Footer                                   
         J     UPDE350                                                          
                                                                                
UPDE310  LA    R2,ERDTWTQ          *** W/C                                      
         J     UPDE350                                                          
                                                                                
UPDE315  LA    R2,ERDTITQ          *** Item                                     
         J     UPDE350                                                          
                                                                                
UPDE320  LA    R2,ERDTCTQ          *** Category                                 
         J     UPDE350                                                          
                                                                                
UPDE350  CLI   X#MODE,FF           for 'update only' must not get here          
         JE    *+2                 get here                                     
                                                                                
         XR    RF,RF                                                            
         OC    RQUTXT1L,RQUTXT1L                                                
         JZ    UPDE390             no text                                      
                                                                                
         USING ERDELD,R4                                                        
UPDE364  XC    ELEMENT,ELEMENT     build element                                
         LA    R4,ELEMENT                                                       
         MVI   ERDEL,ERDELQ                                                     
         LLC   RF,RQUTXT1L         l'text                                       
         LR    R1,RF                                                            
         AHI   RF,ERDTLNQ                                                       
         STC   RF,ERDLN            l'element                                    
         STC   R2,ERDTYP           and type                                     
                                                                                
         AHI   R1,-1               exec length of text                          
         MVC   ERDTEXT(0),RQUTXT1                                               
         EX    R1,*-6                                                           
                                                                                
         GOTOR PUTSAR,NEWBUF                                                    
         JE    UPDE390             next array entry                             
                                                                                
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         OI    TWAMODE,TWAMERP                                                  
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE390  DS    0H                                                               
         J     UPDESTY                                                          
                                                                                
*** Work code values **************************************************         
                                                                                
UPDE400  CLI   X#MODE,FF           for 'update only' must not get here          
         JE    *+2                 get here                                     
                                                                                
         GOTOR VALWCD              validate w/c data                            
         JNE   UPDE410                                                          
                                                                                
         GOTOR PSTWCD              post w/c data element                        
         JNE   UPDE410                                                          
                                                                                
         J     UPDESTY                                                          
                                                                                
UPDE410  ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'X#WORKC),X#WORKC                                       
         OI    TWAMODE,TWAMERP                                                  
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
*** Item values *******************************************************         
                                                                                
UPDE500  CLI   X#MODE,FF           for 'update only' must not get here          
         JE    *+2                 get here                                     
                                                                                
         GOTOR VALITM              validate item data                           
         JNE   UPDE510                                                          
                                                                                
         GOTOR PSTITM              post item data element                       
         JNE   UPDE510                                                          
                                                                                
         J     UPDESTY                                                          
                                                                                
UPDE510  ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(4),X#ICODE                                               
         MVI   XERRTXT+5,C'('                                                   
         MVC   XERRTXT+6(2),X#WORKC                                             
         MVI   XERRTXT+8,C')'                                                   
         OI    TWAMODE,TWAMERP                                                  
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
*** Xtra data values **************************************************         
                                                                                
UPDE600  CLI   X#MODE,FF           for 'update only' must not get here          
         JE    *+2                 get here                                     
                                                                                
         GOTOR SVPXDF              set and validate XDF element                 
         JE    UPDE610                                                          
                                                                                
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         OI    TWAMODE,TWAMERP                                                  
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE610  DS    0H                                                               
         J     UPDESTY                                                          
                                                                                
*** Category values ***************************************************         
                                                                                
UPDE700  CLI   X#MODE,FF           for 'update only' must not get here          
         JE    *+2                 get here                                     
                                                                                
         GOTOR SVPCAT              set and validate category data               
         JE    UPDE710                                                          
                                                                                
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         OI    TWAMODE,TWAMERP                                                  
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE710  DS    0H                                                               
         J     UPDESTY                                                          
                                                                                
*** Source (copy) values *********************************************          
                                                                                
UPDE750  CLI   X#ACTION,RQUGACQ    For create action                            
         JNE   *+2                                                              
                                                                                
UPDE754  L     R1,AFREEST                                                       
                                                                                
UPDE755  CLI   0(R1),0             spare entry?                                 
         JE    UPDE760                                                          
         AHI   R1,L'EGNPNUM                                                     
         J     UPDE755                                                          
                                                                                
UPDE760  MVC   0(L'EGNPNUM,R1),RQUSEST                                          
         MVI   L'EGNPNUM(R1),0   clear next entry                               
                                                                                
         J     UPDESTY                                                          
                                                                                
*** Last for update call **********************************************         
                                                                                
UPDE800  TM    TWAMODE,TWAMERP+TWAMEDP   exit on all errors                     
         JNZ   UPDESTY                                                          
                                                                                
         CLI   X#ACTION,RQUGACQ    Create action:                               
         JNE   UPDE815                                                          
                                                                                
UPDE802  GOTOR GETEGN              get estimate global number                   
         JE    UPDE810                                                          
         OI    TWAMODE,TWAMEDP                                                  
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE810  GOTOR GETELN              get local estimate number                    
         JE    UPDE815                                                          
         OI    TWAMODE,TWAMEDP+TWAMUWD                                          
         ICM   R1,3,ROUERRV                                                     
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR                                                           
         J     UPDESTN                                                          
                                                                                
UPDE815  CLI   X#ACTION,X#ACTISQ   Action is status change only?                
         JNE   UPDE816                                                          
*                                                                               
         CLI   X#STATUS,RQUGABQ    - submit to internal approver                
         JE    UPDE870                                                          
         CLI   X#STATUS,RQUGASQ    - submit to client approver                  
         JE    UPDE870                                                          
         CLI   X#STATUS,RQUGA1Q    - submit for electronic signature            
         JE    UPDE870                                                          
         CLI   X#STATUS,RQUGA2Q    - recall for electronic signature            
         JE    UPDE870                                                          
         CLI   X#STATUS,RQUGAAQ    - approve                                    
         JE    UPDE870                                                          
         CLI   X#STATUS,RQUGAIQ    - internally approve                         
         JE    UPDE870                                                          
         CLI   X#STATUS,RQUGARQ    - reject                                     
         JE    UPDE870                                                          
         CLI   X#STATUS,RQUGADQ    - logical delete                             
         JE    UPDE870                                                          
         CLI   X#STATUS,RQUGA#Q    - physical delete                            
         JE    UPDE870                                                          
                                                                                
*        add/maintain code here                                                 
UPDE816  DS    0H                                                               
*&&US                                                                           
         CLI   X#SCNVAL,ESTKCAPP   CLIENT APPROVED  ?                           
         JE    UPDE818             YES, UPDATE CE STATUS                        
*                                  NO..                                         
         CLI   X#SCNVAL,ESTKSUBM   SUBMITTED ?                                  
         JNE   UPDE820             NO, CONTINUE                                 
*                                  YES                                          
UPDE818  GOTOR UPDCES              UPDATE STAUS DETAILS ON DIR/MSTER            
*&&                                                                             
*                                                                               
UPDE820  CLI   X#ACTION,RQUGACQ    create?                                      
         JE    UPDE825                                                          
                                                                                
         GOTOR DELPAS              delete passives                              
                                                                                
         GOTOR GETREC              get main record                              
                                                                                
         GOTOR DELALL              delete all records                           
                                                                                
         USING ESTRECD,R2                                                       
UPDE825  L     R2,AIO2             R2=A(estimate record)                        
         L     R5,AIO3             R5=A(element buffer)                         
                                                                                
         LR    R0,R2               Clear IO area 2                              
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LR    R0,R5               Clear IO area 3                              
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   ESTKEY,CSVKEY2      set main record key and dummy length         
         MVC   ESTRLEN,=Y(ESTRFST-ESTKEY+1)                                     
                                                                                
         MVC   ESTRSTA1,X#SCNVAL   set status byte 1 on main record             
         MVC   ESTRSTA2,X#SC2NVL   set status byte 2 on main record             
         MVC   ESTRSTA3,X#SC3NVL   set status byte 3 on main record             
         MVC   ESTRSOFF,X#SJOFF                                                 
                                                                                
         USING ZTSARECD,R4                                                      
         L     R4,AIO1             R4=A(element TSAR buffer)                    
         XC    ZTRLEN,ZTRLEN                                                    
         XC    ZTRKEY(ZTRKEYL),ZTRKEY                                           
         XC    ZTRDELM,ZTRDELM                                                  
         MVI   BYTE1,NOQ                                                        
         MVI   BYTE2,NOQ                                                        
         MVI   X#PIDST,NOQ                                                      
                                                                                
         GOTOR BUFELM,DMCB,('TSARDH',NEWBUF),AIO1                               
         J     UPDE835                                                          
                                                                                
UPDE830  GOTOR BUFELM,DMCB,('TSANXT',NEWBUF),AIO1                               
                                                                                
UPDE835  TM    TSARERRS,TSEEOF                                                  
         JZ    UPDE836                                                          
         CLI   X#NODATA,YESQ       no details data scenario?                    
         JNE   UPDE858                                                          
         OI    TWAMODE,TWAMEDP+TWAMUWD                                          
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$NCSCH                                                  
         J     UPDESTN                                                          
                                                                                
         USING EMDELD,R3                                                        
UPDE836  LA    R3,ZTRDELM          R3=A(element)                                
                                                                                
         CLI   EMDEL,EMDELQ        main element?                                
         JNE   UPDE838                                                          
                                                                                
         ZAP   EMDAMT,X#TOTAL      set totals                                   
         ZAP   EMDFCA,X#TOTFC                                                   
         ZAP   EMDTCA,X#TOTCA                                                   
         ZAP   EMDTCF,X#TOTCF                                                   
         ZAP   EMDTVA,X#TOTVA                                                   
         ZAP   EMDTVF,X#TOTVF                                                   
         ZAP   EMDTIWT,X#TOTIW     internal workcode total                      
         ZAP   EMDTXWT,X#TOTXW     external workcode total                      
         ZAP   EMDHRS,X#TOHRS      Estimate total hours                         
*                                                                               
         MVC   EMDGNO,ES#ESTN      and global number                            
                                                                                
         CLC   EMDCUR,AGYCURR      set key status values                        
         JE    *+8                                                              
         OI    ESTRSTA2,ESTKFCUR                                                
                                                                                
         MVC   X#ESIGSO,EMDGSTAT                                                
         CLI   X#STATUS,RQUGA1Q    - submit for electronic signature            
         JNE   *+8                                                              
         MVI   EMDGSTAT,EMDGSENT                                                
         CLI   X#STATUS,RQUGA2Q    - recall for electronic signature            
         JNE   *+8                                                              
         MVI   EMDGSTAT,EMDGRECA                                                
         MVC   X#ESIGSN,EMDGSTAT                                                
                                                                                
         CLI   EMDLAN,YESQ                                                      
         JNE   *+8                                                              
         OI    ESTRSTA2,ESTKLANG                                                
                                                                                
         XOUT  EMDIDN,DUB,2                                                     
         CLC   RQUUIDN,DUB         check against value passed                   
         JE    UPDE837                                                          
                                                                                
         OI    TWAMODE,TWAMEDP+TWAMUWD                                          
         GOTOR PUTERR,AE$EABOU     concurrent file update                       
         J     UPDESTN                                                          
                                                                                
UPDE837  XR    R1,R1               save incremented ID number                   
         ICM   R1,3,EMDIDN                                                      
         AHI   R1,1                                                             
         STCM  R1,3,EMDIDN                                                      
         MVC   X#NEWIDN,EMDIDN                                                  
                                                                                
UPDE838  CLI   EMDEL,EMDELQ        elements for main record                     
         JE    UPDE840                                                          
         CLI   EMDEL,ENMELQ                                                     
         JE    UPDE840                                                          
         CLI   EMDEL,STCELQ                                                     
         JE    UPDE840                                                          
         CLI   EMDEL,PIDELQ                                                     
         JE    UPDE840                                                          
         CLI   EMDEL,FFTELQ                                                     
         JE    UPDE840                                                          
         CLI   EMDEL,GDAELQ                                                     
         JE    UPDE840                                                          
         CLI   EMDEL,EMSELQ                                                     
         JNE   UPDE844                                                          
                                                                                
UPDE840  GOTO1 VHELLO,DMCB,(C'P',ACCMST),ESTRECD,EMDELD,ADDEND,0                
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         CLI   EMDEL,ENMELQ        if name element check adding sources         
         JNE   UPDE842             else next element                            
         GOTOR ADDEMS              add EMS elements after name element          
         J     UPDE830             else next element                            
                                                                                
UPDE842  CLI   EMDEL,PIDELQ        any need to add STC name change?             
         JNE   UPDE830                                                          
         MVI   X#PIDST,YESQ                                                     
         J     UPDE830                                                          
                                                                                
UPDE844  CLI   BYTE1,YESQ                                                       
         JE    UPDE852             processed main record?                       
                                                                                
         CLI   X#STATUS,X#NOSTAQ   skip if no status change, else do            
         JE    UPDE850             UPDE870 ff updates here                      
                                                                                
         GOTOR PROPID              process PID owner element                    
         JE    UPDE850                                                          
                                                                                
         OI    TWAMODE,TWAMEDP+TWAMUWD                                          
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$TMUCS     too many users - cannot save                 
         J     UPDESTN                                                          
                                                                                
UPDE850  MVI   BYTE1,YESQ                                                       
                                                                                
         L     R0,AIO4             copy record to AIO4 for ADDPAS               
         LA    R1,IOLENQ                                                        
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR SAVREC              save estimate record                         
                                                                                
         GOTOR ADDPAS              add passives for main record                 
                                                                                
         USING ERDELD,R3                                                        
BUF      USING ERDELD,R5                                                        
UPDE852  CLI   BUF.ERDEL,0         Is the buffer empty                          
         JE    UPDE856             Yes                                          
*                                                                               
UPDE854  CLI   BUF.ERDEL,ERDELQ                                                 
         JNE   UPDE858             Put out what is in buffer                    
         CLI   BUF.ERDTYP,ERDTWDQ                                               
         JNE   UPDE858             Put out what is in buffer                    
         CLI   ERDEL,ERDELQ                                                     
         JNE   UPDE858             Put out what is in buffer                    
         CLI   ERDTYP,ERDTWDQ                                                   
         JE    UPDE858             Put out what is in buffer                    
         CLI   ERDTYP,ERDTCSQ                                                   
         JE    UPDE858             Put out what is in buffer                    
         CLI   ERDTYP,ERDTFTQ                                                   
         JE    UPDE858             Put out what is in buffer                    
         CLI   ERDTYP,ERDTCTQ                                                   
         JE    UPDE858             Put out what is in buffer                    
UPDE855  LLC   RF,BUF.ERDLN                                                     
         AR    R5,RF                                                            
         CLI   BUF.ERDEL,0         End of buffer found                          
         JNE   UPDE855             No                                           
                                                                                
UPDE856  LLC   RF,ERDLN            Add element to element buffer                
                                                                                
         LR    RE,R5               test enough space left for it                
         AR    RE,RF                                                            
         L     R0,AIO3                                                          
         SR    RE,R0                                                            
         CHI   RE,(2000-8)-(ESTRFST-ESTRECD)                                    
         JL    UPDE857             OK                                           
         L     R5,AIO3                                                          
         J     UPDE858             Else process buffer now                      
                                                                                
UPDE857  SHI   RF,1                                                             
         MVC   BUF.ERDEL(0),ERDEL                                               
         EXRL  RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R5,RF                                                            
         MVI   BUF.ERDEL,0         Set end of element buffer                    
         L     R5,AIO3             R5=A(element buffer)                         
         J     UPDE830             Get next element                             
                                                                                
UPDE858  CLI   BUF.ERDEL,0                                                      
         JE    UPDE862                                                          
         XR    R1,R1                                                            
UPDE859  LLC   RF,BUF.ERDLN                                                     
         AR    R5,RF                                                            
         AR    R1,RF                                                            
         CLI   BUF.ERDEL,0         End of buffer found                          
         JNE   UPDE859             No                                           
         L     R5,AIO3                                                          
         XR    RE,RE                                                            
         ICM   RE,3,ESTRLEN                                                     
         AR    RE,R1                                                            
         AHI   RE,8                                                             
         CHI   RE,2000                                                          
         JL    UPDE860                                                          
                                                                                
         GOTOR SAVREC              save estimate record                         
                                                                                
UPDE860  GOTO1 VHELLO,DMCB,(C'P',ACCMST),ESTRECD,BUF.ERDELD,ADDEND,0            
         CLI   12(R1),0                                                         
         JNE   *+2                 next element                                 
         LLC   RF,BUF.ERDLN                                                     
         AR    R5,RF                                                            
         CLI   BUF.ERDEL,0         End of buffer found                          
         JNE   UPDE860             No                                           
         TM    TSARERRS,TSEEOF     Did we hit end of TSAR buffer                
         JNZ   UPDE862             Yes                                          
         L     R5,AIO3                                                          
         J     UPDE856                                                          
                                                                                
UPDE862  GOTOR SAVREC              save estimate record                         
*                                                                               
         L     R0,AIO2             copy main rec back to AIO2                   
         LA    R1,IOLENQ                                                        
         L     RE,AIO4                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     UPDE960             common code                                  
         DROP  R2,R3,R4                                                         
                                                                                
*        status change code here                                                
                                                                                
UPDE870  DS    0H                                                               
*&&US                                                                           
         CLI   X#SCNVAL,ESTKCAPP   CLIENT APPROVED  ?                           
         JE    UPDE870A            YES, UPDATE CE STATUS                        
*                                  NO..                                         
         CLI   X#SCNVAL,ESTKSUBM   SUBMITTED ?                                  
         JNE   UPDE870B            NO, CONTINUE                                 
*                                  YES                                          
UPDE870A GOTOR UPDCES              UPDATE STAUS DETAILS ON DIR/MSTER            
*&&                                                                             
UPDE870B GOTOR GETREC              GET RECORD INTO AIO2                         
                                                                                
         GOTOR DELPAS              delete passives                              
                                                                                
         USING ESTRECD,R2                                                       
         L     R2,AIO2             check and increment ID number                
         USING EMDELD,R3                                                        
         LA    R3,ESTRFST                                                       
         CLI   EMDEL,EMDELQ        must be first element                        
         JNE   *+2                                                              
*                                                                               
         DS    0H                  This block is just a clone of                
         LA    R1,EMDIDN              XOUT  EMDIDN,DUB,2                        
         LA    RF,DUB              (needed a J  *+20                            
         LA    R0,2                 rather than B  *+20 at the end)             
         SR    RE,RE                                                            
         ICM   RF,8,0(R1)                                                       
         SLDL  RE,4                                                             
         IC    RE,*+40(RE)                                                      
         SLL   RE,20                                                            
         SLDL  RE,4                                                             
         IC    RE,*+28(RE)                                                      
         SRL   RF,8                                                             
         STCM  RE,9,0(RF)                                                       
         LA    R1,1(R1)                                                         
         LA    RF,2(RF)                                                         
         JCT   R0,*-42                                                          
         J     *+20                                                             
         DC    C'0123456789ABCDEF'                                              
*                                                                               
         CLC   RQUUIDN,DUB         check against value passed                   
         JE    UPDE872                                                          
                                                                                
         OI    TWAMODE,TWAMEDP+TWAMUWD                                          
         GOTOR PUTERR,AE$EABOU     concurrent file update                       
         J     UPDESTN                                                          
                                                                                
UPDE872  XR    R1,R1               save incremented ID number                   
         ICM   R1,3,EMDIDN                                                      
         AHI   R1,1                                                             
         STCM  R1,3,EMDIDN                                                      
         MVC   X#NEWIDN,EMDIDN                                                  
         MVC   X#ESIGSO,EMDGSTAT   save electronic signature                    
                                                                                
         MVC   EMDLDT,SVTODAYP                                                  
         MVO   TEMP(L'CTIME),CTIME                                              
         MVC   EMDLTI,TEMP                                                      
         MVC   EMDLPI,CCTPID                                                    
                                                                                
         CLI   X#STATUS,RQUGASQ    - submit to client approver                  
         JNE   UPDE876                                                          
         TM    SCPXEL+CPXSTAT2-CPXELD,CPXSFAPE self approve?                    
         JZ    UPDE876                                                          
         MVC   X#SAPPR,CCTPID      Set connected user as approver               
                                                                                
UPDE876  OC    X#SAPPR,X#SAPPR     set client approver (on submit)              
         JZ    UPDE878                                                          
         MVC   EMDSCA,X#SAPPR      EMDSCB not used anymore                      
                                                                                
UPDE878  OC    X#SIAPP,X#SIAPP     set internal approver (on submit)            
         JZ    UPDE880                                                          
         MVC   EMDSIA,X#SIAPP                                                   
*                                                                               
UPDE880  CLI   X#STATUS,RQUGA1Q    Electronic signature to send                 
         JNE   UPDE882                                                          
         MVI   EMDGSTAT,EMDGSENT                                                
         MVC   X#ESIGSN,EMDGSTAT                                                
                                                                                
UPDE882  CLI   X#STATUS,RQUGA1Q    Electronic signature to send                 
         JE    UPDE885                                                          
         CLI   X#STATUS,RQUGARQ    rejecting                                    
         JE    UPDE884                                                          
         CLI   X#STATUS,RQUGA2Q    - recall for electronic signature            
         JNE   UPDE912                                                          
UPDE884  OC    EMDGSTAT,EMDGSTAT   Check est has electronic status              
         JZ    *+8                                                              
         MVI   EMDGSTAT,EMDGRECA   If so set recall status                      
*                                                                               
         MVC   X#ESIGSN,EMDGSTAT                                                
         USING ESTRECD,R2                                                       
UPDE885 L      R2,AIO2             Find elements related to electronic          
         USING GDAELD,R3            signatures and remove them from             
         LA    R3,ESTRFST            the main estimate record                   
UPDE886  CLI   GDAEL,0                                                          
         JE    UPDE892                                                          
         CLI   GDAEL,GDAELQ        General date element                         
         JE    UPDE888                                                          
         CLI   GDAEL,FFTELQ        Free form text element                       
         JNE   UPDE890                                                          
UPDE888  MVI   GDAEL,X'FF'         Mark element for deletion                    
UPDE890  LLC   R0,GDALN                                                         
         AR    R3,R0                                                            
         J     UPDE886                                                          
                                                                                
UPDE892  GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',ESTRECD),0                      
         CLI   12(R1),0                                                         
         JE    *+6                 Did we delete elements ok                    
         DC    H'0'                No                                           
                                                                                
UPDE912  CLC   X#SCCOMM,SPACES     Any changes to approver comments             
         JH    UPDE914                authoriser or approved date               
         CLC   X#SCCCNA,SPACES         if so remove current elements            
         JH    UPDE914                   and replace with new values            
         CLC   X#SCCCDA,SPACES                                                  
         JNH   UPDE924             No - nothing has changed - skip              
         USING ESTRECD,R2                                                       
         L     R2,AIO2             Find audit elements and remove them          
         USING STCELD,R3            with the exception of the interal           
         LA    R3,ESTRFST            approval comments                          
UPDE914  CLI   STCEL,0                                                          
         JE    UPDE918                                                          
         CLI   STCEL,STCELQ                                                     
         JNE   UPDE916                                                          
         CLI   STCIND,STCIEST2                                                  
         JNE   *+12                                                             
         CLI   STCETY2,STCAPCMI                                                 
         JE    UPDE916                                                          
         MVI   STCEL,X'FF'                                                      
UPDE916  LLC   R0,STCLN                                                         
         AR    R3,R0                                                            
         J     UPDE914                                                          
                                                                                
UPDE918  GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',ESTRECD),0                      
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
                                                                                
UPDE924  CLI   X#STATUS,RQUGASQ    - submit to client approver                  
         JNE   UPDE926                                                          
         TM    SCPXEL+CPXSTAT2-CPXELD,CPXSFAPE self approve?                    
         JZ    UPDE926                                                          
         MVC   X#SAPPR,CCTPID      Set connected user as approver               
         XC    ELEMENT,ELEMENT                                                  
         L     R2,AIO2                                                          
         USING STCELD,R4                                                        
         LA    R4,ELEMENT                                                       
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCAPCMT                                                 
         MVI   STCETYP,STCEADD                                                  
         MVI   STCLN,STCELN2Q                                                   
         MVI   STCEPES,OINPROG                                                  
         MVI   STCECES,OCLIAPP                                                  
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ESTRECD,ELEMENT,ADDEND,0               
         CLI   12(R1),0                                                         
         JE    UPDE926                                                          
         OI    TWAMODE,TWAMEDP+TWAMUWD                                          
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$RECTB     record too big                               
         J     UPDESTN                                                          
         DROP  R2,R4                                                            
                                                                                
UPDE926  GOTOR PROPID              process PID owner element                    
         JE    UPDE928                                                          
                                                                                
         OI    TWAMODE,TWAMEDP+TWAMUWD                                          
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$TMUCS     too many users - cannot save                 
         J     UPDESTN                                                          
*                                                                               
         USING ESTRECD,R2                                                       
UPDE928  L     R2,AIO2             set new status                               
         CLI   X#SCNVAL,ESTKDELT   physical deletion?                           
         JE    UPDE958                                                          
*                                                                               
         MVC   ESTRSTA1,X#SCNVAL                                                
         MVC   ESTRSTA2,X#SC2NVL                                                
         MVC   ESTRSTA3,X#SC3NVL                                                
                                                                                
         USING ZTSARECD,R4                                                      
         L     R4,AIO1             R4=A(element TSAR buffer)                    
         XC    ZTRLEN,ZTRLEN                                                    
         XC    ZTRKEY(ZTRKEYL),ZTRKEY                                           
         XC    ZTRDELM,ZTRDELM                                                  
         MVI   BYTE1,NOQ                                                        
         MVI   BYTE2,NOQ                                                        
         MVI   X#PIDST,NOQ                                                      
                                                                                
         GOTOR BUFELM,DMCB,('TSARDH',NEWBUF),AIO1                               
         J     UPDE932                                                          
                                                                                
UPDE930  GOTOR BUFELM,DMCB,('TSANXT',NEWBUF),AIO1                               
                                                                                
UPDE932  TM    TSARERRS,TSEEOF                                                  
         JNZ   UPDE956                                                          
                                                                                
         USING EMDELD,R3                                                        
UPDE934  LA    R3,ZTRDELM          R3=A(element)                                
         CLI   EMDEL,STCELQ                                                     
         JE    UPDE936                                                          
         CLI   EMDEL,GDAELQ                                                     
         JE    UPDE936                                                          
         CLI   EMDEL,FFTELQ                                                     
         JNE   UPDE932                                                          
                                                                                
UPDE936  GOTO1 VHELLO,DMCB,(C'P',ACCMST),ESTRECD,EMDELD,ADDEND,0                
         CLI   12(R1),0                                                         
         JE    UPDE930                                                          
         DC    H'0'                                                             
                                                                                
UPDE956  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JNE   *+2                                                              
                                                                                
         LA    R2,IOKEY                                                         
         MVC   ESTKEY,CSVKEY2                                                   
         MVC   ESTKSTA1,X#SCNVAL                                                
         MVC   ESTKSTA2,X#SC2NVL                                                
         MVC   ESTKSTA3,X#SC3NVL                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JNE   *+2                                                              
                                                                                
         L     R0,AIO4             copy record to AIO4 for ADDPAS               
         LA    R1,IOLENQ                                                        
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR ADDPAS                                                           
                                                                                
         J     UPDE960             common code                                  
                                                                                
UPDE958  GOTOR DELALL              delete all records                           
                                                                                
         GOTOR DELAUD              delete audit records, too                    
                                                                                
*        General data return on end of update                                   
                                                                                
UPDE960  GOTOR NBRSRT              Read and resort erdelds in buffer            
         GOTOR BLDSTC              Build audit elements                         
         JE    UPDE962                                                          
         OI    TWAMODE,TWAMEDP+TWAMUWD                                          
         GOTOR PUTERR,ROUERRV                                                   
         J     UPDESTN                                                          
                                                                                
UPDE962  CLI   X#SCNVAL,ESTKDELT   skip on physical delete                      
         JE    UPDE964                                                          
         GOTOR BLDAUD              read and update audit records                
                                                                                
UPDE964  DS    0H                  update SJ account?                           
*&&UK*&& CLI   X#SJUPD,YESQ                                                     
*&&UK*&& JNE   UPDE966                                                          
*&&US*&& DS    0H                                                               
                                                                                
         GOTOR UPDSJE                                                           
                                                                                
         USING LIOBD,R4                                                         
UPDE966  L     R4,AALIOB           'finish off' update                          
                                                                                
         GOTOR ESTUPR              estimate upload return                       
                                                                                
         MVI   BYTE4,SE#ESTS                                                    
         GOTOR (#SEMAIL,ASEMAIL),DMCB,X#SJOFF,BYTE4                             
         JNE   UPDE980             CHECK WHETHER TO SEND EMAILS                 
                                                                                
         L     R1,AIO6                                                          
         XC    0(30,R1),0(R1)                                                   
         CLI   X#STATUS,RQUGASQ    submitting to client                         
         JNE   UPDE968                                                          
         OC    X#SAPPR,X#SAPPR                                                  
         JZ    UPDE980                                                          
         MVC   0(2,R1),X#SAPPR                                                  
         MVI   2(R1),C'C'          client approver                              
         XR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN                                                    
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRUN',A#TEAP)                   
                                                                                
         MVI   BYTE1,C'3'          ACBRA11.RQ_TET3Q                             
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#UE#CAE),       *        
               ('LD_CHARQ',BYTE1),(L'BYTE1,0)                                   
                                                                                
UPDE968  CLI   X#STATUS,RQUGABQ    submitting to internal approver              
         JNE   UPDE970                                                          
         OC    X#SIAPP,X#SIAPP                                                  
         JZ    UPDE980                                                          
         MVC   0(2,R1),X#SIAPP                                                  
         MVI   2(R1),C'I'          internal approver                            
                                                                                
         XR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN                                                    
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRUN',A#TEAP)                   
                                                                                
         MVI   BYTE1,C'3'          (Estimates)                                  
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#UE#CAE),       *        
               ('LD_CHARQ',BYTE1),(L'BYTE1,0)                                   
                                                                                
UPDE970  CLI   X#STATUS,RQUGAIQ    internal approving                           
         JNE   UPDE974                                                          
         CLI   X#OSCHAR,STCEREJT     approve from rejected?                     
         JE    UPDE972                                                          
         OC    X#SAPPR,X#SAPPR                                                  
         JZ    UPDE972                                                          
         MVC   0(2,R1),X#SAPPR                                                  
         MVI   2(R1),C'C'          client approver                              
         AHI   R1,3                                                             
                                                                                
UPDE972  MVC   0(2,R1),X#ERAIS                                                  
         MVI   2(R1),C'R'          estimate raiser                              
         XR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN                                                    
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRUN',A#TEAP)                   
                                                                                
         MVI   BYTE1,C'3'          (Estimates)                                  
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#UE#CAE),       *        
               ('LD_CHARQ',BYTE1),(L'BYTE1,0)                                   
                                                                                
UPDE974  CLI   X#STATUS,RQUGARQ    rejecting                                    
         JNE   UPDE976                                                          
                                                                                
         CLI   X#OSCHAR,STCESUBI   reject from submitted (internally)?          
         JE    UPDE978                                                          
         CLI   X#OSCHAR,STCEIAPP   reject from internally approved?             
         JE    UPDE978                                                          
         CLI   X#OSCHAR,STCESUBC   reject from submitted (to client)            
         JE    UPDE978                                                          
         CLI   X#OSCHAR,STCECAPP   client approved?                             
         JE    UPDE978                                                          
         J     UPDE980                                                          
                                                                                
UPDE976  CLI   X#STATUS,RQUGAAQ    client approving                             
         JNE   UPDE980                                                          
                                                                                
         CLI   X#OSCHAR,STCEREJT   approve from rejected?                       
         JE    UPDE978                                                          
         CLI   X#OSCHAR,STCESUBC   submitted to client?                         
         JNE   UPDE980                                                          
                                                                                
UPDE978  MVC   0(2,R1),X#ERAIS                                                  
         MVI   2(R1),C'R'          estimate raiser                              
                                                                                
         XR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN                                                    
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRUN',A#TEAP)                   
                                                                                
         MVI   BYTE1,C'3'                                                       
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#UE#CAE),       *        
               ('LD_CHARQ',BYTE1),(L'BYTE1,0)                                   
                                                                                
UPDE980  DS    0H                                                               
                                                                                
         J     UPDESTY                                                          
                                                                                
*** Global exists for estimate upload *********************************         
                                                                                
UPDESTY  LA    R0,RQUPVAL                                                       
         LA    R1,RQUPLNQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     UPDRECY                                                          
                                                                                
UPDESTN  LA    R0,RQUPVAL                                                       
         LA    R1,RQUPLNQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         TM    TWAMODE,TWAMUWD     unwind required?                             
         JZ    EXITN                                                            
         NI    TWAMODE,FF-TWAMUWD                                               
         OI    GIND1,GIUNWIND      DDLINK will unwind/abend                     
         J     EXITN                                                            
                                                                                
         DROP  RB                                                               
                                                                                
         EJECT                                                                  
                                                                                
                                                                                
* Validate estimate key values                                                  
                                                                                
VEKADD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VEKADD*'                                                      
                                                                                
         GOTOR VALCPJ              validate SJ account                          
         JNE   VEKADDN                                                          
                                                                                
         GOTOR MCSCPJ              check selected SJ is MCS user                
         JNE   VEKADDN                                                          
                                                                                
VEKADDY  J     EXITY                                                            
                                                                                
VEKADDN  J     EXITN                                                            
                                                                                
* Validate client/product/job                                                   
VALCPJ   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALCPJ*'                                                      
                                                                                
         OC    RQUGCLI(L'RQUGCLI+L'RQUGPRO+L'RQUGJOB),SPACES                    
         MVC   SJACCNT,SPACES                                                   
         MVC   XERRTXT,SPACES                                                   
                                                                                
         CLC   RQUGCLI,SPACES                                                   
         JH    VALCPJ02                                                         
         MVC   ROUERRV,=AL2(AE$MSCLI)                                           
         J     EXITN               client is required                           
                                                                                
VALCPJ02 XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   SJACCNT(0),RQUGCLI                                               
         EX    R1,0(RE)                                                         
         LA    R1,SJACCNT+1(R1)                                                 
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         XR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),RQUGPRO                                                  
         EX    RF,0(RE)                                                         
         LA    R1,1(RF,R1)                                                      
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         XR    RF,RF                                                            
         IC    RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         CHI   RF,6                                                             
         JNH   *+8                                                              
         LA    RF,6                                                             
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),RQUGJOB                                                  
         EX    RF,0(RE)                                                         
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         XR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),SJACCNT                                               
         EX    RF,0(RE)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALCPJ10                                                         
         MVC   ROUERRV,=AL2(AE$INCLI)                                           
         J     EXITN                                                            
                                                                                
VALCPJ10 TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VALCPJ12                                                         
         MVC   ROUERRV,=AL2(AE$CLILK)                                           
         J     EXITN                                                            
                                                                                
VALCPJ12 TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VALCPJ14                                                         
         MVC   ROUERRV,=AL2(AE$ACTCL)                                           
         J     EXITN                                                            
                                                                                
VALCPJ14 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
         MVC   X#SJOFF,SPACES                                                   
VALCPJ16 CLI   PPREL,0                                                          
         JE    VALCPJ30                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    VALCPJ20                                                         
         CLI   PPREL,RSTELQ                                                     
         JE    VALCPJ22                                                         
                                                                                
VALCPJ18 IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     VALCPJ16                                                         
                                                                                
VALCPJ20 MVC   X#SJOFF,PPRGAOFF                                                 
         OC    X#SJOFF,SPACES                                                   
         J     VALCPJ18                                                         
                                                                                
         USING RSTELD,R2                                                        
VALCPJ22 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALCPJ18                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
                                                                                
VALCPJ30 MVI   X#SJIND,1           CLIENT OK                                    
         CLC   RQUGPRO,SPACES      PROCESS PRODUCT                              
         JNH   VALCPJ74                                                         
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),SJACCNT                                               
         EX    RF,0(RE)                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALCPJ32                                                         
         MVC   ROUERRV,=AL2(AE$INPRO)                                           
         J     EXITN                                                            
                                                                                
VALCPJ32 TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VALCPJ34                                                         
         MVC   ROUERRV,=AL2(AE$ACTLK)                                           
         J     EXITN                                                            
                                                                                
VALCPJ34 TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VALCPJ36                                                         
         MVC   ROUERRV,=AL2(AE$ACTCL)                                           
         J     EXITN                                                            
                                                                                
VALCPJ36 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
VALCPJ38 CLI   PPREL,0                                                          
         JE    VALCPJ50                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    VALCPJ42                                                         
         CLI   PPREL,RSTELQ                                                     
         JE    VALCPJ44                                                         
                                                                                
VALCPJ40 IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     VALCPJ38                                                         
                                                                                
VALCPJ42 CLI   PPRGAOFF,X'40'                                                   
         JNH   VALCPJ40                                                         
         MVC   X#SJOFF,PPRGAOFF                                                 
         OC    X#SJOFF,SPACES                                                   
         J     VALCPJ40                                                         
                                                                                
         USING RSTELD,R2                                                        
VALCPJ44 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALCPJ40                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
                                                                                
VALCPJ50 MVI   X#SJIND,2           JOB OK                                       
         CLC   RQUGJOB,SPACES      PROCESS JOB                                  
         JNH   VALCPJ74                                                         
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKACT,SJACCNT                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
*&&UK*&& JE    VALCPJ52                                                         
*&&US*&& JE    VALCPJ54                                                         
         MVC   ROUERRV,=AL2(AE$INJOB)                                           
         J     EXITN                                                            
                                                                                
VALCPJ52 TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VALCPJ54                                                         
         MVC   ROUERRV,=AL2(AE$JOBLK)                                           
         J     EXITN                                                            
                                                                                
VALCPJ54 TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VALCPJ56                                                         
         MVC   ROUERRV,=AL2(AE$JOBCL)                                           
         J     EXITN                                                            
                                                                                
VALCPJ56 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
VALCPJ60 CLI   PPREL,0                                                          
         JE    VALCPJ72                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    VALCPJ64                                                         
         CLI   PPREL,RSTELQ                                                     
         JE    VALCPJ66                                                         
         CLI   PPREL,JOBELQ                                                     
         JE    VALCPJ68                                                         
                                                                                
VALCPJ62 IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     VALCPJ60                                                         
                                                                                
VALCPJ64 CLI   PPRGAOFF,X'40'                                                   
         JNH   VALCPJ62                                                         
         MVC   X#SJOFF,PPRGAOFF                                                 
         OC    X#SJOFF,SPACES                                                   
         J     VALCPJ62                                                         
                                                                                
         USING RSTELD,R2                                                        
VALCPJ66 CLI   RSTLN,RSTLN3Q                                                    
         JL    VALCPJ67                                                         
         TM    RSTLSTAT,RSTLSESQ   locked from estimates?                       
         JZ    VALCPJ67                                                         
         MVC   ROUERRV,=AL2(AE$JOBLK)                                           
         MVC   XERRTXT,PRODUL                                                   
         MVC   XERRTXT+2(12),SJACCNT                                            
         J     EXITN                                                            
                                                                                
VALCPJ67 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VALCPJ62                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
                                                                                
         USING JOBELD,R2                                                        
VALCPJ68 CLI   JOBLN,JOBLN3Q                                                    
         JL    VALCPJ70                                                         
         TM    JOBSTA1,JOBSMCSE                                                 
         JNZ   VALCPJ62                                                         
                                                                                
VALCPJ70 MVC   ROUERRV,=AL2(AE$NOMCS)                                           
         J     EXITN                                                            
                                                                                
VALCPJ72 MVI   X#SJIND,3           JOB OK                                       
                                                                                
VALCPJ74 CLI   OFFIND,FULLYQ       OFFICE CHECKING                              
         JNE   VALCPJ80                                                         
         CLC   X#SJOFF,SPACES                                                   
         JNE   VALCPJ78                                                         
         MVC   ROUERRV,=AL2(AE$INVPO)                                           
         J     EXITN                                                            
                                                                                
         USING OFFALD,R1                                                        
VALCPJ78 L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,X#SJOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              VALIDATE OFFICE                              
         JE    VALCPJ80                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
                                                                                
VALCPJ80 MVC   SJACLIC,RQUGCLI     SAVE VALUES                                  
         MVC   SJAPROC,RQUGPRO                                                  
         MVC   SJAJOBC(6),RQUGJOB                                               
         MVI   SJAJOBC+6,C' '                                                   
                                                                                
         J     EXITY                                                            
         DROP  R1,R2                                                            
                                                                                
* Check selected SJ account is MCS user                                         
MCSCPJ   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*MCSCPJ*'                                                      
                                                                                
         USING GOBLOCKD,RF                                                      
***      L     R0,AGOBLOCK         call GETOPT for job currency                 
         L     R0,AGOBLOCB         call GETOPT for job currency                 
         LA    R1,GOBLOCKX-GOBLOCK                                              
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
***      L     RF,AGOBLOCK                                                      
         L     RF,AGOBLOCB                                                      
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOSELCUL(1),CUXCPY                                               
         MVC   GOSELCUL+1(2),PRODUL                                             
         MVC   GOSELCLI,SPACES                                                  
*&&UK*&& MVC   GOSELCLI(5),SJACLIC                                              
*&&US*&& MVC   GOSELCLI(L'SJACLIC),SJACLIC                                      
         MVC   GOSELPRO,SPACES                                                  
*&&UK*&& MVC   GOSELPRO(2),SJAPROC                                              
*&&US*&& MVC   GOSELPRO(L'SJAPROC),SJAPROC                                      
         MVC   GOSELJOB,SJAJOBC                                                 
         CLI   GOSELJOB,C' '                                                    
         JH    *+10                                                             
         XC    GOSELJOB,GOSELJOB                                                
         MVI   GOWHICH,0                                                        
         MVC   GOABEXT,AJOBLOCK    UK USES 2ND EXTENSION BLOck                  
         GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
***      L     RF,AGOBLOCK                                                      
         L     RF,AGOBLOCB                                                      
         CLI   GOESTTYP,C'M'                                                    
         JE    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$NOMCS)                                           
         J     EXITY                                                            
         DROP  RF                                                               
                                                                                
* Get global estimate number                                                    
                                                                                
GETEGN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETEGN*'                                                      
                                                                                
         LA    R2,IOKEY            company based estimate number                
         MVI   BYTE1,0                                                          
                                                                                
         CLI   SCPYEL+CPYLN-CPYELD,CPYLN4Q                                      
         JL    GETEGN02                                                         
         TM    SCPYEL+CPYSTATB-CPYELD,CPYSENOB                                  
         JZ    GETEGN02                                                         
                                                                                
         USING OGRRECD,R2          production office group record               
         MVI   BYTE1,1                                                          
         XC    OGRKEY,OGRKEY       office based estimate number                 
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,CUXCPY                                                   
         MVC   OGRKUNT(2),PRODUL                                                
         MVC   OGRKOFC,X#SJOFF                                                  
         J     GETEGN04                                                         
                                                                                
         USING CPYRECD,R2                                                       
GETEGN02 MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CUXCPY                                                   
                                                                                
GETEGN04 L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    GETEGN08                                                         
                                                                                
GETEGN06 MVC   ROUERRV,=AL2(AE$ENONR)                                           
         J     EXITN                                                            
                                                                                
GETEGN08 L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING FFTELD,R3                                                        
         L     R2,AIO2                                                          
         LA    R3,CPYRFST                                                       
         XR    R0,R0                                                            
                                                                                
GETEGN10 CLI   FFTEL,0                                                          
         JE    GETEGN06                                                         
         CLI   FFTEL,FFTELQ                                                     
         JNE   GETEGN12                                                         
         CLI   FFTTYPE,FFTTESTN                                                 
         JE    GETEGN14                                                         
                                                                                
GETEGN12 IC    R0,FFTLN                                                         
         AR    R3,R0                                                            
         J     GETEGN10                                                         
                                                                                
GETEGN14 ZAP   X#COUNT,PZERO       do max of 50 ios in this loop                
         MVC   ES#ESTN,FFTDATA                                                  
         MVC   HALF1,FFTDATA       save office code (if any)                    
                                                                                
GETEGN16 GOTOR GNXTEGN             increment estimate number                    
         GOTOR LOOKUP              is incremented number already used?          
         JE    GETEGN20            no                                           
                                                                                
GETEGN18 AP    X#COUNT,PONE        yes, so move onto next one                   
         CP    X#COUNT,PMAX        (only check up to next 50 numbers)           
         JNH   GETEGN16                                                         
         MVC   ROUERRV,=AL2(AE$INAGN)                                           
         J     EXITN                                                            
                                                                                
GETEGN20 TM    SCPYEL+CPYSTATB-CPYELD,CPYSENOB                                  
         JZ    GETEGN22                                                         
         CLC   FFTDATA(6),ES#ESTN                                               
         JNH   GETEGN22                                                         
***      MVC   ROUERRV,=AL2(AE$ENONS)                                           
***      J     EXITN                                                            
                                                                                
GETEGN22 MVC   FFTDATA(6),ES#ESTN  save and write back number                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2,R3                                                            
                                                                                
* Get next estimate number                                                      
GNXTEGN  ST    RE,SAVERE                                                        
*&&US                                                                           
         CLI   BYTE1,0             office based?                                
         JE    GNXTE06             no                                           
         MVI   BYTE2,0                                                          
         XR    R1,R1                                                            
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JZ    GNXTE02                                                          
         AHI   R1,1                                                             
                                                                                
GNXTE02  BASR  RE,0                                                             
         MVC   ES#ESTN(0),=C'00'                                                
         EX    R1,0(RE)                                                         
         LA    RF,ES#ESTN+1(R1)    Point to possible A-F byte                   
         CLI   0(RF),C'9'                                                       
         JE    GNXTE04                                                          
         CLI   0(RF),C'A'          >>> bad data ...                             
         JL    *+2                                                              
         CLI   0(RF),C'F'                                                       
         JH    GNXTE06                                                          
                                                                                
GNXTE04  MVC   BYTE2,0(RF)         A-F scenario                                 
         CLI   0(RF),C'9'                                                       
         JE    GNXTE06                                                          
         MVI   0(RF),C'9'                                                       
                                                                                
GNXTE06  PACK  DUB1,ES#ESTN                                                     
         ZAP   DUB1,DUB1                                                        
         AP    DUB1,PONE                                                        
         UNPK  ES#ESTN,DUB1                                                     
         OI    ES#ESTN+L'ES#ESTN-1,X'F0'                                        
                                                                                
         CLI   BYTE1,0             office based?                                
         JE    GNXTEY              no                                           
         CLI   BYTE2,0                                                          
         JE    GNXTE14                                                          
         LLC   RE,BYTE2                                                         
         SHI   RF,1                                                             
         CLI   0(RF),C'0'          Overflow at A-F?                             
         JE    GNXTE12                                                          
         CLI   0(RF),C'1'                                                       
         JNE   *+2                 >>> die as my code must be crap              
         CHI   RE,C'9'                                                          
         JNE   GNXTE10                                                          
         LHI   RE,C'A'                                                          
         J     GNXTE12                                                          
                                                                                
GNXTE10  AHI   RE,1                (works for A-F and can be expanded           
         CHI   RE,C'F'             up to I for X'C1' to X'C9')                  
         JH    *+2                 >>> die if all used up ...                   
                                                                                
GNXTE12  AHI   RF,1                                                             
         STC   RE,0(RF)                                                         
                                                                                
GNXTE14  BASR  RE,0                                                             
         MVC   ES#ESTN(0),HALF1                                                 
         EX    R1,0(RE)                                                         
*&&                                                                             
*                                                                               
*&&UK                                                                           
         MVC   SV#ESTN,=C'000000'                                               
*                                                                               
         CLI   BYTE1,0             office based?                                
         JE    GNXTE02             no                                           
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JZ    GNXTE04             1 Byte office based                          
         MVC   SV#ESTN+2(L'ES#ESTN-2),ES#ESTN+2 2 Byte office based             
         J     GNXTE06                                                          
*                                                                               
GNXTE02  MVC   SV#ESTN,ES#ESTN     save 6 digit Estimate No.                    
         J     GNXTE06                                                          
*                                                                               
GNXTE04  MVC   SV#ESTN+1(L'ES#ESTN-1),ES#ESTN+1 save last 5 digits Est          
*                                                                               
GNXTE06  GOTO1 VHEXIN,DMCB,SV#ESTN,ESTN#HX,L'SV#ESTN                            
         XR    R1,R1                                                            
         ICM   R1,B'0111',ESTN#HX                                               
         AHI   R1,1                increment Current Estimate No. by 1          
         STCM  R1,B'0111',ESTN#HX  save new Estimate No.                        
         GOTO1 VHEXOUT,DMCB,ESTN#HX,SV#ESTN,L'ESTN#HX                           
*                                                                               
         CLI   BYTE1,0             office based?                                
         JE    GNXTE0A             no                                           
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JZ    GNXTE0C                                                          
*                                                                               
         CLI   SV#ESTN+1,C'0'      Check if Est No. are overflowing             
         JNE   INVESTNO                                                         
         MVC   ES#ESTN+2(L'ES#ESTN-2),SV#ESTN+2 save last 4 digits Est          
         J     GNXTEY                                                           
*                                                                               
INVESTNO MVC   ROUERRV,=AL2(AE$INAGN) Overflow error                            
         J     EXITN                                                            
*                                                                               
GNXTE0A  MVC   ES#ESTN,SV#ESTN     save 6 digit Estimate No.                    
         J     GNXTEY                                                           
*                                                                               
GNXTE0C  CLI   SV#ESTN,C'0'      Check if Est No. are overflowing               
         JNE   INVESTNO                                                         
         MVC   ES#ESTN+1(L'ES#ESTN-1),SV#ESTN+1 save last 5 digits Est          
*&&                                                                             
GNXTEY   L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
* Look up new estimate record not in use                                        
LOOKUP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*LOOKUP*'                                                      
                                                                                
         USING EGNPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,CUXCPY                                                   
         MVC   EGNPNUM,ES#ESTN                                                  
         MVC   CSVKEY1,EGNPAS                                                   
                                                                                
         L     R1,=AL4(IOHID+IODIR+IO3)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         CLC   CSVKEY1(EGNPCLI-EGNPASD),EGNPAS                                  
         JNE   EXITY                                                            
         MVC   ROUERRV,=AL2(AE$RECAE)                                           
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
* Get local estimate number (using gaps, too)                                   
GETELN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETELN*'                                                      
                                                                                
         XR    R3,R3                                                            
         XR    R4,R4                                                            
                                                                                
         USING ESTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    ESTKEY,ESTKEY                                                    
         MVI   ESTKTYP,ESTKTYPQ                                                 
         MVI   ESTKSUB,ESTKSUBQ                                                 
         MVC   ESTKCPY,CUXCPY                                                   
         MVC   ESTKCLI,SJACLIC                                                  
         MVC   ESTKPRO,SJAPROC                                                  
         MVC   ESTKJOB,SJAJOBC                                                  
                                                                                
         MVC   CSVKEY1,ESTKEY                                                   
                                                                                
GETELN1  L     R1,=AL4(IOHID+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    GETELN3                                                          
         CLI   IOERR,IOEDEL        skip deleted but be aware of them            
         JNE   *+2                                                              
                                                                                
GETELN3  CLC   CSVKEY1(ESTKLNO-ESTRECD),ESTKEY                                  
         JNE   GETELN9                                                          
         CLI   ESTKSEQ,ESTKSMQ                                                  
         JE    GETELN7                                                          
                                                                                
GETELN5  MVI   ESTKSEQ,FF          only interested in main records              
         IC    R4,ESTKLNO                                                       
         J     GETELN1                                                          
                                                                                
GETELN7  IC    R3,ESTKLNO          take latest number                           
         LR    RE,R3                                                            
         SR    RE,R4               is there a gap?                              
                                                                                
         J     GETELN5             go for highest plus 1                        
                                                                                
         CHI   RE,1                *                                            
         JNH   GETELN5             * then go for next                           
         AHI   R4,1                *                                            
         STC   R4,X#LOCNO          *                                            
         J     GETELNY             *                                            
                                                                                
GETELN9  CHI   R4,FE               all used (FF must remain unused)             
         JNL   GETELNN                                                          
                                                                                
         AHI   R4,1                                                             
         STC   R4,X#LOCNO                                                       
                                                                                
GETELNY  MVC   CSVKEY2,CSVKEY1                                                  
         MVC   CSVKEY2+ESTKLNO-ESTRECD(L'X#LOCNO),X#LOCNO                       
         J     EXITY                                                            
                                                                                
GETELNN  MVC   ROUERRV,=AL2(AE$AENIU)                                           
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
* Get estimate record via ES#ESTN                                               
GETEST   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETEST*'                                                      
                                                                                
         STC   R1,BYTE2                                                         
                                                                                
         USING EGNPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,CUXCPY                                                   
         MVC   EGNPNUM,ES#ESTN                                                  
                                                                                
         MVC   CSVKEY1,EGNPAS                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JNE   *+2                                                              
                                                                                
         CLC   CSVKEY1(EGNPCLI-EGNPASD),EGNPAS                                  
         JE    GETEST2                                                          
                                                                                
         MVC   ROUERRV,=AL2(AE$ESTNF)                                           
         J     EXITN                                                            
                                                                                
GETEST2  MVC   CSVKEY1,EGNPAS      save estimate passive                        
         MVC   X#LOCNO,EGNPLNO     save local number                            
                                                                                
         CLC   EGNPSOFF,SPACES     office set?                                  
         JNH   GETEST4                                                          
                                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,EGNPSOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         DROP  R1                                                               
         GOTO1 VOFFAL              validate office                              
         JE    GETEST4                                                          
                                                                                
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
                                                                                
GETEST4  CLI   BYTE2,0                                                          
         JE    EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JNE   *+2                                                              
                                                                                
         CLI   BYTE2,2                                                          
         JNE   GETEST8                                                          
         L     RF,AIO2             check for client approver set                
         USING EMDELD,RF                                                        
         AHI   RF,ESTRFST-ESTRECD                                               
         CLI   EMDEL,EMDELQ                                                     
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   X#ERAIS,EMDAPI                                                   
         MVC   X#ECLAP,EMDSCA                                                   
         MVC   X#EINAP,EMDSIA                                                   
         CLI   X#ACTION,X#ACTISQ   action is a status change                    
         JNE   GETEST8                                                          
         MVC   X#SAPPR,EMDSCA                                                   
         MVC   X#SIAPP,EMDSIA                                                   
         DROP  RF                                                               
                                                                                
         USING ESTRECD,R2                                                       
GETEST8  L     R2,AIO2                                                          
         MVC   CSVKEY2,ESTKEY      save estimate main key                       
                                                                                
         LA    R2,IOKEY            read main IS key for lock                    
         XC    IOKEY,IOKEY                                                      
         MVC   ESTKEY,CSVKEY2                                                   
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
* Test for any locked work codes (status change call)                           
TSTWCS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*TSTWCS*'                                                      
                                                                                
         USING ESTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ESTKEY,CSVKEY2                                                   
         MVI   ESTKSEQ,1           read details estimate records                
                                                                                
         L     R4,AIO7             buffer work codes                            
         MVI   0(R4),0                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
         J     TSTWCS04                                                         
                                                                                
TSTWCS02 LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
TSTWCS04 CLC   ESTKEY(ESTKSEQ-ESTRECD),CSVKEY2                                  
         JNE   TSTWCS12                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING ERDELD,R3                                                        
         L     R3,AIO1                                                          
         AHI   R3,ESTRFST-ESTRECD                                               
                                                                                
TSTWCS06 CLI   ERDEL,0                                                          
         JE    TSTWCS02                                                         
         CLI   ERDEL,ERDELQ                                                     
         JE    TSTWCS10                                                         
                                                                                
TSTWCS08 LLC   R0,ERDLN                                                         
         AR    R3,R0                                                            
         J     TSTWCS06                                                         
                                                                                
TSTWCS10 CLI   ERDTYP,ERDTWDQ      Work code data                               
         JNE   TSTWCS08                                                         
         MVC   0(L'ERDWCOD,R4),ERDWCOD                                          
         AHI   R4,L'ERDWCOD                                                     
         MVI   0(R4),0                                                          
         J     TSTWCS08                                                         
         DROP  R2,R3                                                            
                                                                                
         USING WCORECD,R2                                                       
TSTWCS12 L     R4,AIO7                                                          
                                                                                
TSTWCS14 CLI   0(R4),0                                                          
         JE    TSTWCSY                                                          
                                                                                
         LA    R2,IOKEY                                                         
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUXCPY                                                   
         MVC   WCOKUNT(2),PRODUL                                                
         MVC   WCOKWRK,0(R4)                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    TSTWCS16                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$WRKNF)                                           
         MVC   XERRTXT(L'ERDWCOD),0(R4)                                         
         J     EXITN                                                            
                                                                                
TSTWCS16 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING WCOELD,R3                                                        
         L     R3,AIO1                                                          
         AHI   R3,WCORFST-WCORECD                                               
                                                                                
TSTWCS18 CLI   WCOEL,0                                                          
         JE    TSTWCS22                                                         
         CLI   WCOEL,WCOELQ                                                     
         JE    TSTWCS20                                                         
         LLC   R0,WCOLN                                                         
         AR    R3,R0                                                            
         J     TSTWCS18                                                         
                                                                                
TSTWCS20 TM    WCOSTAT2,WCOSLEST                                                
         JZ    TSTWCS22                                                         
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'ERDWCOD),0(R4)                                         
         MVC   ROUERRV,=AL2(AE$WCLCK)                                           
         J     EXITN                                                            
                                                                                
TSTWCS22 AHI   R4,L'ERDWCOD                                                     
         J     TSTWCS14                                                         
         DROP  R2,R3                                                            
                                                                                
TSTWCSY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
                                                                                
* Validate user is approver/internal approver                                   
                                                                                
CHKAPP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKAPP*'                                                      
                                                                                
         MVC   BYTE2,0(R1)                                                      
         MVC   SJACCNT,SPACES                                                   
         CLC   RQUGCLI,SPACES                                                   
         JH    CHKAPP02                                                         
         MVC   ROUERRV,=AL2(AE$MSCLI)                                           
         J     EXITN               client is required                           
                                                                                
CHKAPP02 XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   SJACCNT(0),RQUGCLI                                               
         EX    R1,0(RE)                                                         
         LA    R1,SJACCNT+1(R1)                                                 
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         XR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),RQUGPRO                                                  
         EX    RF,0(RE)                                                         
         LA    R1,1(RF,R1)                                                      
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         XR    RF,RF                                                            
         IC    RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         CHI   RF,6                                                             
         JNH   *+8                                                              
         LA    RF,6                                                             
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),RQUGJOB                                                  
         EX    RF,0(RE)                                                         
                                                                                
         LA    R2,APRTAB                                                        
         USING APRTABD,R2                                                       
SJ       USING JOBPASD,IOKEY                                                    
CHKAPP04 XC    SJ.JOBPAS,SJ.JOBPAS                                              
         MVI   SJ.JOBPTYP,JOBPTYPQ                                              
         MVI   SJ.JOBPSUB,JOBPSUBQ                                              
         MVC   SJ.JOBPCPY,CUXCPY                                                
         MVI   SJ.JOBPVIEW,JOBPVOFF                                             
         MVI   SJ.JOBPAPPL,JOBPAEST                                             
         CLI   BYTE2,RQUGAIQ                                                    
         JNE   *+8                                                              
         MVI   SJ.JOBPAPPL,JOBPAESI                                             
         MVC   SJ.JOBPCOFF,SPACES                                               
         MVC   SJ.JOBPCMED,SPACES                                               
         MVC   SJ.JOBPCPJ,SPACES                                                
         MVC   SJ.JOBPPIDB,CCTPID                                               
                                                                                
CHKAPP06 TM    APRSTAT,APRJOB                                                   
         JZ    CHKAPP08                                                         
         LLC   RF,PPROLEN                                                       
         LA    RF,SJACCNT(RF)                                                   
         CLI   0(RF),X'40'         Have we got a product                        
         JNH   CHKAPP22            No                                           
         LLC   RF,PJOBLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),SJACCNT                                            
         EX    RF,0(RE)                                                         
         J     CHKAPP12                                                         
CHKAPP08 TM    APRSTAT,APRPRO                                                   
         JZ    CHKAPP10                                                         
         LLC   RF,PCLILEN                                                       
         LA    RF,SJACCNT(RF)                                                   
         CLI   0(RF),X'40'         Have we got a product                        
         JNH   CHKAPP22            No                                           
         LLC   RF,PPROLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),SJACCNT                                            
         EX    RF,0(RE)                                                         
         J     CHKAPP12                                                         
CHKAPP10 TM    APRSTAT,APRCLI                                                   
         JZ    CHKAPP12                                                         
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),SJACCNT                                            
         EX    RF,0(RE)                                                         
                                                                                
CHKAPP12 TM    APRSTAT,APRMED                                                   
         JZ    CHKAPP14                                                         
         CLC   RQUGMED,SPACES                                                   
*&&UK*&& JNH   CHKAPP22                                                         
*&&US                                                                           
         JH    CHKAPP13                                                         
         LLC   RF,PPROLEN                                                       
         LA    RE,SJACCNT(RF)                                                   
         CLI   0(RE),C' '                                                       
         JNH   CHKAPP22                                                         
         MVC   SJ.JOBPCMED,0(RE)                                                
         J     CHKAPP14                                                         
CHKAPP13 DS    0H                                                               
*&&                                                                             
         MVC   SJ.JOBPCMED,RQUGMED                                              
                                                                                
CHKAPP14 TM    APRSTAT,APROFF                                                   
         JZ    CHKAPP15                                                         
         CLC   X#SJOFF,SPACES                                                   
         JNH   CHKAPP22                                                         
         MVC   SJ.JOBPCOFF,X#SJOFF                                              
                                                                                
CHKAPP15 CLI   APRLVL,AGENCYQ      global approver?                             
         JNE   CHKAPP16                                                         
         MVI   SJ.JOBPCODE,FF                                                   
         MVC   SJ.JOBPCODE+1(L'JOBPCODE-1),SJ.JOBPCODE                          
                                                                                
CHKAPP16 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         J     CHKAPP20                                                         
                                                                                
CHKAPP18 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
                                                                                
CHKAPP20 CLC   SJ.JOBPAS(JOBPSEQ-JOBPASD),IOKEYSAV                              
         JE    EXITY                                                            
CHKAPP22 LA    R2,APRTABL(R2)                                                   
         CLI   0(R2),X'FF'                                                      
         JNE   CHKAPP04                                                         
                                                                                
CHKAPPN  MVC   ROUERRV,=AL2(AE$INAPP)                                           
         CLI   BYTE2,RQUGAIQ                                                    
         JNE   EXITN                                                            
         MVC   ROUERRV,=AL2(AE$INIAP)                                           
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
* Get record into AIO2                                                          
         USING ESTRECD,R2                                                       
GETREC   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETREC*'                                                      
                                                                                
         LA    R2,IOKEY                                                         
         MVC   ESTKEY,CSVKEY2      read for record                              
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   X#IOADR,ESTKDA                                                   
         MVC   CSVKEY2,ESTKEY                                                   
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
* Save record to file                                                           
         USING ESTRECD,R2                                                       
SAVREC   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SAVREC*'                                                      
                                                                                
         L     R2,AIO2                                                          
                                                                                
         CLI   BYTE2,YESQ          flag set to add records?                     
         JE    SAVREC10                                                         
                                                                                
         CLI   X#ACTION,RQUGACQ    on create need to add only                   
         JNE   SAVREC20                                                         
                                                                                
SAVREC10 GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   X#IOADR,IODA        save disk address                            
                                                                                
         J     SAVREC80                                                         
                                                                                
SAVREC20 DS    0H                  on maintain                                  
                                                                                
         L     R0,AIO5             copy record to AIO5                          
         LA    R1,IOLENQ                                                        
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'EXCKEY),ESTRECD                                          
                                                                                
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    SAVREC30                                                         
         CLI   IOERR,IOEDEL                                                     
         JE    SAVREC30                                                         
                                                                                
         MVI   BYTE2,YESQ          set to add records                           
                                                                                
         J     SAVREC10                                                         
                                                                                
SAVREC30 DS    0H                  update existing records                      
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   X#IOADR,IODA        save disk address                            
                                                                                
         L     R0,AIO2             and restore back to AIO2                     
         LA    R1,IOLENQ                                                        
         L     RE,AIO5                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    RE,IOKEY            set new status                               
         MVC   ESTKSTA-ESTRECD(L'ESTKSTA,RE),ESTRSTA                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    SAVREC80                                                         
         DC    H'0'                                                             
                                                                                
SAVREC80 L     R2,AIO2                                                          
         LLC   R3,ESTKSEQ                                                       
         CHI   R3,100              stop at 100 records                          
         JL    *+6                                                              
         DC    H'0'                                                             
         AHI   R3,1                R3=new sequence number                       
                                                                                
         LR    R0,R2               Clear IO area 2                              
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   ESTKEY,CSVKEY2      set main record key and dummy length         
         MVC   ESTRLEN,=Y(ESTRFST-ESTKEY+1)                                     
         STC   R3,ESTKSEQ                                                       
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
* Validate scheme code                                                          
         USING SCHRECD,R2                                                       
VALSCH   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALSCH*'                                                      
                                                                                
         OC    X#SCHCD,SPACES                                                   
                                                                                
         LA    R2,IOKEY                                                         
         XC    SCHKEY,SCHKEY                                                    
         MVI   SCHKTYP,SCHKTYPQ                                                 
         MVI   SCHKSUB,SCHKSUBQ                                                 
         MVC   SCHKCPY,CUXCPY                                                   
         MVC   SCHKUNT(2),PRODUL                                                
         MVC   SCHKCODE,X#SCHCD                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   VALSCH2                                                          
                                                                                
         TM    SCHKSTA,SCHSMCSN    exclude non MCS schemes                      
         JZ    EXITY                                                            
                                                                                
VALSCH2  MVC   ROUERRV,=AL2(AE$INSCH)                                           
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
* Build name element                                                            
                                                                                
         USING ENMELD,R3                                                        
BLDNAME  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDNAM*'                                                      
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ENMEL,ENMELQ                                                     
         LA    R1,ENMLNQ                                                        
                                                                                
         LA    RE,RQUGDES+L'RQUGDES-1                                           
         LA    RF,L'RQUGDES                                                     
                                                                                
BLDNA10  CLI   0(RE),C' '                                                       
         JH    BLDNA20                                                          
         SHI   RE,1                                                             
         JCT   RF,BLDNA10                                                       
         DC    H'0'                                                             
                                                                                
BLDNA20  CHI   RF,L'ENMNAME        ensure 50 chars for name max                 
         JH    *+2                                                              
         AR    R1,RF                                                            
         STC   R1,ENMLN                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ENMNAME(0),RQUGDES                                               
         EX    RF,0(RE)                                                         
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
* Build general date element for expiry date                                    
                                                                                
         USING GDAELD,R3                                                        
         USING ESTRECD,R2                                                       
BLDGDAE  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDGDAE'                                                      
         STCM  R1,1,X#BYTE1                                                     
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDAGAPEX                                                 
         MVC   GDADATE,RQGAPED                                                  
         MVC   X#ESEXDT,GDADATE                                                 
         GOTOR PUTSAR,NEWBUF                                                    
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
         DROP  R2,R3                                                            
                                                                                
* Build general date element for sent date                                      
                                                                                
         USING ESTRECD,R2                                                       
         USING GDAELD,R3                                                        
BLDGDAS  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDGDAS'                                                      
                                                                                
         STCM  R1,1,X#BYTE1                                                     
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDAGAPST                                                 
         MVC   GDADATE,SVTODAYP                                                 
         GOTOR PUTSAR,NEWBUF                                                    
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
         DROP  R2,R3                                                            
                                                                                
* Build free form text element for email addresses                              
                                                                                
         USING ESTRECD,R2                                                       
         USING FFTELD,R3                                                        
BLDEMAL  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDEMAL'                                                      
                                                                                
         STCM  R1,1,X#BYTE1                                                     
         LA    R3,ELEMENT                                                       
         XR    RF,RF               Point to list of email addresses             
         ICM   RF,7,RQGAPEM          in WMP                                     
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RF) # of array elements                        
         JZ    EXITY                                                            
         LA    RE,LW_DATA2-LW_D(RF)  start of data                              
         MVI   BYTE2,0                                                          
*                                                                               
BLDEML02 XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ                                                     
*&&UK*&& MVI   FFTTYPE,FFTTPEML                                                 
*&&US*&& MVI   FFTTYPE,FFTTEML                                                  
         LHI   RF,L'FFTEML1                                                     
         STC   RF,FFTDLEN                                                       
         LA    R4,FFTEML1                                                       
         LA    R1,FFTEML5                                                       
         MVC   FFTSEQ,BYTE2                                                     
*                                                                               
BLDEML04 CR    R4,R1               Hit end of element?                          
         JH    BLDEML06                                                         
         MVC   0(L'FFTEML1,R4),0(RE)                                            
         AHI   R4,L'FFTEML1                                                     
         AHI   RE,L'FFTEML1                                                     
         JCT   R0,BLDEML04                                                      
                                                                                
BLDEML06 SR    R4,R3                                                            
         STC   R4,FFTLN                                                         
         ST    R0,SAVER0                                                        
         ST    RE,SAVERE                                                        
         GOTOR PUTSAR,NEWBUF                                                    
         JNE   EXITN                                                            
                                                                                
BLDEML10 LLC   RF,BYTE2            bump sequence no.                            
         AHI   RF,1                                                             
         STC   RF,BYTE2                                                         
         L     R0,SAVER0                                                        
         L     RE,SAVERE                                                        
         LTR   R0,R0               Any more elements to do?                     
         JNZ   BLDEML02                                                         
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
* Check job for being draft                                                     
                                                                                
         USING ESTRECD,R2                                                       
         USING ACTRECD,R3                                                       
CHKJOB   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKJOB*'                                                      
                                                                                
         L     R2,AIO2             point to estimate record                     
                                                                                
         LA    R3,IOKEY            read account record                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(2),PRODUL                                                
         XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),ESTKCLI                                               
         EX    R1,0(RE)                                                         
         LA    R1,ACTKACT+1(R1)                                                 
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         LLC   RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),ESTKPRO                                                  
         EX    RF,0(RE)                                                         
         LA    R1,1(RF,R1)                                                      
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         XR    RF,RF                                                            
         IC    RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         CHI   RF,6                                                             
         JNH   *+8                                                              
         LA    RF,6                                                             
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),ESTKJOB                                                  
         EX    RF,0(RE)                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   CHKJOB2             should exist ...                             
                                                                                
*&&UK*&& TM    ACTKSTAT,ACTSLOCK   and check for status' OK                     
*&&UK*&& JNZ   CHKJOB4                                                          
         TM    ACTKSTAT,ACTSCLOS                                                
         JNZ   CHKJOB6                                                          
         TM    ACTKSTAT,ACTSDRFT                                                
         JNZ   CHKJOB8                                                          
         J     EXITY                                                            
                                                                                
CHKJOB2  MVC   ROUERRV,=AL2(AE$INJOB)                                           
         J     EXITN                                                            
                                                                                
CHKJOB4  MVC   ROUERRV,=AL2(AE$JOBLK)                                           
         J     EXITN                                                            
                                                                                
CHKJOB6  MVC   ROUERRV,=AL2(AE$JOBCL)                                           
         J     EXITN                                                            
                                                                                
CHKJOB8  MVC   ROUERRV,=AL2(AE$ACCNA)                                           
         J     EXITN                                                            
         DROP  R2,R3                                                            
                                                                                
* Validate media code for ballpark                                              
         USING PMDRECD,R2                                                       
VALBPM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALBPM*'                                                      
                                                                                
         CLC   RQUGJOB,SPACES                                                   
         JNH   VALBPM2                                                          
         MVI   X#BPMED,C' '                                                     
         J     EXITY                                                            
                                                                                
VALBPM2  LA    R2,IOKEY                                                         
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUXCPY                                                   
         MVC   PMDKMED,X#BPMED                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    EXITY                                                            
                                                                                
         MVC   ROUERRV,=AL2(AE$MCINV)                                           
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
* Validate supposed internal approver and approver                              
VALAPP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALAPP*'                                                      
                                                                                
         CLC   RQUGAPP,SPACES      SUPPOSED CLIENT APPROVER                     
         JNH   VALAPP4                                                          
         MVC   TEMP2(8),RQUGAPP                                                 
         OC    TEMP2(8),SPACES                                                  
         GOTOR (#GETPIN,AGETPIN)   GET PERSON NAMES                             
         OC    TEMP2+50(2),TEMP2+50                                             
         JNZ   VALAPP2                                                          
                                                                                
         MVC   ROUERRV,=AL2(AE$INAPP)                                           
         J     EXITN                                                            
                                                                                
VALAPP2  MVC   X#SAPPR,TEMP2+50                                                 
                                                                                
VALAPP4  CLC   RQUGIAP,SPACES      SUPPOSED INTERNAL APPROVER                   
         JNH   VALAPP8                                                          
         MVC   TEMP2(8),RQUGIAP                                                 
         OC    TEMP2(8),SPACES                                                  
         GOTOR (#GETPIN,AGETPIN)   GET PERSON NAMES                             
         OC    TEMP2+50(2),TEMP2+50                                             
         JNZ   VALAPP6                                                          
                                                                                
         MVC   ROUERRV,=AL2(AE$INIAP)                                           
         J     EXITN                                                            
                                                                                
VALAPP6  MVC   X#SIAPP,TEMP2+50                                                 
                                                                                
VALAPP8  DS    0H                                                               
         J     EXITY                                                            
                                                                                
* Put record to TSAR                                                            
         USING ZTSARECD,R2                                                      
         USING EMDELD,R3                                                        
         USING ELSRTABD,R4                                                      
PUTSAR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PUTSAR*'                                                      
                                                                                
         STCM  R1,1,BYTE1          Buffer type old/new                          
                                                                                
         L     R2,AIO1             IO1 used for TSAR (clear it)                 
         LR    R0,R2                                                            
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   BYTE1,OLDBUF        Buffer 2?  Already have A(EMDELD)            
         JE    *+8                                                              
         LA    R3,ELEMENT          point to data                                
                                                                                
         CLI   EMDEL,0             ensure good element                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   EMDLN,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   EMDEL,EMDELQ        these elements should be on main rec         
         JE    PUTS00              so will have sequence of 00                  
         CLI   EMDEL,ENMELQ        Estimate name                                
         JE    PUTS00                                                           
         CLI   EMDEL,STCELQ        Approver comments                            
         JE    PUTS00                                                           
         CLI   EMDEL,GDAELQ        electronic signature expiry/sent dte         
         JE    PUTS00                                                           
         CLI   EMDEL,FFTELQ        electronic signature email addresses         
         JE    PUTS00                                                           
         CLI   EMDEL,PIDELQ        Approval details                             
         JE    PUTS00                                                           
         CLI   EMDEL,EMSELQ        estimate source - copy                       
         JE    PUTS00                                                           
         MVI   X#NODATA,NOQ        found detail data                            
                                                                                
PUTS00   XC    ZTRKEY,ZTRKEY                                                    
         LA    R4,ELSRTAB                                                       
*                                                                               
PUTS02   CLI   0(R4),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                No match found!                              
         CLC   ELSRTC,EMDEL        Match on sort code                           
         JNE   PUTS04                                                           
         CLI   EMDEL,ERDELQ        Any subcode (only for ERDELDs)               
         JNE   PUTS06                                                           
         CLC   ELSUBC,ERDTYP-ERDELD(R3)   Match on subcode                      
         JE    PUTS06                                                           
*                                                                               
PUTS04   LA    R4,ELSRTABL(R4)                                                  
         J     PUTS02                                                           
*                                                                               
PUTS06   MVC   ZTRKELS,ELSRTO      Found a match                                
         CLI   ELSQYN,YESQ         Do we want the sequence?                     
         JNE   PUTS10                                                           
         OC    X#SVELC,X#SVELC     Check same element as before                 
         JZ    PUTS08                                                           
         CLC   X#SVELC,EMDEL                                                    
         JE    PUTS08                                                           
         CLI   EMDEL,PTAELQ        For PTAELDs use separate sequence            
         JE    PUTS08                                                           
         XC    X#TINUM,X#TINUM                                                  
*                                                                               
PUTS08   MVC   X#SVELC,EMDEL                                                    
         XR    RF,RF                                                            
         ICM   RF,1,ELSRTCD        Displace to element code                     
         JZ    PUTS09                   that we can use to sort                 
         AR    RF,R3                                                            
         XR    RE,RE                                                            
         ICM   RE,1,ELSRTCDL                                                    
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   ZTRKSEQ(0),0(RF)                                                 
         EX    RE,0(R1)                                                         
         J     PUTS10                                                           
*                                                                               
PUTS09   CLI   EMDEL,PTAELQ        For PTAELD and FADELD use separate           
*&&UK                                                                           
         JE    *+12                sequence number                              
         CLI   EMDEL,FADELQ                                                     
*&&                                                                             
         JNE   PUTS09A                                                          
         LLH   RF,X#PTNUM          Increment buffer 1 sequence                  
         AHI   RF,1                                                             
         STH   RF,X#PTNUM                                                       
         MVC   ZTRKSEQ,X#PTNUM                                                  
         MVC   ZTRKCSQ(ZTRKEYL-(ZTRKCSQ-ZTRKEY)),EFFS                           
         J     PUTS10                                                           
*                                                                               
PUTS09A  LLH   RF,X#TINUM          Increment buffer 1 sequence                  
         AHI   RF,1                                                             
         STH   RF,X#TINUM                                                       
         MVC   ZTRKSEQ,X#TINUM                                                  
*                                                                               
PUTS10   OC    ELSUBS,ELSUBS       Element with subtype?                        
         JZ    PUTS20                                                           
         MVC   ZTRKSSQ,ELSUBS                                                   
*                                                                               
         CLI   ERDTYP-ERDELD(R3),ERDTCSQ                                        
         JNE   PUTS12              Category type?                               
         LLC   RF,X#CATSQ          Increment category                           
         AHI   RF,1                                                             
         STC   RF,X#CATSQ                                                       
         XC    X#WCSEQ,X#WCSEQ                                                  
         XC    X#SUBSQ,X#SUBSQ                                                  
*                                                                               
PUTS12   CLI   ERDTYP-ERDELD(R3),ERDTWDQ                                        
         JNE   PUTS14              Workcode type?                               
         LLH   RF,X#WCSEQ          Increment category                           
         AHI   RF,1                                                             
         STH   RF,X#WCSEQ                                                       
         XC    X#ITESQ,X#ITESQ     Clear item sequence                          
         XC    X#SUBSQ,X#SUBSQ                                                  
*                                                                               
PUTS14   CLI   ERDTYP-ERDELD(R3),ERDTIRQ Time item record                       
         JE    *+12                                                             
         CLI   ERDTYP-ERDELD(R3),ERDTIDQ Item record                            
         JNE   PUTS16                                                           
         LLC   RF,X#ITESQ                                                       
         AHI   RF,1                                                             
         STC   RF,X#ITESQ                                                       
*                                                                               
PUTS16   OC    X#SVSUB,X#SVSUB     Check same subelement code as before         
         JZ    PUTS18                                                           
         CLC   X#SVSUB,ELSUBS                                                   
         JE    PUTS18                                                           
         XC    X#SUBSQ,X#SUBSQ                                                  
*                                                                               
PUTS18   MVC   X#SVSUB,ELSUBS                                                   
         LLH   RF,X#SUBSQ                                                       
         AHI   RF,1                                                             
         STH   RF,X#SUBSQ                                                       
         MVC   ZTRKSUS,X#SUBSQ     element sub sequence                         
                                                                                
         USING EMDELD,R3                                                        
PUTS20   LLC   R1,EMDLN            determine length                             
         LR    RF,R1                                                            
         AHI   R1,ZTRKEYL                                                       
         AHI   R1,ZTRDLEL                                                       
         AHI   R1,L'ZTRLEN                                                      
         STCM  R1,3,ZTRLEN         and set it                                   
                                                                                
         SHI   RF,1                pass element as data                         
         BASR  RE,0                                                             
         MVC   ZTRDELM(0),EMDELD                                                
         EX    RF,0(RE)                                                         
                                                                                
         USING ERDELD,R3           Sequencing for w/c + catg + item             
         CLI   ERDEL,ERDELQ                                                     
         JNE   PUTS24                                                           
         CLI   ERDTYP,ERDTHTQ      Ignore header text and footer text           
         JE    PUTS24                                                           
         CLI   ERDTYP,ERDTFTQ                                                   
         JE    PUTS24                                                           
         CLI   ERDTYP,ERDTETQ                                                   
         JE    PUTS24                                                           
*                                                                               
         MVC   ZTRKCSQ,X#CATSQ     Category sequence                            
         CLI   ERDTYP-ERDELD(R3),ERDTCSQ                                        
         JE    PUTS24                                                           
         CLI   ERDTYP-ERDELD(R3),ERDTCTQ                                        
         JE    PUTS24                                                           
*                                                                               
         MVC   ZTRKWUS,X#WCSEQ     For BrandO use calculated seq                
         MVC   ZTRKWCS,X#WCSEQ     Workcode sequence                            
                                                                                
         OC    X#UPWCSQ,X#UPWCSQ                                                
         JZ    PUTS22                                                           
                                                                                
         MVC   ZTRKWCS,X#UPWCSQ    For aura use passed seq                      
*                                                                               
PUTS22   CLI   ERDTYP-ERDELD(R3),ERDTWDQ                                        
         JE    PUTS24                                                           
         CLI   ERDTYP-ERDELD(R3),ERDTWTQ                                        
         JE    PUTS24                                                           
*                                                                               
         MVC   ZTRKITS,X#ITESQ                                                  
         MVC   ZTRKIUS,X#ITESQ     item sequence                                
                                                                                
         OC    X#ISEON,X#ISEON                                                  
         JZ    PUTS24                                                           
                                                                                
         MVC   ZTRKITS,X#ISEON     item sequence                                
*                                                                               
PUTS24   CLI   BYTE1,NEWBUF        Old or new buffer?                           
         JE    PUTS26                                                           
         GOTOR BUFELM,DMCB,('TSAADD',OLDBUF),ZTRLEN                             
         J     PUTS28                                                           
*                                                                               
PUTS26   GOTOR BUFELM,DMCB,('TSAADD',NEWBUF),ZTRLEN                             
*                                                                               
PUTS28   CLI   TSARERRS,0                                                       
         JE    EXITY                                                            
         OI    TWAMODE,TWAMEDP     stop processing here                         
         MVC   XERRTXT,SPACES                                                   
         GOTOR PUTERR,AE$ESTTB                                                  
         J     EXITN                                                            
         DROP  R2,R3                                                            
***********************************************************************         
* READ AUDIT RECORD INTO AIO3                                         *         
***********************************************************************         
         USING AUDRECD,R2                                                       
         USING ESTRECD,R4                                                       
AUD      USING STCELD,R3                                                        
BLDAUD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDAUD*'                                                      
         L     R4,AIO2                                                          
         L     R3,AELEAREA                                                      
*                                                                               
         XC    BYTE1,BYTE1                                                      
         MVI   BYTE2,NOQ                                                        
*                                                                               
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,ESTKCPY                                                  
         MVI   AUDKAUDT,AUDKEST                                                 
*                                                                               
         LA    R5,AUDKECPJ                                                      
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,R5),ESTKCLI                                                  
         EX    RF,0(R1)                                                         
         AHI   RF,1                                                             
         AR    R5,RF                                                            
         LLC   RE,PPROLEN                                                       
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,R5),ESTKPRO                                                  
         EX    RE,0(R1)                                                         
         AHI   RE,1                                                             
         AR    R5,RE                                                            
         LHI   RF,L'ESTKJOB-1                                                   
         BASR  R1,0                                                             
         MVC   0(0,R5),ESTKJOB                                                  
         EX    RF,0(R1)                                                         
         OC    AUDKECPJ,SPACES                                                  
         MVC   AUDKELNO,ESTKLNO                                                 
         MVC   CSVKEY1,AUDKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   BLDAUD03                                                         
*                                                                               
BLDAUD02 LA    R2,IOKEY                                                         
         MVC   IOKEY,CSVKEY1                                                    
         LLC   RF,BYTE1            Increment audit sequence                     
         AHI   RF,1                                                             
         STC   RF,BYTE1                                                         
         STC   RF,AUDKSEQ                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    BLDAUD02            record found - read for next                 
         MVC   IOKEY,CSVKEY1                                                    
         LLC   RF,BYTE1            Increment audit sequence                     
         SHI   RF,1                                                             
         STC   RF,BYTE1                                                         
         STC   RF,AUDKSEQ                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    BLDAUD04                                                         
         DC    H'0'                                                             
*                                                                               
BLDAUD03 GOTOR (#CLRIO,ACLRIO),DMCB,AIO3  Clear IO3 to read XDFRECs             
         L     R2,AIO3             clear aio area and add new audit             
         MVC   AUDKEY,CSVKEY1                                                   
         LHI   RF,AUDRFST-AUDRECD                                               
         STH   RF,AUDRLEN                                                       
*        MVC   AUDRSTAT,ESTRSTA                                                 
*        MVC   AUDRSTA2,ESTRSTA2                                                
         MVC   AUDKSEQ,BYTE1                                                    
         MVI   BYTE2,YESQ                                                       
         J     BLDAUD06                                                         
*                                                                               
BLDAUD04 MVC   BYTE1,AUDKSEQ       Save current sequence number                 
         L     R1,=AL4(IOGETRUP+IOMST+IO3)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
N        USING CPTRBLK,ELEMENT                                                  
         XC    N.CPTRBLK(CPTRBLKL),N.CPTRBLK     delete passives                
         MVC   N.LDGLVALN(4),ONERL1L                                            
         GOTO1 VPADDLE,DMCB,(C'D',AIO3),(C'K',N.CPTRBLK),0,0,ACOMFACS           
         DROP  N                                                                
*                                                                               
         L     R2,AIO3                                                          
*        MVC   AUDRSTAT,ESTRSTA    Set new estimate status                      
*        MVC   AUDRSTA2,ESTRSTA2                                                
*                                                                               
BLDAUD06 CLI   AUD.STCEL,0         Any more stcelds?                            
         JE    BLDAUD14                                                         
         GOTOR TSTFIT              will it fit?                                 
         JNL   BLDAUD08                                                         
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,AUD.STCELD,(RF)                
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LLC   RF,AUD.STCLN         Bump to next element                        
         AR    R3,RF                                                            
         J     BLDAUD06                                                         
*                                                                               
BLDAUD08 GOTOR SETDTE              Set low and high date on audit rec           
         CLI   BYTE2,YESQ          Are we adding a new record?                  
         JNE   BLDAUD10                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO3'                           
         JE    BLDAUD11                                                         
         DC    H'0'                                                             
*                                                                               
BLDAUD10 GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3             Update directory status too                  
         LA    R2,IOKEY                                                         
         MVC   AUDKSTA,AUDRSTA-AUDRECD(RF)                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO3'                            
         JE    BLDAUD11                                                         
         DC    H'0'                                                             
*                                                                               
N        USING CPTRBLK,ELEMENT                                                  
BLDAUD11 XC    N.CPTRBLK(CPTRBLKL),N.CPTRBLK     add new passives               
         MVC   N.LDGLVALN(4),ONERL1L                                            
         GOTO1 VPADDLE,DMCB,(C'A',AIO3),N.CPTRBLK,IODA,0,ACOMFACS               
         DROP  N                                                                
*                                  Read next audit record                       
BLDAUD12 MVI   BYTE2,NOQ                                                        
         LA    R2,IOKEY                                                         
         MVC   IOKEY,CSVKEY1                                                    
         LLC   RF,BYTE1            Increment audit sequence                     
         AHI   RF,1                                                             
         STC   RF,BYTE1                                                         
         STC   RF,AUDKSEQ                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    BLDAUD04                                                         
         J     BLDAUD03                                                         
*                                                                               
BLDAUD14 GOTOR SETDTE              Set low and high date on audit rec           
         CLI   BYTE2,YESQ          Are we adding a new record?                  
         JNE   BLDAUD16                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO3'                           
         JE    BLDAUD18                                                         
         DC    H'0'                                                             
*                                                                               
BLDAUD16 GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3             Update directory status too                  
         LA    R2,IOKEY                                                         
         MVC   AUDKSTA,AUDRSTA-AUDRECD(RF)                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO3'                            
         JE    BLDAUD18                                                         
         DC    H'0'                                                             
*                                                                               
N        USING CPTRBLK,ELEMENT                                                  
BLDAUD18 XC    N.CPTRBLK(CPTRBLKL),N.CPTRBLK     add new passives               
         MVC   N.LDGLVALN(4),ONERL1L                                            
         GOTO1 VPADDLE,DMCB,(C'A',AIO3),N.CPTRBLK,IODA,0,ACOMFACS               
         DROP  N                                                                
*                                                                               
BLDAUDX  J     EXITY                                                            
*                                                                               
TSTFIT   SR    R0,R0           *** Test element fits on audit record **         
         ICM   R0,3,AUDRLEN                                                     
         LLC   RF,AUD.STCLN                                                     
         AR    R0,RF                                                            
         CHI   R0,MAXRECLN         Set condition code                           
         BR    RE                                                               
*                              *** Set dates on audit record ***                
SETDTE   XC    FULL2,FULL2                                                      
         MVC   FULL1,EFFS                                                       
         SR    R0,R0                                                            
         LA    RF,AUDRFST                                                       
         USING STCELD,RF                                                        
SETDTE02 CLI   STCEL,0                                                          
         JE    SETDTE20                                                         
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
         CLI   STCIND,STCIEST2     new estimate audit elements                  
         JNE   SETDTE10                                                         
         CLI   STCETY2,STCESTA     Status change                                
         JNE   SETDTE04                                                         
         CLI   STCEPES,OINPROG                                                  
         JE    SETDTE08                                                         
         CLI   STCEPES,OINTAPP                                                  
         JNE   SETDTE04                                                         
SETDTE08 CLI   STCECES,OSUBINT                                                  
         JNE   *+10                                                             
         MVC   X#SUBIN,STCPERS                                                  
         CLI   STCECES,OSUBCLI                                                  
         JNE   SETDTE04                                                         
         MVC   X#SUBCL,STCPERS                                                  
         J     SETDTE04                                                         
                                                                                
SETDTE10 CLI   STCIND,STCIEST      Old estimate audit elements                  
         JNE   SETDTE04                                                         
         CLI   STCDFR,C'C'         Status was originally created                
         JNE   SETDTE12            No                                           
         TM    G#OFSTA2,OFFSIAEQ   Agency have internal approval                
         JZ    SETDTE14            No                                           
         CLI   STCDTO,C'S'         Yes - so submit is to internal app           
         JNE   SETDTE04                                                         
         MVC   X#SUBIN,STCPERS     Capture person                               
SETDTE12 CLI   STCDFR,C'I'         If status was internally approved            
         JNE   SETDTE04             and it went to submitted must be            
SETDTE14 CLI   STCDTO,C'S'           for client approval                        
         JNE   SETDTE04                                                         
         MVC   X#SUBCL,STCPERS                                                  
         J     SETDTE04                                                         
                                                                                
SETDTE20 LR    R7,RE                                                            
         GOTOR VDATCON,DMCB,(1,FULL1),(2,AUDRSTDT)                              
         GOTOR VDATCON,DMCB,(1,FULL2),(2,AUDRENDT)                              
         LR    RE,R7                                                            
         BR    RE                                                               
         DROP  R2,AUD,R4,RF                                                     
***********************************************************************         
* Build STC elements in AELEAREA                                      *         
***********************************************************************         
         USING ESTRECD,R2                                                       
         USING STCELD,R4                                                        
BLDSTC   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDSTC*'                                                      
*                                                                               
         L     R2,AIO2                                                          
         L     R4,AELEAREA                                                      
         MVI   X#SVTYP,X'FF'       Set to initial value                         
*                                                                               
         L     R0,AELEAREA                                                      
         LHI   R1,LELEAREA                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*** Aura new audit code ***                                                     
*                                                                               
BLDST046 CLI   X#ACTION,X#ACTISQ   Action is status change only?                
         JE    BLDST176            If not just status change                    
*                                                                               
         CLI   X#ACTION,RQUGACQ    Create action:                               
         JNE   BLDST076                                                         
*                                                                               
         MVI   BYTE3,STCEADD       All new elms so build from new buff          
*                                  Build estimate added stceld                  
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEAEA     Build created element                        
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
*                                                                               
         L     R3,AFREEST          source buffer                                
         CLI   0(R3),X'40'         Any copied estimate                          
         JNH   BLDST048            No                                           
*                                                                               
         GOTOR BLDBST              Build merge element                          
         MVI   STCETY2,STCECOP     Copy                                         
         MVI   STCLN,STCELN3Q                                                   
         MVC   STCEMRGN,0(R3)                                                   
         MVI   STCEMRPS,0                                                       
         AHI   R4,STCELN3Q                                                      
*                                                                               
NEW      USING ZTSARECD,R5                                                      
OLD      USING ZTSARECD,R3                                                      
BLDST048 L     R5,AIO3             R5=A(new element TSAR buffer)                
         XC    NEW.ZTRLEN,NEW.ZTRLEN                                            
         XC    NEW.ZTRKEY(ZTRKEYL),NEW.ZTRKEY                                   
         XC    NEW.ZTRDELM,NEW.ZTRDELM                                          
         GOTOR BUFELM,DMCB,('TSARDH',NSRBUF),AIO3                               
         J     BLDST052                                                         
*                                                                               
BLDST050 GOTOR BUFELM,DMCB,('TSANXT',NSRBUF),AIO3                               
*                                                                               
BLDST052 TM    TSARERRS,TSEEOF                                                  
         JNZ   BLDSTCX                                                          
         LA    RF,NEW.ZTRDELM                                                   
         CLI   0(RF),EMDELQ        Main data element                            
         JE    BLDST053                                                         
         CLI   0(RF),ENMELQ        Estimate name                                
         JE    BLDST056                                                         
         CLI   0(RF),ERDELQ        Estimate w/c item category                   
         JE    BLDST062                                                         
         CLI   0(RF),XDFELQ        Xdata                                        
         JE    BLDST074                                                         
         J     BLDST050            Ignore other elements                        
*                                                                               
NEWE     USING EMDELD,NEW.ZTRDELM                                               
BLDST053 GOTOR BLDBST              Date added                                   
         MVI   STCETY2,STCEESD                                                  
         MVI   STCLN,STCELN5Q                                                   
         MVC   STCEDAT,NEWE.EMDDAT                                              
         AHI   R4,STCELN5Q                                                      
         CLI   NEWE.EMDIUS,YESQ    Internal use set?                            
         JNE   BLDST054                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEINO                                                  
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
                                                                                
BLDST054 TM    NEWE.EMDSTA,EMDSPQ+EMDSDQ Suppress print lines                   
         JNZ   BLDST055             Yes                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCPRNAL     Must now be print all lines                 
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
         J     BLDST05B                                                         
                                                                                
BLDST055 TM    NEWE.EMDSTA,EMDSPQ   Suppress zero print lines                   
         JZ    BLDST05A             No                                          
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEPRD                                                  
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
         J     BLDST05B                                                         
                                                                                
BLDST05A TM    NEWE.EMDSTA,EMDSDQ   Suppress zero and no desc print lns         
         JZ    BLDST05B             No                                          
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEPR0                                                  
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
                                                                                
BLDST05B TM    NEWE.EMDSTA,EMDSRQ   Suppress printing time rates                
         JNZ   BLDST05C             Yes                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEPTRT     Print time rates                            
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
         J     BLDST05D                                                         
                                                                                
BLDST05C GOTOR BLDBST                                                           
         MVI   STCETY2,STCESTRT     Suppress time rates                         
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
                                                                                
BLDST05D OC    NEWE.EMDSCA,NEWE.EMDSCA                                          
         JZ    BLDST05E            Supposed client approver                     
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCECAP                                                  
         MVC   STCEAPID,NEWE.EMDSCA                                             
         MVI   STCLN,STCELN4Q                                                   
         AHI   R4,STCELN4Q                                                      
                                                                                
BLDST05E OC    NEWE.EMDSIA,NEWE.EMDSIA                                          
         JZ    BLDST050            Supposed internal approver                   
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEIAP                                                  
         MVC   STCEAPID,NEWE.EMDSIA                                             
         MVI   STCLN,STCELN4Q                                                   
         AHI   R4,STCELN4Q                                                      
         J     BLDST050                                                         
*                                                                               
NEWE     USING ENMELD,NEW.ZTRDELM                                               
BLDST056 GOTOR BLDBST                                                           
         MVI   STCETY2,STCEESN     Name added                                   
         LLC   RF,NEWE.ENMLN                                                    
         SHI   RF,1+ENMLNQ                                                      
         BASR  RE,0                                                             
         MVC   STCENAM(0),NEWE.ENMNAME                                          
         EX    RF,0(RE)                                                         
         AHI   RF,1+STCELN1Q                                                    
         STC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     BLDST050                                                         
*                                                                               
NEWE     USING ERDELD,NEW.ZTRDELM                                               
BLDST062 MVI   BYTE4,STCEHTX       Header text                                  
         CLI   NEWE.ERDTYP,ERDTHTQ                                              
         JE    BLDST064                                                         
         MVI   BYTE4,STCEFTX       Footer text                                  
         CLI   NEWE.ERDTYP,ERDTFTQ                                              
         JE    BLDST064                                                         
         MVI   BYTE4,STCEETX       Estimate additional text                     
         CLI   NEWE.ERDTYP,ERDTETQ                                              
         JE    BLDST064                                                         
         MVI   BYTE4,STCEWCT       Workcode text                                
         CLI   NEWE.ERDTYP,ERDTWTQ                                              
         JE    BLDST064                                                         
         MVI   BYTE4,STCEITT       Item text                                    
         CLI   NEWE.ERDTYP,ERDTITQ                                              
         JE    BLDST064                                                         
         MVI   BYTE4,STCECTX       Category text                                
         CLI   NEWE.ERDTYP,ERDTCTQ                                              
         JNE   BLDST068                                                         
*                                                                               
BLDST064 CLC   NEWE.ERDTYP,X#SVTYP Have we already sent out text                
         JE    BLDST050            Yes                                          
         MVC   X#SVTYP,NEWE.ERDTYP                                              
         GOTOR BLDBST              Header/WC/item text addition                 
         MVC   STCETY2,BYTE4                                                    
         MVI   STCLN,STCELN1Q                                                   
         CLI   NEWE.ERDTYP,ERDTHTQ Header text?                                 
         JE    BLDST066                                                         
         CLI   NEWE.ERDTYP,ERDTFTQ Footer text?                                 
         JE    BLDST066                                                         
         CLI   NEWE.ERDTYP,ERDTETQ Estimate additional text?                    
         JE    BLDST066                                                         
         MVI   STCLN,STCELN7Q                                                   
         MVC   STCECATC,X#SVCAT                                                 
         CLI   NEWE.ERDTYP,ERDTCTQ Category text?                               
         JE    BLDST066                                                         
         MVC   STCEWRKC,X#SVWC                                                  
         CLI   NEWE.ERDTYP,ERDTWTQ Workcode text?                               
         JE    BLDST066                                                         
         MVC   STCEITEC,X#ITEC     Item text                                    
*                                                                               
BLDST066 LLC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     BLDST050                                                         
*                                                                               
BLDST068 CLI   NEWE.ERDTYP,ERDTCSQ                                              
         JE    *+12                                                             
         CLI   NEWE.ERDTYP,ERDTWDQ                                              
         JNE   BLDST050                                                         
         MVI   X#SVTYP,X'FF'                                                    
         CLI   NEWE.ERDTYP,ERDTWDQ                                              
         JNE   BLDST050                                                         
*&&DO                                                                           
         GOTOR BLDBST              Workcode add                                 
         MVC   X#SVCAT,NEWE.ERDWCAT                                             
         MVC   STCECATC,NEWE.ERDWCAT                                            
         MVC   STCEWRKC,NEWE.ERDWCOD                                            
         MVC   X#SVWC,NEWE.ERDWCOD                                              
         MVI   STCETY2,STCEWCO                                                  
         MVI   STCLN,STCELN7Q                                                   
         AHI   R4,STCELN7Q                                                      
*&&                                                                             
*                                                                               
         MVC   X#SVCAT,NEWE.ERDWCAT                                             
         MVC   X#SVWC,NEWE.ERDWCOD                                              
         CP    NEWE.ERDWAMT,PZERO  Don't audit zero amounts on add              
         JE    BLDST050                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEWAM     Workcode amount add                          
         MVC   STCEWRKC,NEWE.ERDWCOD                                            
         MVC   STCECATC,NEWE.ERDWCAT                                            
         MVC   STCEAMNT,NEWE.ERDWAMT                                            
         MVI   STCLN,STCELN8Q                                                   
         AHI   R4,STCELN8Q                                                      
         J     BLDST050                                                         
*                                                                               
BLDST072 CLI   NEWE.ERDTYP,ERDTIDQ                                              
         JNE   BLDST050                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEITE     Item add                                     
*                                                                               
         MVC   STCECATC,X#SVCAT                                                 
         MVC   STCEWRKC,X#SVWC                                                  
*                                                                               
         LHI   RF,L'ERDICOD-1                                                   
         CLI   NEWE.ERDLN,ERDITLQ  Normal or 1R element?                        
         JNE   *+8                                                              
         LHI   RF,L'ERDITAC-1                                                   
         BASR  RE,0                                                             
         MVC   STCEITEC(0),NEWE.ERDICOD Item code                               
         EX    RF,0(RE)                                                         
         BASR  RE,0                                                             
         MVC   X#ITEC,NEWE.ERDICOD                                              
         EX    RF,0(RE)                                                         
*&&DO                                                                           
         LA    RF,NEWE.ERDIAPR                                                  
         CLI   NEWE.ERDLN,ERDITLQ  Normal or 1R element?                        
         JNE   *+8                                                              
         LA    RF,NEWE.ERDIRAT                                                  
         ZAP   STCEIPPU,0(L'ERDIAPR,RF) Price Per unit/hour                     
*                                                                               
         LA    RF,NEWE.ERDIMUL                                                  
         CLI   NEWE.ERDLN,ERDITLQ  Normal or 1R element?                        
         JNE   *+8                                                              
         LA    RF,NEWE.ERDIHRS                                                  
         ZAP   STCEINOU,0(L'ERDIMUL,RF) No of units/hours                       
*&&                                                                             
         MVI   STCLN,STCELN7Q                                                   
         AHI   R4,STCELN7Q                                                      
         J     BLDST050                                                         
*                                                                               
NEWE     USING XDFELD,NEW.ZTRDELM                                               
BLDST074 GOTOR BLDBST                                                           
         MVI   STCETY2,STCEEXT     Xdata add                                    
         MVC   STCEXDTT,NEWE.XDFOTYP                                            
         MVC   STCEXDTC,NEWE.XDFOCOD                                            
         LLC   RF,NEWE.XDFLN                                                    
         SHI   RF,XDFOLNQ+1                                                     
         BASR  RE,0                                                             
         MVC   STCEXDAT(0),NEWE.XDFODTA                                         
         EX    RF,0(RE)                                                         
         AHI   RF,1+STCELNCQ                                                    
         STC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     BLDST050                                                         
*                               ** Changes **                                   
NEWE     USING EMDELD,NEW.ZTRDELM                                               
BLDST076 CLC   X#SCCOMM,SPACES     Standard approval comments                   
         JNH   BLDST079                                                         
         GOTOR BLDBST              Approver comments stceld                     
         MVI   STCETY2,STCAPCMT                                                 
         CLI   X#SCNVAL,ESTKINTA   internally approved                          
         JNE   *+8                                                              
         MVI   STCETY2,STCAPCMI    Set internal approver comments               
         LA    R1,STCELN2Q                                                      
                                                                                
         LA    RE,X#SCCOMM+L'X#SCCOMM-1                                         
         LA    RF,L'X#SCCOMM                                                    
                                                                                
BLDST077 CLI   0(RE),C' '                                                       
         JH    BLDST078                                                         
         SHI   RE,1                                                             
         JCT   RF,BLDST077                                                      
         DC    H'0'                                                             
                                                                                
BLDST078 AR    R1,RF                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   STCECOMM(0),X#SCCOMM                                             
         EX    RF,0(RE)                                                         
         STC   R1,STCLN                                                         
         MVC   X#STCEL1,STCELD                                                  
         AR    R4,R1                                                            
*                                                                               
BLDST079 CLC   X#SCCCDA,SPACES     Any client approver date passed?             
         JNH   BLDST080                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCAPDTE                                                 
         GOTOR VDATCON,DMCB,(0,X#SCCCDA+2),(1,STCECAPD)                         
         MVI   STCLN,STCELNDQ                                                   
         AHI   R4,STCELNDQ                                                      
*                                                                               
BLDST080 CLC   X#SCCCNA,SPACES     Any authoriser passed                        
         JNH   BLDST084                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCECCN                                                  
         LA    R1,STCELN1Q                                                      
         LA    RE,X#SCCCNA+L'X#SCCCNA-1                                         
         LA    RF,L'X#SCCCNA                                                    
                                                                                
BLDST081 CLI   0(RE),C' '                                                       
         JH    BLDST082                                                         
         SHI   RE,1                                                             
         JCT   RF,BLDST081                                                      
         DC    H'0'                                                             
                                                                                
BLDST082 AR    R1,RF                                                            
         STC   R1,STCLN                                                         
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   STCECNAM(0),X#SCCCNA                                             
         EX    RF,0(R1)                                                         
         LLC   RF,STCLN                                                         
         AR    R4,RF                                                            
                                                                                
BLDST084 L     R5,AIO3             R5=A(new element TSAR buffer)                
         XC    NEW.ZTRLEN,NEW.ZTRLEN                                            
         XC    NEW.ZTRKEY(ZTRKEYL),NEW.ZTRKEY                                   
         XC    NEW.ZTRDELM,NEW.ZTRDELM                                          
         GOTOR BUFELM,DMCB,('TSARDH',NSRBUF),AIO3                               
         J     BLDST086                                                         
*                                                                               
BLDST085 GOTOR BUFELM,DMCB,('TSANXT',NSRBUF),AIO3                               
*                                                                               
BLDST086 TM    TSARERRS,TSEEOF     Finished?                                    
         JNZ   BLDST140                                                         
                                                                                
         LA    RF,NEW.ZTRDELM                                                   
         CLI   0(RF),EMDELQ        Main data element                            
         JE    BLDST088                                                         
         CLI   0(RF),ENMELQ        Estimate name                                
         JE    BLDST088                                                         
         CLI   0(RF),ERDELQ        Estimate data                                
         JE    BLDST088                                                         
         CLI   0(RF),XDFELQ        Xdata                                        
         JNE   BLDST085                                                         
*                               ** Check for data moving about **               
BLDST088 L     R3,AIO1                                                          
         XC    OLD.ZTRLEN,OLD.ZTRLEN                                            
         MVC   OLD.ZTRKEY(ZTRKEYL),NEW.ZTRKEY                                   
         XC    OLD.ZTRDELM,OLD.ZTRDELM                                          
         GOTOR BUFELM,DMCB,('TSARDH',OLDBUF),AIO1                               
         MVI   BYTE3,STCEADD       New element                                  
         TM    TSARERRS,TSEEOF+TSERNF No element found completely new           
         JNZ   BLDST090                                                         
         MVI   BYTE3,STCECHG       Action change                                
*                                  Previous element found, compare              
BLDST090 LA    RF,NEW.ZTRDELM      data                                         
         CLI   0(RF),EMDELQ        Main data element                            
         JE    BLDST092                                                         
         CLI   0(RF),ENMELQ        Estimate name                                
         JE    BLDST100                                                         
         CLI   0(RF),ERDELQ        Estimate data                                
         JE    BLDST104                                                         
         CLI   0(RF),XDFELQ        Xdata                                        
         JE    BLDST136                                                         
*        CLI   0(RF),STCELQ        STCELD (client approved date/comts)          
*        JE    BLDST142                                                         
         J     BLDST085            Ignore other elements                        
*                                                                               
NEWE     USING EMDELD,NEW.ZTRDELM                                               
OLDE     USING EMDELD,OLD.ZTRDELM                                               
BLDST092 CLC   NEWE.EMDDAT,OLDE.EMDDAT                                          
         JE    BLDST093                                                         
         GOTOR BLDBST              Date change                                  
         MVI   STCETY2,STCEESD                                                  
         MVC   STCEDAT,NEWE.EMDDAT                                              
         MVI   STCLN,STCELN5Q                                                   
         AHI   R4,STCELN5Q                                                      
*                                                                               
BLDST093 MVC   BYTE1,NEWE.EMDSTA                                                
         MVC   BYTE2,OLDE.EMDSTA                                                
         NI    BYTE1,X'FF'-EMDSRQ                                               
         NI    BYTE2,X'FF'-EMDSRQ                                               
         CLC   BYTE1,BYTE2                                                      
         JE    BLDST09C                                                         
         GOTOR BLDBST                                                           
         TM    NEWE.EMDSTA,EMDSPQ+EMDSDQ Suppress print lines                   
         JNZ   BLDST09A             Yes                                         
         MVI   STCETY2,STCPRNAL     Must now be print all lines                 
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
         J     BLDST09C                                                         
*                                                                               
BLDST09A TM    NEWE.EMDSTA,EMDSPQ   Suppress zero print lines                   
         JZ    BLDST09B             No                                          
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEPRD                                                  
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
         J     BLDST09C                                                         
                                                                                
BLDST09B TM    NEWE.EMDSTA,EMDSDQ   Suppress zero and no desc print lns         
         JZ    BLDST09C             No                                          
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEPR0                                                  
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
*                                                                               
BLDST09C MVC   BYTE1,NEWE.EMDSTA                                                
         MVC   BYTE2,OLDE.EMDSTA                                                
         NI    BYTE1,X'FF'-(EMDSDQ+EMDSPQ)                                      
         NI    BYTE2,X'FF'-(EMDSDQ+EMDSPQ)                                      
         CLC   BYTE1,BYTE2                                                      
         JE    BLDST094                                                         
         GOTOR BLDBST                                                           
         TM    NEWE.EMDSTA,EMDSRQ   Suppress time rates                         
         JNZ   BLDST09D                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEPTRT     Print time rates                            
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
         J     BLDST094                                                         
                                                                                
BLDST09D GOTOR BLDBST                                                           
         MVI   STCETY2,STCESTRT     Suppress time rates                         
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
                                                                                
BLDST094 CLC   NEWE.EMDIUS,OLDE.EMDIUS                                          
         JE    BLDST096            Internal use only?                           
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEINF                                                  
         CLI   NEWE.EMDIUS,YESQ                                                 
         JNE   *+8                                                              
         MVI   STCETY2,STCEINO                                                  
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
*                                                                               
BLDST096 CLC   NEWE.EMDSCA,OLDE.EMDSCA                                          
         JE    BLDST098            Supposed client approver                     
         MVC   BYTE4,BYTE3         Save real action                             
         OC    NEWE.EMDSCA,NEWE.EMDSCA   If removed set as deleted              
         JNZ   BLDST097                                                         
         MVI   BYTE3,STCEDEL                                                    
                                                                                
BLDST097 GOTOR BLDBST                                                           
         MVI   STCETY2,STCECAP                                                  
         MVC   STCEAPID,NEWE.EMDSCA                                             
         MVI   STCLN,STCELN4Q                                                   
         MVC   X#STCEL1,STCELD                                                  
         AHI   R4,STCELN4Q                                                      
         MVC   BYTE3,BYTE4         Restore action                               
*                                                                               
BLDST098 CLC   NEWE.EMDSIA,OLDE.EMDSIA                                          
         JE    BLDST085            Supposed internal approver                   
         MVC   BYTE4,BYTE3         Save real action                             
         OC    NEWE.EMDSIA,NEWE.EMDSIA   If removed set as deleted              
         JNZ   BLDST099                                                         
         MVI   BYTE3,STCEDEL                                                    
BLDST099 GOTOR BLDBST                                                           
         MVI   STCETY2,STCEIAP                                                  
         MVC   STCEAPID,NEWE.EMDSIA                                             
         MVI   STCLN,STCELN4Q                                                   
         MVC   X#STCEL2,STCELD                                                  
         AHI   R4,STCELN4Q                                                      
         MVC   BYTE3,BYTE4         Restore action                               
         J     BLDST085                                                         
*                                                                               
NEWE     USING ENMELD,NEW.ZTRDELM                                               
OLDE     USING ENMELD,OLD.ZTRDELM                                               
BLDST100 CLI   BYTE3,STCEADD                                                    
         JE    *+14                                                             
         CLC   NEWE.ENMLN,OLDE.ENMLN                                            
         JNE   BLDST102            Check same length and value                  
         LLC   RF,NEWE.ENMLN                                                    
         SHI   RF,ENMLNQ+1                                                      
         BASR  RE,0                                                             
         CLC   NEWE.ENMNAME(0),OLDE.ENMNAME                                     
         EX    RF,0(RE)                                                         
         JE    BLDST085                                                         
*                                                                               
BLDST102 GOTOR BLDBST                                                           
         MVI   STCETY2,STCEESN     Name change                                  
         LLC   RF,NEWE.ENMLN                                                    
         SHI   RF,ENMLNQ+1                                                      
         BASR  RE,0                                                             
         MVC   STCENAM(0),NEWE.ENMNAME                                          
         EX    RF,0(RE)                                                         
         AHI   RF,1+STCELN1Q                                                    
         STC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     BLDST085                                                         
*                                                                               
NEWE     USING ERDELD,NEW.ZTRDELM                                               
OLDE     USING ERDELD,OLD.ZTRDELM                                               
BLDST104 CLI   BYTE3,STCEADD       New element                                  
         JE    BLDST106                                                         
*                                                                               
         CLI   NEWE.ERDTYP,ERDTWDQ Ignore other elements                        
         JNE   BLDST105                                                         
         CLC   NEW.ZTRKWUS,NEW.ZTRKWCS  Check same sequence now                 
         JE    BLDST105                                                         
         MVI   BYTE3,STCEMOV       Data moved                                   
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEWCO     Workcode move                                
         MVC   STCECATC,NEWE.ERDWCAT                                            
         MVC   STCEWRKC,NEWE.ERDWCOD                                            
         MVI   STCLN,STCELN7Q                                                   
         AHI   R4,STCELN7Q                                                      
         MVI   BYTE3,STCECHG       Action change                                
                                                                                
BLDST105 CLC   NEWE.ERDTYP,OLDE.ERDTYP                                          
         JNE   BLDST085            Check same type                              
BLDST106 MVI   BYTE4,STCEHTX                                                    
         CLI   NEWE.ERDTYP,ERDTHTQ Header?                                      
         JE    BLDST107                                                         
         MVI   BYTE4,STCEFTX                                                    
         CLI   NEWE.ERDTYP,ERDTFTQ Footer?                                      
         JE    BLDST107                                                         
         MVI   BYTE4,STCEETX                                                    
         CLI   NEWE.ERDTYP,ERDTETQ Estimate additional text?                    
         JE    BLDST107                                                         
         MVI   BYTE4,STCEWCT                                                    
         CLI   NEWE.ERDTYP,ERDTWTQ Workcode?                                    
         JE    BLDST107                                                         
         MVI   BYTE4,STCEITT                                                    
         CLI   NEWE.ERDTYP,ERDTITQ Item?                                        
         JE    BLDST107                                                         
         MVI   BYTE4,STCECTX                                                    
         CLI   NEWE.ERDTYP,ERDTCTQ Category?                                    
         JNE   BLDST112                                                         
*                                                                               
BLDST107 CLI   BYTE3,STCEADD                                                    
         JE    BLDST108                                                         
         CLC   NEWE.ERDLN,OLDE.ERDLN                                            
         JNE   BLDST108            Same length text?                            
         LLC   RF,NEWE.ERDLN                                                    
         SHI   RF,ERDTLNQ+1                                                     
         BASR  RE,0                                                             
         CLC   NEWE.ERDTEXT(0),OLDE.ERDTEXT                                     
         EX    RF,0(RE)                                                         
         JE    BLDST085                                                         
*                                                                               
BLDST108 CLC   NEWE.ERDTYP,X#SVTYP Have we already sent out this type           
         JE    BLDST085                                                         
         MVC   X#SVTYP,NEWE.ERDTYP                                              
         GOTOR BLDBST                                                           
         MVC   STCETY2,BYTE4       Header/Workcode text change                  
         MVI   STCLN,STCELN1Q                                                   
         CLI   NEWE.ERDTYP,ERDTHTQ Header text?                                 
         JE    BLDST110                                                         
         CLI   NEWE.ERDTYP,ERDTFTQ Footer text?                                 
         JE    BLDST110                                                         
         CLI   NEWE.ERDTYP,ERDTETQ Estimate additional text?                    
         JE    BLDST110                                                         
         MVI   STCLN,STCELN7Q                                                   
         MVC   STCECATC,X#SVCAT                                                 
         CLI   NEWE.ERDTYP,ERDTCTQ Category text?                               
         JE    BLDST110                                                         
         MVC   STCEWRKC,X#SVWC                                                  
         CLI   NEWE.ERDTYP,ERDTWTQ Workcode text?                               
         JE    BLDST110                                                         
         MVC   STCEITEC,X#ITEC     Item code?                                   
*                                                                               
BLDST110 LLC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     BLDST085                                                         
*                               ** Category sequence element **                 
BLDST112 MVI   X#SVTYP,X'FF'                                                    
         CLI   NEWE.ERDTYP,ERDTCSQ                                              
         JNE   BLDST113                                                         
         CLC   NEWE.ERDCNAM,OLDE.ERDCNAM                                        
         JE    BLDST085            Change in category name?                     
         GOTOR BLDBST                                                           
         MVC   STCECATC,NEWE.ERDCCOD                                            
         MVC   STCCWINM,NEWE.ERDCNAM                                            
         MVI   STCETY2,STCECTN                                                  
         LHI   RF,STCELN7Q                                                      
         AHI   RF,L'ERDCNAM                                                     
         STC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     BLDST085                                                         
*                                                                               
BLDST113 CLI   NEWE.ERDTYP,ERDTWDQ                                              
         JNE   BLDST122                                                         
         MVC   X#SVCAT,NEWE.ERDWCAT                                             
         MVC   X#SVWC,NEWE.ERDWCOD                                              
         CLI   BYTE3,STCEADD                                                    
         JE    *+14                                                             
         CLC   NEWE.ERDWCOD,OLDE.ERDWCOD                                        
         JE    BLDST114            Check workcode add/change                    
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEWCO     Workcode                                     
         MVC   STCECATC,NEWE.ERDWCAT                                            
         MVC   STCEWRKC,NEWE.ERDWCOD                                            
         MVI   STCLN,STCELN7Q                                                   
         AHI   R4,STCELN7Q                                                      
*                                                                               
BLDST114 CLI   BYTE3,STCEADD                                                    
         JE    BLDST116                                                         
         CLC   NEWE.ERDWNAM,OLDE.ERDWNAM                                        
         JE    BLDST116                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEWCN     Workcode name change                         
         MVC   STCECATC,X#SVCAT                                                 
         MVC   STCEWRKC,NEWE.ERDWCOD                                            
         MVC   STCCWINM,NEWE.ERDWNAM                                            
         LHI   RF,STCELN7Q                                                      
         AHI   RF,L'ERDWNAM                                                     
         STC   RF,STCLN                                                         
         AR    R4,RF                                                            
*                                                                               
BLDST116 CLI   BYTE3,STCEADD                                                    
         JNE   BLDST117                                                         
         CP    NEWE.ERDWAMT,PZERO  Don't audit zero amounts on add              
         JE    BLDST118                                                         
         J     *+14                                                             
BLDST117 CP    NEWE.ERDWAMT,OLDE.ERDWAMT                                        
         JE    BLDST118                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEWAM     Workcode amount change                       
         MVC   STCECATC,NEWE.ERDWCAT                                            
         MVC   STCEWRKC,NEWE.ERDWCOD                                            
         MVC   STCEAMNT,NEWE.ERDWAMT                                            
         MVI   STCLN,STCELN8Q                                                   
         AHI   R4,STCELN8Q                                                      
*                                                                               
BLDST118 DS    0H                                                               
*&&DO                              Implement later                              
         CP    NEWE.ERDWFCA,OLDE.ERDWFCA                                        
         JE    BLDST120                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEFCR     Workcode FC rate change                      
         MVI   STCLN,STCELN9Q                                                   
         MVC   STCECATC,X#SVCAT                                                 
         MVC   STCEWRKC,NEWE.ERDWCOD                                            
         MVC   STCEAMNT,NEWE.ERDWFCA                                            
         MVI   STCLN,STCELN8Q                                                   
         AHI   R4,STCELN8Q                                                      
*&&                                                                             
*                                                                               
BLDST120 CLI   BYTE3,STCEADD                                                    
         JE    BLDST085                                                         
         CP    NEWE.ERDWCRA,OLDE.ERDWCRA                                        
         JE    BLDST085                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCECRA     Workcode commission rate change              
         MVC   STCECATC,X#SVCAT                                                 
         MVC   STCEWRKC,NEWE.ERDWCOD                                            
         MVC   STCEAMNT,NEWE.ERDWCRA                                            
         MVI   STCLN,STCELN8Q                                                   
         AHI   R4,STCELN8Q                                                      
         J     BLDST085                                                         
*                                                                               
BLDST122 CLI   NEWE.ERDTYP,ERDTIDQ                                              
         JNE   BLDST085                                                         
*                                                                               
         LA    RF,NEWE.ERDICOD     Item code                                    
         LHI   RE,L'ERDICOD-1                                                   
         CLI   NEWE.ERDLN,ERDITLQ                                               
         JNE   *+12                                                             
         LA    RF,NEWE.ERDITAC                                                  
         LHI   RE,L'ERDITAC-1                                                   
*                                                                               
         BASR  R1,0                                                             
         MVC   X#ITEC(0),0(RF)     Save off for text auditing                   
         EX    RE,0(R1)                                                         
         OC    X#ITEC,SPACES                                                    
*                                                                               
         CLI   BYTE3,STCEADD                                                    
         JE    BLDST124                                                         
         CLC   NEWE.ERDLN,OLDE.ERDLN  Same length?                              
         JNE   BLDST124               change from item to hours/rate            
*                                                                               
         LHI   RE,L'ERDICOD-1                                                   
         CLI   NEWE.ERDLN,ERDITLQ                                               
         JNE   *+8                                                              
         LHI   RE,L'ERDITAC-1                                                   
*                                                                               
         BASR  R1,0                                                             
         CLC   NEWE.ERDICOD(0),OLDE.ERDICOD Item change                         
         EX    RE,0(R1)                                                         
         JE    BLDST126                                                         
*                                                                               
BLDST124 GOTOR BLDBST                                                           
         MVI   STCETY2,STCEITE     Workcode item change                         
         MVC   STCECATC,X#SVCAT                                                 
         MVC   STCEWRKC,X#SVWC                                                  
         LHI   RF,L'ERDICOD-1                                                   
         CLI   NEWE.ERDLN,ERDITLQ  Normal or 1R element?    ment?               
         JNE   *+8                                                              
         LHI   RF,L'ERDITAC-1                                                   
         BASR  RE,0                                                             
         MVC   STCEITEC(0),NEWE.ERDICOD Item code                               
         EX    RF,0(RE)                                                         
         MVI   STCLN,STCELN7Q                                                   
         AHI   R4,STCELN7Q                                                      
*                                                                               
BLDST126 CLI   BYTE3,STCEADD                                                    
         JE    BLDST128                                                         
         LA    RE,NEWE.ERDIMUL                                                  
         LA    RF,OLDE.ERDIMUL                                                  
         CLI   NEWE.ERDLN,ERDITLQ  Normal or 1R element?                        
         JNE   *+12                                                             
         LA    RE,NEWE.ERDIHRS                                                  
         LA    RF,OLDE.ERDIHRS                                                  
         CP    0(6,RE),0(6,RF)     Any change to multiplier?                    
         JE    BLDST130                                                         
*                                                                               
BLDST128 GOTOR BLDBST                                                           
         MVI   STCETY2,STCEIMU     item multiplier change                       
         MVC   STCECATC,X#SVCAT                                                 
         MVC   STCEWRKC,X#SVWC                                                  
         LHI   RF,L'ERDICOD-1                                                   
         CLI   NEWE.ERDLN,ERDITLQ  Normal or 1R element?                        
         JNE   *+8                                                              
         LHI   RF,L'ERDITAC-1                                                   
         BASR  RE,0                                                             
         MVC   STCEITEC(0),NEWE.ERDICOD Item code                               
         EX    RF,0(RE)                                                         
*                                                                               
         LA    RF,NEWE.ERDIMUL                                                  
         CLI   NEWE.ERDLN,ERDITLQ  Normal or 1R element?                        
         JNE   *+8                                                              
         LA    RF,NEWE.ERDIHRS                                                  
         ZAP   STCEAMNT,0(6,RF)                                                 
         MVI   STCLN,STCELN8Q                                                   
         AHI   R4,STCELN8Q                                                      
*                                                                               
BLDST130 CLI   BYTE3,STCEADD                                                    
         JE    BLDST132                                                         
         LA    RE,NEWE.ERDIAPR                                                  
         LA    RF,OLDE.ERDIAPR                                                  
         CLI   NEWE.ERDLN,ERDITLQ  Normal or 1R element?                        
         JNE   *+12                                                             
         LA    RE,NEWE.ERDIRAT                                                  
         LA    RF,OLDE.ERDIRAT                                                  
         CP    0(6,RE),0(6,RF)     Any change to price/hourly rate?             
         JE    BLDST134                                                         
*                                                                               
BLDST132 GOTOR BLDBST                                                           
         MVI   STCETY2,STCEITP     item price/hourly rate                       
         MVC   STCECATC,X#SVCAT                                                 
         MVC   STCEWRKC,X#SVWC                                                  
         LHI   RF,L'ERDICOD-1                                                   
         CLI   NEWE.ERDLN,ERDITLQ  Normal or 1R element?                        
         JNE   *+8                                                              
         LHI   RF,L'ERDITAC-1                                                   
         BASR  RE,0                                                             
         MVC   STCEITEC(0),NEWE.ERDICOD Item code                               
         EX    RF,0(RE)                                                         
*                                                                               
         LA    RF,NEWE.ERDIAPR                                                  
         CLI   NEWE.ERDLN,ERDITLQ  Normal or 1R element?                        
         JNE   *+8                                                              
         LA    RF,NEWE.ERDIRAT                                                  
         ZAP   STCEAMNT,0(6,RF)                                                 
         MVI   STCLN,STCELN8Q                                                   
         AHI   R4,STCELN8Q                                                      
*                                                                               
BLDST134 CLI   NEWE.ERDLN,ERDILNQ  Item element?                                
         JNE   BLDST085                                                         
         CLC   NEWE.ERDIDES,OLDE.ERDIDES                                        
         JE    BLDST085            Same description?                            
*                                                                               
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEINM     Item change                                  
         MVC   STCECATC,X#SVCAT                                                 
         MVC   STCEWRKC,X#SVWC                                                  
         LHI   RF,L'ERDICOD-1                                                   
         CLI   NEWE.ERDLN,ERDITLQ  Normal or 1R element?                        
         JNE   *+8                                                              
         LHI   RF,L'ERDITAC-1                                                   
         BASR  RE,0                                                             
         MVC   STCEITEC(0),NEWE.ERDICOD Item code                               
         EX    RF,0(RE)                                                         
*                                                                               
         MVC   STCCWINM,NEWE.ERDIDES                                            
         LHI   RF,L'ERDIDES                                                     
         AHI   RF,STCELN7Q                                                      
         STC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     BLDST085                                                         
*                                                                               
NEWE     USING XDFELD,NEW.ZTRDELM                                               
OLDE     USING XDFELD,OLD.ZTRDELM                                               
BLDST136 CLI   BYTE3,STCEADD                                                    
         JE    BLDST138                                                         
         CLC   NEWE.XDFLN,OLDE.XDFLN                                            
         JNE   BLDST138            Check same length/same data                  
         LLC   RF,NEWE.XDFLN                                                    
         SHI   RF,XDFOLNQ+1                                                     
         BASR  RE,0                                                             
         CLC   OLDE.XDFODTA(0),NEWE.XDFODTA                                     
         EX    RF,0(RE)                                                         
         JE    BLDST085                                                         
*                                                                               
BLDST138 GOTOR BLDBST                                                           
         MVI   STCETY2,STCEEXT     Xdata code                                   
         MVC   STCEXDTT,NEWE.XDFOTYP                                            
         MVC   STCEXDTC,NEWE.XDFOCOD                                            
         LLC   RF,NEWE.XDFLN                                                    
         SHI   RF,XDFOLNQ+1                                                     
         BASR  RE,0                                                             
         MVC   STCEXDAT(0),NEWE.XDFODTA                                         
         EX    RF,0(RE)                                                         
         AHI   RF,1+STCELNCQ                                                    
         STC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     BLDST085                                                         
*                               ** Now check for deletions **                   
BLDST140 L     R3,AIO1             R3=A(old element TSAR buffer)                
         XC    OLD.ZTRLEN,OLD.ZTRLEN                                            
         XC    OLD.ZTRKEY(ZTRKEYL),OLD.ZTRKEY                                   
         XC    OLD.ZTRDELM,OLD.ZTRDELM                                          
         GOTOR BUFELM,DMCB,('TSARDH',OLDBUF),AIO1                               
         J     BLDST144                                                         
*                                                                               
BLDST142 GOTOR BUFELM,DMCB,('TSANXT',OLDBUF),AIO1                               
*                                                                               
BLDST144 TM    TSARERRS,TSEEOF                                                  
         JNZ   BLDST163                                                         
*                                  Check it is an element we want               
         LA    RF,OLD.ZTRDELM                                                   
         CLI   0(RF),EMDELQ        Main data element                            
         JE    BLDST146                                                         
         CLI   0(RF),ENMELQ        Estimate name                                
         JE    BLDST146                                                         
         CLI   0(RF),ERDELQ        Estimate row data (wc/item/cat)              
         JE    BLDST145                                                         
         CLI   0(RF),XDFELQ        Xdata                                        
         JNE   BLDST142                                                         
         J     BLDST146                                                         
*                                  If workcode data type store                  
OLDE     USING ERDELD,RF           workcode and category information            
BLDST145 CLI   OLDE.ERDTYP,ERDTWDQ                                              
         JNE   BLDST146                                                         
         MVC   X#SVWC,OLDE.ERDWCOD                                              
         MVC   X#SVCAT,OLDE.ERDWCAT                                             
         DROP  OLDE                                                             
*                                                                               
BLDST146 MVI   BYTE3,0                                                          
*                                                                               
         L     R5,AIO3                                                          
         XC    NEW.ZTRLEN,NEW.ZTRLEN                                            
         MVC   NEW.ZTRKEY(ZTRKEYL),OLD.ZTRKEY                                   
         XC    NEW.ZTRDELM,NEW.ZTRDELM                                          
         GOTOR BUFELM,DMCB,('TSARDH',NSRBUF),AIO3                               
         TM    TSARERRS,TSEEOF+TSERNF No element found completely new           
         JZ    BLDST142                                                         
*                                                                               
BLDST148 MVI   BYTE3,STCEDEL       Data deleted                                 
         LA    RF,OLD.ZTRDELM      Previous element found, compare              
         CLI   0(RF),ENMELQ                                                     
         JE    BLDST150                                                         
         CLI   0(RF),ERDELQ                                                     
         JE    BLDST152                                                         
         CLI   0(RF),XDFELQ                                                     
         JE    BLDST162                                                         
         J     BLDST142            Ignore other elements                        
*                                                                               
BLDST150 GOTOR BLDBST                                                           
         MVI   STCETY2,STCEESN     Name deletion                                
         MVI   STCLN,STCELN1Q                                                   
         AHI   R4,STCELN1Q                                                      
         J     BLDST142                                                         
*                                                                               
OLDE     USING ERDELD,OLD.ZTRDELM                                               
BLDST152 MVI   BYTE4,STCEHTX       Header text                                  
         CLI   OLDE.ERDTYP,ERDTHTQ                                              
         JE    BLDST154                                                         
         MVI   BYTE4,STCEFTX       Footer text                                  
         CLI   OLDE.ERDTYP,ERDTFTQ                                              
         JE    BLDST154                                                         
         MVI   BYTE4,STCEETX       Estimate additional text                     
         CLI   OLDE.ERDTYP,ERDTETQ                                              
         JE    BLDST154                                                         
         MVI   BYTE4,STCEWCT       Workcode text                                
         CLI   OLDE.ERDTYP,ERDTWTQ                                              
         JE    BLDST154                                                         
         MVI   BYTE4,STCEITT       Item text                                    
         CLI   OLDE.ERDTYP,ERDTITQ                                              
         JE    BLDST154                                                         
         MVI   BYTE4,STCECTX       Category text                                
         CLI   OLDE.ERDTYP,ERDTCTQ                                              
         JNE   BLDST158                                                         
*                                                                               
BLDST154 CLC   OLDE.ERDTYP,X#SVTYP Have we already sent out this type           
         JE    BLDST142                                                         
         MVC   X#SVTYP,OLDE.ERDTYP                                              
         GOTOR BLDBST                                                           
         MVC   STCETY2,BYTE4       Workcode text deletion                       
         MVI   STCLN,STCELN1Q                                                   
         CLI   OLDE.ERDTYP,ERDTHTQ Header text?                                 
         JE    BLDST156                                                         
         CLI   OLDE.ERDTYP,ERDTFTQ Footer text?                                 
         JE    BLDST156                                                         
         CLI   OLDE.ERDTYP,ERDTETQ Estimate additional text?                    
         JE    BLDST156                                                         
         MVI   STCLN,STCELN7Q                                                   
         MVC   STCECATC,X#SVCAT                                                 
         CLI   OLDE.ERDTYP,ERDTCTQ Category text?                               
         JE    BLDST156                                                         
         MVC   STCEWRKC,X#SVWC                                                  
         CLI   OLDE.ERDTYP,ERDTWTQ Workcode text?                               
         JE    BLDST156                                                         
         MVC   STCEITEC,X#ITEC                                                  
*                                                                               
BLDST156 LLC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     BLDST142                                                         
*                                                                               
BLDST158 MVI   X#SVTYP,X'FF'                                                    
         CLI   OLDE.ERDTYP,ERDTCSQ                                              
         JNE   BLDST159                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCECAT     Category deletion                            
         MVC   STCECATC,OLDE.ERDCCOD                                            
         MVC   STCCWINM,OLDE.ERDCNAM                                            
         LHI   RF,STCELN7Q                                                      
         AHI   RF,L'ERDCNAM                                                     
         STC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     BLDST142                                                         
*                                                                               
BLDST159 CLI   OLDE.ERDTYP,ERDTWDQ                                              
         JNE   BLDST160                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEWCO     Workcode deletion                            
         MVC   STCECATC,OLDE.ERDWCAT                                            
         MVC   STCEWRKC,OLDE.ERDWCOD                                            
         MVC   X#SVCAT,OLDE.ERDWCAT                                             
         MVC   X#SVWC,OLDE.ERDWCOD                                              
         MVI   STCLN,STCELN7Q                                                   
         AHI   R4,STCELN7Q                                                      
         J     BLDST142                                                         
*                                                                               
BLDST160 CLI   OLDE.ERDTYP,ERDTIDQ                                              
         JNE   BLDST142                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCEITE     Item deletion                                
         MVC   STCECATC,OLDE.ERDWCAT                                            
         MVC   STCEWRKC,OLDE.ERDWCOD                                            
*                                                                               
         LHI   RF,L'ERDICOD-1                                                   
         CLI   OLDE.ERDLN,ERDITLQ                                               
         JNE   *+8                                                              
         LHI   RF,L'ERDITAC-1                                                   
         BASR  RE,0                                                             
         MVC   X#ITEC(0),OLDE.ERDICOD                                           
         EX    RF,0(RE)                                                         
*                                                                               
         MVI   STCLN,STCELN7Q                                                   
         AHI   R4,STCELN7Q                                                      
         J     BLDST142                                                         
*                                                                               
OLDE     USING XDFELD,OLD.ZTRDELM                                               
BLDST162 GOTOR BLDBST                                                           
         MVI   STCETY2,STCEEXT     Xdata code                                   
         MVC   STCEXDTT,OLDE.XDFOTYP                                            
         MVC   STCEXDTC,OLDE.XDFOCOD                                            
         LLC   RF,OLDE.XDFLN                                                    
         SHI   RF,XDFOLNQ+1                                                     
         BASR  RE,0                                                             
         MVC   STCEXDAT(0),OLDE.XDFODTA                                         
         EX    RF,0(RE)                                                         
         AHI   RF,1+STCELNCQ                                                    
         STC   RF,STCLN                                                         
         AR    R4,RF                                                            
         J     BLDST142                                                         
*                                                                               
BLDST163 DS    0H                                                               
         CLI   X#ACTION,RQUGAMQ                                                 
         JNE   BLDSTCX             Get here from maint+status?                  
         CLI   X#STATUS,X#NOSTAQ                                                
         JE    BLDSTCX             Maint only, probably don't need this         
*                                                                               
* Status changes                                                                
*                                                                               
BLDST176 MVI   BYTE3,STCECHG                                                    
         CLI   X#STATUS,RQUGAAQ    - approve                                    
         JE    BLDST184                                                         
         CLI   X#STATUS,RQUGAIQ    - internally approve                         
         JE    BLDST184                                                         
         CLI   X#STATUS,RQUGABQ    - submit to internal approver                
         JE    BLDST184                                                         
         CLI   X#STATUS,RQUGASQ    - submit to client approver                  
         JE    BLDST184                                                         
         CLI   X#STATUS,RQUGA1Q    - submit for electronic signatures           
         JE    BLDST196                                                         
         CLI   X#STATUS,RQUGA2Q    - cancel for electronic signatures           
         JE    BLDST196                                                         
         CLI   X#STATUS,RQUGARQ    - reject                                     
         JE    BLDST184                                                         
         CLI   X#STATUS,RQUGADQ    - logical delete                             
         JE    BLDST184                                                         
         CLI   X#STATUS,RQUGA#Q    - physical delete                            
         JNE   BLDST085                                                         
*                                                                               
BLDST184 GOTOR BLDBST              Status change stceld                         
         MVI   STCETY2,STCESTA                                                  
         MVC   STCECES,X#NSENUM                                                 
         MVC   STCEPES,X#OSENUM                                                 
         LA    R1,STCELN2Q                                                      
                                                                                
         CLC   X#SCCOMM,SPACES                                                  
         JNH   BLDST190                                                         
                                                                                
         LA    RE,X#SCCOMM+L'X#SCCOMM-1                                         
         LA    RF,L'X#SCCOMM                                                    
                                                                                
BLDST186 CLI   0(RE),C' '                                                       
         JH    BLDST188                                                         
         SHI   RE,1                                                             
         JCT   RF,BLDST186                                                      
         DC    H'0'                                                             
                                                                                
BLDST188 AR    R1,RF                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   STCECOMM(0),X#SCCOMM                                             
         EX    RF,0(RE)                                                         
                                                                                
BLDST190 STC   R1,STCLN                                                         
         MVC   X#STCEL1,STCELD                                                  
         AR    R4,R1                                                            
                                                                                
         CLC   X#SCCOMM,SPACES                                                  
         JNH   BLDST378                                                         
         GOTOR BLDBST              Approver comments stceld                     
         MVI   STCETY2,STCAPCMT                                                 
         CLI   X#SCNVAL,ESTKINTA   internally approved                          
         JNE   *+8                                                              
         MVI   STCETY2,STCAPCMI    Set internal approver comments               
         LA    R1,STCELN2Q                                                      
                                                                                
         LA    RE,X#SCCOMM+L'X#SCCOMM-1                                         
         LA    RF,L'X#SCCOMM                                                    
                                                                                
BLDST192 CLI   0(RE),C' '                                                       
         JH    BLDST194                                                         
         SHI   RE,1                                                             
         JCT   RF,BLDST192                                                      
         DC    H'0'                                                             
                                                                                
BLDST194 AR    R1,RF                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   STCECOMM(0),X#SCCOMM                                             
         EX    RF,0(RE)                                                         
         STC   R1,STCLN                                                         
         AR    R4,R1                                                            
         J     BLDST378                                                         
                                                                                
BLDST196 GOTOR BLDBST              Electronic status change stceld              
         MVI   STCETY2,STCSTATE                                                 
         MVC   STCELSTP,X#ESIGSO                                                
         MVC   STCELSTC,X#ESIGSN                                                
         MVC   STCEEXPD,X#ESEXDT                                                
         LA    R1,STCELNEQ                                                      
         CLI   X#STATUS,RQUGA2Q    - cancel for electronic signatures           
         JE    BLDST300                                                         
                                                                                
         AHI   R1,L'STCECIDN+L'STCECSEQ  if submit will have email              
         LA    R5,STCECOMT                                addresses             
                                                                                
         XC    BYTE1,BYTE1                                                      
         LA    R3,ESTRFST                                                       
         USING FFTELD,R3                                                        
BLDST198 CLI   FFTEL,0                                                          
         JE    BLDST300                                                         
         CLI   FFTEL,FFTELQ                                                     
         JE    BLDST202                                                         
BLDST200 LLC   RF,FFTLN                                                         
         AR    R3,RF                                                            
         J     BLDST198                                                         
                                                                                
BLDST202 DS    0H                                                               
*&&UK*&& CLI   FFTTYPE,FFTTPEML                                                 
*&&US*&& CLI   FFTTYPE,FFTTEML                                                  
         JNE   BLDST200                                                         
         LLC   RE,FFTLN                                                         
         SHI   RE,FFTLN1Q+1                                                     
         SRDA  RE,32                                                            
         LHI   R2,L'FFTEML1                                                     
         DR    RE,R2               RE=Number of email addresses                 
         MVC   STCECIDN,X#NEWIDN                                                
         MVC   STCECSEQ,BYTE1                                                   
         LA    RE,FFTEML1                                                       
                                                                                
BLDST204 CHI   R1,L'STCECOMT+L'STCECIDN+L'STCECSEQ+STCELNEQ                     
         JNL   BLDST206                                                         
         MVC   0(L'FFTEML1,R5),0(RE)                                            
         AHI   R1,L'FFTEML1                                                     
         LA    RE,L'FFTEML1(RE)                                                 
         LA    R5,L'FFTEML1(R5)                                                 
         JCT   RF,BLDST204                                                      
         J     BLDST200                                                         
                                                                                
BLDST206 STC   R1,STCLN                                                         
         AR    R4,R1                                                            
         LLC   R1,BYTE1                                                         
         AHI   R1,1                                                             
         STC   R1,BYTE1                                                         
         ST    RF,SAVER0                                                        
         ST    RE,SAVERE                                                        
         GOTOR BLDBST                                                           
         L     RF,SAVER0                                                        
         L     RE,SAVERE                                                        
         MVI   STCETY2,STCSTATE                                                 
         MVC   STCELSTP,X#ESIGSO                                                
         MVC   STCELSTC,X#ESIGSN                                                
         MVC   STCEEXPD,X#ESEXDT                                                
         LA    R1,STCELNEQ+L'STCECIDN+L'STCECSEQ                                
         MVC   STCECIDN,X#NEWIDN                                                
         MVC   STCECSEQ,BYTE1                                                   
         LA    R5,STCECOMT                                addresses             
         J     BLDST204                                                         
                                                                                
BLDST300 STC   R1,STCLN                                                         
         MVC   X#STCEL1,STCELD                                                  
         AR    R4,R1                                                            
                                                                                
                                                                                
BLDST378 CLI   X#NSCHAR,RQUGAAQ                                                 
         JE    BLDST380                                                         
         CLI   X#NSCHAR,RQUGAIQ                                                 
         JE    BLDST380                                                         
         CLI   X#NSCHAR,RQUGARQ                                                 
         JNE   BLDST388                                                         
                                                                                
BLDST380 CLC   X#SCCCDA,SPACES       any approver date passed?                  
         JNH   BLDST382                                                         
                                                                                
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCAPDTE                                                 
         MVI   STCLN,STCELNDQ                                                   
         GOTOR VDATCON,DMCB,(0,X#SCCCDA+2),(1,STCECAPD)                         
         LLC   RF,STCLN                                                         
         AR    R4,RF                                                            
                                                                                
BLDST382 CLC   X#SCCCNA,SPACES     Any authoriser passed                        
         JNH   BLDST388                                                         
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCECCN                                                  
         LA    R1,STCELN1Q                                                      
         LA    RE,X#SCCCNA+L'X#SCCCNA-1                                         
         LA    RF,L'X#SCCCNA                                                    
                                                                                
BLDST384 CLI   0(RE),C' '                                                       
         JH    BLDST386                                                         
         SHI   RE,1                                                             
         JCT   RF,BLDST384                                                      
         DC    H'0'                                                             
                                                                                
BLDST386 AR    R1,RF                                                            
         STC   R1,STCLN                                                         
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   STCECNAM(0),X#SCCCNA                                             
         EX    RF,0(R1)                                                         
                                                                                
         LLC   RF,STCLN                                                         
         AR    R4,RF                                                            
                                                                                
BLDST388 CLC   X#ECLAP,X#SAPPR     Is the client approver the same              
         JE    BLDST394                                                         
         MVC   BYTE4,BYTE3         Save real action                             
         OC    X#ECLAP,X#ECLAP     Has the client approver been added           
         JNZ   BLDST390            No                                           
         MVI   BYTE3,STCEADD                                                    
         J     BLDST392                                                         
                                                                                
BLDST390 OC    X#SAPPR,X#SAPPR     Has the client approver been deleted         
         JNZ   BLDST392            No                                           
         MVI   BYTE3,STCEDEL                                                    
                                                                                
BLDST392 GOTOR BLDBST                                                           
         MVI   STCETY2,STCECAP                                                  
         MVC   STCEAPID,X#SAPPR                                                 
         OC    X#SAPPR,X#SAPPR     Has the client approver been deleted         
         JNZ   *+10                No                                           
         MVC   STCEAPID,X#ECLAP                                                 
         MVI   STCLN,STCELN4Q                                                   
         AHI   R4,STCELN4Q                                                      
         MVC   BYTE3,BYTE4         Restore real action                          
                                                                                
BLDST394 CLC   X#EINAP,X#SIAPP     Is the internal approver the same            
         JE    BLDST400                                                         
         MVC   BYTE4,BYTE3         Save real action                             
         OC    X#EINAP,X#EINAP     Has the internal approver been added         
         JNZ   BLDST396            No                                           
         MVI   BYTE3,STCEADD                                                    
         J     BLDST398                                                         
                                                                                
BLDST396 OC    X#SIAPP,X#SIAPP     Has the intnl approver been deleted          
         JNZ   BLDST398            No                                           
         MVI   BYTE3,STCEDEL                                                    
                                                                                
BLDST398 GOTOR BLDBST                                                           
         MVI   STCETY2,STCEIAP                                                  
         MVC   STCEAPID,X#SIAPP                                                 
         OC    X#SIAPP,X#SIAPP     Has the intnl approver been deleted          
         JNZ   *+10                No                                           
         MVC   STCEAPID,X#EINAP                                                 
         MVI   STCLN,STCELN4Q                                                   
         AHI   R4,STCELN4Q                                                      
                                                                                
BLDST400 DS    0H                  more elements go here                        
*                                                                               
BLDSTCX  J     EXITY               return CC                                    
*                              *** Build basic stceld ***                       
BLDBST   MVI   STCEL,STCELQ                                                     
         MVI   STCLN,STCLN1Q                                                    
         MVI   STCIND,STCIEST                                                   
         MVI   STCIND,STCIEST2                                                  
         MVI   STCLN,STCELN1Q                                                   
         MVC   STCETYP,BYTE3                                                    
         MVC   STCUSER,CUUSER                                                   
         MVC   STCPERS,CCTPID                                                   
         MVC   STCDATE,SVTODAYP                                                 
         MVC   STCTIME,CTIME                                                    
         BR    RE                                                               
         DROP  R2,R4,OLD,NEW                                                    
         EJECT                                                                  
                                                                                
***********************************************************************         
* Build client approve comments element for estimate record on Aura   *         
***********************************************************************         
         SPACE 1                                                                
         USING STCELD,R4                                                        
         USING ESTRECD,R3                                                       
BLDAPCM  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDAPCM'                                                      
         L     R3,AIO2                                                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCAPCMT                                                 
         MVI   STCETYP,STCEADD                                                  
         LA    R1,STCELN2Q                                                      
         CLC   X#SCCOMM,SPACES                                                  
         JNH   BLDAPCMN                                                         
                                                                                
         LA    RE,X#SCCOMM+L'X#SCCOMM-1                                         
         LA    RF,L'X#SCCOMM                                                    
                                                                                
BLDACM02 CLI   0(RE),C' '                                                       
         JH    BLDACM04                                                         
         SHI   RE,1                                                             
         JCT   RF,BLDACM02                                                      
         DC    H'0'                                                             
                                                                                
BLDACM04 AR    R1,RF                                                            
         STC   R1,STCLN                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   STCECOMM(0),X#SCCOMM                                             
         EX    RF,0(RE)                                                         
         LA    R1,STATAB           set old status character value               
         MVC   BYTE1,X#SC2OVL                                                   
         NI    BYTE1,ESTKSINA                                                   
*                                                                               
BLDACM06 CLI   0(R1),FF                                                         
         JNE   *+6                                                              
         DC    H'0'                (must not happen)                            
         CLC   X#SCOVAL,0(R1)                                                   
         JNE   BLDACM08                                                         
         CLC   BYTE1,1(R1)                                                      
         JE    BLDACM10                                                         
BLDACM08 AHI   R1,STATABQ                                                       
         J     BLDACM06                                                         
*                                                                               
BLDACM10 MVC   STCEPES,3(R1)                                                    
         LA    R1,STATAB           set new status character value               
         MVC   BYTE1,X#SC2NVL                                                   
         NI    BYTE1,ESTKSINA                                                   
*                                                                               
BLDACM12 CLI   0(R1),FF                                                         
         JNE   *+6                                                              
         DC    H'0'                (must not happen)                            
         CLC   X#SCNVAL,0(R1)                                                   
         JNE   BLDACM14                                                         
         CLC   BYTE1,1(R1)                                                      
         JE    BLDACM16                                                         
BLDACM14 AHI   R1,STATABQ                                                       
         J     BLDACM12                                                         
*                                                                               
BLDACM16 MVC   STCECES,3(R1)                                                    
         CLI   STCECES,OINTAPP                                                  
         JNE   BLDACM18                                                         
         MVI   STCETY2,STCAPCMI    Set internal approver                        
BLDACM18 GOTOR PUTSAR,NEWBUF                                                    
*                                                                               
BLDAPCMY J     EXITY                                                            
BLDAPCMN J     EXITN                                                            
         EJECT                                                                  
         DROP  R3,R4                                                            
***********************************************************************         
* Build authoriser audit element on estimate record for Aura          *         
***********************************************************************         
         SPACE 1                                                                
         USING STCELD,R4                                                        
         USING ESTRECD,R3                                                       
BLDAUTH  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDAUTH'                                                      
         L     R3,AIO2                                                          
*                                                                               
         CLC   X#SCCCNA,SPACES     Any name passed?                             
         JNH   BLDAUTHN                                                         
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCECCN                                                  
         MVI   STCETYP,STCEADD                                                  
         LA    R1,STCELN1Q                                                      
                                                                                
         LA    RE,X#SCCCNA+L'X#SCCCNA-1                                         
         LA    RF,L'X#SCCCNA                                                    
                                                                                
BLDAUTH2 CLI   0(RE),C' '                                                       
         JH    BLDAUTH4                                                         
         SHI   RE,1                                                             
         JCT   RF,BLDAUTH2                                                      
         DC    H'0'                                                             
                                                                                
BLDAUTH4 AR    R1,RF                                                            
         STC   R1,STCLN                                                         
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   STCECNAM(0),X#SCCCNA                                             
         EX    RF,0(R1)            Client contact name (authoriser)             
         GOTOR PUTSAR,NEWBUF                                                    
*                                                                               
BLDAUTHY J     EXITY                                                            
BLDAUTHN J     EXITN                                                            
         EJECT                                                                  
         DROP  R3,R4                                                            
***********************************************************************         
* Build approver date element on estimate record for Aura             *         
***********************************************************************         
         SPACE 1                                                                
         USING STCELD,R4                                                        
         USING ESTRECD,R3                                                       
BLDAPDT  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDAPDT'                                                      
         L     R3,AIO2                                                          
*                                                                               
         CLC   X#SCCCDA,SPACES     any date passed?                             
         JNH   BLDAPDTN                                                         
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         GOTOR BLDBST                                                           
         MVI   STCETY2,STCAPDTE                                                 
         MVI   STCETYP,STCEADD                                                  
         MVI   STCLN,STCELNDQ                                                   
         GOTOR VDATCON,DMCB,(0,X#SCCCDA+2),(1,STCECAPD)                         
         GOTOR PUTSAR,NEWBUF                                                    
*                                                                               
BLDAPDTY J     EXITY                                                            
BLDAPDTN J     EXITN                                                            
         EJECT                                                                  
         DROP  R3,R4                                                            
***********************************************************************         
* Validate w/c data                                                   *         
***********************************************************************         
         SPACE 1                                                                
VALWCD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALWCD*'                                                      
                                                                                
         DS    0H                  RQUWCAT not validated                        
                                                                                
         MVC   X#WORKC,RQUWCOD                                                  
         GOTOR TSTWCD                                                           
         JNE   EXITN                                                            
                                                                                
         DS    0H                  any validation required?                     
                                                                                
         CLC   RQUWNAM,SPACES                                                   
         JNH   *+10                                                             
         MVC   X#WORKN,RQUWNAM                                                  
                                                                                
         ZAP   X#IAMT1,PZERO                                                    
         ZAP   X#IAMT2,PZERO                                                    
         ZAP   X#IMULT,PZERO                                                    
         ZAP   X#WAMT1,PZERO                                                    
         ZAP   X#WAMT2,PZERO                                                    
         ZAP   X#WAMT3,PZERO                                                    
         ZAP   X#WAMT4,PZERO                                                    
         ZAP   X#WRATE,PZERO                                                    
         ZAP   X#WVATR,PZERO                                                    
         ZAP   X#WVATA,PZERO                                                    
         ZAP   X#WVATF,PZERO                                                    
         ZAP   X#WNICA,PZERO                                                    
         ZAP   X#WNICF,PZERO                                                    
         ZAP   X#WESPR,PZERO                                                    
         MVI   X#WVATC,C' '                                                     
         MVC   X#UPWCSQ,RQUWCSQ                                                 
                                                                                
         CLC   RQUWAMT,SPACES                                                   
         JNH   VALWCD05                                                         
         MVC   TEMP2(16),RQUWAMT                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#WAMT1,TEMP2+16(8)                                              
                                                                                
VALWCD05 CLC   RQUWFCA,SPACES                                                   
         JNH   VALWCD10                                                         
         MVC   TEMP2(16),RQUWFCA                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#WAMT2,TEMP2+16(8)                                              
                                                                                
VALWCD10 CLC   RQUWCAM,SPACES                                                   
         JNH   VALWCD15                                                         
         MVC   TEMP2(16),RQUWCAM                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#WAMT3,TEMP2+16(8)                                              
                                                                                
VALWCD15 CLC   RQUWCFC,SPACES                                                   
         JNH   VALWCD20                                                         
         MVC   TEMP2(16),RQUWCFC                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#WAMT4,TEMP2+16(8)                                              
                                                                                
VALWCD20 CLC   RQUWCRA,SPACES                                                   
         JNH   VALWCD25                                                         
         MVC   TEMP2(16),RQUWCRA                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#WRATE,TEMP2+16(8)                                              
                                                                                
VALWCD25 CLC   RQUWCVR,SPACES                                                   
         JNH   VALWCD30                                                         
         MVC   TEMP2(16),RQUWCVR                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#WVATR,TEMP2+16(8)                                              
                                                                                
VALWCD30 CLC   RQUWCVA,SPACES                                                   
         JNH   VALWCD35                                                         
         MVC   TEMP2(16),RQUWCVA                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#WVATA,TEMP2+16(8)                                              
                                                                                
VALWCD35 CLC   RQUWCVF,SPACES                                                   
         JNH   VALWCD40                                                         
         MVC   TEMP2(16),RQUWCVF                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#WVATF,TEMP2+16(8)                                              
                                                                                
VALWCD40 MVC   X#WVATC,RQUWCVC                                                  
                                                                                
         CLC   RQUWNIC,SPACES                                                   
         JNH   VALWCD45                                                         
         MVC   TEMP2(16),RQUWNIC                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#WNICA,TEMP2+16(8)                                              
                                                                                
VALWCD45 CLC   RQUWNFC,SPACES                                                   
         JNH   VALWCD50                                                         
         MVC   TEMP2(16),RQUWNFC                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#WNICF,TEMP2+16(8)                                              
                                                                                
VALWCD50 CLC   RQUWCCR,SPACES                                                   
         JNH   VALWCD55                                                         
         MVC   TEMP2(16),RQUWCCR                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#WESPR,TEMP2+16(8)                                              
                                                                                
VALWCD55 CLC   RQUIPRI,SPACES                                                   
         JNH   VALWCD60                                                         
         MVC   TEMP2(16),RQUIPRI   item price/hourly rate                       
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#IAMT1,TEMP2+16(8)                                              
                                                                                
VALWCD60 CLC   RQUIFCP,SPACES                                                   
         JNH   VALWCD65                                                         
         MVC   TEMP2(16),RQUIFCP                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#IAMT2,TEMP2+16(8)                                              
                                                                                
VALWCD65 CLC   RQUIMUL,SPACES                                                   
         JNH   VALWCD70                                                         
         MVC   TEMP2(16),RQUIMUL   item multiplier/n'hours                      
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#IMULT,TEMP2+16(8)                                              
                                                                                
VALWCD70 DS    0H                                                               
                                                                                
         J     EXITY                                                            
                                                                                
TSTWCD   ST    RE,SAVERE                                                        
                                                                                
         MVC   X#WORKN,SPACES                                                   
                                                                                
         USING WCORECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUXCPY                                                   
         MVC   WCOKUNT(2),PRODUL                                                
         MVC   WCOKWRK,X#WORKC                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    TSTWCD2                                                          
                                                                                
TSTWCD1  MVC   ROUERRV,=AL2(AE$WRKNF)                                           
         J     TSTWCDN                                                          
                                                                                
TSTWCD2  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   TSTWCD1                                                          
                                                                                
         USING WCOELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,WCORFST                                                       
         XR    R0,R0                                                            
                                                                                
TSTWCD3  CLI   WCOEL,0                                                          
         JE    TSTWCD1                                                          
         CLI   WCOEL,WCOELQ                                                     
         JE    TSTWCD4                                                          
         IC    R0,WCOLN                                                         
         AR    R3,R0                                                            
         J     TSTWCD3                                                          
                                                                                
TSTWCD4  TM    WCOSTAT2,WCOSLEST                                                
         JZ    TSTWCD5                                                          
                                                                                
         MVC   ROUERRV,=AL2(AE$WCLCK)                                           
         J     TSTWCDN                                                          
*&&US                                                                           
TSTWCD5  MVI   X#SVINTW,NOQ                                                     
         MVC   X#WORKN(L'WCODESC),WCODESC                                       
         CLI   WCOTYPE,C'T'        Is it time or cost                           
         JNE   TSTWCDY                                                          
*&&                                                                             
*&&UK                                                                           
TSTWCD5  MVC   X#WORKN(L'WCODESC),WCODESC                                       
         MVI   X#SVINTW,NOQ                                                     
         TM    WCOSTAT,WCOSHCOE    Cost workcode?                               
         JZ    TSTWCDY                                                          
*&&                                                                             
         MVI   X#SVINTW,YESQ       Set internal                                 
                                                                                
TSTWCDY  L     RE,SAVERE                                                        
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
TSTWCDN  L     RE,SAVERE                                                        
         LTR   RE,RE                                                            
         BR    RE                                                               
         DROP  R2,R3                                                            
                                                                                
* Post w/c data                                                                 
         USING ERDELD,R3                                                        
PSTWCD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PSTWCD*'                                                      
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ERDEL,ERDELQ                                                     
         MVI   ERDLN,ERDWLXQ                                                    
         CP    X#IMULT,PZERO       Have we got a multiplier                     
         JE    PSTWCD02            No                                           
         MVI   ERDLN,ERDWLX1Q      Yes - must be hours at work code lvl         
         OI    X#SC3NVL,ESTSWCTM   Set WC level time estimate                   
                                                                                
PSTWCD02 MVI   ERDTYP,ERDTWDQ                                                   
                                                                                
         MVC   ERDWCAT,RQUWCAT                                                  
         MVC   ERDWCOD,RQUWCOD                                                  
         MVC   ERDWNAM,X#WORKN                                                  
                                                                                
         GOTOR GETWCSEQ            Get workcode sequence #                      
         MVC   ERDWSEQ,HALF1                                                    
                                                                                
                                                                                
         ZAP   ERDWAMT,X#WAMT1                                                  
         ZAP   ERDWFCA,X#WAMT2                                                  
         ZAP   ERDWCAM,X#WAMT3                                                  
         ZAP   ERDWCFC,X#WAMT4                                                  
         ZAP   ERDWCRA,X#WRATE                                                  
*&&US                                                                           
         CLC   SJACCNT,SPACES      Any client/prod/job?                         
         JNH   PSTWCD04                                                         
         ZAP   DUB2,PZERO                                                       
         GOTOR GETCOM              Get current commission                       
         ZAP   ERDWCRA,DUB2                                                     
PSTWCD04 DS    0H                                                               
*&&                                                                             
         CVB   R1,X#WVATR                                                       
         STCM  R1,3,ERDWVRA                                                     
         ZAP   ERDWVAM,X#WVATA                                                  
         ZAP   ERDWVFC,X#WVATF                                                  
         ZAP   ERDWNIC,X#WNICA                                                  
         ZAP   ERDWNFC,X#WNICF                                                  
         MVC   ERDWVCO,X#WVATC                                                  
         MVI   ERDWIND,0                                                        
                                                                                
         AP    X#TOTAL,ERDWAMT                                                  
         AP    X#TOTFC,ERDWFCA                                                  
         AP    X#TOTCA,ERDWCAM                                                  
         AP    X#TOTCF,ERDWCFC                                                  
         AP    X#TOTVA,ERDWVAM                                                  
         AP    X#TOTVF,ERDWVFC                                                  
         CLI   X#SVINTW,YESQ       Time (internal) workcode?                    
         JNE   *+14                                                             
         AP    X#TOTIW,ERDWAMT                                                  
         J     *+10                                                             
         AP    X#TOTXW,ERDWAMT                                                  
                                                                                
         CLI   RQUWIYN,YESQ                                                     
         JNE   PSTWCD06                                                         
         OI    ERDWIND,ERDWIIQ                                                  
                                                                                
PSTWCD06 CLI   RQUWCYN,YESQ        Commission?                                  
         JNE   PSTWCD08                                                         
         OI    ERDWIND,ERDWICQ     Set commission                               
                                                                                
PSTWCD08 CLI   X#SVINTW,YESQ       Time (internal) workcode?                    
         JNE   PSTWCD12                                                         
         OI    ERDWIND,ERDWITQ     Set internal workcode flag                   
                                                                                
PSTWCD12 CLI   RQUWXYN,YESQ        Contingency?                                 
         JNE   PSTWCD14                                                         
         OI    ERDWIND,ERDWIXQ     Set contingency                              
         MVC   ERDWCCC,RQUWCCC                                                  
         ZAP   ERDWCCR,X#WESPR                                                  
                                                                                
PSTWCD14 CLI   RQUWMYN,YESQ                                                     
         JNE   PSTWCD16                                                         
         OI    ERDWIND,ERDWIMQ                                                  
                                                                                
PSTWCD16 CLI   RQUWOYN,YESQ                                                     
         JNE   PSTWCD18                                                         
         OI    ERDWIND,ERDWIOQ                                                  
                                                                                
PSTWCD18 CLI   RQUWIAC,YESQ                                                     
         JNE   PSTWCD20                                                         
         OI    ERDWIND,ERDWIAC                                                  
                                                                                
PSTWCD20 CP    X#IMULT,PZERO       Have we got a multiplier                     
         JE    PSTWCD40            No                                           
         ZAP   ERDWRAT,X#IAMT1     Set hourly rate                              
         ZAP   ERDWRATF,X#IAMT2    Set foreign currency amount                  
         ZAP   ERDWHRS,X#IMULT     Set number of hours                          
         AP    X#TOHRS,ERDWHRS     Add hours to total                           
         CLI   RQUIOVR,YESQ        Is hourly rate overridden                    
         JNE   PSTWCD22            No                                           
         OI    ERDWIND2,ERDWTOQ    Set overridden                               
PSTWCD22 CP    X#IAMT2,PZERO       Have we got a foreign currency rate          
         JE    PSTWCD40            No                                           
         OI    ERDWIND2,ERDWTFQ    Set foreign currency set                     
                                                                                
PSTWCD40 GOTOR PUTSAR,NEWBUF                                                    
         J     EXIT                CC set                                       
         DROP  R3                                                               
                                                                                
*&&US                                                                           
* Get Commission Amount                                                         
         USING ESTRECD,R2                                                       
         USING GOBLOCKD,R3                                                      
GETCOM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETCOM*'                                                      
         L     R2,AIO2                                                          
***      L     RE,AGOBLOCK         call GETOPT for scheme code                  
         L     RE,AGOBLOCB         call GETOPT for scheme code                  
         LA    RF,GOBLOCKX-GOBLOCK                                              
         XCEF                                                                   
***      L     R3,AGOBLOCK                                                      
         L     R3,AGOBLOCB                                                      
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOSELCUL(1),CUXCPY                                               
         MVC   GOSELCUL+1(2),PRODUL                                             
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELCLI(L'SJACLIC),SJACLIC                                      
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELPRO(L'SJAPROC),SJAPROC                                      
         MVC   GOSELJOB,SJAJOBC                                                 
         MVC   GOSELMED,SJAJOBC                                                 
         CLC   SJAJOBC,SPACES      Do we have a job?                            
         JH    GETCOM10                                                         
         CLI   X#BPMED,C' '        Do we have a media code?                     
         JNH   GETCOMX                                                          
         MVC   GOSELMED,X#BPMED                                                 
GETCOM10 MVC   GOSELWC,RQUWCOD                                                  
         MVI   GOANYWC,YESQ                                                     
         MVI   GOWHICH,0                                                        
         GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
         ZAP   DUB2,GOAGYCOM                                                    
                                                                                
GETCOMX  XIT1                                                                   
         DROP  R2,R3                                                            
                                                                                
*&&                                                                             
                                                                                
* Validate item data                                                            
VALITM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALITM*'                                                      
                                                                                
         ZAP   X#IAMT1,PZERO                                                    
         ZAP   X#IAMT2,PZERO                                                    
         ZAP   X#IMULT,PZERO                                                    
         ZAP   X#INICA,PZERO                                                    
         ZAP   X#INICF,PZERO                                                    
         MVC   X#ISEON,RQUITSO                                                  
         CLI   RQUITYP,C'2'        test material or person rate/hours           
         JNE   VALITM05                                                         
         MVC   X#I1RAC,RQUI1RA                                                  
         GOTO1 VHEXIN,DMCB,RQUISEQ,X#ISEQN,6                                    
         J     VALITM20                                                         
                                                                                
VALITM05 MVC   X#ICODE,RQUINUM                                                  
         MVC   X#IDESC,RQUIDES                                                  
         MVI   X#ISTAT,0                                                        
         MVI   X#ISTA2,0                                                        
                                                                                
         OC    X#ICODE,SPACES                                                   
         CLC   X#ICODE,SPACES                                                   
         JE    VALITM20                                                         
                                                                                
         GOTO1 VHEXIN,DMCB,RQUISEQ,X#ISEQN,6                                    
                                                                                
         USING PASRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    PASKEY,PASKEY                                                    
         MVI   PASKTYP,PASKTYPQ                                                 
         MVI   PASKSUB,PASKSQ                                                   
         MVC   PASKCPY,CUXCPY                                                   
         MVC   PASKSEQ,X#ISEQN                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    VALITM10                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$PRICE)                                           
         J     EXITN                                                            
                                                                                
VALITM10 TM    PASKSTAT,ARTKLOCK   locked?                                      
         JZ    VALITM15                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$IENLV)                                           
         J     EXITN                                                            
                                                                                
VALITM15 MVC   X#ISTAT,PASKSTAT    any more validation?                         
         MVC   X#ISTA2,PASKSTA2                                                 
                                                                                
VALITM20 CLC   RQUIPRI,SPACES                                                   
         JNH   VALITM25                                                         
         MVC   TEMP2(16),RQUIPRI   item price/hourly rate                       
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#IAMT1,TEMP2+16(8)                                              
                                                                                
VALITM25 CLC   RQUIFCP,SPACES                                                   
         JNH   VALITM30                                                         
         MVC   TEMP2(16),RQUIFCP                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#IAMT2,TEMP2+16(8)                                              
                                                                                
VALITM30 CLC   RQUIMUL,SPACES                                                   
         JNH   VALITM35                                                         
         MVC   TEMP2(16),RQUIMUL   item multiplier/n'hours                      
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#IMULT,TEMP2+16(8)                                              
                                                                                
VALITM35 CLC   RQUINIC,SPACES                                                   
         JNH   VALITM40                                                         
         MVC   TEMP2(16),RQUINIC                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#INICA,TEMP2+16(8)                                              
                                                                                
VALITM40 CLC   RQUINFC,SPACES                                                   
         JNH   VALITM45                                                         
         MVC   TEMP2(16),RQUINFC                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#INICF,TEMP2+16(8)                                              
                                                                                
VALITM45 DS    0H                  any amount validation/calculation?           
*&&US                                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING ARTRECD,R2                                                       
         L     R2,AIO1                                                          
                                                                                
         CLC   ARTKOFF,SPACES      Do we have an office?                        
         JNH   EXITY                                                            
         CLC   ARTKOFF,X#SJOFF     Do we have the right office?                 
         JE    EXITY                                                            
                                                                                
         MVC   ROUERRV,=AL2(AE$IENLV)                                           
         J     EXITN                                                            
         DROP  R2                                                               
*&&                                                                             
                                                                                
         J     EXITY                                                            
                                                                                
* Post item data                                                                
                                                                                
         USING ERDELD,R3                                                        
PSTITM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PSTITM*'                                                      
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ERDEL,ERDELQ                                                     
         CLI   RQUITYP,C'1'        test Materials item                          
         JNE   PSTITM10            else Hours                                   
         MVI   ERDLN,ERDILNQ                                                    
         MVI   ERDTYP,ERDTIDQ                                                   
         OI    X#SC3NVL,ESTSITMS   Set estimate contains items                  
                                                                                
         MVC   ERDICOD,X#ICODE                                                  
         CLC   X#ICODE,SPACES                                                   
         JE    PSTITM2                                                          
         MVC   ERDISEQ,X#ISEQN                                                  
                                                                                
PSTITM2  MVC   ERDIDES,X#IDESC                                                  
         ZAP   ERDIAPR,X#IAMT1                                                  
         ZAP   ERDIFPR,X#IAMT2                                                  
         ZAP   ERDIMUL,X#IMULT                                                  
         ZAP   ERDINIC,X#INICA                                                  
         ZAP   ERDINFC,X#INICF                                                  
         MVI   ERDIIND,0                                                        
                                                                                
PSTITM6  CLI   RQUIOVR,YESQ                                                     
         JNE   PSTITM8                                                          
         OI    ERDIIND,ERDIIOQ                                                  
                                                                                
PSTITM8  TM    X#ISTAT,ARTKDAMQ                                                 
         JZ    *+8                                                              
         OI    ERDIIND,ERDIIDQ                                                  
                                                                                
         TM    X#ISTAT,ARTKFLXQ                                                 
         JZ    *+8                                                              
         OI    ERDIIND,ERDIIFQ                                                  
                                                                                
         TM    X#ISTAT,ARTKNOPQ                                                 
         JZ    *+8                                                              
         OI    ERDIIND,ERDIIPQ                                                  
                                                                                
         TM    X#ISTA2,ARTKSTIM                                                 
         JZ    *+8                                                              
         OI    ERDIIND,ERDIITQ                                                  
         J     PSTITM20                                                         
                                                                                
PSTITM10 MVI   ERDLN,ERDITLQ       person/rate/hours item data                  
         MVI   ERDTYP,ERDTIRQ                                                   
         OI    X#SC3NVL,ESTSITTM   Set item level time estimate                 
                                                                                
         MVC   ERDITAC,X#I1RAC     Off/dept/sub/person                          
         MVC   ERDIRAT,X#IAMT1     hourly rate                                  
         ZAP   ERDIHRS,X#IMULT     n'hours                                      
         AP    X#TOHRS,ERDIHRS     Add hours to total                           
*                                                                               
         CLI   RQUIOVR,YESQ                                                     
         JNE   *+8                                                              
         OI    ERDITND,ERDITOQ     set rate overridden by user                  
         ZAP   ERDIRAF,PZERO                                                    
         CLC   RQUIFCP,SPACES                                                   
         JNH   *+14                                                             
         ZAP   ERDIRAF,X#IAMT2     set foreign currency rate                    
         OI    ERDITND,ERDITFQ     set foreign currency rate ind                
                                                                                
         J     PSTITM20                                                         
                                                                                
PSTITM20 GOTOR PUTSAR,NEWBUF                                                    
         J     EXIT                CC set                                       
         DROP  R3                                                               
                                                                                
* Transfer PID element and amend on maintain - or add on create                 
         USING ESTRECD,R2                                                       
         USING PIDELD,R3                                                        
XOAPID   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*XOAPID*'                                                      
                                                                                
         CLI   X#ACTION,RQUGACQ    create?                                      
         JE    XOAPID60                                                         
                                                                                
         L     R2,AIO2                                                          
         LA    R3,ESTRFST                                                       
         LA    R4,ELEMENT                                                       
                                                                                
XOAPID05 XR    R0,R0               (first element is EMD)                       
         IC    R0,PIDLN                                                         
         AR    R3,R0                                                            
         CLI   PIDEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PIDEL,PIDELQ                                                     
         JNE   XOAPID05                                                         
         CLI   PIDTYPE,PIDTOWN                                                  
         JNE   XOAPID05                                                         
                                                                                
         LLC   R1,PIDLN            copy PID element                             
         SHI   R1,1                                                             
         XC    ELEMENT,ELEMENT                                                  
         BASR  RE,0                                                             
         MVC   ELEMENT(0),PIDEL                                                 
         EX    R1,0(RE)                                                         
                                                                                
         OC    CCTPID,CCTPID                                                    
         JZ    XOAPID20                                                         
                                                                                
         XR    R1,R1               check whether user already owner             
         IC    R1,PIDNTR#                                                       
         LA    RF,PIDOWNR                                                       
                                                                                
XOAPID10 CLC   CCTPID,0(RF)                                                     
         JE    XOAPID20                                                         
         AHI   RF,L'PIDOWNR                                                     
         JCT   R1,XOAPID10                                                      
                                                                                
         XR    R1,R1                                                            
         IC    R1,PIDNTR#          increment counter                            
         AHI   R1,1                                                             
         CHI   R1,PIDNTRM                                                       
         JNH   XOAPID15                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$TMUCS)                                           
         J     EXITN                                                            
                                                                                
XOAPID15 LA    R3,ELEMENT                                                       
         STC   R1,PIDNTR#                                                       
         XR    R1,R1                                                            
         IC    R1,PIDLN                                                         
         LA    RF,PIDELD(R1)                                                    
         MVC   0(L'PIDOWNR,RF),CCTPID                                           
         AHI   R1,L'PIDOWNR                                                     
         STC   R1,PIDLN                                                         
                                                                                
XOAPID20 OC    X#SAPPR,X#SAPPR                                                  
         JZ    XOAPID35                                                         
         CLC   X#SAPPR,CCTPID                                                   
         JE    XOAPID35                                                         
                                                                                
         XR    R1,R1               check whether EMDSCA already owner           
         IC    R1,PIDNTR#                                                       
         LA    RF,PIDOWNR                                                       
                                                                                
XOAPID25 CLC   X#SAPPR,0(RF)                                                    
         JE    XOAPID35                                                         
         AHI   RF,L'PIDOWNR                                                     
         JCT   R1,XOAPID25                                                      
                                                                                
         XR    R1,R1                                                            
         IC    R1,PIDNTR#          increment counter                            
         AHI   R1,1                                                             
         CHI   R1,PIDNTRM                                                       
         JNH   XOAPID30                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$TMUCS)                                           
         J     EXITN                                                            
                                                                                
XOAPID30 LA    R3,ELEMENT                                                       
         STC   R1,PIDNTR#                                                       
         XR    R1,R1                                                            
         IC    R1,PIDLN                                                         
         LA    RF,PIDELD(R1)                                                    
         MVC   0(L'PIDOWNR,RF),X#SAPPR                                          
         AHI   R1,L'PIDOWNR                                                     
         STC   R1,PIDLN                                                         
                                                                                
XOAPID35 OC    X#SIAPP,X#SIAPP                                                  
         JZ    XOAPID50                                                         
         CLC   X#SIAPP,CCTPID                                                   
         JE    XOAPID50                                                         
         CLC   X#SIAPP,X#SAPPR                                                  
         JE    XOAPID50                                                         
                                                                                
         XR    R1,R1               check whether EMDSIA already owner           
         IC    R1,PIDNTR#                                                       
         LA    RF,PIDOWNR                                                       
                                                                                
XOAPID40 CLC   X#SIAPP,0(RF)                                                    
         JE    XOAPID50                                                         
         AHI   RF,L'PIDOWNR                                                     
         JCT   R1,XOAPID40                                                      
                                                                                
         XR    R1,R1                                                            
         IC    R1,PIDNTR#          increment counter                            
         AHI   R1,1                                                             
         CHI   R1,PIDNTRM                                                       
         JNH   XOAPID45                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$TMUCS)                                           
         J     EXITN                                                            
                                                                                
XOAPID45 LA    R3,ELEMENT                                                       
         STC   R1,PIDNTR#                                                       
         XR    R1,R1                                                            
         IC    R1,PIDLN                                                         
         LA    RF,PIDELD(R1)                                                    
         MVC   0(L'PIDOWNR,RF),X#SIAPP                                          
         AHI   R1,L'PIDOWNR                                                     
         STC   R1,PIDLN                                                         
                                                                                
XOAPID50 GOTOR PUTSAR,NEWBUF                                                    
         J     EXIT                                                             
                                                                                
XOAPID60 L     R2,AIO2                                                          
         LA    R3,ELEMENT                                                       
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ+L'PIDOWNR                                           
         MVI   PIDNTR#,1                                                        
         MVI   PIDTYPE,PIDTOWN                                                  
         MVC   PIDOWNR,CCTPID                                                   
                                                                                
XOAPID80 GOTOR PUTSAR,NEWBUF                                                    
         J     EXIT                CC set                                       
         DROP  R2,R3                                                            
                                                                                
* Transfer electronic signature elements on maintain                            
                                                                                
         USING ESTRECD,R2                                                       
         USING GDAELD,R3                                                        
XELSIG   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*XELSIG*'                                                      
                                                                                
         CLI   X#ACTION,RQUGACQ    skip on create                               
         JE    EXITY                                                            
         CLI   X#STATUS,RQUGA2Q    Or recall electronic signature               
         JE    EXITY                                                            
                                                                                
         L     R2,AIO2                                                          
         LA    R3,ESTRFST                                                       
         LA    R4,ELEMENT                                                       
                                                                                
XELSIG2  XR    R0,R0               (first element is EMD)                       
         IC    R0,GDALN                                                         
         AR    R3,R0                                                            
         CLI   GDAEL,0                                                          
         JE    EXITY                                                            
         CLI   GDAEL,GDAELQ                                                     
         JE    XELSIG4                                                          
         CLI   GDAEL,FFTELQ                                                     
         JNE   XELSIG2                                                          
                                                                                
XELSIG4  XR    R1,R1               copy general data and free form              
         IC    R1,GDALN                          text elements                  
         SHI   R1,1                                                             
         XC    ELEMENT,ELEMENT                                                  
         BASR  RE,0                                                             
         MVC   ELEMENT(0),GDAEL                                                 
         EX    R1,0(RE)                                                         
                                                                                
         GOTOR PUTSAR,NEWBUF                                                    
         JE    XELSIG2                                                          
         J     EXITN               CC set                                       
         DROP  R2,R3                                                            
                                                                                
* Transfer EMS elements on maintain                                             
                                                                                
         USING ESTRECD,R2                                                       
         USING EMSELD,R3                                                        
XFREMS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*XFREMS*'                                                      
                                                                                
         CLI   X#ACTION,RQUGACQ    skip on create                               
         JE    EXITY                                                            
                                                                                
         L     R2,AIO2                                                          
         LA    R3,ESTRFST                                                       
         LA    R4,ELEMENT                                                       
                                                                                
XFREMS2  XR    R0,R0               (first element is EMD)                       
         IC    R0,EMSLN                                                         
         AR    R3,R0                                                            
         CLI   EMSEL,0                                                          
         JE    EXITY                                                            
         CLI   EMSEL,EMSELQ                                                     
         JNE   XFREMS2                                                          
                                                                                
         XR    R1,R1               copy PID element                             
         IC    R1,EMSLN                                                         
         SHI   R1,1                                                             
         XC    ELEMENT,ELEMENT                                                  
         BASR  RE,0                                                             
         MVC   ELEMENT(0),EMSEL                                                 
         EX    R1,0(RE)                                                         
                                                                                
         GOTOR PUTSAR,NEWBUF                                                    
         JE    XFREMS2                                                          
         J     EXITN               CC set                                       
         DROP  R2,R3                                                            
                                                                                
* Add EMS elements on add                                                       
                                                                                
         USING ESTRECD,R2                                                       
         USING EMSELD,R3                                                        
ADDEMS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ADDEMS*'                                                      
                                                                                
         CLI   X#ACTION,RQUGACQ    skip unless create                           
         JNE   EXITY                                                            
                                                                                
ADDEMS0  L     R4,AFREEST          source buffer                                
                                                                                
ADDEMS2  CLI   0(R4),X'40'         end of buffer                                
         JNH   EXITY                                                            
                                                                                
         LA    R3,ELEMENT                                                       
         MVI   EMSEL,EMSELQ                                                     
         MVI   EMSLN,EMSLNQ                                                     
         MVC   EMSEST,0(R4)                                                     
         OI    EMSIND,EMSICQ                                                    
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ESTRECD,EMSELD,ADDEND,0                
         CLI   12(R1),0                                                         
         JE    ADDEMS8                                                          
         DC    H'0'                                                             
                                                                                
ADDEMS8  AHI   R4,L'EGNPNUM                                                     
         J     ADDEMS2                                                          
         DROP  R2,R3                                                            
                                                                                
                                                                                
* Save, validate and post xtra data                                             
         USING XDFELD,R3                                                        
SVPXDF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SVPXDF*'                                                      
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
                                                                                
         MVI   XDFEL,XDFELQ                                                     
         GOTO1 VHEXIN,DMCB,RQUXCOD,XDFOCOD,6                                    
         MVC   XDFOTYP,RQUXTYP                                                  
                                                                                
         CLI   RQUXTYP,XDFEDDQ      date?                                       
         JE    SVPXDF30                                                         
         CLI   RQUXTYP,XDFEDAQ      amount?                                     
         JE    SVPXDF40                                                         
         CLI   RQUXTYP,XDFEDCQ      character                                   
         JE    SVPXDF10                                                         
         CLI   RQUXTYP,XDFEDNQ      or number string?                           
         JE    SVPXDF10                                                         
         CLI   RQUXTYP,XDFEDYQ      yes/no                                      
         JE    SVPXDF20                                                         
         CLI   RQUXTYP,XDFEDXQ      dropdown list                               
         JE    SVPXDF10                                                         
         DC    H'0'                                                             
                                                                                
SVPXDF10 LA    RE,XDFOLNQ                                                       
         LA    RF,RQUXDTA+L'RQUXDTA-1                                           
         LA    R1,L'RQUXDTA                                                     
SVPXDF12 CLI   0(RF),C' '                                                       
         JH    SVPXDF14                                                         
         SHI   RF,1                                                             
         JCT   R1,SVPXDF12                                                      
         DC    H'0'                empty data string is invalid                 
SVPXDF14 AR    RE,R1                                                            
         STC   RE,XDFLN                                                         
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   XDFODTA(0),RQUXDTA                                               
         EX    R1,0(RE)                                                         
         J     SVPXDF90                                                         
                                                                                
SVPXDF20 LA    RE,XDFOLNQ+1                                                     
         STC   RE,XDFLN                                                         
         MVC   XDFODTA(1),RQUXDTA                                               
         J     SVPXDF90                                                         
                                                                                
SVPXDF30 LA    RE,XDFOLNQ+3                                                     
         STC   RE,XDFLN                                                         
         GOTOR VDATCON,DMCB,(0,RQUXDTA+2),(1,XDFODTA)                           
         J     SVPXDF90                                                         
                                                                                
SVPXDF40 LA    RE,XDFOLNQ+6                                                     
         STC   RE,XDFLN                                                         
         MVC   TEMP2(16),RQUXDTA                                                
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   XDFODTA(6),TEMP2+16(8)                                           
                                                                                
SVPXDF90 DS    0H                  pass element to TSAR                         
                                                                                
         GOTOR PUTSAR,NEWBUF                                                    
         J     EXIT                CC set                                       
         DROP  R3                                                               
                                                                                
* Save, validate and post category data                                         
         USING ERDELD,R3                                                        
SVPCAT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SVPCAT*'                                                      
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
                                                                                
         MVI   ERDEL,ERDELQ                                                     
         MVI   ERDLN,ERDCLNQ                                                    
         MVI   ERDTYP,ERDTCSQ                                                   
                                                                                
         MVC   ERDCCOD,RQUCCOD     pass details                                 
         MVC   ERDCNAM,RQUCNAM                                                  
         MVC   ERDCTNA,RQUCTNA                                                  
         MVC   ERDCINS,RQUCINS                                                  
         MVC   ERDCTYP,RQUCTYP                                                  
         MVC   ERDCELS,RQUCELS                                                  
                                                                                
SVPCAT2  GOTOR PUTSAR,NEWBUF                                                    
         J     EXIT                CC set                                       
         DROP  R3                                                               
                                                                                
* Process PID owner element                                                     
         USING ESTRECD,R2                                                       
         USING PIDELD,R3                                                        
PROPID   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PROPID*'                                                      
                                                                                
         L     R2,AIO2                                                          
         LA    R3,ESTRFST                                                       
         XR    R0,R0                                                            
                                                                                
PROPID05 CLI   PIDEL,0             look for existing PID element                
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PIDEL,PIDELQ                                                     
         JNE   PROPID10                                                         
         CLI   PIDTYPE,PIDTOWN                                                  
         JE    PROPID12                                                         
                                                                                
PROPID10 IC    R0,PIDLN                                                         
         AR    R3,R0                                                            
         J     PROPID05                                                         
                                                                                
PROPID12 XR    R1,R1               look whether user is owner already           
         IC    R1,PIDNTR#                                                       
         LA    RF,PIDOWNR                                                       
         MVI   X#OWNER,0                                                        
                                                                                
         OC    CCTPID,CCTPID                                                    
         JNZ   *+8                                                              
         OI    X#OWNER,X'08'                                                    
                                                                                
         OC    X#SAPPR,X#SAPPR                                                  
         JZ    PROPID14                                                         
         CLC   CCTPID,X#SAPPR                                                   
         JNE   *+8                                                              
PROPID14 OI    X#OWNER,X'80'                                                    
                                                                                
         OI    X#OWNER,X'01'                                                    
         TM    G#OFSTA2,OFFSIAEQ                                                
         JZ    PROPID20                                                         
         OC    X#SIAPP,X#SIAPP                                                  
         JZ    PROPID20                                                         
         CLC   X#SIAPP,X#SAPPR                                                  
         JE    PROPID20                                                         
         CLC   CCTPID,X#SIAPP                                                   
         JE    PROPID20                                                         
         NI    X#OWNER,X'FF'-X'01'                                              
                                                                                
PROPID20 TM    X#OWNER,X'08'                                                    
         JNZ   PROPID25                                                         
         CLC   CCTPID,0(RF)                                                     
         JNE   PROPID25                                                         
         OI    X#OWNER,X'08'                                                    
                                                                                
PROPID25 TM    X#OWNER,X'80'                                                    
         JNZ   PROPID27                                                         
         CLC   X#SAPPR,0(RF)                                                    
         JNE   PROPID27                                                         
         OI    X#OWNER,X'80'                                                    
                                                                                
PROPID27 TM    X#OWNER,X'01'                                                    
         JNZ   PROPID30                                                         
         CLC   X#SIAPP,0(RF)                                                    
         JNE   PROPID30                                                         
         OI    X#OWNER,X'01'                                                    
                                                                                
PROPID30 AHI   RF,L'PIDOWNR                                                     
         JCT   R1,PROPID20                                                      
                                                                                
         TM    X#OWNER,X'80'+X'08'+X'01'                                        
         JO    EXITY                                                            
                                                                                
         XR    RE,RE                                                            
         TM    X#OWNER,X'80'                                                    
         JNZ   *+8                                                              
         AHI   RE,1                                                             
         TM    X#OWNER,X'08'                                                    
         JNZ   *+8                                                              
         AHI   RE,1                                                             
         TM    X#OWNER,X'01'                                                    
         JNZ   *+8                                                              
         AHI   RE,1                                                             
                                                                                
PROPID35 XR    R4,R4                                                            
         IC    R4,PIDNTR#          increment counter                            
         AR    R4,RE                                                            
         ST    RE,SAVERE                                                        
         CHI   R4,PIDNTRM          Ensure counter doesn't go over max           
         JH    EXITN                                                            
                                                                                
         XC    ELEMENT,ELEMENT     copy PID element                             
         IC    R1,PIDLN                                                         
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   ELEMENT(0),PIDEL                                                 
         EX    R1,0(RE)                                                         
                                                                                
         MVI   PIDEL,FF            delete PIDEL                                 
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',ESTRECD),0                      
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,ELEMENT          set new length                               
         L     RE,SAVERE                                                        
         MHI   RE,L'PIDOWNR                                                     
         XR    R1,R1                                                            
         IC    R1,PIDLN                                                         
         LR    RF,R1                                                            
         LA    RF,PIDELD(RF)                                                    
         AR    R1,RE                                                            
         STC   R1,PIDLN                                                         
                                                                                
         STC   R4,PIDNTR#          save number and add data                     
         TM    X#OWNER,X'08'       Do we have the connected user                
         JNZ   PROPID40            Yes                                          
         MVC   0(L'PIDOWNR,RF),CCTPID  No - add them to the element             
         AHI   RF,L'PIDOWNR                                                     
                                                                                
PROPID40 TM    X#OWNER,X'80'       Do we have the client approver               
         JNZ   PROPID42            Yes                                          
         MVC   0(L'PIDOWNR,RF),X#SAPPR  No - add them to the element            
         AHI   RF,L'PIDOWNR                                                     
                                                                                
PROPID42 TM    X#OWNER,X'01'       Do we have the internal approver             
         JNZ   PROPID44            Yes                                          
         MVC   0(L'PIDOWNR,RF),X#SIAPP  No - add them to the element            
         AHI   RF,L'PIDOWNR                                                     
                                                                                
PROPID44 LA    RF,=C'ADD=END'                                                   
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ESTRECD,PIDELD,(RF),0                  
         CLI   12(R1),0                                                         
         J     EXIT                return CC                                    
         DROP  R2,R3                                                            
*                                                                    *          
*&&US                                                                *          
**********************************************************************          
* For USA only changes :-                                            *          
* Read all the records for the job. Remove Current estimate status   *          
* from old estimate and update it on new the estimate record         *          
* *******                                                            *          
* Current Estimate : -                                               *          
* Current estimate is defined as the highest sequenced estimate for  *          
* a job that is approved, if no approved estimates exist for the job *          
* it's the highest submitted sequenced estimate. Sequenced is the    *          
* local numer.                                                       *          
*                                                                    *          
**********************************************************************          
         USING ESTRECD,R2                                                       
UPDCES   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDCES*'                                                      
*                                                                               
         MVI   X#ESTST3,0            INITALIZE FOR STATUS BYTE WORK             
         MVI   X#ESTCST,0            INITALIZE INDICATOR                        
         XC    CSVKEY3,CSVKEY3       CLEAR WORK FOR KEY DETAILS                 
*                                                                               
         LA    R2,IOKEY              ADDRESS WORK AREA                          
         XC    IOKEY,IOKEY           CLEAR KEY WORK AREA                        
         MVC   ESTKEY(ESTKLNO-ESTRECD),CSVKEY2  READ ESTIMATE                   
*                           ## READ ESTIMATE RECORD FOR SAME JOB ##             
         L     R1,=AL4(IOHIUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UPDCES04                                                         
         DC    H'0'                                                             
*                           ## READ SEQ RECORD FOR STATUS UPDATE  ##            
UPDCES02 L     R1,=AL4(IOSQUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
*                                                                               
UPDCES04 CLC   ESTKEY(ESTKLNO-ESTRECD),CSVKEY2  SAME KEY, NO FINAL              
         JNE   UPDCES30                         - UPDATE                        
*                                                                               
         TM    ESTKSTA1,ESTKCAPP     CLIENT APPROVED ?                          
         JZ    UPDCES10              NO, CONTINUE                               
*                                                                               
         OI    X#ESTCST,X#ESTASV     SET  APPROVED RECORD KEY SAVED             
         MVC   CSVKEY3,IOKEY         SAVE KEY DETAILS FOR STATUS CHANGE         
         J     UPDCES02              READ NEXT SEQUENTIAL RECORD                
*                                                                               
UPDCES10 TM    ESTKSTA1,ESTKSUBM     CLIENT SUBMITTED ?                         
         JZ    UPDCES02              NO, PROCESS NEXT SEQUENTIAL                
*                                    YES                                        
         TM    X#SCNVAL,ESTKCAPP     ARE WE HERE FOR APPROVE REQUEST?           
         JO    UPDCES02              YES, DON'T SAVE SUBMITTED KEY              
*                                                                               
         TM    X#ESTCST,X#ESTASV     ANY APPROVED KEY SAVED ?                   
         JO    UPDCES02              YES, DON'T SAVE SUBMITTED KEY              
*                                    NO                                         
         MVC   CSVKEY3,ESTKEY        SAVE KEY DETAILS FOR STATUS CHANGE         
         J     UPDCES02              READ NEXT SEQUENTIAL RECORD                
*                                                                               
UPDCES30 OC    CSVKEY3,CSVKEY3       ANY OLD APPROVED/SUBMITTED FOUND?          
         JZ    UPDCES34              NO , SET CE STATUS ON CURRENT EST          
*                                                                               
         CLC   CSVKEY2(ESTKSEQ-ESTRECD),CSVKEY3 SAME INPUT KEY ?                
         JE    EXIT                  - YES, ALREADY MARKED CE, EXIT             
*                                    NO, MARK CURRENT AS CE                     
         OI    X#ESTCST,X#ESTUCE     UNSET CE STATUS FROM OLD REC               
         TM    X#SCNVAL,ESTKCAPP     ARE WE HERE FOR APPROVE REQUEST?           
         JO    UPDCES32              YES, COMPARE KEY FOR APPROVE               
*                                    NO, FOR SUBMITTED                          
         TM    X#ESTCST,X#ESTASV     CHECK ANY APPROVED KEY SAVED ?             
         JO    EXIT                  YES, ALREADY CE MARKED                     
*                                    NO                                         
UPDCES32 CLC   CSVKEY2(ESTKSEQ-ESTRECD),CSVKEY3 APPROVED REC FOUND WITH         
         JL    EXIT                  - HIGHEST LOCAL NUMBER? EXIT               
*                                    NO, MARK CURRENT AS CE                     
UPDCES34 OI    X#SC3NVL,ESTSCEST     SET CE STATUS ON CURRENT ESTIMATE          
         TM    X#ESTCST,X#ESTUCE     UNSET CE STATUS FROM OLD REC ?             
         JZ    EXIT                  NO, EXIT                                   
*                                    YES                                        
UPDCES40 XC    IOKEY,IOKEY           CLEAR KEY WORK AREA                        
         MVC   ESTKEY(ESTKSEQ-ESTRECD),CSVKEY3  READ MAIN ESTIMATE              
*                           ## READ ESTIMATE MAIN RECORD FOR THE KEY ##         
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
*                                    NO, SET IT NOW                             
         MVC   X#ESTST3,ESTKSTA3     SAVE STATUS 3 BYTES                        
         NI    X#ESTST3,X'FF'-ESTSCEST UNSET CURRENT ESTIMATE STATUS            
*                           ##  Read master for status update ##                
UPDCES50 L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1                                                          
         MVC   ESTRSTA3,X#ESTST3 UPDATE STATUS BYTE 3 DETAILS                   
*                           ## UPDATE MASTER RECORD  ##                         
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                           ## UPDATE DIRE TORY RECORD ##                       
         LA    R2,IOKEY                                                         
         MVC   ESTKSTA3,X#ESTST3 UPDATE STATUS BYTE 3 DETAILS                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         JE    EXIT                                                             
         DC    H'00'                                                            
*                                                                               
         DROP  R2                                                               
*&&                                                                             
                                                                                
* Delete all estimate records on physical deletion                              
         USING ESTRECD,R2                                                       
DELALL   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DELALL '                                                      
                                                                                
         L     R2,AIO2                                                          
         OI    ESTRSTA1,ESTKDELT                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,IOKEY                                                         
         MVC   ESTKEY,CSVKEY2                                                   
         OI    ESTKSTA1,ESTKDELT                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    IOKEY,IOKEY                                                      
         MVC   ESTKEY,CSVKEY2                                                   
         MVI   ESTKSEQ,1                                                        
                                                                                
         L     R1,=AL4(IOHIUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    DELALL4                                                          
         DC    H'0'                                                             
                                                                                
DELALL2  L     R1,=AL4(IOSQUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    DELALL4                                                          
         DC    H'0'                                                             
                                                                                
DELALL4  CLC   ESTKEY(ESTKSEQ-ESTRECD),CSVKEY2                                  
         JNE   EXIT                                                             
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO2                                                          
         OI    ESTRSTA1,ESTKDELT                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,IOKEY                                                         
         OI    ESTKSTA1,ESTKDELT                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JE    DELALL2                                                          
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
* Delete all audit records when estimate is deleted                             
         USING ESTRECD,R3                                                       
         USING AUDRECD,R2                                                       
DELAUD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DELAUD '                                                      
                                                                                
         CLI   X#SCNVAL,ESTKDELT   only on physical delete                      
         JNE   *+2                                                              
                                                                                
         LA    R3,CSVKEY2          estimate record key                          
         LA    R2,IOKEY            audit record key                             
                                                                                
         XC    IOKEY,IOKEY                                                      
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,ESTKCPY                                                  
         MVI   AUDKAUDT,AUDKEST                                                 
                                                                                
         LA    R5,TEMP                                                          
         MVC   TEMP(12),SPACES                                                  
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,R5),ESTKCLI                                                  
         EX    RF,0(R1)                                                         
         AHI   RF,1                                                             
         AR    R5,RF                                                            
         LLC   RE,PPROLEN                                                       
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,R5),ESTKPRO                                                  
         EX    RE,0(R1)                                                         
         AHI   RE,1                                                             
         AR    R5,RE                                                            
         LHI   RF,L'ESTKJOB-1                                                   
         BASR  R1,0                                                             
         MVC   0(0,R5),ESTKJOB                                                  
         EX    RF,0(R1)                                                         
                                                                                
         MVC   AUDKECPJ,TEMP                                                    
         MVC   AUDKELNO,ESTKLNO                                                 
         MVC   CSVKEY1,AUDKEY                                                   
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                 (missing audit record)                       
         J     DELAUD4                                                          
                                                                                
DELAUD2  L     R1,=AL4(IOSQUP+IODIR+IO1)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
DELAUD4  CLC   AUDKEY(AUDKSEQ-AUDRECD),CSVKEY1                                  
         JNE   EXIT                                                             
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO1)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         OI    AUDKSTAT,AUDSDELT                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         JNE   *+2                                                              
         J     DELAUD2                                                          
         DROP  R2,R3                                                            
                                                                                
* Delete passives for 'old record' (record passed in AIO2)                      
         USING CPTRBLK,R2                                                       
DELPAS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DELPAS*'                                                      
                                                                                
         LA    R2,ELEMENT                                                       
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
                                                                                
         MVC   LDGLVALN(4),ONERL1L                                              
         GOTO1 VPADDLE,DMCB,(C'D',AIO2),(C'K',CPTRBLK),0,0,ACOMFACS             
                                                                                
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
* Add passives for 'new record' (record passed in AIO4)                         
         USING CPTRBLK,R2                                                       
ADDPAS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ADDPAS*'                                                      
                                                                                
         LA    R2,ELEMENT                                                       
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
                                                                                
         MVC   LDGLVALN(4),ONERL1L                                              
         GOTO1 VPADDLE,DMCB,(C'A',AIO4),CPTRBLK,X#IOADR,0,ACOMFACS              
                                                                                
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
* Update current SJ account for 'MCS estimate exists' flag, plus US             
* only HR, OE and CE settings                                                   
         USING ACTRECD,R2                                                       
         USING ESTRECD,R4                                                       
UPDSJE   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDSJE*'                                                      
                                                                                
*&&US                                                                           
         XC    X#MCSHIE,X#MCSHIE   HR                                           
         XC    X#MCSHAE,X#MCSHAE                                                
         XC    X#MCSORE,X#MCSORE   OE                                           
         XC    X#MCSCUE,X#MCSCUE   CE                                           
         XC    X#MCSHUE,X#MCSHUE                                                
         MVI   X#MCSUPD,NOQ                                                     
                                                                                
         CLI   X#SJUPD,YESQ        Is this sufficient???                        
         JE    UPDSJE04                                                         
         CLC   X#SCNVAL,X#SCOVAL                                                
         JNE   UPDSJE04                                                         
         J     UPDSJE18                                                         
                                                                                
         USING ESTRECD,R4                                                       
UPDSJE04 LA    R4,IOKEY            read through this job's estimates            
         MVC   ESTKEY,CSVKEY2      (cloned from US.ACJOBBER as in               
         MVI   ESTKLNO,0           ACBRA17.SETHCO)                              
         MVI   ESTKSEQ,ESTKSMQ                                                  
         MVI   X#MCSUPD,YESQ                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
         J     UPDSJE08                                                         
                                                                                
UPDSJE06 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
UPDSJE08 CLC   CSVKEY2(ESTKLNO-ESTKEY),ESTKEY                                   
         JNE   UPDSJE14                                                         
         CLI   ESTKSEQ,ESTKSMQ     main records only                            
         JNE   UPDSJE06                                                         
         TM    ESTKSTA1,ESTKCAPP   filter on status                             
         JNZ   UPDSJE10                                                         
         TM    ESTKSTA1,ESTKSUBM                                                
         JZ    UPDSJE06                                                         
         MVC   X#MCSHUE,ESTKLNO    set highest unapproved                       
         J     UPDSJE06                                                         
                                                                                
UPDSJE10 MVC   X#MCSHAE,ESTKLNO    set highest approved                         
         CLI   X#MCSORE,0                                                       
         JE    UPDSJE12                                                         
         CLC   X#MCSORE,ESTKLNO    original estimate set?                       
         JL    UPDSJE06                                                         
                                                                                
UPDSJE12 MVC   X#MCSORE,ESTKLNO    first approved estimate is OE                
         J     UPDSJE06                                                         
                                                                                
UPDSJE14 MVC   X#MCSHIE,X#MCSHAE   set highest estimate etc.                    
         CLC   X#MCSHAE,X#MCSHUE                                                
         JH    UPDSJE16                                                         
         MVC   X#MCSHIE,X#MCSHUE                                                
                                                                                
UPDSJE16 MVC   X#MCSCUE,X#MCSHUE                                                
         CLI   X#MCSHAE,0                                                       
         JE    UPDSJE18                                                         
         MVC   X#MCSCUE,X#MCSHAE                                                
                                                                                
UPDSJE18 CLI   X#SJUPD,NOQ         established HR, OE and CE values             
         JE    UPDSJE20                                                         
*&&                                                                             
                                                                                
         MVI   BYTE1,YESQ          on add: flag required                        
         CLI   X#SCNVAL,ESTKDELT   on delete: any remaining estimates?          
         JNE   UPDSJE20                                                         
         MVI   BYTE1,NOQ                                                        
                                                                                
         LA    R4,IOKEY                                                         
         MVC   ESTKEY,CSVKEY2                                                   
         MVI   ESTKLNO,0                                                        
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JNE   *+2                                                              
                                                                                
         CLC   ESTKEY(ESTKLNO-ESTRECD),CSVKEY2                                  
         JNE   UPDSJE20                                                         
         MVI   BYTE1,YESQ                                                       
                                                                                
UPDSJE20 L     R4,AIO2             restore current estimate                     
                                                                                
         LA    R2,IOKEY            read current SJ account                      
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(2),PRODUL                                                
*&&UK*&& MVC   ACTKACT(5),ESTKCLI                                               
*&&US*&& MVC   ACTKACT(L'ESTKCLI),ESTKCLI                                       
         XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
         LA    R1,ACTKACT(R1)                                                   
*&&UK                                                                           
         MVC   0(2,R1),ESTKPRO                                                  
         MVC   2(6,R1),ESTKJOB                                                  
*&&                                                                             
*&&US                                                                           
         MVC   0(L'ESTKPRO,R1),ESTKPRO                                          
         MVC   L'ESTKPRO(6,R1),ESTKJOB                                          
*&&                                                                             
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
*&&US*&& CLI   X#SJUPD,NOQ                                                      
*&&US*&& JE    UPDSJE30                                                         
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO2                                                          
         LA    R3,ACTRFST                                                       
                                                                                
UPDSJE22 CLI   RSTEL,0                                                          
         JE    *+2                                                              
         CLI   RSTEL,RSTELQ                                                     
         JE    UPDSJE24                                                         
         LLC   R0,RSTLN                                                         
         AR    R3,R0                                                            
         J     UPDSJE22                                                         
                                                                                
UPDSJE24 CLI   RSTLN,RSTLN3Q                                                    
         JL    UPDSJE26                                                         
         NI    RSTSTAT6,FF-RSTSMCSE      take off flag                          
         CLI   BYTE1,YESQ                estimate exists?                       
         JNE   *+8                                                              
         OI    RSTSTAT6,RSTSMCSE         set flag                               
         J     UPDSJE28                                                         
                                                                                
UPDSJE26 XC    ELEMENT,ELEMENT                                                  
         LLC   R1,RSTLN                                                         
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   ELEMENT(0),RSTELD                                                
         EX    R1,0(RE)                                                         
         MVI   RSTEL,FF                                                         
                                                                                
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',ACTRECD),0                      
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         LA    R3,ELEMENT                                                       
*&&UK*&& MVI   RSTLN,RSTLN4Q                                                    
*&&US*&& MVI   RSTLN,RSTLN3Q                                                    
         CLI   BYTE1,YESQ                                                       
         JNE   *+8                                                              
         OI    RSTSTAT6,RSTSMCSE         set estimate exists flag               
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ACTRECD,RSTELD,0,0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
UPDSJE28 DS    0H                                                               
                                                                                
*&&US                                                                           
UPDSJE30 CLI   X#MCSUPD,YESQ       JOBEL update required?                       
         JE    UPDSJE31                                                         
         CLI   X#SJUPD,NOQ                                                      
         JE    UPDSJEX                                                          
                                                                                
         USING JOBELD,R3                                                        
UPDSJE31 L     R2,AIO2                                                          
         TM    ACTRSTAT-ACTKEY(R2),ACTSABLP   Are we at the job level?          
         JNO   UPDSJE40                                                         
         LA    R3,ACTRFST                                                       
                                                                                
UPDSJE32 CLI   JOBEL,0                                                          
         JE    *+2                                                              
         CLI   JOBEL,JOBELQ                                                     
         JE    UPDSJE34                                                         
         LLC   R0,JOBLN                                                         
         AR    R3,R0                                                            
         J     UPDSJE32                                                         
                                                                                
UPDSJE34 CLI   JOBLN,JOBLN4Q                                                    
         JL    UPDSJE38                                                         
         CLC   JOBAEHR#,X#MCSHIE                                                
         JNE   UPDSJE36                                                         
         CLC   JOBAEOE#,X#MCSORE                                                
         JNE   UPDSJE36                                                         
         CLC   JOBAECE#,X#MCSCUE                                                
         JNE   UPDSJE36                                                         
                                                                                
         CLI   X#SJUPD,NOQ         if no change and no update then quit         
         JE    UPDSJEX                                                          
         J     UPDSJE40                                                         
                                                                                
UPDSJE36 MVC   JOBAEHR#,X#MCSHIE   set values                                   
         MVC   JOBAEOE#,X#MCSORE                                                
         MVC   JOBAECE#,X#MCSCUE                                                
         J     UPDSJE40                                                         
                                                                                
UPDSJE38 XC    ELEMENT,ELEMENT                                                  
         LLC   R1,JOBLN                                                         
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   ELEMENT(0),JOBELD                                                
         EX    R1,0(RE)                                                         
         MVI   JOBEL,FF                                                         
                                                                                
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',ACTRECD),0                      
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         LA    R3,ELEMENT                                                       
         MVI   JOBLN,JOBLN4Q                                                    
         MVC   JOBAEHR#,X#MCSHIE   set values                                   
         MVC   JOBAEOE#,X#MCSORE                                                
         MVC   JOBAECE#,X#MCSCUE                                                
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ACTRECD,JOBELD,0,0                     
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
*&&                                                                             
                                                                                
UPDSJE40 GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JNE   *+2                                                              
                                                                                
UPDSJEX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R2,R3,R4                                                         
                                                                                
                                                                                
* Get workcode sequence number                                                  
         USING WCTAB,R4                                                         
GETWCSEQ NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETWCS*'                                                      
*                                                                               
         XC    HALF1,HALF1                                                      
         L     R4,AGENAXTN                                                      
         XR    R1,R1                                                            
         ICM   R1,3,NXWCSEQ                                                     
         JZ    GETWCS20                                                         
*                                                                               
         CHI   R1,MAX#WCS                                                       
         JL    *+6                                                              
         DC    H'0'                Too many work codes!!!                       
*                                                                               
GETWCS10 CLC   WCTCODE,X#WORKC                                                  
         JNE   *+14                                                             
         CLC   WCTNAME,X#WORKN                                                  
         JE    GETWCSX             Exit if work code found                      
         AHI   R4,WCTABLQ          Bump to next entry                           
         JCT   R1,GETWCS10                                                      
*                                                                               
         ICM   R1,3,NXWCSEQ                                                     
                                                                                
GETWCS20 AHI   R1,1                                                             
         STCM  R1,3,NXWCSEQ                                                     
         MVC   WCTSEQ,NXWCSEQ                                                   
         MVC   WCTCODE,X#WORKC                                                  
         MVC   WCTNAME,X#WORKN                                                  
*                                                                               
GETWCSX  MVC   HALF1,WCTSEQ        Reture workcode sequence #                   
         J     EXIT                                                             
         DROP  R4                                                               
                                                                                
* next routine to go in here                                                    
                                                                                
         EJECT                                                                  
***********************************************************************         
* Interface to TSAR for the element buffers                           *         
*                                                                     *         
* Note that when running off-line the TSAR buffers are acquired only  *         
* once - the XC(TSPNEWL) below is intentional as the first time       *         
* through the code TSAR will issue the GETMAIN for the buffer         *         
***********************************************************************         
         SPACE 1                                                                
BUFELM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BUFELM*'                                                      
*                                                                               
         LR    R2,R1               R2=A(Caller's parameter list)                
         LA    R3,TSAROLD          Point to correct TSAR block                  
         CLI   3(R2),NEWBUF                                                     
         JNE   *+8                                                              
         LA    R3,TSARNEW                                                       
         CLI   3(R2),NSRBUF                                                     
         JNE   *+8                                                              
         LA    R3,TSARSRT                                                       
                                                                                
         USING TSARD,R3            R3=A(TSAR block)                             
         L     R0,AIO1                                                          
         ST    R0,TSAREC           Set A(Record)                                
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   BUFELM02                                                         
         XC    TSARD(TSPNEWL),TSARD                                             
         MVC   TSACTN,0(R2)                                                     
         MVC   TSACOM,ACOMFACS                                                  
         LHI   R0,ONEK                                                          
         STCM  R0,3,TSBUFFL        Set require 1MB off-line                     
         MVI   TSRECI,TSRTSAB1                                                  
         CLI   3(R2),NEWBUF                                                     
         JNE   *+8                                                              
         MVI   TSRECI,TSRTSAB2                                                  
         CLI   3(R2),NSRBUF                                                     
         JNE   *+8                                                              
         MVI   TSRECI,TSRWSSVR                                                  
         OI    TSRECI,TSRVAR+TSRXTN variable and extension records              
         MVI   TSKEYL,ZTRKEYL      Set key length                               
         LHI   R0,300                                                           
         STH   R0,TSRECL           Set record length                            
         MVI   TSINDS,TSINODSK     Set no disk writes (save/restore)            
         GOTOR VTSAR,TSARD                                                      
         TM    TSINDS,TSIINIOK                                                  
         JNZ   EXITY                                                            
         DC    H'0'                Initialisation failure                       
                                                                                
BUFELM02 TM    TSINDS,TSIINIOK     Test initialised                             
         JZ    BUFELM04                                                         
                                                                                
         MVC   TSACTN,0(R2)        Set action                                   
         OC    4(L'TSAREC,R2),4(R2)                                             
         JZ    *+10                                                             
         MVC   TSAREC,4(R2)        Set A(record) if passed                      
         GOTOR VTSAR,TSARD         Call TSAR                                    
         MVC   TSARERRS,TSERRS     Return TSERRS in TSARERRS                    
         J     BUFELMX                                                          
                                                                                
BUFELM04 MVI   TSARERRS,TSEEOF     Set EOF if not initialised                   
                                                                                
BUFELMX  CLI   TSARERRS,0          Set condition code for caller                
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ISSUE PENDING I/O      I N A C T I V E                              *         
***********************************************************************         
                                                                                
GOIO     DS    0H                                                               
         BR    RE                                                               
                                                                                
***********************************************************************         
* CALL LINKIO TO BUILD ERROR RETURN ELEMENT                           *         
***********************************************************************         
                                                                                
PUTERR   NTR1  LABEL=NO                                                         
         STCM  R1,3,WORK                                                        
                                                                                
         XR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN                                                    
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTMAP',(R0))                
                                                                                
         CLC   XERRTXT,SPACES                                                   
         JNH   PUTERR2                                                          
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTERR',D#UPLERR),  *        
               WORK,(L'XERRTXT,XERRTXT)                                         
         J     PUTERR4                                                          
                                                                                
PUTERR2  GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTERR',D#UPLERR),  *        
               WORK,0                                                           
                                                                                
PUTERR4  MVC   XERRTXT,SPACES                                                   
                                                                                
PUTERRX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Read new buffer and resort elements                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING ZTSARECD,R2                                                      
         USING ERDELD,ZTRDELM                                                   
NBRSRT   NTR1  LABEL=NO                                                         
         L     R2,AIO1                                                          
         LR    R0,R2                                                            
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR BUFELM,DMCB,('TSARDH',NEWBUF),AIO1                               
         J     NBR04                                                            
*                                                                               
NBR02    GOTOR BUFELM,DMCB,('TSANXT',NEWBUF),AIO1                               
*                                                                               
NBR04    TM    TSARERRS,TSEEOF     Finished?                                    
         JNZ   NBR10                                                            
         LA    RF,ZTRDELM                                                       
*                                                                               
         CLI   ERDEL,ERDELQ                                                     
         JNE   NBR08                                                            
         CLI   ERDTYP,ERDTWDQ      For workcode/item lines                      
         JE    NBR06               Resort based on actual sequence              
         CLI   ERDTYP,ERDTWTQ                                                   
         JE    NBR06                                                            
         CLI   ERDEL,ERDTIDQ                                                    
         JE    *+12                                                             
         CLI   ERDEL,ERDTITQ                                                    
         JNE   NBR08                                                            
         XC    ZTRKIUS,ZTRKITS     Swap the entries                             
         XC    ZTRKITS,ZTRKIUS                                                  
         XC    ZTRKIUS,ZTRKITS                                                  
*                                                                               
NBR06    XC    ZTRKWUS,ZTRKWCS                                                  
         XC    ZTRKWCS,ZTRKWUS                                                  
         XC    ZTRKWUS,ZTRKWCS                                                  
NBR08    GOTOR BUFELM,DMCB,('TSAADD',NSRBUF),AIO1                               
         J     NBR02                                                            
*                                                                               
NBR10    J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GENERAL EXITS HERE                                                  *         
***********************************************************************         
                                                                                
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
MAXRECLN EQU   IOLENQ-(L'IODA+L'IOWORK)                                         
                                                                                
ACCMST   DC    C'ACCMST  '                                                      
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
PMAX     DC    P'250'                                                           
LSPACES  DC    255C' '                                                          
ADDEND   DC    C'ADD=END'                                                       
EFFS     DC    X'FFFFFFFF'                                                      
                                                                                
SCVTAB   DS    0X                                                               
         DC    AL1(ESTKLOGD),AL1(ESTKREJE,ESTKSUBM,ESTKCREA,0)                  
SCVTABQ  EQU   *-SCVTAB                                                         
         DC    AL1(ESTKDELT),AL1(ESTKCREA,0,0,0)                                
         DC    AL1(ESTKREJE),AL1(ESTKSUBM,ESTKCAPP,0,0)                         
         DC    AL1(ESTKCAPP),AL1(ESTKCREA,ESTKSUBM,ESTKREJE,0)                  
         DC    AL1(ESTKSUBM),AL1(ESTKCREA,ESTKREJE,ESTKSUBM,ESTKCAPP)           
         DC    X'FF'                                                            
                                                                                
SC2TAB   DS    0X                                                               
         DC    AL1(ESTKLOGD),AL1(ESTKREJE,ESTKSUBM,ESTKCREA,ESTKINTA)           
         DC    AL1(ESTKDELT),AL1(ESTKCREA,0,0,0)                                
         DC    AL1(ESTKREJE),AL1(ESTKSUBM,ESTKCAPP,ESTKINTA,0)                  
         DC    AL1(ESTKINTA),AL1(ESTKSUBM,ESTKREJE,0,0)                         
         DC    AL1(ESTKSUBM),AL1(ESTKCREA,ESTKREJE,ESTKINTA,ESTKSUBM)           
         DC    AL1(ESTKCAPP),AL1(ESTKINTA,ESTKSUBM,ESTKREJE,0)                  
         DC    X'FF'                                                            
                                                                                
STATAB   DS    0X                                                               
         DC    AL1(ESTKLOGD,0),CL1'D',AL1(OLOGDEL)                              
STATABQ  EQU   *-STATAB                                                         
         DC    AL1(ESTKREJE,0),CL1'R',AL1(OREJECT)                              
         DC    AL1(ESTKINTA,0),CL1'I',AL1(OINTAPP)                              
         DC    AL1(ESTKCAPP,0),CL1'A',AL1(OCLIAPP)                              
         DC    AL1(ESTKSUBM,ESTKSINA),CL1'S',AL1(OSUBINT)                       
         DC    AL1(ESTKSUBM,0),CL1'S',AL1(OSUBCLI)                              
         DC    AL1(ESTKCREA,0),CL1'C',AL1(OINPROG)                              
         DC    X'FF'                                                            
                                                                                
AUDNCFQ  EQU   C'N'                from/to for 'name change' audit              
AUDNCTQ  EQU   AUDNCFQ                                                          
                                                                                
AUDXCFQ  EQU   C'X'                'internal use only' checkbox audit           
AUDXCTQ  EQU   AUDXCFQ                                                          
                                                                                
APRTAB   DS    0XL1                                                             
         DC    B'11111000'         Office/client/product/job/media              
         DC    AL1(JBMEOFQ)                                                     
         DC    B'11011000'         Office/client/product/media                  
         DC    AL1(PRMEOFQ)                                                     
         DC    B'10011000'         Office/client/media                          
         DC    AL1(CLMEOFQ)                                                     
         DC    B'00011000'         Office/media                                 
         DC    AL1(MEDOFFQ)                                                     
         DC    B'00010000'         Media                                        
         DC    AL1(MEDIAQ)                                                      
         DC    B'11001000'         Office/client/product                        
         DC    AL1(PROOFFQ)                                                     
         DC    B'10001000'         Office/client                                
         DC    AL1(CLIOFFQ)                                                     
         DC    B'10000000'         Client                                       
         DC    AL1(CLIENTQ)                                                     
         DC    B'00001000'         Office                                       
         DC    AL1(OFFICEQ)                                                     
         DC    B'00000000'         Agency                                       
         DC    AL1(AGENCYQ)                                                     
         DC    X'FF'                                                            
*                                                                               
JBMEOFQ  EQU   X'01'                                                            
PRMEOFQ  EQU   X'02'                                                            
CLMEOFQ  EQU   X'03'                                                            
CLIMEDQ  EQU   X'04'                                                            
MEDOFFQ  EQU   X'05'                                                            
MEDIAQ   EQU   X'06'                                                            
PROOFFQ  EQU   X'07'                                                            
CLIOFFQ  EQU   X'08'                                                            
CLIENTQ  EQU   X'09'                                                            
OFFICEQ  EQU   X'10'                                                            
AGENCYQ  EQU   X'11'                                                            
*                                                                               
OLDBUF   EQU   1                   Old estimate element buffer                  
NEWBUF   EQU   2                   New estimate element buffer                  
NSRBUF   EQU   3                   New sorted buffer                            
*                                                                               
OLOGDEL  EQU   5                   Logically deleted                            
OREJECT  EQU   4                   Rejected                                     
OINTAPP  EQU   7                   Internally approved                          
OCLIAPP  EQU   3                   Client approved                              
OSUBINT  EQU   6                   Submitted for internal                       
OSUBCLI  EQU   2                   Submitted for client approval                
OINPROG  EQU   1                   In progress                                  
*                                                                               
RECTAB   DS    0XL(RECTABL)        ** RECORD TABLE **                           
         DC    AL2(A#EMUP),AL1(RECTEMUP)                                        
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
                                                                                
                                                                                
ELSRTAB  DS    0X                                                               
         DC    AL1(EMDELQ,1,0,YESQ,0,0,0)                                       
         DC    AL1(ENMELQ,2,0,YESQ,0,0,0)                                       
         DC    AL1(PIDELQ,3,0,YESQ,0,0,0)                                       
         DC    AL1(STCELQ,4,0,YESQ,0,0,0)                                       
         DC    AL1(GDAELQ,5,0,YESQ,0,0,0)                                       
         DC    AL1(FFTELQ,6,0,YESQ,0,0,0)                                       
         DC    AL1(EMSELQ,7,0,YESQ,0,0,0)                                       
         DC    AL1(XDFELQ,8,0,YESQ,0,XDFOCOD-XDFELD,L'XDFOCOD)                  
*        DC    AL1(XDFELQ,8,0,YESQ,0,0,0)                                       
         DC    AL1(ERDELQ,9,ERDTCSQ,NOQ,1,0,0)                                  
         DC    AL1(ERDELQ,9,ERDTCTQ,NOQ,2,0,0)                                  
         DC    AL1(ERDELQ,9,ERDTWDQ,NOQ,3,0,0)                                  
         DC    AL1(ERDELQ,9,ERDTIDQ,NOQ,4,0,0)                                  
         DC    AL1(ERDELQ,9,ERDTIRQ,NOQ,5,0,0)                                  
         DC    AL1(ERDELQ,9,ERDTITQ,NOQ,6,0,0)                                  
         DC    AL1(ERDELQ,9,ERDTWTQ,NOQ,7,0,0)                                  
         DC    AL1(ERDELQ,10,ERDTHTQ,NOQ,1,0,0)                                 
         DC    AL1(ERDELQ,10,ERDTFTQ,NOQ,2,0,0)                                 
         DC    AL1(ERDELQ,10,ERDTETQ,NOQ,3,0,0)                                 
         DC    AL1(PTAELQ,11,0,YESQ,0,0,0)                                      
*&&UK*&& DC    AL1(FADELQ,11,0,YESQ,0,0,0)                                      
         DC    X'FF'                                                            
                                                                                
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTTYPE DS    AL1                 ** RECORD TYPE **                            
RECTEMUP EQU   1                   Main estimate upload                         
RECTABL  EQU   *-RECTABD                                                        
                                                                                
ELSRTABD DSECT                                                                  
ELSRTC   DS    XL1                 Element code                                 
ELSRTO   DS    XL1                 Element sort order                           
ELSUBC   DS    XL1                 Element sub code                             
ELSQYN   DS    XL1                 Use ZTRKSEQ Y/N                              
ELSUBS   DS    XL1                 Element sub sort order                       
ELSRTCD  DS    XL1                 Displacement to element sort code            
ELSRTCDL DS    XL1                 Length of element sort code                  
ELSRTABL EQU   *-ELSRTC                                                         
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP FOR ESTIMATE UPLOAD                                     *         
***********************************************************************         
                                                                                
ESTUPL   LKREQ H,A#EMUP,NEWREC=Y                                                
Type     LKREQ F,01,(D,B#SAVED,RQUPTYP),CHAR,TEXT=AC#TYPE                       
                                                                                
Actn     LKREQ F,10,(D,B#SAVED,RQUGACT),CHAR,TEXT=AC#ACT                        
GblN     LKREQ F,11,(D,B#SAVED,RQUGGNO),CHAR,TEXT=AC#ESTNO                      
MaSC     LKREQ F,14,(D,B#SAVED,RQUGMASC),CHAR,TEXT=(*,MASCLIT)                  
CliC     LKREQ F,15,(D,B#SAVED,RQUGCLI),CHAR,TEXT=AC#CLIC                       
ProC     LKREQ F,16,(D,B#SAVED,RQUGPRO),CHAR,TEXT=AC#PROC                       
JobC     LKREQ F,17,(D,B#SAVED,RQUGJOB),CHAR,TEXT=AC#JOBC                       
Date     LKREQ F,18,(D,B#SAVED,RQUGDAT),CHAR,TEXT=AC#DATE                       
Desc     LKREQ F,19,(D,B#SAVED,RQUGDES),LOWERCASE=Y,VSTR,TEXT=AC#ESTDS,*        
               MAXLEN=L'RQUGDES                                                 
CurC     LKREQ F,20,(D,B#SAVED,RQUGCUR),CHAR,TEXT=AC#CURRC                      
ExcR     LKREQ F,21,(D,B#SAVED,RQUGEXC),CHAR,TEXT=AC#EXCHR                      
SchC     LKREQ F,22,(D,B#SAVED,RQUGSCH),CHAR,TEXT=AC#SCMCD                      
Lang     LKREQ F,23,(D,B#SAVED,RQUGLAN),CHAR,TEXT=AC#LANG                       
Appr     LKREQ F,24,(D,B#SAVED,RQUGAPP),CHAR,TEXT=AC#APRVR                      
TVRT     LKREQ F,25,(D,B#SAVED,RQUGTVR),CHAR,TEXT=AC#TV                         
Frmt     LKREQ F,26,(D,B#SAVED,RQUGFMT),CHAR,TEXT=AC#FRMAT                      
MedC     LKREQ F,27,(D,B#SAVED,RQUGMED),CHAR,TEXT=AC#MEDC                       
CCNa     LKREQ F,28,(D,B#SAVED,RQUGCCN),CHAR,TEXT=AC#MNCON                      
CCDa     LKREQ F,29,(D,B#SAVED,RQUGCCD),CHAR,TEXT=AC#RSUSD                      
SEfP     LKREQ F,30,(D,B#SAVED,RQUGSEP),CHAR,TEXT=AC#SPR                        
SDfP     LKREQ F,31,(D,B#SAVED,RQUGSDP),CHAR,TEXT=AC#SPR                        
IntAp    LKREQ F,32,(D,B#SAVED,RQUGIAP),CHAR,TEXT=AC#APRVR                      
IntUS    LKREQ F,33,(D,B#SAVED,RQUINUS),CHAR,TEXT=AC#INTUS                      
                                                                                
GAPst    LKREQ F,39,(D,B#SAVED,RQGAPST),LBIN,TEXT=AC#GPST                       
GAPed    LKREQ F,40,(D,B#SAVED,RQGAPED),PDAT,TEXT=AC#GAPED                      
GAPem    LKREQ F,41,(I,B#SAVED,RQGAPEI),CHAR,TEXT=AC#GAPEM,            +        
               LIST=F,OLEN=L'FFTEML1,SORT=NO                                    
PRate    LKREQ F,45,(D,B#SAVED,RQUPRAT),CHAR,TEXT=AC#RATE                       
SCCom    LKREQ F,46,(D,B#SAVED,RQUGCOM),CHAR,TEXT=(*,STCOLIT)                   
                                                                                
Txt1     LKREQ F,50,(D,B#SAVED,RQUTXT1L),(R,VALTXT),TEXT=AC#REMHL,     *        
               LOWERCASE=Y,OLEN=L'RQUTXT1+L'RQUTXT1L                            
                                                                                
CatC     LKREQ F,60,(D,B#SAVED,RQUWCAT),CHAR,TEXT=AC#CAT                        
WrkC     LKREQ F,61,(D,B#SAVED,RQUWCOD),CHAR,TEXT=AC#WC                         
WNam     LKREQ F,62,(D,B#SAVED,RQUWNAM),VSTR,TEXT=AC#WCNAM,LOWERCASE=Y,*        
               MAXLEN=L'RQUWNAM                                                 
Amnt     LKREQ F,63,(D,B#SAVED,RQUWAMT),CHAR,TEXT=AC#AMT                        
FCAm     LKREQ F,64,(D,B#SAVED,RQUWFCA),CHAR,TEXT=AC#FCAMT                      
CAmt     LKREQ F,65,(D,B#SAVED,RQUWCAM),CHAR,TEXT=AC#CMNAM                      
CFCA     LKREQ F,66,(D,B#SAVED,RQUWCFC),CHAR,TEXT=AC#FCAMT                      
CRat     LKREQ F,67,(D,B#SAVED,RQUWCRA),CHAR,TEXT=AC#CRATE                      
WItm     LKREQ F,68,(D,B#SAVED,RQUWIYN),CHAR,TEXT=AC#ITEMS                      
WCom     LKREQ F,69,(D,B#SAVED,RQUWCYN),CHAR,TEXT=AC#CMN                        
WVRa     LKREQ F,72,(D,B#SAVED,RQUWCVR),CHAR,TEXT=AC#VATRT                      
WVAm     LKREQ F,73,(D,B#SAVED,RQUWCVA),CHAR,TEXT=AC#VATAM                      
WVFC     LKREQ F,74,(D,B#SAVED,RQUWCVF),CHAR,TEXT=AC#FCAMT                      
WVCo     LKREQ F,75,(D,B#SAVED,RQUWCVC),CHAR,TEXT=AC#VATC1                      
WNIC     LKREQ F,76,(D,B#SAVED,RQUWNIC),CHAR,TEXT=AC#VATAM                      
WNFC     LKREQ F,77,(D,B#SAVED,RQUWNFC),CHAR,TEXT=AC#VATAM                      
WCon     LKREQ F,78,(D,B#SAVED,RQUWXYN),CHAR,TEXT=AC#CTY                        
WCCC     LKREQ F,79,(D,B#SAVED,RQUWCCC),CHAR,TEXT=AC#PRDCT                      
* see map code 140 for more w/c data                                            
                                                                                
Ityp     LKREQ F,80,(D,B#SAVED,RQUITYP),CHAR,TEXT=AC#TYPE                       
I1Ra     LKREQ F,81,(D,B#SAVED,RQUI1RA),CHAR,TEXT=AC#1RACC                      
Item     LKREQ F,85,(D,B#SAVED,RQUINUM),CHAR,TEXT=AC#ARTCD                      
ISqN     LKREQ F,86,(D,B#SAVED,RQUISEQ),CHAR,TEXT=AC#SEQNO                      
*Des     LKREQ F,87,(D,B#SAVED,RQUIDES),VSTR,TEXT=AC#PLDSC,LOWERCASE=Y          
*              MAXLEN=L'RQUIDES                                                 
IDes     LKREQ F,87,(D,B#SAVED,RQUIDES),CHAR,TEXT=AC#PLDSC                      
IPri     LKREQ F,88,(D,B#SAVED,RQUIPRI),CHAR,TEXT=AC#PRICE                      
IFCP     LKREQ F,89,(D,B#SAVED,RQUIFCP),CHAR,TEXT=AC#LCLCU                      
IMul     LKREQ F,90,(D,B#SAVED,RQUIMUL),CHAR,TEXT=AC#MULT                       
INIC     LKREQ F,93,(D,B#SAVED,RQUINIC),CHAR,TEXT=AC#VATAM                      
INFC     LKREQ F,94,(D,B#SAVED,RQUINFC),CHAR,TEXT=AC#VATAM                      
IOvr     LKREQ F,95,(D,B#SAVED,RQUIOVR),CHAR,TEXT=AC#OVRI                       
                                                                                
CCod     LKREQ F,100,(D,B#SAVED,RQUCCOD),CHAR,TEXT=AC#CATCD                     
CNam     LKREQ F,101,(D,B#SAVED,RQUCNAM),CHAR,TEXT=AC#CATDS                     
CTNm     LKREQ F,102,(D,B#SAVED,RQUCTNA),CHAR,TEXT=AC#CATTO                     
CIns     LKREQ F,103,(D,B#SAVED,RQUCINS),CHAR,TEXT=AC#CATFM                     
CTyp     LKREQ F,104,(D,B#SAVED,RQUCTYP),CHAR,TEXT=AC#CAT                       
CEls     LKREQ F,105,(D,B#SAVED,RQUCELS),CHAR,TEXT=AC#CATPR                     
                                                                                
XCod     LKREQ F,110,(D,B#SAVED,RQUXCOD),CHAR,TEXT=AC#CODE                      
XTyp     LKREQ F,111,(D,B#SAVED,RQUXTYP),CHAR,TEXT=AC#TYPE                      
XDta     LKREQ F,112,(D,B#SAVED,RQUXDTA),VSTR,TEXT=AC#DATA,LOWERCASE=Y,*        
               MAXLEN=L'RQUXDTA                                                 
                                                                                
IDNo     LKREQ F,120,(D,B#SAVED,RQUUIDN),CHAR,TEXT=AC#SPCNU                     
                                                                                
SEst     LKREQ F,130,(D,B#SAVED,RQUSEST),CHAR,TEXT=AC#EST                       
                                                                                
WCCR     LKREQ F,140,(D,B#SAVED,RQUWCCR),CHAR,TEXT=AC#OP159                     
WMrg     LKREQ F,141,(D,B#SAVED,RQUWMYN),CHAR,TEXT=AC#MRGED                     
*&&UK                                                                           
OTxt     LKREQ F,142,(D,B#SAVED,RQUWOYN),CHAR,TEXT=AC#ITTXT                     
*&&                                                                             
*&&US                                                                           
OTxt     LKREQ F,142,(D,B#SAVED,RQUWOYN),CHAR,TEXT=(*,ITMTLIT)                  
*&&                                                                             
OIAC     LKREQ F,143,(D,B#SAVED,RQUWIAC),CHAR,TEXT=(*,ITMTLIT)                  
WCSeq    LKREQ F,144,(D,B#SAVED,RQUWCSQ),LBIN,TEXT=AC#SEQ                       
IOSEQ    LKREQ F,145,(D,B#SAVED,RQUITSO),LBIN,TEXT=AC#SEQ                       
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* END OF REQUEST MAP TABLES                                          *          
***********************************************************************         
                                                                                
         LKREQ X                                                                
***********************************************************************         
* Validate header footer text                                         *         
***********************************************************************         
                                                                                
VALTXT   L     RF,ALP                                                           
         LM    R2,R4,LP_AINP-LP_D(RF)                                           
         STC   R3,0(R4)            Returns L'Text,Text string                   
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         MVC   1(0,R4),0(R2)                                                    
         EX    R3,0(RE)                                                         
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST LITERALS                                                    *         
***********************************************************************         
                                                                                
SBASLIT  DC    C'Base'                                                          
ITMTLIT  DC    C'Item Text'                                                     
MASCLIT  DC    C'Maint + Status'                                                
STCOLIT  DC    C'ChangeComment'                                                 
                                                                                
***********************************************************************         
* ESTIMATE UPLOAD RETURN MAP NUMBERS                                  *         
***********************************************************************         
                                                                                
D#UE#GNO EQU   01                  Global number                                
D#UE#LNO EQU   02                  Local number                                 
D#UE#IDN EQU   03                  ID Number                                    
                                                                                
D#UE#ASF EQU   10                  audit trails from status                     
D#UE#AST EQU   11                  to status                                    
D#UE#ADA EQU   12                  date                                         
D#UE#ATI EQU   13                  time                                         
D#UE#AUS EQU   14                  user ID                                      
D#UE#APC EQU   15                  person code                                  
D#UE#AFN EQU   16                  person first name                            
D#UE#ALN EQU   17                  person last name                             
D#UE#ACC EQU   18                  change comment                               
                                                                                
D#UE#CAE EQU   01                  type field in TEAP call                      
                                                                                
*** ESTIMATE ROW COPY RETURN MAP NUMBERS                                        
                                                                                
D#RC#STA EQU   01                  Status                                       
D#RC#PRI EQU   02                  New Price                                    
D#RC#FLG EQU   03                  Item flags                                   
D#RC#WNO EQU   04                  W/C number                                   
D#RC#INO EQU   05                  Item number                                  
                                                                                
*** GENERAL UPLOAD RESPONSE DATA MAP NUMBERS                                    
                                                                                
D#UPLERR EQU   255                                                              
D#UPLNOD EQU   254                                                              
                                                                                
         EJECT                                                                  
                                                                                
SAVED    DSECT                                                                  
                                                                                
SVTODAYP DS    XL3                 Today's date local storage                   
                                                                                
         DS    0D                                                               
SAVER0   DS    F                   Save full word for register                  
X#FULL1  DS    F                   local full word                              
X#HALF1  DS    H                   local half word                              
X#BYTE1  DS    XL1                 local byte                                   
*                                                                               
X#ELSRT  DS    0D                  Element sort sequences                       
X#TINUM  DS    XL2                 Element sort sequence                        
X#PTNUM  DS    XL2                 PTAELD sort sequence                         
X#CATSQ  DS    XL1                 Category sequence                            
X#SVELC  DS    XL1                 Saved element code                           
X#SVSUB  DS    XL1                 Saved subelement code                        
X#ITESQ  DS    XL1                 Item sequence                                
X#WCSEQ  DS    XL2                 Workcode sequence                            
X#SUBSQ  DS    XL2                 Subelement sequence                          
X#ELSRL  EQU   *-X#ELSRT                                                        
*                                                                               
X#SVCAT  DS    CL2                 Local saved category                         
X#SVWC   DS    CL2                 Local saved WC                               
X#SVTYP  DS    XL1                 Local saved ERDTYP type of data              
X#ITEC   DS    CL12                Item code                                    
X#UPWCSQ DS    XL2                 Uploaded workcode sequence                   
*                                                                               
X#SJUPD  DS    CL1                 Update job record for current est            
*&&US                                                                           
X#MCSUPD DS    CL1                                                              
X#MCSHIE DS    XL1                                                              
X#MCSHAE DS    XL1                                                              
X#MCSORE DS    XL1                                                              
X#MCSCUE DS    XL1                                                              
X#MCSHUE DS    XL1                                                              
*&&                                                                             
X#MODE   DS    XL1                                                              
X#OWNER  DS    XL1                                                              
X#NEWIDN DS    XL2                                                              
X#2OFFC  DS    CL2                                                              
X#2CLIC  DS    CL6                                                              
X#SAPPR  DS    XL2                 new client approver                          
X#SIAPP  DS    XL2                 new internal approver                        
X#ERAIS  DS    XL2                 original estimate raiser                     
X#SUBIN  DS    XL2                 original internal aprvr submitter            
X#SUBCL  DS    XL2                 original client aprvr submitter              
X#ECLAP  DS    XL2                 original client approver                     
X#EINAP  DS    XL2                 original internal approver                   
X#FLANG  DS    XL1                 foreign language setting                     
         DS    XL1                 n/d                                          
X#SCHCD  DS    CL8                                                              
X#BPMED  DS    CL1                                                              
X#PIDST  DS    XL1                 pideld present indicator byte                
X#SJOFF  DS    CL2                                                              
*&&UK                                                                           
X#SJCLI  DS    CL5                                                              
*&&                                                                             
*&&US                                                                           
X#SJCLI  DS    CL3                                                              
*&&                                                                             
*&&US                                                                           
X#ESTST3 DS    XL1    ESTIMATE STAUS BYTE FOR UPDATE                            
X#ESTCST DS    XL1    ESTIMATE STAUS CHANGE INDICATOR                           
X#ESTASV EQU   X'80'  SAVED APPROVED ESTIMATE, DONT OVERRIDE BY SUBMIT          
X#ESTUCE EQU   X'40'  UNSET CE STATUS FROM PREVIOUS RECORD                      
*&&                                                                             
X#SJIND  DS    XL1                                                              
X#LOCNO  DS    XL1                                                              
X#NODATA DS    CL1                                                              
X#ACTION DS    CL1                                                              
X#ACTISQ EQU   C'-'   action is status only                                     
X#STATUS DS    CL1                                                              
X#NOSTAQ EQU   C'-'   maintenance only, no status                               
X#IOADR  DS    XL4                                                              
X#OSCHAR DS    CL1    Old status character format                               
X#NSCHAR DS    CL1    New status character format                               
X#OSENUM DS    XL1    Old status equated hex number                             
X#NSENUM DS    XL1    New status equated hex number                             
X#SCNVAL DS    XL1    New ESTKSTA1                                              
X#SCOVAL DS    XL1    Old ESTKSTA1                                              
X#SCCOMM DS    CL120  Approver comments                                         
X#SCCCNA DS    CL36   Client contact name                                       
X#SCCCDA DS    CL8    Client contact date                                       
X#SC2NVL DS    XL1    New status byte 2                                         
X#SC2OVL DS    XL1    Old status byte 2 (ESTKSTA2)                              
X#SC3NVL DS    XL1    New status byte 3                                         
X#SC3OVL DS    XL1    Old status byte 3 (ESTKSTA3)                              
X#ESIGSO DS    XL1    Old electronic signature status                           
X#ESIGSN DS    XL1    New electronic signature status                           
                                                                                
ES#ESTN  DS    CL6                                                              
*&&UK                                                                           
SV#ESTN  DS    CL(L'ES#ESTN)                                                    
ESTN#HX  DS    XL3                                                              
*&&                                                                             
X#COUNT  DS    PL4                                                              
X#ESEXDT DS    XL3    Electronic signature expiry date                          
                                                                                
X#RCSTA  DS    CL1                                                              
X#RCPRI  DS    PL6                                                              
X#RCFLG  DS    CL10                                                             
*                                                                               
X#TOHRS  DS    PL6    ESTIMATE TOTAL HOURS DETAILS                              
X#TOTAL  DS    PL6    Workcode agency total                                     
X#TOTFC  DS    PL6    Workcode foreign total                                    
X#TOTCA  DS    PL6    Workcode agency total commission                          
X#TOTCF  DS    PL6    Workcode foreign total commission                         
X#TOTVA  DS    PL6    Workcode agency total VAT                                 
X#TOTVF  DS    PL6    Workcode foreign total VAT                                
X#TOTIW  DS    PL6    Workcode internal workcode agency total                   
X#TOTXW  DS    PL6    Workcode external workcode agency total                   
                                                                                
         DS    0H                                                               
X#WAMT1  DS    PL6                                                              
X#WAMT2  DS    PL6                                                              
X#WAMT3  DS    PL6                                                              
X#WAMT4  DS    PL6                                                              
X#WRATE  DS    PL6                                                              
X#WVATR  DS    PL8                                                              
X#WVATA  DS    PL6                                                              
X#WVATF  DS    PL6                                                              
X#WNICA  DS    PL6                                                              
X#WNICF  DS    PL6                                                              
X#WESPR  DS    PL6                                                              
X#WVATC  DS    CL1                                                              
X#WORKC  DS    CL2                                                              
X#WORKN  DS    CL36                                                             
X#SVNAL  DS    XL1                                                              
X#SVNAM  DS    CL50                                                             
X#SVINTW DS    XL1                                                              
                                                                                
X#IAMT1  DS    PL6                                                              
X#IAMT2  DS    PL6                                                              
X#IMULT  DS    PL6                                                              
X#INICA  DS    PL6                                                              
X#INICF  DS    PL6                                                              
X#ICODE  DS    CL4                                                              
X#ISEQN  DS    XL3                                                              
X#ISEON  DS    XL1                                                              
X#IDESC  DS    CL36                                                             
X#ISTAT  DS    XL1                                                              
X#ISTA2  DS    XL1                                                              
X#INTWC  DS    CL1                                                              
                                                                                
X#CURTAB DS    CL(CURTABL+L'CURTSHRT+L'CURTLONG)                                
                                                                                
X#STCEL1 DS    XL170                                                            
X#STCEL2 DS    XL170                                                            
                                                                                
TSAROLD  DS    XL(TSPXTNL)         TSAR block for old buffer                    
TSARNEW  DS    XL(TSPXTNL)         TSAR block for new buffer                    
TSARSRT  DS    XL(TSPXTNL)         TSAR block for sorted buffer                 
                                                                                
X#I1RAC  DS    CL12                                                             
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      RECORD TYPE                                  
                                                                                
NXWCSEQ  DS    XL(L'ERDWSEQ)       NEXT WORKCODE SEQUENCE NUMBER                
                                                                                
                                                                                
* Estimate main upload                                                          
RQUPVAL  DS    2000X               see SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
RQUPTYP  DS    CL1                 Record type                                  
RQUPTGQ  EQU   C'G'                  * RQUGVAL Global values                    
RQUPTHQ  EQU   C'H'                  * RQUTXT1/2 - common for text              
RQUPTWQ  EQU   C'W'                  * RQUWVAL Work code values                 
RQUPTYQ  EQU   C'Y'                  * RQUTXT1/2 - common for text              
RQUPTIQ  EQU   C'I'                  * RQUIVAL Item values                      
RQUPTZQ  EQU   C'Z'                  * RQUTXT1/2 - common for text              
RQUPTFQ  EQU   C'F'                  * RQUTXT1/2 - common for text              
RQUPTEQ  EQU   C'E'                  * RQUTXT1/2 - common for text              
RQUPTXQ  EQU   C'X'                  * RQUXVAL Xtra data values                 
RQUPTCQ  EQU   C'C'                  * RQUCVAL Category values                  
RQUPTVQ  EQU   C'V'                  * RQUTXT1/2 - common for text              
RQUPTSQ  EQU   C'S'                  * RQUSVAL Source values                    
RQUPTUQ  EQU   C'U'                  * RQUUVAL Update values                    
         DS    0X                                                               
RQUGVAL  DS    0X                  Global values                                
RQUGACT  DS    CL1                 - action                                     
RQUGACQ  EQU   C'C'                  * create                                   
RQUGAMQ  EQU   C'M'                  * maintain                                 
RQUGABQ  EQU   C'B'                  * submit to internal approver              
RQUGASQ  EQU   C'S'                  * submit (to client approver)              
RQUGA1Q  EQU   C'1'                  * submit for electronic signature          
RQUGA2Q  EQU   C'2'                  * recall for electronic signature          
RQUGAAQ  EQU   C'A'                  * approve                                  
RQUGAIQ  EQU   C'I'                  * internally approve                       
RQUGARQ  EQU   C'R'                  * reject                                   
RQUGADQ  EQU   C'D'                  * (logical) delete                         
RQUGA#Q  EQU   C'#'                  * (physical) delete                        
RQUPTGS  EQU   C'U'                  * GAP change                               
* see X#ACTISQ = C'*' for action is status only                                 
* see X#NOSTAQ = C'-' for maintenance only, no status                           
RQUGMASC DS    CL1                 - maintenance and status change              
RQUGGNO  DS    CL6                 - global estimate number                     
         DS    0X                  - panel change status Y/N                    
*&&UK                                                                           
RQUGCLI  DS    CL5                 - client code                                
RQUGPRO  DS    CL2                 - product code                               
RQUGJOB  DS    CL7                 - job code                                   
*&&                                                                             
*&&US                                                                           
RQUGCLI  DS    CL3                 - client code                                
RQUGPRO  DS    CL3                 - product code                               
RQUGJOB  DS    CL7                 - job code                                   
*&&                                                                             
RQUGMED  DS    CL1                 - media code                                 
RQUGDAT  DS    CL8                 - date                                       
RQUGDES  DS    CL120               - description (language, 50 chars)           
RQUGCOM  DS    CL120               - status change comments                     
RQUGCUR  DS    CL3                 - currency code                              
RQUGEXC  DS    CL14                - exchange rate                              
RQUGSCH  DS    CL8                 - scheme code                                
RQUGLAN  DS    CL1                 - language Y/N                               
RQUGAPP  DS    CL8                 - supposed approver                          
RQUGIAP  DS    CL8                 - supposed internal approver                 
RQUGTVR  DS    CL16                - TVR total                                  
RQUGFMT  DS    CL8                 - format code                                
RQUGCCN  DS    CL36                - client contact name                        
RQUGCCD  DS    CL8                 - client contact date                        
RQUGSEP  DS    CL1                 - suppress empty lines from print            
RQUGSDP  DS    CL1                 - suppress zero amount lines print           
RQUINUS  DS    CL1                 - internal use only (Y/N)                    
*                                                                               
RQGAPST  DS    XL1                 - GAP status                                 
RQGAPED  DS    PL3                 - GAP expiry date                            
RQGAPEI  DS    X                   - GAP email address array                    
RQGAPEM  DS    AL3                 email address                                
*                                                                               
RQUPRAT  DS    CL1                 - print rates on estimate                    
RQUTXT1L DS    XL1                 - text length                                
RQUTXT1  DS    XL250               - text                                       
         DS    0X                                                               
RQUWVAL  DS    0X                  Work code values                             
RQUWCAT  DS    CL2                 - category                                   
RQUWCOD  DS    CL2                 - work code                                  
RQUWNAM  DS    CL36                - work code name                             
RQUWAMT  DS    CL16                - amount (agency)                            
RQUWFCA  DS    CL16                - FC amount                                  
RQUWCAM  DS    CL16                - commission amount (agency)                 
RQUWCFC  DS    CL16                - FC commission amount                       
RQUWCRA  DS    CL16                - commission rate                            
RQUWCVR  DS    CL16                - VAT rate                                   
RQUWCVA  DS    CL16                - VAT amount                                 
RQUWCVF  DS    CL16                - VAT amount FC                              
RQUWCVC  DS    CL1                 - VAT code used                              
RQUWIYN  DS    CL1                 - items?                                     
RQUWCYN  DS    CL1                 - commission?                                
RQUWNIC  DS    CL16                - NIC amount                                 
RQUWNFC  DS    CL16                - NIC amount FC                              
RQUWXYN  DS    CL1                 - contingency?                               
RQUWMYN  DS    CL1                 - merged from estimate?                      
RQUWOYN  DS    CL1                 - override item text?                        
RQUWIAC  DS    CL1                 - used actuals for w/c calculation?          
RQUWCCC  DS    CL2                 - category contingency to be applied         
RQUWCCR  DS    CL16                - ESP % for contingency                      
RQUWCSQ  DS    XL2                 - Workcode sequence                          
         DS    0X                                                               
RQUIVAL  DS    0X                  Item values                                  
RQUITYP  DS    CL1                 - 1 = material 2 = person                    
*                                  Person                                       
RQUI1RA  DS    CL12                - 1r code (any level)                        
*                                  Material                                     
RQUINUM  DS    CL4                 - number                                     
RQUISEQ  DS    CL6                 - sequence                                   
RQUIDES  DS    CL36                - description (in language)                  
RQUIPRI  DS    CL16                - price selected (agency)/hrly rate          
RQUIFCP  DS    CL16                - FC price selected                          
RQUIMUL  DS    CL16                - multiplier/n'hours                         
RQUIOVR  DS    CL1                 - price overiiden?                           
RQUINIC  DS    CL16                - NIC amount                                 
RQUINFC  DS    CL16                - NIC amount FC                              
RQUITSO  DS    XL1                 - Item sequence order no.                    
         DS    XL1                                                              
         DS    0X                                                               
RQUCVAL  DS    0X                  Category values                              
RQUCCOD  DS    CL2                 - code                                       
RQUCNAM  DS    CL20                - name                                       
RQUCTNA  DS    CL20                - total name                                 
RQUCINS  DS    CL26                - instruction                                
RQUCTYP  DS    CL1                 - type                                       
RQUCELS  DS    CL16                - 'else'                                     
         DS    0X                                                               
RQUXVAL  DS    0X                  Xtra data values                             
RQUXCOD  DS    CL6                 - code                                       
RQUXTYP  DS    CL1                 - type                                       
RQUXDTA  DS    CL90                - data                                       
         DS    0X                                                               
RQUSVAL  DS    0X                  Source copy values (on add only)             
RQUSEST  DS    CL6                 - estimate number                            
         DS    0X                                                               
RQUUVAL  DS    0X                  Update values                                
RQUUIDN  DS    CL4                 - ID number                                  
         DS    0X                                                               
RQUSPARE DS    XL100               KEEP SPARE                                   
         DS    XL(RQUPLNQ-(*-RQUPVAL))                                          
                                                                                
SAVEL    EQU   *-SAVED                                                          
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
       ++INCLUDE GEGENCUR                                                       
         PRINT ON                                                               
                                                                                
         EJECT                                                                  
                                                                                
* TSAR RECORD DSECT                                                             
                                                                                
ZTSARECD DSECT                                                                  
ZTRLEN   DS    XL2                 length of TSAR record                        
ZTRKEY   DS    0X                  key data                                     
ZTRKSRB  DS    0XL2                - big sort order                             
         DS    XL1                                                              
ZTRKELS  DS    XL1                 - element sort order                         
ZTRKSEQ  DS    XL2                 - sequence number (erdelds = 0000)           
ZTRKCSQ  DS    XL1                 - category sequence                          
ZTRKWUS  DS    XL2                 - W/C upload sequence                        
ZTRKIUS  DS    XL1                 - item upload sequence                       
ZTRKSSQ  DS    XL1                 - subtype sequence                           
ZTRKSUS  DS    XL2                 - subelement sequence                        
ZTRKEYL  EQU   *-ZTRKEY                                                         
ZTRDATA  DS    0X                  buffer data                                  
ZTRKWCS  DS    XL2                 - W/C order                                  
ZTRKITS  DS    XL1                 - item sequence                              
ZTRDLEL  EQU   *-ZTRDATA                                                        
ZTRDELM  DS    XL256               - element                                    
ZTRLENQ  EQU   *-ZTSARECD                                                       
*                                                                               
* WORKCODE TABLE DSECT                                                          
                                                                                
WCTAB    DSECT                                                                  
WCTSEQ   DS    XL2                 Work code sequence #                         
WCTCODE  DS    CL2                 Work code                                    
WCTNAME  DS    CL36                Work code description                        
WCTABLQ  EQU   *-WCTAB                                                          
MAX#WCS  EQU   L'GENAEXTN/WCTABLQ  Maximum number of work codes                 
                                                                                
***********************************************************************         
* Approval hierarchy search table DSECT                               *         
***********************************************************************         
APRTABD  DSECT                                                                  
APRSTAT  DS    XL1                 Levels to include                            
APRCLI   EQU   X'80'               Client                                       
APRPRO   EQU   X'40'               Product                                      
APRJOB   EQU   X'20'               Job                                          
APRMED   EQU   X'10'               Media                                        
APROFF   EQU   X'08'               Office                                       
APRLVL   DS    CL1                 Approval level                               
APRTABL  EQU   *-APRTABD                                                        
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'098ACBRA18   12/15/20'                                      
         END                                                                    
