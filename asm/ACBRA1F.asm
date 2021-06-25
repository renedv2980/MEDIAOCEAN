*          DATA SET ACBRA1F    AT LEVEL 093 AS OF 11/02/20                      
*PHASE T6241FA                                                                  
                                                                                
ACBRA1F  TITLE '- BrandOcean New Style Order Downloads'                         
                                                                                
* Level change comments                                                         
* ---------------------                                                         
* UK Levels                                                                     
* ---------                                                                     
* TKLU 001 13JUN07 New style server set up                                      
* TKLU 002 16JUL07 Additional audit comments to be displayed                    
* TKLU 003 26JUL07 Process and download 'Main order data'                       
* TKLU 004 20SEP07 <DU01-6236> Text d/load mapping typo                         
* NSHE 005 08NOV07 <LO01-6995> Set FACPAK code for analysis                     
* TKLU 006 10NOV07 <DU01-6289> Allow for header and footer text                 
* TKLU 007 24NOV07 <DU01-6519> Final fixes pre release 'articles'               
* TKLU 008 29NOV07 Bug fixes for copy/order mode                                
* TKLU 009 11DEC07 US merger + copy call: skip some return data                 
* TKLU 010 01FEB08 <UKCR00016047> 'copy order' change status return             
* TKLU 011 25FEB08 US Merger JSHA                                               
* TKLU 012 10MAR08 <UKCR00016527> Xtra data field - amount return field         
*                  + preparations for 'Item on Order/Copy validation'           
* TKLU 013 02APR08 Field number adjustments towards old order/display           
* TKLU 014 22APR08 <DU01-7554> GETFOR bug fix                                   
* TKLU 014 17APR08 SOSCM bug fix                                                
* NSHE 014 17APR08 Show invoiced amount                                         
* NSHE 015 29APR08 Extend order name to 100 characters + some bug fixes         
* TKLU 016 12JUN08 <LO01-5729> Foreign name support additions                   
* TKLU 017 20JUN08 <DU01-7072> OAMFCAMT bug fix in SETOAM                       
* TKLU 018 09JUL08 <DU01-7767> Foreign name support (item texts)                
* MPEN 019 13AUG08 <DU01-7726> XDLIST fields                                    
* TKLU     27AUG08 <UKCR00018937> Address handling bug fix                      
* TKLU     03SEP08 <DU01-8040> UNIT support for items/articles                  
* JFOS 020 15SEP08 <BR200079L> Ensure OD#FCOD cleared if not set                
* TKLU 021 29SEP08 <UKCR00019401> Order Approver display bug fix                
* TKLU 022 07OCT08 <UKCR00019428> Internal order SJ display bug fix             
* TKLU 023 01DEC08 <LO01-5729> Foreign name support addition                    
* TKLU 024 08DEC08 <DU01-8048> Save order report format to order                
* TKLU 025 09DEC08 <UKCR00020323> ARTSTFQ/flexible price flag                   
* JFOS 026 06NOV08 <LO01-7636> Return extra flags if Invoices call              
* TKLU 027 21JAN09 <DU01-8087> Return phone extension of order raiser           
* JFOS 029 16MAR09 <LO01-7636> Another Supplier VAT FFTEL for Germany           
*                              Return due date formula                          
* JFOS     09APR09 <LO01-7636> Simplify order status for Invoice Log            
* JFOS     24APR09 <LO01-7636> Return errors if invalid/missing order           
* TKLU 030 14MAY09 <LO01-7636> New ORDASTA returned in download                 
* MPEN 031 20MAR09 <NY02-0028> Return new setting for xdata req field           
* MPEN     06APR09 <LO01-8463> CONTROL NOTIFICATIONS ON SUBMIT                  
* JFOS     07JUL09 <UKCR22715> Return KSV Exp anal flags on blb inv too         
* JFOS     20JUL09 <UKCR23452> Return office-lvl aplim for order aprvrs         
* JFOS 032 19AUG09 <UKCR23729> Return F/M orders if Br'Invs, not InvLog         
* JFOS 034 21SEP09 <BR27514L>  Skip SETAPP if SETALIM not executed              
* MPEN 035 30SEP09 <BR14727> Fix for email remind setting being clear           
* NSHE 037 07OCT09 Fix to include dropdown extra data                           
* NSHE 038 12OCT09 Fix to show billable and non billable opt maint              
* JFOS 039 15OCT09 <UKCR25074> Set GETOPT KSV flags only if supp on KSV         
* NSHE 040 30OCT09 Merge US changes into source in UK                           
* NSHE 041 06NOV09 Don't display ledger only for supplier                       
* NSHE 042 07JAN10 Extend length of surname                                     
* JFOS 043 21JAN10 <LO01-9343> Order raiser from presto order if set            
* NSHE 044 03FEB10 Pass client office in order download                         
* JFOS 045 08FEB10 <BR15426D> always init Expense status area                   
* JFOS 046 23FEB10 <BR31234L> fix bug identifying Presto order                  
* TKLU             <PR000004> Order owner implemented                           
* nrak 047 21may10 <BR16020D> fix foreign supplier name display                 
* JFOS 048 22JUN10 <UKCR28345> Return f/c order total + inv'd to date           
* JFOS 050 28JUN10 <BR16207D> Allow closed orders if Invoices call              
* NSHE 051 05AUG10 Change to global storage and order display                   
* MPEN 052 15DEC10 <PR001254> Change all downloads to show job lock sta         
*          03SEP10 <PR000716> Mainframe changes for GAP orders                  
* NSHE 052 04FEB11 Return addresses for client/prod/job                         
* MPEN 054 13MAY11 <UKCR00032176> US show business email address                
* JFOS 055 15MAY11 <BR17985D> Use GENAEXTN to build 'super-order'               
* YNGX 056 23MAY11 <BR18001D> Bug fix for order GAP status                      
* NSHE 057 17MAY12 Remove duplicate approvers from orders                       
* TKLU 058 27JUL12 <BR20023D> Text buffer size adjustment (ACBRA13)             
* NRAK 059 06JUL12 <BR19914D> support XDLREC cutoff dates                       
* NSHE     11SEP12 <PR002898> Support order amendment once matched              
* NSHE 060 31MAY13 <DSBO-28> Ensure we skip extra workcodes                     
* NSHE     14JUN13 <DSBO-26> Handle more text by changing structure             
* NSHE 061 25JUN13 <DSBO-26> Use binary instead of hex for sequence num         
* NRAK 062 27JUN13 <br21396d> fix ioareas for status check                      
* JFOS 063 18DEC13 <DSBO576> Fix error return from SUPGAP                       
* NSHE 064 25FEB14 <DSBO242> Send workcode extra name                           
* NSHE 065 24SEP14 <DSRD-4448> Send xdata as HEX data with zero prefixs         
* TKLU     16Oct14 <RD004766> OrderDisplay by requisition number                
* TKLU     07Nov14 <RD004949> Aura MVP version - error if not standard          
*                             order (date bound, see AURAMVP)                   
* TKLU 066 04Dec14 <RD005066> Show uncommitted amount (Aura) - as total         
*          03Mar15            and broken down by work code                      
* MPEN     17Dec14 <RD005423> Add security to order display                     
* NSHE     31Dec14 <DSRD-5628> Return if job is closed                          
* NSHE     02Jan14 <DSRD-5642> Return if supplier is locked                     
* MPEN     16Jan15 <DSRD-5793> Allow user to view own orders                    
* TKLU     20Jan15 <RD005824> AURAMVP - allow ORDGNSNT GAP status               
* MPEN     20Jan15 <RD005836> Display last rej details on display               
* YNGX     20Jan15 <RD005848> Change err msg if order contains items            
* TKLU     09Feb15 <RD006079> Order Display - IsPDF=Yes request value           
* TKLU     11Dec14 <RD005066> Additional separate call for uncommitted          
* NRAK     16Feb15 <DSRD-6185> Fix IDNo/EtypeName responses                     
* TKLU     19Feb15 <RD006249> Add agency currency to order display              
* TKLU     20Feb15 <RD006261> Add XData field code/name to OrderDisplay         
* TKLU     24Feb15 <RD006318> Count real work codes in count only               
* MPEN     25Feb15 <RD006321> Correct extract of today's date                   
* TKLU     27Feb15 <RD006336> Adjust 'View all' and 'Override LimList'          
* MPEN     09Mar15 <RD006390> Changes for application in audit                  
* MPEN 067 25Mar15 <RD006651> Move application byte for Aura                    
* NSHE     01Apr15 <DSRD-6759> Don't put out supplier when ledger only          
* NRAK     01Apr15 <DSRD-6779> fix Matched Status edit code                     
* MPEN 068 08Apr15 <DSRD-6813> Fix reading of estimate number                   
* NSHE 069 13Apr15 <DSRD-6874> Don't output office when dept change             
* YNGX     14Apr15 <DSRD-6889> Don't sent XDFMXLN if value is 0                 
* TKLU     16Apr15 <DSRD-6898> Fix to uncommitted amount                        
* NSHE     24Apr15 <DSRD-6930> Always send xdata records                        
* NSHE 070 28Apr15 <DSRD-7006> Remove currency stop for Aura orders             
* TKLU     29Apr15 <RD007029> Uncommitted contingency fix                       
* TKLU     06May15 <RD007099> OrderDisplay request parm for order types         
* NSHE     06May15 <DSRD-7109> Ensure approver can see their orders             
* NSHE 071 18May15 <DSRD-7244> Check override limit list security               
* NSHE 072 08Jun15 <DSRD-7352> Error when using an order number and is          
* NSHE                         a requisition                                    
* NSHE     12Jun15 <DSRD-7563  Don't show suppress for print for Aura           
* MPEN 073 16Jun15 <DSBO-1505> Don't clear OD#NUM for copy call                 
* NSHE 074 18Jun15 <DSRD-7649> Read for office record in audit                  
* MPEN 075 03Jul15 <DSRD-7775> Return w/c foreign name and extra name           
* NSHE 076 12Jan16 <DSRD-10052> Show estimate name                              
* NSHE     12Jan16 <DSRD-7318> Set subsequence to 1 when zero for Aura          
* NSHE 077 04Feb16 <DSRD-10248> Show approver comments for Aura                 
* NSHE     10Feb16 <DSRD-10327> Show order with items in Aura                   
* NSHE     25Feb16 <DSRD-10505> Fix copy indicator for items with Aura          
* NSHE     06Apr16 <DSRD-10910> Return foreign name for items                   
* MPEN 078 03Jan17 <DSRD-14501> Fix for extra workcode name not return          
* MPEN 079 09Jan17 <DSRD-14595> Aura disable expense order with items           
* MPEN 080 19Jan17 <DSRD-14701> Fix for media limlist issue                     
* MPEN 081 23Mar17 <DSRD-14959> Allow suppress print/text for Aura              
* NSHE 082 21Jul17 <DSRD-16407> Allow expense orders with items in Aura         
* NSHE 083 16Aug17 <DSRD-16357> Amend status for unmatched closed order         
* NSHE 084 27Apr18 <DSRD-18685> Ensure expense PO treated correctly             
* MPEN 085 13Mar19 <DSRD-21609> Include debtor name on PDF                      
* YNGX     15Apr19 <DSRD-22274> Return isJobLockedFromTimesheets                
* MPEN 086 30Apr19 <DSRD-22426> Send order to supplier changes                  
* NSHE 087 30Aug19 <DSRD-23314> Ensure audit download sends attention,          
* NSHE     estimate number and item text changes                                
* MPEN 088 03Feb20 <DSRD-25387> Fix for uncommitted amount                      
* MPEN 089 10Feb20 DSRD-25377 Relink book                                       
* MPEN 090 07Apr20 <DSRD-25344> Orders extend rej comments                      
* SGAV 091 26May20 DSRD-25716 Save the overridden work code desc to MF          
* NSHE     08Jun20 DSRD-26583 Return currency decimal places                    
* NSHE     03Jul20 DSRD-26791 Return office name                                
* MPEN 092 26Aug20 DSRD-27018 Return mobile application                         
* MPEN 093 02Nov20 DSRD-27930 Fix for bad branch for STCOGAPQ                   
                                                                                
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,                                                 +        
               SERVERTYPE=TSTACBO,WORKERKEY=ACBO,                      +        
               SEGMENT=Y,APPEND=Y,IDF=Y,                               +        
               REQUEST=*,CODE=CODE,                                    +        
               SYSPHASE=X'0624',SYSTEM=ACCSYSQ,                        +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,                    +        
               B#LP_D,LP_D,B#TWAD,TWAD,B#SVRDEF,SVRDEF,                +        
               B#GOBLK,GOBLOCKD,B#GOXBLK,GOXBLKD,                      +        
               B#GOBBLK,GOBBLKD,                                       +        
               B#ACCREC,ACCRECD,B#ORDREC,ORDRECD,B#XDFREC,XDFRECD,     +        
               B#XDLREC,XDLRECD,B#AUD,AUDRECD)                                  
         EJECT                                                                  
                                                                                
CODE     NMOD1 0,**BO1F**,RR=RE                                                 
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(DDLINK PARAMETER BLOCK)                 
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   INIT02                                                           
         ICM   R9,15,LP_ABLK1      ROOT SETS A(WORKD)                           
         JZ    *+2                                                              
         ICM   R8,15,RSVRSAVE      R8=A(4K SAVE AREA)                           
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         LA    R1,WORKD                                                         
         ST    R1,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         B     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         XR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
INIT04   MVC   ATWA,LP_ATWA                                                     
         MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ABUFFRIN,RBUFFRIN                                                
         DROP  R6,R7                                                            
                                                                                
         USING SAVED+4096,R7       R6=A(2ND 4K OF W/S)                          
         LR    R7,R8                                                            
         AHI   R7,4096                                                          
                                                                                
         MVI   TWAMODE,0                                                        
                                                                                
         ST    R5,ALP              SAVE A(LP_D)                                 
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         EJECT                                                                  
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
                                                                                
         L     R0,ATIA                                                          
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNSTR02            NO                                           
                                                                                
         MVC   AROUT1,LP_AUIR1     SET LOCAL INDEX ROUTINE ADDRESSES            
         MVC   AROUT2,LP_AUIR2     WHICH WERE LOADED BY MASTER SERVER           
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE SERVER WORKING STORAGE            
         B     RUNSTR04                                                         
                                                                                
RUNSTR02 GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
                                                                                
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
                                                                                
RUNSTR04 DS    0H                                                               
         MVC   LP_BLKS+((B#ORDREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO2                 
         MVC   LP_BLKS+((B#XDFREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO3                 
         MVC   LP_BLKS+((B#XDLREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO4                 
         MVC   LP_BLKS+((B#AUD-1)*L'LP_BLKS)(L'LP_BLKS),AIO5                    
         MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
         MVC   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)(L'LP_BLKS),LP_ASVR              
         MVC   LP_BLKS+((B#GOBLK-1)*L'LP_BLKS)(L'LP_BLKS),AGOBLOCB              
         MVC   LP_BLKS+((B#GOXBLK-1)*L'LP_BLKS)(L'LP_BLKS),AGOXBLCK             
         MVC   LP_BLKS+((B#GOBBLK-1)*L'LP_BLKS)(L'LP_BLKS),AGOBBLCK             
         MVC   LP_BLKS+((B#LP_D-1)*L'LP_BLKS)(L'LP_BLKS),ALP                    
                                                                                
         MVC   WVALUES(WVALUEL),LVALUES                                         
                                                                                
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,ADCONSN                                                       
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         A     R2,SRVRRELO                                                      
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* First for new work                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         XC    QVALUES(QVALUEL),QVALUES                                         
         XC    DVALUES(DVALUEL),DVALUES                                         
         LA    R0,OVALUES                                                       
         LHI   R1,OVALUESL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    RUNI,RUNI                                                        
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Run a download request                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
                                                                                
         GOTOR VDATCON,DMCB,(5,0),(1,OD#TODYP)                                  
         GOTOR (RF),(R1),,(2,OD#TODYC)                                          
                                                                                
         L     RE,LP_AWMP          BUILD DEFAULT RANGE ELEMENTS IN WMP          
         USING LW_D,RE                                                          
         MVI   LW_TYPE,LW_TALLQ    ALL VALUES                                   
         STCM  RE,7,AALL                                                        
         AHI   RE,LW_LN1Q                                                       
         MVI   LW_TYPE,LW_TNZRQ    NON-ZERO VALUES                              
         STCM  RE,7,ANZR                                                        
         AHI   RE,LW_LN1Q                                                       
         ST    RE,LP_AWMP                                                       
         DROP  RE                                                               
                                                                                
         XC    MAPI,MAPI           INITIALIZE MAP INDICATORS                    
         LA    RF,MAPTAB                                                        
         LHI   R0,MAPTABN                                                       
         BASR  RE,0                                                             
         CLC   LP_QMAPN,0(RF)                                                   
         JNE   RUNREQ02                                                         
         MVC   MAPI,L'LP_QMAPN(RF)                                              
         J     RUNREQ04                                                         
                                                                                
RUNREQ02 AHI   RF,L'MAPTAB                                                      
         BCTR  R0,RE                                                            
                                                                                
RUNREQ04 TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNREQ06                                                         
                                                                                
         L     RF,AMASTC           SET TRACE OPTION                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
                                                                                
RUNREQ06 MVI   GIND2,GI2EBUY                                                    
         GOTOR (#CPYINI,ACPYINI)   INITIALISE COMPANY VALUES                    
         GOTOR (#ORDPRF,AORDPRF),DMCB,XL#ORPF                                   
         MVC   QAGENCY,CUXCPY                                                   
                                                                                
         GOTOR INIOBUF                                                          
                                                                                
RUNREQX  GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY               EXIT BACK TO DDLINK                          
         DROP  RB                                                               
DUMMYD   DSECT                                                                  
DUMMYX   DS    0X                                                               
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Order combined download                                             *         
***********************************************************************         
                                                                                
REQORD   LKREQ H,A#OCDL,OUTORD,NEXTREQ=REQOUCA                                  
OrdNo    LKREQ F,1,(D,B#SAVED,QORDNUM),CHAR,OLEN=L'QORDNUM,            +        
               MAXLEN=L'QORDNUM,TEXT=(*,ORDNLIT),COL=*                          
DlSet    LKREQ F,2,(D,B#SAVED,QDLSETS),CHAR,OLEN=L'QDLSETS,            +        
               MAXLEN=L'QDLSETS,TEXT=(*,DLSELIT),COL=*                          
DMode    LKREQ F,3,(D,B#SAVED,QDIMODE),CHAR,OLEN=L'QDIMODE,            +        
               MAXLEN=L'QDIMODE,TEXT=(*,DMODLIT),COL=*                          
CopCu    LKREQ F,4,(D,B#SAVED,QCOPCUR),CHAR,OLEN=L'QCOPCUR,            +        
               MAXLEN=L'QCOPCUR,TEXT=(*,DCOPCUR),COL=*                          
CopEx    LKREQ F,5,(D,B#SAVED,QCOPEXR),CHAR,OLEN=L'QCOPEXR,            +        
               MAXLEN=L'QCOPEXR,TEXT=(*,DCOPEXR),COL=*                          
Ivdat    LKREQ F,6,(D,B#SAVED,QIVDATE),CHAR,OLEN=L'QIVDATE,            +        
               MAXLEN=L'QIVDATE,TEXT=(*,DIVDATE),COL=*                          
ReqNo    LKREQ F,7,(D,B#SAVED,QREQNUM),CHAR,OLEN=L'QREQNUM,            +        
               MAXLEN=L'QREQNUM,TEXT=(*,REQNLIT),COL=*                          
ShowUC   LKREQ F,8,(D,B#SAVED,QUNCOMM),CHAR,OLEN=L'QUNCOMM,            +        
               MAXLEN=L'QUNCOMM,TEXT=(*,UNCOLIT),COL=*                          
All      LKREQ F,9,(D,B#SAVED,QALL),CHAR,OLEN=L'QALL,                  +        
               MAXLEN=L'QALL,TEXT=(*,VWALLLIT),COL=*                            
Ovlls    LKREQ F,10,(D,B#SAVED,QORDSOVL),CHAR,OLEN=L'QORDSOVL,         +        
               MAXLEN=L'QORDSOVL,TEXT=(*,SOVLLIT),COL=*                         
IsPDF    LKREQ F,11,(D,B#SAVED,QISPDF),CHAR,OLEN=L'QISPDF,             +        
               MAXLEN=L'QISPDF,TEXT=AC#PDFS,COL=*                               
InclExp  LKREQ F,12,(D,B#SAVED,QODIEXP),CHAR,OLEN=L'QODIEXP,           +        
               MAXLEN=L'QODIEXP,TEXT=(*,LITOIEXP),COL=*                         
InclInt  LKREQ F,13,(D,B#SAVED,QODIINT),CHAR,OLEN=L'QODIINT,           +        
               MAXLEN=L'QODIINT,TEXT=(*,LITOIINT),COL=*                         
InclPro  LKREQ F,14,(D,B#SAVED,QODIPRO),CHAR,OLEN=L'QODIPRO,           +        
               MAXLEN=L'QODIPRO,TEXT=(*,LITOIPRO),COL=*                         
Ovlla    LKREQ F,15,(D,B#SAVED,QORDOVL),CHAR,OLEN=L'QORDOVL,           +        
               MAXLEN=L'QORDOVL,TEXT=(*,OVLLIT),COL=*                           
         LKREQ E                                                                
                                                                                
OUTORD   LKOUT H                                                                
                                                                                
MAIORD   LKOUT R,A#ODIS            Main order data                              
Array    LKOUT C,A#ODIS,(A,ARYMAIN) Main order record                           
         LKOUT E                                                                
                                                                                
SQORD    LKOUT R,A#ODIS            Sequential order records                     
Array    LKOUT C,A#ODIS,(A,ARYSEQ)   - for text and items information           
         LKOUT E                                                                
                                                                                
EXORD    LKOUT R,A#ODIS            Extension order record                       
Array    LKOUT C,A#ODIS,(A,ARYEXT)   - for extra data information               
         LKOUT E                                                                
                                                                                
XDFHDR   LKOUT R,A#XDFA            Xtra data - headers                          
Array    LKOUT C,A#XDFA,(A,ARYXDFH)                                             
         LKOUT E                                                                
                                                                                
AUDORD   LKOUT R,A#OAUD            Audit data                                   
Array    LKOUT C,A#OAUD,(A,ARYCAUD),FILTROUT=TSTAUD                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYMAIN  LKOUT A,(R,GETORD),NROWS=1,ROWNAME=ORDRECD                             
Array    LKOUT C,A#OCDL,(A,ARYOAM)                                              
Prout    LKOUT P,,SETALIM                                                       
Array    LKOUT C,104,(A,ARYPID),FILTROUT=TSTCOP                                 
OrdTy    LKOUT C,01,(D,B#SAVED,OD#TYP),CHAR,ND=Y                                
OrdNo    LKOUT C,02,(D,B#SAVED,OD#NUM),CHAR,ND=Y,FILTROUT=TSTCOP                
ReqNo    LKOUT C,03,(D,B#SAVED,OD#REQ),CHAR,ND=Y                                
ExpAc    LKOUT C,04,(D,B#SAVED,OD#EXP),CHAR,ND=Y                                
ExpNa    LKOUT C,05,(D,B#SAVED,OD#EXPN),CHAR,ND=Y                               
CliCd    LKOUT C,06,(D,B#SAVED,OD#CLI),CHAR,ND=Y                                
CliNa    LKOUT C,07,(D,B#SAVED,OD#CLIN),CHAR,ND=Y                               
CliAdr1  LKOUT C,08,(D,B#SAVED,OD#CLIA1),CHAR,ND=Y                              
CliAdr2  LKOUT C,09,(D,B#SAVED,OD#CLIA2),CHAR,ND=Y                              
CliAdr3  LKOUT C,10,(D,B#SAVED,OD#CLIA3),CHAR,ND=Y                              
CliAdr4  LKOUT C,11,(D,B#SAVED,OD#CLIA4),CHAR,ND=Y                              
CliAdr5  LKOUT C,12,(D,B#SAVED,OD#CLIA5),CHAR,ND=Y                              
ProCd    LKOUT C,13,(D,B#SAVED,OD#PRO),CHAR,ND=Y                                
ProNa    LKOUT C,14,(D,B#SAVED,OD#PRON),CHAR,ND=Y                               
ProAdr1  LKOUT C,15,(D,B#SAVED,OD#PROA1),CHAR,ND=Y                              
ProAdr2  LKOUT C,16,(D,B#SAVED,OD#PROA2),CHAR,ND=Y                              
ProAdr3  LKOUT C,17,(D,B#SAVED,OD#PROA3),CHAR,ND=Y                              
ProAdr4  LKOUT C,18,(D,B#SAVED,OD#PROA4),CHAR,ND=Y                              
ProAdr5  LKOUT C,19,(D,B#SAVED,OD#PROA5),CHAR,ND=Y                              
JobCd    LKOUT C,20,(D,B#SAVED,OD#JOB),CHAR,ND=Y                                
JobNa    LKOUT C,21,(D,B#SAVED,OD#JOBN),CHAR,ND=Y                               
JobAdr1  LKOUT C,22,(D,B#SAVED,OD#JOBA1),CHAR,ND=Y                              
JobAdr2  LKOUT C,23,(D,B#SAVED,OD#JOBA2),CHAR,ND=Y                              
JobAdr3  LKOUT C,24,(D,B#SAVED,OD#JOBA3),CHAR,ND=Y                              
JobAdr4  LKOUT C,25,(D,B#SAVED,OD#JOBA4),CHAR,ND=Y                              
JobAdr5  LKOUT C,26,(D,B#SAVED,OD#JOBA5),CHAR,ND=Y                              
Jblck    LKOUT C,27,(D,B#SAVED,OD#JOBL),CHAR,ND=Y                               
SupCd    LKOUT C,28,(D,B#SAVED,OD#SUP),CHAR,ND=Y                                
SupNa    LKOUT C,29,(D,B#SAVED,OD#SUPN),CHAR,ND=Y                               
DepCd    LKOUT C,30,(D,B#SAVED,OD#DEP),CHAR,ND=Y                                
DepNa    LKOUT C,31,(D,B#SAVED,OD#DEPN),CHAR,ND=Y                               
PerCd    LKOUT C,32,(D,B#SAVED,OD#PER),CHAR,ND=Y                                
PerNa    LKOUT C,33,(D,B#SAVED,OD#PERN),CHAR,ND=Y                               
OStat    LKOUT C,34,(D,B#SAVED,OD#STT),CHAR,ND=Y                                
ODate    LKOUT C,35,(D,B#SAVED,OD#DTE),PDAT,ND=Y                                
OffCd    LKOUT C,36,(D,B#SAVED,OD#OFF),CHAR,ND=Y                                
EType    LKOUT C,37,(D,B#SAVED,OD#ETY),CHAR,ND=Y                                
Total    LKOUT C,38,(D,B#SAVED,OD#TOT),SPAK,ND=Y                                
Atten    LKOUT C,39,(D,B#SAVED,OD#ATO),CHAR,ND=Y                                
CurCd    LKOUT C,40,(D,B#SAVED,OD#CUR),CHAR,ND=Y                                
ExRat    LKOUT C,41,(D,B#SAVED,OD#RATC),CHAR,ND=Y                               
NumWC    LKOUT C,42,(D,B#SAVED,OD#WC#),LBIN,FILTROUT=FLTAURA                    
NumWC    LKOUT C,42,(D,B#SAVED,OD#WC#),HEXD,ND=Y,FILTROUT=FLTBRAND              
AutBy    LKOUT C,43,(D,B#SAVED,OD#AUT),CHAR,ND=Y                                
RbyDt    LKOUT C,44,(D,B#SAVED,OD#RBYE),PDAT,ND=Y                               
MText    LKOUT C,45,(D,B#SAVED,OD#TXTM),CHAR,ND=Y                               
PText    LKOUT C,46,(D,B#SAVED,OD#TXTP),CHAR,ND=Y                               
Addr1    LKOUT C,47,(D,B#SAVED,OD#SAD1),CHAR,ND=Y                               
Addr2    LKOUT C,48,(D,B#SAVED,OD#SAD2),CHAR,ND=Y                               
Addr3    LKOUT C,49,(D,B#SAVED,OD#SAD3),CHAR,ND=Y                               
Addr4    LKOUT C,50,(D,B#SAVED,OD#SAD4),CHAR,ND=Y                               
Agent    LKOUT C,51,(D,B#SAVED,OD#AGTC),CHAR,ND=Y                               
AgNam    LKOUT C,52,(D,B#SAVED,OD#AGTN),CHAR,ND=Y                               
AgAd1    LKOUT C,53,(D,B#SAVED,OD#AGT1),CHAR,ND=Y                               
AgAd2    LKOUT C,54,(D,B#SAVED,OD#AGT2),CHAR,ND=Y                               
AgAd3    LKOUT C,55,(D,B#SAVED,OD#AGT3),CHAR,ND=Y                               
AgAd4    LKOUT C,56,(D,B#SAVED,OD#AGT4),CHAR,ND=Y                               
RqbyT    LKOUT C,57,(D,B#SAVED,OD#RBYO),CHAR,ND=Y                               
RaisC    LKOUT C,58,(D,B#SAVED,OD#RAIC),CHAR,ND=Y                               
RaisF    LKOUT C,59,(D,B#SAVED,OD#RAIN),CHAR,ND=Y                               
RaisL    LKOUT C,60,(D,B#SAVED,OD#RAIL),CHAR,ND=Y                               
RaisE    LKOUT C,61,(D,B#SAVED,OD#RAIE),CHAR,ND=Y                               
MisAp    LKOUT C,62,(D,B#SAVED,OD#MAPP),HEXD,ND=Y                               
IDNum    LKOUT C,63,(D,B#SAVED,OD#IDNO),HEXD,FILTROUT=FLTAURA                   
IDNum    LKOUT C,63,(D,B#SAVED,OD#IDNO),HEXD,ND=Y,FILTROUT=FLTBRAND             
ETyNA    LKOUT C,64,(D,B#SAVED,OD#ETYN),CHAR,ND=Y                               
ETyFN    LKOUT C,65,(D,B#SAVED,OD#ETYF),CHAR,ND=Y                               
EstNo    LKOUT C,66,(D,B#SAVED,OD#ESTN),CHAR,ND=Y                               
PrtAm    LKOUT C,67,(D,B#SAVED,OD#PRTA),CHAR,ND=Y                               
PrtTx    LKOUT C,68,(D,B#SAVED,OD#PRTT),CHAR,ND=Y                               
FNinU    LKOUT C,69,(D,B#SAVED,OD#FNIU),CHAR,ND=Y                               
ExWcC    LKOUT C,70,(D,B#SAVED,OD#EOWC),CHAR,ND=Y                               
ExWcD    LKOUT C,71,(D,B#SAVED,OD#EOWD),CHAR,ND=Y                               
ClNaP    LKOUT C,72,(D,B#SAVED,OD#PCLN),CHAR,ND=Y                               
PrNaP    LKOUT C,73,(D,B#SAVED,OD#PPRN),CHAR,ND=Y                               
ForCd    LKOUT C,74,(D,B#SAVED,OD#FCOD),CHAR,ND=Y                               
OName    LKOUT C,75,(D,B#SAVED,OD#ONAM),CHAR,ND=Y                               
ODelA    LKOUT C,76,(D,B#SAVED,OD#ODAD),CHAR,ND=Y                               
SuFax    LKOUT C,77,(D,B#SAVED,OD#SFAX),CHAR,ND=Y                               
SuEma    LKOUT C,78,(D,B#SAVED,OD#SEMA),CHAR,ND=Y                               
EstNa    LKOUT C,79,(D,B#SAVED,OD#ENAM),CHAR,ND=Y                               
SupFn    LKOUT C,80,(D,B#SAVED,OD#SUPF),CHAR,ND=Y                               
Ivtot    LKOUT C,81,(D,B#SAVED,OD#ITOT),SPAK,ND=Y                               
ExpFN    LKOUT C,82,(D,B#SAVED,OD#EXPF),CHAR,ND=Y                               
DepFN    LKOUT C,83,(D,B#SAVED,OD#DEPF),CHAR,ND=Y                               
PerFN    LKOUT C,84,(D,B#SAVED,OD#PERF),CHAR,ND=Y                               
                                                                                
ExWcF    LKOUT C,85,(D,B#SAVED,OD#EOWF),CHAR,ND=Y                               
Ivvtc    LKOUT C,86,(D,B#SAVED,OD#IVTC),CHAR,ND=Y                               
IVKSV    LKOUT C,87,(D,B#SAVED,OD#IKSV),SPAK,ND=Y                               
IVDSC    LKOUT C,88,(D,B#SAVED,OD#IDSC),SPAK,ND=Y                               
RFrmt    LKOUT C,89,(D,B#SAVED,OD#RFMT),CHAR,ND=Y                               
RaisT    LKOUT C,90,(D,B#SAVED,OD#RAIT),CHAR,ND=Y                               
Sddxp    LKOUT C,91,(D,B#SAVED,OD#DDXP),CHAR,ND=Y                               
IPINN    LKOUT C,92,(D,B#SAVED,OD#INUM),SPAK,ND=Y                               
AStat    LKOUT C,93,(D,B#SAVED,OD#ASTA),CHAR,ND=Y                               
BillO    LKOUT C,94,(D,B#GOXBLK,GOBILO),CHAR,ND=Y                               
NoJob    LKOUT C,95,(D,B#GOXBLK,GONJLE),CHAR,ND=Y                               
AppSt    LKOUT C,96,(D,B#SAVED,OD#APSTA),CHAR,ND=Y                              
PPORes   LKOUT C,97,(D,B#GOXBLK,GOPPORES),CHAR,ND=Y                             
FPORes   LKOUT C,98,(D,B#GOXBLK,GOFPORES),CHAR,ND=Y                             
MPORes   LKOUT C,99,(D,B#GOXBLK,GOMPORES),CHAR,ND=Y                             
CliOff   LKOUT C,100,(D,B#SAVED,OD#CLOFF),CHAR,ND=Y                             
OwnrC    LKOUT C,101,(D,B#SAVED,OD#OWNC),CHAR,ND=Y                              
OwnrF    LKOUT C,102,(D,B#SAVED,OD#OWNN),CHAR,ND=Y                              
OwnrL    LKOUT C,103,(D,B#SAVED,OD#OWNL),CHAR,ND=Y                              
Totcu    LKOUT C,104,(D,B#SAVED,OD#TOTC),SPAK,ND=Y                              
Itocu    LKOUT C,105,(D,B#SAVED,OD#ITOTC),SPAK,ND=Y                             
DeptAn   LKOUT C,106,(D,B#SAVED,OD#DEPT),CHAR,ND=Y                              
StafAn   LKOUT C,107,(D,B#SAVED,OD#STAFF),CHAR,ND=Y                             
CstGrp   LKOUT C,108,(D,B#SAVED,OD#CSTG),CHAR,ND=Y                              
ProReq   LKOUT C,109,(D,B#SAVED,OD#PROR),CHAR,ND=Y                              
JobReq   LKOUT C,110,(D,B#SAVED,OD#JOBR),CHAR,ND=Y                              
Miles    LKOUT C,111,(D,B#SAVED,OD#MILES),CHAR,ND=Y                             
*&&UK                                                                           
KSVBl    LKOUT C,112,(D,B#GOBLK,GOBILKSV),CHAR,ND=Y                             
*&&                                                                             
BilTy    LKOUT C,113,(D,B#GOBLK,GOBILTYP),CHAR,ND=Y                             
GapYN    LKOUT C,114,(D,B#SAVED,OD#GAPYN),CHAR,ND=Y                             
GapAQ    LKOUT C,115,(D,B#SAVED,OD#GAPAQ),CHAR,ND=Y                             
GapSt    LKOUT C,116,(D,B#SAVED,OD#GAPST),HEXD,ND=Y                             
GapSD    LKOUT C,117,(D,B#SAVED,OD#GAPSD),PDAT,ND=Y                             
GapED    LKOUT C,118,(D,B#SAVED,OD#GAPED),PDAT,ND=Y                             
GapEM    LKOUT C,119,(A,ARYEML)                                                 
NeedAp   LKOUT C,120,(D,B#GOBLK,GONEEDAE),CHAR,ND=Y                             
LckExt   LKOUT C,121,(D,B#SAVED,OD#JLEX),CHAR,ND=Y                              
GAPDEX   LKOUT C,123,(D,B#GOXBLK,GOGDEO),LBIN,ND=Y                              
SmPID    LKOUT C,124,(D,B#SAVED,OD#SBPID),CHAR,ND=Y                             
ExWcXNm  LKOUT C,125,(D,B#SAVED,OD#EOWX),CHAR,ND=Y                              
Uncomtd  LKOUT C,126,(D,B#SAVED,OD#UNCOM),SPAK,ND=Y                             
SupLkd   LKOUT C,127,(D,B#SAVED,OD#SUPL),CHAR,ND=Y                              
RComm    LKOUT C,128,(D,B#SAVED,OD#RJCOM),CHAR,ND=Y                             
RFNam    LKOUT C,129,(D,B#SAVED,OD#RJFNM),CHAR,ND=Y                             
RMNam    LKOUT C,130,(D,B#SAVED,OD#RJMNM),CHAR,ND=Y                             
RLNam    LKOUT C,131,(D,B#SAVED,OD#RJLNM),CHAR,ND=Y                             
RDate    LKOUT C,132,(D,B#SAVED,OD#RJDAT),PDAT,ND=Y                             
RTime    LKOUT C,133,(D,B#SAVED,OD#RJTIM),CHAR,LEN=6,ND=Y                       
AgyCur   LKOUT C,134,(D,B#SAVED,OD#AGYC),CHAR,LEN=3,ND=Y                        
Debacc   LKOUT C,135,(D,B#SAVED,OD#SRAC),FILTROUT=TSTPDF,              +        
               SKIPCOLS=DBSKIPS,CHAR,ND=Y                                       
DBSKIP   EQU   *                                                                
Debacc   LKOUT C,136,(D,B#SAVED,OD#SRAN),CHAR,ND=Y                              
DBSKIPS  EQU   (*-DBSKIP)/LX_COLSL                                              
TimLk    LKOUT C,137,(D,B#SAVED,OD#JLTS),CHAR,ND=Y                              
FCdp     LKOUT C,138,(D,B#SAVED,OD#FCDP),LBIN,ND=Y                              
OffNam   LKOUT C,139,(D,B#SAVED,OD#2DAC),(U,#EDTOFN,$EDTOFN),ND=Y               
         LKOUT E                                                                
                                                                                
ARYOAM   LKOUT A,(D,B#ORDREC,ORDRFST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(OAMEL,OAMELQ),ROWWIDTH=(V,OAMLN)                          
*&&US                                                                           
OamWC    LKOUT C,01,OAMWORK,CHAR,FILTROUT=TSTXTRA,SKIPCOLS=OAMSKIPS             
*&&                                                                             
*&&UK                                                                           
OamWC    LKOUT C,01,OAMWORK,CHAR                                                
*&&                                                                             
OAMSKIP  EQU   *                                                                
OamSW    LKOUT P,,SETWCD                                                        
OamWD    LKOUT C,02,(D,B#SAVED,OD#WCDS),CHAR,ND=Y                               
OamAM    LKOUT C,03,OAMAMNT,SPAK                                                
OamFA    LKOUT C,04,(D,B#WORKD,DUB),SPAK                                        
OamIV    LKOUT C,05,OAMIVAL,SPAK,FILTROUT=TSTCOP                                
OamFI    LKOUT C,06,OAMFCIVL,SPAK,FILTROUT=TSTCOP                               
OamWF    LKOUT C,07,(D,B#SAVED,OD#WCFDS),CHAR,ND=Y                              
OamWSt   LKOUT C,08,(D,B#WORKD,BYTE1),CHAR,ND=Y                                 
OamInv#  LKOUT C,09,OAMINUM,SPAK,PZERO=S,FILTROUT=TSTCOP                        
OamPen#  LKOUT C,10,OAMIPND,LBIN,ND=Y,FILTROUT=TSTCOP                           
OamWxn   LKOUT C,11,(D,B#SAVED,SXNAME),CHAR,ND=Y                                
OAMSKIPS EQU   (*-OAMSKIP)/LX_COLSL                                             
         LKOUT E                                                                
                                                                                
ARYEML   LKOUT A,(D,B#SAVED,OD#GAPEM),NROWS=MAXEML,                    +        
               ROWNAME=OD#GAPEM,ROWWIDTH=L'OD#GAPEM                             
                                                                                
GapEM    LKOUT C,119,OD#GAPEM,CHAR,ND=Y                                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYPID   LKOUT A,(D,B#ORDREC,ORDRFST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(PIDEL,PIDELQ),ROWWIDTH=(V,PIDLN)                          
AppTy    LKOUT C,01,PIDTYPE,HEXD                                                
Prout    LKOUT P,,SETAPP                                                        
Ap1Ap    LKOUT C,02,PIDNTRS,(R,EDTPST),LEN=1                                    
Pid      LKOUT C,03,PIDNTRS+1,(U,#EDTPID,$EDTPID),LEN=2                         
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,04,(D,B#WORKD,TEMP2),CHAR,LEN=16                               
LastNam  LKOUT C,05,(D,B#WORKD,WORK2),CHAR,LEN=58                               
Ap1EM    LKOUT C,06,(D,B#WORKD,APPEMAIL),CHAR,FILTROUT=TSTEML                   
Ap2Ap    LKOUT C,07,PIDNTRS+L'PIDNTRS,(R,EDTPST),LEN=1,FILTROUT=TSTPLN,+        
               SKIPCOLS=5                                                       
Ap2PC    LKOUT C,08,PIDNTRS+1+L'PIDNTRS,(U,#EDTPID,$EDTPID),LEN=2               
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,09,(D,B#WORKD,TEMP2),CHAR,LEN=16                               
LastNam  LKOUT C,10,(D,B#WORKD,WORK2),CHAR,LEN=58                               
Ap2EM    LKOUT C,11,(D,B#WORKD,APPEMAIL),CHAR,FILTROUT=TSTEML                   
Ap1Al    LKOUT C,12,(D,B#SAVED,OD#APPL1),SPAK,ND=Y,FILTROUT=TSTINVO             
Ap2Al    LKOUT C,13,(D,B#SAVED,OD#APPL1),SPAK,ND=Y,FILTROUT=TSTINVO             
                                                                                
         LKOUT E                                                                
                                                                                
                                                                                
ARYSEQ   LKOUT A,(R,SEQORD),MULTIROW=Y,ROWNAME=ORDRECD                          
Array    LKOUT C,A#OCID,(A,ARYITM),FILTROUT=TSTITM   Items                      
Array    LKOUT C,A#OCIT,(A,ARYITX),FILTROUT=TSTITM   Items text                 
Array    LKOUT C,A#ODIT,(A,ARYTXT),FILTROUT=TSTTXT   Foot/head/wc text          
Array    LKOUT C,A#ODIT,(A,ARYBOX),FILTROUT=TSTTXT   Extra printed desc         
         LKOUT E                                                                
                                                                                
ARYITM   LKOUT A,(D,B#ORDREC,ORDRFST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(ARTEL,ARTELQ),ROWWIDTH=(V,ARTLN)                          
ItmWS    LKOUT C,1,ARTWSQN,LBIN                                                 
ItmWX    LKOUT C,2,ARTWSSN,LBIN                                                 
ItmWC    LKOUT C,3,ARTWC,CHAR                                                   
ItmGD    LKOUT P,,SETITM                                                        
ItmSq    LKOUT C,4,(D,B#SAVED,SISEQN),HEXD,LEN=3,ND=Y                           
ItmCd    LKOUT C,5,(D,B#SAVED,SIANUM),CHAR,LEN=4,ND=Y                           
ItmMu    LKOUT C,6,(D,B#SAVED,SIMULT),SPAK,ND=Y                                 
ItmPr    LKOUT C,7,(D,B#SAVED,SIPRIC),SPAK,ND=Y                                 
ItmDs    LKOUT C,8,(D,B#SAVED,SIDESC),CHAR,LEN=50,ND=Y                          
ItmSt    LKOUT C,9,(D,B#SAVED,SISTAT),CHAR,LEN=8,ND=Y                           
ItmOD    LKOUT C,10,(D,B#SAVED,SINAME),CHAR,LEN=36,ND=Y                         
ItmCI    LKOUT C,11,(D,B#SAVED,SICIND),CHAR,LEN=1,ND=Y                          
ItmT1    LKOUT C,12,(D,B#SAVED,SIETX1),CHAR,LEN=180,ND=Y                        
ItmT2    LKOUT C,13,(D,B#SAVED,SIETX2),CHAR,LEN=180,ND=Y                        
ItmUn    LKOUT C,14,(D,B#SAVED,SIUNIT),CHAR,LEN=15,ND=Y                         
ItmUL    LKOUT C,15,(D,B#SAVED,SIUNLA),CHAR,LEN=15,ND=Y                         
ItmOD2   LKOUT C,16,(D,B#SAVED,SINAME2),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
ARYITX   LKOUT A,(D,B#ORDREC,ORDRFST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(ATXEL,ATXELQ),ROWWIDTH=(V,ATXLN)                          
ItTWC    LKOUT C,1,ATXWSQN,HEXD                                                 
ItTSq    LKOUT C,2,ATXWSSN,HEXD                                                 
ItTSs    LKOUT C,3,ATXWCOD,CHAR                                                 
ITTXT    LKOUT C,4,ATXTXT,CHAR,LEN=V                                            
         LKOUT E                                                                
                                                                                
ARYTXT   LKOUT A,(D,B#ORDREC,ORDRFST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(SCMEL,SCMELQ),ROWWIDTH=(V,SCMLN)                          
TxSeq    LKOUT C,1,SCMSEQ,LBIN,FILTROUT=TSTSCM1,SKIPCOLS=TX1SKIPS               
TX1SKIP  EQU   *                                                                
TxSub    LKOUT C,2,SCMSSEQ,LBIN                                                 
TxWCd    LKOUT C,3,SCMWCOD,CHAR                                                 
TxAmt    LKOUT C,4,SCMAMNT,SPAK                                                 
TxTxt    LKOUT C,5,SCMTEXT,CHAR,LEN=V                                           
TxSt1    LKOUT P,,SETWCD                                                        
TxWDs    LKOUT C,8,(D,B#SAVED,OD#WCDS),CHAR,ND=Y                                
TxSt2    LKOUT P,,SETIND                                                        
TxInd    LKOUT C,9,(D,B#WORKD,BYTE1),CHAR,LEN=1,ND=Y                            
Wcfnm    LKOUT C,10,(D,B#SAVED,OD#WCFDS),FILTROUT=TSTPDF,CHAR,ND=Y              
Wcxnm    LKOUT C,11,(D,B#SAVED,SXNAME),FILTROUT=TSTPDF,CHAR,ND=Y                
TX1SKIPS EQU   (*-TX1SKIP)/LX_COLSL                                             
                                                                                
T2Seq    LKOUT C,1,SCMSEQ,LBIN,FILTROUT=TSTSCM2,SKIPCOLS=TX2SKIPS               
TX2SKIP  EQU   *                                                                
T2Txt    LKOUT C,6,SCMTBOX,CHAR,LEN=V                                           
T2BYN    LKOUT C,7,(D,B#SAVED,WNOQ),CHAR,LEN=1,ND=Y                             
T2St2    LKOUT P,,SETIND                                                        
T2Ind    LKOUT C,9,(D,B#WORKD,BYTE1),CHAR,LEN=1,ND=Y                            
TX2SKIPS EQU   (*-TX2SKIP)/LX_COLSL                                             
         LKOUT E                                                                
                                                                                
ARYBOX   LKOUT A,(D,B#ORDREC,ORDRFST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(SCMEL,SCMELQ),ROWWIDTH=(V,SCMLN)                          
BxSeq    LKOUT C,1,SCMSEQ,LBIN,FILTROUT=TSTSCM3,SKIPCOLS=TX3SKIPS               
TX3SKIP  EQU   *                                                                
BxBox    LKOUT C,6,SCMTBOX,CHAR,LEN=V                                           
BxBYN    LKOUT C,7,(D,B#SAVED,WYESQ),CHAR,LEN=1,ND=Y                            
TX3SKIPS EQU   (*-TX3SKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYEXT   LKOUT A,(R,EXTORD),NROWS=1,ROWNAME=ORDRECD                             
Array    LKOUT C,A#ODIX,(A,ARYXDF),FILTROUT=TSTXDF   xdata dat                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYXDF   LKOUT A,(D,B#ORDREC,ORDRFST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(XDFEL,XDFELQ),ROWWIDTH=(V,XDFLN)                          
XDSet    LKOUT P,,SETXDF                                                        
****     LKOUT C,1,(D,B#SAVED,SXDCODE),HEXD,ND=Y                                
XDCod    LKOUT C,1,(D,B#SAVED,SXDCODE),(U,#EDTHEX,$EDTHEX),ND=Y                 
XDTyp    LKOUT C,2,(D,B#SAVED,SXDTYPE),CHAR,ND=Y                                
XDDta    LKOUT C,3,(D,B#SAVED,SXDDATA),CHAR,ND=Y                                
XDAmt    LKOUT C,4,(D,B#SAVED,SXDAMNT),SPAK,ND=Y                                
XDCode   LKOUT C,5,(D,B#SAVED,SXDXCDE),CHAR,FILTROUT=TSTRODK,ND=Y               
XDName   LKOUT C,6,(D,B#SAVED,SXDXNAM),CHAR,FILTROUT=TSTRODK,ND=Y               
         LKOUT E                                                                
                                                                                
SETTYP   L     R1,LP_AINP          Remove application bytes                     
         MVC   BYTE1,0(R1)                                                      
         NI    BYTE1,X'0F'                                                      
         J     EXIT                                                             
                                                                                
SETNAME  GOTOR (#GETPIN,AGETPIN)                                                
         J     EXIT                                                             
                                                                                
FLTAURA  XR    RF,RF               Aura?                                        
         ICM   RF,B'0011',CUXPNUM                                               
         CHI   RF,XPRODIKQ                                                      
         J     EXIT                                                             
                                                                                
FLTBRAND XR    RF,RF               BrandOcean (not Aura)?                       
         ICM   RF,B'0011',CUXPNUM                                               
         CHI   RF,XPRODIKQ                                                      
         J     SETCCC                                                           
                                                                                
         EJECT                                                                  
***********************************************************************         
* Get order records and other routines                                *         
***********************************************************************         
         SPACE 1                                                                
GETORD   MVI   SDMODE,SDMPDFQ                                                   
         XC    SHALF1,SHALF1                                                    
         MVC   SNAMES,SPACES                                                    
         MVC   SCOPCUR,AGYCURR                                                  
         XC    SCOPRAT,SCOPRAT                                                  
         XR    RF,RF               If Aura check we can display ord             
         ICM   RF,3,CUXPNUM         by getting limit list rights                
         CHI   RF,XPRODIKQ         BrandO has full access                       
         JNE   GETORD04                                                         
*                                                                               
         CLI   QORDSOVL,YESQ       Search override limit list                   
         JE    GETORD04                                                         
*                                                                               
         USING GAPTABD,GAPAREA                                                  
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
*                                  Build table for limit list                   
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT23Q',TSARABUF),           +        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),('QORD',0)             
                                                                                
GETORD04 CLI   QDIMODE,C'B'        Test Br'Ocean Invoices call                  
         JNE   *+8                                                              
         OI    SDMODE,SDMODBQ                                                   
         CLI   QDIMODE,C'I'        Test Invoice Log call                        
         JNE   *+8                                                              
         OI    SDMODE,SDMODIQ                                                   
         CLI   QDIMODE,C'C'        COPY mode?                                   
         JNE   GETORD09                                                         
         OI    SDMODE,SDMODCQ                                                   
                                                                                
         CLC   QCOPCUR,SPACES      Currency?                                    
         JNH   GETORD08                                                         
         MVC   SCOPCUR,QCOPCUR     Note that currency isn't dealt with          
         OI    SDMODE,SDMODFQ      throughout the copy process yet|             
                                                                                
GETORD08 CLC   QCOPEXR,SPACES                                                   
         JNH   GETORD09                                                         
         GOTO1 VHEXIN,DMCB,QCOPEXR,SCOPEXR,14                                   
         OI    SDMODE,SDMODFQ                                                   
                                                                                
GETORD09 MVI   SBYTE1,YESQ                                                      
         MVI   LP_RMODE,LP_RFRST   set mode for NXTREC routine                  
                                                                                
         CLC   QDLSETS,SPACES                                                   
         JH    GETORD10                                                         
         MVC   QDLSETS,WDLDEF                                                   
                                                                                
GETORD10 MVC   OD#NUM,QORDNUM                                                   
         CLC   QORDNUM,SPACES                                                   
         JH    GETORD13                                                         
         CLC   QREQNUM,SPACES                                                   
         JH    GETORD11                                                         
         MVC   LP_ERROR,=AL2(AE$NOONO)                                          
         J     QERROR                                                           
                                                                                
         USING ORNPASD,R2                                                       
GETORD11 LA    R2,IOKEY                                                         
         XC    ORNPKEY,ORNPKEY                                                  
         MVI   ORNPTYP,ORNPTYPQ                                                 
         MVI   ORNPSUB,ORNPSUBQ                                                 
         MVC   ORNPCPY,CUXCPY                                                   
         MVC   ORNPRNUM,QREQNUM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    GETORD12                                                         
         MVC   LP_ERROR,=AL2(AE$MIREQ)                                          
         J     QERROR                                                           
         DROP  R2                                                               
                                                                                
GETORD12 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JNE   *+2                                                              
         USING ORDRECD,R2                                                       
         L     R2,AIO2                                                          
         MVC   OD#NUM,ORDKORD                                                   
         DROP  R2                                                               
                                                                                
         USING ORDRECD,R2                                                       
GETORD13 LA    R2,IOKEY                                                         
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUXCPY                                                   
         MVC   ORDKORD,OD#NUM                                                   
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    GETORD14                                                         
         MVC   LP_ERROR,=AL2(AE$ORDNF) No - display error                       
         J     QERROR                                                           
                                                                                
GETORD14 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JNE   *+2                                                              
         L     R2,AIO2             Point to record                              
         MVI   SORDEBY,NOQ                                                      
         TM    ORDRSTA2,ORDSEXEX   TEST BRANDOCEAN ORDER                        
         JZ    *+8                                                              
         MVI   SORDEBY,YESQ                                                     
*                                                                               
         GOTOR AURAMVP,1           Aura MVP version checks                      
         JNE   QERROR                                                           
*                                                                               
         CLC   ORDROFF,SPACES                                                   
         JNH   GETORD16                                                         
                                                                                
         USING OFFALD,R3                                                        
         L     R3,AOFFAREA                                                      
         MVC   OFFAOFFC,ORDROFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD                                                    
         JE    GETORD16                                                         
         MVC   LP_ERROR,=AL2(AE$SECLK)                                          
         J     QERROR                                                           
         DROP  R3                                                               
                                                                                
GETORD16 MVI   BYTE4,SE#ORDS                                                    
         MVI   ORDEML,NOQ                                                       
         GOTOR (#SEMAIL,ASEMAIL),DMCB,ORDROFF,BYTE4                             
         JNE   GETORD17                                                         
         MVI   ORDEML,YESQ                                                      
GETORD17 MVI   SBYTE1,NOQ                                                       
         MVI   LP_RMODE,LP_RNEXT   set mode for NXTREC routine                  
                                                                                
GETORD40 CLI   QDLSORD,YESQ                                                     
         JE    SETORD              unless main data processed anyway            
         TM    SDMODE,SDMODCQ                                                   
         JZ    SETORD              or no 'copy' request                         
                                                                                
         LA    R0,OD#TYP           clear output values                          
         LHI   R1,OD#LENQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   OD#2DUL,=C'2D'                                                   
         MVC   OD#CUR,AGYCURR                                                   
         MVC   SJACCNT,SPACES                                                   
                                                                                
         MVC   OD#OFF,ORDSOFF-ORDRECD(R2)                                       
         USING ORDELD,R2                                                        
         AHI   R2,ORDRFST-ORDRECD                                               
         XR    R0,R0                                                            
                                                                                
SFCORD05 CLI   ORDEL,0                                                          
         JE    SFCORD50                                                         
         CLI   ORDEL,ORDELQ                                                     
         JNE   SFCORD10                                                         
         CLC   ORDSUPA,SPACES      Have we got an account                       
         JNH   *+10                No                                           
         MVC   OD#SUP,ORDSUPU                                                   
         OC    OD#SUP,SPACES                                                    
         CLC   ORDACCU(2),PRODUL                                                
         JNE   SFCORD30                                                         
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#CLI(0),ORDACCA                                                
         J     SFCORD30                                                         
                                                                                
         USING AFCELD,R2                                                        
SFCORD10 CLI   AFCEL,AFCELQ                                                     
         JNE   SFCORD15                                                         
         MVC   OD#CUR,AFCCURR                                                   
         MVC   OD#RAT,AFCX                                                      
         LLC   RF,AFCXSHFT                                                      
         AHI   RF,2                                                             
         STC   RF,OD#FCDP                                                       
         GOTO1 VHEXOUT,DMCB,OD#RAT,OD#RATC,L'OD#RAT                             
         J     SFCORD30                                                         
                                                                                
         USING SORELD,R2                                                        
SFCORD15 CLI   SOREL,SORELQ                                                     
         JNE   SFCORD20                                                         
         CLI   SORSYS,SORSACC                                                   
         JNE   SFCORD30                                                         
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#CLI(0),SORAACT                                                
         J     SFCORD30                                                         
                                                                                
         USING FFTELD,R2                                                        
SFCORD20 CLI   FFTEL,FFTELQ                                                     
         JNE   SFCORD22                                                         
         CLI   FFTTYPE,FFTTWRKC                                                 
         JNE   SFCORD22                                                         
         MVC   OD#EOWC,FFTWORK                                                  
         J     SFCORD30                                                         
*                                                                               
         USING STCELD,R2                                                        
SFCORD22 CLI   STCEL,STCELQ                                                     
         JNE   SFCORD30                                                         
         GOTOR SOSTC                                                            
         J     SFCORD30                                                         
                                                                                
         USING ORDELD,R2                                                        
SFCORD30 IC    R0,ORDLN                                                         
         AR    R2,R0                                                            
         J     SFCORD05                                                         
                                                                                
         USING ORDELD,R2                                                        
SFCORD50 L     R2,AIO2             Point to record and remove all MAIN          
         AHI   R2,ORDRFST-ORDRECD  elements                                     
         MVC   QCLI,OD#CLI                                                      
         XR    R0,R0                                                            
                                                                                
SFCORD55 CLI   ORDEL,0                                                          
         JE    SETORD                                                           
         MVI   ORDEL,FF                                                         
         IC    R0,ORDLN                                                         
         AR    R2,R0                                                            
         J     SFCORD55                                                         
         DROP  R2                                                               
*                                                                               
SETORD   LA    R0,OD#TYP           clear output values                          
         LHI   R1,OD#LENQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   OD#2DUL,=C'2D'                                                   
         MVC   OD#AGYC,AGYCURR                                                  
         ZAP   OD#TOT,PZERO                                                     
         ZAP   OD#ITOT,PZERO                                                    
         ZAP   OD#TOTC,PZERO                                                    
         ZAP   OD#ITOTC,PZERO                                                   
         ZAP   OD#INUM,PZERO                                                    
         ZAP   OD#HINUM,PZERO                                                   
         XC    OD#HIPND,OD#HIPND                                                
         MVC   OD#CUR,AGYCURR                                                   
         XC    OD#GAPSD,OD#GAPSD                                                
         XC    OD#GAPED,OD#GAPED                                                
                                                                                
         L     R2,AIO2             Point to record                              
                                                                                
         GOTOR SOKEY               key and status data                          
                                                                                
         AHI   R2,ORDRFST-ORDRECD                                               
         MVI   OD#APSTA,OD#ANOAP                                                
         XC    APIND1,APIND1                                                    
         USING ORDELD,R2                                                        
SETORD05 CLI   ORDEL,0                                                          
         JE    SETORD80                                                         
         CLI   ORDEL,ORDELQ                                                     
         JNE   SETORD10                                                         
         GOTOR SOORD                                                            
         JE    SETORD60                                                         
         J     QERROR                                                           
                                                                                
SETORD10 CLI   ORDEL,PIDELQ                                                     
         JNE   SETORD15                                                         
         GOTOR SOPID                                                            
         J     SETORD60                                                         
                                                                                
SETORD15 CLI   ORDEL,FFTELQ                                                     
         JNE   SETORD20                                                         
         GOTOR SOFFT                                                            
         J     SETORD60                                                         
                                                                                
SETORD20 CLI   ORDEL,OAMELQ                                                     
         JNE   SETORD25                                                         
         GOTOR SOOAM                                                            
         J     SETORD60                                                         
                                                                                
SETORD25 CLI   ORDEL,AFCELQ                                                     
         JNE   SETORD30                                                         
         GOTOR SOAFC                                                            
         J     SETORD60                                                         
                                                                                
SETORD30 CLI   ORDEL,SCMELQ                                                     
         JNE   SETORD35                                                         
         GOTOR SOSCM                                                            
         J     SETORD60                                                         
                                                                                
SETORD35 CLI   ORDEL,SPAELQ                                                     
         JNE   SETORD40                                                         
         GOTOR SOSPA                                                            
         J     SETORD60                                                         
                                                                                
SETORD40 CLI   ORDEL,SORELQ                                                     
         JNE   SETORD45                                                         
         GOTOR SOSOR                                                            
         J     SETORD60                                                         
                                                                                
SETORD45 CLI   ORDEL,ENMELQ                                                     
         JNE   SETORD50                                                         
         GOTOR SOENM                                                            
         J     SETORD60                                                         
                                                                                
SETORD50 CLI   ORDEL,OATELQ                                                     
         JNE   SETORD52                                                         
         GOTOR SOOAT                                                            
         J     SETORD60                                                         
                                                                                
SETORD52 CLI   ORDEL,GDAELQ                                                     
         JNE   SETORD54                                                         
         GOTOR SOGAD                                                            
         J     SETORD60                                                         
*                                                                               
SETORD54 CLI   ORDEL,STCELQ                                                     
         JNE   SETORD60                                                         
         GOTOR SOSTC                                                            
         J     SETORD60                                                         
*                                                                               
SETORD60 LLC   R0,ORDLN                                                         
         AR    R2,R0                                                            
         J     SETORD05                                                         
         DROP  R2                                                               
*                                                                               
SETORD80 XR    RF,RF               If Aura don't update number of               
         ICM   RF,3,CUXPNUM          invoices or pending invoices               
         CHI   RF,XPRODIKQ                                                      
         JE    SETORD82                                                         
*&&US                                                                           
         GOTOR SETOAM                                                           
*&&                                                                             
         J     SETORD88                                                         
                                                                                
SETORD82 CLC   QORDNUM,SPACES      Request made with the order number           
         JNH   SETORD86            No                                           
         CLC   OD#REQ,SPACES       Have we found a requisition number           
         JNH   SETORD86            No                                           
         CLI   OD#STT+3,C'Y'       Is the order approved                        
         JE    SETORD86                                                         
         CLI   OD#STT+3,C'A'                                                    
         JE    SETORD86            Yes                                          
         MVC   LP_ERROR,=AL2(AE$ORDNF) No - display error as shouldn't          
         J     QERROR              be                                           
                                                                                
SETORD86 CLI   QORDSOVL,YESQ       Search override limit list                   
         JE    SETORD88                                                         
*                                                                               
         GOTOR CHKVIEW             Check if the user can view this              
         JE    SETORD88            order                                        
         MVC   LP_ERROR,=AL2(AE$NOSTO)                                          
         J     QERROR                                                           
*                                                                               
SETORD88 MVC   QORDNUM,OD#NUM                                                   
         L     R0,AGOBLOCB         CALL GETOPT FOR KSV OPT SETTING              
         LHI   R1,GOBLOCKX-GOBLOCK                                              
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING GOBLOCKD,RF                                                      
         L     RF,AGOBLOCB                                                      
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US uses 1st extension block                  
         MVC   GOABEXT,AGOBBLCK    UK uses 2nd extension block                  
         MVC   GOSELCUL(1),CUXCPY                                               
         MVC   GOSELCUL+1(2),PRODUL                                             
         MVI   GOWHICH,0                                                        
                                                                                
         CLC   OD#CLI,SPACES                                                    
         JNH   SETORD90                                                         
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELCLI(L'OD#CLI),OD#CLI                                        
                                                                                
         CLC   OD#PRO,SPACES                                                    
         JNH   SETORD92                                                         
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELPRO(L'OD#PRO),OD#PRO                                        
                                                                                
         CLC   OD#JOB,SPACES                                                    
         JNH   SETORD92                                                         
         MVC   GOSELJOB,SPACES                                                  
         MVC   GOSELMED,OD#JOB                                                  
         MVC   GOSELJOB,OD#JOB                                                  
         J     SETORD92                                                         
                                                                                
SETORD90 MVC   GOSELOFC,OD#OFF     fill out office from order to get            
SETORD92 GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
                                                                                
SETORD94 TM    SDMODE,SDMODCQ      Copy call?                                   
         JZ    SETORD96                                                         
         XC    OD#REQ,OD#REQ       clear these values                           
         XC    OD#DTE,OD#DTE                                                    
         XC    OD#RAIC,OD#RAIC                                                  
         XC    OD#RAIN,OD#RAIN                                                  
         XC    OD#RAIL,OD#RAIL                                                  
         XC    OD#RAIE,OD#RAIE                                                  
         XC    OD#RAIT,OD#RAIT                                                  
         XC    OD#RBYE,OD#RBYE                                                  
                                                                                
SETORD96 GOTOR GETFOR                                                           
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2,OD#SUP                                                     
         GOTOR SUPGAP              read GAP setting for supplier                
         JH    XERROR                                                           
                                                                                
SETORD97 ZAP   OD#UNCOM,PZERO      Uncommitted applicable/required?             
         CLI   QUNCOMM,YESQ        (must be requested)                          
         JNE   SETORD98                                                         
         CLI   ORDTYPE,PROOQ       (must be production order)                   
         JNE   SETORD98                                                         
         XR    RF,RF                                                            
         ICM   RF,B'0011',CUXPNUM  (must be Aura)                               
         CHI   RF,XPRODIKQ                                                      
         JNE   SETORD98                                                         
                                                                                
**********************************************************************          
*** Note: Changes as per 13May2015 RD005066 applied to GETUCA only ***          
**********************************************************************          
                                                                                
         MVI   QEUBYWC,NOQ         as used by 00ED call                         
         MVI   QEUCONT,NOQ         as used by 00ED call                         
                                                                                
* Uncommitted amount = Estimate total - Debit total - Unmatched total           
* as derived in ACBRA19.ESTPCC - or as derived in ACBRA30.A#ACSD with           
* ULA set, QS_OWCON=Y, all others =N but EstCheck Yes                           
* SingleEstCheck catered for here, too, as in ACBRA13.CHKEST                    
                                                                                
         L     R1,AIO8             init w/c buffer                              
         MVI   0(R1),0                                                          
                                                                                
         GOTOR DOEST               do Presto/Brandocean/Aura estimates          
         JNE   SETORD9E                                                         
         AP    OD#UNCOM,XDOEVE     add figures now                              
         AP    OD#UNCOM,XDOEST                                                  
***      CP    OD#UNCOM,PZERO      skip if 0 from estimates                     
***      JE    SETORD98            (see GETUCA changes for RD006516)            
                                                                                
         GOTOR GETTUP                                                           
                                                                                
         GOTOR DOTRX               do transactions                              
         SP    OD#UNCOM,XDOTRX                                                  
         GOTOR DOEXP               do expense claims                            
         SP    OD#UNCOM,XDOEXP                                                  
         GOTOR DOTIM               do timesheets                                
         SP    OD#UNCOM,XDOTIM                                                  
         GOTOR DOOTX               do order transactions                        
         SP    OD#UNCOM,XDOOTX                                                  
         GOTOR DOORD               do orders                                    
         SP    OD#UNCOM,XDOORD                                                  
                                                                                
SETORD98 MVI   LP_RMODE,LP_RNEXT                                                
         MVC   LP_ADATA,AIO2                                                    
         J     EXITY                                                            
                                                                                
SETORD9E MVC   LP_ERROR,=AL2(AE$ENONS)                                          
         J     QERROR                                                           
                                                                                
* Read sequential order records for text fields                                 
                                                                                
SEQORD   MVC   IOKEY,SVORDKY                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,ORDKEYT,('B#ORDREC',0),          +        
               (0,SAVED),AORDKF,0                                               
         JNE   EXITY                                                            
         MVC   SVORDKY,IOKEY                                                    
         GOTOR AURAMVP,2           Aura MVP version checks                      
         JNE   QERROR                                                           
         J     EXITY                                                            
*                                                                               
EXTORD   GOTOR (#NXTREC,ANXTREC),DMCB,ORXKEYT,('B#ORDREC',0),          +        
               (0,SAVED),0,0                                                    
         J     EXITY                                                            
                                                                                
                                                                                
TSTORD   CLI   QDLSORD,YESQ                                                     
         J     EXIT                                                             
                                                                                
TSTTXT   CLI   QDLSTXT,YESQ                                                     
         J     EXIT                                                             
                                                                                
TSTITM   CLI   QDLSITM,YESQ                                                     
         J     EXIT                                                             
                                                                                
TSTAUD   CLI   QDLSAUD,YESQ                                                     
         J     EXIT                                                             
                                                                                
TSTXDF   CLI   QDLSXDF,YESQ                                                     
         J     EXIT                                                             
                                                                                
TSTPDF   CLI   QISPDF,YESQ         Check if we are pdf                          
         JNE   EXIT                                                             
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         J     EXIT                                                             
                                                                                
         USING PIDELD,R1                                                        
TSTPLN   L     R1,LP_AINP                                                       
         CLI   PIDNTR#,2                                                        
         BNER  RE                                                               
         CLI   PIDLN,PIDLNQ+L'PIDNTRS                                           
         BER   RE                                                               
         CLC   PIDNTRS+1(L'PIDNO),PIDNTRS+1+L'PIDNTRS                           
         J     SETCCC                                                           
                                                                                
*&&US                                                                           
         USING OAMELD,R1                                                        
TSTXTRA  L     R1,LP_AINP                                                       
         TM    OAMSTAT,OAMSXTRA                                                 
         BR    RE                                                               
         DROP  R1                                                               
*&&                                                                             
                                                                                
         USING STCELD,R1                                                        
TSTLDGO  L     R1,LP_AINP          Test if ledger only                          
         CLC   STCOSACT,SPACES           as don't want ledger only              
         J     SETCCC                                                           
         DROP  R1                                                               
                                                                                
         USING STCELD,R1                                                        
TST2DOF  L     R1,LP_AINP                                                       
         LA    R3,STCO2DAC                                                      
         TM    STCOIND2,STCODPTQ   Has dept changed                             
         BZR   RE                                                               
         XR    R2,R2                                                            
         TM    SCPYEL+CPYSTAT1-CPYELD,CPYSOROE                                  
         JZ    *+8                                                              
         LA    R2,1                                                             
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JZ    *+8                                                              
         LA    R2,2                                                             
         AR    R3,R2                                                            
         CLI   0(R3),C' ' Have we got a dept                                    
         J     SETCCC      if not don't put anything out                        
         DROP  R1                                                               
                                                                                
TSTCOP   DS    0H                                                               
         TM    SDMODE,SDMODCQ      COPY mode?                                   
         JNZ   EXITN                                                            
         J     EXITY                                                            
                                                                                
         USING SCMELD,R1                                                        
TSTSCM1  L     R1,LP_AINP                                                       
         CLI   SCMTYPE,SCMTSANP                                                 
         JNE   EXITN                                                            
         XR    RF,RF                                                            
         ICM   RF,B'0011',CUXPNUM  Aura?                                        
         CHI   RF,XPRODIKQ                                                      
         JNE   EXITY                                                            
         OC    SCMSSEQ,SCMSSEQ     If Aura set subsequence to 1 if 0            
         JNZ   EXITY                                                            
         MVI   SCMSSEQ,1                                                        
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
         USING SCMELD,R1                                                        
TSTSCM2  L     R1,LP_AINP                                                       
         CLI   SCMTYPE,SCMTPRAD                                                 
         JE    EXITY                                                            
         CLI   SCMTYPE,SCMTPRBD                                                 
         JE    EXITY                                                            
         J     EXITN                                                            
         DROP  R1                                                               
                                                                                
         USING SCMELD,R1                                                        
TSTSCM3  L     R1,LP_AINP                                                       
         CLI   SCMTYPE,SCMTOPDX                                                 
         J     EXIT                                                             
         DROP  R1                                                               
                                                                                
TSTEML   CLI   ORDEML,YESQ                                                      
         BNER  RE                                                               
         CLC   APPEMAIL,SPACES                                                  
         J     SETCCC                                                           
                                                                                
TSTINVO  TM    SDMODE,SDMODBQ+SDMODIQ  TEST INVOICES                            
         J     SETCCC                                                           
                                                                                
         USING ARTELD,R2                                                        
SETITM   L     R2,LP_AINP                                                       
         LA    R0,SIDATA                                                        
         LHI   R1,SIDATAQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   SICIND,SICIYQ                                                    
         MVC   SIANUM,ARTNUM                                                    
                                                                                
         XR    RF,RF               If Aura check we can display ord             
         ICM   RF,3,CUXPNUM                                                     
         CHI   RF,XPRODIKQ         BrandO has full access                       
         JNE   SETITM40                                                         
                                                                                
         CLC   ARTNUM,SPACES       skip 'no code' items                         
         JE    SETITM40                                                         
         CLI   ARTLN,ARTLN2Q                                                    
         JL    SETITM02                                                         
         TM    ARTSTAT,ARTSTPQ+ARTSTXQ+ARTSTNQ                                  
         JNZ   SETITM40            skip if no fixed price or NIC/artist         
                                                                                
SETITM02 DS    0H                                                               
         MVC   G#SGOOFF,OD#OFF                                                  
         MVC   G#SGOCLI,OD#CLI                                                  
         MVC   G#SGOPRO,SPACES                                                  
         MVC   G#SGOJOB,SPACES                                                  
         MVC   G#SGWRKC,ARTWC                                                   
         MVC   G#SGOLLA,ARTSEQ                                                  
         MVC   G#SGSUPP,OD#SUP                                                  
         MVC   G#SGRATE,SCOPRAT                                                 
         MVC   G#SGCURR,SCOPCUR                                                 
         TM    SDMODE,SDMODFQ                                                   
         JNZ   SETITM04                                                         
         MVC   G#SGRATE,OD#RAT                                                  
         MVC   G#SGCURR,OD#CUR                                                  
                                                                                
SETITM04 LA    R1,C'O'                                                          
T        USING ORDPRFD,XL#ORPF                                                  
         CLI   T.PROPO201,YESQ                                                  
         JNE   SETITM06                                                         
         LA    R1,C'A'                                                          
         DROP  T                                                                
                                                                                
SETITM06 GOTOR (#GETITM,AGETITM)   check item for current values                
         JE    SETITM08                                                         
                                                                                
         MVI   SICIND,SICINQ       drop if none found at all                    
         J     SETITM40                                                         
                                                                                
         USING X_LISTTABD,RF                                                    
SETITM08 L     RF,AOFFAREA         (see EDMR305 of ACBRA17)                     
                                                                                
SETITM10 OC    X_LISTSEQ,X_LISTSEQ end of list?                                 
         JNZ   SETITM12                                                         
                                                                                
         MVI   SICIND,SICIIQ       drop it if not same item                     
         J     SETITM40                                                         
                                                                                
SETITM12 CLC   X_LISTSEQ,ARTSEQ                                                 
         JE    SETITM14                                                         
         AHI   RF,X_LISTLNQ        next entry                                   
         J     SETITM10                                                         
                                                                                
SETITM14 CP    X_LISTPRI,ARTPRICE  if same (currency) price                     
         JE    SETITM40            it is OK, else                               
                                                                                
         MVI   SICIND,SICIPQ       'same item but different price'              
         DROP  RF                                                               
                                                                                
SETITM40 ZAP   SIPRIC,ARTPRICE                                                  
                                                                                
SETITM42 ZAP   SIMULT,ARTMULT                                                   
         MVC   SISEQN,ARTSEQ                                                    
         CLI   ARTLN,ARTLN3Q                                                    
         JNH   SETITM48                                                         
         LLC   RE,ARTLN                                                         
         SHI   RE,ARTLN3Q+1                                                     
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   SIDESC(0),ARTDESC                                                
                                                                                
SETITM48 MVI   SISTAT+0,NOQ                                                     
         MVI   SISTAT+1,NOQ                                                     
         MVI   SISTAT+2,NOQ                                                     
         MVI   SISTAT+3,NOQ                                                     
         CLI   ARTLN,ARTLN2Q                                                    
         JL    SETITM58                                                         
         TM    ARTSTAT,ARTSTPQ                                                  
         JZ    SETITM50                                                         
         MVI   SISTAT+0,YESQ                                                    
         J     SETITM52                                                         
                                                                                
SETITM50 TM    ARTSTAT,ARTSTFQ                                                  
         JZ    SETITM52                                                         
         MVI   SISTAT+0,C'F'                                                    
                                                                                
SETITM52 TM    ARTSTAT,ARTSTXQ                                                  
         JZ    SETITM54                                                         
         MVI   SISTAT+1,YESQ                                                    
                                                                                
SETITM54 TM    ARTSTAT,ARTSTDQ                                                  
         JZ    SETITM56                                                         
         MVI   SISTAT+2,YESQ                                                    
                                                                                
SETITM56 TM    ARTSTAT,ARTSTNQ                                                  
         JZ    SETITM58                                                         
         MVI   SISTAT+3,YESQ                                                    
                                                                                
SETITM58 OC    ARTSEQ,ARTSEQ                                                    
         JZ    SETITM90                                                         
                                                                                
         USING PASRECD,R3                                                       
         LA    R3,IOKEY                                                         
         XC    PASKEY,PASKEY                                                    
         MVI   PASKTYP,PASKTYPQ                                                 
         MVI   PASKSUB,PASKSQ                                                   
         MVC   PASKCPY,CUXCPY                                                   
         MVC   PASKSEQ,ARTSEQ                                                   
         DROP  R2                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SETITM90                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SETITM90                                                         
                                                                                
         L     R3,AIO1                                                          
         LA    R2,ARTRFST-ARTRECD(R3)                                           
         USING NAMELD,R2                                                        
         XR    R0,R0                                                            
                                                                                
SETITM60 CLI   NAMEL,0                                                          
         JE    SETITM90                                                         
         CLI   NAMEL,NAMELQ                                                     
         JE    SETITM64                                                         
         CLI   NAMEL,SCMELQ                                                     
         JE    SETITM66                                                         
         CLI   NAMEL,XNMELQ                                                     
         JE    SETITM70                                                         
         CLI   NAMEL,AFDELQ                                                     
         JNE   SETITM62                                                         
                                                                                
         USING AFDELD,R2                                                        
         CLI   AFDLN,AFDLNXQ                                                    
         JL    SETITM62                                                         
         MVC   SIUNIT,AFDUNIT                                                   
         MVC   SIUNLA,AFDUNLA                                                   
                                                                                
         USING NAMELD,R2                                                        
SETITM62 IC    R0,NAMLN                                                         
         AR    R2,R0                                                            
         J     SETITM60                                                         
                                                                                
SETITM64 XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         LTR   RE,RE                                                            
         JM    SETITM62                                                         
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   SINAME(0),NAMEREC                                                
         J     SETITM62                                                         
                                                                                
         USING SCMELD,R2                                                        
SETITM66 LA    RF,SIETX1                                                        
         TM    SCMSEQ,SCMSEQLQ                                                  
         JZ    SETITM68                                                         
         LA    RF,SIETX2                                                        
                                                                                
SETITM68 LLC   RE,SCMLN                                                         
         SHI   RE,SCMLN1Q+1                                                     
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),SCMNARR                                                  
         J     SETITM62                                                         
                                                                                
         USING XNMELD,R2                                                        
SETITM70 LLC   RE,XNMSUBL                                                       
         SHI   RE,2                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   SINAME2(0),XNMSUBN                                               
         J     SETITM62                                                         
                                                                                
SETITM90 DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
         USING SCMELD,R2                                                        
SETWCD   L     R2,LP_AINP                                                       
         MVC   OD#WCDS,SPACES      Overridden work-code description             
         MVC   OD#WCFDS,SPACES     Foreign work-code description                
         LA    RE,SCMWCOD                                                       
         CLI   SCMEL,SCMELQ                                                     
         JE    SETWCD1                                                          
         CLI   SCMEL,OAMELQ                                                     
         JNE   SETWCDX                                                          
         USING OAMELD,R2                                                        
         LA    RE,OAMWORK                                                       
         ZAP   DUB,PZERO                                                        
         TM    OAMSTAT,OAMSFCUR                                                 
         JZ    SETWCD0                                                          
         ZAP   DUB,OAMFCAMT                                                     
                                                                                
SETWCD0  CLC   OAMWORK,SPACES                                                   
         JNH   SETWCDX                                                          
*        MVC   QWC,OAMWORK                                                      
         MVC   SHALF2,0(RE)                                                     
         GOTOR LP_AAWMP,DMCB,(L'WCOKWRK,OAMWORK),QWCIND,QWCMAXQ,       +        
               LP_D                                                             
         J     SETWCD2                                                          
                                                                                
         USING SCMELD,R2                                                        
SETWCD1  MVC   SHALF2,0(RE)                                                     
         CLC   SCMWODS,SPACES      Work-code description provided?              
         JNH   SETWCD2                                                          
         MVC   OD#WCDS,SCMWODS                                                  
                                                                                
SETWCD2  CLC   SHALF1,SHALF2       same as previous                             
         JNE   SETWCD3                                                          
*                                                                               
         CLC   SCMWODS,SPACES      Work-code description provided?              
         JH    SETWCD2A                                                         
         MVC   OD#WCDS(15),SNAMES  Existing work-code description               
         J     SETWCD2B                                                         
*                                                                               
SETWCD2A MVC   OD#WCDS,SCMWODS     Overridden work-code description             
SETWCD2B MVC   OD#WCFDS,SNAMES+15  Foreign work-code description                
         J     SETWCDX                                                          
                                                                                
SETWCD3  MVC   TEMP2(L'HALF2),SHALF2                                            
         GOTOR (#GETWCD,AGETWCD)                                                
         MVC   OD#WCDS(15),TEMP2                                                
         CLC   SCMWODS,SPACES      Work-code description provided?              
         JNH   SETWCD3A                                                         
         MVC   OD#WCDS,SCMWODS                                                  
                                                                                
SETWCD3A MVC   SXNAME,SPACES                                                    
         L     R1,AIO3                                                          
         AHI   R1,WCORFST-WCORECD                                               
         USING XNMELD,R1                                                        
                                                                                
SETWCD04 CLI   XNMEL,XNMELQ                                                     
         JE    SETWCD08                                                         
         CLI   XNMEL,WCOELQ                                                     
         JE    SETWCD10                                                         
         CLI   XNMEL,NAMELQ                                                     
         JE    SETWCD16                                                         
         CLI   XNMEL,0                                                          
         JE    SETWCD30                                                         
SETWCD06 LLC   R0,XNMLN                                                         
         AR    R1,R0                                                            
         J     SETWCD04                                                         
                                                                                
SETWCD08 LLC   RE,XNMSUBL                                                       
         SHI   RE,2                                                             
         LTR   RE,RE                                                            
         JM    SETWCD06                                                         
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   TEMP+15(0),XNMSUBN                                               
         J     SETWCD06                                                         
         DROP  R1                                                               
                                                                                
         USING WCOELD,R1                                                        
SETWCD10 MVC   BYTE1,SPACES                                                     
*&&UK                                                                           
         TM    WCOSTAT,WCOSEARQ                                                 
         JZ    SETWCD12                                                         
         MVI   BYTE1,C'A'                                                       
         J     SETWCD06                                                         
SETWCD12 TM    WCOSTAT,WCOSEABJ                                                 
         JZ    SETWCD14                                                         
         MVI   BYTE1,C'B'                                                       
         J     SETWCD06                                                         
SETWCD14 TM    WCOSTAT,WCOSEAUJ                                                 
         JZ    SETWCD06                                                         
         MVI   BYTE1,C'U'                                                       
*&&                                                                             
         J     SETWCD06                                                         
                                                                                
         USING NAMELD,R1                                                        
SETWCD16 XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   SXNAME(0),NAMEREC                                                
         J     SETWCD06                                                         
                                                                                
SETWCD30 MVC   SHALF1,SHALF2                                                    
         MVC   SNAMES,TEMP                                                      
                                                                                
SETWCDX  DS    0H                                                               
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
         USING SCMELD,R2                                                        
SETIND   L     R2,LP_AINP                                                       
         MVI   BYTE1,C' '                                                       
         CLI   SCMTYPE,SCMTSANP                                                 
         JE    EXITY                                                            
         MVI   BYTE1,C'F'                                                       
         CLI   SCMTYPE,SCMTPRAD                                                 
         JE    EXITY                                                            
         MVI   BYTE1,C'H'                                                       
         CLI   SCMTYPE,SCMTPRBD                                                 
         JE    EXITY                                                            
         MVI   BYTE1,C'?'                                                       
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
         USING ALIRECD,R2                                                       
SETALIM  LA    R2,IOKEY            Look up order approvers Aplimits             
         CLI   QDLSORD,YESQ                                                     
         JNE   EXITY                                                            
         USING APLTABD,R3                                                       
         LA    R3,APLTAB                                                        
SALIM04  LA    R2,IOKEY                                                         
         XC    ALIKEY,ALIKEY                                                    
         MVI   ALIKTYP,ALIKTYPQ                                                 
         MVI   ALIKSUB,ALIKSUBQ                                                 
         MVC   ALIKCPY,CUXCPY                                                   
         MVI   ALIKCAT,ALIKORD                                                  
         TM    APLTSTAT,APLTORT1                                                
         JZ    SALIM06                                                          
         MVC   ALIKSCAT,OD#TYP                                                  
         J     SALIM10                                                          
*                                                                               
SALIM06  TM    APLTSTAT,APLTORT2                                                
         JZ    SALIM08                                                          
         MVI   ALIKSCAT,ALIKNCLI   TRY NON-CLIENT OR CLIENT TYPE                
         CLC   OD#CLI,SPACES                                                    
         JNH   SALIM10                                                          
         MVI   ALIKSCAT,ALIKCLI                                                 
         J     SALIM10                                                          
*                                                                               
SALIM08  MVI   ALIKSCAT,ALIKDFT                                                 
*                                                                               
SALIM10  TM    APLTSTAT,APLTCLI                                                 
         JZ    SALIM12                                                          
         CLC   OD#CLI,SPACES        CLIENT SPECIFIED                            
         JNH   SALIM18                                                          
         MVC   ALIKCLIC(L'OD#CLI),OD#CLI                                        
         OC    ALIKCLIC,SPACES                                                  
*                                                                               
SALIM12  TM    APLTSTAT,APLTETYP                                                
         JZ    SALIM14                                                          
         CLC   OD#ETY,SPACES                                                    
         JNH   SALIM18                                                          
         MVC   ALIKETYP(L'OD#ETY),OD#ETY                                        
         OC    ALIKETYP,SPACES                                                  
*                                                                               
SALIM14  TM    APLTSTAT,APLTOFFC                                                
         JZ    SALIM16                                                          
         CLC   OD#OFF,SPACES       OFFICE SPECIFIED                             
         JE    SALIM18                                                          
         MVC   ALIKOFFC,OD#OFF                                                  
*                                                                               
SALIM16  MVC   CSVKEY1(L'ALIKEY),ALIKEY                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    SALIM20                                                          
SALIM18  LA    R3,APLTABL(R3)                                                   
         CLI   APLTSTAT,X'FF'                                                   
         JNE   SALIM04                                                          
         J     EXITY                                                            
*                                                                               
SALIM20  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   *+2                                                              
*                                                                               
         L     R2,AIO3                                                          
         LA    R3,ALIRFST                                                       
         LA    R4,OD#APBLK         point to buffer and clear it                 
         LR    R0,R4                                                            
         LHI   R1,L'OD#APBLK                                                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         USING APLELD,R3                                                        
SALIM22  CLI   APLEL,0             END OF RECORD                                
         JNE   SALIM23                                                          
         L     R3,OD#LSTEL                                                      
         SHI   R4,APTABNTQ                                                      
         LTR   R3,R3                                                            
         JNZ   SALIM27                                                          
         DC    H'0'                                                             
SALIM23  CLI   APLEL,APLELQ        FIND APPROVAL LIMIT ELEMENT(S)               
         JE    SALIM26                                                          
SALIM24  XR    R0,R0                                                            
         IC    R0,APLLN                                                         
         AR    R3,R0                                                            
         J     SALIM22                                                          
                                                                                
T        USING APLSTAT,R4                                                       
SALIM26  ST    R3,OD#LSTEL                                                      
         MVI   T.APLSTAT,NOQ                                                    
         ZAP   T.APLVAL,APLVAL     SAVE APPROVAL VALUE                          
         ZAP   DUB,APLVAL                                                       
         MP    DUB,=P'100'                                                      
         MVC   T.APLNUM,APLNUM     N'APPROVERS, THIS LEVEL                      
         MVC   T.APLPREV,APLPREV   N'PREVIOUS LEVELS                            
         CP    DUB,OD#TOT          CONTINUE UNTIL ONE APPROVAL LIMIT..          
         JL    SALIM28             ... => ORDER VALUE HAS BEEN FOUND            
SALIM27  MVI   T.APLSTAT,YESQ                                                   
         CLI   APLFAPP,C'Y'        IF FINAL APPROVER REQUIRED                   
         JNE   SALIM30                                                          
         MVI   APTABNTQ(R4),YESQ                                                
         ZAP   APTABNTQ+1(L'APLVAL,R4),PZERO  ADD A ZERO VALUE ENTRY            
         MVI   APTABNTQ+L'APLVAL+1(R4),1    SET N'FINAL APPROVERS = 1           
         J     SALIM30                                                          
         DROP  T                                                                
                                                                                
SALIM28  AHI   R4,APTABNTQ                                                      
         J     SALIM24                                                          
                                                                                
SALIM30  SR    RF,RF                                                            
         ICM   RF,1,APLPREV        N'PREVIOUS APPROVER LEVELS                   
         JZ    *+12                NONE                                         
         SHI   R4,APTABNTQ         R4=A(PREVIOUS SLOT)                          
         JCT   RF,*-4              N TIMES, TO GET TO 1ST LVL REQUIRED          
         ST    R4,OD#ADDR                                                       
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
T        USING APLSTAT,R4                                                       
SETAPP   CLI   QDLSORD,YESQ                                                     
         JNE   EXITY                                                            
         ICM   R4,15,OD#ADDR                                                    
         JZ    EXITY                                                            
         ZAP   OD#APPL1,PZERO                                                   
         OC    T.APLVAL,T.APLVAL   protect against empty entry (set up          
         JZ    EXITY               may have changed)                            
         ZAP   OD#APPL1,T.APLVAL                                                
         AHI   R4,APTABNTQ                                                      
         ST    R4,OD#ADDR                                                       
         J     EXITY                                                            
         DROP  T                                                                
*                                  INVOICES: RETURN APPROVER'S APLIMIT          
                                                                                
                                                                                
         USING XDFELD,R2                                                        
SETXDF   L     R2,LP_AINP                                                       
         XC    SXDATA(SXDATAQ),SXDATA                                           
         MVC   SXDCODE,XDFOCOD                                                  
         MVC   SXDTYPE,XDFOTYP                                                  
         CLI   XDFOTYP,XDFEDNQ     N=numeric                                    
         JE    SETXDF02                                                         
         CLI   XDFOTYP,XDFEDXQ     X=dropdown                                   
         JE    SETXDF02                                                         
         CLI   XDFOTYP,XDFEDCQ     C=char                                       
         JNE   SETXDF04                                                         
                                                                                
SETXDF02 XR    RE,RE                                                            
         IC    RE,XDFLN                                                         
         SHI   RE,XDFOLNQ+1                                                     
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   SXDDATA(0),XDFODTA                                               
         J     SETXDF12                                                         
                                                                                
SETXDF04 CLI   XDFOTYP,XDFEDDQ     D=DATE                                       
         JNE   SETXDF06                                                         
         GOTOR VDATCON,DMCB,(1,XDFODTA),(20,SXDDATA)                            
         J     SETXDF12                                                         
                                                                                
SETXDF06 CLI   XDFOTYP,XDFEDYQ     Y=YES/NO                                     
         JNE   SETXDF08                                                         
         MVC   SXDDATA(1),XDFODTA                                               
         J     SETXDF12                                                         
                                                                                
SETXDF08 CLI   XDFOTYP,XDFEDAQ     A=AMOUNT                                     
         JE    SETXDF10                                                         
         DC    H'0'                (unknown)                                    
                                                                                
SETXDF10 CURED (P6,XDFODTA),(20,SXDDATA),2,ZERO=YES,ALIGN=LEFT                  
         ZAP   SXDAMNT,XDFODTA(6)                                               
                                                                                
SETXDF12 XR    RF,RF                                                            
         ICM   RF,B'0011',CUXPNUM  Aura?                                        
         CHI   RF,XPRODIKQ                                                      
         JNE   SETXDFX                                                          
                                                                                
         USING XDFPASD,R3                                                       
         LA    R3,IOKEY            READ XDFRECD USING PASSIVES                  
         XC    IOKEY,IOKEY                                                      
         MVI   XDFPTYP,XDFPTYPQ                                                 
         MVI   XDFPSUB,XDFPSUBQ                                                 
         MVC   XDFPCPY,CUXCPY                                                   
         MVC   XDFPPTR(L'XDFPPTR+L'XDFPSEQ),XDFOCOD                             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SETXDFX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
         USING XDFRECD,R3                                                       
         L     R3,AIO1                                                          
XDF      USING XDFEL,R1                                                         
         LA    R1,XDFRFST                                                       
                                                                                
SETXDF14 CLI   XDF.XDFEL,XDFELQ                                                 
         JNE   SETXDF16                                                         
         CLC   XDF.XDFSEQ,XDFOCOD+L'XDFPPTR                                     
         JE    SETXDF20                                                         
         J     SETXDF18                                                         
                                                                                
SETXDF16 CLI   XDF.XDFEL,0                                                      
         JE    SETXDFX                                                          
                                                                                
SETXDF18 LLC   R0,XDF.XDFLN                                                     
         AR    R1,R0                                                            
         J     SETXDF14                                                         
                                                                                
SETXDF20 MVC   SXDXCDE,XDF.XDFCODE                                              
         LLC   RF,XDF.XDFLN                                                     
         SHI   RF,XDFLN1Q                                                       
         CHI   RF,0                                                             
         JNH   SETXDFX                                                          
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SXDXNAM(0),XDF.XDFNAME                                           
         EX    RF,0(RE)                                                         
                                                                                
SETXDFX  DS    0H                                                               
         J     EXITY                                                            
         DROP  R2,R3,XDF                                                        
                                                                                
***********************************************************************         
* Order 'uncommited amount' download                                  *         
***********************************************************************         
                                                                                
REQOUCA  LKREQ H,A#OUCA,OUTOUCA,NEXTREQ=REQXDFH                                 
EUCJob   LKREQ F,1,(D,B#SAVED,QEUCJOB),CHAR,OLEN=L'QEUCJOB,            +        
               MAXLEN=L'QEUCJOB,TEXT=AC#JOB,COL=*                               
EUCOff   LKREQ F,2,(D,B#SAVED,QEUCOFF),CHAR,OLEN=L'QEUCOFF,            +        
               MAXLEN=L'QEUCOFF,TEXT=AC#OFFC,COL=*                              
EUCGEN   LKREQ F,3,(D,B#SAVED,QEUCGEN),CHAR,OLEN=L'QEUCGEN,            +        
               MAXLEN=L'QEUCGEN,TEXT=AC#RSESB,COL=*                             
EUbyWC   LKREQ F,4,(D,B#SAVED,QEUBYWC),CHAR,OLEN=L'QEUBYWC,            +        
               MAXLEN=L'QEUBYWC,TEXT=(*,UNWCLIT),COL=*                          
EUCont   LKREQ F,5,(D,B#SAVED,QEUCONT),CHAR,OLEN=L'QEUCONT,            +        
               MAXLEN=L'QEUCONT,TEXT=(*,CONTLIT),COL=*                          
         LKREQ E                                                                
                                                                                
OUTOUCA  LKOUT H                                                                
                                                                                
MAIOUCA  LKOUT R,A#OUCA            Main order data                              
Array    LKOUT C,A#OUCA,(A,ARYMUCA)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYMUCA  LKOUT A,(R,GETUCA),MULTIROW=Y,ROWNAME=SAVED                            
UncoTot  LKOUT C,1,(D,B#SAVED,OD#UNCOM),SPAK,ND=Y                               
Uncomtd  LKOUT C,2,(D,B#SAVED,OD#UNAMT),SPAK,ND=Y                               
WCode    LKOUT C,3,(D,B#SAVED,OD#UNWC),CHAR,ND=Y                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get order records and other routines                                *         
***********************************************************************         
                                                                                
         USING UCBYWCD,R4                                                       
GETUCA   DS    0H                                                               
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JE    GETUCA08                                                         
         CLI   QEUBYWC,YESQ        multiple = array                             
         JNE   GETUCAXR                                                         
                                                                                
GETUCA02 L     R4,ACURNTRY         get next entry and                           
         XC    OD#UNCOM,OD#UNCOM                                                
                                                                                
GETUCA04 CLI   UCBYWCWC,0          - eot?                                       
         JE    GETUCAXR            - quit or                                    
         CP    UCBYWCAM,PZERO      - pass values out if none zero               
         JNE   GETUCA06                                                         
         AHI   R4,UCBYWCLQ                                                      
         J     GETUCA04                                                         
                                                                                
GETUCA06 ZAP   OD#UNAMT,UCBYWCAM                                                
         MVC   OD#UNWC,UCBYWCWC                                                 
         AHI   R4,UCBYWCLQ                                                      
         ST    R4,ACURNTRY                                                      
         L     R1,AIO6             ensure no overrun                            
         CR    R4,R1               but if so either use different area          
         JNL   *+2                 or implement TSAR - or change 'add           
         J     GETUCAX             to table' by skipping zeroes ...             
                                                                                
GETUCA08 XR    RF,RF                                                            
         ICM   RF,B'0011',CUXPNUM  (must be Aura)                               
         CHI   RF,XPRODIKQ                                                      
         JE    GETUCA10                                                         
         LHI   RF,AE$ENQNS                                                      
         J     GETUCAER                                                         
                                                                                
GETUCA10 L     R4,AIO4             w/c table (covers IO4 and IO5)               
         XC    UCBYWCWC,UCBYWCWC                                                
         ST    R4,ACURNTRY                                                      
                                                                                
         MVC   OD#CLI,SPACES                                                    
         MVC   OD#PRO,SPACES                                                    
         MVC   OD#JOB,SPACES                                                    
         MVC   SJACCNT,QEUCJOB                                                  
         OC    SJACCNT,SPACES                                                   
         MVC   OD#OFF,QEUCOFF                                                   
         OC    OD#OFF,SPACES                                                    
         MVC   OD#ESTN,QEUCGEN                                                  
                                                                                
         LLC   R1,PCLILEN          split into client, product and job           
         LR    R0,R1                                                            
         SHI   R1,1                                                             
         BASR  R2,0                                                             
         EX    R1,4(R2)                                                         
         MVC   OD#CLI(0),SJACCNT                                                
         LA    R1,SJACCNT+1(R1)                                                 
         LLC   RE,PPROLEN                                                       
         LR    RF,RE                                                            
         SR    RE,R0                                                            
         SHI   RE,1                                                             
         BASR  R2,0                                                             
         EX    RE,4(R2)                                                         
         MVC   OD#PRO(0),0(R1)                                                  
         LA    RE,1(R1,RE)                                                      
         LLC   R1,PJOBLEN                                                       
         SR    R1,RF                                                            
         SHI   R1,1                                                             
         BASR  R2,0                                                             
         EX    R1,4(R2)                                                         
         MVC   OD#JOB(0),0(RE)                                                  
                                                                                
         CLC   OD#CLI,SPACES                                                    
         JH    GETUCA12                                                         
         LHI   RF,AE$MSCLI                                                      
         J     GETUCAER                                                         
                                                                                
GETUCA12 CLC   OD#PRO,SPACES                                                    
         JH    GETUCA14                                                         
         LHI   RF,AE$MSPRD                                                      
         J     GETUCAER                                                         
                                                                                
GETUCA14 CLC   OD#JOB,SPACES                                                    
         JH    GETUCA16                                                         
         LHI   RF,AE$MSJOB                                                      
         J     GETUCAER                                                         
                                                                                
         USING ACTRECD,R2                                                       
GETUCA16 LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         MVC   ACTKACT,SJACCNT                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO6'                               
         JE    GETUCA18                                                         
         LHI   RF,AE$INJOB                                                      
         J     GETUCAER                                                         
                                                                                
GETUCA18 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO6'                              
         JNE   *+2                                                              
         DROP  R2                                                               
                                                                                
         L     R0,AGOBLOCB         Getopt for options (validates Cli/           
         LHI   R1,GOBLOCKX-GOBLOCK Pro/Job so may die)                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING GOBLOCKD,RF                                                      
         L     RF,AGOBLOCB                                                      
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US uses 1st extension block                  
         MVC   GOABEXT,AGOBBLCK    UK uses 2nd extension block                  
         MVC   GOSELCUL(1),CUXCPY                                               
         MVC   GOSELCUL+1(2),PRODUL                                             
         MVI   GOWHICH,0                                                        
         OI    GOSPEC3,GOSPNERQ    use none emulated IOs                        
                                                                                
         CLC   OD#CLI,SPACES                                                    
         JNH   GETUCA20                                                         
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELCLI(L'OD#CLI),OD#CLI                                        
                                                                                
         CLC   OD#PRO,SPACES                                                    
         JNH   GETUCA22                                                         
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELPRO(L'OD#PRO),OD#PRO                                        
                                                                                
         CLC   OD#JOB,SPACES                                                    
         JNH   GETUCA22                                                         
         MVC   GOSELJOB,SPACES                                                  
         MVC   GOSELMED,OD#JOB                                                  
         MVC   GOSELJOB,OD#JOB                                                  
         J     GETUCA22                                                         
                                                                                
GETUCA20 MVC   GOSELOFC,OD#OFF     fill out office from order to get            
                                                                                
GETUCA22 GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
                                                                                
         XC    OD#UNAMT,OD#UNAMT   Uncommitted applicable/required?             
         XC    OD#UNWC,OD#UNWC                                                  
         ZAP   OD#UNCOM,PZERO                                                   
                                                                                
         CLI   QEUBYWC,YESQ        skip if w/c break down                       
         JE    GETUCA30                                                         
                                                                                
* RD005066 as of 13March2015/NSHE:                                              
* --------------------------------                                              
*                                                                               
* Nothing should change when work code                                          
* breakdown is requested, that is good                                          
* how it is.                                                                    
*                                                                               
* When it's not work code breakdown and                                         
* GOUESOA is P then set GOECE as GOECETA                                        
* and QEUCONT as Y and read for HR                                              
* estimate, if one doesn't exist use CE.                                        
*                                                                               
* You can make the code global as all the                                       
* option maintain values exist.                                                 
                                                                                
         MVI   GUCAIND,0                                                        
                                                                                
         USING GOXBLKD,R1                                                       
         L     R1,AGOXBLCK                                                      
         LA    RE,GOUESOA                                                       
         LHI   RF,L'GOUESOA                                                     
                                                                                
GETUCA24 CLI   0(RE),C'P'                                                       
         JE    GETUCA26                                                         
         AHI   RE,1                                                             
         JCT   RF,GETUCA24                                                      
         J     GETUCA30                                                         
                                                                                
GETUCA26 MVI   GOECE,GOECETA                                                    
         MVI   QEUCONT,YESQ                                                     
         OI    GUCAIND,GUCAHRQ                                                  
         DROP  R1                                                               
                                                                                
* Uncommitted amount = Estimate total - Debit total - Unmatched total           
* as derived in ACBRA19.ESTPCC - or as derived in ACBRA30.A#ACSD with           
* ULA set, QS_OWCON=Y, all others =N but EstCheck Yes                           
* SingleEstCheck catered for here, too, as in ACBRA13.CHKEST                    
                                                                                
GETUCA30 L     R1,AIO8             init w/c buffer                              
         MVI   0(R1),0                                                          
                                                                                
         GOTOR DOEST               do Presto/Brandocean/Aura estimates          
         JE    GETUCA32                                                         
         LHI   RF,AE$ENONS                                                      
         J     GETUCAER                                                         
                                                                                
GETUCA32 AP    OD#UNCOM,XDOEVE     add figures now                              
         AP    OD#UNCOM,XDOEST                                                  
***      CP    OD#UNCOM,PZERO      skip if 0 from estimates                     
***      JNE   GETUCA34            (skipped - RD006516)                         
***      CLI   QEUBYWC,YESQ        multiple = array                             
***      JE    GETUCAX                                                          
***      J     GETUCAXR                                                         
                                                                                
GETUCA34 GOTOR GETTUP                                                           
                                                                                
         GOTOR DOTRX               do transactions                              
         SP    OD#UNCOM,XDOTRX                                                  
         GOTOR DOEXP               do expense claims                            
         SP    OD#UNCOM,XDOEXP                                                  
         GOTOR DOTIM               do timesheets                                
         SP    OD#UNCOM,XDOTIM                                                  
         GOTOR DOOTX               do order transactions                        
         SP    OD#UNCOM,XDOOTX                                                  
         GOTOR DOORD               do orders                                    
         SP    OD#UNCOM,XDOORD                                                  
                                                                                
GETUCAX  MVI   LP_RMODE,LP_RNEXT                                                
         MVC   LP_ADATA,AIO6                                                    
         J     EXITY                                                            
                                                                                
GETUCAXR MVI   LP_RMODE,LP_RLAST                                                
         MVC   LP_ADATA,AIO6                                                    
         J     EXITY                                                            
                                                                                
GETUCAER STCM  RF,B'0011',LP_ERROR                                              
         J     QERROR                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
* Xdata Headers Request                                               *         
***********************************************************************         
                                                                                
REQXDFH  LKREQ H,A#XDFA,OUTXDF,NEXTREQ=REQAUD                                   
Office   LKREQ F,1,(D,B#SAVED,QOFFICE),CHAR,OLEN=L'QOFFICE,            +        
               MAXLEN=L'QOFFICE,TEXT=AC#OFFC,COL=*                              
OrdTyp   LKREQ F,2,(D,B#SAVED,QORDTYP),CHAR,OLEN=L'QORDTYP,            +        
               TEXT=AC#ORDTY,COL=*                                              
Client   LKREQ F,3,(D,B#SAVED,QCLI),CHAR,OLEN=L'QCLI,                  +        
               TEXT=AC#CLIC,COL=*                                               
ExpTyp   LKREQ F,4,(D,B#SAVED,QETY),CHAR,OLEN=L'QETY,                  +        
               TEXT=AC#ETYPE,COL=*                                              
WC       LKREQ F,5,(I,B#SAVED,QWCIND),CHAR,OLEN=L'WCOKWRK,             +        
               LIST=F,TEXT=AC#WC,COL=*                                          
Media    LKREQ F,6,(D,B#SAVED,QMED),CHAR,OLEN=L'QMED,                  +        
               TEXT=AC#MED,COL=*                                                
Prod     LKREQ F,7,(D,B#SAVED,QPRO),CHAR,OLEN=L'QPRO,                  +        
               TEXT=AC#PRO,COL=*                                                
Schm     LKREQ F,8,(D,B#SAVED,QSCH),CHAR,OLEN=L'QSCH,                  +        
               TEXT=AC#SCM,COL=*                                                
         LKREQ E                                                                
                                                                                
OUTXDF   LKOUT H                                                                
XDFHDR2  LKOUT R,A#XDFA            Xtra data - headers                          
Array    LKOUT C,A#XDFA,(A,ARYXDFH)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYXDFH  LKOUT A,(R,NXTXDF),MULTIROW=Y,ROWNAME=XDFRECD                          
                                                                                
PROUT    LKOUT P,XDFRECD,SETRCODE                                               
Array    LKOUT C,A#XDFA,(A,ARYXDF1)                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYXDF1  LKOUT A,(D,B#XDFREC,XDFRFST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(XDFEL,XDFELQ),ROWWIDTH=(V,XDFLN)                          
                                                                                
Prout    LKOUT P,XDFELD,SETECODE                                                
Fieldcd  LKOUT C,1,(D,B#SAVED,OXDFPTRS),(U,#EDTHEX,$EDTHEX)                     
Field    LKOUT C,2,XDFCODE,CHAR                                                 
DataTyp  LKOUT C,3,XDFEDIT,CHAR                                                 
FieldLn  LKOUT C,4,XDFMXLN,LBIN,ND=Y,FILTROUT=FLTAURA                           
FieldLn  LKOUT C,4,XDFMXLN,LBIN,FILTROUT=FLTBRAND                               
FieldRq  LKOUT C,5,XDFELD,(R,EDTREQD)                                           
FieldAc  LKOUT C,6,XDFELD,(R,EDTACTV)                                           
FieldNm  LKOUT C,7,XDFNAME,CHAR,LEN=V                                           
Array    LKOUT C,8,(A,ARYXDLH)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get Extra Data Records                                              *         
***********************************************************************         
                                                                                
NXTXDF   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTXDF08            No                                           
         CLC   QOFFICE,SPACES      Any office passed                            
         JH    NXTXDF08            Yes assume BO/Aura has it correctly          
*&&UK                                                                           
         CLC   QPRO,SPACES         Any product code passed?                     
         JNH   NXTXDF08                                                         
         CLC   QCLI,SPACES                                                      
         JNH   NXTXDF08                                                         
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         OC    QPRO,SPACES                                                      
         OC    QCLI,SPACES                                                      
         MVC   ACTKEY,SPACES       read for product lvl office override         
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         MVC   ACTKACT(L'QCLI),QCLI                                             
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         LA    RE,ACTKACT-1(RE)                                                 
         MVC   0(L'QPRO,RE),QPRO                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   NXTXDF08                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   NXTXDF08                                                         
         L     R2,AIO1                                                          
         LA    R2,ACTRFST                                                       
         XR    R0,R0                                                            
         USING PPRELD,R2                                                        
NXTXDF02 CLI   PPREL,0                                                          
         JE    NXTXDF08                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    NXTXDF04                                                         
         IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     NXTXDF02                                                         
*                                                                               
NXTXDF04 CLC   PPRGAOFF,SPACES                                                  
         JNH   NXTXDF08                                                         
         MVC   QOFFICE,PPRGAOFF   set if existing                               
         DROP  R2                                                               
*&&                                                                             
*&&US                                                                           
         CLC   QCLI,SPACES                                                      
         JNH   NXTXDF08                                                         
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         OC    QCLI,SPACES                                                      
         MVC   ACTKEY,SPACES       read for product lvl office override         
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         MVC   ACTKACT(L'QCLI),QCLI                                             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   NXTXDF08                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   NXTXDF08                                                         
         L     R2,AIO1                                                          
         LA    R2,ACTRFST                                                       
         XR    R0,R0                                                            
         USING PPRELD,R2                                                        
NXTXDF02 CLI   PPREL,0                                                          
         JE    NXTXDF05                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    NXTXDF04                                                         
         IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     NXTXDF02                                                         
*                                                                               
NXTXDF04 CLC   PPRGAOFF,SPACES                                                  
         JNH   NXTXDF05                                                         
         MVC   QOFFICE,PPRGAOFF   set if existing                               
         DROP  R2                                                               
*                                                                               
NXTXDF05 CLC   QPRO,SPACES                                                      
         JNH   NXTXDF08                                                         
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         OC    QPRO,SPACES                                                      
         OC    QCLI,SPACES                                                      
         MVC   ACTKEY,SPACES       read for product lvl office override         
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         MVC   ACTKACT(L'QCLI),QCLI                                             
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         LA    RE,ACTKACT(RE)                                                   
         MVC   0(L'QPRO,RE),QPRO                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   NXTXDF08                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   NXTXDF08                                                         
         L     R2,AIO1                                                          
         LA    R2,ACTRFST                                                       
         XR    R0,R0                                                            
         USING PPRELD,R2                                                        
NXTXDF06 CLI   PPREL,0                                                          
         JE    NXTXDF08                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    NXTXDF07                                                         
         IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     NXTXDF06                                                         
*                                                                               
NXTXDF07 CLC   PPRGAOFF,SPACES                                                  
         JNH   NXTXDF08                                                         
         MVC   QOFFICE,PPRGAOFF   set if existing                               
         DROP  R2                                                               
*&&                                                                             
*                                                                               
NXTXDF08 GOTOR (#NXTREC,ANXTREC),DMCB,XDFKEYT,('B#XDFREC',0),          +        
               (0,SAVED),AFLTXDF,0                                              
         J     EXITY                                                            
*                                                                               
ARYXDLH  LKOUT A,(R,NXTXDL),MULTIROW=Y,ROWNAME=XDLRECD                          
                                                                                
Array    LKOUT C,8,(A,ARYXDL)                                                   
                                                                                
         LKOUT E                                                                
                                                                                
ARYXDL   LKOUT A,(D,B#XDLREC,XDLRFST),EOT=EOR,NEWEL=B,                 +        
               ROWID=(XDFEL,XDFELQ),ROWWIDTH=(V,XDFLN)                          
                                                                                
Deflt    LKOUT C,8,XDFELD,(R,EDTSTAT)                                           
FieldNm  LKOUT C,9,XDFLDATA,CHAR,LEN=V                                          
FieldAct LKOUT C,10,XDFELD,(R,EDTACTL)                                          
                                                                                
         LKOUT E                                                                
                                                                                
                                                                                
***********************************************************************         
* SAVE OFF XDFCODE                                                    *         
***********************************************************************         
SETECODE L     R2,LP_AINP                                                       
         USING XDFELD,R2                                                        
         MVC   DCODE,XDFCODE      SAVE DATA CODE                                
         MVC   OXDFPTRS+L'XDFRPTR(L'XDFSEQ),XDFSEQ                              
         J     EXITY                                                            
***********************************************************************         
* SAVE OFF XDFRECD INFO                                               *         
***********************************************************************         
SETRCODE L     R2,LP_AINP                                                       
         USING XDFRECD,R2                                                       
         MVC   DOFF,XDFKOFF       SAVE OFFICE                                   
         MVC   DORTY,XDFKORTY     SAVE ORDER TYPE                               
         MVC   DCLI,XDFKCLI       SAVE CLIENT CODE                              
         MVC   DETY,XDFKETY       SAVE EXPENDITURE TYPE                         
         MVC   DWC,XDFKWC         SAVE WORK CODE                                
         MVC   DMED,XDFKMED       SAVE MEDIA CODE                               
         MVC   DSCH,XDFKSCH       SAVE SCHEME CODE                              
         MVC   OXDFPTRS,XDFRPTR                                                 
         J     EXITY                                                            
***********************************************************************         
* read XDL record                                                     *         
***********************************************************************         
NXTXDL   GOTOR (#NXTREC,ANXTREC),DMCB,XDLKEYT,('B#XDLREC',SVXDFKEY),   +        
               (0,SAVED),0,0                                                    
         J     EXITY                                                            
***********************************************************************         
* Order Audit request                                                 *         
***********************************************************************         
                                                                                
REQAUD   LKREQ H,A#OAUD,OUTAUD,NEXTREQ=REQUESTX                                 
OrdNo    LKREQ F,1,(D,B#SAVED,QORDNUM),CHAR,OLEN=L'QORDNUM,            +        
               MAXLEN=L'QORDNUM,TEXT=(*,ORDNLIT),COL=*                          
         LKREQ E                                                                
                                                                                
OUTAUD   LKOUT H                                                                
ORDAUD   LKOUT R,A#OAUD            Audit                                        
Array    LKOUT C,A#OAUD,(A,ARYCAUD)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYCAUD  LKOUT A,(R,NXTAUD),MULTIROW=Y,ROWNAME=AUDRECD                          
                                                                                
Array    LKOUT C,A#OAUD,(A,ARYAUDO)                                             
Array    LKOUT C,22,(A,ARYAUDN)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYAUDO  LKOUT A,(D,B#AUD,AUDRFST),EOT=EOR,NEWEL=B,                    +        
               ROWID=(STCEL,STCELQ),ROWWIDTH=(V,STCLN)                          
*                                                                               
Prout    LKOUT P,,SETAUD                                                        
AuFrS    LKOUT C,1,(D,B#SAVED,SAUFRS),CHAR,LEN=1,ND=Y,                 +        
               FILTROUT=TSTACO,SKIPCOLS=AUTSKIPS                                
AUTSKIP  EQU   *                                                                
AuToS    LKOUT C,2,(D,B#SAVED,SAUTOS),CHAR,LEN=1,ND=Y                           
AuDat    LKOUT C,3,(D,B#SAVED,SAUDTE),PDAT,ND=Y                                 
AuTim    LKOUT C,4,(D,B#SAVED,SAUTIM),CHAR,LEN=6,ND=Y                           
AuUsr    LKOUT C,5,(D,B#SAVED,SAUUSR),CHAR,LEN=10,ND=Y                          
AuPID    LKOUT C,6,(D,B#SAVED,SAUPID),CHAR,LEN=8,ND=Y                           
AuFNa    LKOUT C,7,(D,B#SAVED,SAUPER),CHAR,LEN=16,ND=Y                          
AuLNa    LKOUT C,8,(D,B#SAVED,SAUPEL),CHAR,LEN=16,ND=Y                          
AuTxt    LKOUT C,9,(D,B#SAVED,SAUTXT),CHAR,LEN=50,ND=Y                          
AuTot    LKOUT C,10,(D,B#SAVED,SAUTCT),SPAK,ND=Y                                
AuACo    LKOUT C,11,(D,B#SAVED,SAUACO),CHAR,LEN=50,ND=Y                         
Appl     LKOUT C,25,(D,B#SAVED,SAUAPP),(R,EDTTYP),LEN=1,               +        
               FILTROUT=TSTRODK,ND=Y                                            
AUTSKIPS EQU   (*-AUTSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYAUDN  LKOUT A,(D,B#AUD,AUDRFST),EOT=EOR,NEWEL=B,                    +        
               ROWID=(STCEL,STCELQ),ROWWIDTH=(V,STCLN)                          
*                                                                               
PRout    LKOUT P,STCIND,SETTTYPE                                                
Date     LKOUT C,01,STCODTE,PDAT,ND=Y,                                 +        
               FILTROUT=TSTNEW,SKIPCOLS=NEWSKIPS                                
NEWSKIP  EQU   *                                                                
PRout    LKOUT P,STCELD,PROCTIME                                                
Time     LKOUT C,02,(D,B#SAVED,OA_TIME),CHAR,ND=Y                               
User     LKOUT C,03,STCOUSR,(U,#EDTUSR,$EDTUSR),ND=Y                            
PidChr   LKOUT C,04,STCOPID,(U,#EDTPID,$EDTPID)                                 
PRout    LKOUT P,,SETNAME                                                       
FirstNam LKOUT C,05,(D,B#WORKD,TEMP2),CHAR,LEN=16,ND=Y                          
MidNam   LKOUT C,06,(D,B#WORKD,TEMP2+32),CHAR,LEN=16,ND=Y                       
LastNam  LKOUT C,07,(D,B#WORKD,WORK2),CHAR,LEN=58,ND=Y                          
PRout    LKOUT P,STCOTYP,SETTYP                                                 
Type     LKOUT C,08,(D,B#WORKD,BYTE1),LBIN,ND=Y                                 
Appl     LKOUT C,09,STCOTYP,(R,EDTTYP),LEN=1,                          +        
               FILTROUT=TSTRODK,ND=Y                                            
Array    LKOUT C,01,(A,ARYODTL),FILTROUT=TSTODTL                                
Array    LKOUT C,02,(A,ARYODWC),FILTROUT=TSTOWC                                 
Array    LKOUT C,03,(A,ARYOMCH),FILTROUT=TSTOMCH                                
Array    LKOUT C,05,(A,ARYOXDT),FILTROUT=TSTOXDT                                
Array    LKOUT C,09,(A,ARYGAPS),FILTROUT=TSTGAPST                               
NEWSKIPS EQU   (*-NEWSKIP)/LX_COLSL                                             
                                                                                
         LKOUT E                                                                
                                                                                
ARYODTL  LKOUT A,(*),ROWNAME=STCELD,NROWS=1,NEWEL=B,                   +        
               ROWID=(STCEL,STCELQ),ROWWIDTH=(V,STCLN)                          
DteChg   LKOUT C,01,STCOIND1,(R,EDTDTE)                                         
RDtChg   LKOUT C,02,STCOIND1,(R,EDTRDT)                                         
EtyChg   LKOUT C,03,STCOIND1,(R,EDTETY)                                         
XAcChg   LKOUT C,04,STCOIND3,(R,EDTXAC)                                         
CliChg   LKOUT C,05,STCOIND1,(R,EDTCLI)                                         
ProChg   LKOUT C,06,STCOIND1,(R,EDTPRO)                                         
JobChg   LKOUT C,07,STCOIND1,(R,EDTJOB)                                         
WCChg    LKOUT C,08,STCOIND1,(R,EDTWC)                                          
WCAChg   LKOUT C,09,STCOIND1,(R,EDTWCA)                                         
SupChg   LKOUT C,10,STCOIND2,(R,EDTSUP)                                         
OffChg   LKOUT C,11,STCOIND2,(R,EDTOFF)                                         
DptChg   LKOUT C,12,STCOIND2,(R,EDTDPT)                                         
StfChg   LKOUT C,13,STCOIND2,(R,EDTSTF)                                         
CurChg   LKOUT C,14,STCOIND2,(R,EDTCUR)                                         
CAmChg   LKOUT C,15,STCOIND2,(R,EDTCAM)                                         
AmtChg   LKOUT C,16,STCOIND2,(R,EDTAMT)                                         
NamChg   LKOUT C,17,STCOIND2,(R,EDTNAM)                                         
PDsChg   LKOUT C,18,STCOIND3,(R,EDTPDS)                                         
MDsChg   LKOUT C,19,STCOIND3,(R,EDTMDS)                                         
HdrChg   LKOUT C,20,STCOIND3,(R,EDTHDR)                                         
FtrChg   LKOUT C,21,STCOIND3,(R,EDTFTR)                                         
DelChg   LKOUT C,22,STCOIND3,(R,EDTDEL)                                         
AprChg   LKOUT C,23,STCOIND3,(R,EDTAPR)                                         
WstChg   LKOUT C,24,STCOIND3,(R,EDTWST)                                         
Date     LKOUT C,25,STCODATE,PDAT,ND=Y                                          
RqdDte   LKOUT C,26,STCORQDT,PDAT,ND=Y                                          
Etype    LKOUT C,27,STCOETYP,CHAR,ND=Y                                          
EtyNam   LKOUT C,28,STCOETYP,(R,EDTETN),ND=Y                                    
ExAct    LKOUT C,29,STCOXULA,CHAR,ND=Y                                          
ExNam    LKOUT C,30,STCOXULA,(R,EDTANM),ND=Y                                    
PRout    LKOUT P,STCOSJAC,BLDSJAC                                               
CliCod   LKOUT C,31,(D,B#SAVED,OA_SJULA),(U,#EDTCLI,$EDTCLI)                    
CliNam   LKOUT C,32,(D,B#SAVED,OA_SJULA),(R,EDTCLN)                             
ProCod   LKOUT C,33,(D,B#SAVED,OA_SJULA),(U,#EDTPRD,$EDTPRD)                    
ProNam   LKOUT C,34,(D,B#SAVED,OA_SJULA),(R,EDTPRN)                             
JobCod   LKOUT C,35,(D,B#SAVED,OA_SJULA),(U,#EDTJOB,$EDTJOB)                    
JobNam   LKOUT C,36,(D,B#SAVED,OA_SJULA),(R,EDTJBN)                             
SupAc    LKOUT C,37,STCOSULA,CHAR,ND=Y,FILTROUT=TSTLDGO,SKIPCOLS=1              
SupNam   LKOUT C,38,STCOSULA,(R,EDTANM),ND=Y                                    
TwoD     LKOUT C,39,STCO2DAC,CHAR,ND=Y,FILTROUT=TST2DOF,SKIPCOLS=2              
PRout    LKOUT P,STCO2DAC,BLD2DAC                                               
TwoDNam  LKOUT C,40,(D,B#SAVED,OA_2DULA),(R,EDTANM),ND=Y                        
TwoP     LKOUT C,41,STCO2PAC,CHAR,ND=Y                                          
PRout    LKOUT P,STCO2PAC,BLD2PAC                                               
TwoPNam  LKOUT C,42,(D,B#SAVED,OA_2PULA),(R,EDTANM),ND=Y                        
ForCur   LKOUT C,43,STCOCURC,CHAR,ND=Y                                          
CurAmt   LKOUT C,44,STCOFCAM,SPAK,ND=Y                                          
AgyAmt   LKOUT C,45,STCOAMT,SPAK,ND=Y                                           
WrFrSta  LKOUT C,46,STCOFRST,LBIN,ND=Y                                          
WrToSta  LKOUT C,47,STCOTOST,LBIN,ND=Y                                          
GRvdChg  LKOUT C,48,STCOIND4,(R,EDTGDR)                                         
GoodRcv  LKOUT C,49,STCOSTAT,(R,EDTGDRD)                                        
MStatus  LKOUT C,50,STCOSTAT,(R,EDTMSTA),ND=Y                                   
AttnChg  LKOUT C,51,STCOIND4,(R,EDTATN)                                         
Est#Chg  LKOUT C,52,STCOIND4,(R,EDTESN)                                         
ItTxChg  LKOUT C,53,STCOIND4,(R,EDTITX)                                         
WC       LKOUT C,100,STCOWC,CHAR,ND=Y                                           
WCdesc   LKOUT C,101,STCOWC,(R,EDTWCD),ND=Y                                     
WCAmt    LKOUT C,102,STCOWCAM,SPAK,ND=Y                                         
Comments LKOUT C,103,STCOCOM,CHAR,LEN=V,ND=Y                                    
                                                                                
         LKOUT E                                                                
                                                                                
ARYODWC  LKOUT A,(*,STCOWKNT),ROWNAME=STCELD,NROWS=*,ROWWIDTH=L'STCOWKN+        
               T,NEWEL=B                                                        
WC       LKOUT C,100,STCOWK,CHAR,ND=Y                                           
WCdesc   LKOUT C,101,STCOWK,(R,EDTWCD),ND=Y                                     
WCAmt    LKOUT C,102,STCOWKAM,SPAK,ND=Y                                         
                                                                                
         LKOUT E                                                                
                                                                                
ARYOMCH  LKOUT A,(*),ROWNAME=STCELD,NROWS=1,NEWEL=B,                   +        
               ROWID=(STCEL,STCELQ),ROWWIDTH=(V,STCLN)                          
InvRef   LKOUT C,01,STCOIREF,CHAR,ND=Y                                          
InvDte   LKOUT C,02,STCOIDTE,PDAT,ND=Y                                          
Appl     LKOUT C,03,STCOAPPL,LBIN,ND=Y                                          
BRef     LKOUT C,04,STCOBMRF,CHAR,ND=Y                                          
MStatus  LKOUT C,05,STCOMIND,HEXD,ND=Y                                          
InvLog   LKOUT C,06,STCOILOG,CHAR,ND=Y                                          
*&&UK                                                                           
SupAc    LKOUT C,07,STCOISUA,CHAR,ND=Y                                          
SupNam   LKOUT C,08,STCOISUA,(R,EDTANM),ND=Y                                    
ExAct    LKOUT C,09,STCOIXUA,CHAR,ND=Y                                          
ExNam    LKOUT C,10,STCOIXUA,(R,EDTANM),ND=Y                                    
PRout    LKOUT P,STCOISJA,BLDSJAC                                               
CliCod   LKOUT C,11,(D,B#SAVED,OA_SJULA),(U,#EDTCLI,$EDTCLI)                    
CliNam   LKOUT C,12,(D,B#SAVED,OA_SJULA),(R,EDTCLN)                             
ProCod   LKOUT C,13,(D,B#SAVED,OA_SJULA),(U,#EDTPRD,$EDTPRD)                    
ProNam   LKOUT C,14,(D,B#SAVED,OA_SJULA),(R,EDTPRN)                             
JobCod   LKOUT C,15,(D,B#SAVED,OA_SJULA),(U,#EDTJOB,$EDTJOB)                    
JobNam   LKOUT C,16,(D,B#SAVED,OA_SJULA),(R,EDTJBN)                             
TwoD     LKOUT C,17,STCOI2DA,CHAR,ND=Y                                          
PRout    LKOUT P,STCOI2DA,BLD2DAC                                               
TwoDNam  LKOUT C,18,(D,B#SAVED,OA_2DULA),(R,EDTANM),ND=Y                        
TwoP     LKOUT C,19,STCOI2PA,CHAR,ND=Y                                          
PRout    LKOUT P,STCOI2PA,BLD2PAC                                               
TwoPNam  LKOUT C,20,(D,B#SAVED,OA_2PULA),(R,EDTANM),ND=Y                        
*&&                                                                             
Array    LKOUT C,04,(A,ARYMWC)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYMWC   LKOUT A,(*,STCOMTNT),ROWNAME=STCELD,NROWS=*,ROWWIDTH=L'STCOMTN+        
               T,NEWEL=B                                                        
*&&UK                                                                           
OrWC     LKOUT C,06,STCOOWC,CHAR,ND=Y                                           
OrWCDes  LKOUT C,07,STCOOWC,(R,EDTWCD),ND=Y                                     
*&&                                                                             
MtWC     LKOUT C,08,STCOMWC,CHAR,ND=Y                                           
MtWCDes  LKOUT C,09,STCOMWC,(R,EDTWCD),ND=Y                                     
MtWCAmt  LKOUT C,10,STCOMWCA,SPAK                                               
                                                                                
         LKOUT E                                                                
                                                                                
ARYOXDT  LKOUT A,(*),ROWNAME=STCELD,NROWS=1,NEWEL=B,                   +        
               ROWID=(STCEL,STCELQ),ROWWIDTH=(V,STCLN)                          
XdCode   LKOUT C,01,STCOXDTC,(U,#EDTHEX,$EDTHEX),ND=Y                           
PRout    LKOUT P,STCOXDTT,SETXDTYP                                              
XdDate   LKOUT C,02,STCOXDDT,PDAT,ND=Y,LEN=3,FILTROUT=TESTXDDT                  
XdAmnt   LKOUT C,02,STCOXDAT,SPAK,ND=Y,LEN=6,FILTROUT=TESTXDAM                  
XdData   LKOUT C,02,STCOXDCH,CHAR,ND=Y,LEN=V,FILTROUT=TESTXDCH                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYGAPS  LKOUT A,(*),ROWNAME=STCELD,NROWS=1,NEWEL=B,                   +        
               ROWID=(STCEL,STCELQ),ROWWIDTH=(V,STCLN)                          
Gapfr    LKOUT C,01,STCGAPFR,HEXD,ND=Y                                          
Gapto    LKOUT C,02,STCGAPTO,HEXD,ND=Y                                          
Gapex    LKOUT C,03,STCGEXDT,PDAT,ND=Y                                          
Gapem    LKOUT C,04,STCGEML,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read order audit records                                            *         
***********************************************************************         
                                                                                
NXTAUD   MVC   IOKEY,SVAUDKY                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,AUDKEYT,('B#AUD',0),             +        
               (0,SAVED),0,0                                                    
         JNE   EXITY                                                            
         MVC   SVAUDKY,IOKEY                                                    
         J     EXITY                                                            
                                                                                
         USING STCELD,R1                                                        
PROCTIME L     R1,LP_AINP                                                       
         UNPK  DUB2,STCOTIM                                                     
         OI    DUB2+7,C'0'                                                      
         MVC   OA_TIME,DUB2+2                                                   
         J     EXIT                                                             
                                                                                
SETTTYPE L     R1,LP_AINP          Set type of status change element            
         MVC   WORK(L'STCIND),0(R1)                                             
         J     EXIT                                                             
         DROP  R1                                                               
                                                                                
BLDSJAC  L     R1,LP_AINP                                                       
         MVC   OA_SJULA(L'ACTKUNT+L'ACTKLDG),PRODUL                             
         MVC   OA_SJULA+L'ACTKUNT+L'ACTKLDG(L'ACTKACT),0(R1)                    
         J     EXIT                                                             
                                                                                
BLD2PAC  L     R1,LP_AINP                                                       
         MVC   OA_2PULA(L'ACTKUNT+L'ACTKLDG),=C'2P'                             
         MVC   OA_2PULA+L'ACTKUNT+L'ACTKLDG(L'ACTKACT),0(R1)                    
         J     EXIT                                                             
                                                                                
BLD2DAC  L     R1,LP_AINP                                                       
         MVC   OA_2DULA(L'ACTKUNT+L'ACTKLDG),=C'2D'                             
         MVC   OA_2DULA+L'ACTKUNT+L'ACTKLDG(L'ACTKACT),0(R1)                    
         J     EXIT                                                             
                                                                                
         USING STCELD,R2                                                        
SETAUD   L     R2,LP_AINP                                                       
         LA    R0,SDAREA                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         LHI   R1,SAUDITQ                                                       
         MVCL  R0,RE                                                            
         CLI   STCIND,STCIORD2                                                  
         JE    SETAUD24                                                         
         MVC   SAUFRS,STCDFR                                                    
         CLI   STCDFR,FF           the GAP audit element                        
         JNE   SETAUD02                                                         
         MVI   SAUFRS,C'*'         * to indicate new or =ORD                    
                                                                                
SETAUD02 MVC   SAUTOS,STCDTO                                                    
         MVC   TEMP2(2),STCPERS                                                 
         GOTOR (#GETPID,AGETPID)                                                
         JNE   SETAUD04                                                         
         MVC   SAUPID,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   SETAUD04                                                         
         MVC   SAUPER,TEMP2                                                     
         MVC   SAUPEL,TEMP2+16                                                  
                                                                                
SETAUD04 MVC   SAUDTE,STCDATE                                                   
         UNPK  DUB2,STCTIME                                                     
         OI    DUB2+7,C'0'                                                      
         MVC   SAUTIM,DUB2+2                                                    
         MVC   TEMP2(2),STCUSER                                                 
         GOTOR (#GETUSR,AGETUSR)                                                
         JE    SETAUD06                                                         
         MVI   SAUUSR,C'<'         pass <user> if no name found                 
         MVI   SAUUSR+5,C'>'                                                    
         GOTO1 VHEXOUT,DMCB,STCPERS,SAUUSR+1,L'STCPERS                          
         J     SETAUD08                                                         
                                                                                
SETAUD06 MVC   SAUUSR,TEMP2                                                     
                                                                                
SETAUD08 ZAP   SAUTCT,PZERO                                                     
         CLI   STCDFR,C'X'         work code/amount changes only                
         JNE   SETAUD10                                                         
         CLI   STCDTO,C'W'                                                      
         JE    SETAUD12                                                         
         CLI   STCDTO,C'A'                                                      
         JE    SETAUD12                                                         
                                                                                
SETAUD10 XR    RE,RE                                                            
         IC    RE,STCLN                                                         
         SHI   RE,STCLN1Q                                                       
         LTR   RE,RE                                                            
         JNP   SETAUD14                                                         
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   SAUTXT(0),STCCOMM                                                
         J     SETAUD14                                                         
                                                                                
SETAUD12 ZAP   SAUTCT,STCCOMM(6)                                                
         CLI   STCLN,STCLN1Q+6                                                  
         JNH   SETAUD14                                                         
         XR    RE,RE                                                            
         IC    RE,STCLN                                                         
         SHI   RE,STCLN1Q+6+1                                                   
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   SAUTXT(0),STCCOMM+6 pass old work codes                          
                                                                                
SETAUD14 XR    RE,RE                                                            
         IC    RE,STCLN                                                         
         AR    R2,RE                                                            
         CLI   STCEL,0                                                          
         JE    SETAUDX                                                          
         CLI   STCEL,STCELQ                                                     
         JNE   SETAUDX                                                          
         CLI   STCDFR,C'+'                                                      
         JNE   SETAUDX                                                          
         CLI   STCDTO,C'+'                                                      
         JNE   SETAUDX                                                          
         XR    RE,RE                                                            
         IC    RE,STCLN                                                         
         SHI   RE,STCLN1Q                                                       
         LTR   RE,RE                                                            
         JNP   SETAUDX                                                          
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   SAUACO(0),STCCOMM                                                
         J     SETAUDX                                                          
*                                                                               
SETAUD24 MVC   TEMP2(2),STCOPID                                                 
         GOTOR (#GETPID,AGETPID)                                                
         JNE   SETAUD26                                                         
         MVC   SAUPID,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   SETAUD26                                                         
         MVC   SAUPER,TEMP2                                                     
         MVC   SAUPEL,TEMP2+16                                                  
                                                                                
SETAUD26 MVC   SAUDTE,STCODTE                                                   
         MVC   SAUAPP,STCOTYP                                                   
         UNPK  DUB2,STCOTIM                                                     
         OI    DUB2+7,C'0'                                                      
         MVC   SAUTIM,DUB2+2                                                    
         MVC   TEMP2(2),STCOUSR                                                 
         GOTOR (#GETUSR,AGETUSR)                                                
         JE    SETAUD28                                                         
         MVI   SAUUSR,C'<'         pass <user> if no name found                 
         MVI   SAUUSR+5,C'>'                                                    
         GOTO1 VHEXOUT,DMCB,STCOUSR,SAUUSR+1,L'STCOUSR                          
         J     SETAUD29                                                         
                                                                                
SETAUD28 MVC   SAUUSR,TEMP2                                                     
*                                                                               
SETAUD29 LA    RE,AUDTAB                                                        
*                                                                               
         MVC   BYTE1,STCOTYP                                                    
         NI    BYTE1,X'0F'                                                      
         CLI   BYTE1,STCOADDQ                                                   
         JE    *+12                                                             
         CLI   BYTE1,STCOCHGQ                                                   
         JNE   SETAUD40                                                         
*                                                                               
SETAUD30 CLC   STCOFRST,0(RE)                                                   
         JE    SETAUD32                                                         
         LA    RE,2(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         JNE   SETAUD30                                                         
         DC    H'0'                                                             
*                                                                               
SETAUD32 MVC   SAUFRS,1(RE)                                                     
         LA    RE,AUDTAB                                                        
SETAUD34 CLC   STCOTOST,0(RE)                                                   
         JE    SETAUD36                                                         
         LA    RE,2(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         JNE   SETAUD34                                                         
         DC    H'0'                                                             
*                                                                               
SETAUD36 MVC   SAUTOS,1(RE)                                                     
                                                                                
SETAUD40 MVC   BYTE1,STCOTYP                                                    
         NI    BYTE1,X'0F'                                                      
         CLI   BYTE1,STCOAPPQ                                                   
         JNE   SETAUDX                                                          
         MVI   SAUFRS,STCOPAPP                                                  
         MVI   SAUTOS,STCOPAPP                                                  
*                                                                               
SETAUDX  MVI   LP_RMODE,LP_RNEXT   set mode for NXTREC routine                  
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
SETXDTYP L     R1,LP_AINP          Set type of stcel extra data elem            
         MVC   OA_TYP,0(R1)                                                     
         J     EXIT                                                             
                                                                                
TESTXDDT CLI   OA_TYP,STCXTDT      Test xdata is date                           
         BR    RE                                                               
                                                                                
TESTXDAM CLI   OA_TYP,STCXTAMT     Test xdata is amount                         
         BR    RE                                                               
                                                                                
TESTXDCH CLI   OA_TYP,STCXTCHR     Test xdata is character                      
         BER   RE                                                               
         CLI   OA_TYP,STCXTNUM     Test xdata is numerical                      
         BER   RE                                                               
         CLI   OA_TYP,STCXTYN      Test xdata is Yes/No                         
         BER   RE                                                               
         CLI   OA_TYP,STCXTDRP     Test xdata is dropdown list                  
         BR    RE                                                               
                                                                                
         USING STCELD,R1                                                        
TSTACO   L     R1,LP_AINP                                                       
         TM    SDMODE,SDMPDFQ          For embedded call show new               
         JNZ   *+12                     audit as old style                      
         CLI   STCIND,STCIORD2         If not embedded reject new style         
         JE    EXITN                                                            
         CLI   STCIND,STCIORD2                                                  
         JNE   TSTACO10                                                         
         MVC   BYTE1,STCOTYP                                                    
         NI    BYTE1,X'0F'                                                      
         CLI   BYTE1,STCOADDQ        We only want certain types                 
         JE    EXITY                     for approval and submission            
         CLI   BYTE1,STCOCHGQ                        information                
         JE    EXITY                                                            
         CLI   BYTE1,STCOAPPQ                                                   
         JE    EXITY                                                            
         J     EXITN                   Otherwise reject                         
*                                                                               
TSTACO10 CLI   STCDFR,C'+'                                                      
         JE    EXITN                                                            
         CLI   STCDTO,C'+'                                                      
         JE    EXITN                                                            
         CLI   STCIND,STCISTA      take status changes only                     
         JE    EXITY                                                            
         CLI   STCIND,STCIOMA                                                   
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
TSTNEW   L     R1,LP_AINP                                                       
         TM    SDMODE,SDMPDFQ          For embedded call show don't             
         JNZ   EXITN                    new audit                               
         CLI   STCIND,STCIORD2         Is it new style                          
         JNE   EXITN                   No - don't show here                     
         LH    RF,COUNT                                                         
         AHI   RF,1                                                             
         STH   RF,COUNT                                                         
         CHI   RF,2                                                             
         J     EXITY                                                            
         DC    H'0'                                                             
         J     EXITY                                                            
                                                                                
TSTODTL  L     R1,LP_AINP                                                       
         MVC   BYTE1,STCOTYP                                                    
         NI    BYTE1,X'0F'                                                      
         CLI   BYTE1,STCOCHGQ        Order change                               
         JE    EXITY                   Yes - show detail                        
         CLI   BYTE1,STCOADDQ        Order add                                  
         JE    EXITY                   Yes - show detail                        
         CLI   BYTE1,STCOAPPQ        Order approval                             
         JE    EXITY                   Yes - show detail                        
         J     EXITN                   No - don't show here                     
                                                                                
TSTOWC   L     R1,LP_AINP                                                       
         MVC   BYTE1,STCOTYP                                                    
         NI    BYTE1,X'0F'                                                      
         CLI   BYTE1,STCOWCCQ        Order change/add additional wc             
         JE    EXITY                   Yes - show detail                        
         J     EXITN                                                            
                                                                                
TSTOMCH  L     R1,LP_AINP                                                       
         MVC   BYTE1,STCOTYP                                                    
         NI    BYTE1,X'0F'                                                      
         CLI   BYTE1,STCOMATQ        Order matching                             
         JE    EXITY                   Yes - show detail                        
         J     EXITN                                                            
                                                                                
TSTOXDT  L     R1,LP_AINP                                                       
         MVC   BYTE1,STCOTYP                                                    
         NI    BYTE1,X'0F'                                                      
         CLI   BYTE1,STCOXDCQ        Order extra data change                    
         JE    EXITY                   Yes - show detail                        
         CLI   BYTE1,STCOXDAQ        Order extra data add                       
         JE    EXITY                   Yes - show detail                        
         CLI   BYTE1,STCOXDDQ        Order extra data deletion                  
         JE    EXITY                   Yes - show detail                        
         J     EXITN                                                            
                                                                                
TSTGAPST L     R1,LP_AINP                                                       
         MVC   BYTE1,STCOTYP                                                    
         NI    BYTE1,X'0F'                                                      
         CLI   BYTE1,STCOGAPQ        GAP status                                 
         BR    RE                                                               
                                                                                
TSTRODK  SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         BR    RE                                                               
         DROP  R1                                                               
***********************************************************************         
* End of requests                                                     *         
***********************************************************************         
                                                                                
REQUESTX LKREQ X                                                                
                                                                                
         LKARY T                                                                
         EJECT                                                                  
***********************************************************************         
* Local routines                                                      *         
***********************************************************************         
***********************************************************************         
* Initialise optimisation buffer (uses WSSVR buffer)                  *         
***********************************************************************         
                                                                                
INIOBUF  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*INIOBUF'                                                      
                                                                                
         XC    TSAROBUF(TSPNEWL),TSAROBUF                                       
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAINI     Set action to 'Initialise'                   
         MVI   T.TSRECI,TSRXTN+TSRWSSVR                                         
         MVI   T.TSKEYL,L'OB_KEY                                                
         LHI   R0,ONEK                                                          
         OC    T.TSBUFFL,T.TSBUFFL                                              
         JNZ   *+8                                                              
         STCM  R0,3,T.TSBUFFL                                                   
         LHI   R0,OB_LNQ                                                        
         STCM  R0,3,T.TSRECL                                                    
         MVC   T.TSACOM,ACOMFACS                                                
         GOTOR VTSAR,T.TSARD                                                    
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* Add a record to optimisation buffer                                 *         
*                                                                     *         
* Ntry:- R1 points to caller's OB_D                                   *         
***********************************************************************         
                                                                                
ADDBUF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ADDBUF*'                                                      
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAADD     Set action to 'Add'                          
         ST    R1,T.TSAREC                                                      
         GOTOR VTSAR,T.TSARD                                                    
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* Get a record from optimisation buffer                               *         
*                                                                     *         
* Ntry:- R1 points to caller's OB_D                                   *         
* Exit:- CC=Low if record not found in buffer                         *         
*        CC=Equal if record found and is not posted with an error     *         
*           - record is returned in caller's OB_D                     *         
*        CC=High if record found and is posted with an error (set in  *         
*           ROUERRV)                                                  *         
***********************************************************************         
                                                                                
GETBUF   NTR1  LABEL=NO,WORK=(RC,OB_LNQ)                                        
         J     *+12                                                             
         DC    C'*GETBUF*'                                                      
                                                                                
W        USING OB_D,RC                                                          
         LR    R2,R1                                                            
P        USING OB_D,R2             R2=A(caller's OB_D)                          
         MVC   W.OB_KEY,P.OB_KEY                                                
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSARDH     Set action to 'Read High'                    
         LA    R0,W.OB_D                                                        
         ST    R0,T.TSAREC         Read into acquired storage                   
         GOTOR VTSAR,T.TSARD                                                    
         JNE   EXITL               Not found/EOF                                
         DROP  T                                                                
                                                                                
         MVC   P.OB_D(OB_LNQ),W.OB_D                                            
                                                                                
         MVC   ROUERRV,P.OB_ERROR  Set/test record in error                     
         OC    ROUERRV,ROUERRV                                                  
         JNZ   EXITH                                                            
         J     EXITY                                                            
         DROP  W,P                                                              
         EJECT                                                                  
***********************************************************************         
* Edit expenditure name                                               *         
***********************************************************************         
         SPACE 1                                                                
EDTETN   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ETYKCODE,R2),SPACES                                          
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ETYKCODE),0(R2)                                          
         GOTOR VALETY,TEMP2                                                     
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate expenditure type                                           *         
*                                                                     *         
* Ntry:- R1=A(Etype)                                                  *         
***********************************************************************         
                                                                                
VALETY   NTR1  LABEL=NO,WORK=(RC,VEWORKL)                                       
         J     *+12                                                             
         DC    C'*VALETY*'                                                      
                                                                                
         USING VEWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(account code)                           
         GOTOR CLRWRK,VEWORKL      Clear work area                              
         USING OB_D,VERBAREA                                                    
         MVI   OB_ETYP,OB_ETYPQ    Build key of buffer record                   
         MVI   OB_ESUB,OB_ESUBQ                                                 
         MVC   OB_EETY(L'ETYKCODE),0(R2)                                        
         GOTOR GETBUF,OB_D                                                      
         JL    VALETY02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_NAME),OB_NAME                                         
         MVC   WORK2(L'OB_ECAT),OB_ECAT                                         
         J     VALETYY                                                          
                                                                                
VALETY02 GOTOR (#GETETN,AGETETN)                                                
         MVC   OB_NAME(L'NAMEREC),TEMP2                                         
         MVC   OB_ECAT,WORK2                                                    
         GOTOR ADDBUF,OB_D                                                      
                                                                                
VALETYY  J     EXITY                                                            
                                                                                
         DROP  RC                                                               
                                                                                
VEWORKD  DSECT                     ** VALWCD local w/s **                       
VERBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VERBAREA                                                         
OB_ETYP  DS    X                   ** Buffer key **                             
OB_ETYPQ EQU   X'FF'               Buffer type                                  
OB_ESUB  DS    C                                                                
OB_ESUBQ EQU   C'E'                Buffer sub-type                              
OB_EETY  DS    CL(L'ETYKCODE)      Etype code                                   
         ORG   VERBAREA+(OB_OTHER-OB_D)                                         
OB_ECAT  DS    CL35                Category                                     
         ORG                                                                    
VEWORKL  EQU   *-VEWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit Account name                                                   *         
***********************************************************************         
         SPACE 1                                                                
EDTANM   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         CLC   =C'2D',0(R2)        Are we dealing with office dept ldgr         
         JNE   EDTANM10            No                                           
         TM    SCPYEL+CPYSTAT1-CPYELD,CPYSOROE Do we have offices               
         JZ    EDTANM10            No                                           
         LA    RE,3                Allow for unit ledger                        
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         JZ    *+8                                                              
         LA    RE,4                and further character for 2 char off         
         AR    RE,R2                                                            
         CLI   0(RE),C' '          Check whether we have a dept                 
         JH    EDTANM10            Yes - read for account                       
         MVC   TEMP2,SPACES        No - read for office                         
         MVC   TEMP2(L'TRNOFFC),L'ACTKUNT+L'ACTKLDG(R2)                         
         GOTOR VALOFF,TEMP2                                                     
         J     EDTANM20                                                         
                                                                                
EDTANM10 MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'TRNKULA),0(R2)                                           
         GOTOR VALACT                                                           
                                                                                
EDTANM20 MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate office                                                     *         
*                                                                     *         
* Ntry:- R1=A(Office)                                                 *         
***********************************************************************         
                                                                                
VALOFF   NTR1  LABEL=NO,WORK=(RC,VOWORKL)                                       
         J     *+12                                                             
         DC    C'*VALOFF*'                                                      
                                                                                
         USING VOWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(account code)                           
         GOTOR CLRWRK,VOWORKL      Clear work area                              
         USING OB_D,VORBAREA                                                    
         MVI   OB_OTYP,OB_OTYPQ    Build key of buffer record                   
         MVI   OB_OSUB,OB_OSUBQ                                                 
         MVC   OB_OOFC(L'TRNOFFC),0(R2)                                         
         GOTOR GETBUF,OB_D                                                      
         JL    VALOFF02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_NAME),OB_NAME                                         
         J     VALOFFY                                                          
                                                                                
VALOFF02 GOTOR (#GETOFN,AGETOFN)                                                
         MVC   OB_NAME(L'NAMEREC),TEMP2                                         
         GOTOR ADDBUF,OB_D                                                      
                                                                                
VALOFFY  J     EXITY                                                            
                                                                                
         DROP  RC                                                               
                                                                                
VOWORKD  DSECT                     ** VALOFF local w/s **                       
VORBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VORBAREA                                                         
OB_OTYP  DS    X                   ** Buffer key **                             
OB_OTYPQ EQU   X'FF'               Buffer type                                  
OB_OSUB  DS    C                                                                
OB_OSUBQ EQU   C'F'                Buffer sub-type                              
OB_OOFC  DS    CL(L'TRNOFFC)       Office code                                  
         ORG                                                                    
VOWORKL  EQU   *-VOWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit Client name                                                    *         
***********************************************************************         
         SPACE 1                                                                
EDTCLN   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         AHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   TEMP2(0),0(R2)                                                   
         EX    RE,0(RF)                                                         
         GOTOR VALACT                                                           
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Edit product name - parm 1 unit/ledger/account                      *         
***********************************************************************         
         SPACE 1                                                                
EDTPRN   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
                                                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,PCLILEN                                                       
         IC    RF,PPROLEN                                                       
         SR    RF,RE                                                            
         AR    RE,R2                                                            
         AHI   RE,L'ACTKUNT+L'ACTKLDG                                           
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         CLC   0(0,RE),SPACES      DO WE HAVE A PRODUCT LEVEL                   
         EX    RE,0(R1)                                                         
         JNH   EXITY               NO                                           
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         AHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   TEMP2(0),0(R2)                                                   
         EX    RE,0(R1)                                                         
         GOTOR VALACT                                                           
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Edit job name - parm 1 is unit/ledger/account                       *         
***********************************************************************         
         SPACE 1                                                                
EDTJBN   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'ACTKULA,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2,SPACES                                                     
                                                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,PPROLEN                                                       
         IC    RF,PJOBLEN                                                       
         SR    RF,RE               RF=LENGTH OF JOB CODE                        
         AHI   RE,1                RE=DISPLACEMENT TO JOB CODE                  
         AR    RE,R2                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         CLC   0(0,RE),SPACES      DO WE HAVE A JOB CODE                        
         EX    RE,0(R1)                                                         
         JNH   EXITY               NO EXIT                                      
         MVC   TEMP2(L'ACTKULA),0(R2)                                           
         GOTOR VALACT                                                           
         MVC   0(L'NAMEREC,R4),TEMP2                                            
         LHI   RE,L'NAMEREC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Validate account code                                               *         
*                                                                     *         
* Ntry:- R1=A(ULCliProJob)                                            *         
***********************************************************************         
                                                                                
VALACT   NTR1  LABEL=NO,WORK=(RC,OB_LNQ)                                        
         J     *+12                                                             
         DC    C'*VALACT*'                                                      
                                                                                
         USING VAWORKD,RC          RC=A(local working storage)                  
         GOTOR CLRWRK,VAWORKL      Clear work area                              
         USING OB_D,VARBAREA                                                    
         MVC   OB_KEY(L'ACTKULA),TEMP2                                          
         GOTOR GETBUF,OB_D                                                      
         JL    VALACT02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_NAME),OB_NAME                                         
         J     VALACTY                                                          
                                                                                
VALACT02 GOTOR (#GETACN,AGETACN)                                                
         MVC   OB_NAME,TEMP2                                                    
                                                                                
         GOTOR ADDBUF,OB_D                                                      
                                                                                
VALACTY  J     EXITY                                                            
                                                                                
         DROP  RC                                                               
                                                                                
VAWORKD  DSECT                     ** VALACT local w/s **                       
VARBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG   VARBAREA+(OB_OTHER-OB_D)                                         
VAWORKL  EQU   *-VAWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit workcode description - parm 1 is workcode                      *         
***********************************************************************         
         SPACE 1                                                                
EDTWCD   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'WCOKWRK,R2),SPACES                                           
         JNH   EXITY                                                            
         MVC   TEMP2(L'WCOKWRK),0(R2)                                           
         GOTOR VALWCD,TEMP2                                                     
         MVC   0(L'WCODESC,R4),TEMP2                                            
         LHI   RE,L'WCODESC                                                     
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit applicationt type parm 1 is STCOTYP                            *         
***********************************************************************         
         SPACE 1                                                                
EDTTYP   LM    R2,R4,LP_AINP                                                    
         MVC   BYTE4,0(R2)                                                      
         NI    BYTE4,X'F0'                                                      
         MVI   0(R4),C'1'          Default to BrandOcean                        
         CLI   BYTE4,STCOAURQ                                                   
         JNE   *+8                                                              
         MVI   0(R4),C'2'          Set Aura                                     
         CLI   BYTE4,STCOINVQ                                                   
         JNE   *+8                                                              
         MVI   0(R4),C'3'          Set invoice log                              
         CLI   BYTE4,STCOPSMQ                                                   
         JNE   *+8                                                              
         MVI   0(R4),C'4'          Set postman                                  
         CLI   BYTE4,STCOESGQ                                                   
         JNE   *+8                                                              
         MVI   0(R4),C'5'          Set electronic signature                     
         CLI   BYTE4,STCOMOBQ                                                   
         JNE   *+8                                                              
         MVI   0(R4),C'6'          Set mobile                                   
EDTTYP8  LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate workcode                                                   *         
*                                                                     *         
* Ntry:- R1=A(WC)                                                     *         
***********************************************************************         
                                                                                
VALWCD   NTR1  LABEL=NO,WORK=(RC,VWWORKL)                                       
         J     *+12                                                             
         DC    C'*VALWCD*'                                                      
                                                                                
         USING VWWORKD,RC          RC=A(local working storage)                  
         LR    R2,R1               R2=A(account code)                           
         GOTOR CLRWRK,VWWORKL      Clear work area                              
         USING OB_D,VWRBAREA                                                    
         MVC   OB_KEY(L'WCOKWRK),0(R2)                                          
         GOTOR GETBUF,OB_D                                                      
         JL    VALWCD02                                                         
         JH    EXITN                                                            
         MVC   TEMP2(L'OB_NAME),OB_NAME                                         
         J     VALWCDY                                                          
                                                                                
VALWCD02 GOTOR (#GETWCD,AGETWCD)                                                
         MVC   OB_NAME(L'WCODESC),TEMP2                                         
VALWCD06 GOTOR ADDBUF,OB_D                                                      
                                                                                
VALWCDY  J     EXITY                                                            
                                                                                
         DROP  RC                                                               
                                                                                
VWWORKD  DSECT                     ** VALWCD local w/s **                       
VWRBAREA DS    XL(OB_LNQ)          Buffer area                                  
         ORG                                                                    
VWWORKL  EQU   *-VWWORKD                                                        
                                                                                
SVRDEF   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Edit whether extra data field is required                           *         
***********************************************************************         
                                                                                
EDTREQD  LM    R2,R4,LP_AINP                                                    
         USING XDFELD,R2                                                        
         TM    XDFSTAT1,XDFSRECP                                                
         JZ    EDTREQ2                                                          
         MVI   0(R4),C'X'                                                       
         J     EDTREQDY                                                         
*                                                                               
EDTREQ2  MVI   0(R4),C'N'                                                       
         TM    XDFSTAT1,XDFSREQD                                                
         JZ    EDTREQDY                                                         
         MVI   0(R4),C'R'                                                       
                                                                                
EDTREQDY LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Edit order data changed                                             *         
***********************************************************************         
                                                                                
EDTDTE   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCODTEQ      Has order date changed                       
         JZ    EDTDTE02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTDTE02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit required date changed                                          *         
***********************************************************************         
                                                                                
EDTRDT   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCORQDQ      Has required by date changed                 
         JZ    EDTRDT02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTRDT02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit expenditure type changed                                       *         
***********************************************************************         
                                                                                
EDTETY   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOETYQ      Has Etype changed                            
         JZ    EDTETY02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTETY02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit client code changed                                            *         
***********************************************************************         
                                                                                
EDTCLI   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOCLIQ      Has client changed                           
         JZ    EDTCLI02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTCLI02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit product code changed                                           *         
***********************************************************************         
                                                                                
EDTPRO   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOPROQ      Has product changed                          
         JZ    EDTPRO02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTPRO02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit job changed                                                    *         
***********************************************************************         
                                                                                
EDTJOB   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOJOBQ      Has job changed                              
         JZ    EDTJOB02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTJOB02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit workcode changed                                               *         
***********************************************************************         
                                                                                
EDTWC    LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOWCQ       Has workcode changed                         
         JZ    EDTWC02                                                          
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTWC02  LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit workcode amount changed                                        *         
***********************************************************************         
                                                                                
EDTWCA   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOWCAQ      Has workcode amount changed                  
         JZ    EDTWCA02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTWCA02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit supplier changed                                               *         
***********************************************************************         
                                                                                
EDTSUP   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOSUPQ      Has supplier changed                         
         JZ    EDTSUP02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTSUP02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit office changed                                                 *         
***********************************************************************         
                                                                                
EDTOFF   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOOFFQ      Has office changed                           
         JZ    EDTOFF02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTOFF02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit department changed                                             *         
***********************************************************************         
                                                                                
EDTDPT   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCODPTQ      Has department changed                       
         JZ    EDTDPT02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTDPT02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit staff changed                                                  *         
***********************************************************************         
                                                                                
EDTSTF   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOSTFQ      Has staff changed                            
         JZ    EDTSTF02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTSTF02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit foreign currency changed                                       *         
***********************************************************************         
                                                                                
EDTCUR   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOFCQ       Has foreign currency changed                 
         JZ    EDTCUR02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTCUR02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit foreign currency amount changed                                *         
***********************************************************************         
                                                                                
EDTCAM   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOFCAQ      Has foregin currency amount changed          
         JZ    EDTCAM02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTCAM02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit agency amount changed                                          *         
***********************************************************************         
                                                                                
EDTAMT   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOAMTQ      Has agency amount changed                    
         JZ    EDTAMT02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTAMT02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit name has changed                                               *         
***********************************************************************         
                                                                                
EDTNAM   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCONAMQ      Has order name changed                       
         JZ    EDTNAM02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTNAM02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit printed description changed                                    *         
***********************************************************************         
                                                                                
EDTPDS   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOPDSQ      Has printed description changed              
         JZ    EDTPDS02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTPDS02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit matching description changed                                   *         
***********************************************************************         
                                                                                
EDTMDS   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOMDSQ      Has matching description changed             
         JZ    EDTMDS02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTMDS02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit header text changed                                            *         
***********************************************************************         
                                                                                
EDTHDR   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOHEDQ      Has header text changed                      
         JZ    EDTHDR02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTHDR02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit footer text changed                                            *         
***********************************************************************         
                                                                                
EDTFTR   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOFOTQ      Has footer text changed                      
         JZ    EDTFTR02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTFTR02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit delivery address changed                                       *         
***********************************************************************         
                                                                                
EDTDEL   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCODADQ      Has delivery address changed                 
         JZ    EDTDEL02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTDEL02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit approvers changed                                              *         
***********************************************************************         
                                                                                
EDTAPR   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOAPRQ      Have approvers changed                       
         JZ    EDTAPR02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTAPR02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit goods received changed                                         *         
***********************************************************************         
                                                                                
EDTGDR   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOGDRQ      Goods received changed                       
         JZ    EDTGDR02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTGDR02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit attention changed                                              *         
***********************************************************************         
                                                                                
EDTATN   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOATNQ      Attention changed                            
         JZ    EDTATN02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTATN02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit estimate number changed                                        *         
***********************************************************************         
                                                                                
EDTESN   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOESNQ      Estimate number changed                      
         JZ    EDTESN02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTESN02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit item text changed                                              *         
***********************************************************************         
                                                                                
EDTITX   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOITXQ      Item text changed                            
         JZ    EDTITX02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTITX02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit goods received                                                 *         
***********************************************************************         
                                                                                
EDTGDRD  LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOGDRD      Goods received                               
         JZ    EDTGDD02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTGDD02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit Matched Status                                                 *         
***********************************************************************         
                                                                                
EDTMSTA  LM    R2,R4,LP_AINP                                                    
                                                                                
*        MVC   0(1,R4),0(R2)                                                    
*        NI    BYTE1,FF-STCOGDRD                                                
         MVC   BYTE1,0(R2)                                                      
         NI    BYTE1,FF-STCOGDRD                                                
         CLI   BYTE1,0                                                          
         JE    EXITY                                                            
         GOTO1 VHEXOUT,DMCB,BYTE1,(R4),1                                        
*                                                                               
         LHI   R3,2                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit workflow status changed                                        *         
***********************************************************************         
                                                                                
EDTWST   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOWSTQ      Has workflow status changed                  
         JZ    EDTWST02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTWST02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit expense account changed                                        *         
***********************************************************************         
                                                                                
EDTXAC   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),STCOXACQ      Have approvers changed                       
         JZ    EDTXAC02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTXAC02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit pidel status of approval                                       *         
***********************************************************************         
                                                                                
EDTPST   LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),NOQ                                                        
         TM    0(R2),PIDAPPQ                                                    
         JZ    EDTPST02                                                         
         MVI   0(R4),YESQ                                                       
*                                                                               
EDTPST02 LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* Edit whether extra data field is active                             *         
***********************************************************************         
                                                                                
EDTACTV  LM    R2,R4,LP_AINP                                                    
         USING XDFELD,R2                                                        
         MVI   0(R4),C'A'                                                       
         OC    XDFCUT,XDFCUT                                                    
         JZ    EDTACTVY                                                         
         CLC   XDFCUT,OD#TODYP                                                  
         JH    EDTACTVY                                                         
         MVI   0(R4),C'N'                                                       
                                                                                
EDTACTVY LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Key Filter For XDFRECD                                              *         
***********************************************************************         
                                                                                
FLTXDF   NTR1  ,                                                                
         J     *+12                                                             
         DC    C'*FLTXDF*'                                                      
         LA    R2,IOKEY                                                         
         USING XDFRECD,R2                                                       
         OC    XDFKOFF,XDFKOFF                                                  
         JNZ   FLTXDF02                                                         
         OC    XDFKCLI(XDFKSEQ-XDFKCLI),XDFKCLI                                 
         JNZ   FLTXDF02                                                         
         CLI   XDFKORTY,XDFKDFT    Always show it if it is top level            
         JE    EXITY                                                            
         CLC   XDFKORTY,QORDTYP    or top application level                     
         JE    EXITY                                                            
         J     EXITN               Otherwise - get next record                  
*                                                                               
FLTXDF02 CLI   XDFKORTY,XDFKDFT    Default order type ?                         
         JE    *+14                                                             
         CLC   XDFKORTY,QORDTYP                                                 
         JNE   EXITN               No - get the next one                        
                                                                                
         OC    XDFKOFF,XDFKOFF                                                  
         JZ    *+14                                                             
         CLC   XDFKOFF,QOFFICE                                                  
         JNE   EXITN                                                            
         OC    XDFKCLI,XDFKCLI                                                  
         JZ    *+14                                                             
         CLC   XDFKCLI,QCLI                                                     
         JNE   EXITN                                                            
         OC    XDFKETY,XDFKETY                                                  
         JZ    *+14                                                             
         CLC   XDFKETY,QETY                                                     
         JNE   EXITN                                                            
         OC    XDFKWC,XDFKWC                                                    
         JZ    FLTXDF10                                                         
         USING LW_D,R3                                                          
         XR    R3,R3               Point to list of WC in WMP                   
         ICM   R3,7,QAWC                                                        
         JZ    EXITN                                                            
         XR    RF,RF                                                            
         ICM   RF,3,LW_NUMN        RF=number of entries                         
         JZ    EXITN                                                            
         LA    R4,LW_DATA2         R4=A(workcode list)                          
FLTXDF04 CLC   XDFKWC,0(R4)                                                     
         JE    FLTXDF10                                                         
         LA    R4,L'WCOKWRK(R4)                                                 
         JCT   RF,FLTXDF04                                                      
         J     EXITN                                                            
FLTXDF10 OC    XDFKMED,XDFKMED                                                  
         JZ    *+14                                                             
         CLC   XDFKMED,QMED                                                     
         JNE   EXITN               Get next XData record                        
         OC    XDFKSCH,XDFKSCH                                                  
         JZ    *+14                                                             
         CLC   XDFKSCH,QSCH                                                     
         JNE   EXITN               Get next XData record                        
         J     EXITY                                                            
*                                                                               
                                                                                
*** Key filter routine for ORDRECD ***                                          
         USING ORDRECD,R2                                                       
ORDKF    NTR1                                                                   
         J     *+12                                                             
         DC    C'**ORDKF*'                                                      
                                                                                
         LA    R2,IOKEY                                                         
         CLI   ORDKSEQ,ORDKEXTN    ignore extension record                      
         JE    EXITN                                                            
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
*** Aura MVP version checks ***                                                 
AURAMVP  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*AURAMVP'                                                      
                                                                                
         CLI   QISPDF,YESQ                                                      
         JE    AMVPYES                                                          
                                                                                
         XR    RF,RF                                                            
         ICM   RF,B'0011',CUXPNUM  Aura?                                        
         CHI   RF,XPRODIKQ                                                      
         JNE   AMVPYES                                                          
                                                                                
         CHI   R1,1                                                             
         JE    AMVP02                                                           
         CHI   R1,2                                                             
         JE    AMVP20                                                           
         DC    H'0'                                                             
                                                                                
         USING ORDRECD,R2                                                       
         USING ORDELD,R3                                                        
AMVP02   L     R2,AIO2                                                          
         LA    R3,ORDRFST                                                       
                                                                                
AMVP04   CLI   ORDEL,ORDELQ                                                     
         JE    AMVP08                                                           
         CLI   ORDEL,0                                                          
         JE    AMVPYES                                                          
                                                                                
AMVP06   LLC   R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     AMVP04                                                           
                                                                                
AMVP08   MVC   ROUERRV,=AL2(AE$IAUBO)                                           
         CLI   ORDSUPL,SKLDGQ                                                   
         JE    AMVPNO                                                           
         CLI   ORDSUPL,SILDGQ                                                   
         JE    AMVPNO                                                           
         CLI   ORDSUPL,STLDGQ                                                   
         JE    AMVPNO                                                           
                                                                                
AMVP10   MVC   ROUERRV,=AL2(AE$INPRE)                                           
         TM    ORDSTAT,ORDSPRES    Exclude Presto order                         
         JNZ   AMVPNO                                                           
         MVC   ROUERRV,=AL2(AE$IAUBO)                                           
         TM    ORDRSTA2,ORDSEXEX   Exclude none BrandOcean/Aura order           
         JZ    AMVPNO                                                           
                                                                                
         J     AMVP06                                                           
         DROP  R2,R3                                                            
                                                                                
         USING ORDRECD,R2                                                       
         USING ARTELD,R3                                                        
AMVP20   L     R2,AIO2                                                          
         LA    R3,ORDRFST                                                       
                                                                                
AMVP22   CLI   ARTEL,ARTELQ                                                     
         JE    AMVP26                                                           
         CLI   ARTEL,0                                                          
         JE    AMVPYES                                                          
                                                                                
AMVP24   LLC   R0,ARTLN                                                         
         AR    R3,R0                                                            
         J     AMVP22                                                           
                                                                                
AMVP26   MVC   ROUERRV,=AL2(AE$OIUBO)                                           
         CLI   ORDTYPE,EXPOQ       Allow only expense and production            
         JE    AMVP28                for items                                  
         CLI   ORDTYPE,PROOQ                                                    
         JNE   AMVPNO                                                           
AMVP28   TM    ARTSTAT,ARTSTXQ     Don't show no price items                    
         JNZ   AMVPNO                                                           
                                                                                
         USING PASRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    PASKEY,PASKEY                                                    
         MVI   PASKTYP,PASKTYPQ                                                 
         MVI   PASKSUB,PASKSQ                                                   
         MVC   PASKCPY,CUXCPY                                                   
         MVC   PASKSEQ,ARTSEQ                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   AMVP24                                                           
         TM    PASKSTA2,ARTKSTIM   Don't show time items                        
         JNZ   AMVPNO                                                           
         J     AMVP24                                                           
         DROP  R2,R3                                                            
                                                                                
AMVPYES  XC    ROUERRV,ROUERRV                                                  
         J     EXITY                                                            
                                                                                
AMVPNO   MVC   LP_ERROR,ROUERRV                                                 
         J     EXITN                                                            
                                                                                
*** Set order key/status routine ***                                            
         USING ORDRECD,R2                                                       
SOKEY    NTR1                                                                   
                                                                                
         MVC   OD#NUM,ORDKORD                                                   
                                                                                
         MVC   OD#ETY,ORDREXTY                                                  
         MVC   QETY,ORDREXTY                                                    
         MVC   QOFFICE,ORDROFF                                                  
                                                                                
         CLC   ORDREXTY,SPACES                                                  
         JH    SOKEY02                                                          
         MVC   OD#ETY,DEFEXPTY     (default expenditure type)                   
                                                                                
         USING ETYRECD,R3                                                       
SOKEY02  LA    R3,IOKEY                                                         
         XC    ETYKEY,ETYKEY                                                    
         MVI   ETYKTYP,ETYKTYPQ                                                 
         MVI   ETYKSUB,ETYKSUBQ                                                 
         MVC   ETYKCPY,CUXCPY                                                   
         MVC   ETYKCODE,OD#ETY                                                  
         MVC   IOKEYSAV,IOKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         CLC   IOKEYSAV(ETYKOFFC-ETYRECD),IOKEY                                 
         JNE   SOKEY12                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SOKEY12                                                          
                                                                                
         L     R3,AIO1                                                          
         LA    R3,ETYRFST                                                       
         USING NAMELD,R3                                                        
         XR    RE,RE                                                            
                                                                                
SOKEY04  CLI   NAMEL,NAMELQ                                                     
         JE    SOKEY08                                                          
         CLI   NAMEL,ENMELQ                                                     
         JE    SOKEY10                                                          
         CLI   NAMEL,0                                                          
         JE    SOKEY12                                                          
                                                                                
SOKEY06  IC    RE,NAMLN                                                         
         AR    R3,RE                                                            
         J     SOKEY04                                                          
                                                                                
SOKEY08  IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#ETYN(0),NAMEREC                                               
         J     SOKEY06                                                          
                                                                                
         USING ENMELD,R3                                                        
SOKEY10  XR    RE,RE                                                            
         IC    RE,ENMLN                                                         
         SHI   RE,ENMLNQ+1                                                      
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#ETYF(0),ENMNAME                                               
         J     SOKEY06                                                          
         DROP  R3                                                               
                                                                                
SOKEY12  DS    0H                                                               
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
*** Set order 'ORDELD' routine ***                                              
         USING ORDELD,R2                                                        
SOORD    NTR1                                                                   
                                                                                
         XR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        If rodick check we have                      
         CHI   RF,XPRODIKQ         security to view all orders                  
         JNE   SOORD01B                                                         
                                                                                
         GOTOR PIDISAP             Check if approver via PIDELs                 
         JE    SOORD01A            Is an approver                               
         CLC   ORDCPID,CCTPID      If not check if order created by me          
         JNE   SOORD01                                                          
         CLI   QORDOVL,YESQ        Do we have override limit list               
         JE    SOORD01A            Yes - set they can see everything            
         J     SOORD01B                                                         
                                                                                
SOORD01  CLI   QALL,YESQ           Security to view all orders?                 
         JE    SOORD01B                                                         
         MVC   LP_ERROR,=AL2(AE$NOSTO) otherwise error                          
         J     EXITN                                                            
*                                                                               
SOORD01A MVI   QORDSOVL,YESQ       Set override srch limit list                 
SOORD01B MVI   ORDTYPE,ARTOQ       determine order type                         
         CLI   ORDSUPL,STLDGQ                                                   
         JE    SOORD01C                                                         
         MVI   ORDTYPE,INTOQ                                                    
         CLI   ORDSUPL,SKLDGQ                                                   
         JE    SOORD01C                                                         
         CLI   ORDSUPL,SILDGQ                                                   
         JE    SOORD01C                                                         
         MVI   ORDTYPE,EXPOQ                                                    
         CLC   ORDACCU(2),PRODUL                                                
         JNE   SOORD01C                                                         
         MVI   ORDTYPE,PROOQ                                                    
                                                                                
SOORD01C XR    RF,RF               Aura: always skip artist orders              
         ICM   RF,B'0011',CUXPNUM                                               
         CHI   RF,XPRODIKQ                                                      
         JNE   SOORD02                                                          
                                                                                
         CLI   ORDTYPE,ARTOQ                                                    
         JE    SOORD01F                                                         
                                                                                
         CLI   ORDTYPE,EXPOQ       apply passed security                        
         JNE   SOORD01D                                                         
         CLI   QODIEXP,YESQ                                                     
         JNE   SOORD01F                                                         
                                                                                
SOORD01D CLI   ORDTYPE,INTOQ                                                    
         JNE   SOORD01E                                                         
         CLI   QODIINT,YESQ                                                     
         JNE   SOORD01F                                                         
                                                                                
SOORD01E CLI   ORDTYPE,PROOQ                                                    
         JNE   SOORD02                                                          
         CLI   QODIPRO,YESQ                                                     
         JNE   SOORD01F                                                         
         J     SOORD02                                                          
                                                                                
SOORD01F DS    0H                                                               
         MVC   LP_ERROR,=AL2(AE$INVOT)  ? AE$SECLK ?                            
         J     EXITN                                                            
                                                                                
SOORD02  MVC   OD#TYP,ORDTYPE                                                   
         MVC   QORDTYP,ORDTYPE                                                  
                                                                                
         MVI   OD#STAFF,NOQ                                                     
         MVI   OD#DEPT,NOQ                                                      
         MVI   OD#MILES,NOQ                                                     
         MVI   OD#JOBR,NOQ                                                      
         MVI   OD#PROR,NOQ                                                      
         MVC   OD#CSTG,SPACES                                                   
         TM    SDMODE,SDMODCQ      If copy mode don't copy status               
         JNZ   SOORD03             set as not sent                              
         TM    ORDSTAT,ORDSEBUY    Test BrandOcean order                        
         JZ    SOORD03             No - Skip                                    
*&&UK*&& CLI   ORDLN,ORDLN2Q                                                    
*&&UK*&& JL    *+10                                                             
*&&US*&& CLI   ORDLN,ORDLN3Q                                                    
*&&US*&& JL    *+10                                                             
         MVC   OD#GAPST,ORDGSTAT   Gap status                                   
*                                                                               
SOORD03  CLI   ORDTYPE,EXPOQ                                                    
         JNE   SOORD09                                                          
         USING ACTRECD,R3                                                       
         MVC   OD#EXP,ORDACCU      expense account values                       
         CLC   ORDACCA,SPACES                                                   
         JNH   SOORD10                                                          
         LA    R3,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,ORDEXP                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SOORD10                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SOORD10                                                          
         L     R3,AIO1                                                          
         LA    R3,ACTRFST                                                       
         USING NAMELD,R3                                                        
         XR    RE,RE                                                            
                                                                                
SOORD04  CLI   NAMEL,0                                                          
         JE    SOORD10                                                          
         CLI   NAMEL,NAMELQ                                                     
         JE    SOORD06                                                          
         CLI   NAMEL,XNMELQ                                                     
         JE    SOORD07                                                          
         CLI   NAMEL,RSTELQ                                                     
         JE    SOORD08                                                          
SOORD05  IC    RE,NAMLN                                                         
         AR    R3,RE                                                            
         J     SOORD04                                                          
                                                                                
SOORD06  LLC   RF,NAMLN                                                         
         SHI   RF,3                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   OD#EXPN(0),NAMEREC                                               
         J     SOORD05                                                          
         DROP  R3                                                               
                                                                                
         USING XNMELD,R3                                                        
SOORD07  LLC   RF,XNMSUBL                                                       
         SHI   RF,2                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   OD#EXPF(0),XNMSUBN                                               
         J     SOORD05                                                          
         DROP  R3                                                               
                                                                                
         USING RSTELD,R3                                                        
SOORD08  TM    RSTSTAT1,RSTSEADD   Dept analysis                                
         JZ    *+8                                                              
         MVI   OD#DEPT,YESQ                                                     
         TM    RSTSTAT1,RSTSGPEI   Staff analysis                               
         JZ    *+8                                                              
         MVI   OD#STAFF,YESQ                                                    
                                                                                
         MVC   OD#CSTG,RSTCOSTG                                                 
                                                                                
         CLI   RSTLN,RSTLN2Q                                                    
         JL    SOORD05                                                          
         TM    RSTSTAT2,RSTSMILE   TEST MILEAGE REQUIRED                        
         JZ    *+8                                                              
         MVI   OD#MILES,YESQ                                                    
                                                                                
         TM    RSTSTAT4,RSTSJREA   TEST JOB REQ'D FOR EXP ANALYSIS              
         JZ    *+8                                                              
         MVI   OD#JOBR,YESQ                                                     
                                                                                
         CLI   RSTLN,RSTLN3Q                                                    
         JL    SOORD05                                                          
         TM    RSTSTAT5,RSTSPREA   PRODUCT REQUIRED                             
         JZ    *+8                                                              
         MVI   OD#PROR,YESQ                                                     
         J     SOORD05                                                          
         DROP  R3                                                               
                                                                                
SOORD09  MVC   SJACCNT,ORDACCA     SJ ACCOUNT VALUES                            
         MVI   BYTE2,YESQ                                                       
         GOTOR SETSJA                                                           
*                                                                               
SOORD10  CLC   ORDSUPA,SPACES      Have we got an account                       
         JNH   *+10                No                                           
         MVC   OD#SUP,ORDSUPU      supplier values                              
         OC    OD#SUP,SPACES                                                    
         MVI   BYTE3,0                                                          
*                                  READ LEDGER FOR LEVELS                       
         USING LDGRECD,R3                                                       
         LA    R3,IOKEY            (XLDGR  XOPOS  XACCT                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY(3),ORDSUPC                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SOORD16                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SOORD16                                                          
         L     R3,AIO1                                                          
         LA    R3,LDGRFST                                                       
         USING ACLELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
SOORD12  CLI   ACLEL,0                                                          
         JE    SOORD20                                                          
         CLI   ACLEL,ACLELQ                                                     
         JE    SOORD13                                                          
         IC    R0,ACLLN                                                         
         AR    R3,R0                                                            
         J     SOORD12                                                          
                                                                                
SOORD13  CLI   ORDSUPL,STLDGQ      TEST ARTIST LEDGER                           
         JNE   SOORD14                                                          
         MVC   BYTE3,ACLELLVC      SAVE AGENT LEVLEN                            
         CLI   ACLELLVD,L'ACTKACT                                               
         JE    SOORD14                                                          
         MVC   BYTE3,ACLELLVB                                                   
         CLI   ACLELLVC,L'ACTKACT                                               
         JE    SOORD14                                                          
         MVC   BYTE3,ACLELLVA                                                   
                                                                                
SOORD14  LA    R3,ACLVALS          SAVE LVLS FOR POSS DUE DATE LOOKUP           
         USING ACLVALS,R3                                                       
         XC    SLLEVS,SLLEVS                                                    
         LA    R1,SLLEVS                                                        
SOORD15  CLI   ACLVLEN,12          TEST LOWEST LEVEL                            
         JE    SOORD16             SKIP, AS LOW-LVL A/C IS READ ANYWAY          
         MVC   0(L'ACLVLEN,R1),ACLVLEN                                          
         AHI   R1,1                                                             
         AHI   R3,L'ACLVALS                                                     
         J     SOORD15                                                          
         DROP  R3                                                               
                                                                                
SOORD16  CLC   ORDSUPA,SPACES                                                   
         JNH   SOORD54                                                          
                                                                                
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY            READ SUPPLIER                                
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,ORDSUPC                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SOORD30                                                          
         MVI   OD#SUPL,NOQ                                                      
         TM    ACTKSTAT,ACTSLOCK                                                
         JZ    *+8                                                              
         MVI   OD#SUPL,YESQ                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SOORD30                                                          
         L     R3,AIO1                                                          
         LA    R3,ACTRFST                                                       
         USING NAMELD,R3                                                        
         XC    FULL1,FULL1                                                      
         XC    FULL2,FULL2                                                      
                                                                                
SOORD18  CLI   NAMEL,0                                                          
         JE    SOORD36                                                          
         CLI   NAMEL,NAMELQ                                                     
         JE    SOORD22                                                          
         CLI   NAMEL,ASTELQ                                                     
         JE    SOORD23                                                          
         CLI   NAMEL,RATEDSCQ                                                   
         JE    SOORD26                                                          
         CLI   NAMEL,XNMELQ                                                     
         JE    SOORD27                                                          
         CLI   NAMEL,FFTELQ                                                     
         JE    SOORD28                                                          
         CLI   NAMEL,ADRELQ                                                     
         JE    SOORD32                                                          
         CLI   NAMEL,OATELQ                                                     
         JE    SOORD34                                                          
         CLI   NAMEL,DEXELQ                                                     
         JE    SOORD35                                                          
                                                                                
SOORD20  XR    R0,R0                                                            
         IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         J     SOORD18                                                          
                                                                                
SOORD22  XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#SUPN(0),NAMEREC                                               
         J     SOORD20                                                          
                                                                                
         USING ASTELD,R3                                                        
SOORD23  TM    ASTSTAT1,ASTISFOR                                                
         JZ    *+8                                                              
         MVI   OD#FNIU,C'S'                                                     
*&&US*&& J     SOORD20                                                          
*&&UK                                                                           
         TM    SDMODE,SDMODBQ+SDMODIQ  TEST INVOICES CALL                       
         JZ    SOORD20                                                          
         CLI   CUCTRY,CTRYGER      FOR GERMANY                                  
         JNE   SOORD20                                                          
         CLI   AST13VAT,C' '       TEST 1 VAT CODE SET ON SUPPLIER              
         JNH   *+10                                                             
         MVC   OD#IVTC(1),AST13VAT SET FIRST                                    
         CLI   ASTKSVTY,C' '       TEST KSV SET ON SUPPLIER                     
         JNH   SOORD20                                                          
         CLC   QIVDATE,SPACES      TEST INVOICE DATE PASSED                     
         JNH   SOORD24                                                          
         GOTOR VDATCON,DMCB,(0,QIVDATE+2),(1,TEMP2)                             
         LA    RF,WORK                                                          
         USING CONBLKD,RF                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONFLD,CONFKSV      KSV CALL                                     
         MVI   CONACTN,CONAGETQ    GET KSV DETAILS                              
         MVI   CONILEN,L'ASTKSVTY  LENGTH OF CODE                               
         LA    RE,ASTKSVTY                                                      
         STCM  RE,15,CONIADD       INPUT ADDRESS                                
         LA    RE,TEMP                                                          
         STCM  RE,15,CONOADD       OUTPUT ADDRESS                               
         MVC   CONCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   CONKSVDT,TEMP2      EFFECTIVE KSV DATE                           
         GOTOR VCONVERT,CONBLK                                                  
         JE    *+14                                                             
SOORD24  MVC   LP_ERROR,=AL2(AE$NOKSV)                                          
         J     EXITN                                                            
         DROP  RF                                                               
         ZAP   OD#IKSV,TEMP(3)        KSV %                                     
         J     SOORD20                                                          
*&&                                                                             
                                                                                
         USING RATELD,R3                                                        
SOORD26  SR    RF,RF                                                            
         ICM   RF,3,RATRATE                                                     
         CVD   RF,DUB                                                           
         ZAP   OD#IDSC,DUB                                                      
         J     SOORD20                                                          
                                                                                
         USING XNMELD,R3                                                        
SOORD27  XR    RE,RE                                                            
         IC    RE,XNMSUBL                                                       
         SHI   RE,2            for suppliers XNMSUBL includes l'itself          
         BASR  R1,0                      not so for other recs!                 
         EX    RE,4(R1)                                                         
         MVC   OD#SUPF(0),XNMSUBN                                               
         J     SOORD20                                                          
                                                                                
         USING FFTELD,R3                                                        
SOORD28  CLI   FFTTYPE,FFTTPFAX                                                 
         JNE   SOORD30                                                          
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#SFAX(0),FFTDATA                                               
         J     SOORD20                                                          
                                                                                
SOORD30  DS    0H                                                               
*&&UK                                                                           
         CLI   FFTTYPE,FFTTPEML                                                 
         JNE   SOORD31                                                          
*&&                                                                             
*&&US                                                                           
         CLI   FFTTYPE,FFTTBEML    BUSINESS EMAIL?                              
         JE    *+12                                                             
         CLI   FFTTYPE,FFTTEML                                                  
         JNE   SOORD20                                                          
*&&                                                                             
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#SEMA(0),FFTDATA                                               
         J     SOORD20                                                          
                                                                                
*&&UK                                                                           
SOORD31  CLI   FFTTYPE,FFTTG13B    GERMANY:13B VAT CODE(S)                      
         JE    *+12                                                             
         CLI   FFTTYPE,FFTTVATC    OR REGULAR VAT ?                             
         JNE   SOORD20                                                          
         LA    RF,OD#IVTC                                                       
         CLI   OD#IVTC,C' '        MAY BE ONE CODE (FROM ASTEL) ALREADY         
         JNH   *+8                                                              
         AHI   RF,1                SO APPEND OTHERS                             
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),FFTDATA     UP TO FOUR MORE                              
         J     SOORD20                                                          
*&&                                                                             
                                                                                
         USING ADRELD,R3                                                        
SOORD32  ST    R3,FULL1                                                         
         J     SOORD20                                                          
                                                                                
         USING OATELD,R3                                                        
SOORD34  CLI   OATSUB,OATSUB1Q                                                  
         JE    *+12                                                             
         CLI   OATSUB,OATSUB5Q                                                  
         JNE   SOORD20                                                          
         ST    R3,FULL2                                                         
         J     SOORD20                                                          
*                                  DUE DATE EL                                  
SOORD35  TM    SDMODE,SDMODBQ+SDMODIQ  TEST INVOICES CALL                       
         JZ    SOORD20                                                          
         BRAS  RE,GETDDF                                                        
         J     SOORD20                                                          
                                                                                
SOORD36  ICM   R3,15,FULL2                                                      
         JZ    SOORD37                                                          
*&&UK                                                                           
         LLC   R0,OATNUM                                                        
         LA    RF,OATADD1                                                       
         J     SOORD38                                                          
*&&                                                                             
*&&US                                                                           
         TM    OATSTAT,OATCSZ                                                   
         JZ    SOORD3A                                                          
         MVC   OD#SAD3,SPACES                                                   
         MVC   OD#SAD3(L'OATCITY),OATCITY                                       
         CLC   OD#SAD3,SPACES                                                   
         JNH   SOORD3A                                                          
         LA    R1,OD#SAD3+L'OD#SAD3                                             
         ST    R1,FULL1                                                         
         AHI   R1,-1               Back up 1 byte to get in the field           
         CLI   0(R1),C' '          Find last significant character              
         JH    *+12                                                             
         AHI   R1,-1                                                            
         J     *-12                                                             
         L     RE,FULL1                                                         
         CR    RE,R1                                                            
         JNH   SOORD3A                                                          
         MVI   1(R1),C','          put in comma                                 
         AHI   R1,3                Increase length to incl comma/space          
         CR    RE,R1                                                            
         JNH   SOORD3A                                                          
         SR    RE,R1               Do we have enough room for ST                
         CHI   RE,L'OATSTATE                                                    
         JL    SOORD3A                                                          
         MVC   0(L'OATSTATE,R1),OATSTATE                                        
         AHI   R1,L'OATSTATE+1                                                  
         L     RE,FULL1                                                         
         SR    RE,R1               Do we have enough room for ZIP               
         CHI   RE,L'OATZIP                                                      
         JL    SOORD3A                                                          
         MVC   0(L'OATZIP,R1),OATZIP                                            
         AHI   R1,L'OATZIP                                                      
         L     RE,FULL1                                                         
         SR    RE,R1               Do we have enough room for EXT               
         CHI   RE,L'OATZIPRN                                                    
         JL    SOORD3A                                                          
         MVC   0(L'OATZIPRN,R1),OATZIPRN                                        
*                                                                               
SOORD3A  LA    R1,2                                                             
         LA    RF,OATLINE1                                                      
         LA    RE,OD#SAD1                                                       
SOORD3B  MVC   0(L'OD#SAD1,RE),0(RF)                                            
         AHI   RE,L'OD#SAD1                                                     
         AHI   RF,L'OATLINE1                                                    
         JCT   R1,SOORD3B                                                       
         J     SOORD42                                                          
*&&                                                                             
         USING ADRELD,R3                                                        
SOORD37  ICM   R3,15,FULL1                                                      
         JZ    SOORD42                                                          
         LLC   R0,ADRNUM                                                        
         LA    RF,ADRADD1                                                       
                                                                                
SOORD38  LTR   R0,R0                                                            
         JZ    SOORD42                                                          
         LA    RE,OD#SAD1                                                       
                                                                                
SOORD40  BASR  R1,0                                                             
         MVC   0(L'ADRADD1,RE),0(RF)                                            
         AHI   RE,L'OD#SAD1                                                     
         AHI   RF,L'ADRADD1                                                     
         BCTR  R0,R1                                                            
                                                                                
SOORD42  CLI   BYTE3,0             IF ARTIST PUT OUT AGENT DETAILS              
         JNE   SOORD43                                                          
         TM    SDMODE,SDMODBQ+SDMODIQ ELSE TEST INVOICES                        
         JZ    SOORD54                                                          
         CLC   OD#DDXP,SPACES      TEST ALREADY FOUND DUE DATE FORMULA          
         JH    SOORD54                                                          
         USING ACTRECD,R3                                                       
SOORD43  LA    R3,IOKEY            SEARCH LOW TO HIGH LEVELS                    
         MVC   ACTKEY,SPACES                                                    
         LA    R1,SLLEVS+L'SLLEVS-1                                             
         LA    R0,L'SLLEVS                                                      
         BASR  RF,0                                                             
         CLI   0(R1),0                                                          
         JH    SOORD44                                                          
         AHI   R1,-1                                                            
         BCTR  R0,RF                                                            
         J     SOORD54             FINISHED                                     
SOORD44  MVC   ACTKCPY(3),ORDSUPC                                               
         XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         MVI   0(R1),0             CLEAR FOR NEXT TIME                          
         AHI   RF,-1                                                            
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   ACTKACT(0),ORDSUPA                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SOORD54                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SOORD54                                                          
         L     R3,AIO1                                                          
         LA    R3,ACTRFST                                                       
         USING ADRELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
SOORD46  CLI   ADREL,0                                                          
         JNE   SOORD47                                                          
         MVI   BYTE3,0                                                          
         TM    SDMODE,SDMODBQ+SDMODIQ  TEST INVOICES CALL                       
         JZ    SOORD54                                                          
         J     SOORD43                                                          
SOORD47  CLI   ADREL,DEXELQ        DUE DATE EL                                  
         JE    SOORD53                                                          
         CLI   BYTE3,0             ARE WE LOOKING FOR ARTIST AGENT              
         JE    SOORD48             NO                                           
         CLI   ADREL,ADRELQ        YES - GET ADDRESS AND NAME                   
         JE    SOORD50                                                          
         CLI   ADREL,NAMELQ                                                     
         JE    SOORD52                                                          
                                                                                
SOORD48  IC    R0,ADRLN                                                         
         AR    R3,R0                                                            
         J     SOORD46                                                          
                                                                                
SOORD50  IC    R0,ADRNUM                                                        
         LA    RF,ADRADD1                                                       
         LTR   R0,R0                                                            
         JZ    SOORD48                                                          
         LA    RE,OD#AGT1                                                       
                                                                                
         BASR  R1,0                                                             
         MVC   0(L'ADRADD1,RE),0(RF)                                            
         AHI   RE,L'OD#AGT1                                                     
         AHI   RF,L'ADRADD1                                                     
         BCTR  R0,R1                                                            
         J     SOORD48                                                          
                                                                                
         USING NAMELD,R3                                                        
SOORD52  XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#AGTN(0),NAMEREC                                               
         MVC   OD#AGTC,IOKEY+ACTKACT-ACTRECD                                    
         J     SOORD48                                                          
         DROP  R3                                                               
*                                  DUE DATE EL                                  
SOORD53  TM    SDMODE,SDMODBQ+SDMODIQ  TEST INVOICES CALL                       
         JZ    SOORD48                                                          
         BRAS  RE,GETDDF                                                        
         XC    SLLEVS,SLLEVS       CLEAR ANY REMAINING LEVELS                   
         J     SOORD48                                                          
                                                                                
SOORD54  MVC   OD#DTE,ORDDATE      other details                                
         MVC   OD#AUT,ORDAUTH                                                   
         MVI   OD#PRTA,NOQ                                                      
*&&UK*&& TM    ORDSTAT,ORDSPRTA                                                 
*&&US*&& TM    ORDSTAT3,ORDSPRTA                                                
         JZ    SOORD56                                                          
SOORD55  MVI   OD#PRTA,YESQ                                                     
                                                                                
SOORD56  MVI   OD#PRTT,NOQ                                                      
*&&UK*&& TM    ORDSTAT,ORDSPRTT                                                 
*&&US*&& TM    ORDSTAT3,ORDSPRTT                                                
         JZ    SOORD058                                                         
SOORD57  MVI   OD#PRTT,YESQ                                                     
*                                                                               
* Brandocean orders                                                             
*                                                                               
SOORD058 CLI   SORDEBY,YESQ        IS IT BRANDOCEAN ORDER                       
         JNE   SOORD064            NO                                           
         MVC   OD#RBYE,ORDRQBD     YES                                          
         MVC   OD#IDNO,ORDIDNO                                                  
         MVC   OD#RFMT,ORDRFMT                                                  
         OC    ORDOWNER,ORDOWNER                                                
         JZ    SOORD060                                                         
         MVC   TEMP2(2),ORDOWNER                                                
         GOTOR (#GETPID,AGETPID)                                                
         JNE   SOORD060                                                         
         MVC   OD#OWNC,TEMP2                                                    
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   SOORD060                                                         
         MVC   OD#OWNN,TEMP2                                                    
         MVC   OD#OWNL,WORK2                                                    
SOORD060 CLI   ORDLN,ORDLN4Q       TEST LONGER ELEMENT                          
         JL    SOORD062                                                         
         MVC   TEMP2(L'ORDCSUBM),ORDCSUBM                                       
         GOTOR (#GETPID,AGETPID)                                                
         JNE   SOORD062                                                         
         MVC   OD#SBPID,TEMP2                                                   
SOORD062 TM    ORDASTA,ORDSTAQ                                                  
         JZ    *+8                                                              
         MVI   OD#ASTA,C'A'                                                     
         TM    ORDASTA,ORDSTSQ                                                  
         JZ    SOORD066                                                         
         MVI   OD#ASTA,C'S'                                                     
         J     SOORD066                                                         
*                                                                               
* Old orders from presto and mf                                                 
*                                                                               
SOORD064 DS    0H                                                               
*&&UK*&& MVC   OD#RBYO,ORDREQD                                                  
*&&US*&& MVC   OD#RBYO,ORDDDTE                                                  
         TM    ORDSTAT,ORDSPRES    TEST PRESTO ORDER                            
         JZ    SOORD070                                                         
*&&UK                                                                           
         CLI   ORDLN,ORDLN4Q       TEST LONGER ELEMENT                          
         JL    SOORD070                                                         
         MVC   TEMP2,ORDCPIDP      PRESTO ORDER RAISER PID HERE                 
         J     *+10                                                             
*&&                                                                             
*                                                                               
* All orders                                                                    
*                                                                               
SOORD066 MVC   TEMP2(2),ORDCPID                                                 
         MVC   OD#RAPI,ORDCPID                                                  
         GOTOR (#GETPID,AGETPID)                                                
         JNE   SOORD070                                                         
         MVC   OD#RAIC,TEMP2                                                    
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   SOORD070                                                         
         MVC   OD#RAIN,TEMP2                                                    
         MVC   OD#RAIL,WORK2                                                    
         CLI   ORDEML,YESQ                                                      
         JNE   *+10                CHECK WHETHER TO SEND EMAILS                 
         MVC   OD#RAIE,APPEMAIL                                                 
         MVC   OD#RAIT,TEMP2+52                                                 
                                                                                
SOORD070 CLI   QDIMODE,C'C'        COPY call?                                   
         JE    SOORD092                                                         
                                                                                
         MVI   OD#STT,C'O'         =Order order                                 
         TM    ORDSTAT,ORDSPRES                                                 
         JZ    SOORD072                                                         
         MVI   OD#STT,C'P'         PRESTO order                                 
                                                                                
SOORD072 TM    ORDSTAT,ORDSHSE                                                  
         JZ    SOORD074                                                         
         MVI   OD#STT,C'H'         HOUSE order                                  
                                                                                
SOORD074 TM    ORDSTAT,ORDSEBUY                                                 
         JZ    SOORD076                                                         
         MVI   OD#STT,C'E'         eBuyer order                                 
                                                                                
SOORD076 MVI   OD#STT+1,C'D'                                                    
         TM    ORDSTAT,ORDSPART+ORDSMNUP   Pending fully matched will           
         JZ    SOORD078                     be seen as part invoiced            
         MVI   OD#STT+1,C'P'                                                    
                                                                                
         USING ORDRECD,R3                                                       
SOORD078 L     R3,AIO2                                                          
         TM    ORDRSTAT,ORDSFMCH   Is the the order closed/matched              
         JZ    SOORD080                                                         
         TM    ORDSTAT,ORDSPART+ORDSMNUP  But is it truely matched              
         JZ    SOORD080                                                         
         MVI   OD#STT+1,C'F'                                                    
*&&US                                                                           
         LA    R4,ORDRFST                                                       
         USING OAMELD,R4                                                        
SOORD079 CLI   OAMEL,0                                                          
         JE    SOORD080                                                         
         CLI   OAMEL,OAMELQ                                                     
         JE    SOORD07B                                                         
SOORD07A LLC   RE,OAMLN                                                         
         AR    R4,RE                                                            
         J     SOORD079                                                         
*                                                                               
SOORD07B OC    OAMIPND,OAMIPND                                                  
         JZ    SOORD07A                                                         
         MVI   OD#STT+1,C'P'                                                    
         DROP  R4                                                               
*&&                                                                             
         DROP  R3                                                               
                                                                                
SOORD080 MVI   OD#STT+2,C'N'                                                    
         TM    ORDSTAT2,ORDGDRCV                                                
         JZ    SOORD082                                                         
         MVI   OD#STT+2,C'Y'                                                    
                                                                                
SOORD082 MVI   OD#STT+3,C'A'       (default APPROVED on non eBuyer)             
         L     RE,=A(STATAB)                                                    
         A     RE,SRVRRELO                                                      
         MVC   SBYTE1,ORDSTAT2                                                  
         NI    SBYTE1,FF-(ORDGDRCV+ORDSEXEX+ORDSSTAT)                           
                                                                                
SOORD084 CLI   0(RE),FF                                                         
         JE    SOORD088                                                         
         CLC   SBYTE1,1(RE)                                                     
         JE    SOORD086                                                         
         AHI   RE,2                                                             
         J     SOORD084                                                         
                                                                                
SOORD086 MVC   OD#STT+3(1),0(RE)                                                
                                                                                
         USING ORDRECD,R3                                                       
SOORD088 L     R3,AIO2                                                          
         MVI   OD#STT+4,C'D'                                                    
         TM    ORDRSTAT,ORDCLOSE                                                
         JZ    SOORD090                                                         
         MVI   OD#STT+4,C'C'                                                    
                                                                                
SOORD090 TM    ORDRSTAT,ORDSLDEL                                                
         JZ    SOORD094                                                         
         MVI   OD#STT+4,C'L'                                                    
         J     SOORD094                                                         
         DROP  R3                                                               
                                                                                
SOORD092 MVC   OD#STT,=C'EDNDD'                                                 
                                                                                
SOORD094 TM    SDMODE,SDMODBQ+SDMODIQ  TEST CALLED FROM INVOICES                
         JZ    SOORD100                                                         
         TM    SDMODE,SDMODBQ      TEST BRANDOCEAN INVOICES                     
         JNZ   SOORD096            YES - FULLY-MATCHED IS OK                    
         CLI   OD#STT+1,C'F'       ELSE TEST FULLY-MATCHED                      
         JNE   *+14                                                             
         MVC   LP_ERROR,=AL2(AE$OFMCH)                                          
         J     EXITN                                                            
SOORD096 CLI   OD#STT,C'E'         TEST BRANDOCEAN                              
         JE    SOORD098            YES                                          
         CLI   OD#STT+4,C'D'       TEST OPEN                                    
         JE    SOORD100                                                         
         TM    SDMODE,SDMODBQ      TEST BRANDOCEAN INVOICES                     
         JZ    *+12                NO - ERROR                                   
         CLI   OD#STT+4,C'C'       YES - CLOSED IS OK                           
         JE    SOORD100                                                         
         MVC   LP_ERROR,=AL2(AE$ORDCD)   ORDER CLOSED/DELETED                   
         J     EXITN                                                            
SOORD098 CLI   OD#STT+2,C'Y'       TEST GOODS RECEIVED                          
         JE    SOORD100                                                         
         CLI   OD#STT+3,C'F'       OR FULLY APPROVED                            
         JE    SOORD100                                                         
         CLI   OD#STT+3,C'A'       OR FULLY APPROVED (NON-BR'O)                 
         JE    SOORD100                                                         
         MVC   LP_ERROR,=AL2(AE$OAOGR)   MUST BE APPROVED OR GOODS REC          
         J     EXITN                                                            
                                                                                
SOORD100 DS    0H                                                               
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
         USING PIDELD,R2                                                        
PIDISAP  NTR1                                                                   
         L     R2,AIO2                                                          
         AHI   R2,ORDRFST-ORDRECD                                               
                                                                                
PIDISAP1 CLI   PIDEL,0                                                          
         JE    PIDISAPN                                                         
         CLI   PIDEL,PIDELQ                                                     
         JNE   PIDISAP5                                                         
         CLI   PIDTYPE,PIDT1Q                                                   
         JL    PIDISAP5                                                         
         CLI   PIDTYPE,PIDT4Q                                                   
         JH    PIDISAP5                                                         
         LA    RE,PIDNTRS          list of approvals                            
         LLC   RF,PIDNTR#          number of entries                            
                                                                                
PIDISAP3 CLC   CCTPID,1(RE)        am I approver?                               
         JE    PIDISAPY                                                         
         AHI   RE,L'PIDNTRS        next list entry                              
         JCT   RF,PIDISAP3                                                      
                                                                                
PIDISAP5 LLC   R0,PIDLN                                                         
         AR    R2,R0                                                            
         J     PIDISAP1                                                         
                                                                                
PIDISAPY J     EXITY                                                            
PIDISAPN J     EXITN                                                            
         DROP  R2                                                               
                                                                                
*** TRANSLATE DUE DATE EXPRESSION ***                                           
         USING DEXELD,R3                                                        
GETDDF   ST    RE,SAVERE                                                        
         LA    RF,WORK                                                          
         USING CONBLKD,RF                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONACTN,CONATRAQ    TRANSLATE                                    
         MVI   CONFLD,CONFIDUE     DUE DATE EXPRESSION                          
         MVI   CONILEN,L'DEXVAL    LENGTH OF CODE                               
         LA    RE,DEXVAL                                                        
         STCM  RE,15,CONIADD       INPUT ADDRESS                                
         LA    RE,OD#DDXP                                                       
         STCM  RE,15,CONOADD       OUTPUT ADDRESS                               
         MVC   CONCOMF,ACOMFACS    A(COMFACS)                                   
         GOTOR VCONVERT,CONBLK                                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
*** Set order 'PIDELD' data routine ***                                         
         USING PIDELD,R2                                                        
SOPID    NTR1                                                                   
         XR    RE,RE                                                            
         IC    RE,OD#MAPP                                                       
         CLI   PIDNTRS,PIDAPPQ                                                  
         JE    SOPID05                                                          
         AHI   RE,1                                                             
                                                                                
SOPID05  CLI   PIDNTR#,1                                                        
         JE    SOPID10                                                          
         TM    PIDNTRS+L'PIDNTRS,PIDAPPQ                                        
         JNZ   SOPID10                                                          
         AHI   RE,1                                                             
                                                                                
SOPID10  TM    APIND1,APIUNA       Any unapproval from previous level           
         JNZ   SOPID16             Yes                                          
         CLC   CCTPID,PIDNTRS+1                                                 
         JNE   SOPID12                                                          
         MVI   OD#APSTA,OD#AAWAT                                                
         TM    PIDNTRS,PIDAPPQ                                                  
         JZ    *+8                                                              
         MVI   OD#APSTA,OD#AAPPR                                                
                                                                                
SOPID12  CLI   PIDNTR#,2                                                        
         JL    SOPID14                                                          
         CLC   CCTPID,PIDNTRS+L'PIDNTRS+1                                       
         JNE   SOPID14                                                          
         MVI   OD#APSTA,OD#AAWAT                                                
         TM    PIDNTRS+L'PIDNTRS,PIDAPPQ                                        
         JZ    *+8                                                              
         MVI   OD#APSTA,OD#AAPPR                                                
                                                                                
T        USING ORDPRFD,XL#ORPF                                                  
SOPID14  CLI   T.PROEBY04,YESQ       Sequential approval?                       
         JNE   SOPID16               No                                         
         DROP  T                                                                
         TM    PIDNTRS,PIDAPPQ                                                  
         JNZ   *+8                                                              
         OI    APIND1,APIUNA         Set unapproved                             
*                                                                               
         CLI   PIDNTR#,2                                                        
         JL    SOPID16                                                          
         TM    PIDNTRS+L'PIDNTRS,PIDAPPQ                                        
         JNZ   SOPID16                                                          
         OI    APIND1,APIUNA         Set unapproved                             
                                                                                
SOPID16  STC   RE,OD#MAPP                                                       
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
*** Set order 'FFTELD' data routine ***                                         
         USING FFTELD,R2                                                        
SOFFT    NTR1                                                                   
                                                                                
         CLI   FFTTYPE,FFTTOFFC                                                 
         JNE   SOFFT05                                                          
         MVC   OD#OFF,FFTDATA                                                   
         J     SOFFTX                                                           
                                                                                
SOFFT05  CLI   FFTTYPE,FFTTSATN                                                 
         JNE   SOFFT10                                                          
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#ATO(0),FFTDATA                                                
         J     SOFFTX                                                           
                                                                                
SOFFT10  CLI   FFTTYPE,FFTTESTN                                                 
         JNE   SOFFT20                                                          
         LA    RF,FFTESTN                                                       
         CLI   FFTLN,FFTESLNQ      Any original estimate number?                
         JL    SOFFT12             No - OK                                      
         CLC   FFTOESTN,SPACES                                                  
         JNH   SOFFT12                                                          
         LA    RF,FFTOESTN         global estimate number                       
*                                                                               
SOFFT12  MVC   OD#ESTN,0(RF)                                                    
         USING EGNPASD,R3                                                       
         LA    R3,IOKEY            build passive                                
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,CUXCPY                                                   
         MVC   EGNPNUM,OD#ESTN                                                  
         MVC   SAVEKEY1,EGNPAS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   SOFFTX                                                           
         CLC   EGNPAS(EGNPCLI-EGNPASD),SAVEKEY1                                 
         JNE   SOFFTX                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SOFFTX                                                           
         L     R3,AIO1                                                          
         AHI   R3,ESTRFST-ESTRECD                                               
         USING ENMELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
SOFFT15  IC    R0,ENMLN                                                         
         AR    R3,R0                                                            
         CLI   ENMEL,0                                                          
         JE    SOFFTX                                                           
         CLI   ENMEL,ENMELQ                                                     
         JNE   SOFFT15                                                          
         XR    RE,RE                                                            
         IC    RE,ENMLN                                                         
         SHI   RE,ENMLNQ+1                                                      
         LTR   RE,RE                                                            
         JM    SOFFTX                                                           
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#ENAM(0),ENMNAME                                               
         J     SOFFTX                                                           
         DROP  R3                                                               
                                                                                
SOFFT20  CLI   FFTTYPE,FFTTORNO                                                 
         JNE   SOFFT25                                                          
         MVC   OD#REQ,FFTDATA                                                   
         J     SOFFTX                                                           
                                                                                
SOFFT25  DS    0H                                                               
*&&US*&& J     SOFFT40                                                          
*&&UK                                                                           
         CLI   FFTTYPE,FFTTWRKC                                                 
         JNE   SOFFT40                                                          
         MVC   OD#EOWC,FFTWORK                                                  
*        MVC   QWC,FFTWORK                                                      
         GOTOR LP_AAWMP,DMCB,(L'WCOKWRK,FFTWORK),QWCIND,QWCMAXQ,       +        
               LP_D                                                             
         MVC   TEMP2(L'OD#EOWC),OD#EOWC                                         
         GOTOR (#GETWCD,AGETWCD)                                                
         MVC   OD#EOWD,TEMP2                                                    
         MVC   OD#EOWF,SPACES                                                   
         MVC   OD#EOWX,SPACES                                                   
         L     R1,AIO3                                                          
         AHI   R1,WCORFST-WCORECD                                               
         USING XNMELD,R1                                                        
         XR    R0,R0                                                            
                                                                                
SOFFT30  CLI   XNMEL,XNMELQ                                                     
         JE    SOFFT34                                                          
         CLI   XNMEL,NAMELQ                                                     
         JE    SOFFT36                                                          
         CLI   XNMEL,0                                                          
         JE    SOFFTX                                                           
SOFFT32  IC    R0,XNMLN                                                         
         AR    R1,R0                                                            
         J     SOFFT30                                                          
                                                                                
SOFFT34  LLC   RE,XNMSUBL                                                       
         SHI   RE,2                                                             
         LTR   RE,RE                                                            
         JM    SOFFT32                                                          
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   OD#EOWF(0),XNMSUBN                                               
         J     SOFFT32                                                          
                                                                                
         USING NAMELD,R1                                                        
SOFFT36  XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   OD#EOWX(0),NAMEREC                                               
         J     SOFFT32                                                          
*&&                                                                             
                                                                                
         USING FFTELD,R2                                                        
SOFFT40  DS    0H                  GAP email address                            
*&&UK*&& CLI   FFTTYPE,FFTTPEML                                                 
*&&US*&& CLI   FFTTYPE,FFTTEML                                                  
         JNE   SOFFT44                                                          
         TM    SDMODE,SDMODCQ      IF COPY MODE DON'T COPY EMAIL ADDR           
         JNZ   SOFFT44                                                          
         LA    R4,OD#GAPEM                                                      
*                                                                               
SOFFT41  MVC   0(L'OD#GAPEM,R4),FFTEML1                                         
         AHI   R4,L'OD#GAPEM                                                    
         CLI   FFTLN,FFEM1LL                                                    
         JNH   SOFFTX                                                           
         MVC   0(L'OD#GAPEM,R4),FFTEML2                                         
         AHI   R4,L'OD#GAPEM                                                    
         CLI   FFTLN,FFEM2LL                                                    
         JNH   SOFFTX                                                           
         MVC   0(L'OD#GAPEM,R4),FFTEML3                                         
         AHI   R4,L'OD#GAPEM                                                    
         CLI   FFTLN,FFEM3LL                                                    
         JNH   SOFFTX                                                           
         MVC   0(L'OD#GAPEM,R4),FFTEML4                                         
         AHI   R4,L'OD#GAPEM                                                    
         CLI   FFTLN,FFEM4LL                                                    
         JNH   SOFFTX                                                           
         MVC   0(L'OD#GAPEM,R4),FFTEML5                                         
         AHI   R4,L'OD#GAPEM                                                    
*                                                                               
SOFFT44  DS    0H                                                               
SOFFTX   J     EXITY                                                            
         DROP  R2                                                               
                                                                                
*** Set order 'OAMELD' data routine ***                                         
         USING OAMELD,R2                                                        
SOOAM    NTR1                                                                   
                                                                                
         XR    RF,RF               (DSRD-6318: for Aura count real w/cs         
         ICM   RF,3,CUXPNUM        only)                                        
         CHI   RF,XPRODIKQ                                                      
         JNE   SOOAM2                                                           
         CLC   OAMWORK,SPACES                                                   
         JNH   SOOAM4                                                           
                                                                                
SOOAM2   LLC   RE,OD#WC#                                                        
         AHI   RE,1                                                             
         STC   RE,OD#WC#                                                        
                                                                                
SOOAM4   AP    OD#TOT,OAMAMNT                                                   
         TM    SDMODE,SDMODCQ      IF COPY MODE DON'T COPY INVOICED             
         JNZ   *+16                                     AMOUNTS                 
         AP    OD#ITOT,OAMIVAL                                                  
*&&UK*&& ZAP   OD#INUM,OAMINUM     N'PRIOR INVS NOT ACCUMULATED                 
*&&US                                                                           
         CP    OD#INUM,OAMINUM    TO DEAL WITH HOW US INPUT UPDATES             
         JNL   *+10                              ORDERS                         
         ZAP   OD#INUM,OAMINUM                                                  
         CP    OD#HINUM,OAMINUM                                                 
         JNL   *+10                                                             
         ZAP   OD#HINUM,OAMINUM                                                 
         CLC   OD#HIPND,OAMIPND                                                 
         JNL   *+10                                                             
         MVC   OD#HIPND,OAMIPND                                                 
*&&                                                                             
         CLI   OAMLN,OAMLN3Q                                                    
         JL    EXITY                                                            
         AP    OD#TOTC,OAMFCAMT                                                 
         TM    SDMODE,SDMODCQ                                                   
         JNZ   EXITY                                                            
         AP    OD#ITOTC,OAMFCIVL                                                
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
*** Set order 'OAMELD' data routine ***                                         
                                                                                
SETOAM   NTR1  ,                                                                
                                                                                
         L     R2,AIO2                                                          
         AHI   R2,ORDRFST-ORDRECD                                               
         USING OAMELD,R2                                                        
SETOAM02 CLI   OAMEL,0                                                          
         JE    EXITY                                                            
         CLI   OAMEL,OAMELQ                                                     
         JE    SETOAM06                                                         
SETOAM04 LLC   RF,OAMLN                                                         
         AR    R2,RF                                                            
         J     SETOAM02                                                         
                                                                                
SETOAM06 MVC   OAMIPND,OD#HIPND                                                 
         ZAP   OAMINUM,OD#HINUM                                                 
         J     SETOAM04                                                         
         DROP  R2                                                               
                                                                                
*** Set order 'SCMELD' data routine ***                                         
         USING SCMELD,R2                                                        
SOSCM    NTR1                                                                   
                                                                                
         LA    R1,OD#TXTM                                                       
         CLI   SCMTYPE,SCMTOMOC                                                 
         JE    SOSCM05                                                          
         LA    R1,OD#TXTP                                                       
         CLI   SCMTYPE,SCMTSTND                                                 
         JNE   EXITY                                                            
                                                                                
SOSCM05  LLC   RE,SCMLN                                                         
         SHI   RE,SCMLN1Q+1                                                     
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   0(0,R1),SCMNARR                                                  
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
*** Set order 'AFCELD' data routine ***                                         
         USING AFCELD,R2                                                        
SOAFC    NTR1                                                                   
                                                                                
         MVC   OD#CUR,AFCCURR                                                   
         MVC   OD#RAT,AFCX                                                      
         LLC   RF,AFCXSHFT                                                      
         AHI   RF,2                                                             
         STC   RF,OD#FCDP                                                       
         TM    SDMODE,SDMODFQ                                                   
         JZ    SOAFC02                                                          
         MVC   OD#CUR,SCOPCUR                                                   
         MVC   OD#RAT,SCOPEXR                                                   
         LLC   RF,SCOSHFT                                                       
         AHI   RF,2                                                             
         STC   RF,OD#FCDP                                                       
SOAFC02  GOTO1 VHEXOUT,DMCB,OD#RAT,OD#RATC,L'OD#RAT                             
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
*** Set order 'SPAELD' data routine ***                                         
         USING SPAELD,R2                                                        
SOSPA    NTR1                                                                   
                                                                                
         CLI   SPATYPE,SPATDEPT                                                 
         JNE   SOSPA20                                                          
         MVC   OD#DEP,SPAAACT                                                   
         MVC   TEMP2(14),SPACES                                                 
         MVC   TEMP2(2),=C'2D'                                                  
         MVC   TEMP2+2(12),SPAAACT                                              
         GOTOR (#GETACN,AGETACN)                                                
         MVC   OD#DEPN,TEMP2                                                    
         L     R1,AIO3                                                          
         AHI   R1,ACTRFST-ACTRECD                                               
         XR    R0,R0                                                            
                                                                                
         USING XNMELD,R1                                                        
SOSPA05  CLI   XNMEL,0                                                          
         JE    EXITY                                                            
         CLI   XNMEL,XNMELQ                                                     
         JE    SOSPA10                                                          
         IC    R0,XNMLN                                                         
         AR    R1,R0                                                            
         J     SOSPA05                                                          
                                                                                
SOSPA10  LLC   RE,XNMSUBL                                                       
         SHI   RE,2                                                             
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   OD#DEPF(0),XNMSUBN                                               
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
SOSPA20  CLI   SPATYPE,SPATPERS                                                 
         JNE   EXITY                                                            
         MVC   OD#PER,SPAAACT                                                   
         MVC   TEMP2(14),SPACES                                                 
         MVC   TEMP2(2),=C'2P'                                                  
         MVC   TEMP2+2(12),SPAAACT                                              
         GOTOR (#GETACN,AGETACN)                                                
         MVC   OD#PERN,TEMP2                                                    
         L     R1,AIO3                                                          
         AHI   R1,ACTRFST-ACTRECD                                               
         XR    R0,R0                                                            
                                                                                
         USING XNMELD,R1                                                        
SOSPA25  CLI   XNMEL,0                                                          
         JE    EXITY                                                            
         CLI   XNMEL,XNMELQ                                                     
         JE    SOSPA30                                                          
         IC    R0,XNMLN                                                         
         AR    R1,R0                                                            
         J     SOSPA25                                                          
                                                                                
SOSPA30  LLC   RE,XNMSUBL                                                       
         SHI   RE,2                                                             
         BASR  RF,0                                                             
         EX    RE,4(RF)                                                         
         MVC   OD#PERF(0),XNMSUBN                                               
         J     EXITY                                                            
         DROP  R1,R2                                                            
                                                                                
*** Set order 'SORELD' data routine ***                                         
         USING SORELD,R2                                                        
SOSOR    NTR1                                                                   
                                                                                
         CLI   SORSYS,SORSACC                                                   
         JNE   EXITY                                                            
         MVC   SJACCNT,SORAACT                                                  
         MVI   BYTE2,NOQ                                                        
         GOTOR SETSJA                                                           
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
*** Set order 'ENMELD' data routine ***                                         
         USING ENMELD,R2                                                        
SOENM    NTR1                                                                   
                                                                                
         XR    RF,RF                                                            
         IC    RF,ENMLN                                                         
         SHI   RF,ENMLNQ+1                                                      
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   OD#ONAM(0),ENMNAME                                               
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
*** Set order 'OATELD' data routine ***                                         
         USING OATELD,R2                                                        
SOOAT    NTR1                                                                   
                                                                                
         CLI   OATSUB,OATSUB5Q                                                  
         JNE   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,OATNUM                                                        
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   OD#ODAD(0),OATDABOX                                              
         J     EXITY                                                            
         DROP  R2                                                               
** Set order 'GDAELD' data routine ***                                          
         USING GDAELD,R2                                                        
SOGAD    NTR1                                                                   
*                                                                               
         TM    SDMODE,SDMODCQ      If copy mode don't copy dates                
         JNZ   SOGADX                                                           
         CLI   GDATYPE,GDAGAPEX    Gap expiry/ sent date                        
         JNE   SOGAD02                                                          
         LA    R4,OD#GAPED                                                      
         J     SOGAD04                                                          
                                                                                
SOGAD02  CLI   GDATYPE,GDAGAPST                                                 
         JNE   SOGAD06                                                          
         LA    R4,OD#GAPSD                                                      
                                                                                
SOGAD04  MVC   0(L'GDADATE,R4),GDADATE                                          
         J     SOGADX                                                           
                                                                                
SOGAD06  DS    0H                                                               
                                                                                
SOGADX   J     EXITY                                                            
         EJECT                                                                  
*                                                                               
** Set order 'STCELD' data routine ***                                          
         USING STCELD,R2                                                        
SOSTC    NTR1                                                                   
         MVC   OD#RJCOM,SPACES                                                  
         MVC   OD#RJFNM,SPACES                                                  
         MVC   OD#RJMNM,SPACES                                                  
         MVC   OD#RJLNM,SPACES                                                  
         XC    OD#RJDAT,OD#RJDAT                                                
         MVC   OD#RJTIM,SPACES                                                  
*                                                                               
         LLC   RF,STCLN                                                         
         SHI   RF,STCOLN3Q+1                                                    
         LTR   RF,RF                                                            
         JM    SOSTC02                                                          
         BASR  R1,0                                                             
         MVC   OD#RJCOM(0),STCOCOM                                              
         EX    RF,0(R1)                                                         
*                                                                               
SOSTC02  MVC   TEMP2(L'STCPERS),STCOPID                                         
         GOTOR (#GETPID,AGETPID)                                                
         GOTOR (#GETPIN,AGETPIN)                                                
         MVC   OD#RJFNM,TEMP2                                                   
         MVC   OD#RJMNM,TEMP2+32                                                
         MVC   OD#RJLNM,WORK2                                                   
         MVC   OD#RJDAT,STCODTE                                                 
         UNPK  DUB2,STCOTIM                                                     
         OI    DUB2+7,C'0'                                                      
         MVC   OD#RJTIM,DUB2+2                                                  
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
* Get TUP profile and 'translate' filter out based on GODITSB and               
* GODITSN and GODITSR (must be called after getopt call)                        
GETTUP   NTR1                                                                   
                                                                                
         USING COBLOCKD,R2                                                      
         USING GOXBLKD,R4                                                       
         L     R2,ACOBLOCK         Get cost allocation profiles                 
         L     R4,AGOXBLCK                                                      
         GOTOR (#CSTPRF,ACSTPRF),DMCB,SPACES                                    
                                                                                
         LA    R1,RTIMTUPT                                                      
*NEC*    LA    R0,TMSTNMN                                                       
         LA    R0,TMSTNMB                                                       
*NEC*    LA    R3,GODITSR                                                       
         LA    R3,GOETMR                                                        
                                                                                
         USING COGOTABD,R4                                                      
GETTUP2  LARL  R4,COGOTAB                                                       
                                                                                
GETTUP4  CLC   COGOCAP,COTUP       Find match on COTUP                          
         JNE   GETTUP6             Get next entry in table if no match          
         CLC   COGOGOPT,0(R3)      Find match on getopt setting                 
         JE    GETTUP8                                                          
GETTUP6  AHI   R4,COGOTABL                                                      
         J     GETTUP4                                                          
                                                                                
GETTUP8  MVC   0(TMSTTABL,R1),COGOMPTY Set whether to read passives             
         AHI   R3,1                                                             
         AHI   R1,TMSTTABL         Point R1 to next type of time                
         JCT   R0,GETTUP2                                                       
                                                                                
GETTUPX  J     EXITY                                                            
         DROP  R2,R4                                                            
                                                                                
COGOTAB  DS    0H                                                               
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
                                                                                
*** Check estimate checks applicable for work code ***                          
CHKWCEC  NTR1                                                                   
                                                                                
*&&US*&& J     EXITY               UK only                                      
                                                                                
*&&UK                                                                           
         MVC   SWCODE,0(R1)        R1=A(WORKCODE)                               
         L     R3,AIO8                                                          
         LHI   R0,IOLENQ/(2+1)                                                  
                                                                                
CHKWC02  CLI   0(R3),0             EoB?                                         
         JE    CHKWC06                                                          
         CLC   SWCODE,0(R3)        Match?                                       
         JE    CHKWC04                                                          
         AHI   R3,2+1                                                           
         JCT   R0,CHKWC02          Max reached - start again                    
         L     R3,AIO8                                                          
         J     CHKWC06                                                          
                                                                                
CHKWC04  TM    2(R3),WCOSESCK                                                   
         JNZ   CHKWCECL                                                         
         J     CHKWCECY                                                         
                                                                                
         USING WCORECD,R2                                                       
CHKWC06  MVC   CSVKEY3,IOKEY       save key                                     
         LA    R2,IOKEY                                                         
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUXCPY                                                   
         MVC   WCOKUNT(L'PRODUL),PRODUL                                         
         MVC   WCOKWRK,SWCODE                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO7'                               
         JE    CHKWC07                                                          
         MVC   IOKEY,CSVKEY3                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1' RE-ESTABLISH SEQUENCE         
         J     CHKWCECL            but exclude if not found                     
                                                                                
CHKWC07  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO7'                              
         JNE   *+2                                                              
                                                                                
         MVC   IOKEY,CSVKEY3                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1' RE-ESTABLISH SEQUENCE         
                                                                                
         L     R2,AIO7                                                          
         LA    R1,WCORFST                                                       
         USING WCOELD,R1                                                        
                                                                                
CHKWC08  CLI   WCOEL,WCOELQ        Look for WCO element                         
         JE    CHKWC10                                                          
         CLI   WCOEL,0                                                          
         JE    *+2                                                              
         LLC   R0,WCOLN                                                         
         AR    R1,R0                                                            
         J     CHKWC08                                                          
                                                                                
CHKWC10  MVC   0(2,R3),SWCODE                                                   
         MVC   2(1,R3),WCOSTAT3                                                 
         MVI   (2+1)(R3),0                                                      
                                                                                
         TM    WCOSTAT3,WCOSESCK                                                
         JNZ   CHKWCECL                                                         
                                                                                
CHKWCECY J     EXITY                                                            
CHKWCECL J     EXITL                                                            
         DROP  R1,R2                                                            
*&&                                                                             
                                                                                
*** Uncommitted amount - do EVERECs/ESTRECs via Jobber ***                      
DOEST    NTR1                                                                   
                                                                                
         XC    CESTNUM,CESTNUM                                                  
         MVI   CESTLOC,0                                                        
         ZAP   XDOEVE,PZERO                                                     
         ZAP   XDOEST,PZERO                                                     
                                                                                
         L     RF,AGOXBLCK          SINGLE ESTIMATE CHECKING?                   
         CLI   GOECE-GOXBLOCK(RF),GOECSWC                                       
         JE    DOEST02                                                          
         CLI   GOECE-GOXBLOCK(RF),GOECSTA                                       
         JNE   DOEST06                                                          
                                                                                
         USING EGNPASD,R1                                                       
DOEST02  LA    R1,IOKEY                                                         
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,CUXCPY                                                   
         MVC   EGNPNUM,OD#ESTN                                                  
                                                                                
DOEST04  MVC   CESTNUM,EGNPNUM                                                  
         MVC   CSVKEY2,IOKEY                                                    
         CLC   EGNPNUM,SPACES      Do we have an estimate number                
         JNH   DOESTN                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   DOESTN                                                           
         LA    R1,IOKEY                                                         
         CLC   EGNPAS(EGNPCLI-EGNPASD),CSVKEY2                                  
         JNE   DOESTN                                                           
         MVC   CESTLOC,EGNPLNO                                                  
* may add code to check OD#CLI/PRO/JOB against EGNPCLI/PRO/JOB                  
         DROP  R1                                                               
                                                                                
DOEST06  LA    R2,DMCB             emulate saved job record                     
         L     RF,AACCFACS                                                      
         L     RF,X_AACCEMU-X_ACCFACSD(RF)                                      
         GOTO1 (RF),DMCB,=C'NEWO',,AIO6,AIO6                                    
                                                                                
*&&UK*&& LAY   RF,JOBFLDS                                                       
*&&UK*&& GOTO1 VJOBCOL,DMCB,(X'FF',(RF)),ACOLIST,ACOMFACS                       
*&&US*&& LAY   RF,LOOKFLDH                                                      
*&&US*&& GOTO1 VJOBCOL,DMCB,(RF),ACOLIST,ACOMFACS                               
         CLI   4(R1),0                                                          
*&&UK*&& JE    DOEST08                                                          
*&&US*&& JNE   DOEST08                                                          
         DC    H'0'                                                             
                                                                                
         USING JBLOCKD,R3                                                       
DOEST08  L     R3,AJOBLOCK                                                      
         LR    R0,R3                                                            
         LHI   R1,JBLOCKL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   JBAJOB,AIO6                                                      
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
*&&UK*&& MVC   JBCSHVAL,VCASHVAL                                                
*&&UK*&& MVC   JBTOBACO,VTOBACCO                                                
         MVC   JBAGOBLK,AGOBLOCB                                                
         MVC   JBAIO,AIO1                                                       
         MVC   JBGETOPT,VGETOPT                                                 
         MVC   JBAOPVTB,AELEAREA   Use ELEAREA here                             
         LHI   RE,L'ELEAREA                                                     
         ST    RE,JBLOPVTB                                                      
         MVC   JBACOLTB,AFREEST    Uses FREESTOR+GENAETXN                       
                                                                                
         LHI   RE,L'FREESTOR+L'GENAEXTN                                         
         ST    RE,JBLCOLTB                                                      
         MVI   JBSELFUN,JBGETDE    MCS: GET DETAILS AND SET ORIGINAL            
*&&UK*&& LAY   RE,JOBFLDS               COLUMN LIST  ADDRESS (OE/CE)            
*&&US*&& LAY   RE,LOOKFLDH              COLUMN LIST  ADDRESS (OE/CE)            
         ST    RE,JBORICLI                                                      
         CLI   CESTLOC,0                                                        
         JE    DOEST10                                                          
         OI    JBSELFUN,JBGETSS                                                 
         MVC   JBSELEST,CESTLOC                                                 
                                                                                
DOEST10  GOTO1 VJOBBER,DMCB,AJOBLOCK                                            
         CLI   JBERROR,0                                                        
         JNE   *+2                                                              
                                                                                
         TM    GUCAIND,GUCAHRQ     HR required?                                 
         JZ    DOEST12                                                          
         CLC   JBHIREV,JBCURVER    HR existing?                                 
         JE    DOEST12                                                          
         CLI   JBHIREV,0                                                        
         JE    DOEST12                                                          
         OI    GUCAIND,GUCAUHQ     use it                                       
                                                                                
DOEST12  CLI   JBNEWEST,JBMCSQ     MCS estimates?                               
         JE    DOEST30                                                          
                                                                                
         USING JBCOLD,R4                                                        
         L     R4,JBACOLTB                                                      
         XR    R2,R2                                                            
         ICM   R2,B'0011',JBNROWS                                               
         JZ    DOEST46                                                          
                                                                                
DOEST14  CLI   JBCOLTYP,JBCOLTWC   test for wc entry                            
         JNE   DOEST28                                                          
                                                                                
         L     RF,AGOXBLCK                                                      
         CLI   GOWCF-GOXBLOCK(RF),YESQ                                          
         JNE   DOEST16                                                          
         GOTOR CHKWCEC,JBCOLWC                                                  
         JL    DOEST28                                                          
                                                                                
DOEST16  ZAP   DUB2,JBCOLVAL+6(6)  CE                                           
         TM    GUCAIND,GUCAUHQ                                                  
         JZ    DOEST18                                                          
         ZAP   DUB2,JBCOLVAL+12(6) HR                                           
DOEST18  AP    XDOEVE,DUB2                                                      
         ZAP   MYPL16,DUB2                                                      
         CLI   QEUCONT,YESQ        Add contingency                              
         JNE   DOEST20             No                                           
         L     RF,AGOBLOCB         Yes                                          
         MP    MYPL16,GOOVRPER-GOBLOCKD(L'GOOVRPER,RF)                          
         SRP   MYPL16,64-4,5       Calculate current est*maxper                 
         AP    XDOEVE,MYPL16       and reduce by '100%'                         
         SP    XDOEVE,DUB2                                                      
***      SP    MYPL16,DUB2                                                      
                                                                                
DOEST20  CLI   QEUBYWC,YESQ        w/c break down?                              
         JNE   DOEST28                                                          
         USING UCBYWCD,R1                                                       
         L     R1,AIO4                                                          
DOEST22  CLI   UCBYWCWC,0                                                       
         JE    DOEST26                                                          
         CLC   UCBYWCWC,JBCOLWC                                                 
         JE    DOEST24                                                          
         AHI   R1,UCBYWCLQ                                                      
         J     DOEST22                                                          
DOEST24  AP    UCBYWCAM,MYPL16                                                  
         J     DOEST28                                                          
DOEST26  MVC   UCBYWCWC,JBCOLWC                                                 
         ZAP   UCBYWCAM,MYPL16                                                  
         XC    UCBYWCWC+UCBYWCLQ(L'UCBYWCWC),UCBYWCWC+UCBYWCLQ                  
         J     DOEST28                                                          
         DROP  R1                                                               
                                                                                
DOEST28  AH    R4,JBLCOL                                                        
         JCT   R2,DOEST14                                                       
         J     DOEST46                                                          
         DROP  R4                                                               
                                                                                
         USING MJETABD,R4                                                       
DOEST30  L     R4,JBACOLTB         1st entry is total (MJETTYP=MJETTTQ)         
         L     RF,AGOXBLCK                                                      
         CLI   QEUBYWC,YESQ        w/c level break up?                          
         JE    DOEST34             No                                           
         CLI   QEUCONT,YESQ        adding contingency?                          
         JE    DOEST34             No                                           
         CLI   GOWCF-GOXBLOCK(RF),YESQ                                          
         JE    DOEST34                                                          
         ZAP   DUB2,MJETVAL+6(6)                                                
         TM    GUCAIND,GUCAUHQ                                                  
         JZ    DOEST32                                                          
         ZAP   DUB2,MJETVAL+12(6)                                               
DOEST32  AP    XDOEST,DUB2                                                      
         J     DOEST46                                                          
                                                                                
DOEST34  LLC   R0,MJETLEN          first entry is totals                        
         AR    R4,R0                                                            
         CLI   MJETTYP,MJETTEQ     end of table?                                
         JE    DOEST46                                                          
         CLI   MJETTYP,MJETTWQ     look for work code entries                   
         JNE   DOEST34                                                          
         OC    MJET1RA,MJET1RA     ...but not 1R-lvl entries                    
         JNZ   DOEST34                                                          
                                                                                
         GOTOR CHKWCEC,MJETWCD                                                  
         JL    DOEST34                                                          
         ZAP   DUB2,MJETVAL+6(6)   CE                                           
         TM    GUCAIND,GUCAUHQ                                                  
         JZ    DOEST36                                                          
         ZAP   DUB2,MJETVAL+12(6)  HR                                           
DOEST36  AP    XDOEST,DUB2                                                      
         ZAP   MYPL16,DUB2                                                      
         CLI   QEUCONT,YESQ        Add contingency                              
         JNE   DOEST38             No                                           
         L     RF,AGOBLOCB         Yes                                          
         MP    MYPL16,GOOVRPER-GOBLOCKD(L'GOOVRPER,RF)                          
         SRP   MYPL16,64-4,5       Calculate current est*maxper                 
         AP    XDOEST,MYPL16       and reduce by '100%'                         
         SP    XDOEST,DUB2                                                      
***      SP    MYPL16,DUB2                                                      
                                                                                
DOEST38  CLI   QEUBYWC,YESQ        w/c break down?                              
         JNE   DOEST34                                                          
         USING UCBYWCD,R1                                                       
         L     R1,AIO4                                                          
DOEST40  CLI   UCBYWCWC,0                                                       
         JE    DOEST44                                                          
         CLC   UCBYWCWC,MJETWCD                                                 
         JE    DOEST42                                                          
         AHI   R1,UCBYWCLQ                                                      
         J     DOEST40                                                          
DOEST42  DS    0H                                                               
         AP    UCBYWCAM,MYPL16                                                  
         J     DOEST34                                                          
DOEST44  MVC   UCBYWCWC,MJETWCD                                                 
         ZAP   UCBYWCAM,MYPL16                                                  
         XC    UCBYWCWC+UCBYWCLQ(L'UCBYWCWC),UCBYWCWC+UCBYWCLQ                  
         J     DOEST34                                                          
         DROP  R1,R3,R4                                                         
                                                                                
DOEST46  DS    0H                                                               
                                                                                
DOESTX   J     EXITY                                                            
                                                                                
DOESTN   J     EXITN                                                            
                                                                                
         DS    0H                                                               
JOBFLDS  DC    AL2(AC#OE,AC#CE,AC#RSHR,0,0)                                     
                                                                                
LOOKFLDH DC    AL1(8+L'LOOKFLD),4X'00',AL1(L'LOOKFLD),AL2(0)                    
LOOKFLD  DC    C'OE,CE,HR'                                                      
         DS    0H                                                               
                                                                                
***FLDS  DS    0XL10                                                            
***US*&& DC    AL2(AC#CE,0,0,0,0)                                               
***UK*&& DC    AL2(AC#CE,AC#CEC,AC#CEB,AC#CECB,0)                               
***KFLDH DS    0XL13                                                            
***      DC    AL1(8+5),4X'00',AL1(5),AL2(0),CL5'OE,CE'                         
                                                                                
*** Uncommitted amount - do transaction **                                      
DOTRX    NTR1                                                                   
                                                                                
         ZAP   XDOTRX,PZERO                                                     
                                                                                
         USING TRNRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA(2),PRODUL                                                
         MVC   TRNKACT,SJACCNT                                                  
         MVC   TRNKWORK,ORDWC                                                   
         MVI   TRNKCULC,FF                                                      
         MVC   CSVKEY4,TRNKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
         J     DOTRX04                                                          
                                                                                
DOTRX02  LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
DOTRX04  CLC   CSVKEY4(TRNKWORK-TRNRECD),TRNKEY                                 
         JNE   DOTRX90                                                          
*&&UK                                                                           
         CLC   TRNKWORK,BILWC                                                   
         JE    DOTRX90                                                          
*&&                                                                             
         CLC   TRNKDATE,SPACES     TRXs only                                    
         JNH   DOTRX02                                                          
         CLC   TRNKREF,SPACES                                                   
         JNH   DOTRX02                                                          
         CLC   TRNKCULC,SPACES                                                  
         JNH   DOTRX02                                                          
                                                                                
         TM    TRNKSTAT,TRNSREVS+TRNSDRFT  skip reversals                       
         JNZ   DOTRX02       skip drafts (incl. FB advances)                    
                                                                                
         L     RF,AGOXBLCK                                                      
         CLI   GOWCF-GOXBLOCK(RF),YESQ                                          
         JNE   DOTRX05                                                          
         GOTOR CHKWCEC,TRNKWORK                                                 
         JL    DOTRX02                                                          
                                                                                
DOTRX05  LHI   R1,IOGET+IOMST+IO1                                               
         TM    TRNKSTAT,TRNSARCH                                                
         JZ    DOTRX06                                                          
         LHI   R1,IOGET+IOARC+IO1                                               
                                                                                
DOTRX06  GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         USING TRNELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,TRNRFST                                                       
         CLI   TRNEL,TRNELQ                                                     
         JNE   DOTRX02             just in case ...                             
         XC    FULL1,FULL1                                                      
         MVI   BYTE2,0                                                          
         CLC   TRNKULC(2),LDG1R                                                 
         JNE   DOTRX08                                                          
         CP    TRNAMNT,PZERO       Is it billable                               
         JE    DOTRX08             No                                           
         L     RF,AGOXBLCK                                                      
*NEC*    MVC   BYTE2,GODITSB-GOXBLOCK(RF)                                       
         MVC   BYTE2,GOETMB-GOXBLOCK(RF)                                        
                                                                                
DOTRX08  ZAP   DUB2,PZERO                                                       
         MVI   BYTE1,YESQ                                                       
         OC    CESTNUM,CESTNUM                                                  
         JZ    DOTRX10                                                          
         MVI   BYTE1,NOQ                                                        
                                                                                
         USING SCIELD,R3                                                        
DOTRX10  LLC   R0,SCILN                                                         
         AR    R3,R0                                                            
         CLI   SCIEL,0                                                          
         JE    DOTRX30                                                          
         CLI   SCIEL,SCIELQ                                                     
         JE    DOTRX12                                                          
         CLI   SCIEL,FFTELQ                                                     
         JE    DOTRX20                                                          
         CLI   SCIEL,TRSELQ                                                     
         JE    DOTRX22                                                          
         CLI   SCIEL,PRTELQ                                                     
         JNE   DOTRX10                                                          
                                                                                
         USING PRTELD,R3                                                        
         CLI   BYTE2,0                                                          
         JH    DOTRX10A                                                         
         CLC   TRNKULC(2),LDG1R                                                 
         JNE   DOTRX10A                                                         
         L     RF,AGOXBLCK                                                      
*NEC*    LA    RF,GODITSR                                                       
         MVC   BYTE2,GOETMR-GOXBLOCK(RF)                                        
         CP    PRTRATE,PZERO       R type has a rate                            
         JNE   DOTRX10A            Must be R type                               
         OC    PRTSTRT,PRTSTRT                                                  
         JNZ   DOTRX10A            non-bill doesn't                             
         MVC   BYTE2,GODITSN-GOXBLOCK(RF)                                       
                                                                                
DOTRX10A DS    0H                                                               
*&&US                                                                           
         TM    PRTSTAT,PRTSBILQ    Is this B-Time?                              
         JNZ   DOTRX10                                                          
         TM    PRTSTAT,PRTSRTEQ    Is this R-Time?                              
         JZ    DOTRX10                                                          
         ZAP   DUB,PRTRATE                                                      
         MP    DUB,PRTHOUR                                                      
         DP    DUB,=P'100'                                                      
         L     RF,AGOXBLCK                                                      
*NEC*    CLI   GOINCR-GOXBLOCK(RF),YESQ R time to be treated as actual          
*NEC*    JNE   DOTRX10                                                          
         ZAP   DUB2,DUB(6)                                                      
*&&                                                                             
         J     DOTRX10                                                          
                                                                                
         USING SCIELD,R3                                                        
DOTRX12  DS    0H                                                               
*&&UK                                                                           
         CLI   SCITYPE,SCITMCRT    COST AMOUNT                                  
         JNE   DOTRX14                                                          
         LA    RF,TRNRFST                                                       
         CP    TRNAMNT-TRNELD(L'TRNAMNT,RF),PZERO                               
         JNE   DOTRX10             Skip if billable time                        
         L     RF,AGOXBLCK                                                      
*NEC*    CLI   GOINCR-GOXBLOCK(RF),YESQ R time to be treated as actual          
*NEC*    JNE   DOTRX10                                                          
         CLI   GOCSAT-GOXBLOCK(RF),GOCSCOST Want cost amount?                   
         JNE   DOTRX10                                                          
         ZAP   DUB2,SCIAMNT        Debit amount                                 
         J     DOTRX10                                                          
                                                                                
DOTRX14  CLI   SCITYPE,SCITMSRT    SALES AMOUNT                                 
         JNE   DOTRX16                                                          
         LA    RF,TRNRFST                                                       
         CP    TRNAMNT-TRNELD(L'TRNAMNT,RF),PZERO                               
         JNE   DOTRX10             Skip if billable time                        
         L     RF,AGOXBLCK                                                      
*NEC*    CLI   GOINCR-GOXBLOCK(RF),YESQ R time to be treated as actual          
*NEC*    JNE   DOTRX10                                                          
         CLI   GOCSAT-GOXBLOCK(RF),GOCSCOST Want cost amount?                   
         JE    DOTRX10                                                          
         ZAP   DUB2,SCIAMNT      Debit amount                                   
         J     DOTRX10                                                          
                                                                                
DOTRX16  CLI   SCITYPE,SCITMEXP    Memo expenses (expense invoices)             
         JNE   DOTRX10                                                          
*&&                                                                             
*&&US                                                                           
DOTRX16  CLI   SCITYPE,SCITSJXP    Memo expenses (expense invoices)             
         JNE   DOTRX10                                                          
*&&                                                                             
         LA    RF,TRNRFST                                                       
         CP    TRNAMNT-TRNELD(L'TRNAMNT,RF),PZERO                               
         JNE   DOTRX10                                                          
         L     RF,AGOXBLCK                                                      
         CLI   GOEMIN-GOXBLOCK(RF),GONO  Do we include expense items            
         JE    DOTRX02                   Yes                                    
*NEC*    CLI   GODEIS-GOXBLOCK(RF),GODEIN Dashboard includes exp items          
*NEC*    JE    DOTRX02                    amount?                               
*NEC*    CLI   GODEIS-GOXBLOCK(RF),GODAMT no, skip element                      
*NEC*    JNE   DOTRX10                                                          
                                                                                
         ZAP   DUB2,SCIAMNT        Add expense amount to buffer                 
         LA    RF,TRNRFST                                                       
         TM    TRNSTAT-TRNELD(RF),TRNSDR                                        
         JNZ   DOTRX10                                                          
         ZAP   DUB2,PZERO                                                       
         J     DOTRX10                                                          
                                                                                
         USING FFTELD,R3                                                        
DOTRX20  CLI   FFTTYPE,FFTTESTN                                                 
         JNE   DOTRX10                                                          
         OC    CESTNUM,CESTNUM                                                  
         JZ    DOTRX10                                                          
*                                                                               
         LA    RF,FFTESTN                                                       
         CLI   FFTLN,FFTESLNQ      Any original estimate number?                
         JL    DOTRX21             No - OK                                      
         CLC   FFTOESTN,SPACES                                                  
         JNH   DOTRX21                                                          
         LA    RF,FFTOESTN                                                      
*                                                                               
DOTRX21  CLC   CESTNUM,0(RF)                                                    
         JNE   DOTRX02                                                          
         MVI   BYTE1,YESQ                                                       
         J     DOTRX10                                                          
                                                                                
         USING TRSELD,R3                                                        
DOTRX22  ST    R3,FULL1                                                         
         J     DOTRX10                                                          
                                                                                
         USING TRNELD,R3                                                        
DOTRX30  LA    R3,TRNRFST                                                       
***      CLI   BYTE1,YESQ          DSRD-6892                                    
***      JNE   DOTRX02             DSRD-6892                                    
                                                                                
         USING TRSELD,R1                                                        
         ICM   R1,B'1111',FULL1                                                 
         JZ    DOTRX36                                                          
         CLC   TRNKULC(2),LDG1R                                                 
         JNE   DOTRX36                                                          
         CLI   BYTE2,GONONE                 want none included?                 
         JE    DOTRX02                                                          
         CLI   BYTE2,GOINPR                 in progress or higher               
         JE    DOTRX36                                                          
         TM    TRSSTAT4,TRSSSAVT            ignore in progress                  
         JNZ   DOTRX02                                                          
         TM    TRSSTAT4,TRSSREJT            and rejected                        
         JNZ   DOTRX02                                                          
         CLI   BYTE2,GOSUB                  submitted or higher                 
         JE    DOTRX36                                                          
         TM    TRSSTAT4,TRSSSUBT            ignore submitted                    
         JNZ   DOTRX02                                                          
         CLI   BYTE2,GOMANPAP               line manager approved               
         JNE   DOTRX32                                                          
         TM    TRSSTAT4,TRSSMAAP            ignore part approved                
         JNZ   DOTRX36                                                          
         OC    TRSSTAT4,TRSSTAT4                                                
         JZ    DOTRX36                                                          
         J     DOTRX02                                                          
                                                                                
DOTRX32  CLI   BYTE2,GOCLIPAP      Client approved                              
         JNE   DOTRX34             No                                           
         TM    TRSSTAT4,TRSSSJAT   Is the time client approved                  
         JNZ   DOTRX36             Yes                                          
         OC    TRSSTAT4,TRSSTAT4   Or fully approved                            
         JZ    DOTRX36                                                          
         J     DOTRX02                                                          
                                                                                
DOTRX34  TM    TRSSTAT4,TRSSSJAT+TRSSMAAP   ignore part approved                
         JNZ   DOTRX02                                                          
         CLI   BYTE2,GOAPPR                 approved                            
         JE    DOTRX36                                                          
         DC    H'0'                                                             
         DROP  R1                                                               
                                                                                
DOTRX36  CP    TRNAMNT,PZERO       eg memo, Estimate trx etc                    
         JE    DOTRX38                                                          
         TM    TRNSTAT,TRNSDR                                                   
         JZ    DOTRX38                                                          
         ZAP   DUB2,TRNAMNT                                                     
                                                                                
DOTRX38  AP    XDOTRX,DUB2         add up now and carry on with IOs             
                                                                                
         CLI   QEUBYWC,YESQ        w/c break down?                              
         JNE   DOTRX02                                                          
                                                                                
         USING UCBYWCD,R1                                                       
         L     R1,AIO4                                                          
DOTRX40  CLI   UCBYWCWC,0                                                       
         JE    DOTRX44                                                          
         CLC   UCBYWCWC,TRNKWORK                                                
         JE    DOTRX42                                                          
         AHI   R1,UCBYWCLQ                                                      
         J     DOTRX40                                                          
DOTRX42  SP    UCBYWCAM,DUB2                                                    
         J     DOTRX02                                                          
DOTRX44  MVC   UCBYWCWC,TRNKWORK                                                
         ZAP   UCBYWCAM,PZERO                                                   
         SP    UCBYWCAM,DUB2                                                    
         XC    UCBYWCWC+UCBYWCLQ(L'UCBYWCWC),UCBYWCWC+UCBYWCLQ                  
         J     DOTRX02                                                          
         DROP  R1                                                               
                                                                                
DOTRX90  DS    0H                                                               
         DROP  R2,R3                                                            
                                                                                
DOTRXX   J     EXITY                                                            
                                                                                
*** Uncommitted amount - do expense claims **                                   
DOEXP    NTR1                                                                   
                                                                                
         ZAP   XDOEXP,PZERO                                                     
                                                                                
         USING GOXBLKD,RF          do we want any expenses?                     
         L     RF,AGOXBLCK                                                      
*NEC*    CLI   GODIEB,C'N'         billable?                                    
*NEC*    JNE   DOEXP02             yes - go read all                            
         CLI   GOEXPB,C'N'         billable?                                    
         JNE   DOEXP02             yes - go read all                            
                                                                                
*NEC*    CLI   GODIEN,C'N'         non-billable?                                
*NEC*    JE    DOEXPX              no, neither - exit                           
         CLI   GOEXPN,C'N'         non-billable?                                
         JE    DOEXPX              no, neither - exit                           
                                                                                
DOEXP02  MVI   BYTE1,EXJPCLI1                                                   
         DROP  RF                                                               
                                                                                
         USING EXJPASD,R2                                                       
DOEXP04  LA    R2,IOKEY                                                         
         XC    EXJPAS,EXJPAS                                                    
         MVI   EXJPTYP,EXJPTYPQ                                                 
         MVI   EXJPSUB,EXJPSUBQ                                                 
         MVC   EXJPCPY,CUXCPY                                                   
         MVC   EXJPVIEW,BYTE1                                                   
         MVC   EXJPCOFF,OD#OFF                                                  
         MVC   EXJPCPJ,SJACCNT                                                  
         MVC   CSVKEY4,EXJPAS                                                   
                                                                                
DOEXP06  LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
         J     DOEXP10                                                          
                                                                                
DOEXP08  LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
DOEXP10  CLC   CSVKEY4(EXJPVIEW-EXJPASD),EXJPAS                                 
         JNE   DOEXP90                                                          
         CLC   BYTE1,CSVKEY4+EXJPVIEW-EXJPASD                                   
         JE    DOEXP16                                                          
                                                                                
DOEXP12  CLI   BYTE1,EXJPCNB1      process various view types                   
         JE    DOEXP90                                                          
         CLI   BYTE1,EXJPCBL1                                                   
         JE    DOEXP14                                                          
         MVI   BYTE1,EXJPCBL1                                                   
         J     DOEXP04                                                          
                                                                                
DOEXP14  MVI   BYTE1,EXJPCNB1                                                   
         J     DOEXP04                                                          
                                                                                
DOEXP16  CLC   CSVKEY4(EXJPMED-EXJPASD),EXJPAS                                  
         JNE   DOEXP12             match on job?                                
                                                                                
         USING GOXBLKD,R4                                                       
         L     R4,AGOXBLCK                                                      
         TM    EXJPSTAT,EXCSLOGD   ignore logically deleted                     
         JNZ   DOEXP08                                                          
*NEC*    CLI   GODIEN,GOINPR       find lowest level of both settings           
*NEC*    JE    DOEXP18                                                          
*NEC*    CLI   GODIEB,GOINPR                                                    
*NEC*    JE    DOEXP18                                                          
         CLI   GOEXPN,GOINPR       find lowest level of both settings           
         JE    DOEXP18                                                          
         CLI   GOEXPB,GOINPR                                                    
         JE    DOEXP18                                                          
         OC    EXJPSTAT,EXJPSTAT   ignore in progress                           
         JZ    DOEXP08                                                          
         TM    EXJPSTAT,EXCSREJE   ignore and rejected                          
         JNZ   DOEXP08                                                          
*NEC*    CLI   GODIEN,GOSUB        Submitted?                                   
*NEC*    JE    DOEXP18                                                          
*NEC*    CLI   GODIEB,GOSUB                                                     
*NEC*    JE    DOEXP18                                                          
         CLI   GOEXPN,GOSUB        Submitted?                                   
         JE    DOEXP18                                                          
         CLI   GOEXPB,GOSUB                                                     
         JE    DOEXP18                                                          
         TM    EXJPSTAT,EXCSSUBM   Ignore submitted                             
         JNZ   DOEXP08                                                          
*NEC*    CLI   GODIEN,GOPAP        Part approved                                
*NEC*    JE    DOEXP18                                                          
*NEC*    CLI   GODIEB,GOPAP                                                     
*NEC*    JE    DOEXP18                                                          
         CLI   GOEXPN,GOPAP        Part approved                                
         JE    DOEXP18                                                          
         CLI   GOEXPB,GOPAP                                                     
         JE    DOEXP18                                                          
         TM    EXJPSTAT,EXCSPAPP+EXCSFNTA                                       
         JNZ   DOEXP08                                                          
         TM    EXJPSTAT,EXCSCOMP                                                
         JZ    DOEXP08                                                          
*NEC*    CLI   GODIEN,GOAPPR       Approved                                     
*NEC*    JE    DOEXP18                                                          
*NEC*    CLI   GODIEB,GOAPPR                                                    
*NEC*    JNE   *+2                                                              
         CLI   GOEXPN,GOAPPR       Approved                                     
         JE    DOEXP18                                                          
         CLI   GOEXPB,GOAPPR                                                    
         JNE   *+2                                                              
         DROP  R4                                                               
                                                                                
DOEXP18  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING EXCRECD,R2                                                       
         L     R2,AIO1                                                          
         USING CIDELD,R3                                                        
         LA    R3,EXCRFST                                                       
         J     DOEXP22                                                          
                                                                                
DOEXP20  LLC   R0,CIDLN            find appropriate CIDEL cluster(s)            
         AR    R3,R0                                                            
                                                                                
DOEXP22  CLI   CIDEL,0             look for main CIDELs                         
         JE    DOEXP08                                                          
         CLI   CIDEL,CIDELQ                                                     
         JNE   DOEXP20                                                          
         CLI   CIDTYPE,CIDTYMQ                                                  
         JNE   DOEXP20                                                          
                                                                                
         USING GOXBLKD,R4                                                       
         L     R4,AGOXBLCK                                                      
         CLC   CIDMWCD,SPACES      Have we got a workcode                       
         JNH   DOEXP23                                                          
         CLI   GOWCF,YESQ                                                       
         JNE   DOEXP23                                                          
         GOTOR CHKWCEC,CIDMWCD                                                  
         JL    DOEXP20                                                          
                                                                                
DOEXP23  CLI   CIDMTYP,C'B'        Billable                                     
         JNE   DOEXP24                                                          
         TM    EXCRSTAT,EXCSCOMP   Ignore fully approved - double count         
         JNZ   DOEXP20                                                          
*NEC*    LA    RF,GODIEB                                                        
         LA    RF,GOEXPB                                                        
         J     DOEXP26                                                          
                                                                                
DOEXP24  CLI   CIDMTYP,C'N'        Non-billable                                 
         JNE   *+2                 (bad CIDEL)                                  
                                                                                
*NEC*    LA    RF,GODIEN                                                        
         LA    RF,GOEXPN                                                        
*NEC*    CLI   GODEIS,NOQ          Have we already got memo invoice             
*NEC*    JE    DOEXP26             No                                           
         CLI   GOEMIN,NOQ          Have we already got memo invoice             
         JE    DOEXP26             No                                           
                                                                                
         TM    EXCRSTAT,EXCSCOMP   Yes ignore fully approved to avoid           
         JNZ   DOEXP20                             double counting              
                                                                                
DOEXP26  CLI   0(RF),GONONE        Filter by GODIEB OR GODIEN                   
         JE    DOEXP20             None                                         
         CLI   0(RF),GOINPR        In progress?                                 
         JE    DOEXP28                                                          
         OC    EXCRSTAT,EXCRSTAT   Ignore in progress                           
         JZ    DOEXP20                                                          
         TM    EXCRSTAT,EXCSREJE   Ignore rejected                              
         JNZ   DOEXP20                                                          
         CLI   0(RF),GOSUB         Submitted?                                   
         JE    DOEXP28                                                          
         TM    EXCRSTAT,EXCSSUBM   Ignore submitted                             
         JNZ   DOEXP20                                                          
         CLI   0(RF),GOPAP         Part approved                                
         JE    DOEXP28                                                          
         TM    EXCRSTAT,EXCSPAPP+EXCSFNTA  ignore part appr                     
         JNZ   DOEXP20                                                          
         TM    EXCRSTAT,EXCSCOMP                                                
         JZ    DOEXP20                                                          
         CLI   0(RF),GOAPPR        approved                                     
         JE    DOEXP28                                                          
         DC    H'0'                bad getopt setting                           
         DROP  R4                                                               
                                                                                
CUR      USING CIDELD,R5                                                        
DOEXP28  LA    R5,CIDELD           filter CIDEL and its cluster                 
DOEXP30  LLC   R0,CUR.CIDLN                                                     
         AR    R5,R0                                                            
         CLI   CUR.CIDEL,CIDELQ                                                 
         JNE   DOEXP20                                                          
         CLC   CUR.CIDSEQ,CIDSEQ                                                
         JNE   DOEXP20                                                          
         CLI   CUR.CIDTYPE,CIDTYCQ find account element                         
         JNE   DOEXP30                                                          
         CLC   CUR.CIDCCPJ,SJACCNT look for this job's items                    
         JNE   DOEXP20                                                          
* No estimate check CESTNUM (if present) vs. CIDCEGN here                       
* Note: cannot use BYTE1 here?|                                                 
         AP    XDOEXP,CIDMAMT      add to debit amount                          
                                                                                
         CLI   QEUBYWC,YESQ        w/c break down?                              
         JNE   DOEXP20                                                          
                                                                                
         USING UCBYWCD,R1                                                       
         L     R1,AIO4                                                          
DOEXP40  CLI   UCBYWCWC,0                                                       
         JE    DOEXP44                                                          
         CLC   UCBYWCWC,CIDMWCD                                                 
         JE    DOEXP42                                                          
         AHI   R1,UCBYWCLQ                                                      
         J     DOEXP40                                                          
DOEXP42  SP    UCBYWCAM,CIDMAMT                                                 
         J     DOEXP20                                                          
DOEXP44  MVC   UCBYWCWC,CIDMWCD                                                 
         ZAP   UCBYWCAM,PZERO                                                   
         SP    UCBYWCAM,CIDMAMT                                                 
         XC    UCBYWCWC+UCBYWCLQ(L'UCBYWCWC),UCBYWCWC+UCBYWCLQ                  
         J     DOEXP20                                                          
         DROP  R1,R2,R3,CUR                                                     
                                                                                
DOEXP90  DS    0H                                                               
                                                                                
DOEXPX   J     EXITY                                                            
                                                                                
*** Uncommitted amount - do timesheets **                                       
DOTIM    NTR1                                                                   
                                                                                
         ZAP   XDOTIM,PZERO                                                     
                                                                                
         USING TSJPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    TSJPAS,TSJPAS                                                    
         MVI   TSJPTYP,TSJPTYPQ                                                 
         MVI   TSJPSUB,TSJPSUBQ                                                 
         MVC   TSJPCPY,CUXCPY                                                   
         MVI   TSJPVIEW,TSJPSJAQ                                                
         MVC   TSJPCOFF,OD#OFF                                                  
         MVC   TSJPACT,SJACCNT                                                  
         MVC   CSVKEY4,TSJPAS                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
         J     DOTIM04                                                          
                                                                                
DOTIM02  LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
DOTIM04  CLC   CSVKEY4(TSJPMED-TSJPASD),TSJPAS                                  
         JNE   DOTIM90                                                          
                                                                                
         USING TMSTTABD,R3                                                      
         LA    R3,RTIMTUPT                                                      
         LA    R1,TMSTNMB                                                       
DOTIM06  CLI   TMSTMPTY,YESQ       Are we interested in saved timesheet         
         JNE   DOTIM08             No                                           
         OC    TSJPSTAT,TSJPSTAT   Yes - have we got one                        
         JZ    DOTIM14             Yes - want this time record                  
DOTIM08  OC    TMSTSTAT,TMSTSTAT   Are we interested in other statuses          
         JZ    DOTIM12             No - get next time type                      
         MVC   BYTE1,TMSTSTAT      Yes - do we have a match                     
         NC    BYTE1,TSJPSTAT                                                   
         OC    BYTE1,BYTE1         Any match on the statuses                    
         JZ    DOTIM12             No                                           
         OC    TMSTXSTA,TMSTXSTA   Yes - do we have an exception status         
         JZ    DOTIM10             No                                           
         MVC   BYTE1,TMSTXSTA      Yes - check for a match                      
         NC    BYTE1,TSJPSTAT                                                   
         OC    BYTE1,BYTE1         Any match                                    
         JNZ   DOTIM12             Yes - get next time type                     
DOTIM10  OC    TMSTCLST,TMSTCLST   Are we interested in client approval         
         JZ    DOTIM14             No - we want time record                     
         MVC   BYTE1,TMSTCLST      Yes - check it matches                       
         NC    BYTE1,TSJPKSTA                                                   
         OC    BYTE1,BYTE1                                                      
         JNZ   DOTIM14             Match found accept timel                     
                                                                                
DOTIM12  AHI   R3,TMSTTABL                                                      
         JCT   R1,DOTIM06                                                       
         J     DOTIM02                                                          
         DROP  R3                                                               
                                                                                
DOTIM14  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING TIMRECD,R2                                                       
         L     R2,AIO1                                                          
         USING TIMELD,R3                                                        
         LA    R3,TIMRFST                                                       
         ZAP   DUB,PZERO                                                        
         J     DOTIM18                                                          
                                                                                
DOTIM16  LLC   R0,TIMLN            find appropriate TIMEL cluster(s)            
         AR    R3,R0                                                            
                                                                                
DOTIM18  CLI   TIMEL,0                                                          
         JE    DOTIM02                                                          
         CLI   TIMEL,TIMELQ                                                     
         JNE   DOTIM16                                                          
                                                                                
         CLI   TIMETYP,TIMEINP                                                  
         JNE   DOTIM16                                                          
         XC    FULL1,FULL1                                                      
         CLC   TIMACC(L'PRODUL),PRODUL    FILTER BY REQ ACCOUNT                 
         JNE   DOTIM16                                                          
         CLC   TIMACC+2(L'ACTKACT),SJACCNT                                      
         JNE   DOTIM16                                                          
                                                                                
         CLC   TIMTSK,SPACES       Have we got a workcode                       
         JNH   DOTIM19                                                          
         L     RF,AGOXBLCK                                                      
         CLI   GOWCF-GOXBLOCK(RF),YESQ                                          
         JNE   DOTIM19                                                          
         GOTOR CHKWCEC,TIMTSK                                                   
         JL    DOTIM16                                                          
         MVC   SHALF1,TIMTSK                                                    
                                                                                
         USING TMSTTABD,R1                                                      
DOTIM19  CLI   TIMTTYP,TIMTCB      Billable time?                               
         JNE   DOTIM20                                                          
         LA    R1,BTIMTUPT                                                      
         J     DOTIM24                                                          
                                                                                
DOTIM20  CLI   TIMTTYP,TIMTCR      R type time?                                 
         JNE   DOTIM22                                                          
         LA    R1,RTIMTUPT                                                      
         J     DOTIM24                                                          
                                                                                
DOTIM22  CLI   TIMTTYP,TIMTCN      non billable time?                           
         JNE   *+2                                                              
         LA    R1,NTIMTUPT                                                      
                                                                                
DOTIM24  OC    TMSTSTAT,TMSTSTAT   Are we interested in any statuses            
         JNZ   DOTIM26             Yes                                          
         CLI   TMSTMPTY,NOQ        Are we interested in saved TS                
         JE    DOTIM16                                                          
                                                                                
DOTIM26  CLI   TMSTMPTY,YESQ       Are we interested in saved TS                
         JNE   DOTIM28             No                                           
         DS    0H                  Yes - do we have a saved timesheet           
         CLI   IOKEY+TSJPSTAT-TSJPASD,0                                         
         JE    DOTIM32             Yes - accept time line                       
         OC    TMSTSTAT,TMSTSTAT   No - are we interested in any other          
         JZ    DOTIM16                          statuses                        
DOTIM28  MVC   BYTE1,TMSTSTAT                                                   
         NC    BYTE1,IOKEY+TSJPSTAT-TSJPASD                                     
         OC    BYTE1,BYTE1         Any match on the statuses                    
         JZ    DOTIM16             No - don't want this timel                   
         OC    TMSTXSTA,TMSTXSTA   Yes - do we have an exception status         
         JZ    DOTIM30             No                                           
         MVC   BYTE1,TMSTXSTA      Yes - check for a match                      
         NC    BYTE1,IOKEY+TSJPSTAT-TSJPASD                                     
         OC    BYTE1,BYTE1                                                      
         JNZ   DOTIM16             Reject timel if found                        
                                                                                
DOTIM30  DS    0H                                                               
         OC    TMSTCLST,TMSTCLST   Are we interested in client approval         
         JZ    DOTIM32             No                                           
         MVC   BYTE1,TMSTCLST      Yes - check it matches                       
         NC    BYTE1,IOKEY+TSJPKSTA-TSJPASD                                     
         OC    BYTE1,BYTE1         Match found accept timel else                
         JZ    DOTIM16             reject timel                                 
         DROP  R1                                                               
                                                                                
DOTIM32  MVC   FULL1(L'TIMSEQ),TIMSEQ  Want this seq number                     
                                                                                
         ZAP   DUB1,TIMRATE        Amount = Rate * Hours                        
         MP    DUB1,TIMHRS                                                      
         SRP   DUB1,64-2,5         / 100 (hours is 2 dec places)                
*&&US*&& ZAP   DUB2,PZERO          No sales amount in US.                       
*&&UK                                                                           
         ZAP   DUB2,TIMCRATE       Amount = Rate * Hours                        
         MP    DUB2,TIMHRS                                                      
         SRP   DUB2,64-2,5         / 100 (hours is 2 dec places)                
*&&                                                                             
         CLI   TIMTTYP,TIMTCR        R-time                                     
         JNE   DOTIM34                                                          
         L     RF,AGOXBLCK                                                      
         CLI   GOINCR-GOXBLOCK(RF),YESQ R time to be treated as actual          
         JNE   DOTIM36                                                          
         ZAP   DUB,DUB1                                                         
*&&UK                                                                           
         CLI   GOCSAT-GOXBLOCK(RF),GOCSCOST Want cost amount?                   
         JNE   DOTIM36                                                          
         ZAP   DUB,DUB2        Debit amount                                     
*&&                                                                             
         J     DOTIM36                                                          
*                                                                               
DOTIM34  CLI   TIMTTYP,TIMTCB      B-time                                       
         JNE   DOTIM35             Unknown type of time                         
         ZAP   DUB,TIMAMNT                                                      
         J     DOTIM36                                                          
                                                                                
DOTIM35  CLI   TIMTTYP,TIMTCN      N-time                                       
         JNE   *+2                 Ignore else unknown type of time             
                                                                                
DOTIM36  DS    0H                                                               
* No estimate check CESTNUM (if present) vs. TIMSESNM here                      
                                                                                
DOTIM40  AP    XDOTIM,DUB          add to total and do next cluster             
                                                                                
         CLI   QEUBYWC,YESQ        w/c break down?                              
         JNE   DOTIM16                                                          
                                                                                
         USING UCBYWCD,R1                                                       
         L     R1,AIO4                                                          
DOTIM42  CLI   UCBYWCWC,0                                                       
         JE    DOTIM46                                                          
         CLC   UCBYWCWC,SHALF1                                                  
         JE    DOTIM44                                                          
         AHI   R1,UCBYWCLQ                                                      
         J     DOTIM42                                                          
DOTIM44  SP    UCBYWCAM,DUB                                                     
         J     DOTIM16                                                          
DOTIM46  MVC   UCBYWCWC,SHALF1                                                  
         ZAP   UCBYWCAM,PZERO                                                   
         SP    UCBYWCAM,DUB                                                     
         XC    UCBYWCWC+UCBYWCLQ(L'UCBYWCWC),UCBYWCWC+UCBYWCLQ                  
         J     DOTIM16                                                          
         DROP  R1                                                               
                                                                                
DOTIM90  DS    0H                                                               
         DROP  R2,R3                                                            
                                                                                
DOTIMX   J     EXITY                                                            
                                                                                
*** Uncommitted amount - do order transactions **                               
DOOTX    NTR1                                                                   
                                                                                
         ZAP   XDOOTX,PZERO                                                     
                                                                                
         L     RF,AGOXBLCK                                                      
         CLI   GODIOS-GOXBLKD(RF),C'N' don't want any orders                    
         JE    DOOTXX                                                           
                                                                                
         USING TRNRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA(2),PRODUL                                                
         MVC   TRNKACT,SJACCNT                                                  
         MVC   TRNKWORK,ORDWC                                                   
         MVC   CSVKEY4,TRNKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
         J     DOOTX04                                                          
                                                                                
DOOTX02  LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
DOOTX04  CLC   CSVKEY4(TRNKWORK-TRNRECD),TRNKEY                                 
         JNE   DOOTX90                                                          
                                                                                
         CLI   TRNKSTYP,TRNTORD    Order transactions only                      
         JNE   DOOTX02                                                          
         CLC   TRNKDATE,SPACES                                                  
         JNH   DOOTX02                                                          
         CLC   TRNKREF,SPACES                                                   
         JNH   DOOTX02                                                          
         CLC   TRNKCULC,SPACES                                                  
         JNH   DOOTX02                                                          
                                                                                
         TM    TRNKSTAT,TRNSREVS+TRNSDRFT    skip reversals                     
         JNZ   DOOTX02   skip drafts (incl. FB advances)                        
                                                                                
         LHI   R1,IOGET+IOMST+IO1                                               
         TM    TRNKSTAT,TRNSARCH                                                
         JZ    DOOTX06                                                          
         LHI   R1,IOGET+IOARC+IO1                                               
                                                                                
DOOTX06  GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         USING TRNELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,TRNRFST                                                       
         CLI   TRNEL,TRNELQ                                                     
         JNE   DOOTX02             just in case ...                             
                                                                                
         OC    CESTNUM,CESTNUM                                                  
         JZ    DOOTX10                                                          
                                                                                
         USING FFTELD,R3                                                        
DOOTX08  LLC   R0,FFTLN                                                         
         AR    R3,R0                                                            
         CLI   FFTEL,0                                                          
         JE    DOOTX02                                                          
         CLI   FFTEL,FFTELQ                                                     
         JNE   DOOTX08                                                          
         CLI   FFTTYPE,FFTTESTN                                                 
         JNE   DOOTX08                                                          
*                                                                               
         LA    RF,FFTESTN                                                       
         CLI   FFTLN,FFTESLNQ      Any original estimate number?                
         JL    DOOTX09             No - OK                                      
         CLC   FFTOESTN,SPACES                                                  
         JNH   DOOTX09                                                          
         LA    RF,FFTOESTN                                                      
*                                                                               
DOOTX09  CLC   CESTNUM,0(RF)                                                    
         JNE   DOOTX02                                                          
                                                                                
         USING OAMELD,R3                                                        
DOOTX10  ZAP   DUB,PZERO                                                        
         LA    R3,TRNRFST                                                       
                                                                                
DOOTX12  LLC   R0,OAMLN                                                         
         AR    R3,R0                                                            
         CLI   OAMEL,0                                                          
         JE    DOOTX22                                                          
         CLI   OAMEL,OAMELQ                                                     
         JNE   DOOTX12                                                          
         CLC   OAMWORK,SPACES                                                   
         JNH   DOOTX12                                                          
         GOTOR CHKWCEC,OAMWORK                                                  
         JL    DOOTX12                                                          
         AP    DUB,OAMAMNT                                                      
         SP    DUB,OAMIVAL                                                      
                                                                                
         CLI   QEUBYWC,YESQ        w/c break down?                              
         JNE   DOOTX12                                                          
                                                                                
         USING UCBYWCD,R1                                                       
         L     R1,AIO4                                                          
DOOTX14  CLI   UCBYWCWC,0                                                       
         JE    DOOTX18                                                          
         CLC   UCBYWCWC,OAMWORK                                                 
         JE    DOOTX16                                                          
         AHI   R1,UCBYWCLQ                                                      
         J     DOOTX14                                                          
DOOTX16  SP    UCBYWCAM,OAMAMNT                                                 
         AP    UCBYWCAM,OAMIVAL                                                 
         J     DOOTX12                                                          
DOOTX18  MVC   UCBYWCWC,OAMWORK                                                 
         ZAP   UCBYWCAM,PZERO                                                   
         SP    UCBYWCAM,OAMAMNT                                                 
         AP    UCBYWCAM,OAMIVAL                                                 
         XC    UCBYWCWC+UCBYWCLQ(L'UCBYWCWC),UCBYWCWC+UCBYWCLQ                  
         J     DOOTX12                                                          
         DROP  R1                                                               
                                                                                
DOOTX22  AP    XDOOTX,DUB                                                       
         J     DOOTX02                                                          
                                                                                
DOOTX90  DS    0H                                                               
         DROP  R2,R3                                                            
                                                                                
DOOTXX   J     EXITY                                                            
                                                                                
*** Uncommitted amount - do orders **                                           
DOORD    NTR1                                                                   
                                                                                
         ZAP   XDOORD,PZERO        Approved memo and unapproved orders          
                                                                                
         USING OSJPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    OSJPAS,OSJPAS                                                    
         MVI   OSJPTYP,OSJPTYPQ                                                 
         MVI   OSJPSUB,OSJPSUBQ                                                 
         MVC   OSJPCPY,CUXCPY                                                   
         MVC   OSJPACT,SJACCNT                                                  
         MVC   CSVKEY4,OSJPAS                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   *+2                                                              
         J     DOORD04                                                          
                                                                                
DOORD02  LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
DOORD04  CLC   CSVKEY4(OSJPMEM-OSJPASD),OSJPAS                                  
         JNE   DOORD90                                                          
                                                                                
         TM    OSJPSTAT,ORDCLOSE+ORDSFMCH+ORDSLDEL+ORDGDRCV                     
         JNZ   DOORD02                                                          
         TM    OSJPSTA2,ORDSOREJ   Ignore rejected orders                       
         JNZ   DOORD02                                                          
                                                                                
         CLI   OSJPMEM,OSJPMYEQ    Test memo order                              
         JE    DOORD06                                                          
         TM    OSJPSTA2,ORDSAPPR   If not, check unapproved                     
         JNZ   DOORD02             No, GETOTX will handle                       
                                                                                
         USING GOXBLKD,R4                                                       
DOORD06  L     R4,AGOXBLCK                                                      
                                                                                
*NEC*    LA    RF,GODIOS           order option                                 
         LA    RF,GOEORN           order option                                 
         CLI   OSJPMEM,OSJPMYEQ    is it memo?                                  
         JNE   DOORD08                                                          
*NEC*    LA    RF,GODIEO           yes-use memo order option                    
         LA    RF,GOEORM           yes-use memo order option                    
                                                                                
DOORD08  CLI   0(RF),GONONE        none                                         
         JE    DOORD02                                                          
         CLI   0(RF),GOINPR        in progress                                  
         JE    DOORD10                                                          
         TM    OSJPSTA2,ORDSDRFT   ignore in progress                           
         JNZ   DOORD02                                                          
         CLI   0(RF),GOSUB         submitted                                    
         JE    DOORD10                                                          
         TM    OSJPSTA2,ORDSSUBM   ignore submitted                             
         JNZ   DOORD02                                                          
         CLI   0(RF),GOPAP         part approved                                
         JE    DOORD10                                                          
         TM    OSJPSTA2,ORDSPAPP   ignore part approved                         
         JNZ   DOORD02                                                          
         CLI   0(RF),GOAPPR        approved                                     
         JE    DOORD10                                                          
         DC    H'0'                Status not known                             
                                                                                
DOORD10  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING ORDRECD,R2                                                       
         L     R2,AIO1                                                          
         USING OAMELD,R3                                                        
         LA    R3,ORDRFST                                                       
         ZAP   DUB,PZERO                                                        
         MVI   BYTE2,YESQ                                                       
         MVC   SHALF1,SPACES                                                    
                                                                                
         USING FFTELD,R3                                                        
DOORD12  LLC   R0,FFTLN                                                         
         AR    R3,R0                                                            
         CLI   FFTEL,0                                                          
         JE    DOORD16                                                          
         CLI   FFTEL,FFTELQ                                                     
         JNE   DOORD12                                                          
                                                                                
         CLI   FFTTYPE,FFTTESTN                                                 
         JNE   DOORD14                                                          
         OC    CESTNUM,CESTNUM                                                  
         JZ    DOORD12                                                          
         CLI   GOECE,GOECSTA                                                    
         JNE   DOORD12                                                          
*                                                                               
         LA    RF,FFTESTN                                                       
         CLI   FFTLN,FFTESLNQ      Any original estimate number?                
         JL    DOORD13             No - OK                                      
         CLC   FFTOESTN,SPACES                                                  
         JNH   DOORD13                                                          
         LA    RF,FFTOESTN                                                      
*                                                                               
DOORD13  CLC   CESTNUM,0(RF)                                                    
         JNE   DOORD02                                                          
         J     DOORD12                                                          
                                                                                
DOORD14  CLI   FFTTYPE,FFTTWRKC                                                 
         JNE   DOORD12                                                          
         CLC   FFTWORK,SPACES                                                   
         JNH   DOORD12                                                          
         MVC   SHALF1,FFTWORK                                                   
         CLI   GOWCF,YESQ                                                       
         JNE   DOORD12                                                          
         GOTOR CHKWCEC,FFTWORK                                                  
         JE    DOORD12                                                          
         MVI   BYTE2,NOQ                                                        
         J     DOORD12                                                          
                                                                                
DOORD16  CLI   BYTE2,YESQ                                                       
         JNE   DOORD02                                                          
                                                                                
         USING OAMELD,R3                                                        
         LA    R3,ORDRFST                                                       
                                                                                
DOORD18  LLC   R0,OAMLN                                                         
         AR    R3,R0                                                            
         CLI   OAMEL,0                                                          
         JE    DOORD40                                                          
         CLI   OAMEL,OAMELQ                                                     
         JNE   DOORD18                                                          
                                                                                
         CLC   OAMWORK,SPACES      ??? or skip if no w/c ???                    
         JNH   DOORD20                                                          
         GOTOR CHKWCEC,OAMWORK                                                  
         JL    DOORD18                                                          
         MVC   SHALF1,OAMWORK                                                   
         J     DOORD22                                                          
                                                                                
DOORD20  CLC   SHALF1,SPACES                                                    
         JNH   DOORD18                                                          
                                                                                
DOORD22  AP    DUB,OAMAMNT                                                      
         SP    DUB,OAMIVAL                                                      
                                                                                
DOORD24  CLI   QEUBYWC,YESQ        w/c break down?                              
         JNE   DOORD18                                                          
                                                                                
         USING UCBYWCD,R1                                                       
         L     R1,AIO4                                                          
DOORD26  CLI   UCBYWCWC,0                                                       
         JE    DOORD30                                                          
         CLC   UCBYWCWC,SHALF1                                                  
         JE    DOORD28                                                          
         AHI   R1,UCBYWCLQ                                                      
         J     DOORD26                                                          
DOORD28  SP    UCBYWCAM,OAMAMNT                                                 
         AP    UCBYWCAM,OAMIVAL                                                 
         J     DOORD18                                                          
DOORD30  MVC   UCBYWCWC,SHALF1                                                  
         ZAP   UCBYWCAM,PZERO                                                   
         SP    UCBYWCAM,OAMAMNT                                                 
         AP    UCBYWCAM,OAMIVAL                                                 
         XC    UCBYWCWC+UCBYWCLQ(L'UCBYWCWC),UCBYWCWC+UCBYWCLQ                  
         J     DOORD18                                                          
         DROP  R1                                                               
                                                                                
DOORD40  AP    XDOORD,DUB                                                       
         J     DOORD02                                                          
                                                                                
DOORD90  DS    0H                                                               
         DROP  R2,R3,R4                                                         
                                                                                
DOORDX   J     EXITY                                                            
                                                                                
*** Set SJ account details from SJACCNT **                                      
SETSJA   NTR1                                                                   
                                                                                
         OC    SJACCNT,SPACES                                                   
         MVC   OD#CLI,SPACES                                                    
         MVC   OD#CLOFF,SPACES                                                  
         MVC   OD#PRO,SPACES                                                    
         MVC   OD#JOB,SPACES                                                    
         L     R1,AIO6                                                          
         XC    0(L'ACTKEY,R1),0(R1)                                             
                                                                                
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#CLI(0),SJACCNT                                                
         AHI   RE,1                                                             
         LR    R0,RE                                                            
         LA    RE,SJACCNT(RE)                                                   
         XR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   OD#PRO(0),0(RE)                                                  
         AHI   RF,1                                                             
         LR    R0,RF                                                            
         IC    RF,PPROLEN                                                       
         LA    RF,SJACCNT(RF)                                                   
         XR    RE,RE                                                            
         IC    RE,PJOBLEN                                                       
         SR    RE,R0                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#JOB(0),0(RF)                                                  
                                                                                
         MVC   QCLI,OD#CLI                                                      
         MVC   QPRO,OD#PRO                                                      
         MVC   QMED,OD#JOB                                                      
         CLC   OD#CLI,SPACES       client                                       
         JNH   EXITY                                                            
                                                                                
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   ACTKACT(0),SJACCNT                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
                                                                                
         L     R3,AIO1                                                          
         LA    R3,ACTRFST                                                       
         USING NAMELD,R3                                                        
         XR    R0,R0                                                            
         MVC   TEMP,SPACES                                                      
         MVC   WORK,SPACES                                                      
                                                                                
SETSJA02 CLI   NAMEL,0                                                          
         JE    SETSJA18                                                         
         CLI   NAMEL,PPRELQ                                                     
         JE    SETSJA05                                                         
         CLI   NAMEL,NAMELQ                                                     
         JE    SETSJA06                                                         
*&&UK*&& CLI   NAMEL,SNMELQ                                                     
*&&UK*&& JE    SETSJA08                                                         
*&&UK*&& CLI   NAMEL,XNMELQ                                                     
*&&UK*&& JE    SETSJA10                                                         
         CLI   NAMEL,ADRELQ                                                     
         JE    SETSJA14                                                         
                                                                                
SETSJA04 IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         J     SETSJA02                                                         
                                                                                
         USING PPRELD,R3                                                        
SETSJA05 MVC   OD#SRAC,PPRRECVU                                                 
         CLC   PPRGAOFF,SPACES                                                  
         JNH   SETSJA04                                                         
         MVC   OD#CLOFF,PPRGAOFF                                                
         J     SETSJA04                                                         
         DROP  R3                                                               
                                                                                
         USING NAMELD,R3                                                        
SETSJA06 XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   WORK(0),NAMEREC                                                  
         J     SETSJA04                                                         
                                                                                
*&&UK                                                                           
         USING SNMELD,R3                                                        
SETSJA08 TM    SCPYEL+CPYSTATC-CPYELD,CPYSMEDN                                  
         JZ    SETSJA04                                                         
         XR    RE,RE                                                            
         IC    RE,SNMLN                                                         
         SHI   RE,SNMLN1Q+1                                                     
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TEMP(0),SNMNAME                                                  
         J     SETSJA04                                                         
                                                                                
         USING XNMELD,R3                                                        
SETSJA10 TM    SCPYEL+CPYSTATC-CPYELD,CPYSALTN                                  
         JZ    SETSJA04                                                         
         XR    RE,RE                                                            
         IC    RE,XNMSUBL                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TEMP+40(0),XNMSUBN                                               
         J     SETSJA04                                                         
*&&                                                                             
         USING ADRELD,R3                                                        
SETSJA14 XR    R1,R1                                                            
         ICM   R1,1,ADRNUM         N'ADDRESS LINES                              
         JZ    SETSJA04                                                         
         LA    RF,ADRADD1                                                       
         LA    RE,OD#CLIA1                                                      
                                                                                
SETSJA16 MVC   0(L'ADRADD1,RE),0(RF)                                            
         AHI   RE,L'ADRADD1                                                     
         AHI   RF,L'ADRADD1                                                     
         JCT   R1,SETSJA16                                                      
         J     SETSJA04                                                         
                                                                                
                                                                                
SETSJA18 MVC   OD#CLIN,WORK                                                     
         CLC   TEMP(36),SPACES                                                  
         JNH   SETSJA20                                                         
         MVC   OD#CLIN,TEMP                                                     
         MVC   OD#PCLN,WORK                                                     
         J     SETSJA22                                                         
                                                                                
SETSJA20 CLC   TEMP+40(36),SPACES                                               
         JNH   SETSJA22                                                         
         MVC   OD#CLIN,TEMP+36                                                  
         MVC   OD#PCLN,WORK                                                     
                                                                                
SETSJA22 CLC   OD#PRO,SPACES       product                                      
         JNH   EXITY                                                            
                                                                                
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   ACTKACT(0),SJACCNT                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
                                                                                
         L     R3,AIO1                                                          
         LA    R3,ACTRFST                                                       
         USING NAMELD,R3                                                        
         XR    R0,R0                                                            
         MVC   TEMP,SPACES                                                      
         MVC   WORK,SPACES                                                      
                                                                                
SETSJA24 CLI   NAMEL,0                                                          
         JE    SETSJA38                                                         
         CLI   NAMEL,NAMELQ                                                     
         JE    SETSJA28                                                         
*&&UK*&& CLI   NAMEL,SNMELQ                                                     
*&&UK*&& JE    SETSJA30                                                         
*&&UK*&& CLI   NAMEL,XNMELQ                                                     
*&&UK*&& JE    SETSJA32                                                         
         CLI   NAMEL,ADRELQ                                                     
         JE    SETSJA34                                                         
         CLI   NAMEL,PPRELQ                                                     
         JE    SETSJA37                                                         
                                                                                
SETSJA26 IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         J     SETSJA24                                                         
                                                                                
SETSJA28 XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   WORK(0),NAMEREC                                                  
         J     SETSJA26                                                         
                                                                                
*&&UK                                                                           
         USING SNMELD,R3                                                        
SETSJA30 TM    SCPYEL+CPYSTATC-CPYELD,CPYSMEDN                                  
         JZ    SETSJA26                                                         
         XR    RE,RE                                                            
         IC    RE,SNMLN                                                         
         SHI   RE,SNMLN1Q+1                                                     
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TEMP(0),SNMNAME                                                  
         J     SETSJA26                                                         
                                                                                
         USING XNMELD,R3                                                        
SETSJA32 TM    SCPYEL+CPYSTATC-CPYELD,CPYSALTN                                  
         JZ    SETSJA26                                                         
         XR    RE,RE                                                            
         IC    RE,XNMSUBL                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TEMP+40(0),XNMSUBN                                               
         J     SETSJA26                                                         
*&&                                                                             
         USING ADRELD,R3                                                        
SETSJA34 XR    R1,R1                                                            
         ICM   R1,1,ADRNUM         N'ADDRESS LINES                              
         JZ    SETSJA26                                                         
         LA    RF,ADRADD1                                                       
         LA    RE,OD#PROA1                                                      
                                                                                
SETSJA36 MVC   0(L'ADRADD1,RE),0(RF)                                            
         AHI   RE,L'ADRADD1                                                     
         AHI   RF,L'ADRADD1                                                     
         JCT   R1,SETSJA36                                                      
         J     SETSJA26                                                         
                                                                                
         USING PPRELD,R3                                                        
SETSJA37 CLC   PPRRECVU,SPACES                                                  
         JNH   *+10                                                             
         MVC   OD#SRAC,PPRRECVU                                                 
         CLC   PPRGAOFF,SPACES                                                  
         JNH   SETSJA26                                                         
         MVC   OD#CLOFF,PPRGAOFF                                                
         J     SETSJA26                                                         
                                                                                
SETSJA38 MVC   OD#PRON,WORK                                                     
         CLC   TEMP(36),SPACES                                                  
         JNH   SETSJA40                                                         
         MVC   OD#PRON,TEMP                                                     
         MVC   OD#PPRN,WORK                                                     
         J     SETSJA42                                                         
                                                                                
SETSJA40 CLC   TEMP+40(36),SPACES                                               
         JNH   SETSJA42                                                         
         MVC   OD#PRON,TEMP+36                                                  
         MVC   OD#PPRN,WORK                                                     
                                                                                
SETSJA42 CLC   OD#JOB,SPACES       job                                          
         JNH   EXITY                                                            
                                                                                
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         MVC   ACTKACT,SJACCNT                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   EXITY                                                            
         MVI   OD#JOBL,NOQ                                                      
         TM    ACTKSTAT,ACTSLOCK                                                
         JZ    *+8                                                              
         MVI   OD#JOBL,YESQ                                                     
         TM    ACTKSTAT,ACTSCLOS                                                
         JZ    *+8                                                              
         MVI   OD#JOBL,C'C'                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   EXITY                                                            
                                                                                
         L     R0,AIO6             save job in IO6                              
         LA    R1,IOLENQ                                                        
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R3,AIO1                                                          
         LA    R3,ACTRFST                                                       
         USING NAMELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
SETSJA44 CLI   NAMEL,0                                                          
         JE    SETSJA90                                                         
         CLI   NAMEL,NAMELQ                                                     
         JE    SETSJA48                                                         
*&&UK*&& CLI   NAMEL,ASTELQ                                                     
*&&UK*&& JE    SETSJA50                                                         
         CLI   NAMEL,ADRELQ                                                     
         JE    SETSJA70                                                         
         CLI   NAMEL,RSTELQ                                                     
         JE    SETSJA60                                                         
         CLI   NAMEL,PPRELQ                                                     
         JE    SETSJA62                                                         
                                                                                
SETSJA46 IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         J     SETSJA44                                                         
                                                                                
SETSJA48 XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   OD#JOBN(0),NAMEREC                                               
         J     SETSJA46                                                         
                                                                                
*&&UK                                                                           
         USING ASTELD,R3                                                        
SETSJA50 TM    ASTSTAT1,ASTISFOR                                                
         JZ    SETSJA46                                                         
         CLI   BYTE2,YESQ                                                       
         JNE   SETSJA46                                                         
         MVI   OD#FNIU,C'J'                                                     
         J     SETSJA46                                                         
*&&                                                                             
                                                                                
         USING RSTELD,R3                                                        
SETSJA60 MVI   OD#JLEX,NOQ                                                      
         MVI   OD#JLTS,NOQ         Set whether job locked from t/s              
         CLI   RSTLN,RSTLN2Q                                                    
         JNH   SETSJA46                                                         
         TM    RSTLSTAT,RSTLSEXQ                                                
         JZ    *+8                                                              
         MVI   OD#JLEX,YESQ                                                     
         TM    RSTLSTAT,RSTLSTIQ                                                
         JZ    SETSJA46                                                         
         MVI   OD#JLTS,YESQ                                                     
         J     SETSJA46                                                         
*                                                                               
         USING PPRELD,R3                                                        
SETSJA62 CLC   PPRRECVU,SPACES                                                  
         JNH   SETSJA46                                                         
         MVC   OD#SRAC,PPRRECVU                                                 
         J     SETSJA46                                                         
*                                                                               
         USING ADRELD,R3                                                        
SETSJA70 XR    R1,R1                                                            
         ICM   R1,1,ADRNUM         N'ADDRESS LINES                              
         JZ    SETSJA46                                                         
         LA    RF,ADRADD1                                                       
         LA    RE,OD#JOBA1                                                      
                                                                                
SETSJA72 MVC   0(L'ADRADD1,RE),0(RF)                                            
         AHI   RE,L'ADRADD1                                                     
         AHI   RF,L'ADRADD1                                                     
         JCT   R1,SETSJA72                                                      
         J     SETSJA46                                                         
*                                                                               
                                                                                
SETSJA90 CLC   OD#SRAC,SPACES      Any debtor account?                          
         JNH   EXITY                                                            
         CLI   QISPDF,YESQ         PDF request?                                 
         JNE   EXITY                                                            
         MVC   TEMP2(L'OD#SRAC),OD#SRAC                                         
         GOTOR (#GETACN,AGETACN)                                                
         MVC   OD#SRAN,TEMP2                                                    
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
** Get FORMAT code for current order **                                         
         USING ERFPASD,R2                                                       
GETFOR   NTR1                                                                   
         LA    R2,IOKEY            build passive                                
         XC    ERFPAS,ERFPAS                                                    
         MVI   ERFPTYP,ERFPTYPQ                                                 
         MVI   ERFPSUB,ERFPSUBQ                                                 
         MVC   ERFPCPY,CUXCPY                                                   
         MVI   ERFPAPPL,ERFKORDQ                                                
         MVC   ERFPETYP,OD#ETY                                                  
         MVC   ERFPOTYP,ORDTYPE                                                 
         MVC   ERFPSACC,OD#SUP                                                  
         MVC   SAVEKEY4,ERFPAS                                                  
                                                                                
         DS    0H                  ** E O S **                                  
         GOTOR GFORIO                                                           
         JE    GETFOR9                                                          
                                                                                
         MVC   ERFPAS,SAVEKEY4     ** E . S **                                  
         MVC   ERFPOTYP,FFS                                                     
         GOTOR GFORIO                                                           
         JE    GETFOR9                                                          
                                                                                
         MVC   ERFPAS,SAVEKEY4     ** . O S **                                  
         MVC   ERFPETYP,FFS                                                     
         GOTOR GFORIO                                                           
         JE    GETFOR9                                                          
                                                                                
         MVC   ERFPAS,SAVEKEY4     ** . . S **                                  
         MVC   ERFPETYP,FFS                                                     
         MVC   ERFPOTYP,FFS                                                     
         GOTOR GFORIO                                                           
         JE    GETFOR9                                                          
                                                                                
         MVC   ERFPAS,SAVEKEY4     ** E O . **                                  
         MVC   ERFPSACC,FFS                                                     
         GOTOR GFORIO                                                           
         JE    GETFOR9                                                          
                                                                                
         MVC   ERFPAS,SAVEKEY4     ** E . . **                                  
         MVC   ERFPOTYP,FFS                                                     
         MVC   ERFPSACC,FFS                                                     
         GOTOR GFORIO                                                           
         JE    GETFOR9                                                          
                                                                                
         MVC   ERFPAS,SAVEKEY4     ** . O . **                                  
         MVC   ERFPETYP,FFS                                                     
         MVC   ERFPSACC,FFS                                                     
         GOTOR GFORIO                                                           
         JE    GETFOR9                                                          
                                                                                
         MVC   ERFPAS,SAVEKEY4     ** . . . **                                  
         MVC   ERFPETYP,FFS                                                     
         MVC   ERFPOTYP,FFS                                                     
         MVC   ERFPSACC,FFS                                                     
         GOTOR GFORIO                                                           
         JNE   GETFORX                                                          
                                                                                
GETFOR9  MVC   OD#FCOD,ERFPFRMT                                                 
                                                                                
GETFORX  XIT1                                                                   
                                                                                
GFORIO   ST    RE,SAVERE                                                        
         XC    TEMP2,TEMP2         init                                         
         MVC   SAVEKEY3,ERFPAS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JNE   GFORIOH                                                          
GFORIO1  CLC   ERFPAS(ERFPETYP-ERFPASD),SAVEKEY3                                
         JNE   GFORIOH                                                          
         CLC   ERFPAS(ERFPFRMT-ERFPASD),SAVEKEY3                                
         JNE   GFORIOL                                                          
         CLC   OD#OFF,SPACES                                                    
         JNH   GFORIO3                                                          
         CLC   ERFPSOFF,SPACES                                                  
         JH    GFORIO2                                                          
         MVC   TEMP2(L'ERFPAS),ERFPAS                                           
         J     GFORION                                                          
GFORIO2  CLC   ERFPSOFF,OD#OFF                                                  
         JNE   GFORION                                                          
         J     GFORIOE                                                          
GFORIO3  CLC   ERFPSOFF,SPACES                                                  
         JH    GFORION                                                          
         J     GFORIOE                                                          
GFORION  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JE    GFORIO1                                                          
         J     GFORIOH                                                          
GFORIOL  OC    TEMP2(L'ERFPAS),TEMP2                                            
         JNZ   GFORIO4                                                          
         MVI   BYTE1,0                                                          
         J     GFORIOX                                                          
GFORIOH  OC    TEMP2(L'ERFPAS),TEMP2                                            
         JNZ   GFORIO4                                                          
         MVI   BYTE1,2                                                          
         J     GFORIOX                                                          
GFORIO4  MVC   ERFPAS,TEMP2                                                     
GFORIOE  MVI   BYTE1,1                                                          
         J     GFORIOX                                                          
GFORIOX  L     RE,SAVERE                                                        
         CLI   BYTE1,1                                                          
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ SUPPLIER ACCOUNT TO CHECK WHETHER GAP IS IN USE                *         
* ON NTRY TEMP2=SUPPLIER ACCOUNT                                      *         
***********************************************************************         
         USING ACTRECD,R2                                                       
         USING RSTELD,R3                                                        
SUPGAP   NTR1                                                                   
*&&US                                                                           
         MVI   OD#GAPYN,NOQ                                                     
         MVI   OD#GAPAQ,NOQ                                                     
*&&                                                                             
         LA    R2,IOKEY                                                         
         MVC   IOKEY,SPACES                                                     
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,TEMP2                                                    
         CLC   TEMP2,SPACES                                                     
         JNH   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   CHKGPRR                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
         L     R2,AIO1                                                          
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('RSTELQ',ACTRECD),0                   
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
         L     R3,12(R1)                                                        
         CLI   RSTLN,RSTLN2Q       Element long enough?                         
         JNH   SUPGAP02                                                         
*&&US                                                                           
         TM    RSTSTAT7,RSTGAPYN   GAP supplier setting                         
         JZ    *+8                                                              
         MVI   OD#GAPYN,YESQ                                                    
         TM    RSTSTAT7,RSTGAPAQ   Acknowledge/query                            
         JZ    *+8                                                              
         MVI   OD#GAPAQ,YESQ                                                    
*&&                                                                             
*&&UK                                                                           
         TM    RSTSTAT7,RSTGAPQY   GAP supplier setting                         
         JZ    *+8                                                              
         MVI   OD#GAPYN,YESQ                                                    
         TM    RSTSTAT7,RSTGAPAY   Acknowledge/query                            
         JZ    *+8                                                              
         MVI   OD#GAPAQ,YESQ                                                    
         TM    RSTSTAT7,RSTGAPQN   GAP supplier setting                         
         JZ    *+8                                                              
         MVI   OD#GAPYN,NOQ                                                     
         TM    RSTSTAT7,RSTGAPAN   Acknowledge/query                            
         JZ    *+8                                                              
         MVI   OD#GAPAQ,NOQ                                                     
*&&                                                                             
*                                                                               
SUPGAP02 LA    R6,SLLEVS                                                        
*&&US                                                                           
         CLI   OD#GAPYN,YESQ       Is the GAP set at the lowest level           
         JNE   *+12                No - read higher levels                      
         CLI   OD#GAPAQ,YESQ                                                    
         JE    EXITY               Yes - nothing to do                          
*&&                                                                             
*&&UK                                                                           
         CLI   OD#GAPYN,C' '       Is the GAP set at the lowest level           
         JNH   *+12                No - read higher levels                      
         CLI   OD#GAPAQ,C' '                                                    
         JH    EXITY               Yes - nothing to do                          
*&&                                                                             
         CLI   0(R6),L'ACTKACT     First level is full length                   
         JE    EXITY                                                            
         LHI   R0,4                                                             
         LA    R6,3(R6)                                                         
*                                                                               
SUPGAP04 CLI   0(R6),0                                                          
         JE    SUPGAP06                                                         
         CLI   0(R6),L'ACTKACT                                                  
         JE    SUPGAP06                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,1                                                             
         LA    RF,ACTKACT(RF)      FIND LENGTH OF ACCOUNT                       
         CLI   0(RF),C' '                                                       
         JH    SUPGAP08                                                         
SUPGAP06 SHI   R6,1                                                             
         JCT   R0,SUPGAP04                                                      
         J     EXITY                                                            
*                                                                               
K        USING ACTRECD,IOKEY                                                    
SUPGAP08 MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         MVC   K.ACTKUNT(L'ACTKUNT+L'ACTKLDG),ACTKUNT                           
         LLC   RF,0(R6)                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   K.ACTKACT(0),ACTKACT                                             
         EX    RF,0(RE)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
         L     R2,AIO1                                                          
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('RSTELQ',ACTRECD),0                   
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
         L     R3,12(R1)                                                        
         CLI   RSTLN,RSTLN2Q       Element long enough?                         
         JNH   SUPGAP12                                                         
*&&US                                                                           
         TM    RSTSTAT7,RSTGAPYN       GAP supplier controls                    
         JZ    *+8                                                              
         MVI   OD#GAPYN,YESQ                                                    
         TM    RSTSTAT7,RSTGAPAQ       Acknowledge/query                        
         JZ    *+8                                                              
         MVI   OD#GAPAQ,YESQ                                                    
*&&                                                                             
*&&UK                                                                           
         CLI   OD#GAPYN,C' '                                                    
         JH    SUPGAP10                                                         
         TM    RSTSTAT7,RSTGAPQY       GAP supplier controls                    
         JZ    *+8                                                              
         MVI   OD#GAPYN,YESQ                                                    
         TM    RSTSTAT7,RSTGAPQN                                                
         JZ    *+8                                                              
         MVI   OD#GAPYN,NOQ                                                     
*                                                                               
SUPGAP10 CLI   OD#GAPAQ,C' '                                                    
         JH    SUPGAP12                                                         
         TM    RSTSTAT7,RSTGAPAY       Acknowledge/query                        
         JZ    *+8                                                              
         MVI   OD#GAPAQ,YESQ                                                    
         TM    RSTSTAT7,RSTGAPAN                                                
         JZ    *+8                                                              
         MVI   OD#GAPAQ,NOQ                                                     
*                                                                               
*&&                                                                             
SUPGAP12 DS    0H                                                               
*&&US                                                                           
         CLI   OD#GAPYN,YESQ           Both settings set to yes?                
         JNE   *+12                                                             
         CLI   OD#GAPAQ,YESQ                                                    
         JE    EXITY                                                            
*&&                                                                             
*&&UK                                                                           
         CLI   OD#GAPYN,C' '           Both settings set?                       
         JNH   *+12                                                             
         CLI   OD#GAPAQ,C' '                                                    
         JH    EXITY                                                            
*&&                                                                             
         J     SUPGAP06                                                         
*                                                                               
CHKGPRR  MVC   LP_ERROR,=AL2(AE$INACC) No - display error                       
         LA    RF,TEMP2                                                         
         STCM  RF,7,LP_EATXT       Append supplier a/c code to errmsg           
         MVI   LP_ELTXT,L'ACTKULA                                               
         J     EXITH                                                            
         EJECT                                                                  
**********************************************************************          
* Check if the connected user can view the timesheet                 *          
**********************************************************************          
         SPACE 1                                                                
         USING GAPTABD,R4                                                       
CHKVIEW  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*CHKVIE*'                                                      
*                                                                               
         MVI   X#LLUIND,0          Clear limit list indicators                  
*                                                                               
         LA    R4,GAPAREA          loop through elements                        
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         J     CHKVW04                                                          
*                                                                               
CHKVW02  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
*                                                                               
CHKVW04  TM    ATSRERRS,TSEEOF     LOOK FOR A MAIN ENTRY MATCH                  
         JNZ   CHKVW14                                                          
*                                                                               
         CLI   GAPTDAT1,GAPTT2Q    SJ entry                                     
         JNE   CHKVW08                                                          
         CLC   SJACCNT,SPACES      Any SJ account on order?                     
         JH    CHKVW06                                                          
         OI    X#LLUIND,X#LLUICQ                                                
         J     CHKVW02                                                          
*                                                                               
CHKVW06  CLC   GAPTACC,SPACES      if 1st entry is spaces, means...             
         JNH   CHKVW07             no entries + default access=yes              
         LLC   RF,GAPTLEN                                                       
         SHI   RF,1+L'GAPTCOFF                                                  
         BASR  RE,0                                                             
         CLC   GAPTACC(0),SJACCNT                                               
         EX    RF,0(RE)            MATCH ON ACCOUNT                             
         JNE   CHKVW02                                                          
         CLC   GAPTCOFF,OD#OFF     MATCH ON OFFICE                              
         JNE   CHKVW02                                                          
CHKVW07  TM    GAPTSTA,GAPTSMQ     NEXT MAIN ENTRY, WE'RE OK                    
         JZ    *+12                                                             
         OI    X#LLUIND,X#LLUICQ                                                
         J     CHKVW02                                                          
         NI    X#LLUIND,X'FF'-X#LLUICQ                                          
         J     CHKVW02             OVERRIDE ENTRY                               
*                                                                               
CHKVW08  CLI   GAPTDAT1,GAPTT5Q    Media code                                   
         JNE   CHKVW12                                                          
         CLC   GAPTACT,SPACES      if 1st entry is spaces                       
         JNH   *+14                means default access=y + no limlist          
         CLC   SJACCNT,SPACES      Any SJ account on order?                     
         JH    CHKVW10                                                          
         OI    X#LLUIND,X#LLUIMQ                                                
         J     CHKVW02                                                          
*                                                                               
CHKVW10  LLC   RF,PPROLEN          Check match on media code in job             
         LA    RF,SJACCNT(RF)                                                   
         CLI   0(RF),C' '          Check order is for client/product            
         JNH   CHKVW11                                                          
         CLC   GAPTMEDI,0(RF)                                                   
         JNE   CHKVW02                                                          
CHKVW11  OI    X#LLUIND,X#LLUIMQ                                                
         J     CHKVW02                                                          
*                                                                               
CHKVW12  CLI   GAPTDAT1,GAPTT7Q    Expenditure type code                        
         JNE   CHKVW02             ignore 06 (SJ+off+med) and 08 (sup)          
         CLC   GAPTETYP,SPACES     if 1st entry is spaces                       
         JNH   *+14                means default access=y + no limlist          
         CLC   OD#ETY,GAPTETYP     Match on expenditure type?                   
         JNE   CHKVW02                                                          
         OI    X#LLUIND,X#LLUIEQ                                                
         J     CHKVW02                                                          
*                                                                               
CHKVW14  TM    X#LLUIND,X#LLUICQ+X#LLUIMQ+X#LLUIEQ Have access?                 
         JO    EXITY                                                            
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Edit whether extra data field is active                             *         
***********************************************************************         
                                                                                
EDTACTL  LM    R2,R4,LP_AINP                                                    
         USING XDFELD,R2                                                        
         LA    R3,XDFLCDAT                                                      
         MVI   0(R4),C'A'                                                       
         OC    XDFLCDAT,XDFLCDAT                                                
         JZ    EDTACTLY                                                         
         CLC   XDFLCDAT,OD#TODYC                                                
         JNL   EDTACTLY                                                         
         MVI   0(R4),C'N'                                                       
                                                                                
EDTACTLY LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* EDIT WHETHER DEFAULT IS SET                                         *         
***********************************************************************         
EDTSTAT  LM    R2,R4,LP_AINP                                                    
         USING XDFELD,R2                                                        
         OC    XDFLCDAT,XDFLCDAT                                                
         JZ    EDTSTAT5                                                         
         L     RF,ATWA                  RF=A(ON/OFFLINE TWA)                    
         CLC   XDFLCDAT,TODAYC-TWAD(RF)                                         
         JL    EDTSTATY                 ENTRY IS CUT OFF, CANNOT BE             
*                                                           DEFAULT             
EDTSTAT5 TM    XDFLSTAT,XDFLDFT                                                 
         JZ    EDTSTATY                                                         
         MVI   0(R4),C'Y'                                                       
*                                                                               
EDTSTATY LHI   R3,1                                                             
         STCM  R3,15,LP_OLEN                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Clear work area (RC=A(Work area), R1=L'Work area)                   *         
***********************************************************************         
                                                                                
CLRWRK   STM   RE,R1,12(RD)                                                     
         LR    R0,RC                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* GLOBAL EXITS                                                        *         
***********************************************************************         
                                                                                
SETCCC   JE    *+8                 SET CONVERSE CONDITION CODE                  
         CR    RE,RE               NOT EQUAL TO EQUAL/NOT ZERO TO ZERO          
         BR    RE                                                               
         LTR   RE,RE               EQUAL TO NOT EQUAL/ZERO TO NOT ZERO          
         BR    RE                                                               
                                                                                
MORE     MVI   LP_RMODE,LP_RMORE   SET NORE TO COME                             
         J     EXITY                                                            
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS & EXIT                   
         J     EXITY                                                            
                                                                                
QERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         J     EXITY                                                            
                                                                                
XERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         MVI   LP_EMSYS,6                                                       
         J     EXITN                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     CLEAR OUTPUT FIELD LENGTH & EXIT             
         J     EXITY                                                            
*                                                                               
EXITL    DS    0H                  SET CONDITION CODE TO LOW                    
EXITN    LHI   RE,0                SET CONDITION CODE TO NOT EQUAL              
         J     EXITCC                                                           
EXITY    LHI   RE,1                SET CONDITION CODE TO EQUAL                  
         J     EXITCC                                                           
EXITH    LHI   RE,2                SET CONDITION CODE TO HIGH                   
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                   GENERAL EXIT POINT                           
         EJECT                                                                  
***********************************************************************         
* LIST OF ACCOUNT FILES TO OPEN IN ALL SYSTEMS                        *         
***********************************************************************         
                                                                                
FILES    DS    0X                  ** FILE INFO **                              
         DC    C'ACCOUNT'          SYSTEM NAME FOR OPEN                         
                                                                                
         DC    C'nCTFILE '         FILE LIST                                    
         DC    C'UACCDIR '                                                      
         DC    C'UACCMST '                                                      
         DC    C'UACCARC '                                                      
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* GLOBAL LITERALS                                                     *         
***********************************************************************         
                                                                                
GLOBALS  DS    0D                                                               
                                                                                
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
***********************************************************************         
* MAP/INDICATORS TABLE                                                *         
***********************************************************************         
                                                                                
MAPTAB   DS    0XL4                                                             
                                                                                
MA#LLDL  DC    AL2(A#OCDL)         Order Combined Download                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
PZERO    DC    P'0'                                                             
ORDWC    DC    CL2'**'                                                          
BILWC    DC    CL2'99'                                                          
LDG1R    DC    CL2'1R'                                                          
FFS      DC    13X'FF'                                                          
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
                                                                                
APLTAB   DS    0XL1                                                             
         DC    B'11110000'         Order type/Office/Etype/Client               
         DC    B'10110000'         Order type/Etype/Client                      
         DC    B'11010000'         Order type/Office/Client                     
         DC    B'10010000'         Order type/Client                            
         DC    B'11100000'         Order type/Office/Etype                      
         DC    B'10100000'         Order type/Etype                             
         DC    B'11000000'         Order type/Office                            
         DC    B'10000000'         Order type                                   
         DC    B'01111000'         Client ord type/Office/Etype/Client          
         DC    B'01011000'         Client ord type/Office/Client                
         DC    B'00111000'         Client ord type/Client/Etype                 
         DC    B'00011000'         Client ord type/Client                       
         DC    B'01101000'         Client ord type/Office/Etype                 
         DC    B'00101000'         Client ord type/Etype                        
         DC    B'01001000'         Client ord type/Office                       
         DC    B'00001000'         Client ord type                              
         DC    B'01110000'         Office/Etype/Client                          
         DC    B'01010000'         Office/Client                                
         DC    B'00110000'         Client/Etype                                 
         DC    B'00010000'         Client                                       
         DC    B'01100000'         Office/Etype                                 
         DC    B'00100000'         Etype                                        
         DC    B'01000000'         Office                                       
         DC    B'00000000'         Agency                                       
         DC    X'FF'                                                            
                                                                                
STATAB   DC    AL1(STCODRFT),AL1(ORDSDRFT)                                      
         DC    AL1(STCOSUBM),AL1(ORDSSUBM)                                      
         DC    AL1(STCOPAPP),AL1(ORDSPAPP)                                      
         DC    AL1(STCOFAPP),AL1(ORDSAPPR)                                      
         DC    AL1(STCOREJ),AL1(ORDSOREJ)                                       
         DC    X'FF'                                                            
                                                                                
AUDTAB   DC    AL1(STCOADDD),C'*'                                               
         DC    AL1(STCOINPR),C'D'                                               
         DC    AL1(STCOSUBD),C'S'                                               
         DC    AL1(STCOPAPR),C'P'                                               
         DC    AL1(STCOREJT),C'R'                                               
         DC    AL1(STCOAPPD),C'A'                                               
         DC    AL1(STCOCOMP),C'C'                                               
         DC    AL1(STCODELT),C'L'                                               
         DC    AL1(STCOCAND),C'C'                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* KEY DRIVER TABLES                                                   *         
***********************************************************************         
                                                                                
ORDKEYT  LKKEY H,ORDKEY,SAVED      ** ORDER RECORD KEY DRIVER **                
         LKKEY LIT,ORDKTYP,ORDKTYPQ                                             
         LKKEY SIN,ORDKCPY,QAGENCY                                              
         LKKEY ALL,ORDKSP1                                                      
         LKKEY SIN,ORDKORD,OD#NUM                                               
         LKKEY NZR,ORDKSEQ                                                      
         LKKEY ALL,ORDKSP2                                                      
         LKKEY E                                                                
                                                                                
ORXKEYT  LKKEY H,ORDKEY,SAVED      ** ORDER EXTENSION REC KEY DRIVER **         
         LKKEY LIT,ORDKTYP,ORDKTYPQ                                             
         LKKEY SIN,ORDKCPY,QAGENCY                                              
         LKKEY ALL,ORDKSP1                                                      
         LKKEY SIN,ORDKORD,OD#NUM                                               
         LKKEY LIT,ORDKSEQ,ORDKEXTN                                             
         LKKEY ALL,ORDKSP2                                                      
         LKKEY E                                                                
                                                                                
AUDKEYT  LKKEY H,AUDKEY            ** AUDIT RECORD KEY DRIVER **                
         LKKEY LIT,AUDKTYP,AUDKTYPQ                                             
         LKKEY LIT,AUDKSUB,AUDKSUBQ                                             
         LKKEY SIN,AUDKCPY,QAGENCY                                              
         LKKEY LIT,AUDKAUDT,AUDKORD                                             
         LKKEY LIT,AUDKREMO,0                                                   
         LKKEY SIN,AUDKORDN,QORDNUM                                             
         LKKEY ALL,AUDKSEQ                                                      
         LKKEY E                                                                
                                                                                
XDFKEYT  LKKEY H,XDFKEY            ** XDATA RECORD KEY DRIVER **                
         LKKEY LIT,XDFKTYP,XDFKTYPQ                                             
         LKKEY LIT,XDFKSUB,XDFKSUBQ                                             
         LKKEY SIN,XDFKCPY,QAGENCY                                              
         LKKEY LIT,XDFKREM,0                                                    
         LKKEY ALL,XDFKOFF                                                      
         LKKEY ALL,XDFKORTY                                                     
         LKKEY ALL,XDFKCLI                                                      
         LKKEY ALL,XDFKETY                                                      
         LKKEY ALL,XDFKWC                                                       
         LKKEY ALL,XDFKMED                                                      
         LKKEY ALL,XDFKSCH                                                      
         LKKEY ALL,XDFKSEQ                                                      
         LKKEY E                                                                
                                                                                
XDLKEYT  LKKEY H,XDLKEY            ** XDLIST RECORD KEY DRIVER **               
         LKKEY LIT,XDLKTYP,XDLKTYPQ                                             
         LKKEY LIT,XDLKSUB,XDLKSUBQ                                             
         LKKEY SIN,XDLKCPY,QAGENCY                                              
         LKKEY LIT,XDLKREM,0                                                    
         LKKEY SIN,XDLKOFF,DOFF                                                 
         LKKEY SIN,XDLKORTY,DORTY                                               
         LKKEY SIN,XDLKCLI,DCLI                                                 
         LKKEY SIN,XDLKETY,DETY                                                 
         LKKEY SIN,XDLKWC,DWC                                                   
         LKKEY SIN,XDLKMED,DMED                                                 
         LKKEY SIN,XDLKSCH,DSCH                                                 
         LKKEY SIN,XDLKCODE,DCODE                                               
         LKKEY E                                                                
                                                                                
         EJECT                                                                  
         LTORG ,                                                                
***********************************************************************         
* OTHER LITERALS AND DECLARATIONS                                     *         
***********************************************************************         
                                                                                
ORDNLIT  DC    C'Order Number'                                                  
REQNLIT  DC    C'Requisition #'                                                 
UNCOLIT  DC    C'Show uncommitted'                                              
UNWCLIT  DC    C'Uncommitted by w/c'                                            
CONTLIT  DC    C'Apply contingency'                                             
DLSELIT  DC    C'Set of Downloads'                                              
DMODLIT  DC    C'Display Mode'                                                  
DCOPCUR  DC    C'Copy: currency'                                                
DCOPEXR  DC    C'Copy: exch rate'                                               
DIVDATE  DC    C'Invs: Inv date'                                                
LITOIEXP DC    C'Include Expense Orders'                                        
LITOIINT DC    C'Include Internal Orders'                                       
LITOIPRO DC    C'Include Production Orders'                                     
VWALLLIT DC    C'View all orders'                                               
SOVLLIT  DC    C'Search override limit list'                                    
OVLLIT   DC    C'Override limit list'                                           
                                                                                
LVALUES  DS    0F                  ** LITERALS MOVED TO SAVED **                
         DC    A(ORDKF)                                                         
         DC    A(FLTXDF)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    CL5'YNNNN'                                                       
         EJECT                                                                  
                                                                                
B#ACCREC EQU   3                   GENAREA FOR 'SUPER' ORDER                    
B#ORDREC EQU   4                   IO2 - ORDER RECORDS                          
B#XDFREC EQU   5                   IO3 - EXTRA DATA RECORDS                     
B#XDLREC EQU   6                   IO4 - EXTRA DATA LIST RECORDS                
B#AUD    EQU   7                   IO5 - AUDIT RECORD                           
B#GOBLK  EQU   8                   AGOBLOCB                                     
B#GOXBLK EQU   9                   AGOXBLCK                                     
B#GOBBLK EQU   10                  AGOBBLCK                                     
B#SVRDEF EQU   11                  ACBRA1F                                      
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)                                 
B#LP_D   EQU   12                  LP_D                                         
MAXEML   EQU   10                  maximum of 10 email addresses                
                                                                                
EOR      EQU   0                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER SAVED STORAGE                                        *         
***********************************************************************         
                                                                                
SAVED    DSECT ,                                                                
                                                                                
WVALUES  DS    0X                  ** LITERAL VALUES **                         
                                                                                
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
AORDKF   DS    A                   A(ORDKF)                                     
AFLTXDF  DS    A                   A(FLTXDF)                                    
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
                                                                                
WNOQ     DS    CL1                                                              
WYESQ    DS    CL1                                                              
WDLDEF   DS    CL5                                                              
                                                                                
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
ACURNTRY DS    A                                                                
SPOINTER DS    A                                                                
SBYTE1   DS    XL1                 General saved byte                           
X#LLUIND DS    XL1                 Limit list indicator                         
X#LLUICQ EQU   X'80'               Client limlist                               
X#LLUIMQ EQU   X'40'               Media limlist                                
X#LLUIEQ EQU   X'20'               Expenditure type limlist                     
SWCODE   DS    CL2                                                              
SHALF1   DS    H                                                                
SHALF2   DS    H                                                                
SNAMES   DS    CL(15+36)                                                        
SXNAME   DS    CL36                                                             
SLLEVS   DS    0XL4                Supplier ledger lengths                      
SLLEV1   DS    XL1                 level 1                                      
SLLEV2   DS    XL1                 level 2                                      
SLLEV3   DS    XL1                 level 3                                      
SLLEV4   DS    XL1                 level 4                                      
MYPL16   DS    PL16                                                             
                                                                                
SDMODE   DS    XL1                 Display mode                                 
SDMODCQ  EQU   X'80'                 - copy                                     
SDMODFQ  EQU   X'40'                 - currency/rate on copy                    
SDMPDFQ  EQU   X'04'                 - PDF audit data                           
SDMODBQ  EQU   X'02'                 - BrandOcean Invoices                      
SDMODIQ  EQU   X'01'                 - Invoice Log                              
                                                                                
GAPAREA  DS    XL(GAPTLNQ)         Area to use for buffer rec                   
ATSRERRS DS    XL1                 Error area for buffer                        
GAPLPARM DS    XL1                 Additonal parameter for GAPLST calls         
TSARABUF DS    XL(TSPXTNL)         TSAR block for approval buffer               
TSAROBUF DS    XL(TSPXTNL)         TSAR block for optimisation buffer           
         DS    0D                                                               
COUNT    DS    H                                                                
CESTNUM  DS    CL6                                                              
CESTLOC  DS    XL1                                                              
XDOEVE   DS    PL6                                                              
XDOEST   DS    PL6                                                              
XDOTRX   DS    PL6                                                              
XDOEXP   DS    PL6                                                              
XDOTIM   DS    PL6                                                              
XDOORD   DS    PL6                                                              
XDOOTX   DS    PL6                                                              
RTIMTUPT DS    XL(TMSTTABL)        R time settings from GETOPT/GETCAP           
BTIMTUPT DS    XL(TMSTTABL)        B time settings from GETOPT/GETCAP           
TMSTNMB  EQU   (*-RTIMTUPT)/TMSTTABL When doing estimate checking               
NTIMTUPT DS    XL(TMSTTABL)        N time settings from GETOPT/GETCAP           
TMSTNMN  EQU   (*-RTIMTUPT)/TMSTTABL                                            
                                                                                
APIND1   DS    XL1                 Approver indicator 1                         
APIUNA   EQU   X'80'                                                            
                                                                                
GUCAIND  DS    XL1                                                              
GUCAHRQ  EQU   X'80'                HR required                                 
GUCAUHQ  EQU   X'40'                use HR as exists                            
                                                                                
SCOPCUR  DS    CL3                                                              
SCOPEXR  DS    0XL7                                                             
         DS    XL1                                                              
SCOPRAT  DS    XL5                                                              
SCOSHFT  DS    XL1                                                              
OD#TODYP DS    PL3                  local today's date                          
OD#TODYC DS    XL2                                                              
                                                                                
SORDEBY  DS    CL1                                                              
ORDTYPE  DS    CL1                                                              
ORDEML   DS    CL1                  Send email                                  
XL#ORPF  DS    XL(ORDPRFL)                                                      
                                                                                
SVXDFKEY DS    XL(L'IOKEY)           SAVED XDFKEY                               
SVORDKY  DS    XL(L'IOKEY)           SAVED ORDREC KEY                           
SVAUDKY  DS    XL(L'IOKEY)           SAVED AUDIT KEY                            
SAVEKEY1 DS    XL(L'IOKEY)           OVERLAY SAVED KEYS                         
SAVEKEY2 DS    XL(L'IOKEY)                                                      
SAVEKEY3 DS    XL(L'IOKEY)                                                      
SAVEKEY4 DS    XL(L'IOKEY)                                                      
                                                                                
SDAREA   DS    0X                    ** Save data area **                       
*                                                                               
SAUFRS   DS    CL1                   from status                                
SAUTOS   DS    CL1                   to status                                  
SAUDTE   DS    XL3                   date                                       
SAUTIM   DS    CL6                   time                                       
SAUUSR   DS    CL10                  userID                                     
SAUPID   DS    CL8                   PersonID                                   
SAUPER   DS    CL16                  person first name                          
SAUPEL   DS    CL16                  person last name                           
SAUTCT   DS    PL6                   total on W type change                     
SAUTXT   DS    CL50                  text/comment                               
SAUACO   DS    CL50                  additional comments                        
SAUAPP   DS    CL1                   Application byte                           
SAUDITQ  EQU   *-SDAREA                                                         
SVGSKY   DS    CL(STCCLNAM-STCUSER)  Saved key for main aud elem                
SDAREAL  EQU   *-SDAREA                                                         
         DS    XL(SIDATAQ-SDAREAL)   Overflow trap                              
                                                                                
         ORG   SDAREA                                                           
SXDATA   DS    0X                                                               
SXDCODE  DS    XL3                   field code                                 
SXDTYPE  DS    CL1                   field type                                 
SXDAMNT  DS    PL6                   amount                                     
SXDDATA  DS    CL90                  edited data                                
SXDXCDE  DS    CL(L'XDFCODE)         extra data field code                      
SXDXNAM  DS    CL30                  field name                                 
SXDATAQ  EQU   *-SXDATA                                                         
         DS    XL(SIDATAQ-SXDATAQ)   Overflow trap                              
                                                                                
         ORG   SDAREA                                                           
SIDATA   DS    0X                                                               
SICIND   DS    CL1                                                              
SICIYQ   EQU   YESQ                                                             
SICINQ   EQU   NOQ                                                              
SICIPQ   EQU   C'P'                                                             
SICIIQ   EQU   C'I'                                                             
SISTAT   DS    CL8                                                              
SIUNIT   DS    CL15                                                             
SIUNLA   DS    CL15                                                             
SINAME   DS    CL36                Item name from item record                   
SINAME2  DS    CL36                Item foreign name from item record           
SIETX1   DS    CL180               Item extra text from item record             
SIETX2   DS    CL180               Item foreign extra text from itm rec         
SISEQN   DS    XL3                                                              
SIANUM   DS    CL4                                                              
SIMULT   DS    PL6                                                              
SIPRIC   DS    PL6                                                              
SIDESC   DS    CL50                Overriden item name from order               
SIDATAQ  EQU   *-SIDATA                                                         
                                                                                
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
                                                                                
MAPI1    DS    X                   ** MAP INDICATOR BYTE 1 **                   
MAP#OCDL EQU   MAPI1                                                            
MAPSOCDL EQU   X'80'               Order combined d/load                        
                                                                                
MAPI2    DS    X                   ** MAP INDICATOR BYTE 2 **                   
                                                                                
RUNI     DS    0XL(L'RUNI1)                                                     
                                                                                
RUNI1    DS    X                   ** RUN INDICATOR BYTE 1 **                   
                                                                                
         DS    0F                                                               
QVALUES  DS    0XL256              ** REQUEST VALUES **                         
                                                                                
QAGENCY  DS    XL1                 CUXCPY agency code                           
QORDNUM  DS    CL6                 Order number                                 
QREQNUM  DS    CL6                 Requisition number                           
QUNCOMM  DS    CL1                 Return est/uncommitted                       
QEUCJOB  DS    CL12                Est/uncommitted - SJ account code            
QEUCOFF  DS    CL2                 Est/uncommitted - SJ office code             
QEUCGEN  DS    CL6                 Est/uncommitted - Global estimate no         
QEUBYWC  DS    CL1                 Est/uncommitted - split by w/c               
QEUCONT  DS    CL1                 Est/uncommitted - apply contingency          
QALL     DS    CL1                 View all orders                              
QORDSOVL DS    CL1                 Search override limlist - srch over          
QISPDF   DS    CL1                 IsPDF mode?                                  
QODIEXP  DS    CL1                 Security: exclude expense orders             
QODIINT  DS    CL1                 Security: exclude internal orders            
QODIPRO  DS    CL1                 Security: exclude production orders          
QORDOVL  DS    CL1                 Override limit list - override               
QDLSETS  DS    0CL5                                                             
QDLSITM  DS    CL1                                                              
QDLSTXT  DS    CL1                                                              
QDLSAUD  DS    CL1                                                              
QDLSXDF  DS    CL1                                                              
QDLSORD  DS    CL1                                                              
QDIMODE  DS    CL1                                                              
QCOPCUR  DS    CL3                                                              
QCOPEXR  DS    CL14                                                             
QIVDATE  DS    CL8                 Invoices call:invoice date                   
QOFFICE  DS    CL(L'TRNOFFC)       Office                                       
QORDTYP  DS    CL1                 Order Type                                   
QCLI     DS    CL5                 Client                                       
QETY     DS    CL3                 Expenditure Type                             
QWCIND   DS    X                                                                
*&&US                                                                           
QWCMAXQ  EQU   20                                                               
*&&                                                                             
*&&UK                                                                           
QWCMAXQ  EQU   6                                                                
*&&                                                                             
QAWC     DS    AL3                 Workcode                                     
QMED     DS    CL1                 Media                                        
QPRO     DS    CL5                 Product                                      
QSCH     DS    CL8                 Scheme                                       
                                                                                
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
         DS    0F                                                               
DVALUES  DS    0X                  ** DERIVED VALUES **                         
DCODE    DS    CL(L'XDFCODE)       XDFCODE                                      
DOFF     DS    XL(L'XDFKOFF)       XDFKOFF                                      
DORTY    DS    XL(L'XDFKORTY)      XDFKORTY                                     
DCLI     DS    XL(L'XDFKCLI)       XDFKCLI                                      
DETY     DS    XL(L'XDFKETY)       XDFKETY                                      
DWC      DS    XL(L'XDFKWC)        XDFKWC                                       
DMED     DS    XL(L'XDFKMED)       XDFKMED                                      
DSCH     DS    XL(L'XDFKSCH)       XDFKSCH                                      
                                                                                
         DS    H                                                                
                                                                                
DVALUEL  EQU   *-DVALUES                                                        
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
AALL     DS    AL3                 ALL VALUE WMP ENTRY                          
ANZR     DS    AL3                 NON-ZERO VALUE WMP ENTRY                     
                                                                                
OVALUES  DS    0D                  ** OUTPUT VALUES **                          
OD#ADDR  DS    F                   ADDRESS OF APLIMITS                          
OD#LSTEL DS    A                                                                
                                                                                
OA_TIME  DS    CL6                 AUDIT TIME                                   
OA_TYP   DS    XL1                 EXTRA DATA TYPE FOR AUDIT                    
OA_SJULA DS    CL14                AUDIT SJ ACCOUNT                             
OA_2PULA DS    CL14                AUDIT 2P ACCOUNT                             
OA_2DULA DS    CL14                AUDIT 2D ACCOUNT                             
OXDFPTRS DS    XL(L'XDFRPTR+L'XDFSEQ)                                           
OD#TYP   DS    CL1                 ORDER TYPE                                   
OD#NUM   DS    CL6                 ORDER NUMBER                                 
OD#REQ   DS    CL6                 REQUISITION NUMBER                           
OD#EXP   DS    CL14                EXPENSE ACCOUNT                              
OD#EXPN  DS    CL36                AND NAME                                     
OD#EXPF  DS    CL36                AND FOREIGN NAME                             
OD#CLI   DS    CL5                 SJ CLIENT                                    
OD#CLIN  DS    CL36                AND NAME                                     
OD#CLOFF DS    CL2                 CLIENT OFFICE                                
OD#CLIA1 DS    CL(L'ADRADD1)       ADDRESS LINE 1                               
OD#CLIA2 DS    CL(L'ADRADD2)       ADDRESS LINE 2                               
OD#CLIA3 DS    CL(L'ADRADD3)       ADDRESS LINE 3                               
OD#CLIA4 DS    CL(L'ADRADD4)       ADDRESS LINE 4                               
OD#CLIA5 DS    CL(L'ADRADD5)       ADDRESS LINE 5                               
OD#PRO   DS    CL5                 SJ PRODUCT                                   
OD#PRON  DS    CL36                AND NAME                                     
OD#PROA1 DS    CL(L'ADRADD1)       ADDRESS LINE 1                               
OD#PROA2 DS    CL(L'ADRADD2)       ADDRESS LINE 2                               
OD#PROA3 DS    CL(L'ADRADD3)       ADDRESS LINE 3                               
OD#PROA4 DS    CL(L'ADRADD4)       ADDRESS LINE 4                               
OD#PROA5 DS    CL(L'ADRADD5)       ADDRESS LINE 5                               
OD#JOB   DS    CL7                 SJ JOB                                       
OD#JOBN  DS    CL36                AND NAME                                     
OD#JOBL  DS    CL1                 JOB LOCKED/CLOSED STATUS                     
OD#JOBA1 DS    CL(L'ADRADD1)       ADDRESS LINE 1                               
OD#JOBA2 DS    CL(L'ADRADD2)       ADDRESS LINE 2                               
OD#JOBA3 DS    CL(L'ADRADD3)       ADDRESS LINE 3                               
OD#JOBA4 DS    CL(L'ADRADD4)       ADDRESS LINE 4                               
OD#JOBA5 DS    CL(L'ADRADD5)       ADDRESS LINE 5                               
OD#SRAC  DS    CL14                SR ACCOUNT CODE                              
OD#SRAN  DS    CL36                AND NAME                                     
OD#SUP   DS    CL14                SUPPLIER                                     
OD#SUPN  DS    CL36                AND NAME                                     
OD#SUPF  DS    CL36                AND FOREIGN NAME                             
OD#DEP   DS    CL12                DEPARTMENT                                   
OD#DEPN  DS    CL36                AND NAME                                     
OD#DEPF  DS    CL36                AND FOREIGN NAME                             
OD#PER   DS    CL12                PERSON (2P CODE)                             
OD#PERN  DS    CL36                AND NAME                                     
OD#PERF  DS    CL36                AND FOREIGN NAME                             
OD#STT   DS    CL5                 STATUS BLOCK                                 
OD#DTE   DS    XL3                 ORDER DATE                                   
OD#AGYC  DS    CL3                 Agency currency                              
OD#2DAC  DS    0CL4                2D account for office look up                
OD#2DUL  DS    CL2                 2D unit ledger                               
OD#OFF   DS    CL2                 OFFICE CODE                                  
OD#ETY   DS    CL3                 EXPENDITURE TYPE                             
OD#TOT   DS    PL6                 ORDER TOTAL AMOUNT IN AGY CURR               
OD#ITOT  DS    PL6                 ORDER INVOICED TOTAL AMOUNT IN AGY           
OD#TOTC  DS    PL6                 ORDER TOTAL AMOUNT IN FOREIGN CURR           
OD#ITOTC DS    PL6                 ORDER INVOICED TOTAL AMOUNT IN F/CUR         
OD#ATO   DS    CL36                ATTENTION TO                                 
OD#CUR   DS    CL3                 CURRENCY CODE                                
OD#RAT   DS    XL7                 CURRENCY RATE (XOUT AFCX)                    
OD#RATC  DS    CL14                CURRENCY RATE CHARACTER FORMAT               
OD#FCDP  DS    XL1                 CURRENCY DECIMAL PLACES                      
OD#WC#   DS    XL1                 NUMBER OF WORK CODES                         
OD#AUT   DS    CL15                AUTHORISED BY                                
OD#RBYE  DS    XL3                 REQUIRED BY (DATE EBUYER)                    
OD#TXTM  DS    CL120               TEXT (MATCHING)                              
OD#TXTP  DS    CL240               TEXT (COMMON PRINTING)                       
OD#SAD1  DS    CL26                SUPPLIER ADDRESS LINE 1                      
OD#SAD2  DS    CL26                SUPPLIER ADDRESS LINE 2                      
OD#SAD3  DS    CL26                SUPPLIER ADDRESS LINE 3                      
OD#SAD4  DS    CL26                SUPPLIER ADDRESS LINE 4                      
OD#AGTC  DS    CL12                AGENT'S CODE                                 
OD#AGTN  DS    CL36                AGENT'S NAME                                 
OD#AGT1  DS    CL26                AGENT'S ADDRESS LINE 1                       
OD#AGT2  DS    CL26                AGENT'S ADDRESS LINE 2                       
OD#AGT3  DS    CL26                AGENT'S ADDRESS LINE 3                       
OD#AGT4  DS    CL26                AGENT'S ADDRESS LINE 4                       
OD#RBYO  DS    CL20                REQUIRED BY (TEXT NON EBUYER)                
OD#RAPI  DS    XL2                 ORDER RAISER 2 BYTE PID                      
OD#RAIC  DS    CL8                 ORDER RAISED BY PERSON CODE                  
OD#RAIN  DS    CL16                & FIRST NAME                                 
OD#RAIL  DS    CL58                & LAST NAME                                  
OD#RAIE  DS    CL44                & EMAIL ADDRESS                              
OD#RAIT  DS    CL5                 & PHONE EXTENSION                            
OD#OWNC  DS    CL8                 ORDER 'OWNER' PERSON CODE                    
OD#OWNN  DS    CL16                & FIRST NAME                                 
OD#OWNL  DS    CL58                & LAST NAME                                  
OD#MAPP  DS    XL1                 # OF MISSING APPROVALS                       
OD#IDNO  DS    XL2                 ID NUMBER                                    
OD#ETYN  DS    CL36                EXPENDITURE TYPE NAME                        
OD#ETYF  DS    CL36                EXPENDITURE TYPE FORIGN NAME                 
OD#ESTN  DS    CL6                 ESTIMATE NUMBER                              
OD#PRTA  DS    CL1                 PRINT AMOUNTS ON PDF                         
OD#PRTT  DS    CL1                 PRINT TEXTS ON PDF                           
OD#FNIU  DS    CL1                 FOR. NAME IN USE: S(UPPL), J(OB)             
OD#EOWC  DS    CL2                 EXPENSE ORDER W/C                            
OD#EOWD  DS    CL15                EXPENSE ORDER W/C DESCRIPTION                
OD#EOWF  DS    CL36                EXPENSE ORDER W/C FOREIGN NAME               
OD#EOWX  DS    CL36                EXPENSE ORDER W/C EXTRA NAME                 
OD#PCLN  DS    CL36                PRINT CLIENT NAME                            
OD#PPRN  DS    CL36                PRINT PRODUCT NAME                           
OD#FCOD  DS    CL8                 FORMAT CODE (PDF)                            
OD#ONAM  DS    CL100               ORDER NAME                                   
OD#ODAD  DS    CL240               ORDER DELIVERY ADDRESS LINE                  
OD#SFAX  DS    CL26                SUPPLIER FAX NUMBER                          
OD#WCDS  DS    CL36                WORK-CODE DESCRIPTION                        
OD#WCFDS DS    CL36                WORK-CODE FOREIGN DESCRIPTION                
OD#SEMA  DS    CL50                SUPPLIER EMAIL ADDRESS                       
OD#ENAM  DS    CL50                ESTIMATE NAME                                
OD#RFMT  DS    CL8                 REPORT FORMAT                                
OD#APSTA DS    CL1                 APPROVAL STATUS TO VIEWER                    
OD#ANOAP EQU   C'4'                NO APPROVAL BY PERSON VIEWING                
OD#AAWAT EQU   C'3'                AWAITING                                     
OD#AREJC EQU   C'2'                REJECTED                                     
OD#AAPPR EQU   C'1'                APPROVED                                     
OD#ASTA  DS    CL1                 ORDER APPROVAL STATUS                        
OD#IVTC  DS    CL5                 SUPPLIER VAT CODE (GER)                      
OD#IKSV  DS    CL4                 SUPPLIER KSV %                               
OD#IDSC  DS    CL4                 SUPPLIER DISCOUNT %                          
OD#DDXP  DS    CL8                 SUPPLIER DUE DATE FORMULA                    
OD#INUM  DS    PL6                 PRIOR INVOICES (NUMBER)                      
OD#HINUM DS    PL6                 HIGHEST PRIOR INVOICES (NUMBER)              
OD#HIPND DS    XL1                 HIGHEST PENDING INVOICES (NUMBER)            
OD#CSTG  DS    CL1                 COSTING GROUP                                
OD#STAFF DS    CL1                 STAFF REQUIRED                               
OD#DEPT  DS    CL1                 DEPT REQUIRED                                
OD#PROR  DS    CL1                 PRODUCT REQUIRED                             
OD#JOBR  DS    CL1                 JOB REQUIRED                                 
OD#MILES DS    CL1                 MILEAGE REQUIRED                             
OD#JLEX  DS    CL1                 JOB LOCKED FROM EXTERNALS                    
OD#GAPYN DS    CL1                 GAP IN USE FOR THIS ORDER?                   
OD#GAPAQ DS    CL1                 GAP ACKNOWLEDGE/QUERY?                       
OD#GAPST DS    XL1                 GAP STATUS                                   
OD#GAPSD DS    PL3                 SENT DATE                                    
OD#GAPED DS    PL3                 EXPIRY DATE                                  
OD#APPL1 DS    PL6                 APPROVAL LIMIT                               
OD#UNCOM DS    PL6                 UNCOMMITTED AMOUNT                           
OD#UNWC  DS    CL2                 WORK CODE OF UC                              
OD#UNAMT DS    PL6                 AMOUNT OF WC UC                              
OD#SUPL  DS    CL1                 SUPPLIER ACCOUNT LOCKED Y/N                  
OD#JLTS  DS    CL1                 JOB LOCKED FROM T/S                          
OD#SBPID DS    CL8                 PID OF SUBMITTER                             
OD#RJCOM DS    CL(L'STCOCOM)       REJECTION COMMENTS                           
OD#RJFNM DS    CL16                REJECTERS FIRST NAME                         
OD#RJMNM DS    CL16                REJECTERS MIDDLE NAME                        
OD#RJLNM DS    CL58                REJECTERS LAST NAME                          
OD#RJDAT DS    PL3                 DATE OF REJECTION                            
OD#RJTIM DS    CL6                 TIME OF REJECTION                            
OD#GAPEM DS    CL50                EMAIL ADDRESS                                
         DS    (MAXEML-1)CL(L'OD#GAPEM)                                         
                                                                                
OD#APBLK DS    XL1000              APLIMITS FOR ORDER                           
*                                                                               
*                                                                               
OD#LENQ  EQU   *-OD#TYP                                                         
OVALUESL EQU   *-OVALUES                                                        
                                                                                
         EJECT                                                                  
***********************************************************************         
* Uncommitted amount by wok code table                                *         
***********************************************************************         
                                                                                
UCBYWCD  DSECT                                                                  
UCBYWCWC DS    CL2                                                              
UCBYWCAM DS    PL6                                                              
UCBYWCLQ EQU   *-UCBYWCD                                                        
                                                                                
***********************************************************************         
* Optimisation buffer record layout                                   *         
***********************************************************************         
                                                                                
OB_D     DSECT                                                                  
                                                                                
OB_KEY   DS    XL64                Record key                                   
                                                                                
OB_ERROR DS    XL2                 Error number (Zero=OK)                       
                                                                                
OB_DATA  DS    0X                  Data starts here                             
OB_NAME  DS    CL(L'NAMEREC)       Record name                                  
OB_OTHER DS    0X                  Other data                                   
         ORG   OB_D+256                                                         
                                                                                
OB_DATAL EQU   *-OB_ERROR                                                       
OB_LNQ   EQU   *-OB_D                                                           
                                                                                
***********************************************************************         
* INCLUDED DSECTS                                                     *         
***********************************************************************         
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
         PRINT ON                                                               
                                                                                
CONBLKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
                                                                                
APLTABD  DSECT                                                                  
APLTSTAT DS    CL1                                                              
APLTORT1 EQU   X'80'               Order type - expense prod inter arts         
APLTOFFC EQU   X'40'               Office                                       
APLTETYP EQU   X'20'               Expenditure type                             
APLTCLI  EQU   X'10'               Client                                       
APLTORT2 EQU   X'08'               Order type - client or non client            
APLTABL  EQU   *-APLTABD           length of entry                              
                                                                                
APTABNTQ EQU   L'APLSTAT+L'APLVAL+L'APLNUM+L'APLPREV                            
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093ACBRA1F   11/02/20'                                      
         END                                                                    
