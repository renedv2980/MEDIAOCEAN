*          DATA SET ACBRA2E    AT LEVEL 025 AS OF 03/20/20                      
*PHASE T6242EA                                                                  
                                                                                
ACBRA2E  TITLE 'Aura Orders - List and Search Server'                           
                                                                                
* Level change comments                                                         
* ---------------------                                                         
*TKLU 001 04Nov14 <RD004703> New server for Aura Orders List & Search           
*MPEN     21Jan15 <RD005884> New aura orders approver lookup                    
*                  <RD006166> Fix for approver lookup                           
*                  <RD006297> Don't show WC level error if job error            
*         09MAR15 <RD006486> Fix CHKEST bug                                     
*         11MAR15 <RD006456> New error message for highest revision             
*         11MAR15 <RD006387> Fix for no current estimate                        
*MPEN     12MAR15            Update to use GAPLST for limit list                
*NSHE 002 02APR15 <DSRD-6620> Amend CHKVIEW routine to deal with GAPLST         
*MPEN 003 07APR15 <DSRD-6813> Fix bug in reading estimate number                
*TKLU                         Add tracing for approver lookup                   
*MPEN 004 09APR15 <DSRD-6847> Make sure estimate number is passed               
*MPEN 005 13APR15 <DSRD-6835> Fix bug in reading approver with supplier         
*TKLU             <RD006910> Skip deleted orders                                
*MPEN     15APR15 <RD006902>  Increase maximum number of approvers              
*NSHE 006 23APR15 <RD006969>  Whenever editing always treat as submittr         
*TKLU 007 06MAY15 <RD007084>  Allow TSTCPJS orders through that have no         
*                             job set (cli/pro only)?                           
*                 <RD007099>  OrderList request parm for order types            
*NSHE 008 12Jun15 <DSRD-7532> Always go through MYAPSTA rountine                
*JFOS 009 12Nov15 <PCA01989>  Support Limit Account Access                      
*TKLU     03Dec15 <RD009622>  Order Name Search Support (wip)                   
*TKLU     08Dec15 <RD009698>  OrderList additional request parameters           
*TKLU     06JAN15             Prepare for general IO tracing                    
*TKLU 010 21Jan16 <RD010161>  Check/skip duplicate orders (name search)         
*                 <RD010165>  Staff/department search fix                       
*TKLU 013 27Jan15 <RD010160>  Performance improvements (GETOPT)                 
*                 <RD010275>  Approver code for actually approved only          
*NSHE 014 17May16 <RD011410>  Ensure applic in approver list is binary          
*MPEN     21Jun16 <RD001915>  Ensure approver level not 0                       
*NSHE 015 04Feb16 <RD014792>  Remove QA system date as not needed               
*NSHE     04Feb16 <RD-14924> Allow search on closed + locked jobs only          
*MPEN 016 12Jul17 <RD-16318>  Skip estimate check for inv/exp                   
*NSHE 017 21Jul17 <RD-16407>  Show workcode for expense order                   
*NSHE 018 11Aug17 <RD-16359>  Show new status as cancelled                      
*MPEN 019 17Oct17 <RD-17225>  Fix for incorrect order status                    
*RGUP 020 14Dec17 <RD-17701>  Fix for estimate name from order list             
*MPEN 021 09Apr18 <RD-18733>  Limlist saved search filtering                    
*NSHE 022 21Jun18 DSRD-19490  Fix order approvals looks up                      
*MPEN 023 01Jul19 <RD-22971>  Include GAP details in order list/search          
*ABID 024 05DEC19 <RD-23587>  AMEND APPROVER RECORD FOR ORDERS AND              
*                             INVOICES TO ACCEPT OFFICE AND OLIST               
*ABID 025 10DEC19 <DSRD-24764> Increase Maximum number of OLIST                 
*                               associated with OFFICE TO 100                   
*ABID 026 21MAR20 <SPEC-43014> FIX ORDER NAME WITH LEADING SPACES               
*                              ISSUE                                            
**********************************************************************          
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,           +        
               SLOWLIST=SLOWS,WORKERKEY=ACBO,ABENDLIST=FAILS,          +        
               SYSPHASE=X'0624',SYSTEM=ACCSYSQ,APPEND=Y,               +        
               SERVERTYPE=TSTACBO,SEGMENT=Y,LOADFACSOFF=Y,             +        
               BLOCKS=(B#WORKD,WORKD,                                  +        
               B#SAVED,SAVED,                                          +        
               B#LP_D,LP_D,                                            +        
               B#TWAD,TWAD,                                            +        
               B#ORDREC,ORDRECD,                                       +        
               B#PIDREC,PIDRECD,                                       +        
               B#OSTREC,OSTPASD,                                       +        
               B#OSUPAS,OSUPASD,                                       +        
               B#OSJPAS,OSJPASD,                                       +        
               B#SVRDEF,SVRDEF)                                                 
                                                                                
SLOWS    DC    C':'                                                             
FAILS    DC    C':'                                                             
                                                                                
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO2E**,CLEAR=YES,RR=RE                                       
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP         R7=A(RUNPARMS)                               
         USING RUNPARMD,R7                                                      
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   INIT02                                                           
         ICM   R9,15,LP_ABLK1      ROOT SETS A(WORKD)          BLOCK #1         
         JZ    *+2                                                              
         ICM   R8,15,RSVRSAVE      R8=A(7K SAVE AREA)                           
         J     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE         A(64K SAVE AREA)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)              BLOCK #1         
         XR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)        R8=A(46K SAVE AREA)                          
                                                                                
INIT04   ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)              BLOCK #2         
         MVC   ATWA,LP_ATWA                                                     
                                                                                
         USING CPXELD,SCPXEL                                                    
         USING CPYELD,SCPYEL                                                    
         MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   APRINTER,RPRINTER                                                
         ST    R6,ARUNFACS                                                      
         DROP  R6,R7                                                            
*                                                                               
         MVI   TWAMODE,0                                                        
         ST    R5,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         L     R1,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R1                                                      
         CLI   RUNPMODE,RRUNSTRQ   FIRST FOR RUN                                
         JE    RUNSTR                                                           
         CLI   RUNPMODE,RPRCWRKQ   PROCESS WORK                                 
         JE    PRCWRK                                                           
         CLI   RUNPMODE,RRUNREQQ   RUN REQUEST                                  
         JE    RUNREQ                                                           
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   L     R0,ATIA                                                          
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    RUNSTR02            NO                                           
         MVC   AROUT1,LP_AUIR1     SET LOCAL INDEX ROUTINE ADDRESSES            
         MVC   AROUT2,LP_AUIR2     WHICH WERE LOADED BY MASTER SERVER           
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE SERVER WORKING STORAGE            
         L     RF,AMASTC                                                        
         MVC   VACCEMU,MCVACEMU-MASTD(RF)                                       
         J     RUNSTR04                                                         
                                                                                
RUNSTR02 GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
         L     RF,AACCFACS                                                      
         MVC   VACCEMU,X_AACCEMU-X_ACCFACSD(RF)                                 
                                                                                
RUNSTR04 OI    LP_AIND1,LP_AICOM   COMMA OK FOR LIST DELIMITERS                 
         MVC   LP_BLKS+((B#ORDREC-1)*L'LP_BLKS)(L'LP_BLKS),AIO2                 
         MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
         MVC   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)(L'LP_BLKS),LP_ASVR              
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
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   LA    R0,SAVEVAR                                                       
         LHI   R1,SAVEVARL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         ZAP   OA_EJLE,PZERO       Clear packed fields                          
         ZAP   OA_APAL,PZERO                                                    
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    EXITY                                                            
                                                                                
         LA    R0,WORKD            CLEAR I/O AREAS                              
         AHI   R0,IOAREA1-WORKD                                                 
         LHI   R1,IOAREASL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   MVI   GIND2,GI2EBUY                                                    
         GOTOR (#CPYINI,ACPYINI)   (RE)INITIALISE COMPANY VALUES                
         GOTOR (#ORDPRF,AORDPRF),DMCB,ORDPRFS                                   
         GOTO1 VDICTATE,DMCB,C'LL  ',DCDICTL,DSDICTL                            
         GOTOR VDATCON,DMCB,(5,0),(1,XL#TODP)                                   
         GOTOR (RF),(R1),(5,0),(2,XL#TODC)                                      
                                                                                
RUNREQ01 MVC   AGENCY,CUXCPY                                                    
         MVC   PERSON,CCTPID                                                    
                                                                                
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
RUNREQ04 BCTR  R0,RE                                                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    RUNREQX                                                          
                                                                                
         L     R0,AGOBLOCB                                                      
         AHI   R0,GOADM-GOBLOCK                                                 
         LHI   R1,GOBLOCKX-GOBLOCKD                                             
         SHI   R0,GOADM-GOBLOCK                                                 
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,AMASTC           SET TRACE OPTION                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,CTFILE,(4,0),0                               
                                                                                
RUNREQX  GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY               EXIT BACK TO DDLINK                          
         EJECT                                                                  
**********************************************************************          
* Order list views                                                   *          
**********************************************************************          
                                                                                
REQOLVD  LKREQ H,A#AOLD,OUTAOLD,NEXTREQ=REQOALK                                 
View     LKREQ F,01,(D,B#SAVED,R_OLVIEW),CHAR,OLEN=L'R_OLVIEW,         +        
               MAXLEN=L'R_OLVIEW,TEXT=(*,L_OLVIEW),COL=*                        
ApStat   LKREQ F,02,(I,B#SAVED,R_OLAIND),CHAR,TEXT=(*,L_OLASTA),       +        
               OLEN=1,LIST=F,SORT=NO                                            
MaStat   LKREQ F,03,(I,B#SAVED,R_OLMIND),CHAR,TEXT=(*,L_OLMSTA),       +        
               OLEN=1,LIST=F,SORT=NO                                            
Client   LKREQ F,04,(D,B#SAVED,R_OLCLIC),CHAR,OLEN=L'R_OLCLIC,         +        
               MAXLEN=L'R_OLCLIC,TEXT=(*,L_OLCLIC),COL=*                        
Product  LKREQ F,05,(D,B#SAVED,R_OLPROC),CHAR,OLEN=L'R_OLPROC,         +        
               MAXLEN=L'R_OLPROC,TEXT=(*,L_OLPROC),COL=*                        
Job      LKREQ F,06,(D,B#SAVED,R_OLJOBC),CHAR,OLEN=L'R_OLJOBC,         +        
               MAXLEN=L'R_OLJOBC,TEXT=(*,L_OLJOBC),COL=*                        
Supplier LKREQ F,07,(D,B#SAVED,R_OLSUPC),CHAR,OLEN=L'R_OLSUPC,         +        
               MAXLEN=L'R_OLSUPC,TEXT=(*,L_OLSUPC),COL=*                        
Exptype  LKREQ F,08,(D,B#SAVED,R_OLETYP),CHAR,OLEN=L'R_OLETYP,         +        
               MAXLEN=L'R_OLETYP,TEXT=(*,L_OLETYP),COL=*                        
StaDate  LKREQ F,09,(D,B#SAVED,R_OLSDAT),CHAR,OLEN=L'R_OLSDAT,         +        
               MAXLEN=L'R_OLSDAT,TEXT=(*,L_OLSDAT),COL=*                        
EndDate  LKREQ F,10,(D,B#SAVED,R_OLEDAT),CHAR,OLEN=L'R_OLEDAT,         +        
               MAXLEN=L'R_OLEDAT,TEXT=(*,L_OLEDAT),COL=*                        
Type     LKREQ F,11,(D,B#SAVED,R_OLTYPE),CHAR,OLEN=L'R_OLTYPE,         +        
               MAXLEN=L'R_OLTYPE,TEXT=(*,L_OLTYPE),COL=*                        
Creator  LKREQ F,12,(D,B#SAVED,R_OCREAT),CHAR,OLEN=L'R_OCREAT,         +        
               MAXLEN=L'R_OCREAT,TEXT=(*,L_OCREAT),COL=*                        
MyStat   LKREQ F,13,(D,B#SAVED,R_OLMYST),CHAR,OLEN=L'R_OLMYST,         +        
               MAXLEN=L'R_OLMYST,TEXT=(*,L_OLMYST),COL=*                        
InclLJ   LKREQ F,14,(D,B#SAVED,R_OLINLJ),CHAR,OLEN=L'R_OLINLJ,         +        
               MAXLEN=L'R_OLINLJ,TEXT=(*,L_OLINLJ),COL=*                        
InclCJ   LKREQ F,15,(D,B#SAVED,R_OLINCJ),CHAR,OLEN=L'R_OLINCJ,         +        
               MAXLEN=L'R_OLINCJ,TEXT=(*,L_OLINCJ),COL=*                        
OvrLL    LKREQ F,16,(D,B#SAVED,R_OLOLL),CHAR,OLEN=L'R_OLOLL,           +        
               MAXLEN=L'R_OLOLL,TEXT=(*,L_OLOLL),COL=*                          
InclExp  LKREQ F,17,(D,B#SAVED,R_OLIEXP),CHAR,OLEN=L'R_OLIEXP,         +        
               MAXLEN=L'R_OLIEXP,TEXT=(*,L_OLIEXP),COL=*                        
InclInt  LKREQ F,18,(D,B#SAVED,R_OLIINT),CHAR,OLEN=L'R_OLIINT,         +        
               MAXLEN=L'R_OLIINT,TEXT=(*,L_OLIINT),COL=*                        
InclPro  LKREQ F,19,(D,B#SAVED,R_OLIPRO),CHAR,OLEN=L'R_OLIPRO,         +        
               MAXLEN=L'R_OLIPRO,TEXT=(*,L_OLIPRO),COL=*                        
NameSrch LKREQ F,20,(I,B#SAVED,R_OLNSIN),CHAR,TEXT=(*,L_OLNSRC),       +        
               OLEN=L'SRCKWRD1,LOWERCASE=Y,LIST=F,SORT=NO                       
Office   LKREQ F,21,(D,B#SAVED,R_OLOFFC),CHAR,OLEN=L'R_OLOFFC,         +        
               MAXLEN=L'R_OLOFFC,TEXT=(*,L_OLOFFC),COL=*                        
Currency LKREQ F,22,(D,B#SAVED,R_OLCURR),CHAR,OLEN=L'R_OLCURR,         +        
               MAXLEN=L'R_OLCURR,TEXT=(*,L_OLCURR),COL=*                        
ReqBSt   LKREQ F,23,(D,B#SAVED,R_OLRBDS),CHAR,OLEN=L'R_OLRBDS,         +        
               MAXLEN=L'R_OLRBDS,TEXT=(*,L_OLRBDS),COL=*                        
ReqBEn   LKREQ F,24,(D,B#SAVED,R_OLRBDE),CHAR,OLEN=L'R_OLRBDE,         +        
               MAXLEN=L'R_OLRBDE,TEXT=(*,L_OLRBDE),COL=*                        
Estimate LKREQ F,25,(D,B#SAVED,R_OLESTN),CHAR,OLEN=L'R_OLESTN,         +        
               MAXLEN=L'R_OLESTN,TEXT=(*,L_OLESTN),COL=*                        
ExpAcc   LKREQ F,26,(D,B#SAVED,R_OLEXPA),CHAR,OLEN=L'R_OLEXPA,         +        
               MAXLEN=L'R_OLEXPA,TEXT=(*,L_OLEXPA),COL=*                        
Approver LKREQ F,27,(D,B#SAVED,R_OLAPPC),CHAR,OLEN=L'R_OLAPPC,         +        
               MAXLEN=L'R_OLAPPC,TEXT=(*,L_OLAPPC),COL=*                        
Dep2D    LKREQ F,28,(D,B#SAVED,R_OLDEPC),CHAR,OLEN=L'R_OLDEPC,         +        
               MAXLEN=L'R_OLDEPC,TEXT=(*,L_OLDEPC),COL=*                        
Staff2P  LKREQ F,29,(D,B#SAVED,R_OLSTAC),CHAR,OLEN=L'R_OLSTAC,         +        
               MAXLEN=L'R_OLSTAC,TEXT=(*,L_OLSTAC),COL=*                        
         LKREQ E                                                                
                                                                                
*    Order List return values                                                   
                                                                                
OUTAOLD  LKOUT H                                                                
                                                                                
ORDLST   LKOUT R,A#AOLD                                                         
PROUT    LKOUT P,,PRESET                                                        
Array    LKOUT C,1,(A,ARYORD)                                                   
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
*    Order List processor array                                                 
                                                                                
ARYORD   LKOUT A,(R,NXTORD),MULTIROW=Y,ROWNAME=ORDRECD                          
                                                                                
         LKOUT P,,SETORD                                                        
OrdNum   LKOUT C,01,ORDKORD,CHAR,LEN=L'ORDKORD                                  
ReqNum   LKOUT C,02,(D,B#SAVED,O_REQNO),CHAR,ND=Y                               
Type     LKOUT C,03,(D,B#SAVED,O_TYPE),CHAR,ND=Y                                
AStatus  LKOUT C,04,(D,B#SAVED,O_APSTAT),CHAR,ND=Y                              
MStatus  LKOUT C,05,(D,B#SAVED,O_MASTAT),CHAR,ND=Y                              
CPJAcc   LKOUT C,06,(D,B#SAVED,O_CPJAC),CHAR,ND=Y                               
ExpAcc   LKOUT C,07,(D,B#SAVED,O_EXPAC),CHAR,ND=Y                               
SupAcc   LKOUT C,08,(D,B#SAVED,O_SUPAC),CHAR,ND=Y                               
SupNam   LKOUT C,09,(D,B#SAVED,O_SUPNM),CHAR,ND=Y                               
ODate    LKOUT C,10,(D,B#SAVED,O_ODATE),PDAT,ND=Y                               
Creator  LKOUT C,11,(D,B#SAVED,O_CRPER),CHAR,ND=Y                               
CrFirst  LKOUT C,12,(D,B#SAVED,O_CFSTN),CHAR,ND=Y                               
CrMiddle LKOUT C,13,(D,B#SAVED,O_CMIDN),CHAR,ND=Y                               
CrLast   LKOUT C,14,(D,B#SAVED,O_CLSTN),CHAR,ND=Y                               
ExpTyp   LKOUT C,15,(D,B#SAVED,O_ETYP),CHAR,ND=Y                                
ExpTNam  LKOUT C,16,(D,B#SAVED,O_ETNAME),CHAR,ND=Y                              
ReqByDat LKOUT C,17,(D,B#SAVED,O_RBDAT),PDAT,ND=Y                               
Office   LKOUT C,18,ORDROFF,CHAR,ND=Y                                           
OffNam   LKOUT C,19,(D,B#SAVED,O_OFFNAM),CHAR,ND=Y                              
EstNo    LKOUT C,20,(D,B#SAVED,O_ESTNO),CHAR,ND=Y                               
EstNam   LKOUT C,21,(D,B#SAVED,O_ESTNAM),CHAR,ND=Y                              
Amount   LKOUT C,22,(D,B#SAVED,O_AMOUNT),SPAK,ND=Y                              
Invoiced LKOUT C,23,(D,B#SAVED,O_INVAMT),SPAK,ND=Y                              
Foreign  LKOUT C,24,(D,B#SAVED,O_FORAMT),SPAK,ND=Y                              
ISOCode  LKOUT C,25,(D,B#SAVED,O_CURR),CHAR,ND=Y                                
GAPSta   LKOUT C,26,(D,B#SAVED,O_GAPST),HEXD,ND=Y                               
Agent    LKOUT C,27,(D,B#SAVED,O_AGENT),CHAR,ND=Y                               
AgName   LKOUT C,28,(D,B#SAVED,O_ANAME),CHAR,ND=Y                               
Name     LKOUT C,29,(D,B#SAVED,O_NAME),CHAR,ND=Y                                
ClName   LKOUT C,30,(D,B#SAVED,O_CLINM),CHAR,ND=Y                               
PrName   LKOUT C,31,(D,B#SAVED,O_PRONM),CHAR,ND=Y                               
JoName   LKOUT C,32,(D,B#SAVED,O_JOBNM),CHAR,ND=Y                               
ExpNam   LKOUT C,33,(D,B#SAVED,O_EXPNM),CHAR,ND=Y                               
GoodsR   LKOUT C,34,(D,B#SAVED,O_GOODS),CHAR,ND=Y                               
JobStat  LKOUT C,35,(D,B#SAVED,O_JSTAT),CHAR,ND=Y                               
IDNum    LKOUT C,36,(D,B#SAVED,O_IDNUM),HEXD,ND=Y                               
Source   LKOUT C,37,(D,B#SAVED,O_OSRCE),CHAR,ND=Y                               
IsAwait  LKOUT C,38,(D,B#SAVED,O_OAMYA),CHAR,ND=Y                               
DepAct   LKOUT C,39,(D,B#SAVED,O_DEPACT),CHAR,ND=Y                              
OSubm    LKOUT C,40,(D,B#SAVED,O_OSUBM),CHAR,ND=Y                               
ETSApp   LKOUT C,41,(D,B#SAVED,O_ETSAPP),CHAR,ND=Y                              
CrEMail  LKOUT C,42,(D,B#SAVED,O_CEMAIL),CHAR,ND=Y                              
AgyCur   LKOUT C,43,(D,B#SAVED,O_AGYCUR),CHAR,ND=Y                              
Gapius   LKOUT C,44,(D,B#SAVED,O_GAPIY),CHAR,ND=Y                               
Gapsdt   LKOUT C,45,(D,B#SAVED,O_GAPSED),PDAT,ND=Y                              
Gapexd   LKOUT C,46,(D,B#SAVED,O_GAPEXD),PDAT,ND=Y                              
GapEM    LKOUT C,47,(A,ARYEML)                                                  
SupEM    LKOUT C,48,(D,B#SAVED,O_SUPEM),CHAR,ND=Y                               
ExpWC    LKOUT C,50,(D,B#SAVED,O_WORKCD),CHAR,ND=Y                              
Array    LKOUT C,01,(A,ARYWCS)                                                  
         LKOUT E                                                                
                                                                                
ARYWCS   LKOUT A,(D,B#ORDREC,ORDRFST),EOT=EOR,NEWEL=N,                 +        
               ROWID=(OAMEL,OAMELQ),ROWWIDTH=(V,OAMLN)                          
WrkCd    LKOUT C,50,OAMWORK,CHAR,ND=Y                                           
         LKOUT E                                                                
                                                                                
ARYEML   LKOUT A,(D,B#SAVED,O_GAPEMD),NROWS=MAXEML,                    +        
               ROWNAME=O_GAPEMD,ROWWIDTH=L'O_GAPEMD                             
GapEM    LKOUT C,47,O_GAPEMD,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Order Approval And Estimate Check Lookup                            *         
***********************************************************************         
                                                                                
REQOALK  LKREQ H,A#ALKE,OUTOALD,NEXTREQ=REQUESTX                                
OAtot    LKREQ F,01,(D,B#SAVED,R_OATOTA),SPAK,TEXT=(*,L_OAAMNT)                 
OAtyp    LKREQ F,02,(D,B#SAVED,R_OATYPE),CHAR,OLEN=L'R_OATYPE,         +        
               MAXLEN=L'R_OATYPE,TEXT=(*,L_OATYPE)                              
OAety    LKREQ F,03,(D,B#SAVED,R_OAETY),CHAR,OLEN=L'R_OAETY,           +        
               MAXLEN=L'R_OAETY,TEXT=(*,L_OLETYP)                               
OAcli    LKREQ F,04,(D,B#SAVED,R_OACLI),CHAR,OLEN=L'R_OACLI,           +        
               MAXLEN=L'R_OACLI,TEXT=(*,L_OLCLIC)                               
OA2Da    LKREQ F,05,(D,B#SAVED,R_OA2DA),CHAR,OLEN=L'R_OA2DA,           +        
               MAXLEN=L'R_OA2DA,TEXT=(*,L_OA2DAC)                               
OAown    LKREQ F,06,(D,B#SAVED,R_OAOWN),CHAR,OLEN=L'R_OAOWN,           +        
               MAXLEN=L'R_OAOWN,TEXT=(*,L_OAOWN)                                
OApro    LKREQ F,07,(D,B#SAVED,R_OAPRC),CHAR,OLEN=L'R_OAPRC,           +        
               MAXLEN=L'R_OAPRC,TEXT=(*,L_OLPROC)                               
OAjob    LKREQ F,08,(D,B#SAVED,R_OAJBC),CHAR,OLEN=L'R_OAJBC,           +        
               MAXLEN=L'R_OAJBC,TEXT=(*,L_OLJOBC)                               
OAsup    LKREQ F,09,(D,B#SAVED,R_OASUPC),CHAR,OLEN=L'R_OASUPC,         +        
               MAXLEN=L'R_OASUPC,TEXT=(*,L_OLSUPC)                              
OAapl    LKREQ F,10,(D,B#SAVED,R_OAAPLC),LBIN,OLEN=L'R_OAAPLC,         +        
               MAXLEN=L'R_OAAPLC,TEXT=(*,L_OAAPLC)                              
OAesf    LKREQ F,11,(D,B#SAVED,R_OAETYS),CHAR,OLEN=L'R_OAETYS,         +        
               MAXLEN=L'R_OAETYS,TEXT=(*,L_OASLFA)                              
OA2Pa    LKREQ F,12,(D,B#SAVED,R_OA2PAC),CHAR,OLEN=L'R_OA2PAC,         +        
               MAXLEN=L'R_OA2PAC,TEXT=(*,L_OA2PAC)                              
OAedo    LKREQ F,13,(D,B#SAVED,R_OAEDOS),CHAR,OLEN=L'R_OAEDOS,         +        
               MAXLEN=L'R_OAEDOS,TEXT=(*,L_OAEDTS)                              
OAord    LKREQ F,14,(D,B#SAVED,R_OAORDN),CHAR,OLEN=L'R_OAORDN,         +        
               MAXLEN=L'R_OAORDN,TEXT=(*,L_OAORDN)                              
OAacl    LKREQ F,15,(D,B#SAVED,R_OAALLC),CHAR,OLEN=L'R_OAALLC,         +        
               MAXLEN=L'R_OAALLC,TEXT=(*,L_OAALLC)                              
OAscl    LKREQ F,16,(D,B#SAVED,R_OAAMIX),CHAR,OLEN=L'R_OAAMIX,         +        
               MAXLEN=L'R_OAAMIX,TEXT=(*,L_OASMCL)                              
OAcpc    LKREQ F,17,(D,B#SAVED,R_OACLPA),CHAR,OLEN=L'R_OACLPA,         +        
               MAXLEN=L'R_OACLPA,TEXT=(*,L_OACPCA)                              
OAcdt    LKREQ F,18,(D,B#SAVED,R_OACLPD),CHAR,OLEN=L'R_OACLPD,         +        
               MAXLEN=L'R_OACLPD,TEXT=(*,L_OACLDT)                              
OAske    LKREQ F,19,(D,B#SAVED,R_OASKEC),CHAR,OLEN=L'R_OASKEC,         +        
               MAXLEN=L'R_OASKEC,TEXT=(*,L_OASKES)                              
OAEstn   LKREQ F,20,(D,B#SAVED,R_OAESTN),CHAR,OLEN=L'R_OAESTN,         +        
               MAXLEN=L'R_OAESTN,TEXT=(*,L_OAESTN)                              
*                                                                               
* UK max 6 workcodes, US max 20 workcodes                                       
*                                                                               
WCod     LKREQ F,30,(I,B#SAVED,R_OAWRKI),CHAR,TEXT=(*,L_OAWRKC),       +        
               LIST=NOSORT,OLEN=L'WRKCODE,ARRAY=S                               
WAmt     LKREQ F,31,,SPAK,TEXT=(*,L_OAWAMT),OLEN=L'WRKAMT,             +        
               ARRAY=E                                                          
                                                                                
         LKREQ E                                                                
                                                                                
*    Order approval return                                                      
                                                                                
OUTOALD  LKOUT H                                                                
                                                                                
APPLST   LKOUT R,A#ALKE                 Main data return                        
Array    LKOUT C,A#ODIS,(A,ARYMAIN)                                             
         LKOUT E                                                                
                                                                                
APPWCR   LKOUT R,R#OAWCR                Errors/warnings                         
APPWCR   LKOUT C,1,(A,ARYWCR)                                                   
         LKOUT E                                                                
                                                                                
APPHDR   LKOUT R,R#OAHDR                Aplimit levels                          
Array    LKOUT C,2,(A,ARYHDR)                                                   
         LKOUT E                                                                
                                                                                
APPOAL   LKOUT R,R#OAOAL                Approver list                           
Array    LKOUT C,3,(A,ARYOAL)                                                   
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
* Order main response                                                           
ARYMAIN  LKOUT A,(R,SETAPP),NROWS=1,ROWNAME=SAVED                               
OAecke   LKOUT C,1,OA_ECKE,LBIN,ND=Y                                            
OAecwm   LKOUT C,2,OA_ECWM,LBIN,ND=Y                                            
OAJlec   LKOUT C,3,OA_EJLE,SPAK,ND=Y                                            
         LKOUT E                                                                
                                                                                
* Order estimate amount remaining array                                         
ARYWCR   LKOUT A,(D,B#SAVED,SVESTWK),NROWS=(B#SAVED,NUMWCR),NEWEL=B,   +        
               ROWID=(ESTCKID,ESTCKIQ),ROWWIDTH=(V,ESTCLEN),           +        
               ROWNAME=ESTCHKD                                                  
OAWcre   LKOUT C,01,ESTCHWC,CHAR,ND=Y                                           
OAecra   LKOUT C,02,ESTRAMT,SPAK,ND=Y                                           
OAwcle   LKOUT C,03,ESTCHKE,HEXD,ND=Y                                           
OAwclw   LKOUT C,04,ESTCHKW,HEXD,ND=Y                                           
         LKOUT E                                                                
                                                                                
* Order aplimit header return array                                             
                                                                                
ARYHDR   LKOUT A,(D,B#SAVED,APLVTAB),NROWS=(B#SAVED,NUMHDR),NEWEL=B,   +        
               ROWID=(APLVLID,APLVTIQ),ROWWIDTH=(V,APLVLEN),           +        
               ROWNAME=APLVLTD                                                  
OAapli   LKOUT C,01,APLVLVAL,SPAK,ND=Y                                          
OAnoat   LKOUT C,02,APLVLNUM,HEXD,ND=Y                                          
OAnopl   LKOUT C,03,APLVLPRV,HEXD,ND=Y                                          
         LKOUT E                                                                
                                                                                
* Order approver list return array                                              
                                                                                
ARYOAL   LKOUT A,(R,NXTOAL),MULTIROW=Y,ROWNAME=SAVED                            
OAAppc   LKOUT C,01,OA_APPC,CHAR,ND=Y                                           
OAApfn   LKOUT C,02,OA_APFN,CHAR,ND=Y                                           
OAAppm   LKOUT C,03,OA_APPM,CHAR,ND=Y                                           
OAApln   LKOUT C,04,OA_APLN,CHAR,ND=Y                                           
OAAppt   LKOUT C,05,OA_APPT,CHAR,ND=Y                                           
OADeft   LKOUT C,06,OA_DEFT,CHAR,ND=Y                                           
OAAppe   LKOUT C,07,OA_APPE,CHAR,ND=Y                                           
OAappl   LKOUT C,08,OA_APAL,SPAK,ND=Y                                           
OALeva   LKOUT C,09,OA_LEVA,CHAR,ND=Y                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get order records for list view                                               
***********************************************************************         
                                                                                
NXTORD   NTR1  BASE=*,LABEL=*                                                   
         OC    ROUERRV,ROUERRV                                                  
         JZ    NXTORD0                                                          
         MVC   LP_ERROR,ROUERRV                                                 
         XC    ROUERRV,ROUERRV                                                  
         J     QERROR                                                           
                                                                                
NXTORD0  CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTORD2                                                          
                                                                                
         GOTOR PPARRY              (preprocess arrays)                          
                                                                                
         GOTOR TRACEIT,0           Trace request                                
                                                                                
         GOTOR GOTSAR,DMCB,('TSAINI',OLBUF),0                                   
         XC    ARTLVLS,ARTLVLS                                                  
                                                                                
NXTORD1  TM    LLINDS,LLINREQ      exit if empty limlist (unless not            
         JNZ   NXTORD2             required)                                    
         TM    LLINDS,LLISEMP                                                   
         JZ    NXTORD2                                                          
         MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
                                                                                
NXTORD2  CLI   R_OLVIEW,R_OLSRCQ   all orders search?                           
         JE    NXTORD4                                                          
         CLI   R_OLVIEW,R_OLMYOQ   my orders (created)?                         
         JE    NXTORD6                                                          
         CLI   R_OLVIEW,R_OLMYAQ   my approvals?                                
         JE    NXTORD7                                                          
         DC    H'0'                                                             
                                                                                
NXTORD4  CLC   R_OCREAT,SPACES                                                  
         JH    NXTORD6A                                                         
         CLI   XORNAM#,0                                                        
         JH    NXTORD4C                                                         
         CLC   R_OLSUPC,SPACES                                                  
         JH    NXTORD4A                                                         
         CLC   R_OLCLIC,SPACES                                                  
         JH    NXTORD4B                                                         
         MVI   DIRTYPE,ORDKQ                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,ORDKEYT,('B#ORDREC',0),          +        
               (0,SAVED),AORDKF,AORDRF                                          
         JE    EXITY                                                            
         J     NXTORDT                                                          
                                                                                
NXTORD4A MVI   DIRTYPE,OSUKQ                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,OSUKEYT,('B#OSUREC',0),          +        
               (0,SAVED),AOSUKF,AORDRF                                          
         JE    EXITY                                                            
         J     NXTORDT                                                          
                                                                                
NXTORD4B MVI   DIRTYPE,OSJKQ                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,OSJKEYT,('B#OSJREC',0),          +        
               (0,SAVED),AOSJKF,AORDRF                                          
         JE    EXITY                                                            
         J     NXTORDT                                                          
                                                                                
NXTORD4C MVI   DIRTYPE,ONSKQ                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,ONSKEYT,('B#OSJREC',0),          +        
               (0,SAVED),AONSKF,AORDRF                                          
         JE    EXITY                                                            
         J     NXTORDT                                                          
                                                                                
NXTORD6  MVC   CURRPID,PERSON                                                   
                                                                                
NXTORD6A MVI   DIRTYPE,PIDKQ                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,PIDKEYT,('B#PIDREC',0),          +        
               (0,SAVED),APIDKF,AORDRF                                          
         JE    EXITY                                                            
         J     NXTORDT                                                          
                                                                                
NXTORD7  CLI   R_OLMYST,AWMYAPRQ   my approvals except approved                 
         JE    NXTORD7A                                                         
         CLI   R_OLMYST,AWOTHERQ                                                
         JE    NXTORD7A                                                         
         CLI   R_OLMYST,REJBYMEQ                                                
         JNE   NXTORD8                                                          
                                                                                
         USING LW_D,R2                                                          
NXTORD7A XR    R2,R2               Clear status WMP                             
         ICM   R2,B'0111',AWMPOST                                               
         JZ    NXTORD7B                                                         
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R3,LW_DATA2                                                      
         XC    0((WMPOSTMQ*L'BYTE2),R3),0(R3)                                   
         DROP  R2                                                               
                                                                                
NXTORD7B MVI   BYTE2,ORDSOREJ                                                   
         GOTOR LP_AAWMP,DMCB,(L'BYTE2,BYTE2),IWMPOST,WMPOSTMQ,LP_D              
                                                                                
         CLI   R_OLMYST,REJBYMEQ                                                
         JE    NXTORD7C                                                         
         MVI   BYTE2,ORDSDRFT                                                   
         GOTOR LP_AAWMP,DMCB,(L'BYTE2,BYTE2),IWMPOST,WMPOSTMQ,LP_D              
         MVI   BYTE2,ORDSPAPP                                                   
         GOTOR LP_AAWMP,DMCB,(L'BYTE2,BYTE2),IWMPOST,WMPOSTMQ,LP_D              
         MVI   BYTE2,ORDSSUBM                                                   
         GOTOR LP_AAWMP,DMCB,(L'BYTE2,BYTE2),IWMPOST,WMPOSTMQ,LP_D              
                                                                                
NXTORD7C MVI   DIRTYPE,OSTKQ                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,OSTKEYT,('B#OSTREC',0),          +        
               (0,SAVED),AOSTKF,AORDRF                                          
         JE    EXITY                                                            
         J     NXTORDT                                                          
                                                                                
NXTORD8  MVI   DIRTYPE,ORDKQ                                                    
         GOTOR (#NXTREC,ANXTREC),DMCB,ORDKEYT,('B#ORDREC',0),          +        
               (0,SAVED),AORDKF,AORDRF                                          
         JE    EXITY                                                            
                                                                                
NXTORDT  DS    0H                                                               
         GOTOR TRACEIT,1           Trace IOs                                    
         J     EXITY                                                            
                                                                                
NXTORDX  MVI   LP_RMODE,LP_RLAST                                                
         J     EXITN                                                            
                                                                                
         USING ORDRECD,R2                                                       
ORDKF    NTR1                                                                   
                                                                                
         LA    R2,IOKEY                                                         
                                                                                
         TM    ORDKSTAT,ORDSDEL    (no read for deletes, just in case)          
         JNZ   ORDKFN                                                           
         CLC   ORDKORD,CONREC      ignore order control                         
         JE    ORDKFN                                                           
         CLI   ORDKSEQ,0           only main records                            
         JNE   ORDKFN                                                           
         TM    ORDKSTA2,ORDSEXEX   BrandOcean/Aura orders only                  
         JZ    ORDKFN                                                           
                                                                                
         GOTOR TSTASTA,ORDKSTA     test approval status                         
         JNE   ORDKFN                                                           
                                                                                
         CLC   ORDSOFF,SPACES                                                   
         JNH   ORDKF2                                                           
                                                                                
         USING OFFALD,R3                                                        
         L     R3,AOFFAREA                                                      
         MVC   OFFAOFFC,ORDSOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD                                                    
         JNE   ORDKFN                                                           
         DROP  R3                                                               
                                                                                
ORDKF2   CLC   R_OLETYP,SPACES                                                  
         JNH   ORDKF4                                                           
         CLC   R_OLETYP,ORDKEXTY                                                
         JNE   ORDKFN                                                           
                                                                                
ORDKF4   CLC   R_OLOFFC,SPACES                                                  
         JNH   ORDKF6                                                           
         CLC   R_OLOFFC,ORDSOFF                                                 
         JNE   ORDKFN                                                           
                                                                                
ORDKF6   DS    0H                                                               
                                                                                
         J     EXITY                                                            
                                                                                
ORDKFN   DS    0H                                                               
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
         USING OSUPASD,R2                                                       
OSUKF    NTR1                                                                   
                                                                                
         LA    R2,IOKEY                                                         
                                                                                
         TM    OSUPSTAT,ORDSDEL    (no read for deletes, just in case)          
         JNZ   OSUKFN                                                           
         TM    OSUPSTA2,ORDSEXEX   BrandOcean/Aura orders only                  
         JZ    OSUKFN                                                           
                                                                                
         GOTOR TSTASTA,OSUPSTA     test approval status                         
         JNE   OSUKFN                                                           
                                                                                
         CLC   OSUPOFF,SPACES                                                   
         JNH   OSUKF2                                                           
                                                                                
         USING OFFALD,R3                                                        
         L     R3,AOFFAREA                                                      
         MVC   OFFAOFFC,OSUPOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD                                                    
         JNE   OSUKFN                                                           
         DROP  R3                                                               
                                                                                
OSUKF2   CLC   R_OLETYP,SPACES                                                  
         JNH   OSUKF4                                                           
         CLC   R_OLETYP,OSUPEXTY                                                
         JNE   OSUKFN                                                           
                                                                                
OSUKF4   CLC   R_OLOFFC,SPACES                                                  
         JNH   OSUKF6                                                           
         CLC   R_OLOFFC,OSUPOFF                                                 
         JNE   OSUKFN                                                           
                                                                                
OSUKF6   DS    0H                                                               
***      CLC   OSUPORD,ANORDNO                                                  
***      JE    *+2                                                              
         J     EXITY                                                            
                                                                                
OSUKFN   DS    0H                                                               
***      CLC   OSUPORD,ANORDNO                                                  
***      JE    *+2                                                              
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
         USING SONRECD,R2                                                       
ONSKF    NTR1                                                                   
                                                                                
         LA    R2,IOKEY                                                         
                                                                                
         TM    SONKSTA,ORDSDEL     (no read for deletes, just in case)          
         JNZ   ONSKFN                                                           
         TM    SONKSTA+ORDKSTA2-ORDKSTA,ORDSEXEX                                
         JZ    ONSKFN              BrandOcean/Aura orders only                  
                                                                                
         GOTOR TSTASTA,SONKSTA     test approval status                         
         JNE   ONSKFN                                                           
                                                                                
         CLC   SONKSTA+ORDSOFF-ORDKSTA(L'ORDSOFF),SPACES                        
         JNH   ONSKF2                                                           
                                                                                
         USING OFFALD,R3                                                        
         L     R3,AOFFAREA                                                      
         MVC   OFFAOFFC,SONKSTA+ORDSOFF-ORDKSTA                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD                                                    
         JNE   ONSKFN                                                           
         DROP  R3                                                               
                                                                                
ONSKF2   CLC   R_OLETYP,SPACES                                                  
         JNH   ONSKF4                                                           
         CLC   R_OLETYP,SONKSTA+ORDKEXTY-ORDKSTA                                
         JNE   ONSKFN                                                           
                                                                                
ONSKF4   CLC   R_OLOFFC,SPACES                                                  
         JNH   ONSKF6                                                           
         CLC   R_OLOFFC,SONKSTA+ORDSOFF-ORDKSTA                                 
         JNE   ONSKFN                                                           
                                                                                
ONSKF6   DS    0H                                                               
         J     EXITY                                                            
                                                                                
ONSKFN   DS    0H                                                               
***      CLC   SONKORD,ANORDNO                                                  
***      JE    *+2                                                              
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
                                                                                
         USING OSJPASD,R2                                                       
OSJKF    NTR1                                                                   
                                                                                
         LA    R2,IOKEY                                                         
                                                                                
         TM    OSJPSTAT,ORDSDEL    (no read for deletes, just in case)          
         JNZ   OSJKFN                                                           
         TM    OSJPSTA2,ORDSEXEX   BrandOcean/Aura orders only                  
         JZ    OSJKFN                                                           
                                                                                
         GOTOR TSTASTA,OSJPSTA     test approval status                         
         JNE   OSJKFN                                                           
                                                                                
         CLC   OSJPOFF,SPACES                                                   
         JNH   OSJKF2                                                           
                                                                                
         USING OFFALD,R3                                                        
         L     R3,AOFFAREA                                                      
         MVC   OFFAOFFC,OSJPOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD                                                    
         JNE   OSJKFN                                                           
         DROP  R3                                                               
                                                                                
OSJKF2   CLC   R_OLETYP,SPACES                                                  
         JNH   OSJKF4                                                           
         CLC   R_OLETYP,OSJPEXTY                                                
         JNE   OSJKFN                                                           
                                                                                
OSJKF4   CLC   R_OLOFFC,SPACES                                                  
         JNH   OSJKF6                                                           
         CLC   R_OLOFFC,OSJPOFF                                                 
         JNE   OSJKFN                                                           
                                                                                
OSJKF6   DS    0H                                                               
         J     EXITY                                                            
                                                                                
OSJKFN   DS    0H                                                               
***      CLC   OSJPORD,ANORDNO                                                  
***      JE    *+2                                                              
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
         USING OSTPASD,R2                                                       
OSTKF    NTR1                                                                   
                                                                                
         LA    R2,IOKEY                                                         
                                                                                
         TM    OSTPSTAT,ORDSDEL    (no read for deletes, just in case)          
         JNZ   OSTKFN                                                           
         TM    OSTPSTA2,ORDSEXEX   BrandOcean/Aura orders only                  
         JZ    OSTKFN                                                           
                                                                                
         GOTOR TSTASTA,OSTPSTA     test approval status                         
         JNE   OSTKFN                                                           
                                                                                
         CLC   OSTPOFF,SPACES                                                   
         JNH   OSTKF2                                                           
                                                                                
         USING OFFALD,R3                                                        
         L     R3,AOFFAREA                                                      
         MVC   OFFAOFFC,OSTPOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD                                                    
         JNE   OSTKFN                                                           
         DROP  R3                                                               
                                                                                
OSTKF2   CLC   R_OLETYP,SPACES                                                  
         JNH   OSTKF4                                                           
         CLC   R_OLETYP,OSTPEXTY                                                
         JNE   OSTKFN                                                           
                                                                                
OSTKF4   CLC   R_OLOFFC,SPACES                                                  
         JNH   OSTKF6                                                           
         CLC   R_OLOFFC,OSTPOFF                                                 
         JNE   OSTKFN                                                           
                                                                                
OSTKF6   DS    0H                                                               
***      CLC   ANORDNO,OSTPORD                                                  
***      JE    *+2                                                              
         J     EXITY                                                            
                                                                                
OSTKFN   DS    0H                                                               
***      CLC   ANORDNO,OSTPORD                                                  
***      JE    *+2                                                              
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
         USING PIDRECD,R2                                                       
PIDKF    NTR1                                                                   
                                                                                
         LA    R2,IOKEY                                                         
                                                                                
         TM    PIDKSTAT,PIDSDELT   (no read for deletes, just in case)          
         JNZ   PIDKFN                                                           
         TM    PIDKSTAT+ORDKSTA2-ORDKSTAT,ORDSEXEX                              
         JZ    PIDKFN              BrandOcean/Aura orders only                  
                                                                                
         GOTOR TSTASTA,PIDKSTA     test approval status                         
         JNE   PIDKFN                                                           
                                                                                
         CLC   PIDKSTAT+ORDSOFF-ORDKSTAT(2),SPACES                              
         JNH   PIDKF2                                                           
                                                                                
         USING OFFALD,R3                                                        
         L     R3,AOFFAREA                                                      
         MVC   OFFAOFFC,PIDKSTAT+ORDSOFF-ORDKSTAT                               
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL,OFFALD                                                    
         JNE   PIDKFN                                                           
         DROP  R3                                                               
                                                                                
PIDKF2   CLC   R_OLETYP,SPACES                                                  
         JNH   PIDKF4                                                           
         CLC   R_OLETYP,PIDKSTA+ORDKEXTY-ORDKSTA                                
         JNE   PIDKFN                                                           
                                                                                
PIDKF4   CLC   R_OLOFFC,SPACES                                                  
         JNH   PIDKF6                                                           
         CLC   R_OLOFFC,PIDKSTAT+ORDSOFF-ORDKSTAT                               
         JNE   PIDKFN                                                           
                                                                                
PIDKF6   DS    0H                                                               
***      CLC   PIDKORD,ANORDNO                                                  
***      JE    *+2                                                              
         J     EXITY                                                            
                                                                                
PIDKFN   DS    0H                                                               
***      CLC   PIDKORD,ANORDNO                                                  
***      JE    *+2                                                              
         J     EXITN                                                            
         DROP  R2                                                               
                                                                                
         USING ORDRECD,R2                                                       
         USING ORDELD,R3                                                        
ORDRF    NTR1                                                                   
                                                                                
         MVC   SJACCNT,SPACES                                                   
                                                                                
         LR    R2,R1                                                            
         LA    R3,ORDRFST          no filtering yet                             
*                                                                               
ORDRF0A  CLI   ORDEL,0             Extract SJACCNT for CHKVIEW                  
         JE    ORDRF0E                                                          
         CLI   ORDEL,ORDELQ                                                     
         JE    ORDRF0C                                                          
         CLI   ORDEL,SORELQ                                                     
         JE    ORDRF0D                                                          
ORDRF0B  LLC   R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     ORDRF0A                                                          
*                                                                               
         USING ORDELD,R3                                                        
ORDRF0C  CLC   PRODUL,ORDACCU      Is it SJ?                                    
*&&US*&& JNE   ORDRF0B                                                          
*&&UK                                                                           
         JE    ORDRF0C1                                                         
         GOTOR CHKLASC,ORDACCU     Check account security                       
         JE    ORDRF0B                                                          
         J     ORDRFN                                                           
ORDRF0C1 DS    0H                                                               
*&&                                                                             
         CLC   ORDACCA,SPACES      Any account?                                 
         JNH   ORDRF0B                                                          
         MVC   SJACCNT,ORDACCA     Extract SJ account                           
         J     ORDRF0B                                                          
*                                                                               
         USING SORELD,R3                                                        
ORDRF0D  CLI   SORSYS,SORSACC      Accounting type                              
         JNE   ORDRF0B                                                          
         CLC   PRODUL,SORAULA      Is it SJ?                                    
*&&US*&& JNE   ORDRF0B                                                          
*&&UK                                                                           
         JE    ORDRF0D1                                                         
         GOTOR CHKLASC,SORAULA     Check account security                       
         JE    ORDRF0B                                                          
         J     ORDRFN                                                           
ORDRF0D1 DS    0H                                                               
*&&                                                                             
         MVC   SJACCNT,SORAACT     Extract SJ account                           
         J     ORDRF0B                                                          
*                                                                               
ORDRF0E  GOTOR CHKVIEW             filter against limit list                    
         JNE   ORDRFN                                                           
         LA    R3,ORDRFST          no filtering yet                             
                                                                                
         USING ORDELD,R3                                                        
ORDRF1   CLI   ORDEL,ORDELQ                                                     
         JE    ORDRF2                                                           
         CLI   ORDEL,0                                                          
         JE    *+2                                                              
         LLC   R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     ORDRF1                                                           
                                                                                
ORDRF2   GOTOR TSTMSTA,DMCB,ORDRSTA,ORDEL                                       
         JNE   ORDRFN              test matching status                         
                                                                                
         GOTOR FLTACT,ORDEL        filter on activity                           
         JNE   ORDRFN                                                           
                                                                                
         GOTOR MYAPSTA,ORDRECD     test 'my approvals' status                   
         JNE   ORDRFN                                                           
                                                                                
         GOTOR TSTOTYP,ORDEL       test order type                              
         JNE   ORDRFN                                                           
                                                                                
         GOTOR TSTODAT,ORDEL       test order date                              
         JNE   ORDRFN                                                           
                                                                                
         GOTOR TSTCPJS,ORDRECD     test cpj and supplier                        
         JNE   ORDRFN                                                           
                                                                                
         GOTOR TSTCEXD,ORDRECD     test currency, estimate, expense             
         JNE   ORDRFN              account and due date                         
                                                                                
         GOTOR TSTNAMS,ORDRECD     test name search                             
         JNE   ORDRFN                                                           
                                                                                
         CLI   DIRTYPE,ONSKQ                                                    
         JNE   ORDRFY                                                           
                                                                                
         L     R1,AGENAXTN         order buffer - check for dupes               
         LHI   R0,L'GENAEXTN/L'ORDKORD                                          
         SHI   R0,1                                                             
                                                                                
ORDRF3   CLI   0(R1),FF                                                         
         JE    ORDRF4                                                           
         CLC   ORDKORD,0(R1)                                                    
         JE    ORDRFN                                                           
         AHI   R1,L'ORDKORD                                                     
         JCT   R0,ORDRF3           if used up start again ...                   
         L     R1,AGENAXTN                                                      
                                                                                
ORDRF4   MVC   0(L'ORDKORD,R1),ORDKORD                                          
         AHI   R1,L'ORDKORD                                                     
         MVI   0(R1),FF                                                         
                                                                                
ORDRFY   DS    0H                                                               
         J     EXITY                                                            
                                                                                
ORDRFN   DS    0H                                                               
***      CLC   ORDKORD,ANORDNO                                                  
***      JE    *+2                                                              
         J     EXITN                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Set order details for list                                          *         
***********************************************************************         
         USING ORDRECD,R2                                                       
SETORD   NTR1  BASE=*,LABEL=*                                                   
         LA    RE,O_DATA           Clear and set output data                    
         LHI   RF,O_DATALQ                                                      
         XCEF                                                                   
         L     R2,AIO2                                                          
         MVC   CSVKEY1,IOKEY                                                    
         MVI   REREAD,NOQ                                                       
         MVC   O_AGYCUR,AGYCURR                                                 
         MVC   O_ETYP,ORDREXTY                                                  
         CLC   O_ETYP,SPACES                                                    
         JH    SETO02                                                           
         MVC   O_ETYP,DEFEXPTY                                                  
                                                                                
SETO02   MVI   O_TYPE,PRODORDQ                                                  
         MVI   O_APSTAT,FULAPPQ    default                                      
         TM    ORDRSTAT,ORDSLDEL                                                
         JZ    SETO04                                                           
         MVI   O_APSTAT,DELETEQ    unless deleted                               
                                                                                
SETO04   MVI   O_MASTAT,C'1'       unmatched                                    
         ZAP   O_AMOUNT,PZERO                                                   
         ZAP   O_INVAMT,PZERO                                                   
         ZAP   O_FORAMT,PZERO                                                   
         GOTOR GETOFN,ORDROFF                                                   
         GOTOR GETETN                                                           
         TM    ORDRSTAT,ORDGDRCV                                                
         JZ    SETO06                                                           
         MVI   O_GOODS,YESQ                                                     
                                                                                
         USING ORDELD,R3                                                        
SETO06   LA    R3,ORDRFST                                                       
         LA    R4,O_GAPEMD                                                      
                                                                                
SETO08   CLI   ORDEL,ORDELQ                                                     
         JE    SETO12                                                           
         CLI   ORDEL,FFTELQ                                                     
         JE    SETO42                                                           
         CLI   ORDEL,OAMELQ                                                     
         JE    SETO52                                                           
         CLI   ORDEL,AFCELQ                                                     
         JE    SETO54                                                           
         CLI   ORDEL,ENMELQ                                                     
         JE    SETO56                                                           
         CLI   ORDEL,SPAELQ                                                     
         JE    SETO60                                                           
         CLI   ORDEL,SORELQ                                                     
         JE    SETO70                                                           
         CLI   ORDEL,GDAELQ                                                     
         JE    SETO72                                                           
         CLI   ORDEL,0                                                          
         JE    SETO80                                                           
                                                                                
SETO10   LLC   R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     SETO08                                                           
                                                                                
SETO12   CLI   ORDSUPA,SPACEQ       u/l only then skip                          
         JNH   SETO14                                                           
         MVC   O_SUPAC,ORDSUPU                                                  
         GOTOR GETSAN                                                           
                                                                                
SETO14   LA    RE,O_CPJULA                                                      
         CLC   PRODUL,ORDACCU                                                   
         JE    SETO16                                                           
         LA    RE,O_EXPAC                                                       
         MVI   O_TYPE,EXPORDQ                                                   
                                                                                
SETO16   MVC   0(L'O_CPJULA,RE),ORDACCU                                         
         CLC   O_CPJULA,SPACES                                                  
         JNH   SETO18                                                           
         GOTOR GETCPJ                                                           
                                                                                
SETO18   CLC   O_EXPAC,SPACES                                                   
         JNH   SETO20                                                           
         GOTOR GETEAN                                                           
                                                                                
SETO20   MVC   O_ODATE,ORDDATE                                                  
         TM    ORDSTAT,ORDSPART+ORDSFMCH If pending fully matched               
         JZ    SETO22                     show as part invoiced                 
         TM    ORDRSTAT,ORDCLOSE                                                
         JNZ   SETO22                                                           
         MVI   O_MASTAT,C'2'       part matched                                 
                                                                                
SETO22   TM    ORDRSTAT,ORDSFMCH+ORDCLOSE                                       
         JZ    SETO24                                                           
         MVI   O_MASTAT,C'3'       fully matched or closed                      
         TM    ORDSTAT,ORDSPART+ORDSMNUP                                        
         JNZ   SETO24                                                           
         MVI   O_MASTAT,C'4'       cancelled (unmatched and closed)             
                                                                                
SETO24   MVI   O_OSRCE,ORDERQ                                                   
         TM    ORDSTAT,ORDSPRES                                                 
         JZ    SETO26                                                           
         MVI   O_OSRCE,PRESTOQ                                                  
                                                                                
SETO26   TM    ORDRSTA2,ORDSEXEX   BrandOcean/Aura order?                       
         JZ    SETO28                                                           
         MVC   O_IDNUM,ORDIDNO                                                  
         MVI   O_OSRCE,AURAQ                                                    
         GOTOR CREATOR,ORDCPID                                                  
*&&UK*&& MVC   O_RBDAT,ORDREQD                                                  
*&&US*&& MVC   O_RBDAT,ORDRQBD                                                  
                                                                                
SETO28   CLC   SKLEDGER,ORDSUPU                                                 
         JE    SETO30                                                           
         CLC   SILEDGER,ORDSUPU                                                 
         JE    SETO30                                                           
         CLC   STLEDGER,ORDSUPU                                                 
         JNE   SETO32                                                           
         MVI   O_TYPE,ARTORDQ                                                   
         J     SETO32                                                           
                                                                                
SETO30   MVI   O_TYPE,INTORDQ                                                   
                                                                                
SETO32   CLI   ORDLN,ORDLN3Q                                                    
         JL    SETO40                                                           
         TM    ORDSTAT,ORDSEBUY    Test BrandOcean/Aura order                   
         JZ    SETO34                                                           
         CLI   ORDLN,ORDLN2Q                                                    
         JL    SETO34                                                           
*&&US*&& CLI   ORDLN,ORDLN3Q                                                    
*&&US*&& JL    SETO34                                                           
         MVC   O_GAPST,ORDGSTAT    Gap status                                   
                                                                                
SETO34   CLI   O_APSTAT,DELETEQ                                                 
         JE    SETO40                                                           
         LAY   RE,OSTATAB                                                       
         MVC   BYTE4,ORDSTAT2                                                   
         NI    BYTE4,FF-(ORDGDRCV+ORDSEXEX+ORDSSTAT)                            
SETO36   CLI   0(RE),FF                                                         
         JE    SETO40                                                           
         CLC   BYTE4,1(RE)                                                      
         JE    SETO38                                                           
         AHI   RE,2                                                             
         J     SETO36                                                           
                                                                                
SETO38   MVC   O_APSTAT,0(RE)                                                   
                                                                                
SETO40   CLI   ORDLN,ORDLN4Q                                                    
         JL    SETO10                                                           
         OC    ORDCSUBM,ORDCSUBM                                                
         JZ    SETO10                                                           
         GOTOR OSUBMTR,ORDCSUBM                                                 
         J     SETO10                                                           
         DROP  R3                                                               
                                                                                
         USING FFTELD,R3                                                        
SETO42   CLI   FFTTYPE,FFTTORNO                                                 
         JNE   SETO44                                                           
         MVC   O_REQNO,FFTDATA                                                  
         J     SETO10                                                           
                                                                                
SETO44   CLI   FFTTYPE,FFTTESTN                                                 
         JNE   SETO48                                                           
         MVC   O_ESTNO,FFTESTN     global estimate number                       
         CLI   FFTLN,FFTESLNQ      Any original estimate number?                
         JL    SETO46              No - OK                                      
         CLC   FFTOESTN,SPACES                                                  
         JNH   SETO46                                                           
         MVC   O_ESTNO,FFTOESTN    Yes - use it instead                         
                                                                                
SETO46   GOTOR GETESTN                                                          
         J     SETO10                                                           
                                                                                
SETO48   CLI   FFTTYPE,FFTTWRKC    Is this work code                            
         JNE   SETO50              No                                           
         MVC   O_WORKCD,FFTWORK    Yes - extract work code                      
         J     SETO10                                                           
                                                                                
SETO50   DS    0H                                                               
*&&UK*&& CLI   FFTTYPE,FFTTPEML    Check whether GAP email address              
*&&US*&& CLI   FFTTYPE,FFTTEML                                                  
         JNE   SETO10                                                           
         LA    RF,O_GAPEMD                                                      
         AHI   RF,O_GAPEML                                                      
         CR    R4,RF               Check whether more than 10 email             
         JNL   SETO10              addresses                                    
*                                                                               
         MVC   0(L'O_GAPEMD,R4),FFTEML1                                         
         AHI   R4,L'O_GAPEMD                                                    
         CLI   FFTLN,FFEM1LL                                                    
         JNH   SETO10                                                           
         MVC   0(L'O_GAPEMD,R4),FFTEML2                                         
         AHI   R4,L'O_GAPEMD                                                    
         CLI   FFTLN,FFEM2LL                                                    
         JNH   SETO10                                                           
         MVC   0(L'O_GAPEMD,R4),FFTEML3                                         
         AHI   R4,L'O_GAPEMD                                                    
         CLI   FFTLN,FFEM3LL                                                    
         JNH   SETO10                                                           
         MVC   0(L'O_GAPEMD,R4),FFTEML4                                         
         AHI   R4,L'O_GAPEMD                                                    
         CLI   FFTLN,FFEM4LL                                                    
         JNH   SETO10                                                           
         MVC   0(L'O_GAPEMD,R4),FFTEML5                                         
         AHI   R4,L'O_GAPEMD                                                    
         J     SETO10                                                           
         DROP  R3                                                               
                                                                                
         USING OAMELD,R3                                                        
SETO52   AP    O_AMOUNT,OAMAMNT                                                 
         AP    O_INVAMT,OAMIVAL                                                 
*&&US                                                                           
         CLI   O_MASTAT,C'3'       Does the order amount element                
         JNE   SETO10               have pending invoices if so                 
         OC    OAMIPND,OAMIPND       can't be fully matched                     
         JZ    SETO10                                                           
         MVI   O_MASTAT,C'2'        so set as partly invoiced                   
*&&                                                                             
         J     SETO10                                                           
         DROP  R3                                                               
                                                                                
         USING AFCELD,R3                                                        
SETO54   MVC   O_FORAMT,AFCAMNT                                                 
         MVC   O_CURR,AFCCURR                                                   
         J     SETO10                                                           
         DROP  R3                                                               
                                                                                
         USING ENMELD,R3                                                        
SETO56   LLC   R1,ENMLN                                                         
         SHI   R1,ENMLNQ                                                        
         CHI   R1,0                                                             
         JNH   SETO10                                                           
         CHI   R1,L'O_NAME                                                      
         JNH   SETO58                                                           
         LHI   R1,L'O_NAME                                                      
                                                                                
SETO58   SHI   R1,1                                                             
         MVC   O_NAME(0),ENMNAME                                                
         EX    R1,*-6                                                           
         J     SETO10                                                           
         DROP  R3                                                               
                                                                                
         USING SPAELD,R3                                                        
SETO60   CLI   SPATYPE,SPATDEPT                                                 
         JNE   SETO10                                                           
         MVC   O_DEPACT,SPAAACT                                                 
         J     SETO10                                                           
         DROP  R3                                                               
                                                                                
         USING SORELD,R3                                                        
SETO70   CLC   PRODUL,SORAULA                                                   
         JNE   SETO10                                                           
         MVC   O_CPJULA,SORAULA                                                 
         GOTOR GETCPJ                                                           
         J     SETO10                                                           
         DROP  R3                                                               
                                                                                
         USING GDAELD,R3                                                        
SETO72   CLI   GDATYPE,GDAGAPEX    Gap expiry/ sent date                        
         JNE   SETO74                                                           
         LA    RF,O_GAPEXD                                                      
         J     SETO76                                                           
                                                                                
SETO74   CLI   GDATYPE,GDAGAPST                                                 
         JNE   SETO10                                                           
         LA    RF,O_GAPSED                                                      
                                                                                
SETO76   MVC   0(L'GDADATE,RF),GDADATE                                          
         J     SETO10                                                           
                                                                                
SETO80   CP    O_INVAMT,PZERO      Do we have any invoice amount                
         JNE   SETO82               Yes                                         
*&&US                                                                           
         CLI   O_MASTAT,C'2'       If no then reset part matched                
         JNE   SETO82               to no matching                              
         MVI   O_MASTAT,C'1'                                                    
*&&                                                                             
                                                                                
SETO82   CLI   O_TYPE,ARTORDQ      agent if artist order                        
         JNE   SETO84                                                           
         GOTOR GETAGNT                                                          
                                                                                
SETO84   CLI   OAPSTAT,AWAITQ      indicate person to approve next              
         JNE   SETO86                                                           
         MVI   O_OAMYA,YESQ                                                     
                                                                                
SETO86   DS    0H                                                               
***      CLC   ORDKORD,=C'P02898'                                               
***      JNE   SETOX                                                            
***      J     SETOX                                                            
         DROP  R2                                                               
                                                                                
SETOX    CLI   REREAD,NOQ          reestablish IO sequence?                     
         JE    SETOXIT                                                          
         MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JNE   *+2                                                              
                                                                                
SETOXIT  DS    0H                                                               
         J     EXITY                                                            
                                                                                
**********************************************************************          
* Set creator details from PID, also used for original submitter     *          
**********************************************************************          
                                                                                
         DS    0H                                                               
CREATOR  MVI   XBYTE1,0                                                         
         J     CREATOR0                                                         
                                                                                
OSUBMTR  MVI   XBYTE1,1                                                         
                                                                                
CREATOR0 NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING BUFFERD,R2                                                       
         LA    R2,TSAROLBF                                                      
         XC    TSAROLBF,TSAROLBF                                                
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKPIDQ                                                 
         MVC   BUFKVAL(L'ORDCPID),0(R1)                                         
         MVC   SVTSAREA,TSAROLBF                                                
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    CREATOR4                                                         
         CLI   TSARERRS,TSEEOF                                                  
         JE    CREATOR4                                                         
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                                                              
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
         MVI   REREAD,YESQ                                                      
         MVC   TEMP2(2),BUFKVAL                                                 
         GOTOR (#GETPID,AGETPID)                                                
         JNE   CREATOR2                                                         
         MVC   BUFRPCOD,TEMP2                                                   
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   CREATOR2                                                         
         MVC   BUFRPFST,TEMP2                                                   
         MVC   BUFRPMID,TEMP2+32                                                
         MVC   BUFRPLST,WORK2                                                   
         MVC   BUFRPEMA,APPEMAIL                                                
                                                                                
CREATOR2 GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    CREATOR4                                                         
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
                                                                                
CREATOR4 CLI   XBYTE1,0                                                         
         JE    CREATOR6                                                         
         MVC   O_OSUBM,BUFRPCOD                                                 
         J     CREATORX                                                         
                                                                                
CREATOR6 MVC   O_CRPER,BUFRPCOD                                                 
         MVC   O_CFSTN,BUFRPFST                                                 
         MVC   O_CMIDN,BUFRPMID                                                 
         MVC   O_CLSTN,BUFRPLST                                                 
         MVC   O_CEMAIL,BUFRPEMA                                                
         DROP  R2                                                               
                                                                                
CREATORX DS    0H                                                               
         J     EXITY                                                            
                                                                                
**********************************************************************          
* Preprocess arrays                                                  *          
**********************************************************************          
                                                                                
PPARRY   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   ORSTAT,NOQ                                                       
         MVI   ORSTOK,NOQ                                                       
                                                                                
         L     R1,AGENAXTN         order buffer                                 
         MVI   0(R1),FF                                                         
                                                                                
         MVC   R_OLASTA,SPACES     approval status array                        
         LA    R3,R_OLASTA                                                      
                                                                                
         XR    R2,R2                                                            
         ICM   R2,B'0111',R_OLAADR                                              
         JZ    PPARRY2                                                          
         XR    R0,R0                                                            
         ICM   R0,B'0011',LW_NUMN-LW_D(R2)    # of array elements               
         LA    R2,LW_DATA2-LW_D(R2)           start of list                     
         MVI   XBYTE1,0                                                         
                                                                                
PPARRY1  MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         JCT   R0,PPARRY1                                                       
                                                                                
         CLI   R_OLMYST,AWMYAPRQ   in combination with 'awaiting my             
         JNE   PPARRY2             approvals'?                                  
         MVI   ORSTAT,YESQ                                                      
                                                                                
PPARRY2  XC    XORNAMS,XORNAMS     initialise internal list for name            
         OC    R_OLNSAD,R_OLNSAD   search array                                 
         JZ    PPARRY7                                                          
                                                                                
         LARL  R1,UINGER                                                        
         LARL  RE,TRTSPEC                                                       
         CLI   CUCTRY,CTRYGER                                                   
         JE    PPARRY3                                                          
         LARL  R1,UINUKUS                                                       
         LARL  RE,TRTSGER                                                       
                                                                                
PPARRY3  ST    R1,AUINTAB                                                       
         ST    RE,ASRCTAB                                                       
                                                                                
         USING LW_D,R1                                                          
         XR    R1,R1               point to list in WMP                         
         ICM   R1,7,R_OLNSAD                                                    
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        number of entries                            
         STC   R0,XORNAM#                                                       
         LA    R5,LW_DATA2         start of list                                
         LA    R4,XORNAL1                                                       
         DROP  R1                                                               
*                                                                               
PPARRY4  LA    RE,ELEMENT          ADDRESS WORK AREA                            
         LHI   RF,L'XORNAM1        SAVE NAME LENGTH                             
         MVC   0(L'XORNAM1,RE),0(R5)  SAVE INPUT DETAILS                        
*                               ## STRIP OFF LEADING SPACES ##                  
PPARRY4A CLI   0(RE),C' '          LEADING SPACE?                               
         BH    PPARRY4B            NO, SAVE DETAILS FOR SEARCH                  
*                                  YES                                          
         BCTR  RF,0                REDUCE LENGTH BY 1 BYTE                      
         LA    RE,1(RE)            POINT TO THE NEXT BYTE                       
         B     PPARRY4A            CHECK FOR NEXT BYTE                          
*                                                                               
PPARRY4B BCTR  RF,0                REDUCE LENGTH BY 1 BYTE FOR EXECUTE          
         MVC   L'XORNAL1(0,R4),0(RE)  SAVE DETAILS                              
         EX    RF,*-6                                                           
*                                                                               
         L     R1,AUINTAB          translate to upper case                      
         TR    L'XORNAL1(L'XORNAM1,R4),0(R1)                                    
*                                                                               
         LA    R1,L'XORNAL1+L'XORNAM1-1(R4)                                     
         LHI   R2,L'XORNAM1       save name and its length                      
*                                                                               
PPARRY5  CLI   0(R1),C' '                                                       
         JH    PPARRY6                                                          
         SHI   R1,1                                                             
         JCT   R2,PPARRY5                                                       
         DC    H'0'                                                             
*                                                                               
PPARRY6  STC   R2,0(R4)                                                         
         AHI   R5,L'XORNAM1                                                     
         AHI   R4,L'XORNAL1+L'XORNAM1                                           
         JCT   R0,PPARRY4                                                       
*                                                                               
         GOTOR TRANS4S             translate for search                         
         CLI   XORNAL,0                                                         
         JNH   *+2                                                              
*                                                                               
PPARRY7  MVI   R_OLMSTA,0          matching status array                        
                                                                                
         XR    R2,R2                                                            
         ICM   R2,B'0111',R_OLMADR                                              
         JZ    PPARRYX                                                          
         XR    R0,R0                                                            
         ICM   R0,B'0011',LW_NUMN-LW_D(R2)    # of array elements               
         LA    R2,LW_DATA2-LW_D(R2)           start of list                     
                                                                                
PPARRY7A CLI   0(R2),C'1'          unmatched order                              
         JNE   PPARRY7B                                                         
         OI    R_OLMSTA,UNMATQ                                                  
                                                                                
PPARRY7B CLI   0(R2),C'2'          part matched (not closed)                    
         JNE   PPARRY7C                                                         
         OI    R_OLMSTA,PARMATQ                                                 
                                                                                
PPARRY7C CLI   0(R2),C'3'          completed (=matched or closed)               
         JNE   PPARRY7D                                                         
         OI    R_OLMSTA,COMMATQ                                                 
                                                                                
PPARRY7D CLI   0(R2),C'4'          cancelled (=unmatched and closed)            
         JNE   PPARRY7E                                                         
         OI    R_OLMSTA,CANCLDQ                                                 
                                                                                
PPARRY7E AHI   R2,1                                                             
         JCT   R0,PPARRY7A                                                      
                                                                                
PPARRYX  J     EXITY                                                            
                                                                                
* Apply GESRCHPARS.TRTSPEC and .TRTSGER character handling                      
                                                                                
TRANS4S  NTR1                                                                   
                                                                                
         MVC   ELEMENT(L'XORNAM1),XORNAM1                                       
         L     R3,ASRCTAB                                                       
         TR    ELEMENT(L'XORNAM),0(R3)                                          
                                                                                
         LHI   R0,L'XORNAM         now squash out any unknowns (X'00')          
         LA    RE,ELEMENT                                                       
         LA    R1,XORNAM                                                        
         MVC   XORNAM,SPACES                                                    
                                                                                
TRANS4S1 CLI   0(RE),X'40'                                                      
         JNH   TRANS4S2            SKIP ZEROES and spaces                       
         MVC   0(1,R1),0(RE)                                                    
         AHI   R1,1                                                             
                                                                                
TRANS4S2 AHI   RE,1                                                             
         JCT   R0,TRANS4S1                                                      
                                                                                
TRANS4S3 LA    R1,XORNAM+L'XORNAM-1                                             
         LHI   R0,L'XORNAM                                                      
                                                                                
TRANS4S4 CLI   0(R1),X'40'                                                      
         JH    TRANS4S5                                                         
         SHI   R1,1                                                             
         JCT   R0,TRANS4S4                                                      
                                                                                
TRANS4S5 STC   R0,XORNAL           length + name value for SRCKWRD1 set         
         J     EXIT                                                             
                                                                                
         DS    0H                                                               
       ++INCLUDE FACTRYXLAT                                                     
                                                                                
         DS    0H                                                               
TRTSPEC  DC    256AL1(0)           TRANSLATED VALUE IS X'00', EXCEPT            
*                                                                               
         ORG   TRTSPEC+C'A'        UPPER CASE LETTERS                           
         DC    C'ABCDEFGHI'                                                     
         ORG   TRTSPEC+C'J'                                                     
         DC    C'JKLMNOPQR'                                                     
         ORG   TRTSPEC+C'S'                                                     
         DC    C'STUVWXYZ'                                                      
*                                                                               
         ORG   TRTSPEC+C'0'        UPPER CASE NUMBERS                           
         DC    C'0123456789'                                                    
*                                                                               
         ORG   TRTSPEC+C' '        SPACES STAY SAME                             
         DC    X'40'                                                            
*                                                                               
         ORG   TRTSPEC+C'*'        '*' STAYS SAME, IN CASE PARTIAL FLAG         
         DC    C'*'                                                             
*                                                                               
         ORG   TRTSPEC+C'.'        THESE SPECIAL CHARS BECOME SPACES            
         DC    X'40'               AND ARE TREATED AS WORD SEPARATORS           
         ORG   TRTSPEC+C'<'        ALL OTHERS BECOME NULLS AND ARE              
         DC    X'40'               IGNORED AS IF COMPRESSED OUT.                
         ORG   TRTSPEC+C'('                                                     
         DC    X'40'                                                            
         ORG   TRTSPEC+C'+'                                                     
         DC    C'+'                                                             
         ORG   TRTSPEC+C'&&'                                                    
         DC    X'40'                                                            
         ORG   TRTSPEC+C')'                                                     
         DC    X'40'                                                            
         ORG   TRTSPEC+C';'                                                     
         DC    X'40'                                                            
         ORG   TRTSPEC+C'-'                                                     
         DC    X'40'                                                            
         ORG   TRTSPEC+C'/'                                                     
         DC    X'40'                                                            
         ORG   TRTSPEC+C','                                                     
         DC    X'40'                                                            
         ORG   TRTSPEC+C'_'                                                     
         DC    X'40'                                                            
         ORG   TRTSPEC+C'>'                                                     
         DC    X'40'                                                            
         ORG   TRTSPEC+C':'                                                     
         DC    X'40'                                                            
         ORG   TRTSPEC+C'='                                                     
         DC    X'40'                                                            
*                                                                               
         ORG   ,                                                                
*                                                                               
TRTSGER  DC    256AL1(0)           TRANSLATED VALUE IS X'00', EXCEPT            
*                                                                               
         ORG   TRTSGER+C'A'        UPPER CASE LETTERS                           
         DC    C'ABCDEFGHI'                                                     
         ORG   TRTSGER+C'J'                                                     
         DC    C'JKLMNOPQR'                                                     
         ORG   TRTSGER+C'S'                                                     
         DC    C'STUVWXYZ'                                                      
*                                                                               
         ORG   TRTSGER+X'4A'       4A=U/C A UMLAUT                              
         DC    X'4A'                                                            
         ORG   TRTSGER+X'5A'       5A=U/C U UMLAUT (! IN ENGLISH)               
         DC    X'5A'                                                            
         ORG   TRTSGER+X'A1'       A1=SS                                        
         DC    X'A1'                                                            
         ORG   TRTSGER+X'E0'       E0=U/C O UMLAUT                              
         DC    X'E0'                                                            
*                                                                               
         ORG   TRTSGER+C'0'        UPPER CASE NUMBERS                           
         DC    C'0123456789'                                                    
*                                                                               
         ORG   TRTSGER+C' '        SPACES STAY SAME                             
         DC    X'40'                                                            
*                                                                               
         ORG   TRTSGER+C'*'        '*' STAYS SAME, IN CASE PARTIAL FLAG         
         DC    C'*'                                                             
*                                                                               
         ORG   TRTSGER+C'.'        THESE SPECIAL CHARS BECOME SPACES            
         DC    X'40'               AND ARE TREATED AS WORD SEPARATORS           
         ORG   TRTSGER+C'<'        ALL OTHERS BECOME NULLS AND ARE              
         DC    X'40'               IGNORED AS IF COMPRESSED OUT.                
         ORG   TRTSGER+C'('                                                     
         DC    X'40'                                                            
         ORG   TRTSGER+C'+'                                                     
         DC    X'40'                                                            
         ORG   TRTSGER+C'&&'                                                    
         DC    X'40'                                                            
         ORG   TRTSGER+C')'                                                     
         DC    X'40'                                                            
         ORG   TRTSGER+C';'                                                     
         DC    X'40'                                                            
         ORG   TRTSGER+C'-'                                                     
         DC    X'40'                                                            
         ORG   TRTSGER+C'/'                                                     
         DC    X'40'                                                            
         ORG   TRTSGER+C','                                                     
         DC    X'40'                                                            
         ORG   TRTSGER+C'_'                                                     
         DC    X'40'                                                            
         ORG   TRTSGER+C'>'                                                     
         DC    X'40'                                                            
         ORG   TRTSGER+C':'                                                     
         DC    X'40'                                                            
         ORG   TRTSGER+C'='                                                     
         DC    X'40'                                                            
*                                                                               
         ORG   ,                                                                
**********************************************************************          
* Preset request values and get limlist                              *          
**********************************************************************          
                                                                                
PRESET   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XR    RF,RF               This server is Aura only                     
         ICM   RF,B'0011',CUXPNUM                                               
         CHI   RF,XPRODIKQ                                                      
         JNE   *+2                                                              
                                                                                
         XC    SRCHAPP,SRCHAPP                                                  
         XC    SRBDSTA,SRBDSTA                                                  
         CLC   R_OLRBDS,SPACES                                                  
         JNH   PRES02                                                           
         GOTOR VDATCON,DMCB,(0,R_OLRBDS+2),(1,SRBDSTA)                          
                                                                                
PRES02   MVC   SRBDEND,FFS                                                      
         CLC   R_OLRBDE,SPACES                                                  
         JNH   PRES04                                                           
         GOTOR VDATCON,DMCB,(0,R_OLRBDE+2),(1,SRBDEND)                          
                                                                                
PRES04   XC    CPJRANGE,CPJRANGE                                                
         MVC   CPJFACC,SPACES                                                   
         MVC   CPJFULG(L'PRODUL),PRODUL                                         
         MVI   CPJLACC,0                                                        
                                                                                
         OC    R_OLCLIC,SPACES                                                  
         OC    R_OLPROC,SPACES                                                  
         OC    R_OLJOBC,SPACES                                                  
         OC    R_OLSUPC,SPACES                                                  
         OC    R_OLOFFC,SPACES                                                  
         OC    R_OLEXPA,SPACES                                                  
         OC    R_OLDEPC,SPACES                                                  
         OC    R_OLSTAC,SPACES                                                  
                                                                                
         CLC   R_OLCLIC,SPACES                                                  
         JNH   PRES11                                                           
                                                                                
         MVC   CPJRSTA,SPACES                                                   
         MVC   CPJREND,FFS                                                      
         LLC   R1,PCLILEN                                                       
         SHI   R1,1                                                             
         MVC   CPJRSTA(0),R_OLCLIC                                              
         EX    R1,*-6                                                           
         MVC   CPJREND(0),R_OLCLIC                                              
         EX    R1,*-6                                                           
         MVC   CPJFACC(0),R_OLCLIC                                              
         EX    R1,*-6                                                           
         MVC   CPJLACC,PCLILEN                                                  
                                                                                
         CLC   R_OLPROC,SPACES                                                  
         JNH   PRES10                                                           
         LLC   R1,PCLILEN                                                       
         LR    R0,R1                                                            
         LA    RE,CPJRSTA(R1)                                                   
         LA    RF,CPJREND(R1)                                                   
         LA    R2,CPJFACC(R1)                                                   
         LLC   R1,PPROLEN                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   0(0,RE),R_OLPROC                                                 
         EX    R1,*-6                                                           
         MVC   0(0,RF),R_OLPROC                                                 
         EX    R1,*-6                                                           
         MVC   0(0,R2),R_OLPROC                                                 
         EX    R1,*-6                                                           
         MVC   CPJLACC,PPROLEN                                                  
                                                                                
         CLC   R_OLJOBC,SPACES                                                  
         JNH   PRES10                                                           
         LLC   R1,PPROLEN                                                       
         LR    R0,R1                                                            
         LA    RE,CPJRSTA(R1)                                                   
         LA    RF,CPJREND(R1)                                                   
         LA    R2,CPJFACC(R1)                                                   
         LLC   R1,PJOBLEN                                                       
         SR    R1,R0                                                            
         CHI   R1,L'R_OLJOBC                                                    
         JNH   PRES08                                                           
         LHI   R1,L'R_OLJOBC                                                    
PRES08   SHI   R1,1                                                             
         MVC   0(0,RE),R_OLJOBC                                                 
         EX    R1,*-6                                                           
         MVC   0(0,RF),R_OLJOBC                                                 
         EX    R1,*-6                                                           
         MVC   0(0,R2),R_OLJOBC                                                 
         EX    R1,*-6                                                           
         MVC   CPJLACC,PJOBLEN                                                  
*                                                                               
PRES10   OC    CPJFACC,SPACES      (ensure space filled key)                    
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'CPJFULA),CPJFULA                                         
         GOTOR (#GETACN,AGETACN)   USES AIO3                                    
         CLC   TEMP2(36),SPACES                                                 
         JH    *+14                                                             
         MVC   ROUERRV,=AL2(AE$ACTNF) FILE ERROR                                
         J     EXITN                                                            
*                                                                               
         L     R0,AIO4             Copy to AIO4                                 
         LA    R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'CPJFULA),CPJFULA                                         
         GOTOR SSJOFF              Read for office                              
*                                                                               
PRES11   XC    FLTMONS,FLTMONS     Filter on n months activity                  
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
PRES12   CLI   R_OLVIEW,R_OLMYOQ   my orders - 13 months (RD005445)             
         JE    PRES14                                                           
         CLI   R_OLVIEW,R_OLMYAQ   my approvals - 3 months                      
         JNE   PRES20                                                           
         CLI   R_OLMYST,AWMYAPRQ                                                
         JE    PRES20                                                           
         CLI   R_OLMYST,AWOTHERQ                                                
         JE    PRES20                                                           
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'-3'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,FLTMONS)                              
         J     PRES20                                                           
                                                                                
PRES14   GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'-13'                            
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,FLTMONS)                              
                                                                                
PRES20   MVI   LLINDS,LLINREQ      preset to not required                       
         CLI   R_OLVIEW,R_OLSRCQ   required for search only                     
         JNE   PRES70                                                           
         CLI   R_OLOLL,YESQ        Limlist override?                            
         JE    PRES70                                                           
*                                                                               
         MVI   LLINDS,0                                                         
*                                                                               
         USING GAPTABD,GAPAREA                                                  
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
*                                  Build table for LIMLIST entries              
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT23Q',TSARABUF),           +        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),('QORD',0)             
         JE    *+12                                                             
         OI    LLINDS,LLISEMP      Error getting LIMLIST                        
         J     PRES70                                                           
*                                                                               
GAP      USING GAPTABD,GAPAREA                                                  
         CLC   R_OLCLIC,SPACES    Any checking of filters to do?                
         JH    *+14                                                             
         CLC   R_OLETYP,SPACES    Any etype code?                               
         JNH   PRES70                                                           
         XC    GAPAREA,GAPAREA                                                  
         XC    ATSRERRS,ATSRERRS                                                
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         J     PRES30                                                           
*                                                                               
PRES25   GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
*                                                                               
PRES30   TM    ATSRERRS,TSEEOF      Hit the end?                                
         JZ    PRES35               Passed CPJ, supp,med and ety filt?          
         TM    LLINDS,LLINCQ+LLINMQ+LLINEQ                                      
         JO    PRES70                                                           
         J     PRESCLK                                                          
*                                                                               
PRES35   CLI   GAP.GAPTDAT1,GAPTT2Q SJ entry                                    
         JNE   PRES50                                                           
         CLC   CPJFACC,SPACES      Any c/p/j?                                   
         JNH   PRES47                                                           
         CLC   GAP.GAPTCODE,SPACES All access entry then ok                     
         JH    *+14                                                             
         CLC   GAP.GAPTCOFF,SPACES Any office?                                  
         JNH   PRES45                                                           
*                                                                               
         LA    R4,GAP.GAPTACC                                                   
         LLC   R1,PCLILEN          Determine limlist entry level                
         LR    RF,R1                                                            
         AR    RF,R4                                                            
         CLI   0(RF),C' '                                                       
         JNH   PRES40                                                           
         LLC   R1,PPROLEN                                                       
         LR    RF,R1                                                            
         AR    RF,R4                                                            
         CLI   0(RF),C' '                                                       
         JNH   PRES40                                                           
         LLC   R1,PJOBLEN                                                       
*                                                                               
PRES40   BCTR  R1,0                                                             
         LLC   RF,CPJLACC                                                       
         CR    R1,RF                                                            
         JNH   *+6                                                              
         LR    R1,RF                                                            
         BASR  RF,0                                                             
         CLC   GAP.GAPTACC(0),CPJFACC  Match on SJ account code                 
         EX    R1,0(RF)                                                         
         JNE   PRES25                                                           
*                                                                               
PRES45   CLC   GAP.GAPTCOFF,SPACES Check office code                            
         JNH   *+14                                                             
         CLC   GAP.GAPTCOFF,D_OFFC                                              
         JNE   PRES25                                                           
PRES47   OI    LLINDS,LLINCQ      Set CPJ limlist found                         
         J     PRES25                                                           
*                                 Concatenate media list                        
PRES50   CLI   GAP.GAPTDAT1,GAPTT5Q media entry                                 
         JNE   PRES60                                                           
         CLC   GAP.GAPTCODE,SPACES  All access entry then ok                    
         JNH   PRES55                                                           
         CLC   R_OLJOBC,SPACES      Any job code?                               
         JNH   PRES55                                                           
         CLC   GAP.GAPTMEDI,R_OLJOBC                                            
         JNE   PRES25                                                           
PRES55   OI    LLINDS,LLINMQ        Passed job media filtering                  
         J     PRES25                                                           
*                                                                               
PRES60   CLI   GAP.GAPTDAT1,GAPTT7Q Etype entry                                 
         JNE   PRES25                                                           
         CLC   GAP.GAPTETYP,SPACES  All access entry then ok                    
         JNH   PRES65                                                           
         CLC   R_OLETYP,SPACES      Any etype code?                             
         JNH   PRES65                                                           
         CLC   GAP.GAPTETYP,R_OLETYP                                            
         JNE   PRES25                                                           
PRES65   OI    LLINDS,LLINEQ        Passed etype filtering                      
         J     PRES25                                                           
*                                                                               
PRES70   CLC   R_OCREAT,SPACES                                                  
         JNH   PRES75                                                           
         MVC   TEMP2(L'R_OCREAT),R_OCREAT                                       
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   EXITN                                                            
         MVC   CURRPID,TEMP2+50                                                 
                                                                                
PRES75   CLC   R_OLAPPC,SPACES                                                  
         JNH   PRES80                                                           
         MVC   TEMP2(L'R_OLAPPC),R_OLAPPC                                       
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   EXITN                                                            
         MVC   SRCHAPP,TEMP2+50                                                 
                                                                                
PRES80   MVC   CSVKEY2,IOKEY       Save current IO key                          
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,=CL2'SE'     Get SE offpos                                
         GOTOR (#SETLDG,ASETLDG)                                                
         MVC   SEOFPOS,LDGAOP                                                   
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,=CL2'SQ'     Get SQ offpos                                
         GOTOR (#SETLDG,ASETLDG)                                                
         MVC   SQOFPOS,LDGAOP                                                   
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,=CL2'SV'     Get SV offpos                                
         GOTOR (#SETLDG,ASETLDG)                                                
         MVC   SVOFPOS,LDGAOP                                                   
                                                                                
         MVC   IOKEY,CSVKEY2       Restore IO sequence                          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
                                                                                
         J     EXITY                                                            
                                                                                
PRESCLK  MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
         EJECT                                                                  
                                                                                
                                                                                
**********************************************************************          
* Test order type applies                                            *          
**********************************************************************          
                                                                                
TSTOTYP  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ORDELD,R3                                                        
         LR    R3,R1                                                            
         MVI   BYTE4,PRODORDQ                                                   
                                                                                
         CLC   PRODUL,ORDACCU                                                   
         JE    TSTOT1                                                           
         MVI   BYTE4,EXPORDQ                                                    
                                                                                
TSTOT1   CLC   SKLEDGER,ORDSUPU                                                 
         JE    TSTOT2                                                           
         CLC   SILEDGER,ORDSUPU                                                 
         JE    TSTOT2                                                           
         CLC   STLEDGER,ORDSUPU                                                 
         JNE   TSTOT3                                                           
         MVI   BYTE4,ARTORDQ                                                    
         J     TSTOT3                                                           
                                                                                
TSTOT2   MVI   BYTE4,INTORDQ                                                    
                                                                                
TSTOT3   CLI   R_OLTYPE,SPACEQ                                                  
         JNH   TSTOT4                                                           
                                                                                
         CLC   BYTE4,R_OLTYPE                                                   
         JNE   TSTOTN                                                           
                                                                                
TSTOT4   CLI   BYTE4,ARTORDQ       always skip artist orders                    
         JE    TSTOTN                                                           
                                                                                
         CLI   BYTE4,EXPORDQ                                                    
         JNE   TSTOT5                                                           
         CLI   R_OLIEXP,YESQ       apply passed security                        
         JNE   TSTOTN                                                           
                                                                                
TSTOT5   CLI   BYTE4,INTORDQ                                                    
         JNE   TSTOT6                                                           
         CLI   R_OLIINT,YESQ                                                    
         JNE   TSTOTN                                                           
                                                                                
TSTOT6   CLI   BYTE4,PRODORDQ                                                   
         JNE   EXITY                                                            
         CLI   R_OLIPRO,YESQ                                                    
         JE    EXITY                                                            
                                                                                
TSTOTN   J     EXITN                                                            
                                                                                
         DROP  R3                                                               
                                                                                
**********************************************************************          
* Filter on 3 months activity                                        *          
**********************************************************************          
                                                                                
FLTACT   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         OC    FLTMONS,FLTMONS                                                  
         JZ    FLTACTY                                                          
                                                                                
         USING ORDELD,R3                                                        
         LR    R3,R1                                                            
         CLC   ORDAMDT,FLTMONS                                                  
         JL    FLTACTN                                                          
                                                                                
FLTACTY  J     EXITY                                                            
                                                                                
FLTACTN  J     EXITN                                                            
         DROP  R3                                                               
                                                                                
**********************************************************************          
* Test order date range applies                                      *          
**********************************************************************          
                                                                                
TSTODAT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ORDELD,R3                                                        
         LR    R3,R1                                                            
                                                                                
         XC    FULL1,FULL1                                                      
         MVC   FULL2,FFS                                                        
                                                                                
         CLC   R_OLSDAT(R_OLDATL),SPACES                                        
         JNH   TSTODY                                                           
                                                                                
         CLC   R_OLSDAT,SPACES                                                  
         JNH   TSTOD1                                                           
         GOTOR VDATCON,DMCB,(0,R_OLSDAT+2),(1,FULL1)                            
                                                                                
TSTOD1   CLC   R_OLEDAT,SPACES                                                  
         JNH   TSTOD2                                                           
         GOTOR VDATCON,DMCB,(0,R_OLEDAT+2),(1,FULL2)                            
                                                                                
TSTOD2   CLC   ORDDATE,FULL1                                                    
         JL    TSTODN                                                           
         CLC   ORDDATE,FULL2                                                    
         JH    TSTODN                                                           
                                                                                
TSTODY   J     EXITY                                                            
TSTODN   J     EXITN                                                            
         DROP  R3                                                               
                                                                                
**********************************************************************          
* Test client/product/job and supplier apply                         *          
**********************************************************************          
                                                                                
TSTCPJS  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ORDRECD,R2                                                       
         LR    R2,R1                                                            
         LR    R6,R2               (for TSTCX debugging)                        
         USING ORDELD,R3                                                        
         LA    R3,ORDRFST                                                       
         XR    R5,R5                                                            
         MVI   XBYTE1,0                                                         
                                                                                
TSTC02   CLI   ORDEL,ORDELQ                                                     
         JE    TSTC04                                                           
         CLI   ORDEL,0                                                          
         JE    TSTCN                                                            
         LLC   R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     TSTC02                                                           
                                                                                
TSTC04   CLC   R_OLSUPC,SPACES     supplier passed?                             
         JNH   TSTC06                                                           
         CLI   DIRTYPE,OSUKQ       if supplier passives skip                    
         JE    TSTC06                                                           
         CLC   R_OLSUPC,ORDSUPU                                                 
         JNE   TSTCN                                                            
                                                                                
TSTC06   CLC   PRODUL,ORDACCU      Is it expense job?                           
         JNE   TSTC08              Yes - read SORELD for job                    
         LA    R5,ORDACCU                                                       
         OI    XBYTE1,X'01'                                                     
         CLI   CPJLACC,0           c/p/j filter passed?                         
         JE    TSTC16              Skip filter                                  
         LLC   R1,CPJLACC                                                       
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         CLC   ORDACCA(0),CPJFACC                                               
         EX    R1,0(RF)                                                         
         JE    TSTC16                                                           
         J     TSTCN                                                            
                                                                                
         USING SORELD,R4                                                        
TSTC08   LA    R4,ORDRFST                                                       
                                                                                
TSTC10   CLI   SOREL,0                                                          
         JNE   TSTC11                                                           
         CLI   CPJLACC,0           c/p/j filter passed?                         
         JNE   TSTCN               Yes - but none on order                      
         J     TSTCY               No - so display order                        
TSTC11   CLI   SOREL,SORELQ                                                     
         JNE   TSTC12                                                           
         CLC   PRODUL,SORAULA                                                   
         JE    TSTC14                                                           
                                                                                
TSTC12   LLC   R0,SORLN                                                         
         AR    R4,R0                                                            
         J     TSTC10                                                           
                                                                                
TSTC14   LA    R5,SORAULA                                                       
         OI    XBYTE1,X'02'                                                     
         CLI   CPJLACC,0           c/p/j filtered?                              
         JE    TSTC16              No                                           
         LLC   R1,CPJLACC                                                       
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         CLC   SORAACT(0),CPJFACC                                               
         EX    R1,0(RE)                                                         
         JNE   TSTCN                                                            
         DROP  R4                                                               
                                                                                
TSTC16   CLC   CPJLACC,PJOBLEN     If filter is job level then skip             
         JE    TSTCY               lock/close check                             
                                                                                
         CLI   R_OLINLJ,YESQ       skip if locked and closed requested          
         JNE   TSTC20                                                           
         CLI   R_OLINCJ,YESQ                                                    
         JE    TSTCY                                                            
                                                                                
TSTC20   MVC   CSVKEY2,IOKEY       save key for reread                          
                                                                                
         USING BUFFERD,R2                                                       
         LA    R2,TSAROLBF                                                      
         XC    TSAROLBF,TSAROLBF   Job in Buffer?                               
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKACCQ                                                 
         MVC   BUFKVAL(14),0(R5)                                                
         MVC   SVTSAREA,TSAROLBF                                                
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    TSTC24                                                           
         CLI   TSARERRS,TSEEOF                                                  
         JE    TSTC24                                                           
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                                                              
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
         MVC   TEMP2(14),BUFKVAL                                                
         GOTOR (#GETACN,AGETACN)                                                
         MVC   BUFRACCN,TEMP2                                                   
         CLC   BUFRACCN,SPACES                                                  
         JNH   TSTC22                                                           
         MVC   BUFRACCS,IOKEY+ACTKSTAT-ACTRECD                                  
                                                                                
TSTC22   GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    TSTC24                                                           
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
                                                                                
TSTC24   TM    BUFRACCS,ACTSABLP   low level = job?                             
         JZ    TSTC48                                                           
                                                                                
TSTC26   TM    BUFRACCS,ACTSCLOS                                                
         JZ    TSTC28                                                           
         CLI   R_OLINCJ,C'O'       only allow closed jobs?                      
         JE    TSTC30                                                           
         CLI   R_OLINCJ,YESQ       allow closed jobs?                           
         JNE   TSTC46                                                           
         J     TSTC30                                                           
                                                                                
TSTC28   CLI   R_OLINCJ,C'O'       If not closed reject job                     
         JE    TSTC46                                                           
                                                                                
TSTC30   TM    BUFRACCS,ACTSLOCK                                                
         JZ    TSTC32                                                           
         CLI   R_OLINLJ,C'O'       only allow locked jobs?                      
         JE    TSTC48                                                           
         CLI   R_OLINLJ,YESQ       allow locked jobs?                           
         JNE   TSTC46                                                           
         J     TSTC48                                                           
                                                                                
TSTC32   CLI   R_OLINLJ,C'O'       If not locked reject job                     
         JNE   TSTC48                                                           
                                                                                
TSTC46   OI    XBYTE1,X'80'                                                     
                                                                                
TSTC48   CLC   CSVKEY2(L'ACTKEY),IOKEY                                          
         JE    TSTC50                                                           
         MVC   IOKEY,CSVKEY2                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JNE   *+2                                                              
                                                                                
TSTC50   TM    XBYTE1,X'80'                                                     
         JNZ   TSTCN                                                            
                                                                                
TSTCY    J     EXITY                                                            
TSTCN    J     EXITN                                                            
         DROP  R2,R3                                                            
**********************************************************************          
* Test currency, estimate, expense account, required by/due date,    *          
* 2D and 2P accounts                                                            
**********************************************************************          
                                                                                
TSTCEXD  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ORDRECD,R2                                                       
         LR    R2,R1                                                            
                                                                                
         USING AFCELD,R3                                                        
         LA    R3,ORDRFST                                                       
         MVI   XBYTE2,0                                                         
                                                                                
TCEXD02  CLI   AFCEL,0                                                          
         JE    TCEXD30                                                          
         CLI   AFCEL,ORDELQ                                                     
         JE    TCEXD12                                                          
         CLI   AFCEL,FFTELQ                                                     
         JE    TCEXD08                                                          
         CLI   AFCEL,AFCELQ                                                     
         JE    TCEXD06                                                          
         CLI   AFCEL,SPAELQ                                                     
         JE    TCEXD20                                                          
                                                                                
TCEXD04  LLC   R0,AFCLN                                                         
         AR    R3,R0                                                            
         J     TCEXD02                                                          
                                                                                
TCEXD06  OI    XBYTE2,XB2AFCQ                                                   
         CLC   R_OLCURR,SPACES                                                  
         JNH   TCEXD04                                                          
         CLC   AFCCURR,R_OLCURR                                                 
         JNE   TCEXDN                                                           
         J     TCEXD04                                                          
                                                                                
         USING FFTELD,R3                                                        
TCEXD08  CLI   FFTTYPE,FFTTESTN                                                 
         JNE   TCEXD04                                                          
         OI    XBYTE2,XB2ESTQ                                                   
         CLC   R_OLESTN,SPACES                                                  
         JNH   TCEXD04                                                          
         LA    RE,FFTESTN                                                       
         CLI   FFTLN,FFTESLNQ      Any original estimate number?                
         JL    TCEXD10             No - OK                                      
         CLC   FFTOESTN,SPACES                                                  
         JNH   TCEXD10                                                          
         LA    RE,FFTOESTN                                                      
                                                                                
TCEXD10  CLC   R_OLESTN,0(RE)                                                   
         JNE   TCEXDN                                                           
         J     TCEXD04                                                          
                                                                                
         USING ORDELD,R3                                                        
TCEXD12  OI    XBYTE2,XB2ORDQ                                                   
         CLC   R_OLEXPA,SPACES                                                  
         JNH   TCEXD14                                                          
         CLC   R_OLEXPA,ORDACCU    exact match|                                 
         JNE   TCEXDN                                                           
                                                                                
TCEXD14  TM    ORDRSTA2,ORDSEXEX                                                
         JZ    TCEXD04                                                          
         OI    XBYTE2,XB2RBDQ                                                   
         CLC   ORDRQBD,SRBDSTA                                                  
         JL    TCEXDN                                                           
         CLC   ORDRQBD,SRBDEND                                                  
         JH    TCEXDN                                                           
         J     TCEXD04                                                          
                                                                                
         USING SPAELD,R3                                                        
TCEXD20  CLI   SPATYPE,SPATDEPT                                                 
         JNE   TCEXD22                                                          
         OI    XBYTE2,XB2DEPQ                                                   
         CLC   R_OLDEPC,SPACES                                                  
         JNH   TCEXD04                                                          
         CLC   SPAAACT,R_OLDEPC                                                 
         JNE   TCEXDN                                                           
         J     TCEXD04                                                          
                                                                                
TCEXD22  CLI   SPATYPE,SPATPERS                                                 
         JNE   TCEXD04                                                          
         OI    XBYTE2,XB2STAQ                                                   
         CLC   R_OLSTAC,SPACES                                                  
         JNH   TCEXD04                                                          
         CLC   SPAAACT,R_OLSTAC                                                 
         JNE   TCEXDN                                                           
         J     TCEXD04                                                          
                                                                                
TCEXD30  CLC   R_OLCURR,SPACES                                                  
         JNH   TCEXD32                                                          
         TM    XBYTE2,XB2AFCQ                                                   
         JZ    TCEXDN                                                           
                                                                                
TCEXD32  CLC   R_OLESTN,SPACES                                                  
         JNH   TCEXD34                                                          
         TM    XBYTE2,XB2ESTQ                                                   
         JZ    TCEXDN                                                           
                                                                                
TCEXD34  CLC   R_OLEXPA,SPACES                                                  
         JNH   TCEXD36                                                          
         TM    XBYTE2,XB2ORDQ                                                   
         JZ    TCEXDN                                                           
                                                                                
TCEXD36  CLC   R_OLRBDS,SPACES                                                  
         JNH   TCEXD38                                                          
         TM    XBYTE2,XB2RBDQ                                                   
         JZ    TCEXDN                                                           
                                                                                
TCEXD38  CLC   R_OLRBDE,SPACES                                                  
         JNH   TCEXD40                                                          
         TM    XBYTE2,XB2RBDQ                                                   
         JZ    TCEXDN                                                           
                                                                                
TCEXD40  CLC   R_OLDEPC,SPACES                                                  
         JNH   TCEXD42                                                          
         TM    XBYTE2,XB2DEPQ                                                   
         JZ    TCEXDN                                                           
                                                                                
TCEXD42  CLC   R_OLSTAC,SPACES                                                  
         JNH   TCEXD44                                                          
         TM    XBYTE2,XB2STAQ                                                   
         JZ    TCEXDN                                                           
                                                                                
TCEXD44  DS    0H                                                               
                                                                                
TCEXDY   J     EXITY                                                            
TCEXDN   J     EXITN                                                            
         DROP  R2,R3                                                            
                                                                                
XB2AFCQ  EQU   X'80'                                                            
XB2ESTQ  EQU   X'40'                                                            
XB2ORDQ  EQU   X'20'                                                            
XB2RBDQ  EQU   X'10'                                                            
XB2DEPQ  EQU   X'08'                                                            
XB2STAQ  EQU   X'04'                                                            
                                                                                
**********************************************************************          
* Test name search                                                   *          
**********************************************************************          
                                                                                
TSTNAMS  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING ORDRECD,R2                                                       
         LR    R2,R1                                                            
                                                                                
         CLI   XORNAM#,0                                                        
         JE    TSTNY                                                            
                                                                                
         USING ENMELD,R3                                                        
         LA    R3,ORDRFST                                                       
                                                                                
TSTN02   CLI   ENMEL,ENMELQ                                                     
         JE    TSTN04                                                           
         CLI   ENMEL,0                                                          
         JE    TSTNN                                                            
         LLC   R0,ENMLN                                                         
         AR    R3,R0                                                            
         J     TSTN02                                                           
                                                                                
TSTN04   LA    R5,XORNAL1          point to names                               
         LLC   R4,XORNAM#                                                       
         LLC   RE,ENMLN                                                         
         SHI   RE,ENMLNQ                                                        
         LARL  R1,TSTN16                                                        
         EX    RE,0(R1)                                                         
         L     R1,AUINTAB                                                       
         LARL  RF,TSTN14                                                        
         EX    RE,0(RF)                                                         
                                                                                
TSTN06   LLC   R0,ENMLN            disregard L'NAMEREC as 99,99% OK             
         SHI   R0,ENMLNQ                                                        
         LA    RE,TEMP                                                          
         LLC   RF,0(R5)                                                         
         SHI   RF,1                                                             
TSTN08   LARL  R1,TSTN12                                                        
         EX    RF,0(R1)                                                         
         JE    TSTN10                                                           
         AHI   RE,1                                                             
         JCT   R0,TSTN08                                                        
         J     TSTNN               extra name (filter) not on record            
                                                                                
TSTN10   AHI   R5,L'XORNAL1+L'XORNAM1    extra name (filter) matches            
         JCT   R4,TSTN06                                                        
         J     TSTNY               all OK                                       
                                                                                
TSTN12   CLC   0(0,RE),L'XORNAL2(R5)                                            
TSTN14   TR    TEMP(0),0(R1)                                                    
TSTN16   MVC   TEMP(0),ENMNAME     TEMP+TEMP2 used                              
                                                                                
TSTNY    J     EXITY                                                            
TSTNN    J     EXITN                                                            
         DROP  R2,R3                                                            
                                                                                
**********************************************************************          
* Test matching status applies                                       *          
**********************************************************************          
                                                                                
TSTMSTA  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   R_OLMSTA,0                                                       
         JE    TSTMSY                                                           
         CLI   R_OLMSTA,UNMATQ+PARMATQ+COMMATQ+CANCLDQ                          
         JE    TSTMSY                                                           
                                                                                
TST      USING ORDKSTA,R2                                                       
         L     R2,0(R1)                                                         
         USING ORDELD,R3                                                        
         L     R3,4(R1)                                                         
                                                                                
         MVI   BYTE4,0                                                          
         CLI   ORDLN,ORDLN3Q                                                    
         JL    TSTMS1                                                           
         MVC   BYTE4,ORDSTAT                                                    
                                                                                
TSTMS1   TM    R_OLMSTA,UNMATQ     unmatched order                              
         JZ    TSTMS2                                                           
         TM    TST.ORDKSTAT,ORDSFMCH+ORDCLOSE                                   
         JNZ   TSTMS2                                                           
         TM    BYTE4,ORDSPART      (ORDSMNUP ?)                                 
         JZ    TSTMSY                                                           
                                                                                
TSTMS2   TM    R_OLMSTA,PARMATQ    part matched (not closed)                    
         JZ    TSTMS3                                                           
         TM    BYTE4,ORDSPART                                                   
         JZ    TSTMS3                                                           
         TM    TST.ORDKSTAT,ORDSFMCH+ORDCLOSE                                   
         JZ    TSTMSY                                                           
                                                                                
TSTMS3   TM    R_OLMSTA,COMMATQ    completed (=matched or closed)               
         JZ    TSTMS4                                                           
         TM    BYTE4,ORDSPART+ORDSMNUP Only want those that have                
         JZ    TSTMS4                                   matching                
         TM    TST.ORDKSTAT,ORDSFMCH+ORDCLOSE                                   
         JNZ   TSTMSY                                                           
                                                                                
TSTMS4   TM    R_OLMSTA,CANCLDQ    cancelled (=unmatched and closed)            
         JZ    TSTMSN                                                           
         TM    BYTE4,ORDSPART+ORDSMNUP  Only want those unmatched               
         JNZ   TSTMSN                                                           
         TM    TST.ORDKSTAT,ORDSFMCH+ORDCLOSE  This status gets set             
         JZ    TSTMSN                            when order is closed           
                                                                                
TSTMSY   J     EXITY                                                            
TSTMSN   J     EXITN                                                            
         DROP  TST                                                              
                                                                                
**********************************************************************          
* Test 'my approvals' status                                         *          
**********************************************************************          
                                                                                
MYAPSTA  NTR1  BASE=*,LABEL=*                                                   
                                                                                
*        CLI   ORSTOK,YESQ         did order pass via ORSTAT?                   
*        JE    MYAPSY                                                           
                                                                                
* first get my approval status of this order                                    
*    rewrite of ACBRA10.ORDLST.OL_ASTAT/OL_RVAP                                 
                                                                                
         USING ORDRECD,R2                                                       
         LR    R2,R1                                                            
         USING PIDELD,R3                                                        
         LA    R3,ORDRFST                                                       
         USING ORDPRFD,R4                                                       
         LA    R4,ORDPRFS                                                       
                                                                                
         MVI   XBYTE1,NOQ          have I approved?                             
         MVI   XBYTE2,NOQ          is SRCHAPP an approver?                      
         OC    SRCHAPP,SRCHAPP                                                  
         JNZ   MYAPS01                                                          
         MVI   XBYTE2,YESQ                                                      
                                                                                
MYAPS01  MVI   BYTE2,NOQ           do I need to approve?                        
         MVI   BYTE3,NOQ           any other approvers first?                   
         MVI   BYTE4,NOQ           have I rejected?                             
                                                                                
MYAPS02  CLI   PIDEL,PIDELQ                                                     
         JE    MYAPS10                                                          
         CLI   PIDEL,STCELQ                                                     
         JE    MYAPS06                                                          
         CLI   PIDEL,0                                                          
         JE    MYAPS20                                                          
                                                                                
MYAPS04  LLC   R0,PIDLN                                                         
         AR    R3,R0                                                            
         J     MYAPS02                                                          
                                                                                
         USING STCELD,R3                                                        
MYAPS06  TM    ORDRSTA2,ORDSOREJ   order still rejected?                        
         JZ    MYAPS04                                                          
         CLI   STCIND,STCIORD2                                                  
         JNE   MYAPS04                                                          
         CLC   STCOPID,PERSON                                                   
         JNE   MYAPS04                                                          
         CLI   STCLN,STCOLN2Q                                                   
         JL    MYAPS04                                                          
         CLI   STCOTOST,STCOREJT                                                
         JNE   MYAPS04                                                          
         MVI   BYTE4,YESQ          rejected by requestor                        
         J     MYAPS04                                                          
                                                                                
         USING PIDELD,R3                                                        
MYAPS10  CLI   PIDTYPE,PIDT1Q                                                   
         JL    MYAPS04                                                          
         CLI   PIDTYPE,PIDT4Q                                                   
         JH    MYAPS04                                                          
         LA    RE,PIDNTRS          list of approvals                            
         LLC   RF,PIDNTR#          number of entries                            
                                                                                
MYAPS12  CLI   XBYTE2,YESQ         approver search?                             
         JE    MYAPS13                                                          
         CLC   SRCHAPP,1(RE)                                                    
         JNE   MYAPS13                                                          
         CLI   0(RE),PIDAPPQ       actually approved?                           
         JNE   MYAPS13                                                          
         MVI   XBYTE2,YESQ                                                      
                                                                                
MYAPS13  CLI   0(RE),PIDAPPQ       approved entry?                              
         JNE   MYAPS14                                                          
         CLC   CCTPID,1(RE)        approved by me?                              
         JNE   MYAPS18                                                          
         MVI   XBYTE1,YESQ                                                      
         J     MYAPS18                                                          
                                                                                
MYAPS14  CLC   CCTPID,1(RE)        awaiting me?                                 
         JE    MYAPS16                                                          
         CLI   PROEBY04,YESQ       sequential approval?                         
         JNE   MYAPS18                                                          
         CLI   BYTE2,YESQ                                                       
         JE    MYAPS18                                                          
         MVI   BYTE3,YESQ                                                       
         J     MYAPS18             others need to approve first                 
                                                                                
MYAPS16  MVI   BYTE2,YESQ          I need to approve                            
                                                                                
MYAPS18  AHI   RE,L'PIDNTRS        next list entry                              
         JCT   RF,MYAPS12                                                       
         J     MYAPS04                                                          
                                                                                
MYAPS20  MVI   OAPSTAT,SPACEQ      (no approvals)                               
         CLI   BYTE2,YESQ          do I need to approve?                        
         JNE   MYAPS22                                                          
         MVI   OAPSTAT,IAMAPPRQ    (I'm approver but others first)              
         CLI   BYTE3,YESQ                                                       
         JE    MYAPS24                                                          
         MVI   OAPSTAT,AWAITQ      (awaiting my approvals)                      
         J     MYAPS24                                                          
                                                                                
MYAPS22  CLI   XBYTE1,YESQ                                                      
         JNE   MYAPS24                                                          
         MVI   OAPSTAT,APPROVEQ    (I have approved)                            
                                                                                
MYAPS24  CLI   BYTE4,YESQ          have I rejected?                             
         JNE   MYAPS26                                                          
         MVI   OAPSTAT,REJECTDQ    (I have rejected)                            
                                                                                
MYAPS26  DS    0H                                                               
                                                                                
* now apply my approval status to request parms                                 
                                                                                
MYAPS30  CLI   R_OLMYST,SPACEQ                                                  
         JNH   MYAPS40                                                          
                                                                                
         CLI   R_OLMYST,AWMYAPRQ                                                
         JNE   MYAPS32                                                          
         CLI   OAPSTAT,AWAITQ                                                   
         JNE   MYAPSN                                                           
         J     MYAPS40                                                          
                                                                                
MYAPS32  CLI   R_OLMYST,APPBYMEQ                                                
         JNE   MYAPS34                                                          
         CLI   OAPSTAT,APPROVEQ                                                 
         JNE   MYAPSN                                                           
         J     MYAPS40                                                          
                                                                                
MYAPS34  CLI   R_OLMYST,REJBYMEQ                                                
         JNE   MYAPS36                                                          
         CLI   OAPSTAT,REJECTDQ                                                 
         JNE   MYAPSN                                                           
         J     MYAPS40                                                          
                                                                                
MYAPS36  CLI   R_OLMYST,AWOTHERQ                                                
         JNE   MYAPS40                                                          
         CLI   OAPSTAT,IAMAPPRQ                                                 
         JNE   MYAPSN                                                           
         J     MYAPS40                                                          
                                                                                
MYAPS40  CLI   XBYTE2,YESQ         SRCHAPP is an approver?                      
         JNE   MYAPSN                                                           
                                                                                
MYAPSY   J     EXITY                                                            
MYAPSN   J     EXITN                                                            
         DROP  R2,R3,R4                                                         
                                                                                
**********************************************************************          
* Test approval and my approval status applies                       *          
**********************************************************************          
                                                                                
TSTASTA  NTR1  BASE=*,LABEL=*                                                   
                                                                                
TST      USING ORDKSTA,R2                                                       
         LR    R2,R1                                                            
         MVI   ORSTOK,NOQ                                                       
                                                                                
         LA    R3,R_OLASTA                                                      
         CLI   R_OLASTA,SPACEQ                                                  
         JNH   TSTAS6A                                                          
                                                                                
TSTAS0   CLI   0(R3),SPACEQ                                                     
         JNH   TSTASORS                                                         
                                                                                
         TM    TST.ORDKSTAT,ORDSLDEL+ORDSDEL                                    
         JNZ   TSTAS5                                                           
         CLI   0(R3),INPROGQ       in progress                                  
         JNE   TSTAS1                                                           
         TM    TST.ORDKSTA2,ORDSDRFT                                            
         JZ    TSTASNXT                                                         
         J     TSTAS6                                                           
                                                                                
TSTAS1   CLI   0(R3),SUBMITQ       submitted                                    
         JNE   TSTAS2                                                           
         TM    TST.ORDKSTA2,ORDSSUBM                                            
         JZ    TSTASNXT                                                         
         J     TSTAS6                                                           
                                                                                
TSTAS2   CLI   0(R3),PARTAPQ       part approved                                
         JNE   TSTAS3                                                           
         TM    TST.ORDKSTA2,ORDSPAPP                                            
         JZ    TSTASNXT                                                         
         J     TSTAS6                                                           
                                                                                
TSTAS3   CLI   0(R3),FULAPPQ       fully approved                               
         JNE   TSTAS4                                                           
         TM    TST.ORDKSTA2,ORDSAPPR                                            
         JZ    TSTASNXT                                                         
         J     TSTAS6                                                           
                                                                                
TSTAS4   CLI   0(R3),REJECTQ       rejected                                     
         JNE   TSTAS5                                                           
         TM    TST.ORDKSTA2,ORDSOREJ                                            
         JZ    TSTASNXT                                                         
         J     TSTAS6                                                           
                                                                                
TSTAS5   CLI   0(R3),DELETEQ       deleted                                      
         JNE   TSTASNXT                                                         
         TM    TST.ORDKSTAT,ORDSLDEL+ORDSDEL                                    
         JZ    TSTASNXT                                                         
         J     TSTAS6                                                           
                                                                                
TSTASNXT AHI   R3,1                                                             
         J     TSTAS0                                                           
                                                                                
TSTASORS CLI   ORSTAT,YESQ         'OR' with my appr status - then              
         JNE   TSTASN              if no match so far try it                    
         J     TSTAS6B                                                          
                                                                                
TSTAS6   MVI   ORSTOK,YESQ         memorise passed this test                    
                                                                                
TSTAS6A  CLI   ORSTAT,YESQ         'OR' with my appr status - if so             
         JE    TSTASY              this is a match anyway so skip               
         CLI   R_OLMYST,SPACEQ                                                  
         JNH   TSTASY                                                           
                                                                                
TSTAS6B  CLI   R_OLMYST,AWMYAPRQ   awaiting my approvals                        
         JNE   TSTAS7                                                           
***      TM    TST.ORDKSTA2,ORDSSUBM+ORDSPAPP+ORDSOREJ                          
         TM    TST.ORDKSTA2,ORDSSUBM+ORDSPAPP                                   
         JZ    TSTASN                                                           
         TM    TST.ORDKSTAT,ORDSLDEL                                            
         JNZ   TSTASN                                                           
         J     TSTASY                                                           
                                                                                
TSTAS7   CLI   R_OLMYST,APPBYMEQ   approved by me                               
         JNE   TSTAS8                                                           
         TM    TST.ORDKSTA2,ORDSSUBM+ORDSOREJ                                   
         JNZ   TSTASN                                                           
         TM    TST.ORDKSTAT,ORDSLDEL                                            
         JNZ   TSTASN                                                           
         J     TSTASY                                                           
                                                                                
TSTAS8   CLI   R_OLMYST,REJBYMEQ   rejected by me                               
         JNE   TSTAS9                                                           
         TM    TST.ORDKSTA2,ORDSOREJ                                            
         JZ    TSTASN                                                           
         TM    TST.ORDKSTAT,ORDSLDEL                                            
         JNZ   TSTASN                                                           
         J     TSTASY                                                           
                                                                                
TSTAS9   CLI   R_OLMYST,AWOTHERQ   awaiting others to approve                   
         JNE   *+2                                                              
         TM    TST.ORDKSTA2,ORDSSUBM+ORDSPAPP+ORDSOREJ                          
         JZ    TSTASN                                                           
         TM    TST.ORDKSTAT,ORDSLDEL                                            
         JNZ   TSTASN                                                           
                                                                                
TSTASY   J     EXITY                                                            
TSTASN   J     EXITN                                                            
         DROP  TST                                                              
                                                                                
**********************************************************************          
* Get supplier account name                                          *          
**********************************************************************          
                                                                                
GETSAN   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLC   O_SUPAC+2(L'O_SUPAC-2),SPACES                                    
         JNH   GETSANX                                                          
                                                                                
         USING BUFFERD,R2                                                       
         LA    R2,TSAROLBF                                                      
         XC    TSAROLBF,TSAROLBF                                                
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKACCQ                                                 
         MVC   BUFKVAL(L'O_SUPAC),O_SUPAC                                       
         MVC   SVTSAREA,TSAROLBF                                                
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    GETSAN10                                                         
         CLI   TSARERRS,TSEEOF                                                  
         JE    GETSAN10                                                         
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                                                              
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
         MVI   REREAD,YESQ                                                      
         MVC   TEMP2(14),O_SUPAC                                                
         GOTOR (#GETACN,AGETACN)                                                
         MVC   BUFRACCN,TEMP2                                                   
*                                                                               
         CLC   TEMP2,SPACES        Account found?                               
         JNH   GETSAN08                                                         
         L     R3,AIO3                                                          
         AHI   R3,ACTRFST-ACTRECD                                               
*                                                                               
         USING RSTELD,R3                                                        
GETSAN0  CLI   RSTEL,0                                                          
         JE    GETSAN08                                                         
         CLI   RSTEL,RSTELQ                                                     
         JE    GETSAN04                                                         
         CLI   RSTEL,FFTELQ                                                     
         JE    GETSAN06                                                         
GETSAN02 LLC   R0,RSTLN                                                         
         AR    R3,R0                                                            
         J     GETSAN0                                                          
*                                                                               
GETSAN04 MVI   BUFGAPIY,NOQ                                                     
         CLI   RSTLN,RSTLN2Q       Check el is long enough                      
         JNH   GETSAN02                                                         
*&&UK*&& TM    RSTSTAT7,RSTGAPQY   GAP in use?                                  
*&&US*&& TM    RSTSTAT7,RSTGAPYN   GAP supplier setting                         
         JZ    GETSAN02                                                         
         MVI   BUFGAPIY,YESQ                                                    
         J     GETSAN02                                                         
*                                                                               
         USING FFTELD,R3                                                        
GETSAN06 DS    0H                                                               
*&&US*&& CLI   FFTTYPE,FFTTEML    Test whether type is email                    
*&&UK*&& CLI   FFTTYPE,FFTTPEML    Supplier email address                       
         JNE   GETSAN02                                                         
         LLC   RE,FFTLN                                                         
         SHI   RE,(FFTDATA-FFTELD)+1                                            
         LTR   RE,RE                                                            
         JM    GETSAN02                                                         
         BASR  R1,0                                                             
         MVC   BUFGAPEM(0),FFTDATA                                              
         EX    RE,0(R1)                                                         
         OC    BUFGAPEM,SPACES                                                  
         J     GETSAN02                                                         
                                                                                
GETSAN08 GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    GETSAN10                                                         
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
                                                                                
GETSAN10 DS    0H                                                               
         MVC   O_SUPNM,BUFRACCN                                                 
         MVC   O_SUPEM,BUFGAPEM                                                 
         MVC   O_GAPIY,BUFGAPIY                                                 
         DROP  R2                                                               
                                                                                
GETSANX  J     EXITY                                                            
                                                                                
**********************************************************************          
* Get expense account name                                           *          
**********************************************************************          
                                                                                
GETEAN   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLC   O_EXPAC+2(L'O_EXPAC-2),SPACES                                    
         JNH   GETEANX                                                          
                                                                                
         USING BUFFERD,R2                                                       
         LA    R2,TSAROLBF                                                      
         XC    TSAROLBF,TSAROLBF                                                
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKACCQ                                                 
         MVC   BUFKVAL(L'O_EXPAC),O_EXPAC                                       
         MVC   SVTSAREA,TSAROLBF                                                
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    GETEAN2                                                          
         CLI   TSARERRS,TSEEOF                                                  
         JE    GETEAN2                                                          
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                                                              
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
         MVI   REREAD,YESQ                                                      
         MVC   TEMP2(14),O_EXPAC                                                
         GOTOR (#GETACN,AGETACN)                                                
         MVC   BUFRACCN,TEMP2                                                   
                                                                                
         GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    GETEAN2                                                          
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
                                                                                
GETEAN2  DS    0H                                                               
         MVC   O_EXPNM,BUFRACCN                                                 
         DROP  R2                                                               
                                                                                
GETEANX  J     EXITY                                                            
                                                                                
**********************************************************************          
* Get client, product and job names                                  *          
**********************************************************************          
                                                                                
GETCPJ   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING BUFFERD,R2                                                       
         LA    R2,TSAROLBF                                                      
                                                                                
         XC    TSAROLBF,TSAROLBF   Client                                       
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKACCQ                                                 
         MVC   BUFKVAL(14),SPACES                                               
         LLC   R1,PCLILEN                                                       
         AHI   R1,2-1                                                           
         MVC   BUFKVAL(0),O_CPJULA                                              
         EX    R1,*-6                                                           
         MVC   SVTSAREA,TSAROLBF                                                
         MVC   PREVCPJ,BUFKVAL                                                  
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    GETCPJ2                                                          
         CLI   TSARERRS,TSEEOF                                                  
         JE    GETCPJ2                                                          
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                                                              
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
         MVI   REREAD,YESQ                                                      
         MVC   TEMP2(14),BUFKVAL                                                
         GOTOR (#GETACN,AGETACN)                                                
         MVC   BUFRACCN,TEMP2                                                   
                                                                                
         GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    GETCPJ2                                                          
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
                                                                                
GETCPJ2  DS    0H                                                               
         MVC   O_CLINM,BUFRACCN                                                 
                                                                                
         LLC   R1,PCLILEN          Product                                      
         LA    R1,O_CPJULA+2(R1)                                                
         LLC   RF,PPROLEN                                                       
         SHI   RF,1                                                             
         EX    RF,GETCPJ2C                                                      
         JNH   GETCPJX                                                          
                                                                                
GETCPJ2C CLC   0(0,R1),SPACES                                                   
                                                                                
         XC    TSAROLBF,TSAROLBF                                                
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKACCQ                                                 
         MVC   BUFKVAL(14),SPACES                                               
         LLC   R1,PPROLEN                                                       
         AHI   R1,2-1                                                           
         MVC   BUFKVAL(0),O_CPJULA                                              
         EX    R1,*-6                                                           
         MVC   SVTSAREA,TSAROLBF                                                
                                                                                
         CLC   PREVCPJ,BUFKVAL                                                  
         JE    GETCPJ5                                                          
         MVC   PREVCPJ,BUFKVAL                                                  
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    GETCPJ4                                                          
         CLI   TSARERRS,TSEEOF                                                  
         JE    GETCPJ4                                                          
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                                                              
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
         MVI   REREAD,YESQ                                                      
         MVC   TEMP2(14),BUFKVAL                                                
         GOTOR (#GETACN,AGETACN)                                                
         MVC   BUFRACCN,TEMP2                                                   
                                                                                
         GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    GETCPJ4                                                          
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
                                                                                
GETCPJ4  DS    0H                                                               
         MVC   O_PRONM,BUFRACCN                                                 
                                                                                
GETCPJ5  DS    0H                  Job                                          
         LLC   R1,PPROLEN                                                       
         LA    R1,O_CPJULA+2(R1)                                                
         CLI   0(R1),SPACEQ                                                     
         JNH   GETCPJX                                                          
                                                                                
         XC    TSAROLBF,TSAROLBF                                                
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKACCQ                                                 
         MVC   BUFKVAL(14),O_CPJULA                                             
         MVC   SVTSAREA,TSAROLBF                                                
                                                                                
         CLC   PREVCPJ,BUFKVAL                                                  
         JE    GETCPJX                                                          
         MVC   PREVCPJ,BUFKVAL                                                  
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    GETCPJ9                                                          
         CLI   TSARERRS,TSEEOF                                                  
         JE    GETCPJ9                                                          
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                                                              
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
         MVI   REREAD,YESQ                                                      
         MVC   TEMP2(14),BUFKVAL                                                
         GOTOR (#GETACN,AGETACN)                                                
         MVC   BUFRACCN,TEMP2                                                   
         CLC   BUFRACCN,SPACES                                                  
         JNH   GETCPJ8                                                          
         MVC   BUFRACCS,IOKEY+ACTKSTAT-ACTRECD                                  
                                                                                
GETCPJ8  GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    GETCPJ9                                                          
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
                                                                                
GETCPJ9  MVC   O_JOBNM,BUFRACCN                                                 
         MVI   O_JSTAT,C'3'        close                                        
         TM    BUFRACCS,ACTSCLOS                                                
         JNZ   GETCPJX                                                          
         MVI   O_JSTAT,C'2'        locked                                       
         TM    BUFRACCS,ACTSLOCK                                                
         JNZ   GETCPJX                                                          
         MVI   O_JSTAT,C'1'        open                                         
                                                                                
GETCPJX  J     EXITY                                                            
         DROP  R2                                                               
                                                                                
**********************************************************************          
* Get agent code and name                                            *          
**********************************************************************          
                                                                                
GETAGNT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLC   O_SUPAC+2(L'O_SUPAC-2),SPACES                                    
         JNH   GETAGNTX                                                         
                                                                                
         OC    ARTLVLS,ARTLVLS     ledger levels resolved?                      
         JNZ   GETAGNT1                                                         
         MVI   REREAD,YESQ                                                      
         MVC   LDGAUL,O_SUPAC                                                   
         GOTOR (#SETLDG,ASETLDG)                                                
         MVC   ARTLVLS,LDGAL1                                                   
                                                                                
GETAGNT1 LA    R1,ARTLVLS+L'ARTLVLS-1                                           
         LHI   R0,L'ARTLVLS-1                                                   
         BASR  RF,0                                                             
         CLI   0(R1),0             agent is on penultimate level                
         JH    GETAGNT2                                                         
         SHI   R1,1                                                             
         BCTR  R0,RF                                                            
         J     GETAGNTX            done                                         
                                                                                
         USING BUFFERD,R2                                                       
GETAGNT2 LA    R2,TSAROLBF                                                      
         XC    TSAROLBF,TSAROLBF                                                
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKACCQ                                                 
         MVC   BUFKVAL(14),SPACES                                               
         SHI   R1,1                                                             
         LLC   RF,0(R1)                                                         
         AHI   RF,2-1                                                           
         MVC   BUFKVAL(0),O_SUPAC                                               
         EX    RF,*-6                                                           
         MVC   SVTSAREA,TSAROLBF                                                
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    GETAGNT4                                                         
         CLI   TSARERRS,TSEEOF                                                  
         JE    GETAGNT4                                                         
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                                                              
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
         MVI   REREAD,YESQ                                                      
         MVC   TEMP2(14),O_SUPAC                                                
         GOTOR (#GETACN,AGETACN)                                                
         MVC   BUFRACCN,TEMP2                                                   
                                                                                
         GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    GETAGNT4                                                         
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
                                                                                
GETAGNT4 MVC   O_AGENT,BUFKVAL                                                  
         MVC   O_ANAME,BUFRACCN                                                 
                                                                                
GETAGNTX J     EXITY                                                            
         DROP  R2                                                               
                                                                                
**********************************************************************          
* Get estimate name                                                  *          
**********************************************************************          
                                                                                
GETESTN  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLC   O_ESTNO,SPACES                                                   
         JNH   GETESTNX                                                         
                                                                                
         USING BUFFERD,R2                                                       
         LA    R2,TSAROLBF                                                      
         XC    TSAROLBF,TSAROLBF                                                
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKESTQ                                                 
         MVC   BUFKVAL(L'O_ESTNO),O_ESTNO                                       
         MVC   SVTSAREA,TSAROLBF                                                
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    GETESTN9                                                         
         CLI   TSARERRS,TSEEOF                                                  
         JE    GETESTN9                                                         
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                                                              
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
         MVI   REREAD,YESQ                                                      
                                                                                
         USING EGNPASD,R3                                                       
         LA    R3,IOKEY                                                         
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,AGENCY                                                   
         MVC   EGNPNUM,O_ESTNO                                                  
         MVC   IOKEYSAV,EGNPAS                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         CLC   IOKEYSAV(EGNPCLI-EGNPASD),IOKEY                                  
         JNE   GETESTN6                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
         L     R3,AIO1                                                          
         AHI   R3,ESTRFST-ESTRECD                                               
         DROP  R3                                                               
                                                                                
         USING ENMELD,R3                                                        
GETESTN1 CLI   ENMEL,ENMELQ                                                     
         JE    GETESTN3                                                         
         CLI   ENMEL,0                                                          
         JE    GETESTN6                                                         
                                                                                
GETESTN2 LLC   R0,ENMLN                                                         
         AR    R3,R0                                                            
         J     GETESTN1                                                         
                                                                                
GETESTN3 LLC   RE,ENMLN                                                         
         SHI   RE,ENMLNQ+1                                                      
         CHI   RE,0                                                             
         JL    GETESTN2                                                         
         CHI   RE,L'BUFRESTN-1                                                  
         JNH   GETESTN4                                                         
         LHI   RE,L'BUFRESTN-1                                                  
                                                                                
GETESTN4 MVC   BUFRESTN(0),ENMNAME                                              
         EX    RE,*-6                                                           
         J     GETESTN2                                                         
                                                                                
GETESTN6 GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    GETESTN9                                                         
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
                                                                                
GETESTN9 DS    0H                                                               
         MVC   O_ESTNAM,BUFRESTN                                                
         DROP  R2,R3                                                            
                                                                                
GETESTNX J     EXITY                                                            
                                                                                
**********************************************************************          
* Get expenditure type name                                          *          
**********************************************************************          
                                                                                
GETETN   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING BUFFERD,R2                                                       
         LA    R2,TSAROLBF                                                      
         XC    TSAROLBF,TSAROLBF                                                
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKETYQ                                                 
         MVC   BUFKVAL(L'O_ETYP),O_ETYP                                         
         MVC   SVTSAREA,TSAROLBF                                                
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    GETETN9                                                          
         CLI   TSARERRS,TSEEOF                                                  
         JE    GETETN9                                                          
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                                                              
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
         MVI   REREAD,YESQ                                                      
                                                                                
         USING ETYRECD,R3                                                       
         LA    R3,IOKEY                                                         
         XC    ETYKEY,ETYKEY                                                    
         MVI   ETYKTYP,ETYKTYPQ                                                 
         MVI   ETYKSUB,ETYKSUBQ                                                 
         MVC   ETYKCPY,AGENCY                                                   
         MVC   ETYKCODE,BUFKVAL                                                 
         MVC   IOKEYSAV,ETYKEY                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         CLC   IOKEYSAV(ETYKOFFC-ETYRECD),IOKEY                                 
         JNE   GETETN6                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
         L     R3,AIO1                                                          
         LA    R3,ETYRFST                                                       
         TM    ETYRSTA2,ETYSFAP                                                 
         JNZ   GETETN1                                                          
         MVI   BUFRETSA,YESQ                                                    
         DROP  R3                                                               
                                                                                
         USING NAMELD,R3                                                        
GETETN1  CLI   NAMEL,NAMELQ                                                     
         JE    GETETN3                                                          
         CLI   NAMEL,0                                                          
         JE    GETETN6                                                          
                                                                                
GETETN2  LLC   R0,NAMLN                                                         
         AR    R3,R0                                                            
         J     GETETN1                                                          
                                                                                
GETETN3  LLC   RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         CHI   RE,0                                                             
         JL    GETETN2                                                          
         CHI   RE,L'BUFRETYN-1                                                  
         JNH   GETETN4                                                          
         LHI   RE,L'BUFRETYN-1                                                  
                                                                                
GETETN4  MVC   BUFRETYN(0),NAMEREC                                              
         EX    RE,*-6                                                           
         J     GETETN2                                                          
                                                                                
GETETN6  GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    GETETN9                                                          
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
                                                                                
GETETN9  DS    0H                                                               
         MVC   O_ETNAME,BUFRETYN                                                
         MVC   O_ETSAPP,BUFRETSA                                                
         DROP  R2,R3                                                            
                                                                                
         J     EXITY                                                            
                                                                                
**********************************************************************          
* Get office name                                                    *          
**********************************************************************          
                                                                                
GETOFN   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   TEMP2(2),0(R1)                                                   
         CLC   TEMP2(2),SPACES                                                  
         JNH   GETOFNX                                                          
                                                                                
         USING BUFFERD,R2                                                       
         LA    R2,TSAROLBF                                                      
         XC    TSAROLBF,TSAROLBF                                                
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKOFFQ                                                 
         MVC   BUFKVAL(L'HALF1),TEMP2                                           
         MVC   SVTSAREA,TSAROLBF                                                
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    GETOFN2                                                          
         CLI   TSARERRS,TSEEOF                                                  
         JE    GETOFN2                                                          
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                                                              
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
         MVI   REREAD,YESQ                                                      
                                                                                
         GOTOR (#GETOFN,AGETOFN)   get office name via 2D or OFFREC             
         MVC   BUFROFFN,TEMP2                                                   
                                                                                
         GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    GETOFN2                                                          
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
                                                                                
GETOFN2  DS    0H                                                               
         MVC   O_OFFNAM,BUFRETYN                                                
         DROP  R2                                                               
                                                                                
GETOFNX  J     EXITY                                                            
                                                                                
***********************************************************************         
* End of requests                                                     *         
***********************************************************************         
                                                                                
REQUESTX LKREQ X                                                                
                                                                                
         LKARY T                                                                
                                                                                
***********************************************************************         
* Local routines                                                      *         
***********************************************************************         
                                                                                
***********************************************************************         
* Trace request                                                       *         
***********************************************************************         
                                                                                
TRACEIT  NTR1  BASE=*,LABEL=*      Trace in SYSPRINT (offline)                  
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test offline                                 
         JZ    TRACEITX                                                         
                                                                                
         L     RF,ARUNFACS         Get A(RUNFACS)                               
         L     R3,RCPRINT-RUNFACSD(RF)                                          
         AHI   R3,P_P-P_DPRINT                                                  
         USING PTRACED,R3                                                       
                                                                                
         XR    R2,R2                                                            
         ICM   R2,3,LP_QMAPN                                                    
                                                                                
         CHI   R1,0                Start of request                             
         JNE   TRACIT70                                                         
                                                                                
         L     RF,ARUNFACS         Get A(RUNFACS)                               
         MVC   LIOTRACE,RTOTSIO-RUNFACSD(RF)                                    
                                                                                
         MVC   PTTWHO,TAOLD                                                     
         CHI   R2,A#AOLD                                                        
         JE    TRACIT02                                                         
         MVC   PTTWHO,TALKE                                                     
         CHI   R2,A#ALKE                                                        
         JE    TRACIT02                                                         
         MVC   PTTWHO,TUNKN                                                     
                                                                                
TRACIT02 MVC   PTTTIME,TTIMED                                                   
         THMS                                                                   
         ST    R1,FULL1                                                         
         UNPK  WORK(6),FULL1                                                    
         MVC   PTHH,WORK           Set HH:MM:SS                                 
         MVI   PTCO1,C':'                                                       
         MVC   PTMM,WORK+2                                                      
         MVI   PTCO2,C':'                                                       
         OI    WORK+5,X'F0'                                                     
         MVC   PTSS,WORK+4                                                      
                                                                                
         MVC   PTTALPH,TALPHAID                                                 
         MVC   PTALPH,CUAALF                                                    
         MVC   PTTUSER,TUSERPID                                                 
         XOUT  CCTPID,PTUSER,2     (CUPASS may be empty)                        
                                                                                
         CHI   R2,A#AOLD                                                        
         JNE   TRACIT60                                                         
                                                                                
         MVC   PTFIELD(L'TDETS),TDETS                                           
         LA    R4,PFIELD                                                        
         LA    R3,L'PFIELD-1(R4)                                                
                                                                                
         MVC   0(5,R4),=C'View='                                                
         MVC   5(1,R4),R_OLVIEW                                                 
         AHI   R4,5+1+1                                                         
                                                                                
         CLC   R_OLASTA,SPACES                                                  
         JNH   TRACIT04                                                         
         MVC   0(5,R4),=C'ASta='                                                
         MVC   5(L'R_OLASTA-1,R4),R_OLASTA                                      
         AHI   R4,5+1+L'R_OLASTA-1                                              
         CLI   0(R4),SPACEQ                                                     
         JH    *+12                                                             
         SHI   R4,1                                                             
         J     *-12                                                             
         AHI   R4,2                                                             
                                                                                
TRACIT04 CLI   R_OLMSTA,0                                                       
         JE    TRACIT10                                                         
         MVC   0(5,R4),=C'MSta='                                                
         AHI   R4,5                                                             
         TM    R_OLMSTA,UNMATQ                                                  
         JZ    TRACIT05                                                         
         MVI   0(R4),C'1'                                                       
         AHI   R4,1                                                             
TRACIT05 TM    R_OLMSTA,PARMATQ                                                 
         JZ    TRACIT06                                                         
         MVI   0(R4),C'2'                                                       
         AHI   R4,1                                                             
TRACIT06 TM    R_OLMSTA,COMMATQ                                                 
         JZ    TRACIT08                                                         
         MVI   0(R4),C'3'                                                       
         AHI   R4,1                                                             
TRACIT08 TM    R_OLMSTA,CANCLDQ                                                 
         JZ    TRACIT09                                                         
         MVI   0(R4),C'4'                                                       
         AHI   R4,1                                                             
TRACIT09 AHI   R4,1                                                             
                                                                                
TRACIT10 CLI   R_OLMYST,SPACEQ                                                  
         JNH   TRACIT12                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(5,R4),=C'MySt='                                                
         MVC   5(1,R4),R_OLMYST                                                 
         AHI   R4,5+1+1                                                         
                                                                                
TRACIT12 CLC   R_OLCLIC,SPACES                                                  
         JNH   TRACIT14                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(4,R4),=C'Cli='                                                 
         MVC   4(5,R4),R_OLCLIC                                                 
         AHI   R4,4+5+1                                                         
         CLI   0(R4),SPACEQ                                                     
         JH    *+12                                                             
         SHI   R4,1                                                             
         J     *-12                                                             
         AHI   R4,2                                                             
                                                                                
TRACIT14 CLC   R_OCREAT,SPACES                                                  
         JNH   TRACIT16                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(3,R4),=C'Cr='                                                  
         MVC   3(8,R4),R_OCREAT                                                 
         AHI   R4,3+8+1                                                         
         CLI   0(R4),SPACEQ                                                     
         JH    *+12                                                             
         SHI   R4,1                                                             
         J     *-12                                                             
         AHI   R4,2                                                             
                                                                                
TRACIT16 CLC   R_OLETYP,SPACES                                                  
         JNH   TRACIT18                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(3,R4),=C'ET='                                                  
         MVC   3(3,R4),R_OLETYP                                                 
         AHI   R4,3+3+1                                                         
                                                                                
TRACIT18 CLC   R_OLTYPE,SPACES                                                  
         JNH   TRACIT20                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(3,R4),=C'Ty='                                                  
         MVC   3(1,R4),R_OLTYPE                                                 
         AHI   R4,3+1+1                                                         
                                                                                
TRACIT20 CLC   R_OLINCJ,SPACES                                                  
         JH    TRACIT22                                                         
         CLC   R_OLINLJ,SPACES                                                  
         JNH   TRACIT24                                                         
TRACIT22 CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(6,R4),=C'J_C_L='                                               
         MVC   6(1,R4),R_OLINCJ                                                 
         MVI   7(R4),C'/'                                                       
         MVC   8(1,R4),R_OLINLJ                                                 
         AHI   R4,6+3+1                                                         
                                                                                
TRACIT24 CLC   R_OLSDAT,SPACES                                                  
         JNH   TRACIT26                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(3,R4),=C'SD='                                                  
         MVC   3(8,R4),R_OLSDAT                                                 
         AHI   R4,3+8+1                                                         
                                                                                
TRACIT26 CLC   R_OLEDAT,SPACES                                                  
         JNH   TRACIT28                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(3,R4),=C'ED='                                                  
         MVC   3(8,R4),R_OLEDAT                                                 
         AHI   R4,3+8+1                                                         
                                                                                
TRACIT28 CLC   R_OLSUPC,SPACES                                                  
         JNH   TRACIT30                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(4,R4),=C'Sup='                                                 
         MVC   4(2,R4),R_OLSUPC    U/L only                                     
         AHI   R4,4+2+1                                                         
                                                                                
TRACIT30 CLC   R_OLIEXP(L'R_OLIEXP+L'R_OLIINT+L'R_OLIPRO),SPACES                
         JNH   TRACIT38                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(5,R4),=C'Incl='                                                
         AHI   R4,5                                                             
         CLI   R_OLIEXP,YESQ                                                    
         JNE   TRACIT32                                                         
         MVI   0(R4),EXPORDQ                                                    
         AHI   R4,1                                                             
TRACIT32 CLI   R_OLIINT,YESQ                                                    
         JNE   TRACIT34                                                         
         MVI   0(R4),INTORDQ                                                    
         AHI   R4,1                                                             
TRACIT34 CLI   R_OLIPRO,YESQ                                                    
         JNE   TRACIT36                                                         
         MVI   0(R4),PRODORDQ                                                   
         AHI   R4,1                                                             
TRACIT36 AHI   R4,1                                                             
                                                                                
TRACIT38 CLI   XORNAL,0            Name Search                                  
         JE    TRACIT40                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(6,R4),=C'NSrch='                                               
         MVC   6(10,R4),XORNAM                                                  
         AHI   R4,6+10+1                                                        
         CLI   XORNAM#,1                                                        
         JNH   TRACIT40                                                         
         SHI   R4,1                                                             
         MVI   0(R4),C'/'                                                       
         XOUT  XORNAM#,1(R4),1                                                  
         AHI   R4,1+2+1                                                         
                                                                                
TRACIT40 CLC   SRCHAPP,SPACES                                                   
         JNH   TRACIT42                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(3,R4),=C'Ap='                                                  
         MVC   3(8,R4),SRCHAPP                                                  
         AHI   R4,3+8+1                                                         
         CLI   0(R4),SPACEQ                                                     
         JH    *+12                                                             
         SHI   R4,1                                                             
         J     *-12                                                             
         AHI   R4,2                                                             
                                                                                
TRACIT42 CLC   R_OLOFFC,SPACES                                                  
         JNH   TRACIT44                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(3,R4),=C'OF='                                                  
         MVC   3(2,R4),R_OLOFFC                                                 
         AHI   R4,3+2+1                                                         
                                                                                
TRACIT44 CLC   R_OLCURR,SPACES                                                  
         JNH   TRACIT46                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(4,R4),=C'CUR='                                                 
         MVC   4(3,R4),R_OLCURR                                                 
         AHI   R4,4+3+1                                                         
                                                                                
TRACIT46 CLC   R_OLESTN,SPACES                                                  
         JNH   TRACIT48                                                         
         CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(4,R4),=C'EST='                                                 
         MVC   4(6,R4),R_OLESTN                                                 
         AHI   R4,4+6+1                                                         
                                                                                
TRACIT48 CLC   R_OLRBDS,SPACES                                                  
         JH    TRACIT50                                                         
         CLC   R_OLRBDE,SPACES                                                  
         JNH   TRACIT52                                                         
TRACIT50 CR    R4,R3                                                            
         JH    TRACIT58                                                         
         MVC   0(4,R4),=C'RBD*'                                                 
         AHI   R4,4+0+1                                                         
                                                                                
TRACIT52 DS    0H                                                               
         J     TRACIT90                                                         
                                                                                
TRACIT58 MVC   0(L'TETC,R4),TETC                                                
         J     TRACIT90                                                         
                                                                                
TRACIT60 CHI   R2,A#ALKE           no extra details for this download           
         JNE   TRACIT90                                                         
         J     TRACIT90                                                         
                                                                                
TRACIT70 CHI   R1,1                IO tracing                                   
         JNE   TRACEITX                                                         
                                                                                
         CHI   R2,A#AOLD                                                        
         JE    TRACIT72                                                         
         CHI   R2,A#ALKE                                                        
         JNE   TRACEITX                                                         
                                                                                
TRACIT72 MVC   PTTIO(L'TIOTRACE),TIOTRACE                                       
         L     R0,LIOTRACE                                                      
         EDITR (R0),(8,PTTIO+12),0,ZERO=NOBLANK                                 
         MVC   PTTIO+12+8+2(2),TFROMTO                                          
         L     RF,ARUNFACS         Get A(RUNFACS)                               
         L     R0,RTOTSIO-RUNFACSD(RF)                                          
         EDITR (R0),(8,PTTIO+12+8+2+2+2),0,ZERO=NOBLANK                         
         J     TRACIT90                                                         
                                                                                
TRACIT90 GOTO1 APRINTER                                                         
                                                                                
TRACEITX J     EXITY                                                            
                                                                                
TTIMED   DC    C'Time:'                                                         
TALPHAID DC    C'AlphaID:'                                                      
TUSERPID DC    C'PID:'                                                          
TAOLD    DC    C'A#AOLD'                                                        
TALKE    DC    C'A#ALKE'                                                        
TUNKN    DC    C'A#????'                                                        
TDETS    DC    C'Details:'                                                      
TIOTRACE DC    C'I/O Trace:'                                                    
TETC     DC    C'...'                                                           
TFROMTO  DC    C'=>'                                                            
                                                                                
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Filter record against limit list (copied from ACBRA10)              *         
***********************************************************************         
                                                                                
         USING GAPTABD,R4                                                       
         USING ORDRECD,R2                                                       
         USING ORDELD,R3                                                        
CHKVIEW  NTR1  BASE=*,LABEL=*                                                   
         TM    LLINDS,LLINREQ      skip if not required                         
         JNZ   EXITY                                                            
*                                                                               
         MVI   LLINDS,0             Clear limit list indicators                 
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
         CLC   SJACCNT,SPACES      Any SJ account on order?                     
         JH    CHKVW06                                                          
         OI    LLINDS,LLINCQ       No - set they can access this                
         CLI   GAPTDAT1,GAPTT2Q    Skip all SJ entries in buffer                
         JE    CHKVW02                                                          
         J     CHKVW08                                                          
                                                                                
CHKVW06  CLI   GAPTDAT1,GAPTT2Q    Skip all SJ entries in buffer                
         JNE   CHKVW08                                                          
*                                                                               
         CLC   GAPTACC,SPACES      if 1st entry is spaces, means...             
         JH    *+12                no entries + default access=yes              
         OI    LLINDS,LLINCQ       Set we have access to view                   
         J     CHKVW02                                                          
         LLC   RF,GAPTLEN          If we have an entry do a compare             
         SHI   RF,1+L'GAPTCOFF                                                  
         BASR  RE,0                                                             
         CLC   GAPTACC(0),SJACCNT                                               
         EX    RF,0(RE)            On account                                   
         JNE   CHKVW02                                                          
         CLC   GAPTCOFF,ORDROFF    On office                                    
         JNE   CHKVW02                                                          
         OI    LLINDS,LLINCQ       If matches set we can view order             
         J     CHKVW02                                                          
*                                                                               
CHKVW08  CLI   GAPTDAT1,GAPTT5Q    Media code                                   
         JNE   CHKVW12                                                          
         CLC   GAPTACT,SPACES      if 1st entry is spaces                       
         JNH   *+14                means default access=y + no limlist          
         CLC   SJACCNT,SPACES      Any SJ account on order?                     
         JH    CHKVW10                                                          
         OI    LLINDS,LLINMQ                                                    
         J     CHKVW02                                                          
*                                                                               
CHKVW10  LLC   RF,PPROLEN          Check match on media code in job             
         LA    RF,SJACCNT(RF)                                                   
         CLC   GAPTMEDI,0(RF)                                                   
         JNE   CHKVW02                                                          
         OI    LLINDS,LLINMQ                                                    
         J     CHKVW02                                                          
*                                                                               
CHKVW12  CLI   GAPTDAT1,GAPTT7Q    Expenditure type code                        
         JNE   CHKVW02             ignore 06 (SJ+off+med) and 08 (sup)          
         CLC   GAPTETYP,SPACES     if 1st entry is spaces                       
         JNH   *+14                means default access=y + no limlist          
         CLC   ORDREXTY,GAPTETYP   Match on expenditure type?                   
         JNE   CHKVW02                                                          
         OI    LLINDS,LLINEQ                                                    
         J     CHKVW02                                                          
*                                                                               
CHKVW14  TM    LLINDS,LLINCQ+LLINMQ+LLINEQ         Have access?                 
         JO    EXITY                                                            
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Test Account security on SE/SQ                                      *         
* NTRY R1=A(ULACC)                                                    *         
***********************************************************************         
*&&UK                                                                           
CHKLASC  NTR1  BASE=*,LABEL=*                                                   
         L     R7,ATWA             R7=A(ON/OFFLINE TWA)                         
         USING TWAD,R7                                                          
                                                                                
         CLC   0(L'ACTKUNT+L'ACTKLDG,R1),SPACES                                 
         JNH   EXITY                                                            
         CLC   L'ACTKUNT+L'ACTKLDG(L'ACTKACT,R1),SPACES                         
         JNH   EXITY                                                            
                                                                                
         USING BUFFERD,R2                                                       
         LA    R2,TSAROLBF                                                      
         XC    TSAROLBF,TSAROLBF                                                
         LHI   R0,BUFFRLNQ                                                      
         STCM  R0,B'0011',BUFLEN                                                
         MVI   BUFKTYP,BUFKACSQ                                                 
         MVC   BUFKVAL(L'ACTKULA),0(R1)                                         
         MVC   SVTSAREA,TSAROLBF                                                
                                                                                
         GOTOR GOTSAR,DMCB,('TSARDH',OLBUF),0                                   
         JE    CLASC10                                                          
         CLI   TSARERRS,TSEEOF                                                  
         JE    CLASC10             ?                                            
         TM    TSARERRS,TSERNF     Test record not found                        
         JZ    *+2                 Some other error                             
                                                                                
         MVC   TSAROLBF,SVTSAREA                                                
                                                                                
         MVC   CSVKEY2,IOKEY       Not in buffer, so read account               
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,BUFKVAL                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   *+2                                                              
         L     R4,AIO3                                                          
         AHI   R4,ACTRFST-ACTRECD                                               
         USING RSTELD,R4                                                        
         SR    R0,R0                                                            
CLASC02  CLI   RSTEL,0                                                          
         JE    *+2                 No RSTEL?                                    
         CLI   RSTEL,RSTELQ                                                     
         JE    *+14                                                             
         IC    R0,RSTLN                                                         
         AR    R4,R0                                                            
         J     CLASC02                                                          
                                                                                
         MVI   BUFRASC,NOQ                                                      
         CLC   RSTSECY+1(1),TWAAUTH+1  Test old security                        
         JH    CLASC06                 Fails                                    
         MVI   BUFRASC,YESQ                                                     
         TM    CPYSTAT4,CPYSOFF2   Test 2CO agency                              
         JZ    CLASC06                                                          
         TM    CPXSTATA,CPXLACAC   Test Limit Account Access in use             
         JZ    CLASC06                                                          
         LA    RF,SEOFPOS                                                       
         CLI   ACTKLDG,C'E'        Test SE                                      
         JE    CLASC04                                                          
         LA    RF,SQOFPOS                                                       
         CLI   ACTKLDG,C'Q'        Test SQ                                      
         JNE   CLASC06                                                          
CLASC04  CLI   0(RF),LDGOTRAN      Only applies to Offpos=T                     
         JNE   CLASC08                                                          
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVI   OFFAOPOS,LDGOTRAN                                                
         MVC   OFFAREC,AIO3                                                     
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         MVI   BUFRASC,YESQ                                                     
         JE    CLASC06                                                          
         MVI   BUFRASC,NOQ                                                      
CLASC06  GOTOR GOTSAR,DMCB,('TSAADD',OLBUF),0                                   
         JE    CLASC08                                                          
         TM    TSARERRS,TSEEOF                                                  
         JZ    *+2                 Buffer full                                  
                                                                                
CLASC08  MVC   IOKEY,CSVKEY2       Restore I/O sequence                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JNE   *+2                                                              
                                                                                
CLASC10  CLI   BUFRASC,YESQ        Set CC                                       
         J     EXIT                                                             
         DROP  R2,R3,R4,R7                                                      
*&&                                                                             
***********************************************************************         
* Interface to TSAR                                                   *         
* P1(1)   = ACTION                                                    *         
* P1(2-4) = A(TSAR BLOCK)                                             *         
* P2(1)   = KEY LENGTH IF INIT                                        *         
* P2(2-4) = A(RECORD IOAREA) IF INIT, SORTCARD IF ACTION IS SORT      *         
* P3(1)   = TSRECI BUFFER ID BITS IF INIT, ONLINE  (TSRXTN ALWAYS SET)*         
* P3(2)   = N/D                                                       *         
* P3(3-4) = MAX REC LENGTH IF INIT                                    *         
* ACQUIRES 1MB OF STORAGE IF OFFLINE                                  *         
* ERROR BITS RETURNED IN TSARERRS, CC SET IF ANY ON                   *         
***********************************************************************         
                                                                                
GOTSAR   NTR1  LABEL=*,BASE=*                                                   
                                                                                
         LR    R2,R1               R2=A(Caller's parameter list)                
TB       USING TSARD,TSARRECS      R3=A(TSAR block)                             
         LA    R1,TSAROLBF         Point to correct TSAR block                  
         CLI   3(R2),OABUF         Doing order approval lookup?                 
         JNE   *+8                                                              
         LA    R1,TSAROABF                                                      
         ST    R1,TB.TSAREC        Address of record buffer area                
                                                                                
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   GOTSAR02                                                         
         XC    TSACNT,TSACNT                                                    
         XC    TB.TSARD(TSPNEWL),TB.TSARD                                       
         MVC   TB.TSACTN,0(R2)                                                  
         MVC   TB.TSACOM,ACOMFACS                                               
         LHI   R0,1024                                                          
         LHI   R0,ONEK                                                          
         CLI   3(R2),OABUF         Doing order approval lookup?                 
         JNE   *+8                                                              
         LHI   R0,2*ONEK                                                        
         OC    TB.TSBUFFL,TB.TSBUFFL                                            
         JNZ   GOTSAR00                                                         
         STCM  R0,3,TB.TSBUFFL      Set require 1MB off-line                    
                                                                                
GOTSAR00 LHI   R0,L'BUFKEY                                                      
         MVI   TB.TSRECI,TSRXTN+TSRMINB1+TSRVAR                                 
         CLI   3(R2),OABUF         Doing order approval lookup?                 
         JNE   *+12                                                             
         LHI   R0,APEKEYL                                                       
         MVI   TB.TSRECI,TSRXTN+TSRWSSVR                                        
         STC   R0,TB.TSKEYL         Set key length                              
         MVI   TB.TSINDS,TSINODSK   Set no disk writes (save/restore)           
         MVI   TB.TSIND2,TSI2MANY                                               
         LHI   R0,BUFFRLNQ                                                      
         CLI   3(R2),OABUF         Doing order approval lookup?                 
         JNE   *+8                                                              
         LHI   R0,APETABL                                                       
         STCM  R0,3,TB.TSRECL       Set maximum record length                   
         GOTOR VTSAR,TB.TSARD                                                   
         TM    TB.TSINDS,TSIINIOK                                               
         JNZ   GOTSARY                                                          
         DC    H'0'                 Initialisation failure                      
                                                                                
GOTSAR02 TM    TB.TSINDS,TSIINIOK   Test initialised                            
         JZ    GOTSAR08                                                         
                                                                                
         MVC   TB.TSACTN,0(R2)      Set action                                  
                                                                                
         CLI   0(R2),TSAADD         Adding?                                     
         JNE   GOTSAR04                                                         
         L     R0,TSACNT                                                        
         AHI   R0,1                                                             
         ST    R0,TSACNT                                                        
                                                                                
GOTSAR04 CLI   TB.TSACTN,TSASRT     Test sorting                                
         JNE   GOTSAR06                                                         
         L     R1,4(R2)                                                         
         MVC   TB.TSRTPARM,0(R1)    Yes - set sort parameters                   
                                                                                
GOTSAR06 GOTOR VTSAR,TB.TSARD       Call TSAR                                   
         MVC   TSARERRS,TB.TSERRS   Return TSARERRS                             
         J     GOTSARX                                                          
                                                                                
GOTSAR08 MVI   TSARERRS,TSEEOF                                                  
                                                                                
GOTSARX  CLI   TSARERRS,0          Set condition code for caller                
         JNE   GOTSARN                                                          
GOTSARY  J     EXITY                                                            
                                                                                
GOTSARN  J     EXITN                                                            
         DROP  TB                                                               
***********************************************************************         
* Setup for order approval limit (copied from ACBRA10)                *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SETAPP   NTR1  BASE=*,LABEL=*                                                   
         ST    R8,LP_ADATA         Store A(data)                                
         OC    R_OACLI,SPACES                                                   
         OC    R_OAPRC,SPACES                                                   
         OC    R_OAJBC,SPACES                                                   
         OC    R_OASUPC,SPACES                                                  
         GOTOR (#ORDPRF,AORDPRF),DMCB,D_ORPF                                    
         GOTOR GOTSAR,DMCB,('TSAINI',OABUF),0                                   
         MVI   OA_INDS,0                                                        
                                                                                
         GOTOR TRACEIT,0           Trace request                                
                                                                                
X        USING ORDPRFD,D_ORPF                                                   
         MVC   D_OFF,SPACES        COMPANY LEVEL APPROVER                       
         TM    CPYSTAT1,CPYSOROE                                                
         JZ    SETA002                                                          
         OC    R_OA2DA,R_OA2DA     ANY OFFICE CODE ?                            
         JZ    SETA002             NO                                           
         XR    RF,RF                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         AHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   D_OFF(0),R_OA2DA    SAVE OFFICE CODE                             
         EX    RF,0(RE)                                                         
*                                                                               
SETA002  XR    RF,RF                                                            
         ICM   RF,3,LP_QMAPN                                                    
         CHI   RF,A#AOLD           Order list call?  Skip below                 
         JE    SETA005             already done in PRESET                       
         MVC   CPJFULA,SPACES                                                   
         CLC   R_OACLI,SPACES      Do we have a client code request             
         JNH   SETA006                                                          
*                                                                               
         USING ACTKULA,R2                                                       
         LA    R2,CPJFULA                                                       
         MVC   ACTKULA(L'PRODUL),PRODUL                                         
         LLC   RF,PCLILEN                                                       
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),R_OACLI                                               
         EX    RF,0(RE)            Move client code to key                      
         LA    R1,ACTKACT+1(RF)                                                 
         LLC   RE,PCLILEN                                                       
         LLC   RF,PPROLEN                                                       
         CLC   R_OAPRC,SPACES                                                   
         JNH   SETA004                                                          
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),R_OAPRC     Move product code to key                     
         EX    RF,0(RE)                                                         
         LA    R1,1(RF,R1)                                                      
         LLC   RE,PPROLEN                                                       
         LLC   RF,PJOBLEN                                                       
         CLC   R_OAJBC,SPACES                                                   
         JNH   SETA004                                                          
         SR    RF,RE                                                            
         CHI   RF,L'GOSELJOB       Job can't JE longer than GOSELJOB            
         JNH   *+8                                                              
         LHI   RF,L'GOSELJOB                                                    
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),R_OAJBC     Move job code to key                         
         EX    RF,0(RE)                                                         
*                                                                               
SETA004  OC    ACTKACT,SPACES      (ensure space filled key)                    
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'CPJFULA),CPJFULA                                         
         GOTOR (#GETACN,AGETACN)   USES AIO3                                    
         CLC   TEMP2(36),SPACES                                                 
         JH    *+12                                                             
         LHI   R0,AE$ACTNF         FILE ERROR                                   
         J     XERROR                                                           
*                                                                               
         L     R0,AIO4             Copy to AIO4                                 
         LA    R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'CPJFULA),CPJFULA                                         
         GOTOR SSJOFF              Read for office                              
         JZ    SETA006                                                          
*                                                                               
SETA005  TM    CPYSTAT1,CPYSOROE                                                
         JZ    SETA006                                                          
         CLI   R_OATYPE,ALIKEXP    if anything other than expense ord           
         JE    SETA006               ensure office matches client prod          
         MVC   D_OFF,D_OFFC        replace office code with cli office          
*                                                                               
SETA006  GOTOR GETOPT                                                           
         MVI   BYTE4,SE#ORDS                                                    
         GOTOR (#SEMAIL,ASEMAIL),DMCB,D_OFF,BYTE4                               
         JNE   *+8                                                              
         OI    OA_INDS,OA_IEML                                                  
         CLC   R_OACLPA,SPACES     any claimant person code                     
         JNH   SETA010                                                          
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2,R_OACLPA      Make sure valid person code                  
         OC    R_OACLPD,R_OACLPD                                                
         JZ    *+10                                                             
         MVC   XL#TODP,R_OACLPD                                                 
         GOTOR PERDTL                                                           
         JE    *+14                                                             
         LLH   R0,FULL2                                                         
         J     XERROR                                                           
         L     R2,ALOCEL                                                        
         LA    R3,D_1RACC                                                       
*                                                                               
SETA008  LLC   RF,ONERL1L          Move into D_1RACC for later                  
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   0(0,R3),LOCOFF-LOCELD(R2)                                        
         EX    RF,0(R1)                                                         
         MVC   D_OFF,LOCOFF-LOCELD(R2)                                          
         AHI   RF,1                                                             
         AR    R3,RF                                                            
         LLC   RF,ONERL2L                                                       
         LLC   R0,ONERL1L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,R3),LOCDEPT-LOCELD(R2)                                       
         EX    RF,0(R1)                                                         
         AHI   RF,1                                                             
         AR    R3,RF                                                            
         LLC   RF,ONERL3L                                                       
         LLC   R0,ONERL2L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,R3),LOCSUB-LOCELD(R2)                                        
         EX    RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         AR    R3,RF                                                            
         MVC   0(L'R_OACLPA,RF),R_OACLPA                                        
*                                                                               
SETA010  MVI   OL_INDS,0           INITALIZE OLIST PROCESS INDICATOR            
         CLC   D_OFF,SPACES        OFFICE SPECIFIED ?                           
         JNH   SETA011             NO, CONTINUE                                 
         GOTOR GETOLST             YES, GET OLIST DETAILS FOR OFFICE            
*                                                                               
SETA011  MVC   ESTAPUN,R_OAAPLC                                                 
         CLC   R_OAJBC,SPACES      Do we have a job                             
         JNH   SETA012             No - can't do an estimate check              
         CLI   ESTAPUN,ALIKINVN    Skip for invoice, invoice                    
         JE    SETA012             no PO                                        
         CLI   ESTAPUN,ALIKINV                                                  
         JE    SETA012                                                          
         GOTOR CHKEST              Perform estimate check                       
         JH    QERROR                                                           
         JL    SETAEXIT            Exit if throwing an estcheck error           
*                                                                               
         USING ALIRECD,R2                                                       
SETA012  ZAP   DUB,R_OATOTA                                                     
         SRP   DUB,64-1,9          / 100, always round up                       
         SRP   DUB,64-1,9                                                       
*                                                                               
         LA    R2,IOKEY                                                         
         XC    ALIKEY,ALIKEY                                                    
         MVI   ALIKTYP,ALIKTYPQ                                                 
         MVI   ALIKSUB,ALIKSUBQ                                                 
         MVC   ALIKCPY,CUXCPY                                                   
         MVC   ALIKCAT,ESTAPUN     READ FOR REQUESTED CATEGORY                  
         MVC   CSVKEY1(L'ALIKEY),ALIKEY                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   ALIKEY(ALIKSCAT-ALIRECD),CSVKEY1                                 
         JE    SETA014                                                          
         CLI   ESTAPUN,ALIKEXPS    Test expense not found                       
         JE    SETAERR6                                                         
         CLI   ESTAPUN,ALIKESTS    Test estimates not found                     
         JE    SETAERR8                                                         
         CLI   ESTAPUN,ALIKOEAP    Test orders apprvd est not found             
         JE    SETAERRA                                                         
         CLI   ESTAPUN,ALIKOEUN    Test orders unapprvd est not found           
         JE    SETAERR9                                                         
         CLI   ESTAPUN,ALIKESTI    Test estimates internal not found            
         JE    SETAERRC                                                         
         CLI   ESTAPUN,ALIKINVN    Test 'invoice no po' not found               
         JNE   *+12                                                             
         MVI   ESTAPUN,ALIKINV     ...try regular invoices                      
         J     SETA012                                                          
         MVI   ESTAPUN,ALIKORD     Otherwise default to orders                  
                                                                                
         USING ALIRECD,R2                                                       
SETA014  LA    R3,APLTAB                                                        
         USING APLTABD,R3                                                       
SETA016  LA    R2,IOKEY            Look up aplimit record                       
         XC    ALIKEY,ALIKEY                                                    
         MVI   ALIKTYP,ALIKTYPQ                                                 
         MVI   ALIKSUB,ALIKSUBQ                                                 
         MVC   ALIKCPY,CUXCPY                                                   
         MVC   ALIKCAT,ESTAPUN     Use requested category/default               
         TM    APLTSTAT,APLTORT1                                                
         JZ    SETA022                                                          
*&&UK                                                                           
         CLI   ESTAPUN,ALIKINV                                                  
         JE    SETA018                                                          
         CLI   ESTAPUN,ALIKINVN                                                 
         JE    SETA018                                                          
         CLI   ESTAPUN,ALIKEXPS                                                 
         JNE   SETA020             (no need for APRTORT1 ?|)                    
SETA018  CLI   R_OATYPE,ALIKDFT    For Inv./EXp. no type means 'Mixed'          
         JH    SETA020                                                          
         MVI   R_OATYPE,ALIKMIX                                                 
SETA020  DS    0H                                                               
*&&                                                                             
         MVC   ALIKSCAT,R_OATYPE   REQUESTED ORDER TYPE                         
         J     SETA028                                                          
*                                                                               
SETA022  TM    APLTSTAT,APLTORT2                                                
         JZ    SETA026                                                          
         CLI   R_OAAMIX,YESQ       Have we a mix of client and none             
         JE    SETA036             Yes - skip this table entry                  
         CLI   R_OAALLC,YESQ       Have we all clients but different            
         JE    SETA024             Yes - set order type as client               
         MVI   ALIKSCAT,ALIKNCLI   TRY NON-CLIENT OR CLIENT TYPE                
         CLC   R_OACLI,SPACES                                                   
         JNH   SETA028                                                          
SETA024  MVI   ALIKSCAT,ALIKCLI                                                 
         J     SETA028                                                          
*                                                                               
SETA026  MVI   ALIKSCAT,ALIKDFT                                                 
*                                                                               
SETA028  TM    APLTSTAT,APLTCLI                                                 
         JZ    SETA030                                                          
         CLC   R_OACLI,SPACES      CLIENT SPECIFIED                             
         JNH   SETA036                                                          
         MVC   ALIKCLIC(L'R_OACLI),R_OACLI                                      
         OC    ALIKCLIC,SPACES                                                  
*                                                                               
SETA030  TM    APLTSTAT,APLTETYP                                                
         JZ    SETA032                                                          
         CLC   R_OAETY,SPACES      CLIENT SPECIFIED                             
         JNH   SETA036                                                          
         MVC   ALIKETYP(L'R_OAETY),R_OAETY                                      
         OC    ALIKETYP,SPACES                                                  
*                                                                               
SETA032  TM    APLTSTAT,APLTOFFC                                                
         JZ    SETA034                                                          
         CLC   D_OFF,SPACES        OFFICE SPECIFIED                             
         JE    SETA036                                                          
         MVC   ALIKOFFC,D_OFF                                                   
*                                                                               
SETA034  MVC   CSVKEY1(L'ALIKEY),ALIKEY                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    SETA038                                                          
SETA036  LA    R3,APLTABL(R3)                                                   
         CLI   APLTSTAT,X'FF'                                                   
         JE    SETAERR2            NO APPLICABLE APLIMIT RECS FOUND             
         J     SETA016                                                          
*                                                                               
SETA038  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO3                                                          
         LA    R4,APLVTAB2         Approver table                               
         LHI   R6,MAXLVL                                                        
                                                                                
         XC    MYCOUNT,MYCOUNT     clear count of levels                        
         LA    R3,ALIRFST                                                       
         USING APLELD,R3                                                        
SETA040  CLI   APLEL,0             END OF RECORD                                
         JE    SETA056                                                          
SETA042  CLI   APLEL,APLELQ        Find approval limit element(S)               
         JE    SETA046                                                          
SETA044  XR    R0,R0                                                            
         IC    R0,APLLN                                                         
         AR    R3,R0                                                            
         J     SETA040                                                          
                                                                                
         USING APLVLTD,R4                                                       
SETA046  ZAP   APLVLVAL,APLVAL     Save approval value                          
         MVI   APLVLEN,APLVLTBL                                                 
         MVI   APLVLID,APLVTIQ     Put indicator in table                       
         MVC   APLVLNUM,APLNUM     N'approvers, this level                      
         MVC   APLVLPRV,APLPREV    N'previous levels                            
         CLI   APLLN,APLLN3Q       New style record                             
         JL    SETA048                                                          
         MVC   APLVLFAP(L'APLVLFAP+L'APLVLHLV),APLFAPP                          
         J     SETA050                                                          
SETA048  TM    ALIRSTAT,ALISFAPP                                                
         JZ    SETA050                                                          
         MVI   APLVLFAP,YESQ                                                    
*                                                                               
SETA050  CP    APLVAL,DUB          Is approval limit great or equal to          
         JL    SETA052             order                                        
         TM    OA_INDS,OA_GREA     Have we saved 1st > than order level         
         JNZ   SETA052                                                          
         ST    R4,ADRAPLVL         Save address of current entry                
         OI    OA_INDS,OA_GREA     Set we saved the address                     
*                                                                               
SETA052  TM    OA_INDS,OA_GREA                                                  
         JNZ   SETA054                                                          
         LLC   RF,MYCOUNT          save no. of previous levels added            
         AHI   RF,1                to table                                     
         STC   RF,MYCOUNT                                                       
SETA054  LA    R4,APLVLTBL(R4)     Bump to next free entry                      
         JCT   R6,SETA044                                                       
         DC    H'0'                Table full!                                  
         DROP  R3                                                               
*                                                                               
SETA056  TM    OA_INDS,OA_GREA     Did we find appropriate level                
         JZ    SETAERR2            No - order is > any approval limit           
         L     R4,ADRAPLVL         R4=A(Highest level approval)                 
*                                                                               
SETA058  ZAP   DUB,APLVLVAL        Save approval value                          
         LHI   R3,1                Init to one level                            
         SR    RF,RF                                                            
         ICM   RF,1,APLVLPRV       N'previous approver levels                   
         JZ    SETA060             None                                         
         CLM   RF,1,MYCOUNT        Do we have enough previous levels            
         JH    SETAERR5            No - error                                   
         AR    R3,RF               N'previous levels plus current one           
         SHI   R4,APLVLTBL         R4=A(previous slot)                          
         JCT   RF,*-4              N times, to get to 1st lvl required          
SETA060  ST    R4,ADRFSLVL         Save A(1st level)                            
         STH   R3,NUMHDR                                                        
*                                                                               
         LA    RF,APLVTAB2                                                      
         SR    R4,RF                                                            
         LA    R0,APLVTAB          Move APLVTAB2 to APLVTAB                     
         LHI   R1,L'APLVTAB        And remove uneeded entries                   
         SR    R1,R4                                                            
         L     RE,ADRFSLVL                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               Store table as it is now, amended            
         USING OLISTD,R7           MAP WITH ORDER LIST                          
         LA    R7,SAVOLIST         ADDRESS OLIST                                
*                                                                               
SETA061  L     R4,ADRFSLVL         R4=A(1ST LEVEL)                              
*                                                                               
SETA062  ST    R4,ADRCULVL                                                      
         MVC   APLVLLVL,=X'0001'      Set approver level                        
         LA    R3,APRTAB                                                        
         USING APRTABD,R3                                                       
         XC    TSAROABF,TSAROABF                                                
P        USING APETABD,TSAROABF                                                 
SETA064  CLI   0(R3),X'FF'                                                      
         JE    SETA154                                                          
         NI    OA_INDS,X'FF'-OA_FOND                                            
         LA    R2,IOKEY            Read approver recs via passives              
         USING APPPASD,R2                                                       
         XC    APPPAS,APPPAS                                                    
         MVI   APPPTYP,APPPTYPQ                                                 
         MVI   APPPSUB,APPPSUBQ                                                 
         MVC   APPPCPY,CUXCPY                                                   
         MVI   APPPCAT,APPPORD                                                  
         CLI   R_OAAPLC,ALIKINV    Invoices?                                    
         JE    *+8                                                              
         CLI   R_OAAPLC,ALIKINVN   Not invoices                                 
         JNE   *+8                                                              
         MVI   APPPCAT,APPPINV                                                  
         CLI   ESTAPUN,ALIKOEAP    Test orders apprvd est not found             
         JNE   *+8                                                              
         MVI   APPPCAT,APPPAEO                                                  
         CLI   ESTAPUN,ALIKOEUN    Test orders unapprvd est not found           
         JNE   *+8                                                              
         MVI   APPPCAT,APPPUEO                                                  
         CLI   ESTAPUN,ALIKESTI    Test internal estimate approver              
         JNE   *+8                                                              
         MVI   APPPCAT,APPPIEA                                                  
         CLI   ESTAPUN,ALIKESTS    Test client estimate approver                
         JNE   *+8                                                              
         MVI   APPPCAT,APPPEST                                                  
         CLI   ESTAPUN,ALIKEXPS    Test expenses approver                       
         JNE   *+8                                                              
         MVI   APPPCAT,APPPEXPS                                                 
                                                                                
         TM    APRTSTAT,APRTORT1                                                
         JZ    SETA066                                                          
         MVC   APPPSCAT,R_OATYPE                                                
         J     SETA072                                                          
*                                                                               
SETA066  TM    APRTSTAT,APRTORT2                                                
         JZ    SETA070                                                          
         CLI   R_OAAMIX,YESQ       Have we a mix of client and none             
         JE    SETA104             Yes - skip this table entry                  
         CLI   R_OAALLC,YESQ       Have we all clients but different            
         JE    SETA068             Yes - set order type as client               
         MVI   APPPSCAT,ALIKNCLI   Try non-client or client type                
         CLC   R_OACLI,SPACES                                                   
         JNH   *+8                                                              
SETA068  MVI   APPPSCAT,ALIKCLI                                                 
         J     SETA072                                                          
*                                                                               
SETA070  MVI   APPPSCAT,ALIKDFT                                                 
*                                                                               
SETA072  CP    APLVLVAL,PZERO                                                   
         JNE   *+8                                                              
         MVI   APPPCAT,APPPORDF                                                 
         ZAP   APPPVAL,APLVLVAL    Approval value from table                    
                                                                                
         MVC   APPPACLA,SPACES                                                  
         MVC   APPPOFFC,SPACES                                                  
         MVC   APPPMED,SPACES                                                   
         MVC   APPPDEPT,SPACES                                                  
         MVC   APPPETYP,SPACES                                                  
         TM    CPYSTAT1,CPYSOROE                                                
         JZ    SETA074                                                          
         TM    APRTSTAT,APRTOFFC                                                
         JZ    SETA074                                                          
*                                                                               
         TM    OL_INDS,OL_PROLT    PROCESS DETAILS FROM OLIST ?                 
         JZ    SETA073             NO, CONTINUE                                 
         MVC   APPPOFFC,OLISTCDE   SAVE OLIST FOR PROCESS                       
         J     SETA074             CONTINUE                                     
*                                                                               
SETA073  CLC   D_OFF,SPACES                                                     
         JNH   SETA104                                                          
         MVC   APPPOFFC,D_OFF      OFFICE CODE OR FF'S                          
                                                                                
SETA074  TM    APRTSTA2,APRTMED    MEDIA                                        
         JZ    SETA076                                                          
         CLI   R_OAJBC,C' '                                                     
         JNH   SETA104                                                          
         MVC   APPPMED,R_OAJBC                                                  
                                                                                
SETA076  TM    APRTSTAT,APRTDEPT   DEPARTMENT                                   
         JZ    SETA078                                                          
         LHI   RF,1                                                             
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         LHI   RF,2                                                             
         LA    R1,R_OA2DA                                                       
         AR    RF,R1                                                            
         CLI   0(RF),C' '                                                       
         JNH   SETA104                                                          
         MVC   APPPDEPT,0(RF)                                                   
                                                                                
SETA078  TM    APRTSTAT,APRTETYP   ETYPE                                        
         JZ    SETA080                                                          
         CLI   R_OAETY,C' '                                                     
         JNH   SETA104                                                          
         MVC   APPPETYP,R_OAETY                                                 
                                                                                
SETA080  TM    APRTSTA2,APRTSUP    SUPPLIER CODE                                
         JZ    SETA082                                                          
         CLI   R_OASUPC,C' '                                                    
         JNH   SETA104                                                          
         MVC   APPPACLA,R_OASUPC+L'ACTKUNT                                      
         J     SETA098                                                          
                                                                                
SETA082  TM    APRTSTAT,APRTJOB    JOB                                          
         JZ    SETA084                                                          
         CLI   R_OAJBC,C' '                                                     
         JNH   SETA104                                                          
         MVC   APPPACLA,CPJFULA+L'ACTKUNT                                       
         J     SETA098                                                          
                                                                                
SETA084  TM    APRTSTAT,APRTPRO    PRODUCT                                      
         JZ    SETA086                                                          
         CLI   R_OAPRC,C' '                                                     
         JNH   SETA104                                                          
         LLC   RF,PPROLEN                                                       
         BASR  RE,0                                                             
         MVC   APPPACLA(0),CPJFULA+L'ACTKUNT                                    
         EX    RF,0(RE)                                                         
         J     SETA098                                                          
                                                                                
SETA086  TM    APRTSTAT,APRTCLI    CLIENT                                       
         JZ    SETA088                                                          
         CLI   R_OACLI,C' '                                                     
         JNH   SETA104                                                          
         LLC   RF,PCLILEN                                                       
         BASR  RE,0                                                             
         MVC   APPPACLA(0),CPJFULA+L'ACTKUNT                                    
         EX    RF,0(RE)                                                         
         J     SETA098                                                          
                                                                                
SETA088  TM    APRTSTA2,APRT2PA    2P ACCOUNT                                   
         JZ    SETA090                                                          
         CLI   R_OA2PAC,C' '                                                    
         JNH   SETA104                                                          
         MVI   APPPACLG,C'P'                                                    
         MVC   APPPACCD,R_OA2PAC                                                
         J     SETA098                                                          
*                                                                               
SETA090  TM    APRTSTA2,APRT2PE    1R PERSON LEVEL                              
         JZ    SETA092                                                          
         CLC   D_1RACC,SPACES                                                   
         JNH   SETA104                                                          
         MVI   APPPACLG,C'R'                                                    
         LA    RF,L'ACTKACT                                                     
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   APPPACCD(0),D_1RACC+L'ACTKUNT+L'ACTKLDG                          
         EX    RF,0(RE)                                                         
         J     SETA098                                                          
*                                                                               
SETA092  TM    APRTSTA2,APRT2SD    1R SUBDEP LEVEL                              
         JZ    SETA094                                                          
         CLC   D_1RACC,SPACES                                                   
         JNH   SETA104                                                          
         MVI   APPPACLG,C'R'                                                    
         LLC   RF,ONERL3L                                                       
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   APPPACCD(0),D_1RACC+L'ACTKUNT+L'ACTKLDG                          
         EX    RF,0(RE)                                                         
         J     SETA098                                                          
*                                                                               
SETA094  TM    APRTSTA2,APRT2DE    1R DEPARTMENT LEVEL                          
         JZ    SETA096                                                          
         CLC   D_1RACC,SPACES                                                   
         JNH   SETA104                                                          
         MVI   APPPACLG,C'R'                                                    
         LLC   RF,ONERL2L                                                       
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   APPPACCD(0),D_1RACC+L'ACTKUNT+L'ACTKLDG                          
         EX    RF,0(RE)                                                         
         J     SETA098                                                          
*                                                                               
SETA096  TM    APRTSTA2,APRT2OF    1R OFFICE LEVEL                              
         JZ    SETA098                                                          
         CLC   D_1RACC,SPACES                                                   
         JNH   SETA104                                                          
         MVI   APPPACLG,C'R'                                                    
         LLC   RF,ONERL1L                                                       
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   APPPACCD(0),D_1RACC+L'ACTKUNT+L'ACTKLDG                          
         EX    RF,0(RE)                                                         
         J     SETA098                                                          
*                                                                               
SETA098  MVC   CSVKEY1(L'APPPAS),APPPAS                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     SETA102                                                          
                                                                                
SETA100  NI    OA_INDS,X'FF'-OA_ISLF  Remove self approved                      
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         LA    R2,IOKEY                                                         
SETA102  CLC   CSVKEY1(APPPPIDB-APPPAS),APPPAS                                  
         JE    SETA108                                                          
SETA104  TM    OA_INDS,OA_FOND                                                  
         JZ    SETA106                                                          
         LH    RF,APLVLLVL                                                      
         AHI   RF,1                                                             
         STH   RF,APLVLLVL                                                      
SETA106  LA    R3,APRTABL(R3)                                                   
         J     SETA064                                                          
                                                                                
SETA108  CLI   R_OAAPLC,ALIKINV    Test invoices                                
         JE    SETA116             Skip self-approval                           
         CLI   R_OAAPLC,ALIKEXPS   Test expenses                                
         JE    SETA112             allow self approval always                   
         CLI   R_OAAPLC,ALIKINVN                                                
         JE    SETA116                                                          
*        CLI   R_OAEDOS,NOQ        Is editor not the submitter - ignore         
*        JE    SETA116             self approval rules                          
         LA    RE,RNSASEQ          Self approval by order type                  
         CLI   R_OATYPE,ALIKEXP                                                 
         JE    SETA110                                                          
         LA    RE,RNSASPQ                                                       
         CLI   R_OATYPE,ALIKPROD                                                
         JE    SETA110                                                          
         LA    RE,RNSASAQ                                                       
         CLI   R_OATYPE,ALIKART                                                 
         JE    SETA110                                                          
         LA    RE,RNSASIQ                                                       
         CLI   R_OATYPE,ALIKINT                                                 
         JE    SETA110                                                          
         DC    H'0'                invalid order type                           
SETA110  BASR  R1,0                                                             
         TM    X.RNSAIND,0                                                      
         EX    RE,0(R1)                                                         
         JNZ   SETA112             self approval OK                             
         CLI   R_OAOWN,NOQ         Can approver self approve                    
         JE    SETA112             Yes                                          
         CLC   APPPPIDB,CCTPID     No - check not approver not self             
         JE    SETA100             It is ignore entry                           
                                                                                
SETA112  LA    RE,CCTPID                                                        
                                                                                
SETA114  CLC   APPPPIDB,0(RE)      Is the pid the same                          
         JNE   SETA116             No                                           
         OI    OA_INDS,OA_ISLF     Set self approver                            
         CLI   R_OAETYS,YESQ       Self approval allowed for etype              
         JNE   SETA100             No                                           
         CP    APPPSEL,APPPVAL     Yes - check we haven't exceeded it           
         JNL   SETA118                                                          
         J     SETA100                                                          
                                                                                
SETA116  CP    APPPSEL,APPPVAL     Don't show self approver limits              
         JH    SETA100             to other users                               
                                                                                
SETA118  MVC   SAVEKEY,IOKEY                                                    
*                                                                               
SETA120  CURED (B2,APLVLLVL),(L'APERLVL,P.APERLVL),0,ZERO=YES,         +        
               ALIGN=LEFT                                                       
         ZAP   P.APERAPL,APPPVAL     Set approval limit                         
         ZAP   P.APERAPL2,APPPVAL                                               
         MVC   TEMP2(L'APPPPIDB),APPPPIDB                                       
         GOTOR (#GETPID,AGETPID)   Get 8 Character PID                          
         MVC   P.APERPID,TEMP2       Person ID                                  
         GOTOR (#GETPIN,AGETPIN)   Get person names                             
         L     R2,AIO1                                                          
         LA    RF,SAPEDATA-SAPEREC(R2)                                          
*                                                                               
         USING SAPERD,RF                                                        
         XR    R0,R0                                                            
SETA122  CLI   SAPEREL,0           Check if user terminated                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SAPEREL,SAPERELQ                                                 
         JE    SETA124                                                          
         IC    R0,SAPERLN          L'ELEMENT                                    
         AR    RF,R0                                                            
         J     SETA122                                                          
*                                                                               
SETA124  OC    SAPERDTE,SAPERDTE   Are they terminated?                         
         JZ    SETA126                                                          
         CLC   SAPERDTE,XL#TODC                                                 
         JNH   SETA100                                                          
         DROP  RF                                                               
*                                                                               
SETA126  MVC   P.APERPFN,TEMP2                                                  
         MVC   P.APERPLN,WORK2                                                  
         MVC   P.APERPMN,TEMP2+32                                               
         TM    OA_INDS,OA_IEML                                                  
         JZ    *+10                                                             
         MVC   P.APEREMA,APPEMAIL    AND EMAIL ADDRESS                          
         MVC   IOKEY,SAVEKEY       RESTORE READ SEQUENCE                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,IOKEY            Set/Reset R2 to IOKEY                        
                                                                                
         MVI   P.APERAPT,AGENCYQ                                                
         MVI   P.APERDEF,NOQ                                                    
         CLI   APPPCAT,APPPORDF    IS IT A PURCHASING APPROVER                  
         JNE   SETA134             No                                           
         MVI   P.APERAPT,PURCHSQ                                                
SETA134  TM    APPPSTAT,APPPDFLT   Is it a default approver                     
         JZ    SETA136                                                          
         MVI   P.APERDEF,YESQ      Set as default                               
*                                                                               
SETA136  TM    OA_INDS,OA_IHLV     Are we reading high levels for low           
         JNZ   SETA140             levels only                                  
                                                                                
SETA138  L     R0,AIO1             copy buffer to IO1                           
         LA    R1,L'TSAROABF                                                    
         LA    RE,TSAROABF                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         XC    P.APERAPL2(APESRTL),P.APERAPL2                                   
*                                                                               
         GOTOR GOTSAR,DMCB,('TSARDH',OABUF),0                                   
*                                                                               
         L     R1,AIO1                                                          
         TM    TSARERRS,TSEEOF                                                  
         JNZ   *+14                                                             
         CLC   TSAROABF(APEDUPL),0(R1)                                          
         JE    SETA100             Record already exists on file                
*                                                                               
         LA    R0,TSAROABF         Move back to TSAROABF                        
         LA    R1,L'TSAROABF                                                    
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTOR GOTSAR,DMCB,('TSAADD',OABUF),0                                   
         JE    SETA140                                                          
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
*                                                                               
SETA140  TM    OA_INDS,OA_IPUR     Are we dealing with purchasing               
         JNZ   SETA152             Yes - don't look at lower levels             
         L     R6,ADRFSLVL         Load first level                             
                                                                                
PREV     USING APLVLTD,R6                                                       
SETA142  CLM   R6,15,ADRCULVL      Ensure it's before current level             
         JNL   SETA152                                                          
         CLM   R6,15,ADRAPLVL      Don't add items for level above              
         JH    SETA152                                                          
         TM    OA_INDS,OA_ISLF     Is it self approved                          
         JNZ   SETA144             No                                           
         TM    OA_INDS,OA_IHSL     Are we only interested in self appr          
         JNZ   SETA152                                                          
         CLI   PREV.APLVLHLV,YESQ  Does this level accept higher level          
         JNE   SETA150                                    approvers             
SETA144  ZAP   P.APERAPL,PREV.APLVLVAL  Set approval limit                      
         ZAP   P.APERAPL2,PREV.APLVLVAL                                         
                                                                                
         TM    OA_INDS,OA_FOND                                                  
         JNZ   SETA146                                                          
         LH    RF,PREV.APLVLLVL                                                 
         AHI   RF,1                                                             
         STH   RF,PREV.APLVLLVL                                                 
SETA146  CURED (B2,PREV.APLVLLVL),(L'APERLVL,P.APERLVL),0,ZERO=YES,    +        
               ALIGN=LEFT                                                       
         XR    RF,RF                                                            
         ICM   RF,1,PREV.APLVLNUM  Decrement approvers required                 
         JZ    SETA148             Don't let it go less than zero               
         SHI   RF,1                                                             
         STC   RF,PREV.APLVLNUM                                                 
*                                                                               
SETA148  L     R0,AIO1             Move back to TSAROABF                        
         LHI   R1,L'TSAROABF                                                    
         LA    RE,TSAROABF                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         XC    P.APERAPL2(APESRTL),P.APERAPL2                                   
*                                                                               
         GOTOR GOTSAR,DMCB,('TSARDH',OABUF),0                                   
*                                                                               
         L     R1,AIO1                                                          
         TM    TSARERRS,TSEEOF                                                  
         JNZ   *+14                                                             
         CLC   TSAROABF(APEDUPL),0(R1)                                          
         JE    SETA100                                                          
*                                                                               
         LA    R0,TSAROABF         Move back to TSAROABF                        
         LHI   R1,L'TSAROABF                                                    
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTOR GOTSAR,DMCB,('TSAADD',OABUF),0                                   
         JE    SETA149                                                          
         TM    TSARERRS,TSEEOF     Buffer full - so it is ...                   
         JZ    *+2                 ... else duplicate key                       
*                                                                               
SETA149  LA    R0,TSAROABF         copy buffer to IO1                           
         LA    R1,L'TSAROABF                                                    
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
SETA150  LA    R6,APLVLTBL(R6)                                                  
         J     SETA142                                                          
                                                                                
SETA152  OI    OA_INDS,OA_FOND                                                  
         XR    RF,RF                                                            
         ICM   RF,1,APLVLNUM       Decrement approvers required                 
         JZ    SETA100             Don't let it go less than zero               
         SHI   RF,1                                                             
         STC   RF,APLVLNUM                                                      
         J     SETA100                                                          
                                                                                
SETA154  L     R6,ADRAPLVL         Load level greater than order                
         CLM   R4,15,ADRAPLVL      Don't add items for level above              
         JL    SETA156                                                          
         OI    OA_INDS,OA_IHLV     Set flag get higher levels                   
         CLI   PREV.APLVLHLV,YESQ  Are we getting higher levels                 
         JE    SETA156                                                          
         OI    OA_INDS,OA_IHSL     Set flag only for self approvers             
SETA156  LH    RF,APLVLLVL         Remove 1 from the current level              
         CHI   RF,1                Don't go less than 1!                        
         JNH   *+12                                                             
         SHI   RF,1                as will add 1 when we add higher             
         STH   RF,APLVLLVL         levels                                       
         LA    R4,APLVLTBL(R4)     Bump to next approval value                  
         CLI   0(R4),X'FF'         Reached the end?                             
         JE    SETA158                                                          
         OC    APLVLVAL,APLVLVAL   Any more approval limits                     
         JNZ   SETA062                                                          
SETA158  CLI   PREV.APLVLFAP,YESQ  Are we getting purchasing approver           
         JNE   SETA160                                                          
         TM    OA_INDS,OA_IPUR     Have we already processed                    
         JNZ   SETA160             Yes                                          
         ZAP   APLVLVAL,PZERO      Build entry for purchasing approver          
         MVI   APLVLNUM,X'01'                                                   
         XC    APLVLPRV,APLVLPRV                                                
         XC    APLVLHLV,APLVLHLV                                                
         OI    OA_INDS,OA_IPUR                                                  
         NI    OA_INDS,X'FF'-(OA_IHLV+OA_IHSL)                                  
         J     SETA062                                                          
*                                                                               
SETA160  TM    OL_INDS,OL_PRSNT    OLIST DETAILS PRESENT ?                      
         JZ    SETA161             NO, CONTINUE                                 
*                                  YES                                          
         TM    OL_INDS,OL_PROLT    1ST TIME HERE ?                              
         JZ    SETA160A            YES, CONTINUE                                
         AHI   R7,OLSTDLNQ         POINT TO THE NEXT OLIST ENTRY                
*                                                                               
SETA160A OI    OL_INDS,OL_PROLT    SET - PROCESS OLIST DETAILS                  
         CLC   OLISTCDE,SPACES     OLIST/OFFICE CODE PRESENT ?                  
         JNH   SETA161             YES, CONTINUE                                
*                                  NO, PROCESS FOR OLIST                        
         NI    OA_INDS,X'FF'-(OA_IHLV+OA_IHSL)                                  
         J     SETA061                                                          
*                                                                               
SETA161  L     R4,ADRFSLVL         CHECK WE HAVE ENOUGH APPROVERS               
         NI    OA_INDS,X'FF'-OA_IPUR  for each level                            
SETA162  TM    OA_INDS,OA_IPUR                                                  
         JNZ   SETA164                                                          
         CLM   R4,15,ADRAPLVL      Don't check levels above level for           
         JH    SETA166                                      this order          
*                                                                               
SETA164  XR    RF,RF                                                            
         ICM   RF,1,APLVLNUM       Decrement approvers required                 
         JZ    SETA166             Don't let it go less than zero               
         CP    APLVLVAL,PZERO                                                   
         JNE   SETAERR3            Value approver missing                       
         J     SETAERR4            Purchasing approver missing                  
*                                                                               
SETA166  LA    R4,APLVLTBL(R4)     Bump to next approval value                  
         CLI   0(R4),X'FF'         Finished?                                    
         JE    SETA168                                                          
         OC    APLVLVAL,APLVLVAL   Any more approval limits                     
         JZ    SETA168                                                          
         CP    APLVLVAL,PZERO      Is it purchasing level                       
         JNE   SETA162                                                          
         OI    OA_INDS,OA_IPUR     Set purchasing level                         
         J     SETA162                                                          
*                                       Sort our records before return          
SETA168  GOTOR GOTSAR,DMCB,('TSASRT',OABUF),                           +        
               =AL1(0,APERAPL2-APERAPL,APESRTL)                                 
*                                                                               
SETAEXIT J     EXIT                                                             
                                                                                
SETAERR2 LHI   R0,AE$OVGAP               Order value greater than               
         J     XERROR                    any approval limit                     
                                                                                
SETAERR3 LHI   R0,AE$NEAPF               Not enough approvers found             
         J     XERROR                                                           
                                                                                
SETAERR4 LHI   R0,AE$FARNF               Final approver not found               
         J     XERROR                                                           
                                                                                
SETAERR5 LHI   R0,AE$ALRIN               Aplimit previous lvls invalid          
         J     XERROR                                                           
                                                                                
SETAERR6 LHI   R0,AE$ALIXP               Expense aplimit record missing         
         J     XERROR                                                           
                                                                                
SETAERR7 LHI   R0,AE$ESTNF               Estimate not found                     
         J     XERROR                                                           
*                                                                               
SETAERR8 LHI   R0,AE$ALIES               Estimate aplimit rec missing           
         J     XERROR                                                           
*                                                                               
SETAERR9 LHI   R0,AE$ALIOU               Order un aplimit rec missing           
         J     XERROR                                                           
*                                                                               
SETAERRA LHI   R0,AE$ALIOA               Order ap aplimit rec missing           
         J     XERROR                                                           
*                                                                               
SETAERRB LHI   R0,AE$ORVES               Order amount over estimate             
         J     XERROR                                                           
*                                                                               
SETAERRC LHI   R0,AE$ARESI               Estimates internal missing             
         J     XERROR                                                           
         DROP  R2,R3,R7,P                                                       
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* Filter record against limit list (copied from ACBRA10)              *         
***********************************************************************         
         SPACE 1                                                                
P        USING APETABD,TSAROABF                                                 
NXTOAL   NTR1  BASE=*,LABEL=*                                                   
         ST    R8,LP_ADATA         Store A(data)                                
         CLI   LP_RMODE,LP_RFRST   First record to be returned?                 
         JNE   NXTOA02                                                          
         XC    TSAROABF,TSAROABF   Set action to 'Read High'                    
         GOTOR GOTSAR,DMCB,('TSARDH',OABUF),0                                   
         JE    NXTOA06                                                          
*&&US                                                                           
         TM    TSARERRS,TSEEOF     Finished?                                    
         JZ    NXTOA06                                                          
         MVC   LP_ERROR,=AL2(AE$NEAPF)                                          
         J     XERROR              US throw error not enough approvers          
*&&                                                                             
         J     NXTOA04                                                          
*                                                                               
NXTOA02  GOTOR GOTSAR,DMCB,('TSANXT',OABUF),0                                   
         JE    NXTOA06                                                          
*                                                                               
NXTOA04  TM    TSARERRS,TSEEOF     Finished?                                    
         JZ    NXTOA06                                                          
         MVI   LP_RMODE,LP_RLAST   Throw last time                              
         J     EXITY                                                            
*                                                                               
NXTOA06  LA    R0,OVALUES                                                       
         LHI   R1,OVALUESL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ZAP   OA_APAL,P.APERAPL   approver limit                               
         MVC   OA_APFN,P.APERPFN                                                
         MVC   OA_APPC,P.APERPID                                                
         MVC   OA_APLN,P.APERPLN                                                
         MVC   OA_APPM,P.APERPMN                                                
         MVC   OA_APPE,P.APEREMA                                                
         MVC   OA_LEVA,P.APERLVL                                                
         MVC   OA_APPT,P.APERAPT                                                
         MVC   OA_DEFT,P.APERDEF                                                
         J     EXITY                                                            
         EJECT                                                                  
         DROP  P                                                                
***********************************************************************         
* Set SJ office code                                                  *         
***********************************************************************         
                                                                                
SSJOFF   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*SSJOFF*'                                                      
         CLC   PRODUL,TEMP2                                                     
         JNE   SSJOFFX                                                          
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
                                                                                
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   ACTKACT(0),TEMP2+2                                               
         EX    RE,0(RF)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SSJOFFX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SSJOFFX                                                          
                                                                                
         L     R2,AIO1                                                          
         LA    R3,ACTRFST-ACTRECD(R2)                                           
         USING PPRELD,R3                                                        
         XR    R0,R0                                                            
SSJOFF02 CLI   PPREL,0                                                          
         JE    SSJOFF06                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    SSJOFF04                                                         
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     SSJOFF02                                                         
                                                                                
SSJOFF04 MVC   D_OFFC,PPRGAOFF                                                  
         OC    D_OFFC,SPACES                                                    
                                                                                
SSJOFF06 LA    R2,IOKEY                                                         
         LLC   RE,PPROLEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   ACTKACT(0),TEMP2+2                                               
         EX    RE,0(RF)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SSJOFFX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SSJOFFX                                                          
                                                                                
         L     R2,AIO1                                                          
         LA    R3,ACTRFST-ACTRECD(R2)                                           
         XR    R0,R0                                                            
SSJOFF08 CLI   PPREL,0                                                          
         JE    SSJOFF12                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    SSJOFF10                                                         
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     SSJOFF08                                                         
                                                                                
SSJOFF10 CLC   PPRGAOFF,SPACES                                                  
         JNH   SSJOFF12                                                         
         MVC   D_OFFC,PPRGAOFF                                                  
         OC    D_OFFC,SPACES                                                    
                                                                                
SSJOFF12 LA    R2,IOKEY                                                         
         MVC   ACTKACT,TEMP2+2                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SSJOFFX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SSJOFFX                                                          
                                                                                
         L     R2,AIO1                                                          
         LA    R3,ACTRFST-ACTRECD(R2)                                           
         XR    R0,R0                                                            
SSJOFF14 CLI   PPREL,0                                                          
         JE    SSJOFFX                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    SSJOFF16                                                         
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     SSJOFF14                                                         
                                                                                
SSJOFF16 CLC   PPRGAOFF,SPACES                                                  
         JNH   SSJOFFX                                                          
         MVC   D_OFFC,PPRGAOFF                                                  
         OC    D_OFFC,SPACES                                                    
                                                                                
SSJOFFX  J     EXIT                                                             
         DROP  R2,R3                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* GET PERSON DETAILS (USE TS#PBLK)                                    *         
***********************************************************************         
                                                                                
PERDTL   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    CL8'*PERDTL*'                                                    
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY            READ FOR PERSON RECORD                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,TEMP2                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    PERDTL10                                                         
         MVC   FULL2(2),=AL2(AE$IVPER)                                          
         J     EXITN                                                            
                                                                                
PERDTL10 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         L     R2,AIO1                                                          
         LA    R3,PERRFST          LOCATE ELEMENTS                              
         USING EMPELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
PERDTL20 CLI   EMPEL,0                                                          
         JE    PERDTL80                                                         
         CLI   EMPEL,LOCELQ                                                     
         JE    PERDTL60                                                         
         CLI   EMPEL,PIDELQ                                                     
         JE    PERDTL70                                                         
                                                                                
PERDTL30 IC    R0,EMPLN                                                         
         AR    R3,R0                                                            
         J     PERDTL20                                                         
                                                                                
         USING LOCELD,R3                                                        
         USING OFFALD,R1                                                        
PERDTL60 CLI   CUACCS,0            LIMIT ACCESS?                                
         JE    PERDTL62            NO                                           
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,LOCOFF     VALIDATE CURRENT OFFICE                      
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JNE   PERDTL30                                                         
         DROP  R1                                                               
                                                                                
PERDTL62 ST    R3,ACLOCL          STORE MOST RECENT LOCATION                    
         CLC   XL#TODP,LOCSTART   FIND LOCATION FOR CURRENT DATE                
         JL    PERDTL30                                                         
         OC    LOCEND,LOCEND                                                    
         JZ    PERDTL65                                                         
         CLC   XL#TODP,LOCEND                                                   
         JH    PERDTL30                                                         
PERDTL65 ST    R3,ALOCEL                                                        
         J     PERDTL30                                                         
                                                                                
         USING PIDELD,R3                                                        
PERDTL70 MVC   X#PPID,PIDNO       EXTRACT PID                                   
         J     PERDTL30                                                         
                                                                                
PERDTL80 OC    ALOCEL,ALOCEL      ENSURE THIS IS SET                            
         JNZ   PERDTL85                                                         
         OC    ACLOCL,ACLOCL      IF CAN'T FIND LOCATION FOR CURRENT            
         JZ    PERDTL82           DATE USE MOST RECENT LOCATION                 
         MVC   ALOCEL,ACLOCL                                                    
         J     PERDTL85                                                         
                                                                                
PERDTL82 MVC   FULL2(2),=AL2(AE$IVLDT)                                          
         J     EXITN              INVALID LOCATION FOR DATE                     
                                                                                
PERDTL85 OC    X#PPID,X#PPID                                                    
         JNZ   PERDTL90                                                         
         MVC   FULL2(2),=AL2(AE$NCPID)                                          
         J     EXITN              NO VALID PID CONNECTED                        
                                                                                
PERDTL90 J     EXITY                                                            
                                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Estimate check on approver lookup                                   *         
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
CHKES002 CLC   R_OATYPE,0(R2)      Was estimate check done when we              
         JE    CHKES004            got approvers?                               
         LA    R2,1(R2)                                                         
         JCT   R1,CHKES002                                                      
         J     CHKESTY             No estimate checking done                    
                                                                                
CHKES004 CLI   R_OASKEC,YESQ       Have we already done this check?             
         JE    CHKESTY             No estimate checking done                    
         MVI   GOEWE,GOEERR                                                     
         MVI   GOECE,GOECETA                                                    
         J     CHKES010                                                         
*                                                                               
CHKES006 CLI   R_OATYPE,INTOQ      only for internal orders                     
         JE    CHKES010                                                         
         CLI   R_OATYPE,PROOQ      only for production orders                   
         JE    CHKES010                                                         
         CLI   R_OATYPE,ARTOQ      and artist orders                            
         JNE   CHKESTY             No estimate checking done                    
                                                                                
CHKES010 CLI   GOEWE,GOEERR        Error (no Warning)                           
         JE    CHKES012                                                         
                                                                                
CHKES012 GOTOR GETTUP                                                           
         ZAP   X#TOTOE,PZERO                                                    
         ZAP   X#TOTCE,PZERO                                                    
         ZAP   X#TOTHR,PZERO                                                    
         ZAP   X#TOTOR,PZERO                                                    
                                                                                
         USING ESTCHKD,R6                                                       
         LAY   R6,SVESTWK          set w/c values to estimate table             
                                                                                
         USING WRKD,R2                                                          
         XR    R2,R2                                                            
         ICM   R2,7,R_OAWRKA                                                    
         JZ    CHKES018            NO MEDIA DATA                                
         MVI   MYCOUNT,0           byte1 = no. of w/codes                       
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R2)  Number of workcodes                       
         LA    R2,LW_LN2Q(R2)      R2=A(first media block)                      
                                                                                
CHKES014 STC   R0,MYCOUNT                                                       
         GOTOR CHKWCEC,DMCB,WRKCODE                                             
         JL    CHKES016                                                         
         JH    CHKESTWN                                                         
         MVC   ESTCHWC,WRKCODE                                                  
         MVI   ESTCKID,ESTCKIQ                                                  
         MVI   ESTCLEN,ESTCHLQ                                                  
         ZAP   ESTCHOE,PZERO                                                    
         ZAP   ESTCHCE,PZERO                                                    
         ZAP   ESTCHHR,PZERO                                                    
         ZAP   ESTCHAM,WRKAMT                                                   
         ZAP   ESTCHOR,PZERO                                                    
         ZAP   ESTRAMT,PZERO                                                    
         AHI   R6,ESTCHLQ                                                       
         LLH   RF,NUMWCR                                                        
         AHI   RF,1                                                             
         STH   RF,NUMWCR                                                        
                                                                                
CHKES016 XC    ESTCHWC,ESTCHWC     end of table                                 
         AHI   R2,WRKDL                                                         
         LLC   R0,MYCOUNT                                                       
         JCT   R0,CHKES014                                                      
         DROP  R2,R6                                                            
                                                                                
CHKES018 CLI   GOEORN,GONONE       Check all non memo orders?                   
         JE    CHKES038                                                         
                                                                                
         USING OSJPASD,R2                                                       
         LA    R2,IOKEY            read for other orders to this job            
         XC    OSJPAS,OSJPAS       (doesn't take est # on order into            
         MVI   OSJPTYP,OSJPTYPQ    account)                                     
         MVI   OSJPSUB,OSJPSUBQ    and add amounts                              
         MVC   OSJPCPY,CUXCPY                                                   
         MVC   OSJPACT,CPJFACC                                                  
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
         CLC   OSJPORD,R_OAORDN                                                 
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
         CLC   R_OAESTN,SPACES                                                  
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
         CLC   R_OAESTN,SPACES                                                  
         JNH   CHKES024                                                         
*                                                                               
         LA    RF,FFTESTN                                                       
         CLI   FFTLN,FFTESLNQ      Any original estimate number?                
         JL    CHKES24A            No - OK                                      
         CLC   FFTOESTN,SPACES                                                  
         JNH   CHKES24A                                                         
         LA    RF,FFTOESTN                                                      
*                                                                               
CHKES24A CLC   R_OAESTN,0(RF)                                                   
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
                                                                                
CHKES027 CLC   ORDKORD,R_OAORDN                                                 
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
         CLC   ORDKORD,R_OAORDN                                                 
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
                                                                                
CHKES037 AP    X#TOTOR,DUB                                                      
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
         MVC   OSJPACT,CPJFACC                                                  
         MVI   OSJPMEM,X'FF'                                                    
         MVC   CSVKEY2,OSJPAS                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    CHKES022                                                         
         DC    H'0'                                                             
                                                                                
         USING TRNRECD,R2                                                       
CHKES040 LA    R2,IOKEY            read for charges transactions                
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,CPJFULA                                                  
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
         CLC   R_OAESTN,SPACES                                                  
         JNH   CHKES052                                                         
*                                                                               
         LA    RF,FFTESTN                                                       
         CLI   FFTLN,FFTESLNQ      Any original estimate number?                
         JL    CHKES61A            No - OK                                      
         CLC   FFTOESTN,SPACES                                                  
         JNH   CHKES61A                                                         
         LA    RF,FFTOESTN                                                      
*                                                                               
CHKES61A CLC   R_OAESTN,0(RF)                                                   
         JE    CHKES052            MATCH, CARRY ON                              
         J     CHKES044            SKIP THIS T/X                                
*                                                                               
         USING ESTCHKD,RF                                                       
CHKES062 LAY   RF,SVESTWK                                                       
         AP    X#TOTOR,DUB1                                                     
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
         LA    R1,CPJFACC                                                       
         AR    R1,RE                                                            
         MVC   TP.TSJPMED,0(R1)                                                 
         MVC   TP.TSJPACT,CPJFACC                                               
         MVC   TP.TSJPCOFF,D_OFFC                                               
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
                                                                                
CHKES078 CLC   TIMACC,CPJFULA                                                   
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
                                                                                
CHKES086 AP    X#TOTOR,DUB1                                                     
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
                                                                                
CHKES090 CLC   TIMIULA,CPJFULA                                                  
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
         MVC   EXJPCOFF,D_OFFC                                                  
         MVC   EXJPCPJ,CPJFACC                                                  
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
                                                                                
         CLC   CUR.CIDCCPJ,CPJFACC                                              
         JNE   CHKES102            look for this job's items                    
         AP    X#TOTOR,CIDMAMT                                                  
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
                                                                                
CHKES114 GOTO1 VACCEMU,DMCB,=C'NEWO',,AIO4,AIO4                                 
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
                                                                                
         CLI   GOECE,GOECSWC       Single estimate checking?                    
         JE    CHKES115                                                         
         CLI   GOECE,GOECSTA                                                    
         JNE   CHKES116                                                         
         CLC   R_OAESTN,SPACES     Make sure estimate number is passed          
         JH    CHKES115                                                         
         MVC   LP_ERROR,=AL2(AE$ENONS)                                          
         J     CHKESTWN                                                         
                                                                                
         USING EGNPASD,R1                                                       
CHKES115 LA    R1,IOKEY                                                         
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,CUXCPY                                                   
         MVC   EGNPNUM,R_OAESTN                                                 
         MVC   CSVKEY2,IOKEY                                                    
         CLC   R_OAESTN,SPACES     Do we have an estimate number                
         JH    *+14                Yes                                          
         MVC   LP_ERROR,=AL2(AE$ENONS) No - give error                          
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
         JL    CHKES126            Include in estimate checking?                
         JH    CHKESTWN                                                         
*                                                                               
CHKES120 AP    X#TOTOE,JBCOLVAL                                                 
         AP    X#TOTCE,JBCOLVAL+L'JBCOLVAL(L'JBCOLVAL)                          
         AP    X#TOTHR,JBCOLVAL+(2*L'JBCOLVAL)(L'JBCOLVAL)                      
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
                                                                                
CHKES132 AP    X#TOTOE,MJETVAL                                                  
         AP    X#TOTCE,MJETVAL+L'MJETVAL(L'MJETVAL)                             
         AP    X#TOTHR,MJETVAL+(2*L'MJETVAL)(L'MJETVAL)                         
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
         ZAP   MJETVAL,X#TOTOE                                                  
         ZAP   MJETVAL+L'MJETVAL(L'MJETVAL),X#TOTCE                             
         ZAP   MJETVAL+(2*L'MJETVAL)(L'MJETVAL),X#TOTHR                         
         DROP  R4                                                               
                                                                                
CHKES140 CLI   GOECE,GOECEESA      Do we want an approved estimate              
         JE    CHKES142            Yes                                          
         CLI   GOECE,GOECEWCA                                                   
         JE    CHKES142            Yes                                          
         CLI   GOECE,GOECETAA                                                   
         JNE   CHKES144            No                                           
                                                                                
CHKES142 CLI   JBHIAPP,0           does approved estimate exist?                
         JNE   CHKES144                                                         
*        CLI   GOECE,GOECEWCA      Workcode level?                              
*        JNE   CHKES143            Yes                                          
         LHI   R0,OA_NAEQ                                                       
         J     CHKESTNX                                                         
*                                                                               
*HKES143 CLI   GOEWE,GOEWARN       Warning or error?                            
*        JE    CHKESTN                                                          
*        LHI   R0,OA_NAEQ          No current estimate                          
*        CLI   GOECE,GOECETAA                                                   
*        JE    CHKESTNX                                                         
*        J     CHKESTN                                                          
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
         CP    X#TOTCE,PZERO       Any estimate figure                          
         JE    CHKESTN             No - error                                   
         J     CHKESTY                                                          
                                                                                
CHKES150 LA    R2,GOUESOA                                                       
         LA    R0,4                                                             
CHKES152 CLC   R_OATYPE,0(R2)      Are we doing new estimate check              
         JE    CHKES154            Yes                                          
         LA    R2,1(R2)                                                         
         JCT   R0,CHKES152                                                      
         J     CHKES168                                                         
                                                                                
CHKES154 CP    X#TOTCE,PZERO       Any estimate figure                          
         JH    CHKES156                                                         
         LHI   R0,OA_NCEQ          No current estimate                          
         J     CHKESTNX            No - error                                   
*                                                                               
CHKES156 L     RF,AGOBLOCB                                                      
         USING GOBLOCKD,RF                                                      
         ZAP   X#PL16,X#TOTCE                                                   
         MP    X#PL16,GOOVRPER                                                  
         SRP   X#PL16,64-4,5       Calculate current est*maxper                 
         ZAP   X#TOTCE,X#PL16      Current estimate incl contingency            
         SP    X#TOTCE,X#TOTOR     Minus charges                                
         ZAP   OA_EJLE,X#TOTCE     Remaining amount left to spend               
         SP    X#TOTCE,R_OATOTA    Minus order value                            
         CP    X#TOTCE,PZERO       Do we have any money left                    
         JNL   CHKESTAL            Yes                                          
         CLC   JBHIREV,JBCURVER    No - so do we have a higher revision         
         JNH   CHKESTN             No - error order over estimate               
         ZAP   X#PL16,X#TOTHR                                                   
         MP    X#PL16,GOOVRPER                                                  
         SRP   X#PL16,64-4,5       Calculate highest rev est*maxper             
         ZAP   X#TOTHR,X#PL16      Highest rev estimate incl contingcy          
         SP    X#TOTHR,X#TOTOR     Minus charges                                
         ZAP   OA_EJLE,X#TOTHR     Remaining amount left to spend               
         SP    X#TOTHR,R_OATOTA    Minus order value                            
         MVI   ESTAPUN,ALIKOEUN    use unapproved estimate aplimit              
         CP    X#TOTHR,PZERO       Do we have any money left                    
         JNL   CHKESTY                                                          
         LHI   R0,OA_OHSQ          Order exceeds highest sub est                
         CLI   R_OATYPE,PROOQ                                                   
         JE    CHKESTNX                                                         
         J     CHKESTN                                                          
                                                                                
CHKES168 ZAP   DUB,X#TOTCE                                                      
         SP    DUB,X#TOTOR                                                      
         ZAP   OA_EJLE,DUB         Remaining amount left                        
         CP    R_OATOTA,DUB        check totals                                 
         JH    CHKESTN                                                          
         J     CHKESTY                                                          
         DROP  R1                                                               
                                                                                
CHKES170 LAY   R5,SVESTWK                                                       
*                                                                               
         USING ESTCHKD,R5                                                       
CHKES171 OC    ESTCHWC,ESTCHWC     check w/c                                    
         JZ    CHKESTY                                                          
         LA    R2,ESTCHKE                                                       
         CLI   GOEWE,GOEWARN       Warn or error                                
         JNE   *+8                                                              
         LA    R2,ESTCHKW                                                       
*                                                                               
         CP    ESTCHCE,PZERO       any estimates                                
         JNE   CHKES172                                                         
         GOTOR SETWCE,(R2)                                                      
CHKES172 ZAP   DUB,ESTCHCE                                                      
         SP    DUB,ESTCHOR                                                      
         CP    ESTCHAM,DUB         Any amount left?                             
         JNH   CHKES174                                                         
         ZAP   ESTRAMT,DUB         Remaining amount                             
         GOTOR SETWCE,(R2)                                                      
*                                                                               
CHKES174 AHI   R5,ESTCHLQ                                                       
         J     CHKES171                                                         
*                                                                               
CHKESTY  J     EXITY                                                            
*                                                                               
CHKESTAL MVI   ESTAPUN,ALIKOEUN    Set aplimit category                         
         CLI   JBHIAPP,0           Does approved estimate exist?                
         JE    EXITY                No                                          
         MVI   ESTAPUN,ALIKOEAP    Yes - use approved estimate aplimit          
         J     EXITY                                                            
                                                                                
         USING EAETABD,RF                                                       
CHKESTN  LA    RF,EAETAB           Lookup correct error code                    
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
         MVC   OA_ECWM,EAETERR                                                  
         J     EXITY                                                            
*                                                                               
CHKESTN6 MVC   OA_ECKE,EAETERR                                                  
         J     EXITL                                                            
*                                                                               
CHKESTNX CLI   GOEWE,GOEERR        <warning no error but response>              
         JNE   CHKESTX2                                                         
         STC   R0,OA_ECKE          Error now                                    
         J     EXITL                                                            
*                                                                               
CHKESTX2 STC   R0,OA_ECWM          Return warning message                       
         J     EXITY                                                            
*                                                                               
CHKESTWN J     EXITH                                                            
         DROP  R3,R5,R6,RF                                                      
         EJECT                                                                  
         USING LP_D,R5                                                          
***********************************************************************         
* CHECK IF WC INCLUDED FOR ESTIMATES                                  *         
***********************************************************************         
         SPACE 1                                                                
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
         MVC   LP_ERROR,=AL2(AE$WRKNF)                                          
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'WCOKWRK),0(R2)                                         
         LA    RF,XERRTXT                                                       
         STCM  RF,7,LP_EATXT                                                    
         LHI   RF,L'WCOKWRK                                                     
         STC   RF,LP_ELTXT                                                      
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
* Get TUP profile and 'translate' filter out based on GOETMB and      *         
* GOETMR (must be called after getopt call)                           *         
***********************************************************************         
         SPACE 1                                                                
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
* FILTER TIME RECORDS ACCORDING TO  GOETMB AND GOETMR                 *         
* ROUTINE #2-FILTER TRNRECD                                           *         
* ON NTRY P1=A(TRNRECD)                                               *         
***********************************************************************         
         SPACE 1                                                                
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
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* Filter time records according to lowest Getopt setting              *         
* ON NTRY P1=TSJPASD                                                  *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
FLTTMP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*FLTTMP*'                                                      
                                                                                
         LM    R3,R4,0(R1)         R4=A(TIMELD),R5=A(TSJPAS)                    
                                                                                
         USING TSJPASD,R4                                                       
         USING TIMELD,R3                                                        
                                                                                
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
* Filter expenses according to GOEXPN and GOEXPB                      *         
* On ntry P1=EXJPASD                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING EXJPASD,R2                                                       
         USING GOXBLKD,R3                                                       
FLTEXK   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*FLTEXK*'                                                      
         L     R2,0(R1)                                                         
         L     R3,AGOXBLCK                                                      
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
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Filter expenses according to GOEXPN AND GOEXPB                      *         
* Filter CIDELDS                                                     *          
* On ntry P1=A(CIDELD)                                                *         
*         P2=A(EXCRECD)                                               *         
***********************************************************************         
         SPACE 1                                                                
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
* Call getopt to get option maintain settings                         *         
***********************************************************************         
         SPACE 1                                                                
GETOPT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETOPT*'                                                      
                                                                                
         USING GOBLOCKD,RF                                                      
         L     RF,AGOBLOCB        CALL GETOPT FOR DEFAULT EXPIRATION            
                                                                                
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
                                                                                
         MVC   GOADM,VDATAMGR     PERIOD                                        
         MVC   GOAEXT,AGOXBLCK                                                  
         MVC   GOABEXT,AGOBBLCK                                                 
         MVC   GOSELOFC,D_OFF     FILL OUT OFFICE CODE                          
                                                                                
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELJOB,SPACES                                                  
                                                                                
         MVC   GOCTRY,CUCTRY                                                    
         MVC   GOSELCUL(1),CUXCPY                                               
         MVC   GOSELCUL+1(2),PRODUL                                             
         OI    GOSPEC3,GOSPNERQ                                                 
                                                                                
         GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
                                                                                
         CLC   R_OACLI,SPACES     Any client passed?                            
         JNH   EXITY                                                            
                                                                                
         L     RF,AGOBLOCB                                                      
                                                                                
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
                                                                                
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELJOB,SPACES                                                  
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK                                                  
         MVC   GOABEXT,AGOBBLCK                                                 
         MVC   GOCTRY,CUCTRY                                                    
         MVC   GOSELCUL(1),CUXCPY                                               
         MVC   GOSELCUL+1(2),PRODUL                                             
         MVC   GOSELCLI(L'R_OACLI),R_OACLI                                      
         OC    GOSELCLI,SPACES                                                  
         CLC   R_OAPRC,SPACES                                                   
         JNH   *+10                                                             
         MVC   GOSELPRO(L'R_OAPRC),R_OAPRC                                      
         OC    GOSELPRO,SPACES                                                  
         CLC   R_OAJBC,SPACES                                                   
         JNH   *+10                                                             
         MVC   GOSELJOB(L'R_OAJBC),R_OAJBC                                      
         OC    GOSELJOB,SPACES                                                  
         OI    GOSPEC3,GOSPNERQ                                                 
                                                                                
         GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
         J     EXITY                                                            
         DROP  RF                                                               
*                                                                               
***********************************************************************         
* CALL GETOLST TO GET THE LIST OF OLIST FOR GIVEN OFFICE CODE         *         
***********************************************************************         
*                                                                               
GETOLST  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GTOLST*'                                                      
*                                                                               
         USING OFLPASD,R2          MAP WITH OLIST PASSIVE RECORD                
         USING OLISTD,R3           MAP OLIST SAVE AREA DSECT                    
*                                                                               
         XC    SAVOLIST,SAVOLIST   CLEAR WORK AREA                              
         LA    R3,SAVOLIST         ADDRESS WORK AREA                            
*                                                                               
         LHI   R4,MAXOLSTQ-1       MAXIMUM OLIST ENTRIES ALLOWED 20             
         LA    R2,IOKEY            ADDRESS KEY WORK AREA                        
         XC    OFLPAS,OFLPAS       INIT KEY TO X'00'                            
         MVI   OFLPTYP,OFLPTYPQ    READ OFFICE LIST PASSIVE                     
         MVI   OFLPSUB,OFLPSUBQ    OFFICE LIST PASSIVE SUB TYPE                 
         MVC   OFLPCPY,CUXCPY      SAVE COMPANY CODE                            
         MVC   OFLPOFF,D_OFF       GET OLIST FOR OFFICE CODE                    
         MVC   CSVKEY1(L'OFLPAS),OFLPAS   SAVE KEY DETAILS                      
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    GETOLST4                                                         
         J     GETOLSTX                                                         
*                                                                               
GETOLST2 DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   GETOLSTX                                                         
*                                                                               
GETOLST4 CLC   OFLPAS(OFLPOFL-OFLPASD),CSVKEY1                                  
         JNE   GETOLSTX                                                         
*                                                                               
         MVC   OLISTCDE,OFLPOFL    SAVE OFFICE LIST CODE                        
         OI    OL_INDS,OL_PRSNT    SET OLIST IS PRESENT FOR OFFICE              
         AHI   R3,OLSTDLNQ         POINT TO THE NEXT ENTRY                      
         JCT   R4,GETOLST2         PROCESS NEXT RECORD                          
         DC    H'00'               MORE OLIST ENTRIES,ABEND                     
*                                                                               
GETOLSTX J     EXITY                                                            
         DROP  R2,R3                                                            
*                                                                               
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
**********************************************************************          
* General exit and declarations                                      *          
**********************************************************************          
                                                                                
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
         MVI   LP_EMSYS,6                                                       
         J     EXITY                                                            
                                                                                
XERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         MVI   LP_EMSYS,6                                                       
         STCM  R0,3,LP_ERROR                                                    
         J     EXITN                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     CLEAR OUTPUT FIELD LENGTH & EXIT             
         J     EXITY                                                            
                                                                                
EXITH    LHI   RE,2                                                             
         J     EXITCC                                                           
EXITL    DS    0H                                                               
EXITN    LHI   RE,0                SET CONDITION CODE TO NOT EQUAL              
         J     EXITCC                                                           
EXITY    LHI   RE,1                SET CONDITION CODE TO EQUAL                  
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                   GENERAL EXIT POINT                           
         EJECT                                                                  
***********************************************************************         
* List of account files to open in all systems                        *         
***********************************************************************         
                                                                                
FILES    DS    0X                                                               
         DC    C'ACCOUNT'          SYSTEM NAME FOR OPEN                         
                                                                                
         DC    C'NCTFILE '         FILE LIST                                    
         DC    C'NGENFIL '                                                      
         DC    C'NGENDIR '                                                      
         DC    C'NACCDIR '                                                      
         DC    C'NACCMST '                                                      
         DC    C'NACCARC '                                                      
         DC    C'X'                                                             
                                                                                
***********************************************************************         
* Global literals                                                     *         
***********************************************************************         
                                                                                
GLOBALS  DS    0D                                                               
OLBUF    EQU   1                                                                
OABUF    EQU   2                                                                
ANORDNO  DC    CL6'000141'                                                      
MAXLVL   EQU   50                  max entries in APLVTAB                       
                                                                                
         LTORG                                                                  
CLIENTQ  EQU   C'C'                CLIENT LEVEL APPROVER                        
DEPTQ    EQU   C'D'                DEPARTMENT LEVEL APPROVER                    
EXPTYPQ  EQU   C'E'                EXPENDITURE TYPE APPROVER                    
AGENCYQ  EQU   C'P'                AGENCY LEVEL APPROVER                        
PURCHSQ  EQU   C'F'                PURCHASING APPROVER                          
FINANCQ  EQU   C'I'                FINANCE APPROVER                             
SUPPLRQ  EQU   C'S'                SUPPLIER APPROVER                            
R#OAWCR  EQU   1                   WORKCODE ERRORS                              
R#OAHDR  EQU   2                   WORKCODE HEADER                              
R#OAOAL  EQU   3                   WORKCODE APPROVER LIST                       
MAXEML   EQU   10                  maximum of 10 email addresses                
                                                                                
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
MAPTAB   DS    0XL4                ** MAP/INDICATORS TABLE **                   
                                                                                
OLSTDL#  DC    AL2(A#AOLD)         AURA ORDERS DOWNLOAD                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
OALKEL#  DC    AL2(A#ALKE)         AURA APPROVER LOOKUP AND ESTCHK              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
DCDICTL  DS    0X                                                               
         DCDDL AC#ALL,L'AC@ALL                                                  
         DCDDL AC#YES,L'AC@YES                                                  
DCDICTLX DC    X'FF'                                                            
                                                                                
OSTATAB  DS    0H                                                               
         DC    AL1(INPROGQ),AL1(ORDSDRFT)                                       
         DC    AL1(SUBMITQ),AL1(ORDSSUBM)                                       
         DC    AL1(PARTAPQ),AL1(ORDSPAPP)                                       
         DC    AL1(FULAPPQ),AL1(ORDSAPPR)                                       
         DC    AL1(REJECTQ),AL1(ORDSOREJ)                                       
         DC    X'FF'                                                            
                                                                                
EXPTYTAB DS    0X                                                               
         DC    AL1(EXJPCLI1)                                                    
         DC    AL1(EXJPCBL1)                                                    
         DC    AL1(EXJPCNB1)                                                    
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
                                                                                
* new version - use equates                                                     
APRTAB   DS    0XL1                                                             
*  section 1 - with media and/or 2P, but no cli/pro/job                         
*                                                                               
* Exp/Prod order type, off, dep, etype, media, 2P a/c                           
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(APRTMED+APRT2PA)                                             
* Client order type, off, dep, etype, med, 2P a/c                               
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(APRTMED+APRT2PA)                                             
* No order type, off, dep, etype, med, 2P a/c                                   
         DC    AL1(APRTOFFC+APRTDEPT+APRTETYP)                                  
         DC    AL1(APRTMED+APRT2PA)                                             
* Exp/Prod order type, off, dep, med, 2P a/c                                    
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT)                                  
         DC    AL1(APRTMED+APRT2PA)                                             
* Client order type, off, dep, med, 2P a/c                                      
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT)                                  
         DC    AL1(APRTMED+APRT2PA)                                             
* No order type, off, dep, med, 2P a/c                                          
         DC    AL1(APRTOFFC+APRTDEPT)                                           
         DC    AL1(APRTMED+APRT2PA)                                             
* Exp/Prod order type, off, etype, med, 2P a/c                                  
         DC    AL1(APRTORT1+APRTOFFC+APRTETYP)                                  
         DC    AL1(APRTMED+APRT2PA)                                             
* Client order type, off, etype, med, 2P a/c                                    
         DC    AL1(APRTORT2+APRTOFFC+APRTETYP)                                  
         DC    AL1(APRTMED+APRT2PA)                                             
* No order type, off, etype, med, 2P a/c                                        
         DC    AL1(APRTOFFC+APRTETYP)                                           
         DC    AL1(APRTMED+APRT2PA)                                             
* Exp/Prod order type, etype, med, 2P a/c                                       
         DC    AL1(APRTORT1+APRTETYP)                                           
         DC    AL1(APRTMED+APRT2PA)                                             
* Client order type, etype, med, 2P a/c                                         
         DC    AL1(APRTORT2+APRTETYP)                                           
         DC    AL1(APRTMED+APRT2PA)                                             
* No order type, etype, med, 2P a/c                                             
         DC    AL1(APRTETYP)                                                    
         DC    AL1(APRTMED+APRT2PA)                                             
* Exp/Prod order type, off, med, 2P a/c                                         
         DC    AL1(APRTORT1+APRTOFFC)                                           
         DC    AL1(APRTMED+APRT2PA)                                             
* Client order type, off, med, 2P a/c                                           
         DC    AL1(APRTORT2+APRTOFFC)                                           
         DC    AL1(APRTMED+APRT2PA)                                             
* No order type, off, med, 2P a/c                                               
         DC    AL1(APRTOFFC)                                                    
         DC    AL1(APRTMED+APRT2PA)                                             
* Exp/Prod order type, med, 2P a/c                                              
         DC    AL1(APRTORT1)                                                    
         DC    AL1(APRTMED+APRT2PA)                                             
* Client order type, med, 2P a/c                                                
         DC    AL1(APRTORT2)                                                    
         DC    AL1(APRTMED+APRT2PA)                                             
* No order type, med, 2P a/c                                                    
         DC    AL1(0)                                                           
         DC    AL1(APRTMED+APRT2PA)                                             
* Exp/Prod order type, off, dep, etype, 2P a/c                                  
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(APRT2PA)                                                     
* Client order type, off, dep, etype, 2P a/c                                    
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(APRT2PA)                                                     
* No order type, off, dep, etype, 2P a/c                                        
         DC    AL1(APRTOFFC+APRTDEPT+APRTETYP)                                  
         DC    AL1(APRT2PA)                                                     
* Exp/Prod order type, off, dep, 2P a/c                                         
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT)                                  
         DC    AL1(APRT2PA)                                                     
* Client order type, off, dep, 2P a/c                                           
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT)                                  
         DC    AL1(APRT2PA)                                                     
* No order type, off, dep, 2P a/c                                               
         DC    AL1(APRTOFFC+APRTDEPT)                                           
         DC    AL1(APRT2PA)                                                     
* Exp/Prod order type, off, etype, 2P a/c                                       
         DC    AL1(APRTORT1+APRTOFFC+APRTETYP)                                  
         DC    AL1(APRT2PA)                                                     
* Client order type, off, etype, 2P a/c                                         
         DC    AL1(APRTORT2+APRTOFFC+APRTETYP)                                  
         DC    AL1(APRT2PA)                                                     
* No order type, off, etype, 2P a/c                                             
         DC    AL1(APRTOFFC+APRTETYP)                                           
         DC    AL1(APRT2PA)                                                     
* Exp/Prod order type, etype, 2P a/c                                            
         DC    AL1(APRTORT1+APRTETYP)                                           
         DC    AL1(APRT2PA)                                                     
* Client order type, etype, 2P a/c                                              
         DC    AL1(APRTORT2+APRTETYP)                                           
         DC    AL1(APRT2PA)                                                     
* No order type,  etype, 2P a/c                                                 
         DC    AL1(APRTETYP)                                                    
         DC    AL1(APRT2PA)                                                     
* Exp/Prod order type, off, 2P a/c                                              
         DC    AL1(APRTORT1+APRTOFFC)                                           
         DC    AL1(APRT2PA)                                                     
* Client order type, off, 2P a/c                                                
         DC    AL1(APRTORT2+APRTOFFC)                                           
         DC    AL1(APRT2PA)                                                     
* No order type, off, 2P a/c                                                    
         DC    AL1(APRTOFFC)                                                    
         DC    AL1(APRT2PA)                                                     
* Exp/Prod order type, 2P a/c                                                   
         DC    AL1(APRTORT1)                                                    
         DC    AL1(APRT2PA)                                                     
* Client order type, 2P a/c                                                     
         DC    AL1(APRTORT2)                                                    
         DC    AL1(APRT2PA)                                                     
* No order type, 2P a/c                                                         
         DC    AL1(0)                                                           
         DC    AL1(APRT2PA)                                                     
*                                                                               
*  section 2 - with cli/pro/job, no supplier or 2P                              
*                                                                               
* Exp/Prod order type, off, dep, etype, cli, pro, job, med                      
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTETYP+APRTCLI+APRTPRO+X        
               APRTJOB)                                                         
         DC    AL1(APRTMED)                                                     
* Client order type, off, dep, etype, cli, pro, job, med                        
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTETYP+APRTCLI+APRTPRO+X        
               APRTJOB)                                                         
         DC    AL1(APRTMED)                                                     
* No order type, off, dep, etype, cli, pro, job, med                            
         DC    AL1(APRTOFFC+APRTDEPT+APRTETYP+APRTCLI+APRTPRO+APRTJOB)          
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, dep, cli, pro, job, med                             
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTCLI+APRTPRO+APRTJOB)          
         DC    AL1(APRTMED)                                                     
* Client order type, off, dep,  cli, pro, job, med                              
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTCLI+APRTPRO+APRTJOB)          
         DC    AL1(APRTMED)                                                     
* No order type, off, dep,  cli, pro, job, med                                  
         DC    AL1(APRTOFFC+APRTDEPT+APRTCLI+APRTPRO+APRTJOB)                   
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, etype, cli, pro, job, med                           
         DC    AL1(APRTORT1+APRTOFFC+APRTETYP+APRTCLI+APRTPRO+APRTJOB)          
         DC    AL1(APRTMED)                                                     
* Client order type, off, etype, cli, pro, job, med                             
         DC    AL1(APRTORT2+APRTOFFC+APRTETYP+APRTCLI+APRTPRO+APRTJOB)          
         DC    AL1(APRTMED)                                                     
* No order type, off, etype, cli, pro, job, med                                 
         DC    AL1(APRTOFFC+APRTETYP+APRTCLI+APRTPRO+APRTJOB)                   
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, cli, pro, job, med                                  
         DC    AL1(APRTORT1+APRTOFFC+APRTCLI+APRTPRO+APRTJOB)                   
         DC    AL1(APRTMED)                                                     
* Client order type, off, cli, pro, job, med                                    
         DC    AL1(APRTORT2+APRTOFFC+APRTCLI+APRTPRO+APRTJOB)                   
         DC    AL1(APRTMED)                                                     
* No order type, off, cli, pro, job, med                                        
         DC    AL1(APRTOFFC+APRTCLI+APRTPRO+APRTJOB)                            
         DC    AL1(APRTMED)                                                     
*                                                                               
*  section 3 - cli/pro                                                          
* Exp/Prod order type, off, dep, etype, cli, pro, med                           
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTETYP+APRTCLI+APRTPRO)         
         DC    AL1(APRTMED)                                                     
* Client order type, off, dep, etype, cli, pro, med                             
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTETYP+APRTCLI+APRTPRO)         
         DC    AL1(APRTMED)                                                     
* No order type, off, dep, etype, cli, pro, med                                 
         DC    AL1(APRTOFFC+APRTDEPT+APRTETYP+APRTCLI+APRTPRO)                  
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, dep, cli, pro, med                                  
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTCLI+APRTPRO)                  
         DC    AL1(APRTMED)                                                     
* Client order type, off, dep,  cli, pro, med                                   
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTCLI+APRTPRO)                  
         DC    AL1(APRTMED)                                                     
* No order type, off, dep,  cli, pro, med                                       
         DC    AL1(APRTOFFC+APRTDEPT+APRTCLI+APRTPRO)                           
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, etype, cli, pro, med                                
         DC    AL1(APRTORT1+APRTOFFC+APRTETYP+APRTCLI+APRTPRO)                  
         DC    AL1(APRTMED)                                                     
* Client order type, off, etype, cli, pro, med                                  
         DC    AL1(APRTORT2+APRTOFFC+APRTETYP+APRTCLI+APRTPRO)                  
         DC    AL1(APRTMED)                                                     
* No order type, off, etype, cli, pro, med                                      
         DC    AL1(APRTOFFC+APRTETYP+APRTCLI+APRTPRO)                           
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, cli, pro, med                                       
         DC    AL1(APRTORT1+APRTOFFC+APRTCLI+APRTPRO)                           
         DC    AL1(APRTMED)                                                     
* Client order type, off, cli, pro, med                                         
         DC    AL1(APRTORT2+APRTOFFC+APRTCLI+APRTPRO)                           
         DC    AL1(APRTMED)                                                     
* No order type, off, cli, pro, med                                             
         DC    AL1(APRTOFFC+APRTCLI+APRTPRO)                                    
         DC    AL1(APRTMED)                                                     
*                                                                               
*  section 4 - cli only                                                         
*                                                                               
* Exp/Prod order type, off, dep, etype, cli, med                                
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTETYP+APRTCLI)                 
         DC    AL1(APRTMED)                                                     
* Client order type, off, dep, etype, cli, med                                  
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTETYP+APRTCLI)                 
         DC    AL1(APRTMED)                                                     
* No order type, off, dep, etype, cli, med                                      
         DC    AL1(APRTOFFC+APRTDEPT+APRTETYP+APRTCLI)                          
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, dep, cli, med                                       
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTCLI)                          
         DC    AL1(APRTMED)                                                     
* Client order type, off, dep,  cli, med                                        
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTCLI)                          
         DC    AL1(APRTMED)                                                     
* No order type, off, dep,  cli, med                                            
         DC    AL1(APRTOFFC+APRTDEPT+APRTCLI)                                   
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, etype, cli, med                                     
         DC    AL1(APRTORT1+APRTOFFC+APRTETYP+APRTCLI)                          
         DC    AL1(APRTMED)                                                     
* Client order type, off, etype, cli, med                                       
         DC    AL1(APRTORT2+APRTOFFC+APRTETYP+APRTCLI)                          
         DC    AL1(APRTMED)                                                     
* No order type, off, etype, cli, med                                           
         DC    AL1(APRTOFFC+APRTETYP+APRTCLI)                                   
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, etype, cli, med                                          
         DC    AL1(APRTORT1+APRTETYP+APRTCLI)                                   
         DC    AL1(APRTMED)                                                     
* Client order type, etype, cli, med                                            
         DC    AL1(APRTORT2+APRTETYP+APRTCLI)                                   
         DC    AL1(APRTMED)                                                     
* No order type, etype, cli, med                                                
         DC    AL1(APRTETYP+APRTCLI)                                            
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, cli, med                                            
         DC    AL1(APRTORT1+APRTOFFC+APRTCLI)                                   
         DC    AL1(APRTMED)                                                     
* Client order type, off, cli, med                                              
         DC    AL1(APRTORT2+APRTOFFC+APRTCLI)                                   
         DC    AL1(APRTMED)                                                     
* No order type, off, cli, med                                                  
         DC    AL1(APRTOFFC+APRTCLI)                                            
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, cli, med                                                 
         DC    AL1(APRTORT1+APRTCLI)                                            
         DC    AL1(APRTMED)                                                     
* Client order type, cli, med                                                   
         DC    AL1(APRTORT2+APRTCLI)                                            
         DC    AL1(APRTMED)                                                     
* No order type, cli, med                                                       
         DC    AL1(APRTCLI)                                                     
         DC    AL1(APRTMED)                                                     
*                                                                               
*  section 5 - media+supplier, no SJ                                            
*                                                                               
* Exp/Prod order type, off, dep, etype, med, supp                               
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(APRTMED+APRTSUP)                                             
* Client order type, off, dep, etype, med, supp                                 
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(APRTMED+APRTSUP)                                             
* No order type, off, dep, etype, med, supp                                     
         DC    AL1(APRTOFFC+APRTDEPT+APRTETYP)                                  
         DC    AL1(APRTMED+APRTSUP)                                             
* Exp/Prod order type, off, dep, med, supp                                      
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT)                                  
         DC    AL1(APRTMED+APRTSUP)                                             
* Client order type, off, dep, med, supp                                        
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT)                                  
         DC    AL1(APRTMED+APRTSUP)                                             
* No order type, off, dep, med, supp                                            
         DC    AL1(APRTOFFC+APRTDEPT)                                           
         DC    AL1(APRTMED+APRTSUP)                                             
* Exp/Prod order type, off, etype, med, supp                                    
         DC    AL1(APRTORT1+APRTOFFC+APRTETYP)                                  
         DC    AL1(APRTMED+APRTSUP)                                             
* Client order type, off, etype, med, supp                                      
         DC    AL1(APRTORT2+APRTOFFC+APRTETYP)                                  
         DC    AL1(APRTMED+APRTSUP)                                             
* No order type, off, etype, med, supp                                          
         DC    AL1(APRTOFFC+APRTETYP)                                           
         DC    AL1(APRTMED+APRTSUP)                                             
* Exp/Prod order type, etype, med, supp                                         
         DC    AL1(APRTORT1+APRTETYP)                                           
         DC    AL1(APRTMED+APRTSUP)                                             
* Client order type, etype, med, supp                                           
         DC    AL1(APRTORT2+APRTETYP)                                           
         DC    AL1(APRTMED+APRTSUP)                                             
* No order type, etype, med, supp                                               
         DC    AL1(APRTETYP)                                                    
         DC    AL1(APRTMED+APRTSUP)                                             
* Exp/Prod order type, off, med, supp                                           
         DC    AL1(APRTORT1+APRTOFFC)                                           
         DC    AL1(APRTMED+APRTSUP)                                             
* Client order type, off, med, supp                                             
         DC    AL1(APRTORT2+APRTOFFC)                                           
         DC    AL1(APRTMED+APRTSUP)                                             
* No order type, off, med, supp                                                 
         DC    AL1(APRTOFFC)                                                    
         DC    AL1(APRTMED+APRTSUP)                                             
* Exp/Prod order type, med, supp                                                
         DC    AL1(APRTORT1)                                                    
         DC    AL1(APRTMED+APRTSUP)                                             
* Client order type, med, supp                                                  
         DC    AL1(APRTORT2)                                                    
         DC    AL1(APRTMED+APRTSUP)                                             
* No order type, med, supp                                                      
         DC    AL1(0)                                                           
         DC    AL1(APRTMED+APRTSUP)                                             
*                                                                               
*  section 6 - No supplier, client, 2P                                          
*                                                                               
* Exp/Prod order type, off, dep, etype, med                                     
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(APRTMED)                                                     
* Client order type, off, dep, etype, med                                       
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(APRTMED)                                                     
* No order type, off, dep, etype, med                                           
         DC    AL1(APRTOFFC+APRTDEPT+APRTETYP)                                  
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, dep, med                                            
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT)                                  
         DC    AL1(APRTMED)                                                     
* Client order type, off, dep, med                                              
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT)                                  
         DC    AL1(APRTMED)                                                     
* No order type, off, dep, med                                                  
         DC    AL1(APRTOFFC+APRTDEPT)                                           
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, etype, med                                          
         DC    AL1(APRTORT1+APRTOFFC+APRTETYP)                                  
         DC    AL1(APRTMED)                                                     
* Client order type, off, etype, med                                            
         DC    AL1(APRTORT2+APRTOFFC+APRTETYP)                                  
         DC    AL1(APRTMED)                                                     
* No order type, off, etype, med                                                
         DC    AL1(APRTOFFC+APRTETYP)                                           
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, etype, med                                               
         DC    AL1(APRTORT1+APRTETYP)                                           
         DC    AL1(APRTMED)                                                     
* Client order type, etype, med                                                 
         DC    AL1(APRTORT2+APRTETYP)                                           
         DC    AL1(APRTMED)                                                     
* No order type, etype, med                                                     
         DC    AL1(APRTETYP)                                                    
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, off, med                                                 
         DC    AL1(APRTORT1+APRTOFFC)                                           
         DC    AL1(APRTMED)                                                     
* Client order type, off, med                                                   
         DC    AL1(APRTORT2+APRTOFFC)                                           
         DC    AL1(APRTMED)                                                     
* No order type, off, med                                                       
         DC    AL1(APRTOFFC)                                                    
         DC    AL1(APRTMED)                                                     
* Exp/Prod order type, med                                                      
         DC    AL1(APRTORT1)                                                    
         DC    AL1(APRTMED)                                                     
* Client order type, med                                                        
         DC    AL1(APRTORT2)                                                    
         DC    AL1(APRTMED)                                                     
* No order type, med                                                            
         DC    AL1(0)                                                           
         DC    AL1(APRTMED)                                                     
*                                                                               
*  section 7 - cli/pro,  but no supplier, media, 2P                             
*                                                                               
* Exp/Prod order type, off, dep, etype, cli, pro                                
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTETYP+APRTCLI+APRTPRO)         
         DC    AL1(0)                                                           
* Client order type, off, dep, etype, cli, pro                                  
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTETYP+APRTCLI+APRTPRO)         
         DC    AL1(0)                                                           
* No order type, off, dep, etype, cli, pro                                      
         DC    AL1(APRTOFFC+APRTDEPT+APRTETYP+APRTCLI+APRTPRO)                  
         DC    AL1(0)                                                           
* Exp/Prod order type, off, dep, cli, pro                                       
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTCLI+APRTPRO)                  
         DC    AL1(0)                                                           
* Client order type, off, dep,  cli, pro                                        
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTCLI+APRTPRO)                  
         DC    AL1(0)                                                           
* No order type, off, dep,  cli, pro                                            
         DC    AL1(APRTOFFC+APRTDEPT+APRTCLI+APRTPRO)                           
         DC    AL1(0)                                                           
* Exp/Prod order type, off, etype, cli, pro                                     
         DC    AL1(APRTORT1+APRTOFFC+APRTETYP+APRTCLI+APRTPRO)                  
         DC    AL1(0)                                                           
* Client order type, off, etype, cli, pro                                       
         DC    AL1(APRTORT2+APRTOFFC+APRTETYP+APRTCLI+APRTPRO)                  
         DC    AL1(0)                                                           
* No order type, off, etype, cli, pro                                           
         DC    AL1(APRTOFFC+APRTETYP+APRTCLI+APRTPRO)                           
         DC    AL1(0)                                                           
* Exp/Prod order type, off, cli, pro                                            
         DC    AL1(APRTORT1+APRTOFFC+APRTCLI+APRTPRO)                           
         DC    AL1(0)                                                           
* Client order type, off, cli, pro                                              
         DC    AL1(APRTORT2+APRTOFFC+APRTCLI+APRTPRO)                           
         DC    AL1(0)                                                           
* No order type, off, cli, pro                                                  
         DC    AL1(APRTOFFC+APRTCLI+APRTPRO)                                    
         DC    AL1(0)                                                           
*                                                                               
*  section 8 - with client, but no supplier, media, 2P                          
*                                                                               
* Exp/Prod order type, off, dep, etype, cli                                     
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTETYP+APRTCLI)                 
         DC    AL1(0)                                                           
* Client order type, off, dep, etype, cli                                       
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTETYP+APRTCLI)                 
         DC    AL1(0)                                                           
* No order type, off, dep, etype, cli                                           
         DC    AL1(APRTOFFC+APRTDEPT+APRTETYP+APRTCLI)                          
         DC    AL1(0)                                                           
* Exp/Prod order type, off, dep, cli                                            
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTCLI)                          
         DC    AL1(0)                                                           
* Client order type, off, dep,  cli                                             
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTCLI)                          
         DC    AL1(0)                                                           
* No order type, off, dep,  cli                                                 
         DC    AL1(APRTOFFC+APRTDEPT+APRTCLI)                                   
         DC    AL1(0)                                                           
* Exp/Prod order type, off, etype, cli                                          
         DC    AL1(APRTORT1+APRTOFFC+APRTETYP+APRTCLI)                          
         DC    AL1(0)                                                           
* Client order type, off, etype, cli                                            
         DC    AL1(APRTORT2+APRTOFFC+APRTETYP+APRTCLI)                          
         DC    AL1(0)                                                           
* No order type, off, etype, cli                                                
         DC    AL1(APRTOFFC+APRTETYP+APRTCLI)                                   
         DC    AL1(0)                                                           
* Exp/Prod order type, off, cli                                                 
         DC    AL1(APRTORT1+APRTOFFC+APRTCLI)                                   
         DC    AL1(0)                                                           
* Client order type, off, cli                                                   
         DC    AL1(APRTORT2+APRTOFFC+APRTCLI)                                   
         DC    AL1(0)                                                           
* No order type, off, cli                                                       
         DC    AL1(APRTOFFC+APRTCLI)                                            
         DC    AL1(0)                                                           
* Exp/Prod order type, cli                                                      
         DC    AL1(APRTORT1+APRTCLI)                                            
         DC    AL1(0)                                                           
* Client order type, cli                                                        
         DC    AL1(APRTORT2+APRTCLI)                                            
         DC    AL1(0)                                                           
* No order type, cli                                                            
         DC    AL1(APRTCLI)                                                     
         DC    AL1(0)                                                           
*                                                                               
*  section 9 - with supplier,but no cli, media or 2P                            
*                                                                               
* Exp/Prod order type, off, dep, etype                                          
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(APRTSUP)                                                     
* Client order type, off, dep, etype                                            
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(APRTSUP)                                                     
* No order type, off, dep, etype                                                
         DC    AL1(APRTOFFC+APRTDEPT+APRTETYP)                                  
         DC    AL1(APRTSUP)                                                     
* Exp/Prod order type, off, dep                                                 
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT)                                  
         DC    AL1(APRTSUP)                                                     
* Client order type, off, dep                                                   
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT)                                  
         DC    AL1(APRTSUP)                                                     
* No order type, off, dep                                                       
         DC    AL1(APRTOFFC+APRTDEPT)                                           
         DC    AL1(APRTSUP)                                                     
* Exp/Prod order type, off, etype                                               
         DC    AL1(APRTORT1+APRTOFFC+APRTETYP)                                  
         DC    AL1(APRTSUP)                                                     
* Client order type, off, etype                                                 
         DC    AL1(APRTORT2+APRTOFFC+APRTETYP)                                  
         DC    AL1(APRTSUP)                                                     
* No order type, off, etype                                                     
         DC    AL1(APRTOFFC+APRTETYP)                                           
         DC    AL1(APRTSUP)                                                     
* Exp/Prod order type, etype                                                    
         DC    AL1(APRTORT1+APRTETYP)                                           
         DC    AL1(APRTSUP)                                                     
* Client order type, etype                                                      
         DC    AL1(APRTORT2+APRTETYP)                                           
         DC    AL1(APRTSUP)                                                     
* No order type, etype                                                          
         DC    AL1(APRTETYP)                                                    
         DC    AL1(APRTSUP)                                                     
* Exp/Prod order type, off                                                      
         DC    AL1(APRTORT1+APRTOFFC)                                           
         DC    AL1(APRTSUP)                                                     
* Client order type, off                                                        
         DC    AL1(APRTORT2+APRTOFFC)                                           
         DC    AL1(APRTSUP)                                                     
* No order type, off                                                            
         DC    AL1(APRTOFFC)                                                    
         DC    AL1(APRTSUP)                                                     
* Exp/Prod order type                                                           
         DC    AL1(APRTORT1)                                                    
         DC    AL1(APRTSUP)                                                     
* Client order type                                                             
         DC    AL1(APRTORT2)                                                    
         DC    AL1(APRTSUP)                                                     
* No order type                                                                 
         DC    AL1(0)                                                           
         DC    AL1(APRTSUP)                                                     
*                                                                               
* Section 10 1R account levels and types                                        
*                                                                               
* Expenses/prod order type full 1R etype                                        
         DC    AL1(APRTETYP+APRTORT1)                                           
         DC    AL1(APRT2PE)                                                     
* Cli/non cli order type full 1R etype                                          
         DC    AL1(APRTETYP+APRTORT2)                                           
         DC    AL1(APRT2PE)                                                     
* No order type full 1R etype                                                   
         DC    AL1(APRTETYP)                                                    
         DC    AL1(APRT2PE)                                                     
*                                                                               
* Expenses/prod order type full 1R                                              
         DC    AL1(APRTORT1)                                                    
         DC    AL1(APRT2PE)                                                     
* Cli/non-cli order type full 1R                                                
         DC    AL1(APRTORT2)                                                    
         DC    AL1(APRT2PE)                                                     
* No order type full 1R                                                         
         DC    AL1(0)                                                           
         DC    AL1(APRT2PE)                                                     
*                                                                               
* Expenses/prod order type subdepartment etype                                  
         DC    AL1(APRTETYP+APRTORT1)                                           
         DC    AL1(APRT2SD)                                                     
* Cli/non cli order type subdepartment etype                                    
         DC    AL1(APRTETYP+APRTORT2)                                           
         DC    AL1(APRT2SD)                                                     
* No order type full subdepartment etype                                        
         DC    AL1(APRTETYP)                                                    
         DC    AL1(APRT2SD)                                                     
*                                                                               
* Expenses/prod order type subdepartment                                        
         DC    AL1(APRTORT1)                                                    
         DC    AL1(APRT2SD)                                                     
* Cli/non-cli order type subdepartment                                          
         DC    AL1(APRTORT2)                                                    
         DC    AL1(APRT2SD)                                                     
* No order type subdepartment                                                   
         DC    AL1(0)                                                           
         DC    AL1(APRT2SD)                                                     
*                                                                               
* Expenses/prod order type department etype                                     
         DC    AL1(APRTETYP+APRTORT1)                                           
         DC    AL1(APRT2DE)                                                     
* Cli/non cli order type department etype                                       
         DC    AL1(APRTETYP+APRTORT2)                                           
         DC    AL1(APRT2DE)                                                     
* No order type full department etype                                           
         DC    AL1(APRTETYP)                                                    
         DC    AL1(APRT2DE)                                                     
                                                                                
* Expenses/prod order type department                                           
         DC    AL1(APRTORT1)                                                    
         DC    AL1(APRT2DE)                                                     
* Cli/non-cli order type department                                             
         DC    AL1(APRTORT2)                                                    
         DC    AL1(APRT2DE)                                                     
* No order type department                                                      
         DC    AL1(0)                                                           
         DC    AL1(APRT2DE)                                                     
*                                                                               
* Expenses/prod order type office etype                                         
         DC    AL1(APRTETYP+APRTORT1)                                           
         DC    AL1(APRT2OF)                                                     
* Cli/non cli order type office etype                                           
         DC    AL1(APRTETYP+APRTORT2)                                           
         DC    AL1(APRT2OF)                                                     
* No order type full office etype                                               
         DC    AL1(APRTETYP)                                                    
         DC    AL1(APRT2OF)                                                     
*                                                                               
* Expenses/prod order type office                                               
         DC    AL1(APRTORT1)                                                    
         DC    AL1(APRT2OF)                                                     
* Cli/non-cli order type office                                                 
         DC    AL1(APRTORT2)                                                    
         DC    AL1(APRT2OF)                                                     
* No order type office                                                          
         DC    AL1(0)                                                           
         DC    AL1(APRT2OF)                                                     
*                                                                               
*  section 11 - only 2D off/dep and types                                       
*                                                                               
* Exp/Prod order type, off, dep, etype                                          
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(0)                                                           
* Client order type, off, dep, etype                                            
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT+APRTETYP)                         
         DC    AL1(0)                                                           
* No order type, off, dep, etype                                                
         DC    AL1(APRTOFFC+APRTDEPT+APRTETYP)                                  
         DC    AL1(0)                                                           
* Exp/Prod order type, off, dep                                                 
         DC    AL1(APRTORT1+APRTOFFC+APRTDEPT)                                  
         DC    AL1(0)                                                           
* Client order type, off, dep                                                   
         DC    AL1(APRTORT2+APRTOFFC+APRTDEPT)                                  
         DC    AL1(0)                                                           
* No order type, off, dep                                                       
         DC    AL1(APRTOFFC+APRTDEPT)                                           
         DC    AL1(0)                                                           
* Exp/Prod order type, off, etype                                               
         DC    AL1(APRTORT1+APRTOFFC+APRTETYP)                                  
         DC    AL1(0)                                                           
* Client order type, off, etype                                                 
         DC    AL1(APRTORT2+APRTOFFC+APRTETYP)                                  
         DC    AL1(0)                                                           
* No order type, off, etype                                                     
         DC    AL1(APRTOFFC+APRTETYP)                                           
         DC    AL1(0)                                                           
* Exp/Prod order type, etype                                                    
         DC    AL1(APRTORT1+APRTETYP)                                           
         DC    AL1(0)                                                           
* Client order type, etype                                                      
         DC    AL1(APRTORT2+APRTETYP)                                           
         DC    AL1(0)                                                           
* No order type, etype                                                          
         DC    AL1(APRTETYP)                                                    
         DC    AL1(0)                                                           
* Exp/Prod order type, off                                                      
         DC    AL1(APRTORT1+APRTOFFC)                                           
         DC    AL1(0)                                                           
* Client order type, off                                                        
         DC    AL1(APRTORT2+APRTOFFC)                                           
         DC    AL1(0)                                                           
* No order type, off                                                            
         DC    AL1(APRTOFFC)                                                    
         DC    AL1(0)                                                           
* No order type, dept                                                           
         DC    AL1(APRTDEPT)                                                    
         DC    AL1(0)                                                           
* Exp/Prod order type                                                           
         DC    AL1(APRTORT1)                                                    
         DC    AL1(0)                                                           
* Client order type                                                             
         DC    AL1(APRTORT2)                                                    
         DC    AL1(0)                                                           
* No order type                                                                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
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
                                                                                
PZERO    DC    P'0'                                                             
FFS      DC    16X'FF'                                                          
SILEDGER DC    C'SI'                                                            
SKLEDGER DC    C'SK'                                                            
STLEDGER DC    C'ST'                                                            
CONREC   DC    C'000000'                                                        
                                                                                
JOBFLDS  DC    AL2(AC#OE,AC#CE,AC#RSHR,0)                                       
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
ACCARC   DC    CL8'ACCARC'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
                                                                                
ORDKEYT  LKKEY H,ORDKEY,SAVED                                                   
         LKKEY LIT,ORDKTYP,ORDKTYPQ                                             
         LKKEY SIN,ORDKCPY,AGENCY                                               
         LKKEY LIT,ORDKSP1,0                                                    
         LKKEY ALL,ORDKORD                                                      
         LKKEY LIT,ORDKSEQ,0                                                    
         LKKEY LIT,ORDKSP2,0                                                    
         LKKEY E                                                                
                                                                                
PIDKEYT  LKKEY H,PIDKEY,SAVED                                                   
         LKKEY LIT,PIDKTYP,PIDKTYPQ                                             
         LKKEY LIT,PIDKSUB,PIDKSUBQ                                             
         LKKEY SIN,PIDKCPY,AGENCY                                               
         LKKEY LIT,PIDKREM,0                                                    
         LKKEY SIN,PIDKPID,CURRPID                                              
         LKKEY LIT,PIDKSTYP,PIDKORDQ                                            
         LKKEY ALL,PIDKORD                                                      
         LKKEY LIT,PIDKOR2,0                                                    
         LKKEY LIT,PIDKSEQ,0                                                    
         LKKEY E                                                                
                                                                                
OSUKEYT  LKKEY H,OSUPAS,SAVED                                                   
         LKKEY LIT,OSUPTYP,OSUPTYPQ                                             
         LKKEY LIT,OSUPSUB,OSUPSUBQ                                             
         LKKEY SIN,OSUPCPY,AGENCY                                               
         LKKEY LIT,OSUPREM,0                                                    
         LKKEY SIN,OSUSULA,R_OLSUPC                                             
         LKKEY ALL,OSUPMEM                                                      
***      LKKEY LIT,OSUPMEM,OSUPMNOQ                                             
         LKKEY ALL,OSUPORD                                                      
         LKKEY E                                                                
                                                                                
OSJKEYT  LKKEY H,OSJPAS,SAVED                                                   
         LKKEY LIT,OSJPTYP,OSJPTYPQ                                             
         LKKEY LIT,OSJPSUB,OSJPSUBQ                                             
         LKKEY SIN,OSJPCPY,AGENCY                                               
         LKKEY LIT,OSJPREM,0                                                    
         LKKEY RNG,OSJPACT,CPJRANGE                                             
         LKKEY ALL,OSJPMEM                                                      
         LKKEY ALL,OSJPORD                                                      
         LKKEY E                                                                
                                                                                
ONSKEYT  LKKEY H,SONKEY,SAVED                                                   
         LKKEY LIT,SONKTYP,SONKTYPQ                                             
         LKKEY SIN,SONKCPY,AGENCY                                               
         LKKEY LIT,SONKSRC,SONKSRCQ                                             
         LKKEY LIT,SONKSUB,SONKWDSQ                                             
         LKKEY LIT,SONKUNDF,0                                                   
         LKKEY ALL,SONKWRD1,XORNAM                                              
         LKKEY ALL,SONKSEQ1                                                     
         LKKEY ALL,SONKORD                                                      
         LKKEY E                                                                
                                                                                
OSTKEYT  LKKEY H,OSTPAS,SAVED                                                   
         LKKEY LIT,OSTPTYP,OSTPTYPQ                                             
         LKKEY LIT,OSTPSUB,OSTPSUBQ                                             
         LKKEY SIN,OSTPCPY,AGENCY                                               
         LKKEY LIT,OSTPREM,0                                                    
         LKKEY WMP,OSTPOST,AWMPOST                                              
         LKKEY ALL,OSTPORD                                                      
         LKKEY E                                                                
                                                                                
***********************************************************************         
* Literal pool                                                        *         
***********************************************************************         
                                                                                
L_OAAMNT DC    C'Order amount'                                                  
L_OLVIEW DC    C'View type'                                                     
L_OLASTA DC    C'Approval status'                                               
L_OLMSTA DC    C'Matching status'                                               
L_OLMYST DC    C'My approval status'                                            
L_OLCLIC DC    C'Client code'                                                   
L_OLPROC DC    C'Product code'                                                  
L_OLJOBC DC    C'Job code'                                                      
L_OLSUPC DC    C'Supplier code'                                                 
L_OLETYP DC    C'Expenditure type'                                              
L_OLSDAT DC    C'Order date - start'                                            
L_OLEDAT DC    C'Order date - end'                                              
L_OLTYPE DC    C'Order type'                                                    
L_OLOFFC DC    C'Office code'                                                   
L_OLCURR DC    C'Currency code'                                                 
L_OLRBDS DC    C'Req by start date'                                             
L_OLRBDE DC    C'Req by end date '                                              
L_OLESTN DC    C'Estimate number'                                               
L_OLEXPA DC    C'Expense account'                                               
L_OLAPPC DC    C'Approver PID'                                                  
L_OCREAT DC    C'Order creator'                                                 
L_OLINLJ DC    C'Incl. locked Jobs'                                             
L_OLINCJ DC    C'Incl. closed Jobs'                                             
L_OATYPE DC    C'Order type'                                                    
L_OA2DAC DC    C'2D account code'                                               
L_OAOWN  DC    C'Owner'                                                         
L_OAPLMT DC    C'Aplimit category'                                              
L_OASLFA DC    C'Etype allows self approval'                                    
L_OA2PAC DC    C'2P account code'                                               
L_OAAPLC DC    C'Aplimit category'                                              
L_OAEDTS DC    C'Editor is original submitter'                                  
L_OAORDN DC    C'Order number'                                                  
L_OAALLC DC    C'All client but different'                                      
L_OASMCL DC    C'Some clients but not all'                                      
L_OACPCA DC    C'Claimant person code accounting'                               
L_OACLDT DC    C'Claim date'                                                    
L_OASKES DC    C'Skip estimate check'                                           
L_OAWAMT DC    C'Workcode amount'                                               
L_OAESTN DC    C'Estimate number'                                               
L_OAWRKC DC    C'Workcode'                                                      
L_OLOLL  DC    C'Override Limit List'                                           
L_OLIEXP DC    C'Include Expense Orders'                                        
L_OLIINT DC    C'Include Internal Orders'                                       
L_OLIPRO DC    C'Include Production Orders'                                     
L_OLDEPC DC    C'2D department a/c code'                                        
L_OLSTAC DC    C'2P staff a/c code'                                             
L_OLNSRC DC    C'Name Search'                                                   
                                                                                
LVALUES  DS    0D                                                               
         DC    A(ORDKF)                                                         
         DC    A(PIDKF)                                                         
         DC    A(OSUKF)                                                         
         DC    A(OSJKF)                                                         
         DC    A(ONSKF)                                                         
         DC    A(OSTKF)                                                         
         DC    A(ORDRF)                                                         
LVALUESL EQU   *-LVALUES                                                        
                                                                                
ORDKQ    EQU   C'O'                                                             
PIDKQ    EQU   C'P'                                                             
OSUKQ    EQU   C'S'                                                             
OSJKQ    EQU   C'J'                                                             
OSTKQ    EQU   C'T'                                                             
ONSKQ    EQU   C'N'                                                             
COMPQ    EQU   C'C'                    Compulsory                               
INTORDQ  EQU   C'I'                                                             
PRODORDQ EQU   C'P'                                                             
ARTORDQ  EQU   C'A'                                                             
EXPORDQ  EQU   C'E'                                                             
ORDERQ   EQU   C'O'                                                             
PRESTOQ  EQU   C'P'                                                             
AURAQ    EQU   C'A'                                                             
AWAITQ   EQU   C'1'                                                             
APPROVEQ EQU   C'2'                                                             
REJECTDQ EQU   C'3'                                                             
IAMAPPRQ EQU   C'4'                                                             
AWMYAPRQ EQU   C'1'                                                             
APPBYMEQ EQU   C'2'                                                             
REJBYMEQ EQU   C'3'                                                             
AWOTHERQ EQU   C'4'                                                             
INPROGQ  EQU   C'1'                                                             
SUBMITQ  EQU   C'2'                                                             
PARTAPQ  EQU   C'3'                                                             
FULAPPQ  EQU   C'4'                                                             
REJECTQ  EQU   C'5'                                                             
DELETEQ  EQU   C'6'                                                             
SPACEQ   EQU   C' '                                                             
EOR      EQU   0                                                                
                                                                                
B#ORDREC EQU   3                       IO2 Order record                         
B#PIDREC EQU   B#ORDREC                IO2 Order record                         
B#OSTREC EQU   B#ORDREC                IO2 Order record                         
B#OSUREC EQU   B#ORDREC                IO2 Order record                         
B#OSJREC EQU   B#ORDREC                IO2 Order record                         
B#SVRDEF EQU   4                       - SERVER DEFINITION                      
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)                                 
B#LP_D   EQU   5                       - LP_D                                   
                                                                                
***********************************************************************         
* Saved storage                                                       *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
                                                                                
TSARRECS DS    XL(TSPXTNL)         TSAR block for record buffer                 
                                                                                
WVALUES  DS    0X                                                               
ADCONS   DS    0A                                                               
AORDKF   DS    A                                                                
APIDKF   DS    A                                                                
AOSUKF   DS    A                                                                
AOSJKF   DS    A                                                                
AONSKF   DS    A                                                                
AOSTKF   DS    A                                                                
AORDRF   DS    A                                                                
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
AMASTC   DS    A                                                                
APRINTER DS    A                                                                
VACCEMU  DS    A                                                                
ARUNFACS DS    A                                                                
AUINTAB  DS    A                   A(upper case translate table)                
ASRCTAB  DS    A                   A(search translate table)                    
LIOTRACE DS    F                                                                
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
MAPI1    DS    X                   ** MAP INDICATOR BYTE1 1 **                  
MAPI2    DS    X                   ** MAP INDICATOR BYTE1 2 **                  
                                                                                
SAVEVAR  DS    0F                  ** Variables **                              
                                                                                
SAVERF   DS    F                   Temp save area for RF                        
ACLOCL   DS    F                   saved address of most recent loctn           
ALOCEL   DS    F                   saved location element                       
ADRFSLVL DS    F                   First approval level of interest             
ADRCULVL DS    F                   Current approval level entry                 
ADRAPLVL DS    F                   Approval level greater than order            
ANXTSVE  DS    F                   Address of next entry in wc table            
NUMHDR   DS    H                   Number of rows in APLVTAB                    
NUMWCR   DS    H                   Number of entries in SVESTWK                 
TSACNT   DS    F                   tsar record count                            
MYCOUNT  DS    F                   general counter                              
XBYTE1   DS    XL1                                                              
XBYTE2   DS    XL1                                                              
REREAD   DS    CL1                                                              
DIRTYPE  DS    CL1                                                              
ORSTAT   DS    CL1                                                              
ORSTOK   DS    CL1                                                              
FLTMONS  DS    XL3                                                              
AGENCY   DS    XL1                                                              
IWMPOST  DS    XL1                                                              
AWMPOST  DS    AL3                                                              
ESTAPUN  DS    CL1                 Flag for approved/unapproved ests            
WMPOSTMQ EQU   8                                                                
PERSON   DS    XL2                                                              
CURRPID  DS    XL2                                                              
SRCHAPP  DS    XL2                                                              
SRBDSTA  DS    XL3                                                              
SRBDEND  DS    XL3                                                              
ARTLVLS  DS    XL4                                                              
CPJRANGE DS    0XL(2*12)                                                        
CPJRSTA  DS    XL12                                                             
CPJREND  DS    XL12                                                             
LLINDS   DS    XL1                 Order list indicators                        
LLINREQ  EQU   X'80'               Limlist not found/not required               
LLISEMP  EQU   X'40'               Limlist                                      
LLINCQ   EQU   X'20'               Client                                       
LLINMQ   EQU   X'10'               Media                                        
LLINEQ   EQU   X'08'               Expenditure type                             
OA_INDS  DS    XL1                 Order approver lookup indicators             
OA_GREA  EQU   X'80'               Level > than order found                     
OA_FOND  EQU   X'40'               Approver found at this level                 
OA_IEML  EQU   X'20'               emails can be sent                           
OA_ISLF  EQU   X'10'               Approver can self approve                    
OA_IPUR  EQU   X'08'               Purchasing approver required                 
OA_IHLV  EQU   X'04'               Reading high levels for low levels           
OA_IHSL  EQU   X'02'               As above but for self approvers only         
LLRST12  DS    XL2                                                              
OAPSTAT  DS    CL1                                                              
XL#TODP  DS    XL3                 Today's date packed                          
         DS    XL1                                                              
XL#TODC  DS    XL2                 Today's date compressed                      
X#PPID   DS    XL2                                                              
X#TOTOE  DS    PL6                 Total original estimated amount              
X#TOTCE  DS    PL6                 Total current estimated amount               
X#TOTHR  DS    PL6                 Total highest revision estimated amt         
X#TOTOR  DS    PL6                 uninvoiced amt+posted amt incl time          
X#PL16   DS    PL16                                                             
                                                                                
X#RTTRX  DS    XL(TMSTTABL)        R time type settings                         
X#BTTRX  DS    XL(TMSTTABL)        B time type settings                         
TMSTNUM  EQU   (*-X#RTTRX)/TMSTTABL                                             
                                                                                
ORDPRFS  DS    XL(ORDPRFL)         Saved order profiles                         
                                                                                
SEOFPOS  DS    XL1                 SE ledger Offpos                             
SQOFPOS  DS    XL1                 SQ ledger Offpos                             
SVOFPOS  DS    XL1                 SV ledger Offpos                             
*                                                                               
*                                                                               
OL_INDS  DS    XL1                 OLIST - OFFICE CODE INDICATOR                
OL_PRSNT EQU   X'80'               OLIST IS PRESENT FOR OFFICE CODE             
OL_PROLT EQU   X'40'               PROCESS OLIST DETAILS                        
*                                                                               
SAVOLIST DS    CL(OLSTDLNQ*MAXOLSTQ) WORK AREA TO SAVE OLIST                    
MAXOLSTQ EQU   100                 MAXIMUM OLIST ENTRIES ALLOWED - 1            
*                                                                               
APLVTAB  DS    CL(APLVLTBL*MAXLVL) Aplimit level table                          
APLVTAB2 DS    CL(APLVLTBL*MAXLVL) Aplimit level table                          
                                                                                
         DS    0H                                                               
GAPAREA  DS    XL(GAPTLNQ)         Area to use for buffer rec                   
ATSRERRS DS    XL1                 Error area for buffer                        
GAPLPARM DS    XL1                 Additonal parameter for GAPLST call          
TSARABUF DS    XL(TSPXTNL)         TSAR block for approval buffer               
TSAROLBF DS    XL200               Order list buffer                            
TSAROABF DS    XL200               Order approval buffer                        
SVTSAREA DS    XL200               Saved TSAR area                              
PREVCPJ  DS    CL14                Previous CPJ account                         
                                                                                
DSDICTL  DS    0C                                                               
AC@ALL   DS    CL4                 All                                          
AC@YES   DS    CL5                 Yes                                          
                                                                                
         DS    0H                  Aura name search                             
XORNAMS  DS    0XL((5*(10+1))+1)                                                
XORNAL   DS    XL1                                                              
XORNAM   DS    CL10                                                             
XORNAM#  DS    XL1                                                              
XORNAL1  DS    XL1                                                              
XORNAM1  DS    CL10                                                             
XORNAL2  DS    XL1                                                              
XORNAM2  DS    CL10                                                             
XORNAL3  DS    XL1                                                              
XORNAM3  DS    CL10                                                             
XORNAL4  DS    XL1                                                              
XORNAM4  DS    CL10                                                             
                                                                                
***********************************************************************         
* Request in/out definition in storage                                *         
***********************************************************************         
                                                                                
QVALUES  DS    0XL256              ** REQUEST VALUES **                         
                                                                                
*                              **  Aura order list call **                      
                                                                                
R_OLAIND DS    XL1                 see R_OLASTA approval status                 
R_OLAADR DS    AL3                                                              
R_OLMIND DS    XL1                 see R_OLMSTA matching status                 
R_OLMADR DS    AL3                                                              
R_OLNSIN DS    X                   Aura name search                             
R_OLNSAD DS    AL3                                                              
                                                                                
R_OLVIEW DS    CL1                 Main view type                               
R_OLSRCQ EQU   C'0'                - all orders search                          
R_OLMYOQ EQU   C'1'                - my orders (created)                        
R_OLMYAQ EQU   C'2'                - my approvals                               
R_OLASTA DS    CL8                 Approval status                              
R_OLMYST DS    CL1                 My approval status                           
R_OLMSTA DS    XL1                 Matching status                              
UNMATQ   EQU   X'08'               Unmatched                                    
PARMATQ  EQU   X'04'               Part matched                                 
COMMATQ  EQU   X'02'               Fully matched or closed                      
CANCLDQ  EQU   X'01'               Cancelled (unmatched and closed)             
R_OLCLIC DS    CL6                 Other search criteria ...                    
R_OLPROC DS    CL6                                                              
R_OLJOBC DS    CL6                                                              
R_OLSUPC DS    CL14                Supplier code                                
R_OLETYP DS    CL3                 Expenditure type                             
R_OLSDAT DS    CL8                                                              
R_OLEDAT DS    CL8                                                              
R_OLDATL EQU   *-R_OLSDAT                                                       
R_OLTYPE DS    CL1                                                              
R_OCREAT DS    CL8                                                              
R_OLINLJ DS    CL1                 Locked jobs: Yes, no and only                
R_OLINCJ DS    CL1                 Closed jobs: Yes, no and only                
R_OLOLL  DS    CL1                 Override limit list                          
R_OLIEXP DS    CL1                 Security: exclude expense orders             
R_OLIINT DS    CL1                 Security: exclude internal orders            
R_OLIPRO DS    CL1                 Security: exclude production orders          
R_OLOFFC DS    CL2                 Office code                                  
R_OLCURR DS    CL3                 Currency code                                
R_OLRBDS DS    CL8                 Required by start date                       
R_OLRBDE DS    CL8                 Required by end date                         
R_OLESTN DS    CL6                 Estimate global number                       
R_OLEXPA DS    CL14                Expense account                              
R_OLAPPC DS    CL8                 Approver PID                                 
R_OLDEPC DS    CL12                Department 2D account code                   
R_OLSTAC DS    CL12                Staff 2P account code                        
         DS    CL(L'QVALUES-(*-QVALUES)) remaining storage                      
         ORG   QVALUES                                                          
*                               ** Aura order approval lookup **                
R_OATOTA DS    PL6                 Order total amount                           
R_OATYPE DS    CL1                 Order type                                   
R_OAEXP  EQU   C'E'                expense                                      
R_OAPRD  EQU   C'P'                production                                   
R_OAART  EQU   C'A'                artist                                       
R_OAINT  EQU   C'I'                internal                                     
R_OAETY  DS    CL3                 Expenditure type                             
R_OACLI  DS    CL5                 Client code                                  
R_OAPRC  DS    CL6                 Product code                                 
R_OAJBC  DS    CL6                 Job code                                     
R_OA2DA  DS    CL12                2D account code                              
R_OAOWN  DS    CL1                 Owner Y/N                                    
R_OASUPC DS    CL14                Supplier account                             
R_OAAPLC DS    CL1                 Aplimit category Y/N                         
R_OAETYS DS    CL1                 Etype allows self approval Y/N               
R_OA2PAC DS    CL2                 2P account code                              
R_OAEDOS DS    CL1                 Editor is original submitter Y/N             
R_OAORDN DS    CL6                 Order number                                 
R_OAALLC DS    CL1                 All client but different                     
R_OAAMIX DS    CL1                 Some clients but not all                     
R_OACLPA DS    CL8                 Claimant person code accounting              
R_OACLPD DS    CL8                 Claim date                                   
R_OASKEC DS    CL1                 Skip estimate check                          
R_OAESTN DS    CL6                 Estimate number                              
R_OAWRKI DS    CL1                 Workcode array indicator                     
R_OAWRKA DS    AL3                 Workcode array address                       
         DS    CL(L'QVALUES-(*-QVALUES)) remaining storage                      
         ORG                                                                    
                                                                                
DVALUES  DS    0F                  ** DERIVED VALUES **                         
D_ORPF   DS    XL(ORDPRFL)         Order profiles                               
D_OFF    DS    CL(L'TRNOFFC)       Office code                                  
D_OFFC   DS    CL(L'TRNOFFC)       Client office code                           
CPJFULA  DS    0CL14               u/l/a                                        
CPJFULG  DS    CL2                 u/l                                          
CPJFACC  DS    CL(L'ACTKACT)       SJ account code                              
CPJLACC  DS    XL1                 c/p/j account length                         
         DS    XL1                                                              
D_1RACC  DS    CL(L'TRNKULA)       1R account code                              
SAVEKEY  DS    XL(L'IOKEY)         Overlay save key area                        
DVALUESL EQU   *-DVALUES                                                        
                                                                                
AALL     DS    AL3                 ALL VALUE WMP ENTRY                          
ANZR     DS    AL3                 NON-ZERO VALUE WMP ENTRY                     
SVPERKEY DS    XL(L'IOKEY)         Saved personal control key                   
                                                                                
OVALUES  DS    0XL2048             ** OUTPUT VALUES **                          
                                                                                
O_DATA   DS    0X                  Output values A#OLVD/A#OSRC                  
O_REQNO  DS    CL6                                                              
O_TYPE   DS    CL1                                                              
O_APSTAT DS    CL1                                                              
O_MASTAT DS    CL1                                                              
O_CPJULA DS    0CL14                                                            
         DS    CL2                                                              
O_CPJAC  DS    CL12                                                             
O_EXPAC  DS    CL14                                                             
O_SUPAC  DS    CL14                                                             
O_SUPNM  DS    CL36                Supplier name                                
O_SUPEM  DS    CL50                Supplier email address                       
O_ODATE  DS    XL3                                                              
O_CRPER  DS    CL8                                                              
O_CFSTN  DS    CL16                                                             
O_CMIDN  DS    CL16                                                             
O_CLSTN  DS    CL58                                                             
O_CEMAIL DS    CL52                                                             
O_AGYCUR DS    CL3                 Agency currency                              
O_WORKCD DS    CL2                 Expense order work code                      
O_ETYP   DS    CL3                 Expenditure type                             
O_ETNAME DS    CL36                Expenditure type name                        
O_OFFNAM DS    CL36                                                             
O_RBDAT  DS    XL3                                                              
O_ESTNO  DS    CL6                                                              
O_ESTNAM DS    CL50                                                             
O_AMOUNT DS    PL6                                                              
O_INVAMT DS    PL6                                                              
O_FORAMT DS    PL6                                                              
O_CURR   DS    CL3                                                              
O_GAPST  DS    XL1                                                              
O_AGENT  DS    CL12                                                             
O_ANAME  DS    CL36                                                             
O_CLINM  DS    CL36                                                             
O_PRONM  DS    CL36                                                             
O_JOBNM  DS    CL36                                                             
O_EXPNM  DS    CL36                                                             
O_GOODS  DS    CL1                                                              
O_JSTAT  DS    CL1                                                              
O_IDNUM  DS    XL2                                                              
O_OSRCE  DS    CL1                                                              
O_OAMYA  DS    CL1                                                              
O_DEPACT DS    CL12                                                             
O_OSUBM  DS    CL8                                                              
O_ETSAPP DS    CL1                                                              
O_NAME   DS    CL100                                                            
O_GAPIY  DS    CL1                 GAP IN USE                                   
O_GAPSED DS    PL3                 GAP SEND DATE                                
O_GAPEXD DS    PL3                 GAP EXPIRY DATE                              
O_GAPEMD DS    CL50                EMAIL ADDRESS                                
         DS    (MAXEML-1)CL(L'O_GAPEMD)                                         
O_GAPEML EQU   *-O_GAPEMD                                                       
O_DATALQ EQU   *-O_DATA                                                         
         DS    CL(L'OVALUES-(*-OVALUES))      space left                        
                                                                                
         ORG   OVALUES                                                          
OA_APPC  DS    CL8                 Approver person code                         
OA_APFN  DS    CL16                Approver first name                          
OA_APLN  DS    CL58                Approver last name                           
OA_APAL  DS    PL6                 Approval limit                               
OA_APPT  DS    CL1                 Approver type                                
OA_DEFT  DS    CL1                 Default (Y/N)                                
OA_APPE  DS    CL52                Approver person email address                
OA_LEVA  DS    CL3                 Level of approver in hierarchy               
OA_APPM  DS    CL16                Approver person middle name                  
OA_MTOF  DS    CL1                 Matched at office (Y/N)                      
OA_MTDP  DS    CL1                 Matched at department (Y/N)                  
OA_MTET  DS    CL1                 Matched at etype (Y/N)                       
OA_MTSU  DS    CL1                 Matched at supplier (Y/N)                    
OA_MT2P  DS    CL1                 Matched at 2P account (Y/N)                  
OA_MTJB  DS    CL1                 Matched at job (Y/N)                         
OA_MTPR  DS    CL1                 Matched at product (Y/N)                     
OA_MTCL  DS    CL1                 Matched at client (Y/N)                      
OA_MTMD  DS    CL1                 Matched at media (Y/N)                       
OA_ECKE  DS    XL1                 Estimate check error message                 
OA_NCEQ  EQU   1                   No current estimate                          
OA_NAEQ  EQU   2                   No approved estimate                         
OA_ORJQ  EQU   3                   Order exceeds job total                      
OA_OESQ  EQU   4                   Order exceeds est total                      
OA_OHSQ  EQU   5                   Order exceeds amt on highest sub est         
OA_ECWM  DS    XL1                 Estimate check warning message               
OA_EJLE  DS    PL6                 Job level remaining amount                   
OVALUESL EQU   *-OVALUES                                                        
         DS    CL(L'OVALUES-(*-OVALUES))      space left                        
                                                                                
         ORG   OVALUES+L'OVALUES                                                
                                                                                
         DS    0H                                                               
SVESTWK  DS    200CL(L'JBCOLWC+L'JBCOLVAL)  (1600)                              
                                                                                
SAVEVARL EQU   *-SAVEVAR                                                        
                                                                                
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
                                                                                
***********************************************************************         
* Included books and DSECTS                                           *         
***********************************************************************         
WRKD     DSECT                                                                  
WRKCODE  DS    CL2                 WORKCODE CODE                                
WRKAMT   DS    PL6                 WORKCODE AMOUNT                              
WRKDL    EQU   *-WRKD                                                           
                                                                                
ESTCHKD  DSECT                                                                  
ESTCKID  DS    XL1              Estimate check ID                               
ESTCKIQ  EQU   X'40'            Unique identifier                               
ESTCLEN  DS    XL1              Estimate check table length                     
ESTCHWC  DS    CL2              Workcode                                        
ESTCHOE  DS    PL6              Original estimate                               
ESTCHCE  DS    PL6              Current estimate                                
ESTCHHR  DS    PL6              Highest revised estimate                        
ESTCHAM  DS    PL6              Order workcode amount                           
ESTCHOR  DS    PL6              Other charges amount                            
ESTRAMT  DS    PL6              Remaining amount                                
ESTCHKE  DS    XL1              estimate check error message                    
ESTNCESQ EQU   1                No current estimate                             
ESTNAPEQ EQU   2                No approved estimate                            
ESTREJBQ EQU   3                Work code exceeds remaining job amount          
ESTREESQ EQU   4                Work code exceeds remaining est amount          
ESTCHKW  DS    XL1              estimate check warning message                  
ESTCHLQ  EQU   *-ESTCHKD                                                        
                                                                                
EAETABD  DSECT                                                                  
EAETGOV  DS    CL1                 ECE SETTING                                  
EAETERR  DS    XL1                 ECE ERROR                                    
EAETLNQ  EQU   *-EAETABD                                                        
                                                                                
EWCTABD  DSECT                                                                  
EWCTGOV  DS    CL1                 ECE SETTING                                  
EWCTERR  DS    XL1                 ECE ERROR                                    
EWCTLNQ  EQU   *-EWCTABD                                                        
                                                                                
BUFFERD  DSECT                                                                  
BUFLEN   DS    XL2                 TSAR layout                                  
BUFKEY   DS    0XL15                                                            
BUFKTYP  DS    CL1                                                              
BUFKPIDQ EQU   C'1'                                                             
BUFKETYQ EQU   C'2'                                                             
BUFKESTQ EQU   C'3'                                                             
BUFKOFFQ EQU   C'4'                                                             
BUFKACCQ EQU   C'5'                                                             
BUFKACSQ EQU   C'6'                                                             
BUFKVAL  DS    XL14                                                             
BUFREC   DS    0X                                                               
BUFRESTN DS    CL50                Estimate   - Name                            
         ORG   BUFREC                                                           
BUFRETYN DS    CL36                Exptype    - Name                            
BUFRETSA DS    CL1                            - Self approval allowed Y         
         ORG   BUFREC                                                           
BUFROFFN DS    CL36                Office     - Name                            
         ORG   BUFREC                                                           
BUFRACCN DS    CL36                Account    - Name                            
BUFRACCS DS    CL1                            - Status                          
BUFGAPIY DS    CL1                 GAP in use                                   
BUFGAPSD DS    PL3                 GAP sent date                                
BUFGAPED DS    PL3                 GAP expiry date                              
BUFGAPEM DS    CL50                Supplier email address                       
         ORG   BUFREC                                                           
BUFRASC  DS    CL1                 Account    - Security access                 
         ORG   BUFREC                                                           
BUFRPCOD DS    CL8                 PID        - code                            
BUFRPFST DS    CL16                           - first name                      
BUFRPMID DS    CL16                           - middle name                     
BUFRPLST DS    CL58                           - last name                       
BUFRPEMA DS    CL52                           - eMail address                   
BUFFRLNQ EQU   *-BUFFERD                                                        
*                                                                               
APLTABD  DSECT                                                                  
APLTSTAT DS    CL1                                                              
APLTORT1 EQU   X'80'               Order type - expense prod inter arts         
APLTOFFC EQU   X'40'               Office                                       
APLTETYP EQU   X'20'               Expenditure type                             
APLTCLI  EQU   X'10'               Client                                       
APLTORT2 EQU   X'08'               Order type - client or non client            
APLTABL  EQU   *-APLTABD           length of entry                              
                                                                                
APLVLTD  DSECT                                                                  
APLVLID  DS    XL1                 Identifier                                   
APLVTIQ  EQU   X'80'               Identifier                                   
APLVLEN  DS    XL1                 Aplimit level length                         
APLVLVAL DS    PL6                 Value of approval level                      
APLVLNUM DS    XL1                 Number of approvers at this level            
APLVLPRV DS    XL1                 Number of previous levels                    
APLVLFAP DS    CL1                 Final approval required                      
APLVLHLV DS    CL1                 High level approvers for lower lvls          
APLVLLVL DS    H                   Level of approver so far                     
APLVLTBL EQU   *-APLVLTD           length of entry                              
*                                                                               
APETABD  DSECT                                                                  
APERAPL  DS    PL6                 Approval limit                               
APERPID  DS    CL8                 Approver PID                                 
APEDUPL  EQU   *-APETABD           Duplicate length check                       
APERAPL2 DS    PL6                 Approval limit - used for sort               
APERLVL  DS    CL3                 Approval level                               
APERPLN  DS    CL58                Approver last name                           
APERPFN  DS    CL16                Approver first name                          
APERPMN  DS    CL16                Approver middle name                         
APEKEYL  EQU   *-APETABD           Key length                                   
APESRTL  EQU   *-APERAPL2          Sort length                                  
APEREMA  DS    CL52                Email of approver                            
APERAPT  DS    CL1                 Agency or purchasing approver                
APERDEF  DS    CL1                 Default approver                             
APETABL  EQU   *-APETABD                                                        
                                                                                
APRTABD  DSECT                                                                  
APRTSTAT DS    CL1                 Status byte 1                                
APRTORT1 EQU   X'80'               Order type - expense prod inter arts         
APRTORT2 EQU   X'40'               Order type - client or non client            
APRTOFFC EQU   X'20'               Office                                       
APRTDEPT EQU   X'10'               Department                                   
APRTETYP EQU   X'08'               Expenditure type                             
APRTCLI  EQU   X'04'               Client                                       
APRTPRO  EQU   X'02'               Product                                      
APRTJOB  EQU   X'01'               Job                                          
APRTSTA2 DS    XL1                 Status byte 2                                
APRTMED  EQU   X'80'               Media                                        
APRTSUP  EQU   X'40'               Supplier                                     
APRT2PA  EQU   X'20'               2P account                                   
APRT2PE  EQU   X'10'               1R person level                              
APRT2SD  EQU   X'08'               1R sub department level                      
APRT2DE  EQU   X'04'               1R department level                          
APRT2OF  EQU   X'02'               1R office level                              
APRTABL  EQU   *-APRTABD           length of entry                              
         EJECT                                                                  
PTRACED  DSECT                                                                  
PTTWHO   DS    CL6                                                              
         DS    CL2                                                              
PTTIO    DS    0C                                                               
PTTTIME  DS    CL(L'TTIMED)                                                     
         DS    CL1                                                              
PTHH     DS    CL2                                                              
PTCO1    DS    CL1                                                              
PTMM     DS    CL2                                                              
PTCO2    DS    CL1                                                              
PTSS     DS    CL2                                                              
         DS    CL2                                                              
PTTALPH  DS    CL(L'TALPHAID)                                                   
         DS    CL1                                                              
PTALPH   DS    CL(L'CUAALF)                                                     
         DS    CL2                                                              
PTTUSER  DS    CL(L'TUSERPID)                                                   
         DS    CL1                                                              
PTUSER   DS    CL(2*L'CUPASS)                                                   
         DS    CL2                                                              
PTFIELD  DS    CL10                                                             
         DS    CL1                                                              
PFIELD   DS    CL85                                                             
PFIELDOF DS    CL15                (overflow)                                   
         DS    CL1                                                              
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
         PRINT ON                                                               
                                                                                
PIDRECD  DSECT                                                                  
         ORG   PIDKORD+L'PIDKORD                                                
PIDKOR2  DS    XL8                                                              
                                                                                
OB_D     DSECT                                                                  
*                    SUPPLIER GAP OPTIMISATION                                  
OB_KEY   DS    XL42                Record key                                   
                                                                                
OB_ERROR DS    XL2                 Error number (Zero=OK)                       
                                                                                
OB_DATA  DS    0X                  Data starts here                             
OB_NAME  DS    CL(L'NAMEREC)       Record name                                  
OB_OTHER DS    0X                  Other data                                   
         ORG   OB_D+256                                                         
                                                                                
OB_DATAL EQU   *-OB_ERROR                                                       
OB_LNQ   EQU   *-OB_D                                                           
*                                                                               
OLISTD   DSECT                                                                  
OLISTCDE DS    CL(L'OFLPOFL)       OLIST CODE                                   
OLSTDLNQ EQU   *-OLISTD            LENGTH OF ENTRY                              
*                                                                               
* DDCONBLK                                                                      
         PRINT OFF                                                              
CONBLKD  DSECT                                                                  
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
                                                                                
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* DDDPRINTL                                                                     
         PRINT OFF                                                              
*PREFIX=P_                                                                      
       ++INCLUDE DDDPRINTL                                                      
*PREFIX=                                                                        
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025ACBRA2E   03/20/20'                                      
         END                                                                    
