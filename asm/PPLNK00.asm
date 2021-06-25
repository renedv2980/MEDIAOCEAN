*          DATA SET PPLNK00    AT LEVEL 017 AS OF 09/16/15                      
*PHASE T41400A                                                                  
PPLNK00  TITLE '- PRINT SYSTEM FALINK/DDLINK INTERFACE'                         
PPLNK00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**PL00**,RR=RE,CLEAR=YES                                   
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
                                                                                
         ST    RB,BASEADDR                                                      
         ST    RE,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,AFAPARM                                                       
                                                                                
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   APRTFACS,8(R1)                                                   
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
         OI    GIND1,GIONLINE      SET RUNNING ONLINE                           
                                                                                
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         GOTOR (RF),(R1),('O#ROU1',0),0,0                                       
         MVC   AROUT1,0(R1)        SET A(ROUTINE OVERLAY 1)                     
         GOTOR (RF),(R1),('O#ROU2',0),0,0                                       
         MVC   AROUT2,0(R1)        SET A(ROUTINE OVERLAY 2)                     
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
                                                                                
***********************************************************************         
* INITIALIZE CONTROL BLOCKS AND CALL DDLINK                           *         
***********************************************************************         
                                                                                
         SR    R2,R2                                                            
         ICM   R2,3,=AL2(LINKW-WORKD)                                           
         LA    R2,WORKD(R2)                                                     
         USING LINKW,R2                                                         
         USING RUNPARMD,DDRUNP     RUNPARMD EMULATION                           
         USING RUNFACSD,DDRUNF     RUNFACS EMULATION                            
         USING LP_D,DDLINKC        DDLINK CONTROL BLOCK                         
         USING FALINKD,FALINKC     FALINK CONTROL BLOCK                         
         USING WRKIOD,DDWRKIOC     WRKIO CONTROL BLOCK                          
                                                                                
         LA    R0,LNKINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R0,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
         MVC   FALASWCH,VSWITCH    A(SWITCH)                                    
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R0,FAMSGBLK         A(MESSAGE BLOCK)                             
         ST    R0,FALAMSG                                                       
         LA    R0,FACON            A(CONTROL FIELD BUFFER)                      
         ST    R0,FALACON                                                       
         L     R0,ATWA                                                          
         AHI   R0,SVFALINK-TWAD    A(FALINK SAVED STORAGE)                      
         ST    R0,FALASVE                                                       
         MVC   FALAPGS,=AL4(FALATMS)                                            
                                                                                
***********************************************************************         
* INITIALIZE ONLINE RUNNER VALUES (RUNFACSD)                          *         
***********************************************************************         
                                                                                
         LA    R0,TWAD             PASS A(4K SERVER SAVE AREA)                  
         AHI   R0,SVSERVER-TWAD                                                 
         STCM  R0,15,RSVRSAVE                                                   
         LA    R0,DDWRKIOC                                                      
         ST    R0,RWRKBLK1         SET A(WRKIO CONTROL BLOCK)                   
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RWRKIO,VWRKIO                                                    
         MVC   RRUNIT,VRUNIT                                                    
         LA    R0,RUNFACSD                                                      
         STCM  R0,7,RUNPARUN       SET A(RUNFACS) IN RUNPARMS                   
                                                                                
***********************************************************************         
* INITIALIZE DDLINK CONTROL BLOCK VALUES (LP_D)                       *         
***********************************************************************         
                                                                                
         MVC   LP_USRID,TWAUSRID   SET USER-ID                                  
         MVC   LP_ACCS,TWAACCS     SET LIMIT ACCESS BYTES                       
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
         MVI   LP_AIND1,LP_AICOM+LP_AIRLP+LP_AIUPL+LP_AISUD                     
         MVI   LP_FLAG,0           PATCHABLE FOR TESTING                        
         LA    R0,TWAD                                                          
         ST    R0,LP_ATWA          SET A(TWA)                                   
         AHI   R0,SVDDLINK-TWAD                                                 
         ST    R0,LP_ASAVE         SET A(DDLINK SAVE AREA)                      
         LA    R0,TWAD                                                          
         AHI   R0,SVSECRET-TWAD                                                 
         ST    R0,LP_ASECD         SET A(SECRET BLOCK)                          
         LA    R0,DDNDX                                                         
         STCM  R0,15,LP_ANDX       SET A(MASTER MAP INDEX)                      
         MVC   LP_ATIOB,ATIOB      SET A(TIOB)                                  
         MVC   LP_ACOM,ACOMFACS    SET A(COMFACS)                               
         LA    R0,DDLINKW                                                       
         STCM  R0,15,LP_AWORK      SET A(DDLINK WORK AREA)                      
         LA    R0,RUNPARMD                                                      
         ST    R0,LP_ARUNP         SET A(DUMMY RUNPARMS)                        
         LA    R0,FALINKD                                                       
         ST    R0,LP_AFBLK         SET A(FALINKD)                               
         MVC   LP_AFALK,VFALINK    SET A(FALINK)                                
         LA    R0,WORKD                                                         
         ST    R0,LP_ABLK1         PASS A(WORK AREA AS BLOCK 1)                 
         MVC   LP_ABLK2,RSVRSAVE   PASS A(SAVE AREA AS BLOCK 2)                 
                                                                                
***********************************************************************         
* INITIALIZE WRKIO CONTROL BLOCK VALUES (WRKIOD)                      *         
***********************************************************************         
                                                                                
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO7       IO7 USED FOR WORKER RECORD AREA              
         MVC   WRKIABUF,ATIA       PASS A(TIA) AS WORKER BUFFER                 
                                                                                
         GOTOR VDDLINK,LP_D        PASS CONTROL TO DDLINK                       
                                                                                
         OI    LNKSERVH+(FHAT-FHD),FHATMO                                       
         OI    LNKSERVH+(FHOI-FHD),FHOITR                                       
         OI    LNKINPH+(FHOI-FHD),FHOICU+FHOITR                                 
         XIT1  ,                                                                
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DDLINK MASTER MAP INDEX                                             *         
***********************************************************************         
                                                                                
DDNDX    LKMMI H,PRTSYSQ           ** MASTER MAP INDEX **                       
                                                                                
         LKMMI D,M#DLINI,O#DLINI,PP#DLINI              INITIAL DL               
         LKMMI D,M#DLOST,O#DLCPE,(*,OSTDLLIT)          OS Tool Ini DL           
         LKMMI D,M#DLCPE,O#DLCPE,PP#DLCPE,RUNNER=B     C/P/E DL                 
         LKMMI D,M#DLACL,O#DLCPE,(*,ACLDLLIT),RUNNER=B Accessible Clt           
         LKMMI D,M#DLBS#,O#DLCPE,(*,BS#VRLIT),RUNNER=B Buy Ser# Verif           
         LKMMI D,M#BYSTA,O#DLCPE,(*,BUYSTLIT),RUNNER=B Buy stat DL              
         LKMMI D,M#DLINS,O#DLINS,PP#DLBUY,RUNNER=B     INSERTION DL             
         LKMMI D,M#DLHST,O#DLHST,PP#DLHST              INS HISTORY DL           
         LKMMI D,M#DLWIZ,O#DLWIZ,PP#DLWIZ              DRAFT INS WIZARD         
         LKMMI D,M#DLVER,O#DLVER,PP#DLVER              C/P/E/PUB VERIF          
         LKMMI D,M#DLAVD,O#DLAVD,PP#DLAVD              ADCODE VAL DL            
         LKMMI D,M#DLCPG,O#DLCPG,PP#CPGRV              CGRP/PGRP VERIF          
         LKMMI D,M#DLREF,O#DLREF,PP#DLREF              REFRESH INS DL           
         LKMMI D,M#DLPO#,O#DLPO#,PP#PONUM              PO# RECORD DL            
         LKMMI US,M#ULDFT,O#ULDFT,PP#ULDFT             DRAFT INS UL             
         LKMMI US,M#ULNEW,O#ULNEW,PP#ULNEW,UPDATIVE=Y  LIVE INS UL              
         LKMMI US,M#ULIDK,O#ULIDK,PP#DKIUL,UPDATIVE=Y  IDESK INS UL             
         LKMMI US,M#ULCHA,O#ULCHA,PP#ULCHA,UPDATIVE=Y  CHANGE INS UL            
         LKMMI US,M#ULDEL,O#ULDEL,PP#ULDEL,UPDATIVE=Y  DELETE INS UL            
         LKMMI US,M#ULIOR,O#ULIOR,PP#ULIOR,UPDATIVE=Y  INSERTION ORDER          
         LKMMI D,M#DLINVN,O#DLINVN,PP#DLIVN,RUNNER=B   DL BY INV #              
         LKMMI D,M#DLINVK,O#DLINVK,PP#DLIVK,RUNNER=B   DL BY INV KEY            
         LKMMI D,M#DL_RNO,O#DL_RNO,PP#RNODL,RUNNER=B   RNO DOWNLOAD             
         LKMMI US,M#ULCLRP,O#ULCLP,PP#ULCLP,UPDATIVE=Y CLR FOR PAYMENT          
         LKMMI US,M#DLRPA,O#DLRPA,PP#DLRPA             REFRESH PAY INFO         
         LKMMI US,M#ULAIH,O#ULINV,PP#AINVH,UPDATIVE=Y  ADD INV HDR UL           
         LKMMI US,M#ULAII,O#ULINV,PP#AINVD,UPDATIVE=Y  ADD INV DET UL           
         LKMMI US,M#ULCIH,O#ULINV,PP#CINVH,UPDATIVE=Y  CHG INV HDR UL           
         LKMMI US,M#ULCII,O#ULINV,PP#CINVD,UPDATIVE=Y  CHG INV DET UL           
         LKMMI US,M#ULDIH,O#ULINV,PP#DINVH,UPDATIVE=Y  DEL INV HDR UL           
         LKMMI US,M#ULDII,O#ULINV,PP#DINVD,UPDATIVE=Y  DEL INV DET UL           
         LKMMI US,M#ULULK,O#ULINV,PP#ULKIV,UPDATIVE=Y  UNLINK INV & INS         
         LKMMI US,M#ULDMT,O#ULINV,PP#ULDMT,UPDATIVE=Y  DEL MATCHED ITEM         
         LKMMI US,M#ULNVHS,O#ULINV,PP#ULHST            INVOICE HISTORY          
         LKMMI D,M#DLPROF,O#DLPROF,PP#PRFDL            PROFILE DL               
         LKMMI US,M#ULIOR2,O#ULIO2,PP#ULIOR,UPDATIVE=Y INS ORDER 2 UL           
         LKMMI US,M#ULIOFX,O#ULWIO,PP#IOFAX,UPDATIVE=Y IO2 FAX UL               
         LKMMI US,M#IOFXLN,O#ULWIO,PP#IOFAX,UPDATIVE=Y IO2 FAX LINES UL         
         LKMMI US,M#ULIOIQ,O#ULWIO,PP#DLIOS            IO2 STAT INQUIRY         
         LKMMI US,M#ULIODL,O#ULWIO,PP#IODEL,UPDATIVE=Y IO2 DELETION             
         LKMMI US,M#ULIOST,O#ULWIO,PP#IOSTA            IO2 STATUS               
         LKMMI US,M#ULIODW,O#ULWIO,PP#IOSTA            IO2 STATUS               
         LKMMI D,M#SETRDL,O#DLSETR,PP#SCHMA,RUNNER=B   EIO SETUP REC DL         
         LKMMI US,M#UL_SR2,O#ULSR2,PP#ULESR,UPDATIVE=Y ESR UL                   
         LKMMI US,M#ULSRFX,O#ULESR,PP#SRFAX,UPDATIVE=Y ESR FAX UL               
         LKMMI US,M#SRFXLN,O#ULESR,PP#SRFAX,UPDATIVE=Y ESR FAX LINES UL         
         LKMMI US,M#ULSRIQ,O#ULESR,PP#DLSRS            ESR STAT INQUIRY         
         LKMMI US,M#ULSRDL,O#ULESR,PP#SRDEL,UPDATIVE=Y ESR DELETION             
         LKMMI US,M#ULSRST,O#ULESR,PP#SRSTA            ESR STATUS               
         LKMMI US,M#ULSRDW,O#ULESR,PP#SRSTA            ESR STATUS               
         LKMMI US,M#UL_ADF,O#ULADF,PP#ADCUL,UPDATIVE=Y AD CODE UL               
         LKMMI D,M#ADF_DL,O#DLADF,PP#ADCDL,RUNNER=B    AD CODE DL               
         LKMMI D,M#FXR_DL,O#DLFXR,PP#FXRDL             FX RATE DL               
         LKMMI D,M#CON_DL,O#DLCON,PP#CONDL,RUNNER=B    CONTRACT DL              
         LKMMI D,X'0350',X'10',(*,PUBDLLIT),RUNNER=B   Pub Rec DL               
         LKMMI U,M#UL_CLT,O#ULCFM,(*,ULCLTLIT),UPDATIVE=Y                       
         LKMMI U,M#UL_PRD,O#ULCFM,(*,ULPRDLIT),UPDATIVE=Y                       
         LKMMI D,M#DLCOM,O#DLCOM,(*,COMDLLIT),RUNNER=B Comment DL               
                                                                                
         LKMMI D,M#CFMMED,O#CFMMED,PP#CFMMD            MFM MED DL               
         LKMMI D,M#CFMCLT,O#CFMCLT,PP#CFMCL,RUNNER=B   MFM CLT DL               
         LKMMI D,M#CFMPRD,O#CFMPRD,PP#CFMPR,RUNNER=B   MFM PRD DL               
         LKMMI D,M#CFMPUB,O#CFMPUB,PP#CFMPU,RUNNER=B   MFM PUB DL               
         LKMMI D,M#STATSD,O#STATSD,PP#STATD,RUNNER=B   STATS DL                 
                                                                                
         LKMMI D,I#CFMIDL,O#ICFDL,(*,PCFMILIT)           CFM INITIAL DL         
         LKMMI D,I#CFMCDL,O#DLCFM,(*,PCFMCLIT),RUNNER=B  CFM DL                 
                                                                                
         LKMMI E                                                                
                                                                                
ULCLTLIT DC    C'CFM Client record upload'                                      
ULPRDLIT DC    C'CFM Product record upload'                                     
PCFMILIT DC    C'CFM Initial download'                                          
PCFMCLIT DC    C'CFM Client download'                                           
COMDLLIT DC    C'Comment download'                                              
PUBDLLIT DC    C'Pub record download'                                           
ACLDLLIT DC    C'Accessible client'                                             
BS#VRLIT DC    C'Buy Ser# Verifier'                                             
BUYSTLIT DC    C'Buy status download'                                           
OSTDLLIT DC    C'OS Tool Initial download'                                      
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE GEMAPEQUS                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017PPLNK00   09/16/15'                                      
         END                                                                    
