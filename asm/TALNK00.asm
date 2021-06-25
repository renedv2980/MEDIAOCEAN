*          DATA SET TALNK00    AT LEVEL 001 AS OF 05/11/15                      
*PHASE T70400A                                                                  
TALNK00  TITLE '- TALENT SYSTEM DDLINK INTERFACE'                               
TALNK00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**TL00**,RR=RE,CLEAR=YES                                   
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
                                                                                
         ST    RB,BASEADDR                                                      
         ST    RE,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,AFAPARM                                                       
                                                                                
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
                                                                                
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         L     RF,ACOMFACS         LOAD SUPPORT ROUTINES                        
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         GOTOR (RF),(R1),('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)       SET A(ROUTINE OVERLAY 1)                     
         GOTOR (RF),(R1),('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)       SET A(ROUTINE OVERLAY 2)                     
                                                                                
         SR    R2,R2                                                            
         ICM   R2,3,=AL2(LINKW-WORKD)                                           
         LA    R2,WORKD(R2)                                                     
         USING LINKW,R2                                                         
         USING RUNPARMD,DDRUNP     RUNPARMD EMULATION                           
         USING RUNFACSD,DDRUNF     RUNFACS EMULATION                            
         USING LP_D,DDLINKC        DDLINK CONTROL BLOCK                         
         USING FALINKD,FALINKC     FALINK CONTROL BLOCK                         
         USING WRKIOD,DDWRKIOC     WRKIO CONTROL BLOCK                          
                                                                                
         LA    R0,LP_D             SET A(LP_D) IN GLOBAL W/S                    
         ST    R0,ALP                                                           
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
                                                                                
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
                                                                                
         MVC   RSVRSAVE,ATIA       (FOR DDLINK USE)                             
         LA    R0,DDWRKIOC                                                      
         ST    R0,RWRKBLK1         SET A(WRKIO CONTROL BLOCK)                   
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RWRKIO,VWRKIO                                                    
         MVC   RRUNIT,VRUNIT                                                    
         LA    R0,RUNFACSD                                                      
         STCM  R0,7,RUNPARUN       SET A(RUNFACS) IN RUNPARMS                   
                                                                                
         MVC   LP_AGY,TWAAGY       SET AGENCY ALPHA CODE                        
         MVC   LP_USRID,TWAUSRID   SET USER-ID                                  
         MVC   LP_ACCS,TWAACCS     SET LIMIT ACCESS BYTES                       
         MVC   LP_AUIR1,AROUTS1    SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUTS2    SET A(INDEX ROUTINES 2)                      
         MVI   LP_AIND1,LP_AICOM+LP_AIUPL                                       
         MVC   LP_RECL,=H'12000'                                                
         LA    R0,TWAD                                                          
         ST    R0,LP_ATWA          SET A(TWA)                                   
         AHI   R0,SVDDLINK-TWAD                                                 
         ST    R0,LP_ASAVE         SET A(DDLINK SAVE AREA)                      
         LA    R0,DDNDX                                                         
         STCM  R0,15,LP_ANDX       SET A(MASTER MAP INDEX)                      
         MVC   LP_ATIOB,ATIOB      SET A(TIOB)                                  
         MVC   LP_ACOM,ACOMFACS    SET A(COMFACS)                               
         LA    R0,LINKW                                                         
         AHI   R0,DDLINKW-LINKW                                                 
         STCM  R0,15,LP_AWORK      SET A(DDLINK WORK AREA)                      
         LA    R0,RUNPARMD                                                      
         ST    R0,LP_ARUNP         SET A(DUMMY RUNPARMS)                        
         LA    R0,FALINKD                                                       
         ST    R0,LP_AFBLK         SET A(FALINKD)                               
         MVC   LP_AFALK,VFALINK    SET A(FALINK)                                
         LA    R0,WORKD                                                         
         ST    R0,LP_ABLK1         PASS A(WORK AREA AS BLOCK 1)                 
         LA    R0,TWAD                                                          
         AHI   R0,SVSERVER-TWAD                                                 
         STCM  R0,15,LP_ABLK2      PASS A(SAVE AREA AS BLOCK 2)                 
                                                                                
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO7       IO7 USED FOR WORKER RECORD AREA              
         MVC   WRKIABUF,ATIA       PASS A(TIA) AS WORKER BUFFER                 
                                                                                
         MVC   DATADISP,=AL2(TLRCELEM-TLRCD)                                    
                                                                                
         MVI   LP_FLAG,0           FOR TESTING (PATCH HERE)                     
         GOTOR VDDLINK,LP_D        PASS CONTROL TO DDLINK                       
         OI    LNKSERVH+1,X'01'                                                 
         OI    LNKSERVH+6,X'80'                                                 
         OI    LNKINPH+6,X'C0'                                                  
         XIT1  ,                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
DDNDX    LKMMI H,TALSYSQ           ** MASTER MAP INDEX **                       
                                                                                
         LKMMI D,I#INIDLD,P#LNK10,TA#DLINI     INITIAL DOWNLOAD                 
         LKMMI D,I#IN2DLD,P#LNK11,TA#DLIN2     INITIAL DOWNLOAD 2               
         LKMMI D,I#CTYDLD,P#LNK11,TA#DLCTY     COMMERCIAL TYPE DOWNLOAD         
         LKMMI D,I#COMDLD,P#LNK10,TA#DLCOM     COMMERCIAL DOWNLOAD              
         LKMMI D,I#STVDLD,P#LNK11,TA#DLSTV     STAFF VALID. DOWNLOAD            
         LKMMI D,I#STFDLD,P#LNK11,TA#DLSTF     STAFF DOWNLOAD                   
         LKMMI US,I#PAYULD,P#LNK12,TA#ULPAY,UPDATIVE=Y   PAY UPLOAD             
         LKMMI US,I#PJVULD,P#LNK19,TA#ULPJV              PO/JOB UPLOAD          
         LKMMI D,I#RELDLD,P#LNK11,TA#DLREL,UPDATIVE=Y    REL DOWNLOAD           
         LKMMI U,I#IRCULD,P#LNK1A,TA#ULIRC,UPDATIVE=Y    INV REO/CAN            
         LKMMI U,I#CVULD,P#LNK1F,TA#ULCVY,UPDATIVE=Y     COMML VERIFY           
         LKMMI U,I#CCLULD,P#LNK20,TA#ULCCL,UPDATIVE=Y    CMML COMP LOCK         
         LKMMI D,I#ANGDLD,P#LNK14,TA#DLANG,UPDATIVE=Y    ANG DOWNLOAD           
         LKMMI D,I#USEDLD,P#LNK13,TA#DLUSE               USE SEARCH             
         LKMMI D,I#W4SDLD,P#LNK11,TA#DLW4S               W4 SEARCH              
         LKMMI U,I#W4ULD,P#LNK15,TA#ULW4,UPDATIVE=Y      W4 UPLOAD              
         LKMMI D,I#ANSDLD,P#LNK11,TA#DLANS               AGENT SEARCH           
         LKMMI D,I#COSDLD,P#LNK13,TA#DLCOS               COMML SEARCH           
         LKMMI U,I#COULD,P#LNK16,TA#ULCO,UPDATIVE=Y      COMML UPLOAD           
         LKMMI U,I#VRULD,P#LNK18,TA#ULVR,UPDATIVE=Y      VERSN UPLOAD           
         LKMMI U,I#VRDULD,P#LNK18,TA#ULVD,UPDATIVE=Y     VERSN DELETE           
         LKMMI D,I#CASDLD,P#LNK11,TA#DLCAS               CAST SEARCH            
         LKMMI U,I#CAULD,P#LNK17,TA#ULCA,UPDATIVE=Y      CAST UPLOAD            
         LKMMI U,I#CADULD,P#LNK17,TA#ULCAD,UPDATIVE=Y    CAST DELETE            
         LKMMI D,I#GUSDLD,P#LNK13,TA#DLGUS               GRT SEARCH             
         LKMMI U,I#GUULD,P#LNK1B,TA#ULGU,UPDATIVE=Y      GRT UPLOAD             
         LKMMI D,I#INSDLD,P#LNK11,TA#DLINS               INV SEARCH             
         LKMMI D,I#FCSDLD,P#LNK11,TA#DLFCY               FX CY SEARCH           
         LKMMI U,I#FCULD,P#LNK1D,TA#ULFC,UPDATIVE=Y      FX CY UPLOAD           
         LKMMI D,I#FTSDLD,P#LNK11,TA#DLFCT               FX CY TRK SRCH         
         LKMMI D,I#CMSDLD,P#LNK11,TA#DLCMT               COMMENT SEARCH         
         LKMMI U,I#CMULD,P#LNK1C,TA#ULCM,UPDATIVE=Y      COMMENT UPLOAD         
         LKMMI D,I#NISDLD,P#LNK13,TA#DLINM               NMD INT SEARCH         
         LKMMI U,I#NIULD,P#LNK1E,TA#ULINM,UPDATIVE=Y     NMD INT UPLOAD         
         LKMMI D,I#PYSDLD,P#LNK21,TA#DLPYS               PAY SIMULATOR          
         LKMMI US,I#TMULD,P#LNK22,TA#ULTM,UPDATIVE=Y     TIME UPLOAD            
                                                                                
         LKMMI E                                                                
                                                                                
* TALNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TALNK00   05/11/15'                                      
         END                                                                    
