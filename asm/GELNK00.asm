*          DATA SET GELNK00    AT LEVEL 001 AS OF 11/16/09                      
*PHASE TA0600A                                                                  
GELNK00  TITLE '- Control system FALINK/DDLINK interface'                       
GELNK00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**GL00**,RR=RE,CLEAR=YES                                   
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(global w/s)                             
                                                                                
         ST    RB,BASEADDR                                                      
         ST    RE,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,AFAPARM                                                       
                                                                                
         MVC   ATIA,4(R1)                                                       
         MVC   ACOMFACS,12(R1)                                                  
         MVC   ATWA,20(R1)                                                      
         MVC   ATIOB,28(R1)                                                     
                                                                                
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         L     RF,ACOMFACS         Load support routines                        
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         GOTOR (RF),(R1),('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)       Set A(routine overlay 1)                     
         GOTOR (RF),(R1),('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)       SET A(routine overlay 2)                     
                                                                                
         SR    R2,R2                                                            
         ICM   R2,3,=AL2(LINKW-WORKD)                                           
         LA    R2,WORKD(R2)                                                     
         USING LINKW,R2                                                         
         USING RUNPARMD,DDRUNP     RUNPARMD emulation                           
         USING RUNFACSD,DDRUNF     RUNFACS emulation                            
         USING LP_D,DDLINKC        DDLINK control block                         
         USING FALINKD,FALINKC     FALINK control block                         
         USING WRKIOD,DDWRKIOC     WRKIO control block                          
                                                                                
         LA    R0,LP_D             Set A(LP_D) in global w/s                    
         ST    R0,ALP                                                           
         GOTOR (#WRKINI,AWRKINI)   Initialize working storage                   
                                                                                
         LA    R0,LNKINPH          Set A(first screen position)                 
         ST    R0,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
         MVC   FALASWCH,VSWITCH    A(SWITCH)                                    
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R0,FAMSGBLK         A(message block)                             
         ST    R0,FALAMSG                                                       
         LA    R0,FACON            A(control field buffer)                      
         ST    R0,FALACON                                                       
         L     R0,ATWA                                                          
         AHI   R0,SVFALINK-TWAD    A(FALINK saved storage)                      
         ST    R0,FALASVE                                                       
         MVC   FALAPGS,=AL4(FALATMS)                                            
                                                                                
         MVC   RSVRSAVE,ATIA       (for DDLINK use)                             
         LA    R0,DDWRKIOC                                                      
         ST    R0,RWRKBLK1         Set A(WRKIO control block)                   
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RRUNIT,VRUNIT                                                    
         LA    R0,RUNFACSD                                                      
         STCM  R0,7,RUNPARUN       Set A(RUNFACS) in RUNPARMS                   
                                                                                
         MVC   LP_AGY,TWAAGY       Set agency alpha id                          
         MVC   LP_USRID,TWAUSRID   Set user-id                                  
         MVC   LP_ACCS,TWAACCS     Set limit access control bytes               
         MVC   LP_AUIR1,AROUTS1    Set A(index routines 1)                      
         MVC   LP_AUIR2,AROUTS2    Set A(index routines 2)                      
         MVI   LP_AIND1,LP_AICOM   Set comma valid as delimiter                 
         LA    R0,TWAD                                                          
         ST    R0,LP_ATWA          Set A(TWA)                                   
         AHI   R0,SVDDLINK-TWAD                                                 
         ST    R0,LP_ASAVE         Set A(DDLINK save area)                      
         LA    R0,TWAD                                                          
         AHI   R0,SVSECRET-TWAD                                                 
         ST    R0,LP_ASECD         Set A(SECRET block)                          
         LA    R0,DDNDX                                                         
         STCM  R0,15,LP_ANDX       Set A(master map index)                      
         MVC   LP_ATIOB,ATIOB      Set A(TIOB)                                  
         MVC   LP_ACOM,ACOMFACS    Set A(COMFACS)                               
         LA    R0,LINKW                                                         
         AHI   R0,DDLINKW-LINKW                                                 
         STCM  R0,15,LP_AWORK      Set A(DDLINK work area)                      
         LA    R0,RUNPARMD                                                      
         ST    R0,LP_ARUNP         Set A(dummy RUNPARMS)                        
         LA    R0,FALINKD                                                       
         ST    R0,LP_AFBLK         Set A(FALINKD)                               
         MVC   LP_AFALK,VFALINK    Set A(FALINK)                                
         LA    R0,WORKD                                                         
         ST    R0,LP_ABLK1         Pass A(work area as block 1)                 
         LA    R0,TWAD                                                          
         AHI   R0,SVSERVER-TWAD                                                 
         STCM  R0,15,LP_ABLK2      Pass A(save area as block 2)                 
                                                                                
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO7       IO7 used for worker record area              
         MVC   WRKIABUF,ATIA       Pass A(TIA) as worker buffer                 
                                                                                
         MVI   LP_FLAG,0           For testing                                  
         GOTOR VDDLINK,LP_D        Pass control to DDLINK                       
         OI    LNKSERVH+1,X'01'                                                 
         OI    LNKSERVH+6,X'80'                                                 
         OI    LNKINPH+6,X'C0'                                                  
         XIT1  ,                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
DDNDX    LKMMI H,CTLSYSQ           ** Master map index **                       
                                                                                
         LKMMI D,I#CFMIDL,P#CFMIDL,(*,GELCFMID)                                 
         LKMMI D,I#CFMCDL,P#CFMCDL,(*,GELCFMCD),RUNNER=Y                        
         LKMMI D,I#MFMIDL,P#MFMIDL,(*,GELMFMID),RUNNER=Y                        
         LKMMI U,I#MFMMLN,P#MFMMLN,(*,GELMFMLN)                                 
         LKMMI U,I#MFMMSR,P#MFMMSR,(*,GELMFMSR)                                 
         LKMMI U,I#MFMMMP,P#MFMMMP,(*,GELMFMMR)                                 
         LKMMI U,I#MFMMCP,P#MFMMCP,(*,GELMFMCP)                                 
         LKMMI U,I#MFMMBP,P#MFMMBP,(*,GELMFMBP)                                 
         LKMMI U,I#MFMMVP,P#MFMMVP,(*,GELMFMSP)                                 
         LKMMI D,I#MFMSDL,P#MFMSDL,(*,GELMFMSD),RUNNER=Y                        
         LKMMI D,I#MFMCLT,P#MFMCLT,(*,GELMFMCD),RUNNER=Y                        
         LKMMI D,I#MFMBRD,P#MFMBRD,(*,GELMFMBD),RUNNER=Y                        
         LKMMI D,I#MFMVEN,P#MFMVEN,(*,GELMFMXD),RUNNER=Y                        
         LKMMI D,I#MFMMKT,P#MFMMKT,(*,GELMFMMD),RUNNER=Y                        
         LKMMI D,I#MVANDL,P#MVANDL,(*,GELMVAND),RUNNER=Y                        
                                                                                
         LKMMI E                                                                
                                                                                
GELCFMID DC    C'CFM Initial download'                                          
GELCFMCD DC    C'CFM Client download'                                           
GELMFMID DC    C'MFM Initial download'                                          
GELMFMLN DC    C'MFM Maintain Level names'                                      
GELMFMSR DC    C'MFM Maintain Structure records'                                
GELMFMMR DC    C'MFM Maintain Media records'                                    
GELMFMCP DC    C'MFM Maintain Client pointers'                                  
GELMFMBP DC    C'MFM Maintain Brand pointers'                                   
GELMFMSP DC    C'MFM Maintain Supplier pointers'                                
GELMFMSD DC    C'MFM Structure download'                                        
GELMFMCD DC    C'MFM Client download'                                           
GELMFMBD DC    C'MFM Brand download'                                            
GELMFMXD DC    C'MFM Supplier download'                                         
GELMFMMD DC    C'MFM Spot Market download'                                      
GELMVAND DC    C'MediaVantage download'                                         
                                                                                
* GELNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE GELNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001GELNK00   11/16/09'                                      
         END                                                                    
