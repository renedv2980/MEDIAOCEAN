*          DATA SET RELNK00    AT LEVEL 002 AS OF 05/02/14                      
*PHASE T82B00A                                                                  
RELNK00  TITLE '- REP SYSTEM FALINK/DDLINK INTERFACE'                           
RELNK00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**RL00**,RR=RE,CLEAR=YES                                   
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
                                                                                
         ST    RB,BASEADDR                                                      
         ST    RE,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,AFAPARM                                                       
                                                                                
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   AREPFACS,8(R1)                                                   
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
                                                                                
*                                                                               
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
*                                                                               
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
*                                                                               
         GOTOR CHKGLOBB                                                         
*                                                                               
         GOTOR VDDLINK,LP_D        PASS CONTROL TO DDLINK                       
                                                                                
         OI    LNKSERVH+(FHAT-FHD),FHATMO                                       
         OI    LNKSERVH+(FHOI-FHD),FHOITR                                       
         OI    LNKINPH+(FHOI-FHD),FHOICU+FHOITR                                 
         J     EXIT                                                             
                                                                                
EXITN    LHI   RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1  ,                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DDLINK MASTER MAP INDEX                                             *         
***********************************************************************         
                                                                                
DDNDX    LKMMI H,REPSYSQ           ** MASTER MAP INDEX **                       
                                                                                
         LKMMI D,Q#DLINI,O#DLINI,(*,STXIDLIT)              STX Init DL          
         LKMMI D,Q#DLCON,O#DLCON,(*,RCONDLIT)              Contract DL          
         LKMMI D,Q#DLPCO,O#DLCON,(*,PENDCLIT)              Pend Con DL          
         LKMMI U,Q#UPORD,O#UPORD,(*,UPORDLIT),UPDATIVE=Y   Upd Order            
         LKMMI D,Q#DLMKG,O#DLMKG,(*,ROGRPLIT)              Makegood DL          
         LKMMI U,Q#UPOFR,O#UPOFR,(*,UOGRPLIT),UPDATIVE=Y   Upd Offer            
         LKMMI D,Q#AUDFR,O#DLAUD,(*,AUDRFLIT)              Audit DL             
         LKMMI U,Q#UPBUY,O#UPBUY,(*,UPBUYLIT),UPDATIVE=Y   Upd Buy              
                                                                                
         LKMMI E                                                                
                                                                                
STXIDLIT DC    C'Station Xpress Initial Download'                               
RCONDLIT DC    C'Contract/Buy Download'                                         
PENDCLIT DC    C'Pending Contract Download'                                     
UPORDLIT DC    C'Update Order Request'                                          
ROGRPLIT DC    C'Refresh Offer Group Request'                                   
UOGRPLIT DC    C'Update Offer Group Request'                                    
AUDRFLIT DC    C'Audit Data Refresh Request'                                    
UPBUYLIT DC    C'Update Buy Request'                                            
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
CHKGLOBB NTR1  BASE=*,LABEL=*      Check Globber control object                 
*                                                                               
         GOTOR VGLOBBER,DMCB,=C'GETD',ELEM2,24,GLVXCTL                          
         TM    DMCB+8,X'10'                                                     
         JNZ   EXITN                                                            
         GOTOR VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,ELEM2                                                         
         USING GLVXFRSY,R1                                                      
         TM    GLVXFLG1,GLV1RETN   Is this a return call?                       
         JZ    EXITN                                                            
         CLC   GLVXFRSY,=C'REP'    From Rep system?                             
         JNE   EXITN                                                            
         CLC   GLVXFRPR,=C'CON'    From Rep's Contract program?                 
         JNE   EXITN                                                            
         DROP  R1                                                               
*                                                                               
         GOTOR VGLOBBER,DMCB,=C'GETD',ELEM2,GLRLINLQ,GLRKLINC                   
         TM    DMCB+8,X'10'                                                     
         JZ    CHKGL50                                                          
         MVI   LNKINPH+5,8         Issue a dummy offer update request           
         MVC   LNKINP(8),=C'U O B=70'                                           
         J     EXITY                                                            
*                                                                               
CHKGL50  GOTOR VGLOBBER,DMCB,=C'DELE',,,GLRKLINC                                
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,ELEM2                                                         
         USING GLRLINCD,R1                                                      
         MVC   LNKINPH+5(1),GLRLINLN                                            
         MVC   LNKINP,GLRLINFD                                                  
         DROP  R1                                                               
*                                                                               
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP  R2                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE RELNKWRK                                                       
         PRINT ON                                                               
*                                                                               
                                                                                
       PRINT OFF                                                                
LIOBD    DSECT ,                                                                
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002RELNK00   05/02/14'                                      
         END                                                                    
