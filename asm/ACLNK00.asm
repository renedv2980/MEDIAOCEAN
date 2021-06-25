*          DATA SET ACLNK00    AT LEVEL 001 AS OF 12/02/09                      
*PHASE T61F00A                                                                  
ACLNK00  TITLE '- ACCOUNTING SYSTEM FALINK/DDLINK INTERFACE'                    
ACLNK00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**AL00**,RR=RE,CLEAR=YES                                   
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
                                                                                
         LAY   R2,LINKW                                                         
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
                                                                                
         MVI   LP_RLPSL,ACCLETQ    SET SYSTEM LETTER FOR RLP INTERFACE          
         MVC   LP_AGY,TWAAGY       SET AGENCY ALPHA CODE                        
         L     RF,AFAPARM                                                       
         MVC   LP_AGYB,0(RF)       SET COMPANY CODE                             
         MVC   LP_USRID,TWAUSRID   SET USER-ID                                  
         MVC   LP_ACCS,TWAACCS     SET LIMIT ACCESS BYTES                       
         MVC   LP_AUIR1,AROUTS1    SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUTS2    SET A(INDEX ROUTINES 2)                      
         MVI   LP_AIND1,LP_AICOM+LP_AIUPL+LP_AIRLP+LP_AISUD                     
         LA    R0,TWAD                                                          
         ST    R0,LP_ATWA          SET A(TWA)                                   
         AHI   R0,SVDDLINK-TWAD                                                 
         ST    R0,LP_ASAVE         SET A(DDLINK SAVE AREA)                      
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
         MVC   WRKIABUF,ATIA       PASS A(TIA) AS WORKER BUFFER                 
         L     R0,WRKIABUF                                                      
         LAY   R0,WRKRREC                                                       
         ST    R0,WRKIAREC         SET A(WORKER RECORD)                         
         GOTOR (#GETCPY,AGETCPY)   READ THE COMPANY RECORD                      
         MVC   LP_ACPYR,ACPYREC                                                 
         MVI   LP_FLAG,0           FOR TESTING (PATCH HERE)                     
         GOTOR VDDLINK,LP_D        PASS CONTROL TO DDLINK                       
         OI    LNKSERVH+1,X'01'                                                 
         OI    LNKSERVH+6,X'80'                                                 
         OI    LNKINPH+6,X'C0'                                                  
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACLNKWRKD                                                      
       ++INCLUDE GEMAPEQUS                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACLNK00   12/02/09'                                      
         END                                                                    
