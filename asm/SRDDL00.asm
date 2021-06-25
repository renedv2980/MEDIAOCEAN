*          DATA SET SRDDL00    AT LEVEL 001 AS OF 05/01/08                      
*PHASE T1DD00A                                                                  
*INCLUDE TWABLD                                                                 
SRLNK00  TITLE '- Service System DDLINK Interface'                              
SRLNK00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKLON,**LK00**,RR=RE,CLEAR=YES                                 
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(S/R PARM LIST)                          
                                                                                
         ST    RB,BASEADDR                                                      
         ST    RE,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,AFAPARM                                                       
         L     RF,=V(TWABLD)                                                    
         AR    RF,RE                                                            
         ST    RF,VTWABLD                                                       
                                                                                
         MVC   ATIOB,SRQATIOB                                                   
         MVC   ATWA,SRQATWA                                                     
         MVC   ATIA,SRQATIA                                                     
         MVC   ACOMFACS,SRQACOMF                                                
                                                                                
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         USING RUNPARMD,DDRUNP     RUNPARMD EMULATION                           
         USING RUNFACSD,DDRUNF     RUNFACS EMULATION                            
         USING LP_D,DDLINKC        DDLINK CONTROL BLOCK                         
         USING FALINKD,FALINKC     FALINK CONTROL BLOCK                         
         USING WRKIOD,DDWRKIOC     WRKIO CONTROL BLOCK                          
                                                                                
         LA    R0,LNKACTNH         SET A(FIRST SCREEN POSITION)                 
         ST    R0,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
         L     RF,ACOMFACS                                                      
         MVC   CXTRAINF-COMFACSD(,RF),SRQAINFO                                  
         MVC   FALASWCH,CSWITCH-COMFACSD(RF)                                    
         MVC   VRUNIT,CRUNIT-COMFACSD(RF)                                       
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R0,FAMSGBLK         A(MESSAGE BLOCK)                             
         ST    R0,FALAMSG                                                       
         LA    R0,FACON            A(CONTROL FIELD BUFFER)                      
         ST    R0,FALACON                                                       
         LAY   R0,SVFALINK                                                      
         ST    R0,FALASVE                                                       
         MVC   FALAPGS,=AL4(FALATMS)                                            
                                                                                
         MVC   RSVRSAVE,ATIA       (FOR DDLINK USE)                             
         LA    R0,DDWRKIOC                                                      
         ST    R0,RWRKBLK1         SET A(WRKIO CONTROL BLOCK)                   
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RRUNIT,VRUNIT                                                    
         LA    R0,RUNFACSD                                                      
         STCM  R0,7,RUNPARUN       SET A(RUNFACS) IN RUNPARMS                   
                                                                                
         MVI   LP_AIND1,LP_AICOM+LP_AIUPL+LP_AIRLP                              
         MVC   LP_AGY,TWAAGY       SET AGENCY ALPHA CODE                        
         MVC   LP_USRID,TWAUSRID   SET USER-ID                                  
         MVC   LP_ACCS,TWAACCS     SET LIMIT ACCESS BYTES                       
         LA    R0,TWAD                                                          
         ST    R0,LP_ATWA          SET A(TWA)                                   
         LAY   R0,SVDDLINK                                                      
         ST    R0,LP_ASAVE         SET A(DDLINK SAVE AREA)                      
         LA    R0,DDNDX                                                         
         ST    R0,LP_ANDX          SET A(MASTER MAP INDEX)                      
         MVC   LP_ATIOB,ATIOB      SET A(TIOB)                                  
         MVC   LP_ACOM,ACOMFACS    SET A(COMFACS)                               
         LAY   R0,DDLINKW                                                       
         ST    R0,LP_AWORK         SET A(DDLINK WORK AREA)                      
         LAY   R0,WMP                                                           
         ST    R0,LP_AUWMP         SET A(WMP)                                   
         LHI   R0,L'WMP                                                         
         STH   R0,LP_WMPL                                                       
         LA    R0,RUNPARMD                                                      
         ST    R0,LP_ARUNP         SET A(DUMMY RUNPARMS)                        
         LA    R0,FALINKD                                                       
         ST    R0,LP_AFBLK         SET A(FALINKD)                               
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LHI   R0,QFALINK                                                       
         ICM   R0,B'1110',=X'D9000A'                                            
         GOTOR (RF),DMCB,0,(R0)                                                 
         MVC   LP_AFALK,0(R1)      SET A(FALINK)                                
                                                                                
         MVC   WRKIACOM,ACOMFACS                                                
         L     R0,ATIA                                                          
         AHI   R0,4096             WORKER BUFFER AT TIA+4K                      
         ST    R0,WRKIABUF                                                      
         LAY   R0,WRKIOA                                                        
         ST    R0,WRKIAREC                                                      
                                                                                
         MVI   LP_FLAG,0           FOR TESTING (PATCH HERE)                     
         LHI   R0,QDDLINK                                                       
         ICM   R0,B'1110',=X'D9000A'                                            
         GOTOR (RF),DMCB,0,(R0)                                                 
         L     RF,0(R1)                                                         
         GOTOR (RF),LP_D           PASS CONTROL TO DDLINK                       
         OI    LNKSERVH+1,X'01'                                                 
         OI    LNKSERVH+6,X'80'                                                 
         OI    LNKACTNH+6,X'C0'                                                 
         XIT1  ,                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
DDNDX    LKMMI H,SRVSYSQ           ** MASTER MAP INDEX **                       
                                                                                
         LKMMI E                                                                
                                                                                
SRVSYSQ  EQU   1                                                                
ONEK     EQU   1024                                                             
         EJECT                                                                  
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
                                                                                
BASERD   DS    A                                                                
BASEADDR DS    A                                                                
BASERELO DS    A                                                                
                                                                                
AFAPARM  DS    A                                                                
ATIOB    DS    A                                                                
ATWA     DS    A                                                                
ATIA     DS    A                                                                
ACOMFACS DS    A                   A(COMMON FACILITES LIST)                     
                                                                                
VTWABLD  DS    A                                                                
VRUNIT   DS    A                                                                
                                                                                
DMCB     DS    6F                                                               
                                                                                
***********************************************************************         
* DDLINK/FALINK REQUIRED STORAGE AREAS                                *         
***********************************************************************         
                                                                                
LINKW    DS    0F                                                               
DDWRKIOC DS    (WRKIOBL)X          WRKIO CONTROL BLOCK                          
DDRUNP   DS    (RUNPARML)X         RUNPARMS EMULATION                           
DDRUNF   DS    (RUNFACSL)X         RUNFACS EMULATION                            
FALINKC  DS    XL(FALINKDL)        FALINK CONTROL BLOCK                         
DDLINKC  DS    (LP_LNQ)X           DDLINK CONTROL BLOCK                         
DDLINKW  DS    XL(10*ONEK)         DDLINK WORK AREA                             
WMP      DS    XL(10*ONEK)         WORK MAP POOL                                
WRKIOA   DS    XL(10*ONEK)         WRKIO I/O AREA                               
                                                                                
WORKLON  EQU   *-WORKD             ONLINE WORKING STORAGE                       
                                                                                
       ++INCLUDE FATWA                                                          
       ++INCLUDE SRDDLFFD                                                       
         ORG   TWAD+2304                                                        
FAMSGBLK DS    CL(FAMSGDL)         BLOCK FOR FALINK ERROR MESSAGES              
FACON    DS    CL(L'FALCON)        BLOCK FOR FALINK CONTROL FIELD               
                                                                                
         DS    XL((TWANOGO-SVALUEL-8)-(*-TWAD))                                 
                                                                                
SVALUES  DS    0D                  ** SAVED STORAGE AREAS **                    
SVDDLINK DS    XL(LS_LNQ)          DDLINK SAVED STORAGE                         
SVFALINK DS    XL(2*ONEK)          FALINK SAVED STORAGE                         
SVSERVER DS    XL(9*ONEK)          SERVER SAVED STORAGE                         
SVALUEL  EQU   *-SVALUES                                                        
                                                                                
* OTHER INCLUDED BOOKS FOLLOW                                                   
         PRINT OFF                                                              
       ++INCLUDE FASRPARM                                                       
SRQAINFO DS    A                                                                
       ++INCLUDE FALINKBLK                                                      
       ++INCLUDE DDLINKD                                                        
       ++INCLUDE DDRUNNERD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
WRKIOD   DSECT                                                                  
       ++INCLUDE DDWRKIOD                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SRDDL00   05/01/08'                                      
         END                                                                    
