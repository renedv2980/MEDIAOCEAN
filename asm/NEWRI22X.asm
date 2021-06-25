*          DATA SET NEWRI22X   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEWRI22    AT LEVEL 031 AS OF 08/10/00                      
*PHASE T32022X                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T32022 - N5  EDIT  PHASE'                                       
******************************************************                          
* THIS PROGRAM (THE N5) IS DISCONTINUED              *                          
* N5 SCREEN IS NEWRIE2X AND PRINT MODULE IS NEWRI23X *                          
******************************************************                          
T32022   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NWN5**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD        * ANETWS1 = OPEN WORK AREAS                    
         USING SPOOLD,R8                                                        
         L     R9,ASYSD          * ANETWS2 = FIXED WORK AREA                    
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2                                                       
         USING MYD,R7                                                           
         LR    RE,R7             * ANETWS3 = CLIST                              
         LA    RF,MYDLENE                                                       
         XCEF                                                                   
         L     R6,ANETWS4                                                       
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         ST    R2,RELO           * ANETWS4 = NDDEMBLK,DEDBLOCK                  
         SPACE 1                                                                
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   *+8                                                              
         BAS   RE,EDITMOD                                                       
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE 3                                                                
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   NBDATA,C'U'         UNIT RECORDS ONLY                            
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
         L     R3,NBAIO                                                         
         USING CLTHDR,R3                                                        
         L     RF,ANETWS3                                                       
         ST    RF,ACLIST                                                        
         LA    RE,CLIST                                                         
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
         L     R3,NBAIO                                                         
         USING ESTHDR,R3                                                        
         MVC   NDDEMOS,EDEMLST                                                  
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH                DAYPART                                
         NETGO NVDPTALL,DMCB                                                    
         OI    SPLDPTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPAKH                PACKAGE                                
         NETGO NVPAKLOK,DMCB                                                    
         OI    SPLPAKNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLSTRTH               START DATE                             
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDDH               END DATE                               
         NETGO NVENDDAT,DMCB                                                    
         SPACE                                                                  
*                                  DIG OUT ANY OPTIONS                          
*        LA    R2,SPLOPTH          OPTIONS                                      
         MVI   DEMONUM,1           1 DEMO DEFAULT                               
*        NETGO NVGETFLD,DMCB                                                    
*        BZ    EDT20                                                            
         B     EDT20                                                            
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
         SPACE 1                                                                
EDT16    CLC   12(4,R3),=C'DNUM'    NUMBER OF DEMOS                             
         BNE   EDT18                                                            
         TM    2(R3),X'80'          IS IT NUMERIC                               
         BZ    EDINV                                                            
         CLI   8(R3),4                                                          
         BH    EDINV                                                            
         CLI   8(R3),1                                                          
         BL    EDINV                                                            
         MVC   DEMONUM,8(R3)       SET BINARY NUM IN DEMONUM                    
         SPACE 1                                                                
EDT18    LA    R3,32(R3)                                                        
         BCT   R0,EDT16                                                         
         SPACE 1                                                                
EDT20    LA    R2,SPLDEMH                                                       
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
         BZ    EDT22                                                            
         MVC   DEMOSV,NDDEMOS      SAVE NEW DEMOS                               
*        ZIC   R1,NDNDEMOS **NEW N5 HAS ONLY ONE DEMO BEYOND TARGET             
*        BCTR  R1,0                                                             
*        STC   R1,DEMONUM          NUMBER OF DEMOS BEYOND TARGET                
         SPACE 1                                                                
EDT22    LA    R2,SPLTITLH                                                      
         MVC   NDTITLE,SPACES                                                   
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT24                                                            
         MVC   NDTITLE,FLD                                                      
         SPACE 1                                                                
EDT24    DS    0H                                                               
         LA    R4,UNIV                                                          
         NETGO NVUNIV,DMCB,(0,NDDEMBLK),(C'E',DBLOCK),0(R4)                     
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         PRINT GEN                                                              
EDERR    GOTO1 ERREX                                                            
         PRINT NOGEN                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
BINDMCB  DS    6F                                                               
RELO     DS    A                                                                
ACLIST   DS    A                                                                
ANETGOAL DS    A                                                                
ACURBIN  DS    A                   ADDRESS OF CURRENT BINREC                    
UNIV     DS    F                                                                
GOALIMP  DS    F                                                                
DEMOSV   DS    CL15                                                             
DEMONUM  DS    CL1          NUMBER OF DEMOS REQUESTED BEYOND TARGET             
FRST     DS    CL1                                                              
PEROPT   DS    CL1                                                              
PREVREC  DS    CL1                                                              
CURDPT   DS    CL1                                                              
CURMED   DS    CL1                                                              
CURPRD   DS    CL3                                                              
CUREST   DS    CL1                                                              
PERTYPE  DS    CL4                                                              
PRDCDSV  DS    CL3                                                              
DPTNAME  DS    CL8                                                              
MYWORK   DS    CL100                                                            
PRODNAME DS    CL20                                                             
EDIN     DS    CL4                                                              
EDOUT    DS    CL5                                                              
ACTCPM   DS    F                                                                
ESTCPM   DS    F                                                                
GOALCPM  DS    F                                                                
ACTIMP   DS    F                                                                
ESTIMP   DS    F                                                                
         DS    CL500               SPARE SEE REPORT MODULE                      
MYDLENE  EQU   *-MYD                                                            
*******                    ANY NEW FIELDS GO HERE                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE2D                                                       
       ++INCLUDE DRDICFILE                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEWRI22X  05/01/02'                                      
         END                                                                    
