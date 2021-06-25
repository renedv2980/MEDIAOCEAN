*          DATA SET NEWRI24    AT LEVEL 012 AS OF 05/01/02                      
*PHASE T32024A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T32024 - N6  EDIT  PHASE'                                       
T32024   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**N6ED**,RR=R2                                                 
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
         L     R1,ANETWS4                                                       
         A     R1,=F'1500'         1500 SHOULD BE ENOUGH FOR DEMBLK             
         ST    R1,NDPRGBUF         NEED 440 BYTES FOR PRDGRPS                   
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
         CLI   NBSELPGR,0                                                       
         BE    ESTIM                                                            
*                                                                               
ESTIM    LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
         MVC   NDDEMOS(3),=X'00D901'    FORCE TO RHOMES                         
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
         LA    R2,SPLOPTH          OPTIONS                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT20                                                            
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
         SPACE 1                                                                
EDT16    CLC   12(4,R3),=C'DOWN'   OPTION TO DOWNLOAD                           
         BNE   EDT18                                                            
         SPACE 1                                                                
EDT18    LA    R3,32(R3)                                                        
         BCT   R0,EDT16                                                         
         SPACE 1                                                                
EDT20    LA    R2,SPLDEMOH                  DEMO OVERRIDE                       
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
         SPACE 1                                                                
EDT22    LA    R2,SPLTITLH                                                      
         MVC   NDTITLE,SPACES                                                   
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT30                                                            
         MVC   NDTITLE,FLD                                                      
         SPACE 1                                                                
EDT30    DS    0H                                                               
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
RELO     DS    A                                                                
ACLIST   DS    F                                                                
UNIV     DS    F                                                                
MYDLENE  EQU   *-MYD                                                            
*******                    ANY NEW FIELDS GO HERE                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE3D                                                       
       ++INCLUDE DRDICFILE                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NEWRI24   05/01/02'                                      
         END                                                                    
