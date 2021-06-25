*          DATA SET NEWRI35    AT LEVEL 011 AS OF 05/01/02                      
*PHASE T32035A,+0                                                               
         TITLE 'T32035 - N8  MISS/MG REPORT'                                    
T32035   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**N8ED**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4         ANETWS4 = WORKING STORAGE                     
         USING MYD,R7                                                           
         ST    R2,RELO                                                          
         SPACE 1                                                                
RP2      CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
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
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH                DAYPART                                
         NETGO NVDPT,DMCB                                                       
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
         EJECT                                                                  
*                                  DIG OUT ANY OPTIONS                          
EDT14    LA    R2,SPLOPTH          OPTIONS                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT20                                                            
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDT20                                                            
OPT5     CLC   12(3,R3),=C'BOX'       SEPERATE PAGE PER PRODUCT                 
         BNE   OPT7                                                             
         MVC   NOBOX,22(R3)                                                     
         B     OPTX                                                             
*                                                                               
OPT7     DS    0H                                                               
         CLC   12(5,R3),=C'PCODE'  USE ONLY PROD NAME                           
         BNE   OPT9                                                             
         CLI   22(R3),C'N'                                                      
         BNE   EDINV                                                            
         MVI   PRDCODE,C'N'                                                     
         B     OPTX                                                             
OPT9     DS    0H                                                               
         MVI   ACT$,0                                                           
         CLC   12(3,R3),=C'ACT'    GIVE ACTUAL $                                
         BNE   EDINV                                                            
         MVI   ACT$,C'Y'                                                        
         B     OPTX                                                             
*                                                                               
         SPACE 1                                                                
OPTX     LA    R3,32(R3)                                                        
         BCT   R0,OPT5                                                          
         SPACE 1                                                                
EDT20    LA    R2,SPLTITLH                                                      
         MVC   NDTITLE,SPACES                                                   
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT22                                                            
         MVC   NDTITLE,FLD                                                      
         SPACE 1                                                                
EDT22    LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
*                                                                               
EDERR    GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
NOBOX    DS    CL1                                                              
PRDCODE  DS    CL1                                                              
ACT$     DS    CL1                                                              
RELO     DS    A                                                                
LINENUM  DS    F                                                                
ATABLE   DS    F                                                                
NUMMONS  DS    F                                                                
PREVNO   DS    F                                                                
PERTYPE  DS    CL1                                                              
PEROPT   DS    CL1                                                              
NEWLINE  DS    CL1                                                              
MONLIST  DS    CL20                                                             
NTWKSV   DS    CL4                                                              
NEWNET   DS    CL4                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE5D                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011NEWRI35   05/01/02'                                      
         END                                                                    
