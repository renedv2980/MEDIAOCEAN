*          DATA SET NEWRI92    AT LEVEL 034 AS OF 05/01/02                      
*PHASE T32092A,+0                                                               
*INCLUDE NUMVAL                                                                 
*                                                                               
*        TITLE 'T32092 - NETWORK PRICING REPORT'                                
************************************************************                    
*                                                                               
*                                                                               
*  ** EDIT MODULE                                                               
*                                                                               
*                                                                               
*************************************************************                   
         SPACE 2                                                                
T32092   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*NET92**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R7,ANETWS1                                                       
         USING MYWORKD,R7                                                       
*                                                                               
         L     R6,ANETWS4                                                       
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         ST    R2,RELO                                                          
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
         CLC   =C'@OJX',SPLCLI            IF SPECIAL REQUEST                    
         BNE   *+8                                                              
         BAS   RE,SETCLT                 SET CLIENT                             
                                                                                
         NETGO NVCLIALL,DMCB                                                    
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB                                                    
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTALL,DMCB,SPLESTN,NDDEMBLK                                   
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH                DAYPART                                
         NETGO NVDPTALL,DMCB,SPLDPTN                                            
         OI    SPLDPTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLSTRTH               START DATE                             
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDDH               END DATE                               
         NETGO NVENDDAT,DMCB                                                    
         SPACE                                                                  
*                                                                               
         LA    R2,SPLDEMH                     DEMOS                             
         CLI   5(R2),0                                                          
         BNE   OPTDEM                         NO DEMOS                          
         CLI   NBSELEST,0                     BUT IF EST = ALL                  
         BE    EDINV                          MUST HAVE DEMOS                   
OPTDEM   NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
         SPACE                                                                  
*                                                                               
OPTS     DS    0H                                                               
         MVI   DETOPT,1            GRP COMPREHENSIVE IS DEFAULT                 
         LA    R2,SPLOPTH                        OPTIONS                        
         CLI   5(R2),0                                                          
         BE    EDT10                                                            
         GOTO1 SCANNER,DMCB,(R2),(5,BLOCK),0                                    
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    EDT10                                                            
OPT2     CLC   12(4,R4),=C'IMPD'   DETAILED IMP REPORT?                         
         BNE   OPT4                                                             
         MVI   DETOPT,4            YES                                          
         B     OPTEND                                                           
OPT4     DS    0H                                                               
         CLC   12(3,R4),=C'IMP'    COMPREHENSIVE IMP REPORT?                    
         BNE   OPT5                                                             
         MVI   DETOPT,3            YES                                          
         B     OPTEND                                                           
OPT5     DS    0H                                                               
         CLC   12(4,R4),=C'GRPD'   DETAILED GRP REPORT?                         
         BNE   OPT6                                                             
         MVI   DETOPT,2            YES                                          
         B     OPTEND                                                           
OPT6     DS    0H                                                               
         CLC   12(3,R4),=C'GRP'    COMPREHENSIVE GRP?                           
         BNE   OPT7                                                             
         MVI   DETOPT,1            YES                                          
         B     OPTEND                                                           
OPT7     DS    0H                                                               
         B     EDINV                                                            
         SPACE                                                                  
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
*                                                                               
EDT10    DS    0H                                                               
         LA    R2,SPLTITLH                    TITLE                             
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDTX                                                             
         MVC   TITLE,FLD                                                        
         MVI   QTITLE,C'Y'                                                      
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
SETCLT   NTR1                          ONLY ALLOW OM/JW                         
         CLC   =C'OM',NBSELAGY         FOR CROSS AGY REQ                        
         BE    SETCLT5                                                          
         CLC   =C'JW',NBSELAGY                                                  
         BNE   EDERR                                                            
SETCLT5  MVC   SPLCLI(4),=C'ALL '                                               
         MVI   SPLCLIH+5,3                                                      
         MVI   SPLCLIH,11                                                       
         XIT1                                                                   
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
*                                                                               
EDERR    GOTO1 ERREX                                                            
*                                                                               
         EJECT                                                                  
               LTORG                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
MYWORKD  DSECT                                                                  
QTITLE   DS    CL1                 *** PASSED TO PRINT MODULE                   
TITLE    DS    CL40                ***                                          
DETOPT   DS    CL1                                                              
*                                                                               
RELO     DS    F                                                                
*                                                                               
WORKLENE EQU   *-QTITLE                                                         
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE5D                                                       
         PRINT OFF                                                              
DEMBLK   DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034NEWRI92   05/01/02'                                      
         END                                                                    
