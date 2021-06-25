*          DATA SET NEWRI36    AT LEVEL 021 AS OF 05/01/02                      
*PHASE T32036A                                                                  
         TITLE 'T32036 - P5  EDIT MODULE'                                       
T32036   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**P5ED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4         ANETWS4 = WORKING STORAGE                     
         USING MYD,R7                                                           
         L     R6,ANETWS2                                                       
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
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
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         L     R3,ANETWS3                                                       
         NETGO NVCLIALL,DMCB,SPLCLIN,(R3)                                       
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
         CLC   SPLPRO(3),=C'ALL'                                                
         BNE   *+8                                                              
         MVI   PRDALL,C'Y'                                                      
         CLC   SPLPRO(3),=C'POL'                                                
         BNE   *+8                                                              
         MVI   POLFLG,C'Y'                                                      
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         CLC   SPLEST(3),=C'ALL'                                                
         BE    EDINV                                                            
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         MVI   NETALL,C'T'                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT7                                                             
         MVI   NETALL,0                                                         
         NETGO NVNETALL,DMCB                                                    
         CLC   SPLNET(3),=C'ALL'                                                
         BNE   *+8                                                              
         MVI   NETALL,C'Y'                                                      
*                                                                               
EDT7     LA    R2,SPLDPTH                DAYPART                                
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
         LA    R2,SPLDEMOH                                                      
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
*                                                                               
*                                  DIG OUT ANY OPTIONS                          
EDT14    LA    R2,SPLOPTH          OPTIONS                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT20                                                            
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDT20                                                            
OPT5     CLC   12(3,R3),=C'BOX'       NO BOXES                                  
         BNE   OPT7                                                             
         MVC   NOBOX,22(R3)                                                     
         B     OPTX                                                             
*                                                                               
OPT7     DS    0H                                                               
         CLC   12(3,R3),=C'ASS'       ASSIGNED COST                             
         BNE   OPT8                                                             
         MVI   ASSFLG,C'Y'                                                      
         B     OPTX                                                             
*                                                                               
OPT8     B     EDINV                                                            
         B     OPTX                                                             
*                                                                               
         SPACE 1                                                                
OPTX     LA    R3,32(R3)                                                        
         BCT   R0,OPT5                                                          
         SPACE 1                                                                
EDT20    LA    R2,SPLTITLH                                                      
         NETGO NVTITLE,DMCB                                                     
         BZ    EDT22                                                            
         MVI   CLTITLE,1                                                        
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
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
NOBOX    DS    CL1                                                              
ASSFLG   DS    CL1                                                              
POLFLG   DS    CL1                                                              
CLTITLE  DS    CL40                                                             
PRDALL   DS    CL1                                                              
NETALL   DS    CL1                                                              
ESTALL   DS    CL1                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE7D                                                       
       ++INCLUDE NEGENUNIT                                                      
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021NEWRI36   05/01/02'                                      
         END                                                                    
