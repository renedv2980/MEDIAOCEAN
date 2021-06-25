*          DATA SET NEMED05    AT LEVEL 014 AS OF 08/10/00                      
*PHASE T31E05A                                                                  
         TITLE 'T31E05 - EDIT FOR CALENDAR'                                     
T31E05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CAED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          USE W/S AREA 2 FOR ARGS TO PRINT             
         USING NDDEMBLK,R7                                                      
         ST    R7,NBADEM           1ST PART IS NET DEMO BLOCK                   
         USING CALD,R7                                                          
         EJECT                                                                  
*              EDIT FIELDS                                                      
         SPACE 3                                                                
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         SPACE 1                                                                
*                                  THESE FIELDS ARE REQUIRED                    
         MVI   FTERMFLG,0                                                       
         SPACE 1                                                                
         LA    R2,SPLCLIH          CLIENT                                       
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,SPLPROH          PRODUCT                                      
***      CLI   9(R2),C'='          (CHECK FOR GROUP)                            
****     BE    VGROUP                                                           
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
         B     VEST                                                             
         SPACE 1                                                                
VGROUP   NETGO NVGETFLD,DMCB       FOR PRODUCT GROUP                            
         MVC   NBSELPGR(1),FLD     REMOVE EQUAL SIGN  (=)                       
         MVC   NBSELPGR+1(4),FLD+2                                              
         SPACE 1                                                                
VEST     LA    R2,SPLESTH          ESTIMATE                                     
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         OI    SPLESTH+6,X'80'                                                  
         SPACE 1                                                                
         MVI   FTERMFLG,1          FOLLOWING ARE OPTIONAL                       
         SPACE 1                                                                
         LA    R2,SPLNETH          NETWORK                                      
         NETGO NVNETALL,DMCB                                                    
         SPACE 1                                                                
         LA    R2,SPLDPTH          DAYPART                                      
         NETGO NVDPTALL,DMCB,SPLDPTN                                            
         OI    SPLDPTNH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,SPLPAKH          PACKAGE                                      
         NETGO NVPAKLOK,DMCB,SPLPAKN                                            
         OI    SPLPAKNH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,SPLSTRTH         START DATE                                   
         NETGO NVSTRDAT,DMCB                                                    
         SPACE 1                                                                
         LA    R2,SPLENDH          END DATE                                     
         NETGO NVENDDAT,DMCB                                                    
         SPACE 1                                                                
         LA    R2,SPLDEMH          DEMO SELECTION                               
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
         SPACE 1                                                                
*                                                                               
*                                  DIG OUT ANY OPTIONS                          
         MVI   NOPRDOPT,0                                                       
         LA    R2,SPLOPTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT20                                                            
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDT20                                                            
OPT5     CLC   12(4,R3),=C'TIME'      PROGRAM TIMES                             
         BNE   OPT10                                                            
         MVI   TIMEOPT,C'Y'                                                     
         B     OPTX                                                             
*                                                                               
OPT10    CLC   12(4,R3),=C'NOPRD'     DON'T PRINT PROD NAMES/CODES              
         BNE   OPT15                                                            
         MVI   NOPRDOPT,C'N'                                                    
         B     OPTX                                                             
*                                                                               
OPT15    DS    0H                                                               
         B     EDINV                                                            
*                                                                               
*                                                                               
OPTX     LA    R3,32(R3)                                                        
         BCT   R0,OPT5                                                          
         SPACE 1                                                                
EDT20    LA    R2,SPLCLIH                                                       
         SPACE 1                                                                
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 1                                                                
XIT      XIT1                                                                   
*                                                                               
EDINV    MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
*              DSECTS ETC                                                       
         SPACE 3                                                                
         PRINT OFF                                                              
*              NETINCLS HERE                                                    
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF5D                                                       
         SPACE 1                                                                
CALD     DSECT                                                                  
*              NETDEMOD HERE                                                    
*              AND DEDBLOCK                                                     
         PRINT OFF                                                              
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
*                                                                               
TIMEOPT  DS    CL1     * BE CAREFUL - PASS TO PRINT MODULE                      
NOPRDOPT DS    CL1     * BE CAREFUL - PASS TO PRINT MODULE                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014NEMED05   08/10/00'                                      
         END                                                                    
