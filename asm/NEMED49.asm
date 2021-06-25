*          DATA SET NEMED49    AT LEVEL 013 AS OF 08/10/00                      
*PHASE T31E49A                                                                  
         TITLE 'T31E49 - EDIT FOR NETWORK ESTIMATE'                             
T31E49   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ESED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          PASS ARGS TO PRINT IS W/S AREA 2             
         A     R7,=F'500'                                                       
         USING ESTD,R7                                                          
         EJECT                                                                  
*        INITIALIZE                                                             
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         MVI   NBDATA,C'P'         WILL WANT PACKAGE RECORD FIRST               
         L     R2,ANETWS1          CLIENT RECORD PASSED IN W/S AREA 1           
         ST    R2,NBACLI                                                        
*                                                                               
*              EDIT FIELDS                                                      
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         LA    R2,SPLCLIH          CLIENT. REQUIRED.                            
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         LA    R2,SPLPROH          PRODUCT. REQUIRED.                           
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'    TRANSMIT PRODUCT NAME.                       
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
         LA    R2,SPLESTH          ESTIMATE. OPTIONAL                           
         NETGO NVEST,DMCB,SPLESTN                                               
         OI    SPLESTNH+6,X'80'    XMIT ESTIMATE NAME                           
*                                                                               
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN   DAYPART. OPTIONAL.                          
         OI    SPLDPTNH+6,X'80'    TRANSMIT DAYPART NAME                        
*                                                                               
         LA    R2,SPLPAKH          PACKAGE ALLOW LOCK/BOTH/F=                   
         NETGO NVPAKLOK,DMCB,SPLPAKN                                            
*                                                                               
         OI    SPLPAKNH+6,X'80'    TRANSMIT PACKAGE NAME                        
*                                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLFILTH                                                      
         NETGO NVFILT,DMCB                                                      
*                                                                               
ED8      LA    R2,SPLFORMH                                                      
         MVI   MENU,0                                                           
         CLI   5(R2),0                                                          
         BE    ED10                                                             
         GOTO1 ANY                                                              
         GOTO1 VALINUM                                                          
         MVC   MENU,ACTUAL                                                      
         CLI   MENU,12                                                          
         BH    EDERR                                                            
         SPACE 2                                                                
ED10     MVI   REQSPAC,1                                                        
         LA    R2,SPLSPACH                                                      
         CLI   5(R2),0                                                          
         BE    ED12                                                             
         GOTO1 ANY                                                              
         GOTO1 VALINUM                                                          
         MVC   REQSPAC,ACTUAL                                                   
         CLI   REQSPAC,3                                                        
         BH    EDERR                                                            
         SPACE 2                                                                
ED12     LA    R2,SPLCLIH                                                       
         B     XMOD                                                             
*                                                                               
EDERR    MVI   ERROR,INVALID                                                    
         GOTO1 ERREX,DMCB                                                       
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE9D                                                       
*                                                                               
**** COMMON WITH PRINT                                                          
ESTD     DSECT                                                                  
*                                                                               
*** ARGS TO PRINT                                                               
*                                                                               
DPFILT   DS    CL1                                                              
MENU     DS    CL1                                                              
REQSPAC  DS    CL1                                                              
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013NEMED49   08/10/00'                                      
         END                                                                    
