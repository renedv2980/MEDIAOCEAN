*          DATA SET TAGENB0    AT LEVEL 002 AS OF 05/01/02                      
*PHASE T702B0A                                                                  
         TITLE 'T702B0 - AREA RECORD MAINTENANCE'                               
T702B0   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702B0                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         SPACE 3                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   MAIN10                                                           
         GOTO1 RECVAL,DMCB,TLARCDQ,(X'40',SARAREAH) BUILD THE KEY               
         B     MAINX                                                            
         SPACE 3                                                                
MAIN10   CLI   MODE,DISPREC        IF DISPLAY RECORD                            
         BE    MAIN15                                                           
         CLI   MODE,XRECADD        OR RECORD ADDED                              
         BE    MAIN15                                                           
         CLI   MODE,XRECDEL        OR DELETED                                   
         BE    MAIN15                                                           
         CLI   MODE,XRECREST       OR RESTORED                                  
         BE    MAIN15                                                           
         CLI   MODE,XRECPUT        OR CHANGED                                   
         BNE   MAIN20                                                           
MAIN15   BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     MAINX                                                            
         SPACE 3                                                                
MAIN20   CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   MAIN30                                                           
         BAS   RE,BLDREC           BUILD IT                                     
         B     MAINX                                                            
         SPACE 3                                                                
MAIN30   CLI   MODE,DISPKEY        DISPLAY KEY FOR SELECT                       
         BNE   MAINX                                                            
         L     R3,AIO                                                           
         USING TLARD,R3                                                         
         MVC   SARAREA,TLARAREA    AREA CODE                                    
         OI    SARAREAH+6,X'80'    TRANSMIT                                     
MAINX    B     XIT                                                              
         SPACE 3                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         GOTO1 NAMIN,DMCB,TANAELQ,SARARENH    AREA NAME                         
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0                  LAST CHANGED                      
         B     XIT                                                              
         SPACE 3                                                                
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TANAELQ,SARARENH  AREA NAME                         
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,SARLCHGH          LAST CHANGED                      
         B     XIT                                                              
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'AREA',CL8'LIST'                                       
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRB0D                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAGENB0   05/01/02'                                      
         END                                                                    
