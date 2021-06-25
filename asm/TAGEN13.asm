*          DATA SET TAGEN13    AT LEVEL 034 AS OF 05/01/02                      
*PHASE T70213A                                                                  
         TITLE 'T70213 - AGENCY GROUP MAINTENANCE'                              
T70213   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70213                                                         
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
         GOTO1 INITIAL,DMCB,PFTBL  INITIALIZE                                   
         SPACE 2                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   AGY10                                                            
         GOTO1 RECVAL,DMCB,TLAGCDQ,(X'40',SAGAGGH) BUILD THE KEY                
         B     XIT                                                              
         SPACE 3                                                                
AGY10    CLI   THISLSEL,C'D'       IF DELETING FROM A LIST                      
         BE    *+12                DON'T DISPLAY TILL XRECDEL                   
         CLI   MODE,DISPREC                                                     
         BE    AGY15                                                            
         CLI   MODE,XRECADD                                                     
         BE    AGY13               IF MODE IS NEW RECORD ADDED                  
         CLI   MODE,XRECDEL                                                     
         BE    AGY13               OR RECORD DELETED                            
         CLI   MODE,XRECREST                                                    
         BE    AGY13               OR RESTORED                                  
         CLI   MODE,XRECPUT        OR CHANGED                                   
         BNE   AGY20                                                            
AGY13    GOTO1 ADDPTRS,DMCB,PTRBLK ADD ACTIVE AND PASSIVE PTRS                  
         SPACE                                                                  
AGY15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
AGY20    CLI   MODE,VALREC                                                      
         BE    AGY25               IF MODE IS VALIDATE RECORD                   
         CLI   MODE,RECDEL                                                      
         BE    AGY25               OR DELETE                                    
         CLI   MODE,RECREST        OR RESTORE                                   
         BNE   AGY30                                                            
         SPACE                                                                  
AGY25    GOTO1 SAVPTRS,DMCB,PTRBLK SAVE PASSIVE PTRS                            
         SPACE                                                                  
         CLI   MODE,RECDEL         IF DELETING                                  
         BE    CHKDEL              CHECK IF OK FOR DELETION                     
         CLI   MODE,RECREST                                                     
         BE    XIT                 XIT IF RESTORE                               
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         SPACE 3                                                                
         USING TLAGD,R3                                                         
AGY30    CLI   MODE,DISPKEY        DISPLAY KEY FOR SELECT                       
         BNE   XIT                                                              
         L     R3,AIO                                                           
         MVC   SAGAGG,TLAGAGG      GROUP CODE                                   
         OI    SAGAGGH+6,X'80'     TRANSMIT                                     
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SAGNAMEH                                                         
         GOTO1 CHAROUT,DMCB,TANAELQ,SAGNAMEH NAME                               
         GOTO1 (RF),(R1),TASNELQ,SAGSNMEH    SHORT NAME                         
         GOTO1 ACTVOUT,DMCB,SAGLCHGH         LAST CHANGED                       
         B     XIT                                                              
         SPACE 3                                                                
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         GOTO1 NAMIN,DMCB,TANAELQ,SAGNAMEH NAME                                 
         GOTO1 (RF),(R1),TASNELQ,(X'80',SAGSNMEH) OPTIONAL SHORT NAME           
         GOTO1 ACTVIN,DMCB,SAGLCHGH        LAST CHANGED                         
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE CHECKS THAT THERE ARE NO AGENCIES IN THIS                
*              AGENCY GROUP BEFORE DELETING                                     
         SPACE                                                                  
         USING TLAGD,R3                                                         
         USING TLAYPD,R4                                                        
CHKDEL   DS    0H                                                               
         MVC   SVKEY,KEY           GENCON USES KEY TO DELETE ACTIVE PTR         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         L     R3,AIO                                                           
         MVI   TLAYPCD,TLAYGCDQ    BUILD PASSIVE KEY FOR AGENCY                 
         MVC   TLAYGAGG,TLAGAGG    USING SAME AGENCY GROUP AS RECORD            
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   TLAYGAGG,TLAGAGG    IF NO AGENCIES IN AGENCY GROUP               
         BE    *+14                                                             
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     XIT                                                              
         SPACE                                                                  
         LA    R2,SAGAGGH          IF AGENCIES IN AGENCY GROUP EXISTS           
         BAS   RE,DISPLAY          DISPLAY RECORD                               
         B     NODELETE            ERROR - CAN'T DELETE                         
         SPACE 3                                                                
NODELETE MVI   ERROR,ERINVDEL      RECORD NOT AVAILABLE FOR DELETION            
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTBL    DS    0C                  PF KEYS TABLE                                
         SPACE                                                                  
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'AGROUP',CL8'LIST'                                     
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'AGY',CL8'LIST'                                        
PF14     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGAGG-1),AL2(TGAGG-TGD)                           
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR13D                                                       
         SPACE 3                                                                
SVKEY    DS    CL38                SAVED KEY WITH ADDRESS                       
PTRBLK   DS    CL(L'TLDRREC*2+1)   SAVED ACTIVE AND 1 PASSIVE PTR               
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
**PAN#1  DC    CL21'034TAGEN13   05/01/02'                                      
         END                                                                    
