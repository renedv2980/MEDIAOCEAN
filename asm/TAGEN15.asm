*          DATA SET TAGEN15    AT LEVEL 030 AS OF 05/01/02                      
*PHASE T70215A                                                                  
         TITLE 'T70215 - CLIENT GROUP MAINTENANCE'                              
T70215   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70215                                                         
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
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   CLG10                                                            
         GOTO1 RECVAL,DMCB,TLCGCDQ,(X'40',SCGCLGH) BUILD THE KEY                
         B     XIT                                                              
         SPACE 3                                                                
CLG10    CLI   THISLSEL,C'D'       IF DELETING FROM A LIST                      
         BE    *+12                DON'T DISPLAY TILL XRECDEL                   
         CLI   MODE,DISPREC                                                     
         BE    CLG15                                                            
         CLI   MODE,XRECADD                                                     
         BE    CLG13               IF MODE IS NEW RECORD ADDED                  
         CLI   MODE,XRECDEL                                                     
         BE    CLG13               OR RECORD DELETED                            
         CLI   MODE,XRECREST                                                    
         BE    CLG13               OR RESTORED                                  
         CLI   MODE,XRECPUT        OR CHANGED                                   
         BNE   CLG20                                                            
CLG13    GOTO1 ADDPTRS,DMCB,PTRS   ADD ACTIVE AND PASSIVE PTRS                  
         SPACE                                                                  
CLG15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
CLG20    CLI   MODE,VALREC                                                      
         BE    CLG25               IF MODE IS VALIDATE RECORD                   
         CLI   MODE,RECDEL                                                      
         BE    CLG25               OR DELETE                                    
         CLI   MODE,RECREST        OR RESTORE                                   
         BNE   CLG30                                                            
         SPACE                                                                  
CLG25    GOTO1 SAVPTRS,DMCB,PTRS   SAVE PASSIVE PTRS                            
         SPACE                                                                  
         CLI   MODE,RECDEL         IF DELETING                                  
         BE    CHKDEL              CHECK IF OK FOR DELETION                     
         CLI   MODE,RECREST                                                     
         BE    XIT                 XIT IF RESTORE                               
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         SPACE 3                                                                
         USING TLCGD,R3                                                         
CLG30    CLI   MODE,DISPKEY        DISPLAY KEY FOR SELECT                       
         BNE   XIT                                                              
         L     R3,AIO                                                           
         MVC   SCGCLG,TLCGCLG      GROUP CODE                                   
         OI    SCGCLGH+6,X'80'     TRANSMIT                                     
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SCGNAMEH                                                         
         GOTO1 CHAROUT,DMCB,TANAELQ,SCGNAMEH NAME                               
         GOTO1 (RF),(R1),TASNELQ,SCGSNMEH    SHORT NAME                         
         GOTO1 ACTVOUT,DMCB,SCGLCHGH         LAST CHANGED                       
         B     XIT                                                              
         SPACE 3                                                                
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         GOTO1 NAMIN,DMCB,TANAELQ,SCGNAMEH NAME                                 
         GOTO1 (RF),(R1),TASNELQ,(X'80',SCGSNMEH) OPTIONAL SHORT NAME           
         GOTO1 ACTVIN,DMCB,SCGLCHGH        LAST CHANGED                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS THAT THERE ARE NO CLIENTS IN THIS                 
*              CLIENT GROUP BEFORE DELETING                                     
         SPACE                                                                  
         USING TLCGD,R3                                                         
CHKDEL   DS    0H                                                               
         MVC   SVKEY,KEY           GENCON USES KEY TO DELETE ACTIVE PTR         
         L     R3,AIO                                                           
         LA    R4,KEY                                                           
         SPACE                                                                  
         USING TLCLPD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   TLCLPCD,TLCLGCDQ    BUILD PASSIVE KEY FOR CLIENT                 
         MVC   TLCLGCLG,TLCGCLG    USING SAME CLIENT GROUP AS RECORD            
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   TLCLPKEY(TLCLGAGY-TLCLPD),KEYSAVE  IF CLIENTS IN GROUP           
         BE    CHKDERR                            THEN CAN'T DELETE             
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     XIT                                                              
         SPACE                                                                  
CHKDERR  LA    R2,SCGCLGH          IF CLIENTS IN CLIENT GROUP EXISTS            
         MVI   ERROR,ERINVDEL      RECORD NOT AVAILABLE FOR DELETION            
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         SPACE                                                                  
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'CGROUP',CL8'LIST'                                     
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CLIENT',CL8'LIST'                                     
PF14     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGCLG-1),AL2(TGCLG-TGD)                           
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR15D                                                       
         SPACE 3                                                                
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY WITH ADDRESS                       
PTRS     DS    CL(L'TLDRREC*2+1)   SAVED ACTIVE AND 1 PASSIVE PTR               
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
**PAN#1  DC    CL21'030TAGEN15   05/01/02'                                      
         END                                                                    
