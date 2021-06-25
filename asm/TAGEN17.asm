*          DATA SET TAGEN17    AT LEVEL 018 AS OF 05/01/02                      
*PHASE T70217A                                                                  
         TITLE 'T70217 - PRODUCT GROUP MAINTENANCE'                             
T70217   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70217                                                         
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
         GOTO1 INITIAL,DMCB,PFTABLE          INITIALIZE                         
         SPACE 1                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   THISLSEL,C'D'       IF DELETING FROM LIST                        
         BE    PRG15                  DON'T DISPLAY FIRST                       
         CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    PRG30                                                            
*                                                                               
PRG15    CLI   MODE,RECDEL         DELETE RECORD                                
         BE    PRG40                                                            
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    PRG40                                                            
         CLI   MODE,XRECDEL        RECORD DELETED                               
         BE    PRG20                                                            
         CLI   MODE,XRECREST       RECORD RESTORED                              
         BE    PRG20                                                            
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BE    PRG20                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BNE   PRG50                                                            
*                                                                               
PRG20    GOTO1 ADDPTRS,DMCB,PTRBLK    ADD PASSIVE POINTERS                      
*                                                                               
PRG30    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     PRGX                                                             
*                                                                               
PRG40    GOTO1 SAVPTRS,DMCB,PTRBLK HANDLE PASSIVE PONTERS                       
         CLI   MODE,RECDEL         IF DELETING                                  
         BE    CHKDEL              CHECK IF OK FOR DELETION                     
         B     PRGX                                                             
*                                                                               
PRG50    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   PRGX                                                             
         BAS   RE,BLDREC                                                        
*                                                                               
PRGX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              VALIDATE KEY                                                     
*                                                                               
VK       GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SPGAGYH),SPGAGYNH                     
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',SPGCLIH),SPGCLINH                     
         GOTO1 RECVAL,DMCB,TLPGCDQ,(X'40',SPGPRGH)   BUILD THE KEY              
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*              DISPLAY KEY                                                      
*                                                                               
DK       MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2            AND SWITCH I/O AREAS                         
*                                                                               
         L     R3,AIO1             R3=A(PRODUCT GROUP RECORD)                   
         USING TLPGD,R3                                                         
*                                                                               
         MVC   SPGAGY,TLPGAGY      AGENCY                                       
         OI    SPGAGYH+6,X'80'                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',SPGAGY),SPGAGYNH                      
*                                                                               
         MVC   SPGCLI,TLPGCLI      CLIENT                                       
         OI    SPGCLIH+6,X'80'                                                  
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'88',SPGCLI),SPGCLINH                      
*                                                                               
         MVC   SPGPRG,TLPGPRG      PRODUCT GROUP                                
         OI    SPGPRGH+6,X'80'                                                  
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            AND I/O AREA                                 
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY THE RECORD                                               
*                                                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SPGNAMEH                                                         
         GOTO1 CHAROUT,DMCB,TANAELQ,SPGNAMEH NAME                               
         GOTO1 (RF),(R1),TASNELQ,SPGSNMEH    SHORT NAME                         
         GOTO1 ACTVOUT,DMCB,SPGLCHGH         LAST CHANGED                       
         B     XIT                                                              
         SPACE 5                                                                
*                                                                               
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         GOTO1 NAMIN,DMCB,TANAELQ,SPGNAMEH                                      
         GOTO1 (RF),(R1),TASNELQ,(X'80',SPGSNMEH) SHORT NAME                    
         GOTO1 ACTVIN,DMCB,SPGLCHGH        LAST CHANGED                         
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE CHECKS THAT THERE ARE NO PRODUCTS IN THIS                
*              PRODUCT GROUP BEFORE DELETING                                    
         SPACE                                                                  
         USING TLPGD,R3                                                         
         USING TLPRPD,R4                                                        
CHKDEL   DS    0H                                                               
         MVC   SVKEY,KEY           GENCON USES KEY TO DELETE ACTIVE PTR         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         L     R3,AIO                                                           
         MVI   TLPRPCD,TLPRGCDQ    BUILD PASSIVE KEY FOR PRODUCT                
         MVC   TLPRGAGY,TLPGAGY    USING SAME AGENCY AS RECORD                  
         MVC   TLPRGCLI,TLPGCLI               CLIENT                            
         MVC   TLPRGPRG,TLPGPRG               PRODUCT GROUP                     
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLPRGPRG+L'TLPRGPRG-TLPRPKEY),KEYSAVE                        
         BE    CHKD15                                                           
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     XIT                                                              
*                                                                               
CHKD15   LA    R2,SPGPRGH          IF PRODUCTS IN PRODUCT GROUP EXISTS          
         B     NODELETE            ERROR - CAN'T DELETE                         
         EJECT                                                                  
XIT      XIT1                                                                   
*                                                                               
         SPACE 2                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
         SPACE 2                                                                
NODELETE MVI   ERROR,ERINVDEL      RECORD NOT AVAILABLE FOR DELETION            
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
*                                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'PGROUP  ',CL8'LIST'                                   
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'PRODUCT ',CL8'LIST'                                   
PF14     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCLI-1),AL2(TGCLI-TGD)                           
         DC    AL1(KEYTYGLB,L'TGPRG-1),AL2(TGPRG-TGD)                           
PF14X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR17D                                                       
         EJECT                                                                  
*                                                                               
         ORG   SPGWORK                                                          
*                                                                               
SVKEY    DS    CL38                SAVE THE KEY                                 
         DS    0D                                                               
PTRBLK   DS    CL(2*L'TLDRREC+1)   1 ACTIVE & 1 PASSIVE                         
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018TAGEN17   05/01/02'                                      
         END                                                                    
