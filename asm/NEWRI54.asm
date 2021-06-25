*          DATA SET NEWRI54    AT LEVEL 013 AS OF 05/01/02                      
*PHASE T32054A                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'T32054 - CPACKAGE UPDATE REPORT'                                
T32054   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CPAK**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1          ANETWS1=WORKING STORAGE                      
         USING WORKD,R7                                                         
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE                                                                  
EDITMOD  NTR1                                                                   
*                                                                               
EDITM1   MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH               CLIENT                                  
         NETGO NVCLI,DMCB,                                                      
*                                                                               
         LA    R2,SPLESTH               ESTIMATE                                
         NETGO NVEST,DMCB                                                       
*                                                                               
         LA    R2,SPLNETH               NETWORK                                 
         NETGO NVNET,DMCB                                                       
         CLC   =C'ALL',SPLNET                                                   
         BE    EDINV                                                            
*                                                                               
         LA    R2,SPLPAKH               PACKAGE                                 
         NETGO NVPAK,DMCB                                                       
*                                                                               
         LA    R2,SPLRUNH               RUN TYPE                                
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LA    R3,STATBL                                                        
EDT7     EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R3)                                                    
         BE    EDT10                                                            
         LA    R3,10(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BE    EDINV                                                            
         B     EDT7                                                             
EDT10    MVC   STATUS,0(R3)        SET STATUS                                   
         MVC   8(10,R2),0(R3)                                                   
         OI    SPLRUNH+6,X'80'                                                  
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
STATBL   DS    0H                                                               
         DC    CL10'LOCKED'                                                     
         DC    CL10'UNLOCKED'                                                   
         DC    CL10'FROZEN'                                                     
         DC    CL10'UNFROZEN'                                                   
         DC    CL10'UNNETINT'                                                   
         DC    X'FF'                                                            
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
REPMOD   NTR1                                                                   
         CLC   =C'UNLOCKED',STATUS    ...IF UNLOCKING                           
         BNE   *+8                                                              
         MVI   NBSELPST,C'L'          ...READ LOCKED PACK/UNITS                 
         XC    COUNTER,COUNTER                                                  
         MVI   NBDATA,C'B'         PACKAGES/UNITS                               
         MVI   NBRESUME,NBPROCPK   START READING PACKAGES AGAIN                 
         LA    R1,MAINLINE                                                      
         ST    R1,NBHOOK                                                        
RPM10    NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BNE   RPM10                                                            
         MVC   P+1(15),=C'RECORDS UPDATED'                                      
         L     R2,COUNTER                                                       
         EDIT  (R2),(10,P+17),ALIGN=LEFT                                        
         LTR   R2,R2                                                            
         BNZ   *+10                                                             
         MVC   P+17(5),=C'=NONE'                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
MAINLINE NTR1                                                                   
         DS    0H                                                               
         MVI   NBUPUNIT,C'N'                                                    
         MVI   NBNOWRIT,C'N'                                                    
         CLI   NBMODE,NBPROCUN                                                  
         BE    MN50                                                             
         CLI   NBMODE,NBPROCPK                                                  
         BNE   MNX                                                              
*                                                                               
         DS    0H                  PACKAGE RECORDS                              
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   MNX                                                              
         USING NPAKEL,R2                                                        
         CLC   =C'LOCKED',STATUS                                                
         BNE   *+12                                                             
         OI    NPAKSTAT,X'20'      PACKAGE IS LOCKED                            
         B     MN70                                                             
         CLC   =C'UNLOCKED',STATUS                                              
         BNE   *+12                                                             
         NI    NPAKSTAT,X'FF'-X'20' PACKAGE IS UNLOCKED                         
         B     MN70                                                             
         CLC   =C'FROZEN',STATUS                                                
         BNE   *+12                                                             
         OI    NPAKSTAT,X'80'      PACKAGE IS FROZEN                            
         B     MN70                                                             
         CLC   =C'UNFROZEN',STATUS                                              
         BNE   *+12                                                             
         NI    NPAKSTAT,X'FF'-X'80'  PACKAGE IS UNFROZEN                        
         B     MN70                                                             
         CLC   =C'UNNETINT',STATUS                                              
         BNE   MN70                                                             
         OI    NPAKSTAT,X'04'      INTEGRATION                                  
         B     MN70                                                             
*                                                                               
MN50     DS    0H                  UNIT RECORDS                                 
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   MNX                                                              
         USING NUMAINEL,R2                                                      
         CLC   =C'LOCKED',STATUS                                                
         BNE   *+12                                                             
         OI    NUPACKST,X'20'      PACKAGE IS LOCKED                            
         B     MN70                                                             
         CLC   =C'UNLOCKED',STATUS                                              
         BNE   *+12                                                             
         NI    NUPACKST,X'FF'-X'20' PACKAGE IS UNLOCKED                         
         B     MN70                                                             
         CLC   =C'FROZEN',STATUS                                                
         BNE   *+12                                                             
         OI    NUPACKST,X'80'      PACKAGE IS FROZEN                            
         B     MN70                                                             
         CLC   =C'UNFROZEN',STATUS                                              
         BNE   *+12                                                             
         NI    NUPACKST,X'FF'-X'80'  PACKAGE IS UNFROZEN                        
         B     MN70                                                             
         CLC   =C'UNNETINT',STATUS                                              
         BNE   MNX                                                              
         OI    NUPACKST,X'04'      INTEGRATION                                  
         B     MN70                                                             
*                                                                               
MN70     L     R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER                                                       
         MVI   NBUPUNIT,C'Y'                                                    
         MVI   NBNOWRIT,C'Y'                                                    
*                                                                               
         B     MNX                                                              
         GOTO1 =V(CLUNPK),DMCB,NBACTCLI,P+1    **TESTING**                      
         MVC   P+5(6),NBACTPRG                                                  
         MVC   P+13(4),NBACTNET                                                 
         ZIC   R3,NBACTEST                                                      
         EDIT  (R3),(3,P+19)                                                    
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,P+24)                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R3,NBAIO                                                         
         GOTO1 HEXOUT,DMCB,0(R3),P,90                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
MNX      B     XIT                                                              
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
         GETEL (R2),NBDTADSP,ELCODE                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
COUNTER  DS    F                                                                
TESTRUN  DS    CL1                                                              
REQRUN   DS    CL10                                                             
STATUS   DS    CL10                                                             
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIDAD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013NEWRI54   05/01/02'                                      
         END                                                                    
