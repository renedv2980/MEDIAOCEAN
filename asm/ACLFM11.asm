*          DATA SET ACLFM11    AT LEVEL 013 AS OF 05/01/02                      
*PHASE T60311A                                                                  
         TITLE 'MODULE TO HANDLE POOL ALLOCATIONS'                              
T60311   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM11*                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         EJECT                                                                  
*              BUILD KEY - FIRST TIME NEED COMPANY/LEDGER INFO                  
         SPACE 3                                                                
         LA    R2,LOGCLIH                                                       
         CLI   MODE,BUILDKEY                                                    
         BNE   PA30                                                             
         CLI   36(RA),0                                                         
         BNE   PA10                                                             
         SPACE 2                                                                
         MVC   KEY,SPACES          COMPANY                                      
         MVC   KEY(1),COMPANY                                                   
         GOTO1 READ                                                             
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
         USING ACCOMPD,R4                                                       
         MVC   PRODLEDG,ACMPJOB                                                 
         SPACE 2                                                                
         MVC   KEY+1(2),PRODLEDG   LEDGER                                       
         GOTO1 READ                                                             
         SR    R3,R3                                                            
         SPACE 2                                                                
PA2      CLI   0(R4),X'16'         LOOK FOR HEIRARCHY                           
         BE    PA4                                                              
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     PA2                                                              
         SPACE 2                                                                
PA4      DS    0H                                                               
         USING ACHEIRD,R4                                                       
         MVC   LEVELA,ACHRLEVA                                                  
         MVC   LEVELB,ACHRLEVB                                                  
         MVC   LEVELC,ACHRLEVC                                                  
         MVI   36(RA),X'FF'                                                     
         EJECT                                                                  
*              NOW HANDLE CLIENT/PRODUCT/JOB                                    
         SPACE 3                                                                
PA10     MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODLEDG                                                
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   KEY+3(12),WORK                                                   
         CLC   5(1,R2),LEVELA                                                   
         BNH   PA12                                                             
         MVI   ERROR,ACTOOLNG                                                   
         B     XIT                                                              
         SPACE 2                                                                
PA12     TM    4(R2),X'20'                                                      
         BO    PA14                                                             
         FOUT  LOGCLINH,SPACES,36                                               
         FOUT  LOGPRONH,SPACES,36                                               
         FOUT  LOGJOBNH,SPACES,36                                               
         NI    LOGPROH+4,X'DE'                                                  
         NI    LOGJOBH+4,X'DE'                                                  
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         LA    R5,CLIRECV                                                       
         BAS   RE,DIGPROF                                                       
         MVC   PRORECV(30),CLIRECV                                              
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         SPACE 2                                                                
PA14     LA    R2,LOGPROH          PRODUCT                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLC   WORK(6),=CL6'ALL'                                                
         BNE   PA16                                                             
         TM    4(R2),X'20'                                                      
         BO    PA62                                                             
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         FOUT  LOGPRONH,SPACES,36                                               
         FOUT  LOGJOBNH,SPACES,36                                               
         FOUT  LOGJOBH,SPACES,6                                                 
         B     PA62                                                             
         SPACE 2                                                                
PA16     SR    R3,R3                                                            
         IC    R3,LEVELA                                                        
         LA    R5,KEY+3(R3)                                                     
         MVC   0(6,R5),WORK                                                     
         SR    R4,R4                                                            
         IC    R4,LEVELB                                                        
         SR    R4,R3                                                            
         STC   R4,WORK                                                          
         CLC   5(1,R2),WORK                                                     
         BNH   PA18                                                             
         MVI   ERROR,ACTOOLNG                                                   
         B     XIT                                                              
         SPACE 2                                                                
PA18     TM    4(R2),X'20'                                                      
         BO    PA20                                                             
         FOUT  LOGPRONH,SPACES,36                                               
         FOUT  LOGJOBNH,SPACES,36                                               
         NI    LOGJOBH+4,X'DE'                                                  
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         SPACE 2                                                                
PA20     LA    R2,LOGJOBH          JOB                                          
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLC   WORK(6),=CL6'ALL'                                                
         BNE   PA22                                                             
         TM    4(R2),X'20'                                                      
         BO    PA62                                                             
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         FOUT  LOGJOBNH,SPACES,36                                               
         B     PA62                                                             
         SPACE 2                                                                
PA22     SR    R3,R3                                                            
         IC    R3,LEVELB                                                        
         LA    R5,KEY+3(R3)                                                     
         MVC   0(6,R5),WORK                                                     
         SR    R4,R4                                                            
         IC    R4,LEVELC                                                        
         SR    R4,R3                                                            
         STC   R4,WORK                                                          
         CLC   5(1,R2),WORK                                                     
         BNH   PA24                                                             
         MVI   ERROR,ACTOOLNG                                                   
         B     XIT                                                              
         SPACE 2                                                                
PA24     TM    4(R2),X'20'                                                      
         BO    PA62                                                             
         FOUT  LOGJOBNH,SPACES,36                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         B     PA62                                                             
         EJECT                                                                  
*              ROUTINES TO DISPLAY ELEMENTS                                     
         SPACE 3                                                                
PA30     CLI   MODE,DSPLYREC                                                    
         BNE   PA40                                                             
         LA    R2,LOGFRSTH                                                      
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         LA    R5,12                                                            
         SPACE 2                                                                
PA32     CLI   0(R4),0             LOOK FOR POOL ELEMENTS                       
         BE    PA36                                                             
         CLI   0(R4),X'3A'                                                      
         BNE   PA34                                                             
         USING ACPOOLD,R4                                                       
         FOUT  (R2),ACPLBRND,6                                                  
         BAS   RE,BUMP                                                          
         FOUT  (R2),SPACES,6                                                    
         EDIT  (P3,ACPLPCNT),(6,8(R2)),2,ALIGN=LEFT                             
         MVC   KEY,SPACES          LOOK UP PRODUCT NAME                         
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODLEDG                                                
         MVC   KEY+3(6),LOGCLI                                                  
         IC    R3,LEVELA                                                        
         LA    R1,KEY+3(R3)                                                     
         MVC   0(6,R1),ACPLBRND                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BCT   R5,PA34                                                          
         B     XIT                                                              
         SPACE 2                                                                
PA34     IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     PA32                                                             
         SPACE 2                                                                
PA36     CLC   8(6,R2),SPACES                                                   
         BE    XIT                                                              
         FOUT  (R2),SPACES,6                                                    
         BAS   RE,BUMP                                                          
         FOUT  (R2),SPACES,6                                                    
         BAS   RE,BUMP                                                          
         FOUT  (R2),SPACES,36                                                   
         BAS   RE,BUMP                                                          
         BCT   R5,PA36                                                          
         B     XIT                                                              
         SPACE 2                                                                
BUMP     SR    R3,R3                                                            
         IC    R3,0(R2)                                                         
         AR    R2,R3                                                            
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINES TO BUILD RECORD                                         
         SPACE 3                                                                
PA40     LA    R2,LOGFRSTH                                                      
         GOTO1 REMANEL,DMCB,(X'3A',0)                                           
         XC    SPLIT,SPLIT                                                      
         LA    R4,ELEMENT                                                       
         USING ACPOOLD,R4                                                       
         LA    R5,12                                                            
         SPACE 2                                                                
PA42     CLI   5(R2),0             NO DATA IN THIS FIELD                        
         BNE   PA50                                                             
         BAS   RE,BUMP                                                          
         CLC   8(6,R2),SPACES                                                   
         BE    PA44                                                             
         FOUT  (R2),SPACES,6                                                    
         SPACE 2                                                                
PA44     BAS   RE,BUMP                                                          
         CLC   8(36,R2),SPACES                                                  
         BE    PA46                                                             
         FOUT  (R2),SPACES,36                                                   
         SPACE 2                                                                
PA46     BAS   RE,BUMP                                                          
         BCT   R5,PA42                                                          
         B     PA60                                                             
         SPACE 2                                                                
PA50     GOTO1 ANY                 DATA HERE                                    
         GOTO1 MOVE                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODLEDG                                                
         MVC   KEY+3(6),LOGCLI                                                  
         SR    R3,R3                                                            
         IC    R3,LEVELA                                                        
         LA    R1,KEY+3(R3)                                                     
         MVC   0(6,R1),WORK                                                     
         GOTO1 READ                                                             
         BAS   RE,BUMP                                                          
         GOTO1 NAMOUT                                                           
         MVC   ACPLEL(2),=X'3A29'                                               
         MVC   ACPLBRND,WORK                                                    
         MVC   ACPLRECV(30),CLIRECV                                             
         LR    R0,R5                                                            
         LA    R5,ACPLRECV         SEE IF THIS BRAND HAS AN OVERRIDE            
         BAS   RE,DIGPROF                                                       
         LR    R5,R0                                                            
         GOTO1 ANY                                                              
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+7(1),5(R2)                                                  
         GOTO1 CASHVAL,DMCB,8(R2)                                               
         CLI   DMCB,X'FF'                                                       
         BNE   PA52                                                             
         MVI   ERROR,CASHERR                                                    
         B     XIT                                                              
         SPACE 2                                                                
PA52     L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
         ZAP   ACPLPCNT,DUB                                                     
         GOTO1 ADDANEL                                                          
         A     R1,SPLIT                                                         
         ST    R1,SPLIT                                                         
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BCT   R5,PA42                                                          
         EJECT                                                                  
*              CHECK FOR 100 AND XITS                                           
         SPACE 3                                                                
PA60     CLC   SPLIT,=F'10000'                                                  
         BE    XIT                                                              
         MVI   ERROR,NOHUNDRD                                                   
         B     XIT                                                              
         SPACE 2                                                                
PA62     LA    R2,LOGFRSTH                                                      
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              ROUTINE TO DIG OUT PROFILE DETAILS                               
         SPACE 3                                                                
DIGPROF  NTR1                                                                   
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
         SPACE 2                                                                
DP2      CLI   0(R4),0                                                          
         BE    DPEXT                                                            
         CLI   0(R4),X'24'                                                      
         BNE   DP4                                                              
         USING ACPROFD,R4                                                       
         OC    ACPRRECV,ACPRRECV                                                
         BZ    *+10                                                             
         MVC   0(15,R5),ACPRRECV                                                
         OC    ACPRCOST,ACPRCOST                                                
         BZ    *+10                                                             
         MVC   15(15,R5),ACPRCOST                                               
         B     DPEXT                                                            
         SPACE 2                                                                
DP4      IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     DP2                                                              
         SPACE 2                                                                
DPEXT    XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         EJECT                                                                  
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMEED                                                       
CLIRECV  DS    CL15                                                             
CLICOST  DS    CL15                                                             
PRORECV  DS    CL15                                                             
PROCOST  DS    CL15                                                             
LEVELA   DS    CL1                                                              
LEVELB   DS    CL1                                                              
LEVELC   DS    CL1                                                              
PRODLEDG DS    CL2                                                              
SPLIT    DS    F                                                                
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE ACLFMEQU                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACLFM11   05/01/02'                                      
         END                                                                    
