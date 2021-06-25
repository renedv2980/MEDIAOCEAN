*          DATA SET ACLFM04    AT LEVEL 023 AS OF 08/10/00                      
*PHASE T60304A                                                                  
*INCLUDE DATVAL                                                                 
         TITLE 'MODULE TO HANDLE BUDGETS'                                       
T60304   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM4**                                                       
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         EJECT                                                                  
*              BUILD KEY                                                        
         SPACE 3                                                                
         CLI   MODE,BUILDKEY                                                    
         BNE   BU10                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         LA    R2,LOGUNITH         UNIT                                         
         GOTO1 ANY                                                              
         CLC   LOGUNIT(3),=C'ALL'                                               
         BE    XIT                                                              
         MVC   KEY+1(1),LOGUNIT                                                 
         TM    4(R2),X'20'                                                      
         BO    BU2                                                              
         MVI   KEY+1,C' '                                                       
         GOTO1 READ                                                             
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
         USING ACCOMPD,R4                                                       
         MVC   SAVEJOB,ACMPJOB                                                  
         MVC   SAVEREC,ACMPRECV                                                 
         MVC   KEY+1(1),LOGUNIT                                                 
         NI    LOGLEDGH+4,X'DF'                                                 
         NI    LOGACCH+4,X'DF'                                                  
         FOUT  LOGUNAMH,SPACES,36                                               
         FOUT  LOGLNAMH,SPACES,36                                               
         FOUT  LOGANAMH,SPACES,36                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         MVI   ANYKEY,C'Y'                                                      
         OI    LOGUNITH+4,X'20'                                                 
         SPACE 2                                                                
BU2      LA    R2,LOGLEDGH                                                      
         GOTO1 ANY                                                              
         CLC   LOGLEDG(3),=C'ALL'                                               
         BE    XIT                                                              
         MVC   KEY+2(1),LOGLEDG                                                 
         MVI   ERROR,INVALID       CANT HAVE BUDGETS ON PRODUCTION              
         CLC   KEY+1(2),SAVEJOB                                                 
         BE    XIT                                                              
         MVI   ERROR,X'FF'                                                      
         TM    4(R2),X'20'                                                      
         BO    BU4                                                              
         NI    LOGACCH+4,X'DF'                                                  
         FOUT  LOGLNAMH,SPACES,36                                               
         FOUT  LOGANAMH,SPACES,36                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    LOGLEDGH+4,X'20'                                                 
         MVI   ANYKEY,C'Y'                                                      
         SPACE 2                                                                
BU4      LA    R2,LOGACCH                                                       
         GOTO1 ANY                                                              
         CLC   LOGACC(3),=C'ALL'                                                
         BE    XIT                                                              
         GOTO1 MOVE                                                             
         MVC   KEY+3(12),WORK                                                   
         TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         FOUT  LOGANAMH,SPACES,36                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         MVI   ANYKEY,C'Y'                                                      
         OI    4(R2),X'20'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              NOW BUILD BUDGET ELEMENT                                         
         SPACE 3                                                                
BU10     LA    R9,ELEMENT                                                       
         USING ACBUDGD,R9                                                       
         MVI   ACBDEL,X'34'                                                     
         MVI   ACBDLEN,X'1C'                                                    
         LA    R2,LOGCNTH          CONTRA ACCOUNT OPTIONAL                      
         MVC   ACBDSBAC,SPACES                                                  
         CLI   5(R2),0                                                          
         BE    BU11                                                             
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLC   WORK(6),=CL6'ALL'                                                
         BE    BU11                                                             
         MVC   ACBDSBAC(1),COMPANY                                              
         MVC   ACBDSBAC+1(14),WORK                                              
         CLC   KEY+1(2),SAVEREC    FOR RECEIVABLES                              
         BNE   BU10D                                                            
         MVC   ACBDSBAC(15),WORK   USE ALL 15 BYTES                             
         B     BU11                                                             
         SPACE 2                                                                
BU10D    MVC   KEY(15),ACBDSBAC    FOR OTHERS, VALIDATE CONTRA-ACCOUNT          
         FOUT  LOGCNAMH,SPACES,36                                               
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE                                                  
         BE    BU10E                                                            
         MVI   ERROR,NOTVLREC                                                   
         B     XIT                                                              
         MVC   ACBDSBAC(15),WORK                                                
         B     BU11                                                             
BU10E    DS    0H                                                               
         GOTO1 NAMOUT                                                           
         SPACE 2                                                                
BU11     DS    0H                                                               
         LA    R2,LOGSTRTH                                                      
         GOTO1 ANY                                                              
         GOTO1 =V(DATVAL),DMCB,(2,8(R2)),WORK,RR=RB                             
         OC    DMCB(4),DMCB                                                     
         BZ    BUDTER                                                           
         SPACE 2                                                                
BUDT2    GOTO1 DATCON,DMCB,(0,WORK),(1,ACBDSTRT)                                
         LA    R2,LOGENDH                                                       
         SPACE 2                                                                
         CLI   5(R2),0             ALLOW MISSING                                
         BE    BUDT4                                                            
         GOTO1 =V(DATVAL),DMCB,(2,8(R2)),WORK,RR=RB                             
         OC    DMCB(4),DMCB                                                     
         BZ    BUDTER                                                           
         SPACE 2                                                                
BUDT4    GOTO1 DATCON,DMCB,(0,WORK),(1,ACBDEND)                                 
         CLC   ACBDSTRT,ACBDEND                                                 
         BH    BUDTER                                                           
         LA    R2,LOGTYPEH                                                      
         GOTO1 ANY                                                              
         MVC   ACBDTYPE,LOGTYPE                                                 
         CLI   ACBDTYPE,C'C'                                                    
         BE    BU12                                                             
         CLI   ACBDTYPE,C'D'                                                    
         BE    BU12                                                             
         CLI   ACBDTYPE,C'B'                                                    
         BE    BU12                                                             
         MVI   ERROR,NOTVLCDE                                                   
         B     XIT                                                              
         SPACE 2                                                                
BUDTER   MVI   ERROR,DATERR                                                     
         B     XIT                                                              
         SPACE 2                                                                
BU12     CLI   MODE,DSPLYREC                                                    
         BE    BU15                                                             
         LA    R2,LOGAMNTH                                                      
         GOTO1 ANY                                                              
         ZAP   DUB,=P'0'                                                        
         MVI   DELETER,C'Y'                                                     
         CLC   8(6,R2),=C'DELETE'                                               
         BE    BU15                                                             
         MVI   DELETER,C'N'                                                     
         GOTO1 VALICASH                                                         
         ZAP   ACBDBUDG,DUB                                                     
         CLI   MODE,NEWELEM                                                     
         BNE   BU14                                                             
         SPACE 2                                                                
BU13     GOTO1 ADDANEL                                                          
         LA    RF,IO2                                                           
         CLC   42(2,RF),=Y(IOLENQ)                                              
         BL    XIT                                                              
         CLC   LOGOPT(5),=C'ERASE'                                              
         BE    BU13B                                                            
         MVI   ERROR,TOOLONG       RECORD HAS GOT TOO BIG                       
         LA    R2,LOGOPTH                                                       
         B     XIT                                                              
         SPACE 2                                                                
BU13B    LA    R9,IO2              USER HAS OPTED TO REMOVE OLDEST              
         AH    R9,DATADISP                                                      
         SR    R8,R8                                                            
         SPACE 2                                                                
BU13D    CLI   0(R9),0                                                          
         BE    XIT                                                              
         CLI   0(R9),X'34'         FIND OLDEST BUDGET                           
         BNE   BU13F                                                            
         CLC   ACBDTYPE,LOGTYPE    FOR THE SAME TYPE                            
         BNE   BU13F                                                            
         MVI   0(R9),X'FF'         FOUND - DROP IT                              
         GOTO1 REMANEL,DMCB,(X'FF',0)                                           
         B     XIT                                                              
         SPACE 2                                                                
BU13F    IC    R8,1(R9)                                                         
         AR    R9,R8                                                            
         B     BU13D                                                            
         SPACE 2                                                                
BU14     ZAP   DUB,ACBDBUDG                                                     
         SPACE 2                                                                
BU15     LA    R9,IO2                                                           
         AH    R9,DATADISP                                                      
         SR    R8,R8                                                            
         SPACE 2                                                                
BU16     CLI   0(R9),0             FOR AMEND FIND THE ELEMENT                   
         BE    BU20                                                             
         CLC   0(7,R9),ELEMENT                                                  
         BNE   BU17                                                             
         CLC   13(15,R9),ELEMENT+13                                             
         BE    BU18                                                             
         SPACE 2                                                                
BU17     IC    R8,1(R9)                                                         
         AR    R9,R8                                                            
         B     BU16                                                             
         SPACE 2                                                                
BU18     CLI   MODE,DSPLYREC                                                    
         BNE   BU19                                                             
         LA    R2,LOGAMNTH                                                      
         EDIT  (P6,ACBDBUDG),(12,8(R2)),2,ALIGN=LEFT,MINUS=YES                  
         FOUT  (R2)                                                             
         B     XIT                                                              
         SPACE 2                                                                
BU19     ZAP   ACBDBUDG,DUB                                                     
         CLI   DELETER,C'Y'        OPTION TO DELETE                             
         BNE   XIT                                                              
         MVI   0(R9),X'FF'                                                      
         GOTO1 REMANEL,DMCB,(X'FF',0)                                           
         B     XIT                                                              
         SPACE 2                                                                
BU20     LA    R2,LOGSTRTH                                                      
         MVI   ERROR,NOELEM                                                     
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMFBD                                                       
SAVEJOB  DS    CL2                                                              
SAVEREC  DS    CL2                                                              
DELETER  DS    CL1                                                              
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACLFM04   08/10/00'                                      
         END                                                                    
