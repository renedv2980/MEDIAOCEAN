*          DATA SET CTREP8002S AT LEVEL 028 AS OF 05/01/02                      
*PHASE CT8002A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE HEXIN                                                                  
         TITLE 'TERMINAL REPORT'                                                
CT8002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TERM**,RR=R2                                                 
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING CT8002+4096,R9                                                   
         ST    R2,RELO                                                          
         CLI   MODE,RUNFRST                                                     
         BNE   TRA                                                              
         L     R8,=A(BUFFALOC)                                                  
         A     R8,RELO                                                          
         GOTO1 BUFFALO,DMCB,=C'SET',(R8)                                        
         B     XIT                                                              
         SPACE 2                                                                
TRA      CLI   MODE,REQFRST                                                     
         BNE   TRC                                                              
         MVI   RCSUBPRG,1                                                       
         MVI   ANYACT,C'N'                                                      
         BAS   RE,SYSHEAD                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         SPACE 2                                                                
TRC      CLI   MODE,REQLAST                                                     
         BNE   TRD                                                              
         CLI   ANYACT,C'Y'                                                      
         BNE   XIT                                                              
         BAS   RE,PROGXREF                                                      
         BAS   RE,IDXREF                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS A TERMINAL                                               
         SPACE 3                                                                
TRD      CLI   MODE,PROCTERM                                                    
         BNE   XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         L     R2,ADRECORD                                                      
         USING CTTREC,R2                                                        
         MVC   HEAD4+19(4),CTTKLINE                                             
         MVC   HEAD5+19(4),CTTKADDR                                             
         OC    CTTKPASS,CTTKPASS                                                
         BZ    *+16                                                             
         MVC   HEAD4+46(8),=C'PASSWORD'                                         
         MVC   HEAD4+55(10),CTTKPASS                                            
         L     R4,ADACTIV                                                       
         USING CTACTD,R4                                                        
         GOTO1 DATCON,DMCB,(3,CTACTDT),(8,HEAD6+94)                             
         L     R4,ADDESC                                                        
         MVC   HEAD6+19(13),=C'NOT SPECIFIED  '                                 
         LTR   R4,R4                                                            
         BZ    TR2                                                              
         MVC   HEAD6+19(13),SPACES                                              
         USING CTDSCD,R4                                                        
         SR    R3,R3                                                            
         IC    R3,CTDSCLEN                                                      
         SH    R3,=H'2'                                                         
         GOTO1 CHOPPER,DMCB,((R3),CTDSC),(60,HEAD6+19),(C'P',2)                 
         SPACE 2                                                                
TR2      L     R4,ADRECORD                                                      
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         B     TR6                                                              
         SPACE 2                                                                
TR4      BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
TR6      BNE   TR8                                                              
         BAS   RE,CLEAR                                                         
         BAS   RE,AUTH                                                          
         B     TR4                                                              
         SPACE 2                                                                
TR8      BAS   RE,CLEAR                                                         
         MVI   ELCODE,X'20'                                                     
         L     R4,ADRECORD                                                      
         BAS   RE,GETEL                                                         
         BNE   PL2                                                              
         MVI   FIRSTID,C'1'                                                     
         B     TR12                                                             
         SPACE 2                                                                
TR10     BAS   RE,NEXTEL                                                        
         MVI   FIRSTID,C'2'                                                     
         SPACE 2                                                                
TR12     BNE   TR14                                                             
         BAS   RE,IDLIST                                                        
         MVI   ANYACT,C'Y'                                                      
         B     TR10                                                             
         SPACE 2                                                                
TR14     GOTO1 =V(SQUASHER),DMCB,AREA,1000,RR=RB                                
         MVC   P+1(7),=C'ID LIST'                                               
         MVC   PSECOND+1(7),=7C'-'                                              
         GOTO1 CHOPPER,DMCB,(250,AREA),(96,P+11),(C'P',4)                       
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         EJECT                                                                  
*              ROUTINE FOR PROGRAM EXCEPTION LIST                               
         SPACE 3                                                                
PL2      L     R4,ADRECORD                                                      
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   TI2                                                              
         USING CTPRGD,R4                                                        
         MVC   P+1(22),=C'PROGRAM EXCEPTION LIST'                               
         MVC   PSECOND+1(22),=22C'-'                                            
         BAS   RE,CLEAR                                                         
         LA    R5,AREA                                                          
         SPACE 2                                                                
PL4      MVC   0(4,R5),CTPRGRAM                                                 
         MVI   4(R5),C'='                                                       
         MVC   5(1,R5),CTPRGTST                                                 
         LA    R5,7(R5)                                                         
         MVC   WORK(8),CTTKLINE                                                 
         MVI   WORK+8,C'='                                                      
         MVC   WORK+9(1),CTPRGTST                                               
         BAS   RE,EXOUT                                                         
         BAS   RE,NEXTEL                                                        
         BE    PL4                                                              
         GOTO1 CHOPPER,DMCB,(250,AREA),(67,P+41),(C'P',4)                       
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         B     TI2                                                              
         EJECT                                                                  
*              ROUTINE FOR TERMINAL INFO                                        
         SPACE 3                                                                
TI2      L     R4,ADRECORD                                                      
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING CTTIND,R4                                                        
         MVC   P+1(20),=C'TERMINAL INFORMATION'                                 
         MVC   PSECOND+1(20),=22C'-'                                            
         MVC   P+41(14),=C'SERIAL NUMBER='                                      
         MVC   P+55(15),CTTINSER                                                
         OC    CTTINDAT,CTTINDAT                                                
         BZ    T14                                                              
         MVC   PSECOND+41(18),=C'DATE LAST CHANGED='                            
         GOTO1 DATCON,DMCB,(3,CTTINDAT),(8,PSECOND+59)                          
T14      MVC   PTHIRD+41(7),=C'VENDOR='                                         
         ZIC   R3,CTTINLEN                                                      
         SH    R3,=H'21'                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   PTHIRD+48(0),CTTINVEN                                            
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         B     XIT                                                              
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
ELCODE   DC    X'00'                                                            
         DC    X'00'                                                            
ANYACT   DC    C'N'                                                             
         EJECT                                                                  
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO GET SYSTEM AND PROGRAM NAME FROM SELIST               
         SPACE 3                                                                
GETPROG  NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
         MVC   0(20,R4),=CL20'UNKNOWN   UNKNOWN   '                             
         L     R5,SELIST                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         SPACE 2                                                                
GP2      CLC   SEOVSYS,0(R2)                                                    
         BE    GP4                                                              
         BXLE  R5,R6,GP2                                                        
         B     XIT                                                              
         SPACE 2                                                                
GP4      MVC   0(7,R4),SENAME                                                   
         LA    R7,6(R4)                                                         
         LA    R6,7                                                             
         SPACE 2                                                                
GP6      CLI   0(R7),C'Z'                                                       
         BH    GP8                                                              
         BCTR  R7,0                                                             
         BCT   R6,GP6                                                           
         B     GP10                                                             
         SPACE 2                                                                
GP8      MVI   0(R7),C' '          GET RID OF TRAILING NUMBER                   
         SPACE 2                                                                
GP10     L     R5,SEPGMS                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
         SPACE 2                                                                
GP12     CLC   PGMNUM,0(R3)                                                     
         BE    GP14                                                             
         BXLE  R5,R6,GP12                                                       
         B     XIT                                                              
         SPACE 2                                                                
GP14     MVC   10(7,R4),PGMNAME                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CLEAR AND ADD IDS                                     
         SPACE 3                                                                
CLEAR    MVI   AREA,C' '                                                        
         MVC   AREA+1(249),AREA                                                 
         MVC   AREA+250(250),AREA+000                                           
         MVC   AREA+500(250),AREA+250                                           
         MVC   AREA+750(250),AREA+500                                           
         BR    RE                                                               
         SPACE 2                                                                
IDLIST   NTR1                                                                   
         LA    R5,AREA                                                          
         SPACE 2                                                                
ID2      CLC   0(10,R5),SPACES                                                  
         BE    ID4                                                              
         LA    R5,11(R5)                                                        
         B     ID2                                                              
         SPACE 2                                                                
ID4      DS    0H                                                               
         USING CTIDD,R4                                                         
         MVC   0(10,R5),CTID                                                    
         XC    BUFFIO,BUFFIO       WRITE RECORDS FOR ID/TERM XREF               
         MVI   BUFFIO,2                                                         
         MVC   BUFFIO+1(10),CTID                                                
         BAS   RE,BUFFOUT                                                       
         MVI   BUFFIO+11,X'FF'                                                  
         BAS   RE,BUFFOUT                                                       
         MVC   BUFFIO+11(21),SPACES                                             
         MVC   BUFFIO+11(1),FIRSTID                                             
         MVC   BUFFIO+12(8),CTTKLINE                                            
         OC    CTTKPASS,CTTKPASS                                                
         BZ    *+14                                                             
         MVI   BUFFIO+20,C','                                                   
         MVC   BUFFIO+21(10),CTTKPASS                                           
         CLI   BUFFIO+15,C' '                                                   
         BNE   *+8                                                              
         MVI   BUFFIO+15,C'*'                                                   
         CLI   BUFFIO+15,0                                                      
         BNE   *+8                                                              
         MVI   BUFFIO+15,C'*'                                                   
         BAS   RE,BUFFOUT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE AUTHORIZATIONS                                 
         SPACE 2                                                                
AUTH     NTR1                                                                   
         USING CTSYSD,R4                                                        
         MVI   BYTE,2                                                           
         GOTO1 GETPROG,DMCB,CTSYSNUM,BYTE,WORK                                  
         MVC   P+1(7),WORK                                                      
         MVC   AREA(15),=C'DEFAULT=X''0000'''                                   
         GOTO1 HEXOUT,DMCB,CTSYSALL,AREA+10,2                                   
         LA    R2,CTSYSPGM                                                      
         LA    R3,AREA+16                                                       
         LA    R5,1                                                             
         ZIC   R6,CTSYSLEN                                                      
         SH    R6,=H'16'                                                        
         SRL   R6,1                                                             
         LTR   R6,R6                                                            
         BZ    AUTH6                                                            
         SPACE 2                                                                
AUTH2    STC   R5,BYTE                                                          
         CLC   CTSYSALL,0(R2)      DONT SHOW IF IT EQUALS DEFAULT               
         BE    AUTH4                                                            
         GOTO1 GETPROG,DMCB,CTSYSNUM,BYTE,WORK                                  
         CLC   WORK+10(7),=C'UNKNOWN'                                           
         BE    AUTH4                                                            
         MVC   0(4,R3),WORK+10                                                  
         MVC   4(8,R3),=C'=X''0000'''                                           
         GOTO1 HEXOUT,DMCB,(R2),7(R3),2                                         
         CLC   4(8,R3),=C'=X''0000'''                                           
         BNE   *+10                                                             
         MVC   4(8,R3),=C'=N        '                                           
         LA    R3,13(R3)                                                        
         SPACE 2                                                                
AUTH4    LA    R2,2(R2)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,AUTH2                                                         
         SPACE 2                                                                
AUTH6    GOTO1 =V(SQUASHER),DMCB,AREA,1000,RR=RB                                
         GOTO1 CHOPPER,DMCB,(252,AREA),(96,P+11),(C'P',4)                       
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT ID/TERMINAL XREF                                
         SPACE 3                                                                
IDXREF   NTR1                                                                   
         XC    BUFFIO,BUFFIO                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,3                                                       
         MVI   BUFFIO,2                                                         
         L     R8,=A(BUFFALOC)                                                  
         A     R8,RELO                                                          
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R8),BUFFIO,0                              
         B     IX4                                                              
         SPACE 2                                                                
IX2      GOTO1 BUFFALO,DMCB,=C'SEQ',(R8),BUFFIO,0                               
         SPACE 2                                                                
IX4      TM    DMCB+8,X'80'                                                     
         BO    IX20                                                             
         CLI   BUFFIO,2                                                         
         BNE   IX20                                                             
         CLI   BUFFIO+11,0         ID HEADER                                    
         BNE   IX6                                                              
         MVC   P+1(10),BUFFIO+1                                                 
         BAS   RE,CLEAR                                                         
         B     IX2                                                              
         SPACE 2                                                                
IX6      CLI   BUFFIO+11,X'FF'     ID TRAILER                                   
         BNE   IX10                                                             
         GOTO1 =V(SQUASHER),DMCB,AREA,1000,RR=RB                                
         SPACE 2                                                                
IX8      GOTO1 CHOPPER,DMCB,(250,AREA),(92,P+14),(C'P',4)                       
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     IX2                                                              
         SPACE 2                                                                
IX10     LA    R2,AREA             TERMINAL(,PASSWORD) RECORD                   
         LA    R3,230                                                           
         SPACE 2                                                                
IX12     CLC   0(20,R2),SPACES                                                  
         BE    IX14                                                             
         LA    R2,1(R2)                                                         
         BCT   R3,IX12                                                          
         GOTO1 CHOPPER,DMCB,(250,AREA),(92,P+14),(C'P',4)                       
         GOTO1 REPORT                                                           
         BAS   RE,CLEAR                                                         
         B     IX10                                                             
         SPACE 2                                                                
IX14     LA    R2,1(R2)                                                         
         CLI   BUFFIO+11,C'1'                                                   
         BE    *+12                                                             
         MVI   0(R2),C'('          BRACKET SUB-IDS                              
         LA    R2,1(R2)                                                         
         MVC   0(19,R2),BUFFIO+12                                               
         CLI   BUFFIO+11,C'1'                                                   
         BE    IX2                                                              
         SPACE 2                                                                
IX16     CLI   0(R2),C' '                                                       
         BE    IX18                                                             
         LA    R2,1(R2)                                                         
         B     IX16                                                             
         SPACE 2                                                                
IX18     MVI   0(R2),C')'                                                       
         B     IX2                                                              
         SPACE 2                                                                
IX20     MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO WRITE BUFFALO RECORDS FOR SYSTEM/PROGRAMS             
         SPACE 3                                                                
SYSHEAD  NTR1                                                                   
         L     R8,=A(BUFFALOC)                                                  
         A     R8,RELO                                                          
         GOTO1 BUFFALO,DMCB,=C'RESET',(R8)                                      
         LA    R2,2                                                             
         SPACE 2                                                                
SYSHEAD2 ST    R2,DUB                                                           
         GOTO1 GETPROG,DMCB,DUB+3,1,WORK                                        
         CLC   WORK(7),=C'UNKNOWN'                                              
         BE    SYSHEAD8                                                         
         XC    BUFFIO,BUFFIO                                                    
         MVI   BUFFIO,1                                                         
         MVC   BUFFIO+1(1),WORK                                                 
         MVC   BUFFIO+3(7),WORK                                                 
         BAS   RE,BUFFOUT          SYSTEM HEADER                                
         MVI   BUFFIO+2,X'FF'                                                   
         BAS   RE,BUFFOUT                 TRAILER                               
         LA    R3,1                                                             
         LA    R4,31                                                            
         SPACE 2                                                                
SYSHEAD4 STM   R2,R3,DUB                                                        
         GOTO1 GETPROG,DMCB,DUB+3,DUB+7,WORK                                    
         CLC   WORK+10(7),=C'UNKNOWN'                                           
         BE    SYSHEAD6                                                         
         STM   R2,R3,DUB                                                        
         GOTO1 HEXOUT,DMCB,DUB+3,BUFFIO+2,1,=C'TOG'                             
         GOTO1 HEXOUT,DMCB,DUB+7,BUFFIO+4,1,=C'TOG'                             
         MVI   BUFFIO+2,C'T'                                                    
         MVI   BUFFIO+6,0                                                       
         MVC   BUFFIO+7(7),WORK+10                                              
         BAS   RE,BUFFOUT                                                       
         MVI   BUFFIO+6,X'FF'                                                   
         BAS   RE,BUFFOUT                                                       
         SPACE 2                                                                
SYSHEAD6 LA    R3,1(R3)                                                         
         BCT   R4,SYSHEAD4                                                      
         SPACE 2                                                                
SYSHEAD8 CH    R2,=H'15'                                                        
         BE    XIT                                                              
         LA    R2,1(R2)                                                         
         B     SYSHEAD2                                                         
         SPACE 2                                                                
BUFFOUT  NTR1                                                                   
         L     R8,=A(BUFFALOC)                                                  
         A     R8,RELO                                                          
         GOTO1 BUFFALO,DMCB,=C'PUT',(R8),BUFFIO                                 
         B     XIT                                                              
         SPACE 2                                                                
BUFFIO   DS    CL32                                                             
         EJECT                                                                  
*              ROUTINE TO WRITE OUT EXCEPTIONS                                  
         SPACE 3                                                                
EXOUT    NTR1                                                                   
         L     R8,=A(BUFFALOC)                                                  
         A     R8,RELO                                                          
         USING CTPRGD,R4                                                        
         XC    BUFFIO,BUFFIO                                                    
         MVI   BUFFIO,1                                                         
         MVC   BUFFIO+1(1),CTPRGRAM                                             
         MVC   BUFFIO+2(4),CTPRGRAM                                             
         MVC   BUFFIO+6(10),WORK                                                
         CLI   BUFFIO+1,C'T'                                                    
         BE    EXOUT2                                                           
         BAS   RE,BUFFOUT                                                       
         MVI   BUFFIO+6,0                                                       
         MVC   BUFFIO+7(10),=CL10'OFFLINE'                                      
         BAS   RE,BUFFOUT                                                       
         MVI   BUFFIO+6,X'FF'                                                   
         BAS   RE,BUFFOUT                                                       
         B     XIT                                                              
         SPACE 2                                                                
EXOUT2   MVI   DUB,X'F0'                                                        
         MVC   DUB+1(1),BUFFIO+3                                                
         GOTO1 =V(HEXIN),DMCB,DUB,DUB+2,2,RR=RB                                 
         GOTO1 GETPROG,DMCB,DUB+2,1,WORK                                        
         MVC   BUFFIO+1(1),WORK                                                 
         BAS   RE,BUFFOUT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT XREF OF PROGRAMS                                
         SPACE 3                                                                
PROGXREF NTR1                                                                   
         L     R8,=A(BUFFALOC)                                                  
         A     R8,RELO                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         XC    BUFFIO,BUFFIO                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R8),BUFFIO,0                              
         B     PX4                                                              
         SPACE 2                                                                
PX2      GOTO1 BUFFALO,DMCB,=C'SEQ',(R8),BUFFIO,0                               
         SPACE 2                                                                
PX4      CLI   BUFFIO,1                                                         
         BE    PX6                                                              
         MVI   RCSUBPRG,1                                                       
         B     XIT                                                              
         SPACE 2                                                                
PX6      CLI   BUFFIO+2,0          SYSTEM HEADER                                
         BNE   PX8                                                              
         MVC   P+1(7),BUFFIO+3                                                  
         B     PX2                                                              
         SPACE 2                                                                
PX8      CLI   BUFFIO+2,X'FF'      SYSTEM TRAILER                               
         BNE   PX10                                                             
         GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         B     PX2                                                              
         SPACE 2                                                                
PX10     CLI   BUFFIO+6,0          PROGRAM HEADER                               
         BNE   PX12                                                             
         MVC   P+15(4),BUFFIO+2                                                 
         MVC   P+28(7),BUFFIO+7                                                 
         BAS   RE,CLEAR                                                         
         B     PX2                                                              
         SPACE 2                                                                
PX12     CLI   BUFFIO+6,X'FF'      PROGRAM TRAILER                              
         BNE   PX14                                                             
         GOTO1 =V(SQUASHER),DMCB,AREA,1000,RR=RB                                
         GOTO1 CHOPPER,DMCB,(250,AREA),(60,P+44),(C'P',4)                       
         GOTO1 REPORT                                                           
         B     PX2                                                              
         SPACE 2                                                                
PX14     LA    R2,AREA                                                          
         SPACE 2                                                                
PX15     CLC   0(10,R2),SPACES                                                  
         BE    PX16                                                             
         LA    R2,11(R2)                                                        
         B     PX15                                                             
         SPACE 2                                                                
PX16     MVC   0(10,R2),BUFFIO+6                                                
         B     PX2                                                              
         EJECT                                                                  
*              AREAS / BUFF / LTORG                                             
         SPACE 3                                                                
FIRSTID  DC    C'1'                                                             
RELO     DS    A                                                                
         LTORG                                                                  
         SPACE 2                                                                
AREA     DC    1000C' '                                                         
         BUFF  LINES=2000,ROWS=0,COLUMNS=0,FLAVOR=DATA,KEYLIST=(32,A)           
         EJECT                                                                  
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAPGMLST                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028CTREP8002S05/01/02'                                      
         END                                                                    
