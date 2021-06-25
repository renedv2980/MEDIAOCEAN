*          DATA SET ACCSH00    AT LEVEL 065 AS OF 05/01/02                      
*PHASE T60700A,+0                                                               
*INCLUDE SCANNER                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE PUBVAL                                                                 
         TITLE 'CASH ENQUIRY-T60700'                                            
T60700   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 T607X-T607D,**CSH**,R9,CLEAR=YES,RR=R5                           
         USING T607D,RC                                                         
         L     RA,4(R1)                                                         
         USING T607FFD,RA                                                       
         ST    R5,PRELOC                                                        
         L     RF,8(R1)                                                         
         MVC   FACLIST,0(RF)                                                    
         ST    RA,ATWA0                                                         
         MVC   TERMINAL,0(RA)      TASK NO                                      
         MVC   COMPANY,0(R1)                                                    
         MVI   DMINBTS,X'C0'       DON'T WANT DELETES                           
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   SPACES,X'40'                                                     
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         ZAP   TOTNET,=P'0'                                                     
         ZAP   TOTCD,=P'0'                                                      
         MVI   CHANGES,0           NO CHANGES MADE                              
         OI    CSHSERVH+1,X'01'                                                 
         OI    CSHSERVH+6,X'80'                                                 
         OI    CSHMSGH+6,X'80'                                                  
         EJECT                                                                  
*              SAVE SOME COMPANY VALUES                                         
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         CLC   COMPANY,COMPSAVE                                                 
         BE    CSH10                                                            
         MVC   COMPSAVE,COMPANY                                                 
         BAS   RE,READ                                                          
         LA    R2,IOAREA                                                        
*                                                                               
CSH2     CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT                           
         SR    R1,R1                                                            
         CLI   0(R2),X'10'                                                      
         BE    CSH4                                                             
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     CSH2                                                             
*                                                                               
CSH4     MVC   MYCOMPEL,0(R2)                                                   
         EJECT                                                                  
*              VALIDATE SYSTEM                                                  
*                                                                               
CSH10    LA    R2,CSHSYH                                                        
         MVI   ERRNUM,INVALID                                                   
         BAS   RE,ANY                                                           
         LA    R3,SYSTAB                                                        
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,R0                                                            
*                                                                               
CSH12    CLI   0(R3),X'FF'                                                      
         BE    ERROR                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   CSHSY(0),1(R3)                                                   
         BE    CSH14                                                            
         LA    R3,L'SYSTAB(R3)                                                  
         B     CSH12                                                            
*                                                                               
CSH14    MVC   KEY+2(1),11(R3)     LEDGER                                       
         MVC   SYSTEM,11(R3)                                                    
         CLI   11(R3),C'E'                                                      
         BNE   CSH16              HANDLE EXPENSE DEFAULT                        
         LA    R7,MYCOMPEL                                                      
         USING ACCOMPD,R7                                                       
         MVC   KEY+2(1),ACMPSUPX                                                
*                                                                               
CSH16    MVI   KEY+1,C'S'                                                       
         TM    CSHSYH+4,X'20'      ANY CHANGES MADE                             
         BO    PAYVAL              NO                                           
         MVI   CHANGES,1                                                        
         NI    CSHACH+4,X'DF'      MUST THEREFORE VALIDATE PAYEE                
         IC    RF,0(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CSHSY(0),1(R3)                                                   
         OI    CSHSYH+6,X'80'                                                   
*                                                                               
         MVI   ERRNUM,9       INVALID LEDGER                                    
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    R2,IOAREA                                                        
         SR    R1,R1                                                            
*                                                                               
CSH16A   CLI   0(R2),0                                                          
         BE    CSH16E                                                           
         CLI   0(R2),X'30'                                                      
         BE    CSH16C                                                           
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     CSH16A                                                           
*                                                                               
         USING ACSTATD,R2                                                       
         USING TWAD,R5                                                          
CSH16C   LR    R5,RA                                                            
         CLC   TWAAUTH+1(1),ACSTSECY+1                                          
         BNL   CSH16E                                                           
         MVI   ERRNUM,0                                                         
         OI    DMCB+8,X'04'                                                     
         LA    R2,CSHSYH                                                        
         B     ERROR                                                            
*                                                                               
CSH16E   CLI   SYSTEM,C'S'                                                      
         BE    CSH16G                                                           
         CLI   SYSTEM,C'U'         NETWORK IS LIKE SPOT                         
         BE    CSH16G                                                           
         CLI   SYSTEM,C'T'                                                      
         BNE   CSH16J                                                           
*                                                                               
CSH16G   MVC   CSHHDG2+22(10),=CL10'  PERIOD'                                   
         OI    CSHHDG2H+6,X'80'                                                 
         B     CSH16L                                                           
*                                                                               
CSH16J   CLI   SYSTEM,C'V'                                                      
         BE    CSH16K                                                           
         CLI   SYSTEM,C'W'                                                      
         BNE   CSH16L                                                           
*                                                                               
CSH16K   MVC   CSHHDG,=C'CLI/PRD/JOB'                                           
         OI    CSHHDGH+6,X'80'                                                  
*                                                                               
CSH16L   OI    CSHSYH+4,X'20'                                                   
         EJECT                                                                  
*              PAYEE VALIDATION                                                 
         SPACE 2                                                                
PAYVAL   LA    R2,CSHACH                                                        
         BAS   RE,ANY                                                           
         MVI   ERRNUM,BADACC                                                    
         SR    R1,R1                                                            
         IC    R1,CSHACH+5                                                      
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),CSHAC                                                   
         CLI   KEY+2,C'P'          DEAL WITH PUB NUMBERS                        
         BE    *+12                                                             
         CLI   KEY+2,C'Q'                                                       
         BNE   CSH17                                                            
         CLI   CSHACH+5,6          ASSUME PRINT REPS ARE LESS                   
         BL    CSH17               THAN 6 LONG                                  
         CLI   CSHAC+9,C'*'        ACCOUNTS CREATED AS OFFICE OVERRIDES         
         BE    CSH17                                                            
         LR    R4,R1                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=C'ACCOUNT',KEY,KEY                    
         CLI   DMCB+8,0                                                         
         BE    CSH17                                                            
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+4(20),SPACES                                                 
         GOTO1 =V(PUBVAL),DMCB,((R4),CSHAC+1),(1,KEY+4),RR=PRELOC               
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
*                                                                               
CSH17    TM    CSHACH+4,X'20'      ANY CHANGES MADE                             
         BO    CSH30               NO                                           
         MVI   CHANGES,1                                                        
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    R2,IOAREA                                                        
         SR    R1,R1                                                            
*                                                                               
CSH18    CLI   0(R2),0                                                          
         BE    CSH26                                                            
         CLI   0(R2),X'30'                                                      
         BE    CSH22                                                            
         CLI   0(R2),X'20'                                                      
         BE    CSH24                                                            
*                                                                               
CSH20    IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     CSH18                                                            
*                                                                               
         USING ACSTATD,R2                                                       
CSH22    LR    R5,RA                                                            
         USING TWAD,R5                                                          
         CLC   TWAAUTH+1(1),ACSTSECY+1     SECURITY                             
         BNL   CSH20                                                            
         MVI   ERRNUM,0                                                         
         OI    DMCB+8,X'04'                                                     
         LA    R2,CSHACH                                                        
         MVC   CSHACNM,SPACES                                                   
         OI    CSHACNMH+6,X'80'                                                 
         B     ERROR                                                            
*                                                                               
         USING ACNAMED,R2                                                       
CSH24    SR    R3,R3                                                            
         IC    R3,ACNMLEN                                                       
         SH    R3,=H'2'                                                         
         MVC   DMCB+4(4),=X'D9000A02'   CHOPPER                                 
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,((R3),ACNMNAME),(30,CSHACNM),1                         
         SR    R1,R1                                                            
         OI    CSHACNMH+6,X'80'                                                 
         B     CSH20                                                            
*        SPACE 1                                                                
CSH26    OI    CSHACH+4,X'20'                                                   
         EJECT                                                                  
*              DATE & OPTIONS                                                   
*                                                                               
CSH30    LA    R2,CSHMOH                                                        
         CLC   CSHMOH+5(1),LASTDATE NEW LENGTH SAME AS LAST LENGTH              
         BNE   CSH30A              NO - MUST HAVE CHANGED                       
         CLI   CSHMOH+5,0          WERE THEY ZERO                               
         BE    CSH30A2             YES - SO DON'T BOTHER                        
         TM    CSHMOH+4,X'20'      SAME NONZERO LENGTH - IS IT NEW?             
         BO    CSH40               NO                                           
*                                                                               
CSH30A   MVI   CHANGES,1                                                        
         MVC   LASTDATE,CSHMOH+5                                                
*                                                                               
CSH30A2  XC    DATE3,DATE3                                                      
         MVI   DATYPE,2                                                         
         CLI   SYSTEM,C'S'                                                      
         BE    CSH30B                                                           
         CLI   SYSTEM,C'U'         NETWORK IS LIKE SPOT                         
         BE    CSH30B                                                           
         CLI   SYSTEM,C'T'                                                      
         BE    CSH30B                                                           
         MVI   DATYPE,0                                                         
*                                                                               
CSH30B   CLI   CSHMOH+5,0                                                       
         BE    CSH34                                                            
         MVI   ERRNUM,BADATE                                                    
         MVI   DATYPE,0                                                         
         GOTO1 DATVAL,DMCB,(0,CSHMO),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   CSH32                                                            
         MVI   DATYPE,2                                                         
         CLI   SYSTEM,C'S'                                                      
         BE    CSH31                                                            
         CLI   SYSTEM,C'U'         NETWORK IS LIKE SPOT                         
         BE    CSH31                                                            
         CLI   SYSTEM,C'T'                                                      
         BNE   ERROR                                                            
*                                                                               
CSH31    GOTO1 (RF),(R1),(2,CSHMO)                                              
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         MVI   WORK+4,C'1'         10TH OF MONTH IS IN RIGHT BRD MONTH          
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+6,RR=PRELOC                          
         MVC   WORK(6),WORK+12     END DATE OF BROAD. MONTH                     
*                                                                               
CSH32    GOTO1 DATCON,DMCB,(0,WORK),(1,DATE3)                                   
CSH34    OI    CSHMOH+4,X'20'                                                   
         EJECT                                                                  
*              FILTERS & OPTIONS                                                
*                                                                               
CSH40    LA    R2,CSHF1H                                                        
         MVI   ERRNUM,INVALID                                                   
         CLC   CSHF1H+5(1),LASTFILT NEW LENGTH SAME AS LAST LENGTH              
         BNE   CSH41               NO - MUST HAVE CHANGED                       
         CLI   CSHF1H+5,0          WERE THEY ZERO                               
         BE    CSH41A              YES - SO DON'T BOTHER                        
         TM    CSHF1H+4,X'20'      ANY CHANGES MADE                             
         BO    CSH41A              NO                                           
*                                                                               
CSH41    MVI   CHANGES,1           YES                                          
         MVC   LASTFILT,CSHF1H+5                                                
*                                                                               
CSH41A   MVI   URGENT,C'N'                                                      
         CLI   CSHF1H+5,0                                                       
         BE    CSH69                                                            
         GOTO1 =V(SCANNER),DMCB,(R2),SCANTAB,RR=PRELOC                          
         SR    R3,R3                                                            
         IC    R3,DMCB+4           NO OF LINES                                  
         LTR   R3,R3                                                            
         BZ    ERROR                                                            
         LA    R4,SCANTAB                                                       
*                                                                               
CSH42    CLC   12(3,R4),=C'CLI'                                                 
         BNE   CSH44                                                            
         MVC   CLIENT,22(R4)                                                    
         B     CSH66                                                            
*                                                                               
CSH44    CLC   12(3,R4),=C'PRO'                                                 
         BNE   CSH46                                                            
         MVC   PRODUCT,22(R4)                                                   
         B     CSH66                                                            
*                                                                               
CSH46    CLC   12(3,R4),=C'STA'                                                 
         BNE   CSH48                                                            
         MVC   STATN,22(R4)                                                     
         B     CSH66                                                            
*                                                                               
CSH48    CLC   12(3,R4),=C'PUB'                                                 
         BNE   CSH50                                                            
         MVC   PUBLEN,1(R4)        LENGTH OF INPUT PUB NO                       
         MVC   PUBLCN(10),22(R4)                                                
         B     CSH66                                                            
*                                                                               
CSH50    CLC   12(3,R4),=C'JOB'                                                 
         BNE   CSH52                                                            
         MVC   JOBNUM,22(R4)                                                    
         B     CSH66                                                            
*                                                                               
CSH52    CLC   12(3,R4),=C'INV'                                                 
         BNE   CSH54                                                            
         MVC   INVLEN,1(R4)        LENGTH OF INPUT INVOICE NO                   
         MVC   INVNO,22(R4)                                                     
         B     CSH66                                                            
*                                                                               
CSH54    CLC   12(6,R4),=C'OFFICE'                                              
         BNE   CSH56                                                            
         MVC   OFFICE,22(R4)                                                    
         B     CSH66                                                            
*                                                                               
CSH56    CLC   12(4,R4),=C'ZONE'                                                
         BNE   CSH58                                                            
         MVC   ZONE,22(R4)                                                      
         MVC   ZLEN,1(R4)                                                       
         B     CSH66                                                            
*                                                                               
CSH58    CLC   12(3,R4),=C'EDITION'                                             
         BNE   CSH59                                                            
         MVC   EDITION,22(R4)                                                   
         MVC   EDLEN,1(R4)                                                      
         B     CSH66                                                            
*                                                                               
CSH59    CLC   12(3,R4),=C'CHECK'                                               
         BNE   CSH60                                                            
         MVC   CHECKNO,22(R4)                                                   
         B     CSH66                                                            
*                                                                               
CSH60    CLC   12(3,R4),=C'URGENT'                                              
         BNE   CSH61                                                            
         MVI   URGENT,C'Y'                                                      
         B     CSH66                                                            
*                                                                               
CSH61    CLC   12(3,R4),=C'AUTHORISED'                                          
         BNE   CSH62                                                            
         MVC   AUTH,22(R4)                                                      
         CLI   AUTH,C'Y'                                                        
         BE    CSH66                                                            
         CLI   AUTH,C'N'                                                        
         BE    CSH66                                                            
         B     ERROR                                                            
*                                                                               
CSH62    CLC   12(3,R4),=C'APPROVED'                                            
         BNE   CSH63                                                            
         MVC   APPR,22(R4)                                                      
         CLI   APPR,C'Y'                                                        
         BE    CSH66                                                            
         CLI   APPR,C'N'                                                        
         BE    CSH66                                                            
         B     ERROR                                                            
*                                                                               
CSH63    CLC   12(3,R4),=C'SUBREF'                                              
         BNE   CSH64               NO VALID FILTER FOUND                        
         MVC   SUBREF,22(R4)                                                    
         MVC   SUBRLEN,1(R4)       LENGTH OF INPUT                              
         B     CSH66                                                            
*                                                                               
CSH64    CLC   12(3,R4),=C'HELD'   HELD FILTER                                  
         BNE   CSH65                                                            
         MVC   HELD,22(R4)                                                      
         CLI   HELD,C'Y'                                                        
         BE    CSH66                                                            
         CLI   HELD,C'N'                                                        
         BE    CSH66                                                            
         B     ERROR                                                            
*                                                                               
CSH65    CLC   12(3,R4),=C'MARKED' MARKED FILTER                                
         BNE   ERROR                                                            
         CLI   CSHOP,C'U'          ONLY FOR UNDISBURSED                         
         BNE   ERROR                                                            
         CLI   22(R4),C'O'         INCLUDE OFFSETS                              
         BNE   *+12                                                             
         OI    MARK,MOFF                                                        
         B     CSH66                                                            
         CLI   22(R4),C'R'         INCLUDE REVERSED                             
         BNE   *+12                                                             
         OI    MARK,MREV                                                        
         B     CSH66                                                            
         CLI   22(R4),C'B'         INCLUDE BOTH                                 
         BNE   ERROR                                                            
         OI    MARK,MOFF+MREV                                                   
*                                                                               
CSH66    LA    R4,32(R4)                                                        
         BCT   R3,CSH42                                                         
         CLI   PUBLCN,0                                                         
         BE    CSH69                                                            
         OC    PUBLCN,SPACES                                                    
         LA    R4,PUBLCN           BUILD AN EXPRESSION FOR PUBVAL               
         IC    R3,PUBLEN                                                        
         AR    R4,R3                                                            
         CLI   ZONE,0                                                           
         BE    CSH67                                                            
         MVI   PUBLEN,X'FF'                                                     
         CLC   ZONE,=C'ALL'                                                     
         BNE   *+14                                                             
         MVC   PUBLCN+8(3),=C'ZZZ'                                              
         B     CSH69                                                            
         MVI   0(R4),C','                                                       
         IC    R3,ZLEN                                                          
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),ZONE                                                     
         LA    R4,2(R3,R4)         READY FOR NEXT COMMA                         
*                                                                               
CSH67    CLI   EDITION,0                                                        
         BE    CSH68                                                            
         MVI   0(R4),C','                                                       
         IC    R3,EDLEN                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),EDITION                                                  
         MVI   PUBLEN,X'FF'                                                     
*                                                                               
CSH68    LA    R4,PUBLCN+14                                                     
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         LA    R3,PUBLCN                                                        
         SR    R4,R3                                                            
         LA    R4,1(R4)                                                         
         GOTO1 =V(PUBVAL),DMCB,((R4),PUBLCN),(1,WORK),RR=PRELOC                 
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVC   PUBLCN,SPACES                                                    
         MVC   PUBLCN(11),WORK                                                  
         LH    R0,=Y(L'PUBLCN)       ADJUST LENGTH IF NEEDED                    
         LA    R1,PUBLCN+L'PUBLCN-1  LAST BYTE OF PUBLCN                        
         CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         DC    H'0'                                                             
         STC   R0,PUBLEN             SAVE LENGTH                                
*                                                                               
CSH69    OI    CSHF1H+4,X'20'                                                   
*                                                                               
CSH70    LA    R2,CSHOPH           OPTIONS                                      
         MVC   SAVEOPTN,CSHOP                                                   
         CLC   CSHOPH+5(1),LASTOPT NEW LENGTH SAME AS LAST LENGTH               
         BNE   CSH71               NO - MUST HAVE CHANGED                       
         CLI   CSHOPH+5,0          WERE THEY ZERO                               
         BE    CSH72               YES - SO DON'T BOTHER                        
         TM    CSHOPH+4,X'20'      NOT 0 LENGTH - WAS IT CHANGED                
         BO    CSH72               NO                                           
*                                                                               
CSH71    MVI   CHANGES,1                                                        
         MVC   LASTOPT,CSHOPH+5                                                 
         CLI   CHECKNO,0           IF CHECK NUMBER FILTER,                      
         BE    CSH72                                                            
         CLI   CSHOP,C'D'          MUST BE DISBURSED                            
         BE    *+8                                                              
         B     ERROR                                                            
*                                                                               
CSH72    OI    CSHOPH+4,X'20'                                                   
         CLI   SAVEOPTN,C'U'       UNDISBURSED                                  
         BE    CSH100                                                           
         CLI   SAVEOPTN,C'D'       DISBURSED                                    
         BE    CSH100                                                           
         CLI   CSHOPH+5,0          BOTH                                         
         BE    *+12                                                             
         NI    CSHOPH+4,X'DF'      INVALID FIELD                                
         B     ERROR                                                            
         MVI   SAVEOPTN,0                                                       
         B     CSH100                                                           
         EJECT                                                                  
*              DISBURSED/UNDISBURSED/BOTH OPTION                                
*                                                                               
CSH100   CLI   CHANGES,1           WERE ANY CHANGES MADE                        
         BNE   *+10                NO, USE OLDKEY                               
         MVC   OLDKEY,KEY                                                       
         MVC   KEY,OLDKEY                                                       
         NI    BYTE,255-READDONE                                                
         BAS   RE,HIGH                                                          
         B     CSH104                                                           
*                                                                               
CSH102   BAS   RE,SEQ                                                           
CSH104   CLC   KEY(15),KEYSAVE                                                  
         BE    *+12                                                             
         OI    BYTE,READDONE                                                    
         B     CSH120                                                           
*                                                                               
         LA    R4,IOAREA                                                        
         CLI   0(R4),TRNELQ                                                     
         BNE   CSH102                                                           
*                                                                               
         BAS   RE,CHKDRFT          IS THIS A DRAFT                              
         BE    CSH102              YES                                          
*                                                                               
         BAS   RE,WANTIT           FILTER THIS TRANSACTION                      
         BNE   CSH102                                                           
*                                                                               
         USING TRNELD,R4                                                        
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BNO   CSH106                                                           
         CLC   CRKEY,KEY           TEST WITHIN CURRENT CREDIT RANGE             
         BNE   CSH102                                                           
         BAS   RE,PUTDR            POST DEBIT TO INVTAB, IF REQUIRED            
         B     CSH102                                                           
*                                                                               
CSH106   LA    R5,INVTAB           ADD ENTRY TO INVOICE TABLE                   
         USING INVTABD,R5                                                       
         LA    R0,INVTABN                                                       
         OC    INVTABD(INVTABL),INVTABD                                         
         BZ    CSH108                                                           
         LA    R5,INVTABL(R5)                                                   
         BCT   R0,*-14                                                          
         MVC   KEYSAVE,KEY         TABLE FULL - SAVE THIS CREDIT KEY            
         B     CSH110              PROCESS ANY FURTHER DEBITS                   
*                                                                               
CSH108   MVC   INVKEY(INVKEY2L),KEY+(TRNKOFF-TRNRECD)                           
         MVI   INVIND1,0                                                        
         TM    STAL+(TRSSTAT-TRSELD),TRSSVOID                                   
         BZ    *+8                                                              
         OI    INVIND1,INVIM0BV    SET MARKER BANK/VOID                         
         GOTO1 LHBUILD,INVTABD                                                  
         MVC   CRKEY,KEY                                                        
         B     CSH102                                                           
*                                                                               
CSH110   BAS   RE,SEQ              READ FOR ANY FURTHER RELEVANT DEBITS         
         CLC   CRKEY,KEY           TEST WE REACHED THE END OF THIS KEY          
         BNE   CSH120                                                           
*                                                                               
         LA    R4,IOAREA                                                        
         CLI   TRNEL,TRNELQ        TEST TRANSACTION                             
         BNE   CSH110                                                           
*                                                                               
         BAS   RE,CHKDRFT          IS THIS A DRAFT                              
         BE    CSH110              YES, GET NEXT                                
*                                                                               
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BZ    CSH110                                                           
         BAS   RE,GTLM             SET ELEMENT ADDRESSES                        
         BAS   RE,PUTDR            POST TO RELEVANT CREDIT ENTRY                
         B     CSH110                                                           
*                                                                               
         USING INVTABD,R5                                                       
CSH120   CLI   CHECKNO,0           TEST CHEQUE NUMBER FILTER PRESENT            
         BE    CSH140                                                           
         LA    R5,INVTAB                                                        
         LA    R0,INVTABN                                                       
CSH122   OC    INVTABD(INVTABL),INVTABD                                         
         BZ    CSH130                                                           
         CLC   INVCHQ,CHECKNO      TEST CHEQUE NUMBER MATCHES                   
         BE    CSH124                                                           
         LR    RF,R5               NO - DROP THIS ENTRY AND SHUFFLE             
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         LTR   RE,RE               TEST LAST ENTRY                              
         BNZ   *+14                                                             
         XC    0(INVTABL,RF),0(RF) CLEAR IT OUT                                 
         B     CSH130                                                           
         MVC   0(INVTABL,RF),INVTABL(RF)                                        
         LA    RF,INVTABL(RF)                                                   
         BCT   RE,*-10                                                          
         XC    0(INVTABL,RF),0(RF) CLEAR LAST ENTRY                             
         B     CSH122                                                           
CSH124   LA    R5,INVTABL(R5)      NEXT ENTRY                                   
         BCT   R0,CSH122                                                        
*                                                                               
CSH130   TM    BYTE,READDONE       TEST ENQUIRY COMPLETE                        
         BO    CSH140                                                           
         LA    R5,INVTAB+(INVTABL*(INVTABN-1))                                  
         OC    0(INVTABL,R5),0(R5) TEST ROOM FOR MORE                           
         BNZ   CSH140                                                           
         MVC   KEY,KEYSAVE         RESTORE CREDIT KEY                           
         BAS   RE,HIGH             RE-ESTABLISH SEQUENTIAL PATH                 
         CLC   KEY,KEYSAVE                                                      
         BE    CSH106              NOW PROCESS THE CREDIT AGAIN                 
         DC    H'0'                                                             
*                                                                               
CSH140   LA    R2,CSHLNEH          R2=A(FIRST SCREEN LINE)                      
         LA    R5,INVTAB           SET CHEQUE NUMBERS FROM INVTAB               
         USING INVTABD,R5                                                       
         LA    R0,INVTABN                                                       
CSH142   OC    INVTABD(INVTABL),INVTABD                                         
         BZ    CSH146                                                           
         AP    TOTCD,INVCD                                                      
         AP    TOTNET,INVNET                                                    
         MVC   L'CSHLNEH(L'CSHLNE,R2),INVLN                                     
         OI    6(R2),X'80'                                                      
         TM    INVIND1,INVIM0BV    TEST INVOICE IS/WAS MARKER BANK/VOID         
         BZ    *+12                                                             
         MVI   L'CSHLNEH+59(R2),C'+'                                            
         B     CSH144                                                           
         TM    INVIND1,INVIB37V    TEST BT37 VOID                               
         BZ    CSH144                                                           
         MVI   L'CSHLNEH+59(R2),C'*'                                            
         MVI   L'CSHLNEH+66(R2),C'*'                                            
CSH144   LA    R5,INVTABL(R5)                                                   
         LA    R2,L'CSHLNEH+L'CSHLNE(R2)                                        
         BCT   R0,CSH142                                                        
*                                                                               
CSH146   TM    BYTE,READDONE       TEST ENQUIRY IS COMPLETE                     
         BNZ   CSH200                                                           
         MVC   KEY,KEYSAVE         RESTORE LATEST CREDIT KEY                    
         BAS   RE,MORTOCUM                                                      
         B     CSH210                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
CSH200   MVC   CSHMSG,=CL60'INQUIRY COMPLETE - ENTER NEXT'                      
*                                                                               
CSH202   CLI   0(R2),L'CSHLNEH+L'CSHLNE                                         
         BL    CSH208                                                           
         CLC   L'CSHLNEH(L'CSHLNE,R2),SPACES                                    
         BE    CSH206                                                           
         MVC   L'CSHLNEH(L'CSHLNE,R2),SPACES                                    
         OI    6(R2),X'80'                                                      
*                                                                               
CSH206   SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     CSH202                                                           
*                                                                               
CSH208   LA    R2,CSHSYH                                                        
         NI    4(R2),X'FF'-X'20'   FORCE SYSTEM VALIDATION                      
*                                                                               
CSH210   EDIT  TOTCD,(11,CSHTOT),2,MINUS=YES                                    
         LA    R3,CSHTOT+13                                                     
         EDIT  TOTNET,(11,0(R3)),2,MINUS=YES                                    
         CLI   SYSTEM,C'S'                                                      
         BE    CSH212                                                           
         CLI   SYSTEM,C'U'         NETWORK IS LIKE SPOT                         
         BE    CSH212                                                           
         CLI   SYSTEM,C'T'                                                      
         BNE   *+10                                                             
*                                                                               
CSH212   MVC   CSHTOT(11),SPACES                                                
         OI    CSHTOTH+6,X'80'                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* POST A DEBIT TO RELEVANT INVOICE TABLE ENTRY                        *         
*                                                                     *         
* NTRY - R4=A(TRNEL)                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R4           R4=A(TRNEL)                                  
PUTDR    NTR1  ,                                                                
         LA    R2,KEY              R2=A(DEBIT TRANSACTION KEY)                  
         USING TRNRECD,R2                                                       
         LA    R3,MPYL             R3=A(DEBIT MPYEL)                            
         USING MPYELD,R3                                                        
         SR    R1,R1                                                            
         IC    R1,TRNKSBR          DEBIT TRNKSBR-1 IS CREDIT TRNKSBR            
         BCTR  R1,0                                                             
         CLI   MPYLN,MPYLN2Q       TEST DEBIT MPYSUB PRESENT                    
         BL    PUTDR02                                                          
         IC    R1,MPYSUB           DEBIT MPYSUB IS CREDIT TRNKSBR               
*                                                                               
PUTDR02  LA    R5,INVTAB           R5=A(INVOICE TABLE)                          
         USING INVTABD,R5                                                       
         LA    R0,INVTABN                                                       
PUTDR04  CLC   INVKEY(INVKEY1L),TRNKOFF                                         
         BNE   *+12                                                             
         CLM   R1,1,INVKSBR        TEST V INVOICE KEY SUB-REFERENCE             
         BE    PUTDR06                                                          
         LA    R5,INVTABL(R5)      BUMP TO NEXT INVTAB ENTRY                    
         BCT   R0,PUTDR04                                                       
         CLI   MPYLN,MPYLN2Q       TEST USING DEBIT MPYSUB                      
         BNL   PUTDRX              THAT'S ALL FOLKS                             
         SH    R1,=H'1'            REDUCE SUB-REFERENCE                         
         BNM   PUTDR02             TRY AGAIN                                    
         B     PUTDRX                                                           
*                                                                               
PUTDR06  NI    BYTE,255-OLDDEBIT   CLEAR OLD DEBIT BIT                          
         LA    RF,STAL             RF=A(DEBIT TRSEL)                            
         USING TRSELD,RF                                                        
         CLC   TRSDATE,INVADAT     TEST EARLIER THAN LATEST DEBIT               
         BNL   *+12                                                             
         OI    BYTE,OLDDEBIT       YES - SET OLD DEBIT FOUND                    
         B     PUTDR08                                                          
         MVC   INVADAT,TRSDATE     NO - SET LATEST DEBIT DATE ADDED             
         DROP  RF                                                               
         CLC   MPYNO,SPACES                                                     
         BNH   *+10                                                             
         MVC   INVCHQ,MPYNO                                                     
         OC    MPYDTE,MPYDTE                                                    
         BZ    PUTDR08                                                          
         GOTO1 DATCON,DMCB,(2,MPYDTE),(8,INVCDT)                                
         B     PUTDR08                                                          
*                                                                               
PUTDR08  LA    R3,STAL             R3=A(DEBIT TRSEL)                            
         USING TRSELD,R3                                                        
         TM    BYTE,OLDDEBIT       TEST OLD DEBIT BEING PROCESSED               
         BNZ   PUTDR09                                                          
         CLI   TRSMARK,TRSMBVQ     TEST MARKER BANK/VOID                        
         BE    *+12                                                             
         CLI   TRSMARK,TRSMSBVQ    OR SUBSIDIARY BANK/VOID                      
         BNE   *+16                                                             
         MVC   INVCHQ,SPACES       CLEAR CHEQUE NUMBER                          
         MVC   INVCDT,SPACES       CLEAR CHEQUE DATE                            
PUTDR09  NI    TRSMARK,255-TRSMUMQ CLEAR NEGATIVE ACTION BIT                    
         CLI   TRSMARK,TRSMBVQ     TEST VOID/UNVOID                             
         BE    *+12                                                             
         CLI   TRSMARK,TRSMSBVQ    OR SUBSIDIARY VOID/UNVOID                    
         BNE   PUTDR10                                                          
         OI    INVIND1,INVIM0BV    SET INVOICE IS/WAS MARKER BANK/VOID          
         B     PUTDRX                                                           
*                                                                               
PUTDR10  CLI   TRNTYPE,X'81'       TEST CHEQUE                                  
         BNE   PUTDRX                                                           
         TM    BYTE,OLDDEBIT       TEST OLD DEBIT BEING PROCESSED               
         BNZ   *+16                                                             
         MVC   INVCHQ,TRNNARR                                                   
         MVC   INVCDT,TRNNARR+6                                                 
         CLI   TRNNARR+40,C'V'     TEST BT37 VOID                               
         BNE   PUTDRX                                                           
         OI    INVIND1,INVIB37V    SET BT37 VOID                                
*                                                                               
PUTDRX   B     XIT                                                              
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* SET ELEMENT ADDRESSES AND FILTER A TRANSACTION                      *         
*                                                                     *         
* NTRY - R4=A(TRNEL)                                                  *         
* EXIT - CC EQU IF TRANSACTION PASSES                                 *         
*        CC NEQ IF TRANSACTION FAILS                                  *         
***********************************************************************         
         SPACE 1                                                                
WANTIT   NTR1  ,                                                                
         USING TRANSD,R4                                                        
         BAS   RE,GTLM             DIG OUT SOME ELEMENTS                        
         TM    TRNSSTAT,TRNSDR     TEST DEBIT                                   
         BO    WANTYX              THAT WILL DO                                 
*                                                                               
         LA    R5,KEY              FILTER CREDIT TRANSACTION FURTHER            
         USING ACKEYD,R5                                                        
         MVI   ITMST,0             ITEM STATUS                                  
*                                                                               
         USING TRSELD,R6                                                        
WT1A     LA    R6,STAL             TRANSACTION STATUS                           
         CLI   0(R6),0             TEST 60 ELEMENT                              
         BE    WT1B                                                             
         TM    TRSSTAT,TRSSOFFS    MARKED AS OFFSET                             
         BNO   *+8                 NO                                           
         OI    ITMST,MOFF                                                       
*                                                                               
WT1B     MVC   PAYTYPE,TRNSTYPE                                                 
         TM    TRNSSTAT,TRNSREV    IF MARKED AS A REVERSAL                      
         BZ    *+8                                                              
         OI    ITMST,MREV                                                       
*                                                                               
WT2      CLI   SAVEOPTN,C'D'       DISBURSED ONLY                               
         BNE   WT4                                                              
         TM    ITMST,MREV+MOFF     EXCLUDE REVERSED AND OFFSET                  
         BNZ   WANTNX                                                           
         OC    ACDTUSED,ACDTUSED   AND UNDISBURSED                              
         BZ    WANTNX                                                           
         B     WT6                                                              
*                                  UNDISBURSED                                  
WT4      CLI   SAVEOPTN,C'U'       ONLY WANT UNDISBURSED                        
         BNE   WT6                                                              
         CLI   ITMST,0             TEST ITEM STATUS                             
         BNE   WT5                 REVERSED OR OFFSET                           
         OC    ACDTUSED,ACDTUSED   IS ITEM PAID                                 
         BNZ   WANTNX              BRANCH IF PAID                               
         B     WT6                                                              
*                                                                               
WT5      TM    ITMST,MREV          ITEM REVERSED                                
         BZ    *+12                                                             
         TM    MARK,MREV           INCLUDE REVERSED ?                           
         BNO   WANTNX                                                           
         TM    ITMST,MOFF          ITEM OFFSET                                  
         BZ    *+12                                                             
         TM    MARK,MOFF           INCLUDE OFFSET ?                             
         BNO   WANTNX                                                           
*                                                                               
WT6      CLI   CLIENT,0            CLIENT                                       
         BE    WT12                                                             
         CLI   ACKEYACC+2,C'P'                                                  
         BE    WT8                                                              
         CLI   ACKEYACC+2,C'S'                                                  
         BE    WT8                                                              
         CLI   ACKEYACC+2,C'Q'                                                  
         BE    WT8                                                              
         CLI   ACKEYACC+2,C'T'                                                  
         BE    WT8                                                              
         CLI   ACKEYACC+2,C'U'                                                  
         BE    WT8                                                              
         CLC   CLIENT,ACKEYCON+3   MUST BE PRODN/EXPENSE                        
         BE    WT12                                                             
         B     WANTNX                                                           
*                                                                               
WT8      CLC   CLIENT(3),ACKEYCON+12                                            
         BNE   WANTNX                                                           
*                                                                               
WT12     CLI   PRODUCT,0           PRODUCT                                      
         BE    WT18                                                             
         CLI   ACKEYACC+2,C'X'     EXPENSES DONT HAVE PRODUCTS                  
         BE    WT18                                                             
         CLI   ACKEYACC+2,C'Y'                                                  
         BE    WT18                                                             
         CLI   ACKEYACC+2,C'S'                                                  
         BE    WT14A                                                            
         CLI   ACKEYACC+2,C'U'                                                  
         BE    WT14A                                                            
         CLI   ACKEYACC+2,C'P'                                                  
         BE    WT14                                                             
         CLI   ACKEYACC+2,C'Q'                                                  
         BE    WT14                                                             
         CLI   ACKEYACC+2,C'T'                                                  
         BNE   WT15                MUST BE PRODUCTION                           
*                                                                               
WT14     CLC   PRODUCT(3),TRNSREF  PRODUCT IS IN FIRST 3 BYTES                  
         BNE   WANTNX              OF TRANSACTION REFERENCE                     
         B     WT18                                                             
*                                                                               
WT14A    LA    R6,PAYL             FOR SPOT-PROD IN 46 EL OR TRNSREF            
         USING TRPAYD,R6                                                        
         LA    R7,TRPYPROD+4       BUMP PAST "POL="                             
         CLC   TRPYPROD(3),=C'POL' IF IN 46 USE THAT                            
         BE    WT14B                                                            
         LA    R7,TRNSREF          IF NOT USE TRNSREF                           
*                                                                               
WT14B    CLC   PRODUCT(3),0(R7)                                                 
         BNE   WANTNX                                                           
         B     WT18                                                             
*                                                                               
WT15     LA    R6,OTHL                                                          
         CLC   PRODUCT,02(R6)                                                   
         BE    WT18                                                             
         B     WANTNX                                                           
*                                                                               
WT18     CLI   STATN,0                                                          
         BE    WT20                                                             
         CLC   STATN,ACKEYCON+4                                                 
         BE    WT20                                                             
         B     WANTNX                                                           
*                                                                               
WT20     CLI   PUBLCN,0                                                         
         BE    WT24                                                             
         CLI   PUBLEN,X'FF'        STRAIGHT PUB,OR DOES IT HAVE                 
         BE    WT22                EDITION & ZONE                               
         SR    R6,R6                                                            
         IC    R6,PUBLEN                                                        
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   PUBLCN(0),ACKEYCON+1                                             
         BNE   WANTNX                                                           
         B     WT24                                                             
*                                                                               
WT22     CLC   PUBLCN(11),ACKEYCON+1    THIS ONE HAS ZONE ETC                   
         BNE   WANTNX                                                           
         B     WT24                                                             
*                                                                               
WT24     OC    DATE3,DATE3         DATES                                        
         BZ    WT26                                                             
         CLI   ACKEYACC+2,C'S'                                                  
         BE    WT25                                                             
         CLI   ACKEYACC+2,C'U'                                                  
         BE    WT25                                                             
         CLI   ACKEYACC+2,C'T'                                                  
         BE    WT25                                                             
         CLC   TRNSDATE,DATE3                                                   
         BL    WANTNX                                                           
         B     WT26                                                             
*                                                                               
WT25     CLI   DATYPE,2                                                         
         BNE   WT25A                                                            
         CLC   TRNSDATE,DATE3      IF DATE IS YY/MM FOR SPOT                    
         BNE   WANTNX              WE ONLY WANT EXACT MATCHES                   
         B     WT28                                                             
*                                                                               
WT25A    CLC   TRNSDATE,DATE3      IF DATE IS DD/MM/YY                          
         BL    WANTNX              LOGIC IS SAME AS OTHER LEDGERS               
         B     WT28                                                             
*                                                                               
WT26     CLI   JOBNUM,0                                                         
         BE    WT28                                                             
         CLI   ACKEYACC+2,C'V'                                                  
         BNE   WT28                                                             
         LA    R6,OTHL                                                          
         CLC   JOBNUM,8(R6)                                                     
         BE    WT28                                                             
         B     WANTNX                                                           
*                                                                               
WT28     CLI   INVNO,0             INVOICE NUMBER                               
         BE    WT32                                                             
         LA    R7,ACKEYREF                                                      
         CLI   ACKEYACC+2,C'V'     FOR PRODN & EXPENSE INVOICE                  
         BE    WT30                NO IS IN THE KEY                             
         CLI   ACKEYACC+2,C'X'                                                  
         BE    WT30                                                             
         CLI   ACKEYACC+2,C'W'                                                  
         BE    WT30                                                             
         CLI   ACKEYACC+2,C'Y'                                                  
         BE    WT30                                                             
         USING TRPAYD,R6                                                        
         LA    R6,PAYL             FOR SPOT & PRINT - IN 46 ELEMENT             
         LA    R7,TRPYINV                                                       
         DROP  R6                                                               
*                                                                               
WT30     SR    R6,R6                                                            
         IC    R6,INVLEN                                                        
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   INVNO(0),0(R7)                                                   
         BE    WT32                                                             
         B     WANTNX                                                           
*                                                                               
WT32     CLI   OFFICE,0            OFFICE                                       
         BE    WT34                                                             
         CLC   OFFICE,TRNSANAL                                                  
         BNE   WANTNX                                                           
*                                                                               
WT34     CLI   URGENT,C'Y'                                                      
         BNE   WT36                                                             
         TM    TRNSSTAT,X'40'                                                   
         BZ    WANTNX                                                           
*                                                                               
WT36     CLI   APPR,0                                                           
         BE    WT38                                                             
         CLI   APPR,C'Y'                                                        
         BNE   WT37                                                             
         OC    ACDTUSED,ACDTUSED   IF IT'S PAID SKIP IT                         
         BNZ   WANTNX                                                           
         TM    TRNSSTAT,X'02'      WANT APPROVED ONLY                           
         BNO   WANTNX              NOT APPROVED - SKIP                          
         B     WT38                                                             
*                                                                               
WT37     OC    ACDTUSED,ACDTUSED   IS IT PAID                                   
         BNZ   WT38                IF PAID KEEP IT                              
         TM    TRNSSTAT,X'02'      WANT UNAPPROVED ONLY                         
         BO    WANTNX              SKIP IT NOT PAID AND NOT APPROVED            
         B     WT38                                                             
*                                                                               
WT38     CLI   AUTH,0                                                           
         BE    WT40                                                             
         OC    ACDTUSED,ACDTUSED                                                
         BNZ   WANTNX                                                           
         CLI   AUTH,C'Y'                                                        
         BE    *+16                                                             
         TM    TRNSSTAT,X'08'      WANT UNAUTHORISED ONLY                       
         BO    WANTNX                                                           
         B     WT40                                                             
         TM    TRNSSTAT,X'08'      WANT AUTHORISED ONLY                         
         BZ    WANTNX                                                           
*                                                                               
WT40     CLI   HELD,0              FILTER FOR HELD ITEMS                        
         BE    WT45                                                             
         CLI   HELD,C'Y'                                                        
         BNE   WT42                                                             
         OC    ACDTUSED,ACDTUSED   NO PAID ITEMS                                
         BNZ   WANTNX                                                           
         TM    TRNSSTAT,X'04'                                                   
         BNO   WANTNX              SKIP HELD                                    
         B     WT45                                                             
*                                                                               
WT42     OC    ACDTUSED,ACDTUSED   IS IT PAID ** HELD=NO                        
         BNZ   WT45                IF PAID KEEP IT                              
         TM    TRNSSTAT,X'04'      IF HELD                                      
         BO    WANTNX              SKIP IT                                      
*                                                                               
WT45     CLI   SUBREF,0                                                         
         BE    WANTYX                                                           
         LA    R6,OTHL                                                          
         SR    RF,RF                                                            
         IC    RF,SUBRLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SUBREF(0),2(R6)                                                  
         BE    WANTYX                                                           
*                                                                               
WANTNX   LTR   RB,RB               EXCLUDE THIS TRANSACTION                     
         B     XIT                                                              
*                                                                               
WANTYX   CR    RB,RB               INCLUDE THIS TRANSACTION                     
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK IF A TRAN IS A DRAFT    RETURN CC EQ IF TRUE                            
***********************************************************************         
CHKDRFT  NTR1                      FILTER DRAFT TRANSACTIONS                    
         USING ACKEYD,R5                                                        
         LA    R5,KEY                                                           
         TM    ACSTATUS,TRNSDRFT   IS IT A DRAFT?                               
         BZ    CHKDNEQ             NO  RETURN FALSE                             
*                                                                               
         CR    RB,RB               RETURN EQ IF DRAFT TRAN                      
         B     XIT                                                              
*                                                                               
CHKDNEQ  LTR   RB,RB               NOT A DRAFT                                  
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD SCREEN LINE IN INVOICE TABLE                                  *         
* NTRY - R1=A(INVTAB ENTRY)                                           *         
*        R4=A(TRNEL)                                                  *         
***********************************************************************         
         SPACE 1                                                                
LHBUILD  NTR1  ,                                                                
         LR    R2,R1                                                            
         USING INVTABD,R2                                                       
         USING TRANSD,R4                                                        
         ZAP   INVCD,=P'0'                                                      
         ZAP   INVNET,TRNSAMNT                                                  
         MVC   INVLN,SPACES                                                     
         SPACE 1                                                                
         LA    R5,KEY                                                           
         USING ACKEYACC,R5         BUILD CLIENT & PRODUCT                       
         MVC   INVLN(6),ACKEYCON+3                                              
         MVC   WORK(20),SPACES                                                  
         LA    R6,OTHL             PROD FOR PRODUCTION                          
         CLI   ACKEYACC+2,C'X'                                                  
         BE    LH4                                                              
         CLI   ACKEYACC+2,C'Y'                                                  
         BE    LH4                                                              
         CLI   ACKEYACC+2,C'V'     MUST BE SPOT OR PRINT IF LOW                 
         BL    LH2                                                              
         MVC   WORK(6),ACKEYCON+3                                               
         MVC   WORK+7(6),OTHL+2    PRODUCT                                      
         MVC   WORK+14(6),OTHL+8   JOB                                          
*                                                                               
LH1C     MVC   DMCB+4(4),=X'D9000A0D'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB             CALL SQUASHER                                
         GOTO1 (RF),DMCB,WORK,20                                                
         MVC   INVLN(14),WORK                                                   
         B     LH4                                                              
*                                                                               
LH2      MVC   INVLN+3(3),SPACES                                                
         MVC   INVLN(3),ACKEYCON+12  CLI FOR PRINT/SPOT                         
         MVC   INVLN+7(3),TRNSREF    PROD FOR PRINT/SPOT                        
         MVC   INVLN+10(3),SPACES                                               
         CLI   ACKEYACC+2,C'P'                                                  
         BE    LH4                                                              
         CLI   ACKEYACC+2,C'Q'                                                  
         BE    LH4                                                              
         LA    R6,PAYL                                                          
         USING TRPAYD,R6                                                        
         CLC   TRPYPROD(3),=C'POL'                                              
         BNE   LH4                                                              
         MVC   INVLN+7(3),TRPYPROD+4                                            
*                                                                               
LH4      GOTO1 DATCON,DMCB,(1,TRNSDATE),(8,INVLN+15)                            
         MVC   INVLN+24(6),TRNSREF                                              
         EDIT  TRNSAMNT,(11,INVLN+47),2,MINUS=YES                               
         LA    R6,PAYL                                                          
         CLI   ACKEYACC+2,C'P'                                                  
         BE    LH8                                                              
         CLI   ACKEYACC+2,C'S'                                                  
         BE    LH8                                                              
         CLI   ACKEYACC+2,C'U'                                                  
         BE    LH8                                                              
         CLI   ACKEYACC+2,C'Q'                                                  
         BE    LH8                                                              
         CLI   ACKEYACC+2,C'T'                                                  
         BE    LH8                                                              
         LA    R6,CSHL                                                          
         B     LH10                                                             
*                                                                               
         USING TRPAYD,R6                                                        
LH8      OC    TRPYCD,TRPYCD       WAS THERE ONE                                
         BZ    LH14                                                             
         CLI   SYSTEM,C'S'         NO CD FOR SPOT                               
         BE    LH8A                                                             
         CLI   SYSTEM,C'U'         NO CD FOR NETWORK                            
         BE    LH8A                                                             
         CLI   SYSTEM,C'T'                                                      
         BE    LH8A                                                             
         EDIT  TRPYCD,(11,INVLN+34),2,MINUS=YES                                 
         MVC   INVLN+24(11),TRPYINV                                             
         B     LH8C                                                             
*                                                                               
LH8A     MVC   INVLN+33(6),TRPYPER                                              
         MVI   INVLN+39,C'-'                                                    
         MVC   INVLN+40(6),TRPYPER+6                                            
         MVC   INVLN+24(9),TRPYINV                                              
*                                                                               
LH8C     ZAP   INVCD,TRPYCD                                                     
         CLI   ACKEYACC+2,C'S'     SPOT-SHOW MOS MONTH/YR ONLY                  
         BE    LH9                                                              
         CLI   ACKEYACC+2,C'U'     AND NETWORK                                  
         BE    LH9                                                              
         CLI   ACKEYACC+2,C'T'                                                  
         BNE   LH9A                                                             
*                                                                               
LH9      CLI   DATYPE,2            UNLESS THEY INPUT DD/MM/YY                   
         BNE   LH14                                                             
         MVC   INVLN+15(8),SPACES                                               
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(9,INVLN+15)                            
         B     LH14                                                             
*                                                                               
LH9A     CLC   TRPYPER(6),SPACES                                                
         BNH   LH14                                                             
         CLC   TRPYPER+4(2),=C'00' IF PRINT-SHOWDD/MM/YY IF PRESENT             
         BE    LH9B                IN PAY PERIOD FIELD                          
         GOTO1 DATCON,DMCB,(0,TRPYPER),(8,INVLN+15)                             
         B     LH14                                                             
*                                                                               
LH9B     MVC   INVLN+15(8),SPACES OR JUST YY/MM                                 
         GOTO1 DATCON,DMCB,(0,TRPYPER),(9,INVLN+15)                             
         B     LH14                                                             
*                                                                               
         USING TRCASHD,R6                                                       
LH10     CLI   TRCASHD,0                                                        
         BE    LH14                                                             
         ZAP   INVCD,TRCSAMNT                                                   
         EDIT  (P6,TRCSAMNT),(8,INVLN+37),2,MINUS=YES                           
*                                                                               
LH14     TM    ITMST,MREV+MOFF                                                  
         BNZ   *+14                                                             
         OC    ACDTUSED,ACDTUSED   EXIT IF NOT USED                             
         BZ    LH27                                                             
         LA    R6,ACRECORD                                                      
*                                                                               
LH17     CLI   0(R6),0             ELSE LOOK FOR A X'64'ELEMENT                 
         BE    LH27                                                             
         CLI   0(R6),X'64'                                                      
         BE    LH21                                                             
         CLI   0(R6),X'60'                                                      
         BE    LH23                                                             
*                                                                               
LH19     SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     LH17                                                             
*                                                                               
         USING MPYELD,R6                                                        
LH21     CLC   MPYNO,SPACES        IS IT A REAL CHEQUE NUMBER                   
         BE    LH19                AND FILL IN RIGHT SIDE OF SCREEN             
         CLI   INVLN+58,C'R'       ALREADY REVERSED                             
         BNE   *+12                                                             
*        CLC   INVLN+60(6),=C'RVERSE' ALREADY REVERSED                          
*        BNE   *+12                                                             
         MVI   INVLN+59,C'*'       SURROUND WITH STARS                          
         MVI   INVLN+66,C'*'                                                    
         MVC   INVCHQ,MPYNO                                                     
         GOTO1 DATCON,DMCB,(2,MPYDTE),(8,INVCDT)                                
         B     LH19                                                             
*                                                                               
         USING TRSELD,R6                                                        
LH23     TM    TRSSTAT,TRSSVOID    TEST MARKER BANK/VOID                        
         BZ    LH24                                                             
         MVI   INVLN+59,C'+'                                                    
         MVC   INVCHQ,SPACES                                                    
         MVC   INVCDT,SPACES                                                    
LH24     TM    TRSSTAT,TRSSOFFS    MARKED AS OFFSET                             
         BNO   LH25                AND FILL IN RIGHT SIDE OF SCREEN             
         MVI   INVLN+58,C'O'                                                    
*        MVC   INVLN+60(6),=C'OFFSET'                                           
*        GOTO1 DATCON,DMCB,(2,ACDTUSED),(8,INVLN+67)                            
         B     LH19                                                             
*                                                                               
LH25     TM    ITMST,MREV          MARKED AS REVERSE                            
         BNO   LH19                AND FILL IN RIGHT SIDE OF SCREEN             
*        MVC   INVLN+60(6),=C'RVERSE'                                           
         MVI   INVLN+58,C'R'                                                    
         OC    TRSREVD,TRSREVD                                                  
         BZ    LH19                                                             
*        GOTO1 DATCON,DMCB,(2,TRSREVD),(8,INVLN+67)                             
         B     LH19                                                             
*                                                                               
LH27     OC    ACDTUSED,ACDTUSED   EXIT IF USED                                 
         BNZ   LHX                                                              
         TM    TRNSSTAT,X'02'      UNDISBURSED - APPROVED                       
         BNO   *+8                                                              
         MVI   INVLN+58,C'A'                                                    
         TM    TRNSSTAT,X'04'      UNDISBURSED - HELD                           
         BNO   *+8                                                              
         MVI   INVLN+58,C'H'                                                    
*                                                                               
LHX      B      XIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT SOME TRANSACTION ELEMENTS                                   *         
* NTRY - R4=A(TRNEL)                                                  *         
***********************************************************************         
         SPACE 1                                                                
GTLM     NTR1  ,                                                                
         USING TRNELD,R4                                                        
         SR    R0,R0                                                            
         LR    R1,R4               R1=A(TRNEL)                                  
         XC    MPYL,MPYL                                                        
         XC    STAL,STAL                                                        
         TM    TRNSTAT,TRNSDR      IF DEBIT, LEAVE SOME ELEMENTS                
         BO    GTLM14                                                           
         XC    OTHL,OTHL                                                        
         XC    PAYL,PAYL                                                        
         XC    CSHL,CSHL                                                        
         B     GTLM14              BUMP R1 TO 1ST ELEMENT BEYOND TRNEL          
*                                                                               
         USING SCIELD,R1           ANY ELEMENT DSECT WILL DO                    
GTLM02   CLI   SCIEL,0             TEST EOR                                     
         BE    GTLMX                                                            
         TM    TRNSTAT,TRNSDR      IF DEBIT, SKIP SOME ELEMENTS                 
         BO    GTLM08                                                           
*                                                                               
         CLI   SCIEL,OTHELQ        OTHERS                                       
         BNE   GTLM04                                                           
         MVC   OTHL,SCIEL                                                       
         B     GTLM14                                                           
*                                                                               
GTLM04   CLI   SCIEL,XPYELQ        EXTRA PAYMENT INFORMATION                    
         BNE   GTLM06                                                           
         MVC   PAYL,SCIEL                                                       
         B     GTLM14                                                           
*                                                                               
GTLM06   CLI   SCIEL,SCIELQ        SUBSIDIARY CASH                              
         BNE   GTLM08                                                           
         CLI   SCITYPE,SCITCDSC    TEST CASH DISCOUNT TYPE                      
         BNE   GTLM14                                                           
         MVC   CSHL,SCIEL                                                       
         B     GTLM14                                                           
*                                                                               
GTLM08   CLI   SCIEL,TRSELQ        TRANSACTION STATUS                           
         BNE   GTLM10                                                           
         MVC   STAL,SCIEL                                                       
         B     GTLM14                                                           
*                                                                               
GTLM10   CLI   SCIEL,MPYELQ        MANUAL PAYMENT                               
         BNE   GTLM12                                                           
         MVC   MPYL,SCIEL                                                       
         B     GTLM14                                                           
*                                                                               
GTLM12   DS    0H                  NEXT ELEMENT TO BE EXTRACTED                 
*                                                                               
GTLM14   IC    R0,SCILN            BUMP TO NEXT TRANSACTION ELEMENT             
         AR    R1,R0                                                            
         B     GTLM02                                                           
*                                                                               
GTLMX    B     XIT                                                              
         DROP  R1,R4                                                            
         EJECT                                                                  
*              GIVE 'MORE TO COME MESSAGE'                                      
*                                                                               
MORTOCUM NTR1                                                                   
         MVC   CSHMSG,=CL60'MORE ITEMS TO COME - HIT ENTER FOR NEXT'            
         LA    R2,CSHNXTH                                                       
         OI    6(R2),X'40'                                                      
         MVC   OLDKEY,KEY                                                       
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                   COMMUNICATION WITH DATA MANAGER(DIRECTORY)                  
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         B     DIRECTRY                                                         
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRECTRY                                                         
*                                                                               
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRECTRY                                                         
*                                                                               
ADD      MVC   COMMAND,=C'DMADD'                                                
         B     DIRECTRY                                                         
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRECTRY                                                         
*                                                                               
DIRECTRY NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',KEY,KEY               
         B     DMCHECK                                                          
*                                                                               
DMCHECK  MVC   HALF(1),DMCB+8                                                   
         NC    HALF(1),DMOUTBTS                                                 
         BNZ   DMERRS                                                           
         XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD)            SORRY MEL                                    
         LM    RE,RC,12(RD)                                                     
         MVI   ERRNUM,0                                                         
*                                                                               
ERROR    L     R4,ATWA0            MESSAGE ALWAYS IN LINE 1                     
         LA    R4,64(R4)                                                        
         GOTO1 GETMSG,DMCB+12,(ERRNUM,8(R4)),(6,DMCB),                 X        
               (TERMINAL,DATAMGR)                                               
         FOUT  (R4)                                                             
         B      EXIT                                                            
*                                                                               
ANY      CLI   5(R2),0                                                          
         BCR   7,RE                                                             
         MVI   ERRNUM,1                                                         
         B     ERROR                                                            
*                                                                               
NUMERIC  MVI   ERRNUM,3                                                         
         TM    4(R2),X'08'                                                      
         BZ    ERROR                                                            
         BR    RE                                                               
*                                                                               
EXIT     OI    6(R2),X'40'         INSERT CURSOR                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EQUATES FOR ERROR NUMBERS                                        
*                                                                               
MISSING  EQU   1                                                                
INVALID  EQU   2                                                                
BADATE   EQU   13                                                               
BADACC   EQU   17                                                               
*                                                                               
FIRSTL   EQU   00                                                               
STATUS   EQU   44                  IN KEY                                       
WCODE    EQU   15                  WORK CODE IN KEY                             
LENGTH   EQU   42                  IN RECORD                                    
LINK     EQU   45                  IN RECORD                                    
*                                                                               
*              SYSTEM TABLE                                                     
*                                                                               
SYSTAB   DS    0CL12                                                            
         DC    AL1(4),CL10'SPOT',C'S'                                           
         DC    AL1(5),CL10'PRINT',C'P'                                          
         DC    AL1(7),CL10'NETWORK',C'U'                                        
         DC    AL1(10),CL10'PRODUCTION',C'V'                                    
         DC    AL1(7),CL10'EXPENSE',C'E'                                        
         DC    AL1(7),CL10'CNPRINT',C'Q'                                        
         DC    AL1(6),CL10'CNSPOT',C'T'                                         
         DC    AL1(6),CL10'CNPROD',C'W'                                         
         DC    AL1(9),CL10'CNEXPENSE',C'Y'                                      
         DC    AL1(7),CL10'MEDLINE',C'F'                                        
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR CASH ENQUIRY                                           
         SPACE 2                                                                
T607D    DSECT                                                                  
FACLIST  DS    0CL24                                                            
DATAMGR  DS    V                                                                
CALLOV   DS    V                                                                
CASHVAL  DS    V                                                                
DATVAL   DS    V                                                                
DATCON   DS    V                                                                
GETMSG   DS    V                                                                
DMWORK   DS    12D                                                              
DUB      DS    D                                                                
DOUBLE   DS    D                                                                
DMCB     DS    6F                                                               
ATWA0    DS    F                                                                
FULL     DS    F                                                                
WORD     DS    F                                                                
PRELOC   DS    F                                                                
HALF     DS    H                                                                
TERMINAL DS    CL1                                                              
DATWRK   DS    XL2                                                              
BYTE     DS    CL1                                                              
READDONE EQU   X'80'               ENQUIRY IS COMPLETE                          
OLDDEBIT EQU   X'40'               DEBIT IS AN OLD ONE                          
COMPANY  DS    CL1                                                              
SYSTEM   DS    CL1                                                              
WORK     DS    CL80                                                             
SPACES   DS    CL80                                                             
COMMAND  DS    CL6                                                              
ERRNUM   DS    CL1                                                              
DMINBTS  DS    CL1                                                              
DMOUTBTS DS    CL1                                                              
SCANTAB  DS    CL192               ALLOWS FOR UP TO 6 FILTERS                   
CLIENT   DS    CL6                                                              
PRODUCT  DS    CL6                                                              
STATN    DS    CL5                                                              
PUBLCN   DS    CL15                                                             
PUBLEN   DS    CL1                                                              
INVNO    DS    CL10                                                             
INVLEN   DS    CL1                                                              
JOBNUM   DS    CL6                                                              
*                                                                               
OTHL     DS    CL(ACOTLNQ2)        OTHERS ELEMENT                               
PAYL     DS    CL(TRPYLNQ)         EXTRA PAY ELEMENT                            
CSHL     DS    CL(TRCSLNQ2)        SUBSIDIARY CASH ELEMENT                      
STAL     DS    CL(TRSLNQ)          TRANSACTION STATUS ELEMENT                   
MPYL     DS    CL(MPYLN3Q)         MANUAL PAYMENT ELEMENT                       
*                                                                               
ITMST    DS    CL1                 ITEM STATUS                                  
TOTNET   DS    PL6                                                              
TOTCD    DS    PL6                                                              
CRKEY    DS    CL41                                                             
OFFICE   DS    CL1                                                              
ZONE     DS    CL2                                                              
ZLEN     DS    CL1                                                              
EDITION  DS    CL3                                                              
EDLEN    DS    CL1                                                              
STANM    DS    CL8                                                              
CHECKNO  DS    CL6                                                              
URGENT   DS    CL1                                                              
AUTH     DS    CL1                                                              
APPR     DS    CL1                                                              
HELD     DS    CL1                                                              
MARK     DS    CL1                                                              
MREV     EQU   X'80'               MARKED AS REVERSE                            
MOFF     EQU   X'40'               MARKED AS OFFSET                             
CHANGES  DS    CL1                                                              
SUBREF   DS    CL6                                                              
SUBRLEN  DS    CL1                                                              
INVTAB   DS    (INVTABN)XL(INVTABL)                                             
INVTABN  EQU   15                  MAXIMUM LINES ON SCREEN/IN TABLE             
KEY      DS    CL49                                                             
IOAREA   DS    CL2000                                                           
T607X    EQU   *                                                                
         SPACE 2                                                                
INVTABD  DSECT                     ** INVOICE TABLE **                          
INVKEY   DS    0X                                                               
INVKOFF  DS    CL(L'TRNKOFF)       OFFICE                                       
INVKCULC DS    CL(L'TRNKCULC)      CONTRA U/L/ACCOUNT                           
INVKDATE DS    XL(L'TRNKDATE)      DATE                                         
INVKREF  DS    CL(L'TRNKREF)       REFERENCE                                    
INVKEY1L EQU   *-INVKEY                                                         
INVKSBR  DS    XL(L'TRNKSBR)       SUB-REFERENCE                                
INVKEY2L EQU   *-INVKEY                                                         
INVIND1  DS    XL1                 INDICATOR - 1                                
INVIM0BV EQU   X'80'               INVOICE IS/WAS MARKER BANK/VOID              
INVIB37V EQU   X'40'               INVOICE IS A BT37 VOID                       
INVCD    DS    PL6                                                              
INVNET   DS    PL6                                                              
INVADAT  DS    XL(L'TRSDATE)       LATEST DEBIT DATE ADDED                      
INVLN    DS    CL(L'CSHLNE)        DISPLAY LINE                                 
         ORG   INVLN+60                                                         
INVCHQ   DS    CL(L'MPYNO)         CHEQUE NUMBER                                
         ORG   INVLN+67                                                         
INVCDT   DS    CL8                 CHEQUE DATE                                  
         ORG                                                                    
INVTABL  EQU   *-INVTABD                                                        
         EJECT                                                                  
       ++INCLUDE ACCSHFFD                                                       
         ORG   CSHMSGH                                                          
         DS    1800C                                                            
SAVEOPTN DS    CL1                                                              
OLDKEY   DS    CL49                                                             
PAYTYPE  DS    CL1                                                              
COMPSAVE DS    CL1                                                              
MYCOMPEL DS    CL40                                                             
KEYSAVE  DS    CL49                                                             
DATYPE   DS    CL1                                                              
DATE3    DS    CL3                                                              
LASTDATE DS    CL1          LENGTHS OF OPTIONAL UNPROTECTED FIELD               
LASTFILT DS    CL1                                                              
LASTOPT  DS    CL1                                                              
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* FATWA                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065ACCSH00   05/01/02'                                      
         END                                                                    
