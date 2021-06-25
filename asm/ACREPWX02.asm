*          DATA SET ACREPWX02  AT LEVEL 077 AS OF 03/14/11                      
*PHASE ACWX02A,+0                                                               
*INCLUDE SCANNER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'PROGRAM TO FIX WORKER FILES'                                    
ACWX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACWX**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACWX02D,RC                                                       
         XC    INID,INID                                                        
         XC    OUTID,OUTID                                                      
         MVI   REVERSE,C'N'                                                     
         CLI   MODE,RUNFRST                                                     
         BNE   XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              READ ALL INPUT PARAMETERS FROM INPUT CARDS                       
*-------------------------------------------------------------------*           
WXL3     GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   =C'/*',CARD                                                      
         BE    WXL30                                                            
         MVC   P(80),CARD                                                       
         GOTO1 ACREPORT                                                         
*                                                                               
         GOTO1 SCANNER,DMCB,(C'C',CARD),(10,BLOCK)                              
         CLI   DMCB+4,0                                                         
         BE    BADCARD                                                          
         CLI   DMCB+4,10                                                        
         BH    BADCARD                                                          
         ZIC   R0,DMCB+4                                                        
         LA    R3,BLOCK                                                         
         USING UKRECD,R5                                                        
         LA    R5,INID                                                          
         CLC   12(2,R3),=C'IN'                                                  
         BE    WXL5                                                             
         LA    R5,OUTID                                                         
         OI    UKFLAG,X'01'        TURN ON DUPLICATE KEY FLAG                   
         CLC   12(3,R3),=C'OUT'                                                 
         BNE   BADCARD                                                          
WXL5     OC    UKINDEX,UKINDEX                                                  
*        BNZ   BADCARD                                                          
         LA    R3,32(R3)                                                        
         BCT   R0,WXL7                                                          
         B     BADCARD                                                          
*                                                                               
WXL7     CLC   12(6,R3),=C'ORIGIN'                                              
         BNE   WXL11                                                            
         MVC   UKUSRID,10(R3)                                                   
         OC    UKUSRID,UKUSRID                                                  
         BNZ   WXLNXT                                                           
         BAS   RE,GETID            ID IS ALPHA                                  
         CLC   P(8),=C'BAD CARD'                                                
         BE    BADCARD                                                          
         B     WXLNXT                                                           
*                                                                               
WXL11    CLC   12(4,R3),=C'PROG'                                                
         BNE   WXL13                                                            
         MVC   UKSYSPRG,22(R3)                                                  
         B     WXLNXT                                                           
*                                                                               
WXL13    CLC   12(3,R3),=C'SUB'                                                 
         BNE   WXL15                                                            
         CLI   22(R3),C'*'                                                      
         BE    WXLNXT                                                           
         MVC   UKSUBPRG,22(R3)                                                  
         B     WXLNXT                                                           
*                                                                               
WXL15    CLC   12(3,R3),=C'DAY'                                                 
         BNE   WXL17                                                            
         PACK  DUB(2),22(3,R3)                                                  
         MVC   UKDAY,DUB                                                        
         B     WXLNXT                                                           
*                                                                               
WXL17    CLC   12(4,R3),=C'TYPE'                                                
         BNE   WXL19                                                            
         MVC   UKCLASS,22(R3)                                                   
         B     WXLNXT                                                           
*                                                                               
WXL19    CLC   12(3,R3),=C'SEQ'                                                 
         BNE   WXL21                                                            
         MVC   UKFILNO,10(R3)                                                   
         B     WXLNXT                                                           
*                                                                               
WXL21    CLC   12(7,R3),=C'REVERSE'                                             
         BNE   WXL23                                                            
         MVC   REVERSE,22(R3)                                                   
         B     WXLNXT                                                           
*                                                                               
WXL23    DS    0H                                                               
         B     BADCARD                                                          
*                                                                               
WXLNXT   LA    R3,32(R3)                                                        
         BCT   R0,WXL7                                                          
         B     WXL3                                                             
*                                                                               
BADCARD  DC    0H'0'                                                            
         MVC   P(8),=C'BAD CARD'                                                
         GOTO1 ACREPORT                                                         
         B     EOJ                                                              
*                                                                               
*                                                                               
WXL30    OC    INID,INID                                                        
         BNZ   WXL32                                                            
         MVC   P(18),=C'MISSING INPUT CARD'                                     
         GOTO1 ACREPORT                                                         
         B     EOJ                                                              
*                                                                               
WXL32    OC    OUTID,OUTID                                                      
         BNZ   WXL40                                                            
         MVC   P(19),=C'MISSING OUTPUT CARD'                                    
         GOTO1 ACREPORT                                                         
         B     EOJ                                                              
*                                                                               
*                                                                               
EOJ      DS    0H                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              READ INDEX                                                       
*                                                                               
WXL40    DC    0H'0'                                                            
         MVC   P+16(L'INID),INID                                                
         GOTO1 ACREPORT                                                         
         GOTO1 WORKER,DMCB,=C'INDEX',AINBUF,INID                                
         TM    DMCB+8,X'10'                                                     
         BO    WXL42                                                            
         CLI   DMCB+8,0                                                         
         BE    WXL50                                                            
         DC    H'0'                SOME KIND OF DISK ERROR                      
*                                                                               
WXL42    DC    0H'0'                                                            
         MVC   P(16),=C'NO SUCH ID ,KEY='                                       
         GOTO1 HEXOUT,DMCB,INID,P+16,16                                         
         GOTO1 ACREPORT                                                         
         B     EOJ                                                              
*                                                                               
WXL50    DC    0H'0'                                                            
         MVI   FORCEHED,C'Y'       INITIALIZE COUNTERS                          
         ZAP   ITEMS,=P'0'                                                      
         ZAP   DEBITS,=P'0'                                                     
         ZAP   CREDITS,=P'0'                                                    
         ZAP   PDUMP,=P'0'                                                      
         ZAP   MAXDUMP,=P'100'                                                  
         MVI   TESTBYTE,OFF                                                     
*                                                                               
         LA    R7,AREA                                                          
         CLI   RCPOSTNG,C'N'                                                    
         BE    READ                                                             
         GOTO1 WORKER,DMCB,=C'OPEN',AOUTBUF,OUTID,(R7) OPEN O/P FILE            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              READ FILE AND PROCESS                                            
*              ACTUAL READING OF WORKER FILE                                    
*-------------------------------------------------------------------*           
READ     LA    R8,T-4                                                           
         GOTO1 WORKER,DMCB,=C'READ',AINBUF,INID,(R8)                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R3,0(R8)            R8: POINTS TO LENGTH HEADER                  
         AR    R3,R8               R3: END OF RECORD                            
         MVI   0(R3),0             SET X'00' BYTE AT END OF RECORD              
*                                                                               
         USING PSHEADD,R2                                                       
         LA    R2,T                R2: BEGINNING OF RECORD                      
         CLI   T,X'52'                                                          
         BE    EOF                                                              
         CLI   T,X'50'                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRANSD,R4                                                        
         SR    R4,R4                                                            
         IC    R4,1(,R2)           R4: TRANSACTION ELEMENT                      
         AR    R4,R2                                                            
         CLI   0(R4),X'44'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
LBLOK    BAS   RE,POSTIT                                                        
         BAS   RE,REPORT                                                        
*                                                                               
LBLNO    B     READ                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        REPORTING                                                              
*-------------------------------------------------------------------*           
REPORT   NTR1                                                                   
         MVC   P+11(14),PSHDACC+1                                               
         MVC   P+27(14),PSHDSBAC+1                                              
         MVC   P+44(2),PSHDANAL                                                 
         MVI   P+46,C'/'                                                        
         MVC   P+47(2),TRNSANAL                                                 
         TM    TRNSSTAT,X'01'                                                   
         BNO   REP02                                                            
         MVC   P+49(3),=C'N/C'                                                  
*                                                                               
REP02    GOTO1 DATCON,DMCB,(1,TRNSDATE),(8,P+54)                                
         MVC   P+63(3),TRNSBTCH                                                 
         MVC   P+66(6),TRNSREF                                                  
         IC    R3,TRNSLEN                                                       
         SH    R3,=H'29'                                                        
         MVC   WORK,SPACES                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TRNSNARR                                                 
         GOTO1 CHOPPER,DMCB,(26,WORK),(26,P+75),1                               
*                                                                               
         TM    TRNSSTAT,X'80'                                                   
         BO    DEBIT                                                            
         EDIT  (P6,TRNSAMNT),(13,P+114),2,MINUS=YES                             
         B     REPORTX                                                          
DEBIT    EDIT  (P6,TRNSAMNT),(13,P+101),2,MINUS=YES                             
REPORTX  GOTO1 ACREPORT                                                         
         B     XIT                                                              
*-------------------------------------------------------------------*           
*              WRITE RECORDS TO WORKER FILE AND TOTAL AMOUNTS                   
*-------------------------------------------------------------------*           
POSTIT   NTR1                                                                   
         USING TRANSD,R4                                                        
         ZIC   R4,1(R2)            R4: TRANSACTION ELEMENT                      
         AR    R4,R2                                                            
         TM    TRNSSTAT,X'80'                                                   
         BO    POST02                                                           
         AP    CREDITS,TRNSAMNT                                                 
         B     POST03                                                           
POST02   AP    DEBITS,TRNSAMNT                                                  
POST03   AP    ITEMS,=P'1'                                                      
         AP    PDUMP,=P'1'                                                      
         CLI   RCPOSTNG,C'N'                                                    
         BE    XIT                                                              
         LA    R8,T-4                                                           
         GOTO1 WORKER,DMCB,=C'ADD',AOUTBUF,OUTID,(R8)                           
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              END OF FILE ROUTINE                                              
*-------------------------------------------------------------------*           
EOF      LA    R8,T-4              WRITE THE LAST RECORD                        
         USING PSSUBFD,R2                                                       
         ZAP   PSSBRECS,ITEMS                                                   
         ZAP   PSSBCASH,DEBITS                                                  
         CLI   RCPOSTNG,C'N'                                                    
         BE    EOF100                                                           
         GOTO1 WORKER,DMCB,=C'ADD',AOUTBUF,OUTID,(R8)                           
         GOTO1 WORKER,DMCB,=C'CLOSE',AOUTBUF,OUTID                              
*                                                                               
EOF100   MVC   P,SPACES                                                         
         GOTO1 ACREPORT                                                         
         MVC   P+64(5),=C'ITEMS'                                                
         MVC   P+39(14),=C'POSTING TOTALS'                                      
         EDIT  (P6,ITEMS),(10,P+63)                                             
         EDIT  (P6,DEBITS),(13,P+93),2,MINUS=YES                                
         EDIT  (P6,CREDITS),(13,P+106),2,MINUS=YES                              
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P+17(15),PSSBDESC                                                
         MVC   P+39(14),=C'SUMMARY TOTALS'                                      
         EDIT  (P6,ITEMS),(10,P+53)                                             
         MVC   P+64(5),=C'ITEMS'                                                
         EDIT  (P6,DEBITS),(13,P+83),2,MINUS=YES                                
         MVC   P+96(13),P+83                                                    
         GOTO1 ACREPORT                                                         
         B     EOJ                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              READ CONTROL FILE FOR ID NUMBER                                  
*-------------------------------------------------------------------*           
GETID    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',=C'NCTFILE X'                
         LA    R7,T                                                             
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,22(R3)       ALPHA ID FROM CARD                           
         MVC   CTSAVE,CTIKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',(R7),(R7),(0,0)               
         CLC   CTSAVE,CTIKEY                                                    
         BNE   GETBAD                                                           
         LA    R7,CTIDATA                                                       
         SR    R6,R6                                                            
GET2     CLI   0(R7),0                                                          
         BE    GETBAD                                                           
         CLI   0(R7),X'02'                                                      
         BE    GET4                                                             
         IC    R6,1(R7)                                                         
         AR    R7,R6                                                            
         B     GET2                                                             
*                                                                               
GET4     MVC   UKUSRID,2(R7)                                                    
         B     XIT                                                              
GETBAD   DC    0H'0'                                                            
         MVC   P(8),=C'BAD CARD'                                                
         B     EOJ                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              DATA CONSTANTS                                                   
*-------------------------------------------------------------------*           
DMPGET   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R4,=C'GET'                                                       
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R4,=C'PUT'                                                       
*                                                                               
DUMP     SR    R6,R6                                                            
         ICM   R6,3,0(R8)                                                       
         GOTO1 PRNTBL,DMCB,(3,(R4)),(R8),C'DUMP',(R6),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              DATA CONSTANTS                                                   
*-------------------------------------------------------------------*           
SCANNER  DC    V(SCANNER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
AINBUF   DC    A(INBUF)                                                         
AOUTBUF  DC    A(OUTBUF)                                                        
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
INBUF    DC    4500X'00'                                                        
OUTBUF   DC    4500X'00'                                                        
*              DSECT FOR PROGRAM                                                
*                                                                               
ACWX02D  DSECT                                                                  
INID     DS    CL16                                                             
OUTID    DS    CL16                                                             
SAVE1A   DS    CL(MDTLNQ)                                                       
*                                                                               
         DS    F                                                                
T        DS    CL1200                                                           
DEBITS   DS    PL6                                                              
CREDITS  DS    PL6                                                              
ITEMS    DS    PL6                                                              
PDUMP    DS    PL4                                                              
MAXDUMP  DS    PL4                                                              
CTSAVE   DS    CL25                                                             
*                                                                               
REVERSE  DS    CL1                                                              
TESTBYTE DS    CL1                                                              
OFF      EQU   C'N'                                                             
ON       EQU   C'Y'                                                             
CARD     DS    CL80                                                             
BLOCK    DS    10CL32                                                           
AREA     DS    CL300                                                            
         EJECT                                                                  
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
* ACREPWORKD                                                                    
* ACGENMODES                                                                    
* ACGENPOST                                                                     
* ACGENBOTH                                                                     
* ACMASTD                                                                       
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'077ACREPWX02 03/14/11'                                      
         END                                                                    
