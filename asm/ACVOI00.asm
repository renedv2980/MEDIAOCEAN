*          DATA SET ACVOI00    AT LEVEL 024 AS OF 05/01/02                      
*PHASE T60900A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE PUBVAL                                                                 
         TITLE 'VOID CHECK PROGRAM'                                             
T60900   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 250,*T60900*                                                     
         USING T609D,RC                                                         
         L     RA,4(R1)                                                         
         USING T609FFD,RA                                                       
         L     RF,8(R1)                                                         
         MVC   FACLIST,0(RF)                                                    
         ST    RA,ATWA0                                                         
         MVC   TERMINAL,0(RA)                                                   
         MVC   COMPANY,0(R1)                                                    
         MVI   DMINBTS,X'C0'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         MVC   DATADISP,=H'49'                                                  
         MVI   SPACES,X'40'                                                     
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         ZAP   CASHTOT,=P'0'                                                    
         ZAP   ITEMS,=P'0'                                                      
         FOUT  VOIMSG1H,SPACES,50                                               
         EJECT ,                                                                
*              READ COMPANY RECORD                                              
         SPACE 2                                                                
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         BAS   RE,READ                                                          
         LA    R4,IOAREA                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT                           
         EJECT ,                                                                
*              VALIDATE SYSTEM                                                  
         SPACE 2                                                                
         LA    R2,VOISYSH                                                       
         MVI   ERRNUM,INVALID                                                   
         TM    4(R2),X'20'                                                      
         BO    VOI20                                                            
         FOUT  VOISYSNH,SPACES,36                                               
         BAS   RE,ANY                                                           
         LA    R3,SYSTAB                                                        
         ZIC   RF,VOISYSH+5                                                     
         BCTR  RF,0                                                             
         SPACE 1                                                                
VOI10    CLI   0(R3),X'FF'                                                      
         BE    ERROR                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   VOISYS(0),0(R3)                                                  
         BE    VOI13                                                            
         LA    R3,L'SYSTAB(R3)                                                  
         B     VOI10                                                            
         SPACE 1                                                                
VOI13    FOUT  VOISYSH,(R3),10                                                  
         MVC   LEDSV,10(R3)        SAVE LEDGER CODE                             
         MVI   KEY+1,C'S'                                                       
         MVC   KEY+2(1),LEDSV                                                   
         CLI   KEY+2,C'E'          EXPENSE                                      
         BNE   VOI15                                                            
         SPACE 1                                                                
         USING ACCOMPD,R4                                                       
         MVC   LEDSV,ACMPSUPX                                                   
         MVC   KEY+2(1),ACMPSUPX                                                
         SPACE 1                                                                
VOI15    BAS   RE,READ                                                          
         LA    R4,IOAREA                                                        
         MVI   ELCODE,X'20'                                                     
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                NO NAME                                      
         SPACE 1                                                                
         USING ACNAMED,R4                                                       
         SR    R3,R3                                                            
         IC    R3,ACNMLEN                                                       
         SH    R3,=H'2'                                                         
         FOUT  VOISYSNH,ACNMNAME,(R3)                                           
         OI    4(R2),X'20'                                                      
         EJECT ,                                                                
*              VALIDATE PAYEE                                                   
         SPACE 2                                                                
VOI20    LA    R2,VOIPAYH                                                       
         TM    4(R2),X'20'                                                      
         BO    VOI30                                                            
         FOUT  VOIPAYNH,SPACES,36                                               
         BAS   RE,ANY                                                           
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         SPACE 1                                                                
         MVI   KEY+1,C'S'                                                       
         MVC   KEY+2(1),LEDSV                                                   
         XR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),VOIPAY                                                  
         SPACE 1                                                                
         CLI   KEY+2,C'P'                                                       
         BE    VOI22                                                            
         CLI   KEY+2,C'Q'                                                       
         BNE   VOI25                                                            
         SPACE 1                                                                
VOI22    CLI   5(R2),6                                                          
         BL    VOI25                                                            
         ZIC   R1,5(R2)                                                         
         SH    R1,=H'2'            IF NEXT TO LAST POSITION                     
         LA    R1,VOIPAY(R1)       IS A STAR                                    
         CLI   0(R1),C'*'          ITS A FUNNY WITH AN OFFICE CODE              
         BE VOI25                                                               
         MVC   KEY+4(20),SPACES                                                 
         GOTO1 =V(PUBVAL),DMCB,((R3),VOIPAY+1),(1,KEY+4),RR=RB                  
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         SPACE 1                                                                
VOI25    BAS   RE,READ                                                          
         MVI   ELCODE,X'20'                                                     
         LA    R4,IOAREA                                                        
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                NO NAME                                      
         SPACE 1                                                                
         IC    R3,ACNMLEN                                                       
         SH    R3,=H'2'                                                         
         FOUT  VOIPAYNH,ACNMNAME,(R3)                                           
         OI    4(R2),X'20'                                                      
         SPACE 1                                                                
         MVC   PAYKY,KEY                                                        
         EJECT ,                                                                
*              VALIDATE  NUMBER,  DATE,  VOID                                   
         SPACE 2                                                                
VOI30    LA    R2,VOICKNOH                                                      
         TM    4(R2),X'20'                                                      
         BO    VOI40                                                            
         BAS   RE,ANY                                                           
         CLI   5(R2),6                                                          
         BNE   ERROR                                                            
*&&UK*&& BAS   RE,NUMERIC                                                       
         OI    4(R2),X'20'                                                      
         SPACE 3                                                                
VOI40    LA    2,VOICKDTH                                                       
         TM    4(R2),X'20'                                                      
         BO    VOI50                                                            
         MVC   CHECKDT,SPACES                                                   
         BAS   RE,ANY                                                           
         MVI   ERRNUM,BADATE                                                    
         GOTO1 DATVAL,DMCB,(0,VOICKDT),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(8,CHECKDT)                                 
         OI    4(R2),X'20'                                                      
         SPACE 3                                                                
VOI50    LA    R2,VOIACTH                                                       
         TM    4(R2),X'20'                                                      
         BO    VOI62                                                            
         BAS   RE,ANY                                                           
         MVI   ACTION,C' '                                                      
         CLI   VOIACT,C'N'                                                      
         BE    VOI60                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   VOIACT,C'Y'                                                      
         BNE   ERROR                                                            
         MVI   ACTION,C'V'                                                      
         SPACE 1                                                                
VOI60    OI    4(R2),X'20'                                                      
         SPACE 1                                                                
VOI62    DS    0H                                                               
         EJECT ,                                                                
*              READ TRANSACTIONS LOOKING FOR MATCHING CHECK NUMBER              
*              AND DATE.                                                        
*              MARK (VOID)  OR UNMARK MATCHING ITEMS                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),PAYKY                                                    
         BAS   RE,READ                                                          
         MVI   ELCODE,X'44'                                                     
         SPACE 1                                                                
VOI80    BAS   RE,SEQ                                                           
         CLC   PAYKY,KEY                                                        
         BNE   VOI90          FINISHED DO OUTPUT                                
         SPACE 1                                                                
         LA    R4,IOAREA                                                        
         BAS   RE,FIRSTEL                                                       
         BNE   VOI80          NO TRANSACTION                                    
         USING TRANSD,R4                                                        
         SPACE 1                                                                
         TM    TRNSTYPE,X'80'      PAYMENT ARE DEBITS                           
         BNO   VOI80                                                            
         CLI   TRNSTYPE,X'81'      CHECKS ARE TYPE 81                           
         BNE   VOI80                                                            
         CLC   VOICKNO,TRNSNARR    CHECK NUMBER                                 
         BNE   VOI80                                                            
         SPACE 1                                                                
         CLC   CHECKDT,TRNSNARR+6  CHECK DATE                                   
         BNE   VOI80                                                            
         AP    CASHTOT,TRNSAMNT                                                 
         AP    ITEMS,=P'1'                                                      
         CLC   TRNSNARR+40(1),ACTION                                            
         BE    VOI80                    ALREADY MARKED                          
         MVC   TRNSNARR+40(1),ACTION                                            
         BAS   RE,WRITE                                                         
         B     VOI80                                                            
         EJECT ,                                                                
*              PUT OUT  COUNTS                                                  
         SPACE 2                                                                
VOI90    LA    R2,VOISYSH                                                       
         LA    R3,NOTOKMS                                                       
         CP    ITEMS,=P'0'                                                      
         BE    *+8                                                              
         LA    R3,OKMS                                                          
         FOUT  VOIMSGH,(R3),60                                                  
         CP    ITEMS,=P'0'                                                      
         BE    EXIT                                                             
         LA    R3,OKMS1                                                         
         EDIT  (P6,CASHTOT),(10,35(R3)),2,COMMAS=YES                            
         EDIT  (P2,ITEMS),(3,24(R3))                                            
         MVC   7(8,R3),=CL8'VOID'                                               
         CLI   ACTION,C'V'                                                      
         BE    *+10                                                             
         MVC   7(8,R3),=CL8'NOT VOID'                                           
         FOUT  VOIMSG1H,(R3),50                                                 
         GOTO1 =V(SQUASHER),DMCB,VOIMSG1,(0,50),RR=RB                           
         B     EXIT                                                             
         EJECT ,                                                                
* GENERAL CODE (WAS ACBTCODEA)                                                  
*                                                                               
ANY      CLI   5(R2),0                                                          
         BCR   7,RE                                                             
         MVI   ERRNUM,1                                                         
         B     ERROR                                                            
         SPACE 2                                                                
NUMERIC  MVI   ERRNUM,3                                                         
         TM    4(R2),X'08'                                                      
         BZ    ERROR                                                            
         BR    RE                                                               
         SPACE 2                                                                
FIRSTL   EQU   00                                                               
STATUS   EQU   44                  IN KEY                                       
WCODE    EQU   15                  WORK CODE IN KEY                             
LENGTH   EQU   42                  IN RECORD                                    
LINK     EQU   45                  IN RECORD                                    
         EJECT ,                                                                
*                   COMMUNICATION WITH DATA MANAGER(DIRECTORY)                  
         SPACE 2                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         B     DIRECTRY                                                         
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRECTRY                                                         
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRECTRY                                                         
ADD      MVC   COMMAND,=C'DMADD'                                                
         B     DIRECTRY                                                         
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRECTRY                                                         
         SPACE 2                                                                
DIRECTRY NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',KEY,         X        
               KEY,(TERMINAL,0)                                                 
         B     DMCHECK                                                          
         SPACE 2                                                                
DMCHECK  MVC   HALF(1),DMCB+8                                                   
         NC    HALF(1),DMOUTBTS                                                 
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD)            SORRY MEL                                    
         LM    RE,RC,12(RD)                                                     
         MVI   ERRNUM,0                                                         
         SPACE 2                                                                
ERROR    L     R4,ATWA0                 MESSAGE ALWAYS IN LINE 1                
         LA    R4,64(R4)                                                        
         GOTO1 GETMSG,DMCB+12,(ERRNUM,8(R4)),(6,DMCB),                 X        
               (TERMINAL,DATAMGR)                                               
         FOUT  (R4)                                                             
         SPACE 1                                                                
EXIT     OI    6(R2),X'40'         INSERT CURSOR                                
         XMOD1 1                                                                
         EJECT ,                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT ,                                                                
*              EQUATES FOR ERROR NUMBERS                                        
MISSING  EQU   1                                                                
INVALID  EQU   2                                                                
NOTNUMRC EQU   3                                                                
BADATE   EQU   13                                                               
BADACC   EQU   17                                                               
         SPACE 3                                                                
*              SYSTEM TABLE                                                     
SYSTAB   DS    0CL11                                                            
         DC    CL10'SPOT',C'S'                                                  
         DC    CL10'PRINT',C'P'                                                 
*&&US*&& DC    CL10'NETWORK',C'U'                                               
         DC    CL10'PRODUCTION',C'V'                                            
*&&US*&& DC    CL10'EXPENSE',C'E'                                               
*&&UK*&& DC    CL10'EXPENSE',C'X'                                               
         DC    CL10'CNPRINT',C'Q'                                               
*&&US*&& DC    CL10'CNSPOT',C'T'                                                
*&&UK*&& DC    CL10'ARTIST',C'T'                                                
         DC    CL10'CNPROD',C'W'                                                
*&&US*&& DC    CL10'CNEXP',C'Y'                                                 
         DC    CL10'MEDLINE',C'F'                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
NOTOKMS  DC    CL60'NO MATCHING ITEMS. CHECK ALL INPUT FIELDS.'                 
OKMS     DC    CL60'ACTION COMPLETE. ENTER NEXT.'                               
OKMS1    DC    CL50'STATUS XXXXXXXX,  ITEMS XXX, TOTAL XXXXXXXXXX'              
         EJECT ,                                                                
*              DSECT FOR VOID CHECK                                             
         SPACE 2                                                                
T609D    DSECT                                                                  
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
HALF     DS    H                                                                
         SPACE 1                                                                
DATADISP DS    H                                                                
TERMINAL DS    CL1                                                              
COMPANY  DS    CL1                                                              
SYSTEM   DS    CL1                                                              
KEY      DS    CL49                                                             
IOAREA   DS    CL1000                                                           
SPACES   DS    CL80                                                             
KEYSAVE  DS    CL49                                                             
ERRNUM   DS    CL1                                                              
COMMAND  DS    CL6                                                              
DMINBTS  DS    CL1                                                              
DMOUTBTS DS    CL1                                                              
         SPACE 1                                                                
CASHTOT  DS    PL6                                                              
ITEMS    DS    PL2                                                              
ELCODE   DS    CL1                                                              
BYTE     DS    CL1                                                              
WORK     DS    CL80                                                             
         EJECT ,                                                                
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         EJECT ,                                                                
       ++INCLUDE ACVOIFFD                                                       
         ORG   VOIMSGH                                                          
         DS    1800C                                                            
LEDSV    DS    CL1                                                              
PAYKY    DS    CL15                                                             
CHECKDT  DS    CL8                                                              
ACTION   DS    CL1                                                              
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACVOI00   05/01/02'                                      
         END                                                                    
