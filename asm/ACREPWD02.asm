*          DATA SET ACREPWD02  AT LEVEL 079 AS OF 05/01/02                      
*PHASE ACWD02A,+0                                                               
*INCLUDE SCANNER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRTREC                                                                 
         TITLE 'PROGRAM TO DUMP WORKER FILE RECORDS'                            
ACWD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACWD**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACWD02D,RC                                                       
         XC    INID,INID                                                        
         CLI   MODE,RUNFRST                                                     
         BNE   XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              READ ALL INPUT PARAMETERS FROM INPUT CARDS                       
*-------------------------------------------------------------------*           
WDL3     GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   =C'/*',CARD                                                      
         BE    WDL30                                                            
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
         BE    *+6                                                              
         DC    H'0'                                                             
WDL5     OC    UKINDEX,UKINDEX                                                  
*        BNZ   BADCARD                                                          
         LA    R3,32(R3)                                                        
         BCT   R0,WDL7                                                          
         B     BADCARD                                                          
*                                                                               
WDL7     CLC   12(6,R3),=C'ORIGIN'                                              
         BNE   WDL11                                                            
         MVC   UKUSRID,10(R3)                                                   
         OC    UKUSRID,UKUSRID                                                  
         BNZ   WDLNXT                                                           
         BAS   RE,GETID            ID IS ALPHA                                  
         CLC   P(8),=C'BAD CARD'                                                
         BE    BADCARD                                                          
         B     WDLNXT                                                           
*                                                                               
WDL11    CLC   12(4,R3),=C'PROG'                                                
         BNE   WDL13                                                            
         MVC   UKSYSPRG,22(R3)                                                  
         B     WDLNXT                                                           
*                                                                               
WDL13    CLC   12(3,R3),=C'SUB'                                                 
         BNE   WDL15                                                            
         CLI   22(R3),C'*'                                                      
         BE    WDLNXT                                                           
         MVC   UKSUBPRG,22(R3)                                                  
         B     WDLNXT                                                           
*                                                                               
WDL15    CLC   12(3,R3),=C'DAY'                                                 
         BNE   WDL17                                                            
         PACK  DUB(2),22(3,R3)                                                  
         MVC   UKDAY,DUB                                                        
         B     WDLNXT                                                           
*                                                                               
WDL17    CLC   12(4,R3),=C'TYPE'                                                
         BNE   WDL19                                                            
         MVC   UKCLASS,22(R3)                                                   
         B     WDLNXT                                                           
*                                                                               
WDL19    CLC   12(3,R3),=C'SEQ'                                                 
         BNE   WDL21                                                            
         MVC   UKFILNO,10(R3)                                                   
         B     WDLNXT                                                           
*                                                                               
WDL21    CLC   12(7,R3),=C'REVERSE'                                             
         BNE   WDL23                                                            
         MVC   REVERSE,22(R3)                                                   
         B     WDLNXT                                                           
*                                                                               
WDL23    DS    0H                                                               
         B     BADCARD                                                          
*                                                                               
WDLNXT   LA    R3,32(R3)                                                        
         BCT   R0,WDL7                                                          
         B     WDL3                                                             
*                                                                               
BADCARD  DC    0H'0'                                                            
         MVC   P(8),=C'BAD CARD'                                                
         GOTO1 ACREPORT                                                         
         B     EOJ                                                              
*                                                                               
*                                                                               
WDL30    OC    INID,INID                                                        
         BNZ   WDL40                                                            
         MVC   P(18),=C'MISSING INPUT CARD'                                     
         GOTO1 ACREPORT                                                         
         B     EOJ                                                              
*                                                                               
EOJ      DS    0H                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              READ INDEX                                                       
*                                                                               
WDL40    DC    0H'0'                                                            
         ZAP   RCDCNT,=P'0'                                                     
         MVC   P+16(L'INID),INID                                                
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         GOTO1 WORKER,DMCB,=C'INDEX',AINBUF,INID                                
         TM    DMCB+8,X'10'                                                     
         BO    WDL42                                                            
         CLI   DMCB+8,0                                                         
         BE    WDL50                                                            
         DC    H'0'                SOME KIND OF DISK ERROR                      
*                                                                               
WDL42    DC    0H'0'                                                            
         MVC   P(16),=C'NO SUCH ID ,KEY='                                       
         GOTO1 HEXOUT,DMCB,INID,P+16,16                                         
         GOTO1 ACREPORT                                                         
         B     EOJ                                                              
*                                                                               
WDL50    DC    0H'0'                                                            
         MVI   FORCEHED,C'Y'       INITIALIZE COUNTERS                          
         MVI   TESTBYTE,OFF                                                     
*                                                                               
         LA    R7,AREA                                                          
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
         AP    RCDCNT,=P'1'                                                     
         LH    R3,0(R8)            R8: POINTS TO LENGTH HEADER                  
         AR    R3,R8               R3: END OF RECORD                            
         MVI   0(R3),0             SET X'00' BYTE AT END OF RECORD              
*                                                                               
         USING PSHEADD,R2                                                       
         LA    R2,T                R2: BEGINNING OF RECORD                      
         BAS   RE,DMPGET                                                        
         CLI   T,X'52'                                                          
         BE    EOJ                                                              
         B     READ                                                             
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
         CLI   RCFFPARM,C' '                                                    
         BH    PRTR                                                             
         LA    R4,=C'GET'                                                       
*                                                                               
DUMP     SR    R6,R6                                                            
         LA    R8,T-4                                                           
         ICM   R6,3,0(R8)                                                       
         GOTO1 PRNTBL,DMCB,(3,(R4)),(R8),C'DUMP',(R6),=C'2D'                    
         B     XIT                                                              
*                                                                               
PRTR     MVC   P,SPACES                                                         
         MVC   P+1(6),=C'RECORD '                                               
         EDIT  RCDCNT,(6,P+8),ALIGN=LEFT                                        
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
*                                                                               
         LA    R8,T-4                                                           
         XR    R0,R0                                                            
         GOTO1 PRTREC,DMCB,(RCFFPARM,(R8)),(4,(R0)),PRINT,HEXOUT                
         MVC   P,SPACES                                                         
         GOTO1 PRINT,DMCB,P,=C'BL01'     SKIP A LINE                            
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              DATA CONSTANTS                                                   
*-------------------------------------------------------------------*           
SCANNER  DC    V(SCANNER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
AINBUF   DC    A(INBUF)                                                         
AOUTBUF  DC    A(OUTBUF)                                                        
PRTREC   DC    V(PRTREC)                                                        
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
INBUF    DC    4500X'00'                                                        
OUTBUF   DC    4500X'00'                                                        
*              DSECT FOR PROGRAM                                                
*                                                                               
ACWD02D  DSECT                                                                  
INID     DS    CL16                                                             
OUTID    DS    CL16                                                             
SAVE1A   DS    CL(MDTLNQ)                                                       
*                                                                               
         DS    F                                                                
T        DS    CL1200                                                           
CTSAVE   DS    CL25                                                             
*                                                                               
RCDCNT   DS    PL4                                                              
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
**PAN#1  DC    CL21'079ACREPWD02 05/01/02'                                      
         END                                                                    
