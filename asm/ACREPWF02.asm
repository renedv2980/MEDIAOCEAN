*          DATA SET ACREPWF02  AT LEVEL 057 AS OF 05/01/02                      
*PHASE ACWF02A,+0                                                               
*INCLUDE SCANNER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'READ WORKER FILE AND CHANGE ELEMENT ON FILE'                    
ACWF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACWF**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACWF02D,RC                                                       
         XC    INID,INID                                                        
         XC    OUTID,OUTID                                                      
         MVI   REVERSE,C'N'                                                     
         CLI   MODE,RUNFRST                                                     
         BNE   XIT                                                              
         EJECT                                                                  
*********************************************************************           
* READ ALL INPUT PARAMETERS FROM INPUT CARDS                        *           
*********************************************************************           
         SPACE 1                                                                
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
         BNZ   WXL40                                                            
         MVC   P(18),=C'MISSING INPUT CARD'                                     
         GOTO1 ACREPORT                                                         
         B     EOJ                                                              
*                                                                               
*XL32    OC    OUTID,OUTID                                                      
*        BNZ   WXL40                                                            
*        MVC   P(19),=C'MISSING OUTPUT CARD'                                    
*        GOTO1 ACREPORT                                                         
*        B     EOJ                                                              
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
         ZAP   PKCOUNT,=P'0'       NO. OF RECORDS CHANGED                       
         ZAP   MAXDUMP,=P'100'                                                  
         MVI   TESTBYTE,OFF                                                     
*                                                                               
         LA    R7,AREA                                                          
*        CLI   RCPOSTNG,C'N'                                                    
*        BE    READ                                                             
*        GOTO1 WORKER,DMCB,=C'OPEN',AOUTBUF,OUTID,(R7) OPEN O/P FILE            
         EJECT                                                                  
*********************************************************************           
* READ FILE AND PROCESS                                             *           
*      ACTUAL READING OF WORKER FILE                                *           
*********************************************************************           
         SPACE 1                                                                
         USING PLINED,R7                                                        
READ     LA    R8,T-4                                                           
         LA    R7,P                                                             
         GOTO1 WORKER,DMCB,=C'READ',AINBUF,INID,(R8)                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R3,0(R8)            R8: POINTS TO LENGTH HEADER                  
         AR    R3,R8               R3: END OF RECORD                            
         MVI   0(R3),0             SET X'00' BYTE AT END OF RECORD              
*                                                                               
         USING MPDRECD,R2                                                       
         LA    R2,T                R2: BEGINNING OF RECORD                      
         CLI   T,X'52'                                                          
         BE    READX               PRINT TOTALS AND EXIT                        
         CLI   MPDKTYP,MPDKTYPQ    IS IT X'2F' MEDIA POSTING DETAIL             
         BNE   READ                                                             
         CLI   MPDKSUB,MPDKSUBQ    X'00'                                        
         BNE   READ                                                             
*                                                                               
         LR    R5,R2                                                            
         AH    R5,DATADISP                                                      
READ02   CLI   0(R5),0                                                          
         BE    READ                                                             
         CLI   0(R5),X'2E'         IS IT MEDIA BILLING TXFR ELEM                
         BE    READ05                                                           
READ03   ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     READ02                                                           
*                                                                               
         USING MBTELD,R5                                                        
READ05   DS    0H                                                               
         CLC   MBTULA,=CL14'SZ325MSR'                                           
         BE    READ07                                                           
         CLC   MBTULA,=CL14'SZ325MST'                                           
         BNE   READ03                                                           
*                                                                               
READ07   DS    0H                                                               
         MVC   PWMED,MPDKMED                                                    
         MVC   PWCLI,MPDKCLI                                                    
         MVC   PWPRD,MPDKPRD                                                    
         MVC   PWEST,MPDKEST                                                    
         MVC   PWINV,MPDKINV                                                    
         MVC   PWBULA,MBTULA                                                    
*                                                                               
         XC    FLAG,FLAG           INIT FLAG                                    
         LA    R4,IOA1                                                          
F        USING MPDRECD,R4                                                       
         XC    F.MPDKEY,F.MPDKEY   READ MEDIA POSTING DETAIL RECD               
         MVC   F.MPDKEY,MPDKEY                                                  
         MVC   SAVKEY,MPDKEY                                                    
         MVC   COMMAND,DMREAD                                                   
         GOTO1 DATAMGR,DMCB,COMMAND,ACCOUNT,MPDKEY,IOA1                         
         LA    R6,IOA1                                                          
         CLC   SAVKEY(41),0(R6)                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MSG,=CL10'REC IN '                                               
         SR    RF,RF                                                            
         ICM   RF,3,F.MPDRLEN        GET LENGTH                                 
         GOTO1 DUMP,DMCB,F.MPDKEY,(RF)                                          
*                                                                               
         AH    R6,DATADISP                                                      
TMP      USING MBTELD,R6                                                        
READ20   CLI   0(R6),0                                                          
         BE    READ40                                                           
         CLI   0(R6),X'2E'         IS IT MEDIA BILLING TXFR ELEM                
         BNE   READ25                                                           
         CLC   TMP.MBTULA,MBTULA                                                
         BE    READ30                                                           
READ25   ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     READ20                                                           
*                                                                               
READ30   DS    0H                                                               
         OI    FLAG,FLGCHG         MARK RECORD WAS CHANGED                      
         AP    PKCOUNT,=P'1'       INC NO. OF RECORDS CHANGE COUNTER            
         MVC   TMP.MBTULA,SPACES                                                
         MVC   TMP.MBTULA,=CL14'SZSR'                                           
         CLC   MBTULA,=CL14'SZ325MSR'                                           
         BE    *+10                                                             
         MVC   TMP.MBTULA,=CL14'SZST'                                           
         MVC   PFBULA,TMP.MBTULA                                                
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   MSG,=CL10'REC OUT'                                               
         SR    RF,RF                                                            
         ICM   RF,3,F.MPDRLEN        GET LENGTH                                 
         GOTO1 DUMP,DMCB,F.MPDKEY,(RF)                                          
         B     READ25                                                           
*                                                                               
*                                                                               
READ40   DS    0H                                                               
         BAS   RE,POSTIT                                                        
         B     READ                                                             
*                                                                               
READX    DS    0H                                                               
         MVC   P(30),=C'TOTAL NO. OF RECORDS CHANGED: '                         
         EDIT  PKCOUNT,(L'PKCOUNT,P+31),FILL=0                                  
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  F,TMP                                                            
         EJECT                                                                  
*********************************************************************           
* WRITE RECORDS TO WORKER FILE AND TOTAL AMOUNTS                    *           
*********************************************************************           
         SPACE 1                                                                
POSTIT   NTR1                                                                   
         TM    FLAG,FLGCHG                                                      
         BNO   POSTX                                                            
         CLI   RCPOSTNG,C'N'                                                    
         BE    POSTX                                                            
         MVC   COMMAND,DMWRT                                                    
         GOTO1 DATAMGR,DMCB,COMMAND,ACCOUNT,MPDKEY,IOA1                         
POSTX    B     XIT                                                              
         EJECT                                                                  
*&&DO                                                                           
*********************************************************************           
* REPORTING                                                         *           
*********************************************************************           
         SPACE 1                                                                
         USING TRANSD,R4                                                        
         USING PLINED,R7                                                        
REPORT   NTR1                                                                   
         LA    R7,P                                                             
         MVC   PWMED,MPDKMED                                                    
         MVC   PWCLI,MPDKCLI                                                    
         MVC   PWPRD,MPDKPRD                                                    
         MVC   PWEST,MPDKEST                                                    
         MVC   PWINV,MPDKINV                                                    
         MVC   PWBULA,MBTULA                                                    
         MVC   PFBULA,TMP.MBTULA                                                
REPORTX  GOTO1 ACREPORT                                                         
         B     XIT                                                              
*********************************************************************           
* END OF FILE ROUTINE                                               *           
*********************************************************************           
         SPACE 1                                                                
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
*        GOTO1 ACREPORT                                                         
*        MVC   P+17(15),PSSBDESC                                                
*        MVC   P+39(14),=C'SUMMARY TOTALS'                                      
*        EDIT  (P6,ITEMS),(10,P+53)                                             
*        MVC   P+64(5),=C'ITEMS'                                                
*        EDIT  (P6,DEBITS),(13,P+83),2,MINUS=YES                                
*        MVC   P+96(13),P+83                                                    
*        GOTO1 ACREPORT                                                         
         B     EOJ                                                              
*&&                                                                             
         EJECT                                                                  
*********************************************************************           
* READ CONTROL FILE FOR ID NUMBER                                   *           
*********************************************************************           
         SPACE 1                                                                
         USING UKRECD,R5                                                        
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
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NTR1                                                                   
*        CLI   QOPT7,C'Y'                                                       
*        BNE   DUMPX                                                            
*        CP    DUMPCNT,MAXDUMP                                                  
*        BH    DUMPX                                                            
*        AP    DUMPCNT,=P'1'                                                    
*                                                                               
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,0(R1)                                                         
         L     R4,4(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
         B     XIT                                                              
*        MVC   MSG,=CL10'SVDA IN'                                               
*        GOTO1 DUMP,DMCB,SVDA,L'SVDA                                            
         EJECT                                                                  
*********************************************************************           
* CONSTANTS                                                         *           
*********************************************************************           
         SPACE 1                                                                
SCANNER  DC    V(SCANNER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
AINBUF   DC    A(INBUF)                                                         
AOUTBUF  DC    A(OUTBUF)                                                        
         EJECT                                                                  
*********************************************************************           
* LITERALS                                                          *           
*********************************************************************           
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
ACCOUNT  DC    C'ACCOUNT '                                                      
COMMAND  DC    C'        '                                                      
INBUF    DC    4500X'00'                                                        
OUTBUF   DC    4500X'00'                                                        
         EJECT                                                                  
*********************************************************************           
* PRINT DSECT FOR PROGRAM                                           *           
*********************************************************************           
PLINED   DSECT                                                                  
         DS    CL2                                                              
PWMED    DS    CL1                 MEDIA FROM WORKER FILE                       
         DS    CL6                                                              
PWCLI    DS    CL3                 CLI FROM WORKER FILE                         
         DS    CL5                                                              
PWPRD    DS    CL3                 PRODUCT FROM WORKER FILE                     
         DS    CL6                                                              
PWEST    DS    CL4                 ESTIMATE FROM WORKER FILE                    
         DS    CL6                                                              
PWINV    DS    CL5                 INVOICE # FROM WORKER FILE                   
         DS    CL4                                                              
PWBULA   DS    CL14                WORKER FILE'S U/L/A FROM 2E ELEM             
         DS    CL3                                                              
PFBULA   DS    CL14                ACCFILE U/L/A FROM 2E ELEM                   
*                                                                               
         EJECT                                                                  
*********************************************************************           
* STORAGE - DSECT FOR PROGRAM                                       *           
*********************************************************************           
         SPACE 1                                                                
ACWF02D  DSECT                                                                  
INID     DS    CL16                                                             
OUTID    DS    CL16                                                             
SAVE1A   DS    CL(MDTLNQ)                                                       
*                                                                               
T        DS    CL1200                                                           
DEBITS   DS    PL6                                                              
CREDITS  DS    PL6                                                              
ITEMS    DS    PL6                                                              
PDUMP    DS    PL4                                                              
MAXDUMP  DS    PL4                                                              
CTSAVE   DS    CL25                                                             
SAVKEY   DS    CL42                                                             
*                                                                               
FLAG     DS    XL1                                                              
FLGCHG   EQU   X'80'                                                            
*                                                                               
MSG      DS    CL10                                                             
PKCOUNT  DS    PL4                                                              
PKAMNT   DS    PL8                                                              
REVERSE  DS    CL1                                                              
TESTBYTE DS    CL1                                                              
OFF      EQU   C'N'                                                             
ON       EQU   C'Y'                                                             
CARD     DS    CL80                                                             
BLOCK    DS    10CL32                                                           
AREA     DS    CL300                                                            
*                                                                               
IOA1     DS    CL2000                                                           
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
**PAN#1  DC    CL21'057ACREPWF02 05/01/02'                                      
         END                                                                    
