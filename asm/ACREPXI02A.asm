*          DATA SET ACREPXI02A AT LEVEL 003 AS OF 08/16/00                      
*PHASE ACXI02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'FIX 23 ELS ON TYPE 1 ON U/L SV'                                 
ACXI02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXI**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXID,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,LEDGLAST                                                    
         BE    LDGL                                                             
         B     XIT                                                              
***********************************************************************         
*              RUN FIRST                                              *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         ZAP   ACDR,=P'0'                                                       
         ZAP   ACCR,=P'0'                                                       
         ZAP   JACDR,=P'0'                                                      
         ZAP   JACCR,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
         ZAP   TOTDR,=P'0'                                                      
         ZAP   JTOTCR,=P'0'                                                     
         ZAP   JTOTDR,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              REQ FIRST                                              *         
***********************************************************************         
*                                                                               
         USING CPYELD,RF                                                        
REQF     DS    0H                                                               
         L     RF,ADCMPELS                                                      
         MVC   SVAGY,CPYALPHA            SAVE ALPHA AGENCY                      
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
*              LEDGER FIRST                                           *         
***********************************************************************         
LDGF     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCRDTRNS,C'N'       AND TRANSACTIONS                             
         L     R2,ADLEDGER                                                      
         CLC   1(2,R2),=C'SV'      FOR SJ                                       
         BNE   XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDTRNS,C'Y'                                                    
         ZAP   CHAREC,=P'0'                                                     
         ZAP   PDUMP,=P'0'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PROCTRNS                                               *         
***********************************************************************         
PTRN     DS    0H                                                               
         USING TRNRECD,R3                                                       
         L     R3,ADTRANS             RECORD DUMP ASSUMES R3 IS                 
         SH    R3,DATADISP            POINTING TO THE RECORD                    
*                                                                               
         USING TRNELD,R2                                                        
         L     R2,ADTRANS                                                       
         CLI   TRNTYPE,1              ONLY PROCESS TYPE ONE                     
         BNE   XIT                                                              
         DROP  R2                                                               
*                                                                               
         USING TRSELD,R2                                                        
         L     R2,ADTRANS                                                       
PTRN06   CLI   0(R2),TRSELQ           GET '60' ELEMENT                          
         BE    PTRN08                                                           
         CLI   0(R2),0                                                          
         BE    PTRN09                                                           
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PTRN06                                                           
PTRN08   CLC   TRSDATE,=X'C6E7'       EARLIER THAN 990709                       
         BL    XIT                                                              
         CLC   TRSDATE,=X'C6EC'       AFTER 990712                              
         BH    XIT                                                              
         DROP  R2                                                               
*                                                                               
         USING OTHELD,R2                                                        
PTRN09   L     R2,ADTRANS                                                       
PTRN10   CLI   0(R2),OTHELQ           GET '23' ELEMENT                          
         BE    PTRN12                                                           
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PTRN10                                                           
PTRN12   CLC   OTHNUM(4),=C'CLI='     INCORRECT ELEMENT IF PRESENT              
         BNE   XIT                                                              
*                                                                               
         BAS   RE,DMPGET              DUMP BEFORE                               
*                                                                               
         MVC   SCANSPC,SPACES                                                   
         SR    R5,R5                                                            
         IC    R5,1(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   SCANSPC(0),0(R2)                                                 
*                                                                               
         MVC   ELEM,SPACES                                                      
         MVC   ELEM(2),=X'FE0F'                                                 
*                                                                               
         LA    R5,SCANSPC+10                                                    
         LA    R6,3                                                             
PTRN15   CLC   0(4,R5),=C'PRD='                                                 
         BE    PTRN20                                                           
         SH    R5,=H'1'                                                         
         BCT   R6,PTRN15                                                        
         DC    H'0'                                                             
PTRN20   MVC   ELEM+2(3),4(R5)                                                  
         CLI   ELEM+3,C','                                                      
         BNE   *+8                                                              
         MVI   ELEM+3,C' '                                                      
         CLI   ELEM+4,C','                                                      
         BNE   *+8                                                              
         MVI   ELEM+4,C' '                                                      
*                                                                               
         LA    R5,SCANSPC+18                                                    
         LA    R6,6                                                             
PTRN25   CLC   0(4,R5),=C'JOB='                                                 
         BE    PTRN30                                                           
         SH    R5,=H'1'                                                         
         BCT   R6,PTRN25                                                        
         DC    H'0'                                                             
PTRN30   MVC   ELEM+8(6),4(R5)                                                  
*                                                                               
         MVI   0(R2),X'FF'                                                      
         L     R4,ADTRANS                                                       
         SH    R4,DATADISP                                                      
         GOTO1 DELEL,DMCB,(X'FF',(R4)),0                                        
*                                                                               
         L     R4,ADTRANS                                                       
         SH    R4,DATADISP                                                      
         LA    R3,ELEM                                                          
         GOTO1 ADDEL,DMCB,(R4),(R3)                                             
*                                                                               
         L     R4,ADTRANS                                                       
PTRN110  CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'FE'                                                      
         BE    PTRN115                                                          
         ZIC   R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     PTRN110                                                          
PTRN115  MVI   0(R4),X'23'                                                      
*                                                                               
PTRN120  DS    0H                                                               
         GOTO1 ACREPORT                                                         
         AP    CHAREC,=P'1'                                                     
         L     R3,ADTRANS                                                       
         SH    R3,DATADISP                                                      
         BAS   RE,DMPPUT                                                        
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         MVI   MODE,WRITRANS                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              LEDGER LAST                                            *         
***********************************************************************         
*                                                                               
LDGL     DS    0H                                                               
         L     R3,ADACC                                                         
         USING TRNRECD,R3                                                       
         GOTO1 ACREPORT                                                         
         MVC   P(7),=C'LEDGER '                                                 
         MVC   P+8(1),TRNKLDG                                                   
         MVC   P+10(6),=C'TOTAL '                                               
         EDIT  (P6,CHAREC),(14,P+20),MINUS=YES                                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
***********************************************************************         
GETEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     XIT                                                              
                                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     XIT                                                              
                                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCOUNT '),(R2),(R3)                         
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DUMP OUT RECORDS                                       *         
***********************************************************************         
*                                                                               
         USING TRNRECD,R3                                                       
DMPGET   NTR1                                                                   
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET'                                                       
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'PUT'                                                       
*                                                                               
DUMP     CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         SR    R8,R8                                                            
         ICM   R8,3,TRNRLEN                                                     
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
XIT      XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                              
*-------------------------------------------------------------------*           
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
         DC    X'FF'                                                            
*                                                                               
CHAREC   DC    PL6'0'                                                           
*                                                                               
DKEY     DC    CL42' '                                                          
KEY1     DC    CL42' '                                                          
*                                                                               
ACDR     DC    PL6'0'                                                           
ACCR     DC    PL6'0'                                                           
JACDR    DC    PL6'0'                                                           
JACCR    DC    PL6'0'                                                           
*                                                                               
TOTDR    DC    PL6'0'                                                           
TOTCR    DC    PL6'0'                                                           
JTOTDR   DC    PL6'0'                                                           
JTOTCR   DC    PL6'0'                                                           
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'500'                                                         
AUTL     DC    A(0)                                                             
*                                                                               
ELEM     DS    CL255                                                            
*                                                                               
SCANSPC  DS    CL80                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        LITERAL POOL                                                           
*------------------------------------------------------------------*            
         LTORG                                                                  
         EJECT                                                                  
*------------------------------------------------------------------*            
*        WORKING STORAGE                                                        
*------------------------------------------------------------------*            
ACXID    DSECT                                                                  
AMARTAB  DS    A                                                                
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
TODAY2   DS    CL2                                                              
DMPSW    DS    CL1                                                              
ELIST    DS    3A                  HELLO PARM LIST                              
ELERR    DS    0XL1                HELLO ERROR RETURN BYTE                      
ELADDR   DS    A                   HELLO ELEMENT ADDRESS (GET)                  
ELADDR2  DS    A                   HELLO ELEMENT ADDRESS (ADD)                  
ELCODE   DS    CL1                                                              
SVMKT    DS    CL4                                                              
SVMKTNM  DS    CL24                                                             
SVMED    DS    CL1                                                              
SVAGY    DS    CL2                         ALPHA AGENCY                         
IO       DS    CL2000                                                           
         EJECT                                                                  
*------------------------------------------------------------------*            
*        INCLUDED DSECTS                                                        
*------------------------------------------------------------------*            
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
*  DDMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPXI02A08/16/00'                                      
         END                                                                    
