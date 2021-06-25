*          DATA SET ACREPXR02  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACXR02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'ADD ORIGINATING SERVER TO PRESTO ORDERS'                        
ACXR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXR**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXRD,RC                                                         
         ST    R5,RELO                                                          
*-------------------------------------------------------------------*           
*        FIRST FOR RUN                                                          
*-------------------------------------------------------------------*           
         CLI   MODE,RUNFRST                                                     
         BNE   ORDER                                                            
*                                                                               
         L     R7,AMONACC                                                       
         USING ACMD,R7                                                          
         MVC   HELLO,ACMVHELO                                                   
         L     RE,=V(PRNTBL)                                                    
         A     RE,RELO                                                          
         ST    RE,PRNTBL                                                        
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCRNTIME,C'Y'                                                    
         MVC   PRODUL,ACMPROD      EXTRACT PRODUCTION UNIT/LEDGER               
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCESS ORDER                                                          
*-------------------------------------------------------------------*           
*                                                                               
ORDER    CLI   MODE,PROCORD                                                     
         BNE   RUNL00                                                           
*                                                                               
         L     R4,ADACC            R4=A(ORDER KEY)                              
         USING ORDRECD,R4                                                       
         CLC   ORDKORD,=C'000000'  TEST FOR CONTROL RECORD                      
         BE    ORDERX                                                           
*                                                                               
         MVI   ELCODE,ORDELQ                                                    
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         BNE   ORDERX                                                           
*                                                                               
         USING ORDELD,R6                                                        
*&&US*&& CLI   ORDLN,ORDLN2Q                                                    
*&&UK*&& CLI   ORDLN,ORDLN3Q                                                    
         BL    ORDERX                                                           
         CLC   PRODUL,ORDACCU      TEST FOR PRODUCTION LEDGER                   
         BNE   ORDERX                                                           
*                                                                               
         TM    ORDSTAT,ORDSPRES    TEST FOR PRESTO ORDER                        
         BNO   ORDERX              NO                                           
*                                                                               
         AP    PRESTORD,=P'1'                                                   
*                                                                               
ORDER10  MVI   ELCODE,FFTELQ                                                    
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         USING FFTELD,R6                                                        
*                                                                               
ORDER20  BNE   ORDER30             NO MORE FREE FORM TEXT                       
         CLI   FFTTYPE,FFTTSQLI    TEST FOR SQL SERVER CODE                     
         BE    ORDERX              YES                                          
*                                                                               
         BAS   RE,NEXTEL                                                        
         B     ORDER20                                                          
*                                                                               
ORDER30  BAS   RE,DMPGET                                                        
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTSQLI                                                 
         MVI   FFTDLEN,8                                                        
         LA    R0,FIXES            R0=COUNTER                                   
         LA    RE,FIXTBL                                                        
         CLC   ALPHAID,0(RE)       MATCH ON AGENCY ALPHA                        
         BE    ORDER40                                                          
         LA    RE,L'FIXTBL(RE)                                                  
         BCT   R0,*-14                                                          
         B     ORDERX              NOT AN AGENCY WE ARE CONCERNED WITH          
*                                                                               
ORDER40  MVC   FFTDATA(8),2(RE)                                                 
         ZIC   R1,FFTDLEN                                                       
         LA    R1,FFTDATA-FFTELD(R1)                                            
         STC   R1,FFTLN                                                         
         GOTO1 ADDEL,DMCB,(R4),FFTELD                                           
         BAS   RE,DMPPUT                                                        
         AP    FIXORD,=P'1'                                                     
*                                                                               
ORDER50  CLI   RCWRITE,C'N'        TEST IF OK TO WRITE                          
         BE    ORDERX              NO                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,DMWRT,ACCOUNT,ORDRECD,ORDRECD                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    WRITORD,=P'1'                                                    
*                                                                               
ORDERX   B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        RUNLAST                                                                
*-------------------------------------------------------------------*           
RUNL00   CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
*                                                                               
         GOTO1 ACREPORT                                                         
         MVC   P(6),=C'AGENCY'                                                  
         MVC   P+08(6),=C'TOTAL '                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         GOTO1 ACREPORT                                                         
         EDIT  (P6,PRESTORD),(14,P+20)                                          
         MVC   P+1(16),=CL16'PRESTO ORDERS'                                     
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         EDIT  (P6,FIXORD),(14,P+20)                                            
         MVC   P+1(16),=CL16'FIXED ORDERS'                                      
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         EDIT  (P6,WRITORD),(14,P+20)                                           
         MVC   P+1(17),=CL17'ORDERS WROTE BACK'                                 
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
YES      CR    RB,RB                                                            
         B     XIT                                                              
NO       LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ROUTINES TO DUMP OUT RECORDS                                           
*-------------------------------------------------------------------*           
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET'                                                       
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT'                                                       
*                                                                               
DUMP     CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         L     R3,ADACC                                                         
         SR    R8,R8                                                            
         ICM   R8,3,ACCORLEN(R3)                                                
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              ROUTINE TO GET AN ELEMENT                                        
*                                                                               
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
*-------------------------------------------------------------------*           
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',ACCOUNT),((R4),(R2)),((R5),(R3))                
         B     XIT                                                              
         SPACE 1                                                                
*-------------------------------------------------------------------*           
*              ROUTINE TO ADD AN ELEMENT                                        
*                                                                               
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
*-------------------------------------------------------------------*           
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCOUNT),(R2),(R3)                              
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                              
*-------------------------------------------------------------------*           
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
*                                                                               
PRESTORD DC    PL6'0'                                                           
FIXORD   DC    PL6'0'                                                           
WRITORD  DC    PL6'0'                                                           
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'5'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'20'                                                          
*                                                                               
FIXTBL   DS    0CL10                                                            
*&&US*&& DC    C'JW',CL8'SJWNY001'                                              
*&&US*&& DC    C'BD',CL8'SBDDE001'                                              
*&&US*&& DC    C'*B',CL8'ESSNY03 '                                              
*&&UK*&& DC    C'WJ',CL8'WCJESS01'                                              
FIXES    EQU   (*-FIXTBL)/L'FIXTBL                                              
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DSECT TO COVER WORKING STORAGE                                                
*                                                                               
ACXRD    DSECT                                                                  
RELO     DS    A                                                                
HELLO    DS    V                                                                
PRNTBL   DS    V                                                                
PARM     DS    6F                                                               
DMPSW    DS    CL1                                                              
ELCODE   DS    CL1                                                              
PRODUL   DS    CL2                                                              
ELEMENT  DS    XL255                                                            
         EJECT                                                                  
*                                                                               
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPXR02 08/16/00'                                      
         END                                                                    
