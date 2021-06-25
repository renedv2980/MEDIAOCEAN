*          DATA SET ACREPXI02  AT LEVEL 048 AS OF 08/16/00                      
*PHASE ACXI02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'LOOK FOR DB ELEMENT'                                            
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
         L     R2,=A(MARTAB)                                                    
         ST    R2,AMARTAB                                                       
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
*                                                                               
         L     RF,ADMASTC                                                       
         MVC   AUTL,MCUTL-MASTD(RF)      GET ADDRESS OF UTL                     
         L     RF,AUTL                                                          
         MVC   SYSTSEN,4(RF)             SAVE SYSTEM UTL                        
*                                                                               
         L     RF,AUTL                                                          
         MVC   4(1,RF),SPOTSEN           SPOT SYSTEM UTL                        
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),SPOT,SPFILEL                         
         TM    8(R1),0                                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,AUTL                                                          
         MVC   4(1,RF),SYSTSEN           RESTORE ACC SYSTEM UTL                 
         MVC   SVMKT,SPACES                                                     
         MVC   SVMKTNM,SPACES                                                   
         L     R3,AMARTAB                   INIT TABLE                          
         MVI   0(R3),X'FF'                                                      
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
*              LEDGER FIRST                                           *         
***********************************************************************         
*                                                                               
LDGF     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FCRDTRNS,C'N'       AND TRANSACTIONS                             
         L     R2,ADLEDGER                                                      
         CLC   1(2,R2),=C'SJ'      FOR SJ                                       
         BE    XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDTRNS,C'Y'                                                    
         ZAP   CHAREC,=P'0'                                                     
         ZAP   ACDR,=P'0'                                                       
         ZAP   ACCR,=P'0'                                                       
         ZAP   JACDR,=P'0'                                                      
         ZAP   JACCR,=P'0'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PROCTRNS                                               *         
***********************************************************************         
*                                                                               
PTRN     DS    0H                                                               
         L     R5,ADTRANS                                                       
         USING TRNELD,R5                                                        
         LR    R3,R5                                                            
         SH    R3,DATADISP                                                      
*                                                                               
         USING TRNRECD,R3                                                       
*                                                                               
         L     R2,ADTRANS                                                       
PTRN06   CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'DB'                                                      
         BE    PTRN10                                                           
PTRN08   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    PTRN06                                                           
         DC    H'0'                                                             
         USING FFTELD,R2                                                        
PTRN10   CLI   FFTTYPE,FFTTMRKT                                                 
         BNE   PTRN08                                                           
         CLI   FFTLN,X'09'                                                      
         BNE   PTRN08                                                           
         BAS   RE,DMPGET                                                        
         BAS   RE,MKTUP                                                         
*                                                                               
         MVI   FFTEL,X'FF'                                                      
         GOTO1 HELLO,ELIST,(C'D',=C'ACCOUNT'),(X'FF',TRNRECD),0                 
         LA    R2,ELEM                                                          
         CLI   FFTEL,FFTELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 HELLO,ELIST,(C'P',=C'ACCOUNT'),TRNRECD,ELEM,            X        
               =C'ADD=END'                                                      
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
*                                                                               
PTRN114  DS    0H                                                               
         MVC   P+1(14),TRNKULA                                                  
         MVC   P+17(14),TRNKULC                                                 
         MVC   P+33(6),TRNKREF                                                  
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(0,P+42)                                
         MVC   P+49(2),TRNOFFC                                                  
         MVC   P+54(4),FFTMRKTC                                                 
         EDIT  (B1,TRNTYPE),(2,P+62)                                            
         TM    TRNSTAT,X'80'                                                    
         BZ    PTRN118                                                          
         EDIT  (P6,TRNAMNT),(12,P+68),2,MINUS=YES                               
         AP    ACDR,TRNAMNT                                                     
         B     PTRN120                                                          
*                                                                               
PTRN118  EDIT  (P6,TRNAMNT),(12,P+91),2,MINUS=YES                               
         AP    ACCR,TRNAMNT                                                     
*                                                                               
PTRN120  DS    0H                                                               
         GOTO1 ACREPORT                                                         
         AP    CHAREC,=P'1'                                                     
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
         EDIT  (P6,ACDR),(12,P+68),2,MINUS=YES                                  
         EDIT  (P6,ACCR),(12,P+91),2,MINUS=YES                                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              MARKET NAME LOOKUP                                     *         
***********************************************************************         
*                                                                               
MKTUP    NTR1                                                                   
         MVC   SVMED,SPACES                                                     
         MVC   SVMKT,SPACES                                                     
         MVC   SVMKTNM,SPACES                                                   
         LA    R3,KEY1                                                          
         USING MARKD,R3                                                         
         MVC   MARKD(MARLEN),SPACES                                             
         USING FFTELD,R2                                                        
         CLI   FFTTYPE,FFTTMRKT                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   FFTLN,X'09'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MARCOD,FFTMRKTC             SAVE MARKET CODE                     
         MVC   SVMKT,FFTMRKTC                                                   
*                                                                               
         L     R2,ADTRANS                                                       
MKTUP02  CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'1A'                 LOOK FOR MEDIA CODE                  
         BE    MKTUP04                                                          
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    MKTUP02                                                          
         DC    H'0'                                                             
         USING MDTELD,R2                                                        
MKTUP04  CLI   MDTSYS,MDTSSPOT                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVMED,MDTMED                                                     
         MVC   MARMED,MDTMED                                                    
         L     R3,AMARTAB                                                       
MRKUP06  CLI   0(R3),X'FF'                                                      
         BE    MRKUP08                                                          
         CLC   MARKD(MARKLEN),KEY1            ALREADY IN NAME TABLE?            
         BE    MRKUP18                                                          
         LA    R3,MARLEN(R3)                                                    
         B     MRKUP06                                                          
*                                                                               
MRKUP08  DS    0H                                                               
         XC    DKEY,DKEY                                                        
         MVI   DKEY,C'0'                                                        
         MVC   DKEY+1(16),DKEY                                                  
         LA    R4,DKEY                                                          
         USING MKTRECD,R4          MARKET RECORD ON STATION FILE                
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,SVMED                                                    
         MVC   MKTKAGY,SVAGY                                                    
         MVC   MKTKMKT,SVMKT       MARKET CODE                                  
         MVC   KEYSAVE,DKEY                                                     
         L     RE,AUTL                                                          
         MVC   4(1,RE),SPOTSEN           SPOT SYSTEM UTL                        
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,DKEY,IO    READ HI STATION           
         LA    R4,IO                                                            
         CLC   MKTREC(MKTKEYLQ),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MARMED,MKTKMED                                                   
         MVC   MARCOD,MKTKMKT                                                   
         MVC   MARNAME,MKTNAME                                                  
         OC    MARNAME,MKTNAME                                                  
         LA    RF,MARLEN(R3)                                                    
         C     RF,=A(MARLAST)                                                   
         BL    *+6                                                              
         DC    H'0'                        TABLE FULL                           
         MVI   0(RF),X'FF'                                                      
*                                                                               
MRKUP18  DS    0H                                                               
         MVC   SVMKT,MARCOD                                                     
         MVC   SVMKTNM,MARNAME                                                  
*                                                                               
         XC    ELEM,ELEM           PRE CLEAR ELEMENT                            
         LA    R2,ELEM             R2=A(ELEMENT TO BUILD)                       
         USING FFTELD,R2                                                        
         MVI   FFTEL,FFTELQ        SET ELEMENT TYPE                             
         LA    RE,FFTLN1Q          ELEMENT OVERHEAD                             
         LA    R1,L'FFTDLEN        ACTUAL LENGTH OF TEXT                        
         AR    RE,R1                                                            
         LA    R1,L'FFTMRKTC       LENGTH OF MARKET CODE                        
         AR    RE,R1                                                            
         STC   RE,FFTLN                  SAVE LENGTH                            
         MVI   FFTTYPE,FFTTMRKT          MARKET CODE EQUATE                     
         MVC   FFTDLEN,=AL1(L'FFTMRKTC)                                         
         MVC   FFTMRKTC,SVMKT            SAVE MARKET CODE                       
         OC    FFTMRKTC,SPACES           MAKE SURE PADDED WITH SPACES           
         CLC   FFTMRKTC,SPACES                                                  
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   FFTMRKTN(L'SVMKTNM),SPACES                                       
         OC    SVMKTNM,SPACES                                                   
         CLC   SVMKTNM,SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,SVMKTNM+L'SVMKTNM-1                                           
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,SVMKTNM                                                       
         SR    R1,R0                                                            
         EX    R1,*+4                                                           
         MVC   FFTMRKTN(0),SVMKTNM                                              
         LA    R1,1(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,FFTLN                                                         
         AR    RE,R1                                                            
         STC   RE,FFTLN            SAVE ELEMENT LENGTH                          
         SR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         AR    RE,R1                                                            
         STC   RE,FFTDLEN          SAVE DATA ENTRY LENGTH                       
*                                                                               
         L     RF,AUTL                                                          
         MVC   4(1,RF),SYSTSEN           RESTORE ACC SYSTEM UTL                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DUMP OUT RECORDS                                       *         
***********************************************************************         
*                                                                               
         USING TRNRECD,R3                                                       
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
SPOT     DC    C'SPOT    '                                                      
STATION  DC    C'STATION '                                                      
SPFILEL  DC    C'NSPTDIR NSPTFIL NSTAFIL X'                                     
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
MAXDUMP  DC    PL4'50'                                                          
AUTL     DC    A(0)                                                             
*                                                                               
SYSNUMB  DC    CL6'SPOT G'         SYSTEM AND NUMBER (SPOT X)                   
SPTNUM   DC    C'G'                SPOT/NET LOGICAL NUMBER CHARACTER            
SEN      DC    X'02'               BASE SYSTEM NUMBER SPOT IS 2                 
SPOTSEN  DC    X'F3'               LOGICAL SYSTEM NUMBER                        
SYSTSEN  DC    X'00'               SYSTEM SE NUMBER                             
ELEM     DS    CL255                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
MARTAB   DS    CL(MARMAX*MARLEN)                                                
         ORG   *-MARLEN                                                         
MARLAST  DS    CL(MARLEN)                                                       
MARMAX   EQU   250                                                              
*                                                                               
         EJECT                                                                  
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
*                                                                               
MARKD    DSECT                                                                  
MARMED   DS    CL1                         MEDIA                                
MARCOD   DS    CL4                         MRKT CODE                            
MARKLEN  EQU   *-MARKD                                                          
MARNAME  DS    CL24                        MRKT NAME                            
MARLEN   EQU   *-MARKD                                                          
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
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
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048ACREPXI02 08/16/00'                                      
         END                                                                    
