*          DATA SET ACREPZ102  AT LEVEL 082 AS OF 10/04/00                      
*PHASE ACZ102A,*                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'DELETE SJ WORKCODE ** TRANSACTIONS FROM YNRA'                   
ACZ102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZ1**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZ1D,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
         XC    LSTUL,LSTUL                                                      
         ZAP   PKCOUNT,=P'0'                                                    
         ZAP   PKDMPCNT,=P'0'                                                   
         ZAP   PKDMPMAX,=P'500'                                                 
*                                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
*                                                                               
         LA    RE,IO               RE=A(IO AREA)                                
         LA    RF,IOLNQ            RF=(LENGTH OF IO AREA)                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
RUNFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                     *           
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
*        BAS   RE,GETUL            GET UNIT AND LEDGER INFO                     
*                                                                               
         USING TRNRECD,R3                                                       
         LA    R3,SVKEY                                                         
         USING PLINED,R4                                                        
         LA    R4,P                                                             
*                                                                               
         MVC   TRNKEY,SPACES       CLEAR KEY                                    
         MVC   TRNKCPY,RCCOMPFL    COMPANY CODE                                 
         MVC   TRNKUNT(2),=C'SJ'   U/L                                          
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         B     REQF20                                                           
*                                                                               
REQF10   GOTO1 =A(DMSEQDR),DMCB,(RC)            READ SEQ                        
REQF20   LA    R1,3                             ASSUME C/U/L FILLED IN          
*        CLC   QUNIT(2),SPACES                                                  
*        BH    *+8                                                              
*        LA    R1,1                             ELSE JUST COMPANY               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVKEY(0),IOKEY                   SAME KEY?                       
         BNE   REQFX                                                            
*                                                                               
         LA    R3,IOKEY                                                         
*                                                                               
         CLC   TRNKACT,SPACES      ALWAYS GET AN ACCOUNT                        
         BNH   REQF10                                                           
*                                                                               
         CLC   TRNKWORK,=C'**'     IS IT WORK CODE **                           
         BNE   REQF10                                                           
*                                                                               
         CLC   =C'T0',TRNKREF    NEED ACCOUNTS WITH T0 AT STRT OF REF           
         BNE   REQF10                                                           
*                                                                               
         LA    R5,ACTABL                                                        
REQF30   CLI   0(R5),X'FF'                                                      
         BE    REQF10              THIS IS NOT THE ACCOUNT WE NEED              
         CLC   TRNKACT,0(R5)                                                    
         BE    REQF40                                                           
         LA    R5,L'ACTABL(R5)                                                  
         B     REQF30                                                           
*                                                                               
REQF40   DS    0H                                                               
         MVC   SVIOKY,IOKEY                                                     
         MVC   MSG,=CL15'DIR REC IN'                                            
         GOTO1 ADUMP,DMCB,(RC),(R3),56                                          
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)     GET RECORD                            
         LA    R3,IO                                                            
*                                                                               
         AP    PKCOUNT,=P'1'                                                    
*                                                                               
         USING TRNELD,R2                                                        
         LA    R2,TRNRFST                                                       
         CLI   0(R2),TRNELQ        X'44'                                        
         BE    *+6                                                              
         DC    H'00'               JUST CHECKING                                
*                                                                               
         CLI   TRNTYPE,TRNTORD     IS IT TYPE 12                                
         BE    REQF10              DON'T NEED IT                                
*                                                                               
         LA    R7,P                POINT TO PRINT LINE                          
*        CP    PKLNCNT,=P'0'       IS IT FIRST TIME IN                          
*        BNE   DMXREC10                                                         
         GOTO1 HEXOUT,DMCB,TRNKCPY,PRTCPY,L'TRNKCPY                             
         MVC   PRTULA,TRNKULA                                                   
         MVC   PRTWCD,TRNKWORK                                                  
         GOTO1 DATCON,DMCB,(1,TRNDATE),(8,PRTDATE)                              
         MVC   PRTREF,TRNREF                         REFERENCE                  
         GOTO1 HEXOUT,DMCB,TRNSUB,PRTSBR,L'TRNSUB    SUB REFERENCE              
         MVC   PRTBTCH,TRNBTCH                       BATCH                      
         EDIT  (P6,TRNAMNT),PRTAMNT,2,MINUS=YES      TRANSATION AMNT            
         DROP  R2                                                               
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   MSG,=CL15'MST REC IN'                                            
         SR    R6,R6                                                            
         ICM   R6,3,TRNRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
*                                                                               
         OI    TRNRSTAT,TRNSDELT                                                
*                                                                               
         MVC   MSG,=CL15'MST REC OUT'                                           
         SR    R6,R6                                                            
         ICM   R6,3,TRNRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
*                                                                               
         CLI   QOPT1,C'Y'          DO WE WANT TO UPDATE?                        
         BNE   REQF50                                                           
         CLI   RCWRITE,C'N'                                                     
         BE    REQF50                                                           
         GOTO1 =A(DMPUTREC),DMCB,(RC)     WRITE RECORD BACK                     
         CLI   DMCB+8,0                   DUMP ON ERROR                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REQF50   LA    R3,SVIOKY                                                        
         OI    TRNKSTAT,TRNSDELT                                                
*                                                                               
         MVC   MSG,=CL15'DIR REC OUT'                                           
         GOTO1 ADUMP,DMCB,(RC),(R3),56                                          
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   REQF10                                                           
         CLI   RCWRITE,C'N'                                                     
         BE    REQF10                                                           
         GOTO1 =A(DMWRTDR),DMCB,(RC)                                            
         CLI   DMCB+8,0                   DUMP ON ERROR                         
         BZ    REQF10                                                           
         DC    H'0'                                                             
*                                                                               
REQFX    DS    0H                                                               
         CP    PKCOUNT,=P'0'                                                    
         BE    EXIT                                                             
         BAS   RE,PRINTIT                                                       
         MVC   P(18),=CL18'RECORDS DELETED : '                                  
         EDIT  PKCOUNT,(10,P+25)                                                
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
* PRINT ROUTINE                                                      *          
**********************************************************************          
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
*        BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SETUP HEADLINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         MVC   HEAD5+10(L'LSTUNT),LSTUNT     UNIT CODE                          
         MVC   HEAD5+20(L'UNTNME),UNTNME     UNIT NAME                          
         MVC   HEAD6+10(L'LSTLDG),LSTLDG     LEDGER CODE                        
         MVC   HEAD6+20(L'LDGNME),LDGNME     LEDGER NAME                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
ACCMST   DC    CL8'ACCMST'                                                      
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
         DC    V(PRNTBL)           PRINT DATA                                   
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* GETEL #2                                                           *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R6,DISP2,ELCODE,2                                               
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         CP    PKDMPCNT,PKDMPMAX                                                
         BH    DUMPX                                                            
         AP    PKDMPCNT,=P'1'                                                   
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLE OF SJ ACCOUNTS THAT NEEDS TO BE DELETED                      *          
**********************************************************************          
ACTABL   DC    0CL12                                                            
         DC    C'ANDITRA47015'                                                  
         DC    C'CBTWS 470229'                                                  
         DC    C'CNLALM470100'                                                  
         DC    C'CNLALR470101'                                                  
         DC    C'CNLBEV470102'                                                  
         DC    C'CNLCHI470106'                                                  
         DC    C'CNLCH5470105'                                                  
         DC    C'CNLCNLA00003'                                                  
         DC    C'CNLDAL470108'                                                  
         DC    C'CNLFAS470109'                                                  
         DC    C'CNLHON470110'                                                  
         DC    C'CNLHOU470111'                                                  
         DC    C'CNLMAU470113'                                                  
         DC    C'CNLNY 470114'                                                  
         DC    C'CNLPAL470115'                                                  
         DC    C'CNLSCP470116'                                                  
         DC    C'CNLSF 470117'                                                  
         DC    C'CNLWAS470119'                                                  
         DC    C'GLXCOR470151'                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMWRTDR  NMOD1 0,WRT               WRITE BACK TO DIR                            
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',SVIOKY,SVIOKY          
         B     DMX                                                              
*                                                                               
DMADDDR  NMOD1 0,ADD               ADD KEY TO DIR                               
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMADD'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         MVC   SVDA,ACCKDA         SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,IO,DMWORK                    
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',SVDA,IO,DMWORK              
*                                                                               
DMX      XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACZ1D    DSECT                                                                  
VTYPES   DS    0A                                                               
ADUMP    DS    A                   ROUTINE TO DITTO                             
PRNTBL   DS    V                   PRINT DATA                                   
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
SVDA     DS    F                   SAVED AREA FOR DISK ADDRESS                  
SVKEY    DS    CL49                SAVED AREA FOR KEY                           
SVIOKY   DS    CL56                SAVED AREA FOR IO KEY WRITE                  
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO KEY                          
ELCODE   DS    CL1                 ELEMENT CODE                                 
MSG      DS    CL15                DUMP MESSAGE                                 
PKCOUNT  DS    PL8                 NUMBER OF ENTRIES FIXED/ADDED                
PKDMPCNT DS    PL8                 NUMBER OF DUMPS                              
PKDMPMAX DS    PL4                 MAXIMUM NUMBER OF DUMPS                      
*                                                                               
LSTUL    DS    0CL2                LAST U/L                                     
LSTUNT   DS    CL1                 UNIT                                         
LSTLDG   DS    CL1                 LEDGER                                       
*                                                                               
UNTNME   DS    CL20                UNIT NAME                                    
LDGNME   DS    CL20                LEDGER NAME                                  
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL2                                                              
PRTCPY   DS    CL2                 COMPANY CODE                                 
         DS    CL3                                                              
PRTULA   DS    CL14                UNIT/LEDGER                                  
         DS    CL2                                                              
PRTWCD   DS    CL2                 WORK CODE                                    
         DS    CL3                                                              
PRTDATE  DS    CL8                 TRANSACTION DATE                             
         DS    CL2                                                              
PRTREF   DS    CL6                 TRANSACTION REFERENCE                        
         DS    CL2                                                              
PRTSBR   DS    CL2                 TRANSACTION SUB REFERENCE                    
         DS    CL7                                                              
PRTBTCH  DS    CL6                 TRANSACTION BATCH REFERENCE                  
         DS    CL2                                                              
PRTAMNT  DS    CL14                TRANSACTION  AMOUNT                          
         DS    CL5                                                              
PLINLNQ  EQU   *-PLINED                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACBIGPRINTD                                                                   
*ACREPWORKD                                                                     
*ACGENFILE                                                                      
*ACGENMODES                                                                     
*DDLOGOD                                                                        
*ACMASTD                                                                        
*DDMASTD                                                                        
*DDBIGBOX                                                                       
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'082ACREPZ102 10/04/00'                                      
         END                                                                    
