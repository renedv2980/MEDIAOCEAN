*          DATA SET ACREPXS02  AT LEVEL 001 AS OF 10/13/97                      
*PHASE ACXS02B,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'RESTORE PEELS TO OFFICE B/FRWD'                                 
ACXS02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXS**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXSD,RC                                                         
         L     R7,AMONACC                                                       
         USING ACMD,R7                                                          
         MVI   FCRESET,C'Y'                                                     
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RUN                                                       *         
***********************************************************************         
                                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   REQF00                                                           
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
                                                                                
         L     RF,GETOPT                                                        
         MVC   0(2,RF),=X'07FE'                                                 
         MVI   FCPRORAT,C'N'                                                    
         MVI   FCRNTIME,C'Y'                                                    
         MVI   FCSUPOFC,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIRST FOR REQUEST                                                   *         
***********************************************************************         
                                                                                
REQF00   CLI   MODE,REQFRST                                                     
         BNE   LDGF00                                                           
         MVI   RCSUBPRG,0                                                       
         MVI   OPT,0                                                            
         CLI   QOPT1,C'D'                                                       
         BNE   *+8                                                              
         OI    OPT,OPDMP                                                        
         CLI   QOPT2,C'F'          FORCE OFFICE                                 
         BNE   *+8                                                              
         OI    OPT,OPFRC                                                        
         MVI   FCRDACC,C'Y'                                                     
         L     R4,ADCMPEL                                                       
         USING CPYELD,R4                                                        
         TM    CPYSTAT4,CPYSOFF2   TEST 2 CHARACTER OFFICE                      
         BO    XIT                                                              
         MVI   FCRDACC,C'N'                                                     
         MVC   ACMMODE,REQFRST     SET FOR NEXT COMPANY                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LEDGER                                                    *         
***********************************************************************         
                                                                                
LDGF00   CLI   MODE,LEDGFRST                                                    
         BNE   PRAC00                                                           
         MVI   FCRDACC,C'Y'        SET TO READ ACCOUNTS                         
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCACC                                                             *         
***********************************************************************         
                                                                                
PRAC00   CLI   MODE,PROCACC                                                     
         BNE   PTRN00                                                           
         L     R2,ADLEDGER                                                      
         CLC   1(2,R2),=C'SJ'      FORCE HIGH FOR SJ                            
         BE    PRAC05                                                           
         MVI   FCRDTRNS,C'N'                                                    
         L     R4,ADACCBAL                                                      
         USING ABLELD,R4                                                        
*        CP    ABLFRWD,=P'0'       TEST ANY PEELS                               
*        BE    XIT                                                              
         BAS   RE,OFFAC            BUILD OFFICE ACCOUNT LIST                    
         CP    TOTBFR,ABLFRWD      TEST TOTALS AGREE                            
         BE    XIT                                                              
         TM    OPT,OPFRC           FORCE BALANCE                                
         BO    *+8                 SKIP TRANSACTIONS                            
         MVI   FCRDTRNS,C'Y'                                                    
                                                                                
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         LA    R3,P                                                             
         USING PLD,R3                                                           
         MVC   PLACC,ACTKULA       ACCOUNT CODE                                 
         L     RF,ADACCNAM                                                      
         USING NAMELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   PLNME(0),NAMEREC    ACCOUNT NAME                                 
         EDIT  (P8,ABLFRWD),(15,PLBFR),2,CR=YES                                 
         EDIT  (P8,ABLFRWD),(15,PLAFT),2,CR=YES                                 
         GOTO1 ACREPORT                                                         
         TM    OPT,OPFRC           FORCE BALANCE                                
         BNO   XIT                                                              
         BAS   RE,ACCL                                                          
         B     XIT                                                              
*                                                                               
PRAC05   MVI   FCRESET,C'N'        FORCE TO END OF SJ                           
         L     R2,ADACC                                                         
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(3),0(R2)                                                    
         MVI   DKEY+3,X'FF'                                                     
         BAS   RE,DMHGH                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF OFFICE ACCOUNT BALANCES                               *         
***********************************************************************         
                                                                                
OFFAC    NTR1  ,                                                                
         XC    OFCNT,OFCNT         CLEAR THE COUNT                              
         ZAP   TOTBFR,=P'0'                                                     
         ZAP   TOTAFT,=P'0'                                                     
         LA    R5,OFFTAB           TABLE OF OFFICES                             
         USING OFFD,R5                                                          
         MVC   DKEY,SPACES                                                      
         LA    R3,DKEY                                                          
         USING OFARECD,R3                                                       
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         MVC   OFAKCULA,ACTKCULA   BUILD OFFICE ACCOUNT KEY                     
*                                                                               
OFFAC3   SR    R1,R1                                                            
         IC    R1,OFAKOFF+1                                                     
         LA    R1,1(R1)            BUMP TO NEXT OFFICE                          
         STC   R1,OFAKOFF+1                                                     
         BAS   RE,DMHGH                                                         
         CLC   OFAKCULA,DIR        TEST SAME ACCOUNT                            
         BNE   XIT                                                              
         LA    R2,IO                                                            
         BAS   RE,DMGETR           GET IT                                       
         SR    R1,R1                                                            
         IC    R1,OFCNT                                                         
         LA    R1,1(R1)                                                         
         STC   R1,OFCNT            UPDATE OFFICE COUNT                          
         CLI   OFCNT,OFMAX                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   OFFCDE,OFAKOFF-OFARECD(R2)  SAVE OFFICE CODE                     
         ZAP   OFFBFR,=P'0'        BEFORE                                       
         ZAP   OFFAFT,=P'0'        AFTER                                        
         LA    R4,OFARFST-OFARECD(R2)                                           
         SR    R0,R0                                                            
                                                                                
OFFAC5   CLI   0(R4),ABLELQ        GET BALANCE ELEMENT                          
         BE    OFFAC7                                                           
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO BALANCE ELEMENT                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     OFFAC5                                                           
                                                                                
         USING ABLELD,R4                                                        
OFFAC7   ZAP   OFFBFR,ABLFRWD      SAVE BAL FRWD                                
         AP    TOTBFR,ABLFRWD                                                   
         TM    OPT,OPFRC           FORCE BALANCE                                
         BNO   *+10                                                             
         AP    TOTAFT,ABLFRWD                                                   
         LA    R5,OFFLNQ(R5)                                                    
         MVC   DKEY,DIR            GET NEXT DIRECTORY RECORD                    
         B     OFFAC3                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTIONS                                                *         
***********************************************************************         
                                                                                
PTRN00   CLI   MODE,PROCTRNS                                                    
         BNE   ACCL00                                                           
         L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         LR    R2,R4                                                            
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2          R2 POINTS TO TRANSACTION RECORD              
         OC    TRNKEY+ACCOPEEL(ACCOPLEN),TRNKEY+ACCOPEEL                        
         BZ    XIT                 ONLY PEELED                                  
         SR    R0,R0                                                            
         IC    R0,OFCNT                                                         
         LA    R5,OFFTAB                                                        
*                                                                               
         USING OFFD,R5                                                          
PTRN03   CLC   OFFCDE,TRNOFFC                                                   
         BE    PTRN05                                                           
         LA    R5,OFFLNQ(R5)                                                    
         BCT   R0,PTRN03                                                        
         DC    H'0'                NO OFFICE ACCOUNT RECORD                     
*                                                                               
PTRN05   ZAP   AMNT,TRNAMNT                                                     
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+10                                                             
         MP    AMNT,=P'-1'                                                      
         AP    OFFAFT,AMNT       ADD AMOUNT FOR PEELED TRANSACTIONS             
         AP    TOTAFT,AMNT                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ACCOUNT LAST                                                        *         
***********************************************************************         
                                                                                
ACCL00   CLI   MODE,ACCLAST                                                     
         BNE   XIT                                                              
         BAS   RE,ACCL                                                          
         B     XIT                                                              
*                                                                               
ACCL     NTR1  ,                                                                
         LA    R3,P                                                             
         USING PLD,R3                                                           
         SR    R0,R0                                                            
         IC    R0,OFCNT            PRINT OFFICE AMOUNTS                         
         LTR   R0,R0                                                            
         BZ    ACCL05                                                           
         LA    R5,OFFTAB                                                        
*                                                                               
         USING OFFD,R5                                                          
ACCL03   CP    OFFBFR,=P'0'                                                     
         BNE   *+14                                                             
         CP    OFFAFT,=P'0'                                                     
         BE    ACCL04                                                           
         MVC   PLOFC,OFFCDE                                                     
         EDIT  (P6,OFFBFR),(15,PLBFR),2,CR=YES                                  
         EDIT  (P6,OFFAFT),(15,PLAFT),2,CR=YES                                  
         GOTO1 ACREPORT                                                         
ACCL04   LA    R5,OFFLNQ(R5)                                                    
         BCT   R0,ACCL03                                                        
                                                                                
ACCL05   MVC   PLNME(15),=CL15' ** TOTAL **'                                    
         EDIT  (P6,TOTBFR),(15,PLBFR),2,CR=YES                                  
         EDIT  (P6,TOTAFT),(15,PLAFT),2,CR=YES                                  
         GOTO1 ACREPORT                                                         
                                                                                
         L     R4,ADACCBAL                                                      
         USING ABLELD,R4                                                        
         ZAP   TOTDIF,ABLFRWD     GET THE DIFFERENCE                            
         SP    TOTDIF,TOTAFT                                                    
         CP    ABLFRWD,TOTAFT     WILL THIS FIX'EM                              
         BNE   ACCL07                                                           
         CLI   OFCNT,0                                                          
         BE    ACCL07                                                           
         MVC   PLNME(15),=CL15'* REPAIRING *'                                   
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         BAS   RE,FXBL             FIX THE BALANCES                             
         B     XIT                                                              
                                                                                
ACCL07   TM    OPT,OPFRC           FORCE OFFICE                                 
         BNO   ACCL09                                                           
         BAS   RE,FRBL                                                          
         BNE   ACCL09                                                           
         B     XIT                                                              
                                                                                
ACCL09   MVC   PLNME(15),=CL15'NOT REPAIRING'                                   
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIX THE OFFICE ACCOUNT BALANCE FORWARDS                             *         
***********************************************************************         
                                                                                
FXBL     NTR1  ,                                                                
         SR    R0,R0                                                            
         IC    R0,OFCNT                                                         
         LA    R5,OFFTAB                                                        
         USING OFFD,R5                                                          
         MVC   DKEY,SPACES                                                      
         LA    R3,DKEY                                                          
         USING OFARECD,R3                                                       
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         MVC   OFAKCULA,ACTKCULA   BUILD OFFICE ACCOUNT KEY                     
*                                                                               
FXBL3    CP    OFFBFR,OFFAFT       NO CHANGE                                    
         BE    FXBL9                                                            
         MVC   OFAKOFF,OFFCDE                                                   
         BAS   RE,DMRD                                                          
         CLC   OFAKEY,DIR          TEST SAME ACCOUNT                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IO                                                            
         BAS   RE,DMGETR                                                        
         LA    R4,OFARFST-OFARECD(R2)                                           
         SR    R1,R1                                                            
                                                                                
FXBL5    CLI   0(R4),ABLELQ        GET BALANCE ELEMENT                          
         BE    FXBL7                                                            
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO BALANCE ELEMENT                           
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     FXBL5                                                            
                                                                                
         USING ABLELD,R4                                                        
FXBL7    DS    0H                                                               
         BAS   RE,DMPGET                                                        
         ZAP   ABLFRWD,=P'0'       UPDATE BALANCE                               
         BAS   RE,DMPUTR           PUT CORRECTED RECORD                         
         BAS   RE,DMPPUT                                                        
FXBL9    LA    R5,OFFLNQ(R5)                                                    
         BCT   R0,FXBL3                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FORCE BALANCE TO ONE OFFICE                                         *         
***********************************************************************         
                                                                                
FRBL     NTR1  ,                                                                
         MVC   DKEY,SPACES                                                      
         LA    R3,DKEY                                                          
         USING OFARECD,R3                                                       
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         MVC   OFAKCULA,ACTKCULA   BUILD OFFICE ACCOUNT KEY                     
*                                                                               
FRBL2    MVC   OFAKOFF,=X'4041'                                                 
         BAS   RE,DMHGH                                                         
         CLC   OFAKEY(L'OFAKCULA),DIR    TEST SAME ACCOUNT                      
         BNE   XIT                 CAN'T FIX                                    
*                                                                               
FRBL3    LA    R2,IO                                                            
         BAS   RE,DMGETR                                                        
         LA    R4,OFARFST-OFARECD(R2)                                           
         SR    R1,R1                                                            
                                                                                
FRBL5    CLI   0(R4),ABLELQ        GET BALANCE ELEMENT                          
         BE    FRBL7                                                            
         CLI   0(R4),0             CAN'T FIX                                    
         BNE   XIT                                                              
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     FRBL5                                                            
                                                                                
         USING ABLELD,R4                                                        
FRBL7    AP    ABLFRWD,TOTDIF      UPDATE BALANCE                               
         BAS   RE,DMPUTR           PUT CORRECTED RECORD                         
         LA    R3,P                                                             
         USING PLD,R3                                                           
         MVC   PLNME(17),=CL17'BALANCE TO OFFICE'                               
         MVC   PLNME+18(2),OFAKOFF-OFARECD(R2)                                  
         EDIT  (P6,TOTDIF),(15,PLAFT),2,CR=YES                                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         CR    RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
                                                                                
DMRD     ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGETR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R2),DMWORK                        
         B     DMERR                                                            
*                                                                               
DMWRTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         B     DMERR                                                            
*                                                                               
DMADDR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R2),DMWORK                        
         B     DMERR                                                            
*                                                                               
DMPUTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R2),DMWORK                        
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO DUMP RECORDS                                            *         
***********************************************************************         
                                                                                
DMPGET   NTR1  ,                                                                
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
DMPPUT   NTR1  ,                                                                
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT'                                                       
*                                                                               
DUMP     TM    OPT,OPDMP           DUMP RECORDS                                 
         BNO   XIT                                                              
         SR    R8,R8                                                            
         ICM   R8,3,ACTKEY+ACCORLEN                                             
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R2),C'DUMP',(R8),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO DELETE/ADD ELEMENTS                                     *         
***********************************************************************         
                                                                                
*  P1   BYTE 0    ELEMENT CODE                                                  
*       BYTE 1-3  A(RECORD)                                                     
*  P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                                     
*       BYTE 1-3  A(SEARCH ARGUMENT)                                            
                                                                                
DELEL    NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',ACCOUNT),((R4),(R2)),((R5),(R3))                
         B     XIT                                                              
                                                                                
*  P1   A(RECORD)                                                               
*  P2   A(ELEMENT)                                                              
                                                                                
ADDEL    NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCOUNT),(R2),(R3)                              
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
                                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
                                                                                
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'1000'                                                        
*                                                                               
ALL      EQU   X'FF'                                                            
FF       EQU   X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ACXSD    DSECT                                                                  
SAVRE    DS    F                                                                
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    XL4                                                              
*                                                                               
TODAY2   DS    CL2                                                              
*                                                                               
OPT      DS    X                   OPTION SWITCH                                
OPDMP    EQU   X'80'               DUMP OUTPUT RECORDS                          
OPFRC    EQU   X'40'               FORCE OFFICE CODE                            
*                                                                               
AMNT     DS    PL10                                                             
OFCNT    DS    XL1                 NUMBER OF OFFICE ENTRIES                     
OFMAX    EQU   200                                                              
OFFTAB   DS    XL(OFMAX*OFFLNQ)    SEE OFFD                                     
*                                                                               
TOTBFR   DS    PL6                 TOTAL BEFORE                                 
TOTAFT   DS    PL6                 TOTAL BEFORE                                 
TOTDIF   DS    PL6                 TOTAL DIFFERENCE                             
IO       DS    XL2000                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
                                                                                
PLD      DSECT                                                                  
         DS    XL1                                                              
PLACC    DS    CL14                ACCOUNT                                      
         ORG   PLACC+2                                                          
PLOFC    DS    CL2                 OFFICE                                       
         ORG   PLACC+L'PLACC                                                    
         DS    XL1                                                              
PLNME    DS    CL36                NAME                                         
         DS    XL1                                                              
PLBFR    DS    CL15                B/FRWD BEFORE                                
         DS    XL1                                                              
PLAFT    DS    CL15                B/FRWD AFTER                                 
         EJECT                                                                  
***********************************************************************         
* DSECT FOR OFFICE TABLE ENTRY                                        *         
***********************************************************************         
                                                                                
OFFD     DSECT                                                                  
OFFCDE   DS    XL2                 OFFICE CODE                                  
OFFBFR   DS    PL6                 BALANCE FORWARD BEFORE                       
OFFAFT   DS    PL6                 BALANCE FORWARD AFTER                        
OFFLNQ   EQU   *-OFFD                                                           
         EJECT                                                                  
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  ACGENMODES                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPXS02 10/13/97'                                      
         END                                                                    
