*          DATA SET ACREPZQ02  AT LEVEL 066 AS OF 03/05/15                      
*PHASE ACZQ02A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'CHANGE MOA IN TMS RECORDS (NON BILL)'                           
         PRINT NOGEN                                                            
ACZQ02   CSECT                                                                  
         NMOD1 0,**ACZQ**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZQD,RC                                                         
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCACC        PROCESS ACCOUNT                              
         BE    PACC                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   TOTHRS,=P'0'                                                     
         ZAP   TOTACC,=P'0'                                                     
         XC    NTSH,NTSH                                                        
         XC    FLAG,FLAG                                                        
         ZAP   PKERR,=P'0'                                                      
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS AN ACCOUNT                                                 *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R6                                                       
PACC     DS    0H                                                               
         L     R6,ADACC                                                         
*                                                                               
         USING RSTELD,R1                                                        
         L     R1,ADACCSTA                                                      
*                                                                               
         CLI   QFILTER1,X'40'      ANYTHING IN FILTER1?                         
         BE    PACC10                                                           
         MVC   BYTE,QFILTER1       SAVE OFF FILTER INFO                         
         MVI   MASK,NOTEQUAL       SET TO NEGATIVE FILTERING                    
         TM    BYTE,X'40'                                                       
         BO    *+8                                                              
         MVI   MASK,EQUAL          SET TO POSITIVE FILTERING                    
         OI    BYTE,X'40'          CAPITALIZE IF NECESSARY                      
         MVC   COND1+1(1),MASK                                                  
*                                                                               
         CLC   BYTE,RSTFILT1                                                    
COND1    BC    0,PACCX                                                          
*                                                                               
PACC10   CLI   QFILTER2,X'40'      ANYTHING IN FILTER2?                         
         BE    PACC20                                                           
         MVC   BYTE,QFILTER2       SAVE OFF FILTER INFO                         
         MVI   MASK,NOTEQUAL       SET TO NEGATIVE FILTERING                    
         TM    BYTE,X'40'                                                       
         BO    *+8                                                              
         MVI   MASK,EQUAL          SET TO POSITIVE FILTERING                    
         OI    BYTE,X'40'          CAPITALIZE IF NECESSARY                      
         MVC   COND2+1(1),MASK                                                  
*                                                                               
         CLC   BYTE,RSTFILT2                                                    
COND2    BC    0,PACCX                                                          
*                                                                               
PACC20   CLI   QFILTER3,X'40'      ANYTHING IN FILTER3?                         
         BE    PACC30                                                           
         MVC   BYTE,QFILTER3       SAVE OFF FILTER INFO                         
         MVI   MASK,NOTEQUAL       SET TO NEGATIVE FILTERING                    
         TM    BYTE,X'40'                                                       
         BO    *+8                                                              
         MVI   MASK,EQUAL          SET TO POSITIVE FILTERING                    
         OI    BYTE,X'40'          CAPITALIZE IF NECESSARY                      
         MVC   COND3+1(1),MASK                                                  
*                                                                               
         CLC   BYTE,RSTFILT3                                                    
COND3    BC    0,PACCX                                                          
*                                                                               
PACC30   CLI   QFILTER4,X'40'      ANYTHING IN FILTER4?                         
         BE    PACC40                                                           
         MVC   BYTE,QFILTER4       SAVE OFF FILTER INFO                         
         MVI   MASK,NOTEQUAL       SET TO NEGATIVE FILTERING                    
         TM    BYTE,X'40'                                                       
         BO    *+8                                                              
         MVI   MASK,EQUAL          SET TO POSITIVE FILTERING                    
         OI    BYTE,X'40'          CAPITALIZE IF NECESSARY                      
         MVC   COND4+1(1),MASK                                                  
*                                                                               
         CLC   BYTE,RSTFILT4                                                    
COND4    BC    0,PACCX                                                          
*                                                                               
PACC40   CLI   QFILTER5,X'40'      ANYTHING IN FILTER4?                         
         BE    PACC50                                                           
         MVC   BYTE,QFILTER5       SAVE OFF FILTER INFO                         
         MVI   MASK,NOTEQUAL       SET TO NEGATIVE FILTERING                    
         TM    BYTE,X'40'                                                       
         BO    *+8                                                              
         MVI   MASK,EQUAL          SET TO POSITIVE FILTERING                    
         OI    BYTE,X'40'          CAPITALIZE IF NECESSARY                      
         MVC   COND5+1(1),MASK                                                  
*                                                                               
         CLC   BYTE,RSTFILT5                                                    
COND5    BC    0,PACCX                                                          
*                                                                               
         DROP  R1                                                               
*                                                                               
         USING TIMRECD,R2                                                       
PACC50   LA    R2,SVKEY                                                         
         MVC   TIMKEY,SPACES              CLEAR KEY                             
         MVC   TIMKCPY,RCCOMPFL           COMPANY CODE                          
         MVC   TIMKUNT(2),=C'1R'          U/L                                   
         MVC   TIMKACT,ACTKACT                                                  
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         B     PACC70                                                           
         DROP  R6                                                               
*                                                                               
PACC60   GOTO1 =A(DMSEQDR),DMCB,(RC)            READ SEQ                        
PACC70   CLC   SVKEY(ACTKEND),IOKEY             SAME KEY?                       
         BNE   PACCX                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         CLC   TIMKREF,=C'*TIME*'  LOOKING FOR ONLY TIME TRANSACTIONS           
         BNE   PACC60                                                           
*                                                                               
         USING CALD,R3                                                          
         LA    R3,CALTAB                                                        
PACC80   CLI   0(R3),X'FF'                                                      
         BE    PACC60                                                           
         CLC   CALPER,TIMKACT      SAVE 1R?                                     
         BNE   PACC85                                                           
         CLC   TIMKPEDT,CALEND                                                  
         BH    PACC85                                                           
         CLC   TIMKPEDT,CALST                                                   
         BNL   *+12                                                             
PACC85   LA    R3,CALLEN(R3)                                                    
         B     PACC80                                                           
*                                                                               
         MVC   SVIOKY,IOKEY        SAVE IO KEY FOR LATER WRT(IF NEEDED)         
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)     GET LEDGER RECORD                     
         LA    R2,IO                                                            
*                                                                               
         XC    SVLMOS(4),SVLMOS    CLEAR SAVED ARE FOR LOW/HIGH MOS             
         LA    R5,TIMRFST                                                       
PACC90   CLI   0(R5),0                                                          
         BE    PACC150                                                          
         CLI   0(R5),X'8B'                                                      
         BE    PACC110                                                          
PACC100  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     PACC90                                                           
*                                                                               
         USING TIMELD,R5                                                        
PACC110  DS    0H                                                               
         NI    FLAG,X'FF'-FLGERR   TURN OFF ERROR FLAG                          
         CLI   TIMETYP,TIMEINP     INPUT DETAIL                                 
         BNE   PACC100                                                          
*        CLI   TIMLN,TIMILN1Q                                                   
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         CLC   TIMMOA,CALMNTH      ALREADY CORRECT WRONG LOGIC                  
         BE    PACC100                                                          
*                                                                               
         MVC   MSG,=CL15'TIMEREC - GET'                                         
         SR    R6,R6                                                            
         ICM   R6,3,TIMRLEN                                                     
         GOTO1 DUMP,DMCB,(R2),(R6)                                              
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,P                                                             
*                                                                               
         CLI   TIMTTYP,TIMTCB      NO BILLABLE ALLOWED                          
         BE    PACC100                                                          
         CLI   TIMLN,TIMILN2Q                                                   
         BNH   PACC120                                                          
         MVC   PERROR,=CL9'*ERROR*'                                             
         AP    PKERR,=P'1'                                                      
         OI    FLAG,FLGERR         SET ERROR TO SKIP                            
*                                                                               
PACC120  LA    R3,CALTAB                                                        
PACC125  CLI   0(R3),X'FF'                                                      
         BE    PACC100                                                          
         CLC   CALPER,TIMKACT      SAVE 1R?                                     
         BNE   PACC127                                                          
         CLC   TIMKPEDT,CALEND                                                  
         BH    PACC127                                                          
         CLC   TIMKPEDT,CALST                                                   
         BL    PACC127                                                          
         OC    CALADAT,CALADAT     CHECK ACTIVITY DATE?                         
         BZ    PACC129                                                          
         CLC   CALADAT,TIMADAT     MUST BE THE SAME                             
         BE    PACC129                                                          
PACC127  LA    R3,CALLEN(R3)                                                    
         B     PACC125                                                          
*                                                                               
PACC129  CLI   QOPT2,C'Y'          ONLY PRINT POSITIVE NUMBERS?                 
         BNE   *+18                                                             
         CP    TIMHRS,=P'0'                                                     
         BH    PACC130                                                          
         B     PACC100                                                          
*                                                                               
         CLI   QOPT2,C'N'          ONLY PRINT POSITIVE NUMBERS?                 
         BNE   *+14                                                             
         CP    TIMHRS,=P'0'                                                     
         BH    PACC100                                                          
*                                                                               
PACC130  MVC   PACCT,TIMKULA                                                    
         GOTO1 DATCON,DMCB,(1,TIMKPEDT),(X'20',PDATE)                           
         MVC   PCACCT,TIMACC       SJ OR 1N                                     
         XC    FULL,FULL                                                        
         MVC   FULL(L'TIMMOA),TIMMOA                                            
         MVI   FULL+3,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(14,PMNTH)                                  
*                                                                               
         TM    FLAG,FLGERR         DO WE WANT TO UPDATE THIS?                   
         BO    *+10                                                             
         MVC   TIMMOA,CALMNTH      UPDATE MOA IN ELEMENT                        
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(L'TIMMOA),TIMMOA                                            
         MVI   FULL+3,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(14,PNEWMNTH)                               
         EDIT  TIMHRS,(14,PHOURS),2,MINUS=YES                                   
         AP    TOTHRS,TIMHRS                                                    
*                                                                               
         USING TIMTABD,R1                                                       
         LA    R1,TIMTAB                                                        
PACC140  CLI   0(R1),EOF           END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TIMEQU,TIMTTYP      MATCH ON EQAUTE                              
         BE    *+12                                                             
         LA    R1,TIMLNQ(R1)                                                    
         B     PACC140                                                          
*                                                                               
         MVC   PTTYP(1),TIMCDE     MOVE IN CODE                                 
         EDIT  TIMLINE#,PLINE#                                                  
         TM    TIMSTAT,TIMTEMPO    IS THIS A TEMPO T/S?                         
         BNO   *+8                                                              
         MVI   PTTYP+1,C'T'                                                     
         MVC   PTYPE,TIMDESC                                                    
         MVC   POFFICE,TIMOFF                                                   
         DROP  R1                                                               
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         TM    FLAG,FLGERR         DO WE WANT THIS ELEMENT?                     
         BO    PACC100             NO - SKIP                                    
*                                                                               
         BAS   RE,ADDTBL                                                        
*                                                                               
         OC    SVLMOS,SVLMOS                                                    
         BZ    *+14                                                             
         CLC   SVLMOS,TIMMOA                                                    
         BL    *+10                                                             
         MVC   SVLMOS,TIMMOA       SAVE OFF LOW                                 
         CLC   SVHMOS,TIMMOA                                                    
         BH    *+10                                                             
         MVC   SVHMOS,TIMMOA       SAVE OFF HIGH                                
         B     PACC100             GET NEXT EL                                  
*                                                                               
PACC150  DS    0H                                                               
         OC    SVLMOS(4),SVLMOS                                                 
         BZ    PACC60              NOTHING CHANGED - DONT WRT/PUT               
         LA    R2,SVIOKY                                                        
         CLC   TIMKLMOS,SVLMOS                                                  
         BNE   *+14                                                             
         CLC   TIMKHMOS,SVHMOS                                                  
         BE    PACC60              NO NEED TO CHANGE                            
         MVC   TIMKLMOS,SVLMOS                                                  
         MVC   TIMKHMOS,SVHMOS                                                  
         MVC   MSG,=CL15'SVIOKEY - PUT'                                         
         GOTO1 DUMP,DMCB,(R2),L'SVIOKY                                          
*                                                                               
         LA    R2,IO                                                            
         MVC   TIMRLMOS,SVLMOS                                                  
         MVC   TIMRHMOS,SVHMOS                                                  
*                                                                               
         MVC   MSG,=CL15'TIMEREC - PUT'                                         
         SR    R6,R6                                                            
         ICM   R6,3,TIMRLEN                                                     
         GOTO1 DUMP,DMCB,(R2),(R6)                                              
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    PACC60                                                           
         GOTO1 =A(DMWRTDR),DMCB,(RC)     WRITE BACK KEY                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(DMPUTREC),DMCB,(RC)    PUT BACK RECORD                        
         CLI   8(R1),0                                                          
         BE    PACC60              RESET DIRECTORY READS                        
         DC    H'0'                DIE WRITING A RECORD                         
*                                                                               
PACCX    B     EXIT                                                             
         EJECT                                                                  
         DROP  R4                                                               
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING TSHD,R3                                                          
REQL     DS    0H                                                               
         LA    R3,TSHTBL                                                        
         SR    R0,R0                                                            
         ICM   R0,3,NTSH                                                        
         BZ    REQLX                                                            
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,P                                                             
*                                                                               
         MVC   PACCT,=C'TOTAL HOURS   '                                         
         EDIT  TOTHRS,(14,PHOURS),2,MINUS=YES                                   
         GOTO1 ACREPORT                                                         
         MVC   PACCT,=C'TOTAL ACCOUNTS'                                         
         EDIT  TOTACC,(14,PHOURS)                                               
         GOTO1 ACREPORT                                                         
         MVI   FORCEHED,C'Y'                                                    
         LA    RF,TSHLEN                                                        
         GOTO1 XSORT,DMCB,(0,(R3)),(R0),(RF),(RF),0                             
*                                                                               
REQL10   MVC   PACCT,TSHACC+1                                                   
         GOTO1 DATCON,DMCB,(1,TSHDTE),(X'20',PDATE)                             
         GOTO1 ACREPORT                                                         
         LA    R3,TSHLEN(R3)                                                    
         BCT   R0,REQL10                                                        
*                                                                               
         MVC   PACCT(16),=C'TOTAL TIMESHEETS'                                   
         EDIT  (B2,NTSH),(14,PHOURS)                                            
         GOTO1 ACREPORT                                                         
         MVC   PACCT(24),=C'TOTAL TIMELINES IN ERROR'                           
         EDIT  PKERR,(14,PHOURS)                                                
         GOTO1 ACREPORT                                                         
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
         DROP  R3,R4                                                            
**********************************************************************          
* ADD ENTRY TO TABLE                                                 *          
*     R2 = A(TIME RECORD)                                            *          
**********************************************************************          
         SPACE 1                                                                
         USING TIMRECD,R2                                                       
ADDTBL   NTR1                                                                   
         CLC   TIMKCULA,LASTACC                                                 
         BE    *+16                                                             
         MVC   LASTACC,TIMKCULA                                                 
         AP    TOTACC,=P'1'                                                     
*                                                                               
         USING TSHD,R3                                                          
         LA    R3,TSHTBL                                                        
         SR    R0,R0                                                            
         ICM   R0,3,NTSH                                                        
         BZ    ADDT20                                                           
ADDT10   CLC   TSHACC,LASTACC                                                   
         BNE   *+14                                                             
         CLC   TSHDTE,TIMKPEDT                                                  
         BE    ADDTX                                                            
         LA    R3,TSHLEN(R3)                                                    
         BCT   R0,ADDT10                                                        
*                                                                               
ADDT20   MVC   TSHACC,LASTACC                                                   
         MVC   TSHDTE,TIMKPEDT                                                  
         SR    R0,R0                                                            
         ICM   R0,3,NTSH                                                        
         AH    R0,=H'1'                                                         
         STCM  R0,3,NTSH                                                        
         CLC   NTSH,=Y(MXTSH)                                                   
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADDTX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NTR1                                                                   
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         CP    DUMPCNT,MAXDUMP                                                  
         BH    DUMPX                                                            
         AP    DUMPCNT,=P'1'                                                    
*                                                                               
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,0(R1)                                                         
         L     R4,4(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
RELO     DC    F'0'                                                             
HELLO    DC    V(HELLO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
LASTACC  DC    CL15' '                                                          
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
MAXDUMP  DC    PL4'0001500'                                                     
*                                                                               
EOF      EQU   X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
TIMTAB   DS    0C                                                               
         DC    AL1(TIMTCB),C'B',CL15'BILLABLE'                                  
         DC    AL1(TIMTCR),C'R',CL15'REALIZATION'                               
         DC    AL1(TIMTCN),C'N',CL15'NON-BILLABLE'                              
         DC    AL1(TIMTNC),C'N',CL15'NON-CLIENT'                                
         DC    AL1(EOF)                                                         
*                                                                               
MXTSH    EQU   10000                                                            
CALTAB   DC    C'SCME008957S ',X'B41109',X'B41109',X'000000',X'B411'            
         DC    C'SCME008957S ',X'B41116',X'B41116',X'000000',X'B411'            
         DC    C'SCME008957S ',X'B41123',X'B41123',X'000000',X'B411'            
         DC    C'SCME008957S ',X'B41130',X'B41130',X'000000',X'B411'            
         DC    C'SCME008957S ',X'B41207',X'B41207',X'000000',X'B412'            
         DC    C'SCME008957S ',X'B41214',X'B41214',X'000000',X'B412'            
         DC    C'SCME008957S ',X'B41221',X'B41221',X'000000',X'B412'            
         DC    C'SCME009151  ',X'B41109',X'B41109',X'000000',X'B411'            
         DC    C'SCME009151  ',X'B41116',X'B41116',X'000000',X'B411'            
         DC    C'SCME009151  ',X'B41123',X'B41123',X'000000',X'B411'            
         DC    C'SCME009151  ',X'B41130',X'B41130',X'000000',X'B411'            
         DC    C'SCME009151  ',X'B41207',X'B41207',X'000000',X'B412'            
         DC    C'SCME009151  ',X'B41214',X'B41214',X'000000',X'B412'            
         DC    C'SCME009151  ',X'B41231',X'B41231',X'000000',X'B412'            
         DC    C'SCME009290  ',X'B41005',X'B41005',X'000000',X'B410'            
         DC    C'SCME009290  ',X'B41012',X'B41012',X'000000',X'B410'            
         DC    C'SCME009290  ',X'B41019',X'B41019',X'000000',X'B410'            
         DC    C'SCME009290  ',X'B41026',X'B41026',X'000000',X'B410'            
         DC    C'SCME009290  ',X'B41031',X'B41031',X'000000',X'B410'            
         DC    C'SCME009290  ',X'B41109',X'B41109',X'000000',X'B411'            
         DC    C'SCME009290  ',X'B41116',X'B41116',X'000000',X'B411'            
         DC    C'SCME009290  ',X'B41123',X'B41123',X'000000',X'B411'            
         DC    C'SCME009290  ',X'B41130',X'B41130',X'000000',X'B411'            
         DC    C'SCME009290  ',X'B41207',X'B41207',X'000000',X'B412'            
         DC    C'SCME009290  ',X'B41221',X'B41221',X'000000',X'B412'            
         DC    C'SCME009334  ',X'B41026',X'B41026',X'000000',X'B410'            
         DC    C'SCME009334  ',X'B41031',X'B41031',X'000000',X'B410'            
         DC    C'SCME009334  ',X'B41109',X'B41109',X'000000',X'B411'            
         DC    C'SCME009334  ',X'B41116',X'B41116',X'000000',X'B411'            
         DC    C'SCME009334  ',X'B41123',X'B41123',X'000000',X'B411'            
         DC    C'SCME009334  ',X'B41130',X'B41130',X'000000',X'B411'            
         DC    C'SCME009334  ',X'B41207',X'B41207',X'000000',X'B412'            
         DC    C'TYME009285  ',X'B40331',X'B40331',X'000000',X'B403'            
         DC    C'TYME009285  ',X'B40430',X'B40430',X'000000',X'B404'            
         DC    C'TYME009285  ',X'B40531',X'B40531',X'000000',X'B405'            
         DC    C'TYME009285  ',X'B40630',X'B40630',X'000000',X'B406'            
         DC    C'TYME009285  ',X'B40731',X'B40731',X'000000',X'B407'            
         DC    C'TYME009285  ',X'B40831',X'B40831',X'000000',X'B408'            
         DC    C'TYME009285  ',X'B40930',X'B40930',X'000000',X'B409'            
         DC    C'TYME009285  ',X'B41031',X'B41031',X'000000',X'B410'            
         DC    C'TYME009285  ',X'B41130',X'B41130',X'000000',X'B411'            
         DC    C'TYME009285  ',X'B41231',X'B41231',X'000000',X'B412'            
         DC    C'TYME009320  ',X'B40131',X'B40131',X'000000',X'B401'            
         DC    C'TYME009320  ',X'B40228',X'B40228',X'000000',X'B402'            
         DC    C'TYME009320  ',X'B40331',X'B40331',X'000000',X'B403'            
         DC    C'TYME009320  ',X'B40430',X'B40430',X'000000',X'B404'            
         DC    C'TYME009320  ',X'B40531',X'B40531',X'000000',X'B405'            
         DC    C'TYME009320  ',X'B40630',X'B40630',X'000000',X'B406'            
         DC    C'TYME009320  ',X'B40731',X'B40731',X'000000',X'B407'            
         DC    C'TYME009320  ',X'B40831',X'B40831',X'000000',X'B408'            
         DC    C'TYME009320  ',X'B40907',X'B40907',X'000000',X'B409'            
         DC    C'TYME009320  ',X'B40930',X'B40930',X'000000',X'B409'            
         DC    C'TYME009320  ',X'B41031',X'B41031',X'000000',X'B410'            
         DC    C'TYME009320  ',X'B41130',X'B41130',X'000000',X'B411'            
         DC    C'TYME009320  ',X'B41231',X'B41231',X'000000',X'B412'            
         DC    C'OGDC009487  ',X'B41231',X'B41231',X'000000',X'B412'            
         DC    C'OGDC009487  ',X'B41221',X'B41221',X'000000',X'B412'            
         DC    C'OGDC009487  ',X'B41214',X'B41214',X'000000',X'B412'            
         DC    C'OGDC009487  ',X'B41207',X'B41207',X'000000',X'B412'            
         DC    X'FF'                                                            
TSHTBL   DS    (MXTSH*TSHLEN)XL1                                                
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
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMADD'),=C'ACCDIR',SVIOKY,SVIOKY          
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
         MVC   SVDA,ACCKDA                                                      
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,IO,DMWORK                    
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',SVDA,IO,DMWORK              
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PROGRAMS WORKING STORAGE                                           *          
**********************************************************************          
         SPACE 1                                                                
ACZQD    DSECT                                                                  
ELCODE   DS    CL1                                                              
SVKEY    DS    CL49                                                             
SVIOKY   DS    CL54                                                             
*                                                                               
SVLMOS   DS    PL2                                                              
SVHMOS   DS    PL2                                                              
*                                                                               
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
*                                                                               
MASK     DS    XL1                                                              
EQUAL    EQU   X'80'               BRANCH EQUAL                                 
NOTEQUAL EQU   X'70'               BRANCH NOT EQUAL                             
*                                                                               
FLAG     DS    CL1                 ERROR FLAG                                   
FLGERR   EQU   X'80'               ERROR IN TRANSACTION - SKIP                  
*                                                                               
PKERR    DS    PL8                 ERROR COUNTER                                
MSG      DS    CL15                                                             
*                                                                               
TOTHRS   DS    PL6                                                              
TOTACC   DS    PL6                                                              
NTSH     DS    H                                                                
SVDA     DS    F                                                                
DMWRK    DS    12D                                                              
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
**********************************************************************          
* CALENDAR TABLE DSECT                                               *          
**********************************************************************          
         SPACE 1                                                                
CALD     DSECT                                                                  
CALPER   DS    CL12                                                             
CALST    DS    XL3                                                              
CALEND   DS    XL3                                                              
CALADAT  DS    XL3                                                              
CALMNTH  DS    XL2                                                              
CALLEN   EQU   *-CALD                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT LINE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
PTTYP    DS    CL2                 TIME TYPE                                    
         DS    CL4                                                              
PLINE#   DS    CL5                                                              
         DS    CL2                                                              
PACCT    DS    CL14                                                             
         DS    CL2                                                              
PCACCT   DS    CL14                                                             
         DS    CL2                                                              
PDATE    DS    CL8                                                              
         DS    CL2                                                              
PHOURS   DS    CL14                                                             
         DS    CL2                                                              
PMNTH    DS    CL6                                                              
         DS    CL1                                                              
PTYPE    DS    CL15                                                             
         DS    CL2                                                              
POFFICE  DS    CL2                                                              
         DS    CL2                                                              
PNEWMNTH DS    CL6                                                              
         DS    CL3                                                              
PERROR   DS    CL7                                                              
*                                                                               
TSHD     DSECT                                                                  
TSHACC   DS    XL15                ACCOUNT                                      
TSHDTE   DS    XL3                 DATE                                         
TSHLEN   EQU   *-TSHD                                                           
         EJECT                                                                  
         EJECT                                                                  
**********************************************************************          
* TIME TABLE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
TIMTABD  DSECT                                                                  
TIMEQU   DS    CL1                                                              
TIMCDE   DS    CL1                                                              
TIMDESC  DS    CL15                                                             
TIMLNQ   EQU   *-TIMTABD                                                        
         EJECT                                                                  
**********************************************************************          
* ++INCLUDE                                                          *          
**********************************************************************          
         SPACE 1                                                                
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACGENPOST                                                              
*        ACMASTD                                                                
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066ACREPZQ02 03/05/15'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
