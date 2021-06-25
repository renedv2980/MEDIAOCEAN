*          DATA SET DDSHFICHK  AT LEVEL 006 AS OF 11/05/19                      
*PHASE SHFICHKA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE SCANNER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE TREE                                                                   
*                                                                               
         TITLE 'SHFICHK - SHARED MEMORY FILE INDEX CHECK'                       
         PRINT NOGEN                                                            
SHFICHK  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         NBASE WORKX-WORKD,**SFIC**,=A(WORKAREA),CLEAR=YES                      
         USING WORKD,RC                                                         
         LARL  RA,COMMON                                                        
         USING COMMON,RA                                                        
         ST    RD,SAVERD                                                        
         USING PLINED,PLINE                                                     
*                                                                               
         BRAS  RE,INIT             READ CARDS AND WHATNOT                       
*                                                                               
         CLI   MODE,C'C'                                                        
         BNE   SFICHK0                                                          
         BRAS  RE,REPREP           DETAILS OF REPORT PARTS                      
         BRAS  RE,LISTIT           LIST ENTRIES IN KEYED BINARY TABLE           
         BRAS  RE,AEQUEUE          DETAILS FOR AVAILABLE ENTRY QUEUES           
         B     SFICHKX                                                          
*                                                                               
SFICHK0  CLI   MODE,C'R'                                                        
         BNE   SFICHK1                                                          
         BRAS  RE,REPREP           DETAILS OF REPORT PARTS                      
*                                                                               
SFICHK1  CLI   MODE,C'T'                                                        
         BNE   SFICHK2                                                          
         BRAS  RE,LISTIT           LIST ENTRIES IN KEYED BINARY TABLE           
*                                                                               
SFICHK2  CLI   MODE,C'Q'                                                        
         BNE   SFICHK3                                                          
         BRAS  RE,AEQUEUE          DETAILS FOR AVAILABLE ENTRY QUEUES           
*                                                                               
SFICHK3  CLI   MODE,C'P'                                                        
         BNE   SFICHKX                                                          
         BRAS  RE,POSTIT           POST ALERT TO MONITORING JOB                 
*                                                                               
SFICHKX  BRAS  RE,DONE                                                          
*                                                                               
         B     XBASE                                                            
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INIT     NTR1  ,                                                                
*                                                                               
         BRAS  RE,PRINTI           INIT PRINTING                                
*                                                                               
         L     RF,=A(SSB)          OPEN SERVICE FILES FOR READ-ONLY             
         OI    SSOMTIND-SSOOFF(RF),SSOWSRN                                      
*                                                                               
         L     R1,=A(IOAREA-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,AIOAREA                                                       
*                                                                               
         L     R1,=A(CIREC-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ACIREC                                                        
*                                                                               
         LA    R3,CARD                                                          
*                                                                               
INIT02   GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT04                                                           
*                                                                               
         MVC   PLINE+1(80),0(R3)                                                
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BRAS  RE,VALCARD          READ KEYWORD=VALUES                          
         BNE   XBASE               NEQ MEANS INVALID KEYWORD                    
*                                                                               
         L     RF,=V(DDSIO)        DDSIO VERSION SPECIFICATION                  
         MVC   0(8,RF),DDSIO                                                    
         B     INIT02                                                           
*                                                                               
INIT04   XC    FIW,FIW             INIT SHARED MEMORY WORK AREA                 
*                                                                               
         CLC   MEMORY,SPACES       BE SURE MEMORY CARD IS THERE                 
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'SHMUSS',ATTACH,MEMORY,0,0                       
         ICM   R1,15,DMCB+12       A(SHARED TABLE)                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R1,FIWSHA                                                        
*                                                                               
         ICM   R1,15,DMCB+16       MAX TABLE SIZE                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R1,SFISIZE                                                       
*                                                                               
         L     R1,=V(DMISGENQ)                                                  
         ST    R1,FIWENQ                                                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLIST,AIOAREA                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,TODAYO)                                    
         GOTO1 VDATCON,DMCB,(5,0),(30,TODAY)                                    
*                                                                               
         B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CLOSE UP                                                                      
***********************************************************************         
DONE     NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,DMCLSE,CONTROL,FLIST,AIOAREA                       
         BRAS  RE,PRINTX           CLOSE PRINT                                  
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* PARAMETER HANDLING ROUTINE                                                    
***********************************************************************         
VALCARD  NTR1  ,                                                                
*                                                                               
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC500                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         CLC   0(2,RF),=C'  '      EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC500                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BRAS  RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC500                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BRAS  RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC500                                                          
*                                                                               
VALC080  TM    9(R4),X'01'         DATE INPUT                                   
         BZ    VALC400                                                          
         LA    R0,1(R1)            SET R0 INPUT LEN                             
         ST    RF,FULL                                                          
         GOTO1 =V(PERVAL),DMCB,((R0),(R2)),(X'60',WORK)                         
         L     RF,FULL                                                          
         CLI   4(R1),X'04'                                                      
         BNE   CERRDAT                                                          
         MVC   0(2,RF),WORK+PVALNSTA-PERVALD NEW CMPRSD DATE                    
         B     VALC500                                                          
*                                                                               
VALC400  CLI   8(R4),0             DONT CARE                                    
         BE    VALC410                                                          
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         SR    RE,RE                                                            
         IC    RE,8(R4)            PAD OUT TO SPACES                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SPACES                                                   
VALC410  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC500  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC500                                                          
         B     EXITOK                                                           
*                                                                               
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDAT  LA    R1,=C'INVALID DATE    '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BRAS  RE,PRINTL                                                        
         B     EXITL                                                            
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU                             
*----------------------------------------------------------------------         
VALTIME  NTR1  ,                                                                
*                                                                               
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         BNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         B     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALF                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BRAS  RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BRAS  RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BRAS  RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BRAS  RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITOK                                                           
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BRAS  RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITOK                                                           
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BRAS  RE,VALTADD                                                       
         B     EXITOK                                                           
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
*                                                                               
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* REPORT AND DETAIL FILE REPORTS                                                
***********************************************************************         
REPREP   NTR1  ,                                                                
         MVI   LPNO,C'Y'                                                        
         ZAP   TEMPC,=P'0'                                                      
*                                                                               
         SAM31                                                                  
*                                                                               
         L     R8,FIWSHA           A(SHARED MEMORY FILE QUEUE HEADER)           
         USING SIHDRD,R8                                                        
         LA    R9,L'SIHDR(,R8)      A(FIRST RESOURCE TABLE HEADER)              
         USING SITABD,R9                                                        
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'='                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
         MVC   PLINE(40),=CL40'- CHECKING SHARED MEMORY FILE INDEXES'           
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'='                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         CLC   SIHEYE,EYESFI       EYECATCHER "**SHFI****SHFI**"                
         BE    CH012                                                            
         MVC   PLINE(40),=CL40'**ERROR** EYECATCHER MISMATCH'                   
         BRAS  RE,PRINTL                                                        
*                                                                               
CH012    CLC   SIHSIZE,SFISIZE                                                  
         BE    CH014                                                            
         MVC   PLINE(40),=CL40'**ERROR** SHARED MEMORY SIZE MISMATCH'           
         BRAS  RE,PRINTL                                                        
*                                                                               
CH014    L     R5,SIHNOFR          PICK UP NUMBER OF RESOURCES                  
         CHI   R5,255                                                           
         BNH   CH016                                                            
         MVC   PLINE(40),=CL40'**ERROR** BAD NUMBER OF RESOURCES'               
         BRAS  RE,PRINTL                                                        
                                                                                
CH016    XC    CNTTAB,CNTTAB                                                    
*                                                                               
         LA    R4,FILES            FILES DEFINED IN PARAMETERS                  
CH050    CLI   0(R4),C' '          END OF LIST                                  
         BE    CHX                                                              
         MVC   FIWRES,0(R4)                                                     
*                                                                               
         BRAS  RE,FIRSET                                                        
         BE    CH090                                                            
         MVC   PLINE(40),=CL40'**ERROR** UNSUPPORTED RESOURCE'                  
         BRAS  RE,PRINTL                                                        
         B     XBASE                                                            
*                                                                               
CH090    L     R9,FIWRHA                                                        
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BRAS  RE,PRINTL                                                        
         MVC   PLINE(20),=CL20'- CHECKING RESOURCE'                             
         MVC   PLINE+21(5),=C'FILE='                                            
         MVC   PLINE+26(L'FIWRES),FIWRES                                        
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BRAS  RE,PRINTL                                                        
*                                                                               
         BRAS  RE,SETFI            SET WORKER FILE INFORMATION                  
         BRAS  RE,CHKPARTS         CHECK RESOURCE PARTS                         
*                                                                               
         L     R0,CNTTAB                                                        
         AHI   R0,1                                                             
         ST    R0,CNTTAB                                                        
*                                                                               
         LA    R4,L'FILES(,R4)                                                  
         BCT   R5,CH050                                                         
*                                                                               
CHX      BRAS  RE,PRINTTOT                                                      
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET FILE INFORMATION                                                          
***********************************************************************         
SETFI    NTR1  ,                                                                
*                                                                               
         SAM24                                                                  
         XC    RINFO(RINFOL),RINFO                                              
         XC    KEY,KEY             INIT FILE INFORMATION                        
*                                                                               
         LA    RF,FIWCIA                                                        
         GOTO1 =V(DATAMGR),DMCB,BUFFER,FIWRES,KEY,(RF),ACIREC                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SAM31                                                                  
*                                                                               
         CLC   =C'PRTQ',FIWRES                                                  
         BE    SETFIPQ                                                          
         CLC   =C'WRKF',FIWRES                                                  
         BE    SETFIWF                                                          
         CLC   =C'WRKZ',FIWRES                                                  
         BE    SETFIWZ                                                          
         DC    H'0'                                                             
*                                                                               
SETFIPQ  MVI   RILKEY,L'PQKEY                                                   
         MVI   RIDSTAT,PQSTAT-PQINDEX                                           
         B     EXITOK                                                           
*                                                                               
SETFIWF  MVI   RILKEY,L'W_KEY+L'W_FILENO                                        
         MVI   RIDSTAT,W_STAT-W_INDEX                                           
         B     EXITOK                                                           
*                                                                               
SETFIWZ  MVI   RILKEY,L'WZW_KEY+L'WZW_FILENO                                    
         MVI   RIDSTAT,WZW_STAT-WZW_INDEX                                       
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK PARTS                                                                   
***********************************************************************         
CHKPARTS NTR1  ,                                                                
*                                                                               
         BRAS  RE,PRINTTI                                                       
*                                                                               
         XC    CNTP1AV,CNTP1AV                                                  
*                                                                               
         L     R2,FIWP1A           POINT TO PART1S                              
         L     R4,SIT1CIC          TOTAL NUMBER OF PART1S                       
         LHI   R5,1                                                             
*                                                                               
         USING SI1PARD,R2                                                       
CP010    ST    R2,ACURPAR1         CURRENT PART1                                
         C     R5,SI1NUM                                                        
         BE    CP012                                                            
         MVC   PLINE(40),=C'**ERROR** PART1 SEQUENCE MISMATCH'                  
         BRAS  RE,PRINTL                                                        
         B     XBASE                                                            
*                                                                               
CP012    MVC   BIGWORK(L'SI1NDX),SI1NDX                                         
*                                                                               
         LA    R3,BIGWORK                                                       
         LLC   R1,RIDSTAT          DISPLACEMENT TO THE STATUS BYTE              
         LA    R1,0(R1,R3)                                                      
         CLI   0(R1),0             SKIP PURGED/EMPTY REPORTS                    
         BNE   CP030                                                            
*                                                                               
         CLC   SI1PTP,ZEROS                                                     
         BNE   CP024                                                            
         CLC   SI1LTP,ZEROS                                                     
         BNE   CP024                                                            
         CLC   SI1RTP,ZEROS                                                     
         BE    CP025                                                            
*                                                                               
CP024    MVC   PLINE(40),=CL40'*ERROR* PURGED REPORT CONTAINS LINK'             
         BRAS  RE,PRINTL                                                        
         B     CP034                                                            
*                                                                               
CP025    L     R0,CNTP1AV          COUNT TOTAL AVAILABLE PART2S                 
         AHI   R0,1                                                             
         ST    R0,CNTP1AV                                                       
         B     CP060                                                            
*                                                                               
CP030    MVC   FIWNDA,ACURPAR1                                                  
         BRAS  RE,FIRNC                                                         
         MVI   BYTE,1                                                           
         CLI   SHORTR,C'Y'                                                      
         BNE   CP034                                                            
         AP    TEMPC,=P'1'                                                      
         CP    TEMPC,=P'100'                                                    
         BH    CP056                                                            
         BL    CP034                                                            
         MVC   PLINE(3),=C'...'                                                 
         BRAS  RE,PRINTL                                                        
         B     CP056                                                            
CP034    BRAS  RE,PRINTNDX         PRINT THE INDEX                              
*                                                                               
         CLI   READCI,C'N'                                                      
         BE    CP044                                                            
         MVC   KEY,SI1NDX                                                       
         SAM24                                                                  
         LA    RF,FIWCIA                                                        
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMREAD),FIWRES,(RF),ACIREC               
         SAM31                                                                  
         CLI   8(R1),0                                                          
         BE    CP038                                                            
         MVC   PLINE(40),=CL40'*ERROR* ERROR READING PART1 CI'                  
         B     CP039                                                            
CP038    L     R1,ACIREC                                                        
         CLC   SI1NDX(L'PQKEY),0(R1)                                            
         BE    CP038A                                                           
         MVC   PLINE(40),=CL40'*ERROR* MEMORY AND CI INDEX MISMATCH'            
         B     CP039                                                            
CP038A   CLC   SI1NDX(L'PQINDEX),0(R1)                                          
*        BE    CP042                                                            
*        MVC   PLINE(40),=CL40'*WARNING* MEMORY AND CI INDEX MISMATCH'          
         B     CP042                                                            
CP039    MVC   BIGWORK(L'SI1NDX),0(R1)                                          
         BRAS  RE,PRINTL                                                        
         BRAS  RE,PRINTNDX         PRINT THE INDEX                              
         MVC   BIGWORK(L'SI1NDX),SI1NDX                                         
*                                                                               
CP042    CLC   PQLINES-PQRECD(L'PQLINES,R1),ZEROS                               
         BNE   CP044                                                            
         CLI   SI1NDX+PQSTAT-PQRECD,PQSTTE                                      
         BE    CP044                                                            
         MVC   PLINE(40),=CL40'*ERROR* REPORT CONTAINS NO LINES'                
         BRAS  RE,PRINTL                                                        
         BRAS  RE,PRINTNDX         PRINT THE INDEX                              
*                                                                               
CP044    L     R0,FIWSHA                                                        
         AL    R0,SI1PTP                                                        
         C     R0,FIWNDA                                                        
         BE    CP046                                                            
         L     R0,FIWSHA                                                        
         AL    R0,SI1LTP                                                        
         C     R0,FIWNDA                                                        
         BE    CP046                                                            
         L     R0,FIWSHA                                                        
         AL    R0,SI1RTP                                                        
         C     R0,FIWNDA                                                        
         BNE   CP056                                                            
*                                                                               
CP046    MVC   PLINE(40),=CL40'*ERROR* REPORT CONTAINS BAD TREE LINK'           
         BRAS  RE,PRINTL                                                        
*                                                                               
CP056    CLI   LOCKIT,C'N'                                                      
         BE    *+8                                                              
         BRAS  RE,FIRRLOCK                                                      
*                                                                               
         CLC   SI1NXT,ZEROS                                                     
         BE    CP058                                                            
         ZAP   CNTPART,=P'1'                                                    
         MVC   SAVEKEY,SI1NDX                                                   
         BRAS  RE,GETPARTS                                                      
*                                                                               
CP058    CLI   LOCKIT,C'N'                                                      
         BE    *+8                                                              
         BRAS  RE,FIRRUNLK                                                      
*                                                                               
CP060    AHI   R2,L'SI1PAR         BUMP UP                                      
         AHI   R5,1                                                             
         BCT   R4,CP010                                                         
*                                                                               
         MVC   TITLE,SPACES                                                     
         MVC   TITLE2,SPACES                                                    
*                                                                               
         CLC   CNTP1AV,SITP1AV     DOES THE COUNT MATCH MEMORY?                 
         BE    CPX                                                              
         MVC   PLINE(36),=CL36'*ERROR* PART1S AVAILABLE MISMATCH - '            
*                                                                               
         MVC   PLINE+36(6),=C'COUNT='                                           
         MVC   FULL,CNTP1AV                                                     
         EDIT  FULL,(6,PLINE+42),FLOAT=-,COMMAS=YES,ALIGN=LEFT,        +        
               ZERO=NOBLANK                                                     
*                                                                               
         MVC   PLINE+50(7),=C'MEMORY='                                          
         MVC   FULL,SITP1AV                                                     
         EDIT  FULL,(6,PLINE+57),FLOAT=-,COMMAS=YES,ALIGN=LEFT,        +        
               ZERO=NOBLANK                                                     
*                                                                               
         BRAS  RE,PRINTL                                                        
*                                                                               
CPX      B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK PARTS                                                                   
***********************************************************************         
GETPARTS NTR1  ,                                                                
*                                                                               
         L     R2,ACURPAR1         CURRENT PART1                                
         USING SI1PARD,R2                                                       
*                                                                               
         L     R3,SI1NXT                                                        
GP000    A     R3,FIWSHA                                                        
         ST    R3,ACURPAR2                                                      
         USING SI2PARD,R3                                                       
*                                                                               
         L     R1,SI2NUM                                                        
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(5,PLSLOT)                                             
*                                                                               
         ICM   R1,15,SI2NXT                                                     
         BZ    GP005                                                            
         A     R1,FIWSHA                                                        
         L     R1,SI2NUM-SI2PAR(R1)                                             
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(5,PLNSLOT)                                            
*                                                                               
GP005    L     R1,SI2PRV                                                        
         A     R1,FIWSHA                                                        
         C     R1,FIWP2A           IS IT A PART2                                
         BNL   GP010               . YES                                        
         L     R1,SI1NUM-SI1PAR(R1)                                             
         B     GP012                                                            
GP010    L     R1,SI2NUM-SI2PAR(R1)                                             
GP012    ST    R1,FULL                                                          
         EDIT  (B4,FULL),(5,PLPSLOT)                                            
*                                                                               
GP020    MVC   FIWNDA,ACURPAR2                                                  
         BRAS  RE,FIRNC                                                         
         MVC   BIGWORK(L'SI2NDX),SI2NDX                                         
         MVI   BYTE,2                                                           
         CLI   SHORTR,C'Y'                                                      
         BNE   GP054                                                            
         CP    TEMPC,=P'100'                                                    
         BH    GP056                                                            
         BL    GP054                                                            
         MVC   PLINE(3),=C'...'                                                 
         BRAS  RE,PRINTL                                                        
         B     GP056                                                            
GP054    BRAS  RE,PRINTNDX                                                      
*                                                                               
GP056    LLC   R1,RILKEY         LENGTH OF KEY                                  
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAVEKEY(0),SI2NDX MAKE SURE THE KEY IS CONSISTENT                
         BNE   GP058                                                            
         CP    CNTPART,=PL2'512' MAKE SURE THERE AREN'T TOO MANY PARTS          
         BNH   GP060                                                            
GP058    MVC   PLINE(40),=CL40'  **ERROR** *** KEY MISMATCH ***'                
         BRAS  RE,PRINTL                                                        
         B     GPX                                                              
*                                                                               
GP060    CLC   SI2NXT,ZEROS                                                     
         BE    GPX                                                              
         MVC   SAVEKEY,SI2NDX                                                   
         AP    CNTPART,=P'1'                                                    
         L     R3,SI2NXT                                                        
         B     GP000                                                            
*                                                                               
GPX      B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* POST ECB TO MONITORING JOB                                                    
***********************************************************************         
POSTIT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'='                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
         MVC   PLINE(40),=CL40'- POSTING ECB TO MONITORING JOB '                
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'='                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         SAM31                                                                  
         L     R8,FIWSHA                                                        
         LH    R2,SIHMASID                                                      
*                                                                               
         LOCASCB STOKEN=SIHMSTOK                                                
         LTR   RF,RF                                                            
         BZ    PI020                                                            
*                                                                               
         MVC   PLINE(40),=CL40'- UNABLE TO LOCATE STOKEN '                      
         BRAS  RE,PRINTL                                                        
         B     POSTITX                                                          
*                                                                               
PI020    LR    R4,R1                                                            
         USING ASCB,R4                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         BE    PI040                                                            
*                                                                               
         MVC   PLINE(40),=CL40'- UNABLE TO FIND ASCB '                          
         BRAS  RE,PRINTL                                                        
         B     POSTITX                                                          
*                                                                               
PI040    L     R3,SIHMECB                                                       
         POST  (R3),99,ASCB=(R4),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTPA)          
         LTR   RF,RF                                                            
         BZ    POSTITX                                                          
*                                                                               
         MVC   PLINE(40),=CL40'- POST FAILED '                                  
         BRAS  RE,PRINTL                                                        
         DROP  R4                                                               
*                                                                               
POSTITX  B     EXITOK                                                           
*                                                                               
POSTPA   POST  ECBKEY=YES,MF=L                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* LIST ENTRIES IN KEYED BINARY TABLE                                            
***********************************************************************         
LISTIT   NTR1  BASE=*,LABEL=*                                                   
         MVI   LPNO,C'N'                                                        
         ZAP   TEMPC,=P'0'                                                      
*                                                                               
         SAM31                                                                  
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'='                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
         MVC   PLINE(40),=CL40'- LISTING RESOURCE TREE'                         
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'='                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
*                                                                               
         LA    R4,FILES            FILES DEFINED IN PARAMETERS                  
LIST010  CLI   0(R4),C' '          END OF LIST                                  
         BE    LISTITX                                                          
*                                                                               
         MVC   FIWRES,0(R4)                                                     
         XC    CNTTAB,CNTTAB                                                    
         XC    CNTDEEP,CNTDEEP                                                  
         XC    CNTDEEPS,CNTDEEPS                                                
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'-'                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
         MVC   PLINE(L'FIWRES),FIWRES                                           
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'-'                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
*                                                                               
         BRAS  RE,FIRSET                                                        
         BE    LIST020                                                          
*                                                                               
         MVC   PLINE(40),=C'**ERROR** UNABLE TO SET SHARED ADDRESSES'           
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
*                                                                               
LIST020  XC    FIWNDX,FIWNDX                                                    
         L     R8,FIWSHA                                                        
         L     R9,FIWRHA                                                        
*                                                                               
         XC    TREEBLK,TREEBLK                                                  
         LA    R5,TREEBLK                                                       
         USING TREED,R5                                                         
         MVC   TRENAME(L'FIWRES),FIWRES                                         
         MVC   TREMAX,SIT1CIC-SITABD(R9)                                        
         MVC   TRABASE,FIWSHA                                                   
         MVC   TRAHEAD,FIWRHA                                                   
         MVC   TRDROOT,=AL2(SITTREE-SITABD)                                     
         MVC   TRDKEY,=AL2(SI1NDX-SI1PARD)    DISPL IN NODE TO KEY              
         MVC   TREKEYL+1(1),SITXKL-SITABD(R9)                                   
         MVC   TRDPAREN,=AL2(SI1PTP-SI1PARD)                                    
         MVC   TRDLEFT,=AL2(SI1LTP-SI1PARD)                                     
         MVC   TRDRIGHT,=AL2(SI1RTP-SI1PARD)                                    
*                                                                               
LIST030  GOTO1 VTREE,DMCB,NEXT,TREEBLK                                          
         BNE   LIST040                                                          
*                                                                               
         L     R1,TREDEEP                                                       
         ST    R1,CNTDEPTH                                                      
*                                                                               
         C     R1,CNTDEEP                                                       
         BNH   *+8                                                              
         ST    R1,CNTDEEP                                                       
*                                                                               
         A     R1,CNTDEEPS                                                      
         ST    R1,CNTDEEPS                                                      
*                                                                               
         L     R3,TRANODE          SET RETURN NODE                              
         ST    R3,FIWNDA                                                        
         USING SI1PARD,R3                                                       
         MVC   BIGWORK(L'SI1NDX),SI1NDX                                         
*                                                                               
         BRAS  RE,FIRNC                                                         
         MVI   BYTE,1                                                           
         CLI   SHORTR,C'Y'                                                      
         BNE   LIST036                                                          
         AP    TEMPC,=P'1'                                                      
         CP    TEMPC,=P'100'                                                    
         BH    LIST038                                                          
         BL    LIST036                                                          
         MVC   PLINE(3),=C'...'                                                 
         BRAS  RE,PRINTL                                                        
         B     LIST038                                                          
LIST036  BRAS  RE,PNDXLIST                                                      
LIST038  MVC   FIWNDX,BIGWORK      SEARCH FOR NEXT                              
*                                                                               
         TM    TRERF,TRFLOOP       DID WE EXIT BECAUSE OF A LOOP?               
         BO    LIST040                                                          
*                                                                               
         L     R1,CNTTAB                                                        
         AHI   R1,1                                                             
         ST    R1,CNTTAB           COUNT NUMBER OF INDEXES                      
*                                                                               
         B     LIST030                                                          
         DROP  R3                                                               
*                                                                               
LIST040  TM    TRERF,TRFLOOP       DID WE EXIT BECAUSE OF A LOOP?               
         BZ    LIST045             NO                                           
         BRAS  RE,PRINTL                                                        
         MVC   PLINE(33),=CL33'*ERROR* CORRUPT TREE POINTER'                    
         BRAS  RE,PRINTL                                                        
         B     LIST050                                                          
*                                                                               
LIST045  BRAS  RE,PRINTL                                                        
         MVC   PLINE(33),=CL33'--- END OF LIST --- # OF ENTRIES='               
         EDIT  (B4,CNTTAB),(8,PLINE+33)                                         
         BRAS  RE,PRINTL                                                        
*                                                                               
         CLC   CNTDEEPS,ZEROS      NOTHING                                      
         BE    LIST050                                                          
*                                                                               
         MVC   PLINE(33),=CL33'------------------- DEEPEST NODE='               
         EDIT  (B4,CNTDEEP),(8,PLINE+33)                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PLINE(33),=CL33'----AVERAGE DEPTH OF A TREE NODE='               
         XR    R0,R0                                                            
         L     R1,CNTDEEPS                                                      
         D     R0,CNTTAB                                                        
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(8,PLINE+33)                                           
         BRAS  RE,PRINTL                                                        
*                                                                               
LIST050  LA    R4,L'FILES(,R4)                                                  
         B     LIST010                                                          
*                                                                               
LISTITX  B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* DETAILS ON AVAILABLE QUEUES                                                   
***********************************************************************         
AEQUEUE  NTR1  BASE=*,LABEL=*                                                   
         MVI   LPNO,C'N'                                                        
*                                                                               
         SAM31                                                                  
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'='                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
         MVC   PLINE(31),=CL31' - SCANNING AVAILABLE QUEUE '                    
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'='                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
*                                                                               
         LA    R4,FILES            FILES DEFINED IN PARAMETERS                  
AEQ010   CLI   0(R4),C' '          END OF LIST                                  
         BE    AEQ200                                                           
         MVC   FIWRES,0(R4)                                                     
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'-'                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+2(L'FIWRES),FIWRES                                         
         BRAS  RE,PRINTL                                                        
         MVI   PLINE,C'-'                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE                                         
         BRAS  RE,PRINTL                                                        
*                                                                               
         BRAS  RE,FIRSET                                                        
         BE    AEQ020                                                           
         MVC   PLINE(40),=C'**ERROR** UNABLE TO SET SHARED ADDRESSES'           
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
                                                                                
*----------------------------------------------------------------------         
* PART1 AVAILABLE QUEUE                                                         
*----------------------------------------------------------------------         
AEQ020   L     R8,FIWSHA                                                        
         L     R9,FIWRHA                                                        
*                                                                               
         XC    CNTTAB,CNTTAB                                                    
*                                                                               
         MVC   PLINE(22),=CL22'   PART1 HEAD IS EMPTY'                          
         ICM   R3,15,SITP1HD                                                    
         BZ    AEQ060                                                           
         MVC   PLINE(22),SPACES                                                 
         MVC   PLINE(16),=CL16'   PART1 HEAD='                                  
*                                                                               
         USING SI1PARD,R3                                                       
AEQ030   A     R3,FIWSHA                                                        
         ST    R3,FIWNDA                                                        
*                                                                               
         BRAS  RE,FIRNC                                                         
         MVC   FULL,SI1NUM                                                      
*                                                                               
*        CLC   SI1NXT,ZEROS                                                     
*        BNE   AEQ031A                                                          
         CLC   SI1PTP,ZEROS                                                     
         BNE   AEQ031                                                           
         CLC   SI1LTP,ZEROS                                                     
         BNE   AEQ031                                                           
         CLC   SI1RTP,ZEROS                                                     
         BE    AEQ032                                                           
AEQ031   MVC   PLINE(40),=CL40'*ERROR* P1 AVAIL Q HAS A TREE LINK'              
         B     AEQ034                                                           
AEQ031A  MVC   PLINE(40),=CL40'*ERROR* P1 AVAIL Q HAS CI LINK'                  
         B     AEQ034                                                           
*                                                                               
AEQ032   ICM   R1,15,CNTTAB                                                     
         BNZ   *+8                                                              
         BRAS  RE,PRINTSLT                                                      
*                                                                               
         CLI   CNTTAB+3,1                                                       
         BNE   AEQ040                                                           
         MVC   PLINE+16(3),=C'...'                                              
AEQ034   BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   FULL,SI1NUM                                                      
         BRAS  RE,PRINTSLT                                                      
*                                                                               
AEQ040   L     R1,CNTTAB                                                        
         AHI   R1,1                                                             
         ST    R1,CNTTAB                                                        
*                                                                               
         C     R1,SIT1CIC                                                       
         BNH   AEQ044                                                           
         MVC   PLINE(40),=C'**ERROR** PART1 AVAILABLE QUEUE LOOPING'            
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
*                                                                               
AEQ044   ICM   R3,15,SI1NAV                                                     
         BNZ   AEQ030                                                           
*                                                                               
         L     R3,FIWNDA                                                        
         BRAS  RE,FIRNC                                                         
         MVC   PLINE(16),=CL16'   LAST PART1='                                  
         MVC   FULL,SI1NUM                                                      
         BRAS  RE,PRINTSLT                                                      
*                                                                               
         MVC   PLINE(22),=CL22'   PART1 TAIL EMPTY'                             
         ICM   R3,15,SITP1TL                                                    
         BZ    AEQ060                                                           
         MVC   PLINE(22),SPACES                                                 
*                                                                               
         MVC   PLINE(16),=CL16'   PART1 TAIL='                                  
         A     R3,FIWSHA                                                        
         ST    R3,FIWNDA                                                        
         BRAS  RE,FIRNC                                                         
         MVC   FULL,SI1NUM                                                      
         BRAS  RE,PRINTSLT                                                      
         DROP  R3                                                               
*                                                                               
AEQ060   BRAS  RE,PRINTL                                                        
                                                                                
*----------------------------------------------------------------------         
* PART2 AVAILABLE QUEUE                                                         
*----------------------------------------------------------------------         
         XC    CNTTAB,CNTTAB                                                    
*                                                                               
         MVC   PLINE(22),=CL22'   PART1 HEAD IS EMPTY'                          
         ICM   R3,15,SITP2HD                                                    
         BZ    AEQ100                                                           
         MVC   PLINE(22),SPACES                                                 
         MVC   PLINE(16),=CL16'   PART2 HEAD='                                  
*                                                                               
         USING SI2PARD,R3                                                       
AEQ080   A     R3,FIWSHA                                                        
         ST    R3,FIWNDA                                                        
*                                                                               
         BRAS  RE,FIRNC                                                         
         MVC   FULL,SI2NUM                                                      
*                                                                               
*        CLC   SI2PRV,ZEROS                                                     
*        BNE   AEQ081                                                           
*        CLC   SI2NXT,ZEROS                                                     
*        BE    AEQ082                                                           
*EQ081   MVC   PLINE(41),=C'*ERROR* P2 AVAIL Q HAS CI LINK'                     
*        B     AEQ084                                                           
*                                                                               
AEQ082   ICM   R1,15,CNTTAB                                                     
         BNZ   *+8                                                              
         BRAS  RE,PRINTSLT                                                      
*                                                                               
         CLI   CNTTAB+3,1                                                       
         BNE   AEQ088                                                           
         MVC   PLINE+16(3),=C'...'                                              
AEQ084   BRAS  RE,PRINTL                                                        
         MVC   FULL,SI2NUM                                                      
         BRAS  RE,PRINTSLT                                                      
*                                                                               
AEQ088   L     R1,CNTTAB                                                        
         AHI   R1,1                                                             
         ST    R1,CNTTAB                                                        
*                                                                               
         C     R1,SIT2CIC                                                       
         BNH   AEQ090                                                           
         MVC   PLINE(40),=C'**ERROR** PART2 AVAILABLE QUEUE LOOPING'            
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
*                                                                               
AEQ090   ICM   R3,15,SI2NAV                                                     
         BNZ   AEQ080                                                           
*                                                                               
         L     R3,FIWNDA                                                        
         BRAS  RE,FIRNC                                                         
         MVC   PLINE(16),=CL16'   LAST PART2='                                  
         MVC   FULL,SI2NUM                                                      
         BRAS  RE,PRINTSLT                                                      
*                                                                               
         MVC   PLINE(22),=CL22'   PART2 TAIL IS EMPTY'                          
         ICM   R3,15,SITP2TL                                                    
         BZ    AEQ100                                                           
         MVC   PLINE(22),SPACES                                                 
         MVC   PLINE(16),=CL16'   PART2 TAIL='                                  
         A     R3,FIWSHA                                                        
         ST    R3,FIWNDA                                                        
         BRAS  RE,FIRNC                                                         
         MVC   FULL,SI2NUM                                                      
         BRAS  RE,PRINTSLT                                                      
*                                                                               
AEQ100   BRAS  RE,PRINTL                                                        
*                                                                               
         LA    R4,L'FILES(,R4)                                                  
         B     AEQ010                                                           
         DROP  R3                                                               
*                                                                               
AEQ200   BRAS  RE,PRINTL                                                        
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT DETAILS ABOUT THE SLOT                                                  
***********************************************************************         
PRINTSLT NTR1  BASE=*,LABEL=*                                                   
         SAM24                                                                  
         MVC   PLINE+16(5),=C'SLOT='                                            
         EDIT  (B4,FULL),(8,PLINE+21),ALIGN=LEFT                                
*                                                                               
         MVC   PLINE+31(5),=C'DSPL='                                            
         L     R1,FIWNDA                                                        
         S     R1,FIWSHA                                                        
         ST    R1,FULL                                                          
         GOTO1 VHEXOUT,DMCB,FULL,PLINE+36,4                                     
*                                                                               
         MVC   FULL,FIWCIA                                                      
         MVC   PLINE+46(5),=C'ADDR='                                            
         GOTO1 VHEXOUT,DMCB,FULL,PLINE+51,4                                     
*                                                                               
         MVC   PLINE+61(6),=C'COUNT='                                           
         EDIT  (B4,CNTTAB),(8,PLINE+67),ALIGN=LEFT                              
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* PRINT THE INDEX                                                               
***********************************************************************         
PRINTNDX NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'PRTQ',FIWRES                                                  
         BE    PNDXPQ                                                           
         CLC   =C'WRKF',FIWRES                                                  
         BE    PNDXWF                                                           
         CLC   =C'WRKZ',FIWRES                                                  
         BE    PNDXWZ                                                           
         DC    H'0'                                                             
*                                                                               
PNDXPQ   SAM24                                                                  
*                                                                               
         LA    R4,BIGWORK                                                       
         USING PQRECD,R4                                                        
         MVC   PLUSER,SPACES                                                    
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,PQSRCID                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,IOKEY,AIOAREA                        
         BNE   PXPQ020                                                          
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CTIDATA                                                       
PXPQ010  CLI   0(R3),0                                                          
         BE    PXPQ020                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    PXPQ012                                                          
         CLI   0(R3),CTAGYELQ                                                   
         BE    PXPQ013                                                          
PXPQ011  LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PXPQ010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
PXPQ012  MVC   PLUSER,CTDSC                                                     
         B     PXPQ011                                                          
*                                                                               
         USING CTAGYD,R3                                                        
PXPQ013  MVC   SVAGYID,CTAGYID                                                  
         B     PXPQ011                                                          
         DROP  R2,R3                                                            
                                                                                
PXPQ020  CLC   PLUSER,SPACES                                                    
         BH    PXPQ022                                                          
         GOTO1 VHEXOUT,DMCB,PQSRCID,PLUSER,L'PQSRCID                            
PXPQ022  MVC   PLSYSP(L'PQSUBID),PQSUBID                                        
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),PQREPNO                                                
         EDIT  (B4,FULL),(5,PLREPNO)                                            
         MVC   PLPCLASS,PQCLASS                                                 
         GOTO1 VHEXOUT,DMCB,PQTYPE,PLTYPE,L'PQTYPE                              
         GOTO1 VHEXOUT,DMCB,PQATTB,PLATTB,L'PQATTB                              
         GOTO1 VHEXOUT,DMCB,PQSTAT,PLSTAT,L'PQSTAT                              
         EDIT  (B1,PQSEQ),(3,PLSEQ)                                             
*                                                                               
         MVC   HALF1,PQAGELD                                                    
         GOTO1 VDATCON,DMCB,(14,HALF1),(21,PLAGE)                               
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         MVC   HALF1,PQAGERD                                                    
         GOTO1 VDATCON,DMCB,(14,HALF1),(21,PLAGER)                              
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
         LLC   R1,PQAGERT                                                       
         MHI   R1,10*60                                                         
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(8,PLAGERT)                                            
*                                                                               
         MVC   FULL,FIWCIA                                                      
         GOTO1 VHEXOUT,DMCB,FULL,PLCI,2                                         
         GOTO1 VHEXOUT,DMCB,PQCINEXT,PLNEXT,L'PQCINEXT                          
*                                                                               
         SAM31                                                                  
*                                                                               
         CLI   BYTE,2                                                           
         BE    PXPQX                                                            
*                                                                               
         L     R1,ACURPAR1                                                      
         L     R1,SI1NUM-SI1PAR(R1)                                             
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(5,PLSLOT)                                             
*                                                                               
         L     R1,ACURPAR1                                                      
         ICM   R1,15,SI1NXT-SI1PAR(R1)                                          
         BZ    PXPQX                                                            
         A     R1,FIWSHA                                                        
         L     R1,SI2NUM-SI2PAR(R1)                                             
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(5,PLNSLOT)                                            
*                                                                               
PXPQX    CLC   AGENCYID,SPACES         FILTER ON ALPHA ID?                      
         BNH   PXPQXX                  NO                                       
         MVC   PLAGYID,SVAGYID                                                  
*                                                                               
         CLC   AGENCYID,SVAGYID        THIS THE ID WE'RE LOOKING FOR?           
         BE    PXPQXX                  YES                                      
         MVC   PLINE,SPACES            NO: CLEAR THE LINE                       
         B     EXITOK                                                           
*                                                                               
PXPQXX   BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         DROP  R4                                                               
                                                                                
*----------------------------------------------------------------------         
* PRINT THE INDEX - WRKF WORKER FILES                                           
*----------------------------------------------------------------------         
PNDXWF   SAM24                                                                  
*                                                                               
         LA    R4,BIGWORK                                                       
         USING W_RECD,R4                                                        
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,W_USRID                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,IOKEY,AIOAREA                        
         BNE   PXWF020                                                          
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CTIDATA                                                       
PXWF010  CLI   0(R3),0                                                          
         BE    PXWF020                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    PXWF012                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PXWF010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
PXWF012  MVC   PLUSER,CTDSC                                                     
         B     PXWF022                                                          
         DROP  R2,R3                                                            
                                                                                
PXWF020  GOTO1 VHEXOUT,DMCB,W_USRID,PLUSER,L'W_USRID                            
PXWF022  MVC   PLSYSP,W_SYSPRG                                                  
         GOTO1 VHEXOUT,DMCB,W_DAY,PLDAY,L'W_DAY                                 
         MVC   PLCLASS,W_CLASS                                                  
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),W_FILENO                                               
         EDIT  (B4,FULL),(5,PLREFNO)                                            
         GOTO1 VHEXOUT,DMCB,W_TYPE,PLTYPE,L'W_TYPE                              
         GOTO1 VHEXOUT,DMCB,W_ATTB,PLATTB,L'W_ATTB                              
         GOTO1 VHEXOUT,DMCB,W_STAT,PLSTAT,L'W_STAT                              
         EDIT  (B1,W_SEQ),(3,PLSEQ)                                             
*                                                                               
         MVC   HALF1,W_AGELD                                                    
         GOTO1 VDATCON,DMCB,(14,HALF1),(21,PLAGE)                               
         ORG   *-2                                                              
         TM    W_ATTB,W_ATNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         MVC   HALF1,W_AGERD                                                    
         GOTO1 VDATCON,DMCB,(14,HALF1),(21,PLAGER)                              
         ORG   *-2                                                              
         TM    W_ATTB,W_ATNCD      SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         LLC   R1,W_AGERT                                                       
         MHI   R1,10*60                                                         
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(8,PLAGERT)                                            
*                                                                               
         MVC   FULL,FIWCIA                                                      
         GOTO1 VHEXOUT,DMCB,FULL,PLCI,2                                         
         GOTO1 VHEXOUT,DMCB,W_CIPREV,PLPREV,L'W_CIPREV                          
         GOTO1 VHEXOUT,DMCB,W_CINEXT,PLNEXT,L'W_CINEXT                          
*                                                                               
         SAM31                                                                  
*                                                                               
         CLI   BYTE,2                                                           
         BE    PXWFX                                                            
*                                                                               
         L     R1,ACURPAR1                                                      
         L     R1,SI1NUM-SI1PAR(R1)                                             
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(5,PLSLOT)                                             
*                                                                               
         L     R1,ACURPAR1                                                      
         ICM   R1,15,SI1NXT-SI1PAR(R1)                                          
         BZ    PXWFX                                                            
         A     R1,FIWSHA                                                        
         L     R1,SI2NUM-SI2PAR(R1)                                             
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(5,PLNSLOT)                                            
*                                                                               
PXWFX    BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   FIWNDA,ACURPAR1                                                  
         BRAS  RE,FIRNC             CALCULATE THE REFERENCE NUMBER              
*                                                                               
         CLC   W_FILENO,FIWREF+2                                                
         BE    CHKDATE                                                          
         MVC   PLINE+1(8),=CL8'>>>>>>>>'                                        
         MVC   PLINE+10(20),=CL20'*BAD FILE NUMBER'                             
         BRAS  RE,PRINTL                                                        
         B     CHK020                                                           
*                                                                               
*        TEMP UNTIL 2027  CHECK DATE                                            
*                                                                               
CHKDATE  XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RE,3,TODAYO         TODAY IN NCD DATE                            
         SRL   RE,9                SET RE TO YEAR ONLY                          
         CHI   RE,127              2027                                         
         JNL   CHK020              IF 2027+ DON'T CHECK                         
*                                                                               
         TM    W_ATTB,W_ATNCD      NEW COMP DATE                                
         JZ    CHK010                                                           
*                                                                               
         ICM   RE,3,TODAY          TODAY IN NCD DATE                            
         SRL   RE,9                SET RE TO YEAR ONLY                          
         ICM   RF,3,W_AGELD                                                     
         SRL   RF,9                                                             
         SR    RE,RF               HOW MANY YEARS OLD                           
         JM    CHKERR              NEGATIVE MUST BE WRONG                       
         CHI   RE,40               >40 MUST BE WRONG                            
         JH    CHKERR                                                           
*                                                                               
         ICM   RE,3,TODAY          TODAY IN NCD DATE                            
         SRL   RE,9                SET RE TO YEAR ONLY                          
         ICM   RF,3,W_AGERD                                                     
         CLM   RF,3,=X'FF9F'       PERMANENT RETAIN IS OK                       
         JNL   CHK020                                                           
         SRL   RF,9                                                             
         SR    RF,RE               HOW MANY YEARS TO EXPIRY                     
         JM    CHKERR              NEGATIVE MUST BE WRONG                       
         CHI   RF,40               >40 MUST BE WRONG                            
         JH    CHKERR                                                           
         J     CHK020                                                           
*                                                                               
CHK010   ICM   RE,3,TODAYO         TODAY IN OLD DATE                            
         SRL   RE,9                SET RE TO YEAR ONLY                          
         ICM   RF,3,W_AGELD                                                     
         SRL   RF,9                                                             
         SR    RE,RF               HOW MANY YEARS OLD                           
         JM    CHKERR              NEGATIVE MUST BE WRONG                       
         CHI   RE,40               >40 MUST BE WRONG                            
         JH    CHKERR                                                           
*                                                                               
         ICM   RE,3,TODAYO         TODAY IN NCD DATE                            
         SRL   RE,9                SET RE TO YEAR ONLY                          
         ICM   RF,3,W_AGERD                                                     
         CLM   RF,3,=X'FF9F'       PERMANENT RETAIN IS OK                       
         JNL   CHK020                                                           
         SRL   RF,9                                                             
         SR    RF,RE               HOW MANY YEARS TO EXPIRY                     
         JM    CHKERR              NEGATIVE MUST BE WRONG                       
         CHI   RF,40               >40 MUST BE WRONG                            
         JH    CHKERR                                                           
         J     CHK020                                                           
*                                                                               
CHKERR   MVC   PLINE+1(8),=CL8'>>>>>>>>'                                        
         MVC   PLINE+10(20),=CL20'*NCD DATE ERROR '                             
         BRAS  RE,PRINTL                                                        
*                                                                               
CHK020   DS    0H                                                               
*                                                                               
*        ENDTEMP UNTIL 2027                                                     
*                                                                               
         B     EXITOK                                                           
         DROP  R4                                                               
                                                                                
*----------------------------------------------------------------------         
* PRINT THE INDEX - WRKZ WORKER FILES                                           
*----------------------------------------------------------------------         
PNDXWZ   SAM24                                                                  
*                                                                               
         LA    R4,BIGWORK                                                       
         USING WZW_RECD,R4                                                      
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,WZW_USRID                                                
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,IOKEY,AIOAREA                        
         BNE   PXWZ020                                                          
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CTIDATA                                                       
PXWZ010  CLI   0(R3),0                                                          
         BE    PXWZ020                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    PXWZ012                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PXWZ010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
PXWZ012  MVC   PLUSER,CTDSC                                                     
         B     PXWZ022                                                          
         DROP  R2,R3                                                            
                                                                                
PXWZ020  GOTO1 VHEXOUT,DMCB,WZW_USRID,PLUSER,L'WZW_USRID                        
PXWZ022  MVC   PLSYSP,WZW_SYSPRG                                                
         GOTO1 VHEXOUT,DMCB,WZW_DAY,PLDAY,L'WZW_DAY                             
         MVC   PLCLASS,WZW_CLASS                                                
         MVC   FULL,WZW_FILENO                                                  
         EDIT  (B4,FULL),(5,PLREFNO)                                            
         GOTO1 VHEXOUT,DMCB,WZW_TYPE,PLTYPE,L'WZW_TYPE                          
         GOTO1 VHEXOUT,DMCB,WZW_ATTB,PLATTB,L'WZW_ATTB                          
         GOTO1 VHEXOUT,DMCB,WZW_STAT,PLSTAT,L'WZW_STAT                          
         EDIT  (B1,WZW_SEQ),(3,PLSEQ)                                           
*                                                                               
         MVC   HALF1,WZW_AGELD                                                  
         GOTO1 VDATCON,DMCB,(14,HALF1),(21,PLAGE)                               
         ORG   *-2                                                              
         TM    WZW_ATTB,WZW_ATNCD  SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         MVC   HALF1,WZW_AGERD                                                  
         GOTO1 VDATCON,DMCB,(14,HALF1),(21,PLAGER)                              
         ORG   *-2                                                              
         TM    WZW_ATTB,WZW_ATNCD  SET HALF1 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         LLC   R1,WZW_AGERT                                                     
         MHI   R1,10*60                                                         
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(8,PLAGERT)                                            
*                                                                               
         MVC   FULL,FIWCIA                                                      
         GOTO1 VHEXOUT,DMCB,FULL,PLCI,2                                         
         GOTO1 VHEXOUT,DMCB,WZW_CIPREV,PLPREV,L'WZW_CIPREV                      
         GOTO1 VHEXOUT,DMCB,WZW_CINEXT,PLNEXT,L'WZW_CINEXT                      
*                                                                               
         SAM31                                                                  
*                                                                               
         CLI   BYTE,2                                                           
         BE    PXWZX                                                            
*                                                                               
         L     R1,ACURPAR1                                                      
         L     R1,SI1NUM-SI1PAR(R1)                                             
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(5,PLSLOT)                                             
*                                                                               
         L     R1,ACURPAR1                                                      
         ICM   R1,15,SI1NXT-SI1PAR(R1)                                          
         BZ    PXWZX                                                            
         A     R1,FIWSHA                                                        
         L     R1,SI2NUM-SI2PAR(R1)                                             
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(5,PLNSLOT)                                            
*                                                                               
PXWZX    BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
* PRINT THE INDEX FOR LIST                                                      
***********************************************************************         
PNDXLIST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'PRTQ',FIWRES                                                  
         BE    PILPQ                                                            
         CLC   =C'WRKF',FIWRES                                                  
         BE    PILWF                                                            
         CLC   =C'WRKZ',FIWRES                                                  
         BE    PILWZ                                                            
         DC    H'0'                                                             
*                                                                               
PILPQ    SAM24                                                                  
         LA    R4,TEMP                                                          
         USING PQRECD,R4                                                        
         MVC   TEMP,BIGWORK                                                     
         CLC   SVUSER,PQSRCID                                                   
         BE    PIPQ024                                                          
         MVC   SVUSER,PQSRCID                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,PQSRCID                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,IOKEY,AIOAREA                        
         BNE   PIPQ020                                                          
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CTIDATA                                                       
PIPQ010  CLI   0(R3),0                                                          
         BE    PIPQ020                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    PIPQ012                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PIPQ010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
PIPQ012  MVC   PLINE+1(8),CTDSC                                                 
         B     PIPQ022                                                          
         DROP  R2,R3                                                            
*                                                                               
PIPQ020  MVI   PLINE+1,C'?'                                                     
PIPQ022  BAS   RE,PRINTL                                                        
*                                                                               
PIPQ024  MVI   PTNUSOP,C'('                                                     
         EDIT  (B2,PQSRCID),(5,PTNUSNO)                                         
         MVI   PTNUSCP,C')'                                                     
         MVC   PTNSYSP,SPACES                                                   
         MVC   PTNSYSP(L'PQSUBID),PQSUBID                                       
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),PQREPNO                                                
         EDIT  (B4,FULL),(5,PTNREPNO)                                           
*                                                                               
         MVC   PTPARENT(13),=C'PARENT=*ROOT*'                                   
         MVC   PTLEFT(11),=C'LEFT=*NULL*'                                       
         MVC   PTRIGHT(12),=C'RIGHT=*NULL*'                                     
         MVC   PTDEPTH(18),=CL18'DEPTH=*NULL*'                                  
*                                                                               
         SAM31                                                                  
         L     R2,FIWNDA                                                        
I        USING SI1PARD,R2                                                       
N        USING SI1PARD,R1                                                       
*                                                                               
         ICM   R1,15,I.SI1PTP                                                   
         BZ    PIPQ060                                                          
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1NDX                                                    
         SAM24                                                                  
         MVC   PTPSYSP,SPACES                                                   
         MVC   PTPSYSP(L'PQSUBID),PQSUBID                                       
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),PQREPNO                                                
         EDIT  (B4,FULL),(5,PTPREPNO)                                           
         SAM31                                                                  
*                                                                               
PIPQ060  ICM   R1,15,I.SI1LTP                                                   
         BZ    PIPQ070                                                          
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1NDX                                                    
         SAM24                                                                  
         MVC   PTLSYSP,SPACES                                                   
         MVC   PTLSYSP(L'PQSUBID),PQSUBID                                       
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),PQREPNO                                                
         EDIT  (B4,FULL),(5,PTLREPNO)                                           
         SAM31                                                                  
*                                                                               
PIPQ070  ICM   R1,15,I.SI1RTP                                                   
         BZ    PIPQ080                                                          
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1NDX                                                    
         SAM24                                                                  
         MVC   PTRSYSP,SPACES                                                   
         MVC   PTRSYSP(L'PQSUBID),PQSUBID                                       
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),PQREPNO                                                
         EDIT  (B4,FULL),(5,PTRREPNO)                                           
*                                                                               
PIPQ080  EDIT  (B4,CNTDEPTH),(8,PTDDEEP)                                        
         DROP  I,N                                                              
*                                                                               
PIPQX    BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         DROP  R4                                                               
                                                                                
*----------------------------------------------------------------------         
* WRKF WORKER FILE PRINT LIST                                                   
*----------------------------------------------------------------------         
PILWF    SAM24                                                                  
         LA    R4,TEMP                                                          
         USING W_RECD,R4                                                        
         MVC   TEMP,BIGWORK                                                     
         CLC   SVUSER,W_USRID                                                   
         BE    PIWF024                                                          
         MVC   SVUSER,W_USRID                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,W_USRID                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,IOKEY,AIOAREA                        
         BNE   PIWF020                                                          
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CTIDATA                                                       
PIWF010  CLI   0(R3),0                                                          
         BE    PIWF020                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    PIWF012                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PIWF010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
PIWF012  MVC   PLINE+1(8),CTDSC                                                 
         B     PIWF022                                                          
         DROP  R2,R3                                                            
*                                                                               
PIWF020  MVI   PLINE+1,C'?'                                                     
PIWF022  BAS   RE,PRINTL                                                        
*                                                                               
PIWF024  MVI   PTNUSOP,C'('                                                     
         EDIT  (B2,W_USRID),(5,PTNUSNO)                                         
         MVI   PTNUSCP,C')'                                                     
         MVC   PTNSYSP,W_SYSPRG                                                 
         GOTO1 VHEXOUT,DMCB,W_DAY,PTNDAY,L'W_DAY                                
         MVC   PTNCLASS,W_CLASS                                                 
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),W_FILENO                                               
         EDIT  (B4,FULL),(5,PTNREFNO)                                           
*                                                                               
         MVC   PTPARENT(13),=C'PARENT=*ROOT*'                                   
         MVC   PTLEFT(11),=C'LEFT=*NULL*'                                       
         MVC   PTRIGHT(12),=C'RIGHT=*NULL*'                                     
         MVC   PTDEPTH(18),=CL18'DEPTH=*NULL*'                                  
*                                                                               
         SAM31                                                                  
         L     R2,FIWNDA                                                        
I        USING SI1PARD,R2                                                       
N        USING SI1PARD,R1                                                       
*                                                                               
         ICM   R1,15,I.SI1PTP                                                   
         BZ    PIWF060                                                          
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1NDX                                                    
         SAM24                                                                  
         MVC   PTPSYSP,W_SYSPRG                                                 
         GOTO1 VHEXOUT,DMCB,W_DAY,PTPDAY,L'W_DAY                                
         MVC   PTPCLASS,W_CLASS                                                 
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),W_FILENO                                               
         EDIT  (B4,FULL),(5,PTPREFNO)                                           
         SAM31                                                                  
*                                                                               
PIWF060  ICM   R1,15,I.SI1LTP                                                   
         BZ    PIWF070                                                          
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1NDX                                                    
         SAM24                                                                  
         MVC   PTLSYSP,W_SYSPRG                                                 
         GOTO1 VHEXOUT,DMCB,W_DAY,PTLDAY,L'W_DAY                                
         MVC   PTLCLASS,W_CLASS                                                 
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),W_FILENO                                               
         EDIT  (B4,FULL),(5,PTLREFNO)                                           
         SAM31                                                                  
*                                                                               
PIWF070  ICM   R1,15,I.SI1RTP                                                   
         BZ    PIWF080                                                          
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1NDX                                                    
         SAM24                                                                  
         MVC   PTRSYSP,W_SYSPRG                                                 
         GOTO1 VHEXOUT,DMCB,W_DAY,PTRDAY,L'W_DAY                                
         MVC   PTRCLASS,W_CLASS                                                 
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),W_FILENO                                               
         EDIT  (B4,FULL),(5,PTRREFNO)                                           
*                                                                               
PIWF080  EDIT  (B4,CNTDEPTH),(8,PTDDEEP)                                        
         DROP  I,N                                                              
*                                                                               
PIWFX    BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         DROP  R4                                                               
                                                                                
*----------------------------------------------------------------------         
* WRKZ WORKER FILE PRINT THE INDEX FOR LIST                                     
*----------------------------------------------------------------------         
PILWZ    SAM24                                                                  
         LA    R4,TEMP                                                          
         USING WZW_RECD,R4                                                      
         MVC   TEMP,BIGWORK                                                     
         CLC   SVUSER,WZW_USRID                                                 
         BE    PIWZ024                                                          
         MVC   SVUSER,WZW_USRID                                                 
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,WZW_USRID                                                
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,IOKEY,AIOAREA                        
         BNE   PIWZ020                                                          
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CTIDATA                                                       
PIWZ010  CLI   0(R3),0                                                          
         BE    PIWZ020                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    PIWZ012                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PIWZ010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
PIWZ012  MVC   PLINE+1(8),CTDSC                                                 
         B     PIWZ022                                                          
         DROP  R2,R3                                                            
*                                                                               
PIWZ020  MVI   PLINE+1,C'?'                                                     
PIWZ022  BAS   RE,PRINTL                                                        
*                                                                               
PIWZ024  MVI   PTNUSOP,C'('                                                     
         EDIT  (B2,WZW_USRID),(5,PTNUSNO)                                       
         MVI   PTNUSCP,C')'                                                     
         MVC   PTNSYSP,WZW_SYSPRG                                               
         GOTO1 VHEXOUT,DMCB,WZW_DAY,PTNDAY,L'WZW_DAY                            
         MVC   PTNCLASS,WZW_CLASS                                               
         MVC   FULL,WZW_FILENO                                                  
         EDIT  (B4,FULL),(5,PTNREFNO)                                           
*                                                                               
         MVC   PTPARENT(13),=C'PARENT=*ROOT*'                                   
         MVC   PTLEFT(11),=C'LEFT=*NULL*'                                       
         MVC   PTRIGHT(12),=C'RIGHT=*NULL*'                                     
         MVC   PTDEPTH(18),=CL18'DEPTH=*NULL*'                                  
*                                                                               
         SAM31                                                                  
         L     R2,FIWNDA                                                        
I        USING SI1PARD,R2                                                       
N        USING SI1PARD,R1                                                       
*                                                                               
         ICM   R1,15,I.SI1PTP                                                   
         BZ    PIWZ060                                                          
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1NDX                                                    
         SAM24                                                                  
         MVC   PTPSYSP,WZW_SYSPRG                                               
         GOTO1 VHEXOUT,DMCB,WZW_DAY,PTPDAY,L'WZW_DAY                            
         MVC   PTPCLASS,WZW_CLASS                                               
         MVC   FULL,WZW_FILENO                                                  
         EDIT  (B4,FULL),(5,PTPREFNO)                                           
         SAM31                                                                  
*                                                                               
PIWZ060  ICM   R1,15,I.SI1LTP                                                   
         BZ    PIWZ070                                                          
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1NDX                                                    
         SAM24                                                                  
         MVC   PTLSYSP,WZW_SYSPRG                                               
         GOTO1 VHEXOUT,DMCB,WZW_DAY,PTLDAY,L'WZW_DAY                            
         MVC   PTLCLASS,WZW_CLASS                                               
         MVC   FULL,WZW_FILENO                                                  
         EDIT  (B4,FULL),(5,PTLREFNO)                                           
         SAM31                                                                  
*                                                                               
PIWZ070  ICM   R1,15,I.SI1RTP                                                   
         BZ    PIWZ080                                                          
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1NDX                                                    
         SAM24                                                                  
         MVC   PTRSYSP,WZW_SYSPRG                                               
         GOTO1 VHEXOUT,DMCB,WZW_DAY,PTRDAY,L'WZW_DAY                            
         MVC   PTRCLASS,WZW_CLASS                                               
         MVC   FULL,WZW_FILENO                                                  
         EDIT  (B4,FULL),(5,PTRREFNO)                                           
*                                                                               
PIWZ080  EDIT  (B4,CNTDEPTH),(8,PTDDEEP)                                        
         DROP  I,N                                                              
*                                                                               
PIWZX    BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
* PRINT TOTALS                                                                  
***********************************************************************         
PRINTTOT NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
         L     R8,FIWSHA           R8= A(START OF SHARED MEMORY)                
*                                                                               
         MVC   TITLE,SPACES                                                     
         MVC   TITLE2,SPACES                                                    
*                                                                               
         L     R4,SIHNOFR          NUMBER OF RESOURCES                          
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BRAS  RE,PRINTL                                                        
         MVC   PTDESC,=CL28'TOTAL NUMBER OF RESOURCES='                         
         EDIT  (R4),PTCOUNT                                                     
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BRAS  RE,PRINTL                                                        
*                                                                               
         LA    R4,FILES                                                         
PT010    CLI   0(R4),C' '                                                       
         BE    PRINTTX                                                          
*                                                                               
         MVC   FIWRES,0(R4)                                                     
*                                                                               
         BRAS  RE,FIRSET                                                        
         L     R8,FIWSHA           R8= A(START OF SHARED MEMORY)                
         L     R9,FIWRHA           R9= A(RESOURCE HEADER)                       
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PRRESO(8),=C'RESOURCE'                                           
         LLC   R2,SITNUM                                                        
         EDIT  (R2),PRNUM                                                       
         MVC   PRNAME,SITFIL                                                    
         MVC   PRDESC(10),=C'TABLE SIZE'                                        
         L     R2,SITSIZE                                                       
         EDIT  (R2),(8,PRVALU),ZERO=NOBLANK                                     
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BRAS  RE,PRINTL                                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PICIS,=CL12'# OF CIS'                                            
         MVC   PIAV,=CL12'AVAILABLE'                                            
         MVC   PIPAV,=CL12'% AVAILABLE'                                         
         MVC   PIVU,=CL12'VULNERABLE'                                           
         MVC   PIPVU,=CL12'% VULNERABLE'                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PICIS,DASHES                                                     
         MVC   PIAV,DASHES                                                      
         MVC   PIPAV,DASHES                                                     
         MVC   PIVU,DASHES                                                      
         MVC   PIPVU,DASHES                                                     
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PIPART(5),=C'PART1'                                              
*                                                                               
         L     R2,SIT1CIC                                                       
         EDIT  (R2),(8,PICIS),ZERO=NOBLANK                                      
         L     R2,SITP1AV                                                       
         EDIT  (R2),(8,PIAV),ZERO=NOBLANK                                       
         L     R2,SITP1VU                                                       
         EDIT  (R2),(8,PIVU),ZERO=NOBLANK                                       
*                                                                               
         XR    R0,R0                                                            
         L     R1,SITP1AV          NUMBER OF AVAILABLE PART1S                   
         MHI   R1,10000                                                         
         D     R0,SIT1CIC          NUMBER OF PART1S FOR THIS RESOURCE           
         STH   R1,HALF             PART1 % AVAILABLE                            
         EDIT  (B2,HALF),(8,PIPAV),2,ZERO=NOBLANK,TRAIL=C'%'                    
*                                                                               
         XR    R0,R0                                                            
         L     R1,SITP1VU          NUMBER OF VULNERABLE PART1S                  
         MHI   R1,10000                                                         
         D     R0,SIT1CIC          NUMBER OF PART1S FOR THIS RESOURCE           
         STH   R1,HALF             PART1 % VULNERABLE                           
         EDIT  (B2,HALF),(8,PIPVU),2,ZERO=NOBLANK,TRAIL=C'%'                    
*                                                                               
         BRAS  RE,PRINTL                                                        
*                                                 PAPART1                       
         MVC   PIPART(5),=C'PART2'                                              
*                                                                               
         L     R2,SIT2CIC                                                       
         EDIT  (R2),(8,PICIS),ZERO=NOBLANK                                      
         L     R2,SITP2AV                                                       
         EDIT  (R2),(8,PIAV),ZERO=NOBLANK                                       
         L     R2,SITP2VU                                                       
         EDIT  (R2),(8,PIVU),ZERO=NOBLANK                                       
*                                                                               
         XR    R0,R0                                                            
         L     R1,SITP2AV          NUMBER OF AVAILABLE PART1S                   
         MHI   R1,10000                                                         
         D     R0,SIT2CIC          NUMBER OF PART2S FOR THIS RESOURCE           
         STH   R1,HALF             PART1 % AVAILABLE                            
         EDIT  (B2,HALF),(8,PIPAV),2,ZERO=NOBLANK,TRAIL=C'%'                    
*                                                                               
         XR    R0,R0                                                            
         L     R1,SITP2VU          NUMBER OF VULNERABLE PART1S                  
         MHI   R1,10000                                                         
         D     R0,SIT2CIC          NUMBER OF PART2S FOR THIS RESOURCE           
         STH   R1,HALF             PART2 % VULNERABLE                           
         EDIT  (B2,HALF),(8,PIPVU),2,ZERO=NOBLANK,TRAIL=C'%'                    
         BRAS  RE,PRINTL                                                        
*                                                                               
         BRAS  RE,PRINTL                                                        
*                                                                               
         LA    R4,L'FILES(,R4)                                                  
         B     PT010                                                            
*                                                                               
PRINTTX  B     EXITOK                                                           
                                                                                
***********************************************************************         
* DROP THESE DSECTS, THEY ARE USED IN THE SFI ROUTINES                          
***********************************************************************         
         DROP  R8,R9                                                            
                                                                                
***********************************************************************         
* SHARED MEMORY FILE INDEX ROUTINES                                             
***********************************************************************         
       ++INCLUDE DDSHFIR                                                        
                                                                                
***********************************************************************         
* USEFUL ROUTINES, EXITS AND OTHER COMMON STORAGE                               
***********************************************************************         
COMMON   DS    0D                                                               
                                                                                
***********************************************************************         
* PRINT ROUTINES                                                                
***********************************************************************         
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTTI  MVC   PLINE,SPACES                                                     
         CLC   =C'PRTQ',FIWRES                                                  
         JNE   PTI10                                                            
         MVC   PLSYSP(3),=C'SUB'                                                
         MVC   PLREPNO(4),=C'REP#'                                              
         MVC   PLPCLASS(2),=C'CL'                                               
         J     PTI20                                                            
PTI10    MVC   PLSYSP(4),=C'FILE'                                               
         MVC   PLREFNO(4),=C'REF#'                                              
         MVC   PLPREV(3),=C'PCI'                                                
PTI20    MVC   PLUSER(4),=C'COID'                                               
         MVC   PLTYPE,=C'TY'                                                    
         MVC   PLATTB,=C'AT'                                                    
         MVC   PLSTAT,=C'ST'                                                    
         MVC   PLSEQ,=C'SQ#'                                                    
         MVC   PLAGE(4),=C'DATE'                                                
         MVC   PLCI(2),=C'CI'                                                   
         MVC   PLNEXT(3),=C'NCI'                                                
         MVC   PLSLOT(4),=C'SLOT'                                               
         MVC   PLPSLOT(2),=C'PS'                                                
         MVC   PLNSLOT(2),=C'NS'                                                
         MVC   PLAGER(5),=C'XDATE'                                              
         MVC   PLAGERT(5),=C'XTIME'                                             
         MVC   TITLE,PLINE                                                      
*                                                                               
         MVC   PLINE,SPACES                                                     
         CLC   =C'PRTQ',FIWRES                                                  
         JNE   PTI30                                                            
         MVC   PLSYSP(3),DASHES                                                 
         MVC   PLREPNO,DASHES                                                   
         MVC   PLPCLASS(2),DASHES                                               
         J     PTI40                                                            
PTI30    MVC   PLSYSP(7),DASHES                                                 
         MVC   PLREFNO,DASHES                                                   
         MVC   PLPREV,DASHES                                                    
PTI40    MVC   PLUSER,DASHES                                                    
         MVC   PLTYPE,DASHES                                                    
         MVC   PLATTB,DASHES                                                    
         MVC   PLSTAT,DASHES                                                    
         MVC   PLSEQ,DASHES                                                     
         MVC   PLAGE,DASHES                                                     
         MVC   PLCI,DASHES                                                      
         MVC   PLNEXT,DASHES                                                    
         MVC   PLSLOT,DASHES                                                    
         MVC   PLPSLOT,DASHES                                                   
         MVC   PLNSLOT,DASHES                                                   
         MVC   PLAGER,DASHES                                                    
         MVC   PLAGERT,DASHES                                                   
         MVC   TITLE2,PLINE                                                     
         MVC   PLINE,SPACES                                                     
         B     PRINTT                                                           
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=PL1'0'        RESET LINECOUNT                              
         AP    PAGE,=PL1'1'        BUMP PAGECOUNT                               
         PUT   SYSPRINT,SPACES                                                  
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         PUT   SYSPRINT,TITLE2     PRINT TITLE2                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=PL1'1'        BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=PL1'3'        RESET LINECOUNT                              
         AP    PAGE,=PL1'1'        BUMP PAGECOUNT                               
         CLI   LPNO,C'N'                                                        
         BE    PRINTL2                                                          
         PUT   SYSPRINT,SPACES                                                  
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         PUT   SYSPRINT,TITLE2     PRINT TITLE2                                 
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITH    LHI   RF,2                                                             
         B     EXITCC                                                           
EXITL    LHI   RF,0                                                             
         B     EXITCC                                                           
EXITOK   LHI   RF,1                                                             
EXITCC   CHI   RF,1                                                             
EXIT     XIT1  ,                                                                
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
                                                                                
***********************************************************************         
* CONSTANTS & LTORG                                                             
***********************************************************************         
SPACES   DC    CL166' '                                                         
DASHES   DC    166C'-'                                                          
STARS    DC    16C'*'                                                           
EFFS     DC    16X'FF'                                                          
ZEROS    DC    16X'00'                                                          
MAXLINE  DC    PL3'60'                                                          
ATTACH   DC    CL8'ATTACH'                                                      
BUFFER   DC    CL8'BUFFER'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLSE   DC    CL8'DMCLSE'                                                      
FFINI    DC    CL8'FFINI'          FORCE FILE INIT                              
CONTROL  DC    CL8'CONTROL'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
SHMUSS   DC    CL8'SHMUSS'                                                      
NEXT     DC    CL8'NEXT'                                                        
EYESFI   DC    CL16'**SHFI****SHFI**'                                           
*                                                                               
FLIST    DC    CL8'NCTFILE'                                                     
         DC    CL8'X'                                                           
                                                                                
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN),X'FLAGS',AL3(OUTPUT)             
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*        X'0100'                   DATE VALUE                                   
*                                                                               
CARDTAB  DS    0F                                                               
         DC    C'DDSIO  ',AL1(4,09),X'0000',AL3(DDSIO)                          
         DC    C'DSPACE ',AL1(5,00),X'0000',AL3(SSB+SSODSPAC-SSOOFF)            
         DC    C'MODE   ',AL1(3,09),X'0000',AL3(MODE)                           
         DC    C'MEMORY ',AL1(5,07),X'0000',AL3(MEMORY)                         
         DC    C'FILE   ',AL1(3,07),X'0400',AL3(FILES)                          
         DC    C'LOCK   ',AL1(3,02),X'0000',AL3(LOCKIT)                         
         DC    C'SHORT  ',AL1(4,03),X'0000',AL3(SHORTR)                         
         DC    C'READCI ',AL1(5,03),X'0000',AL3(READCI)                         
         DC    C'AGENCY ',AL1(5,02),X'0000',AL3(AGENCYID)                       
         DC    X'0000'                                                          
*                                                                               
* CARD OUTPUT AREAS SET WITH DEFAULTS                                           
*                                                                               
DDSIO    DC    CL10'DDSIO'                                                      
MODE     DC    CL10'CHECK'          CHECK                                       
MEMORY   DC    CL8' '                                                           
LOCKIT   DC    CL3'YES'                                                         
SHORTR   DC    CL3'NO '                                                         
READCI   DC    CL3'NO '                                                         
AGENCYID DC    CL2' '                                                           
*                                                                               
FILES    DC    (MAXFILES+1)CL7' '                                               
*                                                                               
         LTORG                                                                  
*                                                                               
MAXFILES EQU   16                                                               
                                                                                
***********************************************************************         
* DCBS & ADCONS                                                                 
***********************************************************************         
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VDATCON  DC    V(DATCON)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VTREE    DC    V(TREE)                                                          
*                                                                               
ACOMM    DC    A(0)                                                             
*                                                                               
UTL      DC    F'0',AL1(10),XL250'00'                                           
SSB      DC    H'0',X'FF',X'14',1022X'00'                                       
                                                                                
***********************************************************************         
* WORKING STORAGE DC                                                            
***********************************************************************         
         DS    0D                                                               
WORKAREA DC    60000D'00'                                                       
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
AIOAREA  DS    A                                                                
ACIREC   DS    A                                                                
CARDEND  DS    A                                                                
ACURPAR1 DS    A                   A(CURRENT PART1 ENTRY)                       
ACURPAR2 DS    A                   A(CURRENT PART2 ENTRY)                       
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
HALF2    DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
*                                                                               
DMCB     DS    7F                  SHARED MEMORY CALL NEEDS 7 PARAMS            
*                                                                               
SFISIZE  DS    F                   SIZE OF SHARED MEMORY STORAGE                
CNTTAB   DS    F                   COUNT RESOURCE TABLES                        
CNTPAR1  DS    F                   COUNT PART1S                                 
CNTPAR2  DS    F                   COUNT PART2S                                 
CNTP1AV  DS    F                   NUMBER OF AVAILABLE PART1S                   
CNTP2AV  DS    F                   NUMBER OF AVAILABLE PART2S                   
CNTPART  DS    F                   TOTAL CI COUNT FOR REPORT                    
CNTDEPTH DS    F                   TREE DEPTH OF INDEX NODE                     
CNTDEEP  DS    F                                                                
CNTDEEPS DS    F                                                                
*                                                                               
WORK     DS    CL64                                                             
BIGWORK  DS    CL128                                                            
TEMP     DS    CL128                                                            
TEMPC    DS    PL3                                                              
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
LPNO     DS    C                                                                
*                                                                               
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
TITLE2   DS    CL166                                                            
*                                                                               
TODAY    DS    XL2                                                              
TODAYO   DS    XL2                                                              
*                                                                               
RINFO    DS    0X                                                               
RILKEY   DS    X                                                                
RIDSTAT  DS    X                                                                
RINFOL   EQU   *-RINFO                                                          
*                                                                               
SVAGYID  DS    CL2                                                              
SVUSER   DS    XL2                                                              
SAVEKEY  DS    XL(L'SI1NDX)                                                     
*                                                                               
MSGW     DS    0CL64               WTO OUTPUT MESSAGE                           
MSGWMSG  DS    CL40                WTO INFORMATION MESSAGE                      
MSGWDET  DS    CL24                WTO INFORMATION DETAIL                       
*                                                                               
TREEBLK  DS    XL(TREEL)                                                        
*                                                                               
       ++INCLUDE DDSHFIW        SHARED MEMORY FILE QUEUE WORK AREA              
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
KEY      DS    CL44                                                             
KEYSAVE  DS    CL44                                                             
IOKEY    DS    CL44                                                             
IOKEYSV  DS    CL44                                                             
*                                                                               
IOAREAF  DS    F                                                                
IOAREA   DS    4096C                                                            
*                                                                               
CIREC    DS    14336C                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
                                                                                
**********************************************************************          
* PRINT LINE DSECT                                                              
**********************************************************************          
PLINED   DSECT                                                                  
*                                                                               
PLUSER   DS    CL8                                                              
         DS    CL1                                                              
PLSYSP   DS    CL4                                                              
PLREPNO  DS    0CL5                                                             
PLDAY    DS    CL2                                                              
PLCLASS  DS    CL1                                                              
         DS    CL1                                                              
PLREFNO  DS    CL5                                                              
         ORG   *-2                                                              
PLPCLASS DS    CL1                                                              
         ORG                                                                    
         DS    CL1                                                              
PLTYPE   DS    CL2                                                              
         DS    CL1                                                              
PLATTB   DS    CL2                                                              
         DS    CL1                                                              
PLSTAT   DS    CL2                                                              
         DS    CL1                                                              
PLSEQ    DS    CL3                                                              
         DS    CL1                                                              
PLAGE    DS    CL10                                                             
         DS    CL1                                                              
PLCI     DS    CL4                                                              
         DS    CL1                                                              
PLPREV   DS    CL4                                                              
         DS    CL1                                                              
PLNEXT   DS    CL4                                                              
         DS    CL1                                                              
PLSLOT   DS    CL5                                                              
         DS    CL1                                                              
PLPSLOT  DS    CL5                                                              
         DS    CL1                                                              
PLNSLOT  DS    CL5                                                              
         DS    CL2                                                              
PLAGER   DS    CL10                                                             
         DS    CL1                                                              
PLAGERT  DS    CL10                                                             
         DS    CL1                                                              
PLAGYID  DS    CL2                                                              
                                                                                
*---------------------------------------------------------------------          
* TOTAL PRINT LINE DSECT                                                        
*---------------------------------------------------------------------          
         ORG   PLUSER                                                           
PTDESC   DS    CL28                                                             
         DS    CL1                                                              
PTCOUNT  DS    CL6                                                              
         DS    CL2                                                              
                                                                                
*---------------------------------------------------------------------          
* RESOURCE PRINT LINE DSECT                                                     
*---------------------------------------------------------------------          
         ORG   PLUSER                                                           
PRRESO   DS    CL8                                                              
         DS    CL1                                                              
PRNUM    DS    CL3                                                              
         DS    CL4                                                              
PRNAME   DS    CL7                                                              
         DS    CL10                                                             
PRDESC   DS    CL16                                                             
         DS    CL1                                                              
PRVALU   DS    CL8                                                              
         DS    CL2                                                              
                                                                                
*---------------------------------------------------------------------          
* RESOURCE INFORMATION LINE                                                     
*---------------------------------------------------------------------          
         ORG   PLUSER                                                           
         DS    CL1                                                              
PIPART   DS    CL8                 PART1/PART2                                  
         DS    CL1                                                              
PICIS    DS    CL12                NUMBER OF CIS                                
         DS    CL2                                                              
PIAV     DS    CL12                NUMBER OF CIS AVAILABLE                      
         DS    CL2                                                              
PIPAV    DS    CL12                % AVAILABLE                                  
         DS    CL2                                                              
PIVU     DS    CL12                NUMBER OF CIS VULNERABLE                     
         DS    CL2                                                              
PIPVU    DS    CL12                % VULNERABLE                                 
         DS    CL2                                                              
                                                                                
*---------------------------------------------------------------------          
* TREE LISTING LINE                                                             
*---------------------------------------------------------------------          
         ORG   PLUSER                                                           
PTNUSOP  DS    CL1                                                              
PTNUSNO  DS    CL5                                                              
PTNUSCP  DS    CL1                                                              
         DS    CL1                                                              
PTNSYSP  DS    CL4                                                              
PTNREPNO DS    0CL5                                                             
PTNDAY   DS    CL2                                                              
PTNCLASS DS    CL1                                                              
PTNREFNO DS    CL5                                                              
         DS    CL2                                                              
PTPARENT DS    CL7                 C'PARENT='                                   
PTPSYSP  DS    CL4                                                              
PTPREPNO DS    0CL5                                                             
PTPDAY   DS    CL2                                                              
PTPCLASS DS    CL1                                                              
PTPREFNO DS    CL5                                                              
         DS    CL1                                                              
PTLEFT   DS    CL5                 C'LEFT='                                     
PTLSYSP  DS    CL4                                                              
PTLREPNO DS    0CL5                                                             
PTLDAY   DS    CL2                                                              
PTLCLASS DS    CL1                                                              
PTLREFNO DS    CL5                                                              
         DS    CL1                                                              
PTRIGHT  DS    CL6                 C'RIGHT='                                    
PTRSYSP  DS    CL4                                                              
PTRREPNO DS    0CL5                                                             
PTRDAY   DS    CL2                                                              
PTRCLASS DS    CL1                                                              
PTRREFNO DS    CL5                                                              
         DS    CL2                                                              
PTDEPTH  DS    CL6                 C'DEPTH='                                    
PTDDEEP  DS    CL8                                                              
                                                                                
**********************************************************************          
* OTHER DSECTS                                                                  
**********************************************************************          
       ++INCLUDE DDSHFID                                                        
       ++INCLUDE DDTREED                                                        
*                                                                               
* DMWRKFD                                                                       
       ++INCLUDE DMPRTQD                                                        
       ++INCLUDE DMWRKFD                                                        
*PREFIX=WZ                                                                      
       ++INCLUDE DMWRKZD                                                        
*PREFIX=                                                                        
*                                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
*                                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* IHAASCB                                                                       
         PRINT OFF                                                              
         IHAASCB                                                                
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DDSHFICHK 11/05/19'                                      
         END                                                                    
