*          DATA SET SHFICHK    AT LEVEL 002 AS OF 07/31/12                      
*PHASE SHFICHKA                                                                 
*INCLUDE FATABOFF                                                               
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE SCANNER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE XSORT                                                                  
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
         MVC   0(2,RF),WORK+PVALCSTA-PERVALD                                    
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
* SET WORKER FILE INFORMATION                                                   
***********************************************************************         
SETFI    NTR1  ,                                                                
*                                                                               
         SAM24                                                                  
         XC    KEY,KEY             GET WRKF CI INFORMATION                      
         MVC   FIWCIA,=X'00010100'                                              
         GOTO1 =V(DATAMGR),DMCB,BUFFER,FIWRES,KEY,FIWCIA,ACIREC                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ACIREC                                                        
         MVC   CIDATA,12(R1)                                                    
         SAM31                                                                  
*                                                                               
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
         USING W_RECD,R3                                                        
         CLI   W_STAT,0            IGNORE PURGED ENTRIES                        
         BNE   CP050                                                            
         DROP  R3                                                               
*                                                                               
         L     R0,CNTP1AV          COUNT TOTAL AVAILABLE PART2S                 
         AHI   R0,1                                                             
         ST    R0,CNTP1AV                                                       
         B     CP060                                                            
*                                                                               
CP050    MVC   FIWNDA,ACURPAR1                                                  
         BRAS  RE,FIRNC                                                         
         MVI   BYTE,1                                                           
         CLI   SHORTR,C'Y'                                                      
         BNE   CP054                                                            
         AP    TEMPC,=P'1'                                                      
         CP    TEMPC,=P'100'                                                    
         BH    CP056                                                            
         BL    CP054                                                            
         MVC   PLINE(3),=C'...'                                                 
         BRAS  RE,PRINTL                                                        
         B     CP056                                                            
CP054    BRAS  RE,PRINTNDX         PRINT THE INDEX                              
*                                                                               
CP056    CLI   LOCKIT,C'N'                                                      
         BE    *+8                                                              
         BRAS  RE,FIRRLOCK                                                      
*                                                                               
         OC    SI1NXT,SI1NXT                                                    
         BZ    CP058                                                            
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
         CLC   CNTP1AV,SITP1AV                                                  
         BE    CPX                                                              
         MVC   PLINE(40),=C'**ERROR** PART1S AVAILABLE MISMATCH'                
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
GP056    OC    SI2NXT,SI2NXT                                                    
         BZ    GPX                                                              
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
LIST020  XC    FIWKEY,FIWKEY                                                    
         L     R8,FIWSHA                                                        
         L     R9,FIWRHA                                                        
*                                                                               
LIST030  BRAS  RE,FIRNTN           GET NEXT                                     
         BNE   LIST040                                                          
*                                                                               
         L     R3,FIWNDA           SET RETURN NODE                              
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
LIST038  MVC   FIWKEY,BIGWORK      SEARCH FOR NEXT                              
                                                                                
         L     R1,CNTTAB                                                        
         AHI   R1,1                                                             
         ST    R1,CNTTAB           COUNT NUMBER OF INDEXES                      
*                                                                               
         B     LIST030                                                          
         DROP  R3                                                               
*                                                                               
*                                                                               
LIST040  BRAS  RE,PRINTL                                                        
         MVC   PLINE(33),=CL33'--- END OF LIST --- # OF ENTRIES='               
         EDIT  (B4,CNTTAB),(8,PLINE+33)                                         
         BRAS  RE,PRINTL                                                        
*                                                                               
         LA    R4,L'FILES(,R4)                                                  
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
* PART2 AVAILABLE QUEUE                                                         
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
         ICM   R1,15,CNTTAB                                                     
         BNZ   *+8                                                              
         BRAS  RE,PRINTSLT                                                      
*                                                                               
         CLI   CNTTAB+3,1                                                       
         BNE   AEQ040                                                           
         MVC   PLINE+16(3),=C'...'                                              
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   FULL,SI1NUM                                                      
         BRAS  RE,PRINTSLT                                                      
*                                                                               
AEQ040   L     R1,CNTTAB                                                        
         AHI   R1,1                                                             
         ST    R1,CNTTAB                                                        
*                                                                               
         C     R1,SIT1CIC                                                       
         BNH   AEQ034                                                           
         MVC   PLINE(40),=C'**ERROR** PART1 AVAILABLE QUEUE LOOPING'            
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
*                                                                               
AEQ034   ICM   R3,15,SI1NAV                                                     
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
         ICM   R1,15,CNTTAB                                                     
         BNZ   *+8                                                              
         BRAS  RE,PRINTSLT                                                      
*                                                                               
         CLI   CNTTAB+3,1                                                       
         BNE   AEQ082                                                           
         MVC   PLINE+16(3),=C'...'                                              
         BRAS  RE,PRINTL                                                        
         MVC   FULL,SI2NUM                                                      
         BRAS  RE,PRINTSLT                                                      
*                                                                               
AEQ082   L     R1,CNTTAB                                                        
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
         SAM24                                                                  
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
         BNE   PNDX020                                                          
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CTIDATA                                                       
PNDX010  CLI   0(R3),0                                                          
         BE    PNDX020                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    PNDX012                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PNDX010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
PNDX012  MVC   PLUSER,CTDSC                                                     
         B     PNDX022                                                          
         DROP  R2,R3                                                            
                                                                                
PNDX020  GOTO1 VHEXOUT,DMCB,W_USRID,PLUSER,L'W_USRID                            
PNDX022  MVC   PLSYSP,W_SYSPRG                                                  
         GOTO1 VHEXOUT,DMCB,W_DAY,PLDAY,L'W_DAY                                 
         MVC   PLCLASS,W_CLASS                                                  
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),W_FILENO                                               
         EDIT  (B4,FULL),(5,PLREFNO)                                            
         GOTO1 VHEXOUT,DMCB,W_TYPE,PLTYPE,L'W_TYPE                              
         GOTO1 VHEXOUT,DMCB,W_ATTB,PLATTB,L'W_ATTB                              
         GOTO1 VHEXOUT,DMCB,W_STAT,PLSTAT,L'W_STAT                              
         EDIT  (B1,W_SEQ),(3,PLSEQ)                                             
         GOTO1 VDATCON,DMCB,(2,W_AGELD),(21,PLAGE)                              
*                                                                               
         GOTO1 VDATCON,DMCB,(2,W_AGERD),(21,PLAGER)                             
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
         BE    PNDXX                                                            
*                                                                               
         L     R1,ACURPAR1                                                      
         L     R1,SI1NUM-SI1PAR(R1)                                             
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(5,PLSLOT)                                             
*                                                                               
         L     R1,ACURPAR1                                                      
         ICM   R1,15,SI1NXT-SI1PAR(R1)                                          
         BZ    PNDXX                                                            
         A     R1,FIWSHA                                                        
         L     R1,SI2NUM-SI2PAR(R1)                                             
         ST    R1,FULL                                                          
         EDIT  (B4,FULL),(5,PLNSLOT)                                            
*                                                                               
PNDXX    BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
* PRINT THE INDEX FOR LIST                                                      
***********************************************************************         
PNDXLIST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SAM24                                                                  
         LA    R4,TEMP                                                          
         USING W_RECD,R4                                                        
         MVC   TEMP,BIGWORK                                                     
         CLC   SVUSER,W_USRID                                                   
         BE    PILI024                                                          
         MVC   SVUSER,W_USRID                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,W_USRID                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,IOKEY,AIOAREA                        
         BNE   PILI020                                                          
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CTIDATA                                                       
PILI010  CLI   0(R3),0                                                          
         BE    PILI020                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    PILI012                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PILI010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
PILI012  MVC   PLINE+1(8),CTDSC                                                 
         B     PILI022                                                          
         DROP  R2,R3                                                            
*                                                                               
PILI020  MVI   PLINE+1,C'?'                                                     
*                                                                               
PILI022  BAS   RE,PRINTL                                                        
*                                                                               
PILI024  MVI   PTNUSOP,C'('                                                     
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
*                                                                               
         SAM31                                                                  
         L     R2,FIWNDA                                                        
I        USING SI1PARD,R2                                                       
N        USING SI1PARD,R1                                                       
*                                                                               
         ICM   R1,15,I.SI1PTP                                                   
         BZ    PILI060                                                          
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1KEY                                                    
         SAM24                                                                  
         MVC   PTPSYSP,W_SYSPRG                                                 
         GOTO1 VHEXOUT,DMCB,W_DAY,PTPDAY,L'W_DAY                                
         MVC   PTPCLASS,W_CLASS                                                 
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),W_FILENO                                               
         EDIT  (B4,FULL),(5,PTPREFNO)                                           
         SAM31                                                                  
*                                                                               
PILI060  ICM   R1,15,I.SI1LTP                                                   
         BZ    PILI070                                                          
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1KEY                                                    
         SAM24                                                                  
         MVC   PTLSYSP,W_SYSPRG                                                 
         GOTO1 VHEXOUT,DMCB,W_DAY,PTLDAY,L'W_DAY                                
         MVC   PTLCLASS,W_CLASS                                                 
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),W_FILENO                                               
         EDIT  (B4,FULL),(5,PTLREFNO)                                           
         SAM31                                                                  
*                                                                               
PILI070  ICM   R1,15,I.SI1RTP                                                   
         BZ    PILIX                                                            
         A     R1,FIWSHA                                                        
         MVC   TEMP,N.SI1KEY                                                    
         SAM24                                                                  
         MVC   PTRSYSP,W_SYSPRG                                                 
         GOTO1 VHEXOUT,DMCB,W_DAY,PTRDAY,L'W_DAY                                
         MVC   PTRCLASS,W_CLASS                                                 
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),W_FILENO                                               
         EDIT  (B4,FULL),(5,PTRREFNO)                                           
*                                                                               
         DROP  I,N                                                              
*                                                                               
PILIX    BRAS  RE,PRINTL                                                        
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
         EJECT                                                                  
                                                                                
***********************************************************************         
* DROP THESE DSECTS, THEY ARE USED IN THE SFI ROUTINES                          
***********************************************************************         
         DROP  R8,R9                                                            
                                                                                
***********************************************************************         
* SFI ROUTINES                                                                  
***********************************************************************         
       ++INCLUDE SHFIR                                                          
                                                                                
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
PRINTTI  MVC   PLUSER(4),=C'COID'                                               
         MVC   PLSYSP(4),=C'FILE'                                               
         MVC   PLREFNO(4),=C'REF#'                                              
         MVC   PLTYPE,=C'TY'                                                    
         MVC   PLATTB,=C'AT'                                                    
         MVC   PLSTAT,=C'ST'                                                    
         MVC   PLSEQ,=C'SQ#'                                                    
         MVC   PLAGE(4),=C'DATE'                                                
         MVC   PLCI(2),=C'CI'                                                   
         MVC   PLPREV(3),=C'PCI'                                                
         MVC   PLNEXT(3),=C'NCI'                                                
         MVC   PLSLOT(4),=C'SLOT'                                               
         MVC   PLPSLOT(2),=C'PS'                                                
         MVC   PLNSLOT(2),=C'NS'                                                
         MVC   PLAGER(5),=C'XDATE'                                              
         MVC   PLAGERT(5),=C'XTIME'                                             
         MVC   TITLE,PLINE                                                      
*                                                                               
         MVC   PLINE,SPACES                                                     
         MVC   PLUSER,DASHES                                                    
         MVC   PLSYSP(7),DASHES                                                 
         MVC   PLREFNO,DASHES                                                   
         MVC   PLTYPE,DASHES                                                    
         MVC   PLATTB,DASHES                                                    
         MVC   PLSTAT,DASHES                                                    
         MVC   PLSEQ,DASHES                                                     
         MVC   PLAGE,DASHES                                                     
         MVC   PLCI,DASHES                                                      
         MVC   PLPREV,DASHES                                                    
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
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,SPACES                                                  
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         PUT   SYSPRINT,TITLE2     PRINT TITLE2                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
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
         EJECT                                                                  
                                                                                
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
EXITH    CHI   RB,0                                                             
         B     EXIT                                                             
EXIT     XIT1  ,                                                                
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
         EJECT                                                                  
                                                                                
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
CONTROL  DC    CL8'CONTROL'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
SHMUSS   DC    CL8'SHMUSS'                                                      
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
         DC    X'0000'                                                          
*                                                                               
* CARD OUTPUT AREAS SET WITH DEFAULTS                                           
*                                                                               
DDSIO    DC    CL10'DDSIO'                                                      
MODE     DC    CL10'CHECK'          CHECK                                       
MEMORY   DC    CL8' '                                                           
LOCKIT   DC    CL3'YES'                                                         
SHORTR   DC    CL3'NO '                                                         
FILES    DC    (MAXFILES+1)CL7' '                                               
*                                                                               
         LTORG                                                                  
*                                                                               
MAXFILES EQU   16                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* DCBS & ADCONS                                                                 
***********************************************************************         
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VDATCON  DC    V(DATCON)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
*                                                                               
ACOMM    DC    A(0)                                                             
*                                                                               
UTL      DC    F'0',AL1(10),XL250'00'                                           
SSB      DC    H'0',X'FF',X'14',1022X'00'                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* WORKING STORAGE DC                                                            
***********************************************************************         
         DS    0D                                                               
WORKAREA DC    60000D'00'                                                       
         EJECT                                                                  
                                                                                
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
SVUSER   DS    XL2                                                              
*                                                                               
MSGW     DS    0CL64               WTO OUTPUT MESSAGE                           
MSGWMSG  DS    CL40                WTO INFORMATION MESSAGE                      
MSGWDET  DS    CL24                WTO INFORMATION DETAIL                       
*                                                                               
       ++INCLUDE SHFIW           SHARED MEMORY FILE QUEUE WORK AREA             
*                                                                               
       ++INCLUDE DMWRKFW                                                        
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
PLDAY    DS    CL2                                                              
PLCLASS  DS    CL1                                                              
         DS    CL1                                                              
PLREFNO  DS    CL5                                                              
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
         EJECT                                                                  
                                                                                
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
         EJECT                                                                  
                                                                                
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
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------          
* TREE LISTING LINE                                                             
*---------------------------------------------------------------------          
         ORG   PLUSER                                                           
PTNUSOP  DS    CL1                                                              
PTNUSNO  DS    CL5                                                              
PTNUSCP  DS    CL1                                                              
         DS    CL1                                                              
PTNSYSP  DS    CL4                                                              
PTNDAY   DS    CL2                                                              
PTNCLASS DS    CL1                                                              
PTNREFNO DS    CL5                                                              
         DS    CL2                                                              
PTPARENT DS    CL7                 C'PARENT='                                   
PTPSYSP  DS    CL4                                                              
PTPDAY   DS    CL2                                                              
PTPCLASS DS    CL1                                                              
PTPREFNO DS    CL5                                                              
         DS    CL1                                                              
PTLEFT   DS    CL5                 C'LEFT='                                     
PTLSYSP  DS    CL4                                                              
PTLDAY   DS    CL2                                                              
PTLCLASS DS    CL1                                                              
PTLREFNO DS    CL5                                                              
         DS    CL1                                                              
PTRIGHT  DS    CL6                 C'RIGHT='                                    
PTRSYSP  DS    CL4                                                              
PTRDAY   DS    CL2                                                              
PTRCLASS DS    CL1                                                              
PTRREFNO DS    CL5                                                              
         EJECT                                                                  
                                                                                
**********************************************************************          
* OTHER DSECTS                                                                  
**********************************************************************          
*                                                                               
* SHFID                                                                         
       ++INCLUDE SHFID                                                          
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
* DMWRKFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFD                                                        
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
**PAN#1  DC    CL21'002SHFICHK   07/31/12'                                      
         END                                                                    
