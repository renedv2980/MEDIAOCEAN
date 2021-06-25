*          DATA SET ACREPOB02  AT LEVEL 051 AS OF 05/19/14                      
*PHASE ACOB02A                                                                  
ACOB02   TITLE 'FIX OFFICE/ACCOUNT BALANCES'                                    
ACOB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**OB02**                                                       
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
         L     R9,ADBXAREA                                                      
         USING BOXD,R9                                                          
         LA    R7,P                                                             
         USING PLID,R7                                                          
         MVI   FCRESET,C'Y'                                                     
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RUN                                                       *         
***********************************************************************         
                                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   OB000                                                            
         L     RF,GETOPT                                                        
         MVC   0(2,RF),=X'07FE'                                                 
         MVI   FCPRORAT,C'N'                                                    
         MVI   FCSUPOFC,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR REQUEST                                                   *         
***********************************************************************         
                                                                                
OB000    CLI   MODE,REQFRST                                                     
         BNE   OB100                                                            
         XC    RMODE,RMODE                                                      
         CLI   RCWRITE,C'Y'                                                     
         BE    *+8                                                              
         OI    RMODE,RWRITX                   SET WRITE=NO                      
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LH    RE,=Y(IO-WORKD)                                                  
         LA    RE,WORKD(RE)                                                     
         ST    RE,AIO                                                           
         LH    RE,=Y(OFFTAB-WORKD)                                              
         LA    RE,WORKD(RE)                                                     
         ST    RE,AOFFTAB                                                       
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+(PLICOLL-PLINED),C'L'                                    
         MVI   BOXCOLS+(PLICOL1-PLINED),C'C'                                    
         MVI   BOXCOLS+(PLICOL2-PLINED),C'C'                                    
         MVI   BOXCOLS+(PLICOL3-PLINED),C'C'                                    
         MVI   BOXCOLS+(PLICOL4-PLINED),C'C'                                    
         MVI   BOXCOLS+(PLICOL5-PLINED),C'C'                                    
         MVI   BOXCOLS+(PLICOL6-PLINED),C'C'                                    
         MVI   BOXCOLS+(PLICOL7-PLINED),C'C'                                    
         MVI   BOXCOLS+(PLICOLR-PLINED),C'R'                                    
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
*                                                                               
         MVI   FCRDACC,C'Y'                                                     
         L     R4,ADCMPEL                                                       
         USING CPYELD,R4                                                        
         TM    CPYSTAT4,CPYSOFF2   TEST 2 CHARACTER OFFICE                      
         BO    EXIT                                                             
         MVI   FCRDACC,C'N'                                                     
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,REQFRST     SET FOR NEXT COMPANY                         
         B     EXIT                                                             
         DROP  R4,RF                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LEDGER                                                    *         
***********************************************************************         
                                                                                
OB100    CLI   MODE,LEDGFRST                                                    
         BNE   OB200                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
                                                                                
OB200    CLI   MODE,PROCACC                                                     
         BNE   OB400                                                            
                                                                                
         XC    OFFTABN,OFFTABN                                                  
*                                                                               
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         CLC   =C'SJ',ACTKUNT                                                   
         BE    SKIP                                                             
         CLC   =C'1J',ACTKUNT                                                   
         BE    SKIP                                                             
         CLC   =C'1R',ACTKUNT                                                   
         BE    SKIP                                                             
         XC    BLFRWD,BLFRWD       GET DATE BALANCE FRWD                        
         ICM   R4,15,ADACCSTA                                                   
         BZ    *+10                                                             
         USING RSTELD,R4           RECORD STATUS ELEMENT                        
         MVC   BLFRWD,RSTBDATE     DATE BALANCE BROUGHT FORWARD                 
         DROP  R4                                                               
*                                                                               
         MVC   ACT,ACTKEY                                                       
         MVI   ACTKEND(R2),X'41'                                                
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,ACTRECD,AIO                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,IOTRACE                                                       
         L     R3,AIO                                                           
         USING TRNRECD,R3                                                       
*                                                                               
OB202    GOTO1 DATAMGR,DMCB,DMRSEQ,ACCFIL,ACTRECD,AIO                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   QOPT1,C'D'          DETAIL TRACE                                 
         BNE   *+8                                                              
         BAS   RE,IOTRACE                                                       
         CLC   TRNKEY(ACTKEND),ACT                                              
         BNE   OB210                                                            
         CLC   TRNKOFF,SPACES      Do we have an office in the key?             
         BNH   OB202A                                                           
         CLC   QOFFICE,SPACES      Do we have an office in QOFFICE?             
         BNH   OB202A                                                           
         CLC   TRNKOFF,QOFFICE                                                  
         BNE   OB202                                                            
OB202A   LA    R5,TRNRECD+ACCORFST                                              
         USING TRNELD,R5                                                        
         CLI   TRNEL,X'43'         ADD OFFICE ENTRY FOR CONTRA                  
         BNE   *+8                                                              
         BAS   RE,SETOFF                                                        
         CLI   TRNEL,TRNELQ                                                     
         BNE   OB202                                                            
         OC    TRNRECD+ACCOPEEL(ACCOPLEN),TRNRECD+ACCOPEEL                      
         BNZ   OB202                                                            
         TM    TRNRECD+ACCOSTAT,TRNSDRFT                                        
         BNZ   OB202                                                            
****     CP    TRNAMNT,=P'0'                                                    
****     BE    OB202                                                            
         CLI   QOPT1,C'D'          DETAIL TRACE IO                              
         BNE   OB203                                                            
         MVC   PLIACTN,FORMPST                                                  
         LA    R4,PLIDR                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    R4,PLICR                                                         
         CURED TRNAMNT,(L'PLIDR,(R4)),2,MINUS=YES                               
         GOTO1 ACREPORT                                                         
*                                                                               
OB203    L     R4,AOFFTAB                                                       
         USING OFFTABD,R4                                                       
         SR    RF,RF                                                            
         ICM   RF,3,OFFTABN                                                     
         BZ    OB205                                                            
*                                                                               
OB204    CLC   OFFTOFF,TRNOFFC                                                  
         BE    OB206                                                            
         LA    R4,OFFTABL(R4)                                                   
         BCT   RF,OB204                                                         
*                                                                               
OB205    ICM   RF,3,OFFTABN                                                     
         LA    RF,1(RF)                                                         
         CLM   RF,3,=Y(OFFTMAXN)                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         STCM  RF,3,OFFTABN                                                     
         MVC   OFFTOFF,TRNOFFC                                                  
         MVC   OFFTMOSL,EFFS                                                    
         XC    OFFTMOSH,OFFTMOSH                                                
         XC    OFFTMOSC,OFFTMOSC                                                
         ZAP   OFFTDR,PZERO                                                     
         ZAP   OFFTCR,PZERO                                                     
*                                                                               
OB206    LA    RF,OFFTDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    RF,OFFTCR                                                        
         AP    0(L'OFFTDR,RF),TRNAMNT                                           
*                                                                               
         USING TRSELD,R5                                                        
         SR    R0,R0                                                            
OB208    IC    R0,TRSLN                                                         
         AR    R5,R0                                                            
         CLI   TRSEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TRSEL,TRSELQ                                                     
         BNE   OB208                                                            
         CLC   TRSPMOS,OFFTMOSL                                                 
         BH    *+10                                                             
         MVC   OFFTMOSL,TRSPMOS                                                 
         CLC   TRSPMOS,OFFTMOSH                                                 
         BNH   *+10                                                             
         MVC   OFFTMOSH,TRSPMOS                                                 
         B     OB202                                                            
*                                                                               
OB210    SR    R0,R0                                                            
         ICM   R0,3,OFFTABN                                                     
         BZ    EXIT                                                             
         L     R4,AOFFTAB                                                       
*                                                                               
         USING OFARECD,R2                                                       
OB212    LA    R2,DIO                                                           
         MVC   OFAKEY,SPACES                                                    
         MVC   OFAKCULA,ACT                                                     
         MVC   OFAKOFF,OFFTOFF                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,OFARECD,OFARECD                       
         BE    OB214                                                            
         TM    8(R1),X'90'                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO                                                           
         XC    OFARECD(256),OFARECD                                             
         MVC   OFAKEY,SPACES                                                    
         MVC   OFAKCULA,ACT                                                     
         MVC   OFAKOFF,OFFTOFF                                                  
         MVC   OFARLMOS,OFFTMOSL                                                
         MVC   OFARHMOS,OFFTMOSH                                                
         LA    R1,OFARFST                                                       
         USING ABLELD,R1                                                        
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,PZERO                                                    
         ZAP   ABLDR,OFFTDR                                                     
         ZAP   ABLCR,OFFTCR                                                     
         XC    ABLTXS,ABLTXS                                                    
         LA    R1,ABLLN3Q(R1)                                                   
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         LA    RE,OFARECD                                                       
         SR    R1,RE                                                            
         STCM  R1,3,OFARLEN                                                     
         GOTO1 FORMAT,FORMNEW                                                   
         GOTO1 DATAMGR,DMCB,DMADDREC,ACCMST,DMDA,OFARECD,DMWORK                 
         ORG   *-2                                                              
         BAS   RE,IOTRACE                                                       
         TM    RMODE,RWRITX                                                     
         BNZ   OB230                                                            
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     OB230                                                            
*****    B     OB226      the bug is when this went to ob226                    
         DROP  R1                                                               
*                                                                               
OB214    MVC   DMDA,OFAKDA                                                      
         L     R2,AIO                                                           
         GOTO1 DATAMGR,DMCB,DMGETREC,ACCMST,DMDA,OFARECD,DMWORK                 
         BE    *+6                                                              
         DC    H'0'                                                             
******* TEMP ********************                                               
*****    LA    RF,DIO                                                           
****     MVC   OFARLMOS(6),OFAKLMOS-OFARECD(RF)                                 
******* TEMP ********************                                               
         MVI   UPSW,0                                                           
*                                                                               
         LA    R3,OFARFST                                                       
         USING ABLELD,R3                                                        
OB215    CLI   ABLEL,ABLELQ                                                     
         BE    OB216                                                            
         ZIC   R6,1(R3)                                                         
         AR    R3,R6                                                            
         CLI   0(R3),0                                                          
         BNE   OB215                                                            
         DC    H'0'                                                             
OB216    CP    ABLDR,OFFTDR                                                     
         BE    *+8                                                              
         OI    UPSW,UPDR                                                        
         CP    ABLCR,OFFTCR                                                     
         BE    *+8                                                              
         OI    UPSW,UPCR                                                        
         CP    ABLFRWD,=P'0'                                                    
         BE    OB217                                                            
         MVC   WORK(2),OFFTMOSL                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)                                  
         GOTO1 ADDAY,DMCB,WORK+3,WORK+9,F'-1'                                   
         GOTO1 DATCON,DMCB,(0,WORK+9),(1,WORK+15)                               
         MVC   OFFTMOSC,WORK+15                                                 
         CLC   OFFTMOSC,BLFRWD     TEST CLOSE > BAL FRWD                        
         BNH   *+10                                                             
         MVC   OFFTMOSC,BLFRWD     USE EARLIER DATE                             
*                                                                               
         XC    OFFTMOSC,OFFTMOSC   REMOVE CLOSED MOA                            
*                                                                               
OB217    CLC   OFARCMOS,OFFTMOSC                                                
         BE    *+8                                                              
         OI    UPSW,UPMOC                                                       
         CLC   OFARLMOS,OFFTMOSL                                                
         BE    *+8                                                              
         OI    UPSW,UPMOL                                                       
         CLC   OFARHMOS,OFFTMOSH                                                
         BE    *+8                                                              
         OI    UPSW,UPMOH                                                       
         CLI   UPSW,0              TEST ANY CHANGES                             
         BE    OB230                                                            
         TM    UPSW,UPDR+UPCR      TEST CASH  DIFFERENCE                        
         BNZ   OB225                                                            
         CLI   QOPT2,C'N'          OPTION TO IGNORE ALL MOS                     
         BE    OB230                                                            
         CLI   QOPT2,C'C'          OPTION TO IGNORE LOW/HIGH                    
         BNE   *+8                                                              
         NI    UPSW,X'FF'-(UPMOL+UPMOH)                                         
         CLI   UPSW,0                                                           
         BE    OB230                                                            
*                                                                               
OB225    GOTO1 FORMAT,FORMBEF                                                   
         MVC   OFARLMOS,OFFTMOSL                                                
         MVC   OFARHMOS,OFFTMOSH                                                
         MVC   OFARCMOS,OFFTMOSC                                                
*                                                                               
         ZAP   DUB,OFFTDR          GET DIFFERENCE IN DUB                        
         SP    DUB,ABLDR                                                        
         SP    DUB,OFFTCR                                                       
         AP    DUB,ABLCR                                                        
         CLI   QOPT3,C'B'          TEST ADJUST BBF                              
         BNE   *+10                                                             
         SP    ABLFRWD,DUB                                                      
*                                                                               
         ZAP   ABLDR,OFFTDR                                                     
         ZAP   ABLCR,OFFTCR                                                     
OB225B   GOTO1 FORMAT,FORMAFT                                                   
         GOTO1 DATAMGR,DMCB,DMPUTREC,ACCMST,DMDA,OFARECD,DMWORK                 
         ORG   *-2                                                              
*                                                                               
OB226    BAS   RE,IOTRACE                                                       
         TM    RMODE,RWRITX                                                     
         BNZ   OB230                                                            
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,DIO                                                           
         MVC   OFAKLMOS,OFFTMOSL                                                
         MVC   OFAKHMOS,OFFTMOSH                                                
         MVC   OFAKCMOS,OFFTMOSC                                                
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,OFARECD,OFARECD                        
*                                                                               
OB230    LA    R4,OFFTABL(R4)                                                   
         BCT   R0,OB212                                                         
         MVI   BOXREQ,C'B'                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET OFFICE ENTRY                                                    *         
***********************************************************************         
                                                                                
SETOFF   L     R3,AIO                                                           
         USING TRNRECD,R3                                                       
         L     R4,AOFFTAB                                                       
         USING OFFTABD,R4                                                       
         SR    RF,RF                                                            
         ICM   RF,3,OFFTABN                                                     
         BZ    SETOFF5                                                          
*                                                                               
SETOFF3  CLC   OFFTOFF,TRNKOFF                                                  
         BER   RE                                                               
         LA    R4,OFFTABL(R4)                                                   
         BCT   RF,SETOFF3                                                       
*                                                                               
SETOFF5  ICM   RF,3,OFFTABN                                                     
         LA    RF,1(RF)                                                         
         CLM   RF,3,=Y(OFFTMAXN)                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         STCM  RF,3,OFFTABN                                                     
         MVC   OFFTOFF,TRNKOFF                                                  
         MVC   OFFTMOSL,EFFS                                                    
         XC    OFFTMOSH,OFFTMOSH                                                
         XC    OFFTMOSC,OFFTMOSC                                                
         ZAP   OFFTDR,PZERO                                                     
         ZAP   OFFTCR,PZERO                                                     
         BR     RE                                                              
         EJECT                                                                  
***********************************************************************         
* LAST FOR REQUEST                                                    *         
***********************************************************************         
                                                                                
OB400    CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SKIP THIS LEDGER                                                    *         
***********************************************************************         
         SPACE 1                                                                
SKIP     MVI   FCRESET,C'N'        SKIP TO END OF LEDGER                        
         L     R2,ADACC                                                         
         L     R3,AIO                                                           
         MVC   0(42,R3),0(R2)                                                   
         MVI   3(R3),X'FF'                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR',(R3),(R3)                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT A PRINT LINE                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING OFARECD,R2                                                       
FORMAT   NTR1  ,                                                                
         STC   R1,DUB                                                           
         MVC   PLIACT,OFAKULA                                                   
         MVC   PLIOFF,OFAKOFF                                                   
         SR    RF,RF                                                            
         IC    RF,DUB                                                           
         MH    RF,=Y(L'FORMACTN)                                                
         LA    RF,FORMACTN-L'FORMACTN(RF)                                       
         MVC   PLIACTN,0(RF)                                                    
         LA    R3,OFARLMOS         FORMAT LOW MOA                               
         LA    R4,PLIMOSL                                                       
         BAS   RE,DATES                                                         
         LA    R3,OFARHMOS         FORMAT HIGH MOA                              
         LA    R4,PLIMOSH                                                       
         BAS   RE,DATES                                                         
         LA    R3,OFARCMOS         FORMAT CLOSE MOA                             
         LA    R4,PLIMOSC                                                       
         BAS   RE,DATES                                                         
FORMAT04 LA    R3,OFARFST                                                       
         USING ABLELD,R3                                                        
FORMAT05 CLI   0(R3),ABLELQ                                                     
         BE    FORMAT06                                                         
         ZIC   R6,1(R3)                                                         
         AR    R3,R6                                                            
         CLI   0(R3),0                                                          
         BNE   FORMAT05                                                         
         DC    H'0'                                                             
FORMAT06 CURED ABLDR,(L'PLIDR,PLIDR),2,MINUS=YES                                
         CURED ABLCR,(L'PLICR,PLICR),2,MINUS=YES                                
         GOTO1 ACREPORT                                                         
FORMATX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE THE DATES                                         *         
*  R3 = INPUT DATE                                                    *         
*  R4 = OUTPUT AREA                                                   *         
***********************************************************************         
         SPACE 1                                                                
DATES    LR    R0,RE                                                            
         OC    0(2,R3),0(R3)       TEST ANY DATE                                
         BZR   RE                                                               
         CLI   0(R3),X'C7'         YEAR > MAX YEAR (2027)                       
         BH    DATES3              YES, DISPLAY DATE IN HEX                     
         MVC   WORK(1),0(R3)       GET  YEAR                                    
         NI    WORK,X'0F'          USE  ONLY YEAR WITHIN DECADE                 
         CLI   WORK,X'09'          YEAR > 9                                     
         BH    DATES3              YES, DISPLAY DATE IN HEX                     
         CLI   1(R3),X'01'                                                      
         BL    DATES3                                                           
         CLI   1(R3),X'09'                                                      
         BNH   DATES1                                                           
         CLI   1(R3),X'12'                                                      
         BH    DATES3                                                           
         CLI   1(R3),X'10'                                                      
         BL    DATES3                                                           
DATES1   MVC   WORK(2),0(R3)                                                    
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,0(R4))                                   
         B     DATESX                                                           
*                                                                               
DATES3   GOTO1 HEXOUT,DMCB,(R3),(R4),2,0,0                                      
DATESX   LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT I/O TRACE                                          *         
***********************************************************************         
         SPACE 1                                                                
IOTRACE  CLI   QOPT1,C' '          TEST TRACED REQUEST                          
         BER   RE                                                               
IOTRACE2 NTR1  ,                                                                
         LM    RE,RF,0(R1)                                                      
         MVC   P+1(6),0(RE)                                                     
         L     R1,12(R1)           R1=A(INPUT RECORD)                           
         LA    R0,OFARFST-OFARECD                                               
         CLC   ACCMST,0(RF)                                                     
         BE    *+8                                                              
         LA    R0,ACCORFST                                                      
         BAS   RE,IOHEX            PRINT RECORD KEY                             
*                                                                               
         AR    R1,R0                                                            
         SR    R0,R0                                                            
IOTRACE4 IC    R0,1(R1)                                                         
         BAS   RE,IOHEX            PRINT RECORD ELEMENTS                        
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   IOTRACE4                                                         
*                                                                               
IOTRACEX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT A CHUNK IN CHARACTER AND HEX FORMAT                *         
*                                                                     *         
* NTRY - R0=LENGTH OF CHUNK TO BE PRINTED                             *         
*        R1=A(CHUNK)                                                  *         
***********************************************************************         
         SPACE 1                                                                
IOHEX    NTR1  ,                                                                
         CLI   LINE,57             TEST LINE COUNT EXCEEDED                     
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         LR    R2,R1               R2=A(CHUNK TO BE PRINTED)                    
         LR    R3,R0               R0=LENGTH OF CHUNK                           
         BCTR  R3,0                R3=LENGTH OF CHUNK-1                         
         LA    R4,P+12                                                          
         EX    R3,IOHEXMVC                                                      
         LA    R4,L'P(R4)                                                       
         GOTO1 HEXOUT,DMCB,(R2),TEMP,(R0),=C'SEP'                               
         LA    R2,TEMP                                                          
         EX    R3,IOHEXMVC                                                      
         LA    R4,L'P(R4)                                                       
         AR    R2,R0                                                            
         EX    R3,IOHEXMVC                                                      
         LA    R4,L'P(R4)                                                       
         MVI   0(R4),0                                                          
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
IOHEXMVC MVC   0(0,R4),0(R2)                                                    
         EJECT                                                                  
         LTORG                                                                  
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
ACCFIL   DC    C'ACCOUNT'                                                       
DMGETREC DC    C'GETREC '                                                       
DMADDREC DC    C'ADDREC '                                                       
DMPUTREC DC    C'PUTREC '                                                       
PZERO    DC    P'0'                                                             
EFFS     DC    X'FFFFFFFFFFFFFFFF'                                              
FORMNEW  EQU   1                                                                
FORMBEF  EQU   2                                                                
FORMAFT  EQU   3                                                                
         SPACE 1                                                                
FORMPST  DC    CL(L'PLIACTN)'Posting'                                           
FORMACTN DS    0CL(L'PLIACTN)                                                   
         DC    CL(L'PLIACTN)'Add'                                               
         DC    CL(L'PLIACTN)'Copy'                                              
         DC    CL(L'PLIACTN)'Change'                                            
         EJECT                                                                  
WORKD    DSECT                                                                  
AIO      DS    A                                                                
AOFFTAB  DS    A                                                                
OFFTABN  DS    H                                                                
RMODE    DS    XL1                                                              
RWRITX   EQU   X'80'                                                            
ACT      DS    XL(ACTKEND)         ACCOUNT CODE                                 
TEMP     DS    XL256                                                            
DIO      DS    XL64                                                             
*                                                                               
UPSW     DS    X                                                                
UPDR     EQU   X'80'                                                            
UPCR     EQU   X'40'                                                            
UPMOL    EQU   X'08'                                                            
UPMOH    EQU   X'04'                                                            
UPMOC    EQU   X'02'                                                            
BLFRWD   DS    XL3                                                              
*                                                                               
DMDA     DS    XL4                                                              
IO       DS    XL2000                                                           
OFFTAB   DS    (OFFTMAXN)XL(OFFTABL),X                                          
         EJECT                                                                  
OFFTABD  DSECT                     ** DSECT TO COVER OFFICE TABLE **            
OFFTOFF  DS    CL2                                                              
OFFTMOSL DS    PL2                                                              
OFFTMOSH DS    PL2                                                              
OFFTMOSC DS    PL2                                                              
OFFTDR   DS    PL8                                                              
OFFTCR   DS    PL8                                                              
OFFTABL  EQU   *-OFFTABD                                                        
OFFTMAXN EQU   256                                                              
         EJECT                                                                  
PLID     DSECT                                                                  
PLINED   DS    0CL(L'P)                                                         
PLICOLL  DS    CL1                                                              
PLIACT   DS    CL14                                                             
PLICOL1  DS    CL1                                                              
PLIOFF   DS    CL2                                                              
PLICOL2  DS    CL1                                                              
PLIACTN  DS    CL8                                                              
PLICOL3  DS    CL1                                                              
PLIMOSL  DS    CL6                                                              
PLICOL4  DS    CL1                                                              
PLIMOSH  DS    CL6                                                              
PLICOL5  DS    CL1                                                              
PLIMOSC  DS    CL6                                                              
PLICOL6  DS    CL1                                                              
PLIDR    DS    CL14                                                             
PLICOL7  DS    CL1                                                              
PLICR    DS    CL14                                                             
PLICOLR  DS    CL1                                                              
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051ACREPOB02 05/19/14'                                      
         END                                                                    
