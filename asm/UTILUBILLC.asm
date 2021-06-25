*          DATA SET UTILUBILLC AT LEVEL 254 AS OF 10/16/03                      
*PHASE T32070C                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRTREC                                                                 
*INCLUDE MOBILE                                                                 
*INCLUDE GETBROAD                                                               
         TITLE 'T32070 - UTILITY PROGRAM'                                       
                                                                                
*******************************************************************             
*  OPTIONS:                                                                     
*  NUBIL       - OPTTYPE=X'80' BUILD NEW 'OE0A' RECORDS                         
*                                                                               
*******************************************************************             
T32070   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEUT**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            ANETWS1=CLIENT RECORD                        
         USING NETSYSD,R9                                                       
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R7,ANETWS2          ANETWS2+300=WORKING STORAGE                  
         A     R7,=F'300'                                                       
         USING WORKD,R7                                                         
* - NEED TO HAVE COPIES WRITTEN TO RECOVERY FILE                                
         ICM   R1,15,TWAMASTC                                                   
         BZ    ENDMST                                                           
         USING MCBLOCK,R1                                                       
         MVC   WRITESW,MCWRITE     SAVE WRITE=Y/N SWITCH                        
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
         DROP  R1                                                               
ENDMST   DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,TODAY)                                      
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
*                                                                               
EDITMOD  NTR1                                                                   
         LA    R2,SPLOPT                                                        
         CLC   =C'NUBIL',SPLOPT                                                 
         BNE   EDINV                                                            
         OI    OPTTYPE,X'80'                                                    
*                                                                               
         MVI   NBUPUNIT,C'N'       DON'T WRITE UNITS                            
         MVI   NBNOWRIT,C'N'                                                    
*                                                                               
         BAS   RE,GOMOBILE                                                      
*                                                                               
         L     R2,=A(TAPEOUT)                                                   
         OPEN  ((2),OUTPUT)                                                     
         TM    48(R2),X'10'        TEST OPEN SUCCEEDED?                         
         BO    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         BAS   RE,BNUBIL                                                        
*                                                                               
         L     R2,=A(TAPEOUT)                                                   
         CLOSE ((2))                                                            
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
* - REPORT MODE                                                                 
* - NETIO READS UNIT AND HOOKS TO MAINLINE FOR PROCESSING                       
                                                                                
REPMOD   NTR1                                                                   
         XC    COUNTER,COUNTER                                                  
         ZAP   ASSTOT,=P'0'                                                     
         ZAP   ACTTOT,=P'0'                                                     
         ZAP   INTTOT,=P'0'                                                     
                                                                                
* - SET UP NETIO PARAMETERS                                                     
         MVI   NBDATA,C'B'         PACKAGES/UNITS                               
         MVI   NBSELUOP,0          EST+ACT SCHEDULE                             
         MVI   NBUSER+13,C'N'      OVERRIDE NO PREEMPTS PROFILE                 
         MVI   NBRESUME,NBPROCPK   START AGAIN AT PACKAGES                      
         MVI   NBSELPST,C'B'       LOCKED/UNLOCKED                              
*                                                                               
         LA    R1,MAINLINE                                                      
         ST    R1,NBHOOK                                                        
*                                                                               
RP10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    RPLAST                                                           
         B     RP10                                                             
*                                                                               
RPLAST   DS    0H                                                               
RPLX     B     XIT                                                              
*                                                                               
* - UNIT PROCESSING                                                             
MAINLINE NTR1                                                                   
MNX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* GET EACH UNIT AND BUILD THE NEW '0E06' RECORD ACCORDINGLY                     
*                                                                               
BNUBIL   NTR1                                                                   
         XC    SVBDATE,SVBDATE                                                  
         XC    HALF,HALF                                                        
         XC    UBCOUNT,UBCOUNT                                                  
         XC    PREVCLT,PREVCLT                                                  
         ZAP   BYTETOT,=P'0'                                                    
         ZAP   CLTBTOT,=P'0'                                                    
         ZAP   BILLTOT,=P'0'                                                    
         MVI   BYTE,0                                                           
*                                                                               
* - SET UP TO READ 1ST UNIT                                                     
*                                                                               
         MVI   NBDATA,C'U'         UNITS                                        
         MVI   NBSELUOP,0          EST+ACT SCHEDULE                             
         MVI   NBUSER+13,C'N'      OVERRIDE NO PREEMPTS PROFILE                 
         MVI   NBSELPST,C'U'       UNLOCKED                                     
*                                                                               
BN20     DS    0H                                                               
         NETGO NSNETIO,DMCB,NETBLOCK                                            
*                                                                               
BN30     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'                                                        
         GOTO1 HIGH                                                             
         B     BN50                                                             
*                                                                               
BNSEQ    GOTO1 SEQ                                                              
*                                                                               
BN50     DS    0H                                                               
         CLI   KEY,X'04'           STILL A UNIT RECORD?                         
         BNE   BN500                                                            
*                                                                               
         OC    PREVCLT,PREVCLT                                                  
         BNZ   *+10                                                             
         MVC   PREVCLT,KEY+2                                                    
*                                                                               
         CLC   PREVCLT,KEY+2       CHANGE IN CLIENT?                            
         BE    BN58                                                             
*                                                                               
         GOTO1 =V(CLUNPK),DMCB,PREVCLT,PCLTA    UNPACK CLIENT                   
         OC    PCLTA,=C'   '                                                    
*                                                                               
         MVC   P(3),PCLTA                                                       
         EDIT  CLTBTOT,(17,P+5),2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=$,     *        
               COMMAS=YES                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         ZAP   CLTBTOT,=P'0'                                                    
         MVC   PREVCLT,KEY+2                                                    
*                                                                               
BN58     DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R2,NBAIO                                                         
*                                                                               
         LA    RE,MYTABLE                                                       
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    R3,MYTABLE                                                       
*                                                                               
         L     R2,NBAIO            SKIP UN-BILLED UNITS                         
*                                                                               
         NI    MYFLAG,X'FF'-FOUND10                                             
         MVI   ELCODE,X'10'                                                     
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'G',=C'UNTFILE '),(ELCODE,(R2)),0               
         CLI   12(R1),0            SET CC ON EXIT                               
         BNE   BNSEQ                                                            
         L     R2,12(R1)           A(BILL ELEMENT)                              
         USING NUBILD,R2                                                        
*                                                                               
         CLI   1(R2),X'18'         24 BYTES?                                    
         BL    BN60                                                             
         CLI   1(R2),X'1C'         28 BYTES                                     
         BH    BN60                                                             
*&&DO                                                                           
         L     RF,UBCOUNT                                                       
         CH    RF,=H'50'                                                        
         BH    BN70                                                             
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'KEY',KEY,C'DUMP',20,=C'1D',          +        
               (C'P',VPRINT)                                                    
*&&                                                                             
         B     BN70                                                             
*                                                                               
BN60     DS    0H                                                               
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),X'10'         ANOTHER BILL ELEMENT                         
         BNE   BN80                                                             
*                                                                               
BN70     DS    0H                                                               
         TM    NUBILST,NUBILUBQ    SKIP UNBILLED ELEMENTS                       
         BO    BN60                                                             
*                                                                               
         SR    RF,RF               RF WILL CONTAIN EFFECTIVE GROSS              
*                                                                               
         USING NUBILD,R2                                                        
*                                                                               
         TM    NUBILST,X'20'        UNBILLED?                                   
         BO    BN60                YES, SKIP THIS ELEMENT                       
*                                                                               
         ICM   R0,15,NUBILGRS       R0 = GROSS                                  
         ICM   R1,15,NUBILNET       R1 = NET                                    
*                                                                               
         LR    RF,R0               NORMAL BILL?                                 
         TM    NUBILST,NUBILSCQ+NUBILSNQ                                        
         BZ    DONE                YES                                          
*                                                                               
         TM    NUBILST,NUBILSCQ                                                 
         BZ    *+12                                                             
         SR    R0,R1               COMMISSION BILLS                             
         LR    RF,R0                                                            
         B     DONE                                                             
*                                                                               
         LR    RF,R1               NET BILL                                     
*                                                                               
DONE     DS    0H                  (ADD RF TO ACCUMULATED CLIENT TOTAL)         
         CVD   RF,DUB                                                           
         AP    CLTBTOT,DUB         TOTAL CLIENT ACCUMULATOR                     
*                                                                               
         OI    MYFLAG,FOUND10                                                   
*                                                                               
         MVC   0(2,R3),NUBILDAT    DATE OF BILLING RUN                          
*                                                                               
         ZIC   RF,NUBILLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R3),0(R2)       SAVE ELEMENT IN TABLE                        
*                                                                               
BN75     LA    R3,35(R3)                                                        
         B     BN60                                                             
         DROP  R2                                                               
*                                                                               
BN80     DS    0H                  ADD NEW NEGENUBILL RECORDS                   
         TM    MYFLAG,FOUND10      FOUND A BILLING ELEMENT?                     
         BZ    BNSEQ                                                            
*                                                                               
BN84     DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R3,MYTABLE          FIRST TIME THROUGH                           
         ST    R3,ABELEM                                                        
         MVC   SVBDATE,0(R3)                                                    
         OI    MYFLAG,NEWREC                                                    
         B     BN90                                                             
*                                                                               
BN85     BAS   RE,GET10            GET NEXT BILL TO PROCESS                     
         TM    MYFLAG,NEWREC                                                    
         BZ    BN86                                                             
*                                                                               
         L     RF,UBCOUNT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,UBCOUNT                                                       
*                                                                               
         LA    RF,UBIO                                                          
         MVC   RECLN,32(RF)        RECORD LENGTH                                
         LH    R2,RECLN                                                         
         CVD   R2,DUB                                                           
         AP    BYTETOT,DUB         TOTAL BYTE ACCUMULATOR                       
*                                                                               
* PUT INTO TAPEOUT HERE                                                         
*                                                                               
         XC    UBIOH,UBIOH                                                      
         LH    R1,RECLN                                                         
         LA    R1,4(R1)                                                         
         STH   R1,UBIOH                                                         
         LA    R0,UBIOH                                                         
         L     R1,=A(TAPEOUT)                                                   
         PUT   (1),(0)                                                          
*                                                                               
         L     RF,UBCOUNT                                                       
         CH    RF,=H'50'                                                        
         BH    BN90                                                             
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'UBIOH',UBIOH,C'DUMP',34,=C'1D',      +        
               (C'P',VPRINT)                                                    
*                                                                               
         GOTO1 =V(PRTREC),DMCB,(R4),(42,32),VPRINT,HEXOUT                       
*                                                                               
         B     BN90                                                             
*                                                                               
BN86     DS    0H                                                               
         OC    ABELEM,ABELEM                                                    
         BZ    BN87                                                             
*                                                                               
         L     R3,ABELEM                                                        
         B     BN100                                                            
*                                                                               
BN87     DS    0H                  ADD LAST UBILL REC AND GET NEXT UNIT         
         L     RF,UBCOUNT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,UBCOUNT                                                       
*                                                                               
         LA    RF,UBIO                                                          
         MVC   RECLN,32(RF)        RECORD LENGTH                                
         LH    R2,RECLN                                                         
         CVD   R2,DUB                                                           
         AP    BYTETOT,DUB         TOTAL BYTE ACCUMULATOR                       
*                                                                               
* PUT INTO TAPEOUT HERE                                                         
*                                                                               
         XC    UBIOH,UBIOH                                                      
         LH    R1,RECLN                                                         
         LA    R1,4(R1)                                                         
         STH   R1,UBIOH                                                         
         LA    R0,UBIOH                                                         
         L     R1,=A(TAPEOUT)                                                   
         PUT   (1),(0)                                                          
*                                                                               
         L     RF,UBCOUNT                                                       
         CH    RF,=H'50'                                                        
         BH    BN89                                                             
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'UBIOH',UBIOH,C'DUMP',34,=C'1D',      +        
               (C'P',VPRINT)                                                    
*                                                                               
         GOTO1 =V(PRTREC),DMCB,(R4),(42,32),VPRINT,HEXOUT                       
*                                                                               
BN89     XC    KEY,KEY                                                          
         MVC   KEY(20),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         B     BNSEQ                                                            
*                                                                               
BN90     DS    0H                                                               
         L     R3,ABELEM                                                        
*                                                                               
         L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
*                                                                               
         MVC   SVACTDAT,NUKDATE    AIR DATE                                     
*                                                                               
         LA    RE,UBIO             IO TO BUILD RECORD                           
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    R4,UBIO                                                          
         USING NUBRECD,R4                                                       
*                                                                               
         MVI   NUBK0SYS,NUBK0SYQ                                                
         MVI   NUBK0STY,NUBK0STQ                                                
         MVC   NUBK0AM,NUKAM       AGENCY/MEDIA                                 
         MVC   NUBK0CLI,NUKCLT     CLIENT                                       
         MVC   NUBK0DAT,NUKDATE    AIR DATE                                     
         MVC   NUBK0TIM,NUKTIME    START 1/4 HOUR                               
         MVC   NUBK0NET,NUKNET     NETWORK                                      
         MVC   NUBK0PRG,NUKPROG    PROGRAM                                      
         MVC   NUBK0EST,NUKEST     ESTIMATE                                     
         MVC   NUBK0SUB,NUKSUB     SUB-LINE                                     
         MVC   NUBK0DPT,NUKDP      DAYPART                                      
         MVC   NUBK0BDT,0(R3)      BILL DATE                                    
*                                                                               
         MVI   NUBRECLN+1,NUBELDQ  RECORD LENGTH                                
         MVC   NUBSTAT(1),NUKSTAT  STATUS                                       
         MVC   NUBSTAT+1(1),NUDAY  DAY CODE                                     
*                                                                               
         ZIC   RF,NUBRECLN+1                                                    
         LA    RF,1(RF)                                                         
         STC   RF,NUBRECLN+1                                                    
*                                                                               
         XC    MYELEM,MYELEM                                                    
         LA    R5,MYELEM                                                        
         MVI   0(R5),X'99'         MARK THIS REC W/ MY ELEMENT                  
         MVI   1(R5),X'08'                                                      
         MVC   2(3,R5),TODAY       TODAY'S DATE                                 
         GOTO1 =V(HELLO),DMCB,(C'P',=C'XSPFIL  '),(X'99',(R4)),(R5),0           
*                                                                               
BN100    DS    0H                                                               
         L     R2,NBAIO                                                         
*                                                                               
         LA    R3,2(R3)                                                         
         USING NUBILD,R3                                                        
*                                                                               
         LA    R5,MYELEM                                                        
         USING NBILD,R5                                                         
         XC    MYELEM,MYELEM                                                    
*                                                                               
         MVI   NBILEL,NBILELQ                                                   
         MVI   NBILLEN,NBILELNQ                                                 
         MVC   NBILCHGT,NUBILTYP   CHARGE TYPE                                  
*                                                                               
         MVC   SVBTYP,NUBILTYP     CHARGE TYPE                                  
*                                                                               
         BAS   RE,GETBTYPC         GET CHARGE TYPE CODE                         
         CLI   SVBTYP,0            INVALID TYPE?                                
         BE    BN160                                                            
*                                                                               
         MVC   NBILCHGC,SVBTYPC    CHARGE TYPE CODE                             
*                                                                               
         MVC   NBILPRD,NUBILPRD    PRODUCT CODE                                 
         MVC   NBILIDT,NUBILIDT    DATE OF INVOICE                              
         MVC   NBILST,NUBILST      STATUS                                       
         NI    NBILST,X'FF'-X'01'  X'01' BIT NO LONGER NEEDED                   
         MVC   NBILGRS,NUBILGRS    GROSS BILLING                                
         MVC   NBILNET,NUBILNET    NET BILLING                                  
         MVC   NBILBTYP,NUBILBTY   BILLING TYPE                                 
*                                                                               
         CLI   NUBILLEN,X'18'      24 BYTES (HAVE 2ND GROSS?)                   
         BE    *+10                                                             
         MVC   NBILGR2,NUBILGR2    GROSS (COST2)                                
*                                                                               
         XC    NBILMKST,NBILMKST                                                
*                                                                               
*&&DO                                                                           
         MVC   NBILMKT,NUMARKET    MARKET                                       
*                                                                               
         MVC   SVSTA(4),NUKNET     SET UP MARKET/STATION FOR MSPACK             
         MVI   SVSTA+4,C'T'                                                     
         MVC   SVMKT,=C'0001'                                                   
*                                                                               
         GOTO1 MSPACK,DMCB,SVMKT,SVSTA,DUB                                      
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   NBILSTA,DUB+2       PACKED STATION                               
*&&                                                                             
         OC    NUBILNUM,NUBILNUM                                                
         BZ    BN150                                                            
*                                                                               
         XC    DUB,DUB                                                          
         PACK  DUB,NUBILNUM        PACK BILL NUMBER                             
         CVB   R0,DUB                                                           
         STCM  R0,3,NBILNUM        BILL NUMBER                                  
*                                                                               
BN150    BAS   RE,GETMOS                                                        
*                                                                               
         MVC   NBILMOS,BYMOS       MONTH OF SERVICE                             
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'XSPFIL  '),(X'10',(R4)),(R5),0           
*                                                                               
BN160    MVI   32(R3),X'FF'        PROCESSED THIS ONE                           
         B     BN85                                                             
*                                                                               
BN500    DS    0H                                                               
         GOTO1 =V(CLUNPK),DMCB,PREVCLT,PCLTA    UNPACK CLIENT                   
         OC    PCLTA,=C'   '                                                    
*                                                                               
         MVC   P(3),PCLTA                                                       
         EDIT  CLTBTOT,(17,P+5),2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=$,     *        
               COMMAS=YES                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P(11),=C'TOTAL ADDED'                                            
         EDIT  (4,UBCOUNT),(12,P+15),COMMAS=YES                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P(11),=C'TOTAL BYTES'                                            
         EDIT  BYTETOT,(17,P+15),ZERO=NOBLANK,ALIGN=LEFT,COMMAS=YES             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
BNUBILX  DS    0H                                                               
         B     XIT                                                              
         DROP  R2,R4                                                            
*                                                                               
* CHECK PAID ELEMENTS IF PAID FOR OLD STYLE SPECIAL CHARGE                      
*                                                                               
CHKPAID  NTR1                                                                   
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'12'                                                     
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'G',=C'UNTFILE '),(ELCODE,(R2)),0               
         CLI   12(R1),0            SET CC ON EXIT                               
         BNE   CHKPX                                                            
         L     R2,12(R1)           A(PAID ELEMENT)                              
         USING NUPAYD,R2                                                        
*                                                                               
CHKP10   DS    0H                                                               
         CLI   0(R2),X'12'                                                      
         BNE   CHKPX                                                            
*                                                                               
         CLI   NUPAYTYP,C'T'       TIME?                                        
         BE    CHKP50                                                           
         CLI   NUPAYTYP,C'I'       INTEGRATION?                                 
         BE    CHKP50                                                           
*                                                                               
         OI    MYFLAG,LIMITSPC                                                  
         B     CHKPX                                                            
*                                                                               
CHKP50   DS    0H                                                               
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     CHKP10                                                           
*                                                                               
CHKPX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
* GET CHARGE TYPE EQUATE                                                        
*                                                                               
GETBTYPC NTR1                                                                   
         LA    R2,CTYPLST                                                       
         LA    R3,12                                                            
*                                                                               
GBT10    DS    0H                                                               
         CLC   0(1,R2),SVBTYP      SAME CHARGE TYPE?                            
         BE    GBTX                                                             
         LA    R2,2(R2)                                                         
         BCT   R3,GBT10                                                         
*                                                                               
         MVI   SVBTYP,0            INVALID TYPE                                 
         B     XIT                                                              
*                                                                               
GBTX     DS    0H                                                               
         MVC   SVBTYPC,1(R2)                                                    
         B     XIT                                                              
*                                                                               
* GET NEXT BILL ELEMENT TO PROCESS FROM UNIT                                    
*                                                                               
GET10    NTR1                                                                   
         LA    R3,MYTABLE                                                       
         ST    R3,ABELEM                                                        
         NI    MYFLAG,X'FF'-NEWREC                                              
*                                                                               
         OC    SVBDATE,SVBDATE                                                  
         BNZ   GT10010                                                          
         MVC   SVBDATE,0(R3)                                                    
         OI    MYFLAG,NEWREC                                                    
         B     GET10X                                                           
*                                                                               
GT10010  DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    GT10100                                                          
*                                                                               
         CLI   34(R3),X'FF'        PROCESSED THIS ONE ALREADY?                  
         BE    GT10050                                                          
*                                                                               
         CLC   SVBDATE,0(R3)       SAME BILL DATE?                              
         BNE   GT10050                                                          
*                                                                               
         ST    R3,ABELEM                                                        
         NI    MYFLAG,X'FF'-NEWREC                                              
         B     GET10X                                                           
*                                                                               
GT10050  DS    0H                                                               
         LA    R3,35(R3)           BUMP TO NEXT TABLE ENTRY                     
         B     GT10010                                                          
*                                                                               
GT10100  DS    0H                  GET FIRST UNPROCESSED UNIT                   
         LA    R3,MYTABLE                                                       
         XC    SVBDATE,SVBDATE                                                  
         XC    ABELEM,ABELEM                                                    
*                                                                               
GT10110  DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    GET10X              FINISHED W/ ELEMS - GET NEXT UNIT            
                                                                                
         CLI   34(R3),X'FF'        PROCESSED?                                   
         BE    GT10200                                                          
*                                                                               
         ST    R3,ABELEM                                                        
         MVC   SVBDATE,0(R3)                                                    
         OI    MYFLAG,NEWREC                                                    
         B     GET10X                                                           
*                                                                               
GT10200  DS    0H                                                               
         LA    R3,35(R3)           BUMP TO NEXT TABLE ENTRY                     
         B     GT10110                                                          
*                                                                               
GET10X   DS    0H                                                               
         B     XIT                                                              
*                                                                               
* PRINT UNIT                                                                    
*                                                                               
PRNTUNIT NTR1                                                                   
         L     R4,NBAIO                                                         
         USING NURECD,R4                                                        
         MVC   P+5(6),NUKPROG      PROGRAM                                      
         MVC   P+13(4),NUKNET      NETWORK                                      
         ZIC   R3,NUKEST           ESTIMATE                                     
         EDIT  (R3),(3,P+19)                                                    
         GOTO1 DATCON,DMCB,(2,NUKDATE),(8,P+24)                                 
         MVI   P+32,C'-'                                                        
         EDIT  (B1,NUKSUB),(3,P+33)  ..PRINT OUT LENGTH                         
         EDIT  (B1,NULEN),(3,P+38)  ..PRINT OUT LENGTH                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R4                                                               
*                                                                               
         L     R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER                                                       
*                                                                               
PRNTUNX  B     XIT                                                              
*                                                                               
ADDDIR   NTR1                                                                   
         CLI   TESTRUN,C'N'                                                     
         BNE   ADDDX                                                            
         GOTO1 DATAMGR,DMCB,=C'DMADD',FILENAME,KEY,KEY                          
ADDDX    B     XIT                                                              
*                                                                               
*                                                                               
GOMOBILE NTR1                                                                   
*                                                                               
* - READS SPOT00 PROFILE TO SET UP DATE LIST                                    
* - NEED TO PASS THESE ADDS TO MOBILE                                           
* - NOT ENOUGH ROOM TO LINK THEM IN                                             
*                                                                               
         LA    R2,MOBILADS                 ADDRESSES FOR MOBILE                 
         XC    0(4,R2),0(R2)               (GETBROAD) LINKED                    
         MVC   4(4,R2),ADDAY                                                    
         MVC   8(4,R2),GETDAY                                                   
         MVC   12(4,R2),DATCON                                                  
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'B3'           GET B3 PROFILE                        
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..IF FILTERING                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R2,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 GETPROF,DMCB,(0,WORK),MYWORK2,(0,(R2))                           
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'00'                                                 
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..MEDIA FILTER                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R2,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 GETPROF,DMCB,(0,WORK),MYWORK,(0,(R2))                            
*                                                                               
* - SET B3 PROFILE VALUES INTO 00 DATE AREAS                                    
*                                                                               
         MVC   MYWORK+2(1),MYWORK2                                              
         MVC   MYWORK+6(3),MYWORK2+1                                            
         IC    R0,MYWORK+2       DATE CONTROL                                   
*                                                                               
* - SET (NBSELSTR - 1 YEAR)  TO DUB FOR BROAD MOBILE DATELIST                   
*                                                                               
         MVC   SVSELSTR,=C'890101'                                              
         MVI   SVSELEND,X'FA'                                                   
         MVC   SVSELEND+1,=C'11231'                                             
*                                                                               
         L     R4,=F'-450'                                                      
         LA    RF,SVSELSTR                                                      
*                                                                               
         GOTO1 ADDAY,DMCB,SVSELSTR,MYWORK2,(R4)                                 
         MVC   MYWORK2+6(6),SVSELEND                                            
                                                                                
         LA    R2,UBIO             BUILD LIST OF DATE PAIRS                     
         LA    R4,MYWORK           PASS ADDRESS OF 00 PROFILE                   
         GOTO1 =V(MOBILE),DMCB,(208,MYWORK2),((R0),(R2)),MOBILADS,(R4)          
*                                                                               
SETD4    DS    0H                  FIND FIRST PERIOD OF A NEW YEAR              
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD6               YES                                          
         CLI   5(R2),0             IF ZERO WE GET LOOP IN SETD8                 
         BE    SETD6                                                            
         LA    R2,4(R2)                                                         
         B     SETD4                                                            
*                                                                               
SETD6    DS    0H                  BUILD A LIST OF YM, START-END                
         LA    R3,PERLIST                                                       
*                                                                               
SETD7    DS    0H                                                               
         ZIC   R0,2(R2)                                                         
         SRL   R0,1                                                             
         STC   R0,BYTE             YEAR                                         
         SR    R4,R4               FOR PER SEQUENCE WITHIN YR                   
*                                                                               
SETD8    DS    0H                                                               
         LA    R4,1(R4)                                                         
         MVC   0(1,R3),BYTE        YEAR                                         
         STC   R4,1(R3)            MONTH                                        
         MVC   2(4,R3),0(R2)       START-END OF PER                             
         LA    R3,6(R3)                                                         
         LA    R2,4(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BE    SETD12              EOL                                          
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD7               YES                                          
         B     SETD8                                                            
*                                                                               
SETD12   DS    0H                                                               
*                                                                               
SETDATEX DS    0H                                                               
GOMOBILX DS    0H                                                               
         B     XIT                                                              
*                                  FIND START OF NEW YEAR                       
*                                  1) A PERIOD THAT SPANS YEAR CHANGE           
*                                     AND BEGINS NO FURTHER AWAY                
*                                     FROM 12/31 THAN IT ENDS                   
*                             OR   2) A PERIOD THAT STARTS BEFORE 1/14          
*                                                                               
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R2)                                                     
         NI    DUB,X'01'           STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
CKNYYES  DS    0H                                                               
         SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
*                                                                               
MOBILADS DS    4F                  ADDRESSES PASSED TO MOBILE                   
*                                                                               
GETMOS   DS    0H                  ROUTINE GETS MONTH OF SERVICE                
         LA    RF,PERLIST                                                       
*                                                                               
GETM4    CLC   SVACTDAT,2(RF)      TEST DATE VS START                           
         BE    GETM8                                                            
         BNL   *+8                                                              
         B     GETMX                                                            
*                                                                               
*!!!!    DC    H'0'                TIME TO LENGTHEN PERLIST                     
*                                                                               
         CLC   SVACTDAT,4(RF)                                                   
         BNH   GETM8                                                            
         LA    RF,6(RF)            NEXT ENTRY                                   
         B     GETM4               NOTE- LIST ENDS IN FF                        
*                                                                               
GETM8    DS    0H                                                               
         MVC   BYMOS,0(RF)         SET YR/MOS                                   
GETMX    BR    RE                                                               
*                                                                               
HOOK     NTR1                                                                   
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         LA    R1,H3                                                            
         MVC   H3+10(5),SPLCLI                                                  
         LA    R1,H4                                                            
         MVC   H4+10(5),SPLPRO                                                  
         LA    R1,H5                                                            
         MVC   H5+10(8),SPLEST                                                  
         LA    R1,H6                                                            
         MVC   H6+10(8),SPLNET                                                  
         MVI   H7,0                                                             
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
*                                                                               
CTYPLST  DS    0D                                                               
         DC    C'T',X'00'                                                       
         DC    C'I',X'01'                                                       
         DC    C'U',X'02'                                                       
         DC    C'B',X'03'                                                       
         DC    C'S',X'04'                                                       
         DC    C'X',X'05'                                                       
         DC    C'E',X'06'                                                       
         DC    C'A',X'07'                                                       
         DC    C'L',X'08'                                                       
         DC    C'O',X'09'                                                       
         DC    C'D',X'0A'                                                       
         DC    C'Q',X'0B'                                                       
*                                                                               
CLTTAB   DC    0H                                                               
         DC    C'ATK'                                                           
         DC    C'AXC'                                                           
         DC    C'BBC'                                                           
         DC    C'BLT'                                                           
         DC    C'CON'                                                           
         DC    C'DIA'                                                           
         DC    C'DRT'                                                           
         DC    C'EAR'                                                           
         DC    C'ESP'                                                           
         DC    C'GEN'                                                           
         DC    C'HAN'                                                           
         DC    C'HPS'                                                           
         DC    C'HSB'                                                           
         DC    C'IBC'                                                           
         DC    C'IND'                                                           
         DC    C'JIF'                                                           
         DC    C'KOC'                                                           
         DC    C'MRT'                                                           
         DC    C'PSS'                                                           
         DC    C'PTC'                                                           
         DC    C'PTP'                                                           
         DC    C'RCC'                                                           
         DC    C'REE'                                                           
         DC    C'RSL'                                                           
         DC    C'SBK'                                                           
         DC    C'SHA'                                                           
         DC    C'SNY'                                                           
         DC    C'SUZ'                                                           
         DC    C'THO'                                                           
         DC    C'VIV'                                                           
         DC    C'DOW'                                                           
         DC    C'DWA'                                                           
         DC    C'BMS'                                                           
         DC    C'GM1'                                                           
         DC    C'EY '                                                           
         DC    C'NRL'                                                           
         DC    C'ACT'                                                           
         DC    C'DTV'                                                           
         DC    C'NBC'                                                           
         DC    C'AT '                                                           
         DC    C'AT3'                                                           
         DC    C'BCC'                                                           
         DC    C'AL1'                                                           
         DC    C'AV1'                                                           
         DC    C'BBN'                                                           
         DC    C'CO1'                                                           
         DC    C'GB1'                                                           
         DC    C'KEN'                                                           
         DC    C'OND'                                                           
         DC    C'SA1'                                                           
         DC    C'SH1'                                                           
         DC    C'TR1'                                                           
         DC    C'VZ1'                                                           
         DC    C'WU '                                                           
         DC    C'CA1'                                                           
         DC    C'MM1'                                                           
         DC    C'PGB'                                                           
         DC    C'PGP'                                                           
         DC    C'ETP'                                                           
         DC    C'CA4'                                                           
         DC    C'AGR'                                                           
         DC    C'ARD'                                                           
         DC    C'AVO'                                                           
         DC    C'MMM'                                                           
         DC    C'PRM'                                                           
         DC    C'AAA'                                                           
         DC    C'AG '                                                           
         DC    C'ZZZ'                                                           
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
COUNTER  DS    F                                                                
ASSTOT   DS    PL8                                                              
ACTTOT   DS    PL8                                                              
INTTOT   DS    PL8                                                              
TESTRUN  DS    CL1                                                              
OPTTYPE  DS    CL1                                                              
NEWRATE  DS    CL1                 NEW RATE TYPE                                
NEWRCOV  DS    CL1                 NEW RATE COVERAGE                            
NEWDTYP  DS    CL1                 NEW DEMO TYPE                                
FIRST    DS    CL1                                                              
SVPRD    DS    XL3                 PRODUCT                                      
SVEST    DS    XL1                 ESTIMATE                                     
SAVEKEY  DS    CL20                                                             
HOOKSAVE DS    CL4                                                              
BYT1     DS    CL1                                                              
SCNBLOCK DS    CL400                                                            
NEWASSGN DS    CL4                                                              
NEWACTUL DS    CL4                                                              
TODAY    DS    CL3                 TODAY'S DATE (YMD)                           
COST2C   DS    XL4                 CLIENT COST2 FACTOR                          
COST2E   DS    XL4                 ESTIMATE COST2 FACTOR                        
*                                                                               
TEMPKEY  DS    XL32                                                             
*                                                                               
SVBDATE  DS    XL2                 BILLING DATE                                 
SVSTA    DS    CL5                 STATION                                      
SVMKT    DS    CL4                 MARKET                                       
SVBTYP   DS    CL1                 CHARGE TYPE                                  
SVBTYPC  DS    XL1                 CHARGE TYPE CODE                             
*                                                                               
WRITESW  DS    C                   WRITE=Y/N                                    
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
NEWREC   EQU   X'01'               ADD NEW UBILL REC                            
FOUND10  EQU   X'02'               FOUND A BILL ELEMENT                         
LIMITSPC EQU   X'04'               LIMIT SPECIAL CHARGE FLAG                    
*                                                                               
ABELEM   DS    F                   A(BILL ELEM TO PROCESS FROM UNIT)            
*                                                                               
UBCOUNT  DS    F                   COUNTER OF UBILLS ADDED                      
*                                                                               
RECLN    DS    H                   RECORD LENGTH                                
BYTETOT  DS    PL10                TOTAL BYTES ADDED                            
*                                                                               
SVSELSTR DS    CL6                 START DATE                                   
SVSELEND DS    CL6                 END DATE                                     
SVACTDAT DS    CL2                 AIR DATE                                     
*                                                                               
BYMOS    DS    XL2                 BILLING YR/MOS FROM GETMOS                   
MYWORK   DS    XL100                                                            
MYWORK2  DS    XL70                                                             
*                                                                               
CLTBTOT  DS    PL10                CLIENT BILLING TOTAL                         
BILLTOT  DS    PL10                TOTAL BILLING                                
*                                                                               
CLTA     DS    CL3                 ALPHA CLIENT                                 
PCLTA    DS    CL3                 ALPHA CLIENT                                 
PREVCLT  DS    XL2                 PREVIOUS CLIENT                              
*                                                                               
*DATADISP DS    H                                                               
MYELEM   DS    XL50                                                             
*                                                                               
PERLIST  DS    XL(15*13*6+1)       15YRS X 13MNTHS X 6                          
*                                                                               
UBIOH    DS    F                                                                
UBIO     DS    XL2000                                                           
MYTABLE  DS    XL4000                                                           
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEGENUBILL                                                     
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIEFD                                                       
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'254UTILUBILLC10/16/03'                                      
         END                                                                    
