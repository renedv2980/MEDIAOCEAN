*          DATA SET SPMAT01    AT LEVEL 125 AS OF 01/09/07                      
*PHASE T21501A                                                                  
*INCLUDE SPGETSNV                                                               
         TITLE 'T21501 - ICS MATCH OVLY 1 - BYBLD, INVBLD'                      
T21501   CSECT                                                                  
         PRINT GEN                                                              
         NMOD1 0,T21501                                                         
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T215FFD,RA                                                       
*                                                                               
         RELOC RELO01                                                           
*                                                                               
         L     RE,ABUFFER                                                       
         ST    RE,AFINV             SET AFINV                                   
*                                  BUILD INVOICE BUFFER                         
         GOTO1 =A(INVBLD),DMCB,(RC),RR=RELO01                                   
         SPACE 3                                                                
         CLI   ERRAREA,0                                                        
         BNE   EXT                                                              
         SPACE 1                                                                
         L     RE,ALINV                                                         
         LA    RE,1(RE)                                                         
         ST    RE,AFBY            SET AFBY                                      
         SPACE 3                                                                
*                                  BUILD BUY BUFFER                             
         GOTO1 =A(BYBLD),DMCB,(RC),RR=RELO01                                    
*                                                                               
EXT      XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'BYBLD - ICS BUYREC BUFFER MODULE'                               
         DS    0D                                                               
BYBLD    NMOD1 0,BYBLD                                                          
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T215FFD,RA                                                       
         USING BYELEMD,R7                                                       
         USING BUYRECD,R9                                                       
         L     R9,AIOABUY                                                       
         USING REGELEM,R8                                                       
         LA    R0,BUYDSKS                                                       
         ST    R0,DSKPOINT                                                      
         XC    TAXTOT,TAXTOT                                                    
         XC    REPLST,REPLST                                                    
         XC    PDGRS(8),PDGRS                                                   
         MVI   ORBSW,C'N'                                                       
         EJECT                                                                  
         L     R7,AFBY                                                          
         BAS   RE,BLDKEY                                                        
         LA    R2,DUB                                                           
         BAS   RE,HIGH                                                          
         B     *+8                                                              
NXTREC   BAS   RE,SEQ                                                           
         BAS   RE,TSTKEY                                                        
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         BAS   RE,GETREC                                                        
         MVI   DMINBTS,0           RESET                                        
         MVI   DMOUTBTS,X'FF'                                                   
         BAS   RE,TSTREC                                                        
         LA    R8,BDELEM                                                        
NXTEL    BAS   RE,GETEL                                                         
         GOTO1 =A(PROCEL),DMCB,(RC),RR=RELO01                                   
         CLI   BYSDT,0             DATE=0 MEANS BY NOT ACCEPTED                 
         BE    NXTEL                                                            
*                                  STORE DISK ADDRESS IN BUYDSKS                
         L     R1,DSKPOINT                                                      
         CLC   KEY+14(4),0(R1)                                                  
         BE    NXTEL2              THIS REC ADDR ALREADY IN LIST                
         OC    0(4,R1),0(R1)                                                    
         BZ    NXTEL1                                                           
         LA    R1,4(R1)                                                         
         LA    R0,BUYDSKSX                                                      
         CR    R1,R0                                                            
         BNH   NXTEL04                                                          
         LA    R3,BUFERR                                                        
         MVC   FULL,=C'DISK'       KIND OF OVERFLOW                             
         B     ERROR                                                            
*                                                                               
NXTEL04  DS    0H                                                               
         ST    R1,DSKPOINT                                                      
NXTEL1   MVC   0(4,R1),KEY+14                                                   
NXTEL2   LA    R7,L'BYELEM(R7)                                                  
         C     R7,ABUFFX                                                        
         BNH   NXTEL                                                            
         LA    R3,BUFERR           BUFFER OVERFLOW                              
         MVC   FULL,=C'BUY '       KIND OF OVERFLOW                             
         B     ERROR                                                            
LSTEL    EQU   NXTREC                                                           
LSTREC   BCTR  R7,R0                                                            
         ST    R7,ALBY                                                          
         S     R7,AFBY                                                          
         LA    R7,1(R7)                                                         
         SR    R6,R6                                                            
         LA    R4,L'BYELEM                                                      
         DR    R6,R4                                                            
         LTR   R7,R7               R7 = NO. OF BYELEMS                          
         BZ    EXIT                                                             
*                                  SORT ON CBLNET/DATE/TIME                     
         GOTO1 VXSORT,DMCB,AFBY,(R7),(R4),11,0                                  
*                                                                               
         XC    DATINBH,DATINBH                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                   BUILD BUY KEY                                               
         SPACE 3                                                                
BLDKEY   XC    KEY,KEY                                                          
         MVC   KEY(1),HKEY+1       A/M                                          
         MVC   KEY+1(2),BCLT     CLT                                            
         OC    KEY+3(1),BPRD       PRD                                          
         BNZ   *+8                                                              
         MVI   KEY+3,X'FF'         POOL                                         
         MVC   KEY+4(5),BMS        M/S                                          
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                   TEST BUY POINTER                                            
         SPACE 3                                                                
TSTKEY   DS    0H                                                               
         TM    CBLHOPT,CBLHNHAQ    IF NOT CBH REQ AND NEW MODE                  
         BNO   TSTK04              AND ALL NETS - NORMAL                        
         CLC   KEY(6),KEYSAVE      TEST KEY THRU MKT                            
         BNE   LSTREC                                                           
         MVC   DUB(3),BMS+2        AND TEST CABLE HEAD                          
         NI    DUB+2,X'80'         CLEAR NETWORK                                
         MVC   DUB+3(3),KEY+6                                                   
         NI    DUB+5,X'80'                                                      
         CLC   DUB(3),DUB+3                                                     
         BNE   LSTREC                                                           
         B     TSTK05                                                           
*                                                                               
TSTK04   DS    0H                                                               
         CLC   KEY(9),KEYSAVE      TEST THRU STATION                            
         BNE   LSTREC                                                           
*                                                                               
TSTK05   DS    0H                                                               
         CLI   BEST,0                                                           
         BE    TSTKEY2                                                          
         CLC   BEST,KEY+9          TEST RIGHT EST                               
         BNE   NXTREC                                                           
TSTKEY2  DS    0H                                                               
         L     R1,AIOABUY          CLEAR IOAREA                                 
         LA    R0,16                                                            
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
*                   TEST BUY RECORD                                             
         SPACE 3                                                                
TSTREC   TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    TR1A                NO                                           
         TM    BDCIND2,BDC2NEG     NEGATIVE RATE                                
         BZ    TR1                 NO                                           
         B     TR1B                NO                                           
TR1A     TM    BDCIND,X'01'        NEGATIVE RATE                                
         BZ    TR1                 NO                                           
TR1B     OC    BDCOST,BDCOST       YES - IS COST 0                              
         BZ    NXTREC              YES - BYPASS                                 
TR1      DS    0H                                                               
         TM    BUYRCNTL,X'80'       TEST DELETED RECORD                         
         BNZ   NXTREC                                                           
*                                                                               
         TM    BDCIND2,X'02'       CONTRACT TRADE BUY?                          
         BZ    *+8                                                              
         OI    BYSTAT,X'08'                                                     
*                                                                               
         MVI   SPCNT,0                                                          
         MVI   PGPRD,0                                                          
         XC    OLDDAT,OLDDAT                                                    
         CLI   BUYKEY+3,X'FF'                                                   
         BE    TR6                                                              
         CLI   BPRD,0                                                           
         BE    *+14                                                             
         CLC   BPRD,BUYKEY+3       TEST RIGHT ACTIVE BRD                        
         BNE   NXTREC                                                           
         LA    R8,BDELEM                                                        
         SR    R0,R0                                                            
TR2      IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         CLI   0(R8),4             PIGGYBACK ELEM                               
         BE    TR3                                                              
         CLI   0(R8),0                                                          
         BE    TR3B                                                             
         B     TR2                                                              
TR3      MVC   PGPRD,2(R8)         SAVE PASSIVE PROD                            
TR3B     DS    0H                                                               
         CLC   BPRD2,PGPRD         0 OR PRD CODE                                
         BE    TR3D                                                             
         CLI   BPRD2,X'FF'                                                      
         BNE   NXTREC                                                           
         CLI   PGPRD,0                                                          
         BE    NXTREC                                                           
         SPACE 2                                                                
*                                                                               
TR3D     DS    0H                                                               
*                                  GO TO GETRATE ONCE FOR NON-POOL RECS         
TR4      IC    R0,BDTIME                                                        
         MVI   BDTIME,0            SET BDTIME TO 0                              
         ST    RE,FULL                                                          
         CLI   PSEUDOPT,C'R'       FOR RESPONSE COUNT REQS                      
         BE    TR5                 SKIP COSTS                                   
         GOTO1 VGETRATE,DMCB,SPOTS,(R9)                                         
TR4B     DS    0H                                                               
         XC    TAXAMT,TAXAMT                                                    
         CLI   TAXOPT,C'Y'         TEST TAX OPTION                              
         BNE   TR5                                                              
         OC    BDNTAX,BDNTAX                                                    
         BZ    TR5                                                              
*                                  EXTRACT TAX FROM BUY                         
TR4D     DS    0H                                                               
         MVC   DUB(4),NET                                                       
         MVC   DUB+5(2),BDNTAX                                                  
         XC    BDNTAX,BDNTAX                                                    
*                                  RECOPMUTE COST WITHOUT TAX                   
         GOTO1 (RF),(R1)                                                        
*                                                                               
         L     RF,DUB                                                           
         S     RF,NET                                                           
         ST    RF,TAXAMT           GET TAX AMOUNT                               
         MVC   BDNTAX,DUB+5                                                     
TR5      DS    0H                                                               
         SPACE 1                                                                
         L     RE,FULL                                                          
         STC   R0,BDTIME                                                        
*                                  GET HIGHEST DAY OF ROTATOR                   
TR6      EQU   *                                                                
         SR    R0,R0                                                            
         IC    R0,BDDAY                                                         
         LA    R1,1                                                             
         SRA   R0,1                                                             
         BZ    *+12                                                             
         SLL   R1,1                                                             
         B     *-12                                                             
         STC   R1,BYTE2            = HIGHEST DAY OF ROTATOR                     
*                                                                               
*                                  LOOK FOR MATCH=NO COMMENT                    
TR6A     DS    0H                                                               
         LA    R8,BDELEM                                                        
*                                                                               
TR6B     DS    0H                                                               
         CLI   0(R8),X'66'                                                      
         BE    TR6D                                                             
         CLI   0(R8),0                                                          
         BE    TR6E                                                             
TR6C     DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,1(R8)                                                         
         AR    R8,RF                                                            
         B     TR6B                                                             
TR6D     DS    0H                                                               
         CLC   =C'MATCH=NO',3(R8)                                               
         BE    NXTREC                                                           
         B     TR6C                                                             
*                                                                               
TR6E     DS    0H                                                               
         TM    QBYID,QBYIDYQ          TEST IF ID REQ                            
         BZ    TR9                    NO, OK                                    
         TM    QBYID,QBYIDHQ+QBYIDNQ  IF 'HOME' OR 'NONE'                       
         BNZ   TR9                    ALSO OK (BECAUSE IN HOME MKT)             
         LA    R8,BDELEM              ELSE FIND ID ELEM                         
*                                                                               
TR6F     DS    0H                                                               
         CLI   0(R8),0             EOR                                          
         BE    TR6J                                                             
         CLI   0(R8),X'70'         ID ELEM                                      
         BE    TR6H                                                             
*                                                                               
         ZIC   R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     TR6F                                                             
*                                                                               
TR6H     DS    0H                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(11),WORK                                                  
         OC    WORK(12),3(R8)                                                   
         SR    RF,RF                                                            
         ICM   RF,1,BUYIDLN        LENGTH FOR ID COMP                           
         BNZ   *+8                                                              
         LA    RF,12               DEFAULT IS 12                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),BUYID      IS IT RIGHT ID                                
         BE    TR9                                                              
         B     NXTREC              NO REJECT                                    
*                                                                               
TR6J     DS    0H                  NO ID ELEM                                   
         B     NXTREC              REJECT                                       
*                                                                               
TR9      DS    0H                                                               
         LR    R0,RE                                                            
         BAS   RE,PDTF             DEFERRAL DATE FIX                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
*        PDTF        SPOT DATE FIX FOR DEFERRAL DATE CHANGING                   
*                                                                               
*        MODULE RESETS THE SPOT DATE OF ANY DEFERRAL ELEMS                      
*        WHOSE DATE WAS CHANGED DURING AN EARLIER MATCH. THE IDEA IS            
*        TO RESTORE THE FILE TO ITS ORIGINAL STATUS.                            
*                                                                               
*        IT IS PRESENT HERE AND IN SPMAT02 WHERE IT IS CALLED TO                
*        MAKE THE SAME RECORD CHANGES BEFORE POSTING SO THAT                    
*        THE ADDEL ROUTINES WON'T BLOW UP.                                      
*                                                                               
**********************************************************************          
*                                                                               
PDTF     NTR1                                                                   
         CLI   DEFDCHG,C'Y'        DEFERRAL DATE CHANGE?                        
         BNE   PDTFX               NO, FORGET IT.                               
         CLI   BUYKEY+3,X'FF'      POL BUYS ONLY                                
         BNE   PDTFX                                                            
         TM    BDSTAT,X'80'        SKIP IF POL NPW                              
         BNZ   PDTFX                                                            
         TM    BDSTAT2,X'80'       SKIP IF DAILY SKED (PER MEL)                 
         BNZ   PDTFX                                                            
         CLI   PROGPROF+9,C'C'     NOT CALENDAR MONTHS                          
         BE    PDTFX                                                            
         CLC   BSTART,BRDMON       START DATE MUST = START OF MONTH             
         BNE   PDTFX                                                            
*                                                                               
         ZIC   R2,BDSEDAY          IS IT AN OUT-OF-WEEK?                        
         SRDL  R2,4                (FOR OUT OF WEEK START DAY                   
         SRL   R3,28               IS HIGHER THAN END)                          
         CR    R2,R3                                                            
         BNH   PDTFX               NO, SKIP IT                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(2,PREVWKP),WORK   SET VARIOUS DATES                
         BCTR  R2,0                                                             
         GOTO1 VADDAY,DMCB,WORK,DUB,(R2)                                        
         GOTO1 VDATCON,DMCB,DUB,(2,PREVWKR)   START OF LAST ROTATOR WK          
         GOTO1 VADDAY,DMCB,DUB,DUB,6                                            
         GOTO1 VDATCON,DMCB,DUB,(2,PREVWKRX)  END OF LAST ROTATOR WEEK          
*                                                                               
         LA    R8,BDELEM                                                        
         USING REGELEM,R8                                                       
         B     PDTF04                                                           
*                                                                               
PDTF02   DS    0H                                                               
         ZIC   R0,RLEN                                                          
         AR    R8,R0                                                            
*                                                                               
PDTF04   DS    0H                                                               
         CLI   0(R8),0             EOR                                          
         BE    PDTFX                                                            
*                                                                               
         CLI   RCODE,0                                                          
         BE    PDTFX                                                            
         CLI   RCODE,X'0B'                                                      
         BL    PDTF02                                                           
         CLI   RCODE,X'0F'                                                      
         BH    PDTF02                                                           
*                                                                               
         CLC   RDATE,BSTART        SPOT DATE VS START DATE                      
         BL    PDTF06                                                           
         CLC   RDATE,PREVWKRX      MUST BE BEFORE LAST ROTATOR WK END           
         BH    PDTF02                                                           
*                                                                               
PDTF06   DS    0H                                                               
         CLC   RDATE,PREVWKP       PICK UP SPOTS IN LAST                        
         BL    PDTF02              WEEK OF PREV MONTH                           
*                                  AND IF NOT MATCHED IN PREV MONTH             
         LR    R2,R8                                                            
PDTF08   DS    0H                                                               
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOR                                          
         BE    PDTF10              YES, SPOT IS OK                              
         CLI   0(R2),X'0B'         ANOTHER SPOT ELEM                            
         BL    PDTF08                                                           
         CLI   0(R2),X'0F'                                                      
         BNH   PDTF10              MEANS LAST SPOT NOT MATCHED                  
         CLI   0(R2),X'10'         AFFID ELEM                                   
         BNE   PDTF08                                                           
         USING AFFELEM,R2                                                       
         CLC   ADATE,BRDMON        TEST MATCHED IN PREV MONTH                   
         BL    PDTF02              YES, SKIP IT                                 
*                                                                               
*                             ELSE WILL BE MATCHED IN CURRENT MONTH             
*                                                                               
         MVC   RDATE,PREVWKR       RESET DATE TO LAST ROTATION WEEK             
*                                  OF PREVIOUS MONTH                            
         DROP  R2                                                               
*                                                                               
PDTF10   DS    0H                                                               
         B     PDTF02              NEX ELEM                                     
*                                                                               
PDTFX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                   FIND BUY ELEMENT                                            
         SPACE 3                                                                
GETEL    SR    R0,R0                                                            
GE2      IC    R0,RLEN                                                          
         AR    R8,R0                                                            
         CLI   RCODE,0                                                          
         BE    LSTEL                                                            
         CLI   RCODE,6                                                          
         BL    GE2                                                              
         CLI   RCODE,14                                                         
         BH    GE2                                                              
*                                  FOR POOL GET ELEM COUNT BY DATE              
         CLI   BUYKEY+3,X'FF'                                                   
         BNE   GE2C                                                             
         CLC   OLDDAT,RDATE                                                     
         BE    GE2B                                                             
         MVC   OLDDAT,RDATE                                                     
         MVI   SPCNT,0                                                          
*                                                                               
GE2B     EQU   *                                                                
         TM    RSTATUS,X'80'       MINUS                                        
         BNZ   GE2C                                                             
         IC    R1,SPCNT                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SPCNT                                                         
*                                                                               
GE2C     DS    0H                                                               
         CLC   RDATE,BSTART        SPOT DATE VS START DATE                      
         BNL   GE2W                OK IF NOT LOW                                
         CLI   BUYKEY+3,X'FF'      ELSE, FOR POOL BUYS ONLY                     
         BNE   GE2                                                              
         CLC   BSTART,BRDMON       IF START DATE = START OF MONTH               
         BNE   GE2                                                              
         CLC   RDATE,PREVWKP       PICK UP SPOTS IN LAST                        
         BL    GE2                 WEEK OF PREV MONTH                           
         ZIC   R2,BDSEDAY          IF OUT-OF-WEEK                               
         SRDL  R2,4                (FOR OUT OF WEEK START DAY                   
         SRL   R3,28               IS HIGHER THAN END)                          
         CR    R2,R3                                                            
         BNH   GE2                                                              
*                                  AND IF NOT MATCHED IN PREV MONTH             
         LR    R2,R8                                                            
GE2D     DS    0H                                                               
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOR                                          
         BE    GE2W                YES, SPOT IS OK                              
         CLI   0(R2),6             ANOTHER SPOT ELEM                            
         BL    GE2D                                                             
         CLI   0(R2),14                                                         
         BNH   GE2W                MEANS LAST SPOT NOT MATCHED                  
         CLI   0(R2),X'10'         AFFID ELEM                                   
         BNE   GE2D                                                             
         USING AFFELEM,R2                                                       
         CLC   ADATE,BRDMON        TEST MATCHED IN PREV MONTH                   
         BL    GE2                 YES, SKIP                                    
         DROP  R2                                                               
*                                                                               
GE2W     DS    0H                                                               
         CLC   RDATE,BEND                                                       
         BH    GE2                 OUT OF PERIOD-HIGH                           
         CLI   BUYKEY+3,X'FF'      TEST POL                                     
         BER   RE                                                               
*                                                                               
         TM    RSTATUS,X'80'       SKIP MINUS SPOT                              
         BNZ   GE2                                                              
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         IC    R4,DMINBTS                                                       
         IC    R3,TERMNAL                                                       
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'SPTDIR',KEY,KEY,((R3),0)         
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+14                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R3,TERMNAL                                                       
         IC    R4,DMINBTS                                                       
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'SPTFILE',               X        
               (R2),AIOABUY,((R3),DMWORK)                                       
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT1                                                                   
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(2,DMCB)                            
         CLM   R3,1,=AL1(BUFERR)   FOR BUFFER OVERFLOWS                         
         BNE   ERROR2                                                           
         MVC   33(4,R4),FULL       SHOW TYPE OF OVERFLOW                        
         MVI   32(R4),C'('                                                      
         MVI   37(R4),C')'                                                      
ERROR2   DS    0H                                                               
         SPACE 2                                                                
EXIT     EQU   *                                                                
EXXMOD   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                   PROCESS BUY ELEMENT                                         
         SPACE 3                                                                
PROCEL   NMOD1 0,PROCEL                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    R8,LASTELAD         SAVE A(ELEM)                                 
         XC    BYELEM,BYELEM                                                    
         CLI   BUYKEY+3,X'FF'                                                   
         BE    PR20                                                             
*                   NON-POOL ROUTINES                                           
         SPACE 1                                                                
*                                  GET NO. OF SPOTS                             
PR10     LR    R2,R8                                                            
         SR    R3,R3                                                            
         SR    R5,R5                                                            
         OI    BYSTAT,X'40'        SET PAID STATUS                              
         B     PR11A                                                            
PR11     ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOR                                          
         BE    PR12                                                             
         CLI   0(R2),6             TEST SPOT ELEM                               
         BL    PR11                                                             
         CLI   0(R2),15            AFFID                                        
         BH    PR11                                                             
         CLC   RDATE,2(R2)         TEST DATES EQUAL                             
         BNE   PR12                                                             
PR11A    SR    R5,R5                                                            
         IC    R5,7(R2)                                                         
         TM    6(R2),X'80'         TEST MINUS SPOT                              
         BZ    *+6                                                              
         LCR   R5,R5                                                            
         AR    R3,R5                                                            
         ST    R2,LASTELAD         SAVE A(LAST ELEM FOR THIS DATE)              
         OC    4(2,R2),4(R2)       TEST PAID                                    
         BNZ   *+12                                                             
         NI    BYSTAT,X'BF'        SET UNPAID                                   
         B     PR11B                                                            
*                                  ADD TO PAID TOTALS                           
         L     R1,GROSS                                                         
         A     R1,TAXAMT                                                        
         MR    R0,R5               X SPOTS                                      
         A     R1,PDGRS                                                         
         ST    R1,PDGRS                                                         
         L     R1,NET                                                           
         A     R1,TAXAMT                                                        
         MR    R0,R5               X SPOTS                                      
         A     R1,PDNET                                                         
         ST    R1,PDNET                                                         
*                                                                               
*                                                                               
PR11B    DS    0H                                                               
         B     PR11                                                             
PR12     DS    0H                                                               
         TM    RSTATUS,X'40'       SKIP MINUSED SPOT                            
         BNZ   PREXT                                                            
         LTR   R3,R3               TEST ANY SPOTS LEFT                          
         BNP   PREXT               NO - EXIT                                    
         STC   R3,BYNOWK                                                        
         STC   R3,BYUNACH                                                       
*                                  ADD TAX INTO TOTAL                           
         L     R1,TAXAMT                                                        
         MR    R0,R3               X NO. OF SPOTS                               
         A     R1,TAXTOT                                                        
         ST    R1,TAXTOT                                                        
         MVC   BYCOST,GROSS                                                     
         CLI   NETINV,C'Y'                                                      
         BNE   *+10                                                             
         MVC   BYCOST,NET                                                       
*                                  FOR THIS DATE                                
         MVC   BYPRD,BUYKEY+3                                                   
         MVC   BYPRD2,PGPRD                                                     
         B     PR30                                                             
*                   POOL ROUTINES                                               
PR20     CLI   RLEN,10             TEST ALLOCATED                               
         BNE   PR20B               YES                                          
         TM    RSTATUS,X'04'       HIATUS                                       
         BNZ   PREXT                                                            
         CLI   BPRD,0                                                           
         BNE   PREXT                                                            
         MVI   FULL,X'FF'          SET PRD = 'POL'                              
         MVI   FULL+1,0                                                         
         B     PR22                                                             
PR20B    DS    0H                                                               
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(1),RPPRD                                                    
         CLI   RLEN,18                                                          
         BNE   *+10                                                             
         MVC   FULL+1(1),RPPRD+4                                                
         SPACE 2                                                                
         CLI   BPRD,0                                                           
         BE    PR22                                                             
*                                  TEST PRDS                                    
         CLC   BPRD,FULL                                                        
         BNE   PREXT                                                            
         CLC   BPRD2,FULL+1        0 OR PRD CODE                                
         BE    PR22                                                             
         CLI   BPRD2,X'FF'         ALL PIGS                                     
         BNE   PREXT                                                            
         CLI   FULL+1,0                                                         
         BE    PREXT                                                            
*                                                                               
PR22     DS    0H                                                               
         OI    BYSTAT,X'02'        SET IS A POL SPOT                            
         MVI   BYNOWK,1            1 SPOT/ELEM                                  
         MVI   BYUNACH,1                                                        
         MVC   BYPRD(2),FULL       PRDS                                         
         IC    R0,BDTIME                                                        
         MVI   BDTIME,0            SET BDTIME TO 0                              
         TM    BDSTAT,X'80'        TEST NPW IN COST                             
         BZ    PR22B                                                            
         IC    R3,RPCOST           YES - SAVE NPW                               
         NI    RPCOST,X'03'                                                     
         OI    RPCOST,X'04'        SET NPW TO 1 FOR GETRATE                     
PR22B    DS    0H                                                               
         CLI   COUNTRY,C'C'        FOR CANADIAN NETWORK                         
         BNE   PR22D                                                            
         CLI   ICSMED,C'N'                                                      
         BNE   PR22D                                                            
         MVI   BDPURP,X'FD'        RETURN COSTS!                                
*                                                                               
PR22D    DS    0H                                                               
         CLI   PSEUDOPT,C'R'       FOR RESPONSE COUNT REQS                      
         BE    PR23                SKIP COSTS                                   
         GOTO1 VGETRATE,DMCB,(X'FF',SPOTS),(R9),(R8)                            
         XC    TAXAMT,TAXAMT                                                    
         CLI   COUNTRY,C'C'        FOR CANADIAN NETWORK                         
         BNE   PR22E                                                            
         CLI   ICSMED,C'N'                                                      
         BNE   PR22E                                                            
         CLI   TAXOPT,C'Y'                                                      
         BNE   PR23                                                             
         TM    RSTATUS,X'C0'       0 TAX FOR MINUS/MINUSED                      
         BNZ   PR23                                                             
         BAS   RE,CANTAX           DO SPECIAL CAN NETWORK TAX                   
         B     PR23                                                             
*                                                                               
PR22E    DS    0H                                                               
         CLI   TAXOPT,C'Y'                                                      
         BNE   PR23                                                             
         OC    BDNTAX,BDNTAX                                                    
         BZ    PR23                                                             
*                                  EXTRACT TAX FROM BUY                         
         MVC   DUB(4),NET                                                       
         MVC   DUB+5(2),BDNTAX                                                  
         XC    BDNTAX,BDNTAX                                                    
         GOTO1 (RF),(R1)           RECOMPUTE COST WITHOUT TAX                   
*                                                                               
         L     RF,DUB                                                           
         S     RF,NET                                                           
         ST    RF,TAXAMT                                                        
*                                                                               
         MVC   BDNTAX,DUB+5                                                     
PR23     DS    0H                                                               
         SPACE 1                                                                
         MVC   BYCOST,GROSS                                                     
         CLI   NETINV,C'Y'                                                      
         BNE   *+10                                                             
         MVC   BYCOST,NET                                                       
         STC   R0,BDTIME                                                        
         TM    BDSTAT,X'80'        TEST NPW IN COST (POL RADIO)                 
         BNZ   PR25                                                             
         MVC   BYSPT,SPCNT                                                      
*                                                                               
         L     RF,TAXTOT                                                        
         A     RF,TAXAMT                                                        
         ST    RF,TAXTOT                                                        
*                                                                               
         OC    RPAY,RPAY                                                        
         BZ    PR23D                                                            
*                             ADD TO PAID TOTALS                                
         L     R1,PDGRS                                                         
         A     R1,GROSS                                                         
         A     R1,TAXAMT                                                        
         ST    R1,PDGRS                                                         
         L     R1,PDNET                                                         
         A     R1,NET                                                           
         A     R1,TAXAMT                                                        
         ST    R1,PDNET                                                         
         OI    BYSTAT,X'40'    SET PAID                                         
*                                                                               
PR23D    DS    0H                                                               
         TM    RSTATUS,X'C0'       SKIP MINUS OR MINUSED SPOT                   
         BNZ   PREXT                                                            
         B     PR30                                                             
*                                                                               
PR25     DS    0H                  NPW IN COST - (POL RADIO)                    
         STC   R3,RPCOST           RESTORE NPW                                  
         MVC   DUB(3),RPCOST       SAVE COST                                    
         NI    DUB,X'03'           STRIP 6 BITS                                 
         SR    R3,R3                                                            
         LR    R2,R8                                                            
         OI    BYSTAT,X'40'                                                     
         B     PR25D                                                            
*                                                                               
PR25B    DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    PR25F                                                            
         CLI   0(R2),11                                                         
         BL    PR25B                                                            
         CLI   0(R2),12                                                         
         BH    PR25B                                                            
*                             COUNT SPOTS WITH SAME DATE,COST,ALLOC             
*                                                                               
         CLC   RDATE,2(R2)         DATE                                         
         BNE   PR25F                                                            
*                                                                               
         MVC   BYTE,RSTATUS                                                     
         XC    BYTE,6(R2)                                                       
         TM    BYTE,X'20'                                                       
         BNZ   PR25F               COST OVERRIDE STATUS NOT =                   
*                                                                               
         MVC   DUB+3(3),7(R2)                                                   
         NI    DUB+3,X'03'                                                      
         CLC   DUB(3),DUB+3                                                     
         BNE   PR25F               COST NOT =                                   
*                                                                               
         CLC   RLEN,1(R2)          ELEM LENTGH                                  
         BNE   PR25F                                                            
         CLI   RLEN,10                                                          
         BE    PR25D                                                            
         CLC   RPPRD,10(R2)        TEST SAME PRD ALLOC                          
         BNE   PR25F                                                            
         CLI   RLEN,14                                                          
         BE    PR25D                                                            
         CLC   14(1,R8),14(R2)     2N PRD                                       
         BNE   PR25F                                                            
*                                                                               
PR25D    DS    0H                                                               
         ZIC   R5,7(R2)            NPW                                          
         SRL   R5,2                                                             
         TM    6(R2),X'80'         NEG ELEM                                     
         BZ    *+6                                                              
         LCR   R5,R5                                                            
         AR    R3,R5                                                            
*                                                                               
         ST    R2,LASTELAD         ADDR OF LAST USED ELEM                       
*                                                                               
         OC    GROSS(8),GROSS      TREAT ZERO SPOT AS PAID                      
         BZ    PR25E                                                            
         OC    4(2,R2),4(R2)                                                    
         BNZ   *+12                                                             
         NI    BYSTAT,X'BF'        SET UNPAID                                   
         B     PR25E                                                            
*                                                                               
*                                  ADD TO PAID TOTALS                           
         L     R1,GROSS                                                         
         A     R1,TAXAMT                                                        
         MR    R0,R5               X SPOTS                                      
         A     R1,PDGRS                                                         
         ST    R1,PDGRS                                                         
         L     R1,NET                                                           
         A     R1,TAXAMT                                                        
         MR    R0,R5               X SPOTS                                      
         A     R1,PDNET                                                         
         ST    R1,PDNET                                                         
*                                                                               
*                                                                               
PR25E    DS    0H                                                               
         B     PR25B                                                            
*                                                                               
PR25F    DS    0H                                                               
         LTR   R3,R3               TEST ANY SPOTS LEFT                          
         BP    *+14                                                             
         XC    BYELEM,BYELEM                                                    
         B     PREXT                                                            
*                                                                               
         STC   R3,BYNOWK                                                        
         STC   R3,BYUNACH                                                       
*                                                                               
*                                  ADD TAX INTO TOTAL                           
         L     R1,TAXAMT                                                        
         MR    R0,R3               X NO. OF SPOTS                               
*                                                                               
         A     R1,TAXTOT                                                        
         ST    R1,TAXTOT                                                        
*                                                                               
*                   COMMON ROUTINES                                             
*                                                                               
PR30     DS    0H                                                               
*                                                                               
         OC    RPAY,RPAY           TEST PAID                                    
         BZ    PR31                NO = NO PROBLEM                              
         CLI   B0AFFDEL,C'N'       ALLOW UPDATE AFFIDS FOR PAID SPOTS?          
         BNE   PR31                                                             
         MVI   PSTOPT,C'X'         SET TO X = NO POSTING/B0 MESSAGE             
*                                                                               
PR31     TM    CBLHOPT,CBLHNHQ     IF CABLE HEAD REQ AND NEW MODE               
         BNO   PR32                                                             
         MVC   BYCBLNET,BUYKEY+8   LAST BYTE OF STATION                         
         NI    BYCBLNET,X'7F'      7 BITS ONLY - MAY BE NULLS                   
*                                                                               
PR32     DS    0H                                                               
         MVC   BYSDT,RDATE         SET DATE                                     
         LA    R0,2                TIMES                                        
         LA    R2,BDPROG                                                        
         LA    R3,BYSTIM                                                        
PR33     LH    R5,0(R2)                                                         
         LTR   R5,R5               END TIME 0                                   
         BNZ   *+14                                                             
         MVC   HALF,BDPROG                                                      
         LH    R5,HALF                                                          
         CH    R5,=H'600'                                                       
         BNL   *+8                                                              
         AH    R5,=H'2400'                                                      
         SR    R4,R4                                                            
         D     R4,=F'100'                                                       
         MH    R5,=H'60'                                                        
         AR    R4,R5               R4 = MINUTES                                 
         STC   R4,1(R3)                                                         
         SRL   R4,8                                                             
         STC   R4,0(R3)                                                         
         LA    R2,2(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,PR33                                                          
         MVC   BYDAY,BDDAY                                                      
         TM    BDSTAT2,X'80'       IF DAILY SKED                                
         BNZ   PR36                                                             
         CLI   RCODE,7             OR OTO                                       
         BE    PR36                                                             
         CLI   RCODE,12                                                         
         BE    PR36                                                             
         CLI   RCODE,8             OR OTO-FLIP                                  
         BE    PR36                                                             
         CLI   RCODE,13                                                         
         BNE   PR37                                                             
*                                  SET BYDAY TO SINGLE DAY                      
PR36     EQU   *                                                                
         CLC   RDATE,DATINBH       CHECK SAME AS BEFORE                         
         BE    PR36B                                                            
         MVC   DATINBH,RDATE                                                    
         GOTO1 VDATCON,DMCB,(2,RDATE),DUB                                       
*                                                                               
         GOTO1 VGETDAY,(R1),DUB,FULL                                            
*                                                                               
         SR    R4,R4                                                            
         IC    R4,DMCB             DAY OF WEEK                                  
         LA    R3,X'80'                                                         
         SRL   R3,1                                                             
         BCT   R4,*-4                                                           
         STC   R3,DAYOTH                                                        
*                                                                               
PR36B    MVC   BYDAY,DAYOTH                                                     
         MVC   BYEDT,BYSDT                                                      
         IC    R0,DMCB             DAY OF WEEK                                  
         LR    R1,R0                                                            
         SLL   R0,4                                                             
         OR    R0,R1                                                            
         STC   R0,BYSEDAY          START AND END DAY                            
         LA    R3,1                FOR OTO  NO. OF DAYS = 1                     
         TM    BDSTAT2,X'80'       UNLESS DAILY SKED                            
         BNZ   PR39                                                             
         CLC   BYDAY,BYTE2         TEST OTO DATE = ROTATOR START DAY            
         BNE   PR39                NO                                           
         MVC   BYDAY,BDDAY         YES - USE FULL ROTATOR                       
PR37     EQU   *                                                                
*                                  COMPUTE NO. OF DAYS                          
         SR    R3,R3                                                            
         IC    R4,BDDAY                                                         
         LA    R0,7                                                             
PR38     SRDL  R4,1                                                             
         SRL   R5,31                                                            
         AR    R3,R5                                                            
         BCT   R0,PR38                                                          
PR39     SR    R2,R2                                                            
         IC    R2,BYNOWK                                                        
         MR    R2,R2               X NO. PER WEEK                               
         MVC   FULL,BYSTIM                                                      
         L     R4,FULL                                                          
         SRDL  R4,16                                                            
         SRL   R5,16                                                            
         SR    R5,R4               END - START                                  
         LPR   R5,R5                                                            
         MR    R2,R5               X MINUTES                                    
         STC   R3,BYAMPW+1                                                      
         SRL   R3,8                                                             
         STC   R3,BYAMPW                                                        
         MVC   BYEST(2),BUYKEY+9                                                
         MVC   BYLEN,BDSEC                                                      
*                                                                               
PR40     DS    0H                                                               
         CLC   BYDAY,BDDAY         IS IT SINGLE DAY OTO OR DAILY SKED           
         BNE   PR40J                                                            
*                                  NO, NEED TO SET BYSEDAY, BYEDT, ETC          
         MVC   BYSEDAY,BDSEDAY     SET START/END DAYS                           
         CLI   BDSEDAY,0           TEST PRESENT                                 
         BH    PR40H                                                            
         MVC   BYTE,BYDAY          NO, FIGURE THEM OUT                          
         GOTO1 =A(SETSEDAY),DMCB,(RC),RR=RELO01                                 
         MVC   BYSEDAY,BYTE        START/END DAY RETURNED IN BYTE               
PR40H    DS    0H                                                               
         MVC   BYEDT,BYSDT         SET END DATE=START                           
         ZIC   R0,BYSEDAY          CALC DAYS IN ROTATOR                         
         LR    R5,R0                                                            
         SR    R4,R4                                                            
         SLDL  R4,28                                                            
         SRL   R5,28                                                            
         SR    R5,R4               R5 HAS NUMBER OF DAYS                        
         BZ    PR40J               TO ADD TO GET TO END DATE                    
         BP    *+8                                                              
         AH    R5,=H'7'            END LESS THAN START                          
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BYSDT),DUB                                       
         GOTO1 VADDAY,(R1),DUB,DUB,(R5)                                         
         GOTO1 VDATCON,(R1),DUB,(2,BYEDT)                                       
*                                                                               
PR40J    DS    0H                                                               
         CLC   BYEDT,BEND          IF OUT OF WEEK BUY EXTENDS PAST              
         BNH   PR40L               END OF PERIOD, SKIP BUY                      
*                                  IF MATCHED IN NEXT MONTH                     
         LR    R2,R8               (NO LONGER 'DEFERRED')                       
PR40J4   DS    0H                                                               
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOR                                          
         BE    PR40L               YES, SPOT IS OK                              
         CLI   0(R2),6             ANOTHER SPOT ELEM                            
         BL    PR40J4                                                           
         CLI   0(R2),14                                                         
         BNH   PR40L               MEANS LAST SPOT NOT MATCHED                  
         CLI   0(R2),X'10'         AFFID ELEM                                   
         BNE   PR40J4                                                           
         USING AFFELEM,R2                                                       
         CLC   ADATE,BEND          TEST MATCHED IN NEXT MONTH                   
         BNH   PR40L               NO, SPOT OK                                  
         MVI   BYSDT,0             YES, SKIP SPOT                               
*                                  AND SUBTRACT FROM PAID AND TAX TOTS          
         ZIC   RF,BYNOWK           NUMBER PER WEEK                              
         TM    BYSTAT,X'40'        TEST PAID                                    
         BZ    PR40K                                                            
         L     R1,GROSS                                                         
         A     R1,TAXAMT                                                        
         MR    R0,RF               X SPOTS                                      
         LCR   R1,R1                                                            
         A     R1,PDGRS                                                         
         ST    R1,PDGRS                                                         
         L     R1,NET                                                           
         A     R1,TAXAMT                                                        
         MR    R0,RF               X SPOTS                                      
         LCR   R1,R1                                                            
         A     R1,PDNET                                                         
         ST    R1,PDNET                                                         
PR40K    DS    0H                                                               
         L     R1,TAXAMT                                                        
         MR    R0,RF               X SPOTS                                      
         LCR   R1,R1                                                            
         A     R1,TAXTOT                                                        
         ST    R1,TAXTOT                                                        
*                                                                               
         B     PREXT                                                            
         DROP  R2                                                               
*                                                                               
PR40L    DS    0H                                                               
         L     R0,GROSS                                                         
         CLI   NETINV,C'Y'                                                      
         BNE   *+8                                                              
         L     R0,NET                                                           
         LTR   R0,R0                                                            
         BNM   PR40M                                                            
         LPR   R0,R0                                                            
         ST    R0,DUB                                                           
         MVC   BYCOST,DUB                                                       
         OI    BYSTAT,X'01'        NEGATIVE                                     
*                                                                               
PR40M    DS    0H                                                               
         CLI   CENTS,C'Y'                                                       
         BE    PR40P                                                            
         MVI   DUB,0                                                            
         MVC   DUB(4),BYCOST                                                    
         L     R0,DUB                                                           
         LA    RF,100                                                           
         BAS   RE,PRDIV                                                         
         ST    R1,DUB                                                           
         MVC   BYCOST,DUB                                                       
*                                                                               
*                                  ADD TO SPECIAL REPS                          
PR40P    DS    0H                                                               
         OC    BDREP,BDREP                                                      
         BZ    PR44                                                             
*                                                                               
         LA    R4,REPLST                                                        
PR42     DS    0H                                                               
         MVC   HALF,0(R4)                                                       
***      NI    HALF,X'7F'          STRIP HOB                                    
         CLC   HALF,BDREP                                                       
         BE    PR42B                                                            
         LA    R0,REPLST+L'REPLST      TEST LIST FULL                           
         CR    R4,R0                                                            
         BNL   PR44                                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    PR42D                                                            
         LA    R4,6(R4)                                                         
         B     PR42                                                             
*                                                                               
PR42B    DS    0H                                                               
         MVC   FULL,2(R4)                                                       
         L     R0,FULL                                                          
         L     RF,GROSS                                                         
         CLI   NETINV,C'Y'                                                      
         BNE   *+8                                                              
         L     RF,NET                                                           
         ZIC   R1,BYNOWK                                                        
         MR    RE,R1                                                            
         AR    R0,RF                                                            
         ST    R0,FULL                                                          
         MVC   2(4,R4),FULL                                                     
         B     PR43M                                                            
*                                                                               
PR42D    DS    0H                                                               
         MVC   0(2,R4),BDREP                                                    
         L     RF,GROSS                                                         
         CLI   NETINV,C'Y'                                                      
         BNE   *+8                                                              
         L     RF,NET                                                           
         ZIC   R1,BYNOWK                                                        
         MR    RE,R1                                                            
         ST    RF,FULL                                                          
         MVC   2(4,R4),FULL                                                     
*                                                                               
         CLI   PROGPROF+12,C'N'    SKIP SYND TEST                               
         BE    PR44                                                             
*                                                                               
*                                  SEE IF SYND REP                              
*                                                                               
         L     RF,ABUFFX           SAVE BUYREC AT END OF BUFFER                 
         SH    RF,=H'150'                                                       
         MVC   0(150,RF),BUYREC                                                 
*                                                                               
         USING REPREC,R9                                                        
         MVI   REPKEY,C'0'                                                      
         MVC   REPKEY+1(16),REPKEY                                              
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,ICSMED                                                   
         MVC   REPKAGY,AGYALPHA                                                 
*                                                                               
         GOTO1 VRCPACK,DMCB,(C'U',0(R4)),REPKREP                                
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'STATION',IOAREA,IOAREA,     X        
               (TERMNAL,0)                                                      
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BNZ   PR43                IF REP NOT FOUND, ASSUME NOT SYND            
*                                                                               
         CLI   RSYND,C'S'                                                       
         BNE   *+8                                                              
         OI    0(R4),X'80'         SET AS SYND REP                              
*                                  RESTORE BUYREC                               
PR43     DS    0H                                                               
         USING BUYRECD,R9                                                       
         L     RF,ABUFFX                                                        
         SH    RF,=H'150'                                                       
         MVC   BUYREC(150),0(RF)                                                
*                                                                               
PR43M    DS    0H                                                               
         TM    0(R4),X'80'         TEST SYNDD REP                               
         BZ    *+8                                                              
         OI    BYSTAT,X'20'        SET AS SYND BUY                              
*                                                                               
PR44     DS    0H                                                               
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    PR44A1                                                           
         CLI   BDCIND,BDCNTP       NTP SPOT                                     
         BNE   PR44B                                                            
         B     PR44A2                                                           
PR44A1   TM    BDCIND,X'FE'        NTP SPOT                                     
         BNZ   PR44B                                                            
PR44A2   CLC   AGYALPHA,=C'DF'     DANCER ONLY                                  
         BNE   PR44B                                                            
         OI    BYSTAT,X'20'                                                     
PR44B    DS    0H                                                               
         CLI   BDMGDATE,0          MG                                           
         BE    *+8                                                              
         OI    BYSTAT,X'04'                                                     
*                                                                               
*                                  TEST FOR ORBITS                              
         LA    R2,BDELEM                                                        
PR46     DS    0H                                                               
         CLI   0(R2),X'67'                                                      
         BE    PR48                                                             
         CLI   0(R2),0                                                          
         BE    PR49                                                             
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PR46                                                             
PR48     DS    0H                                                               
         MVI   ORBSW,C'Y'          SET HAVE ORBS                                
         MVI   PSTOPT,C'N'         SUPPRESS POSTING                             
*                                                                               
PR49     DS    0H                  SET FILM NUMBERS                             
         CLI   TRAFMBF,C'Y'        TEST OPTION                                  
         BE    *+12                                                             
         CLI   TRAFMBF,C'B'        TEST OPTION                                  
         BNE   PR49H                                                            
         LR    RF,R8               LOOK FOR FILM ASSIGN ELEMS                   
         B     PR49D                                                            
*                                                                               
PR49B    DS    0H                                                               
         CLI   0(RF),0             EOR                                          
         BE    PR49H                                                            
         CLI   0(RF),6             ON ANY NEW SPOT ELEM                         
         BL    PR49D                                                            
         CLI   0(RF),14                                                         
         BNH   PR49H               DONE WITH THIS SPOT                          
         CLI   0(RF),X'18'         TRAFFIC ELEM                                 
         BNE   PR49D                                                            
         USING TRACID,RF                                                        
         MVC   BYFILM(4),TRACCSQ   FILM 1 AND FILM 2                            
         B     PR49H                                                            
         DROP  RF                                                               
*                                                                               
PR49D    DS    0H                                                               
         ZIC   R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     PR49B                                                            
*                                                                               
PR49H    DS    0H                                                               
*                                  DONT RESTORE R8                              
PREXT    EQU   *                                                                
         L     R8,LASTELAD         RESTORE A(ELEM)                              
         XIT1  REGS=(R8)                                                        
         SPACE 2                                                                
PRDIV    DS    0H                                                               
         SR    R1,R1                                                            
         SRDL  R0,31                                                            
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         SPACE 2                                                                
*        ROUTINE FOR CANADIAN NETWORK TAX                                       
*                                                                               
CANTAX   NTR1                                                                   
         LA    R2,BDELEM                                                        
*                                                                               
CANTAX4  DS    0H                                                               
         CLI   0(R2),0             EOR                                          
         BE    CANTAXX                                                          
         CLI   0(R2),X'69'         TAX ELEM                                     
         BE    CANTAX6                                                          
         ZIC   R0,1(R2)                                                         
         LTR   R0,R0                                                            
         BNP   CANTAXX                                                          
         AR    R2,R0                                                            
         B     CANTAX4                                                          
*                                                                               
CANTAX6  DS    0H                                                               
         MVC   TAXAMT,2(R2)        TAX IN DOLLARS AND CENTS                     
*                                                                               
CANTAXX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SETSEDAY - ROUTINE TO SET START-END DAY FROM DAY ROTATOR               
*                   ON INPUT BYTE HAS ROTATOR, ON OUTPUT HAS                    
*                   START DAY IN HIGH NIBBLE, END IN LOW,                       
***********************************************************************         
         SPACE 2                                                                
SETSEDAY NMOD1 0,SETSE                                                          
         L     RC,0(R1)                                                         
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         LA    RF,SEDLST                                                        
         LA    R3,7                                                             
         LA    R5,SEDLST-1                                                      
*                                                                               
SSED4    DS    0H                                                               
         ZIC   RE,0(RF)                                                         
         EX    RE,SEDTM            IS DAY ACTIVE?                               
         BZ    SSED6                                                            
         LR    RE,RF               YES                                          
         SR    RE,R5                                                            
         LTR   R0,R0               IS IT THE FIRST?                             
         BNZ   *+6                                                              
         LR    R0,RE               SET AS FIRST                                 
         LR    R1,RE               LASTEST IS LAST                              
*                                                                               
SSED6    DS    0H                                                               
         LA    RF,1(RF)                                                         
         BCT   R3,SSED4                                                         
*                                                                               
         SLL   R0,4                                                             
         OR    R0,R1               GET INTO 8 BITS                              
         STC   R0,BYTE             RETURN IN BYTE                               
*                                  SET BUY SPOT END DATE                        
SSEDX    DS    0H                                                               
         XIT                                                                    
*                                                                               
SEDLST   DC    X'40201008040201'                                                
SEDTM    TM    BYTE,0                                                           
         TITLE 'INVBLD - ICS INVOICE BUFFER MODULE'                             
         DS    0D                                                               
INVBLD   NMOD1 0,INVBLD                                                         
         SPACE 3                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T215FFD,RA                                                       
         USING INVELEM,R9                                                       
         USING IELEMD,R8                                                        
         SPACE 3                                                                
         MVI   NETINV,C'Y'                                                      
         MVI   IDNUM,0                                                          
         MVI   RSET,0                                                           
         MVI   RSETQ,0                                                          
         XC    SAVIKEY,SAVIKEY                                                  
*        XC    INOLIST,INOLIST                                                  
         LA    RE,INOLIST                                                       
         LA    RF,L'INOLIST                                                     
         XCEFL                                                                  
*                                                                               
         XC    IBTOT,IBTOT                                                      
         MVI   IBACT,0                                                          
         MVI   IBOK,C'Y'                                                        
         XC    SVIHDEL,SVIHDEL                                                  
         MVC   QREP,=C'000'        NO-OP REP OPTION                             
*                                  SET BINSRCH PARS FOR FILMTAB                 
         SR    R0,R0                                                            
         LA    R1,FLMTAB                                                        
         SR    R2,R2                                                            
         LA    R3,FLMTABEL                                                      
         LA    R4,FLMTABKL                                                      
         LA    R5,MAXFLMS                                                       
         STM   R0,R5,FLMTPARS                                                   
*                                  BUILD KEY                                    
         XC    KEY,KEY                                                          
         LA    R7,KEY                                                           
         USING INVKEY,R7                                                        
         MVI   INVKEY,X'0B'                                                     
         MVC   INVKAM,HKEY+1       AGENCY/MED                                   
         MVC   INVKCLT,BCLT                                                     
         MVC   INVKSTA,BMS+2                                                    
         MVC   INVKDAT,BRDMON                                                   
         DROP  R7                                                               
         L     R8,AFINV                                                         
         BAS   RE,IBHIGH                                                        
         B     *+8                                                              
IB2      BAS   RE,IBSEQ                                                         
         CLC   KEY(09),KEYSAVE     TEST FOR RIGHT MONTH                         
         BH    IB10                NO- OUT                                      
*                                                                               
         CLC   KEY(10),SAVIKEY     TEST SAME INVOICE                            
         BE    IB3A                YES - OK                                     
         CLI   NEWSW,C'Y'          IF OLD RECORDS                               
         BE    *+14                                                             
         CLC   KEY(9),SAVIKEY      TEST ONLY 9 BYTES                            
         BE    IB3A                                                             
         MVI   IDNUM,0             NEW INVOICE - RESET CTLS                     
         MVC   SAVIKEY,KEY                                                      
         XC    IBTOT,IBTOT                                                      
         MVI   IBACT,0                                                          
         MVI   IBOK,C'Y'                                                        
         XC    SVIHDEL,SVIHDEL                                                  
         ZIC   RF,RSET                                                          
         LA    RF,1(RF)                                                         
         STC   RF,RSET                                                          
*                                                                               
         ZIC   R1,KEY+10           SEQ NO. (NEW POS)                            
         CLI   NEWSW,C'Y'                                                       
         BE    *+8                                                              
         IC    R1,KEY+9            SEQ NO. (OLD POS)                            
         SR    R0,R0                                                            
         D     R0,=F'10'           /10 = SET NO                                 
         STC   R1,RSETQ                                                         
         B     IB3A4                                                            
*                                                                               
IB3A     DS    0H                  TEST DIFFERENT PHYSICAL SET                  
         ZIC   R1,KEY+10           SEQ NO. (NEW)                                
         CLI   NEWSW,C'Y'                                                       
         BE    *+8                                                              
         IC    R1,KEY+9            SEQ NO. (OLD POS)                            
         SR    R0,R0                                                            
         D     R0,=F'10'           /10 = SET NO                                 
         STC   R1,BYTE                                                          
         CLC   BYTE,RSETQ                                                       
         BE    IB3A4                                                            
*                                                                               
         STC   R1,RSETQ                                                         
         ZIC   RF,RSET             BUMP RECORD SET NO                           
         LA    RF,1(RF)                                                         
         STC   RF,RSET                                                          
         MVI   IDNUM,0                                                          
*                                                                               
IB3A4    DS    0H                                                               
         BAS   RE,CLRIO                                                         
         BAS   RE,IBGET                                                         
*                                                                               
         SR    R0,R0                                                            
         LA    R9,IOAREA+24                                                     
*                                                                               
         CLI   NEWSW,C'Y'          FOR NEW MODE                                 
         BNE   IBN8                                                             
         CLI   KEY+10,0            ONLY FOR FIRST REC OF SET                    
         BNE   IBN8                                                             
         XC    SVIHDEL,SVIHDEL                                                  
*                                                                               
IBN4     DS    0H                  LOOK FOR INVOICE HEADER ELEM                 
         CLI   0(R9),X'05'                                                      
         BE    IBN7                                                             
         CLI   0(R9),0                                                          
         BE    IBN7D                                                            
         ZIC   R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     IBN4                                                             
*                                                                               
IBN7     DS    0H                                                               
         B     IB20                NOTE- CODE FOR HEADER ELEM IS                
*                                  NOT IN LINE - RETURNS TO IB3L                
IBN7D    DS    0H                  HERE IF NO HEADER ELEM                       
         CLI   EASISW,C'Y'         TEST DOING EASI ONLY                         
         BNE   IBN8                                                             
         MVI   IBOK,C'N'                                                        
         DROP  R9                                                               
*                                                                               
IBN8     DS    0H                                                               
         CLI   IBOK,C'Y'           IF INVOICE NOT OK                            
         BNE   IB2                 SKIP IT                                      
         SR    R0,R0                                                            
         LA    R9,IOAREA+24                                                     
*                                                                               
         TM    QBYID,QBYIDYQ                                                    
         BO    IB3B                                                             
         CLC   QREP,=C'000'                                                     
         BNH   IB3L                                                             
*                                                                               
IB3B     DS    0H                  FIND ID TRANSLATION ELEMS                    
IB3D     DS    0H                                                               
         CLI   0(R9),X'02'         ID SEQ ELEM                                  
         BE    IB3H                                                             
         CLI   0(R9),0                                                          
         BE    IB3L                                                             
IB3F     DS    0H                                                               
         ZIC   R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     IB3D                                                             
*                                                                               
IB3H     DS    0H                                                               
         USING INVIDEL,R9                                                       
         TM    QBYID,QBYIDYQ       IF DOING BY ID                               
         BZ    IB3H4                                                            
         CLC   INVID,BUYID         TEST ID NUMBER                               
         BE    IB3H6                                                            
         B     IB3F                                                             
IB3H4    DS    0H                                                               
         CLC   QREP,INVID          OR REP CODE                                  
         BNE   IB3F                                                             
IB3H6    DS    0H                                                               
         MVC   IDNUM,INVINO        SAVE INTERNAL ID NUM                         
         DROP  R9                                                               
*                                                                               
IB3L     DS    0H                                                               
         SR    R0,R0                                                            
         LA    R9,IOAREA+24                                                     
         B     IB5                                                              
IB4      SR    R0,R0                                                            
         IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
IB5      CLI   0(R9),X'B1'                                                      
         BE    IB5A                                                             
         CLI   0(R9),X'B0'         CONTROL ELEM                                 
         BE    IB15                                                             
*                                                                               
         CLI   0(R9),4             FILM TRANSLATION ELEM                        
         BNE   IB5A2                                                            
         USING INVFLMEL,R9                                                      
         LA    R6,WORK                                                          
         XC    WORK,WORK                                                        
         USING FLMTABD,R6                                                       
*                                                                               
         MVC   FLMIDNO+1(1),IHDID-IHDELEM+SVIHDEL INT INV ID NO                 
         CLI   FLMIDNO+1,0         IN 2ND BYTE IF PRESENT                       
         BH    IB50D               BECAUSE WON'T HAVE MULTIPLE RSETS            
         MVC   FLMIDNO+1(1),RSET   ELSE RSET IN 2ND                             
         OI    FLMIDNO,X'40'       RSET INDICATOR                               
*                                                                               
IB50D    DS    0H                                                               
         OI    FLMIDNO,X'80'       SET FROM INVBLD, NOT GETSNV                  
*                                                                               
         MVC   FLMICOD,INVFID      INTERNAL ID NO                               
         MVC   FLMSEQ,INVFSQ       CMML SEQ                                     
         MVC   FLMCOD,INVFCD       FILM CODE                                    
*                                                                               
         GOTO1 VBINSRCH,FLMTPARS,(1,FLMTABD)                                    
         OC    1(3,R1),1(R1)                                                    
         BNZ   IB5A2                                                            
         LA    R3,BUFERR                                                        
         MVC   FULL,=C'INV '       KIND OF OVERFLOW                             
         B     IBERROR                                                          
         DROP  R9                                                               
         DROP  R6                                                               
*                                                                               
IB5A2    DS    0H                                                               
         CLI   0(R9),0             EOR                                          
         BNE   IB4                                                              
         BAS   RE,SAVINV           SAVE INVOICE NUMBER IN LIST                  
         B     IB2                                                              
*                                                                               
         USING INVELEM,R9                                                       
IB5A     CLC   INVDAT,BSTART       CHECK DATES                                  
         BL    IB4                                                              
         CLC   INVDAT,BEND                                                      
         BH    IB2                OUT                                           
*                                                                               
         MVC   IDAT,INVDAT                                                      
*                                  GET DAY OF WEEK                              
         CLC   INVDAT,DATINBH                                                   
         BE    IB5E                                                             
         MVC   DATINBH,INVDAT                                                   
         GOTO1 VDATCON,DMCB,(2,INVDAT),DUB                                      
         SPACE 1                                                                
         GOTO1 VGETDAY,(R1),DUB,FULL                                            
         SPACE 1                                                                
         SR    R2,R2                                                            
         IC    R2,DMCB             DAY OF WEEK                                  
         LA    R3,X'80'                                                         
         SRL   R3,1                                                             
         BCT   R2,*-4                                                           
         STC   R3,DAYOTH                                                        
IB5E     MVC   IDAY,DAYOTH                                                      
*                                  CHECK PRODUCT(S)                             
IB6      DS    0H                                                               
         CLI   BPRD,0                                                           
         BE    IB7                                                              
         CLI   BPRD,X'FF'                                                       
         BE    IB7                                                              
         CLI   BPRD2,0                                                          
         BNE   IB6D                TREAT 2 PRD REQ SPECIALLY                    
         CLC   INVPRD,BPRD                                                      
         BNE   IB4                                                              
         TM    INVSTAT,X'40'       TEST ESTIMATE INV                            
         BNZ   IB7                                                              
         TM    INVSTAT,X'08'       TEST ID INV                                  
         BNZ   IB8D                                                             
         CLI   INVPRD2,0           NO 2ND PRD                                   
         BNE   IB4                                                              
         B     IB7                                                              
IB6D     DS    0H                  2 PRD REQ                                    
         TM    INVSTAT,X'48'       BYPASS EST AND ID ITEMS                      
         BNZ   IB4                                                              
         CLC   BPRD,INVPRD                                                      
         BNE   IB4                                                              
         CLC   BPRD2,INVPRD2       2ND PRD                                      
         BE    IB7                                                              
         CLI   BPRD2,X'FF'         ALL PIGS                                     
         BNE   IB4                                                              
         CLI   INVPRD2,0                                                        
         BE    IB4                                                              
IB7      DS    0H                                                               
         CLI   BEST,0                                                           
         BE    IB8D                                                             
         TM    INVSTAT,X'40'         IF REQ BY EST                              
         BZ    IB8D                 ALLOW ONLY INVS NOT BY EST                  
         CLC   INVPRD2,BEST          OR FOR THINVS EST                          
         BNE   IB4                                                              
         B     IB9                                                              
*                                                                               
IB8D     DS    0H                                                               
         TM    QBYID,QBYIDYQ       IF BY ID                                     
         BZ    IB9                                                              
         CLI   NEWIDSW,C'Y'        IF NEW IDS                                   
         BE    IB9                 SKIP OLD ID TEST                             
         TM    QBYID,QBYIDNQ+QBYIDHQ   IF ID=NONE OR ID=HOME                    
         BZ    IB8F                                                             
         TM    INVSTAT,X'08'       BYPASS ALL BY ID                             
         BZ    IB9                                                              
         B     IB4                                                              
IB8F     DS    0H                                                               
         TM    INVSTAT,X'08'                                                    
         BZ    IB4                                                              
         CLC   IDNUM,INVPRD2       NUST BE RIGHT ID                             
         BNE   IB4                                                              
*                                                                               
IB9      DS    0H                                                               
         MVC   IINVID+1,IHDID-IHDELEM+SVIHDEL   INTERNAL INVOICE NO             
         CLI   IINVID+1,0          IN 2ND BYTE IF PRESENT                       
         BH    IB9B                (WON'T HAVE MULTIPLE RSETS)                  
         MVC   IINVID+1(1),RSET    ELSE RSET IN 2ND                             
         OI    IINVID,X'40'        RSET INDICATOR                               
*                                                                               
IB9B     DS    0H                                                               
         OI    IINVID,X'80'        SET FROM INVBLD, NOT GETSNV                  
         MVC   ITIM,INVTIM                                                      
         MVC   ILEN,INVLEN                                                      
         MVC   ICOST(1),INVCOSTX   COST EXTENSION (1ST BYTE)                    
         MVC   ICOST+1(3),INVCOST  COST                                         
         MVC   ISTAT,INVSTAT                                                    
         MVC   IPRD,INVPRD                                                      
         TM    INVSTAT,X'40'       TEST INPUT WITH ESTIMATE                     
         BZ    *+14                                                             
         MVC   IEST,INVPRD2                                                     
         B     IB9C                                                             
         TM    INVSTAT,X'08'       TEST INPUT WITH ID                           
         BZ    *+14                                                             
         MVC   IID,INVPRD2                                                      
         B     IB9C                                                             
         MVC   IPRD2,INVPRD2                                                    
*                                                                               
IB9C     DS    0H                            CONVERT TIME TO MINUTES            
         MVI   IBACT,C'Y'          SET THIS INVOICE ACTUALLY USED               
         SR    R0,R0               TOTAL USED AMOUNTS                           
         ICM   R0,15,ICOST                                                      
         TM    ISTAT,X'01'         TEST NEGATIVE                                
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         A     R0,IBTOT                                                         
         ST    R0,IBTOT                                                         
*                                            CONVERT TIME TO MINUTES            
         MVC   HALF,ITIM                                                        
         LH    R5,HALF                                                          
         SR    R4,R4                                                            
         D     R4,=F'100'                                                       
         MH    R5,=H'60'                                                        
         AR    R5,R4                                                            
         STC   R5,ITIM+1                                                        
         SRL   R5,8                                                             
         STC   R5,ITIM                                                          
         CLI   NETREC,C'Y'                                                      
         BNE   *+8                                                              
         OI    ISTAT2,X'10'        NET ITEM                                     
IB9F     DS    0H                                                               
         TM    INVSTAT2,X'80'      TEST TO IGNORE FILM ERRORS                   
         BZ    *+8                                                              
         OI    ISTAT2,X'02'        **NOTE DIFFERENT BIT IN ISTAT2               
         TM    INVSTAT2,X'20'      CONTRACT TRADE ITEM?                         
         BZ    *+8                                                              
         OI    ISTAT2,ISTCTRQ                                                   
         CLI   INVELEM+1,13        TEST ANY FILM CODES                          
         BNH   IB9H                NO                                           
         MVC   IFILM(2),INVFILM    SET FILMS                                    
*                                                                               
IB9H     DS    0H                                                               
         LA    R8,IEL$MLEN(R8)                                                  
         C     R8,ABUFFX                                                        
         BNH   IB4                                                              
         LA    R3,BUFERR           BUFFER OVERFLOW                              
         MVC   FULL,=C'INV '       KIND OF OVERFLOW                             
         B     IBERROR                                                          
         SPACE 1                                                                
IB10     BCTR  R8,R0                                                            
         ST    R8,ALINV                                                         
*                                                                               
         GOTO1 =A(GOGETSNV),DMCB,(RC),RR=RELO01  NOW TRY FOR SNV'S              
         BE    IB10A                                                            
         CHI   R3,1                AD-ID ERROR?                                 
         BE    ERRADID             YES                                          
         B     IBERROR             NO, SOME OTHER ERROR                         
*                                                                               
IB10A    XC    HOLDS,HOLDS                                                      
         CLC   ALINV,AFINV         TEST ANY INVOICES                            
         BH    *+8                 YES                                          
         MVI   NETINV,C'N'         NO - NOT AN NET INV                          
*                                                                               
         CLI   TRAFRCLO,C'Y'       TEST TO CHECK RECALL DATES                   
         BE    IB10D                                                            
         CLI   TRAFIDSC,C'Y'       OR IF INVALID FILM IS DISCREP                
         BNE   IB12                                                             
*                                  YES, SET THEM IN FLMTAB                      
*        NOTE- FLMTAB IS IN ORDER BY FILM CODE                                  
*              SO THIS CODE IS NOT REALLY SO INEFFICIENT                        
*                                                                               
IB10D    DS    0H                                                               
         L     R6,AFLMTAB          FILM TABLE                                   
         USING FLMTABD,R6                                                       
         ICM   R0,15,FLMTPARS+8 NO. OF ENTRIES                                  
         BZ    IB12                                                             
*                                                                               
IB11     DS    0H                                                               
         GOTO1 GETFLM,DMCB,FLMTABD                                              
         LA    R6,FLMTABEL(R6)     NEXT ENTRY                                   
         BCT   R0,IB11                                                          
*                                  NOW CHECK VS INVOICE DATES                   
         L     R8,AFINV                                                         
*                                                                               
IB11D    DS    0H                                                               
         C     R8,ALINV                                                         
         BNL   IB12                                                             
         CLI   IFILM,0             TEST ANY FILM                                
         BE    IB11G                                                            
*                                                                               
         L     R6,AFLMTAB                                                       
         USING FLMTABD,R6                                                       
         L     R0,FLMTPARS+8                                                    
*                                                                               
IB11E    DS    0H                                                               
         CLC   FLMIDNO,IINVID      TEST FROM RIGHT INVOICE                      
         BNE   IB11F                                                            
         CLC   FLMICOD,IFILM       1ST FILM                                     
         BE    IB11E4                                                           
         CLI   IFILM2,0            TEST ANY 2ND FILM                            
         BE    IB11F                                                            
         CLC   FLMICOD,IFILM2      2ND FILM                                     
         BNE   IB11F                                                            
*                                                                               
IB11E4   DS    0H                                                               
         TM    ISTAT2,X'02'        TEST TO IGNORE FILM ERRORS                   
         BNZ   IB11E6                                                           
*                                                                               
         OC    FLMSEQ,FLMSEQ       IS IT A VALID FILM                           
         BNZ   *+8                 YES, OK                                      
         OI    ISTAT,ISTINVF       NO, SET INVALID BIT                          
*                                                                               
         OC    FLMRCL,FLMRCL       ANY RECALL DATE?                             
         BZ    IB11E5                                                           
         CLC   IDAT,FLMRCL         ELSE, TEST VS RECALL DATE                    
         BNH   *+8                                                              
         OI    ISTAT2,ISTRCRL      SET NO GOOD FOR MATCH                        
IB11E5   DS    0H                                                               
         CLC   IDAT,FLMRLSE        TEST VS RELEASE DATE                         
         BNL   *+8                                                              
         OI    ISTAT2,ISTRCRL      SET NO GOOD FOR MATCH                        
*                                                                               
         CLI   FLMPRDS,X'FF'       CHECK FILM VALID FOR PRD                     
         BE    IB11E6              ALL PRDS VALID                               
         LA    R1,5                                                             
         LA    R2,FLMPRDS                                                       
IB11E5A  CLC   BPRD,0(R2)                                                       
         BE    IB11E6                                                           
         LA    R2,1(R2)                                                         
         BCT   R1,IB11E5A                                                       
         OI    ISTAT,ISTPRDF       INVALID FILM FOR PRODUCT                     
*                                                                               
IB11E6   DS    0H                                                               
         CLI   IFILM2,0            IF NO 2ND FILM                               
         BE    IB11G               DONE                                         
*                                                                               
IB11F    DS    0H                                                               
         LA    R6,FLMTABEL(R6)     NEXT ENTRY                                   
         BCT   R0,IB11E                                                         
*                                                                               
IB11G    DS    0H                                                               
         LA    R8,IEL$MLEN(R8)     NEXT INVOICE ITEM                            
         B     IB11D                                                            
         DROP  R6                                                               
*                                                                               
IB12     DS    0H                                                               
         B     IBEXIT                                                           
*                                                                               
IB15     DS    0H                  CONTROL ELEMENT                              
         USING ICTLELEM,R9                                                      
         MVI   NETREC,C'Y'                                                      
         TM    ICTLCTL,ICTLNET     TEST NET INV                                 
         BNZ   *+12                 YES                                         
         MVI   NETREC,C'N'                                                      
         MVI   NETINV,C'N'         NO - SET OFF NET INDICATOR                   
*                                                                               
         TM    ICTLCTL,ICTLRES     IS IT A RESPONSE "INVOICE"                   
         BZ    IB16                                                             
         CLI   PSEUDOPT,C'R'        RESPONSE REQ?                               
         BE    IB4                                                              
         MVI   IBOK,C'N'                                                        
         B     IB2                                                              
*                                                                               
IB16     DS    0H                                                               
         TM    ICTLCTL,ICTLMCT     IS IT A MCT "INVOICE"                        
         BZ    IB17                                                             
         CLI   PSEUDOPT,C'M'       NO, PSEUDO REQ                               
         BE    IB4                                                              
         MVI   IBOK,C'N'                                                        
         B     IB2                                                              
*                                                                               
IB17     DS    0H                                                               
         CLI   PSEUDOPT,C'N'       NORMAL REQ                                   
         BE    IB4                                                              
         MVI   IBOK,C'N'                                                        
         B     IB2                                                              
         SPACE 2                                                                
*        INVOICE HEADER ELEMENT                                                 
IB20     DS    0H                                                               
         USING IHDELEM,R9                                                       
         MVC   SVIHDEL,0(R9)       SAVE WHOLE ELEM                              
*                                                                               
         CLI   EASISW,C'Y'         TEST DOING EASI INVS ONLY                    
         BNE   IB22                                                             
         CLI   IHDRDT,0            MUST BE EASI INV                             
         BNE   IB24                                                             
         MVI   IBOK,C'N'           SET NOT OK                                   
         B     IB2                                                              
*                                                                               
IB22     DS    0H                                                               
         CLI   EASISW,C'N'         TEST DOING NON-EASI ONLY                     
         BNE   IB24                                                             
         CLI   IHDRDT,0            MUST NOT BE EASI INV                         
         BE    IB24                                                             
         MVI   IBOK,C'N'           SET NOT OK                                   
         B     IB2                                                              
*                                                                               
IB24     DS    0H                  ID'S (CONTRACTS)                             
         TM    QBYID,QBYIDYQ       IF DOING IDS                                 
         BZ    IB30                                                             
         CLI   NEWIDSW,C'Y'        AND NEW ID MODE                              
         BNE   IB30                                                             
         TM    QBYID,QBYIDNQ+QBYIDHQ  IF ID=NONE OR ID=HOME                     
         BZ    IB28                                                             
         CLI   IHDCON,C' '         MUST NOT HAVE ID                             
         BNH   IB30                                                             
         MVI   IBOK,C'N'           SET NOT OK                                   
         B     IB2                                                              
IB28     DS    0H                  TEST HAVE RIGHT ID                           
         OC    BUYID,WORK                                                       
         OC    IHDCON,WORK                                                      
         CLC   BUYID,IHDCON                                                     
         BE    IB30                                                             
         MVI   IBOK,C'N'           SET NOT OK                                   
         B     IB2                                                              
*                                                                               
IB30     DS    0H                  LOOK FOR PRODUCT IN HEADER ELEM              
         CLI   IHDPRD,X'FF'        PRD VARIOUS                                  
         BE    IB34                                                             
         CLI   IHDPRD,0                                                         
         BE    IB34                                                             
         CLI   BPRD,X'FF'                                                       
         BE    IB34                                                             
         CLI   BPRD,0                                                           
         BE    IB34                                                             
*                                   BE GENEROUS WITH PIGGIES                    
         CLC   BPRD,IHDPRD                                                      
         BE    IB34                                                             
         CLC   BPRD,IHDPRD2                                                     
         BE    IB34                                                             
         CLI   BPRD2,0             REJECT IF NO PIGGY REQ                       
         BE    IB33                                                             
         CLC   BPRD2,IHDPRD                                                     
         BE    IB34                                                             
         CLC   BPRD2,IHDPRD2                                                    
         BE    IB34                                                             
IB33     DS    0H                                                               
         MVI   IBOK,C'N'                                                        
         B     IB2                                                              
*                                                                               
IB34     DS    0H                  ESTIMATE                                     
         CLI   IHDEST,0                                                         
         BE    IB36                                                             
         CLI   BEST,0                                                           
         BE    IB36                                                             
         CLC   BEST,IHDEST                                                      
         BE    IB36                                                             
         MVI   IBOK,C'N'                                                        
         B     IB2                                                              
*                                                                               
IB36     DS    0H                                                               
         B     IB3L                                                             
         DROP  R9                                                               
         SPACE 2                                                                
*        SAVINV - SAVE INVOICE NUMBER IN LIST                                   
         SPACE 2                                                                
SAVINV   DS    0H                                                               
         CLI   IBACT,0             TEST INVOICE USED                            
         BER   RE                                                               
*                                                                               
         NTR1                                                                   
         LA    R9,SVIHDEL                                                       
         USING IHDELEM,R9                                                       
*                                                                               
         CLI   IHDINV,C' '         SKIP IF NO INVOICE NUMBER                    
         BNH   SVIX                                                             
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'IHDINV),WORK                                            
         OC    IHDINV,WORK         OR INVOICE WITH SPACES                       
         LA    RF,INOLIST                                                       
         LA    R3,INOLIST+(INOLSTL*MAXINOS)-1   SET EOL                         
         USING INOLSTD,RF                                                       
*                                                                               
SVI4     DS    0H                                                               
         CR    RF,R3               TEST AT END                                  
         BL    SVI5                NO-OK                                        
         SH    RF,=H'17'           YES - MERGE INTO LAST ONE                    
         MVC   INOLINV,=10C'*'                                                  
         XC    INOLDAT,INOLDAT     CLEAR DATE                                   
         B     SVI5D                                                            
SVI5     DS    0H                                                               
         CLC   IHDINV,INOLINV       TEST ALREADY HAVE                           
         BNE   SVI6                                                             
         CLC   IHDIDT,INOLDAT                                                   
         BNE   SVI6                                                             
SVI5D    DS    0H                                                               
         ZIC   R0,15,INOLAMT       ADD TO INVOICE TOTAL                         
         A     R0,IBTOT                                                         
         STCM  R0,15,INOLAMT                                                    
         B     SVI8                DONE                                         
SVI6     DS    0H                                                               
         CLI   0(RF),0                                                          
         BE    SVI7                                                             
         LA    RF,INOLSTL(RF)      NEXT SLOT                                    
         B     SVI4                                                             
SVI7     DS    0H                  ADD NEW ENTRY                                
         MVC   INOLINV,IHDINV                                                   
         MVC   INOLDAT,IHDIDT                                                   
         MVC   INOLAMT,IBTOT                                                    
*                                                                               
SVI8     DS    0H                                                               
SVIX     DS    0H                                                               
         XIT1                                                                   
         DROP  R9                                                               
         DROP  RF                                                               
         SPACE 2                                                                
*        GETFLM- TEST FILM OK TO MATCH                                          
         SPACE 2                                                                
GETFLM   CLI   TRAFRCLO,C'Y'       TEST TO CHECK RECALL DATE                    
         BNER  RE                                                               
*                                                                               
         NTR1                                                                   
         L     R6,0(R1)            A(FLMTAB ENTRY)                              
         USING FLMTABD,R6                                                       
*                                                                               
         LR    RF,R6               IF THIS CODE SAME AS LAST                    
         SH    RF,=Y(FLMTABEL)                                                  
         C     RF,AFLMTAB                                                       
         BL    GF08                                                             
         CLC   FLMCOD,FLMCOD-FLMTABD(RF)                                        
         BNE   GF08                                                             
         MVC   FLMRCL,FLMRCL-FLMTABD(RF)   USE PREVIOUS RECALL DATE             
         MVC   FLMRLSE,FLMRLSE-FLMTABD(RF) AND RELEASE DATE                     
         MVC   FLMSEQ,FLMSEQ-FLMTABD(RF)   AND SEQ NUMBER                       
         MVC   FLMPRDS,FLMPRDS-FLMTABD(RF) AND PRDS                             
         B     GFX                                                              
*                                                                               
GF08     DS    0H                                                               
         XC    FLMRLSE,FLMRLSE     CLEAR RECALL/RELEASE DATES                   
         MVC   FLMRCL,=X'FFFF'                                                  
         XC    KEY,KEY             READ FOR FILM                                
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(1),HKEY+1                                                  
         OC    KEY+3(2),SVTRACLT   USE TRAFFIC OVERRIDE CLIENT                  
         BNZ   *+10                IF ANY                                       
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(8),FLMCOD     FILM CODE                                    
         BAS   RE,IBHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GFX                                                              
*                                                                               
         BAS   RE,IBGET                                                         
         LA    R5,IOAREA+24                                                     
         CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                BAD FILM REC                                 
*                                                                               
GF10     DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    GFX                                                              
         CLI   0(R5),X'10'         CMML DATA ELEM                               
         BE    GF12                                                             
         CLI   0(R5),X'20'         CMML PRD LIST                                
         BE    GF15                                                             
*                                                                               
GF11     ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GF10                                                             
*                                                                               
GF12     DS    0H                                                               
         USING CMLDTAEL,R5                                                      
         GOTO1 VDATCON,DMCB,(3,CMLRCL),(2,FLMRCL)                               
         GOTO1 (RF),(R1),(3,CMLRLSE),(2,FLMRLSE)                                
         B     GF11                                                             
*                                                                               
GF15     DS    0H                                                               
         USING CMLPRDEL,R5                                                      
         XC    FLMPRDS,FLMPRDS                                                  
         MVI   FLMPRDS,X'FF'       DEFAULT TO ALL PRDS                          
         CLI   TRAFRCLO,C'N'       TEST CHECKING RECALL DATES/PRDS              
         BE    GF11                DO SKIP CONVERTS                             
         ZIC   R1,1(R5)                                                         
         AHI   R1,-3               CODE+LEN+1 FOR EX                            
         BM    GF11                ELEM TOO SHORT                               
         CHI   R1,4                MAX 5 PRDS IN TABLE (-1 FOR EX)              
         BNH   *+8                                                              
         LA    R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLMPRDS(0),2(R5)       SAVE PRDS                                 
         B     GF11                                                             
*                                                                               
GFX      DS    0H                                                               
         DROP  R6                                                               
         XIT1                                                                   
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
IBSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         B     IBDIR                                                            
         SPACE 2                                                                
IBHIGH   MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     IBDIR                                                            
         SPACE 2                                                                
IBDIR    NTR                                                                    
         IC    R4,DMINBTS                                                       
         IC    R3,TERMNAL                                                       
         CLI   KEY,X'0A'           IS THIS A TRAFFIC RECORD                     
         BE    IBDIR2                                                           
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'SPTDIR',KEY,KEY,((R3),0)         
         B     IBDMCK                                                           
*                                                                               
IBDIR2   GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'TRFDIR',KEY,KEY,((R3),0)         
         B     IBDMCK                                                           
         SPACE 1                                                                
*                             CLEAR IOAREA                                      
         SPACE 1                                                                
CLRIO    LA    R1,IOAREA                                                        
         LA    R0,8                                                             
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
         BR    RE                                                               
         SPACE 3                                                                
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 2                                                                
IBGET    MVC   COMMAND,=C'GETREC'                                               
         B     IBFIL                                                            
         SPACE 2                                                                
IBFIL    NTR                                                                    
         LA    R2,KEY+14                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R3,TERMNAL                                                       
         IC    R4,DMINBTS                                                       
         CLI   KEY,X'0A'           IS THIS A TRAFFIC RECORD                     
         BE    IBDFIL2                                                          
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'SPTFILE',               X        
               (R2),IOAREA,((R3),DMWORK)                                        
         B     IBDMCK                                                           
*                                                                               
IBDFIL2  GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'TRFFILE',               X        
               (R2),IOAREA,((R3),DMWORK)                                        
         B     IBDMCK                                                           
         SPACE 3                                                                
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 2                                                                
IBDMCK   MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   IBDMERRS                                                         
         XIT1                                                                   
         SPACE 2                                                                
IBDMERRS L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     IBERROR                                                          
         SPACE 3                                                                
ERRADID  L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   8(28,R4),=C'AD-ID NOT SUPPORTED IN MATCH'                        
         B     IBEXIT                                                           
*                  EXITS FROM PROGRAM                                           
         SPACE 2                                                                
IBERROR  L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(2,DMCB)                            
         CLM   R3,1,=AL1(BUFERR)   FOR BUFFER OVERFLOWS                         
         BNE   IBERR2                                                           
         MVC   33(4,R4),FULL       SHOW TYPE OF OVERFLOW                        
         MVI   32(R4),C'('                                                      
         MVI   37(R4),C')'                                                      
IBERR2   DS    0H                                                               
         SPACE 2                                                                
IBEXIT   EQU   *                                                                
IBEXXMOD XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
*                                                                               
*        GOGETSNV - CALL GETSNV FOR NEW-STYLE SNV'S                             
*                                                                               
*******************************************************************             
*                                                                               
GOGETSNV NMOD1 0,GETSNV                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         SR    R3,R3               CLEAR ERROR SWITCH                           
         CLI   I2YPROF+9,C'Y'      TEST USING SNV'S                             
         BC    0,GOSNVX            **NO-OP**                                    
         LA    R4,X                                                             
         USING SBLD,R4                                                          
         XC    SBLD(SBLBLEN),SBLD                                               
*                                                                               
         MVI   SBLMODE,C'$'        SET $MAT MODE                                
         MVC   SBLAGYMD,HKEY+1     AGYMED                                       
         MVC   SBLCLT,BCLT         CLIENT                                       
         MVC   SBLSTA,BMS+2        STATION                                      
         TM    CBLHOPT,CBLHNHAQ    IF CBH REQ AND NEW MODE                      
         BNO   *+8                 AND 'ALL NETS'                               
         NI    SBLSTA+2,X'80'      CLEAR NETWORK                                
         TM    CBLHOPT,CBLHNMQ                                                  
         BZ    *+8                                                              
         MVI   SBLCMOD,C'Y'        CABLE HEAD MODE                              
         MVC   SBLPRD,BPRD         PRD 1                                        
         MVC   SBLPRD2,BPRD2       PRD 2                                        
         MVC   SBLEST,BEST         EST                                          
         MVC   SBLMOS,BRDMON       START DATE OF BRD MONTH                      
         MVC   SBLSDAT,BSTART      START                                        
         MVC   SBLEDAT,BEND        END                                          
         MVC   SBLBUYID,BUYID      CONTRACT/ID                                  
         TM    QBYID,QBYIDHQ       IF ID=HOME                                   
         BZ    *+10                                                             
         MVC   SBLBUYID,=CL12'NONE'  ID=NONE                                    
*                                                                               
         MVC   SBLEASI,EASISW      EASI/NOEASI CONTROL                          
*                                                                               
         MVI   SBLMCT,C'Y'         MCT CONTROL                                  
         CLI   PSEUDOPT,C'M'                                                    
         BE    *+8                                                              
         MVI   SBLMCT,C'N'                                                      
*                                                                               
         MVI   SBLRESP,C'Y'        RESPONSE CONTROL                             
         CLI   PSEUDOPT,C'R'                                                    
         BE    *+8                                                              
         MVI   SBLRESP,C'N'                                                     
*                                                                               
         MVC   SBLACOM,VCOMFACS    A(COMFACS)                                   
         MVC   SBLARCUP,VRECUP     A(RECUP)                                     
         MVC   SBLABSRC,VBINSRCH   A(BINSRCH)                                   
         MVC   SBLARCPK,VRCPACK    A(RCPACK)                                    
         LA    RF,GENOLD                                                        
         A     RF,=A(NINVIOA-GENOLD)                                            
         ST    RF,SBLAIO           A(IOAREA)                                    
         L     RF,ALINV                                                         
         LA    RF,1(RF)                                                         
         ST    RF,SBLAITB          CURRENT SLOT IN INVOICE TABLE                
         MVC   SBLAITBX,ABUFFX     A(END OF TABLE AREA)                         
         LA    RF,FLMTPARS         FILM TAB BINSRCH PARMS                       
         ST    RF,SBLAFTBP                                                      
         LA    RF,INOLIST                                                       
         ST    RF,SBLAINOL         A(INOLST)                                    
         MVI   SBLMXINS,MAXINOS    MAXIMUM INVOICES FOR INOLST                  
*                                                                               
         GOTO1 =V(GETSNV),DMCB,SBLD,RR=RELO01                                   
         TM    SBLSTAT,SBLADID     HAVE AD-ID ON INVOICE?                       
         BZ    *+12                NO, CONTINUE                                 
         LA    R3,1                                                             
         B     GOSNVX                                                           
*                                                                               
         TM    SBLSTAT,SBLINVQ     IF FOUND ANY INVOICE ITEMS                   
         BZ    GOSNV20                                                          
*                                                                               
         MVI   NETINV,C'N'         SET NET SWITCH                               
         TM    SBLSTAT,SBLNETQ                                                  
         BZ    *+8                                                              
         MVI   NETINV,C'Y'         NOTE- LAST INV SETS SWITCH                   
*                                  TEST OVERFLOWS/ERRORS                        
GOSNV20  DS    0H                                                               
         TM    SBLSTAT,SBLITOVQ+SBLFTOVQ+SBLIDOVQ                               
         BZ    GOSNV22                                                          
         LA    R3,BUFERR                                                        
         MVC   FULL,=C'INV '       KIND OF OVERFLOW                             
         B     GOSNVX                                                           
*                                                                               
GOSNV22  DS    0H                                                               
         L     RF,SBLIEND          END OF TABLE                                 
         BCTR  RF,0                BACK UP ONE BYTE                             
         ST    RF,ALINV            FOR ALINV                                    
         DROP  R4                                                               
*                                                                               
GOSNVX   DS    0H                                                               
         LTR   R3,R3               SET CC                                       
         XIT1  REGS=(R3)                                                        
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
BUYRECD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
* SPTRCMML                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPTRCMML                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPMATWKN                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'125SPMAT01   01/09/07'                                      
         END                                                                    
