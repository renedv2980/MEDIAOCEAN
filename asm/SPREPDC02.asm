*          DATA SET SPREPDC02  AT LEVEL 177 AS OF 08/22/05                      
*PHASE SPDC02C                                                                  
*INCLUDE FINDOUT                                                                
         SPACE 1                                                                
* QOPT1 = Y MEANS OUTPUT IS TO TAPE                                             
* QOPT2 = Y MEANS PRINT BUY LINE COMMENTS                                       
* QOPT3 = Y MEANS PRINT NETWORK AFFILIATE                                       
* QOPT4 = Y MEANS SUPPRESS MULTI-DISTRIBUTOR LOGIC                              
* QOPT5 = Y MEANS PRINT THE CHANNEL                                             
* USRSW1 = Y MEANS THERE IS A B9 PROFILE *                                      
*                                                                               
* LEV 18 AUG05/86 - CHANGE FOTABS CSECT SIZE                                    
         TITLE 'SPDC02 - MEDIA CALENDAR - APPLICATION'                          
SPDC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPDC02                                                         
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         USING SPDC02,RB,R8                                                     
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         SPACE 2                                                                
* CONTROL SECTION *                                                             
         SPACE 1                                                                
         CLI   MODE,PROCBUY                                                     
         BE    SP100               BUILD BUFFALO RECORDS                        
         BH    CNTRL10             DO THE 'LAST' BREAKS                         
         SPACE 1                                                                
* THE 'FRST' BREAKS *                                                           
         SPACE 1                                                                
         CLI   MODE,MKTFRST                                                     
         BE    SP090               MARKET LEVEL MULTI-DISTRIBUTOR               
         CLI   MODE,ESTFRST                                                     
         BH    SP080               MKT GRP LEVEL MULTI-DISTRIBUTOR              
         BE    SP070               SAVE REQUEST DATES                           
         CLI   MODE,PRDFRST                                                     
         BE    SP050               FORCE A NEW PAGE                             
         CLI   MODE,CLTFRST                                                     
         BE    SP025               NEW PAGE AND NEW HEADER                      
         CLI   MODE,REQFRST                                                     
         BE    SP000               INITIALIZE BUFFALO RECORDS                   
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
         ZAP   REQCOUNT,=P'1'                                                   
         LA    R0,SPDCHDHK                                                      
         ST    R0,HEADHOOK                                                      
         STM   R8,RB,SPDCR8B                                                    
         B     EXIT                                                             
         SPACE 1                                                                
* THE 'LAST' BREAKS *                                                           
         SPACE 1                                                                
CNTRL10  DS    0H                                                               
         CLI   MODE,MKTLAST                                                     
         BE    SP200               PRINT THE MEDIA CALENDAR                     
         CLI   MODE,REQLAST                                                     
         BE    SP500               UPDATE THE REQUEST COUNTER                   
         CLI   MODE,RUNLAST                                                     
         BE    CLS                 CLOSE THE TAPE, IF ANY                       
EXIT     XIT1                                                                   
         SPACE 1                                                                
         DS    0D                                                               
PATCH1   DS    CL16                                                             
         EJECT                                                                  
* SP000 - THIS SECTION INITIALIZES BUFFALO RECORDS, RELOCATES     *             
*  EXTERNAL ROUTINES & TABLES AND DETERMINES IF OUTPUT IS TO TAPE *             
         SPACE 1                                                                
SP000    DS    0H                                                               
         L     R5,=V(FINDOUT)                                                   
         ST    R5,VFNDOUT          RELOCATE FINDOUT MODULE                      
         L     R5,=V(GDAREA)                                                    
         ST    R5,VGDAREA                                                       
         L     R5,=V(BUFFALOC)     RELOCATE BUFFALO                             
         ST    R5,VBUFFALO                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',(R5)                                        
         L     R5,=V(MULTID)                                                    
         ST    R5,VMULTID          RELOCATE MULTI-DISTRIBUTOR CSECT             
         L     RE,ADAGY                                                         
         USING AGYHDRD,RE                                                       
         CLI   AGYPROF+7,C'C'      IS THIS A CANADIAN AGENCY?                   
         BNE   SP005                NO.                                         
         MVI   CANAGY,C'Y'          YES.                                        
         DROP  RE                                                               
         SPACE 1                                                                
SP005    DS    0H                                                               
         CLI   QOPT5,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT5(1),PROGPROF+2                                              
*                                                                               
         CLC   =CL2'BD',QAGY       IS THIS AGENCY BBDO?                         
         BNE   *+14                 NO.                                         
         CLC   =CL2'JS',QCLT       IS THIS CLIENT SCHLITZ (JS)?                 
         BE    SP015                YES.                                        
         CLC   =CL2'JW',QAGY       IS THIS JWT?                                 
         BNE   EXIT                 NO. EXIT                                    
         LA    R0,NOSPRD           NUMBER OF SCHLITZ PRODUCTS                   
         LA    RF,SCHLITZ          SCHLITZ PRD TABLE                            
SP010    DS    0H                                                               
         CLC   QCLT(2),0(RF)       IS THIS A SCHLITZ CLIENT ID?                 
         BE    SP015                YES.                                        
         LA    RF,3(RF)                                                         
         BCT   R0,SP010                                                         
         B     EXIT                                                             
SP015    DS    0H                                                               
         MVI   QOPT1,C'Y'          OUTPUT SHOULD BE TO TAPE                     
         B     EXIT                                                             
         EJECT                                                                  
* SP025 - THIS SECTION RESETS BUFFALO RECORDS AND READS THE *                   
*  B9 PROFILE TO DETERMINE IF THIS IS DISTRIBUTOR REQUEST   *                   
         SPACE 1                                                                
SP025    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         L     R5,VBUFFALO                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R5)                                      
         MVI   USRSW1,C'N'                                                      
         CLI   QMGR,C' '           ALLOW NORMAL REQUESTS                        
         BE    EXIT                                                             
         CLI   QOPT1,C'Y'          IS THE OUTPUT TO TAPE?                       
         BE    EXIT                 YES. NO MULTI-DISTRIBUTORS                  
         CLI   QOPT4,C'Y'          SUPPRESS MULTI-DISTRIBUTOR LOGIC?            
         BE    EXIT                 YES.                                        
         GOTO1 VMULTID,DMCB,(RA)                                                
         B     EXIT                                                             
         SPACE 2                                                                
* SP050 - FORCE A NEW PAGE *                                                    
         SPACE 1                                                                
SP050    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
* SP070 - SAVE THE REQUEST DATES *                                              
         SPACE 1                                                                
SP070    DS    0H                                                               
*                                                                               
         CLI   PROGPROF+5,0        B9 PROFILE OVERRIDE STUFF                    
         BNE   SP071                NO SPECIAL TESTS IF THERE                   
*                                                                               
         CLI   USRSW1,C'X'         RESET FOR B9 PRESENCE                        
         BNE   *+8                                                              
         MVI   USRSW1,C'Y'                                                      
         CLI   USRSW1,C'Y'         CHECK FOR B9 PRESENCE                        
         BNE   SP071                                                            
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         CLI   ERTLSCHM,C'*'       SEE IF EST QUALIFIES                         
         BH    SP071               YES, HAS RETAIL SCHEME                       
         BE    SP070D              NO, NO SCHEME                                
         CLC   EPROF+1(2),=C'  '   ELSE LOOK WHERE IT USED TO BE                
         BNE   SP071                                                            
*                                                                               
SP070D   DS    0H                                                               
         MVI   USRSW1,C'X'         NO-NEGATE B9 PRESENCE AND REMEMBER           
         DROP  RF                                                               
*                                                                               
SP071    GOTO1 DATCON,DMCB,QSTART,(2,RSTART)                                    
         GOTO1 DATCON,DMCB,QEND,(2,REND)                                        
         B     EXIT                                                             
         SPACE 2                                                                
* SP080 - FINDOUT ALL DISTRIBUTORS FOR THIS MARKET GROUP *                      
         SPACE 1                                                                
SP080    DS    0H                                                               
         CLI   USRSW1,C'Y'         IS THERE A B9 PROFILE?                       
         BNE   EXIT                 NO. EXIT                                    
         GOTO1 VMULTID,DMCB,(RA)                                                
         B     EXIT                                                             
         SPACE 2                                                                
* SP090 - FINDOUT ALL DISTRIBUTORS FOR THIS MARKET *                            
         SPACE 1                                                                
SP090    DS    0H                                                               
         L     RE,ADMARKET                                                      
         PACK  DUB,2(4,RE)         PACK THE MARKET NUMBER                       
         CVB   R0,DUB                                                           
         STH   R0,BINMKT           SAVE BINARY MARKET NUMBER                    
         MVI   USRSW2,C'N'                                                      
         L     R5,VBUFFALO                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R5)                                      
         MVI   RCSUBPRG,1                                                       
         XC    SVGDOUTA,SVGDOUTA                                                
         XC    SVGDTAB,SVGDTAB                                                  
         CLI   PROGPROF+4,C'Y'                                                  
         BNE   EXIT                                                             
         CLI   USRSW1,C'Y'         IS THERE A B9 PROFILE?                       
         BNE   EXIT                 NO.                                         
         GOTO1 VMULTID,DMCB,(RA)                                                
*                                                                               
         L     R2,VGDAREA                                                       
         USING GDSECT,R2                                                        
*                                                                               
         CLI   RCSUBPRG,2          IS THIS MULTI-DISTRIBUTOR?                   
         BNE   EXIT                 NO.                                         
         MVI   GDOUTNO,1            YES. GET 1ST DISTRIBUTOR OUTLET             
*                                                                               
         L     RF,ADEST            NO SCHEME ALWAYS DO CORPORATE                
         USING ESTHDR,RF                                                        
         CLI   PROGPROF+5,0        FORCE DOESN'T NEED SCHEME                    
         BNE   SP90D                                                            
         CLI   ERTLSCHM,C'*'                                                    
         BH    SP90D                                                            
         BE    SP91                                                             
         CLC   EPROF+1(2),=C'  '                                                
         BE    SP91                                                             
         DROP  RF                                                               
*                                                                               
SP90D    DS    0H                                                               
         CLC   GDNSCHMS,=F'2'      CORP ONLY                                    
         BL    SP91                NEVER SUPPRESS IT                            
*                                                                               
         CLI   PROGPROF+3,C'N'     SUPPRESS RETAIL CORP                         
         BNE   *+8                                                              
         MVI   GDOUTNO,2                                                        
*                                                                               
SP91     CLC   QESTEND,SPACES      IS THIS A SINGLE ESTIMATE                    
         BNE   SP95                 NO.                                         
         L     RF,ADEST            FOR A SPECIFIC ESTIMATE                      
         USING ESTHDR,RF                                                        
         MVC   GDSCHM,ERTLSCHM                                                  
         CLI   ERTLSCHM,C'*'       SCHEME                                       
         BH    *+10                                                             
         MVC   GDSCHM,EPROF+1      OLD SPOT FOR SCHEME                          
         DROP  RF                                                               
SP95     DS    0H                                                               
         LA    R7,SVGDTAB                                                       
         XC    SVGDTAB,SVGDTAB                                                  
SP95LP   GOTO1 VFNDOUT,DMCB,(R2)                                                
         CLC   GDOUTAD,=F'0'       ARE WE DONE YET?                             
         BE    EXIT                 YES.                                        
         CLC   GDSCHM,SPACES                                                    
         BE    *+14                                                             
         OC    GDSHRP,GDSHRP                                                    
         BZ    SP95A                                                            
         MVC   0(4,R7),GDOUTAD     SAVE THE ADDRESS IN GDOUTAD                  
         LA    R7,4(R7)                                                         
SP95A    ZIC   RF,GDOUTNO                                                       
         LA    RF,1(RF)                                                         
         STC   RF,GDOUTNO                                                       
         B     SP95LP                                                           
         EJECT                                                                  
* SP100 - THIS SECTION PROCESSES EACH *                                         
*  BUY AND BUILDS THE BUFFALO RECORDS *                                         
         SPACE 1                                                                
SP100    DS    0H                                                               
         CLI   USRSW2,C'Y'                                                      
         BE    EXIT                                                             
         SPACE 1                                                                
         L     R7,ADBUY                                                         
         USING BUYRECD,R7                                                       
         MVC   SVKEST,BUYKEST      SAVE ESTIMATE                                
         SPACE 1                                                                
* BYPASS SPILL MARKETS                                                          
         CLI   PROGPROF+1,C'Y'     PRINT SPILL                                  
         BE    SP102                                                            
         LA    RF,SVBUYKEY+4       SET UP FOR SPILL MARKET                      
         TM    SVBUYKEY,X'0C'      DIFFERENT POS. IF ID POINTER                 
         BNO   *+8                                                              
         LA    RF,SVBUYKEY+6                                                    
         CLC   4(5,R7),0(RF)       SPILL ONLY IF MARKETS ARE                    
         BNE   EXIT                NOT EQUAL                                    
         SPACE 1                                                                
SP102    LA    R3,24(R7)           POINT TO FIRST ELEMENT                       
         USING BDELEM,R3                                                        
         XC    SAVDA,SAVDA                                                      
         CLI   QOPT2,C'N'                                                       
         BE    SP110                                                            
         CLI   QOPT2,C'Y'          DO WE WANT BUY LINE COMMENTS?                
         BE    *+12                 YES.                                        
         CLI   PROGPROF,C'Y'       IS THIS A 'COMMENTS' AGENCY?                 
         BNE   SP110                NO.                                         
         LR    R4,R3                                                            
SP105    DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    SP110                                                            
         CLI   0(R4),X'66'                                                      
         BNE   SP105                                                            
         MVC   SAVDA,KEY+14                                                     
SP110    DS    0H                                                               
         LR    R4,R3                                                            
         USING REGELEM,R4                                                       
SP115    DS    0H'0'                                                            
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             IS THIS THE END OF THE RECORD?               
         BE    EXIT                                                             
         CLI   0(R4),6             IS THIS A BRAND BUY ELEMENT?                 
         BL    SP115                                                            
         CLI   0(R4),8                                                          
         BNH   SP125                                                            
         CLI   0(R4),X'0B'         IS THIS POL BUY ELEMENT?                     
         BL    SP115                                                            
         CLI   0(R4),X'0D'                                                      
         BH    SP115                                                            
         SPACE 1                                                                
* POL BUYS *                                                                    
         SPACE 1                                                                
         CLI   RSTATUS,4           IS THIS A HIATUS SPOT?                       
         BE    SP115                YES.                                        
         TM    RSTATUS,X'C2'       SPOT MINUSED OR MADE GOOD                    
         BNZ   SP115                YES.                                        
         CLI   RLEN,X'0E'          SKIP ALL UNALLOCATED SPOTS                   
         BL    SP115                                                            
         CLI   QPGR,C' '           ANY PRODUCT GROUPS?                          
         BE    SP119                NO.                                         
         CLC   =C'POL',QPRD        IS THIS A PRDGRP POL REQ?                    
         BNE   SP119                NO.                                         
         L     R5,SVPRDADR         GET ADDR OF PRDGRP LIST                      
SP116    DS    0H                                                               
         CLC   RPPRD,5(R5)         IS THIS PRD IN PRDGRP?                       
         BE    SP119                YES.                                        
         CLI   RLEN,18             DOES THIS REG ELT HAVE PIGGYBACK?            
         BL    SP117                NO.                                         
         CLC   14(1,R4),5(R5)      IS THE PIGGYBACK IN PRDGRP?                  
         BE    SP119                YES.                                        
SP117    DS    0H                                                               
         CLC   0(2,R5),6(R5)       IS NEXT PRD IN PRDGRP?                       
         BNE   SP115                NO. FORGET THIS ELEMENT                     
         LA    R5,6(R5)             YES. KEEP LOOKING                           
         B     SP116                                                            
         SPACE 1                                                                
SP119    DS    0H                                                               
         MVC   BUFPRD,RPPRD                                                     
         LH    RF,=H'1'                                                         
         CLI   QMED,C'R'                                                        
         BNE   SP120                                                            
         TM    BDSTAT,X'80'        IS THERE MORE THAN 1 POL RADIO BUY?          
         BZ    SP130                                                            
         MVC   WORD(3),RPCOST                                                   
         L     RF,WORD                                                          
         SRL   RF,26                                                            
         C     RF,=F'0'                                                         
         BE    SP115                                                            
SP120    DS    0H                                                               
         CLI   RSTATUS,X'80'       IS THIS A MINUS SPOT?                        
         BNE   SP130                                                            
         LNR   RF,RF                                                            
         B     SP130                                                            
         SPACE 1                                                                
* BRAND BUY *                                                                   
         SPACE 1                                                                
SP125    DS    0H                                                               
         XC    BUFACC,BUFACC                                                    
         CLI   RSTATUS,4           IS THIS A HIATUS SPOT?                       
         BE    SP115                YES. SKIP IT                                
         CLI   RLEN,X'0A'                                                       
         BNE   SP115                                                            
SP126    CLI   RNUM,0              ARE THERE ANY BRAND SPOTS                    
         BE    SP115                NO.                                         
         MVC   BUFPRD,BUYKPRD                                                   
         ZIC   RF,RNUM                                                          
         CLI   RSTATUS,X'80'       IS THIS A MINUS SPOT?                        
         BNE   *+6                  NO.                                         
         LNR   RF,RF                                                            
         A     RF,BUFACC           FULL DATE AT A TIME                          
         ST    RF,BUFACC                                                        
         CLI   10(R4),7                                                         
         BNE   SP130                                                            
         CLC   1(3,R4),11(R4)                                                   
         BNE   SP130                                                            
         LA    R4,10(R4)                                                        
         B     SP126                                                            
         SPACE 1                                                                
SP130    DS    0H                                                               
         ST    RF,BUFACC                                                        
         CLC   RSTART,RDATE                                                     
         BH    SP115                                                            
         CLC   RDATE,REND                                                       
         BH    SP115                                                            
         CLI   PROGPROF+4,C'Y'                                                  
         BNE   SP132                                                            
         OC    BUFACC,BUFACC                                                    
         BZ    SP132                                                            
* BUILD UP PARAMETER - MGR AND ACT                                              
         MVI   SVPRGP4,C'Y'                                                     
*                                                                               
         LA    R6,SVGDTAB                                                       
         L     R5,0(R6)                                                         
         USING GDOUTD,R5                                                        
SP130LP  DS    0H                                                               
         XC    WORKMGR,WORKMGR                                                  
         XC    WORKACT,WORKACT                                                  
         MVC   WORKMGR(L'MGR2),MGR2                                             
         MVC   WORKMGR+L'MGR2+L'GDOCODE(L'MGR2NM),MGR2NM                        
         LTR   R5,R5                                                            
         BNZ   *+20                                                             
         MVC   WORKMGR+L'MGR2(L'GDOCODE),=C'999999 '                            
         MVC   WORKMGR+L'MGR2+L'GDOCODE+L'MGR2NM(3),=C'MBC'                     
         B     *+16                                                             
         MVC   WORKMGR+L'MGR2(L'GDOCODE),GDOCODE                                
         MVC   WORKMGR+L'MGR2+L'GDOCODE+L'MGR2NM(L'GDONAME),GDONAME             
*                                                                               
         L     RE,ADCLT                                                         
         USING CLTHDRD,RE                                                       
         LA    R0,220              NUMBER OF ENTRIES IN CLIST                   
         LA    RF,CLIST            CONVERT BINARY CODE TO ALPHA                 
         CLC   BUFPRD,3(RF)                                                     
         BE    *+14                                                             
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         DROP  RE                                                               
         MVC   WORKACT(3),0(RF)                                                 
         MVC   WORKACT+3(1),QMED      MEDIA                                     
         MVC   WORKACT+4(L'MGR2+L'GDOCODE),WORKMGR                              
         MVC   WORKACT+4+L'MGR2+L'GDOCODE(1),SVKEST   ESTIMATE                  
*                                                                               
         GOTO1 =V(BLDMLR),DMCB,WORKMGR,WORKACT,RDATE                            
         LA    R6,4(R6)                                                         
         L     R5,0(R6)                                                         
         LTR   R5,R5                                                            
         BZ    SP132                                                            
         B     SP130LP                                                          
         DROP  R5                                                               
*                                                                               
         DS    0F                                                               
SVGDTAB  DS    XL200               SAVE THE ADDRESSES IN GDOUTAD                
SVKEST   DS    CL1                 SAVE ESTIMATE NUMBER                         
SAVER7   DS    A                                                                
OUTDATE  DS    D                                                                
DSTART   DS    CL6                 SAVE STARTING DATE                           
WORKMGR  DS    XL72                MGR2+GDOCODE+NM2+GDONAME                     
WORKACT  DS    XL25                PROD+MED+MGR2+GDOCODE+EST+RDT1+RDT2          
         EJECT                                                                  
*                                                                               
SP132    DS    0H                                                               
         MVC   BUFDATE,RDATE                                                    
         MVC   BUFDAY,BDDAY                                                     
         MVC   BUFPROG,BDPROG                                                   
         MVC   BUFMSTA,BUYMSTA                                                  
         MVC   BUFCOM,SAVDA                                                     
         XC    BUFNTWK,BUFNTWK                                                  
         CLI   QOPT5,C'Y'                                                       
         BE    *+8                                                              
         CLI   QOPT3,C'Y'                                                       
         BNE   SP135                                                            
         BAS   RE,GETNTWK          GET STATION'S NETWORK AFFILIATE              
         CLI   QOPT3,C'Y'                                                       
         BE    SP135                                                            
         XC    BUFNTWK,BUFNTWK                                                  
SP135    DS    0H                                                               
         L     R5,VBUFFALO                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',(R5),BUFFKEY                                
         B     SP115               SKIP THE TRACE                               
         MVI   P,C'1'                                                           
         BAS   RE,SPTRACE                                                       
         B     SP115                                                            
         DROP  R7                                                               
         EJECT                                                                  
* SP200 - THIS SECTION PRINTS THE MEDIA CALENDER *                              
         SPACE 1                                                                
SP200    DS    0H                                                               
         L     R5,VBUFFALO                                                      
         L     R2,VGDAREA                                                       
         USING GDSECT,R2                                                        
         CLI   QOPT1,C'Y'          IS THE OUTPUT TO TAPE?                       
         BE    SP300                YES.                                        
         XC    OLDMKT,OLDMKT                                                    
         LA    RC,P                                                             
         USING PRTDSECT,RC                                                      
         CLI   RCSUBPRG,2          IS THIS MULTI-DISTRIBUTOR?                   
         BNE   SP210                NO.                                         
         MVI   GDOUTNO,1            YES. GET 1ST DISTRIBUTOR OUTLET             
*                                                                               
         L     RF,ADEST            NO SCHEME ALWAYS DO CORPORATE                
         USING ESTHDR,RF                                                        
         CLI   PROGPROF+5,0                                                     
         BNE   SP200B                                                           
         CLI   ERTLSCHM,C'*'                                                    
         BH    SP200B                                                           
         BE    SP201                                                            
         CLC   EPROF+1(2),=C'  '                                                
         BE    SP201                                                            
         DROP  RF                                                               
*                                                                               
SP200B   DS    0H                                                               
         CLC   GDNSCHMS,=F'2'      CORP ONLY                                    
         BL    SP201               NEVER SUPPRESS IT                            
*                                                                               
         CLI   PROGPROF+3,C'N'     SUPPRESS RETAIL CORP                         
         BNE   *+8                                                              
         MVI   GDOUTNO,2                                                        
*                                                                               
SP201    CLC   QESTEND,SPACES      IS THIS A SINGLE ESTIMATE                    
         BNE   SP205                NO.                                         
         L     RF,ADEST            FOR A SPECIFIC ESTIMATE                      
         USING ESTHDR,RF                                                        
         MVC   GDSCHM,ERTLSCHM                                                  
         CLI   ERTLSCHM,C'*'       SCHEME                                       
         BH    *+10                                                             
         MVC   GDSCHM,EPROF+1      OLD SPOT FOR SCHEME                          
         DROP  RF                                                               
SP205    DS    0H                                                               
         GOTO1 VFNDOUT,DMCB,(R2)                                                
         CLC   GDOUTAD,=F'0'       ARE WE DONE YET?                             
         BE    EXIT                 YES.                                        
         CLC   GDSCHM,SPACES                                                    
         BE    *+14                                                             
         OC    GDSHRP,GDSHRP                                                    
         BZ    SP260                                                            
         MVC   SVGDOUTA,GDOUTAD    SAVE THE ADDRESS IN GDOUTAD                  
SP210    DS    0H                                                               
         XC    BUFREC,BUFREC                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R5),BUFFKEY,0                             
         CLI   DM3,X'80'                                                        
         BE    EXIT                                                             
         MVC   BYTE,BPRD                                                        
         B     SP220                                                            
         SPACE 1                                                                
SP215    DS    0H                                                               
         L     R5,VBUFFALO                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R5),BUFFKEY,0                              
         CLI   DM3,X'80'                                                        
         BE    SP260                                                            
SP220    DS    0H                                                               
         B     SP225               SKIP THE TRACE                               
         MVI   P,C'2'                                                           
         BAS   RE,SPTRACE                                                       
SP225    DS    0H                                                               
         CLC   =F'0',BUFACC        ARE THERE ANY SPOTS?                         
         BE    SP215                NO.                                         
         CLI   BPRD,X'FF'          IS THIS A POL BUY?                           
         BE    *+14                 YES. PRINT IT                               
         CLC   BPRD,BUFPRD                                                      
         BNE   SP215                                                            
         MVC   PRTPNAME,BUFNAME                                                 
         GOTO1 DATCON,DMCB,(2,BUFDATE),(4,PRTDATE)                              
         GOTO1 UNTIME,DMCB,BUFPST,PRTTIME                                       
         GOTO1 CODAY,DMCB,BUFDAY,PRTDAY                                         
         GOTO1 MSUNPK,DMCB,BUFMSTA,PRTMKT,PRTSTA                                
         CLI   QOPT5,C'Y'          PRINT THE CHANNEL                            
         BNE   *+10                                                             
         MVC   PRTCHNL,BUFCHNL                                                  
         CLI   CANAGY,C'Y'         IS THIS A CANADIAN AGENCY?                   
         BNE   SP227                NO.                                         
         CLC   BINMKT,BUFMSTA      IS THIS THE SAME MARKET?                     
         BE    SP227                YES.                                        
         MVI   PRTSPL,C'*'          NO. THIS IS SPILL                           
SP227    DS    0H                                                               
         CLC   =CL3'POL',QPRD                                                   
         BNE   SP240                                                            
         L     R7,ADCLT                                                         
         USING CLTHDRD,R7                                                       
         MVC   BYTE,BUFPRD                                                      
         LA    R0,220                                                           
         LA    R6,CLIST                                                         
SP230    DS    0H                                                               
         CLC   BYTE,3(R6)                                                       
         BE    SP235                                                            
         LA    R6,4(R6)                                                         
         BCT   R0,SP230                                                         
         MVC   PRTPRD(7),=C'NOT FND'                                            
         B     SP240                                                            
         DROP  R7                                                               
         SPACE 1                                                                
SP235    DS    0H                                                               
         MVC   PRTPRD(3),0(R6)                                                  
SP240    DS    0H                                                               
         EDIT  (B4,BUFACC),(6,PRTACC),ZERO=NOBLANK                              
         CLC   OLDMKT,BINMKT       IS THIS THE SAME MARKET?                     
         BE    *+14                 NO.                                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   OLDMKT,BINMKT                                                    
         LA    R7,P2                                                            
         CLI   QOPT3,C'Y'          SHOULD I CHECK NETWORK AFFILIATES?           
         BNE   SP245                                                            
         OC    BUFNTWK,BUFNTWK     IS THIS A NETWORK AFFILIATE?                 
         BZ    SP245                                                            
         MVC   10(10,R7),=CL10'NETWORK = '                                      
         MVC   20(3,R7),BUFNTWK                                                 
         LA    R7,132(R7)                                                       
SP245    DS    0H                                                               
         CLI   QOPT2,C'N'                                                       
         BE    SP255                                                            
         CLI   QOPT2,C'Y'          DO WE WANT BUY LINE COMMENTS?                
         BE    *+12                 YES.                                        
         CLI   PROGPROF,C'Y'       IS THIS A 'COMMENTS' AGENCY?                 
         BNE   SP255                NO.                                         
         OC    BUFCOM,BUFCOM       ARE THERE ANY BUY LINE COMMENTS?             
         BZ    SP255                NO.                                         
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),BUFCOM                                                 
         GOTO1 GETBUY                                                           
         L     R5,ADBUY                                                         
         USING BUYRECD,R5                                                       
         LA    R3,24(R5)                                                        
SP250    DS    0H                                                               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             IS THIS THE EOR?                             
         BE    SP255                YES                                         
         CLI   0(R3),X'66'         IS THIS A COMMENT                            
         BNE   SP250                NO.                                         
         ZIC   RF,1(R3)            RF HAS LENGTH OF COMMENT ELEMENT.            
         SH    RF,=H'4'                                                         
         EX    RF,MVCCOM                                                        
         LA    R7,132(R7)                                                       
         B     SP250                                                            
         SPACE 1                                                                
SP255    DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     SP215                                                            
         SPACE 1                                                                
SP260    CLI   RCSUBPRG,2          IS THIS MULTI-DISTRIBUTOR?                   
         BNE   EXIT                 NO. WE'RE DONE                              
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         CLI   PROGPROF+5,0                                                     
         BNE   SP261                                                            
         CLI   ERTLSCHM,C'*'                                                    
         BH    SP261                                                            
         BE    EXIT                                                             
         CLC   EPROF+1,=C'  '      NON RETAIL ESTIMATE                          
         BE    EXIT                JUST EXIT                                    
         DROP  RF                                                               
*                                                                               
SP261    DS    0H                                                               
         ZIC   RF,GDOUTNO           YES. GET THE NEXT OUTLET                    
         LA    RF,1(RF)                                                         
         STC   RF,GDOUTNO                                                       
         MVI   FORCEHED,C'Y'       START NEW OUTLET'S COPY ON NEW PAGE          
         MVC   PAGE,=H'1'                                                       
         B     SP205                                                            
         SPACE 1                                                                
MVCCOM   MVC   10(0,R7),3(R3)      MOVE BUY LINE COMMENT                        
         DROP  R2                                                               
         DROP  R5                                                               
         EJECT                                                                  
* SP300 - THIS SECTION PUTS THE MEDIA CALENDAR ON TAPE *                        
         SPACE 1                                                                
SP300    LA    R6,TAPEOUT                                                       
         SPACE 1                                                                
* RECORD KEY *                                                                  
         SPACE 1                                                                
         MVC   RECREQNO,REQCOUNT   REQUEST NUMBER - PACKED                      
         MVC   RECCLT,QCLT         CLIENT CODE                                  
         MVC   RECPRD,QPRD         PRODUCT CODE                                 
         MVC   RECESTST,QEST       ESTIMATE NUMBER (START)                      
         MVC   RECESTND,QESTEND    ESTIMATE NUMBER (END OR SPACES)              
         MVC   RECMKT,QMKT         MARKET NUMBER                                
         SPACE 1                                                                
* RECORD TYPE 1 - AGENCY NAME, CLIENT CODE AND NAME *                           
         SPACE 1                                                                
         MVC   0(19,R6),RECKEY                                                  
         MVI   19(R6),C'1'                                                      
         MVC   20(33,R6),AGYNM     AGENCY NAME                                  
         MVC   53(27,R6),CLT       CLIENT CODE AND NAME                         
         BAS   RE,PUTTAPE                                                       
         SPACE 1                                                                
* RECORD TYPE 2 - AGENCY ADDRESS, PRODUCT CODE AND NAME *                       
         SPACE 1                                                                
         MVC   0(19,R6),RECKEY                                                  
         MVI   19(R6),C'2'                                                      
         MVC   20(33,R6),AGYADR    AGENCY ADDRESS                               
         MVC   53(27,R6),PRD       PRODUCT CODE AND NAME                        
         BAS   RE,PUTTAPE                                                       
         SPACE 1                                                                
* RECORD TYPE 3 - PERIOD, MEDIA, ESTIMATE CODE AND NAME AND RUN DATE *          
         SPACE 1                                                                
         MVC   0(19,R6),RECKEY                                                  
         MVI   19(R6),C'3'                                                      
         MVC   20(12,R6),QSTART    REQUEST START AND END DATES                  
         MVC   32(10,R6),MEDNM     MEDIA NAME                                   
         MVC   42(27,R6),EST       ESTIMATE CODE AND NAME                       
         GOTO1 DATCON,DMCB,(4,RCDATE),(8,69(R6))                                
         BAS   RE,PUTTAPE                                                       
         SPACE 1                                                                
* RECORD TYPE 4 - MARKET NAME AND NUMBER *                                      
*  MARKET GROUP NAME AND THE RUN TIME    *                                      
         SPACE 1                                                                
         MVC   0(19,R6),RECKEY                                                  
         MVI   19(R6),C'4'                                                      
         MVC   20(28,R6),MKT       MARKET NUMBER AND NAME                       
         MVC   48(24,R6),SPACES                                                 
         BAS   RE,GETADI           GET SCHLITZ ADI NAME                         
*&&DO                                                                           
         GETIME STANDARD                                                        
*&&                                                                             
*&&OS                                                                           
         TIME  DEC                                                              
         LR    R1,R0               GET TIME IN R1                               
         SRL   R1,8                SHIFT OUT TENTHS AND HUNDREDTHS              
         SLL   R1,4                MAKE ROOM FOR SIGN                           
         AH    R1,=H'12'           SET POSITIVE SIGN                            
*&&                                                                             
         ST    R1,WORK                                                          
         XC    DUB,DUB                                                          
         MVC   DUB+5(3),WORK                                                    
         OI    DUB+7,X'0F'                                                      
         CVB   R4,DUB                                                           
         EDIT  (R4),(5,72(R6)),2                                                
         BAS   RE,PUTTAPE                                                       
         SPACE 1                                                                
* RECORD TYPE 5 - BUFFALO RECORDS *                                             
         SPACE 1                                                                
         L     R5,VBUFFALO                                                      
         USING TPEDSECT,R6                                                      
         XC    BUFREC,BUFREC                                                    
         XC    TAPEREC,TAPEREC                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R5),BUFFKEY,0                             
         CLI   DM3,X'80'                                                        
         BE    EXIT                                                             
         MVC   BYTE,BPRD                                                        
         B     SP320                                                            
SP310    DC    0H'0'                                                            
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R5),BUFFKEY,0                              
         CLI   DM3,X'80'                                                        
         BE    SP360               EXIT                                         
SP320    DS    0H                                                               
         CLC   =F'0',BUFACC        ARE THERE ANY SPOTS?                         
         BE    SP310                NO.                                         
         CLI   BPRD,X'FF'          IS THIS A POL BUY?                           
         BE    *+14                 YES. WRITE THIS RECORD TO TAPE              
         CLC   BPRD,BUFPRD                                                      
         BNE   SP310                                                            
         MVC   TAPEKEY,RECKEY      BUILD KEY FOR TYPE 5 RECORDS                 
         MVI   TKEYTYPE,C'5'                                                    
         GOTO1 DATCON,DMCB,(2,BUFDATE),(4,TAPEDATE)                             
         GOTO1 CODAY,DMCB,BUFDAY,TAPEDAY                                        
         MVC   TAPETIME,SPACES                                                  
         GOTO1 UNTIME,DMCB,BUFPST,TAPETIME                                      
         MVC   TAPENAME,BUFNAME                                                 
         GOTO1 MSUNPK,DMCB,BUFMSTA,PRTMKT,TAPESTA                               
         CLC   =CL3'POL',QPRD                                                   
         BNE   SP350                                                            
         L     R7,ADCLT                                                         
         USING CLTHDRD,R7                                                       
         MVC   BYTE,BUFPRD                                                      
         LA    R0,220                                                           
         LA    R8,CLIST                                                         
SP330    CLC   BYTE,3(R8)                                                       
         BE    SP340                                                            
         LA    R8,4(R8)                                                         
         BCT   R0,SP330                                                         
         MVC   TAPEPRD,=CL3'POL'                                                
         B     SP350                                                            
SP340    MVC   TAPEPRD,0(R8)                                                    
SP350    DC    0H'0'                                                            
         L     RF,BUFACC                                                        
         CVD   RF,DUB                                                           
         UNPK  TAPESPTS,DUB+6(2)                                                
         OI    TAPESPTS+3,X'F0'                                                 
         BAS   RE,PUTTAPE                                                       
         B     SP310                                                            
SP360    DC    0H'0'                                                            
         MVC   P+10(17),=CL17'OUTPUT IS TO TAPE'                                
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
* SP500 - THIS SECTION UPDATES THE REQUEST COUNTER *                            
         SPACE 1                                                                
SP500    DS    0H                                                               
         AP    REQCOUNT,=P'1'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
* PUTTAPE - THIS ROUTINE PUTS A RECORD OUT TO TAPE *                            
         SPACE 1                                                                
PUTTAPE  NTR1                                                                   
         BC    0,PUT2                                                           
         OI    *-3,X'F0'           ONLY OPEN THE TAPE ONCE                      
*&&DO                                                                           
         LA    R1,SZTAPE                                                        
         OPENR (1)                                                              
*&&                                                                             
*&&OS                                                                           
         OPEN  (SZTAPE,(OUTPUT))                                                
*&&                                                                             
         MVI   CLS2+1,0            ALLOW TAPE TO BE CLOSED EVENTUALLY           
PUT2     LA    R1,SZTAPE                                                        
         LA    R0,TAPEOUT                                                       
         PUT   (1),(0)                                                          
         B     EXIT                                                             
         SPACE 2                                                                
* CLS - THIS SECTION CLOSES THE TAPE *                                          
         SPACE 1                                                                
CLS      DS    0H                                                               
         CLI   SVPRGP4,C'Y'                                                     
         BNE   CLS2                                                             
*                                                                               
         L     R5,=V(BLDMLR)                                                    
         USING BLDMLR,R5                                                        
         LA    R6,MGRTAB                                                        
         L     R7,AACTTAB                                                       
         L     R3,NUMRECS2                                                      
         OR    R3,R3                                                            
         BZ    CLS2                                                             
*                                                                               
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
*                                                                               
CLSLP    DS    0H                                                               
         OC    21(4,R7),21(R7)                                                  
         BZ    CLSLP1                                                           
         MVC   P+1(3),0(R7)        MOVE IN THREE ALPHA CODE                     
         CLI   3(R7),C'R'          IS MEDIA RADIO?                              
         BNE   *+14                NO                                           
         MVC   P+6(5),=C'RADIO'                                                 
         B     *+10                                                             
         MVC   P+6(2),=C'TV'                                                    
*                                                                               
         GOTO1 BINSRCH,DMCB,(X'00',4(R7)),(R6),NUMRECS1,L'WORKMGR,     X        
               (0,12),300                                                       
         CLI   DMCB,X'01'                                                       
         BNE   *+6                                                              
         DC    H'0'                RECORD NOT FOUND                             
         SR    RE,RE                                                            
         ICM   RE,7,DMCB+1         A(FOUND RECORD)                              
         MVC   P+12(L'MGR2NM),12(RE) CLUSTER                                    
         MVC   P+37(L'GDONAME),36(RE) DISTRIBUTOR                               
         SR    RE,RE                                                            
         ICM   RE,1,16(R7)         ESTIMATE                                     
         XC    DUB,DUB                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+74(3),DUB+6(2)                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(X'02',17(R7)),(X'04',ODATE)                         
         MVC   P+78(5),ODATE                                                    
         MVI   P+84,C'-'                                                        
         GOTO1 DATCON,DMCB,(X'02',19(R7)),(X'04',ODATE)                         
         MVC   P+86(5),ODATE                                                    
         GOTO1 REPORT                                                           
CLSLP1   LA    R7,L'WORKACT(R7)                                                 
         BCT   R3,CLSLP                                                         
CLS2     B     EXIT                                                             
*&&DO                                                                           
         LA    R1,SZTAPE                                                        
         CLOSE (1)                                                              
*&&                                                                             
*&&OS                                                                           
         CLOSE (SZTAPE)                                                         
*&&                                                                             
         B     EXIT                                                             
ODATE    DS    D                                                                
DOTS     DC    CL20'--------------------'                                       
         SPACE 2                                                                
* GETADI - GET THE SCHLITZ ADI NAME *                                           
         SPACE 1                                                                
GETADI   NTR1                                                                   
         CLI   QMGR,C' '           IS THIS REQUEST BY MKT GRP?                  
         BE    EXIT                 NO.                                         
         LA    R2,PRDNM                                                         
         LA    R3,L'PRDNM-1                                                     
GTADI10  DS    0H                                                               
         CLC   =C'ADI',0(R2)                                                    
         BE    GTADI20                                                          
         LA    R2,1(R2)                                                         
         BCT   R3,GTADI10                                                       
         MVC   48(24,R6),MGR2NM                                                 
         B     EXIT                                                             
GTADI20  DS    0H                                                               
         EX    R3,MOVEADI                                                       
         B     EXIT                                                             
         SPACE 1                                                                
MOVEADI  MVC   48(0,R6),0(R2)                                                   
         EJECT                                                                  
* GETNTWK - GET THE STATION'S NETWORK AFFILIATE *                               
         SPACE 1                                                                
GETNTWK  NTR1                                                                   
         L     R7,ADBUY                                                         
         USING BUYRECD,R7                                                       
         MVC   KEY2SAVE(17),KEY                                                 
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED       MEDIA                                        
         GOTO1 MSUNPK,DMCB,BUYMSTA,FULL,KEY+2                                   
         CLI   KEY+6,C' '                                                       
         BNE   *+10                                                             
         MVC   KEY+6(1),QMED                                                    
         MVC   KEY+7(2),QAGY       AGENCY                                       
         MVC   KEY+9(3),QCLT       CLIENT                                       
         DROP  R7                                                               
         SPACE 1                                                                
         L     R7,ADSTAT                                                        
         USING STAREC,R7                                                        
         GOTO1 HIGHSTA                                                          
         CLC   KEYSAVE(12),STAREC  SAME MEDIA/STA/AGY/CLT                       
         BE    GETNT10                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(9),KEYSAVE                                                   
         MVC   KEY+9(3),=CL3'000'                                               
         GOTO1 HIGHSTA                                                          
         CLC   KEYSAVE(12),STAREC  SAME MEDIA/STA/AGY?                          
         BNE   GETNT20                                                          
GETNT10  DS    0H                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   *+10                                                             
         MVC   BUFCHNL,SCHNL                                                    
         OC    SNETWRK,SNETWRK                                                  
         BZ    GETNT20                                                          
         CLC   SNETWRK,SPACES                                                   
         BE    GETNT20                                                          
         MVC   BUFNTWK,SNETWRK     NETWORK AFFILIATE                            
GETNT20  DS    0H                                                               
         MVC   KEY(17),KEY2SAVE                                                 
         B     EXIT                                                             
         DROP  R7                                                               
         SPACE 2                                                                
* SPTRACE - TRACE ROUTINE *                                                     
         SPACE 1                                                                
SPTRACE  NTR1                                                                   
         GOTO1 HEXOUT,DMCB,BUFFKEY,P+10,44,=C'MIX',0                            
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* HEADHOOK ROUTINE *                                                            
         DROP  R8                                                               
         SPACE 1                                                                
         DS    0H                                                               
         USING *,RF                                                             
SPDCHDHK NTR1                                                                   
         LM    R8,RB,SPDCR8B                                                    
         DROP  RF                                                               
         USING SPDC02+4096,R8                                                   
         CLI   CANAGY,C'Y'         IS THIS A CANADIAN AGENCY?                   
         BNE   HDHK0                NO.                                         
         MVC   H6+75(17),=CL17'* INDICATES SPILL'                               
HDHK0    DS    0H                                                               
         CLI   RCSUBPRG,3          SPECIAL HEADS FOR MILLER RECAP               
         BE    HDHK8                                                            
*                                                                               
         CLI   RCSUBPRG,2          IS THIS A MULTI-DISTRIBUTOR REQUEST?         
         BNE   HDHK1                NO                                          
         L     RF,SVGDOUTA          YES.                                        
         USING GDOUTD,RF                                                        
         MVC   H5+47(20),=CL20'DISTRIBUTOR NUMBER -'                            
         MVC   H5+68(6),GDOCODE                                                 
         MVC   H6+48(36),GDONAME                                                
         MVC   H7+48(26),GDOAL1                                                 
         MVC   H8+48(26),GDOAL2                                                 
         MVC   H9+48(26),GDOAL3                                                 
         MVC   H10+48(26),GDOAL4                                                
         DROP  RF                                                               
HDHK1    DC    0H'0'                                                            
         LA    RC,H12                                                           
         BAS   RE,SPSUBHDR                                                      
         LA    RC,H13                                                           
         MVC   PRTSTA(7),=CL7'STATION'                                          
         CLI   QOPT5,C'Y'                                                       
         BNE   *+10                                                             
         MVC   PRTCHNL(4),=C'CHNL'                                              
         MVC   PRTDATE(4),=CL4'DATE'                                            
         MVC   PRTDAY(3),=CL3'DAY'                                              
         MVC   PRTTIME(4),=CL4'TIME'                                            
         MVC   PRTPNAME(7),=CL7'PROGRAM'                                        
         MVC   PRTACC(10),=CL10'SPOTS/WEEK'                                     
         CLC   =CL3'POL',QPRD                                                   
         BNE   *+10                                                             
HDHK2    MVC   PRTPRD(5),=CL5'BRAND'                                            
         LA    RC,H14                                                           
         BAS   RE,SPSUBHDR                                                      
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 1                                                                
SPSUBHDR MVC   PRTSTA(7),=CL10'----------'                                      
         CLI   QOPT5,C'Y'                                                       
         BNE   *+10                                                             
         MVC   PRTCHNL(4),=CL10'----------'                                     
         MVC   PRTDATE(4),=CL10'----------'                                     
         MVC   PRTDAY(3),=CL10'----------'                                      
         MVC   PRTTIME(4),=CL10'----------'                                     
         MVC   PRTPNAME(7),=CL10'----------'                                    
         CLC   =CL3'POL',QPRD                                                   
         BNE   *+10                                                             
         MVC   PRTPRD(5),=CL10'----------'                                      
         MVC   PRTACC(10),=CL10'----------'                                     
         BR    RE                                                               
         SPACE 2                                                                
HDHK8    DS    0H                                                               
         MVC   H12(5),=C'BRAND'                                                 
         MVC   H12+6(4),=C'TYPE'                                                
         MVC   H12+12(7),=C'CLUSTER'                                            
         MVC   H12+37(11),=C'DISTRIBUTOR'                                       
         MVC   H12+74(3),=C'EST'                                                
         MVC   H12+78(15),=C'START/END DATES'                                   
         MVC   H13(5),DOTS                                                      
         MVC   H13+6(4),DOTS                                                    
         MVC   H13+12(7),DOTS                                                   
         MVC   H13+37(11),DOTS                                                  
         MVC   H13+74(3),DOTS                                                   
         MVC   H13+78(15),DOTS                                                  
         B     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* SCHLITZ CLIENT ID TABLE *                                                     
         SPACE 1                                                                
SCHLITZ  DC    0F'0'                                                            
         DC    CL3'SD '            SCHLITZ DISTIBUTORS                          
         DC    CL3'SO '            JOS. SCHLITZ FLD MKT                         
         DC    CL3'SZ '            SCHLITZ                                      
         DC    CL3'TM '            SCHLITZ BEER                                 
         DC    CL3'TN '            OLD MILW.                                    
         DC    CL3'TO '            MALT LIQUOR                                  
         DC    CL3'TP '            SCHLITZ LIGHT                                
         DC    CL3'TQ '            ERLANGER                                     
         DC    CL3'TR '            OLD MILW LIGHT                               
NOSPRD   EQU   (*-SCHLITZ)/3                                                    
         SPACE 1                                                                
* DATA *                                                                        
SVPRGP4  DC    C'N'                                                             
         SPACE 1                                                                
         DS    0D                                                               
PRTMKT   DS    D                                                                
SPDCR8B  DC    4F'0'                                                            
VBUFFALO DS    F                                                                
VMULTID  DS    V                   ADDRESS OF MULTI-DISTRIBUTOR CSECT           
VGDAREA  DS    V                   ADDRESS OF FINDOUT WORK AREA                 
VFNDOUT  DS    V                   ADDRESS OF FINDOUT MODULE                    
SAVDA    DS    F                   SAVE DISK ADDRESS OF BUY RECORD              
SVGDOUTA DS    F                   SAVE THE ADDRESS IN GDOUTAD                  
RSTART   DS    H                                                                
REND     DS    H                                                                
BINMKT   DS    H                   BINARY MARKET NUMBER                         
OLDMKT   DS    H                   LAST BINARY MARKET NUMBER                    
REQCOUNT DS    PL3                                                              
CANAGY   DS    C                   CANADIAN AGENCY FLAG                         
         EJECT                                                                  
* BUFFALO RECORD LAYOUT *                                                       
         SPACE 1                                                                
         DS    0F                                                               
BUFREC   DS    0CL48                                                            
BUFFKEY  DS    0CL44                                                            
BUFDATE  DS    CL2                 DATE                                         
BUFDAY   DS    CL1                 DAY OF WEEK                                  
BUFPROG  DS    0CL22                                                            
BUFPST   DS    CL2                 PROGRAM START TIME                           
BUFPEND  DS    CL2                 PROGRAM END TIME                             
BUFNAME  DS    CL18                PROGRAM NAME                                 
BUFMSTA  DS    CL5                 MARKET/STATION                               
BUFPRD   DS    CL1                 PRODUCT                                      
BUFNTWK  DS    CL3                 NETWORK AFFILIATE                            
BUFCHNL  DS    CL4                 CHANNEL                                      
BUFCOM   DS    CL4                 COMMENT - DISK ADDRESS                       
         DS    CL2                 FILL                                         
BUFACC   DS    CL4                 ACCUMULATOR - NUMBER SPOTS/WEEK              
         SPACE 1                                                                
* TAPE RECORD LAYOUT *                                                          
         SPACE 1                                                                
RECKEY   DS    0CL20                                                            
RECREQNO DS    CL3                 REQUEST NUMBER - PACKED                      
RECCLT   DS    CL3                 CLIENT CODE                                  
RECPRD   DS    CL3                 PRODUCT CODE                                 
RECESTST DS    CL3                 ESTIMATE NUMBER - START                      
RECESTND DS    CL3                                 - END                        
RECMKT   DS    CL4                 MARKET NUMBER                                
RECTYPE  DS    CL1                 RECORD TYPE                                  
         SPACE 1                                                                
TAPEOUT  DS    CL80                WORK AREA FOR BUILDING TAPE RECORDS          
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
SZTAPE   DTFMT BLKSIZE=4000,DEVADDR=SYS005,FILABL=NO,IOAREA1=TAPEBLK,  X        
               RECFORM=FIXBLK,RECSIZE=80,REWIND=UNLOAD,TYPEFLE=OUTPUT, X        
               WORKA=YES                                                        
*&&                                                                             
*&&OS                                                                           
SZTAPE   DCB   DDNAME=SZTAPE,          DOS SYS005                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00080,                                            X        
               BLKSIZE=04000,          DOS BLKSIZE=04000               X        
               MACRF=PM                                                         
*&&                                                                             
         SPACE 1                                                                
*&&DO                                                                           
TAPEBLK  DS    500D                                                             
*&&                                                                             
         SPACE 1                                                                
         BUFF  LINES=2250,ROWS=1,COLUMNS=1,KEYLIST=(44,A)                       
         EJECT                                                                  
BLDMLR   CSECT                                                                  
         NMOD1 0,BLDMLR                                                         
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         MVC   ADRDATE,8(R1)                                                    
*                                                                               
* BUILD MGRTAB AND ACTTAB                                                       
         GOTO1 BINSRCH,DMCB,(X'01',(R2)),MGRTAB,NUMRECS1,              X        
               72,(0,12),4000                                                   
         OC    DMCB(4),DMCB        TEST IF TABLE IS FULL                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   NUMRECS1,8(R1)                                                   
*                                                                               
         L     RF,AACTTAB                                                       
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(RF),NUMRECS2,                X        
               25,(0,17),4000                                                   
         MVC   NUMRECS2,8(R1)                                                   
*                                                                               
* ADJUST START DATE & END DATE                                                  
         L     RF,ADRDATE                                                       
         SR    RE,RE                                                            
         ICM   RE,7,DMCB+1         A(FOUND RECORD)                              
         ICM   R1,15,21(RE)                                                     
         A     R1,BUFACC                                                        
         STCM  R1,15,21(RE)                                                     
         OC    17(2,RE),17(RE)                                                  
         BNZ   *+10                                                             
         MVC   17(2,RE),0(RF)                                                   
         OC    19(2,RE),19(RE)                                                  
         BNZ   *+10                                                             
         MVC   19(2,RE),0(RF)                                                   
*                                                                               
         CLC   17(2,RE),0(RF)      TEST START DATE                              
         BNH   *+10                                                             
         MVC   17(2,RE),0(RF)                                                   
         CLC   19(2,RE),0(RF)      TEST END DATE                                
         BNL   *+10                                                             
         MVC   19(2,RE),0(RF)                                                   
*                                                                               
BLDXIT   XMOD1 1                                                                
         LTORG                                                                  
*                                                                               
NUMRECS1 DC    A(0)                NUMBER OF RECORDS IN MGRTAB                  
NUMRECS2 DC    A(0)                NUMBER OF RECORDS IN ACTTAB                  
ADRDATE  DS    A                                                                
AACTTAB  DC    A(ACTTAB)                                                        
         DC    CL8'*MGRTAB*'                                                    
MGRTAB   DS    4000XL72                                                         
         DC    CL8'*ACTTAB*'                                                    
ACTTAB   DS    4000XL25                                                         
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
MULTID   CSECT                                                                  
         NMOD1 0,MULTD                                                          
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         SPACE 1                                                                
         L     R5,=V(FINDOUT)                                                   
         ST    R5,VFINDOUT         RELOCATE FINDOUT                             
         L     R5,=A(FOTABS)                                                    
         ST    R5,AFOTABS          RELOCATE FINDOUT TABLES                      
         L     R2,=V(GDAREA)                                                    
         USING GDSECT,R2                                                        
         SPACE 1                                                                
* CONTROL SECTION *                                                             
         SPACE 1                                                                
         CLI   MODE,CLTFRST                                                     
         BL    MEXIT                                                            
         BE    M100                                                             
         CLI   MODE,MKTFRST                                                     
         BE    M300                                                             
         BH    MEXIT                                                            
         CLI   MODE,MGR1FRST                                                    
         BNL   M200                                                             
MEXIT    XIT1                                                                   
         EJECT                                                                  
* M100 - THIS SECTION READS THE B9 PROFILE TO DETERMINE *                       
*  IF THIS IS A MULTI-DISTRIBUTOR REQUEST               *                       
         SPACE 1                                                                
M100     DS    0H                                                               
         CLI   PROGPROF+5,X'00'                                                 
         BE    M100A                                                            
         MVC   PROFB9(6),PROGPROF+5                                             
         MVI   PROFB9+10,C'Y'                                                   
         B     M100B                                                            
*                                                                               
M100A    XC    WORK,WORK                                                        
         MVC   WORK(4),=CL4'S0B9'  TYPE                                         
         MVC   WORK+4(2),AGY       AGENCY                                       
         MVC   WORK+6(1),MED       MEDIA                                        
         MVC   WORK+7(3),CLIENT    CLIENT                                       
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         MVI   WORK+10,C'*'         YES                                         
         MVC   WORK+11(1),0(RF)                                                 
         GOTO1 GETPROF,DMCB,WORK,PROFB9,DATAMGR                                 
*                                                                               
M100B    CLI   PROFB9,0            IS THIS A RETAIL BILLING ACCOUNT?            
         BE    MEXIT                NO.                                         
         CLI   PROFB9+10,C'Y'      IS THIS A MEDIA CALENDAR CLIENT?             
         BNE   MEXIT                NO.                                         
         MVI   USRSW1,C'Y'          YES.                                        
         CLC   =C'NO',QEST                                                      
         BE    SPERR10                                                          
         SPACE 1                                                                
* INITIALIZE THE FINDOUT DSECT EXCEPT FOR MARKET AND MARKET GROUP *             
         SPACE 1                                                                
         XC    GDSECT(GDSECTL),GDSECT                                           
         MVC   GDCOMP(3),PROFB9    COMPANY/UNIT/LEDGER                          
         MVC   GDMGRLEN,PROFB9+3   MARKET GROUP LENGTH                          
         CLI   GDMGRLEN,0                                                       
         BNH   M110                IF MGR SET IN PROFILE                        
*                                   THEN MUST BE IN REQUEST                     
         CLI   QMGR,C' '                                                        
         BE    SPERR20                                                          
M110     DS    0H                                                               
         MVI   GDMKTLEN,0                                                       
         CLI   PROFB9+4,C'Y'                                                    
         BNE   *+8                                                              
         MVI   GDMKTLEN,4          MARKET LENGTH = 0 OR 4                       
         MVC   GDOUTLEN,PROFB9+5   OUTLET CODE LENGTH                           
         MVC   GDDMGR,DATAMGR      ADDRESS OF DATAMGR                           
         MVC   GDATABS,AFOTABS     ADDRESS OF FINDOUT TABLES                    
         MVC   GDAUTL,UTL          ADDRESS OF UTILITIES                         
         MVC   GDACCSYS,ACCTSE     WHICH ACCOUNTING SYSTEM                      
         B     MEXIT                                                            
         EJECT                                                                  
* M200 - THIS SECTION FINDS OUT ALL THE *                                       
*  DISTRIBUTORS FOR THIS MARKET GROUP   *                                       
         SPACE 1                                                                
M200     DS    0H                                                               
         CLI   GDMGRLEN,0          ANY MARKET GROUPS TO MOVE?                   
         BE    MEXIT                NO. EXIT                                    
         ZIC   RF,GDMGRLEN                                                      
         ZIC   RE,MGR3LEN          RE = TOTAL MKT GRP LENGTH                    
         SR    RE,RF                                                            
         BNM   *+6                                                              
         SR    RE,RE                                                            
         LA    RE,MGR3+1(RE)       USE LAST N POSITIONS                         
         BCTR  RF,0                                                             
         EX    RF,MOVEMGR                                                       
         B     MEXIT                                                            
         SPACE 1                                                                
MOVEMGR  MVC   GDMGR(0),0(RE)      MOVE THE MARKET GROUP CODE                   
         SPACE 2                                                                
* M300 - THIS SECTION FINDS OUT ALL DISTRIBUTORS FOR THIS MKT *                 
         SPACE 1                                                                
M300     DS    0H                                                               
         MVC   GDSCHM,SPACES       CLEAR SCHEME CODE                            
         CLI   GDMKTLEN,0          ANY MARKETS?                                 
         BE    *+10                 NO.                                         
         MVC   GDMKT,MKT            YES.                                        
         MVI   GDOUTNO,0           FIRST TIME FOR FINDOUT                       
         GOTO1 VFINDOUT,DMCB,GDSECT                                             
         CLC   GDNOUTS,=F'0'       ARE THERE ANY DISTRIBUTORS?                  
         BE    M310                 NO.                                         
         MVI   RCSUBPRG,2           YES.                                        
         B     MEXIT                                                            
         SPACE 1                                                                
M310     DS    0H                                                               
         MVI   USRSW2,C'Y'         NO DISTRIBUTORS FOR THIS MKT GRP             
         B     MEXIT                                                            
         SPACE 2                                                                
* SPERR - ERROR ROUTINE *                                                       
         SPACE 1                                                                
SPERR    DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         SPACE 1                                                                
SPERR10  DS    0H                                                               
         MVC   P+10(37),=CL37'ILLEGAL REQUEST - EST MUST BE NUMERIC'            
         B     SPERR                                                            
SPERR20  DS    0H                                                               
         MVC   P+10(41),=C'ILLEGAL REQUEST - MUST REQUEST BY MKT GRP'           
         B     SPERR                                                            
         EJECT                                                                  
* FINDOUT DATA AREA *                                                           
         SPACE 1                                                                
         DS    0D                                                               
PROFB9   DS    XL16                B9 PROFILE AREA                              
VFINDOUT DS    V                   ADDRESS OF FINDOUT MODULE                    
AFOTABS  DS    A                   ADDRESS OF FINDOUT TABLES                    
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
FOTABS   CSECT                                                                  
         DS    35000D            240K ADDRESS AREA - FINDOUT TABLES             
*                                                                               
PRTDSECT DSECT                                                                  
         DS    CL3                                                              
PRTSPL   DS    CL1                                                              
         DS    CL1                                                              
PRTSTA   DS    CL7                 STATION CALL LETTERS                         
         DS    CL1                                                              
PRTCHNL  DS    CL4                 CHANNEL                                      
         DS    CL1                                                              
PRTDATE  DS    CL5                 DATE - MMMDD                                 
         DS    CL4                                                              
PRTDAY   DS    CL9                 DAY OF WEEK                                  
         DS    CL5                                                              
PRTTIME  DS    CL11                PROGRAMS TIME SLOT                           
         DS    CL5                                                              
PRTPNAME DS    CL17                NAME OF THE PROGRAM                          
         DS    CL6                                                              
PRTACC   DS    CL10                NUMBER OF SPOTS PER WEEK                     
         DS    CL5                                                              
PRTPRD   DS    CL5                 BRAND MNEUMONIC                              
         DS    CL33                                                             
         SPACE 1                                                                
TPEDSECT DSECT                                                                  
TAPEREC  DS    0CL80                                                            
TAPEKEY  DS    0CL20               KEY                                          
TKREQNO  DS    CL3                 REQUEST NUMBER                               
TKEYCLT  DS    CL3                 CLIENT CODE                                  
TKEYPRD  DS    CL3                 PRODUCT CODE                                 
TKESTST  DS    CL3                 ESTIMATE NUMBER - START                      
TKESTND  DS    CL3                                 - END                        
TKEYMKT  DS    CL4                 MARKET NUMBER                                
TKEYTYPE DS    CL1                 RECORD TYPE                                  
         SPACE 1                                                                
TBUFREC  DS    0CL60               BUFFALO RECORD                               
TAPEDATE DS    CL5                 DATE                                         
TAPEDAY  DS    CL9                 DAY                                          
TAPETIME DS    CL11                TIME                                         
TAPENAME DS    CL18                PROGRAM NAME                                 
TAPESTA  DS    CL7                 STATION                                      
TAPEPRD  DS    CL3                 PRODUCT                                      
TAPESPTS DS    CL4                 SPOTS/WEEK                                   
         DS    CL3                 SPARE                                        
         EJECT                                                                  
       ++INCLUDE FOLINKD                                                        
         SPACE 2                                                                
GDAREA   CSECT                                                                  
         DS    (GDSECTL)C          FINDOUT WORK AREA                            
         PRINT OFF                                                              
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'177SPREPDC02 08/22/05'                                      
         END                                                                    
