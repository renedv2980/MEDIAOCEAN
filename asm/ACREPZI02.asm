*          DATA SET ACREPZI02  AT LEVEL 157 AS OF 12/11/09                      
*PHASE ACZI02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE TMSXFR                                                                 
         TITLE 'ADD TYPE 34S TO TMS RECORDS'                                    
ACZI02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZI**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZID,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,LEDGLAST                                                    
         BE    LDGL                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              RUN FIRST                                              *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         ZAP   QUESREC,=P'0'                                                    
         ZAP   CHAREC,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              LEDGER FIRST                                           *         
***********************************************************************         
*                                                                               
LDGF     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDACC,C'N'                                                     
         MVI   FCRDTRNS,C'N'                                                    
         L     R2,ADLEDGER                                                      
         CLC   1(2,R2),=C'1R'      FOR 1R                                       
         BNE   XIT                                                              
         XC    TMSDATE,TMSDATE                                                  
         L     R2,ADCMPEL           COMPANY EL                                  
         USING CPYELD,R2                                                        
         TM    CPYSTAT4,CPYSOFF2    ONLY 1 CHAR OFFICE AGENCIES                 
         BO    XIT                                                              
         TM    CPYSTAT7,CPYSTMSY    AND MUST BE ON TMS                          
         BNO   XIT                                                              
         MVI   FCRDACC,C'Y'                                                     
         MVI   FCRDTRNS,C'Y'                                                    
         CLI   CPYLN,CPYLN3Q          YES, DO WE NEW ELEMENT?                   
         BL    XIT                    NO                                        
         OC    CPYTMSSD,CPYTMSSD      DO WE HAVE A START DATE?                  
         BZ    XIT                    NO                                        
         GOTO1 DATCON,DMCB,(2,CPYTMSSD),(1,TMSDATE)                             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*              PROCTRNS                                               *         
***********************************************************************         
*                                                                               
PTRN     DS    0H                                                               
         XC    BATREF,BATREF                                                    
         XC    SVRATEFF,SVRATEFF                                                
         XC    SVACTDAT,SVACTDAT                                                
         XC    SVTODAY3,SVTODAY3                                                
         XC    SVRATIND,SVRATIND                                                
         XC    SVMOA,SVMOA                                                      
         XC    SVIND,SVIND                                                      
         MVC   BATOFF,SPACES                                                    
         MVC   SVSJCPJ,SPACES                                                   
         MVC   SVTASK,SPACES                                                    
         MVC   SVCLIOFF,SPACES                                                  
         MVC   SVTYPE,SPACES                                                    
         MVC   SVULA,SPACES                                                     
         XC    SAV40,SAV40                                                      
         ZAP   SVAMOUNT,=P'0'                                                   
         ZAP   SVHOURS,=P'0'                                                    
         ZAP   SVRATE,=P'0'                                                     
         ZAP   PK16,=P'0'                                                       
*                                                                               
         L     R1,ADTRANS                                                       
         LR    R4,R1                                                            
         SH    R4,DATADISP                                                      
         XC    DKEY,DKEY                                                        
         LA    R5,DKEY                                                          
         USING TRNRECD,R4                                                       
SJREC    USING TRNRECD,R5                                                       
*                                                                               
         CLC   TRNKDATE,TMSDATE                                                 
         BL    XIT                                                              
*                                                                               
         USING TRNELD,R2                                                        
         L     R2,ADTRANS                                                       
         CLI   0(R2),TRNELQ        X'44'                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   TRNTYPE,34          ONLY TYPE 34                                 
         BNE   XIT                                                              
         LR    R7,R4                                                            
         LA    R6,=C'GETR'                                                      
         BAS   RE,DMPGET                                                        
*                                         BUILD KEY OF SJ TYPE 34               
         MVC   SJREC.TRNKDATE,TRNKDATE    TRANSACTION DATE                      
         MVC   SJREC.TRNKREF,TRNKREF      TRANSACTION REF                       
         MVC   SJREC.TRNKCULC,TRNKCULA    THE 1R IS THE CONTRA                  
         MVC   BATREF,TRNBTCH      SAVE BATCH REFERENCE                         
         MVC   BATOFF,TRNOFFC      AND OFFICE                                   
*                                                                               
PTRN02   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),0             END OF RECORD                                
         BE    PTRN12                                                           
*                                                                               
         USING PRTELD,R2                                                        
         CLI   0(R2),PRTELQ        X'40'                                        
         BNE   PTRN04                                                           
         SR    R1,R1                                                            
         IC    R1,PRTLN                                                         
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAV40(0),PRTELD     SAVE THE 40 EL                               
         MVI   SVTYPE,TIMTCB       CLIENT BILLABLE                              
         TM    PRTSTAT,PRTSRTEQ                                                 
         BNO   *+8                                                              
         MVI   SVTYPE,TIMTCR       CLIENT REALIZATION                           
         TM    PRTSTAT,PRTSNOTQ                                                 
         BNO   *+8                                                              
         MVI   SVTYPE,TIMTCN       CLIENT NON BILLABLE                          
         ZAP   SVHOURS,PRTHOUR     HOURS                                        
         ZAP   SVRATE,PRTRATE      HOURLY RATE                                  
         MVC   SVRATEFF,PRTSTRT    EFFECTIVE DATE                               
         ZAP   PK16,SVRATE                                                      
         MP    PK16,SVHOURS                                                     
         SRP   PK16,64-2,5                                                      
         ZAP   SVAMOUNT,PK16       SAVE RATE X HOURS                            
         B     PTRN02                                                           
*                                                                               
         USING PCIELD,R2                                                        
PTRN04   CLI   0(R2),PCIELQ        X'51'                                        
         BNE   PTRN06                                                           
         CLI   PCILN,PCILN2Q       MUST BE LONG ENOUGH FOR WC                   
         BNL   *+6                                                              
         DC    H'0'                                                             
         MVC   SJREC.TRNKCULA,PCICLI      COMP/CLI/PRO/JOB                      
         MVC   SJREC.TRNKWORK,PCITSK      WC                                    
         MVC   SVSJCPJ,PCICLI+1                                                 
         MVC   SVTASK,PCITSK                                                    
         B     PTRN02                                                           
*                                                                               
         USING TRSELD,R2                                                        
PTRN06   CLI   0(R2),TRSELQ        X'60'                                        
         BNE   PTRN02                                                           
         MVC   SVACTDAT,TRSDATE    ACTIVITY DATE                                
         MVC   SVMOA,TRSPMOS       MOA                                          
         B     PTRN02                                                           
*                                                                               
PTRN12   MVC   DKEYSV,DKEY         SAVE ORIGINAL KEY                            
         BAS   RE,HIGH                                                          
         B     *+8                                                              
PTRN14   BAS   RE,SEQ                                                           
         LA    R5,DIR                                                           
         CLC   SJREC.TRNKEY(L'TRNKEY-1),DKEYSV  ALL BUT SUBREF                  
         BNE   XIT                                                              
*        BE    *+6                                                              
*        DC    H'0'                                                             
         CLC   SJREC.TRNKSMOS,SVMOA      SAME MOA                               
         BNE   PTRN14                                                           
         CLI   SJREC.TRNKSTYP,34         MUST BE A TYPE 34                      
         BNE   PTRN14                                                           
         L     R3,AIO1                                                          
         BAS   RE,GET              GET THE RECORD                               
         L     R5,AIO1                                                          
         LA    R2,SJREC.TRNRFST    DISP TO FIRST EL                             
         USING TRNELD,R2                                                        
         CLI   0(R2),TRNELQ        X'44'                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BATREF,TRNBTCH      MATCH BATCH REFERENCE                        
         BNE   PTRN14                                                           
         CLC   SVAMOUNT,TRNAMNT    MATCH AMOUNT                                 
         BNE   PTRN14                                                           
PTRN14A  SR    R0,R0               LOOK FOR THE 60EL                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),0             END OF RECORD                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         USING TRSELD,R2                                                        
         CLI   0(R2),TRSELQ        X'60'                                        
         BNE   PTRN14A                                                          
         CLC   SVACTDAT,TRSDATE    MATCH ACTIVITY DATE                          
         BNE   PTRN14                                                           
         LA    R2,SJREC.TRNRFST    DISP TO FIRST EL                             
PTRN14B  CLI   0(R2),0             END OF RECORD                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         USING PRTELD,R2                                                        
         CLI   0(R2),PRTELQ        X'40'                                        
         BE    PTRN14C                                                          
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    PTRN14B                                                          
         DC    H'0'                                                             
PTRN14C  SR    R1,R1                                                            
         IC    R1,PRTLN                                                         
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAV40(0),PRTELD     MATCH  40EL                                  
         BNE   PTRN14                                                           
         LA    R2,SJREC.TRNRFST    DISP TO FIRST EL                             
PTRN14D  CLI   0(R2),0             END OF RECORD                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         USING SPDELD,R2                                                        
         CLI   0(R2),SPDELQ        X'4C'                                        
         BE    PTRN14F                                                          
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    PTRN14D                                                          
         DC    H'0'                                                             
PTRN14F  CLC   SPDACCS(2),=C'SK'                                                
         BE    PTRN16                                                           
         CLC   SPDACCS(2),=C'SI'                                                
         BNE   PTRN16A                                                          
PTRN16   SR    R1,R1                                                            
         IC    R1,SPDLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVULA(0),SPDACCS                                                 
*                                                                               
PTRN16A  LA    R2,SJREC.TRNRFST    DISP TO FIRST EL                             
PTRN18   CLI   0(R2),0             END OF RECORD                                
         BNE   PTRN18A             IF NO X'65' NO PROBLEM                       
         LR    R7,R5                                                            
         LA    R6,=C'GETQ'                                                      
         BAS   RE,DMPGET                                                        
         AP    QUESREC,=P'1'       QUESTIONABLE RECS                            
         B     PTRN14              IF NO X'65' NO PROBLEM                       
*                                                                               
         USING ANOELD,R2                                                        
PTRN18A  CLI   0(R2),ANOELQ        X'65'                                        
         BE    PTRN18B                                                          
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    PTRN18                                                           
         DC    H'0'                                                             
PTRN18B  CLI   ANOTYPE,ANOTCLI                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ANOOFFC,SPACES                                                   
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCLIOFF,ANOOFFC                                                 
*                                                                               
PTRN20   DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,SVACTDAT),(1,SVTODAY3)                            
         LR    R7,R5                                                            
         LA    R6,=C'GETJ'                                                      
         BAS   RE,DMPGET                                                        
         BAS   RE,TIMEBLD                                                       
         BAS   RE,TIMEX                                                         
         AP    CHAREC,=P'1'                                                     
*                                                                               
PTRN120  DS    0H                                                               
         L     R1,ADTRANS          MAKE SURE WE HAVE TRANSACTION                
         LR    R4,R1                                                            
         SH    R4,DATADISP                                                      
         OI    TRNRSTA,TRNSDELT                                                 
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         MVI   MODE,WRITRANS                                                    
         B     XIT                                                              
         DROP  R2,R4,SJREC                                                      
         EJECT                                                                  
***********************************************************************         
* BUILD 8B TIMEL TO BE HOOKED ONTO 1R POSTINGS                        *         
***********************************************************************         
*                                                                               
TIMEBLD  NTR1                                                                   
         L     R1,ADTRANS                                                       
         LR    R3,R1                                                            
         SH    R3,DATADISP                                                      
         XC    DKEY,DKEY                                                        
         LA    R4,DKEY                                                          
         USING TRNRECD,R4                                                       
         MVC   TRNKEY,0(R3)        REREAD MONACC RECORD IN NEW STYLE            
         BAS   RE,READ                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         L     R3,AIO2                                                          
         BAS   RE,GET              GET THE RECORD                               
         L     R4,AIO2                                                          
*                                                                               
         LA    RE,ELEMENT          CLEAR EL BUILD AREA                          
         LA    RF,L'ELEMENT                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING TIMELD,R2                                                        
         LA    R2,ELEMENT          *** 8B INPUT DETAIL ELEMENT ***              
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMETYP,TIMEINP                                                  
         MVC   TIMACC,SVSJCPJ      SJ ACCOUNT                                   
         MVC   TIMOFF,SVCLIOFF     CLIENT OFFICE                                
         MVC   TIMTSK,SVTASK       TASK CODE                                    
         MVC   TIMTTYP,SVTYPE      TYPE OF TIME B/N/R                           
         MVI   TIMIND,TIMIADJ      ITEM WAS ADJUSTED                            
         MVC   TIMMOA,SVMOA        MONTH OF ACTIVITY                            
         OI    TIMSTAT,TIMNOTAX                                                 
         MVC   TIMADAT,SVTODAY3    ACTIVITY DATE                                
         ZAP   TIMHRS,SVHOURS      HOURS                                        
         LA    R1,TIMILN1Q         ADD TO RECORD LENGTH                         
*                                                                               
         CLI   SVTYPE,TIMTCB       B AND R TIME HAVE RATE                       
         BE    *+12                                                             
         CLI   SVTYPE,TIMTCR                                                    
         BNE   TIME100                                                          
         MVC   TIMRATE,SVRATE      RATE                                         
         MVI   TIMRBSTA,TIMRORAT   RATE INDICATOR                               
         MVC   TIMREFF,SVRATEFF    RATE EFFECTIVE DATE                          
         CLC   SVULA,SPACES                                                     
         BNH   *+16                                                             
         MVC   TIMINC,SVULA        INCOME ACCOUNT                               
         OC    TIMINC,SPACES                                                    
         ZAP   TIMAMNT,SVAMOUNT    AMOUNT                                       
         LA    R1,TIMILN2Q         ADD TO RECORD LENGTH                         
*                                                                               
TIME100  STC   R1,TIMLN                                                         
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO2,ELEMENT,=C'ADD=END'                
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         LR    R7,R4                                                            
         LA    R6,=C'PUTR'                                                      
         BAS   RE,DMPPUT                                                        
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* PASS RECORD TO TMSXFR FOR UPDATE TO TMS                             *         
***********************************************************************         
*                                                                               
TIMEX    NTR1                                                                   
*                                                                               
         USING TMSXFRD,R2                                                       
         LA    R2,ELEMENT                                                       
         XC    0(TMSXLNQ,R2),0(R2)                                              
         MVC   TMSDMGR,DATAMGR     A(DATAMGR)                                   
         MVC   TMSCFACS,ADCOMFAC   A(COMFACS)                                   
         USING TRNRECD,R4                                                       
         L     R4,AIO2                                                          
         STCM  R4,15,TMSADTRN      A(TRANSACTION RECORD)                        
         L     R6,ADCMPEL           COMPANY EL                                  
         USING CPYELD,R6                                                        
         MVC   TMSCPYS1,CPYSTAT1   COMPANY STATUS 1                             
         MVC   TMSCPYS2,CPYSTAT2   COMPANY STATUS 2                             
         MVC   TMSCPYS3,CPYSTAT3   COMPANY STATUS 3                             
         MVC   TMSCPYS4,CPYSTAT4   COMPANY STATUS 4                             
         MVC   TMSCPYS5,CPYSTAT5   COMPANY STATUS 5                             
         MVC   TMSCPYS6,CPYSTAT6   COMPANY STATUS 6                             
         MVC   TMSCPYS7,CPYSTAT7   COMPANY STATUS 7                             
         MVC   TMS1RACT,TRNKULA    1R ACCOUNT                                   
         MVC   TMSYMD,TRNKDATE     TRANSACTION DATE                             
         MVC   TMSTODAY,SVTODAY3   TODAY'S DATE                                 
         MVI   TMSACTN,TMSADD      ADD A NEW ITEM & BUMP REVISION #             
         MVC   TMSMOA,SVMOA                                                     
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
*                                                                               
         GOTO1 TMSXFR,(R2)                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        MVC   DA,TMS1RLQ4+1                                                    
*        OC    DA,DA                                                            
*        BZ    TIM08                                                            
*        L     R3,AIO2                                                          
*        BAS   RE,GET              GET THE RECORD                               
*        L     R7,AIO2                                                          
*        LA    R6,=C'GETT'                                                      
*        BAS   RE,DMPGET                                                        
*IM08    ICM   R7,15,TMS1RLQ4+5                                                 
*        LA    R6,=C'PUTT'                                                      
*        BAS   RE,DMPPUT                                                        
*                                                                               
         B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*              LEDGER LAST                                            *         
***********************************************************************         
*                                                                               
LDGL     DS    0H                                                               
         GOTO1 ACREPORT                                                         
         MVC   P(7),=C'LEDGER '                                                 
         MVC   P+8(1),QLEDGER                                                   
         MVC   P+10(6),=C'CHNGS '                                               
         EDIT  (P6,CHAREC),(14,P+20),MINUS=YES                                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         MVC   P+10(6),=C'QUESTS'                                               
         EDIT  (P6,QUESREC),(14,P+20),MINUS=YES                                 
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
HIGH     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
READ     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SEQ      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
WRT      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R3),DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ADD      LR    R0,RE                                                            
         XC    DA,DA                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PUT      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              DUMP OUT RECORDS                                       *         
***********************************************************************         
*                                                                               
DMPGET   NTR1                                                                   
         USING TRNRECD,R7                                                       
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
*                                                                               
DUMP     CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         SR    R8,R8                                                            
         ICM   R8,3,TRNRLEN                                                     
         GOTO1 PRNTBL,DMCB,(4,(R6)),(R7),C'DUMP',(R8),=C'2D'                    
XIT      XIT1                                                                   
         DROP                                                                   
         EJECT                                                                  
*****************************************                                       
*        LITERALS                                                               
*****************************************                                       
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                              
*-------------------------------------------------------------------*           
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
TMSXFR   DC    V(TMSXFR)                                                        
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
*                                                                               
CHAREC   DC    PL6'0'                                                           
QUESREC  DC    PL6'0'                                                           
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'5000'                                                        
*                                                                               
DKEY     DS    CL(L'ACCKEY)                                                     
DKEYSV   DS    CL(L'ACCKEY)                                                     
DIR      DS    CL64                                                             
DA       DS    F                                                                
ELEMENT  DS    CL255                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
IO1      DS    CL2000                                                           
IO2      DS    CL2000                                                           
         EJECT                                                                  
ACZID    DSECT                                                                  
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
PARM     DS    6F                                                               
TMSDATE  DS    XL3                                                              
*                                                                               
BATREF   DS    CL6                                                              
         ORG   BATREF                                                           
BATMOS   DS    CL2                                                              
BATBREF  DS    CL4                                                              
*                                                                               
SVSJCPJ  DS    0CL14               SJ UNIT/LEDGER/ACCOUNT                       
SVSJUL   DS    CL2                 SJ UNIT/LEDGER                               
SVSJCODE DS    CL12                SJ ACCOUNT CODE                              
SVTASK   DS    CL2                 TASK CODE                                    
SVCLIOFF DS    CL2                 CLIENT OFFICE                                
SVTYPE   DS    CL1                 TYPE OF TIME (BIT EQUIVALENT)                
SVIND    DS    XL1                 TYPE OF TIME INDICATOR BYTE                  
SVMOA    DS    PL2                 MOA X'60'                                    
SVTODAY3 DS    PL3                 ACTIVITY DATE                                
SVACTDAT DS    XL2                 ACTIVITY DATE X'60'                          
SVHOURS  DS    PL4                 HOURS                                        
SVRATE   DS    PL4                 RATE                                         
SVRATIND DS    XL1                 RATE BITS AS IN RECORD                       
SVRATEFF DS    PL3                 RATE EFFECTIVE DATE                          
SVULA    DS    CL14                SI/SK UNIT/LEDGER/ACCOUNT                    
SVAMOUNT DS    PL6                 AMOUNT                                       
*                                                                               
PK16     DS    PL16                                                             
*                                                                               
ELIST    DS    3A                  HELLO PARM LIST                              
ELERR    DS    0XL1                HELLO ERROR RETURN BYTE                      
ELADDR   DS    A                   HELLO ELEMENT ADDRESS (GET)                  
ELADDR2  DS    A                   HELLO ELEMENT ADDRESS (ADD)                  
*                                                                               
BATOFF   DS    CL2                                                              
ELCODE   DS    CL1                                                              
NOPUT    DS    CL1                                                              
SAV40    DS    CL100                                                            
IO       DS    CL1000                                                           
IOB      DS    CL1000                                                           
         EJECT                                                                  
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
*  ACTMSXFRD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACTMSXFRD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'157ACREPZI02 12/11/09'                                      
         END                                                                    
