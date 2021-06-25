*          DATA SET ACLFM26    AT LEVEL 048 AS OF 05/01/02                      
*PHASE T60326A,+0                                                               
         TITLE 'MODULE TO HANDLE RETAIL SCHEME'                                 
T60326   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**6326**,R7,RR=R5                                              
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         L     RE,COMFACS                                                       
         USING COMFACSD,RE                                                      
         MVC   SCANNER,CSCANNER                                                 
         ST    R5,PRELO                                                         
         MVI   ERROR,INVALID                                                    
         LA    R2,LOGACTH                                                       
         LA    R1,ACTAB                                                         
         SPACE 1                                                                
RTL01    CLC   8(1,R2),0(R1)       VALIDATE ACTION                              
         BE    RTL02                                                            
         LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   RTL01                                                            
         B     RTLXIT              INVALID ACTION                               
         SPACE 1                                                                
RTL02    MVC   THISACT,1(R1)                                                    
         CLC   THISACT,LASTACT     HAS THE ACTION CHANGED                       
         BE    RTL04                                                            
         MVI   ANYKEY,C'Y'         IF IT HAS, CONSIDER IT A KEY CHANGE          
         XC    NEXTKEY,NEXTKEY                                                  
         SPACE 1                                                                
RTL04    MVC   LASTACT,THISACT                                                  
         MVI   CHANGE,C'N'                                                      
         EJECT                                                                  
*              BUILD KEY                                                        
         SPACE 1                                                                
         CLI   MODE,BUILDKEY                                                    
         BNE   RTL20                                                            
         MVI   ERROR,MISSING                                                    
         LA    R2,LOGADVTH         VALID ADVERTISER                             
         CLI   5(R2),0                                                          
         BE    RTLXIT              MISSING INPUT                                
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVI   KEY+1,C'3'                                                       
         MVC   KEY+2(1),LOGADVT                                                 
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    RTL07                                                            
         MVC   LOGADNM,SPACES      CLEAR ADVERTISER NAME FIELD                  
         OI    LOGADNMH+6,X'80'    AND TRANSMIT                                 
         NI    LOGSCHMH+4,X'DF'    TURN-OFF SCHEME VALIDATED BIT                
         MVC   LOGSCNM,SPACES      CLEAR SCHEME NAME FIELD                      
         OI    LOGSCNMH+6,X'80'    AND TRANSMIT                                 
         BAS   RE,INIT             INITIALIZE SCREEN                            
         EJECT                                                                  
*              GET SCHEME RECORD AND DISPLAY NAME                               
         SPACE 1                                                                
RTL07    LA    R2,LOGSCHMH              SCHEME CODE FIELD                       
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    RTLXIT                    MISSING INPUT                          
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'           SCHEMES ARE WORK-CODE RECORDS                
         MVC   KEY+1(1),COMPANY                                                 
         MVI   KEY+2,C'3'          ALWAYS UNIT 3                                
         MVC   KEY+3(1),LOGADVT    LEDGER IS ADVERTISER                         
         MVC   KEY+4(2),LOGSCHM    AND THE CODE                                 
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    RTL08                                                            
         MVC   LOGSCNM,SPACES      CLEAR SCHEME NAME FIELD                      
         OI    LOGSCNMH+6,X'80'    AND TRANSMIT                                 
         SPACE 1                                                                
*              GET THE WORK-CODE RECORD                                         
         SPACE 1                                                                
         BAS   R9,RTLRD                                                         
         LA    R3,IO                                                            
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACANALD,R3                                                       
         MVC   LOGSCNM(15),ACANDESC   DISPLAY SCHEME NAME NAME                  
         OI    LOGSCHMH+4,X'20'       TURN ON VALID BIT                         
         MVI   ANYKEY,C'Y'                                                      
         SPACE 1                                                                
*              TURN OFF VALIDATED BIT FOR ALL KEY INPUT FIELDS                  
         SPACE 1                                                                
         NI    LOGACDEH+4,X'DF'                                                 
         NI    LOGBCDEH+4,X'DF'                                                 
         NI    LOGCCDEH+4,X'DF'                                                 
         NI    LOGFILTH+4,X'DF'                                                 
         EJECT                                                                  
*              VALIDATE HIGHER LEVEL ACCOUNT RECORDS                            
         SPACE 1                                                                
RTL08    MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      COMPANY                                      
         MVI   KEY+1,C'3'          UNIT                                         
         MVC   KEY+2(1),LOGADVT    LEDGER                                       
         SR    R1,R1                                                            
         IC    R1,NUMLEVS                                                       
         BCTR  R1,0                                                             
         MH    R1,=AL2(LVLNQ)                                                   
         LA    R4,LVLTAB(R1)       R4 TO LOW LEVEL ENTRY                        
         ST    R4,ADLVLST          A(ACCOUNT LEVEL ENTRY)                       
         LA    R2,LOGACDEH         VALIDATE ACCOUNT CODES                       
         LA    R0,3                3 POSSIBLE ENTRIES                           
         LA    R4,LVLTAB           HEIRARCHY DATA                               
         SPACE 1                                                                
         USING LVLD,R4                                                          
RTL09    TM    1(R2),X'20'         LEVEL NOT USED                               
         BO    RTL11                                                            
         MVI   ERROR,MISSING       MISSING INPUT                                
         CLI   5(R2),0                                                          
         BE    RTLXIT                                                           
         MVI   ERROR,INVALID       INVALID INPUT                                
         CLC   5(1,R2),LVLIPLN     MAXIMUM ACCOUNT LENGTH                       
         BH    RTLXIT                                                           
         SR    R1,R1                                                            
         IC    R1,LVLDSP           DISPLACEMENT TO THIS LEVEL                   
         LA    R5,KEY+3(R1)                                                     
         IC    R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                                                             
         EX    R1,MVCDE            ACCOUNT CODE TO KEY                          
         CLI   ANYKEY,C'Y'         IF ALREADY HAVE KEY CHANGE                   
         BE    *+12                VALID ALL LOWER LEVELS                       
         TM    4(R2),X'20'         IF NOT CHANGED                               
         BO    RTL10               SKIP THE READ                                
         BAS   R9,RTLRD            GET THE ACCOUNT RECORD                       
         GOTO1 NAMOUT              AND DISPLAY NAME                             
         OI    4(R2),X'20'         TURN OF VALIDATED BIT                        
         MVI   ANYKEY,C'Y'         INDICATE KEY CHANGE                          
         SPACE 1                                                                
RTL10    LA    R4,LVLNQ(R4)        NEXT ENTRY ON LVLTAB                         
         SR    R1,R1                                                            
         LA    R3,3                R2 TO NEXT ACCOUNT CODE                      
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,*-6                                                           
         BCT   R0,RTL09                                                         
         EJECT                                                                  
*              VALIDATE FILTER/OPTIONS LINE                                     
         SPACE 1                                                                
RTL11    DS    0H                                                               
         MVC   HIKEY,KEY                 SAVE HIGH LEVEL KEY                    
         LA    R2,LOGFILTH                                                      
         TM    LOGFILTH+4,X'20'                                                 
         BO    RTL12                    FIELD NOT CHANGED                       
         BAS   RE,VALOPT                                                        
         CLI   ERROR,OK                                                         
         BNE   RTLXIT                   INVALID OPTION                          
         MVI   ANYKEY,C'Y'                                                      
         OI    LOGFILTH+4,X'20'                                                 
         SPACE 1                                                                
RTL12    MVI   ERROR,X'FF'                                                      
         CLI   ANYKEY,C'Y'                                                      
         BNE   XIT                                                              
         XC    NEXTKEY,NEXTKEY     IF KEY CHANGE START AGAIN                    
         LA    R2,LOGDCDEH         R2 TO FIRST DISTRIBUTOR FIELD                
         NI    4(R2),X'DF'         TURN-OFF PREVIOUSLY VALIDATED                
         MVI   MYMODE,DSPLYREC                                                  
         B     RTLXIT                                                           
         EJECT                                                                  
*              DISPLAY NEXT ACCOUNT RECORDS                                     
         SPACE 1                                                                
RTL20    CLI   MYMODE,DSPLYREC                                                  
         BNE   RTL30                                                            
RTL21    LA    R2,LOGDCDEH                                                      
         LA    R3,LOGLAST              A(END OF SCREEN)                         
         TWAXC (R2),(R3),PROT=Y        CLEAR SCREEN                             
         XC    UNITS(40),UNITS                                                  
         LA    R8,UNITS                                                         
         MVC   KEY,SPACES                                                       
         L     R4,ADLVLST          R4 TO ACCOUNT LEVEL ENTRY                    
         CLI   NEXTKEY,X'FF'                                                    
         BNE   *+10                IF END                                       
         XC    NEXTKEY,NEXTKEY     START AT BEGINNING                           
         CLI   NEXTKEY,0           IF NO SAVED KEY                              
         BE    RTL22               START AT THE TOP                             
         MVC   KEY(15),NEXTKEY                                                  
         B     RTL23               NOT FIRST START AT NEXT                      
         SPACE 1                                                                
RTL22    MVC   KEY,HIKEY           FIRST TIME - START AT FIRST ACCOUNT          
         MVI   KEY+14,X'41'                                                     
         CLI   STRKEY,0            IS THERE A START KEY                         
         BE    RTL23                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'STRKEY),STRKEY    START AT SPECIFIED ACCOUNT               
         SPACE 1                                                                
RTL23    BAS   R9,RTLHI                                                         
         CLI   KEY+3,C'*'          SKIP * ACCOUNTS                              
         BE    RTL26                                                            
         MVI   NEXTKEY,X'FF'                                                    
         SR    R1,R1                                                            
         IC    R1,LVLDSP           DISPLACEMENT TO ACCOUNT LEVEL                
         AH    R1,=H'2'            PLUS C/U/L = LENGTH FOR EX INSTR.            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEYSAVE(0),KEY                                                   
         BNE   RTL70               NOT THIS CLUSTER                             
         BAS   R9,GETNAME          SAVE DISTRIBUTOR NAME                        
         CLC   SVNAME,SPACES       DID WE GET A NAME?                           
         BE    RTL26               NO, READ NEXT RECORD                         
         BAS   RE,FILTR            FILTER THE ACCOUNT                           
         BNE   RTL26                                                            
         SPACE 1                                                                
RTL25    LA    R3,LOGENDH          R3 TO END OF SCREEN                          
         CR    R2,R3                                                            
         BE    RTL29               SCREEN IS FULL                               
         BAS   RE,RTLDSP           DISPLAY NAME AND UNITS                       
         LA    R2,LINLNQ(R2)                                                    
         LA    R8,4(R8)            NEXT UNITS SAVE AREA                         
         SPACE 1                                                                
RTL26    SR    R3,R3               READ HIGH TO NEXT ACCOUNT                    
         IC    R3,KEY+14                                                        
         AH    R3,=H'1'                                                         
         STC   R3,KEY+14                                                        
         B     RTL23               GET NEXT ACCOUNT                             
         SPACE 1                                                                
RTL29    MVC   NEXTKEY,KEY         SAVE NEXT KEY                                
         B     RTL70               AND BEAT-IT                                  
         EJECT                                                                  
*              BUILDREC - CHECK FOR CHANGE OF UNITS VALUE                       
         SPACE 1                                                                
         USING LIND,R2                                                          
RTL30    DS    0H                                                               
         CLI   MYMODE,BUILDREC                                                  
         BNE   XIT                                                              
         LA    R2,LOGDCDEH         FIRST ACCOUNT FIELD                          
         LA    R3,LOGENDH          R3 TO END OF SCREEN                          
         LA    R8,UNITS            UNITS SAVE AREA                              
         SPACE 1                                                                
*                                  VALID UNITS BEFORE STARTING UPDATE           
RTL32    MVC   LINPCT,SPACES            CLEAR THE PERCENTS                      
         OI    LINPCTH+6,X'80'                                                  
         OC    LINACC,SPACES                                                    
         CLI   LINACC,X'41'        END OF ACCOUNTS                              
         BL    RTL36                                                            
         TM    LINUNTH+4,X'20'                                                  
         BO    RTL35               NOT CHANGED                                  
*                                                                               
         CLI   THISACT,CHA         ARE WE CHANGING?                             
         BNE   RTL33               NO, SKIP THIS                                
         LR    R5,R2               SAVE R2                                      
         LA    R2,LINUNTH          POINT TO UNITS                               
         MVI   ERROR,INVALID                                                    
         CLI   LFMCTRY,X'08'       IS THIS CANADA?                              
         BE    RTLXIT              YES, CAN'T CHANGE UNITS                      
         LR    R2,R5               RESTORE R2                                   
*                                                                               
RTL33    ZAP   DUB,=P'0'                                                        
         MVI   CHANGE,C'Y'         INDICATE SOME CHANGE                         
         CLI   LINUNTH+5,0                                                      
         BE    RTL34                                                            
         LR    R5,R2               SAVE R2                                      
         LA    R2,LINUNTH                                                       
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         LA    RF,2                                                             
         CLI   TYPE,C'P'         FOR 100%  MUST BE 4 DP                         
         BNE   *+8                                                              
         LA    RF,4                                                             
         MVI   ERROR,CASHERR                                                    
         GOTO1 CASHVAL,DMCB,((RF),8(R2)),(R0)                                   
         CLI   DMCB,0                                                           
         BNE   RTLXIT                   INVALID AMOUNT                          
         L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
**T                                                                             
         CP    DUB,=P'0'                                                        
         BL    RTLXIT                                                           
**T                                                                             
         LR    R2,R5               RESTORE R2                                   
         MVI   ERROR,X'FF'                                                      
         SPACE 1                                                                
RTL34    CVB   R1,DUB                                                           
         ST    R1,0(R8)                 SAVE UNITS IN TWA                       
         SPACE 1                                                                
RTL35    LA    R2,LINLNQ(R2)       R2 TO NEXT LINE                              
         LA    R8,4(R8)            NEXT UNITS FIELD                             
         CR    R2,R3                                                            
         BL    RTL32               CHECK NEXT LINE                              
         SPACE 1                                                                
*              UPDATE SCHEME ELEMENT ON ACCOUNT RECORD                          
         SPACE 1                                                                
RTL36    CLI   CHANGE,C'N'                                                      
         BE    RTL21               NO CHANGES - DISPLAY NEXT SET                
         ZAP   ADJAMT,=P'0'                                                     
         LA    R2,LOGDCDEH         FIRST ACCOUNT FIELD                          
         LA    R8,UNITS                                                         
         SPACE 1                                                                
RTL37    CLI   LINACC,X'41'        END OF ACCOUNTS                              
         BL    RTL60                                                            
         TM    LINUNTH+4,X'20'                                                  
         BO    RTL38               NOT CHANGED                                  
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),HIKEY       C/U/L/LEVL1 ETC                              
         L     R4,ADLVLST          R4 TO ACCOUNT LEVEL ENTRY                    
         SR    R1,R1                                                            
         IC    R1,LVLDSP           DISPLACEMENT TO LOW LEVEL                    
         LA    R5,KEY+3(R1)                                                     
         IC    R1,LVLIPLN                                                       
         BCTR  R1,0                                                             
         EX    R1,MVCDE            STORE CODE TO KEY                            
         L     R1,0(R8)            NEW AMOUNT                                   
         CVD   R1,NEWAMT                                                        
         AP    ADJAMT,NEWAMT       ADD TO TOTAL ADJUSTMENT                      
         MVI   ADJCODE,C'A'        SET ADJUSTMENT CODE TO ACCOUNT LEVEL         
         BAS   RE,RTLUPD           UPDATE SCHEME ELEMENT                        
         OI    LINUNTH+4,X'20'                                                  
         SPACE 1                                                                
RTL38    LA    R8,4(R8)            NEXT UNITS SAVE AREA                         
         LA    R2,LINLNQ(R2)       R2 TO NEXT LINE                              
         CR    R2,R3                                                            
         BL    RTL37               CHECK NEXT LINE                              
         EJECT                                                                  
*              UPDATE MARKET, REGION, ADVERTISER TOTALS                         
         SPACE 1                                                                
RTL60    CP    ADJAMT,=P'0'        NET ADJUSTMENT IS ZERO                       
         BE    RTL71               NO NEED TO UPDATE                            
         MVI   ADJCODE,C'H'        SET ADJUSTMENT CODE TO HIGH LEVEL            
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),HIKEY                                                    
         LA    R2,LOGADVTH                                                      
         SR    R0,R0                                                            
         IC    R0,NUMLEVS          NUMBER OF LEVELS                             
         SH    R0,=H'2'                                                         
         BM    RTL62               ONLY ONE LEVEL                               
         SPACE 1                                                                
*              FIX HIGH LEVEL ACCOUNTS                                          
RTL61    BAS   RE,RTLUPD           CHANGE/ADD SCHEME ELEMENT                    
         LR    R3,R0                                                            
         MH    R3,=AL2(LVLNQ)                                                   
         LA    R4,LVLTAB(R3)       R4 TO LEVEL ENTRY TABLE                      
         SR    R1,R1                                                            
         IC    R1,LVLDSP                                                        
         LA    R5,KEY+3(R1)        R5 TO LEVEL CODE IN KEY                      
         IC    R1,LVLIPLN          LENGTH OF THIS LEVEL                         
         BCTR  R1,0                                                             
         EX    R1,MVSPA            SPACES TO LOW LEVEL KEY FIELD                
         CH    R0,=H'0'                                                         
         BE    RTL62                                                            
         SH    R0,=H'1'                                                         
         B     RTL61               NEXT LOWER LEVEL                             
         SPACE 1                                                                
*                                  NOW FIX THE * ACCOUNTS                       
RTL62    MVC   KEY,SPACES                                                       
         MVC   KEY(3),HIKEY                                                     
         SR    R0,R0                                                            
         IC    R0,NUMLEVS          NUMBER OF LEVELS                             
         LA    R4,LVLTAB                                                        
RTL63    SR    R1,R1                                                            
         IC    R1,LVLDSP                                                        
         LA    R5,KEY+3(R1)        R5 TO LEVEL CODE IN KEY                      
         IC    R1,LVLIPLN          LENGTH OF THIS LEVEL                         
         BCTR  R1,0                                                             
         EX    R1,*+8              * TO ACCOUNT                                 
         B     *+10                                                             
         MVC   0(0,R5),=12C'*'                                                  
         BAS   RE,RTLUPD           CHANGE/ADD SCHEME ELEMENT                    
         LA    R4,LVLNQ(R4)                                                     
         BCT   R0,RTL63            NEXT LEVEL                                   
         B     RTL71                                                            
         EJECT                                                                  
*              EXIT ROUTINES                                                    
         SPACE 1                                                                
RTL70    LA    R2,LOGADVTH                                                      
         ST    R2,FADR                                                          
         LA    R1,NORCL            NO RECORDS TO DISPLAY                        
         ST    R1,MADR                                                          
         CLI   LOGDCDE,X'41'       NO STORE CODE                                
         BL    RTL79                                                            
         LA    R1,RCDSL            RECORDS DISPLAYED                            
         CLI   NEXTKEY,X'FF'                                                    
         BE    *+8                                                              
         LA    R1,RCDSNL         RECORDS DISPLAYED - ENTER FOR NEXT             
         ST    R1,MADR                                                          
         CLI   THISACT,DIS                                                      
         BE    RTL72                                                            
         CLI   CHANGE,C'Y'                                                      
         BE    RTL71                                                            
         LA    R1,RCDSCL         RECORDS DISPLAYED ENTER CHANGES                
         ST    R1,MADR                                                          
         LA    R2,LOGDCDEH                                                      
         LA    R1,LINUNTH                                                       
         ST    R1,FADR                                                          
         MVI   MYMODE,BUILDREC                                                  
         B     RTL72                                                            
         SPACE 1                                                                
RTL71    LA    R2,LOGDCDEH                                                      
         LA    R1,LINUNTH                                                       
         ST    R1,FADR                                                          
         LA    R1,RCAML            RECORDS AMENDED                              
         CLI   NEXTKEY,X'FF'       NO MORE TO DISPLAY                           
         BE    *+8                                                              
         LA    R1,RCAMNL           RECORDS AMENDED - ENTER FOR NEXT             
         ST    R1,MADR                                                          
         EJECT                                                                  
*              GET  *** ACCOUNT AND DISPLAY TOTAL                               
         SPACE 1                                                                
RTL72    MVC   LOGNAR,SPACES       CLEAR TOTAL LINE                             
         OI    LOGNARH+6,X'80'                                                  
         NI    LOGNARH+1,X'FF'-X'0C' NORMAL INTENSITY                           
         MVC   LOGTOT,SPACES                                                    
         OI    LOGTOTH+6,X'80'                                                  
         MVC   KEY,SPACES          BUILD ACCOUNT KEY                            
         MVC   KEY(1),COMPANY      COMPANY                                      
         MVI   KEY+1,C'3'          UNIT                                         
         MVC   KEY+2(1),LOGADVT    LEDGER                                       
         SR    R1,R1                                                            
         IC    R1,STRLEN           LENGTH OF THE * RECORD                       
         BCTR  R1,0                                                             
         EX    R1,MVSTAR                                                        
         CLI   JOBKEY,0                                                         
         BE    *+10                                                             
         MVC   KEY+17(15),JOBKEY   FILTER ON JOB TOTALS                         
         BAS   R9,RTLHI            READ                                         
         CLC   KEY,KEYSAVE                                                      
         BNE   RTL79               NO TOTAL RECORD                              
         ZAP   TOTUNT,=P'0'                                                     
         LA    R3,IO                                                            
         BAS   R9,RTLGTL           GET MATCH 62 ELEMENT                         
         BNE   RTL79               NO TOTAL SO FAR                              
         USING ACDISTD,R3                                                       
         ZAP   TOTUNT,ACDIVAL      TOTAL UNITS                                  
         EDIT  ACDIVAL,(12,EDWRK),2,ALIGN=LEFT                                  
         MVC   LOGTOT,EDWRK        UNITS TO SCREEN                              
         MVC   LOGNAR(16),=C'TOTAL FOR SCHEME'                                  
         MVC   LOGNAR+18(2),LOGSCHM                                             
         CLI   TYPE,C'P'           IS IT PERCENTAGE TYPE                        
         BNE   RTL73               IF NOT - DON'T CHECK TOTAL                   
         EDIT  ACDIVAL,(12,EDWRK),4,ALIGN=LEFT,DROP=2                           
         MVC   LOGTOT,EDWRK        UNITS TO SCREEN                              
         CP    TOTUNT,=P'1000000'  IS THE TOTAL 100%                            
         BE    RTL73                                                            
         MVC   LOGNAR(22),=C'WARNING TOTAL NOT 100%'                            
         OI    LOGNARH+1,X'08'     HIGH INTENSITY                               
         SPACE 1                                                                
*              DISPLAY PERCENTAGES                                              
         SPACE 1                                                                
RTL73    CP    TOTUNT,=P'0'                                                     
         BE    RTL79               TOTAL IS ZERO                                
         LA    R2,LOGDCDEH                                                      
         LA    R8,UNITS                                                         
         LA    R3,10               TEN LINES                                    
         SPACE 1                                                                
RTL74    L     R1,0(R8)            UNITS ON THIS LINE                           
         LTR   R1,R1                                                            
         BZ    RTL78                                                            
         CVD   R1,DUB                                                           
         ZAP   PL16,DUB                                                         
         MP    PL16,=P'10000000'                                                
         DP    PL16,TOTUNT         DIVIDED BY TOTAL                             
         SRP   PL16(10),64-1,5                                                  
         EDIT  (P10,PL16),(8,EDWRK),4,ALIGN=LEFT,DROP=2                         
         MVC   LINPCT,EDWRK                                                     
         OI    LINPCTH+6,X'80'                                                  
         SPACE 1                                                                
RTL78    LA    R2,LINLNQ(R2)                                                    
         LA    R8,4(R8)                                                         
         BCT   R3,RTL74                                                         
         SPACE 1                                                                
RTL79    L     R1,MADR             A(MESSAGE)                                   
         SR    R3,R3                                                            
         IC    R3,0(R1)            LENGTH OF MESSAGE                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LOGHEAD(0),1(R1)    MESSAGE TO SCREEN                            
         MVI   ERROR,X'FE'                                                      
         L     R2,FADR             POSITON CURSOR                               
RTLXIT   XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              GET ACCOUNT RECORD AND UPDATE ELEMENT                            
         SPACE 1                                                                
         USING ACKEYD,R6                                                        
RTLUPD   NTR1                                                                   
         LA    R6,KEY                                                           
         CLI   JOBKEY,0                                                         
         BE    *+10                                                             
         MVC   ACKEYCON,JOBKEY     FILTERING ON JOB                             
         BAS   R9,RTLHI                                                         
         CLC   KEY,KEYSAVE         RECORD ON FILE                               
         BE    RTLUPD2             OK TO UPDATE                                 
         LA    R6,IO2              NOT ON FILE BUILD NEW RECORD                 
         MVC   ACKEYACC(ACLENGTH-ACKEYD),SPACES                                 
         MVC   ACKEYACC(L'KEYSAVE),KEYSAVE    KEY                               
         MVC   ACLENGTH,DATADISP                                                
         XC    ACSTATUS(20),ACSTATUS                                            
         MVC   ELEMENT(L'CNTRJOB),CNTRJOB    CONTRA ELEMENT                     
         CLI   ACCEMU,C'Y'             EMULATED FILE ?                          
         BNE   *+10                    NO                                       
         XC    ACKEYD+36(6),ACKEYD+36  YES, ADJUST KEY FOR NEW FILE             
         CLI   ACKEYCON,X'40'          FILTERING ON JOB?                        
         BNE   RTLUPD1                 YES,OK                                   
         DC    H'0'                    NO, ALL OTHERS ADDED IN INIT             
         SPACE 1                                                                
RTLUPD1  GOTO1 ADDANEL                        ADD THE CONTRA ELEMENT            
         B     RTLUPD3                                                          
         SPACE 1                                                                
RTLUPD2  MVI   UPDATE,C'Y'                                                      
         GOTO1 READ                                                             
         LA    RF,IO2                                                           
         LA    RE,IO                                                            
         SR    R1,R1                                                            
         ICM   R1,3,ACLENGTH-ACKEYD(RE)                                         
         MOVE  ((RF),(R1)),(RE)    MOVE IO TO IO2                               
         SPACE 1                                                                
RTLUPD3  LA    R3,IO2                                                           
         BAS   R9,RTLGTL           GET 62 ELEMENT FOR THIS SCHEME               
         BNE   RTLUPD6             NOT FOUND ADD ONE                            
         CLI   ADJCODE,C'H'        ADJUSTING HIGH LEVEL ACCOUNT                 
         BE    RTLUPD4                                                          
         SP    ADJAMT,ACDIVAL      REDUCE OLD AMOUNT                            
         CP    NEWAMT,=P'0'        INPUT VALUE ZERO                             
         BE    RTLUPD7             DELETE ELEMENT                               
         ZAP   ACDIVAL,NEWAMT                                                   
         B     RTLUPD8             PUT UPDATED RECORD                           
         SPACE 1                                                                
RTLUPD4  AP    ACDIVAL,ADJAMT      NET ADJUSTMENT                               
         CP    ACDIVAL,=P'0'       IF ZERO DELETE ELEMENT                       
         BE    RTLUPD7                                                          
         B     RTLUPD8             PUT UPDATED RECORD                           
         SPACE 1                                                                
RTLUPD5  CP    NEWAMT,=P'0'        NO ELEMENT FOR ZERO                          
         BE    XIT                                                              
         AP    ADJAMT,NEWAMT       BUILD AND ADD NEW ELEMENT                    
         SPACE 1                                                                
RTLUPD6  LA    R3,ELEMENT                                                       
         MVI   ACDIEL,X'62'                                                     
         MVI   ACDILEN,X'0A'                                                    
         MVC   ACDICODE,LOGSCHM                                                 
         ZAP   ACDIVAL,NEWAMT                                                   
         CLI   ADJCODE,C'H'        FOR HIGH LEVEL USE ADJUSTMENT AMOUNT         
         BNE   *+10                                                             
         ZAP   ACDIVAL,ADJAMT                                                   
         GOTO1 ADDANEL                                                          
         B     RTLUPD8                                                          
         SPACE 1                                                                
RTLUPD7  MVI   ACDIEL,X'FF'                                                     
         GOTO1 REMANEL,DMCB,0                                                   
         SPACE 1                                                                
RTLUPD8  CLC   KEY,KEYSAVE                                                      
         BE    RTLUPD9             FOUND RECORD                                 
         GOTO1 ADDREC              OR MUST ADD ONE                              
         B     XIT                                                              
         SPACE 1                                                                
RTLUPD9  GOTO1 PUTREC              WRITE UPDATED RECORD                         
         B     XIT                                                              
         SPACE 1                                                                
MVSTAR   MVC   KEY+3(0),=12C'*'                                                 
MVSPA    MVC   0(0,R5),SPACES                                                   
MVCDE    MVC   0(0,R5),8(R2)       ACCOUNT CODE TO KEY                          
         EJECT                                                                  
*              DISPLAY NAME AND UNITS                                           
         SPACE 1                                                                
RTLDSP   NTR1                                                                   
         LA    R3,IO                                                            
         SR    R5,R5                                                            
         IC    R5,LVLDSP           DISPLACEMENT TO LAST LEVEL                   
         LA    R5,3(R5,R3)         R5 TO STORE CODE IN IO AREA                  
         SR    R1,R1                                                            
         IC    R1,LVLIPLN          LENGTH OF LOW LEVEL ACCOUNT                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINACC(0),0(R5)     STORE CODE TO SCREEN                         
         OC    LINACC,SPACES       FILL WITH SPACES - TO BE USED IN KEY         
         OI    LINUNTH+4,X'20'                                                  
         MVC   LINNM,SVNAME        DISPLAY NAME                                 
         CLI   THISACT,NEW                                                      
         BE    XIT                 NO UNITS IF NEW                              
         BAS   R9,RTLGTL           GET DISTRIBUTION SCHEME ELEMENT              
         BNE   XIT                 NO SCHEME                                    
         EDIT  ACDIVAL,(12,EDWRK),2,ALIGN=LEFT                                  
         CLI   TYPE,C'P'                                                        
         BNE   RTLDSP1                                                          
         EDIT  ACDIVAL,(12,EDWRK),4,ALIGN=LEFT,DROP=2                           
RTLDSP1  MVC   LINUNT,EDWRK        UNITS TO SCREEN                              
         ZAP   DUB,ACDIVAL                                                      
         CVB   R1,DUB                                                           
         ST    R1,0(R8)            SAVE UNITS TO CALCULATE PERCENT              
         B     XIT                                                              
         EJECT                                                                  
*        GET MATCHING 62 ELEMENT                                                
         SPACE 1                                                                
RTLGTL   MVI   ELCODE,X'62'        R3 POINT TO RECORD                           
         BAS   RE,GETEL                                                         
RTLGTL1  BNER  R9                                                               
         CLC   ACDICODE,LOGSCHM    FIND 62 ELEMENT WITH PROPER SCHEME           
         BER   R9                                                               
         BAS   RE,NEXTEL                                                        
         B     RTLGTL1                                                          
         SPACE 1                                                                
GETNAME  MVC   SVNAME,SPACES            GET ACCOUNT NAME                        
         LA    R3,IO                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNER  R9                       RETURN IF NOT FOUND                     
*                                                                               
         USING ACNAMED,R3                                                       
         SR    R1,R1                                                            
         IC    R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+6                                                           
         BR    R9                                                               
         MVC   SVNAME(0),ACNMNAME                                               
         EJECT                                                                  
*              IO ROUTINES                                                      
         SPACE 1                                                                
RTLRD    DS    0H             USING FULLKEY OPTION-KEY MUST BE IN IO            
         MVC   IO(ACLENGTH-ACKEYD),SPACES                                       
         MVC   IO(L'KEY),KEY                                                    
         GOTO1 READ                                                             
         BR    R9                                                               
         SPACE 1                                                                
RTLHI    DS    0H                                                               
         MVC   IO(ACLENGTH-ACKEYD),SPACES                                       
         MVC   IO(L'KEY),KEY                                                    
         GOTO1 HIGH                                                             
         BR    R9                                                               
         EJECT                                                                  
*              FILTER THE ACCOUNT                                               
         SPACE 1                                                                
FILTR    NTR1                                                                   
         OC    FLT1(4),FLT1                                                     
         BZ    FILTR1              NO ACCOUNT FILTERS                           
         MVI   ELCODE,X'30'                                                     
         LA    R3,IO                                                            
         BAS   RE,GETEL            GET STATUS ELEMENT                           
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACSTATD,R3                                                       
         LA    R1,ACSTFILT         CHECK FILTERS                                
         LA    RF,FLT1                                                          
         BAS   RE,FILTER                                                        
         B     FILTNO                                                           
         LA    R1,ACSTFILT+1                                                    
         LA    RF,FLT2                                                          
         BAS   RE,FILTER                                                        
         B     FILTNO                                                           
         LA    R1,ACSTANAL                                                      
         LA    RF,FLT3                                                          
         BAS   RE,FILTER                                                        
         B     FILTNO                                                           
         LA    R1,ACSTSUB                                                       
         LA    RF,FLSC                                                          
         BAS   RE,FILTER                                                        
         B     FILTNO                                                           
         SPACE 1                                                                
         USING ACKEYD,R6                                                        
FILTR1   CLI   THISACT,NEW                                                      
         BE    FILTR3                                                           
         CLI   JOBKEY,0            FILTERING BY JOB                             
         BE    FILTR2                                                           
         MVC   SVKY(15),KEY        SAVE CURRENT                                 
         LA    R6,KEY                                                           
         MVC   ACKEYCON,JOBKEY                                                  
         BAS   R9,RTLHI            READ FOR ACCOUNT WITH JOB AS CONTRA          
         CLC   KEY,KEYSAVE                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),SVKY        RESTORE ACCOUNT KEY                          
         BNE   FILTNO              JOB RECORD NOT  FOUND                        
         SPACE 1                                                                
         USING ACDISTD,R3                                                       
FILTR2   LA    R3,IO                                                            
         BAS   R9,RTLGTL           GET MATCHING 62 ELEMENT                      
         BNE   FILTNO              ELEMENT NOT FOUND                            
         OC    FLTUN,FLTUN                                                      
         BZ    *+14                NO UNITS FILTER                              
         CP    ACDIVAL,FLTUN                                                    
         BNE   FILTNO                                                           
         OC    FLTUNEG,FLTUNEG     NEGATIVE FILTER                              
         BZ    *+14                                                             
         CP    ACDIVAL,FLTUNEG                                                  
         BE    FILTNO                                                           
         B     FILTYES                                                          
         SPACE 1                                                                
FILTR3   DS    0H                  FOR NEW DISPLAY ACCOUNTS                     
         LA    R6,KEY                                                           
         CLI   JOBKEY,0                                                         
         BE    FILTR4                                                           
         MVC   SVKY(15),KEY        SAVE CURRENT                                 
         MVC   ACKEYCON,JOBKEY                                                  
         BAS   R9,RTLHI            READ FOR ACCOUNT WITH JOB AS CONTRA          
         CLC   KEY,KEYSAVE                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),SVKY        RESTORE ACCOUNT KEY                          
         BE    FILTR4              FOUND RECORD - LOOK FOR SCHEME               
         BAS   R9,RTLHI                                                         
         B     FILTYES             DISPLAY FOR NEW                              
         SPACE 1                                                                
FILTR4   LA    R3,IO                                                            
         BAS   R9,RTLGTL           GET MATCHING 62 ELEMENT                      
         BNE   FILTYES             IF NOT FOUND - DISPLAY FOR NEW               
         B     FILTNO                                                           
         SPACE 1                                                                
FILTYES  CR    RB,RB                                                            
         B     XIT                                                              
FILTNO   LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO APPLY REQUEST FILTER TO ACCOUNT FILTER VALUE                       
         SPACE 1                                                                
FILTER   CLI   0(RF),0             NO FILTER                                    
         BE    4(RE)                                                            
         MVC   WORK(1),0(RF)                                                    
         MVC   WORK+1(1),0(R1)                                                  
         LA    R1,X'80'            SET MASK TO EQUAL                            
         TM    WORK,X'40'          TEST POSITIVE FILTER                         
         BNZ   *+12                                                             
         LA    R1,X'70'            SET MASK TO NOT EQUAL                        
         OI    WORK,X'40'                                                       
         CLC   WORK(1),WORK+1      MATCH REQUEST FILTER TO VALUE                
         EX    R1,*+8                                                           
         B     0(RE)               EXCLUDE                                      
         NOP   4(RE)               INCLUDE                                      
         EJECT                                                                  
*              INITIALIZE THE SCREEN                                            
         SPACE 1                                                                
INIT     NTR1                                                                   
         TWAXC LOGADESH,LOGENDH,PROT=Y                                          
         OI    LOGADESH+1,X'0C'    ZERO INTENSITY - DESCRIPTION                 
         OI    LOGACDEH+1,X'20'    PROTECT CODE FIELD                           
         OI    LOGBDESH+1,X'0C'    FOR HIGH LEVEL ACCOUNTS                      
         OI    LOGBCDEH+1,X'20'    IN CASE THERE ARE NOT USED                   
         OI    LOGCDESH+1,X'0C'                                                 
         OI    LOGCCDEH+1,X'20'                                                 
         MVC   LOGUN,=C'UNITS'                                                  
         MVC   LOGPC,=C'PERCENT'                                                
         LA    R6,IO                                                            
         MVC   ACKEYACC(ACLENGTH-ACKEYD),SPACES                                 
         MVC   ACKEYACC(L'KEY),KEY                                              
         GOTO1 READ                GET THE LEDGER RECORD                        
         LA    R3,IO                                                            
         SPACE 1                                                                
         LA    R2,LOGADVTH                                                      
         GOTO1 NAMOUT                                                           
         OI    LOGADVTH+4,X'20'                                                 
         MVI   ANYKEY,C'Y'                                                      
         MVI   TYPE,0                                                           
         LA    R3,IO                                                            
         MVI   ELCODE,X'14'        GET LEDGER ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO LEDGER ELEMENT                            
         USING ACLEDGD,R3                                                       
         TM    ACLTSTAT,X'01'                                                   
         BNO   *+8                                                              
         MVI   TYPE,C'P'           TYPE IS 100%                                 
         LA    R3,IO                                                            
         MVI   ELCODE,X'16'                                                     
         BAS   RE,GETEL            R3 TO LEDGER ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACHEIRD,R3                                                       
         LA    R4,LVLTAB           R4 TO LEVEL CONTROL TABLE                    
         LA    R3,ACHRLEVA         R3 TO LEVEL ENTRIES                          
         USING ACHRLEVA,R3                                                      
         MVI   STRLEN,12           LENGTH OF MOVE FOR THE * ACCOUNT             
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         SR    R5,R5                                                            
         SPACE 1                                                                
INIT1    LA    R1,1(R1)                                                         
         IC    R5,ACHRLEVA                                                      
         STC   R5,LVLKYLN          CUMULATIVE(KEY) LENGTH                       
         CLI   LVLKYLN,12          SAVE LENGTH OF NEXT TO LAST LEVEL            
         BE    *+8                                                              
         STC   R5,STRLEN                                                        
         SR    R5,R2               CUM LESS PREVIOUS CUM                        
         STC   R5,LVLIPLN          LENGTH OF THIS LEVEL                         
         IC    R2,LVLKYLN          SAVE CUM LENGTH FOR NEXT TIME                
         LR    R0,R2                                                            
         SR    R0,R5                                                            
         STC   R0,LVLDSP           DISPLACEMENT TO THIS LEVEL IN KEY            
         MVC   LVLDES,ACHRDESA     SAVE LEVEL DESCRIPTION                       
         LA    R4,LVLNQ(R4)        R4 TO NEXT TABLE ENTRY                       
         LA    R3,16(R3)           R3 TO NEXT LEDGER ELEMENT DESCRIP.           
         CH    R2,=H'12'           IS THIS THE LAST LEVEL                       
         BL    INIT1               IF NOT GO DO NEXT                            
         STC   R1,NUMLEVS          SAVE NUMBER OF LEVELS                        
         EJECT                                                                  
*              HIERARCHY NAMES TO SCREEN                                        
         SPACE 1                                                                
         LA    R2,LOGADESH                                                      
         LA    R4,LVLTAB                                                        
         SR    R0,R0                                                            
         IC    R0,NUMLEVS          NUMBER OF LEVELS                             
         SH    R0,=H'1'            LESS ONE                                     
         BZ    INIT4               ONLY ONE LEVEL                               
         SPACE 1                                                                
INIT2    MVC   8(15,R2),LVLDES     DESCRIPTION TO SCREEN                        
         NI    1(R2),X'FF'-X'04'   CHANGE TO HIGH INTENSITY                     
         SR    R3,R3                                                            
         IC    R3,0(R2)            R2 ACCOUNT INPUT FIELD                       
         AR    R2,R3                                                            
         NI    1(R2),X'FF'-X'20'   UNPROTECT INPUT FIELD                        
         LA    R1,2                BUMP R2 TO NEXT DESCRIPTION FIELD            
         IC    R3,0(R2)                                                         
         AR    R2,R3                                                            
         BCT   R1,*-6                                                           
         LA    R4,LVLNQ(R4)        NEXT HIERARCHY ENTRY                         
         BCT   R0,INIT2                                                         
         SPACE 1                                                                
*              ADD THE * ACCOUNTS, IF NOT YET ON FILE                           
*                                                                               
INIT4    MVC   LOGDDES,LVLDES      LOW(ACCOUNT LEVEL DESCRIPTION)               
         SR    R0,R0                                                            
         IC    R0,NUMLEVS          NUMBER OF LEVELS                             
         LA    R4,LVLTAB                                                        
         SPACE 1                                                                
INIT5    SR    R1,R1                                                            
         IC    R1,LVLDSP                                                        
         LA    R5,KEY+3(R1)        R5 TO LEVEL CODE IN KEY                      
         IC    R1,LVLIPLN          LENGTH OF THIS LEVEL                         
         BCTR  R1,0                                                             
         EX    R1,*+8              * TO ACCOUNT                                 
         B     *+10                                                             
         MVC   0(0,R5),=12C'*'                                                  
         BAS   R9,RTLHI                                                         
         CLC   KEY,KEYSAVE                                                      
         BE    INIT7               ALREADY ON FILE                              
         LA    R6,IO2              NOT ON FILE BUILD NEW RECORD                 
         MVC   ACKEYACC(ACLENGTH-ACKEYD),SPACES                                 
         MVC   ACKEYACC(L'KEYSAVE),KEYSAVE    KEY                               
         MVC   ACLENGTH,DATADISP                                                
         XC    ACSTATUS(20),ACSTATUS                                            
         MVI   ELEMENT,X'20'       DUMMY NAME FOR *** ACCOUNT                   
         MVI   ELEMENT+1,14                                                     
         MVC   ELEMENT+2(12),ACKEYACC+3                                         
         GOTO1 ADDANEL                                                          
         GOTO1 STATIN              STATUS AND                                   
         CH    R0,=H'1'                                                         
         BNE   INIT6               FOR ACCOUNT LOW ADD BALANCE ELEMENT          
         GOTO1 BALIN                                                            
         SPACE 1                                                                
INIT6    GOTO1 ADDREC                                                           
         SPACE 1                                                                
INIT7    LA    R4,LVLNQ(R4)                                                     
         BCT   R0,INIT5            NEXT LEVEL                                   
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE OPTIONS/FILTERS                                                      
         SPACE 1                                                                
VALOPT   NTR1                                                                   
         XC    OPFLDS(OPFLDLQ),OPFLDS    CLEAR OPTIONS FIELD                    
         CLI   LOGFILTH+5,0                                                     
         BE    VALOPTX                                                          
         LA    R2,LOGFILTH                                                      
         LA    R0,SCANRHSL                                                      
         LA    R3,SCANBLK                                                       
         GOTO1 SCANNER,DMCB,((R0),(R2)),(10,(R3))                               
         MVC   FLAG1,4(R1)                                                      
         MVI   ERROR,INVALID                                                    
         CLI   FLAG1,0                                                          
         BE    XIT                                                              
         LA    R2,SCANBLK                                                       
         MVI   FNDX,1                                                           
         SPACE 1                                                                
VALOPT2  CLC   FNDX,FLAG1          ALL PROCESSED - EXIT                         
         BH    VALOPTX                                                          
         MVI   ERROR,NOINPUT        CHECK L'KEYWORD                             
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         SH    RE,=H'1'                                                         
         BM    XIT                                                              
         LA    RF,OPTNTAB                                                       
         MVI   ERROR,INVALID                                                    
         USING PARMD,RF                                                         
         SR    R0,R0                                                            
         SPACE 1                                                                
VALOPT4  CLI   PARMLEN,0           CHECK FOR VALID KEYWORD                      
         BE    XIT                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   PARMWORD(0),12(R2)                                               
         BE    VALOPT6                                                          
         SPACE 1                                                                
VALOPT5  IC    R0,PARMLEN                                                       
         AR    RF,R0                                                            
         B     VALOPT4                                                          
         SPACE 1                                                                
VALOPT6  LA    R1,PARMACTS         COMPATIBLE WITH ACTION                       
VALOPT7  CLI   0(R1),0                                                          
         BE    VALOPT5             END OF LIST                                  
         CLI   0(R1),255                                                        
         BE    VALOPT8             VALID FOR ALL ACTIONS                        
         CLC   THISACT,0(R1)                                                    
         BE    VALOPT8                                                          
         LA    R1,1(R1)                                                         
         B     VALOPT7                                                          
         SPACE 1                                                                
VALOPT8  TM    PARMINDS,DDSONLY    OTHER CHECKS                                 
         BZ    *+14                                                             
         LR    R8,RA                                                            
         USING TWAD,R8                                                          
         CLI   TWAOFFC,C'*'                                                     
         BNE   VALOPT5                                                          
         MVI   ERROR,TOOSHORT                                                   
         CLC   1(1,R2),PARMMIN                                                  
         BL    XIT                                                              
         MVI   ERROR,37                TOO LONG                                 
         CLC   1(1,R2),PARMMAX                                                  
         BH    XIT                                                              
         SPACE 1                                                                
VALOPT9  SR    R4,R4               TEST FOR DUPLICATE                           
         ICM   R4,3,PARMDEST                                                    
         AR    R4,RA               R4 = A(FNDX+OUTPUT VALUE OF PARAM)           
         CLI   0(R4),0                                                          
         MVI   ERROR,DUPED                                                      
         BNE   XIT                                                              
         MVC   0(1,R4),FNDX                                                     
         SPACE 1                                                                
VALOPT10 CLI   PARMSR,0            CALL VALIDATE/CONVERT SR IF ANY              
         BNE   VALOPT12                                                         
         MVI   ERROR,INVALID                                                    
         ICM   RF,15,PARMSR                                                     
         A     RF,PRELO                                                         
         GOTO1 (RF),DMCB,(R2),(R4)                                              
         CLI   ERROR,OK                                                         
         BNE   XIT                                                              
         B     VALOPT20                                                         
         SPACE 1                                                                
VALOPT12 CLI   PARMSR,C'Y'         OR MOVE IN C'Y' TO OUTPUT VALUE              
         BNE   VALOPT14                                                         
         MVI   0(R4),C'Y'                                                       
         B     VALOPT20                                                         
         SPACE 1                                                                
VALOPT14 ZIC   R1,PARMSR           OR MOVE INPUT TO OUTPUT                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VALOPT20                                                         
         MVC   0(0,R4),22(R2)                                                   
         SPACE 1                                                                
VALOPT20 ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R2,SCANTOTL(R2)                                                  
         B     VALOPT2                                                          
         SPACE 1                                                                
VALOPTX  MVI   FNDX,0                                                           
         MVI   ERROR,OK                                                         
XIT      XIT1                                                                   
         EJECT                                                                  
*              FILTER VALIDATION ROUTINES                                       
*    R2 = INPUT FIELD SCAN FIELD                                                
*    R3 = DESTINATION                                                           
         SPACE 1                                                                
*              START ACCOUNT                                                    
         SPACE 1                                                                
VALACC   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(15,R3),HIKEY      HIGH ACCOUNT KEY                             
         L     R4,ADLVLST          R4 TO ACCOUNT LEVEL ENTRY                    
         SR    R1,R1                                                            
         CLC   1(1,R2),LVLIPLN     CHECK LENGTH OF INPUT VS MAX.                
         BH    VALACCX                                                          
         IC    R1,LVLDSP           DISPLACEMENT TO LOW ACCOUNT                  
         LA    R5,3(R3,R1)                                                      
         IC    R1,1(R2)            LENGTH OF INPUT ACCOUNT                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),22(R2)      ACCOUNT CODE TO START KEY                    
         MVI   ERROR,OK                                                         
         B     XIT                                                              
VALACCX  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(36),=C'** ERROR ** START ACCOUNT IS INVALID'             
         B     XIT                                                              
         SPACE 1                                                                
*              JOB FILTER                                                       
         SPACE 1                                                                
VALJOB   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(15,R3),SPACES     JOB FILTER KEY                               
         MVC   0(1,R3),COMPANY                                                  
         MVC   1(2,R3),=C'SJ'                                                   
         IC    R1,1(R2)            LENGTH OF INPUT ACCOUNT                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R3),22(R2)      CLI/PROD/CODE TO KEY                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),0(R3)                                                    
         BAS   R9,RTLHI            READ ACCOUNT                                 
         CLC   KEY,KEYSAVE                                                      
         BNE   VALJOBX             ACCOUNT IS OK                                
         LA    R4,CNTRJOB          BUILD CONTRA ELEMENT                         
         USING TRSUBHD,R4                                                       
         MVI   TRSBEL,X'43'                                                     
         MVC   TRSBACNT,KEY        JOB CODE                                     
         MVI   ELCODE,X'20'                                                     
         LA    R3,IO               GET JOB NAME                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACNAMED,R3                                                       
         SR    R1,R1                                                            
         IC    R1,ACNMLEN          LENGTH OF 20 ELEMENT                         
         LA    R1,15(R1)           PLUS 15 FOR ACCOUNT                          
         STC   R1,TRSBLEN          GIVES LENGTH OF 43 ELEMENT                   
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRSBNAME(0),ACNMNAME NAME TO CONTRA                              
         MVI   ERROR,OK                                                         
         B     XIT                                                              
VALJOBX  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(31),=C'** ERROR ** JOB CODE IS INVALID'                  
         B     XIT                                                              
         EJECT                                                                  
*              FILTER VALIDATION                                                
         SPACE 1                                                                
VALFILT  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   ERROR,OK                                                         
         MVC   0(1,R3),22(R2)      FILTER VALUE TO SAVE AREA                    
         CLI   1(R2),1                                                          
         BE    XIT                                                              
         CLI   22(R2),C'*'         IF 2 BYTES MUST START WITH *                 
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     XIT                                                              
         MVC   0(1,R3),23(R2)                                                   
         NI    0(R3),X'FF'-X'40'   ALL EXECPT                                   
         MVI   ERROR,OK                                                         
         B     XIT                                                              
         SPACE 1                                                                
*              UNITS FILTER                                                     
         SPACE 1                                                                
VALUNT   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   ERROR,X'FE'                                                      
         MVC   0(1,R3),22(R2)     FIRST BYTE OF UNITS VALUE                     
         SR    RF,RF                                                            
         IC    RF,1(R2)           LENGTH OF UNITS INPUT                         
         LA    R4,22(R2)          R4 TO START OF UNITS VALUE                    
         CLI   0(R3),C'*'         IS IT A NEGATIVE FILTER                       
         BNE   *+12                                                             
         LA    R4,1(R4)            IGNORE * FOR CASHVAL                         
         SH    RF,=H'1'            ADJUST INPUT LENGTH                          
         BNP   VALUNTX                                                          
         GOTO1 CASHVAL,DMCB,(R4),(RF)                                           
         CLI   0(R1),X'FF'                                                      
         BE    VALUNTX                                                          
         L     RF,DMCB+4                                                        
         CVD   RF,DUB                                                           
         LA    RF,FLTUN                                                         
         CLI   FLTU,C'*'           IS FILTER NEGATIVE                           
         BNE   *+8                                                              
         LA    RF,FLTUNEG                                                       
         ZAP   0(6,RF),DUB                                                      
         MVI   ERROR,OK                                                         
         B     XIT                                                              
VALUNTX  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(31),=C'** ERROR ** INVALID UNITS FILTER'                 
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
ACTAB    DC    C'N',AL1(NEW)       ACTION EQUATE TABLE NEW                      
         DC    C'A',AL1(CHA)                           AMEND                    
         DC    C'I',AL1(DIS)                           INQUIRY                  
         DC    C'E',AL1(DIS)                           ENQUIRY                  
         DC    X'FF'                                                            
*                                                                               
*                                                                               
* TABLE OF INPUT OPTIONS.                                                       
*                                                                               
*              BYTE 0    =ENTRY LENGTH                                          
*              BYTE 1-8  =OPTION KEYWORD                                        
*              BYTE 9    =INDICATORS - BIT 0ON=DDS-ONLY OPTION                  
*              BYTE 10   =MIN LENGTH OF DATA VALUE                              
*              BYTE 11   =MAX LENGTH OF DATA VALUE                              
*              BYTE 12-13=DISPLACEMENT FROM START OF W/S OF ADDRESS OF          
*                        PROCESSED PARAMETER VALUE                              
*              BYTE 14-17=B0 = 0    - A(VALIDATE/CONVERT SR)                    
*                              C'Y' - MOVE C'Y' TO OUTPUT                       
*                              ELSE - MOVE IN TO OUT FOR L'B0                   
*              BYTE 18-  =STRING OF COMPATIBLE ACTION NUMBERS ENDED BY          
*                         ZERO                                                  
OPTNTAB  DS    0C                                                               
OPTSTR   DC    AL1(OPTSTRX-*),CL8'START',X'00010C',AL2(STRKEY-T603FFD)          
         DC    AL4(VALACC),AL1(OK)                                              
OPTSTRX  DS    0H                                                               
OPTJOB   DC    AL1(OPTJOBX-*),CL8'JOB  ',X'00070C',AL2(JOBKEY-T603FFD)          
         DC    AL4(VALJOB),AL1(OK)                                              
OPTJOBX  DS    0H                                                               
OPTF1    DC    AL1(OPTF1X-*),CL8'F1      ',X'000102',AL2(FLT1-T603FFD)          
         DC    AL4(VALFILT),AL1(OK)                                             
OPTF1X   DS    0H                                                               
OPTF2    DC    AL1(OPTF2X-*),CL8'F2      ',X'000102',AL2(FLT2-T603FFD)          
         DC    AL4(VALFILT),AL1(OK)                                             
OPTF2X   DS    0H                                                               
OPTF3    DC    AL1(OPTF3X-*),CL8'F3      ',X'000102',AL2(FLT3-T603FFD)          
         DC    AL4(VALFILT),AL1(OK)                                             
OPTF3X   DS    0H                                                               
OPTSC    DC    AL1(OPTSCX-*),CL8'SC      ',X'000102',AL2(FLSC-T603FFD)          
         DC    AL4(VALFILT),AL1(OK)                                             
OPTSCX   DS    0H                                                               
OPTUN    DC    AL1(OPTUNX-*),CL8'UNITS   ',X'000110',AL2(FLTU-T603FFD)          
         DC    AL4(VALUNT),AL1(DIS,CHA),AL1(0)                                  
OPTUNX   DS    0H                                                               
         DC    X'00'                                                            
         EJECT                                                                  
NORCL    DC    AL1(L'NORCM)                                                     
NORCM    DC    C'NO RECORDS TO DISPLAY'                                         
RCDSL    DC    AL1(L'RCDSM)                                                     
RCDSM    DC    C'RECORDS DISPLAYED'                                             
RCDSNL   DC    AL1(L'RCDSNM)                                                    
RCDSNM   DC    C'RECORDS DISPLAYED - ENTER FOR NEXT'                            
RCDSCL   DC    AL1(L'RCDSCM)                                                    
RCDSCM   DC    C'RECORDS DISPLAYED - ENTER CHANGES'                             
RCAML    DC    AL1(L'RCAMM)                                                     
RCAMM    DC    C'RECORDS AMENDED'                                               
RCAMNL   DC    AL1(L'RCAMNM)                                                    
RCAMNM   DC    C'RECORDS AMENDED - ENTER FOR NEXT'                              
         SPACE 1                                                                
NEW      EQU   X'80'                                                            
CHA      EQU   X'40'                                                            
DIS      EQU   X'20'                                                            
         SPACE 1                                                                
NOINPUT  EQU   1                                                                
TOOSHORT EQU   36                                                               
DUPED    EQU   35                                                               
OK       EQU   255                                                              
         SPACE 1                                                                
DDSONLY  EQU   X'80'                                                            
         SPACE 1                                                                
SCANRHSL EQU   20                  SCAN RIGHT SIDE                              
SCANTOTL EQU   (22+SCANRHSL)       LENGTH OF SCAN BLOCK                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMDDD                                                       
         EJECT                                                                  
*                                                                               
NEXTKEY  DS    CL15                                                             
LASTACT  DS    XL1                                                              
TYPE     DS    CL1                                                              
*                                                                               
NUMLEVS  DS    CL1                 NUMBER OF ACCOUNT LEVELS                     
LVLTAB   DS    CL(4*LVLNQ)         HEIRARCHY TABLE                              
UNITS    DS    10F                 SAVE UNITS                                   
*                                                                               
STRLEN   DS    CL1                 LENGTH OF KEY FOR THE * ACCOUNT              
*                                                                               
OPFLDS   DS    0C                                                               
STRKEY   DS    CL15                ACCOUNT START KEY                            
JOBKEY   DS    CL15                JOB FILTER                                   
FLT1     DS    CL1                 FILTER 1 VALUE                               
FLT2     DS    CL1                 FILTER 2 VALUE                               
FLT3     DS    CL1                 FILTER 3 VALUE                               
FLSC     DS    CL1                 FILTER 4 SUB-COMPANY                         
FLTU     DS    CL1                 UNITS FILTER                                 
FLTUN    DS    PL6                 UNITS VALUE                                  
FLTUNEG  DS    PL6                 ALL EXCEPT UNITS VALUE                       
OPFLDLQ  EQU   *-OPFLDS                                                         
*                                                                               
MYMODE   DS    CL1                                                              
CNTRJOB  DS    CL53                CONTRA ACCOUNT ELEMENT FOR JOB               
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*              DSECT FOR LVLTAB ENTRY                                           
LVLD     DSECT                                                                  
LVLDES   DS    CL15                LEVEL DESCRIPTION                            
LVLDSP   DS    CL1                 DISPLACEMENT TO LEVEL IN KEY                 
LVLIPLN  DS    CL1                 MAXIMUM LENGTH FOR INPUT                     
LVLKYLN  DS    CL1                 CUMULATIVE(KEY) LENGTH                       
LVLNQ    EQU   *-LVLD                                                           
*                                                                               
*                                                                               
*              DSECT FOR AN ACCOUNT/UNIT LINE                                   
LIND     DSECT                                                                  
LINACCH  DS    CL8                 LOW ACCOUNT(DISTRIBUTOR)                     
LINACC   DS    CL12                                                             
LINNMH   DS    CL8                 NAME                                         
LINNM    DS    CL36                                                             
LINUNTH  DS    CL8                 UNITS                                        
LINUNT   DS    CL12                                                             
LINPCTH  DS    CL8                 PERCENT                                      
LINPCT   DS    CL7                                                              
LINLNQ   EQU   *-LIND                                                           
         SPACE 1                                                                
*              DSECT TO COVER ACTION PARAMETER (OPTION) TABLE ENTRY             
*                                                                               
PARMD    DSECT                                                                  
PARMLEN  DS    CL1       B         LENGTH OF TABLE ENTRY                        
PARMWORD DS    CL8       C         PARAMETER KEYWORD                            
PARMINDS DS    XL1       X         INDICATORS                                   
PARMMIN  DS    XL1       B         MIN LENGTH OF RHS                            
PARMMAX  DS    XL1       B         MAX LENGTH OF RHS                            
PARMDEST DS    CL2       B         DISPLACEMENT FROM START OF W/S OF            
*                                  ADDRESS OF PROCESSED PARAMETER VALUE         
PARMSR   DS    CL4       B         B0 = 0    - A(VALIDATE/CONVERT SR)           
*                                  B0 = C'Y' - MOVE C'Y' TO OUTPUT              
*                                  ELSE      - MOVE IN TO OUT FOR L'B0          
PARMACTS DS    0C        B         STRING OF COMPATIBLE ACTNNUMS ENDED          
*                                  BY ZERO                                      
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
         EJECT                                                                  
*                                                                               
         DS    5F                  USED BY BASE                                 
NEWAMT   DS    D                                                                
SCANNER  DS    A                                                                
PRELO    DS    F                                                                
ADLVLST  DS    A                   A(ACCOUNT LEVEL ENTRY IN LVLTAB)             
FADR     DS    A                                                                
MADR     DS    A                                                                
SCANBLK  DS    10CL(SCANTOTL)                                                   
ADJCODE  DS    CL1                                                              
ADJAMT   DS    PL6                                                              
TOTUNT   DS    PL6                                                              
PL16     DS    PL16                                                             
EDWRK    DS    CL12                                                             
ELCODE   DS    CL1                                                              
THISACT  DS    XL1                                                              
CHANGE   DS    CL1                                                              
FLAG1    DS    CL1                                                              
HIKEY    DS    CL32                                                             
SVKY     DS    CL15                                                             
SVNAME   DS    CL36                                                             
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE ACLFMEQU                                                       
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FATWA                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048ACLFM26   05/01/02'                                      
         END                                                                    
