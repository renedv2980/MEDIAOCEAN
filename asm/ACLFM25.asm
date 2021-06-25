*          DATA SET ACLFM25    AT LEVEL 003 AS OF 11/21/02                      
*PHASE T60325A                                                                  
         TITLE 'HISTORY ADJUSTMENTS'                                            
T60325   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (LWSX-LWSD),*LFM25**,R8,RR=R2,CLEAR=YES                          
         LR    R9,RC                                                            
         USING LWSD,R9                                                          
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
*                                                                               
         ST    R2,RELO                                                          
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         MVC   ADDAY,CADDAY                                                     
         MVC   CUREDIT,CCUREDIT                                                 
         MVC   HELLO,CHELLO                                                     
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   SCANNER,CSCANNER                                                 
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY) GET TODAYS DATE                      
         GOTO1 DATCON,DMCB,(5,0),(19,FULL)                                      
         MVI   FULL+3,X'0F'                                                     
         UNPK  DUB,FULL                                                         
         MVC   TPASS,DUB+1                                                      
         MVI   DMBITS,0                                                         
         DROP  RF                                                               
*                                                                               
         CLI   MODE,BUILDKEY                                                    
         BE    BLDKEY                                                           
         CLI   MODE,DSPLYREC                                                    
         BE    DSPREC                                                           
         CLI   MODE,BUILDREC                                                    
         BE    BLDREC                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD KEY ROUTINE                                                   *         
***********************************************************************         
BLDKEY   LA    R2,HSTACCTH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   KEY,SPACES                  MOVE COMPANY CODES TO KEY            
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),WORK                                                   
         TM    4(R2),X'20'                 HAS DATA PREVIOUSLY BEEN             
         BO    BLDK7                       VALIDATED - IF SO BRANCH             
*                                                                               
         MVC   HSTACNM,SPACES              ACCOUNT NAME                         
         OI    HSTACNMH+6,X'80'                                                 
         MVC   HSTOFNM,SPACES              OFFICE NAME                          
         OI    HSTOFNMH+6,X'80'                                                 
         MVC   HSTCTNM,SPACES              CONTRA ACCOUNT NAME                  
         OI    HSTCTNMH+6,X'80'                                                 
         MVC   HSTINFO,SPACES              INFO BLOCK                           
         OI    HSTINFOH+6,X'80'                                                 
*                                                                               
         NI    HSTOFFCH+4,X'DF'                                                 
         NI    HSTCONTH+4,X'DF'                                                 
         NI    HSTPERDH+4,X'DF'                                                 
*                                                                               
         MVI   ANYKEY,C'Y'                 SET ANY KEY CHANGES TO YES           
         MVI   ERROR,NOTVLREC              SET ERROR MSG                        
*                                                                               
         GOTO1 READ                        READ ACCOUNT NAME                    
         MVC   ACCNT,KEY                   SAVE ACCOUNT CODE                    
         LA    R5,IO                                                            
         USING ACTRECD,R5                                                       
         LA    R4,ACTRECD+ACCORFST                                              
         SR    R0,R0                                                            
BLDK3    CLI   0(R4),0                     MUST HAVE BALANCE ELEMENT            
         BE    CXIT                                                             
         CLI   0(R4),ABLELQ                                                     
         BE    BLDK5                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     BLDK3                                                            
*                                                                               
BLDK5    GOTO1 NAMOUT                      TRANSMIT ACCOUNT NAME                
         OI    4(R2),X'20'                 SET ACCOUNT VALID                    
                                                                                
BLDK7    LA    R2,HSTOFFCH                                                      
         OC    HSTOFFC,SPACES                                                   
         TM    4(R2),X'20'                                                      
         BO    BLDK11                                                           
         MVC   HSTOFNM,SPACES              CLEAR OFFICE NAME                    
         OI    HSTOFNMH+6,X'80'                                                 
         MVC   HSTCTNM,SPACES              CONTRA ACCOUNT NAME                  
         OI    HSTCTNMH+6,X'80'                                                 
*                                                                               
         NI    HSTCONTH+4,X'DF'                                                 
         NI    HSTPERDH+4,X'DF'                                                 
*                                                                               
         MVI   ANYKEY,C'Y'                 SET ANY KEY CHANGES TO YES           
         CLI   5(R2),0                                                          
         BE    BLDK9                       ANY DATA ENTERED?                    
         TM    COMPSTA4,CPYSOFF2           MUST BE 2 CHARACTER                  
         BNO   INVALIDX                                                         
         LA    R5,DKEY                                                          
         USING OFFRECD,R5                                                       
         MVC   OFFKEY,SPACES               GET OFFICE RECORD                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
         MVC   OFFKOFF,HSTOFFC                                                  
         BAS   RE,DMHGH                                                         
         CLC   DKEY,DIR                                                         
         BNE   INVALIDX                                                         
         LA    R5,IO                                                            
         ST    R5,AIO                                                           
         BAS   RE,DMGETR                   GET OFFICE RECORD                    
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELN                                                        
         BE    *+6                                                              
         DC    H'0'                        OFFICE MISSING NAME ELEMENT          
         USING NAMELD,R5                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HSTOFNM(0),NAMEREC          OFFICE NAME                          
BLDK9    OI    4(R2),X'20'                 SET OFFICE VALID                     
*                                                                               
BLDK11   LA    R2,HSTCONTH                 VALIDATE CONTRA ACCOUNT              
         GOTO1 ANY                         ANY DATA ENTERED?                    
         CLI   HSTCONT,C' '                                                     
         BNH   INVALIDX                                                         
         GOTO1 MOVE                                                             
         TM    4(R2),X'20'                 HAS CONTRA BEEN PREVIOUSLY           
         BO    BLDK15                      VALIDATED - IF SO BRANCH             
         MVC   HSTCTNM,SPACES              CLEAR CONTRA NAME                    
         OI    HSTCTNMH+6,X'80'                                                 
*                                                                               
         MVI   ANYKEY,C'Y'                 SET ANY KEY CHANGES TO YES           
         NI    HSTPERDH+4,X'DF'            TURN OFF VALIDITY BIT IN             
*                                          PERIOD                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY              MOVE COMPANY CODES TO KEY            
         MVC   KEY+1(14),WORK                                                   
*                                                                               
         MVC   CNTRA,KEY                   SAVE CONTRA                          
         CLC   CNTRA+1(2),=C'**'                                                
         BNE   *+14                                                             
         MVC   CNTRA(3),SPACES             FUNNY CONTRA                         
         B     BLDK13                                                           
*                                                                               
         GOTO1 HIGH                        READ CONTRA NAME AND                 
         MVC   HSTCTNM(L'MSG1),MSG1        RECORD NOT FOUND                     
         CLC   KEY,KEYSAVE                 RECORD ON FILE ?                     
         BNE   BLDK13                                                           
         GOTO1 NAMOUT                      TRANSMIT CONTRA NAME                 
*                                                                               
BLDK13   OI    4(R2),X'20'                 TURN ON VALIDITY BIT                 
*                                                                               
BLDK15   LA    R2,HSTTYPEH                 VALIDATE TYPE                        
         OI    HSTTYPE,C' '                                                     
         TM    4(R2),X'20'                 HAS TYPE BEEN VALIDATED              
         BO    BLDK17                      IF YES BRANCH                        
         MVI   ANYKEY,C'Y'                 IF NOT ANY KEY CHANGES=YES           
         OI    4(R2),X'20'                 TURN ON VALIDITY                     
         NI    HSTPERDH+4,X'DF'            TURN OFF VALIDITY IN PERIOD          
*                                                                               
BLDK17   LA    R2,HSTPERDH                 VALIDATE PERIOD                      
         TM    4(R2),X'20'                 HAS PERIOD BEEN PREVIOUSLY           
         BO    BLDK19                      VALIDATED - IF SO BRANCH             
         MVI   ANYKEY,C'Y'                 SET ANY KEY CHANGES TO YES           
*                                                                               
         XC    START,START                                                      
         MVI   END,X'FF'                                                        
         CLI   5(R2),0                                                          
         BE    BLDK19                                                           
         GOTO1 VALPERD,DMCB,HSTPERDH,START VALIDATE PERIOD                      
         BNE   CXIT                        EXIT IF ERROR                        
*                                                                               
BLDK19   OI    4(R2),X'20'                 TURN ON VALIDITY IN PERIOD           
         LA    R2,HSTPASSH                 CHECK PASS WORD                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLC   TPASS,WORK                                                       
         BNE   INVALIDX                                                         
*                                                                               
         MVI   ERROR,X'FF'                 SET NO ERRORS                        
         LA    R2,HSTACCTH                 SET CURSOR                           
         B     CXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
DSPREC   BAS   RE,CLRSCN                   CLEAR SCREEN                         
         BAS   RE,GETREC                   GET THE RECORD                       
         CLI   ERROR,X'FF'                 RECORD ERROR ?                       
         BNE   XIT                                                              
         BAS   RE,DSPLY                    DISPLAY ELEMENT DETAIL               
         LA    R2,HSTACCTH                 SET CURSOR                           
         CLC   LOGACT+1(2),=C'NQ'          IF ACTION IS INQUIRY EXIT            
         BE    XIT                                                              
         LA    R5,HSTLIN1H                                                      
         USING LINED,R5                    ELSE                                 
         LA    R2,LINDATH                  SET CURSOR                           
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD RECORD                                                        *         
***********************************************************************         
BLDREC   MVI   DMBITS,DMRDUP               READ FOR UPDATE                      
         BAS   RE,GETREC                   GET THE RECORD                       
         CLI   ERROR,X'FF'                 RECORD ERROR ?                       
         BNE   XIT                                                              
         MVI   DMBITS,0                                                         
         MVI   FLGS,0                                                           
         CLI   IO,0                        TEST RECORD FOUND                    
         BNE   *+14                                                             
         MVC   IO(L'DKEY),DKEY             SET NEW RECORD KEY                   
         OI    FLGS,NEWREC                                                      
*                                                                               
         LA    R5,HSTLIN1H                                                      
         USING LINED,R5                                                         
         LHI   R0,NMNTHS+1                                                      
BLDR1    CLI   LINDATH+5,0                 ANY INPUT ON THIS LINE ?             
         BNE   *+8                                                              
         CLI   LINDR+5,0                                                        
         BNE   *+8                                                              
         CLI   LINCR+5,0                                                        
         BNE   *+8                                                              
         B     BLDR13                                                           
*                                                                               
         TM    LINDATH+4,X'20'             DATE CHANGE ?                        
         BNO   BLDR3                                                            
         TM    LINDRH+4,X'20'              DEBIT CHANGE ?                       
         BNO   BLDR3                                                            
         TM    LINCRH+4,X'20'              CREDIT CHANGED ?                     
         BO    BLDR13                      NO CHANGES                           
*                                                                               
BLDR3    OI    FLGS,ACTIVE                 SET ACTIVITY FLAG                    
         XC    MOAS,MOAS                                                        
         MVI   MOAE,X'FF'                                                       
         LA    R2,LINDATH                                                       
         GOTO1 VALPERD,DMCB,(R2),MOAS      VALIDATE PERIOD                      
         BNE   CXIT                        EXIT IF ERROR                        
         ZAP   DR,=P'0'                                                         
         ZAP   CR,=P'0'                                                         
         CLC   LINDR(6),DELETE                                                  
         BE    BLDR5                                                            
         LA    R2,LINDRH                                                        
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'80',LINDR),(RF)  VALIDATE DEBIT                  
         CLI   DMCB,0                                                           
         BNE   CASHX                                                            
         ZAP   DR,DMCB+4(8)                                                     
         LA    R2,LINCRH                                                        
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'80',LINCR),(RF)  VALIDATE CREDIT                 
         CLI   DMCB,0                                                           
         BNE   CASHX                                                            
         ZAP   CR,DMCB+4(8)                                                     
*                                                                               
BLDR5    LA    R2,LINDATH                                                       
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         CLI   MOAE,X'FF'                  IS IT A RANGE ?                      
         BE    BLDR7                       NO,                                  
         USING PBKELD,R6                                                        
         MVI   PBKEL,PBKELQ                BUILD RANGE ELEMENT                  
         MVI   PBKLN,PBKLNQ                                                     
         MVC   PBKLOW,MOAS                                                      
         MVC   PBKHI,MOAE                                                       
         ZAP   PBKDR,DR                                                         
         ZAP   PBKCR,CR                                                         
         LA    RF,L'PBKLOW+L'PBKHI                                              
         GOTO1 HELLO,DMCB,(C'G',ACCMST),('PBKELQ',IO),((RF),PBKLOW)             
         CLC   LINDR(6),DELETE                                                  
         BE    BLDR11                                                           
         CLI   DMCB+12,0                   TEST ALREADY IN RECORD               
         BNE   BLDR9                                                            
         L     R1,DMCB+12                                                       
         ZAP   PBKDR-PBKELD(L'PBKDR,R1),PBKDR REPLACE CASH                      
         ZAP   PBKCR-PBKELD(L'PBKCR,R1),PBKCR                                   
         B     BLDR13                                                           
*                                                                               
         USING BUKELD,R6                                                        
BLDR7    MVI   BUKEL,BUKELQ                BUILD BUCKET ELEMENT                 
         MVI   BUKLN,BUKLNQ                                                     
         MVC   BUKMOS,MOAS                                                      
         ZAP   BUKDR,DR                                                         
         ZAP   BUKCR,CR                                                         
         LA    RF,L'BUKMOS                                                      
         GOTO1 HELLO,DMCB,(C'G',ACCMST),('BUKELQ',IO),((RF),BUKMOS)             
         CLC   LINDR(6),DELETE                                                  
         BE    BLDR11                                                           
         CLI   DMCB+12,0                   TEST ALREADY IN RECORD               
         BNE   BLDR9                                                            
         L     R1,DMCB+12                                                       
         ZAP   BUKDR-BUKELD(L'BUKDR,R1),BUKDR REPLACE CASH                      
         ZAP   BUKCR-BUKELD(L'BUKCR,R1),BUKCR                                   
         B     BLDR13                                                           
*                                                                               
BLDR9    GOTO1 HELLO,DMCB,(C'P',ACCMST),IO,ELEMENT,0                            
         CLI   DMCB+12,0                                                        
         BNE   XMSG5                       CAN'T ADD ELEMENT                    
         B     BLDR13                                                           
*                                                                               
BLDR11   CLI   DMCB+12,0                   FOUND ELEMENT                        
         BNE   XMSG2                       'ELEMENT NOT FOUND'                  
         L     R1,DMCB+12                                                       
         MVI   0(R1),X'FF'                                                      
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',IO),0                            
*                                                                               
BLDR13   LA    R5,LINLNQ(R5)                                                    
         BCT   R0,BLDR1                                                         
*                                                                               
         LA    R2,HSTACCTH                 SET CURSOR                           
         MVI   ERROR,X'FF'                                                      
         TM    FLGS,ACTIVE                 ANY ACTIVITY ?                       
         BO    BLDR15                      YES, UPDATE RECORD                   
         LA    R2,HSTLIN1H                                                      
         TM    FLGS,NEWREC                                                      
         BNO   XMSG4                       NOTHING CHANGED                      
         MVI   ERROR,MISSING                                                    
         B     CXIT                                                             
*                                                                               
BLDR15   LA    R1,IO                                                            
         ST    R1,AIO                                                           
         L     R5,AIO                                                           
         USING CACRECD,R5                                                       
         CLI   CACRFST,0                   ANY BUCKETS ?                        
         BE    *+12                        NO,                                  
         CLI   CACRFST,BUKELQ              FIRST MUST BE A BUCKET               
         BNE   XMSG7                                                            
*                                                                               
         TM    FLGS,NEWREC                 TEST NEW RECORD                      
         BO    BLDR23                                                           
         BAS   RE,DMPUTR                   PUT OLD RECORD                       
         L     R5,AIO                                                           
         USING CACRECD,R5                                                       
         CLI   CACRFST,0                   ANY BUCKETS ?                        
         BNE   BLDR25                                                           
         MVC   DKEY,CACKEY                                                      
         BAS   RE,DMRD                     READ                                 
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,DIR                                                           
         OI    CACKSTAT,CACSDELT           DELETE BUCKET RECORD                 
         BAS   RE,DMWRTR                                                        
*                                                                               
         LA    R5,DKEY                                                          
         USING CHDRECD,R5                                                       
         MVC   CHDKSPCS,SPACES                                                  
         MVC   CHDKBTYP,SPACES                                                  
         XC    CHDKNULL,CHDKNULL                                                
         BAS   RE,DMRD                     READ HEADER                          
         CLC   DKEY,DIR                                                         
         BNE   BLDR21                      IF NONE, GET OUT                     
*                                                                               
BLDR17   BAS   RE,DMSEQ                    GET NEXT                             
         CLC   DIR(CACKSPAC-CACKEY),DKEY   SAME ACC/OFF/CONTRA                  
         BNE   BLDR19                      NO OTHER RECORDS                     
         TM    DIR+(CACKSTAT-CACKEY),CACSDELT                                   
         BO    BLDR17                      SKIP DELETED                         
         B     BLDR21                      KEEP THE HEADER                      
*                                                                               
BLDR19   BAS   RE,DMRD                     READ FOR HEADER                      
         CLC   DKEY,DIR                                                         
         BNE   BLDR21                      NO HEADER                            
         LA    R5,DIR                                                           
         OI    CHDKSTAT,CHDSDELT           DELETE THE HEADER                    
         BAS   RE,DMWRTR                                                        
*                                                                               
BLDR21   BAS   RE,CLRSCN                   CLEAR SCREEN                         
         B     XMSG6                       RECORD HAS BEEN DELETED              
*                                                                               
BLDR23   BAS   RE,DMADDR                   ADD NEW RECORD                       
         BAS   RE,HEADR                    TEST/ADD HEADER RECORD               
*                                                                               
BLDR25   MVI   ERROR,X'FF'                 CLEAR ERROR                          
         BAS   RE,CLRSCN                   CLEAR SCREEN                         
         LA    R5,DKEY                     GET THE RECORD                       
         USING CACRECD,R5                                                       
         MVC   CACKEY,SPACES                                                    
         MVC   CACKCULA,ACCNT                                                   
         MVC   CACKOFF,HSTOFFC                                                  
         MVC   CACKCULC,CNTRA                                                   
         MVC   CACKBTYP,HSTTYPE                                                 
         BAS   RE,DMRD                     READ                                 
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,IO                                                            
         ST    R5,AIO                                                           
         BAS   RE,DMGETR                   GET THE RECORD                       
         BAS   RE,DSPLY                    DISPLAY SCREEN                       
         LA    R2,HSTACCTH                 SET CURSOR                           
         B     CXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY RECORD DETAIL                                    *         
***********************************************************************         
DSPLY    NTR1  ,                                                                
         ZAP   TOTDR,=P'0'                 CLEAR TOTALS                         
         ZAP   TOTCR,=P'0'                                                      
*                                                                               
         LA    R5,IO                       GET RECORD TOTAL                     
         USING CACRECD,R5                                                       
         CLI   CACRFST,0                                                        
         BE    XIT                         NO DATA                              
         LA    R5,CACRFST                                                       
         SR    R0,R0                                                            
*                                                                               
DSPLY1   CLI   0(R5),PBKELQ                                                     
         BNE   DSPLY3                                                           
         USING PBKELD,R5                                                        
         AP    TOTDR,PBKDR                                                      
         AP    TOTCR,PBKCR                                                      
         B     DSPLY5                                                           
*                                                                               
DSPLY3   CLI   0(R5),BUKELQ                                                     
         BNE   DSPLY5                                                           
         USING BUKELD,R5                                                        
         AP    TOTDR,BUKDR                                                      
         AP    TOTCR,BUKCR                                                      
*                                                                               
DSPLY5   IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BNE   DSPLY1                                                           
         DROP  R5                                                               
*                                                                               
         CURED TOTDR,(L'HSTRCDR,HSTRCDR),2,COMMAS=YES,MINUS=YES                 
         CURED TOTCR,(L'HSTRCCR,HSTRCCR),2,COMMAS=YES,MINUS=YES                 
         ZAP   DUB,TOTDR                                                        
         SP    DUB,TOTCR                                                        
         CURED (P8,DUB),(L'HSTRCBA,HSTRCBA),2,COMMAS=YES,MINUS=YES              
*                                                                               
         LA    R6,NMNTHS                                                        
         LA    R3,HSTLIN2H                                                      
         USING LINED,R3                                                         
         LA    R5,IO                                                            
         USING CACRECD,R5                                                       
         MVI   ELCODE,PBKELQ               FIRST DO PRIOR BUCKETS               
         BAS   RE,GETELN                                                        
         BNE   DSPLY15                                                          
*                                                                               
         USING PBKELD,R5                                                        
DSPLY9   CLC   PBKLOW,END                  AFTER END ?                          
         BH    DSPLY11                     YES, SKIP IT                         
         CLC   PBKHI,START                 BEFORE START ?                       
         BL    DSPLY11                     YES,SKIP IT                          
         MVC   WORK(2),PBKLOW                                                   
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,LINDAT)                                  
         MVI   LINDAT+6,C'-'                                                    
         MVC   WORK(2),PBKHI                                                    
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,LINDAT+7)                                
         OI    LINDATH+4,X'20'                                                  
         CURED PBKDR,(L'LINDR,LINDR),2,COMMAS=YES,MINUS=YES                     
         OI    LINDRH+4,X'20'                                                   
         CURED PBKCR,(L'LINCR,LINCR),2,COMMAS=YES,MINUS=YES                     
         OI    LINCRH+4,X'20'                                                   
         LA    R3,LINLNQ(R3)                                                    
         BCT   R6,*+8                                                           
         B     DSPLY21                                                          
*                                                                               
DSPLY11  BAS   RE,NEXTEL                                                        
         BE    DSPLY9                                                           
*                                                                               
DSPLY15  MVI   ELCODE,BUKELQ                                                    
         LA    R5,IO                                                            
         BAS   RE,GETELN                                                        
         BNE   DSPLY21                                                          
*                                                                               
         USING BUKELD,R5                                                        
DSPLY17  CLC   BUKMOS,END                  TEST PASSED END                      
         BH    DSPLY19                                                          
         CLC   BUKMOS,START                TEST BEFORE START                    
         BL    DSPLY19                                                          
         MVC   WORK(2),BUKMOS                                                   
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,LINDAT)                                  
         OI    LINDATH+4,X'20'                                                  
         CURED BUKDR,(L'LINDR,LINDR),2,COMMAS=YES,MINUS=YES                     
         OI    LINDRH+4,X'20'                                                   
         CURED BUKCR,(L'LINCR,LINCR),2,COMMAS=YES,MINUS=YES                     
         OI    LINCRH+4,X'20'                                                   
         LA    R3,LINLNQ(R3)                                                    
         BCT   R6,*+8                                                           
         B     DSPLY21                                                          
*                                                                               
DSPLY19  BAS   RE,NEXTEL                                                        
         BE    DSPLY17                                                          
*                                                                               
DSPLY21  MVC   HSTINFO(3),=C'DA='                                               
         GOTO1 HEXOUT,DMCB,DA,HSTINFO+3,4,0,0                                   
         MVC   HSTINFO+12(4),=C'LEN='                                           
         SR    R0,R0                                                            
         ICM   R0,3,IO+(ACCRLEN-ACCRECD)                                        
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  HSTINFO+16(4),DUB                                                
         OI    HSTINFOH+6,X'80'                                                 
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* GET RECORD                                                         *          
**********************************************************************          
GETREC   NTR1  ,                                                                
         LA    R2,HSTACCTH                                                      
         MVI   ERROR,X'FF'                                                      
         XC    IO(100),IO                                                       
         LA    R5,DKEY                                                          
         USING CACRECD,R5                                                       
         MVC   CACKEY,SPACES                                                    
         MVC   CACKCULA,ACCNT                                                   
         MVC   CACKOFF,HSTOFFC                                                  
         MVC   CACKCULC,CNTRA                                                   
         MVC   CACKBTYP,HSTTYPE                                                 
         BAS   RE,DMRD                     READ                                 
         CLC   LOGACT(2),=C'NE'            TEST ACTION NEW                      
         BNE   GETREC3                                                          
         CLC   DKEY,DIR                                                         
         BNE   XIT                                                              
         B     XMSG3                       RECORD ON FILE                       
*                                                                               
GETREC3  CLC   DKEY,DIR                                                         
         BNE   XMSG1                       RECORD NOT FOUND                     
         LA    R5,IO                                                            
         ST    R5,AIO                                                           
         BAS   RE,DMGETR                   GET THE RECORD                       
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* CHECK FOR HEADER RECORD                                            *          
**********************************************************************          
HEADR    NTR1  ,                                                                
         LA    R2,IO                                                            
         MVC   DKEY,0(R2)                                                       
         LA    R2,DKEY              READ FOR HEADER                             
         USING CHDRECD,R2                                                       
         MVC   CHDKSPCS,SPACES                                                  
         MVC   CHDKBTYP,SPACES                                                  
         XC    CHDKNULL,CHDKNULL                                                
         BAS   RE,DMHGH                                                         
         CLC   DKEY,DIR             HEADER FOUND ?                              
         BE    XIT                  YES,                                        
         LA    R2,IO3                                                           
         MVC   CHDKEY,DKEY          NO, BUILD ONE                               
         XC    CHDKSTA(30),CHDKSTA                                              
*                                                                               
         LA    R3,ELEMENT           BUILD CONTRA ELEMENT                        
         USING CACEL,R3                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   CACEL,CACELQ                                                     
         MVI   CACLN,CACLN1Q                                                    
         MVC   CACCNT,CHDKCULC                                                  
*                                                                               
         MVC   DKEY,SPACES          READ FOR CONTRA                             
         MVC   DKEY(L'CHDKCULC),CHDKCULC                                        
         LA    RF,IO2                                                           
         ST    RF,AIO                                                           
         BAS   RE,DMHGH                                                         
         CLC   DKEY,DIR             CONTRA FOUND ?                              
         BNE   HEADR7               NO, JUST ADD ACCOUNT CODE                   
         BAS   RE,DMGETR            GET CONTRA ACCOUNT                          
         L     R4,AIO                                                           
         LA    R4,ACTRFST-ACTRECD(R4)                                           
         SR    R1,R1                                                            
*                                                                               
         USING NAMELD,R4                                                        
HEADR3   CLI   0(R4),0                                                          
         BE    HEADR7                                                           
         CLI   0(R4),NAMELQ         GET ACCOUNT NAME                            
         BE    HEADR5                                                           
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     HEADR3                                                           
*                                                                               
HEADR5   IC    R1,NAMLN             ADD NAME TO CACEL                           
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CACNAME(0),NAMEREC                                               
*                                                                               
         LA    R1,CACLN1Q+1(R1)     SET LENGTH OF ELEMENT                       
         STC   R1,CACLN                                                         
*                                                                               
HEADR7   LA    RF,IO3               RESTORE AIO                                 
         ST    RF,AIO                                                           
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO,CACEL,0                             
         BAS   RE,DMADDR            ADD CONTRA HEADER                           
         LA    RF,IO                                                            
         ST    RF,AIO                                                           
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD LINE                                                *         
*  PARAM 1 = A(FIELD HEADER)                                          *         
*  PARAM 2 = A(START,END DATE FIELDS)                                 *         
***********************************************************************         
VALPERD  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         LR    R0,R3                                                            
         MVI   ERROR,DATERR                                                     
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=-='                              
*                                                                               
         SR    R6,R6                       GET DATE RANGES                      
         ICM   R6,1,DMCB+4                 R6=NUMBER OF DATES                   
         BZ    XITNO                                                            
         LA    R5,BLOCK                                                         
*                                                                               
VALPERD3 CLI   1(R5),0                     CAN'T HAVE COMMA                     
         BNE   XITNO                                                            
         CLI   0(R5),6                                                          
         BH    XITNO                                                            
         GOTO1 DATVAL,DMCB,(0,12(R5)),WORK                                      
         OC    0(4,R1),0(R1)               TEST YY/MM/DD                        
         BNZ   XITNO                       YES, INVALID DATE                    
         GOTO1 DATVAL,DMCB,(2,12(R5)),WORK                                      
         OC    0(4,R1),0(R1)               TEST MMM/YY                          
         BZ    XITNO                       NO, INVALID DATE                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,FULL)                                    
         MVC   0(2,R3),FULL                                                     
         CLC   0(2,R3),TODAY               TEST DATE HIGHER THAN TODAY          
         BH    XITNO                                                            
*                                                                               
         LA    R3,2(R3)                    NEXT INPUT DATE                      
         LA    R5,32(R5)                   NEXT LINE IN BLOCK                   
         BCT   R6,VALPERD3                                                      
*                                                                               
         LR    R3,R0                                                            
         CLC   2(2,R3),0(R3)               TEST END BEFORE START                
         BL    XITNO                       YES, ERROR                           
         B     XITYES                                                           
         EJECT                                                                  
**********************************************************************          
* MISCELLANEOUS                                                      *          
**********************************************************************          
         GETEL R5,DATADISP,ELCODE                                               
GETELN   AH    R5,NEWDISP                                                       
         J     FIRSTEL                                                          
*                                                                               
*        ROUTINE TO CLEAR SCREEN                                                
*                                                                               
CLRSCN   NTR1  ,                                                                
         LA    R0,NMNTHS+1                                                      
         LA    R5,HSTLIN1H                  CLEAR TO SPACES:                    
         USING LINED,R5                                                         
CLRSCN1  MVC   LINDAT,SPACES                DATE                                
         OI    LINDATH+6,X'80'                                                  
         OI    LINDATH+4,X'20'                                                  
         MVC   LINDR,SPACES                 DEBITS                              
         OI    LINDRH+6,X'80'                                                   
         OI    LINDRH+4,X'20'                                                   
         MVC   LINCR,SPACES                 CREDITS                             
         OI    LINCRH+6,X'80'                                                   
         OI    LINCRH+4,X'20'                                                   
         LA    R5,LINLNQ(R5)                                                    
         BCT   R0,CLRSCN1                                                       
*                                                                               
         MVC   HSTRCDR,SPACES                                                   
         OI    HSTRCDRH+6,X'80'                                                 
         MVC   HSTRCCR,SPACES                                                   
         OI    HSTRCCRH+6,X'80'                                                 
         MVC   HSTRCBA,SPACES                                                   
         OI    HSTRCBAH+6,X'80'                                                 
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
XMSG1    MVC   LOGHEAD(L'MSG1),MSG1        RECORD NOT FOUND                     
         LA    R2,HSTACCTH                                                      
         B     XMSGX                                                            
XMSG2    MVC   LOGHEAD(L'MSG2),MSG2        ELEMENT NOT FOUND                    
         B     XMSGX                                                            
XMSG3    MVC   LOGHEAD(L'MSG3),MSG3        RECORD ALREADY ON FILE               
         B     XMSGX                                                            
XMSG4    MVC   LOGHEAD(L'MSG4),MSG4        YOU HAVEN'T CHANGED ANYTHING         
         B     XMSGX                                                            
XMSG5    MVC   LOGHEAD(L'MSG5),MSG5        CAN'T ADD ELEMENT                    
         B     XMSGX                                                            
XMSG6    MVC   LOGHEAD(L'MSG6),MSG6        RECORD DELETED                       
         B     XMSGX                                                            
XMSG7    MVC   LOGHEAD(L'MSG7),MSG7        MUST HAVE A BUCKET ELEMENT           
         LA    R2,HSTACCTH                                                      
         B     XMSGX                                                            
XMSGX    MVI   ERROR,X'FE'                                                      
         B     CXIT                                                             
*                                                                               
INVALIDX MVI   ERROR,INVALID                                                    
         B     CXIT                                                             
CASHX    MVI   ERROR,CASHERR                                                    
*                                                                               
CXIT     OI    6(R2),X'40'         INSERT CURSOR                                
         OI    6(R2),X'80'                                                      
*                                                                               
XIT      XIT1  REGS=(R2)                                                        
XITNO    LTR   RB,RB                                                            
         B     *+6                                                              
XITYES   CR    RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
                                                                                
DMRD     ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
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
         GOTO1 DATAMGR,DMCB,(DMBITS,DMGET),ACCMST,DA,AIO,DMWORK                 
         B     DMERR                                                            
*                                                                               
DMWRTR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         B     DMERR                                                            
*                                                                               
DMADDR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMADD,ACCMST,DA,AIO,DMWORK                          
         B     DMERR                                                            
*                                                                               
DMPUTR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMPUT,ACCMST,DA,AIO,DMWORK                          
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* CONSTANTS AND LITERAL POOL                                         *          
**********************************************************************          
NEWDISP  DC    Y(ACCRFST-ACCKEY)                                                
*                                                                               
ACCFIL   DC    CL8'ACCOUNT'                                                     
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMGET    DC    CL8'GETREC'                                                      
DMADD    DC    CL8'ADDREC'                                                      
DMPUT    DC    CL8'PUTREC'                                                      
*                                                                               
DELETE   DC    C'DELETE'                                                        
*                                                                               
MSG1     DC    C'RECORD NOT FOUND'                                              
MSG2     DC    C'ELEMENT NOT FOUND'                                             
MSG3     DC    C'RECORD ALREADY ON FILE'                                        
MSG4     DC    C'YOU HAVEN''T CHANGED ANYTHING'                                 
MSG5     DC    C'CAN''T ADD ELEMENT'                                            
MSG6     DC    C'RECORD HAS BEEN DELETED'                                       
MSG7     DC    C'MUST HAVE A BUKELQ ELEMENT'                                    
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* LOCAL WORKING STORAGE                                              *          
**********************************************************************          
LWSD     DSECT                                                                  
RELO     DS    F                             RELOCATION FACTOR                  
*                                                                               
ADDAY    DS    V                                                                
CUREDIT  DS    V                                                                
HELLO    DS    V                                                                
HEXOUT   DS    V                                                                
SCANNER  DS    V                                                                
*                                                                               
AIO      DS    A                                                                
TODAY    DS    XL3                           TODAYS DATE                        
TPASS    DS    XL6                                                              
*                                                                               
DR       DS    PL8                                                              
CR       DS    PL8                                                              
TOTDR    DS    PL8                           ACCUMULATE TOTAL DEBITS            
TOTCR    DS    PL8                           ACCUMULATE TOTAL CREDITS           
*                                                                               
SAVRE    DS    F                                                                
*                                                                               
DKEY     DS    XL(L'ACCKEY)                                                     
DIR      DS    XL64                                                             
DA       DS    XL4                                                              
ELCODE   DS    XL1                                                              
BYTE     DS    XL1                                                              
DMBITS   DS    XL1                                                              
DMRDUP   EQU   X'80'                       READ FOR UPDATE                      
*                                                                               
IO3      DS    XL(IOLENQ)                                                       
LWSX     EQU   *                                                                
         EJECT                                                                  
**********************************************************************          
* DSECTS                                                             *          
**********************************************************************          
NMNTHS   EQU   12                                                               
*                                                                               
LINED    DSECT                               DSECT FOR SCREEN LINE              
LINDATH  DS    CL8                                                              
LINDAT   DS    CL13                          DATE                               
LINDRH   DS    CL8                                                              
LINDR    DS    CL17                          DEBITS                             
LINCRH   DS    CL8                                                              
LINCR    DS    CL17                          CREDITS                            
LINLNQ   EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG LOGTABH                                                            
       ++INCLUDE ACLFMDED                                                       
ACCNT    DS    CL15                          ACCOUNT KEY                        
CNTRA    DS    CL15                          CONTRA ACCOUNT KEY                 
FLGS     DS    XL1                         FLAGS                                
NEWREC   EQU   X'80'                       NEW RECORD                           
ACTIVE   EQU   X'40'                       ACTIVITY                             
*                                                                               
START    DS    XL2                         START DATE                           
END      DS    XL2                         END DATE                             
*                                                                               
MOAS     DS    XL2                         MOA START                            
MOAE     DS    XL2                         MOA END                              
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
         EJECT                                                                  
       ++INCLUDE ACLFMEQU                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
*                                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACLFM25   11/21/02'                                      
         END                                                                    
