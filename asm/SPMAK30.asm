*          DATA SET SPMAK30    AT LEVEL 026 AS OF 01/29/14                      
*PHASE T22830A                                                                  
*INCLUDE NSIWEEK     ---   NEEDED FOR METERED MKT WKLY                          
*                                                                               
T22830   TITLE 'SPMAK30 - MATCHMAKER - REQUESTS/COMMENTS'                       
*                                                                               
*===============================================================*               
*                                                               *               
* HISTORY                                                       *               
* -------                                                       *               
*                                                               *               
* WHEN    LEV WHAT                                              *               
* ----    --- ----                                              *               
* 29JAN14 025 FIX PROGRAM NAME LOOKUP FOR WEEKLY/OVERNIGHT      *               
* 29JAN07 020 FIX PROGRAM NAME LOOKUP                           *               
* 12OCT04 019 CHANGED PHASE CARD FOR RELINK                     *               
* 13SEP04 018 TEST READ-ONLY MODE                               *               
* 02DEC03 017 MAKE OV I2 REQS VISIBLE IN =REQ                   *               
* 18NOV03 016 SUPPORT DESTINATION ID FOR OV                     *               
* 23OCT03 015 EXTENDED SPDEMLK BLOCK                            *               
* 14JUL03 014 METERED WEEKLY MKT                                *               
* 13AUG02 013 RE-ORDER CODE                                     *               
*         --- PROGRAM NAME LOOKUPS                              *               
* 01MAY02 012 FIX DESTIN ID CODE                                *               
* 07FEB02 011 TEST BRAND ALLOCATION LOCKS                       *               
*                                                               *               
*===============================================================*               
*                                                                               
T22830   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPMK30**                                                       
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         USING SPLKXTD,DMLKXTND                                                 
*                                                                               
         CLI   SVRCVEL+1,H3AQ      ADD I2 REQUEST                               
         BNE   *+12                                                             
         BRAS  RE,ADDI2                                                         
         J     EXIT                                                             
*                                                                               
         CLI   SVRCVEL+1,H46Q      PROGRAM NAME LOOKUP?                         
         BNE   *+12                                                             
         BRAS  RE,GETPROG                                                       
         J     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
*=================================================================*             
* GETPROG: CALL SPGETDEMF TO GET PROGRAM NAME                     *             
*=================================================================*             
         SPACE 1                                                                
GETPROG  NTR1  BASE=*,LABEL=*                                                   
         XC    SPDEMLK(SPDEMLKL),SPDEMLK                                        
         L     R0,AIO2                                                          
         ST    R0,SPLKAREC                                                      
         LHI   R1,LENIO                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AIO3                                                          
         ST    R0,SPLKAVAL         A(OUTPUT DEMO VALUES)                        
         LHI   R1,LENIO                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,=X'000000FF'     DON'T REALLY CARE IF DEMOS OR NOT!           
         ST    R0,SPLKALST                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
         LA    R0,DMLKXTND                                                      
         ST    R0,SPLKXTND                                                      
*                                                                               
         OI    SPLKOPT,SPLKO1WB    AFFIDAVIT LOOKUP OPTION                      
         MVI   SPLKFIL,C'T'                                                     
         MVI   SPLKMED,C'T'                                                     
         TM    FLAGS,FLWEEKLY      TEST WEEKLY METERED OPTION                   
         BZ    *+8                                                              
         MVI   SPLKMED,C'W'                                                     
         TM    FLAGS,FLOVNITE      TEST OVERNIGHT OPTION                        
         BZ    *+8                                                              
         MVI   SPLKMED,C'O'                                                     
         MVI   SPLKSRC,C'N'                                                     
         MVI   SPLKSVI,X'FF'                                                    
         MVC   SPLKAGY,QAGY                                                     
         MVC   SPLKCLI,QCLT                                                     
         MVC   SPLKUID,TWAUSRID                                                 
         MVC   SPLKSTA,QSTA                                                     
*                                                                               
         CLC   QBOOK,=C'LATE'      LATEST BOOK?                                 
         BE    GP10                                                             
         MVC   SPLKDBK,QBOOK                                                    
         TM    FLAGS,FLOVNITE      TEST OVERNIGHT OPTION                        
         BNZ   GP10                 YES QBOOK = YYWW                            
         TM    FLAGS,FLWEEKLY      TEST WEEKLY METERED OPTION                   
         BNZ   GP10                 YES QBOOK = YYWW                            
         MVC   DUB(4),QBOOK                                                     
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 VDATCON,DMCB,(0,DUB),(3,FULL)                                    
         MVC   SPLKDBK,FULL                                                     
         SPACE 1                                                                
*===============================================================*               
* GET THE TIME                                                  *               
*===============================================================*               
         SPACE 1                                                                
GP10     OC    RH6TIME,RH6TIME                                                  
         BNZ   *+6                                                              
         DCHO                                                                   
         LA    R1,RH6TIME                                                       
         SR    R3,R3                                                            
*                                                                               
         CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         AHI   R1,1                                                             
         BCT   R3,*-12                                                          
*                                                                               
         LPR   R3,R3                                                            
         GOTO1 VTIMVAL,DMCB,((R3),RH6TIME),SPLKSTIM                             
         MVC   SPLKETIM,SPLKSTIM                                                
         SPACE 1                                                                
*===============================================================*               
* GET THE DAY                                                   *               
*===============================================================*               
         SPACE 1                                                                
         OC    RH6DAYS,RH6DAYS                                                  
         BNZ   *+6                                                              
         DCHO                                                                   
         LA    R1,RH6DAYS          GET DAYS IN SAME FORMAT AS BDDAY             
         SR    R3,R3                                                            
*                                                                               
         CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         AHI   R1,1                                                             
         BCT   R3,*-12                                                          
*                                                                               
         LPR   R3,R3                                                            
         GOTO1 VCALLOV,DMCB,0,X'D9000A03'  GET ADDRESS OF DAYPAK                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),((R3),RH6DAYS),SPLKDAY,BYTE                            
*                                                                               
         XC    WORK,WORK           SET UP 1W PROFILE KEY                        
         MVC   WORK+0(4),=C'S01W'                                               
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),QMED                                                   
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,PROF1W,VDATAMGR                                   
         LA    R0,PROF1W                                                        
         ST    R0,SPLKA1W                                                       
         SPACE 1                                                                
*===============================================================*               
* SET SPLKXTND OPTS                                             *               
*===============================================================*               
         SPACE 1                                                                
         OI    SPXTFLAG,SPXTRAPP                                                
         SPACE 1                                                                
*===============================================================*               
* CALL SPGETDEMF                                                *               
*===============================================================*               
         SPACE 1                                                                
         GOTO1 VCALLOV,DMCB,0,X'D9000A21'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(X'FF',SPDEMLK)                                        
*                                                                               
         XC    DUB,DUB                                                          
         LA    R1,H46Q                                                          
         BRAS  RE,SENDH                                                         
         OC    SPLKABK,SPLKABK     ANY DATE RETURNED?                           
         BZ    GP30                 NO                                          
*                                                                               
         MVI   BYTE,0                                                           
         TM    FLAGS,FLOVNITE      TEST OVERNIGHTS OPTION                       
         BZ    *+12                                                             
         MVI   BYTE,1              MONDAY START OF WEEK                         
         B     GP16                                                             
         TM    FLAGS,FLWEEKLY      TEST WEEKLY METERED OPTION                   
         BZ    GP20                 NO                                          
GP16     GOTO1 =V(NSIWEEK),DMCB,(C'D',SPLKABK),(BYTE,VGETDAY),VADDAY,  X        
               VDATCON,RR=Y                                                     
         L     RF,0(R1)                                                         
         MVC   DUB(6),0(RF)                                                     
         GOTO1 VDATCON,DMCB,(0,DUB),(X'20',DUB)    *Y2K                         
         LA    R5,6                                                             
         B     GP30                                                             
*                                                                               
GP20     MVC   FULL(2),SPLKABK                                                  
         MVI   FULL+2,1                                                         
         GOTO1 VDATCON,DMCB,(3,FULL),(X'20',DUB)                                
         LA    R5,4                                                             
*                                                                               
GP30     LA    R1,1                                                             
         LA    R4,DUB                                                           
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,4                                                             
         LA    R4,SPLKPRG                                                       
         BRAS  RE,SENDD                                                         
*                                                                               
GPX      J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================*               
* BUILD A REQUEST                                               *               
*===============================================================*               
         SPACE  1                                                               
ADDI2    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,TESTRO                                                        
*                                                                               
* TEST FOR I2 COMMENT DATA                                                      
         L     R4,AIO2                                                          
         OC    0(16,R4),0(R4)                                                   
         BZ    *+8                 NO COMMENT - JUST ADD REQ                    
         BRAS  RE,I2COM                                                         
*                                                                               
         XC    IDNUM,IDNUM                                                      
         CLC   QDEST,SPACES                                                     
         BNH   *+8                                                              
         BAS   RE,GETDEST                                                       
*                                                                               
         MVI   XSP,C'N'                                                         
         XC    BLOCK,BLOCK                                                      
         MVC   BLOCK+26(80),SPACES                                              
*                                                                               
Z        USING ZRECD,BLOCK                                                      
*                                                                               
         OC    IDNUM,IDNUM         TEST OVERRIDE                                
         BZ    *+10                                                             
         MVC   Z.REQDEST,IDNUM                                                  
         OI    Z.REQFLAG,X'01'     LINKED                                       
*                                                                               
         MVC   Z.ZPROG,=C'I2'                                                   
         MVC   Z.ZAGY,QAGY                                                      
         MVC   Z.ZMED,QMED                                                      
         MVC   Z.ZCLT,QCLT                                                      
         MVC   Z.ZPRD,QPRD                                                      
         MVC   Z.ZMKT,QMKT                                                      
         MVC   Z.ZSTA,QSTA                                                      
         CLI   QSTA,C'0'           CABLE STA?                                   
         BL    *+8                  NO                                          
         MVI   Z.ZSTA+4,C'/'                                                    
         MVC   Z.ZEST,QEST                                                      
         CLC   =C'000',QEST                                                     
         BNE   *+10                                                             
         MVC   Z.ZEST,=C'NO '                                                   
         MVC   Z.ZESTEND,QEST2                                                  
         MVC   Z.ZPRD2,QPRD2       PIGGYBACK PARTNER                            
         MVC   Z.ZSTART(4),QPER+6  END YYMM                                     
         MVC   Z.ZBOOK1,QBOOK                                                   
         MVC   Z.ZHUT1,QHUT                                                     
         MVC   Z.ZUESTOR,QUESTOR                                                
         MVI   Z.ZOPT5,C'Y'        TELL I2 TO POST AFFIDS                       
         OC    Z.ZAREA,SPACES      MAKE REQUEST UPPERCASE/NO NULLS              
         DROP  Z                                                                
*                                                                               
         CLI   QWHEN,C'S'          TEST SOON REQUEST                            
         BE    ADD10                                                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',BLOCK,BLOCK                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         MVC   WORK(9),=C'REQUESTED'                                            
         B     ADDI2X                                                           
         EJECT                                                                  
*===========================================================*                   
* TRY TO ADD A SOON REQUEST                                 *                   
*===========================================================*                   
         SPACE 1                                                                
ADD10    DS    0H                                                               
         LA    R2,WORK2                                                         
         USING SPOOK,R2                                                         
         XC    0(SPOOKL,R2),0(R2)  BUILD SPOOK BLOCK                            
*                                                                               
         MVC   SPOOKUID,TWAUSRID   CONNECT ID                                   
         OC    IDNUM,IDNUM         TEST OVERRIDE                                
         BZ    *+10                                                             
         MVC   SPOOKUID,IDNUM                                                   
*                                                                               
         MVC   SPOOKAGY,TWAAGY     TWO CHARACTER ID CODE                        
         MVC   SPOOKDID,QUESTOR    USER INITIALS (ID)                           
         OC    SPOOKDID,SPACES                                                  
         CLI   SPOOKDID+2,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+2,C'*'                                                  
         CLI   SPOOKDID+1,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+1,C'*'                                                  
*                                                                               
         MVC   SPOOKSYS,=C'SP'     SPOT SYSTEM                                  
         MVC   SPOOKEOD,=C'I2'                                                  
         MVC   SPOOKJCL,=C'I2'                                                  
         MVI   SPOOKWEN,5          SET SOON STATUS/UPDATIVE SOON                
*                                                                               
         BRAS  RE,ADDLOCKS                                                      
*                                                                               
         L     RE,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,RE                                                      
         L     RF,CREQTWA                                                       
         DROP  RE                                                               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 (RF),DMCB,(4,(RA)),BLOCK,VDATAMGR,ACOMFACS,(R2)                  
*                                                                               
         L     RE,8(R1)            GET A(PRTQUE) KEY                            
         SR    R0,R0                                                            
         ICM   R0,3,6(RE)                                                       
         EDIT  (R0),(4,FULL),ALIGN=LEFT                                         
* BUILD SOON ID STRING FOR HABES                                                
         MVC   WORK(3),SPOOKDID                                                 
         MVI   WORK+3,C','                                                      
         MVC   WORK+4(4),FULL                                                   
         MVI   WORK+8,C' '                                                      
*                                                                               
ADDI2X   LA    R1,H3AQ                                                          
         BRAS  RE,SENDH                                                         
*                                                                               
         LA    R1,4                                                             
         LA    R4,WORK                                                          
         BRAS  RE,SENDD                                                         
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=============================================================*                 
* ROUTINE TO ADD I2 COMMENTS                                                    
*=============================================================*                 
         SPACE 1                                                                
X        USING XCOMRECD,KEY                                                     
*                                                                               
I2COM    NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   X.COMI2K,=X'0D0C'                                                
         MVC   X.COMI2KAM,BAGYMD                                                
         MVI   X.COMI2KTY,C'I'                                                  
         MVC   X.COMI2KCL,BCLT                                                  
         MVC   X.COMI2KPR,QPRD                                                  
         MVC   X.COMI2KES,BEST                                                  
         MVC   X.COMI2KP2,QPRD2                                                 
         CLC   BEST,BEST2                                                       
         BE    *+10                                                             
         MVC   X.COMI2KE2,BEST2                                                 
         MVC   X.COMI2KST,BSTA                                                  
         MVC   X.COMI2KYM,BMOS                                                  
         MVI   XSP,C'Y'                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         USING XCOMRECD,R8                                                      
         L     R8,AIO1                                                          
         ST    R8,AIO                                                           
*                                                                               
         CLC   KEY(32),KEYSAVE                                                  
         BE    COM10                                                            
         DROP  X                                                                
* BUILD NEW RECORD                                                              
*                                                                               
         XC    0(256,R8),0(R8)                                                  
         MVC   0(32,R8),KEYSAVE                                                 
         MVC   32(2,R8),=AL2(32+10+12)                                          
         MVI   XCOMEL,1                                                         
         MVI   XCOMEL+1,12                                                      
         GOTO1 VDATCON,DMCB,(5,0),(3,XCOMEL+2)  CREATED                         
         B     COM12                                                            
*                                                                               
COM10    MVI   XSP,C'Y'                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
COM12    GOTO1 VDATCON,DMCB,(5,0),(3,XCOMEL+5)  ACTIVITY DATE                   
         SPACE 1                                                                
*=============================================================*                 
* REMOVE ALL 05 ELEMENTS                                                        
*=============================================================*                 
         SPACE 1                                                                
         GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(5,(R8)),0,0                       
*                                                                               
         L     R6,AIO2             POINT TO COMMENT DATA                        
*                                                                               
COM22    CLI   0(R6),0                                                          
         BE    COM30                                                            
*                                                                               
         XC    BLOCK,BLOCK                                                      
         MVI   BLOCK,X'05'                                                      
         SR    RE,RE                                                            
         IC    RE,0(R6)                                                         
         AHI   RE,1                                                             
         STC   RE,BLOCK+1          SET ELEMENT LENGTH                           
         AHI   RE,-3               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+2(0),1(R6)                                                 
         OC    BLOCK+2(80),SPACES  MAKE SURE UPPERCASE                          
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R8),BLOCK,=C'ADD=CODE'            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R6)                                                         
         AR    R6,R0                                                            
         B     COM22                                                            
*                                                                               
COM30    DS    0H                                                               
         MVI   XSP,C'Y'                                                         
         MVI   RDUPDATE,C'Y'                                                    
         L     RF,PUTREC                                                        
         CLC   KEY(32),KEYSAVE     DID WE FIND THE RECORD                       
         BE    *+8                                                              
         L     RF,ADDREC                                                        
         GOTO1 (RF)                                                             
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* READ MY ID RECORD FROM CTFILE AND CALL GETIDS TO VALIDATE DEST  *             
*=================================================================*             
         SPACE 1                                                                
GETDEST  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),TWAUSRID                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                     
*                                                                               
         L     R6,AIO2                                                          
         USING CTIREC,R6                                                        
         CLC   KEY(25),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
* NOW CHECK THAT DESTINATION IS VALID                                           
         GOTO1 VCALLOV,DMCB,0,X'D9000AFA'   GET V(GETIDS)                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DCHO                                                                   
         L     RF,0(R1)                                                         
         XC    WORK,WORK                                                        
         MVC   WORK(8),QDEST                                                    
         OC    WORK(10),SPACES                                                  
         GOTO1 (RF),DMCB,(C'D',AIO2),0,(C'A',VDATAMGR),WORK                     
*                                                                               
         CLI   DMCB+4,0            ANY ERROR FROM GETIDS?                       
         BNE   GETIDERR             YES - ERROR                                 
         CLI   DMCB+12,0           TEST ANY MATCH (EXACT OR ALL <> 0)           
         BE    GETIDERR            NO - ERROR                                   
*                                                                               
         L     RE,DMCB+4           GET A(OUTPUT BLOCK)                          
         MVC   IDNUM,10(RE)        SAVE DEST IDNUM                              
         J     EXIT                                                             
*                                                                               
GETIDERR MVC   ERROR,=Y(BADUSRID)                                               
         GOTO1 SENDMSG                                                          
*                                                                               
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*===============================================================*               
* ADD LOCKS FOR UPDATIVE SOON REQUESTS                          *               
*===============================================================*               
         SPACE 1                                                                
ADDLOCKS NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,BLDNVLK          TEST ANY NV LOCKS                            
         BRAS  RE,TESTIT                                                        
         BRAS  RE,TESTIT                                                        
*                                                                               
L        USING LKKEYD,LKBLOCK                                                   
*                                                                               
         CLC   =C'000',QEST        EST=NO/ALL?                                  
         BE    ADDLK2               YES, DON'T NEED TO TEST 'ALL' LOCK          
         MVC   L.LKBAEST,SPACES                                                 
         BRAS  RE,TESTIT                                                        
*                                                                               
* FOR CANADA, IF MEDIA T OR N, ALSO TEST FOR MEDIA C BA LOCK                    
ADDLK2   CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   ADDLK3               NO                                          
         CLI   QMED,C'T'                                                        
         BE    *+12                                                             
         CLI   QMED,C'N'                                                        
         BNE   ADDLK3                                                           
         BRAS  RE,BLDALOC                                                       
         MVI   L.LKBAMED,C'C'                                                   
         BRAS  RE,TESTIT                                                        
         CLC   =C'000',QEST        EST=NO/ALL?                                  
         BE    ADDLK3               YES, DON'T NEED TO TEST 'ALL' LOCK          
         MVC   L.LKBAEST,SPACES                                                 
         BRAS  RE,TESTIT                                                        
         DROP  L                                                                
*                                                                               
* ADD BUY LOCK                                                                  
ADDLK3   BAS   RE,BLDBULK                                                       
*                                                                               
ADDLK4   L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKLOCKQ',LKBLOCK),ACOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK4                                                           
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
*                                                                               
         BAS   RE,BLDNVLK                                                       
*                                                                               
ADDLK6   L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKLOCKQ',LKBLOCK),ACOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK6                                                           
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
*                                                                               
ADDLKX   J     EXIT                                                             
*                                                                               
         SPACE 1                                                                
TESTIT   LR    R0,RE                                                            
         L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKBLOCK),ACOMFACS                           
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TESTIT               YES - TRY AGAIN                             
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ADDLKERR MVC   ERROR,=Y(DATALOCK)                                               
         GOTO1 SENDMSG                                                          
*                                                                               
         SPACE 1                                                                
L        USING LKKEYD,LKBLOCK                                                   
*                                                                               
BLDNVLK  DS    0H                                                               
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,QAGY                                                   
         MVC   L.LOCKRTY,=C'NV'    INVOICE RECORDS                              
         MVC   L.LKNVMED,QMED                                                   
         MVC   L.LKNVCLT,QCLT                                                   
         MVC   L.LKNVSTA,QSTA                                                   
*                                                                               
         CLI   L.LKNVMED,C'X'      FOR MEDIA X                                  
         BNE   *+8                                                              
         MVI   L.LKNVSTA+4,C'X'    SET BAND X                                   
*                                                                               
         CLI   L.LKNVSTA+4,C' '    SET BAND IF NOT SET                          
         BHR   RE                                                               
         MVI   L.LKNVSTA+4,C'T'                                                 
         CLI   L.LKNVSTA,C'0'      TEST CABLE                                   
         BLR   RE                                                               
         MVI   L.LKNVSTA+4,C'/'                                                 
         BR    RE                                                               
         DROP  L                                                                
*                                                                               
         SPACE 1                                                                
L        USING LKKEYD,LKBLOCK                                                   
*                                                                               
BLDBULK  XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKAGY,QAGY                                                   
         MVC   L.LOCKRTY,=C'BU'    BUY RECORDS                                  
         MVC   L.LKBUMED,QMED                                                   
         MVC   L.LKBUCLT,QCLT                                                   
         MVC   L.LKBUSTA,QSTA                                                   
*                                                                               
         CLI   L.LKBUMED,C'X'      FOR MEDIA X                                  
         BNE   *+8                                                              
         MVI   L.LKBUSTA+4,C'X'    SET BAND X                                   
*                                                                               
         CLI   L.LKBUSTA+4,C' '    SET BAND IF NOT SET                          
         BHR   RE                                                               
         MVI   L.LKBUSTA+4,C'T'                                                 
         CLI   L.LKBUSTA,C'0'      TEST CABLE                                   
         BLR   RE                                                               
         MVI   L.LKBUSTA+4,C'/'                                                 
         BR    RE                                                               
         DROP  L                                                                
*                                                                               
         SPACE 1                                                                
L        USING LKKEYD,LKBLOCK                                                   
*                                                                               
BLDALOC  XC    L.LOCKEY,L.LOCKEY   BUILD SPECIFIC EST LOCK KEY                  
         MVC   L.LOCKAGY,QAGY                                                   
         MVC   L.LOCKRTY,=C'BA'    BUY RECORDS                                  
         MVC   L.LKBAMED,QMED                                                   
         MVC   L.LKBACLT,QCLT                                                   
         CLC   =C'000',QEST        IF EST=NO, DON'T TEST EST                    
         BER   RE                                                               
         MVC   L.LKBAEST,QEST                                                   
         BR    RE                                                               
         DROP  L                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*=================================================================*             
* TEST READ ONLY ID/FACPAK                                        *             
*=================================================================*             
TESTRO   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,(CXTRAINF-COMFACSD)(RF)                                    
         BZR   RE                                                               
         USING XTRAINFD,RF                                                      
**NOP    TM    XIFLAG1,XITSTADV    THIS A TEST SYS?                             
**NOP    BNZR  RE                   YES - WHO CARES                             
         TM    XIFLAG1,XIROSYS+XIROMODE+XIWRONGF                                
         BZR   RE                                                               
         DROP  RF                                                               
*                                                                               
         BRAS  RF,*+6              PUT A(=Y(NOUPDATE)) IN RF                    
         DC    Y(NOUPDATE)                                                      
         MVC   ERROR,0(RF)                                                      
         GOTO1 SENDMSG                                                          
         DC    H'0'                TAKE NO PRISONERS!                           
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LKBUMED  DS    XL1                                                              
LKBUCLT  DS    XL3                                                              
LKBUSTA  DS    XL5                                                              
*                                                                               
         ORG   LOCKKEY                                                          
LKNVMED  DS    XL1                                                              
LKNVCLT  DS    XL3                                                              
LKNVSTA  DS    XL5                                                              
*                                                                               
         ORG   LOCKKEY                                                          
LKBAMED  DS    XL1                                                              
LKBACLT  DS    XL3                                                              
LKBAEST  DS    XL3                                                              
         EJECT                                                                  
ZRECD    DSECT                                                                  
ZCTL     DS    XL26                                                             
ZAREA    DS    0CL80   COLUMN                                                   
ZPROG    DS    0CL2    ------                                                   
ZCODE    DS    CL2        1        PROGRAM CODE                                 
ZAGY     DS    CL2        3        AGENCY CODE                                  
ZMED     DS    CL1        5        MEDIA CODE (R/T)                             
ZCLT     DS    CL3        6        CLIENT CODE                                  
ZPGR     DS    CL1        9        PROCESS BY DIVISION                          
ZMGR     DS    CL1       10        PROCESS BY DISTRICT                          
ZCLOFFC  DS    CL1       11        CLIENT OFFICE FILTER                         
ZBYID    EQU   ZCLOFFC             C'Y' IF BUYS PROCESSED BY ID                 
ZPRD     DS    CL3       12        PRODUCT MNEMONIC                             
ZMKT     DS    CL4       15        MARKET NUMBER                                
ZSTA     DS    CL5       19        STATION CALL LETTERS                         
ZEST     DS    CL3       24        ESTIMATE NUMBER                              
ZESTEND  DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
         DS    CL1       30        Y=DEMO OVERRIDE ACTIVE                       
ZCONTREQ DS    CL1       31        C'*' ==> DATA IN QAREA2                      
ZSTAUTO  DS    CL3       32        AUTO REQUEST START DATE                      
ZENDAUTO DS    CL3       35        AUTO REQUEST END DATE                        
         ORG   *-3                                                              
ZPRD2    DS    CL3       35        PIGGYBACK PARTNER                            
ZSTART   DS    CL6       38        REQUEST START DATE                           
ZEND     DS    0CL6      44        REQUEST END DATE                             
ZTODAY   DS    CL6       44                                                     
ZBOOK1   DS    CL4       50        RATING BOOK (YYMM) GOAL/ESTD DATA            
ZHUT1    DS    CL2       54        HUT ADJUSTMENT MONTH                         
ZRERATE  DS    CL1       56        RERATE TYPE                                  
ZCOMPARE DS    CL1       57        DATA COMPARE OPTION                          
ZAFFIL   DS    CL1       58        AFFILIATION FILTER                           
ZPRGTYPE DS    CL1       59        PROGRAM TYPE FILTER                          
ZDPTDET  DS    CL1       60        DAYPART DETAIL CONTROL                       
*                                  A=SHOW FULL DETAIL (DEFAULT)                 
*                                  B=SUPPRESS SPOT-LENGTH                       
*                                  C=SUPPRESS SPOT-LENGTH & DAY-PART            
ZDPTMENU DS    CL1       61        DAYPART MENU OVERRIDE                        
ZOPT1    DS    CL1       62        OPTION 1                                     
ZOPT2    DS    CL1       63        OPTION 2                                     
ZOPT3    DS    CL1       64        OPTION 3                                     
ZOPT4    DS    CL1       65        OPTION 4                                     
ZOPT5    DS    CL1       66        OPTION 5                                     
ZGRP     DS    CL2       67        GROUP                                        
ZFILTER  EQU   QGRP                FILTER TYPE/VALUE                            
ZUESTOR  DS    CL12      69        REQUESTOR NAME                               
         EJECT                                                                  
         ORG   ZAREA+57                                                         
ZCMRCL   DS    CL8       58        COMMERCIAL FILTER                            
         ORG   ZAREA+29                                                         
ZREP     DS    CL3       30        DISCREPANCY REP                              
         ORG   ZCTL                                                             
       ++INCLUDE DMREQHDR                                                       
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPMAKWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
IDNUM    DS    H                                                                
LKBLOCK  DS    XL25                                                             
PROF1W   DS    CL16                                                             
DMLKXTND DS    XL32                                                             
       ++INCLUDE SPDEMLK                                                        
       ++INCLUDE SPDEMLKXTD                                                     
         ORG                                                                    
       ++INCLUDE DDSPOOK                                                        
XCOMRECD DSECT                                                                  
       ++INCLUDE SPGENXCOM                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAXTRAINF                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPMAK30   01/29/14'                                      
         END                                                                    
