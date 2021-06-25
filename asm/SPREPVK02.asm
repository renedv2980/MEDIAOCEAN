*          DATA SET SPREPVK02  AT LEVEL 001 AS OF 02/26/20                      
*******20200113:042615: NEW MEMBER ADDED BY HWON FOR PROJ# SPEC-36764           
*******        :042615: SPREPVK01/02 - NEW SPOT VENDOR LOCK REQUEST             
*PHASE SPVK02C                                                                  
         TITLE 'SPVK02 - SPOT BUY VENDOR LOCK'                                  
         SPACE 1                                                                
***********************************************************************         
* THIS PROGRAM ORIGINALLY STARTED AS AN FX (COPIED FROM SPREPFXVLK)             
* AND WAS CHANGED TO BE CLT REQUESTABLE REPORT.                                 
* THIS IS NOT A NORMAL SPONSOR JOB, AND ONLY PROCS ON REQFRST                   
***********************************************************************         
SPVK02   CSECT                                                                  
         DS    6000C                                                            
         ORG   SPVK02                                                           
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPVK02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPVK02,RB,RC                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAIN                                                             
*                                                                               
YES      CR    RB,RB               SET CC EQUAL                                 
         B     XIT                                                              
*                                                                               
NO       LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
MAIN     DS    0H                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT1,C'Y'          TEST RUN?                                    
         BNE   *+8                  NO                                          
         MVI   RCSUBPRG,1                                                       
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LAY   R6,IO                                                            
         ST    R6,AREC             IF I FORGET THIS                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY              LOOK AT THE AGENCY RECORD                    
         USING AGYKEY,R6                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 GET                                                              
         L     R6,AREC                                                          
         CLC   0(L'AGYKEY,R6),KEY       DIE IN CASE IF WE GOT A DIFF            
         BE    *+6                          RECORD THAN WHAT WE EXPECT          
         DC    H'0'                                                             
*                                                                               
         MVC   CANADIAN,AGYPCNDA                                                
         DROP  R6                                                               
*                                                                               
         CLI   CANADIAN,C'C'       CANADA?                                      
         BNE   MAIN010                                                          
*                                                                               
         CLI   QMED,C'T'           THESE 2 MEDIA ARE NOT ALLOWED,               
         BE    *+12                  THEY SHOULD REQUEST MEDIA C                
         CLI   QMED,C'N'                                                        
         BNE   MAIN010                                                          
         MVC   P(25),=CL25'MEDIA T NOR N IS ALLOWED!'                           
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         B     MAINX               NO, THEN NOTHING TO DO HERE                  
*                                                                               
MAIN010  GOTO1 DATCON,DMCB,(0,QSTART),(3,LOCKDATE)                              
*                                                                               
         XC    BINPARMS(24),BINPARMS  SETUP BINSRCH PARAMETERS                  
         GOTO1 ,BINPARMS,,STATNTBL,0,L'EBDCSTTN,L'EBDCSTTN,0                    
         L     R1,=AL4(MAXSTATN)                                                
         ST    R1,BINPARMS+20      MAX # RECORDS IN 6TH PARAMETER               
*                                                                               
*                                                                               
         BAS   RE,READBUY          READ BUYS & BLD ACTIVE STATION LIST          
*                                                                               
         BAS   RE,RDLKSTA          READ & LOCK MASTERS NOT ACTIVE               
*                                                                               
         MVC   P(35),=CL35'# OF MASTER RECORDS FOR AGENCY: '                    
         EDIT  (P6,AGYMSTRS),(10,P+35)                                          
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         MVC   P(35),=CL35'# OF MASTER RECORDS WE CAN LOCK: '                   
         EDIT  (P6,LCKMSTRS),(10,P+35)                                          
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
MAINX    GOTO1 AENDREQ                                                          
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
***********************************************************************         
*                                                                               
READBUY  NTR1  LABEL=*                                                          
         ZAP   NUMLOCK,=P'0'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING BUYKEY,R6                                                        
*                                                                               
         MVC   CURRMED,BAGYMD                                                   
         CLI   QMED,C'C'           USER REQUESTED MEDIA C?                      
         BNE   RDBY010                                                          
         NI    CURRMED,X'F0'       GET RID OF THE X'08' FOR MEDIA C             
         OI    CURRMED,X'01'       START WITH MEDIA T BUYS                      
*                                                                               
RDBY010  MVC   BUYKAM,CURRMED      STARTING WITH THIS AGENCY/MEDIA              
*                                                                               
RDBYRHI  GOTO1 HIGH                                                             
*                                                                               
RDBY020  DS    0H                                                               
         CLI   QOPT3,C'Y'          PRINT BUY READ SEQUENCE                      
         BNE   RDBY030                                                          
         MVC   P(10),=C'KEYSAVE=  '                                             
         GOTO1 HEXOUT,DMCB,KEYSAVE,P+10,18                                      
         GOTO1 REPORT                                                           
         MVC   P(10),=C'KEY=      '                                             
         GOTO1 HEXOUT,DMCB,KEY,P+10,18                                          
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
RDBY030  LA    R6,KEY                                                           
         CLC   BUYKAM,CURRMED      STILL THE CURRENT AGY/MEDA BUYS?             
         BNE   RDBY100             NO, WE'RE DONE WITH THIS AGY/MED             
         CLI   BUYKPRD,X'FF'       ONLY WANT POL KEYS                           
         BE    RDBY035                                                          
         MVI   BUYKPRD,X'FF'                                                    
         XC    BUYKMKT(BUYKCNTL-BUYKMKT),BUYKMKT                                
         B     RDBYRHI                                                          
*                                                                               
RDBY035  DS    0H                                                               
         TM    BUYKBUY,X'80'       SPILL?                                       
         BNZ   RDBYRSQ             YES, THEN WE DON'T CARE ABOUT IT             
*                                                                               
         MVC   CHCKSTTN,BUYKSTA                                                 
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'CHCKSTTN),CHCKSTTN                                      
         GOTO1 MSUNPK,DMCB,WORK,WORK+10,EBDCSTTN                                
*                                                                               
         CLI   QMED,C'C'           IF CANADIAN COMBINED, THEN WE DON'T          
         BNE   RDBY040             THEN WE DON'T CARE                           
         MVI   EBDCSTTN+4,C' '     ABOUT THE BAND, (C,N, NOR T)                 
         B     RDBY050                                                          
*                                                                               
RDBY040  CLI   EBDCSTTN+4,C' '                                                  
         BE    RDBY045                                                          
         CLI   EBDCSTTN+4,C'/'                                                  
         BNE   RDBY050                                                          
RDBY045  MVI   EBDCSTTN+4,C'T'                                                  
*                                                                               
RDBY050  CLC   LEBDCSTN,EBDCSTTN   ALREADY LOOKED IN BINSRCH TABLE              
         BE    RDBY090             YES, FOR THIS STATION, CHECK BUY             
         GOTO1 BINSRCH,BINPARMS,(X'00',EBDCSTTN)                                
*                                                                               
         CLI   0(R1),X'01'         RECORD NOT FOUND?                            
         BE    RDBY090             YES, NOT IN BINSRCH TABLE                    
*                                                                               
         CLI   QOPT4,C'Y'          PRINT BINSEARCH CONDITIONS                   
         BNE   RDBY060                                                          
         MVC   P+20(25),=CL25'IN BINSRCH ALREADY'                               
         GOTO1 HEXOUT,DMCB,CHCKSTTN,P,3                                         
         MVC   P+10(L'EBDCSTTN),EBDCSTTN                                        
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
*  SKIP TO NEXT STATION                                                         
RDBY060  LA    R6,KEY                                                           
         CLI   QMED,C'C'           MEDIA C?                                     
         BNE   RDBY070                                                          
         XR    RF,RF               YES, CAN SKIP THE LAST BYTE OF STA           
         ICM   RF,3,BUYKSTA                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,3,BUYKSTA                                                     
         MVI   BUYKSTA+2,0                                                      
         B     RDBY080                                                          
*                                                                               
RDBY070  XR    RF,RF                                                            
         ICM   RF,7,BUYKSTA                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,7,BUYKSTA                                                     
*                                                                               
RDBY080  XC    BUYKEST(BUYKCNTL-BUYKEST),BUYKEST                                
         XC    LEBDCSTN,LEBDCSTN                                                
         B     RDBYRHI             READ FOR NEXT STATION                        
*                                                                               
RDBY090  DS    0H                                                               
         MVC   LEBDCSTN,EBDCSTTN   WE LOOKED IN TABLE FOR THIS STATION          
         GOTO1 GET                                                              
*                                                                               
         L     R6,AREC                                                          
         USING BUYKEY,R6                                                        
         CLC   BDEND,LOCKDATE      BUY END DATE ON OR AFTER LOCKDATE?           
         BL    RDBYRSQ                                                          
*                                                                               
         CLI   QOPT4,C'Y'          PRINT BINSEARCH CONDITIONS                   
         BNE   RDBY095                                                          
         MVC   P+20(10),=C'TO BINSRCH'                                          
         GOTO1 HEXOUT,DMCB,CHCKSTTN,P,3                                         
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'CHCKSTTN),CHCKSTTN                                      
         MVC   P+10(L'EBDCSTTN),EBDCSTTN                                        
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
RDBY095  GOTO1 BINSRCH,BINPARMS,(X'01',EBDCSTTN)  ADD TO BINSRCH                
         OC    1(3,R1),1(R1)       TABLE FULL?                                  
         BNZ   RDBY060             NO, PROCEED TO NEXT STATION IN MKT           
         DC    H'0'                                                             
         DC    C'INCREASE MAXSTATN'  SO WE SEE RIGHT AWAY                       
*                                                                               
RDBYRSQ  GOTO1 SEQ                                                              
         B     RDBY020                                                          
*                                                                               
RDBY100  CLI   QMED,C'C'           USER REQUESTED MEDIA C?                      
         BNE   READBUYX            NO, TRULY DONE READING BUYS                  
         MVC   BYTE,CURRMED                                                     
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,03             HAVE WE CHECK MEDIA N YET?                   
         BE    READBUYX            YES, WE'RE DONE                              
         OI    CURRMED,X'02'       MODIFY MED FROM T-X'01' TO  N-X'03'          
         XC    KEY,KEY                                                          
         B     RDBY010                                                          
READBUYX B     XIT                                                              
         EJECT                                                                  
***********************************                                             
* NOW COMPARE AGAINST MASTER RECORDS                                            
***********************************                                             
RDLKSTA  NTR1  LABEL=*                                                          
         CLI   QOPT2,C'Y'          OPTIONAL PRINT LINE                          
         BNE   RLST010                                                          
         MVC   P(31),=C'*** CHECKING MASTER RECORDS ***'                        
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
RLST010  ZAP   AGYMSTRS,=P'0'                                                   
         ZAP   LCKMSTRS,=P'0'                                                   
*                                                                               
         MVC   CURRMED,QMED         CURRMED IS NOW EBCDIC MEDIA                 
*                                   IF MEDIA C, WE WANT TO START THERE          
*                                     ANYWAYS                                   
RLST020  LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING STAKEY,R4                                                        
         MVI   STAKTYPE,STAKTYPQ    C'S' - MASTER RECORDS                       
         MVC   STAKMED,CURRMED                                                  
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,ADSTAT               
*                                                                               
RLST030  L     R4,ADSTAT                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'STAKEY),STAKEY                                             
*                                                                               
         CLI   QOPT5,C'Y'          PRINT STATION READ SEQUENCE                  
         BNE   RLST040                                                          
         MVC   P(10),=C'KEYSAVE=  '                                             
         GOTO1 HEXOUT,DMCB,KEYSAVE,P+10,15                                      
         MVC   P+50(L'STAKEY),KEYSAVE                                           
         GOTO1 REPORT                                                           
         MVC   P(10),=C'KEY=      '                                             
         GOTO1 HEXOUT,DMCB,KEY,P+10,15                                          
         MVC   P+50(L'STAKEY),KEY                                               
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
RLST040  LA    R4,KEY                                                           
         CLI   STAKTYPE,STAKTYPQ    C'S'                                        
         BNE   RDLKSTAX                                                         
         CLC   STAKMED,CURRMED      SAME MEDIA?                                 
         BNE   RLST090              NO                                          
         CLC   STAKAGY,QAGY                                                     
         BE    RLST050                                                          
*                                                                               
RLSTSEQ  MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),(0,=C'STATION'),KEY,ADSTAT           
         B     RLST030                                                          
*                                                                               
RLST050  L     R4,ADSTAT                                                        
         AP    AGYMSTRS,=P'1'                                                   
*                                                                               
         MVC   EBDCSTTN,STAKCALL                                                
*                                                                               
         CLI   QMED,C'C'                                                        
         BNE   RLST060                                                          
         MVI   EBDCSTTN+4,C' '                                                  
*                                                                               
RLST060  GOTO1 BINSRCH,BINPARMS,(X'00',EBDCSTTN)                                
*                                                                               
         CLI   0(R1),X'01'         RECORD NOT FOUND?                            
         BE    RLST070             NOT FOUND                                    
*                                                                               
         CLI   QOPT5,C'Y'          PRINT STATION READ SEQUENCE                  
         BNE   RLSTSEQ                                                          
         MVC   P+35(25),=CL25'IN BINSRCH ALREADY'                               
         BAS   RE,PRTLKSTA                                                      
         B     RLSTSEQ             STATION IS IN TABLE, HAS BUYS                
*                                                                               
RLST070  TM    SFLAG1,SLOCK        ALREADY LOCKED?                              
         BNZ   RLSTSEQ             YES, DON'T BOTHER SHOWING THIS ONE           
         BAS   RE,PRTLKSTA                                                      
         AP    LCKMSTRS,=P'1'                                                   
*                                                                               
         CLI   QOPT1,C'Y'          TEST RUN?                                    
         BE    RLSTSEQ              YES                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),=C'STATION',KEY,        +        
               ADSTAT                                                           
         L     R4,ADSTAT                                                        
         OI    SFLAG1,SLOCK        NO, LOCK IT NOW                              
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'STATION',KEY,ADSTAT                
*&&DO                                                                           
         CLI   RCWRITE,C'Y'        ARE WE ACTUALLY WRITING?                     
         BNE   RLST080             NO                                           
         AP    NUMLOCK,=P'1'                                                    
         CP    NUMLOCK,=P'10000'   WE DO 10,000 AT A TIME                       
         BNL   RDLKSTAX                                                         
*&&                                                                             
RLST080  GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,ADSTAT               
         B     RLSTSEQ                                                          
*                                                                               
RLST090  CLI   QMED,C'C'           COMBINED MEDIA?                              
         BNE   RDLKSTAX            NO, NOTHING LEFT TO DO                       
         CLI   CURRMED,C'C'        YES, DID WE JUST PROCESSED MEDIA C?          
         BNE   RLST100                  NO, HAS TO BE  N OR T  THEN             
         MVI   CURRMED,C'N'             YES, DO MEDIA N NOW                     
         B     RLST020                                                          
*                                                                               
RLST100  CLI   CURRMED,C'T'        WAS IT MEDIA T?                              
         BE    RDLKSTAX            YES, THEN WE'RE DONE                         
         MVI   CURRMED,C'T'        NO, HAS TO BE MEDIA N, DO T NOW              
         B     RLST020                                                          
RDLKSTAX B     XIT                                                              
*                                                                               
* PRINT THE STATION                                                             
*                                                                               
PRTLKSTA NTR1  LABEL=*                                                          
***      MVC   P(4),=C'==> '                                                    
         MVC   P+6(L'CURRMED),CURRMED                                           
         MVC   P+14(L'STAKCALL),STAKCALL                                        
         CLC   =C'000',STAKCLT     NON-CLIENT SPECIFIC?                         
         BE    *+10                YES                                          
         MVC   P+24(L'STAKCLT),STAKCLT                                          
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
BINPARMS DS    6F                                                               
CANADIAN DS    CL1                 FROM AGENCY RECORD, C'C' IF CANADA           
CURRMED  DS    XL1                 CURRENT BIN AGY/MED OR EBCDIC MED            
LOCKDATE DS    XL3                 LOCK DATE TO COMPARE BUY END DATE            
CHCKSTTN DS    XL3                 WHAT STATION TO CHECK IN BINSRCH             
EBDCSTTN DS    CL(L'STAKCALL)      EBCDIC STATION                               
CBLMRKT  DS    CL2                  FOR CABLE STATIONS IE: FOOD/TO              
LEBDCSTN DS    CL(L'EBDCSTTN)      LAST CHECKED STATION IN BINSRCH              
NUMLOCK  DS    PL6                 NUMBER ACTUALLY LOCKED ON RCWRITE            
AGYMSTRS DS    PL6                 # OF MASTER RECORDS FOR THIS AGY             
LCKMSTRS DS    PL6                 # OF MASTER RECORDS WE CAN LOCK              
*                                                                               
STATNTBL DS    (MAXSTATN)XL(L'EBDCSTTN) ROOM FOR 10000 STATIONS                 
MAXSTATN EQU   10000                                                            
*                                                                               
IO       DS    CL6000                                                           
*                                                                               
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
BUYHDRD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENSTA                                                       
* DSECT FOR PRINT LINE                                                          
         ORG   P                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPVK02 02/26/20'                                      
         END                                                                    
