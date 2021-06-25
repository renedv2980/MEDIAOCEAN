*          DATA SET DDTSARDINE AT LEVEL 018 AS OF 08/28/13                      
*PHASE T00A5DA                                                                  
*INCLUDE BINSR31                                                                
TSAR     TITLE '- TEMPSTR SAVE AND RETRIEVE MODULE'                             
TSAR     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TSAWRKL,**TSAR**,RA,RR=RE,CLEAR=YES                              
BUFFREG  EQU   R3,,,,GR32                                                       
         USING TSBUFFD,BUFFREG                                                  
         USING TSAWRK,RC           RC=A(W/S)                                    
         ST    RE,RELO                                                          
         LA    R2,0(R1)            R1 POINTS TO TSAR BLOCK                      
         USING TSARD,R2            R2=A(TSAR BLOCK)                             
         MVI   TSERRS,0            CLEAR ERROR FLAG                             
         A     RE,=V(BINSRCH)                                                   
         ST    RE,TBINSRCH                                                      
         SR    R1,R1                                                            
         ICM   R1,7,TSACOM+1                                                    
         USING COMFACSD,R1                                                      
         MVC   TDATAMGR,CDATAMGR                                                
         MVC   TCALLOV,CCALLOV                                                  
         MVC   TSWITCH,CSWITCH                                                  
         MVC   TPROTON,CPROTON                                                  
         MVC   TPROTOFF,CPROTOFF                                                
         SAM31 ,                   GET INTO 31-BIT MODE                         
         EJECT                                                                  
***********************************************************************         
* OFF-LINE HANDLING - BUFFERS ACQUIRED DYNAMICALLY - NO DISK SUPPORT  *         
***********************************************************************         
                                                                                
         CLC   TSWITCH,ZERO        SWITCH RESOLVED IF ON-LINE                   
         BNE   INIT02                                                           
         OC    CMASTC,CMASTC       MASTC MUST BE RESOLVED OFF-LINE              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         TM    TSRECI,TSRXTN       AND MUST HAVE EXTENSION DEFINED              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OI    TSFLAG,TSFLAGOL+TSFLAGND                                         
         LA    RE,RETURN                                                        
         ST    RE,TPROTON                                                       
         ST    RE,TPROTOFF                                                      
         ICM   BUFFREG,15,TSBUFFA  POINT TO BUFFER                              
         BNZ   TS1                                                              
         CLI   TSACTN,TSAFRE       NO BUFFER - ALLOW FREE                       
         BE    TSEXIT                                                           
         CLI   TSACTN,TSAINI       ELSE - MUST BE INIT                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,TSBUFFL        ACQUIRE STORAGE FIRST TIME                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MHI   R0,K                (TSBUFFL IS KBYTES)                          
         TM    TSRECI,TSRMGB       TEST TSBUFFL IS MBYTES                       
         BZ    *+8                                                              
         MHI   R0,K                YES - MULTIPLY AGAIN                         
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    BUFFREG,R1                                                       
         STCM  BUFFREG,15,TSBUFFA                                               
         B     TS1                                                              
                                                                                
***********************************************************************         
* ON-LINE HANDLING                                                    *         
***********************************************************************         
                                                                                
INIT02   GOTOR TPROTOFF            TURN STORAGE PROTECTION OFF                  
                                                                                
         OC    TEMPSTPG,TEMPSTPG   TEST HAVE SSB VALUES RESOLVED                
         BNZ   INIT06                                                           
         GOTOR TSWITCH,DMCB,X'FEFFFFFF',XA=OFF                                  
         L     R1,0(R1)            GET SYSFAC ADDRESS                           
         L     R1,VSSB-SYSFACD(R1)                                              
         USING SSBD,R1                                                          
         LLH   R0,SSBWSSVR         R0=LENGTH OF WSSVR BUFFER                    
         CHI   R0,8096             IF LESS THAN 8K THEN IT IS IN K              
         JL    INIT04                                                           
         AHI   R0,4095             CALCULATE K SIZE IF NOT                      
         SRL   R0,12                                                            
         SLL   R0,2                (SAME AS CODE IN FASTART)                    
INIT04   STH   R0,TSARWSVL         SET WSSVR SIZE IN K BYTES                    
         MVC   TEMPSTPG,SSBTWAL    SET PAGE LENGTH FOR TEMPSTR                  
         MVC   TEMPESPG,SSBTMSL    SET PAGE LENGTH FOR TEMPEST                  
         MVC   TSARBUFL,SSBTSAR    SET TSAR BUFFER LENGTH                       
         MVI   TSARBUFL,0          CLEAR HIGH ORDER BYTE OF LENGTH              
         TM    SSBTSAR,X'80'       TEST 2 TSAR BUFFERS AVAILABLE                
         BZ    INIT06                                                           
         MVI   TSARBUFN,2          SET 2 TSAR BUFFERS AVAILABLE                 
         DROP  R1                                                               
                                                                                
INIT06   GOTOR TSWITCH,DMCB,X'00FFFFFF'                                         
         L     R7,0(R1)                                                         
         USING TCBD,R7             R7=A(TCB)                                    
                                                                                
         TM    TSRECI,TSRTSPCL     TEST SPECIAL TSAR PROCESSING                 
         BZ    INIT20                                                           
                                                                                
***********************************************************************         
* SPECIAL TSAR PROCESSING - USES A VARIETY OF BUFFERS BUT DOES NOT    *         
* SUPPORT SAVING AND RESTORING THE BUFFER TO/FROM DISK                *         
***********************************************************************         
                                                                                
         TM    TSRECI,TSRXTN       MUST HAVE EXTENSION DEFINED                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    TSIND2,TSI2BUF2     AND CAN'T USE TWO BUFFERS                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    TSFLAG,TSFLAGND     SET SPECIAL PROCESSING FLAG                  
                                                                                
         TM    TSRECI,TSRWSSVR     TEST USING WSSVR BUFFER                      
         BZ    INIT08                                                           
         L     BUFFREG,TCBAXAWS    POINT TO WSSVR TASK BUFFER                   
         MVC   TSBUFFL,TSARWSVL    SET LENGTH (KBYTES)                          
         B     INIT14                                                           
                                                                                
INIT08   TM    TSRECI,TSRMINB1+TSRMINB2                                         
         BZ    INIT10                                                           
         L     BUFFREG,TCBMINIO                                                 
         TM    TSRECI,TSRMINB2     TEST USE MINIO BUFFER 2                      
         BZ    *+8                                                              
         L     BUFFREG,TCBMINI2                                                 
         L     R0,TCBMINI2                                                      
         SL    R0,TCBMINIO                                                      
         SRL   R0,10               R0=MINIO BUFFER LENGTH/1024                  
         STCM  R0,3,TSBUFFL        SET BUFFER LENGTH                            
         B     INIT14                                                           
                                                                                
INIT10   TM    TSRECI,TSRTSARB     TEST USING TSAR BUFFERS                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     BUFFREG,TCBTSAR     POINT TO TSAR BUFFER                         
         TM    TSRECI,TSRTSAB2     TEST WANT TO USE BUFFER 2                    
         BZ    INIT12                                                           
         CLI   TSARBUFN,1          TEST TWO BUFFERS AVAILABLE                   
         BH    *+6                                                              
         DC    H'0'                SECOND TSAR BUFFER NOT AVAILABLE             
         TM    TSRECI,TSRTSAB1     TEST WANT TO USE BOTH BUFFERS                
         BNZ   INIT12                                                           
         A     BUFFREG,TSARBUFL    NO - POINT TO SECOND BUFFER                  
                                                                                
INIT12   L     R0,TSARBUFL         GET BUFFER LENGTH (BYTES)                    
         SRL   R0,10               CONVERT TO KBYTES                            
         TM    TSRECI,TSRTSARB     TEST USING BOTH BUFFERS                      
         BNO   *+6                                                              
         AR    R0,R0               YES - DOUBLE BUFFER LENGTH                   
         STCM  R0,3,TSBUFFL                                                     
                                                                                
INIT14   STCM  BUFFREG,15,TSBUFFA  SET A(BUFFER) IN TSAR BLOCK                  
         CLI   TSACTN,TSAINI       NO RECORDS IF INITIALIZING                   
         BE    TS1                                                              
         SR    R0,R0                                                            
         ICM   R0,7,TSBRECS                                                     
         TM    TSIND2,TSI2MANY     TEST MORE THAN 64K RECORDS                   
         BNZ   INIT16                                                           
         OC    TSRNUM,TSRNUM       TEST USER SET RECORD NUMBER                  
         BNZ   TS1                                                              
         STCM  R0,3,TSRNUM         NO - SET NUMBER OF RECORDS IN BUFFER         
         B     TS1                                                              
                                                                                
INIT16   OC    TSRNUM3,TSRNUM3     TEST USER SET RECORD NUMBER                  
         BNZ   TS1                                                              
         STCM  R0,7,TSRNUM3        NO - SET NUMBER OF RECORDS IN BUFFER         
         B     TS1                                                              
                                                                                
***********************************************************************         
* REGULAR TSAR PROCESSING WITH SAVE/RESTORE TO TEMPSTR/TEMPEST        *         
***********************************************************************         
                                                                                
INIT20   MVC   TUTL,TCBUTL                                                      
         L     BUFFREG,TCBTSAR     GET BUFFER ADDRESS                           
                                                                                
         MVC   FILE,TEMPSTR        SET FILE NAME                                
         MVC   PAGESIZE,TEMPSTPG                                                
         TM    TSINDS,TSIALLOC     TEST TEMPEST                                 
         BZ    INIT22                                                           
         MVC   FILE,TEMPEST                                                     
         MVC   PAGESIZE,TEMPESPG                                                
                                                                                
INIT22   TM    TSIND2,TSI2BUF2     TEST USE TSAR BUFFER 2                       
         BZ    INIT24              NO                                           
         CLI   TSARBUFN,2          TEST 2 BUFFERS AVAILABLE                     
         BNL   *+6                                                              
         DC    H'0'                                                             
         A     BUFFREG,TSARBUFL    POINT TO SECOND BUFFER                       
                                                                                
INIT24   CLI   TSACTN,TSAINI       TEST INIT CALL                               
         BE    TS1                 YES                                          
         TM    TSIND2,TSI2SCND     TEST SECONDARY USER                          
         BZ    TS1                 NO                                           
         NI    TSPAGN,X'7F'        MAKE SURE CI BIT OFF                         
         CLI   TSPAGN,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TSPAGN,TSPEXPN-2    PRIMARY MUST GET AT LEAST 1 PAGE             
         BNH   *+6                                                              
         DC    H'0'                NUMBER OF PAGES EXCEEDS MAXIMUM              
         LLC   RE,TSPAGN           GET NUMBER OF PAGES                          
         LA    RE,1(RE)            ONE PAGE REQUIRED OVERHEAD                   
         MH    RE,PAGESIZE                                                      
         A     BUFFREG,TSARBUFL    POINT TO END OF BUFFER                       
         SR    BUFFREG,RE          POINT BUFFREG FOR SECONDARY TSAR             
         EJECT                                                                  
***********************************************************************         
* LOOK UP CALLER'S ACTION IN ACTION TABLE AND CALL ACTION ROUTINE     *         
***********************************************************************         
                                                                                
TS1      MVI   ADDFLAG,C'N'                                                     
         LA    RE,ACTNTAB                                                       
TS2      CLI   0(RE),0             TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RE),TSACTN      MATCH ACTION TO TABLE                        
         BE    *+12                                                             
         LA    RE,L'ACTNTAB(RE)                                                 
         B     TS2                                                              
         SR    RF,RF                                                            
         ICM   RF,7,1(RE)                                                       
         A     RF,RELO                                                          
                                                                                
         TM    TSFLAG,TSFLAGND     TEST SPECIAL PROCESSING                      
         BNZ   TS3                                                              
         CLC   FILE,TEMPEST        TEST TEMPEST STORAGE                         
         BNE   TS3                                                              
         CLI   TSACTN,TSAINI       FOR ALL ACTIONS (OTHER THAN INIT AND         
         BE    TS3                 RESTORE) CHECK TO SEE IF THE CURRENT         
         CLI   TSACTN,TSARES       TERMINAL OWNS THE BUFFER AND KILL            
         BE    TS3                 THE TRANSACTION IF IT DOESN'T                
                                                                                
         L     RE,TUTL                                                          
         TM    TSTAT1-UTLD(RE),TSTATDDS                                         
         BNZ   TS3                                                              
         CLC   TSBTERM,TNUM-UTLD(RE)                                            
         BE    TS3                                                              
                                                                                
         GOTOR TSWITCH,DMCB,X'FEFFFFFF',XA=OFF                                  
                                                                                
         L     R1,0(R1)            GET SYSFAC ADDRESS                           
         L     R1,VUTL-SYSFACD(R1)                                              
         USING UTLD,R1                                                          
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,TSBTERM                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         BCTR  RF,0                                                             
         MH    RF,0(R1)                                                         
         LA    RE,6(RF,R1)         RE = A(OFFENDING UTL)                        
                                                                                
         L     R0,8(RD)                                                         
         LH    RF,0(R1)                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE               COPY BAD UTL LOCALLY                         
                                                                                
         L     RF,8(RD)            RF = A(BAD UTL)                              
                                                                                
         L     RE,TUTL                                                          
         ICM   R0,B'1100',TSBTERM  GET TERMINAL NUMBERS IN R0                   
         ICM   R0,B'0011',TNUM-UTLD(RE)                                         
         DC    H'0'                                                             
                                                                                
TS3      BASR  RE,RF               GO TO ACTION ROUTINE                         
                                                                                
         CLI   TSACTN,TSAFRE       TEST JUST ISSUED FREEMAIN                    
         JE    TSPROTX             (BUFFER IS NO LONGER AVAILABLE)              
                                                                                
         TM    TSIND2,TSI2DATA     TEST USER WANTS BUFFER DATA RETURNED         
         BZ    TS4                                                              
         SR    RF,RF                                                            
         ICM   RF,7,TSABUF+1                                                    
         MVC   0(TSBKEYS-TSBUFFD,RF),0(BUFFREG)                                 
                                                                                
TS4      SR    R0,R0               SET NUMBER OF RECORDS IN BUFFER              
         ICM   R0,7,TSBRECS                                                     
         BZ    *+10                                                             
         BCTR  R0,0                                                             
         STH   R0,TSPRECN                                                       
                                                                                
         NI    TSINDS,X'FF'-TSIANYAD                                            
         LTR   R0,R0                                                            
         BZ    TSPROTX                                                          
         OI    TSINDS,TSIANYAD     SET RECORD ADDED FLAG IF ANY RECORDS         
                                                                                
TSPROTX  GOTOR TPROTON             FOR NON-NTR1 ROUTINE EXITS                   
                                                                                
TSEXIT   CLI   TSERRS,0            SET CONDITION CODE FOR CALLER                
         L     RD,4(RD)                                                         
         LM    RE,RC,12(RD)                                                     
         BSM   0,RE                RETURN IN CORRECT ADDRESSING MODE            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALISE TSARD                                         *         
***********************************************************************         
                                                                                
TSINI    NTR1  ,                                                                
         TM    TSFLAG,TSFLAGND     TEST SPECIAL PROCESSING                      
         BNZ   TSINI10             YES                                          
                                                                                
         CLI   TSPAGL,0                                                         
         BNE   *+8                                                              
         MVI   TSPAGL,1            SET LOW PAGE NUMBER IF NOT PASSED            
         TM    TSINDS,TSINODSK     TEST NO DATA TO DISK                         
         BZ    *+12                                                             
         MVI   TSPAGL,0            FORCE LOW PAGE TO 0                          
         NI    TSPAGN,X'7F'        TURN CI BIT OFF IF NO DISK                   
                                                                                
         TM    TSPAGN,X'7F'                                                     
         BNZ   *+8                                                              
         OI    TSPAGN,1            SET 1 PAGE IF USER OMITTED                   
                                                                                
         TM    TSPAGN,TSPAGNCI     TEST ALLOCATING C/I'S                        
         BNZ   TSINI02                                                          
         CLI   TSPAGN,TSPEXPN                                                   
         BNH   TSINI02                                                          
         TM    TSIND2,TSI2INIF     RETURN ERROR FOR INIT?                       
         BO    *+6                                                              
         DC    H'0'                NUMBER OF PAGES EXCEEDS MAXIMUM              
                                                                                
         OI    TSERRS,TSEINIF      SET RETURN FLAG                              
         B     TSEXIT                                                           
                                                                                
TSINI02  TM    TSINDS,TSINODSK                                                  
         BO    TSINI06                                                          
         TM    TSINDS,TSIALLOC     TEST ALLOC FROM TEMPEST                      
         BZ    TSINI06                                                          
         TM    TSINDS,TSIREUSE     TEST RE-USE ALREADY ALLOCATED C/I            
         BNZ   TSINI06                                                          
         MVC   FLAG,TSPAGN                                                      
         NI    FLAG,255-TSPAGNCI                                                
         SR    R0,R0                                                            
         ICM   R0,8,FLAG           R0=NUMBER OF PAGES OR C/I'S                  
         TM    TSPAGN,TSPAGNCI     TEST C/I'S REQUESTED                         
         BNZ   TSINI04                                                          
         ICM   R0,4,=C'P'          NO - RESERVE PAGES                           
         TM    TSINDS,TSIXTTWA     TEST REQ FOR BIG PAGES                       
         BO    TSINI04             YES                                          
* REQUEST IS FOR SMALL PAGES - CONVERT TO BIG ONES                              
         OI    TSINDS,TSIXTTWA     SET BIT FOR USER                             
         SR    RE,RE                                                            
         LLC   RF,FLAG             GET NUMBER OF PAGES                          
         MHI   RF,TSPAGEL          X SMALL PAGE LEN                             
         AR    RF,RF               X 2                                          
         LH    R1,TEMPESPG         DIVIDE BY BIG PAGE SIZE                      
         DR    RE,R1                                                            
         LA    RF,1(RF)            ROUND                                        
         SRL   RF,1                GIVES NUMBER OF BIG PAGES                    
         STC   RF,TSPAGN           SET FOR THE LITTLE USER                      
         LTR   RF,RF                                                            
         BP    *+8                                                              
         MVI   TSPAGN,1                                                         
         ICM   R0,8,TSPAGN         SET R0 FOR DMGR CALL                         
                                                                                
TSINI04  GOTOR TDATAMGR,DMCB,DMRSRV,TEMPEST,(R0),XA=OFF                         
         BE    *+12                                                             
         MVI   TSERRS,TSEEOF       SET E-O-F ON ALLOCATE FAILURE                
         B     TSEXIT                                                           
                                                                                
         LH    R0,10(R1)           GET NUMBER OF OF PAGES PER TRACK             
         MH    R0,12(R1)           MULTIPLY BY PREVIOUS C/I'S ALLOCATED         
         AHI   R0,1                PLUS ONE                                     
         STC   R0,TSPAGL           =LOW PAGE NUMBER IN THIS ALLOCATION          
         TM    TSPAGN,TSPAGNCI     TEST PAGES REQUESTED                         
         BZ    TSINI06                                                          
         LH    R0,10(R1)           GET NUMBER OF PAGES PER TRACK                
         MH    R0,08(R1)           MULTIPLY BY C/I'S ALLOCATED                  
         STC   R0,TSPAGN           =NUMBER OF PAGES ALLOCATED                   
                                                                                
TSINI06  TM    TSIND2,TSI2SCND     TEST SECONDARY USER                          
         BZ    TSINI10             NO                                           
         LLC   RE,TSPAGN           GET NUMBER OF PAGES                          
         NILL  GRE,X'007F'         DROP CI FLAG                                 
         LA    RE,1(RE)            ONE PAGE OVERHEAD                            
         MH    RE,PAGESIZE         X PAGE LENGTH                                
         C     RE,TSARBUFL         COMPARE TO MAX BUFFER LENGTH                 
         BNH   TSINI08                                                          
         TM    TSIND2,TSI2INIF     RETURN ERROR FOR INIT?                       
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    TSERRS,TSEINIF      SET RETURN FLAG                              
         B     TSEXIT                                                           
                                                                                
TSINI08  A     BUFFREG,TSARBUFL    POINT TO END OF BUFFER                       
         SR    BUFFREG,RE          POINT BUFFREG FOR SECONDARY TSAR             
                                                                                
TSINI10  LA    R1,TSAREYE1         SET BUFFER EYECATCHERS                       
         TM    TSIND2,TSI2BUF2                                                  
         BZ    *+8                                                              
         LA    R1,TSAREYE2                                                      
         MVC   TSBLABEL(L'TSAREYE1),0(R1)                                       
         MVC   TSBLABEL+L'TSAREYE1(L'TSBLABEL-L'TSAREYE1),TSBLABEL              
         XC    TSBPGTAB(TSBXCLEN),TSBPGTAB                                      
         MVC   TSBKKK1,TSARKLNS                                                 
         MVC   TSBKKK2,TSARRECS                                                 
         MVC   TSBRECI,TSRECI      SAVE RECI                                    
         MVC   TSBPAGN,TSPAGN      SAVE NUMBER OF ALLOCATED PAGES               
         L     RF,TUTL                                                          
         MVC   TSBTERM,TNUM-UTLD(RF)                                            
                                                                                
         SR    R0,R0                                                            
         ICM   R0,1,TSKEYL                                                      
         BNZ   TSINI12                                                          
         TM    TSIND2,TSI2INIF     RETURN ERROR FOR INIT?                       
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    TSERRS,TSEINIF      SET RETURN FLAG                              
         B     TSEXIT                                                           
                                                                                
TSINI12  STH   R0,TSBKEYL                                                       
         BCTR  R0,0                                                             
         STH   R0,TSBKEYL1                                                      
                                                                                
         LH    R0,TSBKEYL                                                       
         AHI   R0,L'TSBDPTR                                                     
         STH   R0,TSBKEYL3         SAVE KEYLEN + L'DATA POINTER                 
                                                                                
         OC    TSBRECL,TSRECL                                                   
         BZ    TSINI14                                                          
         CLC   TSBRECL,=H'4096'                                                 
         BNH   TSINI16             RECORD LENGTH TOO LONG                       
                                                                                
TSINI14  TM    TSIND2,TSI2INIF     RETURN ERROR FOR INIT?                       
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    TSERRS,TSEINIF      SET RETURN FLAG                              
         B     TSEXIT                                                           
                                                                                
TSINI16  TM    TSFLAG,TSFLAGND     TEST SPECIAL PROCESSING                      
         BZ    TSINI18                                                          
         LLH   R1,TSBUFFL                                                       
         MHI   R1,K                (TSBUFFL IS KBYTES)                          
         TM    TSRECI,TSRMGB       TEST TSBUFFL IS MBYTES                       
         BZ    *+8                                                              
         MHI   R1,K                YES - MULTIPLY AGAIN                         
         B     TSINI20                                                          
                                                                                
TSINI18  LLC   R1,TSPAGN           GET NUMBER OF PAGES REQUESTED                
         LHI   R0,MAXPAGES                                                      
                                                                                
         TM    TSIND2,TSI2BIGN     TEST USING BOTH BUFFERS                      
         BNZ   *+12                                                             
         TM    TSINDS,TSINODSK     TEST NO DATA TO DISK                         
         BZ    *+6                                                              
         AR    R0,R0               DOUBLE MAX IF NO DISK/USING BOTH             
                                                                                
         CR    R1,R0               ACTUAL PAGES TO MAX PAGES                    
         BNH   *+10                                                             
         LR    R1,R0                                                            
         STC   R1,TSPAGN           REDUCE REQUESTED PAGES TO MAX                
                                                                                
         MH    R1,PAGESIZE         PAGES X PAGE LENGTH                          
                                                                                
         L     R0,TSARBUFL                                                      
         TM    TSINDS,TSINODSK     TEST NO DATA TO DISK                         
         BO    *+6                                                              
         AR    R0,R0               DOUBLE MAX LEN IF NO DISK                    
                                                                                
         CR    R1,R0                                                            
         BNH   TSINI20                                                          
         LR    R1,R0                                                            
                                                                                
TSINI20  ST    R1,TSBMAX           SET MAX BYTES                                
         ST    R1,TSBNXDTA         SET DSPL TO NEXT DATA RECORD END             
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PAGE TABLE WITH DISPLACEMENTS TO START OF PAGE           *         
***********************************************************************         
                                                                                
         TM    TSFLAG,TSFLAGND     TEST SPECIAL PROCESSING                      
         BNZ   TSINI26                                                          
         SR    R0,R0                                                            
         ICM   R0,1,TSPAGN         GET NUMBER OF PAGES                          
         BZ    TSINI22                                                          
         SR    R1,R1                                                            
         LA    R4,TSBPGTAB                                                      
         B     TSINI24                                                          
                                                                                
TSINI22  TM    TSIND2,TSI2INIF     RETURN ERROR FOR INIT?                       
         BO    *+6                                                              
         DC    H'0'                NUMBER OF PAGES EXCEEDS MAXIMUM              
         OI    TSERRS,TSEINIF      SET RETURN FLAG                              
         B     TSEXIT                                                           
                                                                                
TSINI24  STCM  R1,7,0(R4)                                                       
         AH    R1,PAGESIZE                                                      
         LA    R4,4(R4)                                                         
         BCT   R0,TSINI24                                                       
*                                  BUILD A HIGH KEY IN THE KEY AREA             
TSINI26  XC    TSBKEYS(L'TSBDPTR),TSBKEYS  CLEAR DSPL FIELD                     
         MVI   TSBKEYS+L'TSBDPTR,X'FF'                                          
         LH    RE,TSBKEYL                                                       
         SHI   RE,2                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TSBKEYS+L'TSBDPTR+1(0),TSBKEYS+L'TSBDPTR  *EXECUTED*             
                                                                                
         LA    RE,5(RE)                GET KEY BYTES USED                       
         LA    RE,TSBKEYS-TSBUFFD(RE)  ADD BUFFER OVERHEAD                      
         ST    RE,TSBACT               SET ACTUAL COUNT                         
         ST    RE,TSBNXKEY             SET DSPL FOR NEXT KEY                    
                                                                                
         LA    R0,1                SET 1 RECORD PRESENT                         
         STCM  R0,7,TSBRECS                                                     
                                                                                
         TM    TSFLAG,TSFLAGND     TEST SPECIAL PROCESSING                      
         BNZ   TSINI28                                                          
         SR    R5,R5                                                            
         SR    R6,R6                                                            
         ICM   R6,1,TSPAGL         GET STARTING PAGE NUMBER                     
         BZ    TSINI28             MAY BE ZERO IF NO DISK INVOLVED              
         GOTOR TSDMGR,DMWRT        ALWAYS WRITE PAGE 1                          
                                                                                
TSINI28  OI    TSINDS,TSIINIOK                                                  
                                                                                
TSINIX   B     TSEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A RECORD BY KEY                                      *         
***********************************************************************         
                                                                                
TSADD    NTR1  ,                                                                
         SR    RE,RE                                                            
         ICM   RE,7,TSAREC+1       RE=A(RECORD TO BE ADDED)                     
         BNZ   *+6                                                              
         DC    H'0'                RECORD ADDRESS NOT PASSED                    
         ST    RE,AREC             SAVE A(RECORD TO BE ADDED)                   
                                                                                
         LH    R1,TSBRECL                                                       
         TM    TSBRECI,TSRVAR      TEST VARIABLE LENGTH RECORDS                 
         BZ    *+8                                                              
         ICM   R1,3,0(RE)                                                       
         STH   R1,RECL             SAVE RECORD LENGTH                           
         CLC   RECL,TSBRECL        COMPARE ACTUAL LEN TO MAX                    
         BNH   *+6                                                              
         DC    H'0'                                                             
                                                                                
         A     R1,TSBACT                                                        
         LA    R1,L'TSBDPTR(R1)    ADD DATA POINTER/KEY                         
         C     R1,TSBMAX                                                        
         BL    *+12                                                             
         MVI   TSERRS,TSEEOF       YES - RECORD WILL NOT FIT                    
         B     TSEXIT                                                           
                                                                                
         MVI   ADDFLAG,C'Y'                                                     
         GOTOR TSRDH               READ HIGH FOR KEY                            
         BNE   *+12                                                             
         MVI   TSERRS,TSEDUP       IF FOUND SET DUPLICATE KEY                   
         B     TSEXIT                                                           
                                                                                
         TM    TSERRS,TSEEOF       DO NOT ALLOW ADD OF X'FF' RECORD             
         BO    TSEXIT                                                           
                                                                                
         MVI   TSERRS,0            RESET ERR IND                                
         ST    R1,TSBACT           SAVE NEW ACTUAL BYTE COUNT                   
         OI    TSBSTAT,TSBQACTV    SET BUFFER ACTIVE                            
                                                                                
         L     R4,KEYADDR          GET ADDR OF ADDED KEY                        
         LH    R0,RECL                                                          
         SH    R0,TSBKEYL          LESS KEY LENGTH                              
         L     R1,TSBNXDTA         GET DATA DSPL                                
         SR    R1,R0               GIVES MOVE 'TO' DSPL                         
         STCM  R1,15,0(R4)         SET DSPL IN KEY TABLE                        
         ST    R1,TSBNXDTA         AND SET NEW DATA DSPL                        
                                                                                
         L     R0,TSBNXKEY         UPDATE NEXT KEY DSPL                         
         AH    R0,TSBKEYL3                                                      
         ST    R0,TSBNXKEY                                                      
*                                  NOW SET UP TO MOVE RECORD TO BUFFER          
         AR    R1,BUFFREG          POINT TO 'TO' DATA ADDRESS                   
         L     RE,AREC             POINT TO USER RECORD                         
         TM    TSBRECI,TSRVAR                                                   
         BZ    TSADD10                                                          
                                                                                
***********************************************************************         
* VARIABLE LENGTH RECORDS                                             *         
***********************************************************************         
                                                                                
         MVC   0(2,R1),0(RE)       MOVE RECORD LENGTH TO DATA AREA              
         LA    R0,2(R1)            SET 'TO' ADDRESS FOR RECORD DATA             
         LH    R1,RECL             RECLEN                                       
         SH    R1,TSBKEYL          LESS KEY LEN                                 
         SHI   R1,2                LESS 2 FOR RECLEN ALREADY MOVED              
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RE,2(RE)            POINT PAST RECLEN                            
         AH    RE,TSBKEYL          AND KEY                                      
         LR    RF,R1               SET 'FROM' LEN = 'TO' LEN                    
         MVCL  R0,RE                                                            
         B     TSADDX                                                           
                                                                                
***********************************************************************         
* FIXED LENGTH RECORDS                                                *         
***********************************************************************         
                                                                                
TSADD10  LR    R0,R1               SET 'TO' ADDRESS FOR RECORD DATA             
         LH    R1,TSBRECL                                                       
         SH    R1,TSBKEYL                                                       
         BNM   *+6                                                              
         DC    H'0'                                                             
         AH    RE,TSBKEYL          POINT PAST KEY                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
TSADDX   SR    RE,RE                                                            
         ICM   RE,7,TSBRECS                                                     
         AHI   RE,1                                                             
         STCM  RE,7,TSBRECS                                                     
         B     TSEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT A RECORD BY NUMBER                                   *         
***********************************************************************         
                                                                                
TSPUT    TM    TSIND2,TSI2MANY     TEST MORE THAN 64K RECORDS                   
         BNZ   TSPUT2                                                           
         OC    TSRNUM,TSRNUM       TEST CALLER SET RECORD NUMBER                
         BNZ   TSPUT4                                                           
         DC    H'0'                                                             
                                                                                
TSPUT2   OC    TSRNUM3,TSRNUM3     TEST CALLER SET RECORD NUMBER                
         BNZ   TSPUT4                                                           
         DC    H'0'                                                             
                                                                                
TSPUT4   GOTOR TSGET               GET RECORD                                   
         BNE   TSPUTX                                                           
         GOTOR TSUPD               UPDATE RECORD IF FOUND                       
                                                                                
TSPUTX   B     TSPROTX                                                          
                                                                                
***********************************************************************         
* ROUTINE TO PUT A RECORD BY KEY                                      *         
***********************************************************************         
                                                                                
TSWRT    MVI   ADDFLAG,C'N'                                                     
         GOTOR TSRDH               READ HIGH FOR KEY                            
         BNE   TSWRTX                                                           
         GOTOR TSUPD               UPDATE RECORD IF FOUND                       
TSWRTX   B     TSPROTX                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE A RECORD                                          *         
***********************************************************************         
                                                                                
TSUPD    NTR1  ,                                                                
         MVI   FLAG,0              RESET FLAG                                   
         OI    TSBSTAT,TSBQACTV                                                 
         SR    RE,RE                                                            
         ICM   RE,7,TSAREC+1       RE=A(NEW RECORD)                             
         L     RF,KEYADDR          RF=A(OLD KEY)                                
         TM    TSBRECI,TSRVAR      TEST VARIABLE LENGTH RECORDS                 
         BZ    *+8                                                              
         LA    RE,2(RE)            POINT TO KEY (AFTER LENGTH)                  
         LH    R1,TSBKEYL1                                                      
         EX    R1,*+8              MATCH OLD/NEW KEY VALUES                     
         B     *+10                                                             
         CLC   L'TSBDPTR(0,RF),0(RE)                                            
         BE    TSUPD6                                                           
         TM    TSINDS,TSIKEYUP     TEST USER KNOWS HE IS CHANGING               
         BO    TSUPD2                                                           
         MVI   TSERRS,TSERNF       IF KEY CHANGED SET RECORD NOT FOUND          
         B     TSUPDX                                                           
                                                                                
TSUPD2   MVI   FLAG,C'K'           SET FLAG THAT KEY CHANGED                    
         EX    R1,*+8                                                           
         B     TSUPD6                                                           
         MVC   L'TSBDPTR(0,RF),0(RE)   *UPDATE KEY*                             
                                                                                
TSUPD6   TM    TSBRECI,TSRVAR      TEST VARIABLE LENGTH RECORDS                 
         BO    TSUPD10                                                          
                                                                                
***********************************************************************         
* FIXED LENGTH RECORDS - MOVE NEW OVER OLD                            *         
***********************************************************************         
                                                                                
         ICM   R0,15,0(RF)         GET DSPL TO DATA                             
         AR    R0,BUFFREG          POINT TO DATA                                
         LH    R1,TSBRECL          GET RECORD LENGTH                            
         SH    R1,TSBKEYL          LESS KEY LENGTH                              
         BNM   *+6                                                              
         DC    H'0'                                                             
         AH    RE,TSBKEYL          POINT TO NEW DATA                            
         LR    RF,R1               'FROM' LEN = 'TO' LEN                        
         MVCL  R0,RE               MOVE RECORD                                  
         B     TSUPDX                                                           
         EJECT                                                                  
***********************************************************************         
* VARIABLE LENGTH RECORDS - TEST LENGTH CHANGED                       *         
***********************************************************************         
                                                                                
TSUPD10  L     RE,KEYADDR                                                       
         ICM   RF,15,0(RE)                                                      
         AR    RF,BUFFREG          RF--> OLD RECORD                             
         SR    RE,RE                                                            
         ICM   RE,7,TSAREC+1       RE--> NEW RECORD                             
         CLC   0(2,RE),0(RF)       TEST SAME LENGTH                             
         BE    TSUPD12                                                          
         CLI   FLAG,C'K'           TEST KEY CHANGED                             
         BNE   TSUPD20             NO                                           
         DC    H'0'                BUFF NOT IN SEQ/CANNOT CHANGE LEN            
                                                                                
***********************************************************************         
* LENGTH NOT CHANGED - MOVE NEW DATA OVER OLD                         *         
***********************************************************************         
                                                                                
TSUPD12  LA    R0,2(RF)            R0--> OLD DATA                               
         LLH   R1,0(RF)            GET RECORD LENGTH                            
         SH    R1,TSBKEYL          LESS KEY LENGTH                              
         SHI   R1,2                LESS RECLEN OVERHEAD                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RE,2(RE)            RE--> NEW KEY                                
         AH    RE,TSBKEYL          RE--> NEW DATA                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     TSUPDX                                                           
                                                                                
***********************************************************************         
* LENGTH CHANGED - DELETE RECORD AND ADD                              *         
***********************************************************************         
                                                                                
TSUPD20  GOTOR TSDEL               DELETE THIS RECORD                           
         GOTOR TSADD               ADD RECORD WITH NEW LENGTH                   
TSUPDX   B     TSEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A RECORD BY NUMBER (IF RECORD NUMBER SET) OR BY   *         
* KEY (IF NOT)                                                        *         
***********************************************************************         
                                                                                
TSDEL    NTR1  ,                                                                
         OI    TSBSTAT,TSBQACTV                                                 
         ICM   R4,15,KEYADDR       TEST GET PREVIOUSLY DONE                     
         BNZ   TSDEL6                                                           
         LA    RF,TSGET                                                         
         TM    TSIND2,TSI2MANY     TEST MORE THAN 64K RECORDS                   
         BNZ   *+14                                                             
         OC    TSRNUM,TSRNUM       TEST RECORD NUMBER GIVEN                     
         B     *+10                                                             
         OC    TSRNUM3,TSRNUM3     TEST RECORD NUMBER GIVEN                     
         BNZ   TSDEL4                                                           
         LA    RF,TSRDH            NO - DO READ FOR KEY                         
                                                                                
TSDEL4   MVI   ADDFLAG,C'N'                                                     
         BASR  RE,RF               GET RECORD ADDRESS                           
         BNE   TSDELX              EXIT IF NOT FOUND                            
                                                                                
TSDEL6   L     R4,KEYADDR                                                       
         L     R0,TSBNXDTA         GET NEXT DATA DSPL                           
         AR    R0,BUFFREG          POINT TO IT                                  
         ST    R0,SAVE             SAVE START OF DATA ADDR                      
                                                                                
         ICM   R0,15,0(R4)         GET DSPL TO DATA                             
         ST    R0,RECDSPL                                                       
         AR    R0,BUFFREG          POINT TO RECORD BEING DELETED                
         LR    R4,R0                                                            
                                                                                
         LH    R5,TSBRECL                                                       
         TM    TSBRECI,TSRVAR                                                   
         BZ    *+8                                                              
         ICM   R5,3,0(R4)          GET RECORD LENGTH                            
         STH   R5,RECL             SAVE RECORD LENGTH                           
         SH    R5,TSBKEYL          LESS KEY LEN GIVES DELETE LEN                
         BNM   *+6                                                              
         DC    H'0'                                                             
         C     R4,SAVE             TEST RECORD IS AT START OF BUFFER            
         BE    TSDEL10             YES - NO MOVE REQUIRED                       
                                                                                
TSDEL8   LR    R0,R4               'TO' ADDR = ADDR OF DELETED RECORD           
         LR    R1,R5               'TO' LEN  = DELETE LENGTH                    
         LR    RE,R4               'FROM' ADDR = START                          
         SR    RE,R5                  - DELETE LENGTH                           
         LR    RF,R5               'FROM' LEN = 'TO' LEN                        
         MVCL  R0,RE                                                            
         C     R4,SAVE             TEST REACHED START OF DATA YET               
         BE    TSDEL10                                                          
                                                                                
         SR    R4,R5               BACK UP FOR NEXT                             
         C     R4,SAVE             TEST BEFORE START OF DATA                    
         BNL   TSDEL8                                                           
                                                                                
         L     R0,SAVE             SET TO MOVE FROM START OF DATA               
         SR    R0,R4                                                            
         AR    R4,R0               ADVANCE START POINTER                        
         SR    R5,R0               DECREASE MOVE LENGTH                         
         BNM   TSDEL8                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* NEED TO ADJUST DISPLACEMENTS IN KEY AREA FOR RECORDS THAT WERE      *         
* MOVED (DSPL LESS THAN THAT OF THE RECORD BEING DELETED)             *         
***********************************************************************         
                                                                                
TSDEL10  LA    R4,TSBKEYS                                                       
         LH    R5,RECL                                                          
         SH    R5,TSBKEYL          THIS IS LENGTH OF RECORD REMOVED             
         SR    R0,R0                                                            
                                                                                
TSDEL12  ICM   R0,15,0(R4)         TEST REACHED HIGH KEY                        
         BZ    TSDEL60                                                          
         C     R0,RECDSPL                                                       
         BH    TSDEL14                                                          
         AR    R0,R5                                                            
         STCM  R0,15,0(R4)                                                      
TSDEL14  AH    R4,TSBKEYL3         POINT TO NEXT KEY                            
         B     TSDEL12                                                          
                                                                                
TSDEL60  L     R0,KEYADDR          'TO' ADDR = ADDRESS OF DELETED KEY           
         L     R1,TSBNXKEY         GET LENGTH OF KEY AREA                       
         AR    R1,BUFFREG          POINT TO END OF KEY AREA                     
         SR    R1,R0               SUBTRACT ADDR OF DELETED KEY                 
         SH    R1,TSBKEYL3         LESS KEYLEN GIVES LEN TO MOVE                
         BZ    TSDEL62                                                          
         BNM   *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         AH    RE,TSBKEYL3         ADD KEYLEN TO GET 'FROM' ADDR                
         LR    RF,R1               'FROM' LEN = 'TO' LEN                        
         MVCL  R0,RE                                                            
                                                                                
TSDEL62  L     R0,TSBNXKEY         UPDATE NEXT KEY DSPL                         
         SH    R0,TSBKEYL3                                                      
         ST    R0,TSBNXKEY                                                      
                                                                                
         L     R0,TSBNXDTA         UPDATE NEXT DATA DSPL                        
         AH    R0,RECL             RECOVER RECORD LENGTH                        
         SH    R0,TSBKEYL          LESS KEY LENGTH                              
         ST    R0,TSBNXDTA                                                      
                                                                                
         L     R0,TSBACT           UPDATE ACTUAL BYTE COUNT                     
         SH    R0,RECL                                                          
         SHI   R0,L'TSBDPTR                                                     
         ST    R0,TSBACT                                                        
                                                                                
         SR    R0,R0                                                            
         ICM   R0,7,TSBRECS                                                     
         BCTR  R0,0                                                             
         STCM  R0,7,TSBRECS                                                     
                                                                                
TSDELX   B     TSEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A RECORD BY NUMBER OR NEXT RECORD BY NUMBER          *         
***********************************************************************         
                                                                                
TSNXT    SR    RF,RF                                                            
         TM    TSIND2,TSI2MANY     TEST MORE THAN 64K RECORDS                   
         BNZ   TSNXT2                                                           
         ICM   RF,3,TSRNUM         UPDATE SHORT RECORD NUMBER                   
         AHI   RF,1                                                             
         STCM  RF,3,TSRNUM                                                      
         B     TSGET                                                            
                                                                                
TSNXT2   ICM   RF,7,TSRNUM3        UPDATE LONG RECORD NUMBER                    
         AHI   RF,1                                                             
         STCM  RF,7,TSRNUM3                                                     
                                                                                
TSGET    NTR1  ,                                                                
         SR    R1,R1                                                            
         TM    TSIND2,TSI2MANY     TEST MORE THAN 64K RECORDS                   
         BNZ   *+12                                                             
         ICM   R1,3,TSRNUM                                                      
         B     *+8                                                              
         ICM   R1,7,TSRNUM3                                                     
         CLM   R1,7,TSBRECS        TEST GREATER THAN ACTUAL                     
         BL    *+12                                                             
         OI    TSERRS,TSEEOF+TSERNF  SET EOF + NOT FOUND                        
         B     TSEXIT                                                           
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         MH    R1,TSBKEYL3         X KEYLEN+3 GIVES DSPL TO KEY                 
         LA    R4,TSBKEYS(R1)      POINT TO THE KEY                             
         ST    R4,KEYADDR                                                       
         B     TSRDH22                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ HIGH FOR A KEY - IF KEY IS NOT FOUND TSERNF IS SET  *         
***********************************************************************         
                                                                                
TSRDH    NTR1  ,                                                                
         MVI   TSERRS,0                                                         
         SR    R1,R1                                                            
         ICM   R1,7,TSAREC+1       GET USER REC ADDRESS                         
         TM    TSBRECI,TSRVAR                                                   
         BZ    *+8                                                              
         LA    R1,2(R1)            POINT TO USER KEY                            
         ST    R1,SAVEKEY                                                       
                                                                                
         LR    RE,R1                                                            
         SHI   RE,L'TSBDPTR        NEED TO SET USER KEY ADDRESS-3               
         ST    RE,DM1                                                           
         LA    RE,TSBKEYS                                                       
         ST    RE,DM2                                                           
         SR    RE,RE                                                            
         ICM   RE,7,TSBRECS                                                     
         ST    RE,DM3                                                           
         LH    RE,TSBKEYL3         RECORD LENGTH = KEYLEN+3                     
         ST    RE,DM4                                                           
         MVI   DM4,X'02'           SET FOR READ HIGH                            
         CLI   ADDFLAG,C'Y'        IS THIS RDHI FOR AN ADD                      
         BNE   *+8                 NO                                           
         MVI   DM4,X'01'           SET INSERT IF NOT FOUND                      
         SR    RE,RE                                                            
         ICM   RE,8,=AL1(L'TSBDPTR) SET DISPLACEMENT OF KEY IN HOB              
         ICM   RE,3,TSBKEYL        AND KEYLEN IN LOW BITS                       
         ST    RE,DM5                                                           
         MVC   DM6,=X'00FFFFFF'    SET LARGE MAX COUNT                          
                                                                                
         GOTOR TBINSRCH,DMCB                                                    
         L     RE,DM1              POINT TO WHATEVER HAS BEEN FOUND             
         LA    R1,0(RE)            CLEAR HOB                                    
         ST    R1,KEYADDR          AND SAVE ITS ADDRESS                         
         EJECT                                                                  
***********************************************************************         
* NEED TO COMPUTE RECORD NUMBER                                       *         
***********************************************************************         
                                                                                
TSRDH12  LA    R0,TSBKEYS          POINT TO START OF DATA                       
         SR    R1,R0               GIVES DSPL TO KEY                            
         SR    R0,R0               CLEAR FOR DIVIDE                             
         LH    RE,TSBKEYL3         GET KEYLEN+3                                 
         DR    R0,RE                                                            
         LTR   R0,R0                                                            
         BZ    *+6                 CANNOT POSSIBLY HAVE REMAINDER               
         DC    H'0'                                                             
         SR    RF,RF                                                            
         ICM   RF,7,TSBRECS                                                     
         CR    R1,RF                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         AHI   R1,1                LOW RECORD NUMBER IS 1                       
         TM    TSIND2,TSI2MANY     TEST MORE THAN 64K RECORDS                   
         BNZ   *+12                                                             
         STCM  R1,3,TSRNUM         SET SHORT RECORD NUMBER                      
         B     *+8                                                              
         STCM  R1,7,TSRNUM3        SET LONG RECORD NUMBER                       
                                                                                
         TM    DM1,X'80'           TEST RECORD NOT FOUND                        
         BZ    TSRDH18                                                          
                                                                                
         OI    TSERRS,TSERNF       SET RECORD NOT FOUND                         
         CLI   TSACTN,TSARDH       NB -- ALWAYS RETURN RECORD IF                
         BNE   TSRDHX                 RDHI FROM CALLER                          
                                                                                
TSRDH18  L     R1,KEYADDR                                                       
         OC    0(L'TSBDPTR,R1),0(R1)  TEST VALID DATA DSPL                      
         BNZ   TSRDH20                                                          
         OI    TSERRS,TSEEOF+TSERNF  SET EOF + NOT FOUND                        
         B     TSRDHX                HOPEFULLY THAT'S ENOUGH                    
         EJECT                                                                  
TSRDH20  CLC   DM3+1(3),TSBRECS    RECORD COUNT SHOULD NOT CHANGE               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TSACTN,TSARDH       TEST READ HIGH                               
         BNE   TSRDH22             NO                                           
         LH    R1,TSBKEYL1                                                      
         L     RE,SAVEKEY          GET ADDR OF KEYARG                           
         L     RF,KEYADDR          RECORD BACK FROM BINSRCH                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),L'TSBDPTR(RF)      *EXECUTED*                            
         BE    *+8                                                              
         OI    TSERRS,TSERNF                                                    
         B     TSRDH30                                                          
                                                                                
TSRDH22  CLI   TSACTN,TSAGET       TEST GET                                     
         BE    TSRDH30             YES - PASS RECORD                            
         CLI   TSACTN,TSANXT       TEST NEXT                                    
         BE    TSRDH30             YES - PASS RECORD                            
         B     TSRDHX              ELSE EXIT                                    
                                                                                
TSRDH30  L     R4,KEYADDR          GET ADDRESS OF KEY                           
         SR    R1,R1                                                            
         ICM   R1,7,TSAREC+1       GET USER REC ADDRESS                         
         TM    TSBRECI,TSRVAR                                                   
         BZ    *+8                                                              
         LA    R1,2(R1)            POINT TO USER KEY                            
         LH    RF,TSBKEYL1                                                      
         EX    RF,*+8              MOVE KEY TO USER RECORD AREA                 
         B     *+10                                                             
         MVC   0(0,R1),L'TSBDPTR(R4)    *EXECUTED*                              
                                                                                
TSRDH32  ICM   RE,15,0(R4)         GET DSPL TO DATA                             
         BZ    TSRDHX              EXIT IF NO DATA                              
         AR    RE,BUFFREG          ADD BUFFER ADDR TO POINT TO RECORD           
         EJECT                                                                  
***********************************************************************         
*     KEY AREA HAS    -- DSPL --   -- KEY --                          *         
*                      0  1  2  3  4  5  6  ...                       *         
*                                                                     *         
*     DATA AREA HAS  (LN LN) DATA ... *                               *         
*                    ( 0  1) 2  3  4  5 ...                           *         
*     THIS LENGTH INCLUDES KEY.                                       *         
*                                                                     *         
*     USER AREA GETS  (LN LN) KEY      ...   DATA                     *         
*                     (0  1 ) 2  3  4  ...                            *         
***********************************************************************         
                                                                                
         TM    TSBRECI,TSRVAR                                                   
         BZ    TSRDH34                                                          
                                                                                
***********************************************************************         
* VARIABLE LENGTH RECORDS                                             *         
***********************************************************************         
                                                                                
         SHI   R1,2                                                             
         MVC   0(2,R1),0(RE)       MOVE RECORD LENGTH IN FRONT OF KEY           
                                                                                
         LA    R0,2(R1)            POINT TO START OF KEY                        
         AH    R0,TSBKEYL          AND ADD KEY LENGTH                           
         LLH   R1,0(RE)            GET RECORD LENGTH                            
         SH    R1,TSBKEYL          LESS KEY LENGTH                              
         SHI   R1,2                ADJUST FOR RECORD LEN                        
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RE,2(RE)            POINT TO START OF RECORD DATA                
         LR    RF,R1               'FROM' LEN = 'TO' LEN                        
         MVCL  R0,RE                                                            
         B     TSRDHX                                                           
                                                                                
***********************************************************************         
* FIXED LENGTH RECORDS                                                *         
***********************************************************************         
                                                                                
TSRDH34  LR    R0,R1                                                            
         AH    R0,TSBKEYL          POINT TO START OF DATA                       
         LH    R1,TSBRECL                                                       
         SH    R1,TSBKEYL                                                       
         BNM   *+6                                                              
         DC    H'0'                                                             
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE RECORD TO CALLER'S I/O AREA             
                                                                                
TSRDHX   B     TSEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SORT KEYS IN TSAR BUFFER                                 *         
* AFTER A SORT KEYS WILL BE OUT OF SEQUENCE                           *         
* IT IS THE USERS RESPONSIBILITY TO MAKE SURE THAT ALL CALLS ARE BY   *         
* NUMBER AND NOT BY KEY. AN ASSOCIATED FEATURE IS THE INDICATOR BIT   *         
* THAT ALLOWS A CHANGE OF KEY ON A PUT BY RECORD NUMBER               *         
***********************************************************************         
                                                                                
TSSRT    NTR1  ,                                                                
         ICM   RF,15,AQSORT        TEST A(QSORT) ALREADY RESOLVED               
         BNZ   TSSRT02                                                          
         LHI   R0,QQSORT           NO - GET ITS ADDRESS                         
         ICM   R0,B'1110',=X'D9000A'                                            
         GOTOR TCALLOV,DMCB,0,(R0),0,XA=OFF                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,0(R1)                                                         
         OILH  GR1,X'8000'         (INDICATE 31 BIT CALLS TO QSORT)             
         GOTOR TPROTOFF                                                         
         STCM  R1,15,AQSORT        SET A(QSORT)                                 
         GOTOR TPROTON                                                          
         ICM   RF,15,AQSORT                                                     
                                                                                
TSSRT02  LA    R1,DMCB                                                          
         LA    RE,TSBKEYS                                                       
         ST    RE,0(R1)                                                         
         SR    R0,R0                                                            
         ICM   R0,7,TSBRECS                                                     
         BZ    TSEXIT              EXIT IF NO SORT RECORDS                      
         BCTR  R0,0                ADJUST FOR EOF REC                           
         ST    R0,4(R1)                                                         
         OC    0(1,R1),TSRTKSEQ    SORT SEQUENCE                                
         LH    R0,TSBKEYL3         GET KEYLEN+3                                 
         ST    R0,8(R1)            IS THE RECORD LENGTH                         
         IC    R0,TSRTKLEN         LENGTH OF SORT KEY                           
         ST    R0,12(R1)                                                        
         IC    R0,TSRTKDSP         DSPL OF SORT KEY                             
         AHI   R0,L'TSBDPTR        ADJUST FOR 4 BYTE DSPL FIELD                 
         ST    R0,16(R1)                                                        
         BASR  RE,RF               NOTE CALL IS IN 31-BIT MODE                  
         XC    TSRTPARM,TSRTPARM   CLEAR INCOMING SORT PARMS                    
         OI    TSBSTAT,TSBQSRTD    SET FLAG THAT BUFFER WAS SORTED              
         B     TSEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE UPDATED TEMPSTR PAGES TO DISK                      *         
***********************************************************************         
                                                                                
TSSAV    NTR1  ,                                                                
         TM    TSFLAG,TSFLAGND     TEST SPECIAL PROCESSING                      
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    TSBSTAT,TSBQACTV+TSBQSRTD TEST ANY UPDATE ACTIVITY               
         BZ    TSEXIT                                                           
         NI    TSBSTAT,X'FF'-TSBQACTV  RESET                                    
         MVC   TSPAGN,TSBPAGN      SET NUMBER OF PAGES FROM BUFFER              
         LLC   R0,TSPAGN           GET NUMBER OF PAGES                          
         SR    R5,R5                                                            
         LLC   R6,TSPAGL           GET LOW PAGE NUMBER                          
                                                                                
TSSAV2   C     R5,TSBNXKEY         TEST KEY DATA ON THIS PAGE                   
         BNL   TSSAV10             NO                                           
         GOTOR TSDMGR,DMWRT                                                     
                                                                                
         AH    R5,PAGESIZE         BUMP BUFFER DSPL                             
         LA    R6,1(R6)            NEXT PAGE NUMBER                             
         BCT   R0,TSSAV2                                                        
         B     TSEXIT                                                           
                                                                                
TSSAV10  LR    RE,R5               SAVE START OF PAGE ADDRESS                   
         AH    RE,PAGESIZE         NOW TEST AGAINST PAGE END                    
         C     RE,TSBNXDTA         TEST DATA ON THIS PAGE                       
         BL    TSSAV12             NO - SKIP                                    
         GOTOR TSDMGR,DMWRT                                                     
                                                                                
TSSAV12  AH    R5,PAGESIZE                                                      
         LA    R6,1(R6)                                                         
         BCT   R0,TSSAV10                                                       
         B     TSEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ PAGES FROM DISK                                     *         
***********************************************************************         
                                                                                
TSRES    NTR1  ,                                                                
         TM    TSFLAG,TSFLAGND     TEST SPECIAL PROCESSING                      
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R5,R5               CLEAR DSPL REG                               
         LLC   R6,TSPAGL           SET LOW PAGE NUMBER                          
         GOTOR TSDMGR,DMREAD       READ PAGE 1                                  
                                                                                
         LA    R1,TSAREYE1                                                      
         TM    TSIND2,TSI2BUF2                                                  
         BZ    *+8                                                              
         LA    R1,TSAREYE2                                                      
         CLC   TSBLABEL(L'TSAREYE1),0(R1)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   FILE,TEMPEST        TEST TEMPEST STORAGE                         
         BNE   TSRES1                                                           
         L     RF,TUTL             CHECK THIS TERMINAL OWNS THE BUFFER          
         TM    TSTAT1-UTLD(RF),TSTATDDS                                         
         BNZ   TSRES1                                                           
         CLC   TSBTERM,TNUM-UTLD(RF)                                            
         BE    TSRES1                                                           
         ICM   R0,B'1100',TSBTERM  GET TERMINAL NUMBERS IN R0                   
         ICM   R0,B'0011',TNUM-UTLD(RF)                                         
         DC    H'0'                                                             
                                                                                
TSRES1   MVC   TSPAGN,TSBPAGN      RESET NUMBER OF ALLOCATED PAGES              
         LLC   R0,TSPAGN           R0=NUMBER OF PAGES                           
         B     TSRES4                                                           
                                                                                
TSRES2   C     R5,TSBNXKEY         IF LOW, READ NEXT PAGE OF KEYS               
         BNL   TSRES10                                                          
         GOTOR TSDMGR,DMREAD                                                    
                                                                                
TSRES4   AH    R5,PAGESIZE         BUMP DSPL REG                                
         LA    R6,1(R6)            BUMP PAGE NUMBER                             
         BCT   R0,TSRES2           LOOP FOR PAGES                               
         B     TSEXIT                                                           
                                                                                
TSRES10  LR    RE,R5               NOW READ ACTIVE DATA PAGES                   
         AH    RE,PAGESIZE         TEST AGAINST PAGE END                        
         C     RE,TSBNXDTA         TEST DATA ON THIS PAGE                       
         BL    TSRES12             NO                                           
         GOTOR TSDMGR,DMREAD                                                    
                                                                                
TSRES12  AH    R5,PAGESIZE                                                      
         LA    R6,1(R6)                                                         
         BCT   R0,TSRES10                                                       
         B     TSEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* FREE ACQUIRED STORAGE                                               *         
***********************************************************************         
                                                                                
TSFRE    NTR1  ,                                                                
         TM    TSFLAG,TSFLAGOL     TEST OFF-LINE                                
         BZ    TSEXIT                                                           
         ICM   R1,15,TSBUFFA                                                    
         BZ    TSEXIT                                                           
         XC    TSBUFFA,TSBUFFA                                                  
         LLH   R0,TSBUFFL                                                       
         MHI   R0,K                (TSBUFFL IS KBYTES)                          
         TM    TSRECI,TSRMGB       TEST TSBUFFL IS MBYTES                       
         BZ    *+8                                                              
         MHI   R0,K                YES - MULTIPLY AGAIN                         
         FREEMAIN RC,A=(1),LV=(0)                                               
         B     TSEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO DATAMGR                                                *         
***********************************************************************         
                                                                                
TSDMGR   ST    RE,SAVERE                                                        
         ST    R1,DMCB             SET A(COMMAND) IN DM1                        
         LA    R1,DMCB                                                          
         OI    0(R1),X'40'         INDICATE 31-BIT BUFFER                       
         LA    RE,FILE                                                          
         ST    RE,4(R1)            SET A(FILE)                                  
         XC    8(4,R1),8(R1)                                                    
         STC   R6,8(R1)            SET PAGE NUMBER                              
*                                                                               
TSDMGR1  CLI   TSESSID,0           TEST IF PASSED SESSION ID + 1                
         BE    TSDMGR2                                                          
         CLC   FILE,TEMPSTR        FOR TEMPSTR ONLY                             
         BNE   TSDMGR2                                                          
         LLC   RE,TSESSID          GET SESSION ID + 1                           
         AHI   RE,-1                                                            
         MHI   RE,5                PAGES PER SESSION                            
         AR    RE,R6               PLUS RELATIVE PAGE IN SESSION                
         AHI   RE,2                PLUS TWO PAGES AT START                      
         STC   RE,8(R1)                                                         
         OI    8(R1),X'80'         SET PASSING ABSOLUTE PAGE NUMBER             
*                                                                               
TSDMGR2  LR    RE,R5               GET BUFF DSPL                                
         AR    RE,BUFFREG                                                       
         ST    RE,12(R1)                                                        
         MVC   20(2,R1),=C'L='     NOTE - USED FOR RES ONLY                     
         MVC   22(2,R1),PAGESIZE                                                
         GOTOR TDATAMGR,XA=OFF                                                  
         L     RE,SAVERE           SINCE SUPPOSED TO BE CALLED BY BRAS          
                                                                                
RETURN   BR    RE                  THIS SHOULD RETURN US TO 31-BIT MODE         
         EJECT                                                                  
         LTORG                                                                  
K        EQU   1024                                                             
TSRTSPCL EQU   TSRTSAB1+TSRTSAB2+TSRWSSVR+TSRMINB1+TSRMINB2                     
MAXPAGES EQU   24                  MAXIMUM NUMBER OF TEMPEST PAGES              
AQSORT   DC    A(0)                A(QSORT)                                     
TEMPSTPG DC    H'0'                TEMPSTR PAGE SIZE                            
TEMPESPG DC    H'0'                TEMPEST PAGE SIZE                            
TSARBUFL DC    F'0'                TSAR BUFFER LENGTH                           
TSARBUFN DC    X'01'               NUMBER OF TSAR BUFFERS                       
TSARWSVL DC    H'0'                WSSVR TASK BUFFER LENGTH (KBYTES)            
TSAREYE1 DC    C'**TSAR**'         TSAR BUFFER 1 EYECATCHER                     
TSAREYE2 DC    C'**TSA2**'         TSAR BUFFER 2 EYECATCHER                     
TSARKLNS DC    C'KLNS'             KLNS EYECATCHER                              
TSARRECS DC    C'RECS'             RECS EYECATCHER                              
                                                                                
DMREAD   DC    C'DMREAD '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMRSRV   DC    C'DMRSRV '                                                       
TEMPSTR  DC    C'TEMPSTR'                                                       
TEMPEST  DC    C'TEMPEST'                                                       
                                                                                
ACTNTAB  DS    0XL4                ** ACTION TABLE **                           
         DC    AL1(TSAADD),AL3(TSADD)                                           
         DC    AL1(TSARDH),AL3(TSRDH)                                           
         DC    AL1(TSAGET),AL3(TSGET)                                           
         DC    AL1(TSANXT),AL3(TSNXT)                                           
         DC    AL1(TSAPUT),AL3(TSPUT)                                           
         DC    AL1(TSAWRT),AL3(TSWRT)                                           
         DC    AL1(TSADEL),AL3(TSDEL)                                           
         DC    AL1(TSAINI),AL3(TSINI)                                           
         DC    AL1(TSASRT),AL3(TSSRT)                                           
         DC    AL1(TSASAV),AL3(TSSAV)                                           
         DC    AL1(TSARES),AL3(TSRES)                                           
         DC    AL1(TSAFRE),AL3(TSFRE)                                           
ACTNTABX DC    AL1(0)                                                           
         EJECT                                                                  
TSAWRK   DSECT                     = TSAR WORKING STORAGE =                     
ZERO     DS    F                                                                
RELO     DS    F                                                                
TBINSRCH DS    V                                                                
TDATAMGR DS    V                                                                
TCALLOV  DS    V                                                                
TSWITCH  DS    V                                                                
TPROTON  DS    V                                                                
TPROTOFF DS    V                                                                
TUTL     DS    A                                                                
DMCB     DS    0XL24                                                            
DM1      DS    F                                                                
DM2      DS    F                                                                
DM3      DS    F                                                                
DM4      DS    F                                                                
DM5      DS    F                                                                
DM6      DS    F                                                                
AREC     DS    A                                                                
KEYADDR  DS    A                                                                
SAVE     DS    A                                                                
SAVEKEY  DS    A                                                                
SAVERE   DS    A                                                                
RECDSPL  DS    F                                                                
PAGESIZE DS    H                                                                
RECL     DS    H                                                                
FLAG     DS    X                                                                
ADDFLAG  DS    X                                                                
FILE     DS    CL7                                                              
TSFLAG   DS    X                                                                
TSFLAGOL EQU   X'80'               OFF-LINE                                     
TSFLAGND EQU   X'40'               SPECIAL PROCESSING FLAG (NO DISK)            
TSAWRKL  EQU   *-TSAWRK                                                         
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
                                                                                
       ++INCLUDE DDTSBUFFD                                                      
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
                                                                                
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
                                                                                
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
                                                                                
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
                                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
                                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018DDTSARDINE08/28/13'                                      
         END                                                                    
