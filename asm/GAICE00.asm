*          DATA SET GAICE00    AT LEVEL 063 AS OF 05/01/02                      
*          DATA SET GAICE00    AT LEVEL 059 AS OF 06/04/83                      
*PHASE TB1300A                                                                  
*INCLUDE RANDOM                                                                 
         TITLE 'ICE-CREAM GAME'                                                 
         PRINT NOGEN                                                            
*        REGISTER USAGE -                                                       
*                                                                               
*        R0    WORK                                                             
*        R1    WORK    /   LINKAGE                                              
*        R2    WORK                                                             
*        R3    WORK                                                             
*        R4    WORK                                                             
*        R5    WORK                                                             
*        R6    WORK                                                             
*        R7    WORK                                                             
*        R8    WORK                                                             
*        R9    PROGRAM BASE 2                                                   
*        RA    TWA                                                              
*        RB    PROGRAM BASE 1                                                   
*        RC    WORK AREA                                                        
*        RD    SAVE AREA CHAIN                                                  
*        RE    WORK    /   LINKAGE                                              
*        RF    WORK    /   LINKAGE                                              
*                                                                               
ICEGAME  CSECT                                                                  
         NMOD1 WRK00L,**ICE0**,R9,RR=R2                                         
         USING WRK00D,RC           RC=A(W/S)                                    
         ST    R2,RELOFACT         STORE RELOCATION FACTOR                      
         A     R2,=V(MESSECT)      RELOCATE MESSAGE CSECT ADDR                  
         ST    R2,VMESSECT                                                      
         L     RA,4(,R1)                                                        
         USING TWAD,RA             RA=A(TWA)                                    
         MVC   AEVENTS,12(R1)      SAVE TIA ADDRESS AS EVENT TAB ADDR           
         L     R2,16(,R1)          ADDRESS COMFACS                              
         USING COMFACSD,R2                                                      
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VSCANNER,CSCANNER                                                
*                                                                               
         XC    SCRMESS,SCRMESS     CLEAR MESSAGE LINE                           
         OI    SCRMESSH+6,X'80'    AND FORCE WRITE                              
         DROP  R2                                                               
*                                                                               
*        PARSE INPUT STRING USING SCANNER MODULE                                
*                                                                               
SCANINP  XC    INPAREA(INPCOUNT-INPAREA+1),INPAREA CLEAR INPUT WKAREA           
         CLI   SCRINPH+5,0         ANY INPUT ?                                  
         BE    INWEGO              NO - SKIP SCAN                               
         CLC   =C'$CANCEL',SCRINP  USER GAME CANCEL ?                           
         BE    RESTART             YES - GO START AGAIN                         
         GOTO1 VSCANNER,DMCB,SCRINPH,(7,INPAREA) SCAN UP TO 7 FIELDS            
         MVC   INPCOUNT,DMCB+4     SET NUMBER OF FIELDS ENTERED                 
*                                                                               
*        RELOAD SAVED STORAGE                                                   
*        DETERMINE WERE WE WERE LAST TIME AND GO TO IT                          
*                                                                               
INWEGO   CLI   DAYNUM,0            IF ZERO, THIS IS FIRST TIME IN               
         BE    INIT                SO GOTO INIT CODE                            
*                                                                               
         LH    R2,TWATRM           GET TERMINAL ID                              
         ICM   R2,4,=X'FF'         ASK FOR 2560 BYTES                           
         LA    R3,WSAVE            GET SAVE ADDRESS                             
         GOTO1 VDATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',(1,(R2)),(R3)               
*                                                                               
         LH    RE,RETRNOFF         GET OFFSET TO BRANCH TO                      
         LA    RE,ICEGAME(RE)      ADD PROGRAM ADDRESS                          
         BR    RE                  AND GOTO IT                                  
         EJECT                                                                  
*        PHASE 1 - FIRST TIME INITIALIZATION FOR NEW GAME                       
*                                                                               
INIT     DC    0H'0'                                                            
*                                                                               
*        CLEAR SAVED WORK AREA AND OTHER STORAGE                                
*                                                                               
         LA    R2,WSAVE            ADDRESS OF AREA TO CLEAR                     
         LA    R3,2560             LENGTH TO CLEAR                              
         XR    R4,R4                                                            
         LR    R5,R4                                                            
         MVCL  R2,R4                                                            
         XC    TWAUSER,TWAUSER                                                  
*                                                                               
*        LOAD RANDOM EVENT TABLE INTO TIA                                       
*                                                                               
         GOTO1 VCALLOV,DMCB,(1,AEVENTS),0                                       
         CLI   DMCB+4,X'FF'        LOAD ERROR ?                                 
         BNE   *+6                 NO - OK                                      
         DC    H'0'                DOWNWEGO - PHASE LOAD ERROR                  
         L     R3,AEVENTS          GET TABLE ADDRESS (TIA)                      
         CLI   0(R3),2             COMPARE WITH MAXIMUM LENGTH                  
         BNH   *+6                 SKIP IF NOT TOO BIG                          
         DC    H'0'                ELSE FALL OVER                               
*                                                                               
*        STORE TABLE TO DISK                                                    
*                                                                               
         LH    R2,TWATRM           GET TERMINAL ID                              
         ICM   R2,4,=X'FF'         ASK FOR 2560 BYTES                           
         XR    R4,R4                                                            
         IC    R4,0(R3)            GET NUMBER OF RECORDS TO SAVE                
         MVC   TIARECS,0(R3)       SAVE NUMBER OF RECORDS                       
INIT2    LA    R5,1(,R4)           USE RECORD TWO ONWARDS                       
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',((R5),(R2)),(R3)             
         LA    R3,2560(,R3)                                                     
         BCT   R4,INIT2            LOOP TILL ALL SAVED                          
*                                                                               
*        INITIALIZE VARIOUS STORAGE                                             
*                                                                               
         MVI   DAYNUM,1            SET WEDNESDAY                                
         MVC   WHOLEICE,=H'1150' ICE-CREAM STARTS AT $11.45 PER GALLON          
*                                  INITIALIZE LOCATION NAMES                    
         L     RE,VMESSECT                                                      
         USING MESSECT,RE                                                       
         MVC   LOCATION+(0*LOCDSL)+LONAME-LOCDS(L'LONAME),LOCTAB+0*LOCTX        
               ABL                                                              
         MVC   LOCATION+(1*LOCDSL)+LONAME-LOCDS(L'LONAME),LOCTAB+1*LOCTX        
               ABL                                                              
         MVC   LOCATION+(2*LOCDSL)+LONAME-LOCDS(L'LONAME),LOCTAB+2*LOCTX        
               ABL                                                              
         MVC   LOCATION+(3*LOCDSL)+LONAME-LOCDS(L'LONAME),LOCTAB+3*LOCTX        
               ABL                                                              
         MVC   LOCATION+(4*LOCDSL)+LONAME-LOCDS(L'LONAME),LOCTAB+4*LOCTX        
               ABL                                                              
         MVC   LOCATION+(5*LOCDSL)+LONAME-LOCDS(L'LONAME),LOCTAB+5*LOCTX        
               ABL                                                              
         EJECT                                                                  
*        PHASE 2 - FIND NUMBER (UPTO 3) OF PLAYERS.                             
*                  DISPLAY INSTRUCTIONS ETC.                                    
*                                                                               
PLAYCHK  CLI   INPCOUNT,1          ONE FIELD INPUT ?                            
         BNE   PLAYINV             NO - ERROR                                   
         LA    R1,INPAREA          POINT TO IT                                  
         USING INPDS,R1                                                         
         CLI   INPL1,0             NULL ENTERED ?                               
         BE    PLAYINV             YES - GO TO INVALID SCREEN                   
         CLI   INPL2,0             WAS REPLY XXX=N FORMAT ?                     
         BNE   PLAYCHK1            YES - SKIP                                   
         ZIC   R2,INPL1            GET LENGTH FIRST FIELD                       
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   INPDATA1(0),=C'HELP ' SEE IF PLAYER WANTS HELP                   
         BE    INSTR1              YES - GIVE HIM INSTRUCTIONS                  
         B     PLAYCHK2            NO - SEE IF THIS IS NO. PLAYERS              
PLAYCHK1 MVC   INPV1,INPV2         MOVE SECOND HALF TO FIRST HALF               
         MVC   INPVAL1,INPVAL2                                                  
PLAYCHK2 TM    INPV1,X'80'         WAS NUMBER OF PLAYERS NUMERIC ?              
         BZ    PLAYINV             NO - ERROR ASK AGAIN                         
         ICM   R2,15,INPVAL1       ELSE GET VALUE ENTERED                       
         BZ    PLAYINV             ZERO IS INVALID                              
         CH    R2,=H'3'            MORE THAN THREE ?                            
         BNH   PLAYOK              NO - SEEMS OK                                
         DROP R1                                                                
PLAYINV  L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'ERRMESS1),ERRMESS1                                     
         B     PLAYASK             PLAYERS INVALID SO ASK AGAIN                 
*                                                                               
INSTR1   LA    R1,=C'INSTR1'       GET INSTRUCTIONS(1) INTO SCREEN              
         BAS   RE,SCRMOVE                                                       
         BAS   RE,SCRTEXT                                                       
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'ENTMESS),ENTMESS    MOVE MESSAGE                       
         XC    SCRHEAD,SCRHEAD     CLEAR TOP LINE                               
         OI    SCRHEADH+6,X'80'    AND FORCE WRITE                              
         XC    SCRINP,SCRINP       CLEAR INPUT FIELD                            
         BAS   RE,SCRWRITE         WRITE/READ SCREEN                            
*                                                                               
         LA    R1,=C'INSTR2'       GET INSTRUCTIONS(2) INTO SCREEN              
         BAS   RE,SCRMOVE          (INCLUDES NUMBER OF PLAYERS REQUEST)         
         BAS   RE,SCRTEXT                                                       
         XC    SCRMESS,SCRMESS     CLEAR MESSAGE LINE                           
PLAYASK  XC    SCRHEAD,SCRHEAD     CLEAR TOP LINE                               
         OI    SCRHEADH+6,X'80'    AND FORCE WRITE                              
         XC    SCRINP,SCRINP       CLEAR INPUT FIELD                            
         BAS   RE,SCRWRITE         WRITE/READ SCREEN                            
*                                                                               
         B     PLAYCHK             GO SEE IF WE GOT NUMBER OF PLAYERS           
*                                                                               
*        WE HAVE A VALID NUMBER OF PLAYERS - INITIALIZE ACCORDINGLY             
*                                                                               
PLAYOK   XC    SCRINP,SCRINP       CLEAR INPUT FIELD                            
*                                                                               
         XC    SCRHEAD,SCRHEAD     CLEAR TOP LINE                               
         OI    SCRHEADH+6,X'80'    AND FORCE WRITE                              
         STC   R2,NPLAYERS         SAVE BINARY NO OF PLAYERS                    
         LA    R1,3(R2,R2)         'CALCULATE' NUMBER OF VANS FOR SALE          
         STC   R1,SALEVANS                                                      
         LA    R1,PLAYERS                                                       
         USING PLAYERDS,R1                                                      
PLAYINIT MVI   PLVANS,1            GIVE PLAYER 1 VAN                            
         MVC   PLMONEY,=F'50000'   AND $500 CASH                                
         LA    R1,PLAYERDL(,R1)    POINT TO NEXT PLAYER                         
         BCT   R2,PLAYINIT         AND INIT EACH PLAYER                         
         DROP  R1                                                               
         EJECT                                                                  
*        PHASE 3 - START OF NEW DAYS PLAY                                       
*                  DETERMINE ALL RANDOM CONDITIONS FOR DAY                      
*                                                                               
NEWDAY   DC    0H'0'                                                            
*                                                                               
EVENTS   LH    R1,DAYCOUNT         INCREMENT NUMBER OF DAYS PLAYED              
         LA    R1,1(,R1)                                                        
         STH   R1,DAYCOUNT                                                      
*                                                                               
         XR    R1,R1                                                            
         IC    R1,DAYNUM           GET DAY OF WEEK                              
         MH    R1,=AL2(DAYTABL)    TIMES LENGTH OF TABLE                        
         L     RE,VMESSECT                                                      
         USING MESSECT,RE                                                       
         LA    R1,DAYTAB-DAYTABL(R1) ADD START TO GIVE TODAYS ENTRY             
         MVC   DAYENT,0(R1)        SAVE TODAYS ENTRY                            
*                                                                               
         LA    R1,=C'WEATHR'       GET TODAYS WEATHER FORECAST                  
         BAS   RE,GETEVENT                                                      
         L     R1,DMCB                                                          
         MVC   WEATHER,4(R1)       SAVE IT                                      
*                                                                               
         LA    R1,=C'OCCASN'       GET TODAYS SPECIAL OCCASION                  
         BAS   RE,GETEVENT                                                      
         L     R1,DMCB                                                          
         MVC   OCCASION,4(R1)       SAVE IT                                     
*                                                                               
         LA    R1,=C'DPROBL'       GET TODAYS PROBLEM                           
         BAS   RE,GETEVENT                                                      
         L     R1,DMCB                                                          
         MVC   PROBLEM,4(R1)       SAVE IT                                      
*                                                                               
         XR    R3,R3                                                            
         IC    R3,NPLAYERS         GET NUMBER OF PLAYERS                        
         LA    R2,PLAYERS          POINT TO PLAYER STATUS AREAS                 
         USING PLAYERDS,R2                                                      
P2PROB01 XC    PLPROB,PLPROB                                                    
         CLI   PLBNKFLG,0          IS PLAYER ACTIVE                             
         BNE   P2PROB02            NO - SKIP                                    
         LA    R1,=C'PPROBL'       GET PLAYERS OWN PROBLEM                      
         BAS   RE,GETEVENT                                                      
         L     R1,DMCB                                                          
         MVC   PLPROB,4(R1)        SAVE IT                                      
P2PROB02 LA    R2,PLAYERDL(,R2)    POINT TO NEXT PLAYER                         
         BCT   R3,P2PROB01         REPEAT FOR EACH PLAYER                       
         DROP  R2                                                               
         EJECT                                                                  
*        PHASE 4 - ASK EACH PLAYER TO ENTER HIS DAYS STRATEGY                   
*                  AND HIS NAME, IF FIRST TIME                                  
*                                                                               
STRATEGY MVI   CPLAYER,1           CURRENT PLAYER NUMBER                        
*                                                                               
*        GET ADDRESS OF CURRENT PLAYER ENTRY AND COPY IT TO A WORK AREA         
*        ALSO SAVE LOCATION TABLE AND VANS FOR SALE                             
*                                                                               
NXTPLAYR DC    0H'0'                                                            
         MVC   LOSAVE(3*L'LOSAVE),LOCATION SAVE LOCATION DATA                   
         MVC   LOSAVE+(3*L'LOSAVE)(3*L'LOSAVE),LOCATION+(3*L'LOSAVE)            
         MVC   VASAVE,SALEVANS     AND NUMBER OF VANS FOR SALE                  
*                                                                               
STAPLAYR XR    R8,R8               GET CURRENT PLAYER ENTRY                     
         IC    R8,CPLAYER                                                       
         BCTR  R8,0                                                             
         MH    R8,=AL2(PLAYERDL)                                                
         LA    R8,PLAYERS(R8)                                                   
*                                                                               
*        FIRST CHECK IF THE PLAYER IS STILL IN THE GAME                         
*                                                                               
         USING PLAYERDS,R8                                                      
         CLI   PLBNKFLG,0          ARE WE BANKRUPT                              
         BNE   ENDPLAY2            YES - IGNORE AND DO NEXT PLAYER              
*                                                                               
         MVC   PLWORK,0(R8)        COPY TO WORK AREA                            
*                                                                               
         LA    R8,PLWORK                                                        
         USING PLAYERDS,R8                                                      
         XC    PLINCOME,PLINCOME   CLEAR TODAYS INCOME                          
         MVC   PLSTRMON,PLMONEY    SAVE TODAYS START BALANCE                    
*                                                                               
*        BUILD SCREEN HEADER AND CLEAR REST OF SCREEN                           
*                                                                               
         XC    SCRHEAD,SCRHEAD     CLEAR TOP LINE                               
         OI    SCRHEADH+6,X'80'    AND FORCE WRITE                              
         MVC   SCRHEAD(10),=C'PLAYER X -'                                       
         MVC   SCRHEAD+7(1),CPLAYER                                             
         OI    SCRHEAD+7,X'F0'                                                  
         MVC   SCRHEAD+11(10),PLNAME                                            
         MVC   SCRHEAD+29(3),=C'DAY'                                            
         EDIT  DAYCOUNT,(4,FULL)                                                
         MVC   SCRHEAD+33(4),FULL                                               
*                                                                               
         TWAXC SCRL0H,PROT=Y       CLEAR SCREEN                                 
*                                                                               
*        IF WE HAVENT GOT A NAME, THIS IS THE FIRST TIME THROUGH                
*        SO ASK FOR ONE                                                         
*                                                                               
         CLI   PLNAME,0            HAVE WE GOT A NAME                           
         BNE   GOTNAME             YES - SKIP                                   
*                                                                               
ASKNAME  L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRL20(L'NAMEMESS),NAMEMESS                                      
         BAS   RE,SCRWRITE         ASK PLAYER FOR HIS NAME                      
         CLI   INPCOUNT,0          ANY INPUT ENTERED ?                          
         BE    ASKNAME             NO - ASK AGAIN                               
         CLI   INPAREA,0           JUST BLANKS ?                                
         BE    ASKNAME             YES - ASK AGAIN                              
*                                                                               
         TWAXC SCRL0H,PROT=Y       CLEAR SCREEN AGAIN                           
*                                                                               
         LA    R8,PLWORK           RELOAD R8                                    
         LA    R1,INPAREA          POINT TO INPUT ARRAY                         
         USING INPDS,R1                                                         
         CLI   INPL2,0             WAS REPLY XXX=N FORMAT ?                     
         BE    *+14                NO -SKIP, IT WAS N ONLY FORM                 
         MVC   PLNAME,INPDATA2     MOVE SECOND HALF TO NAME                     
         B     *+10                                                             
         MVC   PLNAME,INPDATA1     MOVE FIRST HALF TO NAME                      
         DROP  R1                                                               
         XR    R1,R1               GET CURRENT PLAYER ENTRY                     
         IC    R1,CPLAYER                                                       
         BCTR  R1,0                                                             
         MH    R1,=AL2(PLAYERDL)                                                
         LA    R1,PLAYERS(R1)                                                   
         MVC   PLNAME-PLAYERDS(L'PLNAME,R1),PLNAME COPY NAME TO TABLE           
         MVC   SCRHEAD+11(10),PLNAME AND TO SCREEN                              
         OI    SCRHEADH+6,X'80'    AND FORCE WRITE                              
*                                                                               
*        NOW WE CAN BUILD THE VARIABLE PARTS OF THE SCREEN                      
*                                                                               
GOTNAME  DC    0H'0'                                                            
         XC    SCRINP,SCRINP       CLEAR INPUT                                  
         LA    R2,PLPRFS           CALCULATE MEAN PROFIT FOR LAST               
         LA    R3,7                SEVEN DAYS AND DERIVE CREDIT LIMIT           
         LR    R4,R3                                                            
         CH    R4,DAYCOUNT                                                      
         BL    CREDCALC                                                         
         LH    R4,DAYCOUNT                                                      
         BCT   R4,CREDCALC                                                      
         L     R1,=F'10000'                                                     
         B     CREDFRST                                                         
CREDCALC XR    R1,R1                                                            
         LR    R0,R1                                                            
         A     R1,0(R2)                                                         
         LA    R2,4(,R2)                                                        
         BCT   R3,*-8                                                           
         BNM   *+8                                                              
         LH    R0,=H'-1'                                                        
         DR    R0,R4                                                            
         LPR   R2,R1                                                            
         MR    R0,R2                                                            
         D     R0,=F'100000'                                                    
         ZIC   R2,PLVANS                                                        
         MH    R2,=H'10000'                                                     
         AR    R1,R2                                                            
         BP    *+6                                                              
         XR    R1,R1                                                            
CREDFRST ST    R1,PLCREDIT                                                      
         EDIT  (R1),(11,DCREDIT),2,FLOAT=$                                      
*                                                                               
         MVC   DSELLVAN,=C'NO'     SET UP NUMBER OF VANS FOR SALE               
         CLI   SALEVANS,0          ANY TO SELL ?                                
         BE    BUYPSET             NO - SKIP                                    
         EDIT  SALEVANS,(2,DSELLVAN)                                            
BUYPSET  MVC   DBUYFLAG,=C'CAN  '                                               
         XR    R1,R1               CALCULATE SELLING PRICE OF VANS              
         IC    R1,SALEVANS         BASED ON NUMBER AVAILABLE                    
         SLL   R1,5                NUMBER TIMES 32                              
         LA    R2,0(R1,R1)         TIMES 64                                     
         LA    R1,0(R1,R2)         TIMES 96                                     
         LA    R2,1750             BASE PRICE                                   
         SR    R2,R1               MINUS ADJUSTMENT GIVES VAN PRICE             
         EDIT  (R2),(5,DBUYPRC),FLOAT=$                                         
         LR    R3,R2                                                            
         MH    R3,=H'100'          SAVE AS $.PP                                 
         ST    R3,BUYPRC           SAVE FOR LATER                               
         SH    R2,=H'273'          CALCULATE VAN BUY PRICE                      
         EDIT  (R2),(5,DSELPRC),FLOAT=$                                         
         MH    R2,=H'100'          SAVE AS $.PP                                 
         ST    R2,SELPRC           SAVE FOR LATER                               
*                                                                               
         EDIT  PLVANS,(2,DNOVANS)  NUMBER OF VANS OWNED                         
*                                                                               
         MVC   DBALSIGN,=C'HAVE         '                                       
         ICM   R1,15,PLMONEY       GET CURRENT PLAYER BALANCE                   
         BNM   *+12                SKIP IF IN CREDIT                            
         LPR   R1,R1               MAKE IT POSITIVE FOR EDIT                    
         MVC   DBALSIGN,=C'ARE OVERDRAWN' AND SET OVERDRAWN MESSAGE             
         EDIT  (R1),(11,DBALANCE),2,FLOAT=$                                     
*                                                                               
         MVC   DPRFLSS,=C'PROFIT'                                               
         L     R1,PLYEPRF          GET YESTERDAYS PROFIT                        
         LTR   R1,R1                                                            
         BNM   *+12                SKIP IF POSITIVE                             
         LPR   R1,R1               MAKE IT POSITIVE FOR EDIT                    
         MVC   DPRFLSS,=C'LOSS  '  AND SET MESSAGE                              
         EDIT  (R1),(11,DPROFIT),2,FLOAT=$                                      
*                                                                               
         LH    R1,WHOLEICE         GET WHOLESALE ICE-CREAM PRICE                
         EDIT  (R1),(6,DWHOLES),2,FLOAT=$                                       
         EJECT                                                                  
*        FIRST STAGE - BUY AND SELL VANS.                                       
*                                                                               
         XC    DMCB(7),DMCB        INIT TXTDEL PARM LIST                        
         MVI   DMCB+7,X'FF'                                                     
*                                                                               
         MVI   FBUYVANS,0          PRESET CANT BUY OR SELL VANS                 
         MVI   FSELVANS,0                                                       
*                                                                               
         CLI   SALEVANS,0          ANY VANS FOR SALE ?                          
         BE    BSNOVAN             NO - SKIP                                    
         L     R1,PLMONEY          GET MONEY                                    
         A     R1,PLCREDIT         ADD CREDIT LIMIT                             
         C     R1,BUYPRC           CAN WE AFFORD TO BUY A VAN                   
         BL    BSNOMON             NO - SKIP                                    
         MVI   FBUYVANS,X'FF'      SET BUY OK FLAG                              
         B     BSBUYOK                                                          
BSNOMON  MVC   DBUYFLAG,=C'COULD'                                               
         B     BSSELL                                                           
BSNOVAN  MVI   DMCB+2-1,2                                                       
BSBUYOK  MVI   DMCB+3-1,3                                                       
*                                                                               
BSSELL   CLI   PLVANS,1            HAVE WE ENOUGH VANS TO SELL ?                
         BNH   BSNOSELL            NO - SKIP                                    
         MVI   FSELVANS,X'FF'      INDICATE CAN SELL VANS                       
         B     BSSPEC                                                           
BSNOSELL MVI   DMCB+4-1,4          DELETE INVITATION TO SELL                    
         CLI   FBUYVANS,0          CAN WE BUY VANS                              
         BNE   BSSPEC              YES - SKIP                                   
         MVC   DMCB(8),=X'01020304050000FF' CLEAR BUY SELL INVITATION           
*                                                                               
BSSPEC   CLI   OCCDESC,X'FF'       ANY SPECIAL OCCASSION ?                      
         BNE   BSFIRST             YES - SKIP                                   
         MVI   DMCB+7-1,7          NO - DELETE MESSAGE                          
*                                                                               
BSFIRST  LH    R1,DAYCOUNT         GET DAYS INTO GAME                           
         BCT   R1,BSDELS           SKIP IF NOT DAY ONE                          
         MVI   DMCB+6-1,6          DAY 1 DELETE YESTERDAYS PROFIT LINE          
*                                                                               
BSDELS   MVC   WORK(8),DMCB        SAVE DELETE PARMS                            
         LA    R1,=C'STATUS'                                                    
         BAS   RE,SCRMOVE          GET STATUS SCREEN SKELETON                   
*                                   BUILD TXTMERGE PARM LIST                    
         BAS   RE,TXTSTATS         MERGE TEXT TO SCREEN                         
*                                                                               
         MVC   DMCB(8),WORK        RESTORE DELETE PARMS                         
         LA    R1,DMCB                                                          
         BAS   RE,TXTDEL           DELETE UNWANTED TEXT                         
*                                                                               
         CLI   FBUYVANS,0          CAN PLAYER BUY ANY VANS ?                    
         BNE   BSBUYV              YES - SEE IF HE WANTS TO                     
         CLI   FSELVANS,0          CAN PLAYER SELL ANY VANS ?                   
         BNE   BSSELV              YES - SEE IF HE WANTS TO                     
         BE    DEPLOY              NO - GO AND DEPLOY THE VANS HE HAS           
         EJECT                                                                  
*        DETERMINE WHETHER PLAYER WISHES TO BUY ANY OF THE VANS ON              
*        OFFER.                                                                 
*                                                                               
BSBUYV   DC    0H'0'                                                            
         BAS   RE,SCRTEXT          MOVE TEXT TO SCREEN                          
*                                                                               
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRL19(L'BSMESS1),BSMESS1 ASK FOR NUMBER TO BUY                  
         MVC   SCRL19+37(4),=C'BUY '                                            
         MVC   SCRMESS(L'PROMESS),PROMESS                                       
BSBASK   BAS   RE,SCRWRITE                                                      
*                                                                               
         LA    R8,PLWORK                                                        
         LA    R1,INPAREA                                                       
         USING INPDS,R1                                                         
         CLI   INPL1,0             ANYTHING ENTERED ?                           
         BE    BSBERR              NO - ERROR                                   
         CLI   INPL2,0             X=Y FORMAT ?                                 
         BE    *+16                                                             
         MVC   INPV1,INPV2                                                      
         MVC   INPVAL1,INPVAL2                                                  
         TM    INPV1,X'80'         VALID NUMERIC ?                              
         BZ    BSBERR              NO - ERROR                                   
         ICM   R1,15,INPVAL1       GET VALUE                                    
         BNZ   BSBNZERO            SKIP IF NOT ZERO                             
         CLI   FSELVANS,0          CAN PLAYER SELL A VAN ?                      
         BE    BSEND               NO - SKIP                                    
         B     BSSELV2             YES - GO SEE IF HE WANTS TO                  
BSBNZERO ZIC   R2,SALEVANS         GET NUMBER FOR SALE                          
         CR    R1,R2               ARE THERE ENOUGH ?                           
         BNH   BSBENF              YES - SKIP                                   
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'BSMESS2),BSMESS2                                       
         B     BSBASK              SAY NOT ENOUGH                               
BSBERR   L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'ERRMESS1),ERRMESS1                                     
         B     BSBASK                                                           
         DROP  R1                                                               
BSBENF   L     R3,PLMONEY          CLACULATE HOW RICH WE ARE                    
         A     R3,PLCREDIT                                                      
         L     R4,BUYPRC           CALCULATE COSTS                              
         XR    R5,R5                                                            
         LR    R6,R5                                                            
         LR    R7,R1                                                            
BSBCOST  AR    R5,R4                                                            
         AH    R6,=H'15000'                                                     
         BCT   R7,BSBCOST                                                       
         CR    R5,R3               CAN WE AFFORD TO BUY THIS MANY ?             
         BNH   BSBBOK              YES - SKIP                                   
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'BSMESS3),BSMESS3   CANT AFFORD TO BUY THEM             
         MVC   SCRMESS+20(3),=C'BUY '                                           
         B     BSBASK                                                           
BSBBOK   AR    R6,R5               BUY AND RUN COST                             
         CR    R6,R3               CAN WE AFFORD TO RUN THIS MANY ?             
         BNH   BSBROK              YES - SKIP                                   
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'BSMESS3),BSMESS3   CANT AFFORD TO RUN THEM             
         MVC   SCRMESS+20(3),=C'RUN'                                            
         B     BSBASK                                                           
*                                                                               
*        WE ARE ABLE TO BUY THE REQUESTED NUMBER OF VANS.                       
*                                                                               
BSBROK   SR    R2,R1               FOR SALE MINUS SOLD                          
         STC   R2,SALEVANS         GIVES NEW 'FOR SALE'                         
         IC    R2,PLVANS           GET OWNED VANS                               
         AR    R2,R1               ADD THOSE BOUGHT                             
         STC   R2,PLVANS           GIVES NEW 'NUMBER OWNED'                     
         ICM   R1,15,PLMONEY       GET CURRENT PLAYER BALANCE                   
         SR    R1,R5               MINUS COST OF VANS                           
         ST    R1,PLMONEY          GIVES NEW BALANCE                            
         BP    *+12                SKIP IF IN CREDIT                            
         LPR   R1,R1               MAKE IT POSITIVE FOR EDIT                    
         MVC   DBALSIGN,=C'ARE OVERDRAWN' AND SET OVERDRAWN MESSAGE             
         EDIT  (R1),(11,DBALANCE),2,FLOAT=$                                     
         EDIT  PLVANS,(2,DNOVANS)  NUMBER OF VANS OWNED                         
*                                                                               
         B     BSEND               BUY OK - SKIP SELL                           
         EJECT                                                                  
*        DETERMINE WHETHER PLAYER WISHES TO SELL ANY OF HIS VANS                
*                                                                               
BSSELV   DC    0H'0'                                                            
         BAS   RE,SCRTEXT          MOVE TEXT TO SCREEN                          
*                                                                               
BSSELV2  L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRL19(L'BSMESS1),BSMESS1 ASK FOR NUMBER TO SELL                 
         MVC   SCRL19+37(4),=C'SELL'                                            
         OI    SCRL19H+6,X'80'     FORCE WRITE                                  
         MVC   SCRMESS(L'PROMESS),PROMESS                                       
         XC    SCRINP,SCRINP                                                    
BSSASK   BAS   RE,SCRWRITE                                                      
*                                                                               
         LA    R8,PLWORK                                                        
         LA    R1,INPAREA                                                       
         USING INPDS,R1                                                         
         CLI   INPL1,0             ANYTHING ENTERED ?                           
         BE    BSSERR              NO - ERROR                                   
         CLI   INPL2,0             X=Y FORMAT ?                                 
         BE    *+16                                                             
         MVC   INPV1,INPV2                                                      
         MVC   INPVAL1,INPVAL2                                                  
         TM    INPV1,X'80'         VALID NUMERIC ?                              
         BZ    BSSERR              NO - ERROR                                   
         ICM   R1,15,INPVAL1       GET VALUE                                    
         BZ    BSEND               IF ZERO, GO SET UP DEPLOYMENT                
         ZIC   R2,PLVANS           GET NUMBER OWNED                             
         BCTR  R2,0                                                             
         CR    R1,R2               ARE THERE ENOUGH ?                           
         BNH   BSSENF              YES - SKIP                                   
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'BSMESS4),BSMESS4 HAVENT GOT THAT MANY                  
         B     BSSASK              SAY NOT ENOUGH                               
BSSERR   L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'ERRMESS1),ERRMESS1                                     
         B     BSSASK                                                           
         DROP  R1                                                               
BSSENF   L     R4,SELPRC           CALCULATE INCOME                             
         XR    R5,R5                                                            
         LR    R7,R1                                                            
BSSCOST  AR    R5,R4                                                            
         BCT   R7,BSSCOST                                                       
*                                                                               
*        WE ARE ABLE TO SELL THE REQUESTED NUMBER OF VANS.                      
*                                                                               
BSSOK    SR    R2,R1               OWNED(-1) MINUS SOLD                         
         LA    R2,1(,R2)           NUMBER LEFT                                  
         STC   R2,PLVANS           GIVES NEW 'OWNED'                            
         IC    R2,SALEVANS         GET VANS FOR SALE                            
         AR    R2,R1               ADD THOSE SOLD                               
         STC   R2,SALEVANS         GIVES NEW 'FOR SALE'                         
         ICM   R1,15,PLMONEY       GET CURRENT PLAYER BALANCE                   
         AR    R1,R5               PLUS INCOME FROM VANS                        
         ST    R1,PLMONEY          GIVES NEW BALANCE                            
         BM    *+10                SKIP IF (STILL) IN DEBIT                     
         MVC   DBALSIGN,=C'HAVE         ' SET MESSAGE                           
         LPR   R1,R1               MAKE IT POSITIVE FOR EDIT                    
         EDIT  (R1),(11,DBALANCE),2,FLOAT=$                                     
         EDIT  PLVANS,(2,DNOVANS)  NUMBER OF VANS OWNED                         
*                                                                               
         EJECT                                                                  
*        BUYING AND SELLING OF VANS IS COMPLETE. SET UP TO DEPLOY VANS          
*                                                                               
BSEND    LA    R1,=C'STATUS'                                                    
         BAS   RE,SCRMOVE          GET STATUS SCREEN SKELETON                   
*                                   BUILD TXTMERGE PARM LIST                    
         BAS   RE,TXTSTATS         MERGE TEXT TO SCREEN                         
*                                                                               
         MVC   DMCB(8),=X'01020304050000FF' INIT TXTDEL PARM LIST               
*                                                                               
BSESPEC  CLI   OCCDESC,X'FF'       ANY SPECIAL OCCASSION ?                      
         BNE   BSEFIRST            YES - SKIP                                   
         MVI   DMCB+7-1,7          NO - DELETE MESSAGE                          
*                                                                               
BSEFIRST LH    R1,DAYCOUNT         GET DAYS INTO GAME                           
         BCT   R1,BSEDELS          SKIP IF NOT DAY ONE                          
         MVI   DMCB+6-1,6          DAY 1 DELETE YESTERDAYS PROFIT LINE          
*                                                                               
BSEDELS  LA    R1,DMCB                                                          
         BAS   RE,TXTDEL           DELETE UNWANTED TEXT                         
         EJECT                                                                  
*        DEPLOY PLAYERS VANS                                                    
*                                                                               
DEPLOY   DC    0H'0'                                                            
         BAS   RE,SCRTEXT                                                       
         LA    R8,PLWORK                                                        
*                                                                               
         XR    R7,R7                                                            
         IC    R7,CPLAYER          GET CURRENT PLAYER                           
         BCTR  R7,0                MINUS ONE                                    
         MH    R7,=AL2(LOPLDL)     TIMES LENGTH PLAYER ENTRY                    
*                                                                               
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRL11(L'STAHEAD1*2),STAHEAD1                                    
         MVC   SCRL12(L'STAHEAD2*2),STAHEAD2                                    
*                                                                               
         LA    R1,LOCATION                                                      
         LA    R2,6                                                             
         LA    R3,SCRL13                                                        
         LA    R4,3                                                             
         LA    R5,C'A'                                                          
         USING LOCDS,R1                                                         
*                                                                               
DEPINLP  LA    R6,LOPL(R7)         POINT TO PLAYERS ENTRY AT LOCATION           
         XC    0(L'LOPL,R6),0(R6)  CLEAR PLAYERS ENTRY                          
         STC   R5,1(R3)            STORE ID                                     
         MVC   3(L'LONAME,R3),LONAME STORE NAME                                 
         LA    R1,LOCDSL(,R1)      POINT TO NEXT LOCATION                       
         LA    R3,L'SCRL13+L'SCRL13H(,R3)                                       
         LA    R5,1(,R5)                                                        
         BCT   R2,DEPINLP2                                                      
         B     DEPINLPX                                                         
DEPINLP2 BCT   R4,DEPINLP                                                       
         LA    R3,SCRL13+L'STAHEAD1                                             
         LA    R4,3                                                             
         B     DEPINLP                                                          
         DROP  R1                                                               
*                                                                               
DEPINLPX IC    R2,PLVANS           GET NUMBER OF VANS                           
         MH    R2,=H'15000'        TIMES OVERHEAD PER VAN                       
         AH    R2,WHOLEICE         ADD COST OF ONE GALLON ICE-CREAM             
         S     R2,PLMONEY          MINUS MONEY WE HAVE                          
         MVI   FBUYICE,0           ASSUME CANNOT AFFORD ICE-CREAM               
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRL20(L'DEPERRM1),DEPERRM1                                      
         C     R2,PLCREDIT         CAN WE AFFORD ANY ICE CREAM ?                
         BH    DPNOMON             NO - SKIP                                    
         MVI   FBUYICE,X'FF'       YES WE CAN AFFORD ICE CREAM                  
         MVC   SCRL19(L'DEPINST1),DEPINST1                                      
         MVC   SCRL20(L'DEPINST2),DEPINST2                                      
*                                                                               
DPNOMON  MVC   SCRMESS(L'PROMESS),PROMESS                                       
         XC    SCRINP,SCRINP                                                    
GETLOCS  BAS   RE,SCRWRITE                                                      
         LA    R8,PLWORK                                                        
*                                                                               
*        VALIDATE STRING ENTERED BY USER FOR FORMAT ERRORS                      
*                                                                               
         CLI   SCRINPH+5,0         ANY INPUT ?                                  
         BE    GL100               NO - SEE WERE WE ARE SO FAR                  
         CLI   INPCOUNT,7          TOO MANY ?                                   
         BH    GLERR               YES - ERROR                                  
*                                                                               
         XR    R2,R2                                                            
         IC    R2,INPCOUNT         GET NUMBER FIELDS ENTERED                    
         LA    R1,INPAREA                                                       
         USING INPDS,R1                                                         
*                                                                               
GL001    CLI   INPL1,0             ANY MORE ENTRIES ?                           
         BE    GL100               NO - GO CHECK DEPLOYMENT                     
*                                                                               
         CLI   INPL2,0             SECOND HALF PRESENT ?                        
         BNE   GL010               YES - SKIP                                   
         CLC   =C'END ',INPDATA1   NO - IS IT 'END'                             
         BNE   GLERR               NO - ERROR                                   
         MVI   INPL1,0             YES - KNOCK IT OUT OF LIST                   
         BCT   R2,GLERR            MUST BE LAST PARAMETER                       
         B     GL100                                                            
GL010    CLI   INPL1,1             ONE BYTE ID ?                                
         BNE   GLERR               NO - ERROR                                   
         CLI   INPDATA1,C'A'       ALPHA A TO F ?                               
         BL    GLERR                                                            
         CLI   INPDATA1,C'F'       ALPHA A TO F ?                               
         BH    GLERR                                                            
*                                                                               
         TM    INPV2,X'80'         IS P2 NUMERIC ?                              
         BZ    GLERR               NO - NOGOOD                                  
         CLC   INPVAL2,=F'50'      TOO BIG                                      
         BH    GLERR               YES - ERROR                                  
         LA    R1,INPDSL(,R1)      POINT TO NEXT INPUT ENTRY                    
         BCT   R2,GL001            AND LOOP TILL ALL DONE                       
         B     GL100                                                            
GLERR    L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'ERRMESS1),ERRMESS1                                     
         B     GETLOCS                                                          
         DROP  R1                                                               
*                                                                               
*        INPUT STRING IS SYNTACTICALLY CORRECT. SET IT UP ON SCREEN             
*        AND SEE THAT PLAYER CAN DO IT.                                         
*                                                                               
GL100    XC    SCRINP,SCRINP                                                    
         XR    R2,R2                                                            
         IC    R2,INPCOUNT         GET NUMBER FIELDS ENTERED                    
         LA    R1,INPAREA                                                       
         USING INPDS,R1                                                         
*                                                                               
         XR    R7,R7                                                            
         IC    R7,CPLAYER          GET CURRENT PLAYER                           
         BCTR  R7,0                MINUS ONE                                    
         MH    R7,=AL2(LOPLDL)     TIMES LENGTH PLAYER ENTRY                    
*                                                                               
GL110    CLI   INPL1,0             END OF LIST ?                                
         BE    GL200               YES - SKIP                                   
*                                                                               
         NI    INPDATA1,X'0F'      CHANGE ALPHA TO BINARY                       
         ZIC   R6,INPDATA1         STICK IT IN REGISTER                         
         BCTR  R6,0                MINUS 1                                      
         MH    R6,=AL2(LOCDSL)     TIMES DSECT LENGTH                           
         LA    R6,LOCATION(R6)     R6 NOW POINTS TO LOCATION                    
         USING LOCDS,R6                                                         
         LA    R6,LOPL(R7)         R6 POINTS TO PLAYER/LOCATION ENTRY           
         USING LOPLDS,R6                                                        
*                                                                               
         CLI   FBUYICE,0           CAN WE AFFORD ICE-CREAM ?                    
         BE    GL115               NO - IGNORE PLAYERS INPUT                    
         MVC   LOPLMADE,INPVAL2+2  GET  GALLONS TO CARRY                        
*                                                                               
GL115    LA    R1,INPDSL(,R1)                                                   
         BCT   R2,GL110                                                         
         DROP  R6                                                               
         DROP  R1                                                               
*                                                                               
GL200    LA    R1,LOCATION                                                      
         USING LOCDS,R1                                                         
         LA    R1,LOPL(R7)                                                      
         USING LOPLDS,R1                                                        
         LA    R2,6                                                             
         LA    R3,SCRL13                                                        
         LA    R4,3                                                             
         XR    R5,R5                                                            
         LR    R6,R5                                                            
         OI    SCRL13H+6,X'80'     FORCE TRANSMITBITS ON                        
         OI    SCRL14H+6,X'80'                                                  
         OI    SCRL15H+6,X'80'                                                  
*                                                                               
GL210    EDIT  LOPLMADE,(2,26(R3)),ZERO=BLANK                                   
         NC    LOPLMADE,LOPLMADE   NUMBER OF GALLONS ZERO ?                     
         BZ    GL215               YES - SKIP                                   
         AH    R5,LOPLMADE         ACCUMULATE GALLONS MADE                      
         LA    R6,1(,R6)           AND VANS USED                                
GL215    LA    R1,LOCDSL(,R1)      POINT TO NEXT LOCATION                       
         LA    R3,L'SCRL13+L'SCRL13H(,R3)                                       
         BCT   R2,GL220                                                         
         B     GL250                                                            
GL220    BCT   R4,GL210                                                         
         LA    R3,SCRL13+L'STAHEAD1                                             
         LA    R4,3                                                             
         B     GL210                                                            
*                                                                               
GL250    L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRL17(L'DEPSTAT),DEPSTAT BUILD COSTS LINE                       
         MVI   SCRL17H+6,X'80'                                                  
         EDIT  (R5),(4,FULL)       MOVE IN TOTAL GALLONS                        
         MVC   SCRL17+6(4),FULL                                                 
         LTR   R5,R5               ANY GALLONS ?                                
         BNZ   *+8                 YES - SKIP                                   
         MVI   SCRL17+9,X'F0'      NO - SET ZERO                                
         STC   R6,VANUSED          STORE VANS USED                              
         MH    R5,WHOLEICE         CALCULATE COST OF ICE-CREAM                  
         EDIT  (R5),(8,DMCB),2,FLOAT=$                                          
         MVC   SCRL17+23(8),DMCB                                                
         IC    R6,PLVANS                                                        
         MH    R6,=H'15000'        WORK OUT OVERHEADS                           
         EDIT  (R6),(7,DMCB),2,FLOAT=$                                          
         MVC   SCRL17+43(4),DMCB                                                
         L     R2,PLMONEY          CALCULATE WHAT WE HAVE LEFT                  
         SR    R2,R5                                                            
         SR    R2,R6                                                            
         EDIT  (R2),(12,DMCB),2,FLOAT=$,MINUS=YES                               
         MVC   SCRL17+66(12),DMCB                                               
         CLI   VANUSED,0           DID WE USE ANY VANS                          
         BE    GL300               NO - SKIP                                    
         CLC   VANUSED,PLVANS      HAVE WE GOT THIS MANY VANS ?                 
         BNH   GL260               YES - SKIP                                   
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'GLVANERR),GLVANERR                                     
         B     GETLOCS                                                          
*                                                                               
GL260    L     R3,PLCREDIT                                                      
         LCR   R3,R3                                                            
         CR    R2,R3               HAVE WE EXCEEDED OUR CREDIT                  
         BNL   GL300               NO - SKIP                                    
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'GLMONERR),GLMONERR                                     
         B     GETLOCS                                                          
         DROP  R1                                                               
*                                                                               
*        EVERYTHING IS OK. IF PLAYER ENTERED END, WE CAN GO ON TO               
*        GET PRICES (NOTE R2 STILL HOLDS CURRENT BALANCE)                       
*                                                                               
GL300    XR    R1,R1                                                            
         IC    R1,INPCOUNT         GET NUMBER OF INPUT FIELDS                   
         BCTR  R1,0                MINUS ONE                                    
         MH    R1,=AL2(INPDSL)     TIMES LENGTH                                 
         LA    R1,INPAREA(R1)      POINT TO LAST ENTRY                          
         USING INPDS,R1                                                         
         CLC   =C'END ',INPDATA1   WAS LAST ENTRY EQUAL END                     
         BE    DEPLEND             YES - EXIT                                   
*                                                                               
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'GLOKMESS),GLOKMESS                                     
         CLI   VANUSED,6           DID WE USE ALL SIX LOCATIONS                 
         BE    GETLOCS             YES - GO ASK IF PLAYER HAPPY                 
         CLC   VANUSED,PLVANS      DID WE USE ALL AVAILABLE VANS                
         BNL   GETLOCS             YES - GO ASK IF PLAYER HAPPY                 
         XC    SCRMESS,SCRMESS                                                  
         MVC   SCRMESS(L'PROMESS),PROMESS                                       
         CLI   FBUYICE,0           COULD WE HAVE BOUGHT ICE CREAM ?             
         BE    GETLOCS             NO - JUST ASK FOR END                        
         MVC   SCRMESS(L'GLIDLMSS),GLIDLMSS                                     
         B     GETLOCS             ELSE WARN ABOUT IDLE VANS                    
         DROP  R1                                                               
*                                                                               
DEPLEND  DC    0H'0'                                                            
         ST    R2,PLMONEY          UPDATE PLAYERS CURRENT CASH                  
         EDIT  (R2),(11,DBALANCE),2,FLOAT=$                                     
*                                                                               
         CLI   VANUSED,0           DIS WE USE ANY VANS ?                        
         BE    PRCEND              NO - NO NEED FOR PRICES                      
*                                                                               
         LA    R1,=C'STATUS'                                                    
         BAS   RE,SCRMOVE          GET STATUS SCREEN SKELETON                   
*                                   BUILD TXTMERGE PARM LIST                    
         BAS   RE,TXTSTATS         MERGE TEXT TO SCREEN                         
*                                                                               
         MVC   DMCB(7),=X'01020304050000FF' INIT TXTDEL PARM LIST               
*                                                                               
GLSPEC   CLI   OCCDESC,X'FF'       ANY SPECIAL OCCASSION ?                      
         BNE   GLFIRST             YES - SKIP                                   
         MVI   DMCB+7-1,7          NO - DELETE MESSAGE                          
*                                                                               
GLFIRST  LH    R1,DAYCOUNT         GET DAYS INTO GAME                           
         BCT   R1,GLDELS           SKIP IF NOT DAY ONE                          
         MVI   DMCB+6-1,6          DAY 1 DELETE YESTERDAYS PROFIT LINE          
*                                                                               
GLDELS   LA    R1,DMCB                                                          
         BAS   RE,TXTDEL           DELETE UNWANTED TEXT                         
         EJECT                                                                  
*        GET ICE-CREAM PRICES                                                   
*                                                                               
PRICES   DC    0H'0'                                                            
         BAS   RE,SCRTEXT                                                       
*                                                                               
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRL11(L'STAHEAD1*2),STAHEAD1                                    
         MVC   SCRL12(L'STAHEAD2*2),STAHEAD2                                    
*                                                                               
         LA    R6,LOCATION                                                      
         LA    R2,6                                                             
         LA    R3,SCRL13                                                        
         LA    R4,3                                                             
         LA    R5,1                                                             
         USING LOCDS,R6                                                         
*                                                                               
GPINIT   STC   R5,1(R3)            STORE ID                                     
         OI    1(R3),X'C0'         CHANGE TO A TO F                             
         MVC   3(L'LONAME,R3),LONAME STORE NAME                                 
         LA    RF,LOPL(R7)         POINT TO PLAYERS ENTRY                       
         USING LOPLDS,RF                                                        
         LH    R1,LOPLMADE         GET NUMBER MADE                              
         LTR   R1,R1               ANY MADE HERE ?                              
         BZ    GPINIT0             NO - NO NEED TO FART ABOUT                   
         XR    RE,RE                                                            
         IC    RE,LONPLAY          GET PLAYERS AT THIS SITE                     
         LA    RE,1(,RE)           ADD ONE                                      
         STC   RE,LONPLAY          AND STORE NEW TOTAL                          
         MH    R1,=H'60'           CALCULATE CONES                              
         STH   R1,LOPLOK           STORE                                        
         IC    RE,PLPRFACS-1(R5)   GET LOCAL PROBLEM FACTOR                     
         C     RE,=F'100'          LOCAL PROBLEM = 100                          
         BNL   GPINIT0             YES - WONT AFFECT US                         
         S     RE,=F'100'          SUBTRACT 100 (RE MUST BE < 100)              
         LPR   RE,RE               MAKE IT POSITIVE                             
         STH   RE,RANDNUM          GET A RANDOM NUMBER IN RANGE                 
         BAS   RE,GETRAND          IDENTIFIED                                   
         LH    RE,RANDNUM                                                       
         MR    R0,RE               FIND PERCENTAGE OF ORIGINAL FACTOR           
         D     R0,=F'100'                                                       
         SH    R1,LOPLOK           REDUCTION - ORIGINAL                         
         LPR   R1,R1               COPLEMENTED                                  
         STH   R1,LOPLOK           SHOULD GIVE ORIGINAL - REDUCTION             
GPINIT0  MVC   HALF,LOPLMADE       SAVE NUMBER MADE                             
         EDIT  HALF,(2,26(R3)),ZERO=BLANK EDIT NUMBER MADE                      
         LA    R6,LOCDSL(,R6)      POINT TO NEXT LOCATION                       
         LA    R3,L'SCRL13+L'SCRL13H(,R3)                                       
         LA    R5,1(,R5)                                                        
         BCT   R2,GPINIT2                                                       
         B     GPINITX                                                          
GPINIT2  BCT   R4,GPINIT                                                        
         LA    R3,SCRL13+L'STAHEAD1                                             
         LA    R4,3                                                             
         B     GPINIT                                                           
         DROP  R6                                                               
         DROP  RF                                                               
*                                                                               
GPINITX  L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRL19(L'PRCINST1),PRCINST1                                      
         MVC   SCRL20(L'PRCINST2),PRCINST2                                      
*                                                                               
         MVC   SCRMESS(L'PROMESS),PROMESS                                       
         XC    SCRINP,SCRINP       CLEAR INPUT FIELD                            
GETPRCS  BAS   RE,SCRWRITE                                                      
         LA    R8,PLWORK                                                        
*                                                                               
         XR    R7,R7                                                            
         IC    R7,CPLAYER          GET CURRENT PLAYER                           
         BCTR  R7,0                MINUS ONE                                    
         MH    R7,=AL2(LOPLDL)     TIMES LENGTH PLAYER ENTRY                    
*                                                                               
*        VALIDATE STRING ENTERED BY USER FOR FORMAT ERRORS AND                  
*        LOCATION MISMATCHES.                                                   
*                                                                               
         CLI   SCRINPH+5,0         ANY INPUT ?                                  
         BE    GPERR               NO - ERROR                                   
         CLI   INPCOUNT,7          TOO MANY ?                                   
         BH    GPERR               YES - ERROR                                  
*                                                                               
         XR    R2,R2                                                            
         IC    R2,INPCOUNT         GET NUMBER FIELDS ENTERED                    
         LA    R1,INPAREA                                                       
         USING INPDS,R1                                                         
*                                                                               
GP001    CLI   INPL1,0             ANY MORE ENTRIES ?                           
         BE    GP100               NO - GO CHECK DEPLOYMENT                     
*                                                                               
         CLI   INPL2,0             SECOND HALF PRESENT ?                        
         BNE   GP010               YES - SKIP                                   
         CLC   =C'END ',INPDATA1   NO - IS IT 'END'                             
         BNE   GPERR               NO - ERROR                                   
         MVI   INPL1,0             YES - KNOCK IT OUT OF LIST                   
         BCT   R2,GPERR            MUST BE LAST PARAMETER                       
         B     GP100                                                            
GP010    CLI   INPL1,1             ONE BYTE ID ?                                
         BNE   GPERR               NO - ERROR                                   
         CLI   INPDATA1,C'A'       ALPHA A TO F ?                               
         BL    GPERR                                                            
         CLI   INPDATA1,C'F'       ALPHA A TO F ?                               
         BH    GPERR                                                            
*                                                                               
         TM    INPV2,X'80'         IS P2 NUMERIC ?                              
         BZ    GPERR               NO - NOGOOD                                  
         CLC   INPVAL2,=F'999'     TOO BIG                                      
         BH    GPERR               YES - ERROR                                  
*                                                                               
         NI    INPDATA1,X'0F'      CHANGE ALPHA TO BINARY                       
         ZIC   R6,INPDATA1         STICK IT IN REGISTER                         
         BCTR  R6,0                MINUS 1                                      
         MH    R6,=AL2(LOCDSL)     TIMES DSECT LENGTH                           
         LA    R6,LOCATION(R6)     R6 NOW POINTS TO LOCATION                    
         USING LOCDS,R6                                                         
         LA    R6,LOPL(R7)         R6 POINTS TO PLAYER/LOCATION ENTRY           
         USING LOPLDS,R6                                                        
*                                                                               
         NC    LOPLMADE,LOPLMADE   IS THERE A VAN HERE ?                        
         BZ    GPERR2              NO - ERROR                                   
         LA    R1,INPDSL(,R1)      POINT TO NEXT INPUT ENTRY                    
         BCT   R2,GP001            AND LOOP TILL ALL DONE                       
         B     GP100               EXIT                                         
GPERR    L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'ERRMESS1),ERRMESS1                                     
         B     GETPRCS                                                          
GPERR2   L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'GPERRMS1),GPERRMS1                                     
         B     GETPRCS                                                          
         DROP  R6                                                               
         DROP  R1                                                               
*                                                                               
*        INPUT STRING IS CORRECT. SET IT IN TABLE                               
*                                                                               
GP100    XC    SCRINP,SCRINP                                                    
         XR    R2,R2                                                            
         IC    R2,INPCOUNT         GET NUMBER FIELDS ENTERED                    
         LA    R1,INPAREA                                                       
         USING INPDS,R1                                                         
*                                                                               
GP110    CLI   INPL1,0             END OF LIST ?                                
         BE    GP200               YES - SKIP                                   
*                                                                               
         ZIC   R6,INPDATA1         STICK ID (WITH C0 SET OFF) IN REG            
         BCTR  R6,0                MINUS 1                                      
         MH    R6,=AL2(LOCDSL)     TIMES DSECT LENGTH                           
         LA    R6,LOCATION(R6)     R6 NOW POINTS TO LOCATION                    
         USING LOCDS,R6                                                         
         LA    R6,LOPL(R7)         R6 POINTS TO PLAYER/LOCATION ENTRY           
         USING LOPLDS,R6                                                        
*                                                                               
         MVC   LOPLPRI,INPVAL2+2   SET PRICE                                    
*                                                                               
         LA    R1,INPDSL(,R1)                                                   
         BCT   R2,GP110                                                         
         DROP  R6                                                               
         DROP  R1                                                               
*                                                                               
*        PUT PRICES IN SCREEN AND SEE IF ALL THERE                              
*                                                                               
GP200    LA    R1,LOCATION                                                      
         USING LOCDS,R1                                                         
         LA    R1,LOPL(R7)                                                      
         USING LOPLDS,R1                                                        
         LA    R2,6                                                             
         LA    R3,SCRL13                                                        
         LA    R4,3                                                             
         XR    R5,R5                                                            
         OI    SCRL13H+6,X'80'     FORCE TRANSMITBITS ON                        
         OI    SCRL14H+6,X'80'                                                  
         OI    SCRL15H+6,X'80'                                                  
*                                                                               
GP210    EDIT  LOPLMADE,(2,26(R3)),ZERO=BLANK                                   
         EDIT  LOPLPRI,(3,33(R3)),ZERO=BLANK                                    
         NC    LOPLMADE,LOPLMADE   ANY VAN HERE ?                               
         BZ    GP215               NO - SKIP                                    
         NC    LOPLPRI,LOPLPRI     PRICE NULL ?                                 
         BNZ   GP215               NO - SKIP                                    
         LA    R5,1(,R5)           COUNT UNPRICED LOCATIONS                     
GP215    LA    R1,LOCDSL(,R1)      POINT TO NEXT LOCATION                       
         LA    R3,L'SCRL13+L'SCRL13H(,R3)                                       
         BCT   R2,GP220                                                         
         B     GP250                                                            
GP220    BCT   R4,GP210                                                         
         LA    R3,SCRL13+L'STAHEAD1                                             
         LA    R4,3                                                             
         B     GP210                                                            
*                                                                               
GP250    LTR   R5,R5               ANY UNPRICED LOCATIONS ?                     
         BZ    GP300               NO - SKIP                                    
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'GPPRCERR),GPPRCERR                                     
         B     GETPRCS                                                          
         DROP  R1                                                               
*                                                                               
*        EVERYTHING IS OK. IF PLAYER ENTERED END, WE CAN GO ON TO               
*        NEXT PLAYER.                                                           
*                                                                               
GP300    XR    R1,R1                                                            
         IC    R1,INPCOUNT         GET NUMBER OF INPUT FIELDS                   
         BCTR  R1,0                MINUS ONE                                    
         MH    R1,=AL2(INPDSL)     TIMES LENGTH                                 
         LA    R1,INPAREA(R1)      POINT TO LAST ENTRY                          
         USING INPDS,R1                                                         
         CLC   =C'END ',INPDATA1   WAS LAST ENTRY EQUAL END                     
         BE    PRCEND              YES - EXIT                                   
*                                                                               
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'GPOKMESS),GPOKMESS                                     
         B     GETPRCS                                                          
         DROP  R1                                                               
*                                                                               
PRCEND   XC    SCRINP,SCRINP       CLEAR INPUT FIELD                            
         B     ENDPLAYR            GO SET UP NEXT PLAYER                        
*                                                                               
         EJECT                                                                  
RESPLAYR DC    0H'0'               RESTART PLAYER                               
         MVC   LOCATION(3*L'LOSAVE),LOSAVE RESTORE LOCATION DATA                
         MVC   LOCATION+(3*L'LOSAVE)(3*L'LOSAVE),LOSAVE+(3*L'LOSAVE)            
         MVC   SALEVANS,VASAVE     AND NUMBER OF VANS FOR SALE                  
         B     STAPLAYR            GO GET CURRENT PLAYER ENTRY                  
*                                                                               
ENDPLAYR DC    0H'0'               END OF PLAYER                                
         XR    R8,R8               GET CURRENT PLAYER ENTRY                     
         IC    R8,CPLAYER                                                       
         BCTR  R8,0                                                             
         MH    R8,=AL2(PLAYERDL)                                                
         LA    R8,PLAYERS(R8)                                                   
         MVC   0(PLAYERDL,R8),PLWORK COPY WORK AREA BACK                        
ENDPLAY2 XR    R1,R1                                                            
         IC    R1,CPLAYER                                                       
         LA    R1,1(,R1)           GET NEXT PLAYER NUMBER                       
         STC   R1,CPLAYER                                                       
         CLC   CPLAYER,NPLAYERS    HAVE WE DONE LAST PLAYER                     
         BNH   NXTPLAYR            NO - GO TO IT                                
         DROP  R8                                                               
         EJECT                                                                  
*        PHASE 5 - SIMULATE THE DAY'S TRADING                                   
*                                                                               
         XC    SCRHEAD,SCRHEAD     CLEAR TOP LINE                               
         OI    SCRHEADH+6,X'80'    AND FORCE WRITE                              
*                                                                               
*        FIRST DETERMINE STANDARD DEMAND                                        
*                                                                               
         LH    R1,=AL2(2500)       START WITH 1500                              
         MVC   RANDNUM,=AL2(1000)  GET A RANDOM NUMBER BETWEEN 0                
         BAS   RE,GETRAND          AND 1000                                     
         AH    R1,RANDNUM          ADD TO STANDARD                              
*                                                                               
         XR    R2,R2                                                            
         IC    R2,DAYFACT          GET DAY OF WEEK FACTOR                       
         MR    R0,R2               MULTIPLY BY FACTOR                           
         D     R0,=F'100'          AND DIVIDE BY 100                            
*                                                                               
         IC    R2,WEAFACT          GET WEATHER FACTOR                           
         MR    R0,R2               MULTIPLY BY FACTOR                           
         D     R0,=F'100'          AND DIVIDE BY 100                            
*                                                                               
         LR    RE,R1               CALCULATE WHOLESALE ICE-CREAM COST           
         SRL   RE,3                AS ONE EIGHTH DEMAND PLUS 9000P              
         AH    RE,=H'900'                                                       
         STH   RE,WHOLEICE                                                      
*                                                                               
         ST    R1,FULL                                                          
*                                                                               
*        FULL NOW HOLDS THE STANDARD DEMAND. NOW CALCULATE                      
*        DEMAND FOR EACH LOCATION.                                              
*                                                                               
         LA    R8,LOCATION         GET FIRST LOCATION                           
         USING LOCDS,R8                                                         
         LA    R6,6                AND NUMBER THEREOF                           
         LA    R5,OCCFACTS         GET SPECIAL OCCASSION FACTORS                
         LA    R4,PROFACTS         GET LOCAL PROBLEM FACTORS                    
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         LA    R3,LOCTAB           POINT TO LOCATION TABLE                      
*                                                                               
LOCDEM1  IC    R2,LOCTABL-1(R3)    GET LOCATIONS FACTOR                         
         MR    R0,R2               MULTIPLY BY FACTOR                           
         D     R0,=F'100'          DIVIDE BY 100                                
         IC    R2,0(R5)            GET LOCATIONS FACTOR                         
         MR    R0,R2               MULTIPLY BY FACTOR                           
         D     R0,=F'100'          DIVIDE BY 100                                
         LR    RF,R1               SAVE DEMAND                                  
         CLI   0(R4),100           LOCAL PROBLEM = 100                          
         BE    LOCDEM2             YES - WONT AFFECT US                         
         STH   RF,HALF             SAVE DEMAND                                  
         IC    R2,0(R4)            GET LOCAL PROBLEM FACTOR                     
         S     R2,=F'100'          SUBTRACT 100                                 
         LPR   R2,R2               MAKE IT POSITIVE                             
         STH   R2,RANDNUM          GET A RANDOM NUMBER IN RANGE                 
         BAS   RE,GETRAND          IDENTIFIED                                   
         LH    R2,RANDNUM                                                       
         MR    R0,R2               FIND PERCENTAGE OF ORIGINAL FACTOR           
         D     R0,=F'100'                                                       
         LH    RF,HALF                                                          
         CLI   0(R4),100           WAS ORIGINAL LESS THAN 100                   
         BL    *+10                YES - SKIP                                   
         AR    RF,R1               NO - ADJUST UPWARDS                          
         B     *+6                                                              
         SR    RF,R1               YES ADJUST DOWN                              
LOCDEM2  STH   RF,LODEMAND         STORE FOR THIS LOCATIONS                     
*                                                                               
         LA    R8,LOCDSL(,R8)      NEXT LOCATION                                
         LA    R3,LOCTABL(,R3)      NEXT LOCATION                               
         LA    R5,1(,R5)           NEXT FACTOR                                  
         LA    R4,1(,R4)           NEXT FACTOR                                  
         BCT   R6,LOCDEM1          LOOP FOR ALL LOCATIONS                       
         DROP  R8                                                               
*                                                                               
*        NOW FOR EACH LOCATION, ADJUST DEMAND FOR MEAN PRICE                    
*        AND ALLOCATE AS SALES TO EACH PLAYER                                   
*                                                                               
         LA    R8,LOCATION         GET FIRST LOCATION                           
         USING LOCDS,R8                                                         
         LA    R7,6                AND NUMBER THEREOF                           
*                                                                               
ALLOC    LA    R6,LOPL             POINT TO FIRST PLAYER ENTRY FOR              
         USING LOPLDS,R6           LOCATION                                     
         XR    R5,R5               SET NUMBER OF PLAYERS                        
         IC    R5,NPLAYERS         AND FIND FIRST NONZERO                       
         CLI   LONPLAY,1           ANY VANS HERE ?                              
         BL    ALLOCEND            NO - DO NEXT ONE                             
         BH    ALPL00              MORE THAN ONE - GET PRICES ETC               
ALLOC01  LH    R1,LOPLPRI          GET ONE PLAYER PRICE                         
         LTR   R1,R1               IS THIS THE ENTRY ?                          
         BNZ   ALPL01E             YES - SKIP                                   
         LA    R6,LOPLDL(,R6)      NO - LOOK AT NEXT                            
         B     ALLOC01             ONE MUST BE NON ZERO                         
*                                                                               
ALPL00   LA    R1,1000             FIND LOWEST CONE PRICE                       
         XR    R0,R0                                                            
ALPL01   LH    R2,LOPLPRI          GET PRICE                                    
         LTR   R2,R2               ZERO ?                                       
         BZ    ALPL012             YES - SKIP                                   
         CR    R2,R1               IS THIS LOWEST ?                             
         BNL   ALPL012             NO - SKIP                                    
         LR    R1,R2               YES - USE IT                                 
ALPL012  LA    R6,LOPLDL(,R6)      NEXT PLAYER                                  
         BCT   R5,ALPL01           LOOP FOR ALL PLAYERS                         
ALPL01E  XR    R2,R2               ADJUST DEMAND ACCORDING TO LOWEST            
         LR    R0,R2               PRICE CHARGED HERE                           
         L     R3,=F'5000'                                                      
         C     R1,=F'100'          IS COST LESS THAN 100P ?                     
         BNH   *+10                YES - SKIP SQUARES                           
         MR    R0,R1               SQUARE AVERAGE COST                          
         D     R0,=F'100'          ADJUST                                       
         MH    R3,LODEMAND         MULTIPLY DEMAND BY AV COST                   
         DR    R2,R1               DIVIDE INTO 5000 (STANDARD 50P CONE)         
         XR    R2,R2                                                            
         D     R2,=F'100'          ADJUST AS PERCENTAGE                         
         STH   R3,LODEMAND                                                      
*                                                                               
         CLI   LONPLAY,1           MORE THAN ONE PLAYER HERE ?                  
         BNE   ALPL02              YES - SKIP                                   
*                                                                               
         MVC   LOPLSOLD,LODEMAND   ALLOCATE ALL DEMAND TO PLAYER                
         CLC   LOPLOK,LODEMAND     DID WE HAVE ENOUGH FOR DEMAND                
         BNL   ALLOCEND            YES - SKIP                                   
         MVC   LOPLSOLD,LOPLOK     ELSE SALES EQUALS SALEABLE                   
         B     ALLOCEND                                                         
         DROP  R6                                                               
*                                                                               
ALPL02   LH    RF,LOPL+(LOPLPRI-LOPLDS)   GET PLAYER 1'S PRICE                  
         MH    RF,LOPL+(LOPLPRI-LOPLDS)   SQUARE IT                             
         LH    R1,LOPL+LOPLDL+(LOPLPRI-LOPLDS) GET PLAYER 2'S PRICE             
         MH    R1,LOPL+LOPLDL+(LOPLPRI-LOPLDS) SQUARE IT                        
         AR    RF,R1               ADD PLAYER 2 TO PLAYER 1'S                   
         CLI   NPLAYERS,2          TWO PLAYERS ?                                
         BE    ALPL02E             YES                                          
         LH    R1,LOPL+2*LOPLDL+(LOPLPRI-LOPLDS) GET PLAYER 3'S PRICE           
         MH    R1,LOPL+2*LOPLDL+(LOPLPRI-LOPLDS) SQUARE IT                      
         AR    RF,R1               ADD TO TATAL                                 
*                                                                               
ALPL02E  DC    0H'0'                                                            
*                                                                               
ALPL03   LA    R6,LOPL             ALLOCATE TO ALL PLAYERS                      
         USING LOPLDS,R6                                                        
         XR    R5,R5                                                            
         LR    R4,R5                                                            
         LR    RE,R5                                                            
         IC    R5,NPLAYERS                                                      
ALPL031  LR    R1,RF               SUM OF SQUARES OF PRICES                     
         LH    R2,LOPLPRI          GET OUR PRICE                                
         MH    R2,LOPLPRI          SQUARE IT                                    
         SR    R1,R2               SUBTRACT FROM SUM OF SQUARES                 
         SRL   R1,2                DIVIDE BY 4 FOR SAFETY                       
         MH    R1,LODEMAND         TIMES DEMAND                                 
         LR    R2,RF               SUM OF PRICES                                
         SRL   R2,1                DIVIDE BY 2                                  
         CLI   LONPLAY,2           TWO PLAYERS ?                                
         BNE   *+8                 NO - SKIP                                    
         SRL   R2,1                DIVIDE BY TWO AGAIN                          
         XR    R0,R0                                                            
         DR    R0,R2               SHOULD GIVE PLAYERS SALES                    
         STH   R1,LOPLSOLD         STORE                                        
         CH    R1,LOPLOK           HAS PLAYER MADE ENOUGH ?                     
         BNH   ALPL03E             YES - SKIP                                   
         MVC   LOPLSOLD,LOPLOK     NO - SELL WHAT WE HAVE                       
         SH    R1,LOPLSOLD         WORK OUT UNSOLD REMAINDER                    
         AR    RE,R1               AND ACCUMULATE IT                            
*                                                                               
ALPL03E  LA    R6,LOPLDL(,R6)      POINT TO NEXT                                
         BCT   R5,ALPL031          LOOP TILL ALL DONE                           
*                                                                               
         DROP  R6                                                               
         LTR   RE,RE               DID WE SELL ALL DEMAND ?                     
         BZ    ALLOCEND            YES - EASY INIT ?                            
ALPL04   LA    R1,LOPL             FIND PLAYERS NOT SOLD OUT                    
         USING LOPLDS,R1                                                        
         XR    R5,R5                                                            
         LR    R6,R5                                                            
         IC    R5,NPLAYERS                                                      
ALPL041  CLC   LOPLOK,LOPLSOLD     IS THIS SOLD OUT ?                           
         BE    ALPL045             YES - SKIP                                   
         LTR   R6,R6               HAVE WE ALREADY FOUND ONE ?                  
         BNZ   ALPL050             YES - SKIP                                   
         LR    R6,R1               ELSE SAVE HIS ADDRESS                        
ALPL045  LA    R1,LOPLDL(,R1)      POINT TO NEXT                                
         BCT   R5,ALPL041          LOOP TILL ALL DONE                           
         LTR   R6,R6               DID WE FIND ANYONE ?                         
         BZ    ALLOCEND            NO - ALL SOLD OUT                            
         XR    R1,R1               ZERO R1 (NO OTHER ENTRY)                     
         B     ALPL060             GO DO THE ONE WE FOUND                       
*                                                                               
ALPL050  CLC   LOPLPRI,LOPLPRI-LOPLDS(R6) IS THIS CHEAPEST                      
         BNH   ALPL052             YES SKIP                                     
         XR    R1,R6               ELSE SWAP SO R1= CHEAPEST                    
         XR    R6,R1                                                            
         XR    R1,R6                                                            
ALPL052  LH    RF,LOPLPRI-LOPLDS(R6)  ADD PRICES                                
         AH    RF,LOPLPRI                                                       
         LR    R3,RF               SUM OF PRICES                                
         SH    R3,LOPLPRI          MINUS OUR PRICE                              
         XR    R2,R2                                                            
         MR    R2,RE               TIMES KITTY                                  
         DR    R2,RF               SHOULD GIVE PLAYERS SALES                    
         SR    RE,R3               SUBTRACT FROM KITTY                          
         AH    R3,LOPLSOLD         ADD TO EXISTING SALES                        
         STH   R3,LOPLSOLD                                                      
         CH    R3,LOPLOK           HAS PLAYER MADE ENOUGH ?                     
         BNH   ALPL060             YES - SKIP                                   
         MVC   LOPLSOLD,LOPLOK     NO - SELL WHAT WE HAVE                       
         SH    R3,LOPLOK           AND WORK OUT WATS LEFT                       
         AR    RE,R3               PUTTING IT BACK TO KITTY                     
         DROP  R1                                                               
         USING LOPLDS,R6                                                        
ALPL060  AH    RE,LOPLSOLD         ADD R6 SOLD TO KITTY                         
         STH   RE,LOPLSOLD                                                      
         CH    RE,LOPLOK           HAS PLAYER MADE ENOUGH ?                     
         BNH   ALLOCEND            YES - THANK GOD FOR THAT                     
         MVC   LOPLSOLD,LOPLOK     NO - SELL WHAT WE HAVE                       
         SH    RE,LOPLSOLD         WORK OUT LEFTOVER                            
         LTR   R1,R1               DID WE COME FROM ALPL050 ?                   
         BZ    ALLOCEND            NO - ALL FINISHED                            
         DROP  R6                                                               
         USING LOPLDS,R1                                                        
         AH    RE,LOPLSOLD         ADD R1 SOLD TO KITTY                         
         STH   RE,LOPLSOLD                                                      
         CH    RE,LOPLOK           HAS PLAYER MADE ENOUGH ?                     
         BNH   ALLOCEND            YES                                          
         MVC   LOPLSOLD,LOPLOK     NO - SELL WHAT WE HAVE                       
*                                                                               
ALLOCEND LA    R8,LOCDSL(,R8)      NEXT LOCATION                                
         BCT   R7,ALLOC            LOOP FOR ALL LOCATIONS                       
         DROP  R1                                                               
         DROP  R8                                                               
*                                                                               
         EJECT                                                                  
*        PHASE 6 - DISPLAY RESULTS AND DECIDE WHETHER TO DO ANOTHER DAY         
*                                                                               
         LA    R1,=C'DAYSUM'       GET DAILY SUMMARY SCREEN                     
         BAS   RE,SCRMOVE                                                       
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   DUB,=X'000003040506FF'                                           
         CLI   PRODESC,X'FF'       ANY OVERALL PROBLEM ?                        
         BE    DSSNOPR             NO - SKIP                                    
         MVI   DUB+1-1,1           YES - DELETE ALL OK MESSAGE                  
         CLI   PROFACTS,100        GOOD OR BAD ?                                
         BH    DSSHOWEV            GOOD - SKIP                                  
         MVC   DMESS01,UNFLIT1     SET TO MAKE MATTERS WORSE                    
         B     DSSPPROB            GO DO PLAYERS PROBLEMS                       
DSSNOPR  MVI   DUB+2-1,2           DELETE PROB TEXT                             
DSSHOWEV MVC   DMESS01,UNFLIT2     SET HOWEVER                                  
DSSPPROB XR    RE,RE                                                            
         LA    R8,PLAYERS          FIRST PLAYER                                 
         USING PLAYERDS,R8                                                      
DSSTRPLY CLI   PLPRDESC,X'FF'      ANY PLAYER PROBLEM ?                         
         BE    DSNXTPLY                                                         
         LA    R7,PLPRFACS         POINT TO FIRST LOCATION PROBLEM              
         LR    R6,RE               OFFSET OF PLAYER ENTRY                       
         MH    R6,=AL2(LOPLDL)                                                  
         LA    R5,LOCATION         FIRST LOCATION                               
         USING LOCDS,R5                                                         
         LA    R4,6                NUMBER OF LOCATIONS                          
DSSTRLOC CLI   0(R7),100           DOES THIS AFFECT US ?                        
         BE    DSNXTLOC            NO - NEXT LOCATION                           
         LA    R3,LOPL(R6)        POINT TO PLAYER ENTRY                         
         USING LOPLDS,R3                                                        
         NC    LOPLMADE,LOPLMADE   ANY MADE HERE ?                              
         BZ    DSNXTLOC            NO - IGNORE                                  
         MVI   DUB+3-1,0           UNDELETE 3                                   
         LA    R1,DUB+3(RE)                                                     
         MVI   0(R1),0             UNDELETE 4,5 OR 6                            
         B     DSNXTPLY                                                         
DSNXTLOC LA    R5,LOCDSL(,R5)      LOOK AT NEXT LOCATION                        
         LA    R7,1(,R7)           NEXT FACTOR                                  
         BCT   R4,DSSTRLOC         LOOP IF MORE LOCATIONS                       
         DROP  R5                                                               
         DROP  R3                                                               
         DROP  R8                                                               
DSNXTPLY LA    RE,1(,RE)           NEXT PLAYER NO                               
         CLM   RE,1,NPLAYERS       ANY MORE PLAYERS ?                           
         BL    DSSTRPLY            YES - DO IT                                  
         BAS   RE,TXTDAYSM         MERGE TEXT TO SCREEN                         
         LA    R1,DUB                                                           
         BAS   RE,TXTDEL           DELETE NAUGHTY BITS                          
*                                                                               
         BAS   RE,SCRTEXT                                                       
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'ENTMESS),ENTMESS    MOVE MESSAGE                       
         XC    SCRHEAD,SCRHEAD     CLEAR TOP LINE                               
         OI    SCRHEADH+6,X'80'    AND FORCE WRITE                              
         XC    SCRINP,SCRINP       CLEAR INPUT FIELD                            
         BAS   RE,SCRWRITE         WRITE/READ SCREEN                            
*                                                                               
         TWAXC SCRL0H,PROT=Y       CLEAR SCREEN                                 
         MVC   SCRMESS(L'ENTMESS),ENTMESS    MOVE MESSAGE                       
         XC    SCRHEAD,SCRHEAD     CLEAR TOP LINE                               
         OI    SCRHEADH+6,X'80'    AND FORCE WRITE                              
*                                                                               
         L     RE,VMESSECT         POINT TO MESSAGES CSECT                      
         USING MESSECT,RE                                                       
         MVC   SCRHEAD(L'SULIT1),SULIT1                                         
         EDIT  DAYCOUNT,(4,FULL)                                                
         MVC   SCRHEAD+26(3),FULL+1                                             
         MVC   SCRHEAD+30(10),DAYNAME                                           
         MVC   SCRHEAD+41(9),=C'WEATHER -'                                      
         MVC   SCRHEAD+51(9),WEASDESC                                           
         MVC   SCRL17(L'SULIT2),SULIT2                                          
         MVC   SCRL18(L'SULIT3),SULIT3                                          
         MVC   SCRL19(L'SULIT4),SULIT4                                          
         MVC   SCRL20(L'SULIT5),SULIT5                                          
*                                                                               
         LA    RE,LOCATION         POINT TO FIRST LOCATION                      
         USING LOCDS,RE                                                         
         LA    RF,6                NUMBER OF LOCATIONS                          
         LA    R8,SCRL4            FIRST LINE                                   
         CLI   NPLAYERS,3                                                       
         BE    LSUM                                                             
         LA    R8,SCRL5                                                         
*                                                                               
LSUM     MVC   0(L'LONAME,R8),LONAME MOVE LOCATION NAME                         
         CLI   NPLAYERS,3                                                       
         BNE   LSUM2                                                            
         LA    R8,L'SCRL4+L'SCRL4H(,R8) POINT TO NEXT LINE                      
*                                                                               
LSUM2    MVI   LONPLAY,0           ZERO VANS AT THIS LOCATION                   
*                                                                               
         LA    R7,PLAYERS          FIRST PLAYER                                 
         USING PLAYERDS,R7                                                      
         ZIC   R3,NPLAYERS         NUMBER OF PLAYERS                            
         LA    R6,LOPL             FIRST PLAYER ENTRY AT LOCATION               
         USING LOPLDS,R6                                                        
         LA    R4,6(,R8)           FIRST PART OF LINE                           
         CLI   NPLAYERS,3          THREE PLAYERS ?                              
         BE    PSUM                                                             
         LA    R4,24(,R8)                                                       
*                                                                               
PSUM     MVC   0(4,R4),=C'....'                                                 
         CLI   PLBNKFLG,0          IS PLAYER BANKRUPT ?                         
         BNE   PSUMEND             YES - IGNORE HIM                             
         LH    R1,LOPLMADE         GALLONS MADE                                 
         LTR   R1,R1               ANY MADE ?                                   
         BZ    PSUMEND             NO - NOTHING TO DO HERE                      
         MH    R1,=H'60'           GIVES CONES MADE                             
         EDIT  (R1),(4,0(R4)),ZERO=BLANK                                        
         LH    R1,LOPLSOLD                                                      
         EDIT  (R1),(4,5(R4)),ZERO=BLANK NUMBER SOLD                            
         EDIT  LOPLPRI,(3,10(R4)),ZERO=BLANK  PRICE IN PENCE                    
         MH    R1,LOPLPRI          GET TOTAL INCOME IN PENCE                    
         EDIT  (R1),(7,14(R4)),2,ZERO=BLANK EDIT AS $.PP                        
         A     R1,PLINCOME         SUM TOTAL INCOME                             
         ST    R1,PLINCOME                                                      
*                                                                               
PSUMEND  LA    R7,PLAYERDL(,R7)    NEXT PLAYER                                  
         LA    R6,LOPLDL(,R6)      NEXT PLAYER                                  
         LA    R4,25(,R4)          NEXT POINT IN LINE                           
         BCT   R3,PSUM                                                          
         DROP  R6                                                               
*                                                                               
LSUMEND  LA    RE,LOCDSL(,RE)                                                   
         LA    R8,L'SCRL4+L'SCRL4H(,R8) POINT TO NEXT LINE                      
         BCT   RF,LSUM                                                          
         DROP R7                                                                
         DROP RE                                                                
*                                                                               
         LA    R8,PLAYERS          FIRST PLAYER                                 
         USING PLAYERDS,R8                                                      
         LA    R7,1                PLAYER NUMBER                                
         LA    R6,6                OFFSET TO FIRST PART OF LINE                 
         CLI   NPLAYERS,3          THREE PLAYERS ?                              
         BE    PLSUM               YES - SKIP                                   
         LA    R6,24               START FURTHER DOWN                           
*                                                                               
PLSUM    LA    R5,SCRL1(R6)                                                     
         MVC   0(10,R5),=C'PLAYER 0 -'                                          
         STC   R7,7(R5)            STORE PLAYER NUMBER                          
         OI    7(R5),X'F0'                                                      
         MVC   11(10,R5),PLNAME                                                 
         LA    R5,SCRL2(R6)                                                     
         L     RE,VMESSECT         POINT TO MESSAGES CSECT                      
         USING MESSECT,RE                                                       
         MVC   0(L'SULIT6,R5),SULIT6                                            
         LA    R5,SCRL3(R6)                                                     
         MVC   0(L'SULIT7,R5),SULIT7                                            
*                                                                               
         LA    R5,SCRL17(R6)                                                    
         MVC   13(8,R5),=C'BANKRUPT'                                            
         CLI   PLBNKFLG,0          IS PLAYER BANKRUPT ?                         
         BNE   PLSUMEND            YES - IGNORE HIM                             
         L     R2,PLMONEY          CASH BEFORE TRADING                          
         EDIT  (R2),(12,10(R5)),2,FLOAT=$,MINUS=YES   EDIT AS $.PP-             
         LA    R5,SCRL18(R6)                                                    
         L     R3,PLINCOME         INCOME FROM TRADING                          
         EDIT  (R3),(11,10(R5)),2,FLOAT=$             EDIT AS $.PP-             
         LA    R5,SCRL19(R6)                                                    
         AR    R2,R3               GIVES NEW BALANCE                            
         ST    R2,PLMONEY                                                       
         EDIT  (R2),(12,10(R5)),2,FLOAT=$,MINUS=YES   EDIT AS $.PP-             
         LA    R5,SCRL20(R6)                                                    
         S     R2,PLSTRMON         GIVES PROFIT FOR THE DAY                     
         EDIT  (R2),(12,10(R5)),2,FLOAT=$,MINUS=YES   EDIT AS $.PP-             
         MVC   PLPRFS(6*L'PLPRFS),PLPRFS+L'PLPRFS MOVE PROFITS BACK             
         ST    R2,PLYEPRF          STORE LATEST VALUE                           
*                                                                               
         LTR   R2,R2               DID WE MAKE A PROFIT ?                       
         BP    PLSUMEND            YES - SKIP                                   
         L     R3,PLCREDIT         GET CREDIT LIMIT                             
         LNR   R3,R3               MAKE IT NEGATIVE                             
         C     R3,PLMONEY          ARE WE OVER CREDIT LIMIT ?                   
         BL    PLSUMEND            NO - OK SKIP                                 
*                                                                               
         CLI   PLVANS,1            HAVE WE VANS TO SELL ?                       
         BNE   PLSUMEND            YES - OK                                     
*                                                                               
         MVI   PLBNKFLG,X'FF'      FLAG BANKRUPT                                
         LA    R5,SCRL19(R6)       FLAG PLAYER AS BANKRUPT                      
         XC    10(12,R5),10(R5)                                                 
         MVC   13(8,R5),=C'BANKRUPT'                                            
*                                                                               
PLSUMEND LA    R8,PLAYERDL(,R8)    NEXT PLAYER                                  
         LA    R6,25(,R6)          NEXT POINT IN LINE                           
         LA    R7,1(,R7)           NEXT PLAYER NUMBER                           
         CLM   R7,1,NPLAYERS       DONE ALL PLAYERS ?                           
         BNH   PLSUM               NO - SKIP                                    
         DROP  R8                                                               
*                                                                               
         XR    R1,R1                                                            
         IC    R1,DAYNUM           GET DAY OF WEEK                              
         LA    R2,1(,R1)           ADD ONE TO IT                                
         STC   R2,DAYNUM           RESTORE IT                                   
         CLI   DAYNUM,8            IF MORE THAN 7,                              
         BL    *+8                                                              
         MVI   DAYNUM,1            RESET IT TO 1                                
*                                                                               
         MVC   SCRMESS(L'ENTMESS),ENTMESS    MOVE MESSAGE                       
         XC    SCRINP,SCRINP       CLEAR INPUT FIELD                            
         BAS   RE,SCRWRITE                                                      
*                                                                               
         LA    R8,PLAYERS          FIRST PLAYER                                 
         USING PLAYERDS,R8                                                      
         ZIC   R7,NPLAYERS         AND NUMBER THEREOF                           
BNKLOOP  CLI   PLBNKFLG,0          IS PLAYER BANKRUPT ?                         
         BE    NEWDAY              NO - DO ANOTHER DAY                          
         LA    R8,PLAYERDL(,R8)    NEXT PLAYER                                  
         BCT   R7,BNKLOOP          CHECK ALL PLAYERS                            
         DROP  R8                                                               
         EJECT                                                                  
*        PHASE 7 - END OF GAME - PREPARE FOR ANOTHER ONE                        
*                                                                               
*        FINAL GAME END SCREEN                                                  
*                                                                               
         LA    R1,=C'ENDSUM'       GET GAME END SCREEN                          
         BAS   RE,SCRMOVE                                                       
         BAS   RE,SCRTEXT                                                       
         L     RE,VMESSECT         POINT TO MESSAGE CSECT                       
         USING MESSECT,RE                                                       
         MVC   SCRMESS(L'ENTMESS),ENTMESS    MOVE MESSAGE                       
         XC    SCRHEAD,SCRHEAD     CLEAR TOP LINE                               
         OI    SCRHEADH+6,X'80'    AND FORCE WRITE                              
         XC    SCRINP,SCRINP       CLEAR INPUT FIELD                            
         BAS   RE,SCRWRITE         WRITE/READ SCREEN                            
*                                                                               
RESTART  GOTO1 VCALLOV,DMCB,(X'FF',SCREEN),0 RELOAD INIT SCREEN                 
         CLI   DMCB+4,X'FF'        LOAD ERROR ?                                 
         BNE   *+6                 NO - OK                                      
         DC    H'0'                DOWNWEGO - PHASE LOAD ERROR                  
*                                                                               
         MVI   DAYNUM,0            SET TO START AGAIN                           
*                                                                               
         LA    RE,INIT             DUMMY RETURN ADDRESS                         
         B     SCRWRITE            REWRITE INIT SCREEN                          
         EJECT                                                                  
*        TO WRITE A SCREEN USE 'BAL RE,SCRWRITE'. RE IS CONVERTED TO            
*        AN OFFSET AND SAVED. ON RE-ENTRY, SAVED OFFSET IS RECONVERTED          
*        AND PROGRAM BRANCHES TO INSTRUCTION AFTER 'BAL'. REGISTERS             
*        ARE NOT SAVED. WORK SPACE BEYOND LABEL WSAVE IS WRITTEN TO             
*        TEMP STORAGE (MAX 2304 BYTES)                                          
*                                                                               
SCRWRITE DC    0H'0'                                                            
         LA    RE,0(,RE)           ZERO HIGH BYTE                               
         LA    RF,ICEGAME                                                       
         SR    RE,RF               SUBTRACT PROGRAM ADDRESS                     
         STH   RE,RETRNOFF         SAVE RETURN OFFSET                           
*                                                                               
         LH    R2,TWATRM           GET TERMINAL ID                              
         ICM   R2,4,=X'FF'         ASK FOR 2560 BYTES                           
         LA    R3,WSAVE            GET SAVE ADDRESS                             
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(1,(R2)),(R3)                
*                                                                               
         MVC   SCRPRO,=C'==>'                                                   
         OI    SCRINPH+6,X'C1'     FORCE MDTON AND WRITE INPUT                  
         SPACE 3                                                                
EXIT     XMOD1 1                                                                
         TITLE 'SUBROUTINES'                                                    
*        ALL SUBROUTINES USE NTR1 AND EXIT VIA LABEL EXIT                       
         SPACE 3                                                                
*        SUBROUTINE GETEVENT                                                    
*                                                                               
*        ON ENTRY -                                                             
*              R1=A(TABLE ACRONYM)                                              
*        ON EXIT -                                                              
*              DMCB(4)=A(RANDOMLY SELECTED TABLE ENTRY)                         
*                                                                               
*        NOTE - REFER TO GAICE01 PHASE FOR DETAILED FORMAT OF TABLES            
*                                                                               
GETEVENT NTR1                                                                   
*                                                                               
S001     LR    R6,R1               SAVE PARAMETER ADDRESS                       
*                                                                               
*        READ TIA (EVENT TABLE) BACK IN FROM LAST EPISODE                       
*        THIS SHOULD HELP RANDNUM TO PRODUCE MORE RANDOM NUMBERS                
*                                                                               
         LH    R2,TWATRM           GET TERMINAL ID                              
         ICM   R2,4,=X'FF'         ASK FOR 2560 BYTES                           
         L     R3,AEVENTS          GET TABLE ADDRESS (TIA)                      
         XR    R4,R4                                                            
         IC    R4,TIARECS          GET NUMBER OF RECORDS TO READ                
S001GETT LA    R5,1(,R4)           USE RECORD TWO ONWARDS                       
         GOTO1 VDATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',((R5),(R2)),(R3)            
         LA    R3,2560(,R3)                                                     
         BCT   R4,S001GETT         LOOP TILL ALL SAVED                          
*                                                                               
*        SCAN INDEX TABLE FOR OFFSET OF REQUESTED TABLE                         
*                                                                               
         MVI   EVENTCH,0           INDICATE TABLE NOT CHANGED                   
         L     R3,AEVENTS          GET EVENT TABLE INDEX ADDRESS                
         LA    R3,8(,R3)           SKIP PAST ID FIELD                           
S001FTAB CLC   0(6,R6),2(R3)       SEARCH FOR TABLE                             
         BE    S001GTAB            FOUND IT                                     
         LA    R3,8(,R3)           LOOK AT NEXT INDEX ENTRY                     
         CLI   0(R3),X'FF'         END OF TABLE ?                               
         BNE   S001FTAB            NO SEARCH ON                                 
         DC    H'0'                DOWNWEGO IF TABLE NOT FOUND                  
*                                                                               
*        TABLE FOUND. SET UP 'BXLE' REGS. R3=HEADER,R4=LENGTH,R5=END,           
*        R6=CURRENT ENTRY                                                       
*                                                                               
S001GTAB AH    R3,0(R3)            POINT TO TABLE HEADER                        
         LH    R4,2(,R3)           GET LENGTH EACH ENTRY                        
         LR    R5,R3               GET ADDRESS OF END OF TABLE                  
         AH    R5,0(,R5)             INTO R5                                    
         LA    R6,6(,R3)           GET ADDRESS FIRST ENTRY                      
*                                                                               
*        IF TABLE NOT YET INITIALIZED, (TOTAL PROBABILITY ZERO), SET            
*        TOTAL PROBABILITY TO SUM OF ENTRY'S INDIVIDUAL PROBABILITIES           
*                                                                               
         NC    4(2,R3),4(R3)       IS TABLE INITIALIZED ?                       
         BNZ   S001TBOK            YES - SKIP                                   
         MVI   EVENTCH,X'FF'       INDICATE TABLE CHANGED                       
         XR    R1,R1                                                            
S001INIT AH    R1,0(,R6)           SUM EACH ENTRY'S PROBABILITY                 
         BXLE  R6,R4,S001INIT                                                   
         BNZ   *+6                 CHECK IF SUM ZERO - BOOBOO                   
         DC    H'0'                DOWNWEGO - ALL ENTRIES 'DELETED'             
         STH   R1,4(,R3)           STORE TOTAL PROBABILITY                      
         LA    R6,6(,R3)           RESET TO FIRST ENTRY                         
*                                                                               
*        OBTAIN A RANDOM NUMBER NOT GREATER THAN THE TOTAL PROBABILITY          
*        OF ALL THE ENTRIES, AND USE THIS AS A PSEUDO INDEX INTO THE            
*        TABLE. IF EACH ENTRY WERE REPEATED THE NUMBER OF TIMES SPECI-          
*        FIED IN ITS PROBABILITY, THE RANDOM NUMBER WOULD BE A TRUE             
*        INDEX POINTER                                                          
*                                                                               
S001TBOK MVC   RANDNUM,4(R3)       USE TOTAL PROBABILITY AS RANGE AND           
         BAS   RE,GETRAND          GET RANDOM NUMBER                            
         XR    R1,R1                                                            
S001SRCH AH    R1,0(,R6)           SUM EACH ENTRY'S PROBABILITY TILL            
         CH    R1,RANDNUM          RANDOM NUMBER EXCEEDED WHICH GIVES           
         BNL   S001FND             REQUIRED ENTRY                               
         BXLE  R6,R4,S001SRCH                                                   
S001FND  ST    R6,FULL             SAVE ADDRESS OF TABLE ENTRY                  
*                                                                               
*        SOME ENTRIES MAY BE 'ONCE OR TWICE-ONLY' ENTRIES. IF ONE OF            
*        THESE IS SELECTED. (COUNTDOWN NOT ZERO). THE COUNTDOWN IS              
*        DECREMENTED. IF THIS THEN BECOMES ZERO THE ENTRY IS DELETED            
*        BY ZEROING ITS PROBABILITY AND ADJUSTING THE TABLES TOTAL              
*        PROBABILITY ACCORDINGLY                                                
*                                                                               
         ICM   R4,3,2(R6)          GET ENTRY'S COUNTDOWN VALUE                  
         BZ    S001EXIT            IF ZERO, NO NEED TO COUNTDOWN                
         MVI   EVENTCH,X'FF'       INDICATE TABE CHANGED                        
         BCT   R4,S001UPDT         COUNTDOWN ONE, BR IF NOT ZERO                
         LH    R5,4(,R3)           IF ZERO, DELETE ENTRY BY SUBTRACTING         
         SH    R5,0(,R6)           ITS PROBABILITY FROM TOTAL AND               
         XC    0(2,R6),0(R6)       ZEROING IT                                   
         STH   R5,4(,R3)           STORE NEW TOTAL                              
S001UPDT STH   R4,2(,R6)           AND STORE NEW COUNTDOWN VALUE                
*                                                                               
*        SAVE CURRENT TIA BACK IF ITS CHANGED                                   
*                                                                               
S001EXIT MVC   DMCB(4),FULL        SET RETURN PARAMETER                         
         CLI   EVENTCH,0           DID TABLE CHANGE ?                           
         BE    EXIT                NO - SKIP                                    
         LH    R2,TWATRM           GET TERMINAL ID                              
         ICM   R2,4,=X'FF'         ASK FOR 2560 BYTES                           
         L     R3,AEVENTS          GET TABLE ADDRESS (TIA)                      
         XR    R4,R4                                                            
         IC    R4,TIARECS          GET NUMBER OF RECORDS TO SAVE                
S001PUTT LA    R5,1(,R4)           USE RECORD TWO ONWARDS                       
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',((R5),(R2)),(R3)             
         LA    R3,2560(,R3)                                                     
         BCT   R4,S001PUTT         LOOP TILL ALL SAVED                          
         MVC   DMCB(4),FULL       RESET RETURNED FIELD                          
         B     EXIT                                                             
         EJECT                                                                  
*        SUBROUTINE GETRAND                                                     
*                                                                               
*        ON ENTRY -                                                             
*              RANDNUM = HALFWORD RANDOM NUMBER RANGE                           
*        ON EXIT -                                                              
*              RANDNUM = HALFWORD RANDOM NUMBER IN RANGE 0 TO RANGE             
*                                                                               
GETRAND  NTR1                                                                   
*                                                                               
S002     LH    RF,RANDNUM                                                       
         GOTO1 VRANDOM,DMCB,(RF),RR=RELOFACT                                    
         MVC   RANDNUM,DMCB+6                                                   
         B     EXIT                                                             
         EJECT                                                                  
*        SUBROUTINE SCRTEXT                                                     
*                                                                               
*        ON ENTRY -                                                             
*              TEXTWRK CONTAINS A STRING OF TEXT.                               
*        ON EXIT -                                                              
*              TWA = TEXT FROM TEXTWRK IN SCREEN FORMAT WITH MULTIPLE           
*                    BLANKS REMOVED,X'00' BYTES TREATED AS LINE-END AND         
*                    LINES SPLIT AT SPACES (SO WORDS DONT CROSS LINES)          
*              TEXTWRK CONTAINS BLANKS                                          
*              DMCB+0(1) = NUMBER OF LINES LEFT                                 
*              DMCB+1(3) = ADDRESS NEXT SCREEN LINE                             
*                                                                               
SCRTEXT  NTR1                                                                   
*                                                                               
*        INITIALIZE FOR TEXT SCAN                                               
*                                                                               
S003     LA    R4,TEXTWRK          FIND START OF TEXT                           
         LA    R5,L'TEXTWRK        LENGTH OF TEXT WORK AREA                     
S003STRT CLI   0(R4),X'FE'         CLEAR OUT FE AND FF                          
         BL    *+8                                                              
         MVI   0(R4),X'40'                                                      
         LA    R4,1(,R4)                                                        
         BCT   R5,S003STRT                                                      
         MVI   TEXTSTOP,C' '       MAKE SURE WE DONT OVERRUN                    
         LA    R2,SCRL0H           POINT TO FIRST SCREEN LINE HEADER            
         LA    R3,21               NUMBER OF SCREEN LINES                       
         LA    R4,TEXTWRK          FIND START OF TEXT                           
         LA    R5,L'TEXTWRK        LENGTH OF TEXT WORK AREA                     
*                                                                               
*        INIT NEW SCREEN LINE                                                   
*                                                                               
S003STLN XC    8(L'SCRL1,R2),8(R2) CLEAR LINE                                   
         OI    6(R2),X'80'         SETON TRANSMIT BIT                           
         LA    R6,L'SCRL1          SET REMAINING LENGTH LINE                    
*                                                                               
*        LOOK FOR START OF WORD (GREATER THAN BLANK)                            
*                                                                               
S003FIND CLI   0(R4),X'40'         IS THIS START OF WORD ?                      
         BH    S003STWR            YES - GO SCAN WORD                           
         BE    S003BLNK            NO - ITS A BLANK - IGNORE IT                 
         MVI   0(R4),C' '          CHANGE NEW LINE FLAG TO BLANK                
         B     S003NLIN            GO DO NEW LINE                               
S003BLNK LA    R4,1(,R4)           POINT TO NEXT BYTE IN TEXT                   
         BCT   R5,S003FIND         CONTINU SCAN IF NOT END OF TEXT              
*                                                                               
*        ALL TEXT DONE, CLEAR REST OF SCREEN                                    
*                                                                               
S003CLR  XC    DMCB(4),DMCB        CLEAR NEXT LINE INFO                         
         LA    R2,8(R2,R6)         POINT TO NEXT LINE                           
         BCT   R3,S003CLR2         CONTINUE UNTIL ALL LINES DONE                
         B     S003EXIT                                                         
S003CLR2 ST    R2,DMCB             STORE NEXT LINE ADDRESS                      
         STC   R3,DMCB             STORE LINES LEFT                             
         LA    R6,L'SCRL1+8        SET LENGTH LINE                              
S003CLR3 XC    8(L'SCRL1,R2),8(R2) CLEAR REST OF LINES                          
         OI    6(R2),X'80'         AND SETON TRANSMIT BITS                      
         AR    R2,R6               POINT TO NEXT LINE                           
         BCT   R3,S003CLR3         CONTINUE UNTIL ALL LINES DONE                
         B     S003EXIT                                                         
*                                                                               
*        LOCATE NEXT LINE                                                       
*                                                                               
S003NLIN LA    R2,8(R2,R6)         POINT TO NEXT LINE                           
         BCT   R3,S003STLN         START NEW LINE IF ANY LEFT                   
         DC    H'0'                DOWNWEGO - SCREEN FULL                       
*                                                                               
*        SCAN FOR END OF WORD TO DETERMINE LENGTH                               
*                                                                               
S003STWR LR    RF,R4               SAVE WORD ADDRESS                            
         XR    R7,R7               ZERO WORD LENGTH                             
S003SCWR LA    RF,1(,RF)           POINT TO NEXT BYTE OF WORD                   
         LA    R7,1(,R7)           COUNT LENGTH                                 
         CLI   0(RF),X'40'         WORD DELIMETER (BLANK/ZERO)                  
         BH    S003SCWR            NO - CONTINUE WORD SCAN                      
         CH    R7,=AL2(L'SCRL1)    WORD TOO LONG ?                              
         BNH   *+6                 NO - GO MOVE IT                              
         DC    H'0'                DOWNWEGO - WORD TOO LONG FOR LINE            
         CR    R7,R6               IS THERE ROOM ON LINE ?                      
         BH    S003NLIN            NO - GO GET NEXT LINE                        
*                                                                               
*        MOVE WORD INTO LINE AND UPDATE POINTERS                                
*                                                                               
S003MWRD LR    R1,R7               GET WORD LENGTH                              
         BCTR  R1,0                GET WORD LENGTH MINUS ONE                    
         EX    R1,*+8              MOVE WORD TO LINE                            
         B     *+10                                                             
         MVC   8(0,R2),0(R4)                                                    
         LR    R4,RF               SET NEXT WORD POSITION                       
         SR    R5,R7               UPDATE REMAINING TEXT LENGTH                 
         SR    R6,R7               GET LINE LENGTH LEFT                         
         AR    R2,R7               GET NEW LINE POSITION                        
         LTR   R5,R5               HAVE WE REACHED END ?                        
         BNP   S003CLR             YES CLEAR REST OF SCREEN                     
         LTR   R6,R6               END OF LINE ?                                
         BNP   S003NLIN            YES - GET A NEW ONE                          
         LA    R2,1(,R2)           ALLOW FOR A BLANK AFTER WORD                 
         BCT   R6,S003FIND         DECREASE FOR BLANK, GET NEXT WORD            
         B     S003NLIN            IF LINE END NOW, GET NEXT LINE               
*                                                                               
S003EXIT B     EXIT                                                             
         EJECT                                                                  
*        SUBROUTINE SCRMOVE                                                     
*                                                                               
*        ON ENTRY -                                                             
*              R1 = A(NAME) OF SCREEN IN GAICE02.                               
*        ON EXIT -                                                              
*              TEXTWRK CONTAINS SCREEN FROM GAICE02                             
*                                                                               
SCRMOVE  NTR1                                                                   
*                                                                               
*        LOCATE DATA DEFINED BY R1                                              
*                                                                               
S004     DC    0H'0'                                                            
         LR    R2,R1                                                            
         L     R4,AEVENTS                                                       
         GOTO1 VCALLOV,DMCB,(2,(R4)),0                                          
         CLI   DMCB+4,X'FF'        LOAD ERROR ?                                 
         BNE   *+6                 NO - OK                                      
         DC    H'0'                DOWNWEGO - PHASE LOAD ERROR                  
S004FSCR CLC   0(6,R2),2(R4)       SEARCH FOR TABLE                             
         BE    S004MSCR            FOUND IT                                     
         LA    R4,8(,R4)           LOOK AT NEXT ENTRY                           
         CLI   0(R4),X'FF'         END OF TABLE ?                               
         BNE   S004FSCR            NO SEARCH ON                                 
         DC    H'0'                DOWNWEGO IF ENTRY NOT FOUND                  
*                                                                               
*        SCREEN FOUND. MOVE INTO TEXT FIELD                                     
*                                                                               
S004MSCR AH    R4,0(R4)            POINT AT DATA                                
         LA    R5,X'40'            FILL WITH BLANKS                             
         SLL   R5,3*8                                                           
         ICM   R5,3,0(R4)          GET LENGTH OF DATA                           
         LA    R4,2(,R4)           GET START OF DATA                            
         LA    R2,TEXTWRK          ADDRESS OF MOVE TARGET                       
         LA    R3,L'TEXTWRK        COPY LENGTH                                  
         MVCL  R2,R4                                                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        SUBROUTINE TXTMERGE                                                    
*                                                                               
*        ON ENTRY -                                                             
*              R1=ADDRESS OF PLIST. EACH PARM IS A STRING TO BE MERGED          
*              WITH STRING IN TEXTWRK. PLIST IS A STRING OF FULLWORD            
*              ADDRESSES, WITH THE HIGH ORDER BYTE GIVING THE LENGTH            
*              WHICH MUST BE LESS THAN 128. END OF PLIST IS SET BY              
*              X'80' BIT ON IN BYTE FOLLOWING LAST ENTRY. STRINGS ARE           
*              MERGED TO REPLACE X'FF' BYTES                                    
*        ON EXIT -                                                              
*              TEXTWRK CHANGED AS PER PLIST                                     
*                                                                               
TXTSTATS NTR1                                                                   
*                                                                               
         GOTO1 ,DMCB,                                                  X        
               (LDAYNAME,DAYNAME),                                     X        
               (LWEADESC,WEADESC),                                     X        
               (LDSELLVA,DSELLVAN),                                    X        
               (LDBUYFLA,DBUYFLAG),                                    X        
               (LDBUYPRC,DBUYPRC),                                     X        
               (LDSELPRC,DSELPRC),                                     X        
               (LDNOVANS,DNOVANS),                                     X        
               (LDBALSIG,DBALSIGN),                                    X        
               (LDBALANC,DBALANCE),                                    X        
               (LDCREDIT,DCREDIT),                                     X        
               (LDPRFLSS,DPRFLSS),                                     X        
               (LDPROFIT,DPROFIT),                                     X        
               (LDWHOLES,DWHOLES),                                     X        
               (LOCCDESC,OCCDESC),                                     X        
               (X'80',0)                                                        
         B     S005                                                             
*                                                                               
TXTDAYSM NTR1                                                                   
*                                                                               
         GOTO1 ,DMCB,                                                  X        
               (LDAYNAME,DAYNAME),                                     X        
               (LOCCSDES,OCCSDESC),                                    X        
               (LWEASDES,WEASDESC),                                    X        
               (LPRODESC,PRODESC),                                     X        
               (LDMESS01,DMESS01),                                     X        
               (LPLNAME,PLAYERS+PLNAME-PLAYERDS),                      X        
               (LPLPRDES,PLAYERS+PLPRDESC-PLAYERDS),                   X        
               (LPLNAME,PLAYERS+PLNAME-PLAYERDS+PLAYERDL),             X        
               (LPLPRDES,PLAYERS+PLPRDESC-PLAYERDS+PLAYERDL),          X        
               (LPLNAME,PLAYERS+PLNAME-PLAYERDS+(2*PLAYERDL)),         X        
               (LPLPRDES,PLAYERS+PLPRDESC-PLAYERDS+(2*PLAYERDL)),      X        
               (X'80',0)                                                        
         B     S005                                                             
*                                                                               
TXTMERGE NTR1                                                                   
*                                                                               
*        INITIALIZE FOR MERGE. R1 ALREADY POINTS TO FIRST PLIST ENTRY           
*                                                                               
S005     LA    R2,TEXTWRK          POINT TO TEXTWRK AREA                        
         LA    R3,L'TEXTWRK        GET LENGTH                                   
S005SCAN CLI   0(R2),X'FF'         IS THIS A MERGE CHARACTER ?                  
         BE    S005MR01            YES - GO START MERGE                         
S005NEXT LA    R2,1(,R2)           NEXT BYTE TEXTWRK                            
         BCT   R3,S005SCAN         LOOP TILL TEXTWRK END                        
         B     EXIT                THATS IT FOLKS                               
*                                                                               
S005MR01 XR    R5,R5               ZERO LENGTH OF STRING                        
         TM    0(R1),X'80'         HAVE WE EMPTIED PLIST ?                      
         BO    S005MR15            YES - GO MOVE SPACES                         
         L     R4,0(,R1)           ADDRESS OF STRING FROM PLIST                 
         IC    R5,0(R1)            AND LENGTH                                   
S005MR10 MVC   0(1,R2),0(R4)       SUBSTITUTE ONE BYTE FROM STRING              
         LA    R4,1(,R4)           NEXT STRING BYTE                             
         BCTR  R5,0                DECREMENT STRING LENGTH                      
         B     S005MR20            GO CHECK NEXT BYTE                           
S005MR15 MVI   0(R2),C' '          SUBSTITUTE A BLANK                           
S005MR20 LA    R2,1(R2)            POINT TO NEXT BYTE OF TEXTWRK                
         BCT   R3,S005MR25         SKIP IF NOT END                              
         B     EXIT                ELSE DONE ENOUGH                             
S005MR25 CLI   0(R2),X'FF'         IS THIS TO BE SUBSTITUTED ?                  
         BNE   S005MEND            NO - SKIP                                    
         LTR   R5,R5               IS STRING EXHAUSTED ?                        
         BZ    S005MR15            YES - SUBSTITUTE BLANK                       
         B     S005MR10            ELSE SUBSTITUTE NEXT STRING BYTE             
*                                                                               
S005MEND TM    0(R1),X'80'         ARE WE AT PLIST END ?                        
         BO    S005NEXT            YES - SKIP                                   
         LA    R1,4(R1)            POINT TO NEXT PLIST ENTRY                    
         B     S005NEXT            AND LOOK FOR NEXT STRING                     
         B     EXIT                                                             
         EJECT                                                                  
*        SUBROUTINE TXTDEL                                                      
*                                                                               
*        ON ENTRY -                                                             
*              R1=ADDRESS OF DELETE LIST. EACH BYTE IS AN ID TO BE              
*              MATCHED TO A STRING OF TEXT IN TEXTWRK ENCLOSED BY THE           
*              CHARACTERS X'FE..' AT THE FRONT AND X'FEFE' AT THE BACK          
*              WHERE THE ID'S MATCH, THE ENCLESED STRING IS CHANGED TO          
*              SPACES, OTHERWISE, IT IS LEFT ALONE. IN ANY CASE, THE            
*              HEADER AND TRAILER BYTES ARE BLANKED OUT. ITS ASSUMED            
*              THAT ID'S ARE NUMERICALLY CONSECUTIVE AND THAT ENCLOSED          
*              STRINGS DONT OVERLAP. THE DELETE LIST SHOULD END WITH            
*              X'FF', X'0O' IN DELETE LIST IS IGNORED                           
*        ON EXIT -                                                              
*              TEXTWRK CHANGED AS PER DELETE LIST                               
*                                                                               
TXTDEL   NTR1                                                                   
*                                                                               
*        INITIALIZE FOR DELETE. R1 ALREADY POINTS TO FIRST DELETE ID            
*                                                                               
S006     LA    R2,TEXTWRK          POINT TO TEXTWRK AREA                        
         LA    R3,L'TEXTWRK        GET LENGTH                                   
S006ZERO CLI   0(R1),0             ZERO BYTE ?                                  
         BNE   S006SCAN            NO - GO FIND IT                              
         LA    R1,1(,R1)           YES - IGNORE IT                              
         B     S006ZERO                                                         
S006SCAN CLI   0(R2),X'FE'         IS THIS A DELETE CHARACTER ?                 
         BE    S006DE01            YES - GO SEE IF TO BE DELETED                
S006NEXT LA    R2,1(,R2)           NEXT BYTE TEXTWRK                            
         BCT   R3,S006SCAN         LOOP TILL TEXTWRK END                        
         B     EXIT                THATS IT FOLKS                               
*                                                                               
S006DE01 CLC   1(1,R2),0(R1)       DOES ID MATCH PARM LIST ?                    
         BE    S006DE10            YES - START DELETING STRING                  
S006DE05 MVC   0(2,R2),=C'  '      ELSE DELETE FIELD ID                         
         B     S006NEXT            AND GO LOOK FOR NEXT                         
S006DE10 MVI   0(R2),C' '          BLANK OUT BYTE                               
         LA    R2,1(,R2)           POINT TO NEXT ONE                            
         BCT   R3,S006DE15         SKIP IF NOT END OF TEXTWRK                   
         B     EXIT                ELSE EXIT                                    
S006DE15 CLI   0(R2),X'FE'         END OF DELETE STRING ?                       
         BNE   S006DE10            NO CONTINUE TO DELETE                        
S006DE20 LA    R1,1(,R1)           POINT TO NEXT BYTE OF DELETE LIST            
         CLI   0(R1),0             ZERO BYTE ?                                  
         BE    S006DE20            YES - IGNORE IT                              
         B     S006DE05            GO AND CLEAR DELETE BYTES                    
         TITLE 'LITERALS AND STORAGE'                                           
*                                                                               
*        FOLLOWING DSECTS ARE HERE AS THEY AFFECT SUBSEQUENT STORAGE            
*        INCLUDING LITERALS                                                     
*                                                                               
PLAYERDS DSECT COVERS PLAYER STATUS TABLE ENTRY                                 
*                                                                               
PLMONEY  DS    F                   CURRENT BALANCE                              
PLSTRMON DS    F                   BALANCE AT START OF DAY                      
PLINCOME DS    F                   TODAYS INCOME ACCUMULATOR                    
PLCREDIT DS    F                   TODAYS CREDIT LIMIT                          
PLPRFS   DS    7F                  LAST SEVEN DAYS PROFITS                      
         ORG   *-4                                                              
PLYEPRF  DS    F                   YESTERDAYS PROFIT                            
PLNAME   DS    CL10                PLAYERS NAME                                 
PLVANS   DS    X                   NUMBER OF VANS OWNED                         
PLPROB   DS    0CL80   PLAYERS PROBLEM RECORD FROM 'PPROBL' TABLE               
PLPRFACS DS    6X                  PERCENTAGE IMPACT ON EACH LOCATION           
PLPRDESC DS    CL74                PROBLEM DESCRIPTION                          
PLBNKFLG DS    X                   BANKRUPTY FLAG 0 = NOT BANKRUPT              
*                                                                               
PLAYERDL EQU   *-PLAYERDS                                                       
         SPACE 3                                                                
LOPLDS   DSECT COVERS LOCATION STATUS TABLE PLAYER ENTRY                        
*                                                                               
LOPLMADE DS    H                   GALLONS ICE-CREAM MADE                       
LOPLOK   DS    H                   CONES SALEABLE                               
LOPLSOLD DS    H                   CONES SOLD                                   
LOPLPRI  DS    H                   SELLING PRICE                                
*                                                                               
LOPLDL   EQU   *-LOPLDS                                                         
         SPACE 3                                                                
LOCDS    DSECT COVERS LOCATION STATUS TABLE ENTRY                               
*                                                                               
LONAME   DS    CL20                LOCATION NAME                                
LODEMAND DS    H                   CONE DEMAND AT THIS LOCATION TODAY           
LOPL     DS    3CL(LOPLDL)         STATUS BY PLAYER.                            
LONPLAY  DS    X                   NUMBER OF VANS HERE                          
*                                                                               
         DS    0H                                                               
LOCDSL   EQU   *-LOCDS                                                          
         SPACE 3                                                                
INPDS    DSECT COVERS SCANNED INPUT AREA ELEMENT                                
*                                                                               
INPL1    DS    X                   LENGTH LEFT HALF                             
INPL2    DS    X                   LENGTH RIGHT HALF ZERO IF NONE               
INPV1    DS    X                   VALIDITY BITS LEFT HALF                      
INPV2    DS    X                   VALIDITY BITS RIGHT HALF                     
INPVAL1  DS    FL4                 VALUE OF LEFT HALF IF NUMERIC/HEX            
INPVAL2  DS    FL4                 VALUE OF RIGHT HALF IF NUMERIC/HEX           
INPDATA1 DS    CL10                VALUE ENTERED IN LEFT HALF                   
INPDATA2 DS    CL10                VALUE ENTERED IN RIGHT HALF                  
*                                                                               
INPDSL   EQU   *-INPDS                                                          
         EJECT                                                                  
MESSECT  CSECT                                                                  
*                                                                               
*        ERROR AND PROMPTING MESSAGES                                           
*                                                                               
ENTMESS  DC    C'PRESS ENTER TO CONTINUE'                                       
PROMESS  DC    C'PLEASE TYPE YOUR REPLY AND PRESS ENTER'                        
NAMEMESS DC    C'PLEASE ENTER YOUR NAME (UP TO 10 CHARACTERS)'                  
ERRMESS1 DC    C'YOUR INPUT IS INVALID - PLEASE RE-TYPE IT AND PRESS ENX        
               TER'                                                             
BSMESS1  DC    C'ENTER THE NUMBER OF VANS YOU WANT TO XXXX - OR 0 IF NOX        
               NE'                                                              
BSMESS2  DC    C'THERE AREN''T THAT MANY FOR SALE  - TRY AGAIN'                 
BSMESS3  DC    C'YOU CAN''T AFFORD TO XXX THAT MANY - TRY AGAIN'                
BSMESS4  DC    C'YOU HAVEN''T GOT THAT MANY FOR SALE - TRY AGAIN'               
STAHEAD1 DC    2C'ID LOCATION            GALLONS  PENCE  '                      
STAHEAD2 DC    2C' - --------------------   --     ----  '                      
DEPINST1 DC    C'ENTER ''ID=N'' FOR EACH VAN TO BE USED. ''ID'' IS THE X        
                LOCATION, ''N'' IS THE NUMBER'                                  
DEPINST2 DC    C'OF GALLONS OF ICE-CREAM. E.G A=50,C=25 FOR TWO VANS. (X        
               ID=0 WILL DELETE A VAN)'                                         
DEPERRM1 DC    C'YOU CAN''T AFFORD TO BUY ICE-CREAM TODAY.  JUST ENTER X        
               ''END''.'                                                        
DEPSTAT  DC    C'TOTAL NNNN GALLS, COST $NNNN.NN. OVERHEADS $NNN LEAVESX        
                BALANCE OF $NNNNNNN.NN-'                                        
GLVANERR DC    C'YOU DON''T HAVE ENOUGH VANS. PLEASE DELETE SOME.'              
GLMONERR DC    C'YOU DON''T HAVE ENOUGH CREDIT. BE LESS AMBITIOUS.'             
GLOKMESS DC    C'POSITIONS ACCEPTABLE. TYPE ''END'' IF YOU ARE SATISFIEX        
               D'                                                               
GLIDLMSS DC    C'YOU HAVE SOME IDLE VANS. IF YOU DON''T WANT TO USE THEX        
               M TYPE ''END'''                                                  
PRCINST1 DC    C'ENTER ''ID=NNN'' FOR EACH VAN, WHERE ''ID'' IS THE LOCX        
               ATION AND ''NNN'' IS THE PRICE'                                  
PRCINST2 DC    C'IN PENCE TO BE CHARGED FOR EACH CORNET BY THE VAN DRIVX        
               ER AT THAT LOCATION'                                             
GPERRMS1 DC    C'ONE OR MORE OF THE LOCATIONS YOU ENTERED HAS NO VAN - X        
               TRY AGAIN'                                                       
GPPRCERR DC    C'ONE OR MORE LOCATIONS HAVE NO PRICE - PLEASE ENTER THEX        
               M'                                                               
GPOKMESS DC    C'PRICES ACCEPTABLE. TYPE ''END'' IF YOU ARE SATISFIED'          
SULIT1   DC    C'END OF DAY REPORT FOR DAY'                                     
SULIT2   DC    C'BALANCE B/FWD --'                                              
SULIT3   DC    C'TOTAL INCOME ---'                                              
SULIT4   DC    C'NEW BALANCE ----'                                              
SULIT5   DC    C'PROFIT FOR DAY -'                                              
SULIT6   DC    C'--CONES-- PENCE POUNDS'                                        
SULIT7   DC    C'MADE SOLD EACH INCOME'                                         
UNFLIT1  DC    C'TO MAKE MATTERS WORSE, '                                       
UNFLIT2  DC    C'HOWEVER,               '                                       
*                                                                               
*        DAY TABLE. GIVES NAME AND DEMAND FACTOR FOR EACH DAY                   
*        LOCATION TABLE. GIVES NAME AND DEMAND FACTOR FOR EACH LOCATION         
*                                                                               
DAYTAB   DC    CL10'WEDNESDAY.',AL1(060)                                        
DAYTABL  EQU   *-DAYTAB                                                         
         DC    CL10'THURSDAY. ',AL1(075)                                        
         DC    CL10'FRIDAY.   ',AL1(100)                                        
         DC    CL10'SATURDAY. ',AL1(150)                                        
         DC    CL10'SUNDAY.   ',AL1(090)                                        
         DC    CL10'MONDAY.   ',AL1(080)                                        
         DC    CL10'TUESDAY.  ',AL1(060)                                        
*                                                                               
LOCTAB   DC    CL20'TRAFALGAR SQUARE    ',AL1(100)                              
LOCTABL  EQU   *-LOCTAB                                                         
         DC    CL20'BUCKINGHAM PALACE   ',AL1(105)                              
         DC    CL20'OXFORD CIRCUS       ',AL1(070)                              
         DC    CL20'HARROD''S            ',AL1(060)                             
         DC    CL20'ST.PAUL''S CATHEDRAL ',AL1(075)                             
         DC    CL20'MADAME TUSSAUD''S    ',AL1(110)                             
         EJECT                                                                  
ICEGAME  CSECT                                                                  
VRANDOM  DC    V(RANDOM)                                                        
LDAYNAME DC    AL1(L'DAYNAME)                                                   
LWEADESC DC    AL1(L'WEADESC)                                                   
LDSELLVA DC    AL1(L'DSELLVAN)                                                  
LDBUYFLA DC    AL1(L'DBUYFLAG)                                                  
LDBUYPRC DC    AL1(L'DBUYPRC)                                                   
LDSELPRC DC    AL1(L'DSELPRC)                                                   
LDNOVANS DC    AL1(L'DNOVANS)                                                   
LDBALSIG DC    AL1(L'DBALSIGN)                                                  
LDBALANC DC    AL1(L'DBALANCE)                                                  
LDCREDIT DC    AL1(L'DCREDIT)                                                   
LDPRFLSS DC    AL1(L'DPRFLSS)                                                   
LDPROFIT DC    AL1(L'DPROFIT)                                                   
LDWHOLES DC    AL1(L'DWHOLES)                                                   
LOCCDESC DC    AL1(L'OCCDESC)                                                   
LOCCSDES DC    AL1(L'OCCSDESC)                                                  
LWEASDES DC    AL1(L'WEASDESC)                                                  
LPRODESC DC    AL1(L'PRODESC)                                                   
LDMESS01 DC    AL1(L'DMESS01)                                                   
LPLNAME  DC    AL1(L'PLNAME)                                                    
LPLPRDES DC    AL1(L'PLPRDESC)                                                  
         DC    0D'0'                                                            
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FATWA                                                          
         ORG   TWAUSER                                                          
BUYPRC   DS    F                   PURCHASE PRICE OF VANS                       
SELPRC   DS    F                   SELLING PRICE OF VANS                        
RETRNOFF DS    H                   OFFSET OF NEXT STEP TO EXECUTE               
DAYCOUNT DS    H                   NUMBER OF DAYS PLAYED SO FAR                 
WHOLEICE DS    H                   WHOLE-SALE COST GALLON OF ICE-CREAM          
DAYNUM   DS    X        CURRENT DAY (W=1,T=2,F=3,S=4,S=5,M=6,T=7)               
NPLAYERS DS    X                   BINARY NUMBER OF PLAYERS 1 2 OR 3            
CPLAYER  DS    X                   CURRENT PLAYER NUMBER                        
SALEVANS DS    X                   BINARY NUMBER OF VANS FOR SALE               
TIARECS  DS    X                   NUMBER OF RECORDS USED FOR TIA               
FBUYVANS DS    X                   NOT ZERO - OK TO BUY VANS                    
FSELVANS DS    X                   NOT ZERO - OK TO SELL VANS                   
FBUYICE  DS    X                   NOT ZERO - OK TO SELL VANS                   
VANUSED  DS    X                   NUMBER OF VANS USED BY PLAYER                
         ORG   ,                                                                
         DS    CL2                                                              
SCREEN   DS    0C                                                               
       ++INCLUDE GAICEFFD                                                       
         EJECT                                                                  
*              WORK DSECT (ROOT)                                                
         SPACE 3                                                                
WRK00D   DSECT                                                                  
DMCB     DS    20F                 EXTENDED FOR TXTMERGE ROUTINE                
WORK     DS    CL20                                                             
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
RANDNUM  DS    H                   RANDOM NUMBER WORK AREA                      
RELOFACT DS    A                   RELOCATION FACTOR                            
AEVENTS  DS    A                   ADDRESS OF EVENT TABLE                       
VDATAMGR DS    A                                                                
VCALLOV  DS    A                                                                
VSCANNER DS    A                                                                
VMESSECT DS    A                                                                
EVENTCH  DS    X                   INDICATES IF EVENT TABLE CHANGED             
*                                                                               
*        FOLLOWING AREA USED TO HOLD 'SCANNED' DATA FROM INPUT LINE             
*        FILLED AFTER EVERY WRITE/READ OPERATION                                
INPAREA  DS    6CL(INPDSL)                                                      
INPCOUNT DS    X                   BINARY COUNT OF INPUT FIELDS                 
*                                                                               
*        FOLLOWING AREA USED TO BUILD SCREEN TEXT. SUBROUTINE SCRTEXT           
*        WILL FORMAT AND MOVE TEXT TO SCREEN.                                   
TEXTWRK  DS    CL(20*L'SCRL1)                                                   
TEXTSTOP DS    C                   USED AS 'STOPPER'                            
         SPACE 3                                                                
*        THE FOLLOWING 2304 BYTES ARE SAVED TO TEMP STORAGE EVERY TIME          
*        PROGRAM RETURNS, AND IS READ BACK AT EACH ENTRY.                       
*                                                                               
WSAVE    DS    0D'0'                                                            
*                                                                               
PLAYERS  DS    3CL(PLAYERDL)       PLAYERS STATUS                               
LOCATION DS    6CL(LOCDSL)         LOCATION TABLE                               
*                                                                               
*        FOLLOWING 3 AREAS USED WHILE UPDATING TABLES DURING DAILY              
*        PLAYER SET-UP PHASE. ALLOWS EASY BACKOUT.                              
PLWORK   DS    CL(PLAYERDL)        PLAYERS STATUS WORK AREA                     
LOSAVE   DS    6CL(LOCDSL)         LOCATION TABLE SAVE AREA                     
VASAVE   DS    X                   VANS FOR SALE SAVE AREA                      
*                                                                               
*        THE FOLLOWING AREAS ARE USED IN BUILDING THE STATUS SCREEN             
*                                                                               
DSELLVAN DS    CL2                 NUMBER OF VANS FOR SALE                      
DBUYFLAG DS    CL5                 'CAN/COULD' LITERAL                          
DBUYPRC  DS    CL5                 SELLING PRICE VANS                           
DSELPRC  DS    CL5                 SELLING PRICE VANS                           
DNOVANS  DS    CL2                 NUMBER OF VANS OWNED                         
DBALSIGN DS    CL13                'HAVE/OVERDRAWN' LITERAL                     
DBALANCE DS    CL11                MONEY BALANCE                                
DCREDIT  DS    CL11                CREDIT LIMIT                                 
DPRFLSS  DS    CL6                 'PROFIT/LOSS' LITERAL                        
DPROFIT  DS    CL11                YESTERDAYS PROFIT                            
DWHOLES  DS    CL6                 WHOLESALE COST ICE CREAM                     
DMESS01  DS    CL23                'TO MAKE MATTERS WORSE, '                    
*        FOLLOWING AREAS USED TO HOLD RANDOM EVENT DATA FROM EVENT              
*        TABLES ETC.                                                            
*                                                                               
DAYENT   DS    0CL11     DAY OF WEEK ENTRY FROM DAYTAB                          
DAYNAME  DS    CL10                DAY NAME (TRAILING FULL STOP)                
DAYFACT  DS    X                   PERCENTAGE EFFECT OF DAY ON TRADE            
*                                                                               
WEATHER  DS    0CL108    WEATHER ENTRY FROM 'WEATHR' TABLE                      
WEAFACT  DS    X                   PERCENTAGE EFFECT ON TRADE                   
WEADESC  DS    CL98                DESCRIPTION OF WEATHER                       
WEASDESC DS    CL9                 SHORT DESCRIPTION                            
*                                                                               
OCCASION DS    0CL114    SPECIAL EVENT ENTRY FROM 'OCCASN' TABLE                
OCCFACTS DS    6X                  PERCENTAGE EFFECT ON EACH LOCATION           
OCCDESC  DS    CL93                DESCRIPTION OF EVENT                         
OCCSDESC DS    CL15                SHORT DESCRIPTION                            
*                                                                               
PROBLEM  DS    0CL80     DAYS PROBLEM ENTRY FROM 'DPROBL' TABLE                 
PROFACTS DS    6X                  PERCENTAGE IMPACT PER LOCATION               
PRODESC  DS    CL74                PROBLEM DESCRIPTION                          
*                                                                               
WSAVEND  EQU   *            WSAVEND MUST BE LESS THAN ORG ADDRESS               
         ORG   WSAVE+2560                                                       
*                                                                               
*        END OF SAVED STORAGE                                                   
*                                                                               
WRK00L   EQU   *-WRK00D                                                         
         EJECT                                                                  
* DDCOMFACS                                                                     
* DDMEDFACS                                                                     
* DDXXPARM                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDMEDFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063GAICE00   05/01/02'                                      
         END                                                                    
