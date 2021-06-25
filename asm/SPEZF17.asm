*          DATA SET SPEZF17    AT LEVEL 066 AS OF 07/11/18                      
*PHASE T23017A                                                                  
*                                                                               
*  AGENCY ADDRESS - PRINTING FROM 21 RECS, SHOULD IT BE ID REC?                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T23017 - COPY BATCH(ES) TO TAPE FOR ANY ONE AGENCY,         *         
*                  THEN MARK BATCH(ES) KEEP                           *         
*                                                                     *         
*  OUTPUTS: UPDATED INVOICE BATCHES                                   *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & WORKER FILE POINTER                        *         
*          R5 - WORK REG                                              *         
*          R6 - WORK REG                                              *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
*                                                                               
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 - WORKER INDEX                                     *         
*             AIO3 - SAVED HEADER RECS (21, 22, 23, 24, 25)           *         
*                                                                     *         
***********************************************************************         
         TITLE 'T23017 - COPY BATCH(ES) FROM ONE AGENCY TO TAPE'                
T23017   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3017**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         ST    R2,RELO                                                          
*                                                                               
         L     R7,=A(COMMON)                                                    
         A     R7,RELO                                                          
         USING COMMON,R7                                                        
*                                                                               
         CLC   =A(WRKFEND-SYSD),LSYSD                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,INIT                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LR                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
***********************************************************************         
*   VKEY - VALIDATE KEY                                               *         
***********************************************************************         
*                                                                               
VKEY     DS    0H                                                               
         MVI   DMPFLAG,X'00'                                                    
*                                                                               
         CLI   ACTNUM,17          MUST BE ACTION DUMP                           
         BNE   INVAL                                                            
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'                                                     
         BNE   INVAL                                                            
*                                                                               
         GOTO1 VALIMED             VALIMED SETS TO SPOT OR NET                  
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
* AGENCY (USER ID, REALLY)                                                      
*                                                                               
         LA    R2,LINAGYH          AGENCY                                       
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
         XC    RUIDNUM,RUIDNUM                                                  
         MVC   RUID,8(R2)                                                       
         OC    RUID,SPACES                                                      
         GOTO1 =A(LKAGY),DMCB,RUIDNUM,RUID,RUIDPC,AIO1,DATAMGR                  
         BNE   USERIDER                                                         
*                                                                               
         OI    4(R2),X'20'                                                      
*                                                                               
* STATION                                                                       
*                                                                               
         XC    RQSTA,RQSTA                                                      
*                                                                               
         LA    R2,LINSTAH          STATION                                      
         CLI   5(R2),0                                                          
         BE    VK100                                                            
*                                                                               
         CLI   5(R2),3             THIS AN ALL REQUEST                          
         BNE   *+14                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK100                                                            
*                                                                               
         GOTO1 VALISTA                                                          
         MVC   RQSTA,QSTA                                                       
*                                                                               
VK100    OI    4(R2),X'20'                                                      
*                                                                               
* BATCH SEQUENCE NUMBER                                                         
*                                                                               
         LA    R2,LINBSEQH         SEQ                                          
*                                                                               
         XC    RQBSEQ,RQBSEQ                                                    
         CLI   5(R2),0             MUST BE AN ENTRY                             
         BE    VK200                                                            
*                                                                               
         MVC   WORK(8),=8C'0'                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VKMVN                                                         
         EX    R1,VKCLC                                                         
         BNE   NUMERR                                                           
         EX    R1,VKPK                                                          
         OI    DUB+7,X'0F'                                                      
         CVB   R0,DUB                                                           
         STCM  R0,15,RQBSEQ                                                     
*                                                                               
VK200    DS    0H                                                               
         OI    4(R2),X'20'                                                      
*                                                                               
* BATCH DATE(S)                                                                 
*                                                                               
         XC    RQDTES,RQDTES                                                    
*                                                                               
         LA    R2,LINBDTEH         BATCH DATE(S)                                
         CLI   5(R2),0             ANYTHING?                                    
         BE    VK300               YES - PROCEED                                
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         ICM   R3,15,DMCB                                                       
         BZ    BADATE                                                           
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(2,RQSDTE)                                  
         CLM   R3,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VK260                YES                                         
         LA    R3,1+8(R2,R3)                                                    
*                                                                               
         GOTO1 DATVAL,DMCB,(0,(R3)),WORK                                        
         OC    DMCB,DMCB                                                        
         BZ    BADATE                                                           
         GOTO1 DATCON,(R1),(0,WORK),(2,RQEDTE)                                  
         B     *+10                                                             
*                                                                               
VK260    MVC   RQEDTE,RQSDTE                                                    
VK300    OI    4(R2),X'20'                                                      
*                                  FILTERS LINE                                 
* FILTERS                                                                       
*                                                                               
         LA    R2,LINFTRH                                                       
         BRAS  RE,VFTR             GO SET FILTERS                               
*                                                                               
* IF CORE PROCESSING + EMPTY DATE FIELD - CALCULATE DATES                       
* FROM 0 TO TODAY-1                                                             
         TM    FTRFLAG,FTRCORE     CORE PROCESSING                              
         BZ    VK400                                                            
         OC    RQDTES,RQDTES                                                    
         BNZ   VK400                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         GOTO1 ADDAY,DMCB,(C'D',WORK),WORK+6,F'-1'                              
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,RQEDTE)                                
*                                                                               
VK400    DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
VKMVN    MVN   WORK(0),8(R2)                                                    
VKCLC    CLC   WORK(0),8(R2)                                                    
VKPK     PACK  DUB,8(0,R2)                                                      
*                                                                               
*                                                                               
*                                                                               
LR       DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A2E'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APARSE,0(R1)                                                     
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A71'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   AEDITOR,0(R1)           ADDRESS OF EDITOR                        
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A2C'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   EZWRKIO,0(R1)           ADDRESS OF WORKIO                        
*                                                                               
         LA    RE,EZWRKIOB                                                      
         LHI   RF,WRKIOBL                                                       
         XCEFL                                                                  
*                                                                               
         MVC   GETFDISP,=A(GFDISP)                                              
*                                                                               
         L     R0,=A(EZDUMP)                                                    
         OPEN  ((R0),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BNZ   EZDUMPER                                                         
*                                                                               
         XC    CURWKIXD,CURWKIXD                                                
*                                                                               
         MVC   WRKEZUID,RUIDNUM                                                 
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO2                                                    
         MVC   WRKIABUF,AWRKFBUF                                                
         MVI   WRKIFTYP,WRKIFTEZ                                                
*                                                                               
DMP10    DS    0H                                                               
         MVI   READFLG,C'N'        DONE A READ FOR THIS INDEX ALREADY=N         
*                                                                               
         LA    R5,CURWKIXD                                                      
         USING W_RECD,R5                                                        
*                                                                               
         TM    FTRFLAG,FTREXP                                                   
         BZ    *+8                                                              
         OI    WRKINDS,WRKINOXQ                                                 
         MVI   WRKIACTN,WRKIANDX                                                
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    DMP200                                                           
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,FAKEIND                                                       
*                                                                               
         OC    W_USRID,W_USRID     NULL USER ID?                                
         BZ    DMP200              YES - EOF                                    
*                                                                               
* DO INDEX-LEVEL FILTERING HERE                                                 
*                                                                               
         CLI   W_DAY,X'99'         MUST BE DAY 99                               
         BNE   DMP10               BYPASS                                       
*                                                                               
         CLC   RUIDNUM,W_USRID     TEST RIGHT ID                                
         BNE   DMP10                                                            
*                                                                               
         OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BZ    *+14                                                             
         CLC   RQBSEQ,W_FILENO                                                  
         BNE   DMP10                                                            
*                                                                               
         OC    RQDTES,RQDTES       ANY BATCH DATE FILTERS?                      
         BZ    DMP15                                                            
*                                                                               
         CLC   W_AGELD,RQSDTE      CK TO START DATE                             
         BL    DMP10                                                            
         CLC   W_AGELD,RQEDTE      CK TO END DATE                               
         BH    DMP10                                                            
*                                                                               
DMP15    DS    0H                                                               
         TM    FTRFLAG,FTRALL      DUMPING ALL?                                 
         BO    DMP20               YES - DON'T SKIP KEEP BATCHES                
         TM    FTRFLAG,FTRCON      DUMPING CONVERTED?                           
         BO    DMP20               YES - DON'T SKIP KEEP BATCHES                
*                                                                               
         TM    W_STAT,W_STKE       BATCH IN KEEP STATUS?                        
         BO    DMP10               YES, SKIP                                    
*                                                                               
         DROP  R5                                                               
*                                                                               
DMP20    DS   0H                                                                
         USING EZWKRIXD,R5                                                      
*                                                                               
         MVC   SRCESTA(4),EZWISTN  STATION                                      
         CLI   SRCESTA+3,C'.'                                                   
         BNE   *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
         MVC   SRCESTA+4(1),EZWIMED                                             
*                                                                               
         MVC   EQUISTA,SRCESTA                                                  
         MVC   SVAGY,AGENCY                                                     
         MVC   AGENCY,RUIDPC                                                    
         GOTO1 VEQVSTA             GO GET EQUIVALENT STATION                    
         MVC   AGENCY,SVAGY                                                     
*                                                                               
         DROP  R5                                                               
*                                                                               
         MVC   SVMED,EQVMED                                                     
*                                                                               
         TM    FTRFLAG,FTRBOTH     BOTH SYSTEMS?                                
         BO    DMP25               SKIP SPOT/NET FILTERING                      
*                                                                               
         CLI   SPOTNETS,C'N'       NET?                                         
         BNE   DMP23               NO                                           
* NET SYSTEM HERE                                                               
         CLI   SVMED,C'N'          MEDIA N?                                     
         BNE   DMP10               IF NOT - SKIP IT                             
         B     DMP25                                                            
*                                                                               
* SPOT SYSTEM HERE                                                              
DMP23    DS   0H                                                                
         CLI   SVMED,C'N'          MEDIA N?                                     
         BE    DMP10               YES - SKIP IT                                
*                                                                               
DMP25    DS   0H                                                                
         CLI   FTRMEDIA,0                                                       
         BE    DMP30                                                            
*                                                                               
         LLC   R0,SVMED                                                         
         LA    RE,FTRMEDIA                                                      
         LA    RF,L'FTRMEDIA(RE)                                                
         SRST  RF,RE                                                            
         BC    11,DMP10            BRANCH IF NOT FOUND                          
*                                                                               
DMP30    DS    0H                                                               
         OC    RQSTA,RQSTA         STATION FILTER                               
         BZ    DMP40                                                            
         CLC   EQUISTA(5),RQSTA                                                 
         BNE   DMP10                                                            
*                                                                               
DMP40    DS    0H                                                               
         OC    FTRMOS,FTRMOS       FILTERING ON MONTH OF SERVICE                
         BZ    DMP45               NO                                           
*                                                                               
         OC    WRKEZMOS,WRKEZMOS                                                
         BZ    *+14                                                             
         MVC   HALF,WRKEZMOS                                                    
         B     DMP41                                                            
*                                                                               
         CLI   WRKEZUDT,X'00'                                                   
         JE    *+2                 NO MOS IN BATCH INDEX                        
*                                                                               
         MVC   BYTE,WRKEZUDT                                                    
         BRAS  RE,NEW2OLD                                                       
*                                                                               
DMP41    DS    0H                                                               
         CLC   HALF,FTRBMOS                                                     
         BNE   DMP10                                                            
*                                                                               
         USING W_RECD,R5                                                        
DMP45    DS    0H                                                               
         MVC   SVBATDAT,W_AGELD    SAVE BATCH LOADED DATE                       
         DROP  R5                                                               
*                                                                               
* CLEAR AIO3 AS SAVE AREA FOR HEADING RECORDS - 21, 22, 23, 24, 25              
*                                                                               
         L     RE,AIO3                                                          
         L     RF,SIZEIO                                                        
         XCEFL                                                                  
*                                                                               
DMP50    DS    0H                                                               
         L     RE,AIO2                                                          
         L     RF,SIZEIO                                                        
         XCEFL                                                                  
*                                                                               
         MVI   WRKIACTN,WRKIAGET                                                
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    DMP130                                                           
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AWRKFBUF                                                      
*                                                                               
         CLI   READFLG,C'Y'        DID A READ FOR THIS INDES?                   
         BE    DMP54               YES - SKIP SRC FILTER                        
*                                                                               
         MVI   READFLG,C'Y'                                                     
*                                                                               
         OC    FTRSRCE,FTRSRCE     FILTERING ON SOURCE                          
         BZ    *+14                                                             
         CLC   FTRSRCE,WRKEZDSC+EZWCSRCE-EZWKRCMD                               
         BNE   DMP10                                                            
*                                                                               
         AP    BATCT,=P'1'                                                      
*                                                                               
* ALL INDEX-LEVEL (AND SOURCE TOO) FILTERING DONE HERE                          
*                                                                               
DMP54    DS   0H                                                                
         L     R4,AIO2                                                          
*                                                                               
         TM    FTRFLAG,FTRCORE     IF CORE PROCESSING                           
         BZ    *+14                DON'T WRITE THE DDS-ONLY "76" RECORD         
         CLC   =C'76',4(R4)                                                     
         BE    DMP50                                                            
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,0(R4)                                                       
         AHI   RF,-4                                                            
         CLC   =C'31',4(R4)                                                     
         BNE   *+8                                                              
         AHI   RF,-26                                                           
*                                                                               
         CLM   RF,3,EZDUMP+DCBLRECL-IHADCB                                      
         BNH   *+6                                                              
* RECORD LONGER THAN DCB SPEC - WE DON'T WANT TRUNCATION                        
         DC    H'0'                                                             
*                                                                               
* SEE IF TOP/BOT COMMENT, AND CLEAR THE CORRESPONDING SAVE AREA                 
*                                                                               
DMP60    DS    0H                                                               
         CLC   =C'24',4(R4)        STD TOP COMMENT?                             
         BNE   DMP61               NO                                           
         TM    DMPFLAG,DF24Q       HAVE WE STARTED READING THEM?                
         BO    DMP65               YES, DON'T CLEAR SAVE AREA                   
*                                                                               
         OI    DMPFLAG,DF24Q       STARTED READING 24 COMMENTS                  
         LHI   RF,SAVSCT-SAVRECD   DISP TO THAT SAVE AREA IN AIO3               
         B     DMP63                                                            
*                                                                               
DMP61    DS    0H                                                               
         NI    DMPFLAG,X'FF'-DF24Q TURN OFF STD TOPCOM FLAG                     
*                                                                               
         CLC   =C'25',4(R4)        STD BOT COMMENT?                             
         BNE   DMP62               NO                                           
         TM    DMPFLAG,DF25Q       HAVE WE STARTED READING THEM?                
         BO    DMP65               YES, DON'T CLEAR SAVE AREA                   
*                                                                               
         OI    DMPFLAG,DF25Q       STARTED READING 25 COMMENTS                  
         LHI   RF,SAVSCB-SAVRECD                                                
         B     DMP63                                                            
*                                                                               
DMP62    DS    0H                                                               
         NI    DMPFLAG,X'FF'-DF25Q TURN OFF STD BOTCOM FLAG                     
         B     DMP65                                                            
*                                                                               
DMP63    DS    0H                  CLEAR 5-LINE SAVE AREA IN AIO3               
         L     RE,AIO3                                                          
         AR    RE,RF               DISPLACEMENT SET EARLIER                     
         LHI   RF,5*SAVLENQ        LENGTH*5                                     
         XCEFL                                                                  
*                                                                               
DMP65    DS    0H                                                               
         L     R4,AIO2             A(RECORD)                                    
*                                                                               
         TM    DMPFLAG,DFSAVQ                                                   
         BZ    *+12                YES, DO SAVREC STUFF                         
         BRAS  RE,SAVREC                                                        
         BE    DMP50               GET NEXT REC                                 
*                                                                               
         GOTO1 APARSE,DMCB,AIO2,('FTMAXENT',FLDTAB)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* SAVE STATION RECORD INFORMATION                                               
*                                                                               
         CLC   =C'22',4(R4)        IS THIS INVOICE HEADER                       
         BNE   DMP69                NO                                          
*                                                                               
         MVC   ORIGSTA,SPACES                                                   
         LHI   R0,2                                                             
         LA    R1,ORIGSTA                                                       
         BRAS  RE,COPYFLD                                                       
*                                                                               
         MVC   ORIGMED,SPACES                                                   
         LHI   R0,3                                                             
         LA    R1,ORIGMED                                                       
         BRAS  RE,COPYFLD                                                       
*                                                                               
         MVC   ORIGBAND,SPACES                                                  
         LHI   R0,4                                                             
         LA    R1,ORIGBAND                                                      
         BRAS  RE,COPYFLD                                                       
*                                                                               
* DO INVOICE - LEVEL FILTERING HERE                                             
*                                                                               
DMP69    DS    0H                                                               
         CLC   =C'31',4(R4)        IS THIS INVOICE HEADER                       
         BNE   DMP80                NO                                          
*                                                                               
         NI    DMPFLAG,X'FF'-DFSKIPQ TURN OFF SKIP INFOICE FLAG                 
*                                                                               
* CHECK INVOICE STATUS FILTERS                                                  
*                                                                               
         TM    FTRFLAG,FTRALL      DUMPING ALL INVOICES?                        
         BO    DMP70                                                            
*                                                                               
         TM    FTRFLAG,FTRUNCON    UNCONVERTED ONLY?                            
         BZ    *+12                                                             
         TM    7(R4),X'40'         CONVERTED?                                   
         BO    DMP75               YES - SKIP IT                                
*                                                                               
         TM    FTRFLAG,FTRCON      CONVERTED ONLY?                              
         BZ    *+12                                                             
         TM    7(R4),X'40'         CONVERTED?                                   
         BZ    DMP75               NO - SKIP IT                                 
*                                                                               
DMP70    DS    0H                                                               
         OC    FTRACTS,FTRACTS     FILTERING ON ACTIVITY DATE                   
         BZ    DMP72                NO                                          
*                                                                               
         OC    8(3,R4),8(R4)       UNCONVERTED                                  
         BZ    DMP75                YES, BYPASS                                 
*                                                                               
         CLC   FTRACTS,8(R4)       START DATE                                   
         BH    DMP75                                                            
         CLC   FTRACTE,8(R4)       END DATE                                     
         BL    DMP75                                                            
*                                                                               
DMP72    DS    0H                                                               
         OC    FTRINVNO,FTRINVNO   FILTERING ON INVOICE                         
         BZ    DMP76               NO                                           
*                                                                               
         GOTO1 GETFDISP,10                                                      
         CLM   RF,1,FTRINVLN                                                    
         BNE   DMP75                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FTRINVNO(0),0(R1)                                                
         BE    DMP76                                                            
*                                                                               
* SKIP THIS INVOICE                                                             
*                                                                               
DMP75    DS    0H                                                               
         OI    DMPFLAG,DFSKIPQ     SET THE SKIP FLAG                            
         B     DMP50               NO - SKIP IT                                 
*                                                                               
* INVOICE PASSED ALL FILTERS                                                    
*                                                                               
DMP76    DS    0H                                                               
         AP    INVCT,=P'1'                                                      
*                                                                               
         TM    DMPFLAG,DFSAVQ      DID WE DO SAVREC?                            
         BZ    *+8                 YES, DO WRTREC STUFF                         
         BRAS  RE,WRTREC           PUT OUT ANY SAVED RECS FIRST                 
*                                                                               
         MVC   SVIHCNVS,7(R4)      SAVE STATUS & CONVERTED DATA                 
*                                                                               
* REMOVE THE 12-BYTE SAVE FIELDS                                                
*                                                                               
         LA    RE,BLOCK                                                         
         LHI   RF,BLOCKLQ          L'BLOCK                                      
         XCEFL                                                                  
*                                                                               
         LA    R3,BLOCK                                                         
         LA    R2,FLDTAB                                                        
         LHI   R0,1                FIELD COUNTER                                
*                                                                               
DMP77    DS    0H                                                               
         GOTO1 GETFDISP,(R0)                                                    
         BZ    DMP125              NOTHING THERE - DONE                         
*                                                                               
         CHI   R0,2                SKIP 1ST SAVE FIELD                          
         BE    DMP78                                                            
         CHI   R0,37               SKIP 2ND SAVE FIELD                          
         BE    DMP78                                                            
*                                                                               
         LTR   RF,RF               FLD LEN RETURNED BY GETFDISP                 
         BZ    DMP77A              EMPTY FIELD - JUST PUT IN A SEMI             
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)                                                    
         AHI   RF,1                RESTORE FLD LENGTH                           
         AR    R3,RF               ADVANCE DEST ADDRESS                         
*                                                                               
DMP77A   DS    0H                                                               
         MVI   0(R3),X'5E'         SEMICOLON                                    
         LA    R3,1(R3)                                                         
*                                                                               
DMP78    DS    0H                                                               
         LA    R2,FTLENQ(R2)       NEXT FIELD                                   
         AHI   R0,1                INCREMENT FLD COUNTER                        
         CHI   R0,FTMAXENT                                                      
         BNH   DMP77                                                            
         B     DMP125              WRITE THE RECORD, W/O CLEARING BLOCK         
*                                                                               
* NON-31 RECORD                                                                 
*                                                                               
DMP80    DS    0H                                                               
         TM    DMPFLAG,DFSKIPQ     BYPASS INVOICE?                              
         BO    DMP50               YES                                          
*                                                                               
* PROCESS 34 - INVOICE TOTALS RECORD                                            
*                                                                               
         CLC   =C'34',4(R4)        THIS AN INVOICE TOTAL                        
         BNE   DMP100                                                           
*                                                                               
         GOTO1 GETFDISP,3                                                       
         BNZ   *+12                                                             
* NO GROSS FIELD - DON'T CALCULATE                                              
         MVI   GROSS,X'FF'                                                      
         B     DMP90                                                            
*                                                                               
DMP85    DS    0H                                                               
         CLI   GROSS,X'FF'                                                      
         BE    DMP90                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         AP    GROSS,DUB                                                        
*                                                                               
DMP90    DS    0H                                                               
         GOTO1 GETFDISP,5                                                       
         BNZ   *+12                                                             
         MVI   NET,X'FF'                                                        
         B     DMP120                                                           
*                                                                               
DMP95    DS    0H                                                               
         CLI   NET,X'FF'                                                        
         BE    DMP120                                                           
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         AP    NET,DUB                                                          
*                                                                               
         B     DMP120              WRITE THE RECORD                             
*                                                                               
* PROCESS 51 SPOT RECORDS                                                       
*                                                                               
DMP100   DS    0H                                                               
         CLC   =C'51',4(R4)        THIS AN SPOT RECORD                          
         BNE   DMP120                                                           
*                                                                               
         AP    SPTCT,=P'1'             SPOTS                                    
         CLI   7(R4),C'Y'          DID SPOT RUN                                 
         BNE   *+10                                                             
         AP    SPTCTR,=P'1'            SPOTS RUN                                
*                                                                               
* NOW MOVE AND WRITE RECORD *                                                   
*                                                                               
DMP120   DS    0H                                                               
         LA    RE,BLOCK                                                         
         LHI   RF,BLOCKLQ          L'BLOCK                                      
         XCEFL                                                                  
*                                                                               
         LA    R0,BLOCK                                                         
         LA    RE,4(R4)                                                         
         XR    RF,RF                                                            
         ICM   RF,3,0(R4)          GET REC LEN                                  
         AHI   RF,-4                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE               MOVE FROM IO AREA TO BLOCK +4                
*                                                                               
DMP125   DS    0H                                                               
         AP    RECCT,=P'1'                                                      
*                                                                               
         L     R0,=A(EZDUMP)                                                    
         PUT   (0),BLOCK                                                        
*                                                                               
         TM    FTRFLAG,FTRCORE                                                  
         BZ    DMP50                                                            
         CLC   =C'31',4(R4)                                                     
         BNE   DMP50                                                            
         TM    7(R4),X'01'         CAME FROM IM?                                
         BZ    DMP50                                                            
         BRAS  RE,WRTIMGR                                                       
         B     DMP50                                                            
*                                                                               
* DONE                                                                          
*                                                                               
DMP130   DS   0H                                                                
         CLI   TWAWRITE,C'Y'                                                    
         BNE   DMP150                                                           
         TM    FTRFLAG,FTRKEEP     THIS A KEEP RUN                              
         BZ    DMP150               YES, DO NOT MARK FILE                       
*                                                                               
*        GOTO1 DATAMGR,DMCB,=C'KEEP',EASIWK,CURWKIXD,AIO2,AWRKFBUF              
*                                                                               
         MVI   WRKIACTN,WRKIAKEE                                                
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DMP150   DS   0H                                                                
         B     DMP10               READ NEXT INDEX                              
*                                                                               
DMP200   DS    0H                                                               
         CP    INVCT,=P'0'         IF NO INVOICES                               
         BE    DMP210                                                           
*                                                                               
* CREATE THE "12" RECORD                                                        
*                                                                               
         LA    RE,BLOCK                                                         
         LHI   RF,BLOCKLQ          L'BLOCK                                      
         XCEFL                                                                  
*                                                                               
         LA    R2,BLOCK                                                         
*                                                                               
         MVC   0(2,R2),=C'12'                                                   
         MVI   2(R2),X'5E'                                                      
         LA    R2,3(R2)                                                         
*                                                                               
         EDIT  INVCT,(8,0(R2)),ALIGN=LEFT                                       
         LA    R2,8(R2)                                                         
*                                                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),X'5E'                                                      
         LA    R2,2(R2)                                                         
*                                                                               
         LA    RF,GROSS                                                         
         CLI   0(RF),X'FF'                                                      
         BE    DMP210                                                           
*                                                                               
         ST    RF,EBLK+EBAIN-EBLOCK                                             
         ST    R2,EBLK+EBAOUT-EBLOCK                                            
         GOTO1 AEDITOR,DMCB,EBLK                                                
         LA    R2,20(R2)                                                        
*                                                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),X'5E'                                                      
         LA    R2,2(R2)                                                         
*                                                                               
         AP    RECCT,=P'1'                                                      
*                                                                               
         L     R0,=A(EZDUMP)                                                    
         PUT   (0),BLOCK                                                        
*                                                                               
DMP210   DS    0H                                                               
         L     R0,=A(EZDUMP)                                                    
         CLOSE ((0))                                                            
*                                                                               
         TM    DMPFLAG,DFMQOPQ     MQ OPEN?                                     
         BZ    DMP211              NO - DON'T CLOSE IT                          
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CMQRPT-COMFACSD(RF)                                        
         GOTO1 (RF),DMCB,(0,=C'CLOSE'),0,0,0                                    
*                                                                               
DMP211   DS    0H                                                               
         L     R1,=A(HEADING)                                                   
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
*                                                                               
         L     R1,=A(HDHK)                                                      
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         EDIT  BATCT,(6,P+3),COMMAS=YES                                         
         EDIT  INVCT,(6,P+15),COMMAS=YES                                        
         EDIT  SPTCT,(9,P+22),COMMAS=YES                                        
         EDIT  SPTCTR,(9,P+35),COMMAS=YES                                       
*                                                                               
         CLI   NET,X'FF'                                                        
         BE    DMP212                                                           
         EDIT  NET,(15,P+45),COMMAS=YES,2                                       
*                                                                               
DMP212   DS    0H                                                               
         CLI   GROSS,X'FF'                                                      
         BE    DMP214                                                           
         EDIT  GROSS,(15,P+61),COMMAS=YES,2                                     
*                                                                               
DMP214   DS    0H                                                               
         EDIT  RECCT,(6,P+81),COMMAS=YES                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PRINT VARIOUS MEDIAS/BANDS FOUND                                              
*                                                                               
         LA    R2,ELEM                                                          
         LA    R3,256                                                           
DMP220   DS    0H                                                               
         MVC   P+2(4),0(R2)                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,4(,R2)                                                        
         OC    0(4,R2),0(R2)                                                    
         BZ    EXIT                                                             
         BCT   R3,DMP220                                                        
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H2,3,C'EASI'                                                     
         SSPEC H1,36,C'TRANSMITTAL FORM'                                        
         SSPEC H2,36,C'----------------'                                        
         SSPEC H1,68,AGYNAME                                                    
         SSPEC H2,68,AGYADD                                                     
         SSPEC H3,68,REPORT                                                     
         SSPEC H4,68,RUN                                                        
         SSPEC H5,78,PAGE                                                       
         SSPEC H10,3,C'BATCHES'                                                 
         SSPEC H11,3,C'-------'                                                 
         SSPEC H10,14,C'INVOICES'                                               
         SSPEC H11,14,C'--------'                                               
         SSPEC H10,27,C'SPOTS'                                                  
         SSPEC H11,27,C'-----'                                                  
         SSPEC H10,36,C'SPOTS RUN'                                              
         SSPEC H11,36,C'---------'                                              
         SSPEC H10,50,C'NET DOLLARS'                                            
         SSPEC H11,50,C'-----------'                                            
         SSPEC H10,64,C'GROSS DOLLARS'                                          
         SSPEC H11,64,C'-------------'                                          
         SSPEC H10,81,C'RECORDS'                                                
         SSPEC H11,81,C'-------'                                                
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
* HEAD HOOK ROUTINE                                                             
*                                                                               
HDHK     NTR1  BASE=*,LABEL=*                                                   
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
         XIT1                                                                   
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
*                                                                               
         DS    0D                                                               
EZDUMP   DCB   BUFNO=2,                                                C        
               DDNAME=EZDUMP,                                          C        
               DSORG=PS,                                               C        
               MACRF=PM,                                               C        
               RECFM=FB                                                         
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* LKAGY - AGENCY/UID INFORMATION LOOKUP SUBROUTINE                              
*                                                                               
* ON ENTRY: P1 = ADDR OF LKAGYBLK PARAMETER BLOCK FILLED IN WITH                
*           DEFAULT PARAMETER                                                   
*                                                                               
*           P2 = ADDR OF 10-CHARACTER USER ID (LKAGYUID)                        
*                                                                               
*           P3 = ADDR OF 2-BYTE AGENCY (TO BE FILLED IN)                        
*                                                                               
*           P4 = A(I/0 AREA)                                                    
*                                                                               
*           P5 = A(DATAMGR)                                                     
*                                                                               
* ON EXIT:  UID AND AGENCY CODES FILLED IN, EQ COND CODE                        
*           UNEQUAL CONDITION CODE OTHERWISE,                                   
*           AGY POWERCODE SET TO X'FFFF'                                        
*           BINARY ID SET TO X'FFFF'                                            
*           USER ID SET TO C'UNKNOWN'                                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
LKAGY    NMOD1 LKAGYWKL,**LKAGY**,CLEAR=YES                                     
         USING LKAGYWKD,RC                                                      
*                                                                               
         LM    R2,R6,0(R1)         BID,UID,AGY,IOA,ADMGR                        
         ST    R6,LKDMGR                                                        
*                                                                               
         OC    0(2,R2),0(R2)       BINARY ID FILLED IN?                         
         BZ    *+8                 NO                                           
         OI    LKFLAG,X'01'        YES - INDICATE LOOKING FOR BINARY ID         
*                                                                               
         XC    LKKEY,LKKEY                                                      
         LA    R6,LKKEY                                                         
         USING CTIREC,R6                                                        
*                                                                               
         MVI   CTIKTYP,C'I'                                                     
*                                                                               
         MVC   CTIKID+8(2),0(R2)   DEFAULT TO BINARY ID                         
         TM    LKFLAG,X'01'                                                     
         BO    *+10                                                             
         MVC   CTIKID,0(R3)        10-CHAR USER ID                              
*                                                                               
         GOTO1 LKDMGR,LKDMCB,=C'DMRDHI',=C'CTFILE',LKKEY,(R5),LKDMWK            
*                                                                               
         CLI   8(R1),0                                                          
         BNE   LKNEQXIT                                                         
         CLC   LKKEY,0(R5)                                                      
         BNE   LKNEQXIT                                                         
*                                                                               
         LR    R6,R5                                                            
         LA    R6,CTIDATA                                                       
*                                                                               
LK100    DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    LKXIT                                                            
*                                                                               
         CLI   0(R6),X'02'         DESCRIPTION ELEM                             
         BNE   LK150                                                            
*                                                                               
         OI    LKFLAG,X'02'        INDICATE FOUND ID ELEM                       
         TM    LKFLAG,X'01'        LOOKING FOR BINARY ID?                       
         BO    *+14                YES - 02 ELEMENT WILL HAVE CHAR ID           
         MVC   0(2,R2),2(R6)       NO - RETRIEVE THE BINARY ID                  
         B     LK200                                                            
*                                                                               
         MVC   0(10,R3),2(R6)      RETRIEVE THE 10-CHAR ID                      
         B     LK200                                                            
*                                                                               
LK150    DS    0H                                                               
         CLI   0(R6),X'06'         AGY ID ELEM                                  
         BNE   *+14                                                             
         MVC   0(2,R4),2(R6)                                                    
         OI    LKFLAG,X'04'        INDICATE FOUND AGY ALEM                      
*                                                                               
LK200    DS    0H                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LK100                                                            
*                                                                               
LKXIT    DS    0H                                                               
         TM    LKFLAG,X'06'                                                     
         BNO   LKNEQXIT                                                         
*                                                                               
LKEQXIT  DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
LKNEQXIT DS    0H                                                               
         MVC   0(2,R2),=X'FFFF'                                                 
         MVC   0(10,R3),=CL10'UNKNOWN'                                          
         MVC   0(2,R4),=X'FFFF'                                                 
         J     NEQXIT                                                           
*                                                                               
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* SAVE 76, 21, 22, 23, 24, 25 RECS IN AIO3                                      
***********************************************************************         
SAVREC   NTR1  BASE=*,LABEL=*                                                   
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
*                                                                               
         LHI   R0,TYPTABLQ                                                      
         L     R1,ATYPTAB                                                       
         USING TYPTABD,R1                                                       
*                                                                               
* FIRST SEE IF WE EVEN NEED TO SAVE THIS RECORD                                 
SR10     CLC   TYPTYP,4(R4)        RECORD TYPE IN TABLE?                        
         BE    SR20                YES - SAVE THIS RECORD                       
*                                                                               
         LA    R1,TYPLENQ(R1)                                                   
         BCT   R0,SR10                                                          
         J     NEQXIT              NOT IN TABLE - JUST EXIT                     
*                                                                               
SR20     DS    0H                  CHECK RECORD SIZE                            
         CLC   0(2,R4),=AL2(SAVLENQ)                                            
         BL    *+6                                                              
         DC    H'0'                RECORD SIZE > SAVE AREA LENGTH               
*                                                                               
         L     RE,AIO3                                                          
         ICM   RF,15,TYPADDR      DISP OF THAT REC'S SAVE AREA(SAVRECD)         
         AR    RE,RF                                                            
*                                                                               
         TM    TYPFLAG,X'01'       SAVING 5-LINE COMMENTS?                      
         BZ    SR50                NO                                           
         DROP  R1                                                               
*                                                                               
* YES - FIND NEXT AVAILABLE LINE                                                
         LHI   R0,5                5 LINES MAX                                  
*                                                                               
         OC    0(2,RE),0(RE)       ANY DATA ALREADY THERE?                      
         BZ    SR50                                                             
         LA    RE,SAVLENQ(RE)                                                   
         BCT   R0,*-14                                                          
         DC    H'0'                MORE THAN 5 COMMENTS                         
*                                                                               
SR50     DS    0H                                                               
         XR    RF,RF                                                            
         ICM   RF,3,0(R4)          LENGTH OF THE RECORD                         
         LR    R5,RF                                                            
         MVCL  RE,R4               COPY RECORD TO ITS SAVE AREA                 
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* INIT - INITIALIZE ADDRESSES, VARIABLES, ETC.                                  
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         USING GEND,RC                                                          
         USING SYSD,R9                                                          
*                                                                               
         MVC   AIO,AIO1                                                         
         MVI   QMED,C'T'                                                        
         MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
*                                                                               
         LARL  RF,WRKFBUFR                                                      
         ST    RF,AWRKFBUF                                                      
*                                                                               
         L     RF,=A(TYPTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,ATYPTAB                                                       
*                                                                               
         ZAP   BATCT,=P'0'         TOTAL BATCHES                                
         ZAP   INVCT,=P'0'               INVOICES                               
         ZAP   SPTCT,=P'0'               SPOTS                                  
         ZAP   SPTCTR,=P'0'              SPOTS RUN                              
         ZAP   RECCT,=P'0'               RECORDS WRITTEN                        
         ZAP   NET,=P'0'                 NET DOLLARS                            
         ZAP   GROSS,=P'0'               GROSS DOLLARS                          
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         XC    EBLK,EBLK                                                        
         LA    R1,EBLK                                                          
         USING EBLOCK,R1                                                        
         MVI   EBLIN,8             PL8                                          
         MVI   EBTIN,C'P'                                                       
         MVI   EBLOUT,20           ALWAYS MAX LEN                               
         MVI   EBDECS,0                                                         
         MVI   EBALIGN,C'L'                                                     
         OI    EBOPT,EBOQZEN                                                    
         DROP  R1                                                               
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* VALIDATE FILTERS (IF ANY)                                           *         
***********************************************************************         
VFTR     NTR1  BASE=*,LABEL=*                                                   
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
         USING COMMON,R7                                                        
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
*                                                                               
         XC    FILTERS,FILTERS                                                  
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
*                                                                               
         MVI   BYTE,1                                                           
*                                                                               
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,(R2),(5,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    MISSERR             SCANNER DIDN'T FIND ANYTHING                 
*                                                                               
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
*                                                                               
VFTR10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
*                                                                               
VFTR36   DS    0H                                                               
         EX    R1,VFTRCLCI         INVOICE                                      
         BNE   VFTR40                                                           
         CLI   1(R4),10                                                         
         BH    INVLENER                                                         
         MVC   FTRINVNO,22(R4)                                                  
         MVC   FTRINVLN,1(R4)                                                   
         B     VFTR90                                                           
*                                                                               
VFTR40   EX    R1,VFTRCLCF         ACTIVITY (WITH DATE)                         
         BNE   VFTR50                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         ICM   R6,15,DMCB          WAS MO/DA/YR DATE VALID                      
         BZ    BADATE               NO, ERROR                                   
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(1,FTRACTS)                                 
         MVC   FTRACTE,FTRACTS                                                  
*                                                                               
         CLI   1(R4),8                                                          
         BNH   VFTR90                                                           
*                                                                               
         LA    R5,1(R5,R6)                                                      
*                                                                               
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         OC    DMCB,DMCB           WAS MO/DA/YR DATE VALID                      
         BZ    BADATE               NO, ERROR                                   
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(1,FTRACTE)                                 
         B     VFTR90                                                           
*                                                                               
VFTR50   EX    R1,VFTRCLCJ         SOURCE                                       
         BNE   VFTR60                                                           
         CLI   1(R4),4                                                          
         BH    SRCLENER                                                         
         MVC   FTRSRCE,22(R4)                                                   
         B     VFTR90                                                           
*                                                                               
VFTR60   EX    R1,VFTRCLCM         MEDIA                                        
         BNE   VFTR70                                                           
*                                                                               
         LA    R5,22(,R4)                                                       
         LLC   R6,1(R4)                                                         
*                                                                               
VFTR64   LLC   R0,0(R5)            CHARACTER TO LOOK FOR                        
         LA    RE,VFTRMLST         STRING TOO LOOK IN                           
         LA    RF,L'VFTRMLST(RE)   END OF STRING                                
         SRST  RF,RE               SEARCH STRING                                
         BC    11,VFTRMDER         BRANCH IF NOT FOUND                          
         LA    R5,1(R5)                                                         
         BCT   R6,VFTR64                                                        
*                                                                               
         MVC   FTRMEDIA,22(R4)                                                  
         B     VFTR90                                                           
*                                                                               
VFTR70   EX    R1,VFTRCLCK         NOKEEP DUMPED INVOICES                       
         BNE   VFTR72                                                           
         OI    FTRFLAG,FTRKEEP                                                  
         B     VFTR90                                                           
*                                                                               
VFTR72   EX    R1,VFTRCLCQ         CONVERTED ONLY                               
         BNE   VFTR73                                                           
         OI    FTRFLAG,FTRCON                                                   
         B     VFTR90                                                           
*                                                                               
VFTR73   EX    R1,VFTRCLCV         UNCONVERTED ONLY                             
         BNE   VFTR74                                                           
         OI    FTRFLAG,FTRUNCON                                                 
         B     VFTR90                                                           
*                                                                               
VFTR74   EX    R1,VFTRCLCC         IM - GENERATE 'X' FEEDBACK FOR IM            
         BNE   VFTR75                                                           
         OI    FTRFLAG,FTRCORE                                                  
         B     VFTR90                                                           
*                                                                               
VFTR75   EX    R1,VFTRCLCA         ALL - CONVRTD, OVER-RIDES, UNTOUCHED         
         BNE   VFTR76                                                           
         OI    FTRFLAG,FTRALL                                                   
         B     VFTR90                                                           
*                                                                               
VFTR76   DS    0H                                                               
         EX    R1,VFTRCLCP         MOS - MONTH OF SERVICE                       
         BNE   VFTR78                                                           
*                                                                               
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         ICM   RE,15,DMCB          WAS MO/DA/YR DATE VALID                      
         BNZ   MOSERR               YES, ERROR                                  
*                                                                               
         GOTO1 (RF),(R1),(2,(R5)),WORK                                          
         ICM   RE,15,DMCB          WAS MO/YR DATE VALID                         
         BZ    MOSERR               NO, ERROR                                   
         GOTO1 DATCON,(R1),(0,WORK),(X'20',FTRMOS)                              
*                                                                               
         PACK  DUB,FTRMOS(4)                                                    
         ICM   R0,7,DUB+5                                                       
         SRL   R0,4                                                             
         STCM  R0,3,FTRBMOS                                                     
         B     VFTR90                                                           
*                                                                               
VFTR78   DS    0H                                                               
         EX    R1,VFTRCLCE         EXPIRED                                      
         BNE   VFTR79                                                           
         OI    FTRFLAG,FTREXP                                                   
         B     VFTR90                                                           
*                                                                               
VFTR79   DS    0H                                                               
         EX    R1,VFTRCLCB         BOTH SYSTEMS: SPOT, NET                      
         BNE   VFTRERR                                                          
         OI    FTRFLAG,FTRBOTH                                                  
         B     VFTR90                                                           
*                                                                               
VFTR90   ZIC   RE,BYTE             UP FIELD NUMBER CTR                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,32(,R4)                                                       
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
         CLI   FTRINVLN,0          HAVE INV FILTER?                             
         BNE   *+12                YES                                          
         TM    FTRFLAG,FTRCON+FTRUNCON+FTRALL  FILTERING ON INV STATUS?         
         BZ    *+8                 NO                                           
         OI    DMPFLAG,DFSAVQ      SET NEED TO DO SAVREC                        
*                                                                               
VFTRX    J     EQXIT                                                            
*                                                                               
VFTRCLCA CLC   12(0,R4),=CL3'ALL'      ALLOW CONVERTED & OVER-RIDE INVS         
VFTRCLCB CLC   12(0,R4),=CL4'BOTH'     BOTH SYSTEMS: SPOT AND NET               
VFTRCLCC CLC   12(0,R4),=CL4'CORE'     GENERATE 'X' FEEDBACK FOR IM             
VFTRCLCE CLC   12(0,R4),=CL7'EXPIRED'  DO NOT SKIP EXPIRED BATCHES              
VFTRCLCF CLC   12(0,R4),=CL8'ACTIVITY' ACTIVITY (CONVERTED DATE)                
VFTRCLCI CLC   12(0,R4),=CL7'INVOICE'                                           
VFTRCLCJ CLC   12(0,R4),=CL6'SOURCE'                                            
VFTRCLCK CLC   12(0,R4),=CL6'KEEP'     MARK BATCHES KEEP                        
VFTRCLCM CLC   12(0,R4),=CL5'MEDIA'                                             
VFTRCLCP CLC   12(0,R4),=CL3'MOS'                                               
VFTRCLCQ CLC   12(0,R4),=CL4'CONV'     ONLY CONVERTED                           
VFTRCLCV CLC   12(0,R4),=CL6'UNCONV'                                            
VFTRMLST DC    CL6'TCSRNX'                                                      
*                                                                               
*                                                                               
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
*                                                                               
* RECORD TYPES TO BE SAVED                                                      
* MUST BE IN SAME ORDER AS ADDRESSES FOLLOWING ASAVIDB                          
TYPTAB   DC    C'76',X'00',AL4(SAVIDB-SAVRECD)                                  
TYPTLLQ  EQU   *-TYPTAB                                                         
* X'01' FLAG = SAVE UP TO 5 COMMENT RECORDS                                     
         DC    C'21',X'00',AL4(SAVAGY-SAVRECD)                                  
         DC    C'22',X'00',AL4(SAVSTA-SAVRECD)                                  
         DC    C'23',X'00',AL4(SAVPAY-SAVRECD)                                  
         DC    C'24',X'01',AL4(SAVSCT-SAVRECD)                                  
         DC    C'25',X'01',AL4(SAVSCB-SAVRECD)                                  
         DC    C'30',X'00',AL4(SAVVER-SAVRECD)                                  
TYPTABLQ EQU   (*-TYPTAB)/TYPTLLQ                                               
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
* BYTE EXPECTED TO HAVE THE NEW MOS (X'BA' = OCT 2011)                          
* ON EXIT HALF WILL HAVE THE OLD FORMAT MOS (X'1110')                           
* DUB IS USED FOR CONVERSION                                                    
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
NEW2OLD  NTR1  BASE=*,LABEL=*                                                   
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
*                                                                               
         ZIC   RF,BYTE                                                          
         NILL  GRF,X'000F'         ZERO OUT YEAR                                
         CVD   RF,DUB                                                           
         SR    RF,RF                                                            
         ICM   RF,3,DUB+6                                                       
         SRL   RF,4                                                             
         STC   RF,HALF+1                                                        
*                                                                               
         ZIC   RF,BYTE                                                          
         SRL   RF,4                GET RID OF THE MONTH                         
         CVD   RF,DUB                                                           
         SR    RF,RF                                                            
         ICM   RF,3,DUB+6                                                       
         SRL   RF,4                                                             
         STC   RF,HALF                                                          
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
*                                                                               
WRTIMGR  NTR1  BASE=*,LABEL=*                                                   
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         LA    RE,BLOCK                                                         
         LHI   RF,BLOCKLQ          L'BLOCK                                      
         XCEFL                                                                  
*                                                                               
         LA    R2,BLOCK                                                         
         MVC   0(L'WRTIMQNM,R2),WRTIMQNM                                        
         LA    R2,L'WRTIMQNM(R2)                                                
         LHI   R3,L'WRTIMQNM+4                                                  
         EDIT  (R3),(4,(R2))                                                    
         OC    0(4,R2),=C'0000'                                                 
         LA    R2,4(R2)                                                         
         LHI   R3,WMGRDLQ+4                                                     
         EDIT  (R3),(4,(R2))                                                    
         OC    0(4,R2),=C'0000'                                                 
         LA    R2,4(R2)                                                         
*                                                                               
         USING WMGRD,R2                                                         
*                                                                               
* LINE BREAK                                                                    
         MVC   WMGRSEP,=C'<DDSSEPERATOR>'                                       
*                                                                               
* ACTION                                                                        
         MVI   WMGRACT,C'X'        SENT TO CORE                                 
*                                                                               
* INVOICE NUMBER                                                                
         LHI   R0,10               10TH FIELD (COUNTING 12-BYTE SAVE)           
         LA    R1,WMGRINV                                                       
         BRAS  RE,COPYFLD                                                       
*                                                                               
* INVOICE DATE                                                                  
         LHI   R0,7                7TH FIELD (COUNTING 12-BYTE SAVE)            
         LA    R1,WORK                                                          
         BRAS  RE,COPYFLD                                                       
         GOTO1 DATCON,DMCB,(0,WORK),(5,WMGRDATE)                                
*                                                                               
* STATION (ORIGINAL)                                                            
         MVC   WMGRSTA,ORIGSTA     FROM THE 22 RECORD                           
*                                                                               
* BAND (ORIRINAL)                                                               
         MVC   WMGRBND,ORIGBAND    FROM THE 22 RECORD                           
*                                                                               
* MEDIA (ORIGINAL)                                                              
         MVC   WMGRMED,ORIGMED     FROM THE 22 RECORD                           
*                                                                               
* USER ID                                                                       
         MVC   WMGRUID,RUID        USER ID                                      
*                                                                               
* AGENCY ALPHA                                                                  
         MVC   WMGRAGY,RUIDPC                                                   
*                                                                               
* MONTH OF SERVICE                                                              
         LHI   R0,11               11TH FIELD (COUNTING 12-BYTE SAVE)           
         LA    R1,WMGRMOS                                                       
         BRAS  RE,COPYFLD                                                       
*                                                                               
* INVOICE VERSION NUMBER                                                        
         LHI   R0,40               40TH FIELD (COUNTING 12-BYTE SAVE)           
         LA    R1,WMGRVER                                                       
         BRAS  RE,COPYFLD                                                       
*                                                                               
* BATCH DATE                                                                    
         LA    R4,W_AGELD-W_RECD+CURWKIXD                                       
         GOTO1 DATCON,DMCB,(2,0(R4)),(X'20',WMGRBDAT)                           
*                                                                               
         MVI   WMGRACT+L'WMGRACT,X'4F'                                          
         MVI   WMGRINV+L'WMGRINV,X'4F'                                          
         MVI   WMGRDATE+L'WMGRDATE,X'4F'                                        
         MVI   WMGRSTA+L'WMGRSTA,X'4F'                                          
         MVI   WMGRMED+L'WMGRMED,X'4F'                                          
         MVI   WMGRBND+L'WMGRBND,X'4F'                                          
         MVI   WMGRUID+L'WMGRUID,X'4F'                                          
         MVI   WMGRAGY+L'WMGRAGY,X'4F'                                          
         MVI   WMGRNSTA+L'WMGRNSTA,X'4F'                                        
         MVI   WMGRNMED+L'WMGRNMED,X'4F'                                        
         MVI   WMGRNBND+L'WMGRNBND,X'4F'                                        
         MVI   WMGRCLT+L'WMGRCLT,X'4F'                                          
         MVI   WMGRPRD1+L'WMGRPRD1,X'4F'                                        
         MVI   WMGRPRD2+L'WMGRPRD2,X'4F'                                        
         MVI   WMGREST+L'WMGREST,X'4F'                                          
         MVI   WMGRPREP+L'WMGRPREP,X'4F'                                        
         MVI   WMGRPKG+L'WMGRPKG,X'4F'                                          
         MVI   WMGRVER+L'WMGRVER,X'4F'                                          
         MVI   WMGRMOS+L'WMGRMOS,X'4F'                                          
         MVI   WMGRNINV+L'WMGRNINV,X'4F'                                        
         MVI   WMGRRVER+L'WMGRRVER,X'4F'                                        
         MVI   WMGRBDAT+L'WMGRBDAT,X'4F'                                        
*                                                                               
         OC    WMGRACT,SPACES                                                   
         OC    WMGRINV,SPACES                                                   
         OC    WMGRDATE,SPACES                                                  
         OC    WMGRSTA,SPACES                                                   
         OC    WMGRMED,SPACES                                                   
         OC    WMGRBND,SPACES                                                   
         OC    WMGRUID,SPACES                                                   
         OC    WMGRAGY,SPACES                                                   
         OC    WMGRNSTA,SPACES                                                  
         OC    WMGRNMED,SPACES                                                  
         OC    WMGRNBND,SPACES                                                  
         OC    WMGRCLT,SPACES                                                   
         OC    WMGRPRD1,SPACES                                                  
         OC    WMGRPRD2,SPACES                                                  
         OC    WMGREST,SPACES                                                   
         OC    WMGRPREP,SPACES                                                  
         OC    WMGRPKG,SPACES                                                   
         OC    WMGRVER,SPACES                                                   
         OC    WMGRMOS,SPACES                                                   
         OC    WMGRNINV,SPACES                                                  
         OC    WMGRRVER,SPACES                                                  
         OC    WMGRBDAT,SPACES                                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CMQRPT-COMFACSD(RF)                                        
*                                                                               
         TM    DMPFLAG,DFMQOPQ     MQ ALREADY OPEN?                             
         BO    WRTM20              YES - DON'T OPEN IT AGAIN                    
*                                                                               
         OI    DMPFLAG,DFMQOPQ     SET MQ OPEN                                  
         GOTO1 (RF),DMCB,(0,=C'OPEN'),(0,=C'IMSTATUS********'),0,0              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                SOME OTHER ERROR - CRASH                     
*                                                                               
WRTM20   DS    0H                                                               
         LHI   R3,WMGRDLQ+L'WRTIMQNM+8                                          
         GOTO1 (RF),DMCB,=CL8'PUT',BLOCK,(R3)                                   
         CLI   DMCB+8,0                                                         
         BE    WRTX                                                             
*                                                                               
         TM    DMCB+8,X'40'        BUFFER FULL?                                 
         BO    *+6                                                              
         DC    H'0'                SOME OTHER ERROR - CRASH                     
*                                                                               
* BUFFER FULL - TRY CLOSING AND OPENING THE MQ ENTRY                            
*                                                                               
         GOTO1 (RF),DMCB,(0,=C'CLOSE'),0,0,0                                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T CLOSE - CRASH                          
*                                                                               
         GOTO1 (RF),DMCB,(0,=C'OPEN'),(0,=C'IMSTATUS********'),0,0              
         CLI   DMCB+8,0                                                         
         BE    WRTM20                                                           
         DC    H'0'                OPE FAILED - CRASH                           
*                                                                               
WRTX     DS    0H                                                               
         J     EQXIT                                                            
         LTORG                                                                  
         DROP                                                                   
*                                                                               
WRTIMQNM DC    C'IMSTATUS********'                                              
*                                                                               
*                                                                               
*                                                                               
COPYFLD  NTR1  BASE=*,LABEL=*                                                   
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
*                                                                               
         LR    R2,R1                                                            
*                                                                               
         GOTO1 GETFDISP,(R0)                                                    
         JZ    EQXIT                                                            
         LTR   RF,RF                                                            
         JZ    EQXIT                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R1)                                                    
         J     EQXIT                                                            
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* PUT ALL SAVED RECS FROM AIO3                                        *         
***********************************************************************         
WRTREC   NTR1  BASE=*,LABEL=*                                                   
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
*                                                                               
         L     R4,AIO3                                                          
         LHI   R5,SAVNUMQ                                                       
*                                                                               
WRTR10   DS    0H                                                               
         OC    0(2,R4),0(R4)                                                    
         BZ    WRTR20                                                           
*                                                                               
         LA    RE,BLOCK                                                         
         LHI   RF,480              L'BLOCK                                      
         XCEFL                                                                  
*                                                                               
         LA    RE,4(R4)                                                         
         XR    RF,RF                                                            
         ICM   RF,3,0(R4)                                                       
         LA    R0,BLOCK                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         AP    RECCT,=P'1'                                                      
*                                                                               
         L     R0,=A(EZDUMP)                                                    
         PUT   (0),BLOCK                                                        
*                                                                               
WRTR20   DS    0H                                                               
         LA    R4,SAVLENQ(R4)                                                   
         BCT   R5,WRTR10                                                        
*                                                                               
WRTRQX   DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
*                                                                               
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
GFDISP   LR    RF,R1               FIELD NUMBER                                 
         BCTR  RF,0                DEC FOR INDEXING (FIELD 1 = INDEX 0)         
         MHI   RF,FTLENQ           TIMES TABLE LENGTH                           
         LA    RF,FLDTAB(RF)       INDEX INTO THE TABLE                         
         XR    R1,R1                                                            
         ICM   R1,3,FTFLDDSP-FLDTABD(RF) FLD DISPLACEMENT                       
         BZR   RE                  EXIT IF ZERO                                 
         A     R1,AIO2             FIELD ADDRESS                                
         LLC   RF,FTFLDLEN-FLDTABD(RF)                                          
         BR    RE                                                               
         DROP                                                                   
*                                                                               
*                                                                               
*                                                                               
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* FAKEIND - ROUTINE TO FILL IN INDEX FIELDS WITH DDWRKIOD DATA                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
FAKEIND  NTR1  BASE=*,LABEL=*                                                   
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
         LA    R2,CURWKIXD                                                      
         USING UKINDEX,R2                                                       
*                                                                               
         MVC   UKUSRID,WRKEZUID                                                 
         MVC   UKSYSPRG(L'WRKEZSCL),WRKEZSCL                                    
         MVC   UKDAY,WRKEZDAY                                                   
         MVC   UKCLASS,WRKEZMED                                                 
* !!! UKTYPE AND UKATTB SACRIFICED TO FIT IN THE 4-BYTE SEQ NUMBER !!!          
         MVC   UKFILENO(L'WRKEZSQN),WRKEZSQN                                    
         MVC   UKSTAT,WRKEZSTA                                                  
         MVC   UKAGELD,WRKEZBDT                                                 
         MVC   UKUDATA,WRKEZUDT                                                 
         DROP  R2                                                               
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
WRKFBUFR DS   0D                                                                
         DS    XL14336                                                          
*                                                                               
*                                                                               
*                                                                               
COMMON   DS    0D                                                               
         USING COMMON,R7                                                        
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
*                                                                               
USERIDER L     R1,=A(INVUIDMS)                                                  
         B     ERREXIT                                                          
*                                                                               
USIDLNER L     R1,=A(UIDLENMS)                                                  
         B     ERREXIT                                                          
*                                                                               
VFTRMDER L     R1,=A(VFTRMDMS)                                                  
         B     ERREXIT                                                          
*                                                                               
VFTRERRC L     R1,=A(VFTRMS)                                                    
         B     ERREXIT                                                          
*                                                                               
CCLENER  L     R1,=A(CCLENMS)                                                   
         B     ERREXIT                                                          
*                                                                               
PCLENER  L     R1,=A(PCLENMS)                                                   
         B     ERREXIT                                                          
*                                                                               
INVLENER L     R1,=A(INVLENMS)                                                  
         B     ERREXIT                                                          
*                                                                               
SRCLENER L     R1,=A(SRCLENMS)                                                  
         B     ERREXIT                                                          
*                                                                               
MOSERR   L     R1,=A(MOSERMS)                                                   
         B     ERREXIT                                                          
*                                                                               
NOBATFND L     R1,=A(NOBATFMS)                                                  
         B     ERREXIT                                                          
*                                                                               
UIDVRERR L     R1,=A(UIDVRMS)                                                   
         B     ERREXIT                                                          
*                                                                               
EZDUMPER L     R1,=A(EZDMPMS)                                                   
         B     ERREXIT                                                          
*                                                                               
ERRNOMED L     R1,=A(NOMDMS)                                                    
         B     ERREXIT                                                          
*                                                                               
VFTRERR  L     R1,=A(VFTRERMS)                                                  
         B     ERREXIT                                                          
*                                                                               
*                                                                               
ERREXIT  XC    CONHEAD,CONHEAD                                                  
         A     R1,RELO                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),L'CONHEAD-1-10                                             
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         ZIC   RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONHEAD+10(0),1(R1)                                              
*                                                                               
ERREXITA GOTO1 ERREX2                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
BADATE   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
INVAL    MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         DC    AL1(L'VFTRMS-1)                                                  
VFTRMS   DC    C'CAN''T USE CONVERT/UNCOVERT TOGETHER *'                        
         DC    AL1(L'VFTRMDMS-1)                                                
VFTRMDMS DC    C'VALID MEDIAS - T, R, N, C, S, X *'                             
         DC    AL1(L'CCLENMS-1)                                                 
CCLENMS  DC    C'CLIENT CODE MUST BE 2 OR 3 CHARACTERS *'                       
         DC    AL1(L'PCLENMS-1)                                                 
PCLENMS  DC    C'PRODUCT CODE MUST BE 2 OR 3 CHARACTERS *'                      
         DC    AL1(L'INVLENMS-1)                                                
INVLENMS DC    C'INVOICE CAN''T BE MORE THAN 10 CHARACTERS *'                   
         DC    AL1(L'SRCLENMS-1)                                                
SRCLENMS DC    C'SOURCE CAN''T BE MORE THAN 4 CHARACTERS *'                     
         DC    AL1(L'MOSERMS-1)                                                 
MOSERMS  DC    C'ENTER MOS MO/YR OR MOMYR *'                                    
         DC    AL1(L'CVTDELMS-1)                                                
CVTDELMS DC    C'CAN''T DUMP CONVERTED/DELETED/OVERRRIDDEN *'                   
         DC    AL1(L'NOBATFMS-1)                                                
NOBATFMS DC    C'BATCH NOT FOUND *'                                             
         DC    AL1(L'BATDTEMS-1)                                                
BATDTEMS DC    C'BATCH NOT FOUND FOR THIS DATE *'                               
         DC    AL1(L'INVUIDMS-1)                                                
INVUIDMS DC    C'INVALID USER ID *'                                             
         DC    AL1(L'USIDLNMS-1)                                                
USIDLNMS DC    C'USER ID CAN''T BE MORE THAN 8 CHARACTERS *'                    
         DC    AL1(L'UIDVRMS-1)                                                 
UIDVRMS  DC    C'USER ID CAN''T DO CHANGE *'                                    
         DC    AL1(L'NONDDSMS-1)                                                
NONDDSMS DC    C'DDS ONLY FUNCTION *'                                           
         DC    AL1(L'UIDLENMS-1)                                                
UIDLENMS DC    C'USERID CAN''T BE MORE THAN 8 CHARACTERS *'                     
         DC    AL1(L'EZDMPMS-1)                                                 
EZDMPMS  DC    C'EZDUMP FILE OPEN ERROR *'                                      
         DC    AL1(L'NOMDMS-1)                                                  
NOMDMS   DC    C'OPTION DDS MUST HAVE MEDIA=XXX FILTER *'                       
         DC    AL1(L'VFTRERMS-1)                                                
VFTRERMS DC    C'* ERROR * INVALID FILTER FIELD'                                
*                                                                               
         LTORG                                                                  
         DROP                                                                   
*                                                                               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
* CTGENFILE                                                                     
       ++INCLUDE SPGENEZ                                                        
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFFFD                                                       
*                                                                               
         ORG   CONTAGH                                                          
*                                                                               
       ++INCLUDE SPEZFE2D                                                       
*                                                                               
*                                                                               
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
*                                                                               
* DMWRKFD                                                                       
       ++INCLUDE DMWRKFD                                                        
*                                                                               
* DMWRKFK                                                                       
       ++INCLUDE DMWRKFK                                                        
*                                                                               
*                                                                               
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
*                                                                               
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
*                                                                               
       ++INCLUDE DDOFFICED                                                      
*                                                                               
       ++INCLUDE EZFLDTAB                                                       
       ++INCLUDE DDEBLOCK                                                       
       ++INCLUDE SPWMGRD                                                        
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
* DSECT FOR THIS PROGRAM *                                                      
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
BLOCKLQ  EQU   GLOBKEY-BLOCK                                                    
*                                                                               
RELO     DS    A                                                                
APARSE   DS    A                                                                
AEDITOR  DS    A                                                                
EZWRKIO  DS    A                                                                
AWRKFBUF DS    A                                                                
GETFDISP DS    A                                                                
*                                                                               
* ADDRESSES OF SAVE AREAS                                                       
* MUST BE IN SAME ORDER RECORD TYPES IN TYPTAB                                  
*                                                                               
ATYPTAB  DS    A                                                                
*                                                                               
BATCT    DS    PL8                 BATCHES                                      
RECCT    DS    PL8                 RECORD COUNT                                 
INVCT    DS    PL8                 INVOICE                                      
SPTCT    DS    PL8                 SPOTS                                        
SPTCTR   DS    PL8                 SPOTS RUN                                    
NET      DS    PL8                                                              
GROSS    DS    PL8                                                              
*                                                                               
BYTECT   DS    PL6                 BYTE COUNT                                   
*                                                                               
CURWKIXD DS    CL42                CURRENT WORKER INDEX                         
*                                                                               
ORIGSTA  DS    CL4                                                              
ORIGMED  DS    CL2                                                              
ORIGBAND DS    CL2                                                              
*                                                                               
SVMED    DS    CL1                                                              
READFLG  DS    C                   RECORDS READ FROM THIS BATCH                 
*                                                                               
SVWCSTAT DS    XL1                 STATUS                                       
SVWCSTCV EQU   X'40'               ALL INV CONVERTED  OR DELETED                
SVWCPDAT DS    XL3                 PROCESSED DATE                               
SVWCPTIM DS    XL2                 PROCESSED TIME-FORCED 0100                   
SVWCICNT DS    XL2                 INV CT - NOT USED YET                        
SVWCPCNT DS    XL2                 PROCESSED INV CT - NOT USED YET              
SVWCSRCE DS    CL4                                                              
         DS    CL2                 SPARE                                        
*                                                                               
SVIHCNVS DS   0XL12                                                             
SVIHCVST DS    XL1                   STATUS BYTE                                
*              X'80'                   STOP LEADING BLANK ELIMINATION           
SVIHCVQ  EQU   X'40'                   CONVERTED                                
SVIHRCVQ EQU   X'20'                   RECONVERT                                
SVIHCOVR EQU   X'10'                   CLIENT/PROD OVERRIDE                     
SVIHCDEL EQU   X'08'                   INVOICE IS DELETED                       
SVIHCCMB EQU   X'04'                   INVOICE WAS COMBINED WITH                
*                                      ANOTHER (MORE THAN 255)                  
SVIHCVDT DS    XL3                   DATE CONVERTED                             
SVIHCVAD DS    XL2                   CLIENT (MAY BE SET BEFORE CONVERT          
*                                    AS OVERRIDE TO LOOKUP)                     
SVIHCVPR DS    XL1                   PRODUCT (AS ABOVE)                         
SVIHCVP2 DS    XL1                   PRODUCT 2 (AS ABOVE)                       
SVIHCVES DS    XL1                   ESTIMATE (AS ABOVE)                        
         DS    XL3                                                              
*                                                                               
SVQPRD   DS    CL3                                                              
SVQPRD2  DS    CL3                                                              
SVBATDAT DS    XL2                 SAVED BATCH LOADED DATE                      
SVCOFFIC DS    CL1                 SAVED OFFICE FROM CLIENT REC                 
*                                                                               
RUID     DS    CL10                AGENCY                                       
RUIDNUM  DS    H                                                                
RUIDPC   DS    CL2                 POWER CODE                                   
*                                                                               
RQSTA    DS    CL5                 REQUESTED STATION                            
RQDTES   DS   0XL4                           DATES                              
RQSDTE   DS    XL2                           START DATE                         
RQEDTE   DS    XL2                           END DATE                           
RQBSEQ   DS    XL4                                                              
*                                                                               
* EQUIVALENT STATION AS RENAMED BY AGENCY                                       
EZCALL   DS    CL4                                                              
EZMED    DS    CL2                                                              
EZBAND   DS    CL2                                                              
*                                                                               
FILTERS  DS    0CL(FILTERND-FUID)                                               
FUID     DS    CL8                                                              
FUIDNUM  DS    XL2                 USER ID (DDS TERMS ONLY)                     
FTRSRCE  DS    CL4                 SOURCE                                       
FTRMOS   DS    CL4                 MONTH OF SERVICE DATE YYMM                   
         DS    XL2                 PADDING FOR DATCON                           
FTRBMOS  DS    CL4                 MONTH OF SERVICE DATE(BINARY)                
FTRACTS  DS    XL3                 ACTIVITY DATE (PWOS) CONVERSION DATE         
FTRACTE  DS    XL3                 ACTIVITY DATE (PWOS) CONVERSION DATE         
FTRMEDIA DS    CL10                MEDIA                                        
*                                                                               
FTRINVNO DS    CL10                INVOICE NO                                   
FTRINVLN DS    XL1                            LENGTH                            
*                                                                               
FTRFLAG  DS    XL1                                                              
FTRBOTH  EQU   X'40'               BOTH SYSTEMS: SPOT AND NET                   
FTREXP   EQU   X'20'               RETURN EXPIRED BATCHES                       
FTRCORE  EQU   X'10'               GENERATE 'X' FEEDBACK FOR IM                 
FTRKEEP  EQU   X'08'               MARK INVOICES DUMPED KEEP                    
FTRALL   EQU   X'04'               FILTER - DUMP ALL INVOICES                   
FTRCON   EQU   X'02'               FILTER - DUMP ONLY CONVERTED                 
FTRUNCON EQU   X'01'               FILTER - DUMP ONLY UNCONVERTED               
*                                                                               
FILTERND EQU   *                                                                
*                                                                               
*                                                                               
*                                                                               
DMPFLAG  DS    X                                                                
DFMQOPQ  EQU   X'01'               MQ IS OPEN                                   
DFSAVQ   EQU   X'02'               FILTERING ON INV DATA - DO SAVEREC           
DFSKIPQ  EQU   X'04'               SKIP THIS INVOICE                            
DF24Q    EQU   X'08'               STARTED READING STD TOP COMMENTS             
DF25Q    EQU   X'10'               STARTED READING STD BOTTOM COMMENTS          
DF76Q    EQU   X'20'               READ IN A 76 RECORD                          
DF30Q    EQU   X'40'               READ IN A 30 RECORD                          
*                                                                               
SVAGY    DS    CL2                                                              
*                                                                               
         DS    0F                                                               
EBLK     DS    XL24                                                             
*                                                                               
FTMAXENT EQU   50                                                               
FLDTAB   DS    0X                                                               
         DS    (FTMAXENT*FTLENQ)X                                               
*                                                                               
         DS    0F                                                               
EZWRKIOB DS    XL250               DMWRKIO CONTROL BLOCK (194+56 SPARE)         
         ORG   EZWRKIOB                                                         
       ++INCLUDE DDWRKIOD                                                       
         ORG                                                                    
*                                                                               
WRKFEND  EQU   *                                                                
*                                                                               
*                                                                               
*                                                                               
LKAGYWKD DSECT                                                                  
LKDMWK   DS    12D                                                              
LKDMCB   DS    6F                                                               
LKDMGR   DS    F                                                                
LKKEY    DS    XL25                                                             
LKFLAG   DS    X                   FLAG                                         
*                                  X'01' - LOOKING UP BINARY ID                 
*                                  X'02' - ID ELEMENT FOUND                     
*                                  X'04' - AGENCY ELEMENT FOUND                 
LKAGYWKL EQU   *-LKAGYWKD                                                       
*                                                                               
*                                                                               
*                                                                               
TYPTABD  DSECT                                                                  
TYPTYP   DS    CL2                                                              
TYPFLAG  DS    X                                                                
TYPADDR  DS    XL4                                                              
TYPLENQ  EQU   *-TYPTABD                                                        
*                                                                               
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
*                                                                               
*                                                                               
SAVRECD  DSECT                                                                  
SAVLENQ  EQU   250                                                              
*                                                                               
SAVIDB   DS    0X                                                               
         DS    (SAVLENQ)X                                                       
*                                                                               
SAVAGY   DS    0X                                                               
         DS    (SAVLENQ)X                                                       
*                                                                               
SAVSTA   DS    0X                                                               
         DS    (SAVLENQ)X                                                       
*                                                                               
SAVPAY   DS    0X                                                               
         DS    (SAVLENQ)X                                                       
*                                                                               
SAVSCT   DS    0X                                                               
         DS    (5*SAVLENQ)X                                                     
*                                                                               
SAVSCB   DS    0X                                                               
         DS    (5*SAVLENQ)X                                                     
*                                                                               
SAVVER   DS    0X                                                               
         DS    (SAVLENQ)X                                                       
*                                                                               
SAVNUMQ  EQU   (*-SAVRECD)/SAVLENQ                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066SPEZF17   07/11/18'                                      
         END                                                                    
