*          DATA SET ACBAT60    AT LEVEL 039 AS OF 06/28/04                      
*PHASE T61B60A                                                                  
BAT60    TITLE '- BATCH PROGRAM BATCH RECORD HANDLING'                          
BAT60    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BA60**,R8,R7,R6,RR=RE                                        
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA                                                          
         USING BSVALS,R5                                                        
         L     RC,AOVERWRK                                                      
         ST    RE,BORELO                                                        
         A     RE,=A(ADDCOL)                                                    
         ST    RE,BOADDR2          SET A(ADDCOL) ROUTINE                        
         LA    R1,BLTMSBLK                                                      
         ST    R1,ATMSUBLK                                                      
         LA    R1,DISTAB           SET A(DISPLAY COLUMN TABLE)                  
         TM    CUSTAT,CUSDDS                                                    
         BZ    *+8                                                              
         LA    R1,DISTDD                                                        
         ST    R1,BOADDR1          SAVE A(DISPLAY COLUMN TABLE)                 
         L     R1,AMIXNTRY                                                      
         SRL   RF,32-8                                                          
         LTR   RF,RF                                                            
         BNZ   BATNTRY                                                          
         EJECT                                                                  
***********************************************************************         
* REGULAR ENTRY POINT FOR RECORD/ACTION                               *         
***********************************************************************         
         SPACE 1                                                                
         ICM   RF,1,MIXROUT-MIXTABD(R1)                                         
         CLM   RF,1,=AL1(BATROUTM)                                              
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     BATROUTS(RF)                                                     
*                                                                               
BATROUTS DS    0XL4                                                             
         B     BATOPN                                                           
         B     BATDSP                                                           
         B     BATLST                                                           
         B     BATUPD                                                           
         B     BATCLO                                                           
         B     BATCHA                                                           
         B     BATRCL                                                           
         B     BATSAV                                                           
         B     BATDEL                                                           
         B     BATAPP                                                           
         B     BATUAP                                                           
BATROUTM EQU   (*-BATROUTS)/L'BATROUTS                                          
         SPACE 2                                                                
***********************************************************************         
* RETURN TO POINT AFTER NTRSES                                        *         
***********************************************************************         
         SPACE 1                                                                
BATNTRY  BCTR  RF,0                                                             
         CLM   RF,1,=AL1(BATNTRYM)                                              
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     BATNTRYS(RF)                                                     
*                                                                               
BATNTRYS DS    0XL4                                                             
         B     BATLSTR1                                                         
         B     BATACTR1                                                         
BATNTRYM EQU   (*-BATNTRYS)/L'BATNTRYS                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RE-DISPLAY A BATCH ON QUIT                               *         
***********************************************************************         
         SPACE 1                                                                
BATACTR1 SR    R1,R1                                                            
         IC    R1,TWASCRF          SET SCREEN FLAVOUR                           
         GOTO1 ADISHDR,(R1)                                                     
         MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     SETCUR                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OPEN A BATCH                                             *         
***********************************************************************         
         SPACE 1                                                                
BATOPN   XC    CSINDSG,CSINDSG     CLEAR GLOBAL INDICATORS                      
         CLI   TWASCRN,HEADSCRN    TEST BATCH HEADER SCREEN LOADED              
         BNE   BATOPN02                                                         
         CLI   TWASCRF,7           TEST SCREEN FLAVOUR 7                        
         BE    BATOPN04                                                         
         CLI   TWASCRF,8           OR SCREEN FLAVOUR 8                          
         BE    BATOPN04                                                         
         CLI   TWASCRF,1           IF SCREEN FLAVOUR 1 AND BATCH KNOWN          
         BNE   BATOPN02                                                         
         CLI   CSLSTCUR+(LSTTRTYP-LSTTABD),RECBAT                               
         BNE   BATOPN02                                                         
         GOTO1 ADISHDR,8           RE-DISPLAY SCREEN FLAVOUR 8                  
         B     BATOPN04                                                         
BATOPN02 GOTO1 ADISHDR,7+X'80'     LOAD BATCH HEADER SCREEN                     
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     SETCUR                                                           
*                                                                               
BATOPN04 GOTO1 ATSTACS,=AL1(RECITE,ACTINP)                                      
         BE    BATOPN05                                                         
         LA    R0,BASACTH          IF ITEM/INPUT NOT VALID RETURN ERROR         
         ST    R0,FVADDR           ON ACTION FIELD                              
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         B     EXIT                                                             
*                                                                               
BATOPN05 LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(BATCH MONTH TABLE ENTRY)                
         XC    LSTTABD(LSTTABL),LSTTABD                                         
*                                                                               
         MVI   FVMINL,1            BATCH REFERENCE                              
         GOTO1 AVALBRF,BATREFH                                                  
         BNE   EXIT                                                             
*                                                                               
         MVI   FVMINL,1            BATCH NAME                                   
         GOTO1 AVALBNA,BATNAMH                                                  
         BNE   EXIT                                                             
*                                                                               
         MVI   FVMINL,1            BATCH TYPE                                   
         GOTO1 AVALBTY,BATTYPH                                                  
         BNE   EXIT                                                             
         TM    CSBIND8,TYPIDETL    TEST DETAIL SCREEN                           
         BNO   *+8                                                              
         OI    LSTBHDS2,BHDSDETL                                                
*                                                                               
         GOTO1 ATSTBTY,CSACT       TEST ACTION VALID FOR BATCH TYPE             
         BNE   EXIT                                                             
         TM    CSBIND2,TYPICBG     TEST TYPE CREATED BY ACBG                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
         TM    CSBIND8,TYPIDISP    TEST TYPE DISPLAY ONLY                       
         BO    *-14                                                             
*                                                                               
         TM    CSBIND5,TYPICURO    TEST FOREIGN CURRENCY ONLY BATCH             
         BZ    BATOPN06                                                         
         TM    CSCURULE,CSCUFCUR   TEST COMPANY USES FOREIGN CURRENCY           
         BZ    *+12                                                             
         TM    CSCURULE,CSCUXRAT   TEST ANY EXCHANGE RATE TYPES VALID           
         BNZ   BATOPN06                                                         
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXIT                                                             
*                                                                               
BATOPN06 MVC   BATTYP,BCWORK                                                    
         OI    BATTYPH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         MVI   FVMINL,1            BATCH MONTH OF SERVICE                       
         GOTO1 AVALBMO,BATMOAH                                                  
         BNE   EXIT                                                             
*&&UK*&& GOTO1 ACHKBMO                                                          
*&&UK*&& BNE   EXIT                                                             
*                                                                               
         GOTO1 AVALEFD,BATEFDH     EFFECTIVE DATE                               
         BNE   EXIT                                                             
*                                                                               
         GOTO1 ATSTBMO,BATMOAH     TEST BATCH MONTH LOCKED                      
         BNE   EXIT                                                             
*                                                                               
         TM    BATIUPH+(FVATRB-FVIHDR),FVAPROT                                  
         BNZ   BATOPN12                                                         
         ICM   R0,7,FVOMTYP        SAVE MESSAGE TYPE/NUMBER                     
         GOTO1 ATSTBTY,=AL1(ACTIUP)                                             
         STCM  R0,7,FVOMTYP        RESTORE MESSAGE TYPE/NUMBER                  
         BE    BATOPN10                                                         
         XC    BATRHD8,BATRHD8                                                  
         OI    BATRHD8H+(FVOIND-FVIHDR),FVOXMT                                  
         XC    BATIUP,BATIUP                                                    
         OI    BATIUPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    BATIUPH+(FVOIND-FVIHDR),FVOXMT                                   
         B     BATOPN12                                                         
BATOPN10 GOTO1 AVALIUP,BATIUPH     INSTANT UPDATE                               
         BNE   EXIT                                                             
*                                                                               
BATOPN12 MVI   FVMINL,1            BATCH ITEM CONTROL                           
         GOTO1 AVALITE,BATITCH                                                  
         BNE   EXIT                                                             
*                                                                               
         MVI   FVMINL,1            BATCH CASH CONTROL                           
         GOTO1 AVALCSH,BATAMCH                                                  
         BNE   EXIT                                                             
*                                                                               
         TM    BATACRH+(FVATRB-FVIHDR),FVAPROT                                  
         BNZ   BATOPN16                                                         
         ICM   R0,7,FVOMTYP        SAVE MESSAGE TYPE/NUMBER                     
         GOTO1 ATSTBTY,=AL1(ACTACR)                                             
         STCM  R0,7,FVOMTYP        RESTORE MESSAGE TYPE/NUMBER                  
         BE    BATOPN14                                                         
         XC    BATRHDF,BATRHDF                                                  
         OI    BATRHDFH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    BATACR,BATACR                                                    
         OI    BATACRH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    BATACRH+(FVOIND-FVIHDR),FVOXMT                                   
         B     BATOPN16                                                         
BATOPN14 GOTO1 AFVAL,BATACRH       VALIDATE ACCRUAL OPTION                      
         BH    EXIT                                                             
         BE    BATOPN18            VALIDATE ANY INPUT                           
BATOPN16 OC    CSBTYP2,CSBTYP2     NO INPUT - TEST AUTOMATIC ACCRUAL            
         BZ    BATOPN20                                                         
         OI    LSTBHDS1,BHDSACRU   SET THIS IS AN ACCRUAL BATCH                 
         B     BATOPN20                                                         
BATOPN18 SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8              TEST FOR 'NO'                                
         BE    BATOPN20                                                         
         CLC   BC@NO(0),FVIFLD     'NO' IS ALWAYS VALID                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   BC@YES(0),FVIFLD    TEST FOR 'YES'                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         TM    CSBIND1,TYPIACRL    TEST ACCRUAL VALID FOR BATCH TYPE            
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACRNV)                                           
         B     EXIT                                                             
         OI    LSTBHDS1,BHDSACRU   SET THIS IS AN ACCRUAL BATCH                 
*                                                                               
BATOPN20 GOTO1 AADDHDR,BCPARM,BATREFH,(2,BATCOM1H)                              
         BNE   EXIT                                                             
*                                                                               
         GOTO1 ADISHDR,8           RE-DISPLAY THE BATCH                         
         GOTO1 ANTRSES,0           SAVE CURRENT SESSION                         
         GOTO1 AOVRSCR,BCPARM,('DETLSCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AOVRSCR,BCPARM,(CSBITS,BASOLY2H)                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CSBIND2,TYPIOSC     TEST OVERLAY PRESETS SCREEN                  
         BZ    BATOPN22                                                         
         GOTO1 AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         GOTO1 BONTRYA,BCPARM,('TYPIOSC',TWAD),WORKD                            
*                                                                               
BATOPN22 MVI   TWAMODE,0           SET OVERLAY MODE                             
         OI    CSINDSG1,CSINDBHD   SET BATCH HEADER ADDED                       
         GOTO1 ARECACT,=AL1(RECITE,ACTINP)                                      
         GOTO1 ABLDDET,0                                                        
         MVC   FVMSGNO,=AL2(AI$BACED)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     SETCUR                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY A BATCH HEADER                                              *         
***********************************************************************         
         SPACE 1                                                                
BATDSP   LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BNZ   BATDSP04                                                         
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   BATDSP02                                                         
         CLI   TWASCRN,HEADSCRN    TEST HEADER SCREEN LOADED                    
         BNE   *+12                                                             
         CLI   TWASCRF,4           AND FORMAT 4 (UNPROT KEY)                    
         BE    BATDSP02                                                         
         GOTO1 ADISHDR,4+X'80'     BUILD BATCH HEADER SCREEN                    
         GOTO1 AGETUID,CUUSER                                                   
         MVC   BATUID,BCWORK       SET CONNECTED USER-ID                        
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
*                                                                               
BATDSP02 LA    R1,LSTTABD                                                       
         ICM   R1,8,BCEFFS         SET TO TEST BATCH SECURITY                   
         GOTO1 AGETBAT                                                          
         BNE   EXIT                                                             
         GOTO1 TSTBAT,=AL2(LMBATDSP)                                            
         BNE   EXIT                                                             
         GOTO1 ADISHDR,4+X'40'     DISPLAY BATCH RECORD                         
         MVC   FVMSGNO,=AL2(AI$BADSP)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
BATDSP04 GOTO1 ADISHDR,2+X'20'     FORMAT PROTECTED DISPLAY SCREEN              
         MVC   FVMSGNO,=AL2(AI$RDENX)                                           
         SR    R1,R1                                                            
         ICM   R1,3,LSTBITMA                                                    
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R1,BOHALF1                                                       
         CLM   R1,3,LSTBITMC       TEST ITEMS ADDED V ITEM CONTROL              
         BH    BATDSP06                                                         
         LA    R1,BNHQ             BRANCH NOT HIGH                              
         CP    LSTBCSHA,BCPZERO                                                 
         BNL   *+8                                                              
         LA    R1,BNLQ             BRANCH NOT LOW                               
         CP    LSTBCSHA,LSTBCSHC   TEST CASH ADDED V CASH CONTROL               
         EX    R1,*+4                                                           
         NOP   *+10                                                             
BATDSP06 MVC   FVMSGNO,=AL2(AI$WBCEX)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
*                                                                               
BATDSPX  B     SETCUR                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LIST BATCH RECORDS                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING BLWORKD,RC          RC=A(LOCAL W/S)                              
BATLST   MVI   BLFLAG,0            CLEAR FLAG BYTE                              
         LA    R3,BLOPTS                                                        
         ST    R3,AOVEROUT         SAVE A(OUTPUT AREA FOR OPTIONS)              
         USING BLVALS,R3           R3=A(BATCH LIST OPTIONS)                     
         MVC   BLOREFND,BCEFFS     SET MAXIMUM END REFERENCE                    
         MVC   BLOUSR,CUUSER                                                    
*                                                                               
         L     R1,=A(BLOVAL)                                                    
         A     R1,BORELO                                                        
         ST    R1,AOVERVAL         SAVE A(OPTION VALIDATION ROUTINES)           
         GOTO1 AFVAL,BASOPTH                                                    
         GOTO1 AVALOPT,BLOTAB      VALIDATE OPTIONS                             
         BNE   EXIT                                                             
*                                                                               
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   BATLST02                                                         
         GOTO1 AOVRSCR,BCPARM,('BLSTSCRN',BASOLY1H)                             
         BNE   EXIT                                                             
         LA    R0,BLISEQH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EFOEA)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
*                                                                               
BATLST02 MVI   BLOSEQ,TBAKTYPQ                                                  
         L     R0,AACTKTAB         ACTIVE KEY TABLE                             
         LH    R2,=Y(UC@CRTD-TWAD)                                              
         LA    R2,TWAD(R2)                                                      
         GOTO1 AFVAL,BLISEQH       VALIDATE LIST SEQUENCE                       
         BNE   BATLST04                                                         
         SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    BATLST04                                                         
         CLC   FVIFLD(0),0(R2)                                                  
         MVI   BLOSEQ,TBAPTYPQ                                                  
         L     R0,APASKTAB         PASSIVE KEY TABLE                            
         LH    R2,=Y(UC@EFF-TWAD)                                               
         LA    R2,TWAD(R2)                                                      
         EX    RE,*+8                                                           
         BE    BATLST04                                                         
         CLC   FVIFLD(0),0(R2)                                                  
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
*                                                                               
BATLST04 A     R0,BORELO           RELOCATE A(KEY TABLE)                        
         ST    R0,BLAKTAB                                                       
         CLC   BLISEQ(L'UC@CRTD),0(R2)                                          
         BE    *+14                                                             
         MVC   BLISEQ(L'UC@CRTD),0(R2)                                          
         OI    BLISEQH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         GOTO1 AFVAL,BLITYPH                                                    
         BNE   BATLST18                                                         
         MVC   BOPARM+08(2),=C',='                                              
         MVC   BOPARM+10(1),BCCOMMA                                             
         MVI   BOPARM+11,0                                                      
         GOTO1 VSCANNER,BOPARM,FVIHDR,('BLMAXTYP',BLSCNTAB)                     
         MVC   BLBYTE1,4(R1)       SAVE NUMBER OF ITEMS INPUT                   
         CLI   BLBYTE1,0                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         MVI   BLBYTE2,0                                                        
         LA    R4,BLSCNTAB         R4=A(SCANNER TABLE)                          
*                                                                               
BATLST06 IC    R1,BLBYTE2          BUMP INPUT FIELD COUNT                       
         LA    R1,1(R1)                                                         
         STC   R1,BLBYTE2                                                       
         CLC   BLBYTE2,BLBYTE1                                                  
         BH    BATLST12                                                         
         CLI   BLBYTE1,1                                                        
         BE    *+10                                                             
         MVC   FVINDX,BLBYTE2                                                   
         CLI   0(R4),0             TEST FIELD PRESENT                           
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXIT                                                             
         TM    2(R4),X'80'         TEST NUMERIC INPUT FIELD                     
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXIT                                                             
         OC    4(3,R4),4(R4)       TEST FIELD NOT GREATER THAN 255              
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         GOTO1 AGETBTY,7(R4)       VALIDATE BATCH TYPE                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         GOTO1 ATSTBTY,CSACT       TEST BATCH TYPE SECURITY                     
         BNE   EXIT                                                             
         SR    R1,R1                                                            
         ICM   R1,1,BLOTYPN                                                     
         BZ    BATLST10                                                         
*                                                                               
         LR    RE,R1               CHECK DUPLICATED INPUT                       
         LA    RF,BLOTYPS                                                       
BATLST08 CLC   0(1,RF),7(R4)                                                    
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     EXIT                                                             
         LA    RF,1(RF)                                                         
         BCT   RE,BATLST08                                                      
*                                                                               
BATLST10 LA    R1,1(R1)            ADD ENTRY TO BATCH TYPE TABLE                
         STC   R1,BLOTYPN                                                       
         LA    R1,BLOTYPS-1(R1)                                                 
         MVC   0(1,R1),7(R4)                                                    
*                                                                               
         LA    R4,L'BLSCNTAB(R4)   BUMP TO NEXT INPUT FIELD                     
         B     BATLST06                                                         
*                                                                               
BATLST12 MVI   FVINDX,0                                                         
         LA    R1,BLOTYPS          SORT BLOTYPS INTO ASCENDING SEQUENCE         
         SR    R0,R0                                                            
         IC    R0,BLOTYPN                                                       
         SH    R0,=H'1'                                                         
         BZ    BATLST18                                                         
BATLST14 LA    RF,L'BLOTYPS(R1)                                                 
         LR    RE,R0                                                            
BATLST16 CLC   0(L'BLOTYPS,R1),0(RF)                                            
         BNH   *+22                                                             
         XC    0(L'BLOTYPS,R1),0(RF)                                            
         XC    0(L'BLOTYPS,RF),0(R1)                                            
         XC    0(L'BLOTYPS,R1),0(RF)                                            
         LA    RF,L'BLOTYPS(RF)                                                 
         BCT   RE,BATLST16                                                      
         LA    R1,L'BLOTYPS(R1)                                                 
         BCT   R0,BATLST14                                                      
*                                                                               
BATLST18 GOTO1 AVALPER,BOPARM,BLICREH,0                                         
         BH    EXIT                                                             
         LA    R1,BCWORK                                                        
         USING PERVALD,R1                                                       
         MVC   BLOCEND,BCEFFS                                                   
         BNE   *+16                                                             
         MVC   BLOCSTD,PVALCSTA                                                 
         MVC   BLOCEND,PVALCEND                                                 
         MVC   BLOCXST,BLOCEND                                                  
         XC    BLOCXST,BCEFFS                                                   
         MVC   BLOCXND,BLOCSTD                                                  
         XC    BLOCXND,BCEFFS                                                   
*                                                                               
BATLST20 GOTO1 AVALPER,BOPARM,BLIEFFH,0                                         
         BH    EXIT                                                             
         MVC   BLOEEND,BCEFFS                                                   
         BNE   BATLST22                                                         
         LA    R1,BCWORK                                                        
         USING PERVALD,R1                                                       
         MVC   BLOESTD,PVALCSTA                                                 
         MVC   BLOEEND,PVALCEND                                                 
*                                                                               
BATLST22 GOTO1 AVALMOS,BLIMOAH                                                  
         BH    EXIT                                                             
         MVC   BLOMOA,BCFULL                                                    
*                                                                               
         MVI   BLONMASK,FF-(TBAHSDEL)                                           
         GOTO1 AFVAL,BLISTAH                                                    
         BNE   BATLST38                                                         
         MVI   BLONMASK,0                                                       
         MVC   BOPARM+08(2),=C',='                                              
         MVC   BOPARM+10(1),BCCOMMA                                             
         MVI   BOPARM+11,0                                                      
         GOTO1 VSCANNER,BOPARM,FVIHDR,('BLMAXSTA',BLSCNTAB)                     
         MVC   BLBYTE1,4(R1)       SAVE NUMBER OF ITEMS INPUT                   
         CLI   BLBYTE1,0                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         MVI   BLBYTE2,0                                                        
         LA    R4,BLSCNTAB         R4=A(SCANNER TABLE)                          
*                                                                               
BATLST24 IC    R1,BLBYTE2          BUMP INPUT FIELD COUNT                       
         LA    R1,1(R1)                                                         
         STC   R1,BLBYTE2                                                       
         CLC   BLBYTE2,BLBYTE1                                                  
         BH    BATLST38                                                         
         CLI   BLBYTE1,1                                                        
         BE    *+10                                                             
         MVC   FVINDX,BLBYTE2                                                   
         SR    R1,R1                                                            
         ICM   R1,1,0(R4)                                                       
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXIT                                                             
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   BCWORK(0),12(R4)                                                 
         CLI   12(R4),C'*'         TEST 'NOT' FILTER                            
         BNE   BATLST26                                                         
         SH    R1,=H'1'                                                         
         BNM   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BCWORK(0),13(R4)                                                 
*                                                                               
BATLST26 LA    RF,STATAB                                                        
         USING STATABD,RF                                                       
         LA    R0,STATABN                                                       
BATLST28 SR    RE,RE                                                            
         ICM   RE,3,STATLNG                                                     
         LA    RE,TWAD(RE)                                                      
         EX    R1,*+8                                                           
         BE    BATLST32                                                         
         CLC   BCWORK(0),0(RE)                                                  
         CLM   R1,1,=AL1(L'UC3DELD-1)                                           
         BH    BATLST30                                                         
         SR    RE,RE                                                            
         ICM   RE,3,STATSHT                                                     
         LA    RE,TWAD(RE)                                                      
         EX    R1,*+8                                                           
         BE    BATLST32                                                         
         CLC   BCWORK(0),0(RE)                                                  
*                                                                               
BATLST30 LA    RF,STATABL(RF)                                                   
         BCT   R0,BATLST28                                                      
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
*                                                                               
BATLST32 IC    R1,STATBIT          TEST DUPLICATE OPTION                        
         EX    R1,*+8                                                           
         BNZ   BATLST34                                                         
         TM    BLONMASK,0                                                       
         EX    R1,*+8                                                           
         BZ    BATLST36                                                         
         TM    BLOXMASK,0                                                       
BATLST34 MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     EXIT                                                             
*                                                                               
BATLST36 LA    RE,BLONMASK                                                      
         CLI   12(R4),C'*'                                                      
         BNE   *+8                                                              
         LA    RE,BLOXMASK                                                      
         OC    0(1,RE),STATBIT                                                  
*                                                                               
         LA    R4,L'BLSCNTAB(R4)                                                
         B     BATLST24                                                         
*                                                                               
BATLST38 TM    BLONMASK,TBAHSDEL   SET *DELETE AS DEFAULT STATUS                
         BNZ   *+8                                                              
         TM    BLOXMASK,TBAHSDEL                                                
         BNZ   *+8                                                              
         OI    BLOXMASK,TBAHSDEL                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE LIST SUB-ACTIONS                                             *         
***********************************************************************         
         SPACE 1                                                                
BATLST40 MVI   FVINDX,0                                                         
         LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3                                                       
         NI    CSLTINDS,FF-(CSLTIFST+CSLTIHLD+CSLTIANY)                         
*                                                                               
         CLI   BCPFKEY,PFKRFSHQ    TEST REFRESH LIST                            
         BNE   *+12                                                             
         OI    CSLTINDS,CSLTIFST   SET FIRST FOR LIST                           
         MVI   BCPFKEY,0           CLEAR PFKEY                                  
*                                                                               
         CLC   BLOPTS(BLOKOPTL),BLOKOPT                                         
         BE    *+8                                                              
         OI    CSLTINDS,CSLTIFST   SET FIRST FOR LIST IF KEY CHANGED            
*                                                                               
         CLC   BLOPTS+BLOKOPTL(BLODOPTL),BLODOPT                                
         BE    *+14                                                             
         XC    BLODVALS,BLODVALS   CLEAR SCROLLING POINTERS                     
         OI    CSLTINDS,CSLTIHLD   SET HOLD PAGE (NO VERTICAL SCROLL)           
*                                                                               
         TM    BCSCROLL,PFKIHORZ   TEST HORIZONTAL SCROLL                       
         BZ    *+8                                                              
         OI    CSLTINDS,CSLTIHLD   SET HOLD PAGE (NO VERTICAL SCROLL)           
*                                                                               
         MVC   BLVALS(BLVALSL),BLOPTS                                           
         TM    CSLTINDS,CSLTIFST   TEST CHANGE OF KEY FIELDS                    
         BNZ   BATLST72                                                         
*                                                                               
BATLST42 MVC   LSTTRECN,CSPAG#LO                                                
         SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LIST ENTRIES                    
         BZ    BATLST72                                                         
         LA    R2,BLIACT1H                                                      
         USING BLIACT1H,R2                                                      
*                                                                               
BATLST44 NI    BLFLAG,FF-(BLFINPT) CLEAR USER INPUT FLAG BIT                    
         CLI   BLIACT1H+(FVILEN-FVIHDR),0                                       
         BE    *+12                                                             
         OI    BLFLAG,BLFINPT      SET USER INPUT                               
         B     BATLST46                                                         
         TM    CSLTINDS,CSLTIEOL+CSLTIEOP                                       
         BZ    BATLST64                                                         
         CLC   LSTTRECN,CSSEL#LO                                                
         BL    BATLST64                                                         
         CLC   LSTTRECN,CSSEL#HI                                                
         BH    BATLST64                                                         
         SR    R4,R4                                                            
         ICM   R4,3,CSSELMUL                                                    
         A     R4,AOVERSEL                                                      
         USING SELTABD,R4          R4=A(SELECT TABLE ENTRY)                     
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         XC    BLIACT1,BLIACT1                                                  
         MVC   BLIACT1(L'BLIACT1-1),0(RE)                                       
*                                                                               
BATLST46 GOTO1 AFVAL,BLIACT1H                                                   
         BNE   BATLST64                                                         
         L     R4,AOVERSEL         R4=A(SELECT TABLE)                           
         NI    BLFLAG,FF-(BLFNEXT)                                              
         CLI   FVIFLD,C'*'                                                      
         BE    BATLST66                                                         
         CLI   FVIFLD,C'-'                                                      
         BNE   *+12                                                             
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         B     BATLST66                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RE,FVIFLD(RF)       POINT TO END OF INPUT FIELD                  
         CLI   0(RE),C'+'                                                       
         BE    *+12                                                             
         CLI   0(RE),C'&&'                                                      
         BNE   BATLST48                                                         
         MVC   CSSEL#LO,LSTTRECN   SET LOW RECORD NUMBER                        
         MVC   CSSEL#HI,BCEFFS     SET DEFAULT HIGH VALUE                       
         LA    R1,CSLTIEOL                                                      
         CLI   0(RE),C'&&'                                                      
         BE    *+14                                                             
         MVC   CSSEL#HI,CSPAG#HI                                                
         LA    R1,CSLTIEOP                                                      
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         EX    R1,*+4                                                           
         OI    CSLTINDS,0                                                       
         BCTR  RF,0                                                             
         STC   RF,FVXLEN                                                        
*                                                                               
BATLST48 XC    BLASEL,BLASEL       RESET A(UNIQUE ACTION)                       
BATLST49 SR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RE)                                                  
         BNE   BATLST50                                                         
         OC    BLASEL,BLASEL       TEST A(ACTION)                               
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$FLDTS)                                           
         B     EXIT                                                             
         STCM  R4,15,BLASEL        SET A(ACTION)                                
BATLST50 LA    R4,SELTABL(R4)                                                   
         CLI   SELTABD,EOT                                                      
         BNE   BATLST49                                                         
*                                                                               
         ICM   R4,15,BLASEL        R4=A(UNIQUE ACTION) OR 0                     
         BZ    BATLST56                                                         
         GOTO1 ATSTMIX,SELTPARM    VALIDATE RECORD/ACTION                       
         BNE   BATLST56                                                         
*                                                                               
         CLC   LSTTRECN,CSSEL#LO   TEST SELECT MULTIPLE INPUT LINE              
         BNE   BATLST52                                                         
         L     RE,AOVERSEL                                                      
         LA    RF,SELTABD                                                       
         SR    RF,RE                                                            
         STCM  RF,3,CSSELMUL       SET DISPLACEMENT TO SELTAB ENTRY             
*                                                                               
BATLST52 TM    BLFLAG,BLFNEXT      TEST FIRST/NEXT TIME                         
         BNZ   BATLST53                                                         
         OI    BLFLAG,BLFNEXT      SET NOT FIRST TIME                           
         GOTO1 ATSARIO,TSAGET      GET BATCH LIST RECORD                        
         LA    R1,LSTBBTYP                                                      
         TM    LSTBHDS2,BHDSDETL   TEST DETAIL SCREEN                           
         BNO   *+8                                                              
         ICM   R1,8,=AL1(TYPIDETL) SET DETAIL FLAG                              
         GOTO1 AGETBTY                                                          
*                                                                               
BATLST53 CLI   SELTREC,RECBAT      TEST BATCH RECORD TYPE                       
         BNE   BATLST54                                                         
         CLI   SELTACT,ACTPRT      DON'T TEST BATCH VALID FOR PRINT             
         BE    BATLST54                                                         
         MVC   BCWORK(L'FVOMTYP+L'FVMSGNO),FVOMTYP                              
         GOTO1 ATSTBTY,SELTACT     TEST ACTION VALID FOR BATCH TYPE             
         MVC   FVOMTYP(L'FVOMTYP+L'FVMSGNO),BCWORK                              
         BNE   BATLST56                                                         
         OC    CSBSECL(1),BCCPYEL+(CPYBSEC-CPYELD)                              
*                                                                               
BATLST54 MVC   BLMASK,SELTMASK     EXTRACT SELECT ACTION MASK                   
         NC    BLMASK,LSTTMASK     MERGE VALID ACTION MASK                      
         CLC   BLMASK,SELTMASK     TEST MASK MATCHES                            
         BE    BATLST58                                                         
*                                                                               
         CLC   LSTTRECN,CSSEL#LO                                                
         BE    BATLST64                                                         
         BL    BATLST56                                                         
         TM    BLFLAG,BLFINPT      TEST INPUT THIS TIME                         
         BZ    BATLST64                                                         
*                                                                               
BATLST56 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
*                                                                               
BATLST58 MVC   CSLSTCUR,LSTTABD                                                 
         OI    CSLTINDS,CSLTIANY   SET LINE PROCESSED THIS SCREEN               
         OC    SELTREC(L'SELTREC+L'SELTACT),SELTREC                             
         BZ    BATLST60                                                         
         L     RE,ATWA                                                          
         LA    RF,BLIACT1H                                                      
         SR    RF,RE                                                            
         STCM  RF,3,CSSELACT       SET DISPLACEMENT TO FIELD HEADER             
         L     RE,AOVERSEL                                                      
         LA    RF,SELTABD                                                       
         SR    RF,RE                                                            
         STCM  RF,3,CSSELCUR       SET DISPLACEMENT TO SELTAB ENTRY             
         CLC   CSSEL#LO,LSTTRECN                                                
         BNE   *+8                                                              
         STCM  RF,3,CSSELMUL       SET DISPLACEMENT TO MULTI-ENTRY              
         STC   R0,CSSELREM         SET NUMBER OF LINES REMAINING                
         GOTO1 ANTRSES,SELTPARM                                                 
         EJECT                                                                  
***********************************************************************         
* LINE ACTION COMPLETED                                               *         
***********************************************************************         
         SPACE 1                                                                
BATLSTR1 L     R0,AACTKTAB         ACTIVE KEY TABLE                             
         CLI   BLOSEQ,TBAPTYPQ                                                  
         BNE   *+8                                                              
         L     R0,APASKTAB         PASSIVE KEY TABLE                            
         A     R0,BORELO           RELOCATE A(KEY TABLE)                        
         ST    R0,BLAKTAB                                                       
         SR    R2,R2                                                            
         ICM   R2,3,CSSELACT                                                    
         A     R2,ATWA             R2=A(ACTION LINE)                            
         LA    R3,CSLSTCUR         R3=A(LIST ENTRY)                             
         SR    R4,R4                                                            
         ICM   R4,3,CSSELCUR                                                    
         A     R4,AOVERSEL         R4=A(SELECT TABLE ENTRY)                     
         SR    R0,R0                                                            
         ICM   R0,1,CSSELREM       R0=NUMBER OF LINES REMAINING                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     BATLST62                                                         
*                                                                               
BATLST60 LA    RF,BAT60                                                         
         ICM   RF,8,SELTRTN                                                     
         BASR  RE,RF                                                            
*                                                                               
BATLST62 MVI   BLIACT1,C'*'                                                     
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         MVC   BLIACT1+1(L'BLIACT1-1),0(RE)                                     
         GOTO1 ATSARIO,TSAPUT                                                   
         OI    BLIACT1H+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 BLDLIN,BLILIN1H                                                  
         CLI   BCPFKEY,PFKQUITQ                                                 
         BNE   BATLST66                                                         
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         B     BATLST70                                                         
*                                                                               
BATLST64 XC    BLIACT1,BLIACT1                                                  
         MVI   BLIACT1,C'*'                                                     
         OI    BLIACT1H+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
BATLST66 LA    R2,BLIACT2H         BUMP TO NEXT ACTION FIELD                    
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN       BUMP TO NEXT RECORD NUMBER                   
         LA    R1,1(R1)                                                         
         STCM  R1,3,LSTTRECN                                                    
         BCT   R0,BATLST44         DO FOR NUMBER OF ENTRIES ON SCREEN           
*                                                                               
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
         TM    CSLTINDS,CSLTIEOF   TEST END OF FILE REACHED                     
         BZ    BATLST68                                                         
         CLC   LSTTRECN,CSHIRECN   TEST THIS IS LAST RECORD                     
         BNE   BATLST68                                                         
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         DROP  R2                                                               
*                                                                               
BATLST68 TM    CSLTINDS,CSLTIANY                                                
         BZ    BATLST72                                                         
         TM    CSLTINDS,CSLTIEOL                                                
         BNZ   BATLST72                                                         
*                                                                               
BATLST70 LA    R0,BLIACT1H                                                      
         ST    R0,FVADDR                                                        
         TM    BOINDS1,BOIMSGOK    TEST MESSAGE SET BY APPLICATION              
         BZ    BATLST71                                                         
         MVC   FVMSGNO,BOELEM                                                   
         MVC   FVOMTYP,BOELEM+2                                                 
         MVC   FVPARMS,BOELEM+3                                                 
         B     EXIT                                                             
BATLST71 MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE SCROLLING                                                    *         
***********************************************************************         
         SPACE 1                                                                
BATLST72 LA    R2,IOKEY                                                         
         USING TBARECD,R2          R2=A(BATCH RECORD KEY)                       
         MVC   BLPAG#LO,CSPAG#LO   SAVE LAST LOW & HIGH RECORDS                 
         MVC   BLPAG#HI,CSPAG#HI                                                
         XC    CSPAG#LO,CSPAG#LO   CLEAR LOW & HIGH VALUES FOR PAGE             
         XC    CSPAG#HI,CSPAG#HI                                                
         TM    CSLTINDS,CSLTIFST   TEST FIRST FOR LIST                          
         BZ    BATLST74                                                         
         MVI   CSLTINDS,0          CLEAR LIST INDICATORS                        
         MVI   CSLSTNUM,0          CLEAR NUMBER OF LIST ENTRIES                 
         MVC   CSHIRECN,CSPSRECN   RESET HIGH RECORD NUMBER                     
         XC    IOKEY,IOKEY         CLEAR KEY FOR SETKEY ROUTINE                 
         B     BATLST88                                                         
*                                                                               
BATLST74 TM    BCSCROLL,PFKIHORZ   TEST HORIZONTAL SCROLLING                    
         BNZ   *+14                                                             
         MVC   BLSCRNUM,BCSCRNUM   EXTRACT SCROLL MAGNITUDE                     
         B     *+8                                                              
         MVI   BLSCRNUM,PFKIPAGE                                                
         TM    CSLTINDS,CSLTIHLD   TEST HOLD PAGE IF DISPLAY CHANGED            
         BZ    BATLST76                                                         
         MVC   CSPAG#LO,BLPAG#LO   RESTORE LAST LOW & HIGH RECORDS              
         MVC   CSPAG#HI,BLPAG#HI                                                
         B     BATLST94                                                         
*                                                                               
BATLST76 TM    BCSCROLL,PFKIUPDN   TEST SCROLL UP OR DOWN                       
         BZ    BATLST80                                                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,CSPSRECN                                                    
         TM    BLSCRNUM,PFKIMAXN   TEST SCROLL FIRST                            
         BNZ   BATLST78                                                         
         LA    RF,BLISTMAX         SCROLL UP (BACKWARDS)                        
         TM    BLSCRNUM,PFKIHALF                                                
         BZ    *+8                                                              
         SRL   RF,1                                                             
         TM    BLSCRNUM,X'0F'                                                   
         BZ    *+8                                                              
         IC    RF,BLSCRNUM                                                      
         LA    RF,1(RF)                                                         
         SR    RE,RE                                                            
         ICM   RE,3,BLPAG#LO                                                    
         SR    RE,RF               BACK-UP TO RECORD NUMBER-1                   
         BM    *+12                                                             
         CLM   RE,3,CSPSRECN       TEST NOT < LOW RECORD FOR SESSION            
         BNL   BATLST78                                                         
         SR    RE,RE               SET TO START FROM LOW RECORD                 
         ICM   RE,3,CSPSRECN                                                    
*                                                                               
BATLST78 STCM  RE,3,LSTTRECN                                                    
         MVI   CSLSTNUM,0                                                       
         B     BATLST84                                                         
*                                                                               
BATLST80 SR    R1,R1                                                            
         ICM   R1,1,CSLSTNUM       PICK UP NUMBER OF ENTRIES IN PAGE            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   CSLSTNUM,0                                                       
         TM    BLSCRNUM,X'0F'      TEST SCROLL AMOUNT SPECIFIED                 
         BZ    *+16                                                             
         CLM   R1,1,BLSCRNUM       TEST SCROLL EXCEEDS ACTUAL AMOUNT            
         BL    BATLST82                                                         
         IC    R1,BLSCRNUM                                                      
         TM    BLSCRNUM,PFKIHALF   TEST HALF PAGE SCROLL                        
         BZ    *+8                                                              
         SRL   R1,1                                                             
         SH    R1,=H'1'                                                         
         BM    BATLST82                                                         
         SR    R0,R0                                                            
         ICM   R0,3,BLPAG#LO                                                    
         AR    R1,R0                                                            
         STCM  R1,3,LSTTRECN                                                    
         TM    CSLTINDS,CSLTIEOF   TEST END OF FILE ENCOUNTERED                 
         BZ    BATLST84                                                         
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         BL    BATLST84                                                         
*                                                                               
BATLST82 MVC   LSTTRECN,CSPSRECN   SET TO DISPLAY FIRST PAGE                    
*                                                                               
BATLST84 SR    RE,RE               BUMP TO NEXT RECORD                          
         ICM   RE,3,LSTTRECN                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSHIRECN   TEST IN TSAR BUFFER                          
         BH    BATLST86            NO - GET NEXT BATCH RECORD                   
         GOTO1 LSTADD,1            ADD ENTRY TO LSTTAB                          
         BE    BATLST84                                                         
         B     BATLST94                                                         
*                                                                               
BATLST86 TM    CSLTINDS,CSLTIEOF   TEST EOF ENCOUNTERED                         
         BNZ   BATLST94                                                         
         MVC   TBAKEY,BLTBANXT     GET NEXT BATCH RECORD                        
         B     BATLST90                                                         
*                                                                               
BATLST88 GOTO1 SETKEY,BLAKTAB      SET NEXT BATCH RECORD KEY                    
         BNH   BATLST90                                                         
         OI    CSLTINDS,CSLTIEOF   SET END OF FILE                              
         B     BATLST94                                                         
*                                                                               
BATLST90 GOTO1 AIO,IOHID+IOACCDIR+IO1                                           
         GOTO1 FLTDIR              FILTER THE BATCH DIRECTORY KEY               
         BNE   BATLST88                                                         
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         XC    LSTTABD(LSTTABL),LSTTABD                                         
         GOTO1 LSTBLD              BUILD LIST ENTRY/TEST SECURITY               
         BNE   BATLST88                                                         
         GOTO1 FLTBAT              FILTER THE BATCH RECORD                      
         BNE   BATLST88                                                         
         SR    RE,RE                                                            
         ICM   RE,3,CSHIRECN                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,LSTTRECN                                                    
         GOTO1 LSTADD,0            ADD ENTRY TO LIST                            
         BE    BATLST88                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY PAGE OF DATA                                                *         
***********************************************************************         
         SPACE 1                                                                
BATLST94 BAS   RE,BLDDIS           BUILD COLUMN HEADING & DISPLACEMENTS         
         MVC   BLIHD1,BLDHEAD1                                                  
         OI    BLIHD1H+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   BLIHD2,BLDHEAD2                                                  
         OI    BLIHD2H+(FVOIND-FVIHDR),FVOXMT                                   
         TWAXC BLIACT1H,PROT=Y                                                  
*                                                                               
         CLC   CSHIRECN,CSPSRECN   TEST ANY RECORDS FOUND                       
         BNE   BATLST96                                                         
         MVC   FVMSGNO,=AL2(AI$NOBAF)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R0,BLISEQH                                                       
         ST    R0,FVADDR                                                        
         XC    BLVALS(BLVALSL),BLVALS                                           
         B     EXIT                                                             
*                                                                               
BATLST96 MVC   LSTTRECN,CSPAG#LO                                                
         CLC   LSTTRECN,CSSEL#HI   TEST > HIGH MULTIPLE SELECT                  
         BNH   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LINES TO DISPLAY                
         BNZ   *+6                                                              
         DC    H'0'                DIE FOR NOW IF NO DISPLAY LINES              
         LA    R2,BLIACT1H                                                      
         USING BLIACT1H,R2         R2=A(SCREEN LINE)                            
BATLST98 GOTO1 ATSARIO,TSAGET      GET BATCH LIST RECORD                        
         GOTO1 BLDLIN,BLILIN1H                                                  
         MVI   BLIACT1H+(FVILEN-FVIHDR),0                                       
         LA    R2,BLIACT2H                                                      
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,LSTTRECN                                                    
         BCT   R0,BATLST98                                                      
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSSEL#LO   TEST < LOW MULTIPLE SELECT                   
         BNL   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         TM    CSLTINDS,CSLTIEOL   TEST SELECT TO END OF LIST                   
         BNZ   BATLST42                                                         
         DROP  R2                                                               
*                                                                               
BATLSTX  LA    R0,BLIACT1H                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$BADEP)                                           
         MVI   FVOMTYP,GTMINF                                                   
         TM    CSLTINDS,CSLTIEOF   TEST END-OF-FILE ENCOUNTERED                 
         BZ    EXIT                                                             
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         BNE   EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$BADNM)                                           
         B     EXIT                                                             
         DROP  R1,R3,RC                                                         
         SPACE 1                                                                
BLISTMAX EQU   (BLIPFKH-BLIACT1H)/(BLIACT2H-BLIACT1H)                           
         SPACE 1                                                                
BLWORKD  DSECT                     ** BATLST LOCAL W/S **                       
BLAKTAB  DS    A                   A(KEY TABLE)                                 
BLBYTE1  DS    XL1                 WORK BYTE 1                                  
BLBYTE2  DS    XL1                 WORK BYTE 2                                  
BLSCRNUM DS    XL1                 SCROLL MAGNITUDE                             
BLOPTS   DS    XL(BLVALSL)         KEY & OPTIONS                                
BLMAXSTA EQU   08                  MAXIMUM NUMBER OF STATUS FILTERS             
BLMAXTYP EQU   16                  MAXIMUM NUMBER OF TYPE FILTERS               
BLSCNTAB DS    (BLMAXTYP)XL32      SCANNER TABLE                                
BLFLAG   DS    XL1                 FLAG BYTE                                    
BLFINPT  EQU   X'80'               USER INPUT IN SELECT FIELD                   
BLFNEXT  EQU   X'40'               NOT FIRST TIME FOR CURRENT RECORD            
BLPAG#LO DS    XL2                 LOW RECORD ON CURRENT PAGE                   
BLPAG#HI DS    XL2                 HIGH RECORD ON CURRENT PAGE                  
BLMASK   DS    XL(L'LSTTMASK)      VALID ACTION MASK WORK AREA                  
BLASEL   DS    XL4                 A(UNIQUE SELECT TABLE ACTION)                
BAT60    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE A BATCH                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING BUWORKD,RC                                                       
BATUPD   LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         TM    CSINDSL1,CSIRDSPC   TEST DISPLAYED FOR CHANGE/CONFIRM            
         BNZ   BATUPD06                                                         
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BO    BATUPD04                                                         
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   BATUPD02                                                         
         CLI   TWASCRN,HEADSCRN    TEST HEADER SCREEN LOADED                    
         BNE   *+12                                                             
         CLI   TWASCRF,1           AND FORMAT 1 (UNPROT KEY)                    
         BE    BATUPD02                                                         
         GOTO1 ADISHDR,1+X'80'     BUILD BATCH HEADER SCREEN                    
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
*                                                                               
BATUPD02 LA    R1,LSTTABD                                                       
         ICM   R1,8,BCEFFS         SET TO TEST BATCH SECURITY                   
         GOTO1 AGETBAT                                                          
         BNE   EXIT                                                             
*                                                                               
BATUPD04 GOTO1 ADISHDR,1           READ & DISPLAY BATCH RECORD                  
         L     R3,AIO2                                                          
         USING TBARECD,R3          R3=A(BATCH RECORD)                           
         LA    R0,BATIUPH                                                       
         ST    R0,FVADDR                                                        
         TM    TBARHSTA,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CCUIB)                                           
         B     EXIT                                                             
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         TM    TBARHSTA,TBAHSDEL   TEST BATCH DELETED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BADEL)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSUPD   TEST BATCH UPDATED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BAUPD)                                           
         B     EXIT                                                             
         GOTO1 TSTBAT,=AL2(LMBATUPD)                                            
         BNE   EXIT                                                             
         B     BATUPD10                                                         
*                                                                               
BATUPD06 GOTO1 TSTBAT,=AL2(LMBATUPD)                                            
         BNE   EXIT                                                             
         CLC   =C'&&',BATITC                                                    
         BE    BATUPD08                                                         
         MVI   FVMINL,1                                                         
         GOTO1 AVALITE,BATITCH     VALIDATE ITEM COUNT                          
         BNE   EXIT                                                             
BATUPD08 CLC   =C'&&',BATAMC                                                    
         BE    BATUPD10                                                         
         MVI   FVMINL,1                                                         
         GOTO1 AVALCSH,BATAMCH     VALIDATE CONTROL AMOUNT                      
         BNE   EXIT                                                             
*                                                                               
BATUPD10 GOTO1 ATSTBMO,1           TEST BATCH MONTH LOCKED                      
         BE    BATUPD12                                                         
         GOTO1 ADISHDR,1                                                        
         LA    R0,BATMOAH                                                       
         ST    R0,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
BATUPD12 LA    R0,BATITCH                                                       
         SR    R1,R1                                                            
         ICM   R1,3,LSTBITMA                                                    
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R1,BOHALF1                                                       
         CLC   =C'&&',BATITC                                                    
         BNE   BATUPD13                                                         
         STH   R1,BOHALF1                                                       
         CURED BOHALF1,(L'BATITC,BATITC),0,ALIGN=LEFT                           
         MVC   LSTBITMC,BOHALF1                                                 
         B     *+12                                                             
BATUPD13 CLM   R1,3,LSTBITMC       TEST ITEMS ADDED V ITEM CONTROL              
         BNE   BATUPD15                                                         
         LA    R0,BATAMCH                                                       
         CLC   =C'&&',BATAMC                                                    
         BNE   BATUPD14                                                         
*&&UK*&& CURED LSTBCSHA,(L'BATAMC,BATAMC),CSCURTAM,FLOAT=-,ALIGN=LEFT           
*&&US*&& CURED LSTBCSHA,(L'BATAMC,BATAMC),CSCURTAM,MINUS=YES,ALIGN=LEFT         
         MVC   LSTBCSHC,LSTBCSHA                                                
         B     *+14                                                             
BATUPD14 CP    LSTBCSHA,LSTBCSHC   TEST CASH ADDED V CASH CONTROL               
         BNE   BATUPD15                                                         
         TM    CSBIND1,TYPICUMU    TEST TYPE ACCUMULATES TOTAL DR/CR            
         BZ    *+14                                                             
         CP    LSTBTDRS,LSTBTCRS   TEST TOTAL DEBITS V TOTAL CREDITS            
         BNE   BATUPD15                                                         
         TM    CSINDSL1,CSIRDSPC   TEST DISPLAYED FOR CHANGE/CONFIRM            
         BNZ   BATUPD16                                                         
         GOTO1 ADISHDR,2           DISPLAY PROTECTED BATCH SCREEN               
         OI    CSINDSL1,CSIRDSPC   SET DISPLAYED FOR CHANGE/CONFIRM             
         LA    R0,BASACTH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$ETUPD)                                           
         TM    LSTBHDS1,BHDSCOPY+BHDSREVS                                       
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$BCETU)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
*                                                                               
BATUPD15 ST    R0,FVADDR                                                        
         GOTO1 ADISHDR,3                                                        
         CURED LSTBITMC,(L'BATITC,BATITC),0,ALIGN=LEFT                          
*&&UK*&& CURED LSTBCSHC,(L'BATAMC,BATAMC),CSCURTAM,FLOAT=-,ALIGN=LEFT           
*&&US*&& CURED LSTBCSHC,(L'BATAMC,BATAMC),CSCURTAM,MINUS=YES,ALIGN=LEFT         
*                                                                               
         OI    CSINDSL1,CSIRDSPC   SET DISPLAYED FOR CHANGE/CONFIRM             
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$BTDIF)                                           
         TM    LSTBHDS1,BHDSCOPY+BHDSREVS                                       
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$BCCTO)                                           
         TM    CSBIND1,TYPICUMU    TEST TYPE ACCUMULATES TOTAL DR/CR            
         BZ    EXIT                                                             
         CP    LSTBTDRS,LSTBTCRS   TEST TOTAL DEBITS V TOTAL CREDITS            
         BE    EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$DNEQC)                                           
         TM    LSTBHDS1,BHDSCOPY+BHDSREVS                                       
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$BCDNC)                                           
         BE    EXIT                                                             
*                                                                               
BATUPD16 TM    LSTBHDS1,BHDSACRU   TEST FIRST ACCRUAL                           
         BZ    BATUPD20                                                         
         MVC   BUBATCUR,LSTTABD    SAVE CURRENT BATCH ENTRY                     
         GOTO1 ABLDACR,LSTTABD     TEST/BUILD ACCRUAL BATCH                     
         MVC   BUBATACR,LSTTABD    SAVE ACCRUAL BATCH ENTRY                     
         MVC   LSTTABD(LSTTABL),BUBATCUR  RESTORE ORIGINAL BATCH ENTRY          
         BE    BATUPD18            BLDACR GAVE OK TO ADD                        
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         B     EXIT                ERROR MESSAGE SET BY BLDACR                  
BATUPD18 TM    LSTBHDS1,BHDSCOPY   TEST ACCRUAL BATCH BEING COPIED              
         BZ    *+8                                                              
         OI    BUBATACR+(LSTBHDS1-LSTTABD),BHDSCOPY                             
         TM    LSTBHDS1,BHDSREVS   TEST ACCRUAL BATCH BEING REVERSED            
         BZ    *+8                                                              
         OI    BUBATACR+(LSTBHDS1-LSTTABD),BHDSREVS                             
         TM    LSTBHDS1,BHDSGENE   TEST ACCRUAL BATCH BEING GENERATED           
         BZ    *+8                                                              
         OI    BUBATACR+(LSTBHDS1-LSTTABD),BHDSGENE                             
*                                                                               
BATUPD20 CLC   LSTBEFDT,BCTODAYC   TEST CHANGE OF EFFECTIVE DATE                
         BE    BATUPD24                                                         
         GOTO1 ABLDBAP,LSTTABD     READ NEW PASSIVE POINTER                     
         MVC   IOKEY+(TBAPEFDT-TBARECD)(L'TBAPEFDT),BCTODAYC                    
         GOTO1 AIO,IORDUPD+IOACCDIR+IO1                                         
         BE    BATUPD21            FOUND, ERROR                                 
         OI    LSTBIND2,LSTBADPP   CAN WE FIND IT WITH THE ADDED DATE?          
         GOTO1 ABLDBAP,LSTTABD     READ THE PASSIVE POINTER                     
         MVC   IOKEY+(TBAPEFDT-TBARECD)(L'TBAPEFDT),BCTODAYC                    
         GOTO1 AIO,IORDUPD+IOACCDIR+IO1                                         
         BNE   BATUPD22                                                         
*                                                                               
BATUPD21 LA    R0,BATEFDH          ERROR CONDITION IF RECORD FOUND              
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$EFBEX)                                           
         B     EXIT                                                             
*                                                                               
BATUPD22 TM    IOERR,IOEDEL+IOERNF TEST NOT FOUND/DELETED                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,IOKEYSAV      ADD/WRITE NEW PASSIVE POINTER                
         MVI   IOKEY+(TBAKHSTA-TBARECD),0                                       
         MVC   IOKEY+(TBAKDA-TBARECD)(L'TBAKDA),LSTTDA                          
         LA    R1,IOADD+IOACCDIR+IO1                                            
         TM    IOERR,IOEDEL                                                     
         BZ    *+8                                                              
         LA    R1,IOWRITE+IOACCDIR+IO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ABLDBAP,LSTTABD     DELETE OLD PASSIVE POINTER                   
         GOTO1 AIO,IORDUPD+IOACCDIR+IO1                                         
         BE    BATUPD23            OK                                           
         NI    LSTBIND2,X'FF'-LSTBADPP                                          
         GOTO1 ABLDBAP,LSTTABD     TRY WITHOUT THE DATE ADDED                   
         GOTO1 AIO,IORDUPD+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
BATUPD23 OI    IOKEY+(TBAKHSTA-TBARECD),TBAHSDEL                                
         XC    IOKEY+(TBAKDA-TBARECD)(L'TBAKDA),IOKEY+(TBAKDA-TBARECD)          
         GOTO1 AIO,IOWRITE+IOACCDIR+IO1                                         
         MVC   LSTBEFDT,BCTODAYC   SET NEW EFFECTIVE DATE IN LIST               
*                                                                               
BATUPD24 TIME  DEC                 SET START TIME FOR BATCH/UPDATE              
         STCM  R0,15,BUSTTM                                                     
         GOTO1 AUPDBAT                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AI$BAUPD)                                           
         TM    LSTBHDS1,BHDSACRU   TEST FIRST ACCRUAL                           
         BZ    BATUPD30                                                         
         MVC   BUBATCUR,LSTTABD    SAVE CURRENT BATCH ENTRY                     
         MVC   LSTTABD(LSTTABL),BUBATACR  SET ACCRUAL BATCH ENTRY               
         XC    BUAFFTEL,BUAFFTEL                                                
         SR    R0,R0                                                            
         L     R1,AIO2             R1=A(ORIGINAL BATCH RECORD)                  
         LA    R1,TBARFST-TBARECD(R1)                                           
         USING FFTELD,R1                                                        
BATUPD26 CLI   FFTEL,0                                                          
         BE    BATUPD28                                                         
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTFREE                                                 
         BE    *+14                                                             
         IC    R0,FFTLN                                                         
         AR    R1,R0                                                            
         B     BATUPD26                                                         
         ST    R1,BUAFFTEL                                                      
*                                                                               
BATUPD28 GOTO1 APSTACR,BUPARMS                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LSTTABD(LSTTABL),BUBATCUR  RESTORE ORIGINAL BATCH ENTRY          
         GOTO1 ABLDBAK,LSTTABD                                                  
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AI$AARBU)                                           
*                                                                               
BATUPD30 MVI   FVOMTYP,GTMINF                                                   
         TIME  DEC                 SET START TIME FOR BATCH/UPDATE              
         STCM  R0,15,BUNDTM                                                     
         LA    R3,BOELEM                                                        
         USING LOGRECD,R3          ADD RECORD TO ADRFILE                        
         XC    LOGREC,LOGREC                                                    
         MVC   LOGPGID,=C'$POSTMAN'                                             
         MVC   LOGACTN,BASACT                                                   
         MVC   LOGAGID,CUAALF                                                   
         L     RF,BCAUTL                                                        
         MVC   LOGLUID,TSYM-UTLD(RF)                                            
         MVC   LOGBTYP,LSTBBTYP                                                 
         MVC   LOGITEM,LSTBITMA                                                 
         GOTO1 VGETFACT,BOPARM,0                                                
         L     RF,0(R1)                                                         
         MVC   LOGIOCT,FATIOCNT-FACTSD(RF)                                      
         MVC   LOGDATE,ASBDAT                                                   
         MVC   LOGSTTM,BUSTTM                                                   
         MVC   LOGNDTM,BUNDTM                                                   
         L     RF,ASYSFAC                                                       
         L     RF,VLOGGER-SYSFACD(RF)                                           
         GOTO1 (RF),LOGRECD                                                     
         B     BATXIT                                                           
         DROP  R2,R3,RC                                                         
         SPACE 1                                                                
BUWORKD  DSECT                     ** BATUPD LOCAL W/S **                       
BUBATACR DS    XL(LSTTABL)         ACCRUAL BATCH                                
BUPARMS  DS    0A                                                               
BUAFFTEL DS    A                   A(FIRST BATCH FFTEL) OR ZERO                 
BUBATCUR DS    XL(LSTTABL)         ORIGINAL BATCH                               
BUSTTM   DS    PL4                 START TIME (HHMMSSTH)                        
BUNDTM   DS    PL4                 END TIME   (HHMMSSTH)                        
BUPARMSL EQU   *-BUPARMS                                                        
BAT60    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CLOSE A BATCH                                                       *         
***********************************************************************         
         SPACE 1                                                                
BATCLO   LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BNZ   BATCLO06                                                         
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BO    BATCLO04                                                         
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   BATCLO02                                                         
         CLI   TWASCRN,HEADSCRN    TEST HEADER SCREEN LOADED                    
         BNE   *+12                                                             
         CLI   TWASCRF,1           AND FORMAT 1 (UNPROT KEY)                    
         BE    BATCLO02                                                         
         GOTO1 ADISHDR,1+X'80'     BUILD BATCH HEADER SCREEN                    
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
*                                                                               
BATCLO02 LA    R1,LSTTABD                                                       
         ICM   R1,8,BCEFFS         SET TO TEST BATCH SECURITY                   
         GOTO1 AGETBAT                                                          
         BNE   EXIT                                                             
*                                                                               
BATCLO04 GOTO1 ADISHDR,1           READ & DISPLAY BATCH RECORD                  
         L     R3,AIO2                                                          
         USING TBARECD,R3          R3=A(BATCH RECORD)                           
         LA    R0,BATIUPH                                                       
         ST    R0,FVADDR                                                        
         TM    TBARHSTA,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CCUIB)                                           
         B     EXIT                                                             
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         TM    TBARHSTA,TBAHSDEL   TEST BATCH DELETED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BADEL)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSUPD   TEST BATCH UPDATED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BAUPD)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSEND   TEST BATCH CLOSED                            
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BACLO)                                           
         B     EXIT                                                             
         GOTO1 TSTBAT,=AL2(LMBATCLO)                                            
         BNE   EXIT                                                             
         B     BATCLO08                                                         
*                                                                               
BATCLO06 GOTO1 ABLDBAK,LSTTABD                                                  
         GOTO1 AIO,IORDD+IOACCDIR+IO2                                           
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         LA    R3,IOKEY                                                         
         TM    TBAKHSTA,TBAHSDEL   TEST BATCH DELETED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BADEL)                                           
         B     EXIT                                                             
         TM    TBAKHSTA,TBAHSUPD   TEST BATCH UPDATED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BAUPD)                                           
         B     EXIT                                                             
         TM    TBAKHSTA,TBAHSEND   TEST BATCH CLOSED                            
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BACLO)                                           
         B     EXIT                                                             
*                                                                               
         GOTO1 TSTBAT,=AL2(LMBATCLO)                                            
         BNE   EXIT                                                             
         CLC   =C'&&',BATITC                                                    
         BE    BATCLO07                                                         
         MVI   FVMINL,1                                                         
         GOTO1 AVALITE,BATITCH     VALIDATE ITEM COUNT                          
         BNE   EXIT                                                             
BATCLO07 CLC   =C'&&',BATAMC                                                    
         BE    BATCLO08                                                         
         MVI   FVMINL,1                                                         
         GOTO1 AVALCSH,BATAMCH     VALIDATE CONTROL AMOUNT                      
         BNE   EXIT                                                             
*                                                                               
BATCLO08 CLC   LSTBEFDT,BCTODAYC   TEST MONTH IF EFFECTIVE TODAY                
         BH    BATCLO10                                                         
         GOTO1 ATSTBMO,0           TEST BATCH MONTH LOCKED                      
         BE    BATCLO10                                                         
         GOTO1 ADISHDR,1                                                        
         LA    R0,BATMOAH                                                       
         ST    R0,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
BATCLO10 LA    R0,BATITCH                                                       
         SR    R1,R1                                                            
         ICM   R1,3,LSTBITMA                                                    
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R1,BOHALF1                                                       
         CLC   =C'&&',BATITC                                                    
         BNE   BATCLO11                                                         
         STH   R1,BOHALF1                                                       
         CURED BOHALF1,(L'BATITC,BATITC),0,ALIGN=LEFT                           
         MVC   LSTBITMC,BOHALF1                                                 
         B     *+12                                                             
BATCLO11 CLM   R1,3,LSTBITMC       TEST ITEMS ADDED V ITEM CONTROL              
         BNE   BATCLO13                                                         
         LA    R0,BATAMCH                                                       
         CLC   =C'&&',BATAMC                                                    
         BNE   BATCLO12                                                         
*&&UK*&& CURED LSTBCSHA,(L'BATAMC,BATAMC),CSCURTAM,FLOAT=-,ALIGN=LEFT           
*&&US*&& CURED LSTBCSHA,(L'BATAMC,BATAMC),CSCURTAM,MINUS=YES,ALIGN=LEFT         
         MVC   LSTBCSHC,LSTBCSHA                                                
         B     *+14                                                             
BATCLO12 CP    LSTBCSHA,LSTBCSHC   TEST CASH ADDED V CASH CONTROL               
         BNE   BATCLO13                                                         
         TM    CSBIND1,TYPICUMU    TEST TYPE ACCUMULATES TOTAL DR/CR            
         BZ    *+14                                                             
         CP    LSTBTDRS,LSTBTCRS   TEST TOTAL DEBITS V TOTAL CREDITS            
         BNE   BATCLO13                                                         
         TM    LSTBINDS,LSTBIAUT   TEST AUTO CLOSE FROM ITEM/INPUT              
         BZ    BATCLO18                                                         
         SR    R0,R0                                                            
*                                                                               
BATCLO13 NI    LSTBINDS,FF-LSTBIAUT                                             
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$BTDIF)                                           
         TM    LSTBHDS1,BHDSCOPY+BHDSREVS                                       
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$BCCTO)                                           
         TM    CSBIND1,TYPICUMU    TEST TYPE ACCUMULATES TOTAL DR/CR            
         BZ    BATCLO14                                                         
         CP    LSTBTDRS,LSTBTCRS   TEST TOTAL DEBITS V TOTAL CREDITS            
         BE    BATCLO14                                                         
         MVC   FVMSGNO,=AL2(AI$DNEQC)                                           
         TM    LSTBHDS1,BHDSCOPY+BHDSREVS                                       
         BZ    BATCLO14                                                         
         MVC   FVMSGNO,=AL2(AI$BCDNC)                                           
BATCLO14 LTR   R0,R0                                                            
         BNZ   BATCLO16                                                         
         LA    R0,BATITCH                                                       
         MVC   FVMSGNO,=AL2(AI$BIBEC)                                           
         TM    LSTBHDS1,BHDSCOPY+BHDSREVS                                       
         BZ    BATCLO16                                                         
         MVC   FVMSGNO,=AL2(AI$BCETC)                                           
BATCLO16 ST    R0,FVADDR                                                        
         GOTO1 ADISHDR,3                                                        
         CURED LSTBITMC,(L'BATITC,BATITC),0,ALIGN=LEFT                          
*&&UK*&& CURED LSTBCSHC,(L'BATAMC,BATAMC),CSCURTAM,FLOAT=-,ALIGN=LEFT           
*&&US*&& CURED LSTBCSHC,(L'BATAMC,BATAMC),CSCURTAM,MINUS=YES,ALIGN=LEFT         
         OI    CSINDSL1,CSIRDSPC   SET RECORD DISPLAYED FOR CHANGE              
         B     EXIT                                                             
*                                                                               
BATCLO18 GOTO1 ACLOBAT             CLOSE THE BATCH                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AI$BACLO)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     BATXIT                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SAVE A BATCH                                                        *         
***********************************************************************         
         SPACE 1                                                                
BATSAV   LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BO    BATSAV04                                                         
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   BATSAV02                                                         
         CLI   TWASCRN,HEADSCRN    TEST HEADER SCREEN LOADED                    
         BNE   *+12                                                             
         CLI   TWASCRF,1           AND FORMAT 1 (UNPROT KEY)                    
         BE    BATSAV02                                                         
         GOTO1 ADISHDR,1+X'80'     BUILD BATCH HEADER SCREEN                    
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
*                                                                               
BATSAV02 LA    R1,LSTTABD                                                       
         ICM   R1,8,BCEFFS         SET TO TEST BATCH SECURITY                   
         GOTO1 AGETBAT                                                          
         BNE   EXIT                                                             
*                                                                               
BATSAV04 GOTO1 ADISHDR,1           READ & DISPLAY BATCH RECORD                  
         L     R3,AIO2                                                          
         USING TBARECD,R3          R3=A(BATCH RECORD)                           
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         TM    TBARHSTA,TBAHSDEL   TEST BATCH DELETED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BADEL)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSUPD   TEST BATCH UPDATED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BAUPD)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSEND   TEST BATCH CLOSED                            
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BACLO)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSSAV   TEST BATCH SAVED                             
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CSSAV)                                           
         B     EXIT                                                             
         GOTO1 TSTBAT,=AL2(LMBATSAV)                                            
         BNE   EXIT                                                             
         GOTO1 ASAVBAT             SAVE THE BATCH                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AI$BASAV)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     BATXIT                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE A BATCH                                                      *         
***********************************************************************         
         SPACE 1                                                                
BATDEL   LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BNZ   BATDEL10            (THIS IS NESTED MODE ONLY)                   
         TM    CSINDSL2,CSIRDSPD   TEST RECORD DISPLAYED FOR DELETE             
         BZ    BATDEL02            (THIS IS NATIVE MODE ONLY)                   
         GOTO1 AXITSES             RESTORE PREVIOUS SESSION                     
         CLI   BCPFKEY,PFKQUITQ    TEST USER WANTS OUT                          
         BNE   BATDEL10                                                         
         XC    CSINDSL,CSINDSL     RESET VALIDATION INDICATORS                  
*                                                                               
BATDEL02 TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BO    BATDEL06                                                         
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   BATDEL04                                                         
         CLI   TWASCRN,HEADSCRN    TEST HEADER SCREEN LOADED                    
         BNE   *+12                                                             
         CLI   TWASCRF,1           AND FORMAT 1 (UNPROT KEY)                    
         BE    BATDEL04                                                         
         GOTO1 ADISHDR,1+X'80'     BUILD BATCH HEADER SCREEN                    
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
*                                                                               
BATDEL04 LA    R1,LSTTABD                                                       
         ICM   R1,8,BCEFFS         SET TO TEST BATCH SECURITY                   
         GOTO1 AGETBAT                                                          
         BNE   EXIT                                                             
*                                                                               
BATDEL06 GOTO1 ADISHDR,1           READ & DISPLAY BATCH RECORD                  
         L     R3,AIO2                                                          
         USING TBARECD,R3          R3=A(BATCH RECORD)                           
         LA    R0,BATIUPH                                                       
         ST    R0,FVADDR                                                        
         TM    TBARHSTA,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CCUIB)                                           
         B     EXIT                                                             
         LA    R0,BATREFH          CHECK BATCH STATUS                           
         ST    R0,FVADDR                                                        
         TM    TBARHSTA,TBAHSDEL   TEST BATCH DELETED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BADEL)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSUPD   TEST BATCH UPDATED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BAUPD)                                           
         B     EXIT                                                             
         GOTO1 TSTBAT,=AL2(LMBATDEL)                                            
         BNE   EXIT                                                             
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    *+12                                                             
         OI    CSINDSL1,CSIRDSPC   SET RECORD DISPLAYED FOR CHANGE              
         B     BATDEL08                                                         
         GOTO1 ANTRSES,0           CREATE VERIFICATION SESSION                  
         OI    CSINDSL2,CSIRDSPD   SET RECORD DISPLAYED FOR DELETE              
*                                                                               
BATDEL08 GOTO1 ADISHDR,2           DISPLAY PROTECTED BATCH SCREEN               
         LA    R1,BASACTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$ETDEL)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
*                                                                               
BATDEL10 GOTO1 ADELBAT             DELETE THE BATCH                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AI$BADEL)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     BATXIT                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* APPROVE/UNAPPROVE A BATCH                                           *         
***********************************************************************         
         SPACE 1                                                                
BATUAP   DS    0H                                                               
BATAPP   LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    ERRINVAS                                                         
         GOTO1 ABLDBAP,LSTTABD                                                  
         GOTO1 AIO,IORDUP+IOACCDIR+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,IOKEY                                                         
         USING TBARECD,R3          R3=A(BATCH RECORD)                           
         OI    TBAKHSTA,TBAHSAPR                                                
         CLI   CSACT,ACTAPR                                                     
         BE    *+8                                                              
         NI    TBAKHSTA,FF-TBAHSAPR                                             
         GOTO1 AIO,IOWRITE+IOACCDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ABLDBAK,LSTTABD                                                  
         GOTO1 AIO,IORDUP+IOACCDIR+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TBAKHSTA,TBAHSAPR                                                
         CLI   CSACT,ACTAPR                                                     
         BE    *+8                                                              
         NI    TBAKHSTA,FF-TBAHSAPR                                             
         GOTO1 AIO,IOWRITE+IOACCDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO2                                                          
         OI    TBARHSTA,TBAHSAPR                                                
         CLI   CSACT,ACTAPR                                                     
         BE    *+8                                                              
         NI    TBARHSTA,FF-TBAHSAPR                                             
         LA    R1,TBARFST                                                       
         USING BHDELD,R1                                                        
         SR    R0,R0                                                            
BATAPP02 CLI   BHDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    R1,R0                                                            
         B     BATAPP02                                                         
         MVC   BHDAPRVR,CUPASS                                                  
         CLI   CSACT,ACTAPR                                                     
         BE    *+10                                                             
         XC    BHDAPRVR,BHDAPRVR                                                
         MVC   LSTBAPNO,BHDAPRVR   UPDATE LIST ENTRY                            
         MVC   LSTTSTAT,TBARHSTA                                                
*                                                                               
         GOTO1 AIO,IOPUTREC+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     BATXIT                                                           
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* CHANGE A BATCH                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING BCWORKD,RC                                                       
BATCHA   LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BNZ   BATCHA06                                                         
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BNZ   BATCHA04                                                         
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   BATCHA02                                                         
         CLI   TWASCRN,HEADSCRN    TEST HEADER SCREEN LOADED                    
         BNE   BATCHA01                                                         
         CLI   TWASCRF,4           AND FORMAT 4                                 
         BE    BATCHA02                                                         
         CLI   TWASCRF,6           OR FORMAT 6                                  
         BE    BATCHA02                                                         
*                                                                               
BATCHA01 GOTO1 ADISHDR,4+X'80'     BUILD BATCH HEADER SCREEN                    
         GOTO1 AGETUID,CUUSER                                                   
         MVC   BATUID,BCWORK       SET CONNECTED USER-ID                        
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
*                                                                               
BATCHA02 LA    R1,LSTTABD                                                       
         ICM   R1,8,BCEFFS         SET TO TEST BATCH SECURITY                   
         GOTO1 AGETBAT                                                          
         BNE   EXIT                                                             
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         L     R4,AIO2                                                          
         USING TBARECD,R4                                                       
         TM    TBARHSTA,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INSUP)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSDEL   TEST BATCH DELETED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BADEL)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSUPD   TEST BATCH UPDATED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BAUPD)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSEND   TEST BATCH CLOSED                            
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BACLO)                                           
         B     EXIT                                                             
         GOTO1 TSTBAT,=AL2(LMBATCHA)                                            
         BNE   EXIT                                                             
*                                                                               
BATCHA04 GOTO1 ADISHDR,6           DISPLAY BATCH RECORD                         
         OI    CSINDSL1,CSIRDSPC   SET RECORD DISPLAYED FOR CHANGE              
         OI    BATNAMH+(FVIIND-FVIHDR),FVIVAL                                   
         OI    BATEFDH+(FVIIND-FVIHDR),FVIVAL                                   
         OI    BATITCH+(FVIIND-FVIHDR),FVIVAL                                   
         OI    BATAMCH+(FVIIND-FVIHDR),FVIVAL                                   
         MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R0,BATNAMH                                                       
         ST    R0,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
BATCHA06 GOTO1 ABLDBAP,LSTTABD     BUILD AND SAVE PASSIVE KEY                   
         MVC   BCSAVBAP,IOKEY                                                   
*                                                                               
         MVI   FVMINL,1            BATCH NAME                                   
         GOTO1 AVALBNA,BATNAMH                                                  
         BNE   BATCHAEX                                                         
*                                                                               
         TM    BATEFDH+(FVIIND-FVIHDR),FVIVAL                                   
         BNZ   BATCHA10                                                         
         GOTO1 AVALEFD,BATEFDH     EFFECTIVE DATE                               
         BNE   BATCHAEX                                                         
         CLC   LSTBEFDT,BCBATCUR+(LSTBEFDT-LSTTABD)                             
         BE    BATCHA10                                                         
*                                                                               
         NI    LSTBINDS,FF-LSTBIPPD                                             
         GOTO1 ABLDBAP,LSTTABD     BUILD PASSIVE BATCH HEADER KEY               
         LA    R4,IOKEY                                                         
         XC    TBAPBCHR(TBAKLAST-TBAPBCHR),TBAPBCHR                             
         LA    R1,IOHIUPD+IOACCDIR+IO2                                          
BATCHA08 GOTO1 AIO                 ISSUE I/O                                    
         BL    BATCHAEX            EXIT ON HARDWARE ERROR                       
         TM    IOERR,IOEEOF        OR END OF FILE                               
         BNZ   BATCHAEX                                                         
         CLC   TBAPAS(TBAPTSEQ+2-TBAPAS),IOKEYSAV                               
         BNE   BATCHA10                                                         
         TM    TBAKHSTA,TBAHSDEL   TEST PASSIVE IS DELETED                      
         BZ    BATCHAE1                                                         
         CLC   TBAPBCHR,LSTBBCHR                                                
         BNE   *+10                                                             
         CLC   TBAPOFFC,LSTBOFFC                                                
         BNE   *+8                                                              
         OI    LSTBINDS,LSTBIPPD   SET PASSIVE RECORD FOUND BUT DELETED         
         LA    R1,IOSQUPD+IOACCDIR+IO2                                          
         B     BATCHA08                                                         
*                                                                               
BATCHA10 MVI   FVMINL,1            BATCH ITEM CONTROL                           
         GOTO1 AVALITE,BATITCH                                                  
         BNE   BATCHAEX                                                         
*                                                                               
         MVI   FVMINL,1            BATCH CASH CONTROL                           
         GOTO1 AVALCSH,BATAMCH                                                  
         BNE   BATCHAEX                                                         
*                                                                               
         OC    CSBTYP2,CSBTYP2     TEST AUTOMATIC ACCRUAL                       
         BNZ   *+8                                                              
         NI    LSTBHDS1,FF-(BHDSACRU+BHDSACRV)                                  
         GOTO1 AFVAL,BATACRH       VALIDATE ACCRUAL OPTION                      
         BNE   BATCHA11                                                         
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8              TEST FOR 'NO'                                
         BE    BATCHA11                                                         
         CLC   BC@NO(0),FVIFLD     'NO' IS ALWAYS VALID                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   BC@YES(0),FVIFLD    TEST FOR 'YES'                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     BATCHAEX                                                         
         TM    CSBIND1,TYPIACRL    TEST ACCRUAL VALID FOR BATCH TYPE            
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACRNV)                                           
         B     BATCHAEX                                                         
         OI    LSTBHDS1,BHDSACRU   SET THIS IS AN ACCRUAL BATCH                 
*                                                                               
BATCHA11 GOTO1 ABLDBAK,LSTTABD                                                  
         GOTO1 AIO,IORDUP+IOACCDIR+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO2                                                          
         USING TBARECD,R4                                                       
         MVC   TBAHREDT,LSTBEFDT                                                
         LA    R3,TBARFST                                                       
         SR    R0,R0                                                            
         USING BHDELD,R3                                                        
BATCHA12 CLI   BHDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    R3,R0                                                            
         B     BATCHA12                                                         
*                                                                               
         MVC   BHDNAME,LSTBNAME                                                 
         MVC   BHDITEMC,LSTBITMC                                                
         ZAP   BHDCASHC,LSTBCSHC                                                
         MVC   BHDSTAT1,LSTBHDS1                                                
*                                                                               
         IC    R0,BHDLN                                                         
         AR    R3,R0               POINT TO NEXT ELEMENT                        
         CLI   0(R3),ASKELQ        IS THERE A KEY ELEMENT?                      
         BNE   *+10                NO, CONTINUE                                 
         IC    R0,1(R3)            YES, BUMP PAST IT                            
         AR    R3,R0                                                            
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FFTELQ',AIO2),0,0                  
*                                                                               
         USING FFTELD,R3           R3=A(FREE FORM TEXT ELEMENT)                 
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q                                                    
         MVI   FFTTYPE,FFTTFREE                                                 
         MVI   FFTSEQ,1                                                         
         LA    R1,BATCOM1H                                                      
         LA    R0,2                                                             
BATCHA14 GOTO1 AFVAL,(R1)                                                       
         BNE   BATCHA16                                                         
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         LA    RF,FFTELD(RF)                                                    
         USING FFTDLEN,RF          RF=A(NEXT COMMENT)                           
         MVC   FFTDLEN,FVILEN      SET COMMENT LENGTH                           
         SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+4                                                           
         MVC   FFTDATA(0),FVIFLD   SET COMMENT                                  
         DROP  RF                                                               
         LA    RF,2(RE,RF)                                                      
         LA    RE,FFTELD                                                        
         SR    RF,RE                                                            
         STC   RF,FFTLN            SET NEW ELEMENT LENGTH                       
*                                                                               
BATCHA16 BCT   R0,*+8              BUMP TO NEXT COMMENT IN TWA                  
         B     BATCHA20                                                         
         SR    RE,RE                                                            
BATCHA18 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         AR    R1,RE                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   BATCHA18                                                         
         B     BATCHA14                                                         
*                                                                               
BATCHA20 CLI   FFTLN,FFTLN1Q       TEST ANY COMMENTS                            
         BNE   *+14                                                             
         XC    FFTELD(FFTLN1Q),FFTELD                                           
         B     BATCHA22                                                         
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         AR    R3,RF                                                            
*                                                                               
BATCHA22 MVI   0(R3),0             SET END OF RECORD                            
         LA    R0,TBARECD                                                       
         BCTR  R0,0                                                             
         SR    R3,R0                                                            
         STCM  R3,3,TBARLEN        SET RECORD LENGTH                            
*                                                                               
         GOTO1 AIO,IOPUTREC+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   LSTBEFDT,BCBATCUR+(LSTBEFDT-LSTTABD)                             
         BNE   BATCHA23                                                         
         CLC   LSTBHDS1,BCBATCUR+(LSTBHDS1-LSTTABD)                             
         BNE   BATCHA24                                                         
         B     BATCHA32                                                         
*                                                                               
BATCHA23 MVC   IOKEY(L'TBAKEY),TBAKEY                                           
         MVC   IOKEY+(TBAKSTA-TBARECD)(L'TBAKSTA),TBARSTA                       
         MVC   IOKEY+(TBAKDA-TBARECD)(L'TBAKDA),IODA                            
         GOTO1 AIO,IOWRITE+IOACCDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   IOKEY,BCSAVBAP                                                   
         GOTO1 AIO,IORDUP+IOACCDIR+IO2                                          
         BE    BATCHA2A            FOUND                                        
         MVC   IOKEY+(TBAPTSEQ-TBARECD),LSTBADDT                                
         GOTO1 AIO,IORDUP+IOACCDIR+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BATCHA2A OI    IOKEY+(TBAKHSTA-TBARECD),TBAHSDEL                                
         ICM   R0,15,IODA          SAVE DISK ADDRESS OF BATCH                   
         XC    IOKEY+(TBAKDA-TBARECD)(L'TBAKDA),IOKEY+(TBAKDA-TBARECD)          
         GOTO1 AIO,IOWRITE+IOACCDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ABLDBAP,LSTTABD                                                  
         MVC   IOKEY+(TBAKSTA-TBARECD)(L'TBAKSTA),TBARSTA                       
         STCM  R0,15,IOKEY+(TBAKDA-TBARECD)                                     
         LA    R1,IOADD+IOACCDIR+IO2                                            
         TM    LSTBINDS,LSTBIPPD   TEST PASSIVE RECORD FOUND                    
         BZ    *+8                                                              
         LA    R1,IOWRITE+IOACCDIR+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BATCHA24 GOTO1 ABLDBAK,LSTTABD     UPDATE TRANSACTION VALUES                    
         MVC   BCSAVBAK,IOKEY                                                   
BATCHA25 LA    R4,BCSAVBAK         USE SAVED BATCH KEY                          
         CLC   LSTBITMA,TBAKTSEQ   TEST ALL ITEMS ACCOUNTED FOR                 
         BE    BATCHA32            END OF BATCH                                 
         ICM   R1,3,TBAKTSEQ                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,TBAKTSEQ                                                    
         MVC   IOKEY(L'TBAKEY),BCSAVBAK                                         
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    IOKEY+(TBAKESTA-TBARECD),TBAESLDE                                
         BNZ   BATCHA25                                                         
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO1             R4=A(ITEM RECORD)                            
         LA    R4,TBARFST          R4=A(FIRST ELEMENT)                          
         USING ASKELD,R4                                                        
         SR    R0,R0                                                            
BATCHA26 CLI   ASKEL,0             TEST EOR                                     
         BE    BATCHA25                                                         
         CLI   ASKEL,ASKELQ        TEST ACCOUNT SYSTEM KEY ELEMENT              
         BNE   BATCHA30                                                         
         MVC   IOKEY,ASKKEY        READ TRANSACTION                             
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO3                                                          
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         USING TRSELD,R1                                                        
BATCHA28 IC    R0,TRSLN            BUMP TO NEXT TRANSACTION ELEMENT             
         AR    R1,R0                                                            
         CLI   TRSEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TRSEL,TRSELQ                                                     
         BNE   BATCHA28                                                         
         MVC   TRSEFDT,LSTBEFDT    SET NEW EFFECTIVE DATE                       
         NI    TRSSTAT2,FF-(TRSSACRL+TRSSACRV)                                  
         TM    LSTBHDS1,BHDSACRU                                                
         BZ    *+8                                                              
         OI    TRSSTAT2,TRSSACRL                                                
*                                                                               
         GOTO1 AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BATCHA30 IC    R0,ASKLN            BUMP TO NEXT ITEM RECORD ELEMENT             
         AR    R4,R0                                                            
         B     BATCHA26                                                         
*                                                                               
BATCHA32 MVC   FVMSGNO,=AL2(AI$BACHA)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     BATXIT                                                           
*                                                                               
BATCHAE1 MVC   FVMSGNO,=AL2(AE$EFBEX)                                           
         B     BATCHAEX                                                         
*                                                                               
BATCHAEX MVC   LSTTABD(LSTTABL),BCBATCUR                                        
         B     EXIT                                                             
         DROP  R2,R3,R4,RC                                                      
         SPACE 1                                                                
ACCMST   DC    C'ACCMST  '                                                      
         SPACE 1                                                                
BCWORKD  DSECT                     ** BATCHA LOCAL W/S **                       
BCSAVBAK DS    XL(L'IOKEY)         SAVED BATCH ACTIVE KEY                       
         ORG   BCSAVBAK                                                         
BCSAVBAP DS    XL(L'IOKEY)         SAVED BATCH PASSIVE KEY                      
BAT60    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* BATUPD/BATCLO/BATSAV/BATDEL/BATAPP/BATUAP/BATCHA COMMON EXIT        *         
***********************************************************************         
         SPACE 1                                                                
         USING BXWORKD,RC                                                       
BATXIT   MVC   BXMSGNO,FVMSGNO     SAVE MESSAGE                                 
         MVC   BXOMTYP,FVOMTYP                                                  
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BNZ   BATXIT02                                                         
         LA    R1,1+X'40'                                                       
         CLC   CSRECACT,=AL1(RECBAT,ACTCHA)                                     
         BNE   *+8                                                              
         LA    R1,6+X'40'          BATCH/CHANGE USES FORMAT 6 SCREEN            
         GOTO1 ADISHDR,(R1)                                                     
         B     BATXIT14                                                         
*                                                                               
BATXIT02 CLI   CSACT,ACTSAV        TEST FOR SPECIAL BATCH ACTIONS               
         BE    *+8                                                              
         CLI   CSACT,ACTCLO                                                     
         BE    *+8                                                              
         CLI   CSACT,ACTUPD                                                     
         BNE   BATXIT12                                                         
*                                                                               
         LA    R1,TWASESRA         LOCATE FIRST BATCH SESSION                   
         LA    RE,1                                                             
         SR    RF,RF                                                            
         ICM   RF,1,TWASESNL                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
BATXIT04 CLI   0(R1),RECBAT                                                     
         BE    BATXIT06                                                         
         LA    R1,L'TWASESRA(R1)                                                
         LA    RE,1(RE)                                                         
         BCT   RF,BATXIT04                                                      
         LA    RE,1                EXIT TO FIRST SESSION                        
BATXIT06 CLM   RF,1,=AL1(1)        BUT ONLY IF MORE THAN 1 IS ACTIVE            
         BNH   BATXIT10                                                         
         CLC   L'TWASESRA(L'TWASESRA,R1),=AL1(RECBAT,ACTGEN)                    
         BE    BATXIT08                                                         
         CLC   L'TWASESRA(L'TWASESRA,R1),=AL1(RECBAT,ACTCOP)                    
         BE    BATXIT08                                                         
         CLC   L'TWASESRA(L'TWASESRA,R1),=AL1(RECBAT,ACTREV)                    
         BNE   BATXIT10                                                         
BATXIT08 LA    RE,1(RE)                                                         
BATXIT10 STC   RE,TWASESNL         SET RETURN SESSION NUMBER                    
         GOTO1 AXITSES                                                          
         GOTO1 ARECACT,CSRECACT    RESTORE RECORD/ACTION                        
         CLI   TWASCRN,HEADSCRN    TEST HEADER SCREEN                           
         BNE   BATXIT14                                                         
         LA    R1,1+X'80'                                                       
         CLI   BCBP08,C'Y'         TEST LEAVE BATCH DETAILS INTACT              
         BNE   *+8                                                              
         LA    R1,1+X'40'                                                       
         CLC   CSRECACT,=AL1(RECBAT,ACTOPN)                                     
         BNE   *+8                                                              
         LA    R1,7(R1)            WANT FORMAT 8 FOR BATCH/OPEN                 
         GOTO1 ADISHDR                                                          
         B     BATXIT14                                                         
*                                                                               
BATXIT12 GOTO1 AXITSES                                                          
*                                                                               
BATXIT14 XC    CSINDSL,CSINDSL     CLEAR LOCAL/GLOBAL INDICATORS                
         XC    CSINDSG,CSINDSG                                                  
         MVC   FVMSGNO,BXMSGNO                                                  
         MVC   FVOMTYP,BXOMTYP                                                  
         B     SETCUR                                                           
         DROP  RC                                                               
         SPACE 1                                                                
BXWORKD  DSECT                                                                  
BXMSGNO  DS    XL(L'FVMSGNO)                                                    
BXOMTYP  DS    XL(L'FVOMTYP)                                                    
BAT60    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* RECALL A CLOSED, SAVED OR INSTANT BATCH                             *         
***********************************************************************         
         SPACE 1                                                                
BATRCL   LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         CLI   CSBITS,0            TEST WE HAVE A VALID BATCH                   
         BE    *+12                                                             
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BNZ   BATRCL04                                                         
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   BATRCL02                                                         
         CLI   TWASCRN,HEADSCRN    TEST HEADER SCREEN LOADED                    
         BNE   *+12                                                             
         CLI   TWASCRF,1           AND FORMAT 1 (UNPROT KEY)                    
         BE    BATRCL02                                                         
         GOTO1 ADISHDR,1+X'80'     BUILD BATCH HEADER SCREEN                    
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
*                                                                               
BATRCL02 LA    R1,LSTTABD                                                       
         ICM   R1,8,BCEFFS         SET TO TEST BATCH SECURITY                   
         GOTO1 AGETBAT                                                          
         BNE   EXIT                                                             
         GOTO1 ADISHDR,1+X'40'     DISPLAY BATCH RECORD                         
         L     R3,AIO2                                                          
         USING TBARECD,R3          R3=A(BATCH RECORD)                           
         LA    R0,BATREFH          CHECK BATCH STATUS                           
         ST    R0,FVADDR                                                        
         TM    TBARHSTA,TBAHSDEL   TEST BATCH DELETED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BADEL)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSUPD   TEST BATCH UPDATED                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BAUPD)                                           
         B     EXIT                                                             
         TM    TBARHSTA,TBAHSAPR   TEST BATCH APPROVED                          
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BAAPR)                                           
         B     EXIT                                                             
         GOTO1 TSTBAT,=AL2(LMBATRCL)                                            
         BNE   EXIT                                                             
*                                                                               
BATRCL04 GOTO1 ARCLBAT             RECALL THE BATCH                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BATRCL06 TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    *+14                                                             
         MVC   CSINITRA,CSRECACT                                                
         B     BATRCL08                                                         
         GOTO1 ANTRSES,0           SAVE CURRENT SESSION                         
*                                                                               
BATRCL08 GOTO1 AOVRSCR,BCPARM,('DETLSCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  TRAP HERE IF BITS ARE ZERO                   
         CLI   CSBITS,0            NOTIFY JNEW OR RDES                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AOVRSCR,BCPARM,(CSBITS,BASOLY2H)                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CSBIND2,TYPIOSC     TEST OVERLAY PRESETS SCREEN                  
         BZ    BATRCL10                                                         
         GOTO1 AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         GOTO1 BONTRYA,BCPARM,('TYPIOSC',TWAD),WORKD                            
*                                                                               
BATRCL10 MVI   TWAMODE,0           SET OVERLAY MODE                             
         OI    CSINDSG1,CSINDBHD   SET BATCH HEADER ADDED                       
         MVI   CSOLINE,0           RECALL IMPLIES A NEW SCREEN                  
         MVI   CSOLINEL,FF                                                      
         GOTO1 ARECACT,=AL1(RECITE,ACTINP)                                      
         GOTO1 ABLDDET,0                                                        
         MVC   FVMSGNO,=AL2(AI$BARED)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     SETCUR                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER A BATCH DIRECTORY KEY                             *         
*                                                                     *         
* NTRY - IOKEY CONTAINS BATCH DIRECTORY KEY                           *         
* EXIT - CC=NOT EQUAL IF BATCH FILTERED OUT                           *         
***********************************************************************         
         SPACE 1                                                                
         USING BLWORKD,RC          RC=A(LOCAL W/S)                              
FLTDIR   NTR1  ,                                                                
         LA    R2,IOKEY                                                         
         USING TBARECD,R2          R2=A(BATCH KEY)                              
         LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3          R3=A(LIST TABLE ENTRY)                       
         CLI   BLOSEQ,TBAPTYPQ                                                  
         BE    FLTDIR01                                                         
         OC    TBAKTSEQ,TBAKTSEQ   DON'T WANT ITEM RECORDS                      
         BNZ   FLTDIRN                                                          
FLTDIR01 GOTO1 FLTKEY,BLAKTAB      APPLY BATCH KEY FILTERS                      
         BNE   FLTDIRN                                                          
*                                                                               
         CLI   BLOSEQ,TBAKTYPQ     FILTER EFFECTIVE DATE FOR ACTIVE             
         BNE   FLTDIR02                                                         
         CLC   TBAHKEDT,BLOESTD                                                 
         BL    FLTDIRN                                                          
         CLC   TBAHKEDT,BLOEEND                                                 
         BH    FLTDIRN                                                          
*                                                                               
FLTDIR02 CLI   BLOSEQ,TBAPTYPQ     FILTER CREATED DATE FOR PASSIVES             
         BNE   FLTDIR04                                                         
         OC    TBAKDA,TBAKDA       TEST REDUNDANT POINTER                       
         BZ    FLTDIRN                                                          
         CLC   TBAHKADT,BLOCSTD                                                 
         BL    FLTDIRN                                                          
         CLC   TBAHKADT,BLOCEND                                                 
         BH    FLTDIRN                                                          
*                                                                               
FLTDIR04 ICM   RE,1,BLONMASK       FILTER ON BATCH STATUS                       
         BZ    *+16                                                             
         EX    RE,*+8                                                           
         BZ    FLTDIRN                                                          
         TM    TBAKHSTA,0                                                       
         ICM   RE,1,BLOXMASK                                                    
         BZ    *+16                                                             
         EX    RE,*+8                                                           
         BNZ   FLTDIRN                                                          
         TM    TBAKHSTA,0                                                       
*                                                                               
         OC    BLOOFF,BLOOFF       FILTER OFFICE                                
         BZ    FLTDIR06                                                         
         LA    R1,BCNE                                                          
         TM    BLOOFFI,OPTNEQ                                                   
         BZ    *+8                                                              
         LA    R1,BCEQ                                                          
         MVC   BOHALF1,TBAKOFFC                                                 
         OC    BOHALF1,BCSPACES                                                 
         CLC   BLOOFF,BOHALF1                                                   
         EX    R1,*+4                                                           
         NOP   FLTDIRN                                                          
*                                                                               
FLTDIR06 OC    BLOSTT,BLOSTT       FILTER STATUS                                
         BZ    FLTDIR16                                                         
         CLI   BLOSTT,BLOSTOKQ     TEST OK/*OK                                  
         BNE   FLTDIR12                                                         
         LA    RF,TBAHSUPD+TBAHSIAD+TBAHSEND                                    
         CLC   TBAHKADT,TBAHKEDT   TEST REGULAR/EFFECTIVE BATCH                 
         BNE   FLTDIR08                                                         
         TM    BCCPYST5,CPYSBAPR   TEST APPROVING REGULAR BATCHES               
         BZ    FLTDIR10                                                         
         LA    RF,TBAHSUPD+TBAHSIAD+TBAHSAPR                                    
         B     FLTDIR10                                                         
FLTDIR08 TM    BCCPYST5,CPYSBAPE   TEST APPROVING EFFECTIVE BATCHES             
         BZ    FLTDIR10                                                         
         LA    RF,TBAHSUPD+TBAHSIAD+TBAHSAPR                                    
FLTDIR10 LA    R1,BZQ                                                           
         TM    BLOSTTI,OPTNEQ      TEST 'NOT'                                   
         BZ    *+8                                                              
         LA    R1,BNZQ                                                          
         EX    RF,*+8                                                           
         EX    R1,*+8                                                           
         TM    TBAKHSTA,0                                                       
         NOP   FLTDIRN                                                          
*                                                                               
FLTDIR12 DS    0H                                                               
*                                                                               
FLTDIR16 DS    0H                                                               
*                                                                               
FLTDIRY  MVI   BOBYTE1,1                                                        
         B     FLTDIRX                                                          
FLTDIRN  MVI   BOBYTE1,0                                                        
         B     FLTDIRX                                                          
*                                                                               
FLTDIRX  CLI   BOBYTE1,1           SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
         DROP  R2,R3,RC                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER A BATCH RECORD                                    *         
*                                                                     *         
* NTRY - AIO1 CONTAINS BATCH RECORD                                   *         
* EXIT - CC=NOT EQUAL IF BATCH FILTERED OUT                           *         
***********************************************************************         
         SPACE 1                                                                
FLTBAT   NTR1  ,                                                                
         LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         L     R1,AIO1                                                          
         LA    R1,TBARFST-TBARECD(R1)                                           
         SR    R0,R0                                                            
         USING BHDELD,R1                                                        
FLTBAT02 CLI   BHDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    R1,R0                                                            
         B     FLTBAT02                                                         
*                                                                               
         OC    BLOAPR,BLOAPR       FILTER APPROVER                              
         BZ    *+14                                                             
         CLC   BHDAPRVR,BLOAPR                                                  
         BNE   FLTBATN                                                          
*                                                                               
         CLI   BLOSACRL,0          FILTER ACCRUAL ATTRIBUTE                     
         BE    FLTBAT04                                                         
         MVC   BOBYTE1,BHDSTAT1                                                 
         NI    BOBYTE1,BLOSACRN+BLOSREVN                                        
         BNZ   *+8                                                              
         OI    BOBYTE1,BLOSNONN                                                 
         NC    BOBYTE1,BLOSACRL                                                 
         BNZ   FLTBATN                                                          
*                                                                               
FLTBAT04 MVC   BCWORK(L'LSTTMASK),LSTTMASK                                      
         NC    BCWORK(L'LSTTMASK),BLOMASK                                       
         CLC   BCWORK(L'LSTTMASK),BLOMASK                                       
         BNE   FLTBATN                                                          
*                                                                               
         OC    BLONAME,BLONAME                                                  
         BZ    FLTBAT06                                                         
         IC    R1,BLONAME                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LSTBNAME(0),BLONAME+1                                            
         BNE   FLTBATN                                                          
*                                                                               
FLTBAT06 CLI   BLOBHDS1,0          FILTER BHDEL STATUS - 1                      
         BE    FLTBAT08                                                         
         MVC   BOBYTE1,BHDSTAT1                                                 
         NC    BOBYTE1,BLOBHDS1                                                 
         BZ    FLTBATN                                                          
*                                                                               
FLTBAT08 OC    BLOORG,BLOORG       ORIGINAL BATCH REFERENCE FILTER              
         BZ    FLTBAT12                                                         
         L     RE,AIO1                                                          
         LA    RE,TBARFST-TBARECD(RE)                                           
         SR    R0,R0                                                            
         USING ASKELD,RE                                                        
FLTBAT10 CLI   ASKEL,0             TEST END OF RECORD                           
         BE    FLTBATN                                                          
         CLI   ASKEL,ASKELQ        TEST KEY ELEMENT                             
         BE    *+14                                                             
         IC    R0,ASKLN                                                         
         AR    RE,R0                                                            
         B     FLTBAT10                                                         
         CLC   BLOORG,ASKKEY+(TBAKBREF-TBARECD)                                 
         BNE   FLTBATN                                                          
         DROP  RE                                                               
*                                                                               
FLTBAT12 DS    0H                                                               
*                                                                               
FLTBATY  MVI   BOBYTE1,1                                                        
         B     FLTBATX                                                          
FLTBATN  MVI   BOBYTE1,0                                                        
         B     FLTBATX                                                          
*                                                                               
FLTBATX  CLI   BOBYTE1,1           SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET NEXT BATCH RECORD KEY                                *         
***********************************************************************         
         SPACE 1                                                                
         USING SKWORKD,RC                                                       
FLTKEY   NTR1  WORK=(RC,SKWORKX-SKWORKD)                                        
         MVI   SKACTN,SKAFLT       SET ACTION TO 'FILTER'                       
         B     SETKEY00                                                         
         SPACE 1                                                                
SETKEY   NTR1  WORK=(RC,SKWORKX-SKWORKD)                                        
         MVI   SKACTN,SKASET       SET ACTION TO 'SET NEXT KEY'                 
         SPACE 1                                                                
SETKEY00 L     R2,0(R1)            R2=A(KEY TABLE)                              
         ST    R2,SKATAB           SAVE A(KEY TABLE)                            
         USING KEYTABD,R2                                                       
         MVI   SKFUNC,SKFTSTKY     SET TO TEST KEY COMPONENT                    
         MVI   SKACTV,SKANO        SET NO KEY CHANGES                           
SETKEY02 CLI   SKFUNC,SKFTSTKY                                                  
         BE    *+8                                                              
         MVI   SKACTV,SKAYES       SET KEY CHANGED                              
         BNL   SETKEY08                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         SR    RE,RE                                                            
         IC    RE,KEYTKDSP                                                      
         LA    RE,IOKEY(RE)                                                     
         LA    RF,IOKEY+L'IOKEY-1                                               
         SR    RF,RE                                                            
         EX    RF,*+4                                                           
         XC    0(0,RE),0(RE)       CLEAR REMAINDER OF KEY                       
         TM    KEYTIND1,KEYTITAB+KEYTIRNG+KEYTIFIX                              
         BZ    SETKEY06                                                         
         SR    RF,RF                                                            
         ICM   RF,3,KEYTFDSP                                                    
         LA    R0,TWAD                                                          
         TM    KEYTIND1,KEYTITWA                                                
         BNZ   SETKEY04                                                         
         LA    R0,WORKD                                                         
         TM    KEYTIND1,KEYTIWRK                                                
         BNZ   SETKEY04                                                         
         DC    H'0'                INVALID FILTER TABLE                         
SETKEY04 AR    RF,R0               RF=A(FILTER TABLE)                           
         SR    R1,R1                                                            
         IC    R1,KEYTKLEN                                                      
         EX    R1,*+4                                                           
         MVC   0(0,RE),0(RF)       SET LOWEST KEY FILTER VALUE                  
         B     SETKEYNT            NEXT TABLE ENTRY                             
*                                                                               
SETKEY06 TM    KEYTIND1,KEYTILIT   TEST LITERAL VALUE                           
         BO    *+6                                                              
         DC    H'0'                UNKNOWN KEY COMPONENT TYPE                   
         MVC   0(1,RF),KEYTKLIT                                                 
         SR    R1,R1                                                            
         ICM   R1,1,KEYTKLEN                                                    
         BZ    SETKEYNT            NEXT TABLE ENTRY                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     SETKEYNT            NEXT TABLE ENTRY                             
         MVC   1(0,RF),0(RF)                                                    
*                                                                               
SETKEY08 BE    SETKEY12                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         SR    RE,RE                                                            
         IC    RE,KEYTKDSP                                                      
         LA    RE,IOKEY(RE)                                                     
         SR    R1,R1                                                            
         ICM   R1,1,KEYTKLEN                                                    
         LA    RF,0(R1,RE)                                                      
         LA    R1,1(R1)                                                         
         CLI   0(RF),FF                                                         
         BNE   SETKEY10                                                         
         MVI   0(RF),0                                                          
         BCTR  RF,0                                                             
         BCT   R1,*-14                                                          
         B     SETKEYPT                                                         
*                                                                               
SETKEY10 IC    RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)                                                         
         MVI   SKFUNC,SKFSETLO     SET LOW VALUES IN REMAINDER OF KEY           
*                                                                               
SETKEY12 SR    RE,RE                                                            
         IC    RE,KEYTKDSP                                                      
         LA    RE,IOKEY(RE)                                                     
         SR    R1,R1                                                            
         IC    R1,KEYTKLEN                                                      
         TM    KEYTIND1,KEYTITAB+KEYTIRNG+KEYTIFIX                              
         BZ    SETKEY30                                                         
         SR    RF,RF                                                            
         ICM   RF,3,KEYTFDSP                                                    
         LA    R0,TWAD                                                          
         TM    KEYTIND1,KEYTITWA                                                
         BNZ   SETKEY14                                                         
         LA    R0,WORKD                                                         
         TM    KEYTIND1,KEYTIWRK                                                
         BNZ   SETKEY14                                                         
         DC    H'0'                INVALID FILTER TABLE                         
SETKEY14 AR    RF,R0               RF=A(FILTER VALUES)                          
         SR    R0,R0                                                            
         IC    R0,KEYTFNUM                                                      
         TM    KEYTIND1,KEYTITAB   TEST TABULAR                                 
         BZ    SETKEY20                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),0(RF)       TEST ANY VALUE PRESENT                       
         BZ    SETKEYNT            NEXT TABLE ENTRY                             
*                                                                               
SETKEY16 EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),0(RF)       TEST EOT                                     
         BZ    SETKEYPT                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       TEST VALUE                                   
         BE    SETKEYNT            NEXT TABLE ENTRY                             
         BH    SETKEY18                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         MVI   SKFUNC,SKFSETLO     SET LOW VALUES                               
         MVI   SKACTV,SKAYES       SET KEY CHANGED                              
         B     SETKEYNT            NEXT TABLE ENTRY                             
*                                                                               
SETKEY18 LA    RF,1(RF,R1)         NEXT FILTER TABLE ENTRY                      
         BCT   R0,SETKEY16                                                      
         B     SETKEYPT                                                         
*                                                                               
SETKEY20 TM    KEYTIND1,KEYTIRNG   TEST RANGE                                   
         BZ    SETKEY24                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       TEST AGAINST RANGE START VALUE               
         BE    SETKEYNT            NEXT TABLE ENTRY                             
         BH    SETKEY22                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         MVI   SKFUNC,SKFSETLO     SET LOW VALUES                               
         B     SETKEYNT            NEXT TABLE ENTRY                             
*                                                                               
SETKEY22 LA    RF,1(RF,R1)         RF=A(RANGE END VALUE)                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       TEST AGAINST RANGE END VALUE                 
         BH    SETKEYPT                                                         
         B     SETKEYNT            NEXT TABLE ENTRY                             
*                                                                               
SETKEY24 TM    KEYTIND1,KEYTIFIX                                                
         BO    *+6                                                              
         DC    H'0'                INVALID KEY COMPONENT TYPE                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       TEST VALUE                                   
         BE    SETKEYNT            NEXT TABLE ENTRY                             
         BH    SETKEYPT                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       SET VALUE                                    
         MVI   SKFUNC,SKFSETLO     SET LOW VALUES                               
         B     SETKEYNT            NEXT TABLE ENTRY                             
*                                                                               
SETKEY30 TM    KEYTIND1,KEYTILIT                                                
         BO    *+6                                                              
         DC    H'0'                INVALID KEY COMPONENT TYPE                   
         MVC   SKFILL(L'KEYTKLIT),KEYTKLIT                                      
         MVC   SKFILL+L'KEYTKLIT(L'SKFILL-1),SKFILL                             
         SR    R1,R1                                                            
         IC    R1,KEYTKLEN                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),SKFILL      TEST VALUE                                   
         BE    SETKEYNT            NEXT TABLE ENTRY                             
         BH    SETKEYPT                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         EX    R1,*+4                                                           
         MVC   0(0,RE),SKFILL      SET VALUE                                    
         BZ    SETKEYNT            NEXT TABLE ENTRY                             
         MVI   SKFUNC,SKFSETLO     SET LOW VALUES                               
         B     SETKEYNT            NEXT TABLE ENTRY                             
*                                                                               
SETKEYPT CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         MVI   SKFUNC,SKFSETNX     SET NEXT KEY VALUE                           
         SH    R2,=Y(KEYTABL)      BACK UP TO PREVIOUS KEYTAB ENTRY             
         C     R2,SKATAB                                                        
         BNL   SETKEY02                                                         
         B     SETKEYN             EOF                                          
*                                                                               
SETKEYNT CLI   KEYTABD+KEYTABL,KEYTEOTQ  TEST END OF TABLE IS NEXT              
         BE    *+12                                                             
         AH    R2,=Y(KEYTABL)      BUMP TO NEXT KEYTAB ENTRY                    
         B     SETKEY02                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYY                                                          
         CLI   SKACTV,SKANO        TEST KEY CHANGED                             
         BNE   SETKEYY                                                          
         LA    RE,IOKEY+L'ACCKEY-1                                              
         CLI   0(RE),FF                                                         
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         IC    RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)                                                         
         B     SETKEYY                                                          
*                                                                               
SETKEYN  MVI   SKFUNC,2                                                         
         B     SETKEYX                                                          
*                                                                               
SETKEYY  MVI   SKFUNC,1                                                         
*                                                                               
SETKEYX  CLI   SKFUNC,1                                                         
         B     EXIT                                                             
         DROP  R2,RC                                                            
         SPACE 1                                                                
SKWORKD  DSECT                     ** SETKEY LOCAL W/S **                       
SKATAB   DS    A                   A(KEY TABLE)                                 
SKACTN   DS    XL1                 ACTION NUMBER                                
SKAFLT   EQU   1                   APPLY KEY FILTERS                            
SKASET   EQU   2                   SET NEXT KEY                                 
SKFUNC   DS    XL1                 KEY FUNCTION                                 
SKFSETLO EQU   0                   SET LOW VALUE                                
SKFTSTKY EQU   1                   TEST KEY COMPONENT                           
SKFSETNX EQU   2                   SET NEXT KEY VALUE                           
SKACTV   DS    XL1                 KEY CHANGE ACTIVE                            
SKANO    EQU   0                   NO KEY CHANGES                               
SKAYES   EQU   1                   KEY CHANGED                                  
SKFILL   DS    XL16                LITERAL VALUE                                
SKWORKX  EQU   *                                                                
BAT60    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CREATE A BATCH LIST TABLE ELEMENT                        *         
*                                                                     *         
* NTRY - R1=ZERO TO CREATE AN ENTRY, NON-ZERO TO POST AN ENTRY        *         
*        IOKEY CONTAINS BATCH RECORD KEY (IF R1=ZERO)                 *         
*        LSTTRECN IS RECORD NUMBER OR LIST ENTRY (IF R1=NON-ZERO)     *         
*        CSLSTNUM IS NUMBER OF ENTRIES IN PAGE SO FAR                 *         
* EXIT - CC=NOT EQUAL IF PAGE FULL - BLNXTKEY IS NEXT BATCH KEY       *         
***********************************************************************         
         SPACE 1                                                                
LSTADD   NTR1  ,                                                                
         LTR   R1,R1               SAVE BATCH KEY IF ADDING TO LIST             
         BNZ   *+10                                                             
         MVC   BLTBANXT,IOKEY      SAVE NEXT KEY                                
         SR    RE,RE                                                            
         ICM   RE,1,CSLSTNUM                                                    
         LA    R0,1(RE)                                                         
         CLM   R0,1,=AL1(BLISTMAX) TEST TABLE FULL                              
         BH    LSTADDN                                                          
         STC   R0,CSLSTNUM                                                      
         LTR   R1,R1               TEST CREATE/POST                             
         BNZ   LSTADD02                                                         
         GOTO1 ATSARIO,TSAADD      ADD LIST ENTRY TO TSAR                       
*                                                                               
LSTADD02 OC    CSPAG#LO,CSPAG#LO   SET LOW & HIGH RECORDS FOR PAGE              
         BNZ   *+10                                                             
         MVC   CSPAG#LO,CSLSTCUR+(LSTTRECN-LSTTABD)                             
         MVC   CSPAG#HI,CSLSTCUR+(LSTTRECN-LSTTABD)                             
*                                                                               
LSTADDY  CR    RB,RB               SET CC=EQUAL                                 
         B     EXIT                                                             
*                                                                               
LSTADDN  LTR   RB,RB               SET CC=NOT EQUAL FOR FULL PAGE               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD LIST TABLE ENTRY                                   *         
***********************************************************************         
         SPACE 1                                                                
LSTBLD   NTR1  ,                                                                
         LA    R2,IOKEY                                                         
         USING TBARECD,R2          R2=A(BATCH RECORD KEY)                       
         LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3          R3=A(CURRENT LIST ENTRY)                     
         XC    LSTTKEY(LSTTDATL),LSTTKEY                                        
         MVC   LSTBBTYP,TBAKBTYP                                                
         LR    R0,RE                                                            
         GOTO1 AGETBTY,LSTBBTYP                                                 
         LR    RE,R0                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R0,7,FVOMTYP        SAVE MESSAGE TYPE/NUMBER                     
         GOTO1 ATSTBTY,CSACT       TEST BATCH TYPE SECURITY                     
         STCM  R0,7,FVOMTYP        RESTORE MESSAGE TYPE/NUMBER                  
         BNE   LSTBLDN                                                          
         MVC   LSTBGRUP,TBAKGRUP                                                
         MVC   LSTBUSER,TBAKUSER                                                
         MVC   LSTBADDT,TBAHKADT                                                
         MVC   LSTBEFDT,TBAHKEDT                                                
         MVC   LSTBMOSP,TBAKBMOS                                                
         MVC   LSTBMOSC+0(1),LSTBMOSP+0                                         
         OI    LSTBMOSC+0,X'F0'                                                 
         SR    RF,RF                                                            
         IC    RF,LSTBMOSP+1                                                    
         LA    RF,MOSTAB-1(RF)                                                  
         MVC   LSTBMOSC+1(1),0(RF)                                              
         MVC   LSTBBREF,TBAKBREF                                                
         MVC   LSTBOFFC,TBAKOFFC                                                
         MVC   LSTBBCHR,TBAKBCHR                                                
         MVC   LSTBSEQN,TBAKSEQN                                                
         MVC   LSTTSTAT,TBAKHSTA                                                
         TM    TBAKHSTA,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BZ    *+8                                                              
         OI    LSTBINDS,LSTBIIUP                                                
         TM    TBAHKIND,TBAHIGDJ   TEST BATCH CREATED BY ACBG                   
         BZ    *+8                                                              
         OI    LSTBINDS,LSTBBCBG                                                
         TM    TBAHKIND,TBAHIMLT   TEST MULTI ITEM/CHANGE VALID                 
         BZ    *+8                                                              
         OI    LSTBIND2,LSTBIMLT                                                
         TM    CSBIND3,TYPIMSIC    TEST TYPIMSCR ITEM/CHANGE SUPPORTED          
         BZ    *+8                                                              
         OI    LSTBIND2,LSTBIMSI                                                
         TM    CSBIND1,TYPIACRV    TEST BATCH TYPE IS ACCRUAL REVERSAL          
         BZ    *+8                                                              
         OI    LSTBINDS,LSTBACRV                                                
         TM    CSBIND1,TYPICUMU    TEST BATCH ACCUMULATES TOTAL DRS/CRS         
         BZ    *+8                                                              
         OI    LSTBIND2,LSTBICUM                                                
         TM    CSBIND5,TYPIHRTX    TEST BATCH ACCUMULATES HOURS/TAX             
         BZ    *+8                                                              
         OI    LSTBIND2,LSTBIHTX                                                
         TM    CSBIND8,TYPIDISP    TEST BATCH DISPLAY ONLY TYPE                 
         BZ    *+8                                                              
         OI    LSTBIND2,LSTBDISP                                                
         MVC   LSTTDA,TBAKDA                                                    
         MVI   LSTTRTYP,RECBAT                                                  
*                                                                               
         MVC   BOWORK1(L'IOKEY),IOKEY                                           
         GOTO1 ABLDBAP,LSTTABD     READ THE PASSIVE POINTER                     
         GOTO1 AIO,IORDUPD+IOACCDIR+IO3                                         
         BE    LSTBLD01            FOUND IT WITHOUT THE ADDED DATE              
         OI    LSTBIND2,LSTBADPP   CAN WE FIND IT WITH THE ADDED DATE?          
         GOTO1 ABLDBAP,LSTTABD     READ THE PASSIVE POINTER                     
         GOTO1 AIO,IORDUPD+IOACCDIR+IO3                                         
         BE    LSTBLD01            YES - EXCELLENT!                             
         TM    IOERR,IOEDEL        ELSE TEST DELETED                            
         BNZ   *+6                 OK                                           
         DC    H'0'                                                             
LSTBLD01 MVC   IOKEY,BOWORK1                                                    
*                                                                               
         L     R2,AIO1             EXTRACT RECORD VALUES                        
         USING TBARECD,R2                                                       
         LA    R2,TBARFST                                                       
         SR    R0,R0                                                            
         USING BHDELD,R2                                                        
LSTBLD02 CLI   BHDEL,0             TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ        TEST BATCH HEADER ELEMENT                    
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    R2,R0                                                            
         B     LSTBLD02                                                         
         MVC   LSTBITMC,BHDITEMC   EXTRACT VALUES                               
         MVC   LSTBITMA,BHDITEMA                                                
         ZAP   LSTBCSHC,BHDCASHC                                                
         ZAP   LSTBCSHA,BHDCASHA                                                
         MVC   LSTBDELI,BHDDELIT                                                
         MVC   LSTBHISN,BHDHISNO                                                
         CLI   BHDLN,BHDLN2Q                                                    
         BL    *+16                                                             
         ZAP   LSTBTDRS,BHDTOTDR                                                
         ZAP   LSTBTCRS,BHDTOTCR                                                
         MVC   LSTBNAME,BHDNAME                                                 
         MVC   LSTBHDS1,BHDSTAT1                                                
         MVC   LSTBHDS2,BHDSTAT2                                                
         LA    R1,LSTBIBNO         SET INPUT BATCHER NAME, IF PRESENT           
         OC    LSTBIBNO,BHDIBNO                                                 
         BNZ   *+8                                                              
         LA    R1,LSTBBCHR         ELSE SET ORIGINAL BATCHER NAME               
         GOTO1 AGETPID,(R1)                                                     
         MVC   LSTBPID,BCWORK                                                   
         MVC   LSTBAPNO,BHDAPRVR                                                
         GOTO1 ASETMSK,LSTTABD                                                  
*                                                                               
LSTBLDY  CLI   *+1,0                                                            
         B     EXIT                                                             
LSTBLDN  CLI   *,0                                                              
         B     EXIT                                                             
         DROP  R2,R3                                                            
         SPACE 1                                                                
MOSTAB   DC    C'123456789......ABC'                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD COLUMN HEADINGS AND DISPLAY DISPLACEMENTS          *         
***********************************************************************         
         SPACE 1                                                                
         USING BDWORKD,RC                                                       
BLDDIS   NTR1  WORK=(RC,BDWORKX-BDWORKD)                                        
         XC    BDWORKD(BDWORKX-BDWORKD),BDWORKD                                 
         MVC   BLDHEAD1,BCSPACES                                                
         MVC   BLDHEAD2,BCSPACES                                                
         XC    BLDSDSP(BLDSDSPL),BLDSDSP                                        
         XC    BLDRDSP(BLDRDSPL),BLDRDSP                                        
         SR    R0,R0                                                            
         IC    R0,BLODINUM         R0=NUMBER OF DISPLAYED COLUMNS               
         SR    R1,R1                                                            
         IC    R1,BLODINDX         R1=INDEX TO DISPLAYED COLUMNS                
*                                                                               
         TM    BCSCROLL,PFKIHORZ   TEST HORIZONTAL SCROLL SPECIFIED             
         BZ    BLDDIS12                                                         
         TM    BCSCROLL,PFKIUPDN   TEST SCROLL LEFT                             
         BNZ   BLDDIS08                                                         
*                                                                               
         TM    BCSCRNUM,PFKIPAGE   SCROLL RIGHT                                 
         BZ    *+10                                                             
         AR    R1,R0                                                            
         B     BLDDIS12                                                         
         TM    BCSCRNUM,PFKIHALF                                                
         BZ    BLDDIS02                                                         
         SRA   R0,1                                                             
         BNZ   *+6                                                              
         SR    R1,R1                                                            
         AR    R1,R0                                                            
         B     BLDDIS12                                                         
BLDDIS02 MVC   BDTEMP(1),BCSCRNUM                                               
         NI    BDTEMP,X'0F'                                                     
         IC    R0,BDTEMP                                                        
         AR    R1,R0                                                            
         B     BLDDIS12                                                         
*                                                                               
BLDDIS08 TM    BCSCRNUM,PFKIMAXN   TEST MAXIMUM SCROLL LEFT                     
         BZ    *+10                                                             
         SR    R1,R1                                                            
         B     BLDDIS12                                                         
         TM    BCSCRNUM,PFKIHALF                                                
         BZ    *+14                                                             
         SRL   R0,1                                                             
         SR    R1,R0                                                            
         B     BLDDIS12                                                         
         TM    BCSCRNUM,PFKIPAGE                                                
         BZ    BLDDIS10                                                         
         LTR   R1,R1                                                            
         BZ    BLDDIS12                                                         
         LA    RE,BDTEMP           INVERT DISPLAY PROFILE                       
         LA    RF,BLODIS(R1)                                                    
         BCTR  RF,0                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         OI    BDFLAG,BDFINVRT                                                  
         B     BLDDIS22                                                         
BLDDIS10 MVC   BDTEMP(1),BCSCRNUM                                               
         NI    BDTEMP,X'0F'                                                     
         IC    R0,BDTEMP                                                        
         SR    R1,R0                                                            
*                                                                               
BLDDIS12 LA    RE,BLODIS+L'BLODIS-1                                             
         LA    RF,L'BLODIS                                                      
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         DC    H'0'                                                             
         LTR   R1,R1                                                            
         BM    BLDDIS14                                                         
         CR    R1,RF                                                            
         BL    BLDDIS16                                                         
*                                                                               
BLDDIS14 SR    R1,R1                                                            
*                                                                               
BLDDIS16 STC   R1,BLODINDX                                                      
         SR    RF,R1                                                            
         LA    R1,BLODIS(R1)                                                    
         EX    RF,*+4                                                           
         MVC   BDTEMP(0),0(R1)                                                  
*                                                                               
BLDDIS22 LA    R1,BDTEMP           R1=A(DISPLAY COLUMN PROFILE)                 
         SR    R3,R3               R3=DISPLACEMENT TO DISPLAY VALUE             
*                                                                               
BLDDIS24 CLI   0(R1),0             TEST END OF COLUMN DEFINITIONS               
         BE    BLDDIS42                                                         
         L     R2,BOADDR1                                                       
         USING DISTABD,R2          R2=A(DISPLAY COLUMN TABLE)                   
BLDDIS26 CLI   DISTABD,EOT         TEST EOT                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DISCOL,0(R1)        TEST COLUMN MATCH                            
         BE    *+12                                                             
         LA    R2,DISTABL(R2)                                                   
         B     BLDDIS26                                                         
*                                                                               
         TM    BDFLAG,BDFPASS2     TEST SECOND PASS                             
         BNZ   BLDDIS32                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,DISWID                                                        
         AR    RE,R3               ADD DISPLACEMENT SO FAR                      
         CLM   RE,1,=AL1(L'BLILIN1)                                             
         BH    BLDDIS42            OVERFLOW - DROP THIS COLUMN                  
         LA    R3,1(RE)            R3=DISPLACEMENT TO NEXT COLUMN               
         SR    RF,RF                                                            
         IC    RF,BDCOLS           BUMP NUMBER OF DISPLAY COLUMNS               
         LA    RF,1(RF)                                                         
         STC   RF,BDCOLS                                                        
         B     BLDDIS40                                                         
*                                                                               
BLDDIS32 LA    RF,L'FVIHDR(R3)     DISPLACEMENT SO FAR +L'HEADER                
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         STC   RF,BLDSDSP-1(RE)    SET SCREEN LINE DISPLACEMENT                 
         LA    RF,1(R3)            DISPLACEMENT SO FAR +1                       
         STC   RF,BLDRDSP-1(RE)    SET REPORT LINE DISPLACEMENT                 
         SR    RF,RF                                                            
         ICM   RF,3,DISNAM1                                                     
         BZ    BLDDIS34                                                         
         LA    RF,TWAD(RF)                                                      
         SR    RE,RE                                                            
         ICM   RE,1,DISHWI         HEADING WIDTH OVERRIDE                       
         BNZ   *+8                                                              
         IC    RE,DISWID           COLUMN WIDTH                                 
         BCTR  RE,0                                                             
         LA    R4,BLDHEAD1(R3)                                                  
         EX    RE,*+8                                                           
         B     BLDDIS34                                                         
         MVC   0(0,R4),0(RF)                                                    
*                                                                               
BLDDIS34 SR    RF,RF                                                            
         ICM   RF,3,DISNAM2        TEST SECOND HEADING                          
         BZ    BLDDIS36                                                         
         LA    RF,TWAD(RF)         BUILD SECOND HEADING                         
         LA    R4,BLDHEAD2(R3)     INDEX TO SECOND HEADLINE                     
         SR    RE,RE                                                            
         ICM   RE,1,DISHWI         HEADING WIDTH OVERRIDE                       
         BNZ   *+8                                                              
         IC    RE,DISWID           COLUMN WIDTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     BLDDIS38                                                         
         MVC   0(0,R4),0(RF)                                                    
*                                                                               
BLDDIS36 OC    DISNAM1,DISNAM1     TEST FIRST HEADING TO UNDERLINE              
         BZ    BLDDIS38                                                         
         LA    RF,0(RE,R4)         RF=A(LAST POSSIBLE CHARACTER)                
         CLI   0(R4),C' '          LOCATE FIRST CHARACTER                       
         BH    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
         CLI   0(RF),C' '          LOCATE LAST CHARACTER                        
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R4               RF=EXECUTE L'HEADING                         
         BM    BLDDIS38            NOTHING TO UNDERLINE                         
         LA    R4,L'BLDHEAD2(R4)   INDEX TO SECOND HEADLINE                     
         MVI   0(R4),C'-'          UNDERLINE FIRST CHARACTER                    
         SH    RF,=H'1'            DROP A CHARACTER FOR SECOND MOVE             
         BM    BLDDIS38            SINGLE CHARACTER HEADING                     
         EX    RF,*+8                                                           
         B     BLDDIS38                                                         
         MVC   1(0,R4),0(R4)       UNDERLINE REST OF FIRST HEADING              
*                                                                               
BLDDIS38 SR    RE,RE                                                            
         IC    RE,DISWID           RESET COLUMN WIDTH                           
         LA    R3,1(R3,RE)         R3=DISPLACEMENT TO NEXT COLUMN               
         A     R3,BDFULL1          ADD CONSTANT FACTOR FOR SPARE                
*                                                                               
BLDDIS40 LA    R1,1(R1)            BUMP TO NEXT FIELD                           
         B     BLDDIS24            PERFORM FOR NUMBER OF PROFILES               
*                                                                               
BLDDIS42 TM    BDFLAG,BDFPASS2     TEST SECOND PASS                             
         BNZ   BLDDISX             YES - WE HAVE FINISHED                       
         TM    BDFLAG,BDFINVRT     TEST INVERTED LIST                           
         BZ    BLDDIS44                                                         
         MVC   BDSAVE,BDTEMP       RE-INVERT LIST                               
         SR    R1,R1                                                            
         ICM   R1,1,BDCOLS                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RF,BDSAVE(R1)                                                    
         LA    RE,BDTEMP                                                        
         BCTR  RF,0                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         SR    R1,R1               ADJUST INDEX VALUE                           
         IC    R1,BDCOLS                                                        
         SR    R0,R0                                                            
         IC    R0,BLODINDX                                                      
         SR    R0,R1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         STC   R0,BLODINDX                                                      
         NI    BDFLAG,FF-BDFINVRT                                               
*                                                                               
BLDDIS44 OI    BDFLAG,BDFPASS2     SET SECOND TIME                              
         SR    R1,R1                                                            
         IC    R1,BDCOLS                                                        
         LA    R1,BDTEMP(R1)                                                    
         MVI   0(R1),0             SET END OF COLUMN DEFINITION                 
         LA    R1,L'BLILIN1+1                                                   
         SR    R1,R3               GET REMAINING SPACE INTO R1                  
         BNP   BLDDIS22            NOTHING LEFT TO ADD                          
         CLI   BDCOLS,1            TEST SINGLE COLUMN                           
         BE    BLDDIS22            DON'T DIVIDE OR SET FACTOR                   
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    RF,BDCOLS           NO. OF COLS (THOUGH ONE IS FIXED)            
         BCTR  RF,0                                                             
         DR    R0,RF                                                            
         ST    R1,BDFULL1          SET CONSTANT FACTOR FOR SPARE                
         B     BLDDIS22            GO BACK AND SET DISPLACEMENTS                
*                                                                               
BLDDISX  MVC   BLODINUM,BDCOLS     SET NUMBER OF COLUMNS PROCESSED              
         B     EXIT                                                             
         DROP  R2,RC                                                            
         SPACE 1                                                                
BDWORKD  DSECT                     ** BLDDIS LOCAL W/S **                       
BDFULL1  DS    F                                                                
BDTEMP   DS    XL64                                                             
BDSAVE   DS    XL64                                                             
BDFLAG   DS    XL1                                                              
BDFPASS2 EQU   X'80'               ON=SECOND PASS                               
BDFINVRT EQU   X'40'               ON=INVERTED LIST                             
BDCOLS   DS    XL1                                                              
BDCOLUMN DS    XL1                                                              
BDWORKX  EQU   *                                                                
BAT60    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A DISPLAY LINE USING BLDDIS DISPLACEMENTS          *         
***********************************************************************         
         SPACE 1                                                                
         USING LSTTABD,R3                                                       
BLDLIN   NTR1  ,                                                                
         LR    R2,R1               SAVE A(OUTPUT FIELD)                         
         MVC   L'FVIHDR(L'BLILIN1,R2),BCSPACES                                  
         OI    FVOIND-FVIHDR(R2),FVOXMT                                         
         NI    FVATRB-FVIHDR(R2),FF-FVAHIGH                                     
         MVC   IODAOVER,LSTTDA     READ BATCH RECORD                            
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AGETBTY,LSTBBTYP    RESOLVE BATCH TYPE VALUES                    
         ORG   *-2                                                              
         CLC   LSTBBTYP,CSBTYP     TEST BATCH TYPE ALREADY RESOLVED             
         BE    *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,BLDSUSER                                                    
         BZ    BLDLIN06                                                         
         LA    R4,0(R2,R4)                                                      
         LA    R1,IOKEY                                                         
         USING CTIREC,R1                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,LSTBUSER                                                 
         L     R1,AIO1                                                          
         CLC   IOKEY(L'CTIKEY),0(R1)                                            
         BE    BLDLIN02                                                         
         GOTO1 AIO,IOREAD+IOCTFILE+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
BLDLIN02 LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
         USING CTDSCD,R1                                                        
BLDLIN04 CLI   CTDSCEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+14                                                             
         IC    RE,CTDSCLEN                                                      
         AR    R1,RE                                                            
         B     BLDLIN04                                                         
         MVC   0(L'LC$USRID,R4),CTDSC                                           
         DROP  R1                                                               
         SR    R4,R4                                                            
*                                                                               
BLDLIN06 ICM   R4,1,BLDSBREF                                                    
         BZ    BLDLIN08                                                         
         LA    R4,0(R2,R4)                                                      
         MVC   0(L'LSTBBREF,R4),LSTBBREF                                        
         SR    R4,R4                                                            
*                                                                               
BLDLIN08 ICM   R4,1,BLDSITYP                                                    
         BZ    BLDLIN10                                                         
         LA    R4,0(R2,R4)                                                      
         CURED LSTBBTYP,(3,(R4)),0,ALIGN=LEFT                                   
         SR    R4,R4                                                            
*                                                                               
BLDLIN10 ICM   R4,1,BLDSINAM                                                    
         BZ    BLDLIN12                                                         
         LA    R4,0(R2,R4)                                                      
         MVC   0(L'CSBTYPN,R4),CSBTYPN                                          
         SR    R4,R4                                                            
*                                                                               
BLDLIN12 ICM   R4,1,BLDSMNTH                                                    
         BZ    BLDLIN14                                                         
         LA    R4,0(R2,R4)                                                      
         MVC   BOFULL1(2),LSTBMOSP                                              
         MVI   BOFULL1+L'LSTBMOSP,1                                             
         GOTO1 VDATCON,BOPARM,(1,BOFULL1),(9,(R4))                              
         SR    R4,R4                                                            
*                                                                               
BLDLIN14 ICM   R4,1,BLDSEFDT                                                    
         BZ    BLDLIN16                                                         
         LA    R4,0(R2,R4)                                                      
         GOTO1 VDATCON,BOPARM,(2,LSTBEFDT),(17,(R4))                            
         SR    R4,R4                                                            
*                                                                               
BLDLIN16 ICM   R4,1,BLDSINUP                                                    
         BZ    BLDLIN18                                                         
         LA    R4,0(R2,R4)                                                      
         MVC   0(L'BC@NO,R4),BC@NO                                              
         TM    LSTTSTAT,TBAHSIAD                                                
         BZ    *+10                                                             
         MVC   0(L'BC@YES,R4),BC@YES                                            
         SR    R4,R4                                                            
*                                                                               
BLDLIN18 ICM   R4,1,BLDSPRSN                                                    
         BZ    BLDLIN20                                                         
         LA    R4,0(R2,R4)                                                      
         MVC   0(L'LSTBPID,R4),LSTBPID                                          
         SR    R4,R4                                                            
*                                                                               
BLDLIN20 ICM   R4,1,BLDSCRDT                                                    
         BZ    BLDLIN22                                                         
         LA    R4,0(R2,R4)                                                      
         GOTO1 VDATCON,BOPARM,(2,LSTBADDT),(17,(R4))                            
         SR    R4,R4                                                            
*                                                                               
BLDLIN22 ICM   R4,1,BLDSSTAT                                                    
         BZ    BLDLIN24                                                         
         LA    R4,0(R2,R4)                                                      
         LA    RF,DISTSTAT                                                      
         USING DISTABD,RF                                                       
         GOTO1 ADISSTA,BOPARM,(DISWID,(R4)),LSTTABD                             
         DROP  RF                                                               
         SR    R4,R4                                                            
*                                                                               
BLDLIN24 ICM   R4,1,BLDSITMC                                                    
         BZ    BLDLIN26                                                         
         LA    R4,0(R2,R4)                                                      
         CURED LSTBITMC,(L'LC$ITMC1,(R4)),0,ALIGN=LEFT                          
         SR    R4,R4                                                            
*                                                                               
BLDLIN26 ICM   R4,1,BLDSITMA                                                    
         BZ    BLDLIN28                                                         
         LA    R4,0(R2,R4)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,LSTBITMA                                                    
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R0,BOHALF1                                                       
         CURED (R0),(L'LC$ITMS1,(R4)),0,ALIGN=LEFT                              
         SR    R4,R4                                                            
*                                                                               
BLDLIN28 ICM   R4,1,BLDSCSHC                                                    
         BZ    BLDLIN30                                                         
         LA    R4,0(R2,R4)                                                      
*&&UK*&& CURED LSTBCSHC,(L'LC$BATTO,(R4)),CSCURTAM,FLOAT=-                      
*&&US*&& CURED LSTBCSHC,(L'LC$BATTO,(R4)),CSCURTAM,MINUS=YES                    
         SR    R4,R4                                                            
*                                                                               
BLDLIN30 ICM   R4,1,BLDSCSHA                                                    
         BZ    BLDLIN32                                                         
         LA    R4,0(R2,R4)                                                      
*&&UK*&& CURED LSTBCSHA,(L'LC$AMTIN,(R4)),CSCURTAM,FLOAT=-                      
*&&US*&& CURED LSTBCSHA,(L'LC$AMTIN,(R4)),CSCURTAM,MINUS=YES                    
         SR    R4,R4                                                            
*                                                                               
BLDLIN32 ICM   R4,1,BLDSBNAM                                                    
         BZ    BLDLIN34                                                         
         LA    R4,0(R2,R4)                                                      
         MVC   0(L'LSTBNAME,R4),LSTBNAME                                        
         SR    R4,R4                                                            
*                                                                               
BLDLIN34 ICM   R4,1,BLDSOFFC                                                    
         BZ    BLDLIN36                                                         
         LA    R4,0(R2,R4)                                                      
         MVC   0(L'LSTBOFFC,R4),LSTBOFFC                                        
         SR    R4,R4                                                            
*                                                                               
BLDLIN36 ICM   R4,1,BLDSCMTS                                                    
         BZ    BLDLIN50                                                         
         LA    R4,0(R2,R4)                                                      
         L     R1,AIO2                                                          
         LA    R1,TBARFST-TBARECD(R1)                                           
         SR    RE,RE                                                            
         USING FFTELD,R1                                                        
BLDLIN38 CLI   FFTEL,0                                                          
         BE    BLDLIN48                                                         
         CLI   FFTEL,FFTELQ                                                     
         BE    *+14                                                             
         IC    RE,FFTLN                                                         
         AR    R1,RE                                                            
         B     BLDLIN38                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    R0,R1               R0=A(NEXT ELEMENT)                           
         LA    R1,FFTDLEN                                                       
         USING FFTDLEN,R1                                                       
         LA    RF,DISCOMSQ         TOTAL L'COMMENTS AVAILABLE                   
BLDLIN44 CR    R1,R0               TEST BEYOND FFTEL                            
         BE    BLDLIN48                                                         
         IC    RE,FFTDLEN                                                       
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         SR    RF,RE               REDUCE L'COMMENTS AVAILABLE                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R4),FFTDATA                                                  
         CLM   RF,1,=AL1(2)                                                     
         BNH   BLDLIN48                                                         
         LA    R4,2(RE,R4)                                                      
         LA    RE,L'FFTDLEN+1(RE)                                               
         AR    R1,RE                                                            
         BCTR  RF,0                ALLOW FOR SPACE                              
         B     BLDLIN44                                                         
         DROP  R1                                                               
BLDLIN48 SR    R4,R4                                                            
*                                                                               
BLDLIN50 ICM   R4,1,BLDSAPRV                                                    
         BZ    BLDLIN54                                                         
         LA    R4,0(R2,R4)                                                      
         OC    LSTBAPNO,LSTBAPNO                                                
         BZ    BLDLIN52                                                         
         GOTO1 AGETPID,LSTBAPNO                                                 
         MVC   0(L'LSTBPID,R4),BCWORK                                           
BLDLIN52 SR    R4,R4                                                            
*                                                                               
BLDLIN54 ICM   R4,1,BLDSIGRP                                                    
         BZ    BLDLIN56                                                         
         LA    R4,0(R2,R4)                                                      
         MVC   1(L'LSTBGRUP,R4),LSTBGRUP                                        
         SR    R4,R4                                                            
*                                                                               
BLDLIN56 ICM   R4,1,BLDSLUID                                                    
         BZ    BLDLIN60                                                         
         LA    R4,0(R2,R4)                                                      
         L     R1,AIO2                                                          
         LA    R1,TBARFST-TBARECD(R1)                                           
         SR    RE,RE                                                            
         USING BHDELD,R1                                                        
BLDLIN58 CLI   BHDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    RE,BHDLN                                                         
         AR    R1,RE                                                            
         B     BLDLIN58                                                         
         MVC   0(L'BHDLUID,R4),BHDLUID                                          
         DROP  R1                                                               
         SR    R4,R4                                                            
*                                                                               
BLDLIN60 ICM   R4,1,BLDSRNUM                                                    
         BZ    BLDLIN62                                                         
         LA    R4,0(R2,R4)                                                      
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN                                                    
         CVD   R1,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  0(4,R4),BODUB1                                                   
         SR    R4,R4                                                            
*                                                                               
BLDLIN62 ICM   R4,1,BLDSADDR                                                    
         BZ    BLDLIN64                                                         
         LA    R4,0(R2,R4)                                                      
         GOTO1 VHEXOUT,BOPARM,LSTTDA,(R4),L'LSTTDA                              
         SR    R4,R4                                                            
*                                                                               
BLDLIN64 ICM   R4,1,BLDSACTS                                                    
         BZ    BLDLIN76                                                         
         LA    R4,0(R2,R4)                                                      
         MVC   BCWORK,BCSPACES                                                  
         LA    RF,BCWORK                                                        
         L     R1,AOVERSEL                                                      
         USING SELTABD,R1                                                       
BLDLIN66 CLI   SELTABD,EOT         TEST EOT                                     
         BE    BLDLIN70                                                         
         STM   RE,R1,BOPARM                                                     
         GOTO1 ATSTMIX,SELTPARM    VALIDATE RECORD/ACTION                       
         LM    RE,R1,BOPARM                                                     
         BNE   BLDLIN68                                                         
         MVC   BCHALF,SELTMASK                                                  
         NC    BCHALF,LSTTMASK                                                  
         CLC   BCHALF,SELTMASK                                                  
         BNE   BLDLIN68                                                         
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPM       MIXED CASE ACTION WORD                       
         LA    RE,TWAD(RE)                                                      
         MVC   0(3,RF),0(RE)                                                    
         LA    RF,3(RF)                                                         
         MVC   0(L'BCCOMMA,RF),BCCOMMA                                          
         LA    RF,L'BCCOMMA(RF)                                                 
BLDLIN68 LA    R1,SELTABL(R1)                                                   
         B     BLDLIN66                                                         
*                                                                               
BLDLIN70 CLC   BCWORK,BCSPACES                                                  
         BE    BLDLIN74                                                         
         LA    RF,BCWORK+L'LC@VALAC-1                                           
         CLC   0(L'BCCOMMA,RF),BCCOMMA                                          
         BE    BLDLIN72                                                         
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         CLC   0(L'BCCOMMA,RF),BCCOMMA                                          
         BNE   *-12                                                             
BLDLIN72 MVI   0(RF),C' '                                                       
         MVC   0(L'LC@VALAC,R4),BCWORK                                          
         CLI   BCWORK+L'LC@VALAC+1,C' '                                         
         BNH   *+8                                                              
         MVI   L'LC@VALAC-1(R4),C'>'                                            
BLDLIN74 SR    R4,R4                                                            
*                                                                               
BLDLIN76 ICM   R4,1,BLDSTOTD                                                    
         BZ    BLDLIN80                                                         
         TM    LSTBIND2,LSTBICUM+LSTBIHTX                                       
         BZ    BLDLIN78                                                         
         OC    LSTBTDRS,LSTBTDRS   TEST TOTAL DEBITS SET                        
         BZ    BLDLIN78                                                         
         LA    R4,0(R2,R4)                                                      
*&&UK*&& CURED LSTBTDRS,(L'LC$DR,(R4)),CSCURTDR,FLOAT=-                         
*&&US*&& CURED LSTBTDRS,(L'LC$DR,(R4)),CSCURTDR,MINUS=YES                       
BLDLIN78 SR    R4,R4                                                            
*                                                                               
BLDLIN80 ICM   R4,1,BLDSTOTC                                                    
         BZ    BLDLIN82                                                         
         TM    LSTBIND2,LSTBICUM+LSTBIHTX                                       
         BZ    BLDLIN82                                                         
         OC    LSTBTCRS,LSTBTCRS   TEST TOTAL CREDITS SET                       
         BZ    BLDLIN82                                                         
         LA    R4,0(R2,R4)                                                      
*&&UK*&& CURED LSTBTCRS,(L'LC$CR,(R4)),CSCURTCR,FLOAT=-                         
*&&US*&& CURED LSTBTCRS,(L'LC$CR,(R4)),CSCURTCR,MINUS=YES                       
         SR    R4,R4                                                            
*                                                                               
BLDLIN82 ICM   R4,1,BLDSFREF                                                    
         BZ    BLDLIN86                                                         
         LA    R4,0(R2,R4)                                                      
         MVC   0(L'LSTBBREF,R4),LSTBBREF                                        
         CLI   LSTBSEQN,0                                                       
         BE    BLDLIN84                                                         
         LA    R4,L'LSTBBREF-1(R4)                                              
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'/'                                                       
         CURED LSTBSEQN,(3,2(R4)),0,ALIGN=LEFT                                  
BLDLIN84 SR    R4,R4                                                            
*                                                                               
BLDLIN86 DS    0H                                                               
*                                                                               
BLDLINX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET BATCH RECORD AND TEST ACTION VALIDITY                *         
*                                                                     *         
* NTRY - R1=A(2 BYTE RECORD/ACTION MASK (SEE LMXXXXXX EQUATES))       *         
***********************************************************************         
         SPACE 1                                                                
TSTBAT   NTR1  ,                                                                
         LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2                                                       
         MVC   BODUB1(L'LSTTMASK),0(R1)                                         
         NC    BODUB1(L'LSTTMASK),LSTTMASK                                      
         BNZ   TSTBAT02                                                         
         LA    R0,BASACTH          SET CURSOR TO ACTION FIELD                   
         ST    R0,FVADDR           AND EXIT WITH ERROR MESSAGE                  
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         CLI   *,0                 SET CC=NOT EQUAL                             
         B     EXIT                                                             
*                                                                               
TSTBAT02 GOTO1 ATSTBTY,CSACT       TEST ACTION VALID FOR BATCH TYPE             
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXITS FROM PROGRAM                                                  *         
***********************************************************************         
         SPACE 1                                                                
TSTMSG   CLC   FVMSGNO,=AL2(FVFSET)                                             
         BNE   EXIT                                                             
         SPACE 1                                                                
SETCUR   LA    R1,BASOLY1H         SET CURSOR TO FIRST UNPROT FIELD             
         SR    RE,RE                                                            
         LA    RF,OSVALS-1                                                      
         TM    CSBIND8,TYPIXOVL    TEST EXTRA AREA FOR SCREEN                   
         BNO   *+8                                                              
         LA    RF,BSVALS-1                                                      
SETCUR02 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    SETCUR04                                                         
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    SETCUR06                                                         
         BXLE  R1,RE,SETCUR02                                                   
SETCUR04 LA    R1,BASACTH                                                       
SETCUR06 STCM  R1,15,FVADDR                                                     
         B     EXIT                                                             
         SPACE 1                                                                
ERRINVAS MVC   FVMSGNO,=AL2(AE$INACS)                                           
         LA    R0,BASACTH                                                       
         ST    R0,FVADDR                                                        
         SPACE 1                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
FF       EQU   X'FF'                                                            
BCEQ     EQU   X'80'                                                            
BCNE     EQU   X'70'                                                            
DMCB     EQU   BOPARM                                                           
         SPACE 1                                                                
AACTKTAB DC    A(ACTKTAB)                                                       
APASKTAB DC    A(PASKTAB)                                                       
         SPACE 1                                                                
STATAB   DS    0XL5                ** BATCH STATUS TABLE **                     
         DC    AL1(TBAHSDEL),AL2(UC@DELD-TWAD,UC3DELD-TWAD)                     
         DC    AL1(TBAHSIIP),AL2(UC@OPEN2-TWAD,UC4OPEN2-TWAD)                   
         DC    AL1(TBAHSEND),AL2(UC@CLSD-TWAD,UC3CLSD-TWAD)                     
         DC    AL1(TBAHSUPD),AL2(UC@UPDTD-TWAD,UC3UPDTD-TWAD)                   
         DC    AL1(TBAHSIAD),AL2(UC@INSUP-TWAD,UC3INSUP-TWAD)                   
         DC    AL1(TBAHSSAV),AL2(UC@SAVED-TWAD,UC3SAVED-TWAD)                   
         DC    AL1(TBAHSAPR),AL2(UC@APRVD-TWAD,UC3APRVD-TWAD)                   
STATABN  EQU   (*-STATAB)/L'STATAB                                              
         SPACE 1                                                                
STATABD  DSECT                     ** DSECT FOR STATUS TABLE **                 
STATBIT  DS    XL1                 STATUS BIT (SEE TBAKHSTA)                    
STATLNG  DS    AL2                 DISPLACEMENT TO LONG WORD (8)                
STATSHT  DS    AL2                 DISPLACEMENT TO SHORT WORD (3)               
STATABL  EQU   *-STATABD                                                        
BAT60    CSECT                                                                  
         EJECT                                                                  
DISTDD   DS    0X                  ** DDS-ONLY DISPLAY COLUMNS **               
         DC    C'W',AL1(BLDSRNUM+1-BLDSDSP),AL1(L'LC$NUM,0)                     
         DC    AL2(LC$NUM-TWAD,0)                                               
*                                                                               
         DC    C'V',AL1(BLDSADDR+1-BLDSDSP),AL1(L'LC$ADR,0)                     
         DC    AL2(LC$ADR-TWAD,0)                                               
*                                                                               
DISTAB   DS    0X                  ** USER/DDS DISPLAY COLUMNS **               
*                                                                               
         DC    C'?',AL1(BLDSACTS+1-BLDSDSP),AL1(L'LC@VALAC,0)                   
         DC    AL2(LC@VALAC-TWAD,0)                                             
*                                                                               
         DC    C'1',AL1(BLDSUSER+1-BLDSDSP),AL1(L'LC$USRID,0)                   
         DC    AL2(LC$USRID-TWAD,0)                                             
*                                                                               
         DC    C'2',AL1(BLDSBREF+1-BLDSDSP),AL1(L'LC$REF,0)                     
         DC    AL2(LC$REF-TWAD,0)                                               
*                                                                               
         DC    C'3',AL1(BLDSITYP+1-BLDSDSP),AL1(L'LC$INPT1,0)                   
         DC    AL2(LC$INPT1-TWAD,LC$INPT2-TWAD)                                 
*                                                                               
         DC    C'4',AL1(BLDSINAM+1-BLDSDSP),AL1(L'LC$NAME,0)                    
         DC    AL2(LC$NAME-TWAD,0)                                              
*                                                                               
         DC    C'5',AL1(BLDSMNTH+1-BLDSDSP),AL1(L'LC$MOA,0)                     
         DC    AL2(LC$MOA-TWAD,0)                                               
*                                                                               
         DC    C'6',AL1(BLDSEFDT+1-BLDSDSP),AL1(L'LC$EFFDT,0)                   
         DC    AL2(LC$EFFDT-TWAD,0)                                             
*&&US                                                                           
         DC    C'7',AL1(BLDSINUP+1-BLDSDSP),AL1(L'LC$INSU1,0)                   
         DC    AL2(LC$INSU1-TWAD,LC$INSU2-TWAD)                                 
*&&                                                                             
         DC    C'8',AL1(BLDSPRSN+1-BLDSDSP),AL1(L'LC$PRSN,0)                    
         DC    AL2(LC$PRSN-TWAD,0)                                              
*                                                                               
         DC    C'9',AL1(BLDSCRDT+1-BLDSDSP),AL1(L'LC$CRTDT,0)                   
         DC    AL2(LC$CRTDT-TWAD,0)                                             
*&&UK                                                                           
DISTSTAT DC    C'A',AL1(BLDSSTAT+1-BLDSDSP),AL1(18,L'LC$STT)                    
         DC    AL2(LC$STT-TWAD,0)                                               
*&&                                                                             
*&&US                                                                           
DISTSTAT DC    C'A',AL1(BLDSSTAT+1-BLDSDSP),AL1(16,L'LC$STT)                    
         DC    AL2(LC$STT-TWAD,0)                                               
*&&                                                                             
         DC    C'B',AL1(BLDSITMC+1-BLDSDSP),AL1(L'LC$ITMC1,0)                   
         DC    AL2(LC$ITMC1-TWAD,LC$ITMC2-TWAD)                                 
*                                                                               
         DC    C'C',AL1(BLDSITMA+1-BLDSDSP),AL1(L'LC$ITMS1,0)                   
         DC    AL2(LC$ITMS1-TWAD,LC$ITMS2-TWAD)                                 
*                                                                               
         DC    C'D',AL1(BLDSCSHC+1-BLDSDSP),AL1(L'LC$BATTO,0)                   
         DC    AL2(LC$BATTO-TWAD,0)                                             
*                                                                               
         DC    C'E',AL1(BLDSCSHA+1-BLDSDSP),AL1(L'LC$AMTIN,0)                   
         DC    AL2(LC$AMTIN-TWAD,0)                                             
*                                                                               
         DC    C'F',AL1(BLDSBNAM+1-BLDSDSP),AL1(L'LC$BATN,0)                    
         DC    AL2(LC$BATN-TWAD,0)                                              
*                                                                               
         DC    C'G',AL1(BLDSOFFC+1-BLDSDSP),AL1(L'LC$OFF,0)                     
         DC    AL2(LC$OFF-TWAD,0)                                               
*                                                                               
         DC    C'I',AL1(BLDSAPRV+1-BLDSDSP),AL1(L'LC$APRVR,0)                   
         DC    AL2(LC$APRVR-TWAD,0)                                             
*&&US                                                                           
         DC    C'J',AL1(BLDSIGRP+1-BLDSDSP),AL1(L'LC$GROUP,0)                   
         DC    AL2(LC$GROUP-TWAD,0)                                             
*&&                                                                             
         DC    C'K',AL1(BLDSLUID+1-BLDSDSP),AL1(L'LC$LUID,0)                    
         DC    AL2(LC$LUID-TWAD,0)                                              
*                                                                               
         DC    C'L',AL1(BLDSTOTD+1-BLDSDSP),AL1(L'LC$DR,0)                      
         DC    AL2(LC$DR-TWAD,0)                                                
*                                                                               
         DC    C'M',AL1(BLDSTOTC+1-BLDSDSP),AL1(L'LC$CR,0)                      
         DC    AL2(LC$CR-TWAD,0)                                                
*                                                                               
         DC    C'H',AL1(BLDSCMTS+1-BLDSDSP),AL1(DISCOMSQ,L'LC$CMTS)             
         DC    AL2(LC$CMTS-TWAD,0)                                              
*                                                                               
         DC    C'O',AL1(BLDSFREF+1-BLDSDSP),AL1(DISFREFQ,L'LC$REF)              
         DC    AL2(LC$REF-TWAD,0)                                               
*                                                                               
DISTDDN  EQU   (*-DISTDD)/DISTABL                                               
DISTABN  EQU   (*-DISTAB)/DISTABL                                               
DISCOMSQ EQU   60                                                               
DISFREFQ EQU   08                                                               
*                                                                               
         DC    AL1(EOT)            END-OF-TABLE                                 
*                                                                               
         DC    C'L',AL1(21),AL1(0) NEXT USER/DDS COLUMN                         
         DC    AL2(0,0)                                                         
DISTABX  DS    0X                                                               
         SPACE 1                                                                
DISTABD  DSECT                     ** DISPLAY COLUMN TABLE **                   
DISCHR   DS    XL1                 COLUMN CHARACTER                             
DISCOL   DS    AL1                 COLUMN NUMBER                                
DISWID   DS    AL1                 COLUMN WIDTH                                 
DISHWI   DS    AL1                 HEADING WIDTH OVERRIDE                       
DISNAM1  DS    AL2                 DISPLACENT TO COLUMN NAME - 1                
DISNAM2  DS    AL2                 DISPLACENT TO COLUMN NAME - 2                
DISTABL  EQU   *-DISTABD                                                        
BAT60    CSECT                                                                  
         EJECT                                                                  
BLOTAB   DS    0X                  ** BATCH LIST OPTION TABLE **                
*                                                                               
         DC    AL2(UC8PRSN-TWAD,UC3PRSN-TWAD)                                   
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,8,L'TBAKBCHR)                                    
         DC    AL1(1)                                                           
         DC    AL2(1,BLOPER-BLVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*&&US                                                                           
         DC    AL2(UC8GROUP-TWAD,UC3GROUP-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,1,L'BLOGRP)                                      
         DC    AL1(2)                                                           
         DC    AL2(2,BLOGRP-BLVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*&&                                                                             
         DC    AL2(UC8REF-TWAD,UC3REF-TWAD)                                     
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,BLOREFL+1,BLOREFL)                               
         DC    AL1(3)                                                           
         DC    AL2(3,BLOREF-BLVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8OFF-TWAD,UC3OFF-TWAD)                                     
         DC    AL1(OPTNRTN,OPTNEQ)                                              
         DC    AL1(0,0,0,0,0,1,2,L'BLOOFF)                                      
         DC    AL1(4)                                                           
         DC    AL2(4,BLOOFFI-BLVALS)                                            
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,BLODISL,BLODISL)                                 
         DC    AL1(5)                                                           
         DC    AL2(5,BLODISI-BLVALS)                                            
         DC    CL4'++'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8APRVR-TWAD,UC3APRVR-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,8,L'TBAKBCHR)                                    
         DC    AL1(6)                                                           
         DC    AL2(6,BLOAPR-BLVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8USRID-TWAD,UC3USRID-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,10,BLOUSRL)                                      
         DC    AL1(7)                                                           
         DC    AL2(7,BLOUSR-BLVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(1),3X'00'                                                    
*                                                                               
         DC    AL2(UC8GRPID-TWAD,UC3GRPID-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,10,BLOUSRL)                                      
         DC    AL1(8)                                                           
         DC    AL2(8,BLOUSR-BLVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(2),3X'00'                                                    
*                                                                               
         DC    AL2(UC8ALLID-TWAD,UC3ALLID-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,10,BLOUSRL)                                      
         DC    AL1(9)                                                           
         DC    AL2(9,BLOUSR-BLVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(3),3X'00'                                                    
*                                                                               
         DC    AL2(UC8ACRL-TWAD,UC8ACRL-TWAD)                                   
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,10,L'BLOSACRL)                                   
         DC    AL1(10)                                                          
         DC    AL2(10,BLOSACRL-BLVALS)                                          
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8ACT-TWAD,UC3ACT-TWAD)                                     
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,3,L'BLOMASK)                                     
         DC    AL1(11)                                                          
         DC    AL2(11,BLOMASK-BLVALS)                                           
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8NAME-TWAD,UC3NAME-TWAD)                                   
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'LSTBNAME,L'BLONAME)                            
         DC    AL1(12)                                                          
         DC    AL2(12,BLONAME-BLVALS)                                           
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC@COPY2-TWAD,UC@COPY2-TWAD)                                 
         DC    AL1(OPTNRTN+OPTBOOL,0)                                           
         DC    AL1(0,0,0,0,0,1,10,L'BLOBHDS1)                                   
         DC    AL1(13)                                                          
         DC    AL2(13,BLOBHDS1-BLVALS)                                          
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8RVRSL-TWAD,UC8RVRSL-TWAD)                                 
         DC    AL1(OPTNRTN+OPTBOOL,0)                                           
         DC    AL1(0,0,0,0,0,1,10,L'BLOBHDS1)                                   
         DC    AL1(14)                                                          
         DC    AL2(14,BLOBHDS1-BLVALS)                                          
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC@GENED-TWAD,UC@GENED-TWAD)                                 
         DC    AL1(OPTNRTN+OPTBOOL,0)                                           
         DC    AL1(0,0,0,0,0,1,10,L'BLOBHDS1)                                   
         DC    AL1(15)                                                          
         DC    AL2(15,BLOBHDS1-BLVALS)                                          
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC@STT-TWAD,UC@STT-TWAD)                                     
         DC    AL1(OPTNRTN,OPTNEQ)                                              
         DC    AL1(0,0,0,0,0,1,4,L'BLOSTT)                                      
         DC    AL1(16)                                                          
         DC    AL2(16,BLOSTTI-BLVALS)                                           
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8ORGL-TWAD,UC3ORGL-TWAD)                                   
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'TBAKBREF,L'TBAKBREF)                           
         DC    AL1(17)                                                          
         DC    AL2(17,BLOORG-BLVALS)                                            
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
BLOTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
ACTKTAB  DS    0X                  ** ACTIVE BATCH KEY **                       
         DC    AL1(TBAKTYP-TBAKEY,L'TBAKTYP-1)                                  
         DC    AL1(TBAKTYPQ,0)                                                  
         DC    AL1(0)                                                           
         DC    AL1(KEYTILIT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAKCPY-TBAKEY,L'TBAKCPY-1)                                  
         DC    AL2(CUABIN-WORKD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIFIX+KEYTIWRK)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAKUSER-TBAKEY,L'TBAKUSER-1)                                
         DC    AL2(BLOUSR-TWAD)                                                 
         DC    AL1(BLOUSRMX)                                                    
         DC    AL1(KEYTITAB+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAKADDT-TBAKEY,L'TBAKADDT-1)                                
         DC    AL2(BLOCXST-TWAD)                                                
         DC    AL1(2)                                                           
         DC    AL1(KEYTIRNG+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAKGRUP-TBAKEY,L'TBAKGRUP-1)                                
         DC    AL2(BLOGRP-TWAD)                                                 
         DC    AL1(1)                                                           
         DC    AL1(KEYTITAB+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAKBTYP-TBAKEY,L'TBAKBTYP-1)                                
         DC    AL2(BLOTYPS-TWAD)                                                
         DC    AL1(BLMAXTYP)                                                    
         DC    AL1(KEYTITAB+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAKBMOS-TBAKEY,L'TBAKBMOS-1)                                
         DC    AL2(BLOMOA-TWAD)                                                 
         DC    AL1(2)                                                           
         DC    AL1(KEYTIRNG+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAKBREF-TBAKEY,L'TBAKBREF-1)                                
         DC    AL2(BLOREF-TWAD)                                                 
         DC    AL1(2)                                                           
         DC    AL1(KEYTIRNG+KEYTIFLN+KEYTITWA)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAKBCHR-TBAKEY,L'TBAKBCHR-1)                                
         DC    AL2(BLOPER-TWAD)                                                 
         DC    AL1(1)                                                           
         DC    AL1(KEYTITAB+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
ACTKTABX DC    AL1(KEYTEOTQ)                                                    
         EJECT                                                                  
PASKTAB  DS    0X                  ** PASSIVE BATCH KEY **                      
         DC    AL1(TBAPTYP-TBAKEY,L'TBAPTYP-1)                                  
         DC    AL1(TBAPTYPQ,0)                                                  
         DC    AL1(0)                                                           
         DC    AL1(KEYTILIT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAPCPY-TBAKEY,L'TBAPCPY-1)                                  
         DC    AL2(CUABIN-WORKD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIFIX+KEYTIWRK)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAPUSER-TBAKEY,L'TBAPUSER-1)                                
         DC    AL2(BLOUSR-TWAD)                                                 
         DC    AL1(BLOUSRMX)                                                    
         DC    AL1(KEYTITAB+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAPEFDT-TBAKEY,L'TBAPEFDT-1)                                
         DC    AL2(BLOESTD-TWAD)                                                
         DC    AL1(2)                                                           
         DC    AL1(KEYTIRNG+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAPGRUP-TBAKEY,L'TBAPGRUP-1)                                
         DC    AL2(BLOGRP-TWAD)                                                 
         DC    AL1(1)                                                           
         DC    AL1(KEYTITAB+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAPBTYP-TBAKEY,L'TBAPBTYP-1)                                
         DC    AL2(BLOTYPS-TWAD)                                                
         DC    AL1(BLMAXTYP)                                                    
         DC    AL1(KEYTITAB+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAPBMOS-TBAKEY,L'TBAPBMOS-1)                                
         DC    AL2(BLOMOA-TWAD)                                                 
         DC    AL1(2)                                                           
         DC    AL1(KEYTIRNG+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAPBREF-TBAKEY,L'TBAPBREF-1)                                
         DC    AL2(BLOREF-TWAD)                                                 
         DC    AL1(2)                                                           
         DC    AL1(KEYTIRNG+KEYTIFLN+KEYTITWA)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL1(TBAPBCHR-TBAKEY,L'TBAPBCHR-1)                                
         DC    AL2(BLOPER-TWAD)                                                 
         DC    AL1(1)                                                           
         DC    AL1(KEYTITAB+KEYTITWA)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
PASKTABX DC    AL1(KEYTEOTQ)                                                    
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH OPTIONS                                              *         
***********************************************************************         
         SPACE 1                                                                
         DROP  R6,R7,R8,RB                                                      
BLOVAL   NMOD1 250,**BLOV**,CLEAR=YES                                           
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     BLOVPER                                                          
         B     BLOVGRP                                                          
         B     BLOVREF                                                          
         B     BLOVOFF                                                          
         B     BLOVDIS                                                          
         B     BLOVAPR                                                          
         B     BLOVUID                                                          
         B     BLOVGID                                                          
         B     BLOVAID                                                          
         B     BLOVACR                                                          
         B     BLOVACT                                                          
         B     BLOVNAM                                                          
         B     BLOVCOP                                                          
         B     BLOVRVS                                                          
         B     BLOVGEN                                                          
         B     BLOVSTA                                                          
         B     BLOVORG                                                          
*                                                                               
BLOVALX  XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERSON=XXXXX                                               *         
***********************************************************************         
         SPACE 1                                                                
BLOVPER  GOTO1 AVALPID,FVIFLD                                                   
*                                                                               
BLOVPERX B     BLOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE GROUP=X                                                    *         
***********************************************************************         
         SPACE 1                                                                
BLOVGRP  CLI   FVIFLD,TBAGGENQ                                                  
         BE    *+12                                                             
         CLI   FVIFLD,TBAGPRDQ                                                  
         BNE   ERRBGRUP                                                         
         MVC   BCWORK,FVIFLD                                                    
BLOVGRPX B     BLOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE REF=AAAA-9999                                              *         
***********************************************************************         
         SPACE 1                                                                
BLOVREF  MVC   BCWORK+L'BLOREFST(L'BLOREFND),BCEFFS                             
         LA    R1,FVIFLD                                                        
         LA    RF,1(R1)                                                         
         CLI   0(R1),C'-'                                                       
         BNE   BLOVRE02                                                         
         CLI   0(RF),C' '                                                       
         BH    BLOVRE08                                                         
         B     ERRSHRT                                                          
*                                                                               
BLOVRE02 CLI   0(RF),C' '                                                       
         BE    BLOVRE04                                                         
         CLI   0(RF),C'-'                                                       
         BE    BLOVRE04                                                         
         LA    RF,1(RF)                                                         
         B     BLOVRE02                                                         
*                                                                               
BLOVRE04 SR    RF,R1                                                            
         CLM   RF,1,=AL1(L'BLOREFST)                                            
         BH    ERRLONG                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   BCWORK(0),0(R1)                                                  
         LA    R1,1(RF,R1)                                                      
         CLI   0(R1),C'-'          TEST RANGE GIVEN                             
         BE    BLOVRE06                                                         
         EX    RF,*+8                                                           
         B     BLOVREFX                                                         
         MVC   BCWORK+L'BLOREFST(0),BCWORK                                      
*                                                                               
BLOVRE06 LA    RF,1(R1)                                                         
         CLI   0(RF),C' '          TEST END REF INPUT                           
         BNH   BLOVREFX                                                         
*                                                                               
BLOVRE08 LR    R1,RF                                                            
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    *-8                                                              
         SR    RF,R1                                                            
         CLM   RF,1,=AL1(L'BLOREFND)                                            
         BH    ERRLONG                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   BCWORK+L'BLOREFST(0),0(R1)                                       
*                                                                               
         CLC   BCWORK(L'BLOREFST),BCWORK+L'BLOREFST                             
         BH    ERRRANGE            START EXCEEDS END                            
*                                                                               
BLOVREFX B     BLOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE OFFICE=XX                                                  *         
***********************************************************************         
         SPACE 1                                                                
BLOVOFF  LA    R1,1                                                             
         LA    RE,ERROFFN1                                                      
         TM    BCCPYST4,CPYSOFF2                                                
         BZ    *+12                                                             
         LA    R1,2                                                             
         LA    RE,ERROFFN2                                                      
         CLM   R1,1,FVILEN                                                      
         BNER  RE                                                               
*                                                                               
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVI   OFFAACT,OFFAVAL     VALIDATE INPUT OFFICE                        
         MVC   OFFAOFFC,FVIFLD     SET INPUT OFFICE                             
         GOTO1 VOFFAL                                                           
         BNE   ERRSECLO            SECURITY LOCKOUT                             
         MVC   BCWORK,OFFAOFFC     SET OFFICE FILTER                            
*                                                                               
BLOVOFFX B     BLOVALX                                                          
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISPLAY=ABC OR +ABC OR ABC+ OR ++ (ALL COLUMNS)            *         
***********************************************************************         
         SPACE 1                                                                
BLOVDIS  MVI   BCWORK,0                                                         
         SR    RE,RE                                                            
         ICM   RE,1,FVXLEN         TEST MORE THAN ONE CHARACTER INPUT           
         BZ    BLOVDI08                                                         
         CLI   FVIFLD,C'+'         TEST FOR LEADING + SIGN (SUFFIX)             
         BNE   BLOVDI06                                                         
         CLI   FVILEN,2            TEST FOR DIS=++                              
         BNE   BLOVDI04                                                         
         CLI   FVIFLD+1,C'+'                                                    
         BNE   BLOVDI04                                                         
         LA    R1,BCBPDIS          R1=A(PROFILE DISPLAY COLUMNS)                
         OC    0(L'BCBPDIS,R1),0(R1)                                            
         BZ    BLOVDI01                                                         
         LA    R0,L'BCBPDIS                                                     
         L     RF,BOADDR2                                                       
         BASR  RE,RF                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-6                                                           
BLOVDI01 L     R1,BOADDR1                                                       
         USING DISTABD,R1                                                       
         L     RF,BOADDR2                                                       
BLOVDI02 CLI   DISTABD,EOT         TEST END OF TABLE                            
         BE    BLOVDISX                                                         
         BASR  RE,RF               CALL ADDCOL                                  
         LA    R1,DISTABL(R1)                                                   
         B     BLOVDI02                                                         
*                                                                               
BLOVDI04 MVI   BCWORK,BLODISIS                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),FVIFLD+1                                               
         STC   RE,FVILEN                                                        
*                                                                               
         LA    R1,BCBPDIS          R1=A(PROFILE DISPLAY COLUMNS)                
         OC    0(L'BCBPDIS,R1),0(R1)                                            
         BNZ   *+8                                                              
         LA    R1,DEFDISP          OR DEFAULT DISPLAY COLUMNS                   
         LA    R0,L'BCBPDIS                                                     
         L     RF,BOADDR2                                                       
         BASR  RE,RF                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-6                                                           
         B     BLOVDI08                                                         
*                                                                               
BLOVDI06 LA    RF,FVIFLD(RE)       POINT TO END OF INPUT STRING                 
         CLI   0(RF),C'+'          TEST FOR TRAILING + SIGN (PREFIX)            
         BNE   BLOVDI08                                                         
         MVI   BCWORK,BLODISIP                                                  
         MVI   0(RF),C' '                                                       
         STC   RE,FVILEN                                                        
*                                                                               
BLOVDI08 SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
         L     RF,BOADDR2                                                       
BLOVDI10 BASR  RE,RF                                                            
         BE    *+14                                                             
         MVC   FVXTRA(1),0(R1)                                                  
         B     ERRINCOL                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,BLOVDI10                                                      
*                                                                               
         CLI   BCWORK,BLODISIP     TEST INPUT COLUMNS ARE PREFIX                
         BNE   BLOVDISX                                                         
         LA    R1,BCBPDIS          R1=A(PROFILE DISPLAY COLUMNS)                
         OC    0(L'BCBPDIS,R1),0(R1)                                            
         BNZ   *+8                                                              
         LA    R1,DEFDISP          OR DEFAULT DISPLAY COLUMNS                   
         LA    R0,L'BCBPDIS                                                     
         L     RF,BOADDR2                                                       
         BASR  RE,RF                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-6                                                           
*                                                                               
BLOVDISX B     BLOVALX                                                          
         SPACE 1                                                                
DEFDISP  DC    C'1234',AL1(0,0,0,0,0,0)                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE APPROVER=XXX                                               *         
***********************************************************************         
         SPACE 1                                                                
BLOVAPR  GOTO1 AVALPID,FVIFLD                                                   
*                                                                               
BLOVAPRX B     BLOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE USERID(OR UID)=XXXXXXXX                                    *         
***********************************************************************         
         SPACE 1                                                                
BLOVUID  TM    TWAINDS1,TWAIUPID   TEST CONNECTED TO PRINCIPAL ID               
         BZ    BLOVUID5                                                         
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,FVIFLD                                                    
         GOTO1 AIO,IORD+IOCTFILE+IO3                                            
         BNE   ERRKINV                                                          
         L     R2,AIO3                                                          
*                                                                               
         LA    R1,CTIDATA                                                       
         SR    R0,R0                                                            
BLOVUID1 CLI   0(R1),0             TEST EOR                                     
         BE    BLOVUIDX                                                         
         CLI   0(R1),CTDSCELQ      TEST DESCRIPTION ELEMENT (USERID#)           
         BE    BLOVUID3                                                         
         CLI   0(R1),CTAGYELQ      TEST AGENCY ELEMENT                          
         BE    BLOVUID4                                                         
BLOVUID2 IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     BLOVUID1                                                         
*                                                                               
         USING CTDSCD,R1                                                        
BLOVUID3 MVC   BCWORK(L'BLOUSR),CTDSC                                           
         B     BLOVUID2                                                         
*                                                                               
         USING CTAGYD,R1                                                        
BLOVUID4 CLC   CTAGYID,CUAALF                                                   
         BNE   ERRKINV                                                          
         B     BLOVUID2                                                         
*                                                                               
BLOVUID5 TM    TWAINDS1,TWAIUGID   TEST CONNECTED USER IS A GROUP               
         BZ    ERRKINV                                                          
         LA    R2,IOKEY                                                         
         USING CTWREC,R2           BUILD KEY OF GROUP RECORD                    
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVC   CTWKAGY,CUAALF                                                   
         MVI   CTWKREC,CTWKRIDG                                                 
         MVC   CTWKID,BCUSERID     USE CODE FROM USER-ID RECORD                 
         GOTO1 AIO,IORD+IOCTFILE+IO3                                            
         BNE   ERRKINV                                                          
         L     R2,AIO3                                                          
*                                                                               
         LA    R1,CTWDATA                                                       
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         LA    RE,BCWORK                                                        
         USING CTLSTD,R1                                                        
BLOVUID6 CLI   CTLSTD,0                                                         
         BE    BLOVUID8                                                         
         CLI   CTLSTEL,CTLSTELQ                                                 
         BNE   BLOVUID7                                                         
         MVC   0(L'BLOUSR,RE),CTLSTDTA+L'CTIKID                                 
         LA    RE,L'BLOUSR(RE)                                                  
         LA    RF,1(RF)                                                         
BLOVUID7 IC    R0,CTLSTLEN                                                      
         AR    R1,R0                                                            
         B     BLOVUID6                                                         
*                                                                               
BLOVUID8 LTR   R0,RF                                                            
         BZ    ERRKINV                                                          
         BAS   RE,SORTIDS          SORT USER-IDS INTO ASCENDING SEQ             
*                                                                               
BLOVUIDX B     BLOVALX                                                          
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE GROUPID(OR GID)=XXXXXXXX                                   *         
***********************************************************************         
         SPACE 1                                                                
BLOVGID  TM    TWAINDS1,TWAIUPID   TEST CONNECTED USER IS A GROUP               
         BZ    ERRKINV                                                          
         LA    R2,IOKEY                                                         
         USING CTWREC,R2           BUILD KEY OF GROUP RECORD                    
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVC   CTWKAGY,CUAALF                                                   
         MVI   CTWKREC,CTWKRIDG                                                 
         MVC   CTWKID,FVIFLD                                                    
         GOTO1 AIO,IORD+IOCTFILE+IO3                                            
         BNE   ERRKINV                                                          
         L     R2,AIO3                                                          
*                                                                               
         LA    R1,CTWDATA                                                       
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         LA    RE,BCWORK                                                        
         USING CTLSTD,R1                                                        
BLOVGID1 CLI   CTLSTD,0                                                         
         BE    BLOVGID3                                                         
         CLI   CTLSTEL,CTLSTELQ                                                 
         BNE   BLOVGID2                                                         
         MVC   0(L'BLOUSR,RE),CTLSTDTA+L'CTIKID                                 
         LA    RE,L'BLOUSR(RE)                                                  
         LA    RF,1(RF)                                                         
BLOVGID2 IC    R0,CTLSTLEN                                                      
         AR    R1,R0                                                            
         B     BLOVGID1                                                         
*                                                                               
BLOVGID3 LTR   R0,RF                                                            
         BZ    ERRKINV                                                          
         BAS   RE,SORTIDS          SORT USER-IDS INTO ASCENDING SEQ             
*                                                                               
BLOVGIDX B     BLOVALX                                                          
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ALLIDS(OR AID)=Y(ES)                                       *         
***********************************************************************         
         SPACE 1                                                                
BLOVAID  SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   BC@YES(0),FVIFLD                                                 
         BNE   ERRKINV                                                          
*                                                                               
         TM    TWAINDS1,TWAIUPID   TEST CONNECTED USER IS PRINCIPAL             
         BNZ   BLOVAIDX                                                         
*                                                                               
         TM    TWAINDS1,TWAIUGID   TEST CONNECTED USERID IS A GROUP             
         BZ    ERRKINV                                                          
         LA    R2,IOKEY                                                         
         USING CTWREC,R2           BUILD KEY OF GROUP RECORD                    
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVC   CTWKAGY,CUAALF                                                   
         MVI   CTWKREC,CTWKRIDG                                                 
         MVC   CTWKID,BCUSERID     USE CODE FROM USER-ID RECORD                 
         GOTO1 AIO,IORD+IOCTFILE+IO3                                            
         BNE   ERRKINV                                                          
         L     R2,AIO3                                                          
*                                                                               
         LA    R1,CTWDATA                                                       
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         LA    RE,BCWORK                                                        
         USING CTLSTD,R1                                                        
BLOVAID1 CLI   CTLSTD,0                                                         
         BE    BLOVAID3                                                         
         CLI   CTLSTEL,CTLSTELQ                                                 
         BNE   BLOVAID2                                                         
         MVC   0(L'BLOUSR,RE),CTLSTDTA+L'CTIKID                                 
         LA    RE,L'BLOUSR(RE)                                                  
         LA    RF,1(RF)                                                         
BLOVAID2 IC    R0,CTLSTLEN                                                      
         AR    R1,R0                                                            
         B     BLOVAID1                                                         
*                                                                               
BLOVAID3 LTR   R0,RF                                                            
         BZ    ERRKINV                                                          
         BAS   RE,SORTIDS          SORT USER-IDS INTO ASCENDING SEQ             
*                                                                               
BLOVAIDX B     BLOVALX                                                          
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACCRUAL=OPTION                                             *         
***********************************************************************         
         SPACE 1                                                                
BLOVACR  SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    R1,ACRTAB                                                        
BLOVACR2 CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     BLOVALX                                                          
         CLM   RF,1,1(R1)                                                       
         BH    BLOVACR4                                                         
         SR    RE,RE                                                            
         ICM   RE,3,2(R1)                                                       
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    BLOVACR6                                                         
         CLC   FVIFLD(0),0(RE)                                                  
BLOVACR4 LA    R1,L'ACRTAB(R1)                                                  
         B     BLOVACR2                                                         
BLOVACR6 MVC   BCWORK(L'BLOSACRL),0(R1)                                         
         B     BLOVALX                                                          
         SPACE 2                                                                
ACRTAB   DS    0XL4                ** ACCRUAL KEYWORD TABLE **                  
         DC    AL1(BLOSACRN+BLOSREVN)                                           
         DC    AL1(L'UC@NO-1),AL2(UC@NO-TWAD)                                   
         DC    AL1(BLOSREVN+BLOSNONN)                                           
         DC    AL1(L'UC@YES-1),AL2(UC@YES-TWAD)                                 
         DC    AL1(BLOSACRN+BLOSNONN)                                           
         DC    AL1(L'UC@RVRSL-1),AL2(UC@RVRSL-TWAD)                             
         DC    AL1(BLOSNONN)                                                    
         DC    AL1(L'UC@ONLY-1),AL2(UC@ONLY-TWAD)                               
ACRTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION=SELECT ACTION WORD                                  *         
***********************************************************************         
         SPACE 1                                                                
BLOVACT  L     R1,AOVERSEL                                                      
         USING SELTABD,R1                                                       
         SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
BLOVACT2 CLI   SELTABD,EOT         TEST EOT                                     
         BE    ERRKINV                                                          
         SR    RF,RF                                                            
         ICM   RF,3,SELTDSPN                                                    
         LA    RF,TWAD(RF)                                                      
         EX    RE,*+8              MATCH INPUT TO ACTION WORD                   
         BE    BLOVACT4                                                         
         CLC   FVIFLD(0),0(RF)                                                  
         LA    R1,SELTABL(R1)                                                   
         B     BLOVACT2                                                         
BLOVACT4 MVC   BCWORK(L'BLOMASK),SELTMASK                                       
         B     BLOVALX                                                          
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE NAME=X(XXXXXXXXXXXXX)                                      *         
***********************************************************************         
         SPACE 1                                                                
BLOVNAM  MVC   BCWORK(1),FVXLEN                                                 
         MVC   BCWORK+1(L'LSTBNAME),FVIFLD                                      
BLOVNAMX B     BLOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE COPY= OPTION                                               *         
***********************************************************************         
         SPACE 1                                                                
BLOVCOP  SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    R1,COPTAB                                                        
BLOVCOP2 CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     BLOVALX                                                          
         CLM   RF,1,1(R1)                                                       
         BH    BLOVCOP4                                                         
         SR    RE,RE                                                            
         ICM   RE,3,2(R1)                                                       
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    BLOVCOP6                                                         
         CLC   FVIFLD(0),0(RE)                                                  
BLOVCOP4 LA    R1,L'COPTAB(R1)                                                  
         B     BLOVCOP2                                                         
BLOVCOP6 MVC   BCWORK(L'BLOBHDS1),0(R1)                                         
         B     BLOVALX                                                          
         SPACE 2                                                                
COPTAB   DS    0XL4                ** COPY KEYWORD TABLE **                     
         DC    AL1(BHDSCOPY)                                                    
         DC    AL1(L'UC@YES-1),AL2(UC@YES-TWAD)                                 
COPTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE REVERSE= OPTION                                            *         
***********************************************************************         
         SPACE 1                                                                
BLOVRVS  SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    R1,RVSTAB                                                        
BLOVRVS2 CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     BLOVALX                                                          
         CLM   RF,1,1(R1)                                                       
         BH    BLOVRVS4                                                         
         SR    RE,RE                                                            
         ICM   RE,3,2(R1)                                                       
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    BLOVRVS6                                                         
         CLC   FVIFLD(0),0(RE)                                                  
BLOVRVS4 LA    R1,L'RVSTAB(R1)                                                  
         B     BLOVRVS2                                                         
BLOVRVS6 MVC   BCWORK(L'BLOBHDS1),0(R1)                                         
         B     BLOVALX                                                          
         SPACE 2                                                                
RVSTAB   DS    0XL4                ** REVERSE KEYWORD TABLE **                  
         DC    AL1(BHDSREVS)                                                    
         DC    AL1(L'UC@YES-1),AL2(UC@YES-TWAD)                                 
RVSTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE GENERATE= OPTION                                           *         
***********************************************************************         
         SPACE 1                                                                
BLOVGEN  SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    R1,GENTAB                                                        
BLOVGEN2 CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     BLOVALX                                                          
         CLM   RF,1,1(R1)                                                       
         BH    BLOVGEN4                                                         
         SR    RE,RE                                                            
         ICM   RE,3,2(R1)                                                       
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    BLOVGEN6                                                         
         CLC   FVIFLD(0),0(RE)                                                  
BLOVGEN4 LA    R1,L'GENTAB(R1)                                                  
         B     BLOVGEN2                                                         
BLOVGEN6 MVC   BCWORK(L'BLOBHDS1),0(R1)                                         
         B     BLOVALX                                                          
         SPACE 2                                                                
GENTAB   DS    0XL4                ** GENERATE KEYWORD TABLE **                 
         DC    AL1(BHDSGENE)                                                    
         DC    AL1(L'UC@YES-1),AL2(UC@YES-TWAD)                                 
GENTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATUS= OPTION                                             *         
***********************************************************************         
         SPACE 1                                                                
BLOVSTA  SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    R1,STTTAB                                                        
BLOVSTA2 CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     BLOVALX                                                          
         CLM   RF,1,1(R1)                                                       
         BH    BLOVSTA4                                                         
         SR    RE,RE                                                            
         ICM   RE,3,2(R1)                                                       
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    BLOVSTA6                                                         
         CLC   FVIFLD(0),0(RE)                                                  
BLOVSTA4 LA    R1,L'STTTAB(R1)                                                  
         B     BLOVSTA2                                                         
BLOVSTA6 MVC   BCWORK(L'BLOSTT),0(R1)                                           
         B     BLOVALX                                                          
         SPACE 2                                                                
STTTAB   DS    0XL4                ** STATUS= KEYWORD TABLE **                  
         DC    AL1(BLOSTOKQ)                                                    
         DC    AL1(L'UC@OK-1),AL2(UC@OK-TWAD)                                   
STTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE ORIGINAL= OPTION                                           *         
***********************************************************************         
         SPACE 1                                                                
BLOVORG  MVC   BCWORK(L'BLOORG),FVIFLD                                          
BLOVORGX B     BLOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* SORT LIST OF USER-IDS INTO ACSENDING SEQUENCE                       *         
*                                                                     *         
* NTRY - R0=BCWORK CONTAINS LIST, R0=NUMBER OF ENTRIES                *         
***********************************************************************         
         SPACE 1                                                                
SORTIDS  STM   RE,R1,12(RD)                                                     
         LA    R1,BCWORK                                                        
         SH    R0,=H'1'                                                         
         BZ    SORTIDSX                                                         
SORTIDS2 LA    RF,L'BLOUSR(R1)                                                  
         LR    RE,R0                                                            
SORTIDS4 CLC   0(L'BLOUSR,R1),0(RF)                                             
         BNH   *+22                                                             
         XC    0(L'BLOUSR,R1),0(RF)                                             
         XC    0(L'BLOUSR,RF),0(R1)                                             
         XC    0(L'BLOUSR,R1),0(RF)                                             
         LA    RF,L'BLOUSR(RF)                                                  
         BCT   RE,SORTIDS4                                                      
         LA    R1,L'BLOUSR(R1)                                                  
         BCT   R0,SORTIDS2                                                      
SORTIDSX LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ADD A COLUMN TO LIST OF DISPLAY COLUMNS                             *         
*                                                                     *         
* NTRY - R1=A(DISPLAY COLUMN CHARACTER)                               *         
* EXIT - CC=EQUAL IF OK, NOT EQUAL ON ERROR                           *         
***********************************************************************         
         SPACE 1                                                                
         USING *,RF                                                             
ADDCOL   STM   RE,R1,12(RD)                                                     
         CLI   0(R1),0             IGNORE NULL COLUMNS                          
         BE    ADDCOLY                                                          
         L     RE,BOADDR1                                                       
         USING DISTABD,RE          RE=A(DISPLAY COLUMN TABLE)                   
ADDCOL02 CLI   DISTABD,EOT         TEST EOT                                     
         BE    ADDCOLN                                                          
         CLC   DISCHR,0(R1)        MATCH CHARACTER TO TABLE                     
         BE    *+12                                                             
         LA    RE,DISTABL(RE)                                                   
         B     ADDCOL02                                                         
*                                                                               
         LA    R1,BCWORK+1                                                      
         LA    R0,L'BLODIS                                                      
ADDCOL04 CLI   0(R1),0             TEST THIS IS A NEW COLUMN                    
         BE    ADDCOL06                                                         
         CLC   DISCOL,0(R1)        IGNORE DUPLICATE COLUMNS                     
         BE    ADDCOLY                                                          
         LA    R1,L'DISCOL(R1)                                                  
         BCT   R0,ADDCOL04                                                      
         B     ADDCOLN                                                          
ADDCOL06 MVC   0(L'DISCOL,R1),DISCOL  SET INTERNAL VALUE                        
*                                                                               
ADDCOLY  CR    RE,RE                                                            
         B     ADDCOLX                                                          
ADDCOLN  CLI   *,0                                                              
*                                                                               
ADDCOLX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  RE,RF                                                            
         EJECT                                                                  
***********************************************************************         
* BLOVAL ERROR EXITS                                                  *         
***********************************************************************         
         SPACE 1                                                                
ERRNONE  MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     BLOVALX                                                          
ERRDUPE  MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     BLOVALX                                                          
ERRKDUP  MVC   FVMSGNO,=AL2(FVFKDUP)                                            
         B     BLOVALX                                                          
ERRKINV  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     BLOVALX                                                          
ERRNOTV  MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     BLOVALX                                                          
ERRSHRT  MVC   FVMSGNO,=AL2(AE$FLDTS)                                           
         B     BLOVALX                                                          
ERRLONG  MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         B     BLOVALX                                                          
ERRINVDT MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     BLOVALX                                                          
ERRSECLO MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         B     BLOVALX                                                          
ERROFFN1 MVC   FVMSGNO,=AL2(AE$OFFN1)                                           
         B     BLOVALX                                                          
ERROFFN2 MVC   FVMSGNO,=AL2(AE$OFFN2)                                           
         B     BLOVALX                                                          
ERRRANGE MVC   FVMSGNO,=AL2(AE$INVRG)                                           
         B     BLOVALX                                                          
ERRINCOL MVC   FVMSGNO,=AL2(AE$INCOL)                                           
         B     BLOVALX                                                          
ERRBGRUP MVC   FVMSGNO,=AL2(AE$IBGRP)                                           
ERRMSGON B     BLOVALX                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
KEYTABD  DSECT                     ** KEY CONTROL TABLE **                      
KEYTEOTQ EQU   X'FF'               END OF TABLE INDICATOR                       
KEYTKDSP DS    AL1                 DISPLACEMENT TO KEY VALUE                    
KEYTKLEN DS    AL1                 KEY COMPONENT LENGTH-1                       
KEYTFDSP DS    0AL2                DISPLACEMENT TO KEY FILTER                   
KEYTKLIT DS    AL1                 LITERAL KEY FILL VALUE                       
         DS    AL1                 N/D                                          
KEYTFNUM DS    AL1                 MAXIMUM NUMBER OF KEY FILTERS                
KEYTIND1 DS    XL1                 INDICATORS - 1                               
KEYTITAB EQU   X'80'               TABLE OF KEY FILTERS                         
KEYTIRNG EQU   X'40'               RANGE OF KEY FILTERS                         
KEYTILIT EQU   X'20'               LITERAL VALUE AT KEYTKLIT                    
KEYTIFIX EQU   X'10'               FIXED KEY VALUE                              
KEYTIWRK EQU   X'08'               FILTER VALUE IN WORKD                        
KEYTITWA EQU   X'04'               FILTER VALUE IN TWAD                         
KEYTIFLN EQU   X'02'               FILTER VALUE PRECEDED BY LENGTH              
*        EQU   X'01'               N/D                                          
KEYTIND2 DS    XL1                 INDICATORS - 2                               
         DS    XL1                 N/D                                          
KEYTABL  EQU   *-KEYTABD                                                        
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACBATWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBATWORK                                                      
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLY1H                                                         
       ++INCLUDE ACBATAED                                                       
         SPACE 1                                                                
         ORG   OSVALS                                                           
BLVALS   DS    0X                  ** BATCH/LIST VALUES **                      
BLOKOPT  DS    0X                  ** KEY VALUES **                             
BLOSEQ   DS    XL1                 DISPLAY SEQUENCE (KEY TYPE)                  
BLOTYPN  DS    XL1                 NUMBER OF INPUT TYPES                        
BLOTYPS  DS    (BLMAXTYP)X         LIST OF INPUT TYPES                          
BLOCSTD  DS    XL2                 CREATED START DATE                           
BLOCEND  DS    XL2                 CREATED END DATE                             
BLOCXST  DS    XL2                 COMPLEMENT CREATED START DATE                
BLOCXND  DS    XL2                 COMPLEMENT CREATED END DATE                  
BLOESTD  DS    XL2                 EFFECTIVE START DATE                         
BLOEEND  DS    XL2                 EFFECTIVE END DATE                           
BLOMOA   DS    0PL4                MONTH OF ACTIVITY RANGE                      
BLOMOSTR DS    PL2                 START MONTH                                  
BLOMOEND DS    PL2                 END MONTH                                    
BLONMASK DS    XL1                 'AND' MASK FOR RECORD STATUS                 
BLOXMASK DS    XL1                 'XOR' MASK FOR RECORD STATUS                 
*                                                                               
BLOSACRL DS    XL1                 ACCRUAL STATUS                               
BLOSACRN EQU   BHDSACRU            ACCRUAL BATCHES NOT REQUIRED                 
BLOSREVN EQU   BHDSACRV            REVERSAL ACCRUALS NOT REQUIRED               
BLOSNONN EQU   X'20'               NON-ACCRUAL BATCHES NOT REQUIRED             
BLOUSRMX EQU   32                  USER-IDS                                     
BLOUSR   DS    (BLOUSRMX)XL(L'CUUSER)                                           
BLOUSRL  EQU   *-BLOUSR                                                         
BLOPER   DS    CL(L'TBAKBCHR)      PERSON                                       
BLOGRP   DS    CL(L'TBAKGRUP)      BATCH TYPE GROUP                             
BLOREF   DS    0X                  REFERENCE RANGE                              
BLOREFST DS    CL(L'TBAKBREF)                                                   
BLOREFND DS    CL(L'TBAKBREF)                                                   
BLOREFL  EQU   *-BLOREF                                                         
BLOOFFI  DS    XL1                 OFFICE                                       
BLOOFF   DS    CL2                                                              
BLOOFFL  EQU   *-BLOOFF                                                         
BLOAPR   DS    CL(L'TBAKBCHR)      APPROVER                                     
BLOMASK  DS    XL(L'LSTTMASK)      ACTION                                       
BLONAME  DS    XL(L'LSTBNAME+1)    NAME                                         
BLOBHDS1 DS    XL(L'BHDSTAT1)      BHDEL STATUS BYTE - 1                        
BLOSTTI  DS    XL1                 STATUS                                       
BLOSTT   DS    XL1                                                              
BLOORG   DS    CL(L'TBAKBREF)      ORIGINAL (BATCH REFERENCE)                   
BLOSTOKQ EQU   X'80'                                                            
BLOSTTL  EQU   *-BLOSTT                                                         
BLOKOPTL EQU   *-BLOKOPT                                                        
*                                                                               
BLODOPT  DS    0X                  ** DISPLAY OPTIONS **                        
BLODISI  DS    XL1                                                              
BLODISIS EQU   X'80'               BLODIS SUFFIXES PROFILE COLUMNS              
BLODISIP EQU   X'40'               BLODIS PREFIXES PROFILE COLUMNS              
BLODIS   DS    XL32                                                             
BLODISL  EQU   *-BLODIS                                                         
BLODOPTL EQU   *-BLODOPT                                                        
BLVALSL  EQU   *-BLVALS                                                         
*                                                                               
BLODVALS DS    0XL2                SAVED DISPLAY VALUES (FOR SCROLLING)         
BLODINDX DS    XL1                 INDEX TO CURRENT DISPLAY COLUMN              
BLODINUM DS    XL1                 NUMBER OF COLUMNS DISPLYED                   
*                                                                               
BLTBANXT DS    XL(L'TBAKEY)        NEXT TIME BATCH KEY                          
*                                                                               
BLDHEAD1 DS    CL(L'BLILIN1)       DISPLAY COLUMN HEADING                       
BLDHEAD2 DS    CL(L'BLILIN1)       DISPLAY COLUMN HEADING                       
BLDSDSP  DS    0X                  DISPLACEMENTS FOR SCREEN DISPLAY             
BLDSUSER DS    XL1                 DISPLACEMENT TO USER-ID                      
BLDSBREF DS    XL1                 DISPLACEMENT TO REFERENCE NUMBER             
BLDSITYP DS    XL1                 DISPLACEMENT TO INPUT TYPE                   
BLDSINAM DS    XL1                 DISPLACEMENT TO INPUT TYPE NAME              
BLDSMNTH DS    XL1                 DISPLACEMENT TO MONTH OF ACTIVITY            
BLDSEFDT DS    XL1                 DISPLACEMENT TO EFFECTIVE DATE               
BLDSINUP DS    XL1                 DISPLACEMENT TO INSTANT UPDATE               
BLDSPRSN DS    XL1                 DISPLACEMENT TO PERSON                       
BLDSCRDT DS    XL1                 DISPLACEMENT TO CREATED DATE                 
BLDSSTAT DS    XL1                 DISPLACEMENT TO STATUS                       
BLDSITMC DS    XL1                 DISPLACEMENT TO ITEM COUNT                   
BLDSITMA DS    XL1                 DISPLACEMENT TO ITEMS ADDED                  
BLDSCSHC DS    XL1                 DISPLACEMENT TO CASH CONTROL                 
BLDSCSHA DS    XL1                 DISPLACEMENT TO AMOUNT INPUT                 
BLDSBNAM DS    XL1                 DISPLACEMENT TO BATCH NAME                   
BLDSOFFC DS    XL1                 DISPLACEMENT TO OFFICE                       
BLDSCMTS DS    XL1                 DISPLACEMENT TO COMMENTS                     
BLDSAPRV DS    XL1                 DISPLACEMENT TO APPROVER                     
BLDSIGRP DS    XL1                 DISPLACEMENT TO INPUT TYPE GROUP             
BLDSLUID DS    XL1                 DISPLACEMENT TO LUID                         
BLDSACTS DS    XL1                 DISPLACEMENT TO VALID ACTIONS                
BLDSTOTD DS    XL1                 DISPLACEMENT TO TOTAL DEBITS                 
BLDSTOTC DS    XL1                 DISPLACEMENT TO TOTAL CREDITS                
BLDS0024 DS    XL1                 DISPLACEMENT TO N/D                          
BLDS0025 DS    XL1                 DISPLACEMENT TO N/D                          
BLDS0026 DS    XL1                 DISPLACEMENT TO N/D                          
BLDS0027 DS    XL1                 DISPLACEMENT TO N/D                          
BLDS0028 DS    XL1                 DISPLACEMENT TO N/D                          
BLDS0029 DS    XL1                 DISPLACEMENT TO N/D                          
BLDS0030 DS    XL1                 DISPLACEMENT TO N/D                          
BLDSADDR DS    XL1                 DISPLACEMENT TO DISK ADDRESS                 
BLDSRNUM DS    XL1                 DISPLACEMENT TO RECORD NUMBER                
BLDSFREF DS    XL1                 DISPLACEMENT TO FULL REF. NUMBER             
BLDSDSPL EQU   *-BLDSDSP                                                        
BLDRDSP  DS    0X                  DISPLACEMENTS FOR REPORT DISPLAY             
         DS    XL32                                                             
BLDRDSPL EQU   *-BLDRDSP                                                        
*                                                                               
BLTMSBLK DS    0F                  TMS UPDATE BLOCK                             
BLTMSNTB DS    F                   NUMBER OF ENTRIES                            
BLTMSTMX EQU   10                  MAX NUMBER OF ENTRIES                        
BLTMSUTB DS    (BLTMSTMX*16)C                                                   
TMSUTABX DS    C                                                                
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
LOGRECD  DSECT                     ** ADRFILE LOGGING RECORD **                 
LOGREC   DS    0XL38                                                            
LOGPGID  DS    CL8                 PROGRAM-ID '$POSTMAN'                        
LOGACTN  DS    CL3                 ACTION 'UPD'                                 
LOGAGID  DS    CL2                 AGENCY ALPHA-ID                              
LOGLUID  DS    CL8                 LUID                                         
LOGBTYP  DS    XL1                 BATCH TYPE                                   
LOGITEM  DS    XL2                 NUMBER OF ITEMS IN BATCH                     
LOGIOCT  DS    XL2                 I/O COUNT                                    
LOGDATE  DS    XL3                 DATE                                         
LOGSTTM  DS    PL4                 START TIME (HHMMSSTH)                        
LOGNDTM  DS    PL4                 END TIME   (HHMMSSTH)                        
         DS    XL1                 N/D                                          
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039ACBAT60   06/28/04'                                      
         END                                                                    
