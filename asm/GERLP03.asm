*          DATA SET GERLP03    AT LEVEL 013 AS OF 11/04/11                      
*PHASE TF2D03A                                                                  
                                                                                
***********************************************************************         
* MODULE HANDLES THE FOLLOWING FUNCTIONS:-                            *         
*                                                                     *         
* GROUP/CALENDAR  USES X'F9' SCREEN - KEY PREFIXES                    *         
*                 AND  X'F8' SCREEN - CAL PREFIXES - CODE=GRPCAL      *         
* GROUP/RUNDATE   USES X'F9' SCREEN - KEY PREFIXES                    *         
*                 AND  X'F6' SCREEN - DAT PREFIXES - CODE=GRPCAL      *         
*                                                                     *         
* XFILE/CALENDAR  USES X'F9' SCREEN - KEY PREFIXES                    *         
*                 AND  X'F8' SCREEN - CAL PREFIXES - CODE=GRPCAL      *         
* XFILE/RUNDATE   USES X'F9' SCREEN - KEY PREFIXES                    *         
*                 AND  X'F6' SCREEN - DAT PREFIXES - CODE=GRPCAL      *         
*                                                                     *         
***********************************************************************         
                                                                                
RLP03    TITLE '- GROUP CALENDAR'                                               
RLP03    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RLP3**,R8,R7,RR=RE                                           
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA                                                          
         USING LSTTABD,CSLSTCUR    ALWAYS USE CURRENT LIST ENTRY                
         L     R6,ARFPIOB                                                       
         USING RFPD,R6             R6=A(RFP INTERFACE BLOCK)                    
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         SRL   RF,32-8                                                          
         LTR   RF,RF                                                            
         BNZ   CALNTRY                                                          
         B     GRPCAL              GROUP/CALENDAR                               
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* RETURN TO POINT AFTER NTRSES                                        *         
***********************************************************************         
                                                                                
CALNTRY  CHI   RF,CALNTRYM                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     CALNTRYS-L'CALNTRYS(RF)                                          
                                                                                
CALNTRYS DS    0XL4                                                             
                                                                                
         B     RLGRPBLD            RETURN FROM GROUP/BUILD                      
                                                                                
CALNTRYM EQU   (*-CALNTRY)/L'CALNTRY                                            
         EJECT                                                                  
***********************************************************************         
* GROUP/CALENDAR - VALIDATE KEY                                       *         
***********************************************************************         
                                                                                
         USING GCWORKD,RC                                                       
GRPCAL   TM    CSINDSL1,CSIUSELC   TEST NESTED (LIST) CALL                      
         BNZ   GRPCAL06                                                         
         CLC   LSTTRTYP,CSREC      OR WE HAVE A RECORD ALREADY                  
         BE    GRPCAL06                                                         
                                                                                
         TM    CSINDSL1,CSIKEYSL   TEST USER INVITED TO ENTER KEY               
         BNZ   GRPCAL02                                                         
         GOTOR AOVRSCR,PCPARM,('GRPKEYSQ',RLPOLY1H)                             
         GOTOR TSTRUN              TEST RUN/PROTECT KEY FIELDS                  
         CLI   ASSYSL,CONLETQ      IF CONTROL SYSTEM                            
         BNE   *+8                 UNPROTECT THE SYSTEM FIELD                   
         NI    KEYSYSH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         LA    R0,KEYGRPH                                                       
         ST    R0,FVADDR                                                        
         CLI   CSREC,RECXFG                                                     
         BNE   *+14                                                             
         XC    KEYSYSN,KEYSYSN                                                  
         OI    KEYSYSH+(FVATRB-FVIHDR),FVAPROT                                  
         MVC   FVMSGNO,=AL2(GI$EKEY)                                            
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIKEYSL   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
                                                                                
GRPCAL02 GOTOR AFVAL,KEYGRPH       VALIDATE GROUP CODE                          
         BNE   EXIT                                                             
         MVC   GCGRP,FVIFLD                                                     
         CLI   CSREC,RECGRP        TEST XFILE GROUP RECORD                      
         BNE   GRPCAL10                                                         
         MVC   CUSYSL,ASSYSL                                                    
         TM    KEYSYSH+(FVATRB-FVIHDR),FVAPROT                                  
         BNZ   GRPCAL04                                                         
         GOTOR AVALSYS,KEYSYSH     VALIDATE SYSTEM FIELD                        
         BNE   EXIT                                                             
         MVC   CUSYSL,PCWORK                                                    
                                                                                
GRPCAL04 MVC   CUUSER,TWAUSRID                                                  
         MVC   CUAALF,TWAAGY                                                    
         LA    R1,GCGRP                                                         
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFPIO TO VALIDATE GROUP                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     EXIT                                                             
         GOTOR ANTRSES,=AL1(SELTICLR,RECGRP,ACTBLD,1,0,0,0)                     
                                                                                
GRPCAL06 CLI   CSACT,ACTRUN        TEST RUNDATE ACTION                          
         BNE   GRPCAL07                                                         
         CLI   TWASCRN,GRPKEYSQ    TEST WE HAVE KEY/RUNDATE SCREEN              
         BE    *+12                                                             
         CLI   TWASCRN,GRPRUNSQ                                                 
         BNE   GRPCAL08                                                         
         TM    KEYGRPH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   GRPCAL02                                                         
         TM    KEYSYSH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   GRPCAL02                                                         
                                                                                
GRPCAL07 CLI   TWASCRN,GRPKEYSQ    TEST WE HAVE KEY/CALENDAR SCREEN             
         BE    *+12                                                             
         CLI   TWASCRN,GRPCALSQ                                                 
         BNE   GRPCAL08                                                         
         TM    KEYGRPH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   GRPCAL02                                                         
         TM    KEYSYSH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   GRPCAL02                                                         
                                                                                
GRPCAL08 MVC   GCGRP,GRPLGRP                                                    
         MVC   CUUSER,GRPLUSER                                                  
         MVC   CUAALF,GRPLAGY                                                   
         MVC   CUSYSL,GRPLSYST                                                  
         CLI   CSREC,RECGRP        TEST RFP GROUP RECORD                        
         BNE   GRPCAL10                                                         
         LA    R1,GCGRP                                                         
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFPIO TO VALIDATE GROUP                 
         BE    GRPCAL12                                                         
         GOTOR AOVRSCR,PCPARM,('GRPKEYSQ',RLPOLY1H)                             
         GOTOR TSTRUN              TEST RUN/PROTECT KEY FIELDS                  
         OI    KEYGRPH+(FVATRB-FVIHDR),FVAPROT                                  
         MVC   KEYGRP,GCGRP        OUTPUT CODE AND DESCRIPTION                  
         OI    KEYGRPH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTOR AGETSYS,CUSYSL      SHOW SYSTEM                                  
         MVC   KEYSYS,PCWORK                                                    
         OI    KEYSYSH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R0,KEYGRPH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     EXIT                                                             
                                                                                
GRPCAL10 CLI   CSREC,RECXFG        TEST XFILE GROUP RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CUSYSL,RFPFSCTL                                                  
         LA    R2,IOKEY                                                         
         USING XFILED,R2           VALIDATE XFILE GROUPS                        
         XC    XFKEY,XFKEY         BUILD XFILE RECORD KEY                       
         MVI   XFKSYS,XFKSYSQ                                                   
         MVI   XFKSTYP,XFKSTYPQ                                                 
         MVC   XFAGY,CUAALF                                                     
         MVC   XFUSER,CUUSER                                                    
         MVC   XFGRP,GCGRP                                                      
         GOTOR AIO,'IORDUP+IOGENDIS+IO1'                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     EXIT                                                             
         GOTOR AIO,'IOGETRUP+IOGENFIS+IO1'                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AXFGRFP             MOVE XFILE VALUES TO RFP BLOCK               
                                                                                
GRPCAL12 TM    CSINDSL1,CSIKEYSL   TEST KEY SCREEN LOADED                       
         BNZ   GRPCAL14                                                         
         GOTOR AOVRSCR,PCPARM,('GRPKEYSQ',RLPOLY1H)                             
         GOTOR TSTRUN              TEST RUN/PROTECT KEY FIELDS                  
         CLI   CSREC,RECXFG                                                     
         BNE   *+10                                                             
         XC    KEYSYSN,KEYSYSN                                                  
         OI    CSINDSL1,CSIKEYSL   SET KEY SCREEN LOADED                        
         TM    CSINDSL1,CSIUSELC   PROTECT GROUP IF NESTED CALL                 
         BZ    GRPCAL14                                                         
         OI    KEYGRPH+(FVATRB-FVIHDR),FVAPROT                                  
                                                                                
RLGRPBLD MVC   GCGRP,GRPLGRP       RETURN HERE AFTER GROUP/BUILD                
GRPCAL14 LHI   R0,GRPCALSQ                                                      
         CLI   CSACT,ACTCAL                                                     
         BE    *+8                                                              
         LHI   R0,GRPRUNSQ                                                      
         CLM   R0,1,TWASCRN        TEST CALENDAR SCREEN LOADED                  
         BE    GRPCAL16                                                         
         GOTOR AOVRSCR,PCPARM,((R0),KEYPFKH)                                    
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   GRPCAL16                                                         
         OC    RFPXFILE,RFPXFILE   TEST XFILE ATTACHED TO GROUP                 
         BZ    GRPCAL16                                                         
         OI    KEYFRQH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    KEYEFDTH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    KEYSCHDH+(FVATRB-FVIHDR),FVAPROT                                 
                                                                                
GRPCAL16 MVC   KEYGRP,GCGRP        OUTPUT CODE AND DESCRIPTION                  
         OI    KEYGRPH+(FVOIND-FVIHDR),FVOXMT                                   
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   GRPCAL17                                                         
         GOTOR AGETSYS,RFPVSYST    YES - SHOW SYSTEM                            
         MVC   KEYSYS,PCWORK                                                    
         OI    KEYSYSH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
GRPCAL17 CLC   GCKEY(GCKEYL),LVKEY TEST CHANGE OF GROUP                         
         BNE   GRPCAL24                                                         
         CLI   KEYFRQH+(FVILEN-FVIHDR),0                                        
         BE    GRPCAL18                                                         
         GOTOR AVALFRQ,KEYFRQH     VALIDATE THE FREQUENCY                       
         BNE   GRPCAL18                                                         
         CLI   PCWORK,FRQRDAY      TEST RUNDAYS FREQUENCY                       
         BNE   GRPCAL18            YES - DATES ARE IRRELEVENT                   
         SR    RE,RE               Test effective date of 'DELETE'              
         ICM   RE,1,KEYEFDTH+(FVILEN-FVIHDR)                                    
         BZ    GRPCAL25                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    GRPCAL20            Yes - clear everything                       
         CLC   KEYEFDT(0),PCUDEL                                                
         B     GRPCAL25                                                         
                                                                                
GRPCAL18 CLI   KEYEFDTH+(FVILEN-FVIHDR),0                                       
         BE    *+12                                                             
         TM    KEYEFDTH+(FVIIND-FVIHDR),FVITHIS                                 
         BNZ   GRPCAL19            VALIDATE DATA IF DATE INPUT                  
         TM    KEYFRQH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   GRPCAL19            OR FREQUENCY INPUT                           
         CLI   RFPVFREQ,FRQRDAY    TEST RUNDAYS FREQUENCY                       
         BE    GRPCAL25            YES - DATES ARE IRRELEVENT                   
         TM    KEYSCHDH+(FVIIND-FVIHDR),FVITHIS                                 
         BNZ   GRPCAL19            OR RUN SCHEDULE INPUT                        
         OC    RFPVENDR,RFPVENDR                                                
         BNZ   GRPCAL24            IF NO EFFECTIVE END DATE WANT ONE            
                                                                                
GRPCAL19 GOTOR AGOSOFT,PCPARM,('SOFITYMD',KEYEFDTH),                   *        
               ('SOFOTSD8+SOFOTMIX',GCNXTR),('FF-SOFIIONE',MAXPERD)             
         BE    GRPCAL21                                                         
         SR    RE,RE                                                            
         ICM   RE,1,KEYEFDTH+(FVILEN-FVIHDR)                                    
         BZ    EXIT                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BNE   EXIT                                                             
         CLC   KEYEFDT(0),PCUDEL                                                
         MVI   FVOSYS,0            RESET FVOSYS (SET BY GOSOFT)                 
                                                                                
GRPCAL20 XC    GCNXTR,GCNXTR       CLEAR EVERYTHING                             
         XC    GCENDR,GCENDR                                                    
         XC    GCDAYS,GCDAYS                                                    
         XC    GCRNGE,GCRNGE                                                    
         MVC   GCFREQ,RFPVFREQ     SET FREQUENCY CODE                           
         XC    KEYEFDT,KEYEFDT                                                  
         OI    KEYEFDTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    KEYSCHD,KEYSCHD                                                  
         OI    KEYSCHDH+(FVOIND-FVIHDR),FVOXMT                                  
         B     GRPCAL56                                                         
                                                                                
GRPCAL21 L     R1,12(,R1)                                                       
         USING SOFDATD,R1                                                       
         CLC   RFPVNXTR,GCNXTR     Did start date change?                       
         BE    GRPCAL22            No, so don't change anything                 
         CLC   SOFOVSTD,SOFTODAY                                                
         BNL   GRPCAL23                                                         
         MVC   FVMSGNO,=AL2(GE$DCBBT)                                           
         B     EXIT                                                             
                                                                                
GRPCAL22 CLC   RFPVENDR,GCENDR     Did end date change?                         
         BE    GRPCAL23            Yes                                          
         CLC   SOFOVEND,SOFTODAY                                                
         BNL   GRPCAL23                                                         
         MVC   FVMSGNO,=AL2(GE$DCBBT)                                           
         B     EXIT                                                             
                                                                                
GRPCAL23 CLC   SOFOVDIP,MAXDAYS    TEST 366 DAYS IN PERIOD                      
         BL    GRPCAL26                                                         
         CLC   GCNXTRY,GCENDRY     TEST START/END IN SAME YEAR                  
         BE    GRPCAL26                                                         
         MVC   FVMSGNO,=AL2(GE$IDRNG)                                           
         B     EXIT                                                             
         DROP  R1                                                               
                                                                                
GRPCAL24 MVC   GCNXTR,RFPVNXTR     TAKE START/END FROM RPF BLOCK                
         MVC   GCENDR,RFPVENDR                                                  
         OC    RFPVNXTR,RFPVNXTR   TEST DATES SET                               
         BZ    GRPCAL26                                                         
         SR    R0,R0                                                            
         OC    RFPVENDR,RFPVENDR   TEST END DATE SET                            
         BNZ   *+8                                                              
         LHI   R0,SOFIIONE                                                      
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',GCNXTR),                     *        
               ('SOFOTPRT+SOFOTMIX',KEYEFDTH),((R0),0)                          
         B     GRPCAL26                                                         
                                                                                
GRPCAL25 XC    GCNXTR,GCNXTR       CLEAR DATES                                  
         XC    GCENDR,GCENDR                                                    
         XC    GCDAYS,GCDAYS                                                    
         XC    GCRNGE,GCRNGE                                                    
         MVC   GCFREQ,RFPVFREQ     SET FREQUENCY CODE                           
         XC    KEYEFDT,KEYEFDT                                                  
         OI    KEYEFDTH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
GRPCAL26 TM    KEYFRQH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   GRPCAL28                                                         
         TM    KEYSCHDH+(FVIIND-FVIHDR),FVITHIS                                 
         BNZ   GRPCAL28                                                         
         CLC   GCKEY(GCKEYL),LVKEY TEST CHANGE OF GROUP                         
         BE    GRPCAL28                                                         
                                                                                
         GOTOR AGETFRQ,RFPVFREQ    GET FREQUENCY NAME                           
         XC    KEYFRQ,KEYFRQ                                                    
         MVC   KEYFRQ(FRQNAMLQ),PCWORK                                          
         OI    KEYFRQH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   GCFREQ,RFPVFREQ     SET FREQUENCY CODE                           
         B     GRPCAL30                                                         
                                                                                
GRPCAL28 GOTOR AVALFRQ,KEYFRQH     VALIDATE THE FREQUENCY                       
         BNE   EXIT                                                             
                                                                                
         MVC   GCFREQ,PCWORK       SET FREQUENCY                                
         MVC   GCFREQI,PCWORK+FRQNAMLQ                                          
         CLC   GCFREQ,RFPVFREQ     TEST CHANGE OF FREQUENCY                     
         BE    GRPCAL30                                                         
         OI    KEYSCHDH+(FVIIND-FVIHDR),FVITHIS                                 
                                                                                
GRPCAL30 TM    KEYSCHDH+(FVIIND-FVIHDR),FVITHIS                                 
         BNZ   GRPCAL32                                                         
         CLC   GCKEY(GCKEYL),LVKEY TEST CHANGE OF GROUP                         
         BE    GRPCAL32                                                         
                                                                                
         OC    RFPVDAYS,RFPVDAYS   TEST DAYS ARE SET                            
         BZ    GRPCAL31                                                         
         MVC   GCDAYS,RFPVDAYS                                                  
         B     GRPCAL48                                                         
                                                                                
GRPCAL31 CLI   CSACT,ACTCAL                                                     
         BNE   *+8                                                              
         GOTOR CALCLR              CLEAR CALENDAR PORTION OF SCREEN             
         XC    KEYSCHD,KEYSCHD     CLEAR RUN SCHEDULE                           
         OI    KEYSCHDH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R0,LVBLK                                                         
         LHI   R1,GCBLKL                                                        
         LA    RE,GCBLK                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R0,KEYEFDTH                                                      
         CLI   RFPVFREQ,FRQRDAY    TEST RUNDAYS                                 
         BNE   *+8                                                              
         LA    R0,KEYSCHDH                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIRDSPC                                                
         B     EXIT                                                             
                                                                                
GRPCAL32 XC    GCDAYS,GCDAYS       CLEAR RUN SCHEDULE                           
         MVI   GCDCON,GCDCONQ      SET CONVERTED TO NEW FORMAT                  
                                                                                
         TM    GCFREQI,FRQTINOQ    TEST NO RUN SCHEDULE (IRREGULAR)             
         BZ    *+12                                                             
         MVI   GCDTYP,GCDTNOTQ     SET NO RUN SCHEDULE                          
         B     GRPCAL48                                                         
                                                                                
         MVI   FVMINL,1                                                         
         GOTOR AFVAL,KEYSCHDH      VALIDATE RUN SCHEDULE                        
         BNE   EXIT                                                             
                                                                                
         MVI   GCDTYP,GCDTDAYQ     SET DAYS                                     
         CLI   GCFREQ,FRQWEEK      ONLY WEEKLY FREQUENCY                        
         BE    *+8                                                              
         CLI   GCFREQ,FRQRDAY      AND RUNDAYS FREQUENCY HAVE DAYS              
         BE    *+8                                                              
         MVI   GCDTYP,GCDTDATQ     SET DATES                                    
                                                                                
         MVC   PCPARM+08(2),=C',='                                              
         MVC   PCPARM+10(1),PCCOMMA                                             
         MVI   PCPARM+11,0                                                      
         OC    RFPBSYMB,RFPBSYMB   TEST BILLING SYMBOL PRESENT...               
         BZ    *+16                                                             
         CLI   GCFREQ,FRQMNTH      ...AND MONTHLY FREQUENCY                     
         BNE   *+8                                                              
         MVI   PCPARM+11,C'/'      YES - ALLOW ADJUSTMENT CHARACTERS            
         XC    GCBADJS,GCBADJS                                                  
         GOTOR VSCANNER,PCPARM,FVIHDR,('GCSCNMAX',GCSCNTAB)                     
         MVC   PCBYTE1,4(R1)       SAVE NUMBER OF ITEMS INPUT                   
         CLI   PCBYTE1,0                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     EXIT                                                             
         MVI   PCBYTE2,0                                                        
         LA    R2,GCSCNTAB         R2=A(SCANNER TABLE)                          
                                                                                
GRPCAL34 IC    R1,PCBYTE2          BUMP INPUT FIELD COUNT                       
         AHI   R1,1                                                             
         STC   R1,PCBYTE2                                                       
         CLC   PCBYTE2,PCBYTE1     TEST ALL ENTRIES PROCESSED                   
         BNH   *+12                                                             
         MVI   FVINDX,0            YES - RESET MULTIPLE FIELD INDEX             
         B     GRPCAL48                                                         
                                                                                
         CLI   PCBYTE1,1           TEST FIELD# 1                                
         BE    *+10                                                             
         MVC   FVINDX,PCBYTE2      NO - SET MULTIPLE FIELD INDEX                
         CLI   0(R2),0             TEST FIELD PRESENT                           
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$MIF)                                             
         B     EXIT                                                             
         CLI   GCFREQ,FRQWEEK      TEST WEEKLY FREQUENCY                        
         BE    GRPCAL38                                                         
         CLI   GCFREQ,FRQRDAY      TEST RUNDAYS FREQUENCY                       
         BE    GRPCAL38                                                         
                                                                                
         TM    2(R2),X'80'         VALIDATE DATE                                
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$NOTN)                                            
         B     EXIT                                                             
                                                                                
         OC    4(4,R2),4(R2)       TEST WITHIN RANGE 1-31                       
         BZ    *+14                                                             
         CLC   4(4,R2),=AL4(DAYSINMQ)                                           
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     EXIT                                                             
                                                                                
         ICM   RE,15,4(R2)                                                      
         BCTR  RE,0                                                             
         SRDL  RE,3                SHIFT OFF DAY INTO RF                        
         SRL   RF,32-3                                                          
         LA    RE,GCDDATS(RE)                                                   
         IC    RF,DAYBITS(RF)      POINT TO BIT MASK FOR DAY                    
         EX    RF,*+8                                                           
         BZ    GRPCAL36                                                         
         TM    0(RE),0             TEST ALREADY USED THIS ONE                   
         MVC   FVMSGNO,=AL2(GE$DUPIF)                                           
         B     EXIT                                                             
                                                                                
GRPCAL36 EX    RF,*+8                                                           
         B     *+8                                                              
         OI    0(RE),0             TURN ON DAY BIT IN MASK                      
         CLI   1(R2),0             TEST DEFAULT BILLING DAY GIVEN               
         BE    GRPCAL46                                                         
         CLI   1(R2),1                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     EXIT                                                             
         GOTOR VALDAY,22(R2)                                                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     EXIT                                                             
         MVI   GCCHA,1             SET GLOBAL CHANGE FLAG                       
         ICM   RE,15,4(R2)                                                      
         LA    RE,GCBADJS-1(RE)                                                 
         MVC   0(1,RE),GCBADJ                                                   
         MVI   GCBADJ,0                                                         
         B     GRPCAL46                                                         
                                                                                
GRPCAL38 SR    RF,RF               VALIDATE DAY                                 
         IC    RF,0(R2)                                                         
         CHI   RF,L'PCMMON                                                      
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     EXIT                                                             
         BCTR  RF,0                                                             
         LA    R1,PCMMON           R1=A(MIXED CASE DAY NAMES)                   
         LA    R3,PCUMON           R3=A(UPPER CASE DAY NAMES)                   
         LHI   R0,DAYSINWQ                                                      
         LHI   RE,X'80'                                                         
GRPCAL40 EX    RF,*+8                                                           
         BE    GRPCAL42                                                         
         CLC   12(0,R2),0(R1)      MATCH MIXED CASE ENTRY                       
         EX    RF,*+8                                                           
         BE    GRPCAL42                                                         
         CLC   12(0,R2),0(R3)      MATCH UPPER CASE ENTRY                       
         SRL   RE,1                                                             
         AHI   R1,L'PCMMON                                                      
         AHI   R3,L'PCUMON                                                      
         BCT   R0,GRPCAL40                                                      
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     EXIT                                                             
                                                                                
GRPCAL42 EX    RE,*+8              TEST FOR DUPLICATES                          
         BZ    GRPCAL44                                                         
         TM    GCDDAYS,0                                                        
         MVC   FVMSGNO,=AL2(GE$DUPIF)                                           
         B     EXIT                                                             
GRPCAL44 EX    RE,*+8              TURN ON DAY BIT IN MASK                      
         B     GRPCAL46                                                         
         OI    GCDDAYS,0                                                        
                                                                                
GRPCAL46 AHI   R2,L'GCSCNTAB       BUMP TO NEXT INPUT FIELD                     
         B     GRPCAL34                                                         
                                                                                
GRPCAL48 CLI   GCDCON,GCDCONQ      TEST CONVERTED TO NEW FORMAT                 
         BE    GRPCAL52                                                         
                                                                                
         CLI   GCDAYS+5,C'E'       TEST SPECIAL FORMAT (31)                     
         BNE   *+14                                                             
         MVC   GCDAYS,DAYS31                                                    
         B     GRPCAL52                                                         
                                                                                
         CLI   GCDAYS+6,C'S'       TEST SPECIAL FORMAT (15/31)                  
         BNE   *+14                                                             
         MVC   GCDAYS,DAYS1531                                                  
         B     GRPCAL52                                                         
                                                                                
         LA    RE,GCDAYS           ELSE CONVERT DAYS TO DAY MASK                
         LHI   RF,5                                                             
         SR    R1,R1                                                            
GRPCAL50 CLI   0(RE),0                                                          
         BE    *+8                                                              
         AHI   R1,1                                                             
         SLL   R1,1                                                             
         AHI   RE,1                                                             
         BCT   RF,GRPCAL50                                                      
         SLL   R1,2                SHIFT REMAINING BITS                         
         XC    GCDAYS,GCDAYS                                                    
         MVI   GCDCON,GCDCONQ                                                   
         MVI   GCDTYP,GCDTDAYQ     SET TYPE TO DAY LIST                         
         STC   R1,GCDDAYS                                                       
         MVC   RFPVDAYS,GCDAYS     SET RFPVDAYS TO NEW FORMAT                   
                                                                                
GRPCAL52 XC    KEYSCHD,KEYSCHD     RE-DISPLAY RUN SCHEDULE                      
         OI    KEYSCHDH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R1,GCDAYS                                                        
         ICM   R1,8,=X'80'                                                      
         GOTOR AGETRSH                                                          
         MVC   KEYSCHD,PCWORK                                                   
                                                                                
         MVC   GCCALY,PCUYES                                                    
         MVC   GCCALN,PCUNO                                                     
         OC    RFPBSYMB,RFPBSYMB                                                
         BZ    *+12                                                             
         MVI   GCCALY,C'&&'                                                     
         MVI   GCCALN,C'|'                                                      
         MVI   GCCALH,C'H'                                                      
                                                                                
         MVC   GCRNGE,RFPVRNGE     EXTRACT CURRENT SUBMISSION DAYS              
         MVC   GCADJS,RFPBADJS     EXTRACT CURRENT ADJUSTMENTS                  
         CLI   CSACT,ACTRUN                                                     
         BE    GRPCAL72                                                         
         CLC   GCKEY(GCKEYL),LVKEY TEST CHANGE OF GROUP                         
         BE    GRPCAL56                                                         
         GOTOR VDATCON,PCPARM,(3,ASBDAT),(15,GCTDYJ)                            
                                                                                
         LA    R0,LVBLK                                                         
         LHI   R1,GCBLKL                                                        
         LA    RE,GCBLK                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR CALDIS              CLEAR OR DISPLAY CALENDAR                    
                                                                                
         LA    R0,KEYEFDTH                                                      
         CLI   RFPVFREQ,FRQRDAY    TEST RUNDAYS                                 
         BNE   *+8                                                              
         LA    R0,KEYSCHDH                                                      
         ST    R0,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIRDSPC                                                
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
         TM    GCFLAG,GCFXFG       TEST XFILE ATTACHED TO RFP GROUP             
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(GI$RDENR)                                           
         B     EXIT                                                             
                                                                                
GRPCAL56 GOTOR VDATCON,PCPARM,(3,ASBDAT),(15,GCTDYJ)                            
                                                                                
         LA    RF,CALCLR           POINT TO CLEAR ROUTINE                       
         OC    GCDAYS,GCDAYS                                                    
         BZ    GRPCAL58                                                         
         CLI   GCFREQ,FRQRDAY                                                   
         BE    GRPCAL59                                                         
         LA    RF,CALCHA           POINT TO CHANGE ROUTINE                      
         CLC   LVDATA(GCDATAL1),GCDATA                                          
         BE    GRPCAL58                                                         
         BRAS  RE,SETRNGE                                                       
         XC    GCADJS,GCADJS       AND BILLING ADJUSTMENTS                      
         LA    RF,CALBLD           POINT TO BUILD ROUTINE                       
                                                                                
GRPCAL58 BASR  RE,RF               VALIDATE/BUILD CALENDAR                      
         BNE   EXIT                                                             
                                                                                
GRPCAL59 LA    R0,LVDATA           TEST ANYTHING CHANGED                        
         LHI   R1,GCDATAL2                                                      
         LA    RE,GCDATA                                                        
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    GRPCAL64                                                         
                                                                                
         MVI   GCCHA,0             RESET GLOBAL CHANGE BYTE                     
         MVC   RFPVFREQ,GCFREQ     MOVE NEW VALUES INTO RFPD                    
         MVC   RFPVNXTR,GCNXTR                                                  
         MVC   RFPVENDR,GCENDR                                                  
         MVC   RFPVDAYS,GCDAYS                                                  
         MVC   RFPVRNGE,GCRNGE                                                  
         MVC   RFPBADJS(RFPBADJL),GCADJS                                        
         CLI   CSREC,RECGRP        TEST RFP GROUP RECORD                        
         BNE   GRPCAL60                                                         
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPSAVGP    AND SAVE THE GROUP                           
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   TEST FOR ERRORS                              
         BE    GRPCAL62                                                         
         DC    H'0'                                                             
                                                                                
GRPCAL60 GOTOR ARFPXFG             MOVE RFP VALUES TO XFILE RECORD              
         GOTOR UPDRFP              UPDATE SLAVE RFP GROUPS                      
         BNE   EXIT                                                             
         GOTOR AIO,'IOPUT+IOGENFIS+IO1'                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,'IOWRITE+IOGENDIS+IO1'                                       
         BE    GRPCAL62                                                         
         DC    H'0'                                                             
                                                                                
GRPCAL62 TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    GRPCAL64                                                         
         MVC   GRPLFREQ,GCFREQ     YES - UPDATE GROUP LIST ENTRY                
         MVC   GRPLNXTR,GCNXTR                                                  
         MVC   GRPLEND,GCENDR                                                   
                                                                                
GRPCAL64 GOTOR CALDIS              RE-DISPLAY THE RECORD                        
                                                                                
         LA    R0,LVBLK            SAVE CURRENT VALUES                          
         LHI   R1,GCBLKL                                                        
         LA    RE,GCBLK                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         OC    GCRNGE,GCRNGE       TEST ANY DATES SELECTED                      
         BZ    GRPCAL66                                                         
         LA    R0,KEYEFDTH                                                      
         CLI   RFPVFREQ,FRQRDAY    TEST RUNDAYS                                 
         BNE   *+8                                                              
         LA    R0,KEYSCHDH                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$RCENR)                                           
         CLI   RFPMODE,RFPSAVGP    TEST JUST SAVED THE GROUP                    
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(GI$RDECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
                                                                                
GRPCAL66 LA    RF,CALWK1H          POSITION CURSOR TO FIRST WEEK                
         CLI   GCFREQ,FRQRDAY      TEST RUNDAYS FREQUENCY                       
         BNE   *+12                                                             
         LA    RF,KEYSCHDH         POINT TO RUN SCHEDULE                        
         B     GRPCAL68                                                         
         OC    GCENDR,GCENDR       TEST END DATE GIVEN                          
         BNZ   GRPCAL68                                                         
         LA    RF,KEYEFDTH         POINT TO EFFECTIVE DATE IF UNKNOWN           
                                                                                
GRPCAL68 TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    GRPCAL70                                                         
         SR    RE,RE                                                            
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+10                                                             
         AR    RF,RE                                                            
         B     GRPCAL68                                                         
         LA    RF,KEYGRPH                                                       
                                                                                
GRPCAL70 ST    RF,FVADDR           SET CURSOR                                   
                                                                                
         MVC   FVMSGNO,=AL2(GI$EDATA)                                           
         MVI   FVOMTYP,GTMINF      AND ASK FOR DATA                             
         B     EXIT                                                             
                                                                                
GRPCAL72 GOTOR DATDIS              DISPLAY RUN DATES                            
         B     EXIT                                                             
                                                                                
TSTRUN   CLI   CSACT,ACTRUN        TEST RUNDATE ACTION                          
         BNER  RE                                                               
         OI    KEYEFDTH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    KEYFRQH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    KEYSCHDH+(FVATRB-FVIHDR),FVAPROT                                 
         XC    LVLEDAT,LVLEDAT     CLEAR LAST DISPLAYED DATE                    
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RUN DATES                                                   *         
***********************************************************************         
                                                                                
DATDIS   NTR1  ,                                                                
         LA    R1,DATHEADH         CLEAR THE DISPLAY SCREEN                     
         LHI   RE,L'DATHEADH+L'DATHEAD                                          
         LA    RF,DATPFKH-1                                                     
DATDIS02 XC    L'DATHEADH(L'DATHEAD,R1),L'DATHEADH(R1)                          
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BXLE  R1,RE,DATDIS02                                                   
                                                                                
         OC    RFPVENDR,RFPVENDR                                                
         BZ    DATDIS22                                                         
         OC    RFPVNXTR,RFPVNXTR                                                
         BZ    DATDIS22                                                         
         OC    RFPVRNGE,RFPVRNGE                                                
         BZ    DATDIS22                                                         
                                                                                
         OC    RFPBSYMB,RFPBSYMB   TEST BILLING ADJUSTMENTS                     
         BZ    DATDIS10                                                         
         LA    R1,DATHEAD          BILLING DATE DISPLAY                         
         LHI   R0,3                                                             
DATDIS04 MVC   0(8,R1),=C'Run date'                                             
         MVC   BLDDBDAY-BLDD(9,R1),=C'Bill date'                                
         AHI   R1,BLDL                                                          
         BCT   R0,DATDIS04                                                      
                                                                                
         LA    R4,DATLIN1H                                                      
         LHI   R2,L'DATLIN1H+L'DATLIN1                                          
         LA    R3,DATPFKH-1                                                     
DATDIS06 LA    R5,L'DATLIN1H(R4)                                                
         LHI   R0,3                                                             
         USING BLDD,R5                                                          
DATDIS08 GOTOR NXTDAT              GET NEXT DATE TO DISPLAY                     
         BNE   DATDIS22                                                         
         MVC   BLDDRDAY,GCRDAY                                                  
         MVC   BLDDRDAT,GCRDATE                                                 
         MVI   BLDDRHYP,C'-'                                                    
         MVC   BLDDBDAY,GCBDAY                                                  
         MVC   BLDDBDAT,GCBDATE                                                 
         AHI   R5,BLDL                                                          
         BCT   R0,DATDIS08                                                      
         BXLE  R4,R2,DATDIS06                                                   
         B     DATDIS20                                                         
         DROP  R5                                                               
                                                                                
DATDIS10 LA    R1,DATHEAD          REGULAR DATE DISPLAY                         
         LHI   R0,6                                                             
DATDIS12 MVC   0(8,R1),=C'Run date'                                             
         AHI   R1,DATL                                                          
         BCT   R0,DATDIS12                                                      
                                                                                
         LA    R4,DATLIN1H                                                      
         LHI   R2,L'DATLIN1H+L'DATLIN1                                          
         LA    R3,DATPFKH-1                                                     
DATDIS14 LA    R5,L'DATLIN1H(R4)                                                
         LHI   R0,6                                                             
         USING DATD,R5                                                          
DATDIS16 GOTOR NXTDAT              GET NEXT DATE TO DISPLAY                     
         BNE   DATDIS22                                                         
         MVC   DATDRDAY,GCRDAY                                                  
         MVC   DATDRDAT,GCRDATE                                                 
         AHI   R5,DATL                                                          
         BCT   R0,DATDIS16                                                      
         BXLE  R4,R2,DATDIS14                                                   
         DROP  R5                                                               
                                                                                
DATDIS20 MVC   FVMSGNO,=AL2(GI$LDHEN)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     DATDISX                                                          
                                                                                
DATDIS22 MVC   FVMSGNO,=AL2(GI$LDHEF)                                           
         MVI   FVOMTYP,GTMINF                                                   
                                                                                
DATDISX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT RUN DATE/BILL DATE TO BE DISPLAYED                         *         
***********************************************************************         
                                                                                
NXTDAT   NTR1  ,                                                                
         OC    LVLEDAT,LVLEDAT     TEST INITIALIZED                             
         BNZ   NXTDAT02                                                         
         GOTOR VDATCON,PCPARM,(6,RFPVNXTR),(0,LVLEDAT)                          
         B     NXTDAT04                                                         
                                                                                
NXTDAT02 GOTOR VADDAY,PCPARM,LVLEDAT,PCWORK,1                                   
         MVC   LVLEDAT,PCWORK                                                   
                                                                                
NXTDAT04 GOTOR VDATCON,PCPARM,LVLEDAT,(15,PCWORK)                               
         CLC   PCWORK(L'RFPVENDR),RFPVENDR                                      
         BH    NXTDATN                                                          
                                                                                
         ZAP   PCDUB,PCWORK+2(2)                                                
         CVB   RE,PCDUB                                                         
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RE,RFPVRNGE(RE)                                                  
         IC    RF,DAYBITS(RF)                                                   
         EX    RF,*+8                                                           
         BZ    NXTDAT02                                                         
         TM    0(RE),0                                                          
                                                                                
         GOTOR VGETDAY,PCPARM,LVLEDAT,PCWORK+4                                  
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MHI   RE,L'PCMMON                                                      
         LA    RE,PCMMON-L'PCMMON(RE)                                           
         MVC   GCRDAY,0(RE)                                                     
         GOTOR VDATCON,PCPARM,(X'40',LVLEDAT),(17,GCRDATE)                      
                                                                                
         OC    RFPBSYMB,RFPBSYMB   TEST BILLING SYMBOL PRESENT                  
         BZ    NXTDATY                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PCWORK+2                                                    
         SRL   RF,4                                                             
         STCM  RF,3,PCWORK                                                      
                                                                                
         LA    RF,RFPBADJS         RF=A(BILLING DATE ADJUSTMENT LIST)           
         LHI   R0,RFPBADJM         R0=MAXIMUM NUMBER OF ENTRIES                 
NXTDAT06 OC    0(L'RFPBADJS,RF),0(RF)                                           
         BZ    NXTDAT08                                                         
         MVC   PCWORK+L'RFPBADJS(L'RFPBADJS),0(RF)                              
         NC    PCWORK+L'RFPBADJS(L'RFPBADJS),=AL2(GRPBACAL)                     
         CLC   PCWORK(L'RFPBADJS),PCWORK+L'RFPBADJS                             
         BE    NXTDAT10                                                         
         AHI   RF,L'RFPBADJS                                                    
         BCT   R0,NXTDAT06                                                      
                                                                                
NXTDAT08 MVC   GCBDAY,GCRDAY       NOT IN LIST - SET FROM RUN DATE              
         MVC   GCBDATE,GCRDATE                                                  
         B     NXTDATY                                                          
                                                                                
NXTDAT10 MVC   PCWORK(L'RFPBADJS),0(RF)                                         
         SR    RF,RF               CALCULATE BILLING DATE                       
         ICM   RF,3,PCWORK                                                      
         SRL   RF,10               RF=NUMBER OF ADJUSTMENT DAYS                 
         GOTOR VADDAY,PCPARM,LVLEDAT,PCWORK,(RF)                                
         GOTOR VGETDAY,PCPARM,PCWORK,PCWORK+6                                   
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MHI   RE,L'PCMMON                                                      
         LA    RE,PCMMON-L'PCMMON(RE)                                           
         MVC   GCBDAY,0(RE)                                                     
         GOTOR VDATCON,PCPARM,(X'40',PCWORK),(17,GCBDATE)                       
                                                                                
NXTDATY  CLI   *+1,0                                                            
         B     EXIT                                                             
                                                                                
NXTDATN  XC    LVLEDAT,LVLEDAT     RE-START DISPLAY                             
         CLI   *+0,0                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Routine to display calendar                                         *         
***********************************************************************         
                                                                                
CALDIS   NTR1  ,                                                                
         GOTOR CALCLR              CLEAR THE DISPLAY SCREEN                     
         CLI   GCFREQ,FRQRDAY      TEST RUNDAYS FREQUENCY                       
         BE    CALDISX                                                          
         MVI   GCLASTM,0                                                        
         MVI   GCFLAG,0                                                         
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   CALDIS02                                                         
         OC    RFPXFILE,RFPXFILE   TEST XFILE ATTACHED TO GROUP                 
         BZ    CALDIS02                                                         
         OI    GCFLAG,GCFXFG       YES - SET FLAG & PROTECT FIELDS              
         OI    KEYFRQH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    KEYEFDTH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    KEYSCHDH+(FVATRB-FVIHDR),FVAPROT                                 
                                                                                
CALDIS02 OC    GCNXTR,GCNXTR       TEST DATES ARE SET                           
         BZ    CALDISX                                                          
         OC    GCENDR,GCENDR                                                    
         BZ    CALDISX                                                          
                                                                                
         MVC   GCLDAY+DAYMONQ-1(1),PCUMON                                       
         MVC   GCLDAY+DAYTUEQ-1(1),PCUTUE                                       
         MVC   GCLDAY+DAYWEDQ-1(1),PCUWED                                       
         MVC   GCLDAY+DAYTHUQ-1(1),PCUTHU                                       
         MVC   GCLDAY+DAYFRIQ-1(1),PCUFRI                                       
         MVC   GCLDAY+DAYSATQ-1(1),PCUSAT                                       
         MVC   GCLDAY+DAYSUNQ-1(1),PCUSUN                                       
                                                                                
         GOTOR VDATCON,PCPARM,(6,GCNXTR),(3,GCPSDB)                             
         MVC   GCWSDB,GCPSDB                                                    
         GOTOR VDATCON,PCPARM,(6,GCENDR),(3,GCPEDB)                             
                                                                                
         GOTOR VDATCON,PCPARM,(3,GCPSDB),(15,GCPSDJ)                            
         GOTOR VDATCON,PCPARM,(3,GCPEDB),(15,GCPEDJ)                            
                                                                                
         GOTOR VDATCON,PCPARM,(3,GCWSDB),(0,GCWORK)                             
         GOTOR VGETDAY,PCPARM,GCWORK,GCWORK+6                                   
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   GCWORK+6(6),GCWORK                                               
         AHI   R0,-1               TEST START DAY IS A MONDAY                   
         BZ    CALDIS06                                                         
         LCR   R0,R0                                                            
                                                                                
CALDIS04 GOTOR VADDAY,PCPARM,('ADDDAYQ',GCWORK),GCWORK+6,(R0)                   
         GOTOR VDATCON,PCPARM,(0,GCWORK+6),(3,GCWSDB)                           
                                                                                
CALDIS06 GOTOR VADDAY,PCPARM,('ADDDAYQ',GCWORK+6),GCWORK,6                      
         GOTOR VDATCON,PCPARM,(0,GCWORK),(3,GCWEDB)                             
                                                                                
         GOTOR VDATCON,PCPARM,(3,GCWSDB),(15,GCWSDJ)                            
         MVC   GCCDDJ,GCWSDJ       SET AS TODAY TOO                             
         GOTOR VDATCON,PCPARM,(3,GCWEDB),(15,GCWEDJ)                            
                                                                                
         MVC   GCCURRM,GCWSDBM                                                  
         MVC   GCCURRY,GCWSDJY     SET CURRENT YEAR                             
         CLC   GCWSDBM,GCWEDBM     TEST WEEK SPANS A MONTH                      
         BE    CALDIS08                                                         
         CLI   GCLASTM,0           TEST FIRST TIME                              
         BNE   CALDIS08                                                         
         MVC   GCCURRM,GCWEDBM                                                  
         MVC   GCCURRY,GCWEDJY     SET CURRENT YEAR                             
                                                                                
CALDIS08 CLC   GCLASTM,GCCURRM     TEST CHANGE OF MONTH                         
         BE    CALDIS10                                                         
                                                                                
         CLI   GCLASTM,0           TEST FIRST TIME                              
         BNE   *+12                                                             
         LA    R2,CALTAB           POINT TO FIRST ENTRY IN CALTAB               
         USING CALTABD,R2                                                       
         B     *+8                                                              
         AHI   R2,CALTABL          BUMP TO NEXT ENTRY IN CALTAB                 
         MVC   GCLASTM,GCCURRM                                                  
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,CALTHEDL                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RE,TWAD(RE)         POINT TO MONTH HEADLINE                      
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,GCCURRM                                                     
         MHI   RF,L'PCMJAN                                                      
         LA    RF,PCMJAN-L'PCMJAN(RF)                                           
         MVC   0(L'PCMJAN,RE),0(RF)                                             
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,CALTSUBL                                                    
         LA    RE,TWAD(RE)         POINT TO MONTH SUB-HEADLINE                  
         UNPK  PCDUB(6),GCCURRY(3)                                              
         MVI   0(RE),DINK                                                       
         MVC   1(2,RE),PCDUB+3                                                  
         MVC   4(L'GCLDAY,RE),GCLDAY                                            
         LA    R3,CALTWK1H         POINT TO FIRST WEEK IN MONTH                 
         B     CALDIS12                                                         
                                                                                
CALDIS10 AHI   R3,L'CALTWK1H       BUMP TO NEXT WEEK IN MONTH                   
                                                                                
CALDIS12 SR    R4,R4                                                            
         ICM   R4,3,0(R3)                                                       
         LA    R4,TWAD(R4)                                                      
         USING WKDD,R4                                                          
         TM    GCFLAG,GCFXFG       TEST XFILE ATTACHED TO GROUP                 
         BNZ   CALDIS14                                                         
         CLC   GCWEDJ,ASJDAT       OPEN CURRENT WEEKS ONWARDS                   
         BL    *+8                                                              
         NI    WKDDAYSH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
                                                                                
CALDIS14 SR    RF,RF               MOVE W/S DAY TO SCREEN                       
         IC    RF,GCWSDBD                                                       
         CVD   RF,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  WKDSDAY(2),PCDUB                                                 
         CLI   WKDSDAY,NUMERIC                                                  
         BNE   *+14                                                             
         MVC   WKDSDAY(1),WKDSDAY+1                                             
         MVI   WKDSDAY+1,SPACE                                                  
                                                                                
         MVI   WKDDAYS,CANUSEDQ                                                 
         MVC   WKDDAYS+1(L'WKDDAYS-1),WKDDAYS                                   
                                                                                
         MVC   GCCDDJ,GCWSDJ       SET CURRENT DATE                             
         LA    R5,WKDDAYS          POINT TO OUTPUT LIST                         
         LHI   R0,L'WKDDAYS                                                     
         MVI   GCNOTCT,0           COUNT OF NUMBER OF UNUSABLE DAYS             
                                                                                
CALDIS16 CLC   GCCDDJ,GCPSDJ       Test day within this period                  
         BL    CALDIS20            No                                           
         CLC   GCCDDJ,GCPEDJ                                                    
         BH    CALDIS20            No                                           
         ZAP   PCDUB,GCCDDJD                                                    
         CVB   RE,PCDUB                                                         
         SRDL  RE,3                SHIFT OFF DAY INTO RF                        
         SRL   RF,32-3                                                          
         LA    RE,GCRNGE(RE)                                                    
         IC    RF,DAYBITS(RF)      POINT TO BIT MASK FOR DAY                    
         EX    RF,*+8                                                           
         BZ    CALDIS18                                                         
         TM    0(RE),0             TEST DAY BIT IS ON                           
         GOTOR DISDAY,(R5)                                                      
         OI    WKDSDAYH+(FVATRB-FVIHDR),FVAHIGH                                 
         B     CALDIS28                                                         
                                                                                
CALDIS18 GOTOR AGETWRK,GCCDDJ      TEST IF THIS IS A WORKING DAY                
         BH    CALDIS21            High is specail DDS holiday                  
         BE    CALDIS28            Equal means a good day                       
         MVC   0(1,R5),GCCALN      Low means holidays etc.                      
         B     CALDIS24                                                         
                                                                                
CALDIS20 MVI   0(R5),NOTINPDQ      '*' MARKS DAYS NOT IN PERIOD                 
         B     CALDIS24                                                         
                                                                                
CALDIS21 MVC   0(1,R5),GCCALH      'H' Holiday                                  
                                                                                
CALDIS24 IC    R1,GCNOTCT          INCREMENT NON-WORKING DAY COUNT              
         AHI   R1,1                                                             
         STC   R1,GCNOTCT                                                       
                                                                                
CALDIS28 GOTOR VDATCON,PCPARM,(6,GCCDDJ),(0,GCWORK)                             
         GOTOR VADDAY,(R1),('ADDDAYQ',GCWORK),GCWORK+6,1                        
         GOTOR VDATCON,(R1),(0,GCWORK+6),(15,GCCDDJ)                            
         AHI   R5,1                                                             
         BCT   R0,CALDIS16                                                      
                                                                                
         CLI   GCNOTCT,L'WKDDAYS   TEST ALL DAYS ARE UNUSABLE                   
         BNE   *+8                                                              
         OI    WKDDAYSH+(FVATRB-FVIHDR),FVAPROT                                 
                                                                                
         CLC   GCWEDB,GCPEDB       TEST REACHED END OF PERIOD                   
         BNL   CALDISX                                                          
         GOTOR VDATCON,PCPARM,(3,GCWEDB),(0,GCWORK)                             
         LHI   R0,1                                                             
         B     CALDIS04                                                         
                                                                                
CALDISX  B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY DAY OR BILLING DATE ADJUSTMENT DAY                          *         
***********************************************************************         
                                                                                
DISDAY   OC    RFPBSYMB,RFPBSYMB   TEST ADJUSTMENT SYMBOLIC PRESENT             
         BNZ   *+12                                                             
         MVC   0(1,R1),GCCALY      NO - OUTPUT 'Y'                              
         BR    RE                                                               
                                                                                
         STM   RE,R1,12(RD)                                                     
         SR    RF,RF               STRIP SIGN FROM PACKED DAY                   
         ICM   RF,3,GCCDDJD                                                     
         SRL   RF,4                                                             
         STH   RF,OVHALF1          AND SAVE FOR COMPARE                         
         LA    RF,GCADJS           RE=A(ADJUSTMENT TABLE)                       
         LHI   R0,RFPBADJM         R0=MAXIMUM NUMBER OF ENTRIES                 
DISDAY02 OC    0(L'RFPBADJS,RF),0(RF)                                           
         BZ    DISDAY04                                                         
         MVC   OVHALF2,0(RF)       EXTRACT DAY                                  
         NC    OVHALF2,=AL2(GRPBACAL)                                           
         CLC   OVHALF1,OVHALF2     TEST DAY MATCHES                             
         BE    DISDAY06                                                         
         AHI   RF,L'RFPBADJS       NO - BUMP TO NEXT                            
         BCT   R0,DISDAY02                                                      
                                                                                
DISDAY04 MVC   0(1,R1),GCCALY      OUTPUT 'Y' IF NO ADJUSTMENT                  
         B     DISDAYX                                                          
                                                                                
DISDAY06 SR    RE,RE               ISOLATE ADJUSTMENT VALUE                     
         ICM   RE,3,0(RF)                                                       
         SRL   RE,10                                                            
         LA    RE,DAYTAB-1(RE)     INDEX INTO DAY TABLE                         
         MVC   0(1,R1),0(RE)                                                    
                                                                                
DISDAYX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to test for changes in schedule screen                      *         
***********************************************************************         
                                                                                
CALCHA   NTR1  ,                                                                
         XC    GCADJS,GCADJS                                                    
         MVI   GCFLAG,0                                                         
         MVI   GCLASTM,0                                                        
         XC    GCERRS(GCERRSL),GCERRS                                           
         GOTOR VDATCON,PCPARM,(6,GCNXTR),(3,GCPSDB)                             
         MVC   GCWSDB,GCPSDB                                                    
         GOTOR VDATCON,PCPARM,(6,GCENDR),(3,GCPEDB)                             
                                                                                
         GOTOR VDATCON,PCPARM,(3,GCPSDB),(15,GCPSDJ)                            
         GOTOR VDATCON,PCPARM,(3,GCPEDB),(15,GCPEDJ)                            
                                                                                
         GOTOR VDATCON,PCPARM,(3,GCWSDB),(0,GCWORK)                             
         GOTOR VGETDAY,PCPARM,GCWORK,GCWORK+6                                   
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   GCWORK+6(6),GCWORK                                               
         AHI   R0,-1               TEST START DAY IS A MONDAY                   
         BZ    CALCHA04                                                         
         LCR   R0,R0               NO - GO BACK TO START OF WEEK                
                                                                                
CALCHA02 GOTOR VADDAY,PCPARM,('ADDDAYQ',GCWORK),GCWORK+6,(R0)                   
         GOTOR VDATCON,PCPARM,(0,GCWORK+6),(3,GCWSDB)                           
                                                                                
CALCHA04 GOTOR VADDAY,PCPARM,('ADDDAYQ',GCWORK+6),GCWORK,6                      
         GOTOR VDATCON,PCPARM,(0,GCWORK),(3,GCWEDB)                             
                                                                                
         GOTOR VDATCON,PCPARM,(3,GCWSDB),(15,GCWSDJ)                            
         MVC   GCCDDJ,GCWSDJ       SET AS TODAY TOO                             
         GOTOR VDATCON,PCPARM,(3,GCWEDB),(15,GCWEDJ)                            
                                                                                
         MVC   GCCURRM,GCWSDBM                                                  
         MVC   GCCURRY,GCWSDJY     SET CURRENT YEAR                             
         CLC   GCWSDBM,GCWEDBM     TEST WEEK SPANS A MONTH                      
         BE    CALCHA06                                                         
         CLI   GCLASTM,0           TEST FIRST TIME                              
         BNE   CALCHA06                                                         
         MVC   GCCURRM,GCWEDBM                                                  
         MVC   GCCURRY,GCWEDJY     SET CURRENT YEAR                             
                                                                                
CALCHA06 CLC   GCLASTM,GCCURRM     TEST CHANGE OF MONTH                         
         BE    CALCHA08                                                         
                                                                                
         CLI   GCLASTM,0           TEST FIRST TIME                              
         BNE   *+12                                                             
         LA    R2,CALTAB           YES - POINT TO FIRST ENTRY IN CALTAB         
         USING CALTABD,R2                                                       
         B     *+8                                                              
         AHI   R2,CALTABL          NO - BUMP TO NEXT ENTRY IN CALTAB            
         MVC   GCLASTM,GCCURRM                                                  
         LA    R3,CALTWK1H         POINT TO FIRST WEEK IN MONTH                 
         B     CALCHA10                                                         
                                                                                
CALCHA08 AHI   R3,L'CALTWK1H       BUMP TO NEXT WEEK IN MONTH                   
                                                                                
CALCHA10 SR    R4,R4                                                            
         ICM   R4,3,0(R3)                                                       
         LA    R4,TWAD(R4)                                                      
         USING WKDD,R4                                                          
         NI    GCFLAG,FF-(GCFERR)  TURN OFF ERROR FLAG                          
         LA    R0,WKDDAYSH                                                      
         ST    R0,FVADDR                                                        
                                                                                
         MVC   GCCDDJ,GCWSDJ       Set current date                             
         LA    R5,WKDDAYS          Point to output list                         
CALCHA12 CLC   GCCDDJ,GCTDYJ       Don't change prior to todays date            
         BL    CALCHA24            Skip                                         
         CLC   GCCDDJ,GCWEDJ       Test reached end of week                     
         BH    CALCHA26                                                         
         CLC   GCCDDJ,GCPSDJ       Test day within this period                  
         BL    CALCHA14                                                         
         CLC   GCCDDJ,GCPEDJ                                                    
         BH    CALCHA14                                                         
                                                                                
         ZAP   PCDUB,GCCDDJD                                                    
         CVB   RE,PCDUB                                                         
         SRDL  RE,3                SHIFT OFF DAY INTO RF                        
         SRL   RF,32-3                                                          
         LA    RE,GCRNGE(RE)                                                    
         IC    RF,DAYBITS(RF)      POINT TO BIT MASK FOR DAY                    
         EX    RF,*+8                                                           
         BZ    CALCHA16                                                         
         TM    0(RE),0             TEST DAY BIT IS ON                           
         LR    R0,RE               SAVE RANGE POINTER                           
         GOTOR VALDAY,0(R5)                                                     
         BNE   *+12                                                             
         GOTOR SETDAY,GCCDDJ                                                    
         LR    RE,R0               RESTORE RANGE POINTER                        
         BE    CALCHA24                                                         
         CLC   0(1,R5),GCCALN      CAN BE 'N' OR '-'                            
         BE    CALCHA13                                                         
         CLC   0(1,R5),GCCALH      CAN BE 'H'                                   
         BE    CALCHA13                                                         
         CLI   0(R5),CANUSEDQ                                                   
         BNE   CALCHA20                                                         
*                                                                               
CALCHA13 EX    RF,*+8              TURN DAY BIT OFF                             
         B     CALCHA24                                                         
         XI    0(RE),0                                                          
                                                                                
CALCHA14 CLI   0(R5),NOTINPDQ      ALWAYS SHOW 'NOT IN PERIOD'                  
         BE    CALCHA24                                                         
         MVI   0(R5),NOTINPDQ                                                   
         OI    WKDDAYSH+(FVOIND-FVIHDR),FVOXMT                                  
         B     CALCHA24                                                         
                                                                                
CALCHA16 CLC   0(1,R5),PCQUEST     TEST FOR PREVIOUS ERROR                      
         BNE   *+14                                                             
         MVC   0(1,R5),GCCALN      YES - RESET TO 'N'                           
         B     CALCHA24                                                         
                                                                                
         STM   RE,RF,GCSAVREG                                                   
         GOTOR VALDAY,0(R5)                                                     
         BNE   *+12                                                             
         GOTOR SETDAY,GCCDDJ                                                    
         LM    RE,RF,GCSAVREG                                                   
         BE    CALCHA18                                                         
         CLC   0(1,R5),GCCALN      TEST 'N' OR '-'                              
         BE    CALCHA24                                                         
         CLC   0(1,R5),GCCALH      TEST 'H' for holiday                         
         BE    CALCHA24                                                         
         CLI   0(R5),CANUSEDQ                                                   
         BE    CALCHA24                                                         
         B     CALCHA20            OTHERWISE IS ERROR                           
                                                                                
CALCHA18 STM   RE,RF,GCSAVREG                                                   
         GOTOR AGETWRK,GCCDDJ      TEST IF THIS IS A WORKING DAY                
         LM    RE,RF,GCSAVREG                                                   
         BE    CALCHA22                                                         
                                                                                
CALCHA20 OI    GCFLAG,GCFERR       SET ERROR THIS WEEK                          
         LH    R0,GCENERR          INCREMENT N'ERRORS FOUND                     
         AHI   R0,1                                                             
         STH   R0,GCENERR                                                       
         MVC   0(1,R5),PCQUEST     MOVE IN QUESTION MARK                        
         OI    WKDSDAYH+(FVATRB-FVIHDR),FVAHIGH                                 
         OI    WKDSDAYH+(FVOIND-FVIHDR),FVOXMT                                  
         NI    WKDDAYSH+(FVATRB-FVIHDR),FF-(FVAHIGH)                            
         OI    WKDDAYSH+(FVOIND-FVIHDR),FVOXMT                                  
         OC    GCEAERR,GCEAERR     TEST ERROR PREVIOUSLY FOUND                  
         BNZ   CALCHA22                                                         
         LA    R0,WKDDAYSH                                                      
         ST    R0,GCEAERR          NO - POINT TO THIS WEEK                      
         LA    R0,WKDDAYS                                                       
         LR    RF,R5                                                            
         SR    RF,R0                                                            
         STC   RF,GCEINDX          SET INDEX TO ERROR                           
         B     CALCHA24                                                         
                                                                                
CALCHA22 EX    RF,*+8              YES - TURN ON BIT IN MASK                    
         B     CALCHA24                                                         
         OI    0(RE),0                                                          
                                                                                
CALCHA24 GOTOR VDATCON,PCPARM,(6,GCCDDJ),(0,GCWORK)                             
         GOTOR VADDAY,(R1),('ADDDAYQ',GCWORK),GCWORK+6,1                        
         GOTOR VDATCON,(R1),(0,GCWORK+6),(15,GCCDDJ)                            
         AHI   R5,1                BUMP TO NEXT INPUT CHARACTER                 
         B     CALCHA12                                                         
                                                                                
CALCHA26 TM    GCFLAG,GCFERR       TEST ANY ERRORS THIS WEEK                    
         BNZ   CALCHA28                                                         
         TM    WKDSDAYH+(FVATRB-FVIHDR),FVAHIGH                                 
         BZ    CALCHA28                                                         
         NI    WKDSDAYH+(FVATRB-FVIHDR),FF-(FVAHIGH)                            
         OI    WKDSDAYH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    WKDDAYSH+(FVATRB-FVIHDR),FVAHIGH                                 
         OI    WKDDAYSH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
CALCHA28 CLC   GCWEDB,GCPEDB       TEST REACHED END OF PERIOD                   
         BNL   CALCHA30                                                         
         GOTOR VDATCON,PCPARM,(3,GCWEDB),(0,GCWORK)                             
         LHI   R0,1                                                             
         B     CALCHA02                                                         
                                                                                
CALCHA30 OC    GCENERR,GCENERR     TEST ANY ERRORS FOUND                        
         BZ    CALCHAY                                                          
         MVC   FVADDR,GCEAERR                                                   
         MVC   FVERRNDX,GCEINDX                                                 
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         CLI   *+0,0               EXIT CC=NOT EQUAL ON ERROR                   
         B     EXIT                                                             
                                                                                
CALCHAY  CLI   *+1,0               EXIT CC=EQUAL IF ALL OKAY                    
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE DAY OR ADJUSTMENT DAY                                      *         
***********************************************************************         
                                                                                
VALDAY   MVI   GCBADJ,0            SET NO ADJUSTMENT DAY PRESENT                
                                                                                
         CLC   0(1,R1),GCCALY      TEST FOR 'Y' (NO ADJUSTMENT)                 
         BER   RE                                                               
         OC    RFPBSYMB,RFPBSYMB   TEST ADJUSTMENT SYMBOLIC PRESENT             
         BNZ   *+8                                                              
         LTR   RE,RE               NO - SET CC=NOT EQUAL                        
         BR    RE                                                               
                                                                                
         STM   RE,R1,12(RD)        TEST FOR ADJUSTMENT DAY VALUE                
         LA    RE,DAYTAB                                                        
         LHI   R0,L'DAYTAB                                                      
         LHI   RF,1                                                             
VALDAY02 CLC   0(1,R1),0(RE)                                                    
         BE    VALDAY04                                                         
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         BCT   R0,VALDAY02                                                      
         LM    RE,R1,12(RD)                                                     
         LTR   RE,RE               RETURN CC=NOT EQUAL IF NOT IN LIST           
         BR    RE                                                               
                                                                                
VALDAY04 STC   RF,GCBADJ           SET ADJUSTMENT DAY                           
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Set adjustment day                                                  *         
***********************************************************************         
                                                                                
SETDAY   CLI   GCBADJ,0            TEST ADJUSTMENT DAY PRESENT                  
         BER   RE                                                               
         STM   RE,R1,12(RD)                                                     
         SR    RF,RF                                                            
         IC    RF,GCBADJ                                                        
         SLL   RF,10                                                            
         STH   RF,OVHALF2                                                       
         ICM   RF,3,2(R1)                                                       
         SRL   RF,4                SHIFT OFF DAY SIGN                           
         STH   RF,OVHALF1                                                       
         OC    OVHALF1,OVHALF2     OVHALF1=ADJUSTMENT VALUE/DAY                 
                                                                                
         LA    RE,GCADJS           ADD ENTRY TO ADJUSTMENT LIST                 
         LHI   R0,RFPBADJM                                                      
SETDAY02 OC    0(L'RFPBADJS,RE),0(RE)                                           
         BZ    SETDAY04                                                         
         AHI   RE,L'RFPBADJS                                                    
         BCT   R0,SETDAY02                                                      
         LM    RE,R1,12(RD)                                                     
         LTR   RE,RE               RETURN CC=NOT EQUAL IF TABLE FULL            
         BR    RE                                                               
                                                                                
SETDAY04 MVC   0(L'RFPBADJS,RE),OVHALF1                                         
         MVI   GCBADJ,0                                                         
         LM    RE,R1,12(RD)                                                     
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Set GCRNGE prior to today to keep history, based on RFPVRNGE                  
***********************************************************************         
SETRNGE  NTR1                                                                   
         XC    GCRNGE,GCRNGE                                                    
         OC    GCNXTR,GCNXTR       Test next run date set                       
         BZ    SETRNG90            No - exit                                    
         GOTOR VDATCON,PCPARM,(6,GCNXTR),(0,GCWORK)   Start date                
         GOTOR VGETDAY,PCPARM,GCWORK,GCWORK+6                                   
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)          Get day of week Monday=1                     
         BNZ   *+6                                                              
         DC    H'0'                Some thing don't make sense                  
         MVC   GCWORK+6(6),GCWORK                                               
         SHI   R0,1                Test start day is a Monday                   
         BZ    SETRNG10            Monday = 1 , so zero if true                 
         LCR   R0,R0               No - Go back to get prior Monday             
         GOTOR VADDAY,PCPARM,('ADDDAYQ',GCWORK),GCWORK+6,(R0)                   
                                                                                
SETRNG10 GOTOR VDATCON,PCPARM,(0,GCWORK+6),(15,GCCDDJ)                          
         CLC   GCCDDJ,GCTDYJ       Don't change prior to todays date            
         BH    SETRNG90            Done                                         
         GOTOR AGETWRK,GCCDDJ      Test for holidays                            
         ZAP   PCDUB,PCWORK+2(2)                                                
         CVB   RE,PCDUB                                                         
         SRDL  RE,3                Shift off day info into RF from RE           
         SRL   RF,32-3             Move isolated value to low order             
         ST    RE,GCSAVREG                                                      
         LA    RE,RFPVRNGE(RE)     Point into bit range field                   
         IC    RF,DAYBITS(RF)      Point to bit maks for Day                    
         EX    RF,*+8                                                           
         BZ    SETRNG20            Not on so okay as is GCRNGE=RPFVRNGE         
         TM    0(RE),0             Is bit on, if not don't turn it on           
         L     RE,GCSAVREG         Restore index values                         
         LA    RE,GCRNGE(RE)       Turn bit on                                  
         EX    RF,*+8                                                           
         B     SETRNG20            Do again                                     
         OI    0(RE),0                                                          
                                                                                
SETRNG20 GOTOR VADDAY,PCPARM,('ADDDAYQ',GCWORK+6),GCWORK,1                      
         MVC   GCWORK+6(6),GCWORK                                               
         B     SETRNG10                                                         
                                                                                
SETRNG90 B     EXIT                                                             
                                                                                
***********************************************************************         
* Routine to build a new schedule from user input                     *         
***********************************************************************         
                                                                                
CALBLD   NTR1  ,                                                                
         GOTOR AGETFRQ,GCFREQ      LOOK UP FREQUENCY IN TABLE                   
         MVC   GCFREQI,PCWORK+FRQNAMLQ                                          
                                                                                
         TM    GCFREQI,FRQTINOQ    TEST NO RUN SCHEDULE                         
         BNZ   CALBLDY                                                          
         CLI   GCFREQ,FRQRDAY      TEST RUNDAYS FREQUENCY                       
         BE    CALBLDY                                                          
                                                                                
         GOTOR VDATCON,PCPARM,(6,GCNXTR),(3,GCPSDB)                             
         GOTOR VDATCON,PCPARM,(6,GCENDR),(3,GCPEDB)                             
                                                                                
         GOTOR VDATCON,PCPARM,(3,GCPSDB),(15,GCPSDJ)                            
         GOTOR VDATCON,PCPARM,(3,GCPEDB),(15,GCPEDJ)                            
                                                                                
         MVC   GCWSDB,GCPSDB       SET WEEK START                               
         MVC   GCMSDB,GCPSDB       AND MONTH START                              
                                                                                
***********************************************************************         
* Handle weekly increments                                            *         
***********************************************************************         
         TM    GCFREQI,FRQTIWKQ    TEST WEEKLY INCREMENTS                       
         BZ    CALBLD14                                                         
                                                                                
         CLI   GCDTYP,GCDTDAYQ     ENSURE WE HAVE A DAY MASK                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR VDATCON,PCPARM,(3,GCWSDB),(0,GCWORK)                             
         GOTOR VGETDAY,PCPARM,GCWORK,GCWORK+6                                   
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   GCWORK+6(6),GCWORK                                               
         SHI   R0,1                Test start day is a Monday                   
         BZ    CALBLD04            Monday = 1 , so zero if true                 
         LCR   R0,R0               No - Go back to start of week                
*                                  Get first Monday in GCWORK+6                 
                                                                                
CALBLD02 GOTOR VADDAY,PCPARM,('ADDDAYQ',GCWORK),GCWORK+6,(R0)                   
         GOTOR VDATCON,PCPARM,(0,GCWORK+6),(3,GCWSDB)                           
                                                                                
CALBLD04 GOTOR VADDAY,PCPARM,('ADDDAYQ',GCWORK+6),GCWORK,6                      
         GOTOR VDATCON,PCPARM,(0,GCWORK),(3,GCWEDB)                             
                                                                                
         GOTOR VDATCON,PCPARM,(0,GCWORK+6),(15,GCCDDJ)                          
                                                                                
         ICM   R5,8,GCDDAYS        R5=DAY MASK                                  
         LHI   R3,DAYSINWQ         R3=N'DAYS IN MASK (7)                        
         SR    R2,R2               R2=ADDAY increment value                     
                                                                                
CALBLD06 SR    R4,R4                                                            
         SLDL  R4,1                Shift Day into R4 from R5                    
         LTR   R4,R4               Day to set on?                               
         BZ    CALBLD10            No                                           
                                                                                
CALBLD07 MVC   GCWORK(6),GCWORK+6                                               
         LTR   R2,R2               Test zero increment                          
         BZ    CALBLD08            First day                                    
         GOTOR VADDAY,PCPARM,('ADDDAYQ',GCWORK+6),GCWORK,(R2)                   
                                                                                
CALBLD08 GOTOR VDATCON,PCPARM,(0,GCWORK),(15,GCCDDJ)                            
         GOTOR AGETWRK,GCCDDJ      Test for holidays                            
         BNE   CALBLD10            Not equal means not valid day to run         
         CLC   GCCDDJ,GCTDYJ       Don't change prior to todays date            
         BL    CALBLD10            Skip                                         
         CLC   PCWORK(L'GCPSDJ),GCPSDJ                                          
         BL    CALBLD10                                                         
         CLC   PCWORK(L'GCPEDJ),GCPEDJ                                          
         BH    CALBLD12                                                         
         ZAP   PCDUB,PCWORK+2(2)                                                
         CVB   RE,PCDUB                                                         
         SRDL  RE,3                SHIFT OFF DAY INTO RF                        
         SRL   RF,32-3                                                          
         LA    RE,GCRNGE(RE)                                                    
         IC    RF,DAYBITS(RF)      POINT TO BIT MASK FOR DAY                    
         EX    RF,*+8                                                           
         B     CALBLD10                                                         
         OI    0(RE),0                                                          
                                                                                
CALBLD10 AHI   R2,1                BUMP ADDAY INCREMENT                         
         BCT   R3,CALBLD06         DO FOR NUMBER OF DAYS IN WEEK                
                                                                                
CALBLD12 CLC   GCWEDB,GCPEDB       TEST REACHED END OF PERIOD                   
         BNL   CALBLDY                                                          
         GOTOR VDATCON,PCPARM,(3,GCWEDB),(0,GCWORK)                             
         LHI   R0,1                                                             
         B     CALBLD02                                                         
                                                                                
***********************************************************************         
* Handle monthly increments                                           *         
***********************************************************************         
CALBLD14 TM    GCFREQI,FRQTIMNQ    TEST MONTHLY INCREMENTS                      
         BZ    CALBLD30                                                         
                                                                                
         CLI   GCDTYP,GCDTDATQ     ENSURE WE HAVE A DATE MASK                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATCON,PCPARM,(3,GCMSDB),(0,GCWORK)                             
         B     CALBLD18                                                         
                                                                                
CALBLD16 MVC   GCWORK+4(2),DAY01   ADVANCE TO NEXT MONTH                        
         MVC   PCBYTE1,GCFREQI                                                  
         NI    PCBYTE1,FRQTIIVQ                                                 
         SR    R0,R0                                                            
         IC    R0,PCBYTE1          R0=NUMBER OF MONTHS TO ADVANCE               
         GOTOR VADDAY,PCPARM,('ADDMONQ',GCWORK),GCWORK+6,(R0)                   
         MVC   GCWORK(6),GCWORK+6                                               
                                                                                
CALBLD18 MVC   GCWORK+4(2),DAY01   GET MONTH START/END DATES                    
         GOTOR VDATCON,PCPARM,(0,GCWORK),(3,GCMSDB)                             
         CLC   GCMSDB,GCPEDB       TEST MONTH START AFTER PERIOD ENDS           
         BH    CALBLDY                                                          
         GOTOR VADDAY,PCPARM,('ADDDAYQ',GCWORK),GCWORK+6,32                     
         MVC   GCWORK+10(2),DAY01                                               
         GOTOR VADDAY,PCPARM,('ADDDAYQ',GCWORK+6),GCWORK+12,-1                  
         GOTOR VDATCON,PCPARM,(0,GCWORK+12),(3,GCMEDB)                          
                                                                                
         ICM   R5,15,GCDDATS       R5=DAY MASK                                  
         LHI   R3,DAYSINMQ         R3=N'DAYS IN MASK                            
         LHI   R2,1                R2=DAY NUMBER                                
                                                                                
CALBLD20 SR    R4,R4               SHIFT OFF NEXT MASK BIT                      
         SLDL  R4,1                                                             
         LTR   R4,R4               TEST THIS DATE BIT IS ON                     
         BZ    CALBLD26                                                         
         MVC   GCWORK+6(4),GCWORK                                               
         LR    R0,R2                                                            
         CLM   R0,1,GCMEDBD        TEST WITHIN THIS MONTH                       
         BNH   CALBLD22                                                         
         ICM   R0,1,GCMEDBD        NO - USE LAST DAY OF MONTH                   
                                                                                
         CLI   PCPDRCTN,PCPDRFQ    TEST FORWARDS OPTION SET                     
         BNE   CALBLD22                                                         
         CVD   R0,PCDUB            YES - GET FIRST OF NEXT MONTH                
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  GCWORK+10(2),PCDUB                                               
         GOTOR VADDAY,PCPARM,GCWORK+6,GCWORK+12,1                               
         MVC   GCWORK+6(6),GCWORK+12                                            
         B     CALBLD24                                                         
                                                                                
CALBLD22 CVD   R0,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  GCWORK+10(2),PCDUB                                               
                                                                                
CALBLD24 GOTOR VDATCON,PCPARM,(0,GCWORK+6),(15,GCCDDJ)                          
                                                                                
         GOTOR AGETWRK,GCCDDJ      TEST FOR HOLIDAYS                            
                                                                                
         CLC   PCWORK(L'GCPSDJ),GCPSDJ                                          
         BL    CALBLD26                                                         
         CLC   PCWORK(L'GCPEDJ),GCPEDJ                                          
         BH    CALBLD28                                                         
                                                                                
         ZAP   PCDUB,PCWORK+2(2)                                                
         CVB   RE,PCDUB                                                         
         SRDL  RE,3                SHIFT OFF DAY INTO RF                        
         SRL   RF,32-3                                                          
         LA    RE,GCRNGE(RE)                                                    
         IC    RF,DAYBITS(RF)      POINT TO BIT MASK FOR DAY                    
         EX    RF,*+8                                                           
         B     *+8                                                              
         OI    0(RE),0                                                          
                                                                                
         LA    R1,GCBADJS-1(R2)                                                 
         CLI   0(R1),0             TEST FOR BILLING ADJUSTMENT                  
         BE    CALBLD26                                                         
         MVC   GCBADJ,0(R1)                                                     
         GOTOR SETDAY,PCWORK                                                    
                                                                                
CALBLD26 AHI   R2,1                BUMP DAY NUMBER                              
         BCT   R3,CALBLD20         DO FOR NUMBER OF DAYS IN MONTH               
                                                                                
CALBLD28 CLC   GCMEDB,GCPEDB       TEST REACHED END OF PERIOD                   
         BL    CALBLD16                                                         
         B     CALBLDY                                                          
                                                                                
CALBLD30 DC    H'0'                UNKNOWN INCREMENT VALUE                      
                                                                                
CALBLDY  CLI   *+1,0               SET CC=EQUAL FOR CALLER                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLEAR CALENDAR                                           *         
***********************************************************************         
                                                                                
CALCLR   ST    RE,12(RD)                                                        
         XC    CALHED1,CALHED1                                                  
         OI    CALHED1H+(FVOIND-FVIHDR),FVOXMT                                  
         XC    CALSUB1,CALSUB1                                                  
         OI    CALSUB1H+(FVOIND-FVIHDR),FVOXMT                                  
         XC    CALHED2,CALHED2                                                  
         OI    CALHED2H+(FVOIND-FVIHDR),FVOXMT                                  
         XC    CALSUB2,CALSUB2                                                  
         OI    CALSUB2H+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
         LA    RE,CALTAB                                                        
         USING CALTABD,RE                                                       
CALCLR02 CLC   CALTHEDL,=AL2(EOT)  TEST END OF TABLE                            
         BE    CALCLR06                                                         
                                                                                
         LA    R1,CALTWK1H                                                      
         LHI   R0,CALTWKSN                                                      
CALCLR04 SR    RF,RF                                                            
         ICM   RF,3,0(R1)                                                       
         LA    RF,TWAD(RF)                                                      
         USING WKDD,RF                                                          
                                                                                
         XC    WKDSDAY,WKDSDAY                                                  
         NI    WKDSDAYH+(FVATRB-FVIHDR),FF-(FVAHIGH)                            
         OI    WKDSDAYH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    WKDDAYS,WKDDAYS                                                  
         OI    WKDDAYSH+(FVATRB-FVIHDR),FVAHIGH+FVAPROT                         
         OI    WKDDAYSH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
         AHI   R1,L'CALTWK1H       BUMP TO NEXT WEEK ENTRY                      
         BCT   R0,CALCLR04         DO FOR NUMBER OF WEEKS                       
         AHI   RE,CALTABL          BUMP TO NEXT MONTH ENTRY                     
         B     CALCLR02            DO TILL END OF TABLE                         
                                                                                
CALCLR06 L     RE,12(RD)                                                        
         CLI   *+1,0               SET CC=EQUAL                                 
         BR    RE                                                               
         DROP  RE,RF                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE SLAVE RFP GROUPS WITH NEW VALUES                  *         
***********************************************************************         
                                                                                
UPDRFP   NTR1  ,                                                                
         L     R4,AIO1             R4=A(XFILE RECORD)                           
         AHI   R4,XFFRSTEL-XFILED                                               
         USING XFSGRPD,R4                                                       
         SR    R0,R0                                                            
UPDRFP02 IC    R0,XFSGLN           BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         CLI   XFSGCD,0            TEST END OF RECORD                           
         BE    UPDRFPY                                                          
         CLI   XFSGCD,XFSGCDEQ     TEST SYSTEM/GROUP ELEMENT                    
         BNE   UPDRFP02                                                         
                                                                                
         MVC   CUUSER,TWAUSRID     CALL RFP TO GET GROUP                        
         MVC   CUAALF,TWAAGY                                                    
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    *+16                                                             
         MVC   CUUSER,GRPLUSER                                                  
         MVC   CUAALF,GRPLAGY                                                   
         GOTOR AGETSYS,XFSGSYS     CONVERT SYSTEM NUMBER TO LETTER              
         MVC   CUSYSL,PCWORK+L'SYSLNAME                                         
         LA    R1,XFSGGRP                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFP TO VALIDATE THE GROUP               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     UPDRFPN                                                          
*&&UK                                                                           
         CLC   RFPXFILE,GCGRP      ENSURE THIS GROUP POINTS TO XFILE            
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         CLC   RFPVFREQ,GCFREQ     TEST FOR GROUP CHANGES                       
         BNE   UPDRFP04                                                         
         CLC   RFPVNXTR,GCNXTR                                                  
         BNE   UPDRFP04                                                         
         CLC   RFPVENDR,GCENDR                                                  
         BNE   UPDRFP04                                                         
         CLC   RFPVDAYS,GCDAYS                                                  
         BNE   UPDRFP04                                                         
         CLC   RFPVRNGE,GCRNGE                                                  
         BNE   UPDRFP04                                                         
         CLC   RFPBADJS(RFPBADJL),GCADJS                                        
         BE    UPDRFP02                                                         
                                                                                
UPDRFP04 MVC   RFPVFREQ,GCFREQ     MOVE IN NEW VALUES                           
         MVC   RFPVNXTR,GCNXTR                                                  
         MVC   RFPVENDR,GCENDR                                                  
         MVC   RFPVDAYS,GCDAYS                                                  
         MVC   RFPVRNGE,GCRNGE                                                  
         MVC   RFPBADJS(RFPBADJL),GCADJS                                        
                                                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPSAVGP    AND SAVE THE GROUP                           
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   DIE IF ANY ERRORS                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OI    TWAGLI1,TWAGLIRL    SET TO REFRESH GROUP/LIST                    
         B     UPDRFP02                                                         
                                                                                
UPDRFPY  CLI   *+1,0               EXIT WITH CC=EQUAL IF OKAY                   
         B     EXIT                                                             
UPDRFPN  CLI   *+0,0               EXIT WITH CC=NOT EQUAL IF NOT OKAY           
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
GCWORKD  DSECT                     ** GROUP/CALENDAR LOCAL W/S **               
                                                                                
GCCALY   DS    C                                                                
GCCALN   DS    C                                                                
GCCALH   DS    C                   Holiday                                      
                                                                                
GCBADJ   DS    AL1                 BILLING ADJUSTMENT DAY                       
GCBADJS  DS    XL(DAYSINMQ)        BILLING ADJUSTMENT DAYS                      
                                                                                
GCRDAY   DS    CL3                                                              
GCRDATE  DS    CL8                                                              
GCBDAY   DS    CL3                                                              
GCBDATE  DS    CL8                                                              
                                                                                
GCWORK   DS    XL64                GENERAL WORK AREA                            
                                                                                
GCSAVREG DS    2A                  SAVE REGISTER AREA                           
                                                                                
GCBLK    DS    0X                  ** GROUP BLOCK **                            
                                                                                
GCKEY    DS    0X                  ** KEY VALUES **                             
                                                                                
GCGRP    DS    CL(L'RFPVGRP)       GROUP CODE                                   
                                                                                
GCKEYL   EQU   *-GCKEY                                                          
                                                                                
GCDATA   DS    0X                  ** DATA VALUES **                            
                                                                                
GCCHA    DS    X                   GLOBAL CHANGE FLAG                           
                                                                                
GCFREQ   DS    CL(L'RFPVFREQ)      FREQUENCY                                    
                                                                                
GCNXTR   DS    0XL(L'RFPVNXTR)     NEXT RUN DATE (JULIAN)                       
GCNXTRY  DS    XL2                                                              
GCNXTRD  DS    PL2                                                              
                                                                                
GCENDR   DS    0XL(L'RFPVENDR)     LAST RUN DATE (JULIAN)                       
GCENDRY  DS    XL2                                                              
GCENDRD  DS    PL2                                                              
                                                                                
GCTDYJ   DS    0XL4                Today Julian                                 
GCTDYJY  DS    XL2                                                              
GCTDYJD  DS    PL2                                                              
                                                                                
GCDAYS   DS    0XL(L'RFPVDAYS)     RUN SCHEDULE                                 
GCDCON   DS    XL1                 CONTROL BYTE                                 
GCDCONQ  EQU   X'FF'               CONVERTED TO NEW FORMAT                      
GCDTYP   DS    XL1                 LIST TYPE                                    
GCDTDAYQ EQU   1                   LIST OF DAYS                                 
GCDTDATQ EQU   2                   LIST OF DATES                                
GCDTNOTQ EQU   3                   LIST NOT PRESENT (IRREGULAR)                 
GCDDAYS  DS    0XL1                LIST OF DAYS (X'80'=MON ETC.)                
GCDDATS  DS    XL4                 LIST OF DATES (X'80'=1,X'40'=2 ETC.)         
         DS    XL2                 N/D                                          
                                                                                
GCDATAL1 EQU   *-GCDATA                                                         
                                                                                
GCRNGE   DS    XL(L'RFPVRNGE)      SUBMISSION DAYS                              
GCADJS   DS    XL(RFPBADJL)        BILLING DATE ADJUSTMENTS                     
                                                                                
GCDATAL2 EQU   *-GCDATA                                                         
                                                                                
GCBLKL   EQU   *-GCBLK                                                          
                                                                                
GCERRS   DS    0A                  CALCHA S/R SAVED ERROR VALUES                
GCEAERR  DS    A                   A(FIRST FIELD IN ERROR)                      
GCENERR  DS    H                   N'ERRORS FOUND                               
GCEINDX  DS    X                   INDEX TO FIRST CHARACTER IN ERROR            
GCERRSL  EQU   *-GCERRS                                                         
                                                                                
GCFLAG   DS    XL1                 FLAG BYTE                                    
GCFERR   EQU   X'80'               FOUND AN ERROR IN THIS WEEK                  
GCFXFG   EQU   X'40'               XFILE GROUP ATTACHED TO RFP GROUP            
                                                                                
GCNOTCT  DS    XL1                 N'UNUSABLE DAYS IN WEEK                      
                                                                                
GCFREQI  DS    XL(L'FRQTIND)       FREQUENCY INDICATORS                         
                                                                                
GCCURRY  DS    XL2                 CURRENT YEAR                                 
GCCURRM  DS    XL1                 CURRENT MONTH                                
GCLASTM  DS    XL1                 LAST YEAR                                    
                                                                                
GCLDAY   DS    CL7                 DAYS OF WEEK (C'MTWTFSS')                    
                                                                                
GCPSDJ   DS    0XL4                PERIOD START DATE (JULIAN)                   
GCPSDJY  DS    XL2                                                              
GCPSDJD  DS    PL2                                                              
                                                                                
GCPEDJ   DS    0XL4                PERIOD END DATE (JULIAN)                     
GCPEDJY  DS    XL2                                                              
GCPEDJD  DS    PL2                                                              
                                                                                
GCMSDJ   DS    0XL4                CURRENT MONTH START DATE (JULIAN)            
GCWSDJ   DS    0XL4                CURRENT WEEK START DATE (JULIAN)             
GCWSDJY  DS    XL2                                                              
GCWSDJD  DS    PL2                                                              
                                                                                
GCMEDJ   DS    0XL4                CURRENT MONTH END DATE (JULIAN)              
GCWEDJ   DS    0XL4                CURRENT WEEK END DATE (JULIAN)               
GCWEDJY  DS    XL2                                                              
GCWEDJD  DS    PL2                                                              
                                                                                
GCCDDJ   DS    0XL4                CURRENT DAY (JULIAN)                         
GCCDDJY  DS    XL2                                                              
GCCDDJD  DS    PL2                                                              
                                                                                
GCPSDB   DS    0XL3                PERIOD START DATE (BINARY)                   
GCPSDBY  DS    XL1                                                              
GCPSDBM  DS    XL1                                                              
GCPSDBD  DS    XL1                                                              
                                                                                
GCPEDB   DS    0XL3                PERIOD END DATE (BINARY)                     
GCPEDBY  DS    XL1                                                              
GCPEDBM  DS    XL1                                                              
GCPEDBD  DS    XL1                                                              
                                                                                
GCMSDB   DS    0XL3                CURRENT MONTH START DATE (BINARY)            
GCMSDBY  DS    XL1                                                              
GCMSDBM  DS    XL1                                                              
GCMSDBD  DS    XL1                                                              
                                                                                
GCWSDB   DS    0XL3                CURRENT WEEK START DATE (BINARY)             
GCWSDBY  DS    XL1                                                              
GCWSDBM  DS    XL1                                                              
GCWSDBD  DS    XL1                                                              
                                                                                
GCMEDB   DS    0XL3                CURRENT MONTH END DATE (BINARY)              
GCMEDBY  DS    XL1                                                              
GCMEDBM  DS    XL1                                                              
GCMEDBD  DS    XL1                                                              
                                                                                
GCWEDB   DS    0XL3                CURRENT WEEK END DATE (BINARY)               
GCWEDBY  DS    XL1                                                              
GCWEDBM  DS    XL1                                                              
GCWEDBD  DS    XL1                                                              
                                                                                
GCCDDB   DS    0XL3                CURRENT DAY (BINARY)                         
GCCDDBY  DS    XL1                                                              
GCCDDBM  DS    XL1                                                              
GCCDDBD  DS    XL1                                                              
                                                                                
GCSCNTAB DS    (GCSCNMAX)CL32      SCANNER TABLE                                
GCSCNMAX EQU   18                                                               
RLP03    CSECT                                                                  
         DROP  RC                                                               
         EJECT                                                                  
DAYMONQ  EQU   1                   MONDAY                                       
DAYTUEQ  EQU   2                   TUESDAY                                      
DAYWEDQ  EQU   3                   WEDNESDAY                                    
DAYTHUQ  EQU   4                   THURSDAY                                     
DAYFRIQ  EQU   5                   FRIDAY                                       
DAYSATQ  EQU   6                   SATURDAY                                     
DAYSUNQ  EQU   7                   SUNDAY                                       
                                                                                
DAYSINWQ EQU   7                   N'DAYS IN WEEK                               
DAYSINMQ EQU   31                  N'DAYS IN MONTH                              
                                                                                
MONJANQ  EQU   1                   JANUARY                                      
MONDECQ  EQU   12                  DECEMBER                                     
                                                                                
NOTINPDQ EQU   C'*'                DAY NOT IN PERIOD                            
CANUSEDQ EQU   C'-'                DAY AVAILABLE FOR USE                        
DINK     EQU   C''''               FOR 'YY DISPLAY                              
                                                                                
ADDDAYQ  EQU   C'D'                ADDAY DAYS PARAMETER                         
ADDMONQ  EQU   C'M'                ADDAY MONTHS PARAMETER                       
                                                                                
CONLETQ  EQU   C'C'                CONTROL SYSTEM LETTER                        
                                                                                
         LTORG                                                                  
                                                                                
DAYS31   DC    AL1(GCDCONQ,GCDTDATQ),X'00000002',X'0000'                        
DAYS1531 DC    AL1(GCDCONQ,GCDTDATQ),X'00020002',X'0000'                        
                                                                                
DAYTAB   DC    C'123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'                           
                                                                                
PONE     DC    P'1'                                                             
DAY01    DC    C'01'                                                            
DAYBITS  DC    X'8040201008040201'                                              
                                                                                
MAXPERD  DC    AL1(SOFMMNTH),AL2(MONDECQ)                                       
MAXDAYS  DC    AL2(366)                                                         
         EJECT                                                                  
CALTAB   DS    0X                  ** DISPLACEMENT TABLE **                     
                                                                                
         DC    AL2(CALHED1+COL1DISP-TWAD)                                       
         DC    AL2(CALSUB1+COL1DISP-TWAD)                                       
         DC    AL2(CALM1W1H-TWAD)                                               
         DC    AL2(CALM1W2H-TWAD)                                               
         DC    AL2(CALM1W3H-TWAD)                                               
         DC    AL2(CALM1W4H-TWAD)                                               
         DC    AL2(CALM1W5H-TWAD)                                               
         DC    AL2(CALM1W6H-TWAD)                                               
                                                                                
         DC    AL2(CALHED1+COL2DISP-TWAD)                                       
         DC    AL2(CALSUB1+COL2DISP-TWAD)                                       
         DC    AL2(CALM2W1H-TWAD)                                               
         DC    AL2(CALM2W2H-TWAD)                                               
         DC    AL2(CALM2W3H-TWAD)                                               
         DC    AL2(CALM2W4H-TWAD)                                               
         DC    AL2(CALM2W5H-TWAD)                                               
         DC    AL2(CALM2W6H-TWAD)                                               
                                                                                
         DC    AL2(CALHED1+COL3DISP-TWAD)                                       
         DC    AL2(CALSUB1+COL3DISP-TWAD)                                       
         DC    AL2(CALM3W1H-TWAD)                                               
         DC    AL2(CALM3W2H-TWAD)                                               
         DC    AL2(CALM3W3H-TWAD)                                               
         DC    AL2(CALM3W4H-TWAD)                                               
         DC    AL2(CALM3W5H-TWAD)                                               
         DC    AL2(CALM3W6H-TWAD)                                               
                                                                                
         DC    AL2(CALHED1+COL4DISP-TWAD)                                       
         DC    AL2(CALSUB1+COL4DISP-TWAD)                                       
         DC    AL2(CALM4W1H-TWAD)                                               
         DC    AL2(CALM4W2H-TWAD)                                               
         DC    AL2(CALM4W3H-TWAD)                                               
         DC    AL2(CALM4W4H-TWAD)                                               
         DC    AL2(CALM4W5H-TWAD)                                               
         DC    AL2(CALM4W6H-TWAD)                                               
                                                                                
         DC    AL2(CALHED1+COL5DISP-TWAD)                                       
         DC    AL2(CALSUB1+COL5DISP-TWAD)                                       
         DC    AL2(CALM5W1H-TWAD)                                               
         DC    AL2(CALM5W2H-TWAD)                                               
         DC    AL2(CALM5W3H-TWAD)                                               
         DC    AL2(CALM5W4H-TWAD)                                               
         DC    AL2(CALM5W5H-TWAD)                                               
         DC    AL2(CALM5W6H-TWAD)                                               
                                                                                
         DC    AL2(CALHED1+COL6DISP-TWAD)                                       
         DC    AL2(CALSUB1+COL6DISP-TWAD)                                       
         DC    AL2(CALM6W1H-TWAD)                                               
         DC    AL2(CALM6W2H-TWAD)                                               
         DC    AL2(CALM6W3H-TWAD)                                               
         DC    AL2(CALM6W4H-TWAD)                                               
         DC    AL2(CALM6W5H-TWAD)                                               
         DC    AL2(CALM6W6H-TWAD)                                               
                                                                                
         DC    AL2(CALHED2+COL1DISP-TWAD)                                       
         DC    AL2(CALSUB2+COL1DISP-TWAD)                                       
         DC    AL2(CALM7W1H-TWAD)                                               
         DC    AL2(CALM7W2H-TWAD)                                               
         DC    AL2(CALM7W3H-TWAD)                                               
         DC    AL2(CALM7W4H-TWAD)                                               
         DC    AL2(CALM7W5H-TWAD)                                               
         DC    AL2(CALM7W6H-TWAD)                                               
                                                                                
         DC    AL2(CALHED2+COL2DISP-TWAD)                                       
         DC    AL2(CALSUB2+COL2DISP-TWAD)                                       
         DC    AL2(CALM8W1H-TWAD)                                               
         DC    AL2(CALM8W2H-TWAD)                                               
         DC    AL2(CALM8W3H-TWAD)                                               
         DC    AL2(CALM8W4H-TWAD)                                               
         DC    AL2(CALM8W5H-TWAD)                                               
         DC    AL2(CALM8W6H-TWAD)                                               
                                                                                
         DC    AL2(CALHED2+COL3DISP-TWAD)                                       
         DC    AL2(CALSUB2+COL3DISP-TWAD)                                       
         DC    AL2(CALM9W1H-TWAD)                                               
         DC    AL2(CALM9W2H-TWAD)                                               
         DC    AL2(CALM9W3H-TWAD)                                               
         DC    AL2(CALM9W4H-TWAD)                                               
         DC    AL2(CALM9W5H-TWAD)                                               
         DC    AL2(CALM9W6H-TWAD)                                               
                                                                                
         DC    AL2(CALHED2+COL4DISP-TWAD)                                       
         DC    AL2(CALSUB2+COL4DISP-TWAD)                                       
         DC    AL2(CALMAW1H-TWAD)                                               
         DC    AL2(CALMAW2H-TWAD)                                               
         DC    AL2(CALMAW3H-TWAD)                                               
         DC    AL2(CALMAW4H-TWAD)                                               
         DC    AL2(CALMAW5H-TWAD)                                               
         DC    AL2(CALMAW6H-TWAD)                                               
                                                                                
         DC    AL2(CALHED2+COL5DISP-TWAD)                                       
         DC    AL2(CALSUB2+COL5DISP-TWAD)                                       
         DC    AL2(CALMBW1H-TWAD)                                               
         DC    AL2(CALMBW2H-TWAD)                                               
         DC    AL2(CALMBW3H-TWAD)                                               
         DC    AL2(CALMBW4H-TWAD)                                               
         DC    AL2(CALMBW5H-TWAD)                                               
         DC    AL2(CALMBW6H-TWAD)                                               
                                                                                
         DC    AL2(CALHED2+COL6DISP-TWAD)                                       
         DC    AL2(CALSUB2+COL6DISP-TWAD)                                       
         DC    AL2(CALMCW1H-TWAD)                                               
         DC    AL2(CALMCW2H-TWAD)                                               
         DC    AL2(CALMCW3H-TWAD)                                               
         DC    AL2(CALMCW4H-TWAD)                                               
         DC    AL2(CALMCW5H-TWAD)                                               
         DC    AL2(CALMCW6H-TWAD)                                               
                                                                                
CALTABX  DC    AL2(EOT)            END OF TABLE                                 
                                                                                
COLWIDQ  EQU   13                  COLUMN WIDTH                                 
COL1DISP EQU   00                  COLUMN 1 HEADLINE DISPLACEMENT               
COL2DISP EQU   COL1DISP+COLWIDQ    COLUMN 2 HEADLINE DISPLACEMENT               
COL3DISP EQU   COL2DISP+COLWIDQ    COLUMN 3 HEADLINE DISPLACEMENT               
COL4DISP EQU   COL3DISP+COLWIDQ    COLUMN 4 HEADLINE DISPLACEMENT               
COL5DISP EQU   COL4DISP+COLWIDQ    COLUMN 5 HEADLINE DISPLACEMENT               
COL6DISP EQU   COL5DISP+COLWIDQ    COLUMN 6 HEADLINE DISPLACEMENT               
                                                                                
CALTABD  DSECT                     ** DSECT TO COVER CALTAB **                  
CALTHEDL DS    AL2                 DISPLACEMENT TO HEADLINE                     
CALTSUBL DS    AL2                 DISPLACEMENT TO SUB-HEADLINE                 
CALTWK1H DS    AL2                 DISPLACEMENT TO WEEK 1 HEADER                
CALTWK2H DS    AL2                 DISPLACEMENT TO WEEK 2 HEADER                
CALTWK3H DS    AL2                 DISPLACEMENT TO WEEK 3 HEADER                
CALTWK4H DS    AL2                 DISPLACEMENT TO WEEK 4 HEADER                
CALTWK5H DS    AL2                 DISPLACEMENT TO WEEK 5 HEADER                
CALTWK6H DS    AL2                 DISPLACEMENT TO WEEK 6 HEADER                
CALTWKSN EQU   (*-CALTWK1H)/L'CALTWK1H                                          
CALTABL  EQU   *-CALTABD                                                        
                                                                                
WKDD     DSECT                     ** WEEK DISPLAY **                           
WKDSDAYH DS    XL8                                                              
WKDSDAY  DS    CL3                 START DAY                                    
WKDDAYSH DS    XL8                                                              
WKDDAYS  DS    XL7                 DAYS MASK                                    
WKDL     EQU   *-WKDD                                                           
         EJECT                                                                  
* GERLPWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE GERLPWORK                                                      
         PRINT ON                                                               
                                                                                
TWAD     DSECT                     ** TWAD DEFINITIONS **                       
         ORG   RLPOLY1H                                                         
       ++INCLUDE GERLPF9D                                                       
         ORG   KEYPFKH                                                          
       ++INCLUDE GERLPF8D                                                       
         ORG   KEYPFKH                                                          
       ++INCLUDE GERLPF6D                                                       
                                                                                
         ORG   OSVALS                                                           
LVBLK    DS    0X                  ** GROUP BLOCK SAVE AREA **                  
LVKEY    DS    XL(GCKEYL)          KEY                                          
LVDATA   DS    XL(GCDATAL2)        DATA VALUES                                  
LVLEDAT  DS    CL6                 LAST DISPLAYED RUN DATE                      
                                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
                                                                                
BLDD     DSECT                     ** BILL DATE DISPLAY **                      
BLDDRDAY DS    CL3                 RUN DAY                                      
         DS    C                                                                
BLDDRDAT DS    CL8                 RUN DATE                                     
BLDDRHYP DS    C                                                                
BLDDBDAY DS    CL3                 BILL DAY                                     
         DS    C                                                                
BLDDBDAT DS    CL8                 BILL DATE                                    
         DS    CL2                                                              
BLDL     EQU   *-BLDD                                                           
                                                                                
DATD     DSECT                     ** REGULAR DATE DISPLAY **                   
DATDRDAY DS    CL3                 RUN DAY                                      
         DS    C                                                                
DATDRDAT DS    CL8                 RUN DATE                                     
         DS    C                                                                
DATL     EQU   *-DATD                                                           
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013GERLP03   11/04/11'                                      
         END                                                                    
