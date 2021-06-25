*          DATA SET GERLP04    AT LEVEL 023 AS OF 02/19/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE TF2D04A                                                                  
*&&US                                                                           
*INCLUDE CLPACK                                                                 
*&&                                                                             
***********************************************************************         
* MODULE HANDLES THE FOLLOWING FUNCTIONS:-                            *         
*                                                                     *         
* GROUP/SUBMIT &  USES X'F9' SCREEN - KEY PREFIXES                    *         
* GROUP SOFTDATE  USES X'FC' SCREEN - SUB PREFIXES                    *         
*                                                                     *         
* XFILE/SUBMIT    USES X'F9' SCREEN - KEY PREFIXES                    *         
*                 USES X'FC' SCREEN - SUB PREFIXES                    *         
*                                                                     *         
***********************************************************************         
                                                                                
RLP04    TITLE '- GROUP SUBMIT'                                                 
RLP04    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RLP4**,R8,R7,RR=RE                                           
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         L     RC,AOVERWRK                                                      
         USING GSWORKD,RC          RC=A(LOCAL W/S)                              
         USING TWAD,RA             RA=A(TWA & SAVE STORAGE)                     
         USING LSTTABD,CSLSTCUR                                                 
         L     R3,ARFPIOB                                                       
         USING RFPD,R3             R3=A(RFP INTERFACE BLOCK)                    
         USING RQHD,RFPVREQH                                                    
*&&US*&& L     R0,=V(CLPACK)                                                    
*&&US*&& AR    R0,RE                                                            
*&&US*&& ST    R0,GSVCLPK                                                       
                                                                                
         SRL   RF,32-8                                                          
         LTR   RF,RF                                                            
         BNZ   SUBNTRY                                                          
         B     GRPSUB              GROUP/SUBMIT & GROUP/SOFTDATE                
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* RETURN TO POINT AFTER NTRSES                                        *         
***********************************************************************         
                                                                                
SUBNTRY  CHI   RF,SUBNTRYM                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     SUBNTRYS-L'SUBNTRYS(RF)                                          
                                                                                
SUBNTRYS DS    0XL4                                                             
                                                                                
         B     GSGRPBLD            RETURN FROM GROUP/BUILD                      
                                                                                
SUBNTRYM EQU   (*-SUBNTRY)/L'SUBNTRY                                            
         EJECT                                                                  
***********************************************************************         
* SUBMIT GROUP - BUILD LIST OF RFP DATES IF NECESSARY                 *         
***********************************************************************         
                                                                                
GRPSUB   DS    0H                                                               
                                                                                
GSKEYVAL MVC   GSAGY,TWAAGY        SET CONNECTED USER VARIABLES                 
         MVC   GSUSER,TWAUSRID                                                  
         MVC   GSSYST,ASSYSL                                                    
         TM    CSINDSL1,CSIUSELC   TEST NESTED (LIST) CALL                      
         BNZ   *+14                                                             
         CLC   LSTTRTYP,CSREC      OR WE HAVE A RECORD ALREADY                  
         BNE   GSKEYV02                                                         
         XC    TWALRA,TWALRA                                                    
         MVC   GSAGY,GRPLAGY       SET LIST ENTRY VARIABLES                     
         MVC   GSUSER,GRPLUSER                                                  
         MVC   GSSYST,GRPLSYST                                                  
                                                                                
GSKEYV02 TM    CSINDSL1,CSIKEYSL   TEST KEY SCREEN LOADED                       
         BNZ   GSKEYV08                                                         
         GOTOR AOVRSCR,PCPARM,('GRPKEYSQ',RLPOLY1H)                             
         BNE   EXIT                                                             
         OI    CSINDSL1,CSIKEYSL   SET KEY SCREEN LOADED                        
                                                                                
         CLI   ASSYSL,CONLETQ      IF CONTROL SYSTEM                            
         BNE   *+8                 UNPROTECT THE SYSTEM FIELD                   
         NI    KEYSYSH+(FVATRB-FVIHDR),FF-(FVAPROT+FVALOWC)                     
                                                                                
         CLI   CSACT,ACTSOF        TEST SETTING SOFT DATES                      
         BNE   GSKEYV04                                                         
         TM    CSINDSL1,CSIUSELC   TEST NESTED LIST CALL                        
         BZ    *+12                                                             
         OI    KEYGRPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    KEYSYSH+(FVATRB-FVIHDR),FVAPROT+FVALOWC                          
                                                                                
         OI    KEYFRQH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    KEYSCHDH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    KEYEFDTH+(FVATRB-FVIHDR),FVAPROT                                 
         XC    KEYEFDN,KEYEFDN                                                  
         XC    KEYFRQN,KEYFRQN                                                  
         XC    KEYSCHN,KEYSCHN                                                  
         B     GSKEYV06                                                         
                                                                                
GSKEYV04 TM    CSINDSL1,CSIUSELC                                                
         BZ    GSKEYV06                                                         
         OI    KEYGRPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    KEYSYSH+(FVATRB-FVIHDR),FVAPROT+FVALOWC                          
         OI    KEYFRQH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    KEYSCHDH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    KEYEFDTH+(FVATRB-FVIHDR),FVAPROT                                 
                                                                                
GSKEYV06 CLI   CSREC,RECXFG        TEST XFILE RECORD                            
         BNE   GSKEYV08                                                         
         OI    KEYSYSH+(FVATRB-FVIHDR),FVAPROT+FVALOWC                          
         XC    KEYSYSN,KEYSYSN     YES - CLEAR SYSTEM TAG FIELD                 
                                                                                
GSKEYV08 MVC   GSGRP,GRPLGRP       SET (NESTED) GROUP CODE                      
         TM    CSINDSL1,CSIUSELC   TEST NESTED (LIST) CALL                      
         BNZ   GSKEYV14                                                         
         CLC   LSTTRTYP,CSREC                                                   
         BNE   GSKEYV10                                                         
         TM    KEYGRPH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   GSKEYV12                                                         
         TM    KEYSYSH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   GSKEYV12                                                         
         MVC   KEYGRP,GRPLGRP                                                   
         OI    KEYGRPH+(FVOIND-FVIHDR),FVOXMT                                   
         CLI   CSREC,RECXFG                                                     
         BE    GSKEYV12                                                         
         GOTOR AGETSYS,GRPLSYST                                                 
         MVC   KEYSYS,PCWORK                                                    
         OI    KEYSYSH+(FVOIND-FVIHDR),FVOXMT                                   
         B     GSKEYV12                                                         
                                                                                
GSKEYV10 TM    CSINDSL1,CSIK1REQ   TEST USER INVITED TO ENTER KEY               
         BNZ   GSKEYV12                                                         
         LA    R0,KEYGRPH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EKEY)                                            
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIK1REQ   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
                                                                                
GSKEYV12 GOTOR AFVAL,KEYGRPH       VALIDATE GROUP CODE                          
         BNE   EXIT                                                             
         MVC   GSGRP,FVIFLD        SET GROUP CODE FROM INPUT FIELD              
                                                                                
GSKEYV14 CLC   KEYGRP,GSGRP        TEST NEED TO OUTPUT GROUP CODE               
         BE    *+14                                                             
         MVC   KEYGRP,GSGRP                                                     
         OI    KEYGRPH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
         CLI   CSREC,RECGRP        TEST RFP GROUP RECORD                        
         BNE   GSKEYV18                                                         
         TM    CSINDSL1,CSIUSELC                                                
         BNZ   GSKEYV16                                                         
         MVC   GSSYST,ASSYSL                                                    
         TM    KEYSYSH+(FVATRB-FVIHDR),FVAPROT                                  
         BNZ   GSKEYV16                                                         
         GOTOR AVALSYS,KEYSYSH     VALIDATE SYSTEM FIELD                        
         BNE   EXIT                                                             
         MVC   GSSYST,PCWORK                                                    
                                                                                
GSKEYV16 LA    R0,KEYGRPH                                                       
         ST    R0,FVADDR                                                        
         MVC   CUUSER,GSUSER                                                    
         MVC   CUAALF,GSAGY                                                     
         MVC   CUSYSL,GSSYST                                                    
         LA    R1,GSGRP                                                         
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFPIO TO VALIDATE GROUP                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     EXIT                                                             
         GOTOR ANTRSES,=AL1(SELTICLR,RECGRP,ACTBLD,1,0,0,0)                     
                                                                                
GSGRPBLD MVC   GSGRP,GRPLGRP                                                    
         XC    RFPFRQID,RFPFRQID                                                
         XC    RFPFSORT,RFPFSORT                                                
         XC    RFPFSEQN,RFPFSEQN                                                
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPFLAGS,RFPXSYMS                                                
         MVI   RFPMODE,RFPRETRQ                                                 
         GOTOR VRFPIO,PCPARM,RFPD                                               
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     EXIT                                                             
         CLI   RFPRET,RFPEOF                                                    
         BNE   GSKEYV20                                                         
         MVC   FVMSGNO,=AL2(GE$NRITG)                                           
         B     EXIT                                                             
                                                                                
GSKEYV18 CLI   CSREC,RECXFG        TEST XFILE GROUP RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         USING XFILED,R2           VALIDATE XFILE GROUPS                        
         XC    XFKEY,XFKEY         BUILD XFILE RECORD KEY                       
         MVI   XFKSYS,XFKSYSQ                                                   
         MVI   XFKSTYP,XFKSTYPQ                                                 
         MVC   XFAGY,GSAGY                                                      
         MVC   XFUSER,GSUSER                                                    
         MVC   XFGRP,GSGRP                                                      
         GOTOR AIO,'IORDUP+IOGENDIS+IO1'                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     EXIT                                                             
         MVC   GSXFGDA,XFKDA       SAVE XFILE RECORD DISK ADDRESS               
         GOTOR AIO,'IOGETRUP+IOGENFIS+IO1'                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AXFGRFP             MOVE XFILE VALUES TO RFP BLOCK               
                                                                                
GSKEYV20 CLC   GSGRP,GSLGRP        TEST CHANGE OF GROUP CODE                    
         BE    *+14                                                             
         MVC   GSLGRP,GSGRP                                                     
         NI    CSINDSL1,CSIUSELC+CSIKEYSL+CSIK1REQ                              
                                                                                
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   GSKEYV22                                                         
         GOTOR AGETSYS,RFPVSYST    YES - SHOW SYSTEM                            
         MVC   KEYSYS,PCWORK                                                    
         OI    KEYSYSH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
GSKEYV22 TM    CSINDSL1,CSIK2REQ   TEST DATE REQUESTED                          
         BNZ   GSKEYV34                                                         
         CLI   CSACT,ACTSOF        TEST SETTING SOFT DATES                      
         BE    GSKEYV26                                                         
                                                                                
         OC    RFPVENDR,RFPVENDR   TEST END RUN DATE SET FOR GROUP              
         BZ    GSKEYV30                                                         
         LHI   RE,PCMEFFDT-WORKD                                                
         LA    RE,WORKD(RE)                                                     
         CLC   KEYEFDN(L'PCMEFFDT),0(RE)                                        
         BE    GSKEYV24                                                         
         XC    KEYEFDN,KEYEFDN                                                  
         MVC   KEYEFDN(L'PCMEFFDT),0(RE)                                        
         OI    KEYEFDNH+(FVOIND-FVIHDR),FVOXMT                                  
GSKEYV24 OI    KEYEFDTH+(FVATRB-FVIHDR),FVAPROT                                 
                                                                                
         MVC   PCWORK(L'RFPVNXTR),RFPVNXTR                                      
         MVC   PCWORK+L'RFPVNXTR(L'RFPVENDR),RFPVENDR                           
         LHI   R0,SOFOTPRT                                                      
         TM    KEYEFDTH+(FVATRB-FVIHDR),FVAPROT                                 
         BZ    *+8                                                              
         LHI   R0,SOFOTPRT+SOFOTMIX                                             
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ((R0),KEYEFDTH),0                                                
         CLC   RFPVENDR,ASJDAT                                                  
         BNL   GSKEYV26                                                         
         MVC   FVMSGNO,=AL2(GE$DCBBT)                                           
         B     EXIT                                                             
                                                                                
GSKEYV26 GOTOR AOVRSCR,PCPARM,('GRPSUBSQ',KEYPFKH)                              
         BNE   EXIT                                                             
         CLI   CSACT,ACTSOF                                                     
         BNE   GSKEYV28                                                         
         XC    SUBLRUL,SUBLRUL                                                  
         XC    SUBNRUL,SUBNRUL                                                  
         OI    CSINDSL1,CSIK2REQ   SET DATE SCREEN LOADED                       
         B     GSKEYV34                                                         
                                                                                
GSKEYV28 NI    SUBNRUNH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         LA    R0,SUBNRUNH                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$ERUND)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIK2REQ   SET DATE REQUESTED                           
                                                                                
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',RFPVLSTR),                   *        
               ('SOFOTPRT+SOFOTMIX',SUBLRUNH),('SOFIIONE',0)                    
                                                                                
         GOTOR AGETNXT             TEST NEXT RUN DATE SET FOR GROUP             
         BNE   EXIT                                                             
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT',SUBNRUNH),('SOFIIONE',0)                             
         B     GSKEYV34                                                         
                                                                                
GSKEYV30 LHI   RE,PCMNRUND-WORKD                                                
         LA    RE,WORKD(RE)                                                     
         CLC   KEYEFDN(L'PCMNRUND),0(RE)                                        
         BE    GSKEYV32                                                         
         XC    KEYEFDN,KEYEFDN                                                  
         MVC   KEYEFDN(L'PCMNRUND),0(RE)                                        
         OI    KEYEFDNH+(FVOIND-FVIHDR),FVOXMT                                  
GSKEYV32 NI    KEYEFDTH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         XC    KEYFRQN,KEYFRQN                                                  
         OI    KEYFRQNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    KEYSCHN,KEYSCHN                                                  
         OI    KEYSCHNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R0,KEYEFDTH                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$ERUND)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIK2REQ   TEST DATE REQUESTED                          
         OC    RFPVNXTR,RFPVNXTR                                                
         BZ    EXIT                                                             
         LHI   R0,SOFOTPRT                                                      
         TM    KEYEFDTH+(FVATRB-FVIHDR),FVAPROT                                 
         BZ    *+8                                                              
         LHI   R0,SOFOTPRT+SOFOTMIX                                             
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',RFPVNXTR),                   *        
               ((R0),KEYEFDTH),('SOFIIONE',0)                                   
                                                                                
GSKEYV34 CLI   CSACT,ACTSOF        TEST SETTING SOFT DATES                      
         BE    GSKEYV38                                                         
         LA    R0,KEYEFDTH         VALIDATE NEXT RUN DATE                       
         TM    KEYEFDTH+(FVATRB-FVIHDR),FVAPROT                                 
         BZ    GSKEYV36                                                         
         LA    R0,SUBNRUNH                                                      
         TM    SUBNRUNH+(FVATRB-FVIHDR),FVAPROT                                 
         BZ    GSKEYV36                                                         
         DC    H'0'                                                             
                                                                                
GSKEYV36 GOTOR AGOSOFT,PCPARM,('SOFITYMD',(R0)),                       *        
               ('SOFOTSD8',GSNRUN),('FF-SOFIIF1O',0)                            
         BNE   EXIT                                                             
         CLC   GSNRUN,ASJDAT       TEST NEXT RUN DATE NOT BEFORE TODAY          
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$DCBBT)                                           
         B     EXIT                                                             
                                                                                
         MVC   CUSYSL,RFPVSYST                                                  
         LA    R1,GSNRUN                                                        
         ICM   R1,8,=X'80'         SET MANUAL SUBMISSION VALIDATION             
         GOTOR AGETWRK             TEST NEXT RUN DATE IS A WORKING DAY          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$SDNWE)                                           
         B     EXIT                                                             
                                                                                
         OC    RFPVENDR,RFPVENDR   TEST EFFECTIVE END DATE PRESENT              
         BZ    GSKEYV38                                                         
         CLC   GSNRUN,RFPVENDR     TEST WITHIN EFFECTIVE PERIOD                 
         BNH   GSKEYV38                                                         
         MVC   FVMSGNO,=AL2(GE$IDATE)                                           
         B     EXIT                                                             
                                                                                
GSKEYV38 TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BNZ   GSSELECT                                                         
                                                                                
         LA    R0,GSSAVE           CLEAR SAVED VALUES                           
         LHI   R1,GSSAVEL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   GSKEYV40                                                         
         GOTOR BLDDAT              BUILD DATE LIST FOR GROUP                    
         B     GSKEYV44                                                         
                                                                                
GSKEYV40 CLI   CSREC,RECXFG        TEST XFILE RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1             XFILE - BUILD DATES FOR ALL GROUPS           
         LA    R2,XFFRSTEL                                                      
         USING XFSGRPD,R2                                                       
         SR    R0,R0                                                            
GSKEYV42 IC    R0,XFSGLN           BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         CLI   XFSGCD,0            TEST END OF RECORD                           
         BE    GSKEYV44                                                         
         CLI   XFSGCD,XFSGCDEQ     TEST SYSTEM/GROUP ELEMENT                    
         BNE   GSKEYV42                                                         
                                                                                
         MVC   CUUSER,GSUSER                                                    
         MVC   CUAALF,GSAGY                                                     
         GOTOR AGETSYS,XFSGSYS     CONVERT SYSTEM NUMBER TO LETTER              
         MVC   CUSYSL,PCWORK+L'SYSLNAME                                         
         LA    R1,XFSGGRP                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFP TO VALIDATE THE GROUP               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     EXIT                                                             
*&&UK                                                                           
         CLC   RFPXFILE,GSGRP      ENSURE THIS GROUP POINTS TO XFILE            
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         GOTOR BLDDAT              BUILD DATE LIST                              
         B     GSKEYV42                                                         
                                                                                
GSKEYV44 CLI   TWASCRN,GRPSUBSQ    TEST LIST SCREEN LOADED                      
         BE    GSKEYV46                                                         
         GOTOR AOVRSCR,PCPARM,('GRPSUBSQ',KEYPFKH)                              
         BNE   EXIT                                                             
                                                                                
GSKEYV46 CLI   CSACT,ACTSOF        SET SETTING SOFT DATES                       
         BE    GSKEYV48                                                         
         XC    SUBLRUN,SUBLRUN     DISPLAY LAST RUN DATE                        
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',RFPVLSTR),                   *        
               ('SOFOTPRT+SOFOTMIX',SUBLRUNH),('SOFIIONE',0)                    
                                                                                
GSKEYV48 CLI   GSDATNUM,0          TEST ANY RFP SYMBOLS TO BE RESOLVED          
         BE    GSKEYV60            YES                                          
                                                                                
         MVC   CSHIRECN,CSPSRECN                                                
         SR    R0,R0                                                            
         ICM   R0,1,GSDATNUM       R0=N'ENTRIES IN DATE TABLE                   
         LA    R2,GSDATTAB         R2=A(DATE TABLE)                             
         USING DATTABD,R2                                                       
GSKEYV50 XC    LSTTABD(LSTTABL),LSTTABD                                         
         SR    RE,RE                                                            
         ICM   RE,3,CSHIRECN                                                    
         AHI   RE,1                                                             
         STCM  RE,3,LSTTRECN                                                    
         MVI   LSTTRTYP,RECSUB     (NOT A GLOBAL RECORD TYPE)                   
         MVC   SUBLSYST,DATTSYST                                                
         MVC   SUBLSYME,DATTSYME                                                
         MVC   SUBLVRUL,DATTVRUL                                                
         MVC   SUBLPVAL,DATTPVAL                                                
         MVC   SUBLCVAL,DATTCVAL                                                
         GOTOR AGETSYS,SUBLSYST                                                 
         MVC   SUBLSYSN,PCWORK+L'SYSLNAME+L'SYSLUSLT                            
                                                                                
         LA    RF,DVLTAB           LOOK UP DATE TYPE IN TABLE                   
         USING DVLTABD,RF                                                       
GSKEYV52 CLI   DVLTABD,DVLTEOTQ    TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DVLTTYPE,SUBLVRUL   MATCH ON RULE NUMBER                         
         BNE   GSKEYV54                                                         
         CLI   DVLTSYSN,DVLTSDQ    TEST DEFAULT (ALL SYSTEM) ENTRY              
         BE    GSKEYV56                                                         
         CLC   DVLTSYSN,SUBLSYSN   NO - MATCH SYSTEM TO TABLE                   
         BE    GSKEYV56                                                         
GSKEYV54 AHI   RF,DVLTABL          BUMP TO NEXT TABLE ENTRY                     
         B     GSKEYV52                                                         
                                                                                
GSKEYV56 MVC   SUBLINDS,DVLTINDS   SET VALIDATION/DISPLAY VALUES                
         TM    SUBLINDS,DVLTIPVL   TEST POST-VALIDATION REQUIRED                
         BZ    *+8                                                              
         OI    GSDATIND,GSDATIPV   YES - SET GLOBAL POST-VALIDATION BIT         
         TM    SUBLINDS,DVLTISVL   TEST SPECIAL VALIDATION REQUIRED             
         BZ    *+8                                                              
         OI    GSDATIND,GSDATISV   YES - SET SPECIAL VALIDATION BIT             
         MVC   SUBLSOFT,DVLTSOFT                                                
         MVC   SUBLSOFI,DVLTSOFI                                                
         MVC   SUBLSOFO,DVLTSOFO                                                
         MVC   SUBLPVRN,DVLTPVRN                                                
         DROP  RF                                                               
                                                                                
         LA    R1,PCPARM                                                        
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNS      ISSUE SINGLE UPPER TRANSLATE                 
         MVI   DDRETN,DDCASEU                                                   
         MVC   DDSYS,SUBLSYSN                                                   
         MVC   DDLANG,CULANG                                                    
         MVC   SUBLSYMB(L'SUBLSYME),SUBLSYME                                    
         MVI   SUBLSYMB+L'SUBLSYME-1,L'SUBLSYMB                                 
         LA    RF,SUBLSYMB                                                      
         STCM  RF,7,DDIADR                                                      
         STCM  RF,7,DDOADR                                                      
         GOTOR VDICTAT             RESOLVE RFP SYMBOL                           
                                                                                
         GOTOR ATSARIO,TSAADD      ADD LIST ENTRY TO TSAR                       
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,GSLSTNUM                                                    
         AHI   RE,1                                                             
         CLI   DATTVRUL,RRULBDAT   TREAT 'BILLING DATE' LIKE 'TODAY'            
         BE    *+8                                                              
         CLI   DATTVRUL,RRULTODY                                                
         BE    *+8                                                              
         STCM  RE,3,GSLSTNUM                                                    
         AHI   R2,DATTABL          BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,GSKEYV50                                                      
                                                                                
         OC    GSLSTNUM,GSLSTNUM   TEST ANY RFP SYMBOLS TO RESOLVE              
         BZ    GSKEYV60                                                         
         CLI   CSACT,ACTSOF        TEST SETTING SOFT DATES                      
         BNE   GSKEYV58                                                         
         TM    GSDATIND,GSDATIPV   TEST ANY POST VALIDATION REQUIRED            
         BZ    GSKEYV58                                                         
         LA    R0,KEYGRPH          YES - ERROR                                  
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GE$CSSOF)                                           
         B     EXIT                                                             
                                                                                
GSKEYV58 OI    CSLTINDS,CSLTIFST                                                
         OI    CSINDSL1,CSIRDSPC   SET RECORD DISPLAYED FOR CHANGE              
         B     GSSCROLL                                                         
                                                                                
GSKEYV60 CLI   CSACT,ACTSUB        TEST SUBMIT ACTION                           
         BE    GSSUBMIT            YES -  JUST SUBMIT THE GROUP                 
         LA    R0,KEYGRPH          ERROR IF SETTING SOFT DATES                  
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE LIST SUB-ACTIONS                                             *         
***********************************************************************         
                                                                                
GSSELECT NI    CSLTINDS,FF-(CSLTIHLD+CSLTIANY)                                  
         MVC   LSTTRECN,CSPAG#LO                                                
         SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LIST ENTRIES                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R6,SUBLIN1H                                                      
         USING LSTD,R6                                                          
                                                                                
GSSELE02 TM    LSTLINPH+(FVATRB-FVIHDR),FVAPROT                                 
         BNZ   GSSELE04                                                         
         GOTOR ATSARIO,TSAGET      GET SUBMIT LIST RECORD                       
         GOTOR VALDAT,LSTLINPH     VALIDATE THE DATE                            
         BH    EXIT                FIELD IN ERROR                               
         GOTOR ATSARIO,TSAPUT      PUT SUBMIT LIST RECORD                       
                                                                                
GSSELE04 OI    CSLTINDS,CSLTIANY   SET SELECTION THIS PAGE                      
         LA    R6,LSTNEXT          BUMP TO NEXT ACTION FIELD                    
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN       BUMP TO NEXT RECORD NUMBER                   
         AHI   R1,1                                                             
         STCM  R1,3,LSTTRECN                                                    
         BCT   R0,GSSELE02         DO FOR NUMBER OF ENTRIES ON SCREEN           
                                                                                
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSHIRECN   TEST THIS IS LAST RECORD                     
         BNE   GSSELE06                                                         
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         DROP  R6                                                               
                                                                                
GSSELE06 TM    CSLTINDS,CSLTIANY   TEST ANY SELECTIONS THIS PAGE                
         BZ    GSSCROLL                                                         
         TM    CSINDSL2,CSIDPFQY   TEST UPDATE KEY ENABLED                      
         BNZ   GSSUBMIT                                                         
                                                                                
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         LA    R0,SUBINP1H                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$ACENR)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* HANDLE SCROLLING                                                    *         
***********************************************************************         
                                                                                
GSSCROLL TM    CSINDSL2,CSIDPFQY   TEST UPDATE KEY ENABLED                      
         BZ    *+12                                                             
         CLI   PCPFKEY,PFKUPDTQ    YES - TEST UPDATE KEY ENTERED                
         BE    GSSUBMIT                                                         
                                                                                
         MVC   GSPAG#LO,CSPAG#LO   SAVE LAST LOW & HIGH RECORDS                 
         MVC   GSPAG#HI,CSPAG#HI                                                
         XC    CSPAG#LO,CSPAG#LO   CLEAR LOW & HIGH VALUES FOR PAGE             
         XC    CSPAG#HI,CSPAG#HI                                                
         TM    CSLTINDS,CSLTIFST   TEST FIRST FOR LIST                          
         BZ    GSSCRO02                                                         
         MVI   CSLTINDS,0          CLEAR LIST INDICATORS                        
         MVI   CSLSTNUM,0          CLEAR NUMBER OF LIST ENTRIES                 
         B     GSSCRO10                                                         
                                                                                
GSSCRO02 MVI   GSSCRNUM,PFKIPAGE   SET TO SCROLL A PAGE                         
         TM    CSLTINDS,CSLTIHLD   TEST HOLD PAGE IF DISPLAY CHANGED            
         BZ    GSSCRO04                                                         
         MVC   CSPAG#LO,GSPAG#LO   RESTORE LAST LOW & HIGH RECORDS              
         MVC   CSPAG#HI,GSPAG#HI                                                
         B     GSDISPLY                                                         
                                                                                
GSSCRO04 MVC   GSSCRNUM,PCSCRNUM                                                
                                                                                
         TM    PCSCROLL,PFKIUPDN   TEST SCROLL UP OR DOWN                       
         BZ    GSSCRO08                                                         
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,CSPSRECN                                                    
         TM    GSSCRNUM,PFKIMAXN   TEST SCROLL FIRST                            
         BNZ   GSSCRO06                                                         
         LA    RF,GSLSTMAX         SCROLL UP (BACKWARDS)                        
         TM    GSSCRNUM,PFKIHALF                                                
         BZ    *+8                                                              
         SRL   RF,1                                                             
         TM    GSSCRNUM,X'0F'                                                   
         BZ    *+8                                                              
         IC    RF,GSSCRNUM                                                      
         AHI   RF,1                                                             
         SR    RE,RE                                                            
         ICM   RE,3,GSPAG#LO                                                    
         SR    RE,RF               BACK-UP TO RECORD NUMBER-1                   
         BM    *+12                                                             
         CLM   RE,3,CSPSRECN       TEST NOT < LOW RECORD FOR SESSION            
         BNL   GSSCRO06                                                         
         SR    RE,RE               SET TO START FROM LOW RECORD                 
         ICM   RE,3,CSPSRECN                                                    
                                                                                
GSSCRO06 STCM  RE,3,LSTTRECN                                                    
         MVI   CSLSTNUM,0                                                       
         B     GSSCRO12                                                         
                                                                                
GSSCRO08 SR    R1,R1                                                            
         ICM   R1,1,CSLSTNUM       PICK UP NUMBER OF ENTRIES IN PAGE            
         BZ    GRPSUBX                                                          
         MVI   CSLSTNUM,0                                                       
         TM    GSSCRNUM,X'0F'      TEST SCROLL AMOUNT SPECIFIED                 
         BZ    *+16                                                             
         CLM   R1,1,GSSCRNUM       TEST SCROLL EXCEEDS ACTUAL AMOUNT            
         BL    GSSCRO10                                                         
         IC    R1,GSSCRNUM                                                      
         TM    GSSCRNUM,PFKIHALF   TEST HALF PAGE SCROLL                        
         BZ    *+8                                                              
         SRL   R1,1                                                             
         AHI   R1,-1                                                            
         BM    GSSCRO10                                                         
         SR    R0,R0                                                            
         ICM   R0,3,GSPAG#LO                                                    
         AR    R1,R0                                                            
         STCM  R1,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         BL    GSSCRO12                                                         
                                                                                
GSSCRO10 MVC   LSTTRECN,CSPSRECN   SET TO DISPLAY FIRST PAGE                    
                                                                                
GSSCRO12 SR    RE,RE               BUMP TO NEXT RECORD                          
         ICM   RE,3,LSTTRECN                                                    
         AHI   RE,1                                                             
         STCM  RE,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSHIRECN   TEST IN TSAR BUFFER                          
         BH    GSDISPLY            NO - GET NEXT RECORD                         
         SR    RF,RF                                                            
         ICM   RF,1,CSLSTNUM                                                    
         AHI   RF,1                                                             
         CHI   RF,GSLSTMAX                                                      
         BH    GSDISPLY                                                         
         STCM  RF,1,CSLSTNUM                                                    
         OC    CSPAG#LO,CSPAG#LO                                                
         BNZ   *+10                                                             
         MVC   CSPAG#LO,LSTTRECN                                                
         MVC   CSPAG#HI,LSTTRECN                                                
         B     GSSCRO12                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY PAGE OF DATA                                                *         
***********************************************************************         
                                                                                
GSDISPLY LA    R6,SUBLIN1H                                                      
         USING LSTD,R6             R6=A(SCREEN LINE)                            
         LHI   R0,GSLSTMAX         R0=N'ENTRIES ON SCREEN                       
GSDISP02 XC    LSTLIN,LSTLIN                                                    
         OI    LSTLINH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    LSTLINP,LSTLINP                                                  
         OI    LSTLINPH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R6,LSTNEXT                                                       
         BCT   R0,GSDISP02                                                      
         DROP  R6                                                               
                                                                                
GSDISP04 SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LINES TO DISPLAY                
         BNZ   *+6                                                              
         DC    H'0'                DIE FOR NOW IF NO DISPLAY LINES              
         MVC   LSTTRECN,CSPAG#LO                                                
         LA    R6,SUBLIN1H                                                      
         USING LSTD,R6             R6=A(SCREEN LINE)                            
                                                                                
GSDISP06 GOTOR ATSARIO,TSAGET      GET SUBMIT LIST ENTRY                        
         GOTOR BLDLIN,LSTD                                                      
         MVI   LSTLINPH+(FVILEN-FVIHDR),0                                       
         LA    R6,LSTNEXT                                                       
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN                                                    
         AHI   R1,1                                                             
         STCM  R1,3,LSTTRECN                                                    
         BCT   R0,GSDISP06                                                      
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
         DROP  R6                                                               
                                                                                
GRPSUBX  LA    R0,SUBINP1H                                                      
         ST    R0,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$LSTDI)                                           
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         BNE   EXIT                                                             
         MVC   FVMSGNO,=AL2(GI$ELICE)                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SUBMIT THE GROUP                                                    *         
***********************************************************************         
                                                                                
GSSUBMIT TM    GSDATIND,GSDATIPV+GSDATISV                                       
         BZ    GSSUBM02                                                         
         GOTOR PSTVAL              DO DATE POST/EXTRA VALIDATION                
         BNE   EXIT                                                             
                                                                                
GSSUBM02 CLI   CSREC,RECGRP        TEST RFP GROUP RECORD                        
         BNE   GSSUBM12                                                         
         CLI   GSDATNUM,0          TEST ANY RFP SYMBOLS TO BE RESOLVED          
         BE    GSSUBM10            NO - JUST SUBMIT THE GROUP                   
         MVC   LSTTRECN,CSPSRECN   SET FIRST LIST RECORD NUMBER                 
                                                                                
GSSUBM04 ICM   RE,3,LSTTRECN       RE=CURRENT LIST RECORD NUMBER                
         AHI   RE,1                                                             
         CLM   RE,3,CSHIRECN       TEST ALL RECORDS PROCESSED                   
         BH    GSSUBM10                                                         
         STCM  RE,3,LSTTRECN       NO - SET NEXT RECORD NUMBER                  
         GOTOR ATSARIO,TSAGET      GET FIRST/NEXT LIST RECORD                   
                                                                                
         SR    RE,RE                                                            
         IC    RE,RFPVNUMS         RE=N'SYMBOLS FOR GROUP                       
         LA    RF,RFPD                                                          
         AHI   RF,RFPVSYMX-RFPD    RF=A(SYMBOL LIST)                            
SYM      USING RFPVSYME,RF                                                      
                                                                                
GSSUBM06 CLC   SUBLSYME,SYM.RFPVSYME  MATCH LIST ENTRY TO TABLE                 
         BE    GSSUBM08                                                         
         AHI   RF,RFPVSYML         NO MATCH - BUMP TO NEXT TABLE ENTRY          
         BCT   RE,GSSUBM06                                                      
         DC    H'0'                DIE IF CAN'T MATCH SYMBOL                    
                                                                                
GSSUBM08 MVC   SYM.RFPVPVAL,SUBLPVAL   SET PREVIOUS VALUE                       
         MVC   SYM.RFPVCVAL,SUBLCVAL   SET CURRENT VALUE                        
         B     GSSUBM04            AND PROCESS IT                               
         DROP  SYM                                                              
                                                                                
GSSUBM10 CLI   CSACT,ACTSOF        TEST SETTING SOFT DATES                      
         BE    *+8                                                              
         GOTOR UPDRNG              UPDATE THE RANGE MASK/SET RFPVNXTR           
         MVI   RFPFLAGS,RFPXSYMS                                                
         MVI   RFPMODE,RFPSAVGP    SAVE THE GROUP                               
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   DIE IF ANY ERRORS                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     GSSUBM26                                                         
                                                                                
GSSUBM12 L     R2,AIO1             R2=A(XFILE RECORD)                           
         USING XFILED,R2                                                        
                                                                                
         MVC   IODAOVER,GSXFGDA    READ XFILE FILE & DIRECTORY RECORDS          
         GOTOR AIO,'IOGETRUP+IOGENFIS+IO1'                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   IOKEY(L'XFKEY),XFKEY                                             
         GOTOR AIO,'IORDUP+IOGENDIS+IO1'                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,XFFRSTEL         PROCESS SYSTEM ELEMENTS                      
         USING XFSGRPD,R2                                                       
         SR    R0,R0                                                            
GSSUBM14 IC    R0,XFSGLN           BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         CLI   XFSGCD,0            TEST END OF RECORD                           
         BE    GSSUBM24                                                         
         CLI   XFSGCD,XFSGCDEQ     TEST SYSTEM/GROUP ELEMENT                    
         BNE   GSSUBM14                                                         
                                                                                
         MVC   CUUSER,GSUSER                                                    
         MVC   CUAALF,GSAGY                                                     
         GOTOR AGETSYS,XFSGSYS     CONVERT SYSTEM NUMBER TO LETTER              
         MVC   CUSYSL,PCWORK+L'SYSLNAME                                         
         LA    R1,XFSGGRP                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFP TO VALIDATE THE GROUP               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     EXIT                                                             
                                                                                
         CLI   GSDATNUM,0          TEST ANY RFP SYMBOLS TO BE RESOLVED          
         BE    GSSUBM22            NO - JUST SUBMIT THE GROUP                   
         MVC   LSTTRECN,CSPSRECN   SET FIRST LIST RECORD NUMBER                 
                                                                                
GSSUBM16 SR    RE,RE                                                            
         ICM   RE,3,LSTTRECN       RE=CURRENT LIST RECORD NUMBER                
         AHI   RE,1                                                             
         CLM   RE,3,CSHIRECN       TEST ALL RECORDS PROCESSED                   
         BH    GSSUBM22                                                         
         STCM  RE,3,LSTTRECN       NO - SET NEXT RECORD NUMBER                  
         GOTOR ATSARIO,TSAGET      GET FIRST/NEXT LIST RECORD                   
         CLC   SUBLSYST,RFPVSYST   MATCH ON CURRENT SYSTEM                      
         BNE   GSSUBM16                                                         
                                                                                
         SR    RE,RE                                                            
         ICM   RE,1,RFPVNUMS       RE=N'SYMBOLS FOR GROUP                       
         BZ    GSSUBM22                                                         
         LA    RF,RFPD                                                          
         AHI   RF,RFPVSYMX-RFPD    RF=A(SYMBOL LIST)                            
SYM      USING RFPVSYME,RF                                                      
                                                                                
GSSUBM18 CLC   SUBLSYME,SYM.RFPVSYME  MATCH LIST ENTRY TO TABLE                 
         BE    GSSUBM20                                                         
         AHI   RF,RFPVSYML         NO MATCH - BUMP TO NEXT TABLE ENTRY          
         BCT   RE,GSSUBM18                                                      
         DC    H'0'                DIE IF CAN'T MATCH SYMBOL                    
                                                                                
GSSUBM20 MVC   SYM.RFPVPVAL,SUBLPVAL   SET PREVIOUS VALUE                       
         MVC   SYM.RFPVCVAL,SUBLCVAL   SET CURRENT VALUE                        
         B     GSSUBM16            AND PROCESS IT                               
         DROP  SYM                                                              
                                                                                
GSSUBM22 GOTOR UPDRNG              UPDATE THE RANGE MASK/SET RFPVNXTR           
         MVI   RFPFLAGS,RFPXSYMS                                                
         MVI   RFPMODE,RFPSAVGP    SAVE THE GROUP                               
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   DIE IF ANY ERRORS                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     GSSUBM14                                                         
                                                                                
GSSUBM24 GOTOR ARFPXFG             MOVE RFP VALUES TO XFILE RECORD              
                                                                                
         GOTOR AIO,'IOPUT+IOGENFIS+IO1'                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,'IOWRITE+IOGENDIS+IO1'                                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GSSUBM26 CLI   CSACT,ACTSOF                                                     
         BE    GSSUBM28                                                         
         LA    R0,KEYEFDTH         OUTPUT 'GROUP SUBMITTED' MESSAGE             
         TM    KEYEFDTH+(FVATRB-FVIHDR),FVAPROT                                 
         BZ    *+8                                                              
         LA    R0,SUBNRUNH                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$GSENR)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
                                                                                
GSSUBM28 LA    R0,KEYGRPH          OUTPUT 'SOFT DATES SET' MESSAGE              
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$SDSUC)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A DISPLAY LINE                                     *         
***********************************************************************         
                                                                                
         USING LSTD,R6                                                          
BLDLIN   NTR1  ,                                                                
                                                                                
         GOTOR AGETSYS,SUBLSYST                                                 
         MVC   LSTLSYSN,PCWORK     DISPLAY SYSTEM NAME                          
         MVC   LSTLSYMB,SUBLSYMB   DISPLAY SYMBOL NAME                          
                                                                                
         TM    SUBLINDS,DVLTITOD   TEST TODAY/ADJUSTED BILLING DATES            
         BNZ   BLDLIN12                                                         
                                                                                
         TM    SUBLINDS,DVLTIPRT   TEST HELD IN PRINTABLE FORMAT                
         BZ    BLDLIN02                                                         
         MVC   LSTLPREV,SUBLPVAL   YES - JUST MOVE OUT THE VALUES               
         MVC   LSTLINP,SUBLCVAL                                                 
         B     BLDLINX                                                          
                                                                                
BLDLIN02 OC    SUBLPVAL,SUBLPVAL   TEST PREVIOUS VALUE IS SET                   
         BZ    BLDLIN06                                                         
         CLI   SUBLPVAL,C'|'       TEST PREVIOUS VALUE IS SOFT                  
         BE    *+8                                                              
         CLI   SUBLPVAL,C'!'                                                    
         BNE   *+14                                                             
         MVC   LSTLPREV,SUBLCVAL   YES - DISPLAY SOFT EXPRESSION                
         B     BLDLIN06                                                         
         MVC   PCWORK(L'SUBLPVAL),SUBLPVAL                                      
         TM    SUBLINDS,DVLTID01   TEST ASSUME DAY 01 BIT SET                   
         BZ    BLDLIN04                                                         
         MVC   PCWORK+04(2),PCSPACES                                            
         MVC   PCWORK+10(2),PCSPACES                                            
                                                                                
BLDLIN04 MVI   FVTLEN,L'FVIHDR+L'FVIFLD                                         
         GOTOR AGOSOFT,PCPARM,(SUBLSOFO,PCWORK),                       *        
               ('SOFOTPRT+SOFOTMIX',FVIHDR),(SUBLSOFI,0)                        
         MVC   LSTLPREV,FVIFLD                                                  
                                                                                
BLDLIN06 OC    SUBLCVAL,SUBLCVAL   TEST CURRENT VALUE IS SET                    
         BZ    BLDLIN10                                                         
         CLI   SUBLCVAL,C'|'       TEST CURRENT VALUE IS SOFT                   
         BE    *+8                                                              
         CLI   SUBLCVAL,C'!'                                                    
         BNE   *+14                                                             
         MVC   LSTLINP,SUBLCVAL    YES - DISPLAY IT                             
         B     BLDLINX                                                          
         MVC   PCWORK(L'SUBLCVAL),SUBLCVAL                                      
         TM    SUBLINDS,DVLTID01   TEST ASSUME DAY 01 BIT SET                   
         BZ    BLDLIN08                                                         
         MVC   PCWORK+04(2),PCSPACES                                            
         MVC   PCWORK+10(2),PCSPACES                                            
                                                                                
BLDLIN08 GOTOR AGOSOFT,PCPARM,                                         *        
               (SUBLSOFO,PCWORK),('SOFOTPRT',LSTLINPH),(SUBLSOFI,0)             
         B     BLDLINX                                                          
                                                                                
BLDLIN10 CLI   CSACT,ACTSOF        TEST SETTING SOFT DATE                       
         BE    BLDLINX                                                          
         TM    SUBLINDS,DVLTITOD   TEST 'TODAY'                                 
         BZ    BLDLINX                                                          
                                                                                
BLDLIN12 OI    LSTLINPH+(FVATRB-FVIHDR),FVAPROT                                 
         NI    LSTLINPH+(FVATRB-FVIHDR),FF-(FVAHIGH)                            
         GOTOR AGETNXT             TEST NEXT RUN DATE SET                       
         BNE   BLDLINX                                                          
         CLI   SUBLVRUL,RRULBDAT   TEST ADJUSTED BILLING DATE                   
         BE    BLDLIN14                                                         
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',LSTLINPH),('SOFIIONE',0)                    
         B     BLDLINX                                                          
                                                                                
BLDLIN14 SR    RF,RF                                                            
         ICM   RF,3,PCWORK+2                                                    
         SRL   RF,4                                                             
         STCM  RF,3,PCFULL                                                      
                                                                                
         LA    RF,RFPBADJS         RF=A(BILLING DATE ADJUSTMENT LIST)           
         LHI   R0,RFPBADJM         R0=MAXIMUM NUMBER OF ENTRIES                 
BLDLIN16 OC    0(L'RFPBADJS,RF),0(RF)                                           
         BZ    BLDLIN18                                                         
         MVC   PCFULL+L'RFPBADJS(L'RFPBADJS),0(RF)                              
         NC    PCFULL+L'RFPBADJS(L'RFPBADJS),=AL2(GRPBACAL)                     
         CLC   PCFULL(L'RFPBADJS),PCFULL+L'RFPBADJS                             
         BE    BLDLIN20                                                         
         AHI   RF,L'RFPBADJS                                                    
         BCT   R0,BLDLIN16                                                      
                                                                                
BLDLIN18 GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',LSTLINPH),('SOFIIONE',0)                    
         B     BLDLINX                                                          
                                                                                
BLDLIN20 SR    R0,R0               CALCULATE BILLING DATE                       
         ICM   R0,3,0(RF)                                                       
         SRL   R0,10               R0=NUMBER OF ADJUSTMENT DAYS                 
         GOTOR VDATCON,PCPARM,(6,PCWORK),PCWORK+6                               
         GOTOR VADDAY,PCPARM,PCWORK+6,PCWORK,(R0)                               
         GOTOR VDATCON,PCPARM,(X'40',PCWORK),(17,LSTLINP)                       
                                                                                
BLDLINX  B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF RFP DATE SYMBOLS                                      *         
***********************************************************************         
                                                                                
BLDDAT   NTR1  ,                                                                
                                                                                
         SR    R0,R0                                                            
         ICM   R0,1,RFPVNUMS       R0=N'RFP SYMBOLS IN GROUP                    
         BZ    BLDDATX                                                          
         LA    R1,RFPD                                                          
         AHI   R1,RFPVSYMX-RFPD    R1=A(SYMBOL LIST)                            
SYM      USING RFPVSYME,R1                                                      
                                                                                
BLDDAT02 LA    RE,GSDATTAB         TEST ENTRY ALREADY IN DATTAB                 
         USING DATTABD,RE                                                       
         SR    RF,RF                                                            
         ICM   RF,1,GSDATNUM       RF=N'ENTRIES IN TABLE SO FAR                 
         BZ    BLDDAT08                                                         
                                                                                
BLDDAT04 CLC   DATTSYST,RFPVSYST   MATCH SYSTEM                                 
         BNE   BLDDAT06                                                         
         CLC   DATTSYME,SYM.RFPVSYME   AND DICTIONARY SYMBOL                    
         BNE   BLDDAT06                                                         
         CLC   DATTPVAL,PCSPACES                                                
         BH    BLDDAT10                                                         
         MVC   DATTPVAL,SYM.RFPVPVAL                                            
         OC    DATTPVAL,DATTPVAL                                                
         BNZ   BLDDAT10                                                         
         MVC   DATTPVAL,SYM.RFPVCVAL                                            
         B     BLDDAT10                                                         
                                                                                
BLDDAT06 AHI   RE,DATTABL          BUMP TO NEXT DATTAB ENTRY                    
         BCT   RF,BLDDAT04                                                      
                                                                                
BLDDAT08 SR    RF,RE                                                            
         IC    RF,GSDATNUM         BUMP N'ENTRIES IN TABLE                      
         AHI   RF,1                                                             
         CHI   RF,DATTABM                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   RF,GSDATNUM                                                      
                                                                                
         MVC   DATTSYST,RFPVSYST   BUILD NEW ENTRY                              
         MVC   DATTSYME,SYM.RFPVSYME                                            
         MVC   DATTVRUL,SYM.RFPVVRUL                                            
         MVC   DATTPVAL,SYM.RFPVPVAL   TAKE PREVIOUS VALUE                      
         MVC   DATTCVAL,SYM.RFPVCVAL   AND CURRENT VALUE                        
         OC    RFPVLSTR,RFPVLSTR   TEST LAST RUN DATE SET                       
         BZ    BLDDAT10                                                         
         CLC   RFPVLSTR,ASJDAT     AND RUN BEFORE TODAY                         
         BNL   BLDDAT10                                                         
         MVC   DATTPVAL,SYM.RFPVCVAL   USE CURRENT VALUE FOR PREVIOUS           
                                                                                
BLDDAT10 AHI   R1,RFPVSYML         BUMP TO NEXT RFP SYMBOL ENTRY                
         BCT   R0,BLDDAT02         DO FOR NUMBER OF SYMBOLS                     
                                                                                
BLDDATX  B     EXIT                                                             
         DROP  SYM,RE                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE A DATE                                                     *         
*                                                                     *         
* NTRY - R1=A(INPUT FIELD HEADER)                                     *         
*        SUBLSYSN=SYSTEM NUMBER, SUBLVRUL=RFP RULE NUMBER             *         
* EXIT - CC=LOW IF VALID BUT UNCHANGED, CC=EQUAL IF VALID AND CHANGED *         
*        CC=HIGH IF INVALID                                           *         
***********************************************************************         
                                                                                
VALDAT   NTR1  ,                                                                
                                                                                
         MVC   GSSVCVAL,SUBLCVAL   SAVE CURRENT VALUE                           
                                                                                
         MVI   FVMINL,1            MUST HAVE SOME INPUT IN FIELD                
         GOTOR AFVAL,(R1)                                                       
         BNE   VALDATH                                                          
                                                                                
         CLI   FVIFLD,C'+'         TEST INCREMENT                               
         BE    VALDATH                                                          
         CLI   FVIFLD,C'='         TEST AUTO                                    
         BE    VALDATH                                                          
                                                                                
         CLI   CSACT,ACTSOF        TEST SOFT DATE ACTION                        
         BNE   VALDAT02                                                         
         CLI   FVIFLD,C'|'         YES - MUST BE A SOFT DATE EXPRESSION         
         BE    VALDAT02                                                         
         CLI   FVIFLD,C'!'                                                      
         BNE   VALDATH                                                          
                                                                                
VALDAT02 MVC   PCWORK(L'SUBLCVAL),PCSPACES                                      
         SR    R0,R0                                                            
         IC    R0,SUBLSOFI                                                      
         CLI   CSACT,ACTSUB        IF SUBMITTING THE GROUP                      
         BNE   *+8                                                              
         AHI   R0,SOFIIRES         RESOLVE SOFT DATES NOW                       
         CLI   SUBLSOFT,SOFITYM+SOFITYMD                                        
         BE    VALDAT04                                                         
         GOTOR AGOSOFT,PCPARM,                                         *        
               (SUBLSOFT,FVIHDR),(SUBLSOFO,PCWORK),((R0),0)                     
         BNE   VALDATH                                                          
         B     VALDAT06                                                         
                                                                                
VALDAT04 GOTOR AGOSOFT,PCPARM,                                         *        
               ('SOFITYM',FVIHDR),(SUBLSOFO,PCWORK),((R0),0)                    
         BE    VALDAT06                                                         
                                                                                
         GOTOR AGOSOFT,PCPARM,                                         *        
               ('SOFITYMD',FVIHDR),(SUBLSOFO,PCWORK),((R0),0)                   
         BNE   VALDATH                                                          
                                                                                
VALDAT06 CLI   CSACT,ACTSOF        TEST SETTING SOFT DATES                      
         BNE   *+14                                                             
         MVC   SUBLCVAL,FVIFLD     YES - SET CURRENT VALUE FROM INPUT           
         B     VALDAT14                                                         
                                                                                
         MVI   FVOSYS,0            RESET MESSAGE SYSTEM NUMBER                  
         TM    SUBLINDS,DVLTID01   TEST SET DAYS TO 01                          
         BZ    VALDAT08                                                         
         MVC   PCWORK+04(2),=C'01'                                              
         MVC   PCWORK+10(2),=C'01'                                              
                                                                                
VALDAT08 TM    SUBLINDS,DVLTIDSP   TEST SET DAYS TO SPACES                      
         BZ    VALDAT10                                                         
         MVC   PCWORK+04(2),PCSPACES                                            
         MVC   PCWORK+10(2),PCSPACES                                            
                                                                                
VALDAT10 CLI   SUBLSYSN,REPNUMQ    TEST REP SYSTEM                              
         BNE   VALDAT12                                                         
         CLI   SUBLSOFO,SOFOTSD2   YES - TEST EBCDIC OUTPUT TYPE                
         BNE   VALDAT12                                                         
         SR    RE,RE                                                            
         IC    RE,PCWORK+0                                                      
         SHI   RE,10               RE=Y2K ADJUSTED YEAR (START DATE)            
         CLI   PCWORK,C'9'         TEST Y2K START YEAR                          
         BNH   *+8                                                              
         STC   RE,PCWORK+0         YES - SET ADJUSTED VALUE                     
         IC    RE,PCWORK+6                                                      
         SHI   RE,10               RE=Y2K ADJUSTED YEAR (END DATE)              
         CLI   PCWORK+6,C'9'       TEST Y2K END YEAR                            
         BNH   *+8                                                              
         STC   RE,PCWORK+6         YES - SET ADJUSTED VALUE                     
                                                                                
VALDAT12 L     R1,FVADDR                                                        
         MVC   L'FVIHDR(L'SUBINP1,R1),FVIFLD                                    
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
                                                                                
         XC    SUBLCVAL,SUBLCVAL                                                
         MVC   SUBLCVAL(12),PCWORK                                              
         TM    SUBLINDS,DVLTIPRT   TEST OUTPUT IS PRINTABLE                     
         BZ    VALDAT14                                                         
         MVC   SUBLCVAL,FVIFLD                                                  
                                                                                
VALDAT14 TM    SUBLSTAT,SUBLSCTD   TEST CURRENT ENTRY COUNTED                   
         BNZ   VALDAT16                                                         
         OI    SUBLSTAT,SUBLSCTD   SET THIS ONE WAS COUNTED                     
         SR    R0,R0               NO - UPDATE COUNTER                          
         ICM   R0,3,GSUPDNUM                                                    
         AHI   R0,1                                                             
         STCM  R0,3,GSUPDNUM                                                    
         CLC   GSUPDNUM,GSLSTNUM   TEST ALL LIST ENTRIES ARE VALID              
         BL    VALDAT16                                                         
         OI    CSINDSL2,CSIDPFQY   ENABLE 'UPDATE' PFKEY                        
         XC    TWALRA,TWALRA       SET TO RE-BUILD PFKEY LINE                   
                                                                                
VALDAT16 CLC   SUBLCVAL,GSSVCVAL   TEST VALUE CHANGED                           
         BE    VALDATE                                                          
         MVC   SUBLPVAL,GSSVCVAL   SET PREVIOUS FROM SAVED VALUE                
                                                                                
VALDATE  CLI   *+1,0               CC=EQUAL IF OKAY                             
         B     EXIT                                                             
                                                                                
VALDATH  TM    SUBLSTAT,SUBLSCTD   TEST CURRENT ENTRY COUNTED                   
         BZ    VALDATH2                                                         
         NI    SUBLSTAT,FF-(SUBLSCTD)                                           
         GOTOR ATSARIO,TSAPUT      PUT SUBMIT LIST RECORD                       
         SR    R0,R0               DECREMENT UPDATE COUNTER                     
         ICM   R0,3,GSUPDNUM                                                    
         AHI   R0,-1                                                            
         STCM  R0,3,GSUPDNUM                                                    
         NI    CSINDSL2,FF-(CSIDPFQY)                                           
VALDATH2 MVC   FVMSGNO,=AL2(GE$IDATE)                                           
         CLI   *+0,0               CC=HIGH ON ERROR                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DO DATE POST-VALIDATION                                  *         
***********************************************************************         
                                                                                
PSTVAL   NTR1  ,                                                                
                                                                                
         MVC   LSTTRECN,CSPSRECN   BUILD SYMBOL TABLE FROM TSAR RECORDS         
         LA    R4,GSSYMTAB                                                      
         USING SYMTABD,R4          R4=A(RESOLVED SYMBOL TABLE)                  
PSTVAL02 SR    RE,RE               BUILD LIST OF RESOLVED RFP SYMBOLS           
         ICM   RE,3,LSTTRECN       BUMP TO (FIRST/)NEXT RECORD NUMBER           
         AHI   RE,1                                                             
         CLM   RE,3,CSHIRECN       TEST ALL LIST ENTRIES PROCESSED              
         BH    PSTVAL04                                                         
         STCM  RE,3,LSTTRECN                                                    
         GOTOR ATSARIO,TSAGET      GET LIST RECORD                              
         MVC   SYMTSYST,SUBLSYST   BUILD SYMBOL TABLE ENTRY                     
         MVC   SYMTSYSN,SUBLSYSN                                                
         MVC   SYMTSYME,SUBLSYME                                                
         MVC   SYMTPVRN,SUBLPVRN                                                
         MVC   SYMTINDS,SUBLINDS                                                
         MVC   SYMTVAL,SUBLCVAL                                                 
         CLI   SYMTVAL,C'|'        TEST THIS IS A SOFT EXPRESSION               
         BE    *+8                                                              
         CLI   SYMTVAL,C'!'                                                     
         BNE   *+12                                                             
         GOTOR RESDAT,SYMTVAL      YES - RESOLVE IT NOW                         
         AHI   R4,SYMTABL          BUMP TO NEXT TABLE ENTRY                     
         B     PSTVAL02                                                         
                                                                                
PSTVAL04 MVI   SYMTABD,SYMTEOTQ    SET END OF TABLE INDICATOR                   
                                                                                
         OC    GSNRUN,GSNRUN                                                    
         BZ    PSTVAL06                                                         
         GOTOR VDATCON,PCPARM,(6,GSNRUN),(0,GSNXTR)                             
                                                                                
PSTVAL06 CLI   CSREC,RECGRP        TEST RFP GROUP RECORD                        
         BNE   *+12                                                             
         GOTOR GETREQ              PROCESS REQUESTS FOR THIS GROUP              
         B     EXIT                (EXIT WITH CONDITION CODE SET)               
                                                                                
         CLI   CSREC,RECXFG        TEST XFILE GROUP RECORD                      
         BNE   PSTVAL10                                                         
                                                                                
         L     R2,AIO1             XFILE - BUILD DATES FOR ALL GROUPS           
         USING XFILED,R2                                                        
         LA    R2,XFFRSTEL                                                      
         USING XFSGRPD,R2                                                       
         SR    R0,R0                                                            
PSTVAL08 IC    R0,XFSGLN           BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         CLI   XFSGCD,0            TEST END OF RECORD                           
         BE    EXIT                                                             
         CLI   XFSGCD,XFSGCDEQ     TEST SYSTEM/GROUP ELEMENT                    
         BNE   PSTVAL08                                                         
                                                                                
         MVC   CUUSER,GSUSER                                                    
         MVC   CUAALF,GSAGY                                                     
         GOTOR AGETSYS,XFSGSYS     CONVERT SYSTEM NUMBER TO LETTER              
         MVC   CUSYSL,PCWORK+L'SYSLNAME                                         
         LA    R1,XFSGGRP                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFPIO TO VALIDATE THE GROUP             
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GETREQ              PROCESS REQUESTS FOR THIS GROUP              
         BE    PSTVAL08                                                         
         B     EXIT                                                             
                                                                                
PSTVAL10 DC    H'0'                UNKNOWN RECORD TYPE                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET REQUESTS FOR GROUP AND DO POST VALIDATION                       *         
***********************************************************************         
                                                                                
GETREQ   NTR1  ,                                                                
                                                                                
         NI    GSFLAG,FF-(GSFSWS)  SET NO SYSTEM SWITCH OCCURRED                
                                                                                
         XC    RFPFRQID,RFPFRQID   INITIALISE FOR FIRST REQUEST                 
         XC    RFPFSORT,RFPFSORT                                                
         XC    RFPFSEQN,RFPFSEQN                                                
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPFLAGS,RFPXSYMS                                                
         MVI   RFPMODE,RFPRETRQ                                                 
                                                                                
GETREQ02 GOTOR VRFPIO,PCPARM,RFPD  GET FIRST/NEXT REQUEST FOR GROUP             
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR   TEST FOR RFP ERRORS                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     GETREQN                                                          
                                                                                
         CLI   RFPRET,RFPEOF       TEST ALL REQUESTS PROCESSED                  
         BE    GETREQY                                                          
                                                                                
         CLI   RFPVREQD,REQLRSRD   IGNORE RUN=NO REQUESTS                       
         BE    GETREQ02                                                         
                                                                                
         NI    GSFLAG,FF-(GSFSUB+GSFXVR)                                        
                                                                                
         GOTOR VREQRFP,PCPARM,(X'20',RFPVREQH),(RFPFSYS,ACOM),         *        
               (CULANG,0),(2,0),(X'FF',ARFPBUFF),GSNXTR                         
                                                                                
         LA    R1,RFPVREQC         MAKE RFP SYMBOL SUBSTITUTIONS                
         LHI   RF,L'RFPVREQC                                                    
         MHI   RF,RFPVMAXC                                                      
         SHI   RF,L'RFPVSYME-1                                                  
         LA    RF,0(R1,RF)                                                      
         LHI   RE,1                                                             
GETREQ04 CLI   0(R1),DD#ESCL2      TEST FOR DICTIONARY ESCAPE SEQUENCE          
         BL    GETREQ08                                                         
         CLI   0(R1),DD#ESCF                                                    
         BH    GETREQ08                                                         
                                                                                
         LA    R6,GSPVRTAB         R6=A(POST VALIDATION LIST)                   
         USING PVRTABD,R6                                                       
         LA    R4,GSSYMTAB         ESCAPE SEQ FOUND IN REQUEST - LOOK           
GETREQ06 CLI   SYMTABD,SYMTEOTQ    UP SYMBOL IN TABLE AND RESOLVE               
         BE    GETREQ08                                                         
         CLC   SYMTSYST,RFPVSYST   MATCH SYSTEM TO TABLE                        
         BNE   *+10                                                             
         CLC   SYMTSYME,0(R1)      MATCH SYMBOL TO TABLE                        
         BE    *+12                                                             
         AHI   R4,SYMTABL          BUMP TO NEXT TABLE ENTRY                     
         B     GETREQ06                                                         
                                                                                
         SR    R5,R5               RESOLVE REQUEST DATE                         
         IC    R5,SYMTSYME+L'SYMTSYME-1                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SYMTVAL                                                  
         OI    GSFLAG,GSFSUB       SET SUBSTITUTION DONE                        
                                                                                
         TM    SYMTINDS,DVLTIPVL+DVLTISVL                                       
         BZ    GETREQ08                                                         
         OI    GSFLAG,GSFXVR       SET EXTRA VALIDATION REQUIRED                
         MVC   PVRTSYSO,SYMTSYSN   & BUILD POST-VALIDATION TABLE ENTRY          
         MVC   PVRTROUT,SYMTPVRN                                                
         STCM  R1,15,PVRTADDR                                                   
         AHI   R6,PVRTABL          BUMP TO NEXT ENTRY                           
         MVI   PVRTABD,PVRTEOTQ    SET NEW END OF TABLE                         
                                                                                
GETREQ08 BXLE  R1,RE,GETREQ04      PROCESS ALL REQUEST CARDS                    
                                                                                
         TM    GSFLAG,GSFXVR       TEST EXTRA VALIDATION REQUIRED               
         BZ    GETREQ02                                                         
                                                                                
         MVC   GSSYSN,ASSYSN       SAVE CONNECTED SYSTEM NUMBERS                
         MVC   GSSYSO,ASSYSO                                                    
                                                                                
         LA    R6,GSPVRTAB         CALL POST-VALIDATION ROUTINES                
GETREQ10 CLI   PVRTABD,PVRTEOTQ    TEST END OF TABLE                            
         BE    GETREQ02                                                         
                                                                                
         L     RE,ASWSTAB          LOOK UP SYSTEM IN SWITCH TABLE               
         USING SYSSWTAB,RE                                                      
         LA    R0,SYSSWMAX                                                      
GETREQ12 CLC   SYSSWSOV,PVRTSYSO   MATCH ON OV SYSTEM NUMBER                    
         BE    GETREQ14                                                         
         AHI   RE,SYSSWLEN                                                      
         BCT   R0,GETREQ12                                                      
                                                                                
         GOTOR AGETSYS,PVRTSYSO    NOT FOUND - CAN'T ISSUE SWITCH               
         MVC   FVXTRA(L'SYSLNAME),PCWORK                                        
         MVC   FVMSGNO,=AL2(GE$ATSNA)                                           
         B     GETREQN                                                          
                                                                                
GETREQ14 MVC   PCBYTE2,SYSSWSYS    SET SWITCH-TO APPLICATION SYSTEM             
         MVC   GSAGYBIN,SYSSWAGB   GET AGENCY BINARY VALUE FROM PLIST           
         DROP  RE                                                               
         CLC   PVRTSYSO,ASSYSO     TEST SWITCHED TO CORRECT SYSTEM              
         BE    GETREQ18                                                         
                                                                                
         MVC   PCPARM(1),PCBYTE2   SWITCH TO APPLICATION SYSTEM                 
         MVC   PCPARM+1(3),PCEFFS                                               
         XC    PCPARM+4(4),PCPARM+4                                             
         GOTOR VSWITCH,PCPARM                                                   
         CLI   4(R1),0             TEST SWITCH SUCCESSFUL                       
         BE    GETREQ16                                                         
                                                                                
         GOTOR AGETSYS,PVRTSYSO    NO - TELL USER IT'S NOT AVAILABLE            
         MVC   FVXTRA(L'SYSLNAME),PCWORK                                        
         MVC   FVMSGNO,=AL2(GE$SNA)                                             
         B     GETREQN                                                          
                                                                                
GETREQ16 MVC   GSAGYBIN,0(R1)      GET AGENCY BINARY VALUE FROM PLIST           
         MVC   ASSYSO,PVRTSYSO     SET SYSTEM NUMBERS                           
         MVC   ASSYSN,PCBYTE2                                                   
         OI    GSFLAG,GSFSWS       SET SYSTEM SWITCH OCCURRED                   
                                                                                
GETREQ18 SR    RF,RF                                                            
         ICM   RF,1,PVRTROUT       RF=ROUTINE NUMBER                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         LA    RF,PVROUTS-L'PVROUTS(RF)                                         
         MVC   GSADATE,PVRTADDR                                                 
         BASR  RE,RF               CALL POST-VALIDATION ROUTINE                 
         BNE   GETREQN                                                          
         AHI   R6,PVRTABL          BUMP TO NEXT TABLE ENTRY                     
         B     GETREQ10                                                         
         DROP  R6                                                               
                                                                                
PVROUTS  DS    0XL4                ** POST-VALIDATION ROUTINE INDEX **          
ACCBILLD EQU   ((*-PVROUTS)/L'PVROUTS)+1                                        
         B     PVABILLD            ACCPAK - BILL DATE                           
*&&US                                                                           
SPTBILLD EQU   ((*-PVROUTS)/L'PVROUTS)+1                                        
         B     PVSBILLD            SPOTPAK - BILL DATE                          
                                                                                
NETBILLD EQU   ((*-PVROUTS)/L'PVROUTS)+1                                        
         B     PVNBILLD            NETPAK - BILL DATE                           
                                                                                
PRTBILLD EQU   ((*-PVROUTS)/L'PVROUTS)+1                                        
         B     PVPBILLD            PRINTPAK - BILL DATE                         
                                                                                
REPREP1  EQU   ((*-PVROUTS)/L'PVROUTS)+1                                        
         B     PVRREP1             REPPAK - 12 MONTH PERIOD                     
                                                                                
REPREPQ  EQU   ((*-PVROUTS)/L'PVROUTS)+1                                        
         B     PVRREPQ             REPPAK - MONTHS/QUARTER PERIODS              
*&&                                                                             
GETREQY  MVI   PCBYTE1,0                                                        
         B     GETREQX                                                          
GETREQN  MVI   PCBYTE1,1                                                        
                                                                                
GETREQX  TM    GSFLAG,GSFSWS       TEST SYSTEM SWITCH ISSUED                    
         BZ    GETREQXX                                                         
         MVC   PCPARM(1),GSSYSN    YES - SWITCH BACK TO CONNECT SYSTEM          
         MVC   PCPARM+1(3),PCEFFS                                               
         XC    PCPARM+4(4),PCPARM+4                                             
         GOTOR VSWITCH,PCPARM                                                   
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T SWITCH BACK TO CONNECT SYSTEM          
                                                                                
         MVC   ASSYSN,GSSYSN       RESTORE CONNECTED SYSTEM VALUES              
         MVC   ASSYSO,GSSYSO                                                    
                                                                                
GETREQXX CLI   PCBYTE1,0           SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
                                                                                
PSTVALY  CLI   *+1,0               SET CC=EQUAL IF NO ERROR                     
         B     EXIT                                                             
                                                                                
PSTVALN  CLI   *+0,0               SET CC=NOT EQUAL ON ERROR                    
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* SPOTPAK/NETPAK BILL DATE                                            *         
***********************************************************************         
                                                                                
PVNBILLD DS    0H                  NETPAK SHARES SPOTPAK CODE                   
PVSBILLD NTR1  ,                                                                
SPTREQ   USING SPQD,RFPVREQC                                                    
                                                                                
         MVI   PCDUB,SPACE         SET NO OFFICE CODE                           
                                                                                
         CLC   SPTREQ.SPQCLT,PCSPACES                                           
         BE    PVSB04                                                           
         CLI   SPTREQ.SPQCLT,C'$'                                               
         BE    PVSB04                                                           
                                                                                
         LA    R2,IOKEY                                                         
         USING SPAGYKEY,R2         READ SPOTPAK AGENCY RECORD                   
         XC    SPAGYKEY,SPAGYKEY                                                
         MVI   SPAGYKTYPE,SPAGYKTYPQ                                            
         MVC   SPAGYKAGY,GSAGY                                                  
         MVC   IOKEYSAV(L'SPAGYKEY),SPAGYKEY                                    
         GOTOR VDMGR,PCPARM,DMRDHI,SPTDIR,SPAGYKEY,SPAGYKEY                     
         CLC   SPAGYKEY(L'SPAGYKTYPE+L'SPAGYKAGY),IOKEYSAV                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR VDMGR,PCPARM,DMGETR,SPTFIL,SPAGYLEN+1,AIO4,IOWORK                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO4             R2=A(AGENCY RECORD)                          
         CLI   SPAGYEL,SPAGYELQ    TEST AGENCY ELEMENT AT START                 
         BE    *+6                                                              
         DC    H'0'                NO - BAD RECORD                              
         LA    R2,SPAGYEL                                                       
         USING SPAGYMEDEL,R2                                                    
         SR    R0,R0                                                            
PVSB02   IC    R0,1(R2)            LOOK UP MEDIA CODE                           
         AR    R2,R0                                                            
         CLI   SPAGYMEDEL,0        TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SPAGYMEDEL,SPAGYMEDEQ                                            
         BNE   PVSB02                                                           
         CLC   SPAGYMEDCD,SPTREQ.SPQMED                                         
         BNE   PVSB02                                                           
         MVC   PCWORK(L'SPAGYMEDBT),SPAGYMEDBT                                  
                                                                                
         LA    R2,IOKEY                                                         
         USING SPCKEY,R2                                                        
         XC    SPCKEY,SPCKEY       READ SPOTPAK CLIENT RECORD                   
         MVI   SPCKEYTYPE,0                                                     
         MVC   SPCKEYAM,PCWORK                                                  
         GOTOR GSVCLPK,PCPARM,SPTREQ.SPQCLT,SPCKEYCLT                           
         CLI   0(R1),0                                                          
         BNE   PVSB04                                                           
         GOTOR VDMGR,PCPARM,DMREAD,SPTDIR,SPCKEY,SPCKEY                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO4                                                          
         MVC   PCDUB(1),SPCOFFICE  EXTRACT CLIENT OFFICE CODE                   
                                                                                
         USING SPTPRFD,PCWORK                                                   
PVSB04   XC    SPTPRFD(SPTPRFL),SPTPRFD                                         
         MVI   SPTPSYS,SPTPSYSQ    GET SPOTPAK LK PROFILE                       
         MVC   SPTPPRG,=AL2(SPTPPRGQ)                                           
         MVC   SPTPAMC,SPTREQ.SPQAGY                                            
         CLI   PCDUB,SPACE                                                      
         BNH   *+14                                                             
         MVI   SPTPIND,SPTPOFFI                                                 
         MVC   SPTPOFF,PCWORK                                                   
         GOTOR VGETPROF,PCPARM,SPTPRFD,GSPROF,VDMGR                             
         OC    GSPLKMOS,GSPLKMOS   TEST ANY PROFILE LOCK DATE                   
         BNZ   PVSB06                                                           
                                                                                
         USING ACCPRFD,PCWORK                                                   
         XC    ACCPRFD(ACCPRFL),ACCPRFD                                         
         MVI   ACCPSYS,ACCPSYSQ    GET ACCPAK LK PROFILE                        
         MVC   ACCPPRG,=AL2(ACCPPRGQ)                                           
         MVC   ACCPAGY,GSAGY                                                    
         GOTOR VGETPROF,PCPARM,ACCPRFD,GSPROF,VDMGR                             
         OC    GSPLKMOS,GSPLKMOS   TEST ANY PROFILE LOCK DATE                   
         BZ    PSTVALY                                                          
                                                                                
PVSB06   MVC   PCDUB(L'GSPLKMOS),GSPLKMOS                                       
         MVI   PCDUB+L'GSPLKMOS,1                                               
         GOTOR VDATCON,PCPARM,(1,PCDUB),(0,PCWORK)                              
         CLC   PCWORK(4),SPTREQ.SPQUSER2                                        
         BNL   PVSB10                                                           
                                                                                
PVSB08   PACK  PCDUB,GSNXTRDD                                                   
         CVB   RF,PCDUB                                                         
         LCR   RF,RF                                                            
         GOTOR VADDAY,PCPARM,GSNXTR,PCWORK,(RF)                                 
         CLC   SPTREQ.SPQUSER2(4),PCWORK                                        
         BL    PVSB10                                                           
         GOTOR VADDAY,PCPARM,GSNXTR,PCWORK,90                                   
         CLC   SPTREQ.SPQUSER2(4),PCWORK                                        
         BNH   PSTVALY                                                          
         CLC   SPTREQ.SPQAGY,=C'WI'                                             
         BE    PSTVALY                                                          
                                                                                
PVSB10   MVC   FVMSGNO,=AL2(240)                                                
         MVI   FVOSYS,SPTNUMQ                                                   
         B     PSTVALN                                                          
         EJECT                                                                  
***********************************************************************         
* PRINTPAK BILL DATE                                                  *         
***********************************************************************         
                                                                                
PVPBILLD NTR1  ,                                                                
PRTREQ   USING PPQD,RFPVREQC                                                    
                                                                                
         MVI   PCDUB,SPACE         SET NO OFFICE CODE                           
                                                                                
         CLC   PRTREQ.PPQCLT,PCSPACES                                           
         BE    PVPB02                                                           
         CLI   PRTREQ.PPQCLT,C'$'                                               
         BE    PVPB02                                                           
         CLI   PRTREQ.PPQCLT,C'&&'                                              
         BE    PVPB02                                                           
                                                                                
         LA    R2,IOKEY                                                         
         USING PPPCLTKEY,R2        READ PRINTPAK CLIENT RECORD                  
         XC    PPPCLTKEY,PPPCLTKEY                                              
         MVC   PPPCLTKAGY,PRTREQ.PPQAGY                                         
         MVC   PPPCLTKMED,PRTREQ.PPQMED                                         
         MVI   PPPCLTKRCD,X'02'                                                 
         MVC   PPPCLTKCLT,PRTREQ.PPQCLT                                         
         GOTOR VDMGR,PCPARM,DMREAD,PRTDIR,PPPCLTKEY,PPPCLTKEY                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR VDMGR,PCPARM,DMGETR,PRTFIL,PPPCLTLEN+2,AIO4,IOWORK               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO4             EXTRACT CLIENT OFFICE CODE                   
         MVC   PCDUB(L'PPPCLTOFF),PPPCLTOFF                                     
                                                                                
PVPB02   CLC   =C'D1',PRTREQ.PPQPROG                                            
         BE    PVPB04                                                           
         CLC   =C'B1',PRTREQ.PPQPROG                                            
         BE    PVPB04                                                           
         CLC   =C'R1',PRTREQ.PPQPROG                                            
         BE    PVPB04                                                           
         MVC   FVMSGNO,=AL2(2)                                                  
         MVI   FVOSYS,PRTNUMQ                                                   
         B     PSTVALN                                                          
                                                                                
         USING PRTPRFD,PCWORK                                                   
PVPB04   XC    PRTPRFD(PRTPRFL),PRTPRFD                                         
         MVI   PRTPSYS,PRTPSYSQ    GET PRINTPAK LK PROFILE                      
         MVC   PRTPPRG,=AL2(PRTPPRGQ)                                           
         MVC   PRTPAGY,PRTREQ.PPQAGY                                            
         CLI   PCDUB,SPACE                                                      
         BNH   *+14                                                             
         MVI   PRTPIND,PRTPOFFI                                                 
         MVC   PRTPOFF,PCWORK                                                   
         GOTOR VGETPROF,PCPARM,PRTPRFD,GSPROF,VDMGR                             
         OC    GSPLKMOS,GSPLKMOS   TEST ANY PROFILE LOCK DATE                   
         BNZ   PVPB06                                                           
                                                                                
         XC    ACCPRFD(ACCPRFL),ACCPRFD                                         
         MVI   ACCPSYS,ACCPSYSQ    GET ACCPAK LK PROFILE                        
         MVC   ACCPPRG,=AL2(ACCPPRGQ)                                           
         MVC   ACCPAGY,GSAGY                                                    
         GOTOR VGETPROF,PCPARM,ACCPRFD,GSPROF,VDMGR                             
         OC    GSPLKMOS,GSPLKMOS   TEST ANY PROFILE LOCK DATE                   
         BZ    PSTVALY                                                          
                                                                                
PVPB06   MVC   PCDUB(L'GSPLKMOS),GSPLKMOS                                       
         MVI   PCDUB+L'GSPLKMOS,1                                               
         GOTOR VDATCON,PCPARM,(1,PCDUB),(0,PCWORK)                              
                                                                                
         MVC   PCWORK+6(6),GSNXTR                                               
         CLC   PRTREQ.PPQPAY(6),PCSPACES                                        
         BE    *+10                                                             
         MVC   PCWORK+6(6),PRTREQ.PPQPAY                                        
                                                                                
PVPB08   PACK  PCDUB,GSNXTRDD                                                   
         CVB   RF,PCDUB                                                         
         LCR   RF,RF                                                            
         GOTOR VADDAY,PCPARM,GSNXTR,PCWORK,(RF)                                 
         CLC   PCWORK+6(4),PCWORK                                               
         BL    PVPB10                                                           
         GOTOR VADDAY,PCPARM,GSNXTR,PCWORK,90                                   
         CLC   PCWORK+6(4),PCWORK                                               
         BNH   PSTVALY                                                          
                                                                                
PVPB10   MVC   FVMSGNO,=AL2(206)                                                
         MVI   FVOSYS,PRTNUMQ                                                   
         B     PSTVALN                                                          
         EJECT                                                                  
***********************************************************************         
* REPPAK - 12 MONTH PERIOD                                            *         
***********************************************************************         
                                                                                
PVRREP1  NTR1  ,                                                                
REPREQ   USING REPREQD,RFPVREQC                                                 
                                                                                
         CLC   REPREQ.REQSTART,REPREQ.REQEND                                    
         BE    PVRE01              ONLY ONE DATE WAS ENTERED                    
                                                                                
         CLC   REPREQ.REQSTART+4(2),PCSPACES                                    
         BNE   PVRE02                                                           
         CLC   REPREQ.REQEND+4(2),PCSPACES                                      
         BNE   PVRE02                                                           
                                                                                
         MVC   PCWORK+00(4),REPREQ.REQSTART                                     
         MVC   PCWORK+04(2),=C'01'                                              
         MVC   PCWORK+06(4),REPREQ.REQEND                                       
         MVC   PCWORK+10(2),=C'01'                                              
         GOTOR VPERVERT,PCPARM,PCWORK,PCWORK+6                                  
         CLC   PERVMNTH-PERVERTD(,R1),=AL2(12)                                  
         BNE   PVRE01                                                           
         B     PSTVALY                                                          
         EJECT                                                                  
***********************************************************************         
* REPPAK - MONTHLY PERIOD VALIDATION                                  *         
***********************************************************************         
                                                                                
PVRREPQ  NTR1  ,                                                                
         CLC   REPREQ.REQSTART+4(2),PCSPACES                                    
         BNE   PVRE02                                                           
         CLC   REPREQ.REQEND+4(2),PCSPACES                                      
         BNE   PVRE02                                                           
                                                                                
         CLC   REPREQ.REQEND(6),PCSPACES                                        
         BNE   PVRQ02                                                           
         MVC   REPREQ.REQEND,REPREQ.REQSTART                                    
         B     PVRQ04              CHECK IF QUARTERLY                           
                                                                                
PVRQ02   CLC   REPREQ.REQEND,REPREQ.REQSTART                                    
         BL    PVRE04                                                           
                                                                                
         MVC   PCWORK+00(4),REPREQ.REQSTART                                     
         MVC   PCWORK+04(2),=C'01'                                              
         MVC   PCWORK+06(4),REPREQ.REQEND                                       
         MVC   PCWORK+10(2),=C'01'                                              
                                                                                
         GOTOR VDATCON,PCPARM,(0,PCWORK+00),(15,PCDUB+00)                       
         GOTOR VDATCON,PCPARM,(0,PCWORK+06),(15,PCDUB+04)                       
         AP    PCDUB(4),=P'1000'   ADD 1 TO YEAR                                
         CP    PCDUB+4(4),PCDUB(4)                                              
         BNL   PVRE04                                                           
                                                                                
PVRQ04   CLI   REPREQ.REQ2OPT4,C'Q'                                             
         BNE   PSTVALY                                                          
         CLC   =C'01',PCWORK+02    TEST START MONTH ON QTR BOUNDARY             
         BE    PVRQ06                                                           
         CLC   =C'04',PCWORK+02                                                 
         BE    PVRQ06                                                           
         CLC   =C'07',PCWORK+02                                                 
         BE    PVRQ06                                                           
         CLC   =C'10',PCWORK+02                                                 
         BE    PVRQ06                                                           
         B     PVRE03                                                           
                                                                                
PVRQ06   CLC   =C'03',PCWORK+08    TEST END MONTH ON QTR BOUNDARY               
         BE    PSTVALY                                                          
         CLC   =C'06',PCWORK+08                                                 
         BE    PSTVALY                                                          
         CLC   =C'09',PCWORK+08                                                 
         BE    PSTVALY                                                          
         CLC   =C'12',PCWORK+08                                                 
         BE    PSTVALY                                                          
         B     PVRE03                                                           
                                                                                
PVRE01   MVC   FVMSGNO,=AL2(298)   DATES NOT 11 MONTHS APART                    
         B     PVREX                                                            
                                                                                
PVRE02   MVC   FVMSGNO,=AL2(299)   DATE FORMATS MUST BE YM-YM                   
         B     PVREX                                                            
                                                                                
PVRE03   MVC   FVMSGNO,=AL2(472)   PERIOD START/END NOT ON QTR BOUNDARY         
         B     PVREX                                                            
                                                                                
PVRE04   MVC   FVMSGNO,=AL2(13)    INVALID DATE                                 
         B     PVREX                                                            
                                                                                
PVREX    MVI   FVOSYS,REPNUMQ      SET REPPAK SYSTEM NUMBER                     
         B     PSTVALN                                                          
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* ACCPAK BILL DATE                                                    *         
***********************************************************************         
                                                                                
PVABILLD NTR1  ,                                                                
                                                                                
ACCREQ   USING ACQD,RFPVREQC                                                    
                                                                                
         LA    R2,IOKEY            READ ACCPAK COMPANY RECORD                   
         USING ACCPYRECD,R2                                                     
         MVC   ACCPYKEY,PCSPACES                                                
         MVC   ACCPYKCPY,GSAGYBIN                                               
         GOTOR VDMGR,PCPARM,DMREAD,ACCOUNT,ACCPYRECD,AIO4                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO4             LOOK UP OFFICE RULE                          
         AHI   R2,ACACCORFST                                                    
         USING ACCPYELD,R2                                                      
         SR    R0,R0                                                            
PVAB02   CLI   ACCPYEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ACCPYEL,ACCPYELQ                                                 
         BE    *+14                                                             
         IC    R0,ACCPYLN                                                       
         AR    R2,R0                                                            
         B     PVAB02                                                           
                                                                                
         MVI   PCBYTE1,0                                                        
         TM    ACCPYSTAT4,ACCPYSOFF2                                            
         BZ    *+8                                                              
         OI    PCBYTE1,X'80'       SET 2 CHARACTER OFFICES IN USE               
                                                                                
         CLC   ACCREQ.ACQOFFFL,PCSPACES                                         
         BNH   *+14                                                             
         MVC   PCDUB(L'ACCREQ.ACQOFFFL),ACCREQ.ACQOFFFL                         
         B     PVAB08                                                           
                                                                                
         LA    R2,IOKEY            READ ACCPAK LEDGER RECORD                    
         USING ACLDGRECD,R2                                                     
         MVC   ACLDGKEY,PCSPACES                                                
         MVC   ACLDGKCPY,GSAGYBIN                                               
         MVC   ACLDGKUNT,ACCREQ.ACQUNT                                          
         MVC   ACLDGKLDG,ACCREQ.ACQLDG                                          
         GOTOR VDMGR,PCPARM,DMREAD,ACCOUNT,ACLDGRECD,AIO4                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO4                                                          
         AHI   R2,ACACCORFST                                                    
         USING ACACLELD,R2                                                      
         SR    R0,R0                                                            
PVAB04   CLI   ACACLEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ACACLEL,ACACLELQ                                                 
         BE    *+14                                                             
         IC    R0,ACACLLN                                                       
         AR    R2,R0                                                            
         B     PVAB04                                                           
                                                                                
         MVC   PCBYTE2,ACACLVLEN   EXTRACT CLIENT CODE LENGTH                   
                                                                                
         LA    R2,IOKEY            READ ACCPAK CLIENT RECORD                    
         USING ACACTRECD,R2                                                     
         MVC   ACACTKEY,PCSPACES                                                
         MVC   ACACTKCPY,GSAGYBIN                                               
         MVC   ACACTKUNT,ACCREQ.ACQUNT                                          
         MVC   ACACTKLDG,ACCREQ.ACQLDG                                          
         SR    RE,RE                                                            
         IC    RE,PCBYTE2                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACACTKACT(0),ACCREQ.ACQACT                                       
         GOTOR VDMGR,PCPARM,DMREAD,ACCOUNT,ACACTRECD,AIO4                       
         BNE   PVABE2                                                           
                                                                                
         MVC   PCDUB(L'ACPPRGAOFF),PCSPACES                                     
         L     R2,AIO4             FIND PRODUCTION PROFILE ELEMENT              
         AHI   R2,ACACCORFST                                                    
         USING ACPPRELD,R2                                                      
         SR    R0,R0                                                            
PVAB06   CLI   ACPPREL,0                                                        
         BE    PVAB08                                                           
         CLI   ACPPREL,ACPPRELQ                                                 
         BE    *+14                                                             
         IC    R0,ACPPRLN                                                       
         AR    R2,R0                                                            
         B     PVAB06                                                           
                                                                                
         MVC   PCDUB(L'ACPPRGAOFF),ACPPRGAOFF                                   
                                                                                
*&&UK*&& USING ACCPRFD,PCWORK                                                   
PVAB08   XC    ACCPRFD(ACCPRFL),ACCPRFD                                         
         MVI   ACCPSYS,ACCPSYSQ                                                 
         MVC   ACCPPRG,=AL2(ACCPPRGQ)                                           
         MVC   ACCPUL,ACCREQ.ACQUNT                                             
         MVC   ACCPAGY,GSAGY                                                    
                                                                                
         CLC   PCDUB(L'ACCPOFFN),PCSPACES                                       
         BNH   PVAB12                                                           
         TM    PCBYTE1,X'80'       TEST USING 2 CHARACTER OFFICES               
         BZ    PVAB10                                                           
         MVI   ACCPIND,ACCPINDN    YES - SET GETPROF INDICATOR                  
         MVC   ACCPOFFN,PCDUB      AND 2 CHARACTER OFFICE CODE                  
         B     PVAB12                                                           
                                                                                
PVAB10   MVI   ACCPIND,ACCPINDO    SET ONE CHARACTER OFFICE INDICATOR           
         MVC   ACCPOFFO,PCDUB      AND ONE CHARACTER OFFICE CODE                
                                                                                
PVAB12   GOTOR VGETPROF,PCPARM,ACCPRFD,GSPROF,VDMGR                             
         OC    GSPLKMOS,GSPLKMOS   TEST ANY PROFILE LOCK DATE                   
         BZ    PSTVALY                                                          
                                                                                
         ICM   RF,15,GSADATE                                                    
         MVC   PCDUB(L'GSPLKMOS),ASPDAT                                         
         CLC   0(6,RF),PCSPACES                                                 
         BE    PVAB14                                                           
         GOTOR VDATCON,PCPARM,(0,(RF)),(1,PCDUB)                                
                                                                                
PVAB14   CLC   GSPLKMOS,PCDUB                                                   
         BL    PVAB16                                                           
         MVC   FVMSGNO,=AL2(AE$MOSLK)                                           
         MVI   FVOSYS,ACCNUMQ                                                   
         B     PSTVALN                                                          
                                                                                
PVAB16   CLC   ACCREQ.ACQSTART,PCSPACES                                         
         BE    PSTVALY                                                          
         GOTOR VADDAY,PCPARM,GSNXTR,PCWORK,-14                                  
         CLC   ACCREQ.ACQSTART,PCWORK                                           
         BL    PVABE1                                                           
         GOTOR VADDAY,PCPARM,GSNXTR,PCWORK,30                                   
         CLC   ACCREQ.ACQSTART,PCWORK                                           
         BNH   PSTVALY                                                          
                                                                                
PVABE1   MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         MVI   FVOSYS,ACCNUMQ                                                   
         B     PSTVALN                                                          
                                                                                
PVABE2   MVC   FVMSGNO,=AL2(AE$RECNF)                                           
         MVI   FVOSYS,ACCNUMQ                                                   
         B     PSTVALN                                                          
         DROP  R2,ACCREQ                                                        
         EJECT                                                                  
***********************************************************************         
* UPDATE RANGE MASK - ANY RUN DATE AFTER TODAY BUT BEFORE NEXT RUN    *         
* DATE ARE CLEARED & NEXT RUN DATE BIT IS SET                         *         
*                                                                     *         
* NTRY - RFPVRNGE CONTAINS CURRENT RUN MASK                           *         
*        GSNRUN=NEXT RUN DATE FOR GROUP                               *         
***********************************************************************         
                                                                                
UPDRNG   NTR1  ,                                                                
                                                                                
         CLC   RFPVNXTR,ASJDAT     TEST NEXT RUN DATE BEFORE TODAY              
         BNL   *+10                                                             
         MVC   RFPVLSTR,RFPVNXTR   YES - SET LAST RUN DATE FROM NEXT            
         MVC   RFPVNXTR,GSNRUN                                                  
         OC    RFPVENDR,RFPVENDR                                                
         BZ    UPDRNGX                                                          
                                                                                
UPDRNG02 GOTOR AGETNXT             GET NEXT RUN DATE FOR GROUP                  
         BNE   UPDRNG04                                                         
         CLC   PCWORK(L'GSNRUN),GSNRUN                                          
         BNL   UPDRNG04                                                         
         PACK  PCDUB,PCWORK+2(2)                                                
         CVB   RE,PCDUB                                                         
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RE,RFPVRNGE(RE)                                                  
         IC    RF,DAYBITS(RF)                                                   
         EX    RF,*+8                                                           
         B     UPDRNG02                                                         
         XI    0(RE),0                                                          
                                                                                
UPDRNG04 ZAP   PCDUB,GSNRUND       TURN ON RUN BIT IN MASK                      
         CVB   RE,PCDUB                                                         
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RE,RFPVRNGE(RE)     RE=A(BYTE WITH RUN DATE IN)                  
         IC    RF,DAYBITS(RF)      RF=DAY BIT                                   
         EX    RF,*+8                                                           
         B     UPDRNGX                                                          
         OI    0(RE),0                                                          
                                                                                
UPDRNGX  B     EXIT                                                             
         EJECT                                                                  
RECSUB   EQU   4                   SPECIAL RECORD TYPE                          
GSLSTMAX EQU   14                  N'LINES ON LIST SCREEN                       
                                                                                
CONLETQ  EQU   C'C'                CONTROL SYSTEM LETTER                        
                                                                                
         LTORG                                                                  
                                                                                
DMREAD   DC    C'DMREAD '          DATAMGR COMMANDS                             
DMRDHI   DC    C'DMRDHI '                                                       
DMGETR   DC    C'GETREC '                                                       
                                                                                
ACCOUNT  DC    C'ACCOUNT'          DATAMGR FILES                                
SPTDIR   DC    C'SPTDIR '                                                       
SPTFIL   DC    C'SPTFIL '                                                       
PRTDIR   DC    C'PRTDIR '                                                       
PRTFIL   DC    C'PRTFIL '                                                       
                                                                                
DAYBITS  DC    X'8040201008040201'                                              
         EJECT                                                                  
DVLTAB   DS    0X                  ** DATE VALIDATION TABLE **                  
                                                                                
         DC    AL1(RRULYM,DVLTSDQ)                                              
         DC    AL1(0)                                                           
         DC    AL1(SOFITYM)                                                     
         DC    AL1(SOFIIANY+SOFIIONE+SOFIIOUT)                                  
         DC    AL1(SOFOTSD1)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULYMD,DVLTSDQ)                                             
         DC    AL1(0)                                                           
         DC    AL1(SOFITYMD)                                                    
         DC    AL1(SOFIIANY+SOFIIONE+SOFIIOUT)                                  
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULDATE,DVLTSDQ)                                            
         DC    AL1(0)                                                           
         DC    AL1(SOFITYM+SOFITYMD)                                            
         DC    AL1(SOFIIANY+SOFIIONE+SOFIIOUT)                                  
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULYMP,DVLTSDQ)                                             
         DC    AL1(0)                                                           
         DC    AL1(SOFITYM)                                                     
         DC    AL1(SOFIIANY+SOFIIF1O+SOFIIOUT)                                  
         DC    AL1(SOFOTSD1)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULYMDP,DVLTSDQ)                                            
         DC    AL1(0)                                                           
         DC    AL1(SOFITYMD)                                                    
         DC    AL1(SOFIIANY+SOFIIOUT)                                           
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULPERD,DVLTSDQ)                                            
         DC    AL1(DVLTIPRT)                                                    
         DC    AL1(SOFITYM+SOFITYMD)                                            
         DC    AL1(SOFIIANY+SOFIIOUT)                                           
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULPDSP,DVLTSDQ)                                            
         DC    AL1(DVLTIDSP)                                                    
         DC    AL1(SOFITYM)                                                     
         DC    AL1(SOFIIANY+SOFIIOUT)                                           
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULPD01,DVLTSDQ)                                            
         DC    AL1(DVLTID01)                                                    
         DC    AL1(SOFITYM)                                                     
         DC    AL1(SOFIIANY+SOFIIOUT)                                           
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULPVAL,DVLTSDQ)                                            
         DC    AL1(DVLTIPRT)                                                    
         DC    AL1(SOFITYMD)                                                    
         DC    AL1(SOFIIANY+SOFIIOUT)                                           
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULPV1D,DVLTSDQ)                                            
         DC    AL1(DVLTIPRT)                                                    
         DC    AL1(SOFITYMD)                                                    
         DC    AL1(SOFIIANY+SOFIIONE+SOFIIOUT)                                  
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULPV1M,DVLTSDQ)                                            
         DC    AL1(DVLTIPRT)                                                    
         DC    AL1(SOFITYM)                                                     
         DC    AL1(SOFIIANY+SOFIIONE+SOFIIOUT)                                  
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULPVPM,DVLTSDQ)                                            
         DC    AL1(DVLTIPRT)                                                    
         DC    AL1(SOFITYM)                                                     
         DC    AL1(SOFIIANY+SOFIIOUT)                                           
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULTODY,DVLTSDQ)                                            
         DC    AL1(DVLTITOD)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULBDAT,DVLTSDQ)                                            
         DC    AL1(DVLTITOD)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RRULACBD,ACCNUMQ)                                            
         DC    AL1(DVLTIPVL)                                                    
         DC    AL1(SOFITYMD)                                                    
         DC    AL1(SOFIIANY+SOFIIONE+SOFIIOUT)                                  
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(ACCBILLD)                                                    
*&&US                                                                           
         DC    AL1(RRULSPBD,SPTNUMQ)                                            
         DC    AL1(DVLTIPVL)                                                    
         DC    AL1(SOFITYMD)                                                    
         DC    AL1(SOFIIANY+SOFIIONE+SOFIIOUT)                                  
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(SPTBILLD)                                                    
                                                                                
         DC    AL1(RRULNEBD,NETNUMQ)                                            
         DC    AL1(DVLTIPVL)                                                    
         DC    AL1(SOFITYMD)                                                    
         DC    AL1(SOFIIANY+SOFIIONE+SOFIIOUT)                                  
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(NETBILLD)                                                    
                                                                                
         DC    AL1(RRULPPBD,PRTNUMQ)                                            
         DC    AL1(DVLTIPVL)                                                    
         DC    AL1(SOFITYMD)                                                    
         DC    AL1(SOFIIANY+SOFIIONE+SOFIIOUT)                                  
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(PRTBILLD)                                                    
                                                                                
         DC    AL1(RRULREP1,REPNUMQ)                                            
         DC    AL1(DVLTISVL+DVLTIDSP)                                           
         DC    AL1(SOFITYM)                                                     
         DC    AL1(SOFIIANY+SOFIIOUT)                                           
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(REPREP1)                                                     
                                                                                
         DC    AL1(RRULREPQ,REPNUMQ)                                            
         DC    AL1(DVLTISVL+DVLTIDSP)                                           
         DC    AL1(SOFITYM)                                                     
         DC    AL1(SOFIIANY+SOFIIOUT)                                           
         DC    AL1(SOFOTSD2)                                                    
         DC    AL1(REPREPQ)                                                     
*&&                                                                             
DVLTABX  DC    AL1(DVLTEOTQ)                                                    
                                                                                
         DROP  RB,R8,R7                                                         
         EJECT                                                                  
***********************************************************************         
* RESOLVE SOFT DATES                                                  *         
*                                                                     *         
* NTRY - R1=A(SOFT DATE EXPRESSION)                                   *         
***********************************************************************         
                                                                                
RESDAT   NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1                                                            
         MVC   FVIFLD(L'SYMTVAL),0(R2)                                          
         LA    R1,FVIFLD+L'SYMTVAL-1                                            
         LHI   R0,L'SYMTVAL                                                     
         CLI   0(R1),C' '                                                       
         JH    *+10                                                             
         BCTR  R1,0                                                             
         BRCT  R0,*-10                                                          
         XC    FVIHDR,FVIHDR                                                    
         STC   R0,FVILEN                                                        
         AHI   R0,L'FVIHDR                                                      
         STC   R0,FVTLEN                                                        
                                                                                
         MVC   PCWORK(L'SUBLCVAL),PCSPACES                                      
         SR    R0,R0                                                            
         IC    R0,SUBLSOFI                                                      
         AHI   R0,SOFIIRES                                                      
         CLI   SUBLSOFT,SOFITYM+SOFITYMD                                        
         BE    RESDAT02                                                         
         GOTOR AGOSOFT,PCPARM,                                         *        
               (SUBLSOFT,FVIHDR),(SUBLSOFO,PCWORK),((R0),0)                     
         BE    RESDAT04                                                         
         DC    H'0'                                                             
                                                                                
RESDAT02 GOTOR AGOSOFT,PCPARM,                                         *        
               ('SOFITYM',FVIHDR),(SUBLSOFO,PCWORK),((R0),0)                    
         BE    RESDAT04                                                         
                                                                                
         GOTOR AGOSOFT,PCPARM,                                         *        
               ('SOFITYMD',FVIHDR),(SUBLSOFO,PCWORK),((R0),0)                   
         BE    RESDAT04                                                         
         DC    H'0'                                                             
                                                                                
RESDAT04 TM    SUBLINDS,DVLTID01   TEST SET DAYS TO 01                          
         BZ    RESDAT06                                                         
         MVC   PCWORK+04(2),=C'01'                                              
         MVC   PCWORK+10(2),=C'01'                                              
                                                                                
RESDAT06 TM    SUBLINDS,DVLTIDSP   TEST SET DAYS TO SPACES                      
         BZ    RESDAT08                                                         
         MVC   PCWORK+04(2),PCSPACES                                            
         MVC   PCWORK+10(2),PCSPACES                                            
                                                                                
RESDAT08 CLI   SUBLSYSN,REPNUMQ    TEST REP SYSTEM                              
         BNE   RESDAT10                                                         
         CLI   SUBLSOFO,SOFOTSD2   YES - TEST EBCDIC OUTPUT TYPE                
         BNE   RESDAT10                                                         
         SR    RE,RE                                                            
         IC    RE,PCWORK+0                                                      
         SHI   RE,10               RE=Y2K ADJUSTED YEAR (START DATE)            
         CLI   PCWORK,C'9'         TEST Y2K START YEAR                          
         BNH   *+8                                                              
         STC   RE,PCWORK+0         YES - SET ADJUSTED VALUE                     
         IC    RE,PCWORK+6                                                      
         SHI   RE,10               RE=Y2K ADJUSTED YEAR (END DATE)              
         CLI   PCWORK+6,C'9'       TEST Y2K END YEAR                            
         BNH   RESDAT10                                                         
         STC   RE,PCWORK+6         YES - SET ADJUSTED VALUE                     
                                                                                
RESDAT10 XC    0(L'SYMTVAL,R2),0(R2)                                            
         MVC   0(12,R2),PCWORK                                                  
         TM    SUBLINDS,DVLTIPRT   TEST OUTPUT IS PRINTABLE                     
         BZ    *+10                                                             
         MVC   0(L'SYMTVAL,R2),FVIFLD                                           
         J     EXIT                                                             
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
GSWORKD  DSECT                     ** GRPSUB LOCAL W/S **                       
GSSYMTAB DS    32XL(SYMTABL)       RESOLVED SYMBOL TABLE                        
GSPVRTAB DS    12XL(PVRTABL)       POST-VALIDATION TABLE                        
         EJECT                                                                  
* GERLPWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE GERLPWORK                                                      
         PRINT ON                                                               
                                                                                
TWAD     DSECT                     ** TWAD DEFINITIONS **                       
         ORG   RLPOLY1H                                                         
       ++INCLUDE GERLPF9D          GROUP KEY SCREEN                             
         ORG   KEYPFKH                                                          
       ++INCLUDE GERLPFCD          GROUP SUBMIT SCREEN                          
                                                                                
         ORG   OSVALS                                                           
                                                                                
GSLGRP   DS    CL(L'GRPKGRP)       LAST TIME GROUP CODE (GLOBAL)                
                                                                                
GSVCLPK  DS    V                   V(CLPACK)                                    
GSADATE  DS    A                   A(DATE FIELD)                                
                                                                                
GSFLAG   DS    X                   FLAG BYTE                                    
GSFSUB   EQU   X'80'               SYMBOL SUBSTITUTION DONE                     
GSFXVR   EQU   X'40'               EXTRA VALIDATION REQUIRED                    
GSFSWS   EQU   X'20'               SYSTEM SWITCH TAKEN PLACE                    
                                                                                
GSGRP    DS    CL(L'RFPVGRP)       GROUP CODE                                   
                                                                                
GSAGYBIN DS    X                   CURRENT AGENCY BINARY VALUE                  
GSAGY    DS    CL(L'TWAAGY)        CURRENT AGENCY ALPHA ID                      
GSUSER   DS    XL(L'TWAUSRID)      CURRENT USER-D NUMBER                        
GSSYST   DS    CL(L'ASSYSL)        CURRENT SYSTEM LETTER                        
                                                                                
GSNRUN   DS    0XL(L'RFPVNXTR)     NEXT RUN DATE                                
GSNRUNY  DS    XL2                                                              
GSNRUND  DS    PL2                                                              
                                                                                
GSXFGDA  DS    XL(L'IODA)          XFILE GROUP DISK ADDRESS                     
                                                                                
GSBYTE1  DS    XL1                 WORK BYTE 1                                  
GSBYTE2  DS    XL1                 WORK BYTE 2                                  
GSSCRNUM DS    XL1                 SCROLL MAGNITUDE                             
                                                                                
GSPAG#LO DS    XL2                 LOW RECORD ON CURRENT PAGE                   
GSPAG#HI DS    XL2                 HIGH RECORD ON CURRENT PAGE                  
                                                                                
GSSVCVAL DS    CL(L'SUBLCVAL)      SAVED CURRENT VALUE                          
                                                                                
GSSYSO   DS    XL(L'ASSYSO)        SYSTEM OV NUMBER                             
GSSYSN   DS    XL(L'ASSYSN)        SYSTEM SE NUMBER                             
                                                                                
GSNXTR   DS    0CL6                NEXT RUN DATE (EBCDIC YYMMDD)                
GSNXTRYY DS    CL2                 YEAR                                         
GSNXTRMM DS    CL2                 MONTH                                        
GSNXTRDD DS    CL2                 DAY                                          
                                                                                
GSPROF   DS    0XL16               ** PROFILE RETURNED FROM GETPROF **          
GSPLKMOS DS    0PL2                BILLING LOCK MOS                             
GSPLKMYR DS    PL1                 BILLING LOCK YEAR                            
GSPLKMMO DS    PL1                 BILLING LOCK MONTH                           
         ORG   GSPROF+L'GSPROF                                                  
                                                                                
GSSAVE   DS    0X                  ** LIST KEY **                               
                                                                                
GSDATIND DS    X                   INDICATOR BYTE                               
GSDATIPV EQU   X'80'               POST VALIDATION REQUIRED                     
GSDATISV EQU   X'40'               SPECIAL VALIDATION REQUIRED                  
GSLSTNUM DS    XL2                 N'RECORDS IN LIST                            
GSUPDNUM DS    XL2                 N'RECORDS UPDATED                            
GSDATNUM DS    X                   N'ENTRIES IN DATTAB                          
GSDATTAB DS    (DATTABM)XL(DATTABL)                                             
                                                                                
GSSAVEL  EQU   *-GSSAVE                                                         
                                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
                                                                                
LSTD     DSECT                     ** DSECT TO COVER TWA LIST LINE **           
                                                                                
LSTLINH  DS    CL(L'SUBLIN1H)                                                   
LSTLIN   DS    0CL(L'SUBLIN1)                                                   
LSTLSYSN DS    CL(L'SYSLNAME)                                                   
         DS    CL3                                                              
LSTLSYMB DS    CL(L'RFPVSYMB)                                                   
         DS    CL6                                                              
LSTLPREV DS    CL(L'SUBINP1)                                                    
         ORG   LSTLIN+L'LSTLIN                                                  
                                                                                
LSTLINPH DS    CL(L'SUBINP1H)                                                   
LSTLINP  DS    CL(L'SUBINP1)                                                    
LSTLINPX DS    CL(L'SUBINP1X)                                                   
                                                                                
LSTNEXT  EQU   *                                                                
                                                                                
DATTABD  DSECT                     ** RFP DATE TABLE **                         
DATTSYST DS    CL(L'RFPVSYST)      SYSTEM LETTER                                
DATTSYME DS    XL(L'RFPVSYME)      DICTIONARY SYMBOL                            
DATTVRUL DS    XL(L'RFPVVRUL)      EDIT RULE                                    
DATTPVAL DS    CL(L'RFPVPVAL)      PREVIOUS VALUE                               
DATTCVAL DS    CL(L'RFPVCVAL)      CURRENT VALUE                                
DATTABL  EQU   *-DATTABD           LENGTH OF TABLE ENTRY                        
DATTABM  EQU   64                  MAXIMUM N'DATTAB ENTRIES                     
                                                                                
SYMTABD  DSECT                     ** RFP SYMBOL TABLE **                       
SYMTEOTQ EQU   0                   END OF TABLE                                 
SYMTSYST DS    CL(L'RFPVSYST)      SYSTEM LETTER                                
SYMTSYSN DS    CL(L'ASSYSO)        SYSTEM NUMBER                                
SYMTSYME DS    XL(L'RFPVSYME)      DICTIONARY SYMBOL                            
SYMTPVRN DS    XL(L'DVLTPVRN)      POST VALIDATION ROUTINE NUMBER               
SYMTINDS DS    XL1                 SYMBOL INDICATORS                            
SYMTVAL  DS    CL(L'RFPVCVAL)      SYMBOL VALUE                                 
SYMTABL  EQU   *-SYMTABD           LENGTH OF TABLE ENTRY                        
                                                                                
DVLTABD  DSECT                     ** DATE VALIDATION/DISPLAY TABLE **          
                                                                                
DVLTTYPE DS    XL(L'RFPVVRUL)      VALIDATION RULE NUMBER                       
DVLTEOTQ EQU   0                   END OF TABLE                                 
DVLTSYSN DS    XL(L'SYSLNUM)       SYSTEM NUMBER                                
DVLTSDQ  EQU   0                   DEFAULT SYSTEM VALUE                         
                                                                                
DVLTINDS DS    XL1                 INDICATOR BYTE                               
DVLTID01 EQU   X'80'               SET DAYS TO 01                               
DVLTIPRT EQU   X'40'               OUTPUT IS IN PRINTABLE FORMAT                
DVLTITOD EQU   X'20'               'TODAY' - NO INPUT                           
DVLTIDSP EQU   X'10'               SET DAYS TO SPACES                           
DVLTIPVL EQU   X'08'               CALL SYSTEM POST VALIDATION                  
DVLTISVL EQU   X'04'               CALL SYSTEM SPECIAL VALIDATION               
                                                                                
DVLTSOFT DS    XL(L'SOFITYPE)      SOFDAT INPUT TYPE                            
DVLTSOFI DS    XL(L'SOFIINDS)      SOFDAT INPUT INDICATORS                      
DVLTSOFO DS    XL(L'SOFOTYPE)      SOFDAT OUTPUT TYPE                           
                                                                                
DVLTPVRN DS    AL1                 POST-VALIDATION ROUTINE NUMBER               
                                                                                
DVLTABL  EQU   *-DVLTABD           LENGTH OF TABLE ENTRY                        
                                                                                
PVRTABD  DSECT                     ** POST-VALIDATION TABLE **                  
PVRTEOTQ EQU   0                   END OF TABLE INDICATOR                       
PVRTSYSO DS    XL1                 (OVERLAY) SYSTEM NUMBER                      
PVRTROUT DS    AL1                 ROUTINE NUMBER                               
PVRTADDR DS    AL4                 A(DATE VALUE)                                
PVRTABL  EQU   *-PVRTABD           LENGTH OF TABLE ENTRY                        
                                                                                
* DDDICTATED                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
         PRINT ON                                                               
                                                                                
* DDDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
                                                                                
* ACQD                                                                          
         PRINT OFF                                                              
       ++INCLUDE ACQD                                                           
         PRINT ON                                                               
                                                                                
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
*PREFIX=AC                                                                      
       ++INCLUDE ACGENFILE                                                      
*PREFIX=                                                                        
         PRINT ON                                                               
                                                                                
*&&US                                                                           
* SPGENAGY/SPGENCLT                                                             
         PRINT OFF                                                              
*PREFIX=SP                                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
*PREFIX=                                                                        
         PRINT ON                                                               
* SPQD                                                                          
         PRINT OFF                                                              
       ++INCLUDE SPQD                                                           
         PRINT ON                                                               
                                                                                
* PCLTREC                                                                       
         PRINT OFF                                                              
*PREFIX=PP                                                                      
       ++INCLUDE PCLTREC                                                        
*PREFIX=                                                                        
         PRINT ON                                                               
* PPQD                                                                          
         PRINT OFF                                                              
       ++INCLUDE PPQD                                                           
         PRINT ON                                                               
                                                                                
* REGENREQ/REGENREQ2                                                            
         PRINT OFF                                                              
REPREQD  DSECT                                                                  
*PREFIX=RE                                                                      
       ++INCLUDE REGENREQ                                                       
       ++INCLUDE REGENREQ2                                                      
         PRINT ON                                                               
*PREFIX=                                                                        
*&&                                                                             
                                                                                
ACCPRFD  DSECT                     ** ACCPAK GETPROF KEY **                     
ACCPSYS  DS    CL1                 SYSTEM                                       
ACCPSYSQ EQU   C'A'                                                             
         DS    XL1                 N/D                                          
ACCPPRG  DS    CL2                 PROGRAM                                      
ACCPPRGQ EQU   C'LK'                                                            
         DS    XL1                 N/D                                          
ACCPUL   DS    CL2                 UNIT/LEDGER                                  
         DS    XL3                 N/D                                          
ACCPIND  DS    CL1                                                              
ACCPINDO EQU   C'*'                OLD OFFICE INDICATOR                         
ACCPINDN EQU   C'+'                NEW OFFICE INDICATOR                         
ACCPOFFO DS    CL1                 OLD OFFICE CODE                              
ACCPAGY  DS    CL2                 AGENCY ALPHA ID                              
ACCPOFFN DS    CL2                 NEW OFFICE CODE                              
ACCPRFL  EQU   *-ACCPRFD                                                        
                                                                                
SPTPRFD  DSECT                     ** SPOTPAK GETPROF KEY **                    
SPTPSYS  DS    CL1                 SYSTEM                                       
SPTPSYSQ EQU   C'S'                                                             
         DS    XL1                 N/D                                          
SPTPPRG  DS    CL2                 PROGRAM                                      
SPTPPRGQ EQU   C'LK'                                                            
SPTPAMC  DS    0CL(L'SPTPAGY+L'SPTPMED+L'SPTPCLT)                               
SPTPAGY  DS    CL2                 AGENCY ALPHA ID                              
SPTPMED  DS    CL1                 MEDIA CODE                                   
SPTPCLT  DS    CL3                 CLIENT CODE                                  
SPTPIND  DS    CL1                 OFFICE INDICATOR                             
SPTPOFFI EQU   C'*'                OFFICE CODE PASSED                           
SPTPOFF  DS    CL1                 OFFICE CODE                                  
SPTPRFL  EQU   *-SPTPRFD                                                        
                                                                                
PRTPRFD  DSECT                     ** PRINTPAK GETPROF KEY **                   
PRTPSYS  DS    CL1                 SYSTEM                                       
PRTPSYSQ EQU   C'P'                                                             
         DS    XL1                 N/D                                          
PRTPPRG  DS    CL2                 PROGRAM                                      
PRTPPRGQ EQU   C'LK'                                                            
PRTPAGY  DS    CL2                 AGENCY ALPHA ID                              
         DS    XL4                 N/D                                          
PRTPIND  DS    CL1                 OFFICE INDICATOR                             
PRTPOFFI EQU   C'*'                OFFICE CODE PASSED                           
PRTPOFF  DS    CL1                 OFFICE CODE                                  
PRTPRFL  EQU   *-PRTPRFD                                                        
                                                                                
* DDPERVERTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDPERVERTD                                                     
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023GERLP04   02/19/15'                                      
         END                                                                    
