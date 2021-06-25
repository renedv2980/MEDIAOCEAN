*          DATA SET GERLP01    AT LEVEL 027 AS OF 01/26/21                      
*PHASE TF2D01A                                                                  
                                                                                
***********************************************************************         
* MODULE HANDLES THE FOLLOWING FUNCTIONS:-                            *         
*                                                                     *         
* GROUP/ADD       USES X'FD' SCREEN - GRP PREFIXES - CODE=GRPADD      *         
* GROUP/CHANGE    USES X'FD' SCREEN - GRP PREFIXES - CODE=GRPCHA      *         
* GROUP/DISPLAY   USES X'FD' SCREEN - GRP PREFIXES - CODE=GRPDIS      *         
* GROUP/DELETE    USES X'FD' SCREEN - GRP PREFIXES - CODE=GRPDEL      *         
* GROUP/LIST      USES X'FE' SCREEN - GLI PREFIXES - CODE=GRPLST      *         
*                                                                     *         
* XFILE/ADD       USES X'FD' SCREEN - GRP PREFIXES - CODE=GRPADD      *         
* XFILE/CHANGE    USES X'FD' SCREEN - GRP PREFIXES - CODE=GRPCHA      *         
* XFILE/DISPLAY   USES X'FD' SCREEN - GRP PREFIXES - CODE=GRPDIS      *         
* XFILE/DELETE    USES X'FD' SCREEN - GRP PREFIXES - CODE=GRPDEL      *         
* XFILE/LIST      USES X'FE' SCREEN - GLI PREFIXES - CODE=GRPLST      *         
*                                                                     *         
***********************************************************************         
                                                                                
RLP01    TITLE '- GROUP RECORD HANDLING'                                        
RLP01    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RLP1**,R8,R7,RR=RE                                           
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA                                                          
         USING LSTTABD,CSLSTCUR    ALWAYS USE CURRENT LIST ENTRY                
         L     R3,ARFPIOB                                                       
         USING RFPD,R3             R3=A(RFP INTERFACE BLOCK)                    
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
                                                                                
         ST    RE,OVRELO                                                        
         ST    RB,OVBASE1                                                       
         ST    R8,OVBASE2                                                       
         ST    R7,OVBASE3                                                       
                                                                                
         BASR  R1,0                                                             
         AHI   R1,DISTAB-*                                                      
         ST    R1,OVADDR1          SAVE A(DISTAB) FOR GLOVAL ROUTINES           
                                                                                
         L     R1,AMIXNTRY                                                      
         SRL   RF,32-8                                                          
         LTR   RF,RF                                                            
         BNZ   GRPNTRY                                                          
         EJECT                                                                  
***********************************************************************         
* REGULAR ENTRY POINT FOR RECORD/ACTION                               *         
***********************************************************************         
                                                                                
         ICM   RF,1,MIXROUT-MIXTABD(R1)                                         
         CHI   RF,GRPROUTM                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     GRPROUTS(RF)                                                     
                                                                                
GRPROUTS DS    0XL4                                                             
                                                                                
         B     GRPLST              GROUP/LIST & XFILE/LIST                      
         B     GRPADD              GROUP/ADD & XFILE/ADD                        
         B     GRPCHA              GROUP/CHANGE & XFILE/CHANGE                  
         B     GRPDIS              GROUP/DISPLAY & XFILE/DISPLAY                
         B     GRPDEL              GROUP/DELETE & XFILE/DELETE                  
         B     GRPBLD              GROUP/BUILD & XFILE/BUILD                    
                                                                                
GRPROUTM EQU   (*-GRPROUTS)/L'GRPROUTS                                          
                                                                                
***********************************************************************         
* RETURN TO POINT AFTER NTRSES                                        *         
***********************************************************************         
                                                                                
GRPNTRY  BCTR  RF,0                                                             
         CHI   RF,GRPNTRYM                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     GRPNTRYS(RF)                                                     
                                                                                
GRPNTRYS DS    0XL4                                                             
                                                                                
         B     GLDISRET            RETURN FROM REQUEST/DISPLAY ETC.             
         B     GLDELRET            RETURN FROM REQUEST/DELETE                   
                                                                                
GRPNTRYM EQU   (*-GRPNTRYS)/L'GRPNTRYS                                          
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD GROUP LIST ENTRY FOR OTHER OVERLAYS                           *         
***********************************************************************         
                                                                                
GRPBLD   GOTOR LSTBLD,1            BUILD LIST TABLE ENTRY                       
         GOTOR AXITSES             AND RETURN TO CALLER                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY GROUP RECORDS                                               *         
* EXIT - CC=EQUAL IF RECORD DISPLAYED SUCCESSFULLY                    *         
***********************************************************************         
                                                                                
GRPDISN  NTR1  ,                   ** CALLED FROM OTHER ACTIONS **              
                                                                                
GRPDIS   XC    OVWORK1(L'GRPCODE),OVWORK1                                       
         CLI   CSACT,ACTADD                                                     
         BE    GRPDIS02                                                         
         CLI   TWASCRN,GRPDISSQ                                                 
         BNE   GRPDIS04                                                         
         TM    GRPCODEH+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    GRPDIS04                                                         
         MVC   OVWORK1(L'GRPCODE),GRPCODE                                       
GRPDIS02 XC    LSTTABD(LSTTABL),LSTTABD                                         
                                                                                
GRPDIS04 MVI   OVBYTE1,0           X'80'=RECORD IS DELETED                      
         LA    R2,IOKEY                                                         
         CLC   LSTTRTYP,CSREC                                                   
         BE    GRPDIS18                                                         
         TM    CSINDSL1,CSIUSELC   TEST NESTED (LIST) CALL                      
         BNZ   GRPDIS18                                                         
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   GRPDIS06                                                         
         GOTOR BLDSCR              NO - LOAD SCREEN AND EXIT                    
         OC    OVWORK1(L'GRPCODE),OVWORK1                                       
         BZ    GRPDISX                                                          
         MVC   GRPCODE,OVWORK1                                                  
                                                                                
GRPDIS06 MVC   CUSYSL,ASSYSL       SET CONNECTED SYSTEM AS DEFAULT              
         TM    GRPSYSTH+(FVATRB-FVIHDR),FVAPROT                                 
         BNZ   GRPDIS08                                                         
         GOTOR AVALSYS,GRPSYSTH    VALIDATE SYSTEM FIELD                        
         BNE   GRPDISX                                                          
         MVC   CUSYSL,PCWORK       SET INPUT SYSTEM                             
                                                                                
GRPDIS08 MVI   FVMINL,1                                                         
         GOTOR AFVAL,GRPCODEH                                                   
         BNE   GRPDISX                                                          
         CLI   CSREC,RECGRP        TEST GROUP RECORDS                           
         BNE   GRPDIS14                                                         
         CLI   FVIFLD,GRPKDTFQ     TEST RESERVED DESKTOP PREFIX                 
         BNE   GRPDIS10                                                         
         TM    CUSTAT,CUSDDS                                                    
         BNZ   GRPDIS10                                                         
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     GRPDISX                                                          
                                                                                
         USING GRPKEYD,R2          VALIDATE RFP GROUP                           
GRPDIS10 XC    GRPKEY,GRPKEY                                                    
         MVI   GRPKSYS,GRPKSYSQ                                                 
         MVI   GRPKSTYP,GRPKSTYQ                                                
         MVC   GRPKSYST,CUSYSL                                                  
         MVC   GRPKAGY,TWAAGY                                                   
         GOTOR GETUSR,GRPKUSER                                                  
         MVC   GRPKGRP,FVIFLD                                                   
         GOTOR AIO,'IOHIGH+IOGENDIS+IO1'                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GRPKEY(GRPKELEM-GRPKEYD),IOKEYSAV                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     GRPDISX                                                          
         GOTOR AIO,'IOGET+IOGENFIS+IO1'                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     GRPDIS24                                                         
                                                                                
         USING XFILED,R2           VALIDATE XFILE GROUPS                        
GRPDIS14 CLI   CSREC,RECXFG                                                     
         BE    *+6                                                              
         DC    H'0'                UNKNOWN RECORD TYPE                          
         XC    XFKEY,XFKEY         BUILD XFILE RECORD KEY                       
         MVI   XFKSYS,XFKSYSQ                                                   
         MVI   XFKSTYP,XFKSTYPQ                                                 
         MVC   XFAGY,TWAAGY                                                     
         GOTOR GETUSR,XFUSER                                                    
         TM    CSINDSL1,CSIUSELC   TEST NESTED LIST CALL                        
         BZ    *+16                                                             
         MVC   XFAGY,GRPLAGY                                                    
         MVC   XFUSER,GRPLUSER                                                  
         MVC   XFGRP,FVIFLD                                                     
         LHI   R1,IOREAD+IOGENDIS+IO1                                           
         CLI   CSACT,ACTDIS                                                     
         BE    *+8                                                              
         AHI   R1,IOLOCK           LOCK THE RECORD IF CHANGE/DELETE             
         GOTOR AIO,(R1)                                                         
         BE    GRPDIS16                                                         
         BNL   *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     GRPDISX                                                          
GRPDIS16 LHI   R1,IOGET+IOGENFIS+IO1                                            
         CLI   CSACT,ACTDIS                                                     
         BE    *+8                                                              
         AHI   R1,IOLOCK                                                        
         GOTOR AIO,(R1)                                                         
         BE    GRPDIS24                                                         
         DC    H'0'                                                             
                                                                                
GRPDIS18 GOTOR BLDSCR              LOAD THE DISPLAY SCREEN                      
         CLI   LSTTRTYP,RECXFG     TEST XFILE RECORD                            
         BNE   GRPDIS20                                                         
         MVC   IODAOVER,LSTTDA     YES - READ THE RECORD                        
         LHI   R1,IOGET+IOGENFIS+IO1                                            
         CLI   CSACT,ACTDIS                                                     
         BE    *+8                                                              
         AHI   R1,IOLOCK                                                        
         GOTOR AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         MVC   IOKEY,XFKEY                                                      
         LHI   R1,IOREAD+IOGENDIS+IO1                                           
         CLI   CSACT,ACTDIS                                                     
         BE    *+8                                                              
         AHI   R1,IOLOCK                                                        
         GOTOR AIO                                                              
         B     GRPDIS24                                                         
                                                                                
         USING GRPKEYD,R2                                                       
GRPDIS20 MVI   GRPKSYS,GRPKSYSQ                                                 
         MVI   GRPKSTYP,GRPKSTYQ                                                
         MVC   GRPKSYST,GRPLSYST                                                
         MVC   GRPKAGY,GRPLAGY                                                  
         MVC   GRPKUSER,GRPLUSER                                                
         MVC   GRPKGRP,GRPLGRP                                                  
         LHI   R1,IOHIGH+IOGENDIS+IO1                                           
         CLI   CSACT,ACTDIS                                                     
         BE    *+8                                                              
         AHI   R1,IOLOCK                                                        
         GOTOR AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GRPKEY(GRPKELEM-GRPKEY),IOKEYSAV                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     GRPDISX                                                          
         LHI   R1,IOGET+IOGENFIS+IO1                                            
         CLI   CSACT,ACTDIS                                                     
         BE    *+8                                                              
         AHI   R1,IOLOCK                                                        
         GOTOR AIO,(R1)                                                         
         BE    GRPDIS24                                                         
         DC    H'0'                                                             
                                                                                
GRPDIS24 L     R2,AIO1                                                          
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   GRPDIS36                                                         
         MVC   GRPCODE,GRPKGRP                                                  
         MVC   CUUSER,GRPKUSER     CALL RFPIO TO VALIDATE GROUP                 
         MVC   CUAALF,GRPKAGY                                                   
         MVC   CUSYSL,GRPKSYST                                                  
         LA    R1,GRPKGRP                                                       
         CLI   CSACT,ACTDIS                                                     
         BE    *+8                                                              
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP                                                          
         BNH   *+6                                                              
         DC    H'0'                                                             
         TM    RFPVGSTA,RFPVGSDQ   TEST GROUP IS DELETED                        
         BZ    *+8                                                              
         OI    OVBYTE1,X'80'                                                    
                                                                                
         GOTOR AGETAGY,GRPKAGY     GET AGENCY NAME                              
         MVC   GRPAGY,GRPKAGY                                                   
         OI    GRPAGYH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   GRPAGYN,PCWORK                                                   
         OI    GRPAGYNH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
         GOTOR AGETUID,GRPKUSER    GET USER-ID DETAILS                          
*        BE    GRPDIS25                                                         
*        MVC   PCWORK+(GIDCODE-GIDTABD),=CL10'UNKNOWN'                          
*        MVC   PCWORK+(GIDNAME-GIDTABD),=CL33'RECORD NOT FOUND'                 
GRPDIS25 MVC   GRPUSER,PCWORK+(GIDCODE-GIDTABD)                                 
         OI    GRPUSERH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   GRPUSEN,PCWORK+(GIDNAME-GIDTABD)                                 
         OI    GRPUSENH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
         CLI   CSACT,ACTCHA        TEST GROUP/CHANGE                            
         BNE   GRPDIS26                                                         
         OC    RFPXFILE,RFPXFILE   TEST XFILE POINTER ON GROUP                  
         BZ    GRPDIS26                                                         
         OI    GRPFREQH+(FVATRB-FVIHDR),FVAPROT                                 
                                                                                
GRPDIS26 TM    GRPSYSTH+(FVATRB-FVIHDR),FVAPROT                                 
         BZ    GRPDIS28                                                         
         GOTOR AGETSYS,GRPKSYST    GET SYSTEM NAME                              
         MVC   GRPSYST,PCWORK                                                   
         OI    GRPSYSTH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
GRPDIS28 LA    R4,GRPFSTEL                                                      
         USING GRPHD,R4            R4=A(FIRST ELEMENT ON RECORD)                
         SR    R0,R0                                                            
GRPDIS30 CLI   GRPHCD,0            TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   GRPHCD,GRPHCDQ      TEST GROUP HEADER ELEMENT                    
         BE    *+14                                                             
         IC    R0,GRPHLN           BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         B     GRPDIS30                                                         
*&&UK                                                                           
         OI    GRPPRVTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    GRPPRVT,GRPPRVT     PRIVATE?                                     
         MVC   GRPPRVT(L'PCUNO),PCUNO                                           
         TM    GRPHSTAT,GRPHSPVT                                                
         BZ    *+10                                                             
         MVC   GRPPRVT(L'PCUYES),PCUYES                                         
*&&                                                                             
         OI    GRPDESCH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   GRPDESC,GRPHDESC    DESCRIPTION                                  
                                                                                
         OI    GRPFREQH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTOR AGETFRQ,GRPHFREQ    FREQUENCY                                    
         MVC   GRPFREQ(FRQNAMLQ),PCWORK                                         
                                                                                
         OI    GRPOTYPH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   GRPOTYP,GRPHOTYP    OUTPUT TYPE                                  
                                                                                
         OI    GRPDESTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    GRPDEST,GRPDEST                                                  
         OC    GRPHDEST,GRPHDEST   DESTINATION ID                               
         BZ    GRPDIS32                                                         
         GOTOR AGETUID,GRPHDEST                                                 
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVC   GRPDEST,PCWORK+(GIDCODE-GIDTABD)                                 
         MVC   GRPDESN,PCWORK+(GIDNAME-GIDTABD)                                 
                                                                                
GRPDIS32 OI    GRPNAMEH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   GRPNAME,GRPHNAME    FILTER NAME                                  
                                                                                
         OI    GRPXFGH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   GRPXFG,GRPHXFIL     XFILE GROUP CODE                             
                                                                                
         CLI   CSACT,ACTADD        DON'T DO RUN DATES FOR ADD                   
         BE    GRPDIS54                                                         
         GOTOR AGETNXT             GET NEXT RUN DATE FOR GROUP                  
         BNE   GRPDIS34                                                         
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',GRPNXTRH),('SOFIIONE',0)                    
                                                                                
GRPDIS34 GOTOR AGETLST             GET LAST RUN DATE FOR GROUP                  
         BNE   GRPDIS54                                                         
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',GRPLSTRH),('SOFIIONE',0)                    
         B     GRPDIS54                                                         
                                                                                
         USING XFILED,R2                                                        
GRPDIS36 CLI   CSREC,RECXFG        TEST XFILE RECORD                            
         BE    *+6                                                              
         DC    H'0'                UNKNOWN RECORD TYPE                          
                                                                                
         GOTOR AXFGRFP             COPY VALUES INTO RFP BLOCK                   
                                                                                
         MVC   GRPCODE,XFGRP       SET GROUP                                    
                                                                                
         GOTOR AGETAGY,XFAGY       GET AGENCY NAME                              
         MVC   GRPAGY,XFAGY                                                     
         OI    GRPAGYH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   GRPAGYN,PCWORK                                                   
         OI    GRPAGYNH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
         GOTOR AGETUID,XFUSER      GET USER-ID DETAILS                          
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVC   GRPUSER,PCWORK+(GIDCODE-GIDTABD)                                 
         OI    GRPUSERH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   GRPUSEN,PCWORK+(GIDNAME-GIDTABD)                                 
         OI    GRPUSENH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
GRPDIS38 LA    R4,XFFRSTEL                                                      
         USING XFPHD,R4            R4=A(FIRST ELEMENT ON RECORD)                
         SR    R0,R0                                                            
GRPDIS40 CLI   XFPHCD,0            TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   XFPHCD,XFPHCDQ      TEST GROUP HEADER ELEMENT                    
         BE    *+14                                                             
         IC    R0,XFPHLN           BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         B     GRPDIS40                                                         
                                                                                
         OI    GRPDESCH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   GRPDESC,XFPHDESC    DESCRIPTION                                  
                                                                                
         OI    GRPFREQH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTOR AGETFRQ,XFPHFREQ    FREQUENCY                                    
         MVC   GRPFREQ(FRQNAMLQ),PCWORK                                         
                                                                                
         OI    GRPOTYPH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   GRPOTYP,XFPHOTYP    OUTPUT TYPE                                  
                                                                                
         OI    GRPDESTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    GRPDEST,GRPDEST                                                  
         OC    XFPHDEST,XFPHDEST   DESTINATION ID                               
         BZ    GRPDIS42                                                         
         GOTOR AGETUID,XFPHDEST                                                 
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVC   GRPDEST,PCWORK+(GIDCODE-GIDTABD)                                 
         MVC   GRPDESN,PCWORK+(GIDNAME-GIDTABD)                                 
                                                                                
GRPDIS42 OI    GRPNAMEH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   GRPNAME,XFPHNAME    FILTER NAME                                  
                                                                                
         GOTOR AGETNXT             GET NEXT RUN DATE FOR GROUP                  
         BNE   GRPDIS44                                                         
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',GRPNXTRH),('SOFIIONE',0)                    
                                                                                
GRPDIS44 GOTOR AGETLST             GET LAST RUN DATE FOR GROUP                  
         BNE   GRPDIS46                                                         
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',GRPLSTRH),('SOFIIONE',0)                    
                                                                                
         USING XFSGRPD,R4                                                       
GRPDIS46 IC    R0,XFSGLN           BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         CLI   XFSGCD,0            TEST END OF RECORD                           
         BE    GRPDIS54                                                         
         CLI   XFSGCD,XFSGCDEQ     TEST SYSTEM/GROUP ELEMENT                    
         BNE   GRPDIS46                                                         
                                                                                
         GOTOR AGETSYS,XFSGSYS     LOOK UP SYSTEM NAME                          
         LA    RE,GRPSYS1H                                                      
         USING GRPSYS1H,RE                                                      
         LHI   RF,SYSLSTN          N'SYSTEMS ON SCREEN                          
GRPDIS48 CLC   GRPSYS1(3),PCWORK                                                
         BE    GRPDIS50                                                         
         LA    RE,GRPSYS2H                                                      
         BCT   RF,GRPDIS48                                                      
         B     GRPDIS46            UNKNOWN SYSTEM - IGNORE                      
                                                                                
GRPDIS50 OI    GRPGRP1H+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R1,GRPGRP1                                                       
         CLI   0(R1),SPACE                                                      
         BNH   GRPDIS52                                                         
         LA    R1,GRPGRP1+L'GRPGRP1-1                                           
         CLI   0(R1),SPACE                                                      
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   1(L'PCCOMMA,R1),PCCOMMA                                          
         AHI   R1,L'PCCOMMA+1                                                   
GRPDIS52 MVC   0(L'XFSGGRP,R1),XFSGGRP                                          
         B     GRPDIS46                                                         
         DROP  RE                                                               
                                                                                
GRPDIS54 TM    CSINDSL1,CSIUSELC   TEST NESTED LIST CALL                        
         BNZ   GRPDIS56                                                         
         GOTOR LSTBLD,0            NO - BUILD A LIST ENTRY                      
                                                                                
GRPDIS56 LA    R0,GRPCODEH         POSITION CURSOR TO FIRST KEY FIELD           
         TM    GRPSYSTH+(FVATRB-FVIHDR),FVAPROT                                 
         BNZ   *+8                                                              
         LA    R0,GRPSYSTH                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$SDHEN)                                           
         MVI   FVOMTYP,GTMINF                                                   
         TM    OVBYTE1,X'80'       TEST RECORD IS DELETED                       
         BZ    GRPDISX                                                          
         MVC   FVMSGNO,=AL2(GE$RID)                                             
         MVI   FVOMTYP,GTMERR                                                   
                                                                                
GRPDISX  CLC   FVMSGNO,=AL2(GI$SDHEN)                                           
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOAD MAINTENANCE SCREEN AND SET FIELDS                   *         
***********************************************************************         
                                                                                
BLDSCR   LR    R0,RE               LOAD GROUP/DISPLAY SCREEN                    
         GOTOR AOVRSCR,PCPARM,('GRPDISSQ',RLPOLY1H)                             
                                                                                
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    *+8                                                              
         OI    GRPCODEH+(FVATRB-FVIHDR),FVAPROT                                 
                                                                                
         CLI   CSACT,ACTADD        TEST ACTION IS ADD                           
         BNE   *+16                                                             
         XC    GRPLSTL,GRPLSTL     CLEAR LAST/NEXT RUN LABELS                   
         XC    GRPNXTL,GRPNXTL                                                  
                                                                                
         CLI   CSACT,ACTADD        IF ACTION IS ADD OR CHANGE                   
         BE    *+12                THE FREQUENCY IS UPPER CASE                  
         CLI   CSACT,ACTCHA        AND FIELDS ARE UNPROTECTED                   
         BNE   BLDSCR02                                                         
         NI    GRPDESCH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
*&&UK*&& NI    GRPPRVTH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         NI    GRPFREQH+(FVATRB-FVIHDR),FF-(FVAPROT+FVALOWC)                    
         NI    GRPOTYPH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         NI    GRPDESTH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         NI    GRPNAMEH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
                                                                                
BLDSCR02 CLI   CSREC,RECGRP        TEST RFP GROUP                               
         BNE   BLDSCR06                                                         
                                                                                
         XC    GRPSYSH,GRPSYSH     CLEAR SYSTEM AND GROUP HEADERS               
         XC    GRPGRPH,GRPGRPH                                                  
                                                                                
         CLI   CSACT,ACTADD        CLEAR XFILE LABEL IF ADDING                  
         BNE   *+10                                                             
         XC    GRPXFGL,GRPXFGL                                                  
                                                                                
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BNZ   BLDSCRX                                                          
         LA    RF,GRPCODEH         POSITION CURSOR IF NOT NESTED                
         CLI   ASSYSL,CONLETQ      TEST CONTROL SYSTEM                          
         BNE   *+12                                                             
         NI    GRPSYSTH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         LA    RF,GRPSYSTH         ASK FOR SYSTEM                               
         ST    RF,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EKEY)                                            
         MVI   FVOMTYP,GTMINF                                                   
         B     BLDSCRX                                                          
                                                                                
BLDSCR06 CLI   CSREC,RECXFG        TEST XFILE GROUP                             
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    GRPSYSL,GRPSYSL     CLEAR SYSTEM AND XFILE LITERALS              
         XC    GRPXFGL,GRPXFGL                                                  
         GOTOR SETSYS              MOVE SYSTEM NAMES TO SCREEN                  
         CLI   CSACT,ACTCHA                                                     
         BE    *+12                                                             
         CLI   CSACT,ACTADD                                                     
         BNE   BLDSCR08                                                         
         NI    GRPGRP1H+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         NI    GRPGRP2H+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         NI    GRPGRP3H+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         NI    GRPGRP4H+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         NI    GRPGRP5H+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         NI    GRPGRP6H+(FVATRB-FVIHDR),FF-(FVAPROT)                            
                                                                                
BLDSCR08 TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BNZ   BLDSCRX                                                          
         LA    RF,GRPCODEH         POSITION CURSOR IF NOT NESTED                
         ST    RF,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EKEY)                                            
         MVI   FVOMTYP,GTMINF                                                   
                                                                                
BLDSCRX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE GROUP RECORDS                                                *         
***********************************************************************         
                                                                                
         USING GDWORKD,RC                                                       
GRPDEL   CLC   LSTTRTYP,CSREC                                                   
         BE    *+12                                                             
         TM    CSINDSL1,CSIUSELC   TEST NESTED LIST CALL                        
         BZ    *+10                                                             
         MVC   LGRP,GRPLGRP                                                     
                                                                                
         TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BZ    GRPDEL02                                                         
         TM    GRPCODEH+(FVIIND-FVIHDR),FVITHIS                                 
         BNZ   GRPDEL02                                                         
         TM    GRPSYSTH+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    GRPDEL04                                                         
                                                                                
GRPDEL02 GOTOR GRPDISN             CALL GROUP/DISPLAY                           
         BNE   EXIT                                                             
         MVI   FVOMTYP,0                                                        
         GOTOR CHKDEL              CHECK IT'S OKAY TO DELETE GROUP              
         BNE   EXIT                                                             
         MVC   FVMSGNO,=AL2(GI$RDHED)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIRDSPC   SET RECORD DISPLAYED FOR CHANGE              
         MVC   LGRP,GRPCODE                                                     
         B     EXIT                                                             
                                                                                
GRPDEL04 CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   GRPDEL08                                                         
         GOTOR GETUSR,CUUSER                                                    
         MVC   CUAALF,TWAAGY                                                    
         MVC   CUSYSL,ASSYSL                                                    
         TM    CSINDSL1,CSIUSELC                                                
         BZ    GRPDEL06                                                         
         MVC   CUUSER,GRPLUSER     CALL RFPIO TO VALIDATE GROUP                 
         MVC   CUAALF,GRPLAGY                                                   
         MVC   CUSYSL,GRPLSYST                                                  
GRPDEL06 LA    R1,LGRP                                                          
         ICM   R1,8,=AL1(RFPXSYMS) SPECIAL CALL                                 
         GOTOR AVALGRP                                                          
         BE    GRPDEL10                                                         
         CLI   RFPERROR,RFPNOGRP                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     EXIT                                                             
                                                                                
GRPDEL08 CLI   CSREC,RECXFG        TEST XFILE GROUP RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         USING XFILED,R2                                                        
         XC    XFKEY,XFKEY         BUILD XFILE RECORD KEY                       
         MVI   XFKSYS,XFKSYSQ                                                   
         MVI   XFKSTYP,XFKSTYPQ                                                 
         MVC   XFAGY,TWAAGY                                                     
         GOTOR GETUSR,XFUSER                                                    
         TM    CSINDSL1,CSIUSELC   TEST NESTED LIST CALL                        
         BZ    *+16                                                             
         MVC   XFAGY,GRPLAGY                                                    
         MVC   XFUSER,GRPLUSER                                                  
         MVC   XFGRP,LGRP                                                       
         GOTOR AIO,'IORDUP+IOGENDIS+IO1'                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     EXIT                                                             
         GOTOR AIO,'IOGETRUP+IOGENFIS+IO1'                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GRPDEL10 GOTOR CHKDEL              CHECK IT'S OKAY TO DELETE GROUP              
         BNE   EXIT                                                             
                                                                                
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   GRPDEL12                                                         
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPDELGP    SET TO DELETE GROUP                          
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   DIE IF ANY ERRORS                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     GRPDELX                                                          
                                                                                
GRPDEL12 CLI   CSREC,RECXFG        TEST GROUP RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         USING XFILED,R2                                                        
         LA    R4,XFFRSTEL                                                      
         USING XFSGRPD,R4                                                       
         SR    R0,R0                                                            
GRPDEL14 IC    R0,XFSGLN           BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         CLI   XFSGCD,0            TEST END OF RECORD                           
         BE    GRPDEL16                                                         
         CLI   XFSGCD,XFSGCDEQ     TEST SYSTEM/GROUP ELEMENT                    
         BNE   GRPDEL14                                                         
                                                                                
         GOTOR GETUSR,CUUSER                                                    
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
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   RFPXFILE,LGRP       ENSURE THIS GROUP POINTS TO XFILE            
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    RFPXFILE,RFPXFILE   DETACH GROUP                                 
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPSAVGP    SAVE THE GROUP                               
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   DIE IF ANY ERRORS                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XFSGCD,FF           SET THIS ELEMENT TO BE DELETED               
         B     GRPDEL14                                                         
                                                                                
GRPDEL16 GOTOR VHELLO,PCPARM,(C'D',GENFIL),('FF',AIO1),0,0                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1             DELETE GENFIL RECORD                         
         XC    XFSTAT,XFSTAT                                                    
         OI    XFSTAT,X'80'                                                     
         GOTOR AIO,'IOPUT+IOGENFIS+IO1'                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,IOKEY            DELETE GENDIR RECORD                         
         XC    XFKSTAT,XFKSTAT                                                  
         OI    XFKSTAT,X'80'                                                    
         GOTOR AIO,'IOWRITE+IOGENDIS+IO1'                                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GRPDELX  MVC   FVMSGNO,=AL2(GI$RXENR)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R0,GRPCODEH                                                      
         TM    GRPSYSTH+(FVATRB-FVIHDR),FVAPROT                                 
         BNZ   *+8                                                              
         LA    R0,GRPSYSTH                                                      
         ST    R0,FVADDR           SET CURSOR TO FIRST KEY FIELD                
         TM    CSINDSL1,CSIUSELC   TEST NESTED (LIST) CALL                      
         BZ    EXIT                                                             
         SR    RE,RE                                                            
         IC    RE,CSREC                                                         
         SLL   RE,1                                                             
         LA    RE,TWAGLIND-L'TWAGLIND(RE)                                       
         OI    0(RE),TWAGLIRL      SET REFRESH LIST PENDING                     
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTOR AXITSES                                                          
         DROP  RC                                                               
                                                                                
GDWORKD  DSECT                     ** GRPDEL LOCAL W/S **                       
                                                                                
GDNEWXFG DS    (XFGTABM)XL(XFGTABL)                                             
                                                                                
RLP01    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK ACTION DELETE IS OKAY FOR A GROUP                             *         
***********************************************************************         
                                                                                
CHKDEL   NTR1  ,                                                                
                                                                                
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   CHKDEL02                                                         
         OC    RFPXFILE,RFPXFILE   TEST GROUP ATTACHED TO XFILE                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$CXR)                                             
         B     CHKDELN                                                          
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         XC    RFPFRQID(RFPFNUMR-RFPFRQID),RFPFRQID                             
         MVI   RFPMODE,RFPRETRQ    CHECK GROUP HAS REQUESTS                     
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RFPRET,RFPEOF       OKAY TO DELETE IF NO REQUESTS                
         BE    CHKDELY                                                          
         MVC   FVMSGNO,=AL2(GE$CXGHR)                                           
         B     CHKDELN                                                          
                                                                                
CHKDEL02 CLI   CSREC,RECXFG        CHECK DELETE FOR XFILE GROUP OKAY            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
CHKDELY  CLI   *+1,0               CC=EQUAL IF OKAY TO DELETE GROUP             
         B     EXIT                                                             
CHKDELN  CLI   *+0,0               CC=NOT EQUAL IF NOT OKAY TO DELETE           
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ADD GROUP RECORDS                                                   *         
***********************************************************************         
                                                                                
         USING GAWORKD,RC          RC=A(LOCAL W/S)                              
GRPADD   TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   GRPADD04                                                         
         GOTOR BLDSCR              LOAD IN THE GROUP SCREEN                     
                                                                                
         GOTOR AGETAGY,TWAAGY      GET AGENCY NAME                              
         MVC   GRPAGY,TWAAGY                                                    
         OI    GRPAGYH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   GRPAGYN,PCWORK                                                   
         OI    GRPAGYNH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
         GOTOR AGETUID,TWAUSRID    GET USER-ID DETAILS                          
         MVC   GRPUSER,PCWORK+(GIDCODE-GIDTABD)                                 
         OI    GRPUSERH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   GRPUSEN,PCWORK+(GIDNAME-GIDTABD)                                 
         OI    GRPUSENH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
         LA    R0,GRPCODEH                                                      
         CLI   CSREC,RECXFG        XFILE DOESN'T HAVE SYSTEM FIELD              
         BE    GRPADD02                                                         
         CLI   ASSYSL,CONLETQ      TEST CONTROL SYSTEM                          
         BNE   *+16                                                             
         NI    GRPSYSTH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         LA    R0,GRPSYSTH         ASK FOR SYSTEM                               
         B     GRPADD02                                                         
                                                                                
         GOTOR AGETSYS,ASSYSL      GET SYSTEM NAME                              
         MVC   GRPSYST,PCWORK                                                   
         OI    GRPSYSTH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
GRPADD02 ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EKEYD)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
                                                                                
GRPADD04 MVC   CUSYSL,ASSYSL       SET CONNECTED SYSTEM AS DEFAULT              
         TM    GRPSYSTH+(FVATRB-FVIHDR),FVAPROT                                 
         BNZ   GRPADD06                                                         
         GOTOR AVALSYS,GRPSYSTH    VALIDATE SYSTEM FIELD                        
         BNE   EXIT                                                             
         MVC   CUSYSL,PCWORK       SET INPUT SYSTEM                             
                                                                                
GRPADD06 MVI   FVMINL,1            VALIDATE GROUP CODE                          
         GOTOR AFVAL,GRPCODEH                                                   
         BNE   EXIT                                                             
         CLI   FVIFLD,GRPKDTFQ     TEST RESERVED DESKTOP PREFIX                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         J     EXIT                                                             
         MVC   GAXFGGRP,FVIFLD     SET GROUP CODE                               
         MVC   LGRP,GAXFGGRP                                                    
                                                                                
         CLI   CSREC,RECGRP        TEST RFP GROUP RECORD                        
         BNE   GRPADD08                                                         
         MVC   CUUSER,TWAUSRID                                                  
         MVC   CUAALF,TWAAGY                                                    
         LA    R1,GAXFGGRP                                                      
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFPIO TO VALIDATE GROUP                 
         BL    EXIT                                                             
         BNE   GRPADD10                                                         
         MVC   FVMSGNO,=AL2(GE$RAOF)                                            
         B     EXIT                                                             
                                                                                
GRPADD08 CLI   CSREC,RECXFG        TEST XFILE GROUP NOT ON FILE                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         USING XFILED,R2                                                        
         XC    XFKEY,XFKEY         BUILD XFILE RECORD KEY                       
         MVI   XFKSYS,XFKSYSQ                                                   
         MVI   XFKSTYP,XFKSTYPQ                                                 
         MVC   XFAGY,TWAAGY                                                     
         MVC   XFUSER,TWAUSRID                                                  
         TM    CSINDSL1,CSIUSELC   TEST NESTED LIST CALL                        
         BZ    *+16                                                             
         MVC   XFAGY,GRPLAGY                                                    
         MVC   XFUSER,GRPLUSER                                                  
         MVC   XFGRP,GAXFGGRP                                                   
         GOTOR AIO,'IORDUPD+IOGENDIS+IO1'                                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECAE)                                           
         B     EXIT                                                             
         TM    IOERR,IOEDEL                                                     
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RID)                                             
         B     EXIT                                                             
         TM    IOERR,IOERNF        RECORD MUST BE NOT FOUND                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
GRPADD10 DS    0H                                                               
*&&UK*&& GOTOR VALPVT                                                           
                                                                                
         MVC   RFPVDESC,FVIFLD                                                  
         GOTOR AFVAL,GRPDESCH      VALIDATE DESCRIPTION                         
         MVC   RFPVDESC,FVIFLD                                                  
                                                                                
         GOTOR AVALFRQ,GRPFREQH    VALIDATE FREQUENCY                           
         BNE   EXIT                                                             
         MVC   RFPVFREQ,PCWORK                                                  
                                                                                
         GOTOR AVALOUT,GRPOTYPH    VALIDATE OUTPUT TYPE                         
         BH    EXIT                                                             
         MVC   RFPVOTYP,FVIFLD                                                  
                                                                                
         GOTOR AVALDST,GRPDESTH    VALIDATE DESTINATION ID                      
         BNE   EXIT                                                             
         MVC   RFPVDEST,PCWORK                                                  
                                                                                
         GOTOR AFVAL,GRPNAMEH      VALIDATE FILTER NAME                         
         MVC   RFPVNAME,FVIFLD                                                  
                                                                                
         CLI   CSREC,RECGRP        TEST RFP GROUP RECORD                        
         BNE   GRPADD16                                                         
         OI    RFPFFLAG,RFPFSYMS                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPSAVGP    SAVE THE GROUP                               
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   DIE IF ANY ERRORS                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     GRPADDX                                                          
                                                                                
GRPADD16 CLI   CSREC,RECXFG        TEST XFILE GROUP RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1             BUILD RECORD KEY                             
         USING XFILED,R2                                                        
         XC    XFILED(XFDISPQ),XFILED                                           
         MVI   XFKSYS,XFKSYSQ                                                   
         MVI   XFKSTYP,XFKSTYPQ                                                 
         MVC   XFAGY,TWAAGY                                                     
         MVC   XFUSER,TWAUSRID                                                  
         MVC   XFGRP,GAXFGGRP                                                   
                                                                                
         LA    RF,XFFRSTEL                                                      
         USING XFPHD,RF            BUILD HEADER ELEMENT                         
         XC    XFPHD(XFPHLNQ),XFPHD                                             
         MVI   XFPHCD,XFPHCDQ                                                   
         MVI   XFPHLN,XFPHLNQ                                                   
         MVC   XFPHDESC,RFPVDESC                                                
         MVC   XFPHFREQ,RFPVFREQ                                                
         MVC   XFPHOTYP,RFPVOTYP                                                
         MVC   XFPHDEST,RFPVDEST                                                
         MVC   XFPHNAME,RFPVNAME                                                
         MVC   XFPHNXTR,RFPVNXTR                                                
         MVC   XFPHEND,RFPVENDR                                                 
         AHI   RF,XFPHLNQ                                                       
                                                                                
         USING XFDRD,RF            BUILD DATE ELEMENT                           
         XC    XFDRD(XFDRLNQ),XFDRD                                             
         MVI   XFDRCD,XFDRCDEQ                                                  
         MVI   XFDRLN,XFDRLNQ                                                   
         AHI   RF,XFDRLNQ                                                       
                                                                                
         MVI   0(RF),0             SET END OF RECORD                            
         AHI   RF,1                                                             
         LA    RE,XFILED                                                        
         SR    RF,RE               CALCULATE & SET RECORD LENGTH                
         STCM  RF,3,XFLEN                                                       
         DROP  RF                                                               
                                                                                
         GOTOR BLDGRP,GANEWXFG     BUILD NEW LIST OF GROUPS                     
         BNE   EXIT                                                             
                                                                                
         LA    R4,GANEWXFG         R4=A(NEW GROUPS)                             
         USING XFGTABD,R4                                                       
         LA    R5,XFGTABM          R5=(MAXIMUM) NUMBER OF ENTRIES               
GRPADD18 CLI   XFGTSYS,0           TEST END OF NEW LIST                         
         BE    GRPADD20                                                         
                                                                                
         MVC   CUUSER,TWAUSRID     ATTACH XFILE TO GROUP                        
         MVC   CUAALF,TWAAGY                                                    
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    *+16                                                             
         MVC   CUUSER,GRPLUSER                                                  
         MVC   CUAALF,GRPLAGY                                                   
         MVC   CUSYSL,XFGTSYSL                                                  
         LA    R1,XFGTGRP                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFP TO VALIDATE THE GROUP               
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    RFPXFILE,RFPXFILE                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   RFPXFILE,GAXFGGRP   ATTACH XFILE GROUP CODE                      
         XC    RFPVNXTR,RFPVNXTR                                                
         XC    RFPVENDR,RFPVENDR                                                
         XC    RFPVRNGE,RFPVRNGE                                                
         XC    RFPVDAYS,RFPVDAYS                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPSAVGP    SAVE THE GROUP                               
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   DIE IF ANY ERRORS                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    RF,GAELEM           BUILD SYSTEM ELEMENT & ADD TO RECORD         
         USING XFSGRPD,RF                                                       
         XC    XFSGRPD(XFSGLNQ),XFSGRPD                                         
         MVI   XFSGCD,XFSGCDEQ                                                  
         MVI   XFSGLN,XFSGLNQ                                                   
         MVC   XFSGSYS,XFGTSYS                                                  
         MVC   XFSGSYSL,XFGTSYSL                                                
         MVC   XFSGGRP,XFGTGRP                                                  
         GOTOR VHELLO,PCPARM,(C'P',GENFIL),AIO1,XFSGRPD,0,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         AHI   R4,XFGTABL          BUMP TO NEXT ENTRY IN LIST                   
         BCT   R5,GRPADD18                                                      
         DROP  R4,RF                                                            
                                                                                
GRPADD20 GOTOR AIO,'IOADDREC+IOGENFIS+IO1'                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GRPADDX  CLI   CSREC,RECGRP        TEST JUST ADDED A GROUP RECORD               
         BNE   GRPADDX2                                                         
         GOTOR GRPDISN             CALL GROUP/DISPLAY                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GRPADDX2 LA    R0,GRPCODEH         CURSOR TO GROUP FIELD                        
         TM    GRPSYSTH+(FVATRB-FVIHDR),FVAPROT                                 
         BNZ   *+8                                                              
         LA    R0,GRPSYSTH         OR SYSTEM FIELD IF UNPROTECTED               
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$RADEN)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         DROP  RC                                                               
                                                                                
GAWORKD  DSECT                     ** GRPADD LOCAL W/S **                       
                                                                                
GAXFGGRP DS    CL(L'GRPKGRP)       XFILE GROUP CODE                             
GANEWXFG DS    (XFGTABM)XL(XFGTABL)                                             
GAELEM   DS    XL256               ELEMENT BUILD AREA                           
                                                                                
RLP01    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CHANGE GROUP RECORDS                                                *         
***********************************************************************         
                                                                                
         USING GCWORKD,RC          RC=A(LOCAL W/S)                              
GRPCHA   CLC   LSTTRTYP,CSREC                                                   
         BE    *+12                                                             
         TM    CSINDSL1,CSIUSELC   TEST NESTED LIST CALL                        
         BZ    *+10                                                             
         MVC   LGRP,GRPLGRP                                                     
                                                                                
         TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BZ    GRPCHA02                                                         
         TM    GRPCODEH+(FVIIND-FVIHDR),FVITHIS                                 
         BNZ   GRPCHA02                                                         
         TM    GRPSYSTH+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    GRPCHA04                                                         
                                                                                
GRPCHA02 GOTOR GRPDISN             CALL GROUP/DISPLAY                           
         BNE   EXIT                                                             
         MVI   FVOMTYP,0                                                        
         OI    CSINDSL1,CSIRDSPC   SET RECORD DISPLAYED FOR CHANGE              
*&&UK*&& LA    R0,GRPPRVTH         CURSOR TO PRIVATE FIELD                      
*&&US*&& LA    R0,GRPDESCH         CURSOR TO DESCRIPTION FIELD                  
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EDATA)                                           
         MVI   FVOMTYP,GTMINF                                                   
         MVC   LGRP,GRPLGRP                                                     
         B     EXIT                                                             
                                                                                
GRPCHA04 DS    0H                                                               
*&&UK*&& GOTOR VALPVT                                                           
                                                                                
GRPCHA07 GOTOR AFVAL,GRPDESCH      VALIDATE DESCRIPTION                         
         MVC   GCDESC,FVIFLD                                                    
                                                                                
         GOTOR AVALFRQ,GRPFREQH    VALIDATE FREQUENCY                           
         BNE   EXIT                                                             
         MVC   GCFREQ,PCWORK                                                    
                                                                                
         GOTOR AVALOUT,GRPOTYPH    VALIDATE OUTPUT TYPE                         
         BH    EXIT                                                             
         MVC   GCOTYP,FVIFLD                                                    
                                                                                
         GOTOR AVALDST,GRPDESTH    VALIDATE DESTINATION ID                      
         BNE   EXIT                                                             
         MVC   GCDEST,PCWORK                                                    
                                                                                
GRPCHA08 GOTOR AFVAL,GRPNAMEH      VALIDATE FILTER NAME                         
         MVC   GCNAME,FVIFLD                                                    
                                                                                
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   GRPCHA12                                                         
                                                                                
         GOTOR GETUSR,CUUSER                                                    
         CLC   FVMSGNO,=AL2(GE$IID)                                             
         BNE   GRPCHA09                                                         
         LA    R0,GRPUSERH         OR SYSTEM FIELD IF UNPROTECTED               
         ST    R0,FVADDR                                                        
         CFI   R1,0                                                             
         B     EXIT                                                             
*                                                                               
GRPCHA09 MVC   CUAALF,TWAAGY                                                    
         MVC   CUSYSL,ASSYSL                                                    
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    GRPCHA10                                                         
         MVC   CUUSER,GRPLUSER                                                  
         MVC   CUAALF,GRPLAGY                                                   
         MVC   CUSYSL,GRPLSYST                                                  
                                                                                
GRPCHA10 LA    R1,LGRP                                                          
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFP TO VALIDATE THE GROUP               
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   RFPVDESC,GCDESC     MOVE IN NEW VALUES                           
         MVC   RFPVFREQ,GCFREQ                                                  
         MVC   RFPVOTYP,GCOTYP                                                  
         MVC   RFPVDEST,GCDEST                                                  
         MVC   RFPVNAME,GCNAME                                                  
                                                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPSAVGP    SAVE THE GROUP                               
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   DIE IF ANY ERRORS                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     GRPCHA42                                                         
                                                                                
GRPCHA12 CLI   CSREC,RECXFG        TEST XFILE RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CSINDSL1,CSIUSELC   TEST NESTED LIST CALL                        
         BNZ   GRPCHA14                                                         
         LA    R2,IOKEY                                                         
         XC    XFKEY,XFKEY         BUILD XFILE RECORD KEY                       
         MVI   XFKSYS,XFKSYSQ                                                   
         MVI   XFKSTYP,XFKSTYPQ                                                 
         MVC   XFAGY,TWAAGY                                                     
         GOTOR GETUSR,XFUSER                                                    
         CLC   FVMSGNO,=AL2(GE$IID)                                             
         BNE   GRPCHA13                                                         
         LA    R0,GRPUSERH         OR SYSTEM FIELD IF UNPROTECTED               
         ST    R0,FVADDR                                                        
         CFI   R1,0                SET CC NOT EQUAL                             
         B     EXIT                                                             
*                                                                               
GRPCHA13 MVC   XFGRP,LGRP                                                       
         GOTOR AIO,'IOREAD+IOGENDIS+IO1'                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     EXIT                                                             
         GOTOR AIO,'IOGETRUP+IOGENFIS+IO1'                                      
         BE    *+6                                                              
         DC    H'0'                CANT READ RECORD                             
         B     GRPCHA16                                                         
                                                                                
GRPCHA14 MVC   IODAOVER,LSTTDA     READ THE XFILE RECORD                        
         GOTOR AIO,'IOGETRUP+IOGENFIS+IO1'                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GRPCHA16 L     R2,AIO1             R2=A(XFILE GROUP RECORD)                     
         LA    R4,XFFRSTEL                                                      
         USING XFPHD,R4            R4=A(FIRST ELEMENT ON RECORD)                
         SR    R0,R0                                                            
GRPCHA18 CLI   XFPHCD,0            TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   XFPHCD,XFPHCDQ      TEST GROUP HEADER ELEMENT                    
         BE    *+14                                                             
         IC    R0,XFPHLN           BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         B     GRPCHA18                                                         
                                                                                
         MVC   XFPHDESC,GCDESC     MOVE NEW VALUES INTO RECORD                  
         MVC   XFPHFREQ,GCFREQ                                                  
         MVC   XFPHOTYP,GCOTYP                                                  
         MVC   XFPHDEST,GCDEST                                                  
         MVC   XFPHNAME,GCNAME                                                  
                                                                                
         USING XFSGRPD,R4                                                       
         LA    RE,GCOLDXFG                                                      
         USING XFGTABD,RE                                                       
         LHI   RF,XFGTABM                                                       
GRPCHA20 IC    R0,XFSGLN           BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         CLI   XFSGCD,0            TEST END OF RECORD                           
         BE    GRPCHA22                                                         
         CLI   XFSGCD,XFSGCDEQ     TEST SYSTEM/GROUP ELEMENT                    
         BNE   GRPCHA20                                                         
         BCT   RF,*+6                                                           
         DC    H'0'                                                             
         MVC   XFGTSYS,XFSGSYS                                                  
         MVC   XFGTGRP,XFSGGRP                                                  
         MVC   XFGTSYSL,XFSGSYSL                                                
         MVI   XFSGCD,FF           SET THIS ELEMENT FOR DELETION                
         AHI   RE,XFGTABL          BUMP TO NEXT TABLE ENTRY                     
         B     GRPCHA20                                                         
         DROP  R4,RE                                                            
                                                                                
GRPCHA22 GOTOR VHELLO,PCPARM,(C'D',GENFIL),('FF',AIO1),0,0                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR BLDGRP,GCNEWXFG     BUILD NEW LIST OF GROUPS                     
         BNE   EXIT                                                             
                                                                                
         GOTOR AXFGRFP                                                          
                                                                                
         MVC   GCXNXTR,RFPVNXTR    SAVE XFILE VALUES IN W/S                     
         MVC   GCXENDR,RFPVENDR                                                 
         MVC   GCXDAYS,RFPVDAYS                                                 
         MVC   GCXRNGE,RFPVRNGE                                                 
                                                                                
GRPCHA26 LA    R4,GCNEWXFG         R4=A(NEW GROUPS)                             
NEW      USING XFGTABD,R4                                                       
         LHI   R5,XFGTABM                                                       
GRPCHA28 CLI   NEW.XFGTSYS,0       TEST END OF NEW LIST                         
         BE    GRPCHA36                                                         
                                                                                
         LA    RE,GCOLDXFG         MATCH NEW TO OLD IN LIST                     
OLD      USING XFGTABD,RE                                                       
         LHI   RF,XFGTABM                                                       
GRPCHA30 CLC   OLD.XFGTABD(XFGTABL),NEW.XFGTABD                                 
         BNE   *+14                                                             
         XC    OLD.XFGTABD(XFGTABL),OLD.XFGTABD                                 
         B     GRPCHA34                                                         
         AHI   RE,XFGTABL          BUMP TO NEXT OLD ENTRY                       
         BCT   RF,GRPCHA30                                                      
                                                                                
GRPCHA32 GOTOR GETUSR,CUUSER       ATTACH XFILE TO GROUP                        
         CLC   FVMSGNO,=AL2(GE$IID)                                             
         BNE   GRPCHA33                                                         
         LA    R0,GRPUSERH         OR SYSTEM FIELD IF UNPROTECTED               
         ST    R0,FVADDR                                                        
         CFI   R1,0                SET CC NOT EQUAL                             
         B     EXIT                                                             
*                                                                               
GRPCHA33 MVC   CUAALF,TWAAGY                                                    
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    *+16                                                             
         MVC   CUUSER,GRPLUSER                                                  
         MVC   CUAALF,GRPLAGY                                                   
         MVC   CUSYSL,NEW.XFGTSYSL                                              
         LA    R1,NEW.XFGTGRP                                                   
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFP TO VALIDATE THE GROUP               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   RFPXFILE,LGRP                                                    
         BE    GRPCHA34                                                         
         OC    RFPXFILE,RFPXFILE                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   RFPXFILE,LGRP       ATTACH XFILE GROUP CODE                      
         MVC   RFPVNXTR,GCXNXTR                                                 
         MVC   RFPVENDR,GCXENDR                                                 
         MVC   RFPVDAYS,GCXDAYS                                                 
         MVC   RFPVRNGE,GCXRNGE                                                 
         MVC   RFPVFREQ,GCFREQ                                                  
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPSAVGP    SAVE THE GROUP                               
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   DIE IF ANY ERRORS                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GRPCHA34 LA    RF,GCELEM           BUILD SYSTEM ELEMENT & ADD TO RECORD         
         USING XFSGRPD,RF                                                       
         XC    XFSGRPD(XFSGLNQ),XFSGRPD                                         
         MVI   XFSGCD,XFSGCDEQ                                                  
         MVI   XFSGLN,XFSGLNQ                                                   
         MVC   XFSGSYS,NEW.XFGTSYS                                              
         MVC   XFSGGRP,NEW.XFGTGRP                                              
         MVC   XFSGSYSL,NEW.XFGTSYSL                                            
         GOTOR VHELLO,PCPARM,(C'P',GENFIL),AIO1,XFSGRPD,0,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         AHI   R4,XFGTABL          BUMP TO NEXT ENTRY IN LIST                   
         BCT   R5,GRPCHA28                                                      
         DROP  OLD,NEW,RF                                                       
                                                                                
GRPCHA36 LA    R4,GCOLDXFG         DETACH OLD GROUPS NOT USED THIS TIME         
         USING XFGTABD,R4                                                       
         LHI   R5,XFGTABM                                                       
GRPCHA38 CLI   XFGTSYS,0           TEST UNUSED OR MATCHED ENTRY                 
         BE    GRPCHA40                                                         
         CLI   XFGTSYSL,0          IGNORE BAD RECORDS                           
         BE    GRPCHA40                                                         
         GOTOR GETUSR,CUUSER                                                    
         CLC   FVMSGNO,=AL2(GE$IID)                                             
         BNE   GRPCHA39                                                         
         LA    R0,GRPUSERH         OR SYSTEM FIELD IF UNPROTECTED               
         ST    R0,FVADDR                                                        
         CFI   R1,0                SET CC NOT EQUAL                             
         B     EXIT                                                             
*                                                                               
GRPCHA39 MVC   CUAALF,TWAAGY                                                    
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    *+16                                                             
         MVC   CUUSER,GRPLUSER                                                  
         MVC   CUAALF,GRPLAGY                                                   
         MVC   CUSYSL,XFGTSYSL                                                  
         LA    R1,XFGTGRP                                                       
         ICM   R1,8,=AL1(RFPXSYMS)                                              
         GOTOR AVALGRP             CALL RFP TO VALIDATE THE GROUP               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   RFPXFILE,LGRP       ENSURE THIS GROUP POINTS TO XFILE            
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    RFPXFILE,RFPXFILE   DETACH GROUP                                 
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPSAVGP    SAVE THE GROUP                               
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR   DIE IF ANY ERRORS                            
         BE    GRPCHA40                                                         
         DC    H'0'                                                             
GRPCHA40 AHI   R4,XFGTABL          BUMP TO NEXT ENTRY                           
         BCT   R5,GRPCHA38         DO FOR NUMBER OF ENTRIES                     
         DROP  R4                                                               
                                                                                
         GOTOR AIO,'IOPUT+IOGENFIS+IO1'                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GRPCHA42 TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    GRPCHAX                                                          
         MVC   GRPLDESC,GCDESC     YES - UPDATE LIST ENTRY                      
         MVC   GRPLFREQ,GCFREQ                                                  
         MVC   GRPLOTYP,GCOTYP                                                  
         MVC   GRPLDEST,GCDEST                                                  
         MVC   GRPLNAME,GCNAME                                                  
                                                                                
GRPCHAX  DS    0H                                                               
*&&UK*&& LA    R0,GRPPRVTH         POSITION CURSOR TO PRIVATE                   
*&&US*&& LA    R0,GRPDESCH         POSITION CURSOR TO DESCRIPTION               
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$RCENR)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         DROP  RC                                                               
                                                                                
GCWORKD  DSECT                     ** GRPCHA LOCAL W/S DSECT **                 
                                                                                
GCDESC   DS    CL(L'GRPLDESC)      DESCRIPTION                                  
GCFREQ   DS    CL(L'GRPLFREQ)      FREQUENCY                                    
GCOTYP   DS    CL(L'GRPLOTYP)      OUTPUT TYPE                                  
GCDEST   DS    CL(L'GRPLDEST)      DESTINATION ID                               
GCNAME   DS    CL(L'GRPLNAME)      FILTER NAME                                  
                                                                                
GCXNXTR  DS    XL(L'RFPVNXTR)      XFILE NEXT RUN DATE                          
GCXENDR  DS    XL(L'RFPVENDR)      XFILE END RUN DATE                           
GCXDAYS  DS    XL(L'RFPVDAYS)      XFILE RUN SCHEDULE                           
GCXRNGE  DS    XL(L'RFPVRNGE)      XFILE SUBMISSION DAYS                        
                                                                                
GCELEM   DS    XL256               ELEMENT BUILD AREA                           
                                                                                
GCOLDXFG DS    (XFGTABM)XL(XFGTABL)                                             
                                                                                
GCNEWXFG DS    (XFGTABM)XL(XFGTABL)                                             
                                                                                
RLP01    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LIST GROUP RECORDS                                                  *         
***********************************************************************         
                                                                                
         USING GLWORKD,RC          RC=A(LOCAL W/S)                              
GRPLST   L     RE,=A(KEYTGRP)                                                   
         CLI   CSREC,RECGRP                                                     
         BE    *+8                                                              
         L     RE,=A(KEYTXFG)                                                   
         A     RE,OVRELO                                                        
         ST    RE,AKEYTAB                                                       
                                                                                
GLKEYVAL MVI   GLFLAG,0            CLEAR FLAG BYTE                              
         LA    R5,GLOPTS                                                        
         ST    R5,AOVEROUT         SAVE A(OUTPUT AREA FOR OPTIONS)              
         USING GLVALS,R5           R5=A(GROUP LIST OPTIONS)                     
                                                                                
         MVC   AGYFSTR,CUAALF      SET DEFAULT AGENCY FILTERS                   
         MVC   AGYFEND,CUAALF                                                   
                                                                                
         MVC   UIDFSTR,CUUSER      SET DEFAULT USER-ID FILTERS                  
         MVC   UIDFEND,CUUSER                                                   
         MVI   UIDFLTY,KEYTRNGE                                                 
                                                                                
         MVI   SYSFLTY,KEYTLIST    SET DEFAULT SYSTEM                           
         MVC   SYSFSYST(L'ASSYSL),ASSYSL                                        
         CLI   ASSYSL,CONLETQ      TEST CONTROL SYSTEM                          
         BNE   *+16                                                             
         MVI   SYSFLTY,KEYTRNGE    YES - ALLOW ALL SYSTEMS                      
         MVI   SYSFSYST+0,00                                                    
         MVI   SYSFSYST+1,FF                                                    
                                                                                
         L     R1,=A(GLOVAL)                                                    
         A     R1,OVRELO                                                        
         ST    R1,AOVERVAL         SAVE A(OPTION VALIDATION ROUTINES)           
         GOTOR AFVAL,RLPOPTH                                                    
         BASR  R1,0                                                             
         AHI   R1,GLOTAB-*                                                      
         GOTOR AVALOPT             VALIDATE OPTIONS                             
         BNE   EXIT                                                             
                                                                                
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   GLKEYV02                                                         
         GOTOR AOVRSCR,PCPARM,('GRPLSTSQ',RLPOLY1H)                             
         BNE   EXIT                                                             
         LA    R0,GLIGRPH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EKEYD)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         B     EXIT                                                             
                                                                                
GLKEYV02 XC    GRPFSTR,GRPFSTR     SET START GROUP TO ZEROES                    
         MVC   GRPFEND,PCEFFS      SET END GROUP TO X'FF'S                      
         GOTOR AFVAL,GLIGRPH       VALIDATE GROUP FILTER                        
         BNE   GLKEYV08                                                         
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         ICM   RE,1,FVILEN                                                      
         LA    RF,FVIFLD-1(RE)     POINT TO LAST INPUT CHARACTER                
                                                                                
GLKEYV04 CLC   PCWILD,0(RF)        TEST GROUPS STARTING WITH FEATURE            
         BNE   GLKEYV06                                                         
         LHI   R0,1                                                             
         BCTR  RF,0                                                             
         BCT   RE,GLKEYV04                                                      
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     EXIT                                                             
                                                                                
GLKEYV06 BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   GRPFSTR(0),FVIFLD                                                
         LTR   R0,R0                                                            
         BZ    GLKEYV08                                                         
         EX    RE,*+8                                                           
         B     GLKEYV08                                                         
         MVC   GRPFEND(0),FVIFLD                                                
                                                                                
GLKEYV08 XC    FREQFLT(FREQFLTL),FREQFLT                                        
         LA    R1,GLIFRQH                                                       
         ICM   R1,8,=X'80'         WANT 'ALL EXCEPT VALIDATION'                 
         GOTOR AVALFRQ,(R1)                                                     
         BH    EXIT                                                             
         BL    GLKEYV10                                                         
                                                                                
         MVC   FREQFVAL,PCWORK     SET FREQUENCY FILTER                         
         MVI   FREQFCC,CCNE        SET NOT EQUAL                                
         TM    FREQFVAL,SPACE                                                   
         BNZ   GLKEYV10                                                         
         MVI   FREQFCC,CCEQ        SET EQUAL                                    
         OI    FREQFVAL,SPACE                                                   
                                                                                
GLKEYV10 XC    OTYPFLT(OTYPFLTL),OTYPFLT                                        
         GOTOR AFVAL,GLIOTYPH      VALIDATE OUTPUT TYPE FILTER                  
         BNE   GLKEYV12                                                         
         CLI   FVILEN,L'OTYPFVAL                                                
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IOUT)                                            
         B     EXIT                                                             
         MVC   OTYPFLEN,FVILEN                                                  
         MVC   OTYPFVAL,FVIFLD                                                  
                                                                                
GLKEYV12 XC    DSTCFLT(DSTCFLTL),DSTCFLT                                        
         GOTOR AFVAL,GLIDESTH      VALIDATE DESTINATION CODE FILTER             
         BNE   GLKEYV14                                                         
         CLI   FVILEN,L'DSTCFVAL                                                
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IDEST)                                           
         B     EXIT                                                             
         MVC   DSTCFLEN,FVILEN                                                  
         MVC   DSTCFVAL,FVIFLD                                                  
                                                                                
GLKEYV14 GOTOR AGOSOFT,PCPARM,('SOFITYMD',GLINXTRH),                   *        
               ('SOFOTSD8+SOFOTMIX',NXTRSTDT),('FF-SOFIIONE',0)                 
         BNE   EXIT                                                             
                                                                                
         GOTOR AGOSOFT,PCPARM,('SOFITYMD',GLILSTRH),                   *        
               ('SOFOTSD8+SOFOTMIX',LSTRSTDT),('FF-SOFIIONE',0)                 
         BNE   EXIT                                                             
                                                                                
         GOTOR AGOSOFT,PCPARM,('SOFITYMD',GLIENDH),                    *        
               ('SOFOTSD8+SOFOTMIX',ENDSTDT),('FF-SOFIIONE',0)                  
         BNE   EXIT                                                             
                                                                                
         XC    NAMEFLT(NAMEFLTL),NAMEFLT                                        
         GOTOR AFVAL,GLINAMEH      VALIDATE DESTINATION CODE FILTER             
         BNE   GLKEYVX                                                          
         CLI   FVILEN,L'NAMEFVAL                                                
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL1(GE$ILTL)                                            
         B     EXIT                                                             
         MVC   NAMEFLEN,FVILEN                                                  
         MVC   NAMEFVAL,FVIFLD                                                  
                                                                                
GLKEYVX  DS    0H                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE LIST SUB-ACTIONS                                             *         
***********************************************************************         
                                                                                
GLSELECT MVI   FVINDX,0                                                         
         NI    CSLTINDS,FF-(CSLTIFST+CSLTIHLD+CSLTIANY)                         
                                                                                
         CLI   PCPFKEY,PFKRFSHQ    TEST REFRESH LIST                            
         BNE   *+12                                                             
         OI    CSLTINDS,CSLTIFST   SET FIRST FOR LIST                           
         MVI   PCPFKEY,0           CLEAR PFKEY                                  
                                                                                
         LA    R0,GLOPTS                                                        
         LHI   R1,GLOKOPTL                                                      
         LA    RE,GLOKOPT                                                       
         LHI   RF,GLOKOPTL                                                      
         CLCL  R0,RE                                                            
         BE    *+8                                                              
         OI    CSLTINDS,CSLTIFST   SET FIRST FOR LIST IF KEY CHANGED            
                                                                                
         CLC   GLOPTS+GLOKOPTL(GLODOPTL),GLODOPT                                
         BE    *+14                                                             
         XC    GLODVALS,GLODVALS                                                
         OI    CSLTINDS,CSLTIHLD   SET HOLD PAGE (NO VERTICAL SCROLL)           
                                                                                
         TM    PCSCROLL,PFKIHORZ   TEST HORIZONTAL SCROLL                       
         BZ    *+8                                                              
         OI    CSLTINDS,CSLTIHLD   SET HOLD PAGE (NO VERTICAL SCROLL)           
                                                                                
         LA    R0,GLVALS                                                        
         LHI   R1,GLVALSL                                                       
         LA    RE,GLOPTS                                                        
         LHI   RF,GLVALSL                                                       
         MVCL  R0,RE               MOVE IN NEW FILTERS/OPTIONS                  
                                                                                
         TM    CSLTINDS,CSLTIFST   TEST CHANGE OF KEY FIELDS                    
         BNZ   GLSCROLL                                                         
                                                                                
GLSELE02 SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LIST ENTRIES                    
         BZ    GLSCROLL                                                         
         MVC   LSTTRECN,CSPAG#LO                                                
         LA    R3,GLIACT1H                                                      
         USING GLIACT1H,R3                                                      
                                                                                
GLSELE04 NI    GLFLAG,FF-(GLFINPT+GLFIPFK)                                      
         CLI   GLIACT1H+(FVILEN-FVIHDR),0                                       
         BE    GLSELE06                                                         
         CLI   GLIACT1,ACTAOKQ     TEST NULL ACTION                             
         BE    GLSELE06                                                         
         OI    GLFLAG,GLFINPT      SET USER INPUT                               
         B     GLSELE10                                                         
                                                                                
GLSELE06 CLC   CSCURDSP,GLIACT1H+(FVABSA-FVIHDR)                                
         BL    GLSELE08                                                         
         CLC   CSCURDSP,GLIACT2H+(FVABSA-FVIHDR)                                
         BNL   GLSELE08                                                         
         CLI   PCPFKEY,0           TEST PFKEY WAS ENTERED                       
         BE    GLSELE08                                                         
         OI    GLFLAG,GLFIPFK      SET TO MATCH ON PFKEY VALUE                  
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         LA    RF,GLIACT1H         SET CURSOR TO ACTION (FOR ERRORS)            
         ST    RF,FVADDR                                                        
         B     GLSELE12                                                         
                                                                                
GLSELE08 TM    CSLTINDS,CSLTIEOL+CSLTIEOP                                       
         BZ    GLACTAOK                                                         
         CLC   LSTTRECN,CSSEL#LO                                                
         BL    GLACTAOK                                                         
         CLC   LSTTRECN,CSSEL#HI                                                
         BH    GLACTAOK                                                         
         SR    R4,R4                                                            
         ICM   R4,3,CSSELMUL                                                    
         A     R4,AOVERSEL                                                      
         USING SELTABD,R4          R4=A(SELECT TABLE ENTRY)                     
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPM                                                    
         LA    RE,WORKD(RE)                                                     
         XC    GLIACT1,GLIACT1                                                  
         MVC   GLIACT1(L'GLIACT1-1),0(RE)                                       
         OI    GLFLAG,GLFINPT      SET USER INPUT                               
                                                                                
GLSELE10 GOTOR AFVAL,GLIACT1H                                                   
         BNE   GLACTAOK                                                         
                                                                                
         CLI   FVIFLD,ACTAOKQ                                                   
         BE    GLNXTGRP                                                         
         CLI   FVIFLD,ACTNOTQ                                                   
         BNE   *+12                                                             
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         B     GLNXTGRP                                                         
                                                                                
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RE,FVIFLD(RF)       POINT TO END OF INPUT FIELD                  
         CLI   0(RE),ACTEOSQ                                                    
         BE    *+12                                                             
         CLI   0(RE),ACTEOLQ                                                    
         BNE   GLSELE12                                                         
         MVC   CSSEL#LO,LSTTRECN   SET LOW RECORD NUMBER                        
         MVC   CSSEL#HI,PCEFFS     SET DEFAULT HIGH VALUE                       
         LA    R1,CSLTIEOL                                                      
         CLI   0(RE),ACTEOLQ                                                    
         BE    *+14                                                             
         MVC   CSSEL#HI,CSPAG#HI                                                
         LA    R1,CSLTIEOP                                                      
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         OI    CSLTINDS,0                                                       
         BCTR  RF,0                                                             
         STC   RF,FVXLEN                                                        
                                                                                
GLSELE12 L     R4,AOVERSEL         R4=A(SELECT TABLE)                           
         XC    GLASEL,GLASEL       RESET A(UNIQUE ACTION)                       
                                                                                
GLSELE14 TM    GLFLAG,GLFINPT      TEST FIELD INPUT                             
         BNZ   GLSELE16                                                         
         TM    GLFLAG,GLFIPFK      TEST PFKEY INPUT                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   PCPFKEY,SELTPFK     MATCH PFKEY TO SELECT TABLE                  
         BE    GLSELE20                                                         
         B     GLSELE18                                                         
                                                                                
GLSELE16 SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,WORKD(RE)                                                     
         EX    RF,*+8                                                           
         BE    GLSELE17                                                         
         CLC   FVIFLD(0),0(RE)                                                  
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPM                                                    
         LA    RE,WORKD(RE)                                                     
         EX    RF,*+8                                                           
         BNE   GLSELE18                                                         
         CLC   FVIFLD(0),0(RE)                                                  
GLSELE17 TM    SELTIND2,SELTIDEF   TEST THIS IS THE DEFAULT ACTION              
         BZ    *+12                                                             
         STCM  R4,15,GLASEL        YES - USE IT REGARDLESS                      
         B     GLSELE19                                                         
                                                                                
         OC    GLASEL,GLASEL       TEST A(ACTION) SET ALREADY                   
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ACTNR)                                           
         B     EXIT                                                             
         STCM  R4,15,GLASEL        SET A(ACTION)                                
                                                                                
GLSELE18 AHI   R4,SELTABL          BUMP TO NEXT TABLE ENTRY                     
         CLI   SELTABD,EOT                                                      
         BNE   GLSELE14                                                         
                                                                                
GLSELE19 ICM   R4,15,GLASEL        R4=A(UNIQUE ACTION) OR 0                     
         BZ    GLSELE28                                                         
         TM    CSLTINDS,CSLTIEOL   TEST SELECT TO END OF LIST                   
         BZ    *+12                                                             
         TM    SELTIND1,SELTIEOL                                                
         BZ    GLSELE28                                                         
         TM    CSLTINDS,CSLTIEOP   TEST SELECT TO END OF PAGE                   
         BZ    *+12                                                             
         TM    SELTIND1,SELTIEOP                                                
         BZ    GLSELE28                                                         
                                                                                
GLSELE20 CLC   SELTREC(L'SELTREC+L'SELTACT),BZEROES                             
         BE    GLSELE22                                                         
         GOTOR ATSTMIX,SELTRECA    VALIDATE RECORD/ACTION                       
         BNE   GLSELE28                                                         
                                                                                
GLSELE22 CLC   LSTTRECN,CSSEL#LO   TEST SELECT MULTIPLE INPUT LINE              
         BNE   GLSELE24                                                         
         L     RE,AOVERSEL                                                      
         LA    RF,SELTABD                                                       
         SR    RF,RE                                                            
         STCM  RF,3,CSSELMUL       SET DISPLACEMENT TO SELTAB ENTRY             
                                                                                
GLSELE24 GOTOR ATSARIO,TSAGET      GET GROUP LIST RECORD                        
         MVC   GLMASK,SELTMASK     TEST ACTION OKAY FOR THIS LINE               
         NC    GLMASK,LSTTMASK                                                  
         CLC   GLMASK,SELTMASK                                                  
         BE    GLSELE30                                                         
                                                                                
         TM    CSLTINDS,CSLTIEOL+CSLTIEOP                                       
         BZ    GLSELE26                                                         
         CLC   LSTTRECN,CSSEL#LO   ACTION INVALID FOR THIS LINE                 
         BE    GLACTAOK                                                         
         BH    GLACTAOK                                                         
                                                                                
GLSELE26 MVC   FVMSGNO,=AL2(GE$INVIF)                                           
         B     EXIT                                                             
                                                                                
GLSELE28 TM    GLFLAG,GLFIPFK      TEST PFKEY INPUT                             
         BNZ   GLNXTGRP                                                         
         NI    CSLTINDS,FF-(CSLTIEOL+CSLTIEOP)                                  
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         MVC   FVMSGNO,=AL2(GE$ACTNR)                                           
         B     EXIT                                                             
                                                                                
GLSELE30 OI    CSLTINDS,CSLTIANY   SET LINE PROCESSED THIS SCREEN               
         CLC   SELTREC(L'SELTREC+L'SELTACT),BZEROES                             
         BE    GLSELE32                                                         
         L     RE,ATWA                                                          
         LA    RF,GLIACT1H                                                      
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
         GOTOR ANTRSES,SELTPARM                                                 
                                                                                
GLSELE32 LA    RF,RLP01                                                         
         ICM   RF,8,SELTRTN                                                     
         BASR  RE,RF                                                            
         LHI   R1,1                SET TO RE-DISPLAY THIS LINE                  
         B     GLACTCOM                                                         
         EJECT                                                                  
***********************************************************************         
* LINE ACTION COMPLETED                                               *         
***********************************************************************         
                                                                                
GLDELRET SR    R0,R0               RETURN FROM GROUP/DELETE                     
         ICM   R0,3,CSSELACT                                                    
         A     R0,ATWA             R0=A(ACTION LINE)                            
         ST    R0,FVADDR                                                        
         CLC   FVMSGNO,=AL2(FVFOK) TEST GROUP DELETED                           
         BNE   EXIT                                                             
         SR    R1,R1               SET DON'T WANT LINE RE-DISPLAYED             
         B     GLRETALL                                                         
                                                                                
GLDISRET LHI   R1,1                SET DO WANT LINE RE-DISPLAYED                
                                                                                
GLRETALL MVI   PCPFKEY,0           RESET PFKEY VALUE                            
         SR    R3,R3                                                            
         ICM   R3,3,CSSELACT                                                    
         A     R3,ATWA             R3=A(ACTION LINE)                            
         SR    R4,R4                                                            
         ICM   R4,3,CSSELCUR                                                    
         A     R4,AOVERSEL         R4=A(SELECT TABLE ENTRY)                     
         SR    R0,R0                                                            
         ICM   R0,1,CSSELREM       R0=NUMBER OF LINES REMAINING                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,=A(KEYTGRP)                                                   
         CLI   CSREC,RECGRP                                                     
         BE    *+8                                                              
         L     RE,=A(KEYTXFG)                                                   
         A     RE,OVRELO                                                        
         ST    RE,AKEYTAB                                                       
                                                                                
GLACTCOM MVI   GLIACT1,ACTAOKQ     LINE ACTION COMPLETE                         
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPM                                                    
         LA    RE,WORKD(RE)                                                     
         MVC   GLIACT1+1(L'GLIACT1-1),0(RE)                                     
         OI    GLIACT1H+(FVOIND-FVIHDR),FVOXMT                                  
         LTR   R1,R1               TEST RE-DISPLAY THIS LINE                    
         BZ    GLACTC02                                                         
         GOTOR ATSARIO,TSAPUT                                                   
         GOTOR BLDLIN,GLILIN1H                                                  
                                                                                
GLACTC02 CLI   PCPFKEY,PFKQUITQ    TEST USER WANTS TO QUIT                      
         BNE   GLNXTGRP                                                         
         B     GLNXTG04                                                         
                                                                                
GLACTAOK XC    GLIACT1,GLIACT1                                                  
         OI    GLIACT1H+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
GLNXTGRP LA    R3,GLIACT2H         BUMP TO NEXT ACTION FIELD                    
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN       BUMP TO NEXT RECORD NUMBER                   
         AHI   R1,1                                                             
         STCM  R1,3,LSTTRECN                                                    
         BCT   R0,GLSELE04         DO FOR NUMBER OF ENTRIES ON SCREEN           
                                                                                
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
         TM    CSLTINDS,CSLTIEOF   TEST END OF FILE REACHED                     
         BZ    GLNXTG02                                                         
         CLC   LSTTRECN,CSHIRECN   TEST THIS IS LAST RECORD                     
         BNE   GLNXTG02                                                         
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         DROP  R3                                                               
                                                                                
GLNXTG02 TM    CSLTINDS,CSLTIANY                                                
         BZ    GLSCROLL                                                         
         SR    RE,RE                                                            
         IC    RE,CSREC                                                         
         SLL   RE,1                                                             
         LA    RE,TWAGLIND-L'TWAGLIND(RE)                                       
         TM    0(RE),TWAGLIRL      TEST REFRESH LIST PENDING                    
         BZ    *+16                                                             
         NI    0(RE),FF-(TWAGLIRL)                                              
         OI    CSLTINDS,CSLTIFST   TURN OFF REFRESH/TURN ON FIRST               
         B     GLSCROLL                                                         
         TM    CSLTINDS,CSLTIEOL   TEST PROCESS TO END OF LIST                  
         BNZ   GLSCROLL                                                         
                                                                                
GLNXTG04 XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         LA    R0,GLIACT1H                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$ACENR)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE SCROLLING                                                    *         
***********************************************************************         
                                                                                
GLSCROLL LA    R2,IOKEY                                                         
         MVC   GLPAG#LO,CSPAG#LO   SAVE LAST LOW & HIGH RECORDS                 
         MVC   GLPAG#HI,CSPAG#HI                                                
         XC    CSPAG#LO,CSPAG#LO   CLEAR LOW & HIGH VALUES FOR PAGE             
         XC    CSPAG#HI,CSPAG#HI                                                
         CLI   CSLSTNUM,0          REFRESH IF EMPTY LIST                        
         BE    *+12                                                             
         TM    CSLTINDS,CSLTIFST   TEST FIRST FOR LIST                          
         BZ    GLSCRO02                                                         
         SR    RE,RE                                                            
         IC    RE,CSREC                                                         
         SLL   RE,1                                                             
         LA    RE,TWAGLIND-L'TWAGLIND(RE)                                       
         XC    0(L'TWAGLIND,RE),0(RE)                                           
         MVI   CSLTINDS,0          CLEAR LIST INDICATORS                        
         MVI   CSLSTNUM,0          CLEAR NUMBER OF LIST ENTRIES                 
         MVC   CSHIRECN,CSPSRECN   RESET HIGH RECORD NUMBER                     
         XC    IOKEY,IOKEY         CLEAR KEY FOR SETKEY ROUTINE                 
         B     GLSCRO16                                                         
                                                                                
GLSCRO02 TM    PCSCROLL,PFKIHORZ   TEST HORIZONTAL SCROLLING                    
         BNZ   *+14                                                             
         MVC   GLSCRNUM,PCSCRNUM   EXTRACT SCROLL MAGNITUDE                     
         B     *+8                                                              
         MVI   GLSCRNUM,PFKIPAGE                                                
         TM    CSLTINDS,CSLTIHLD   TEST HOLD PAGE IF DISPLAY CHANGED            
         BZ    GLSCRO04                                                         
         MVC   CSPAG#LO,GLPAG#LO   RESTORE LAST LOW & HIGH RECORDS              
         MVC   CSPAG#HI,GLPAG#HI                                                
         B     GLDISPLY                                                         
                                                                                
GLSCRO04 TM    PCSCROLL,PFKIUPDN   TEST SCROLL UP OR DOWN                       
         BZ    GLSCRO08                                                         
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,CSPSRECN                                                    
         TM    GLSCRNUM,PFKIMAXN   TEST SCROLL FIRST                            
         BNZ   GLSCRO06                                                         
         LHI   RF,GLISTMAX         SCROLL UP (BACKWARDS)                        
         TM    GLSCRNUM,PFKIHALF                                                
         BZ    *+8                                                              
         SRL   RF,1                                                             
         TM    GLSCRNUM,X'0F'                                                   
         BZ    *+8                                                              
         IC    RF,GLSCRNUM                                                      
         AHI   RF,1                                                             
         SR    RE,RE                                                            
         ICM   RE,3,GLPAG#LO                                                    
         SR    RE,RF               BACK-UP TO RECORD NUMBER-1                   
         BM    *+12                                                             
         CLM   RE,3,CSPSRECN       TEST NOT < LOW RECORD FOR SESSION            
         BNL   GLSCRO06                                                         
         SR    RE,RE               SET TO START FROM LOW RECORD                 
         ICM   RE,3,CSPSRECN                                                    
                                                                                
GLSCRO06 STCM  RE,3,LSTTRECN                                                    
         MVI   CSLSTNUM,0                                                       
         B     GLSCRO12                                                         
                                                                                
GLSCRO08 SR    R1,R1                                                            
         ICM   R1,1,CSLSTNUM       PICK UP NUMBER OF ENTRIES IN PAGE            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   CSLSTNUM,0                                                       
         TM    GLSCRNUM,X'0F'      TEST SCROLL AMOUNT SPECIFIED                 
         BZ    *+16                                                             
         CLM   R1,1,GLSCRNUM       TEST SCROLL EXCEEDS ACTUAL AMOUNT            
         BL    GLSCRO10                                                         
         IC    R1,GLSCRNUM                                                      
         TM    GLSCRNUM,PFKIHALF   TEST HALF PAGE SCROLL                        
         BZ    *+8                                                              
         SRL   R1,1                                                             
         AHI   R1,-1                                                            
         BM    GLSCRO10                                                         
         SR    R0,R0                                                            
         ICM   R0,3,GLPAG#LO                                                    
         AR    R1,R0                                                            
         STCM  R1,3,LSTTRECN                                                    
         TM    CSLTINDS,CSLTIEOF   TEST END OF FILE ENCOUNTERED                 
         BZ    GLSCRO12                                                         
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         BL    GLSCRO12                                                         
                                                                                
GLSCRO10 MVC   LSTTRECN,CSPSRECN   SET TO DISPLAY FIRST PAGE                    
                                                                                
GLSCRO12 SR    RE,RE               BUMP TO NEXT RECORD                          
         ICM   RE,3,LSTTRECN                                                    
         AHI   RE,1                                                             
         STCM  RE,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSHIRECN   TEST IN TSAR BUFFER                          
         BH    GLSCRO14            NO - GET NEXT GROUP RECORD                   
         GOTOR LSTADD,1            ADD ENTRY TO LSTTAB                          
         BE    GLSCRO12                                                         
         B     GLDISPLY                                                         
                                                                                
GLSCRO14 TM    CSLTINDS,CSLTIEOF   TEST EOF ENCOUNTERED                         
         BNZ   GLDISPLY                                                         
         MVC   IOKEY,GLTBANXT      GET NEXT GROUP RECORD                        
         B     GLSCRO18                                                         
                                                                                
GLSCRO16 GOTOR ASETKEY             SET NEXT GROUP RECORD KEY                    
         BNH   GLSCRO18                                                         
         OI    CSLTINDS,CSLTIEOF   SET END OF FILE                              
         B     GLDISPLY                                                         
                                                                                
GLSCRO18 GOTOR AIO,'IOHIGH+IOGENDIS+IO1'                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   CSREC,RECGRP        FILTER OUT DESKTOP GROUPS                    
         BNE   GLSCRO20                                                         
         TM    CUSTAT,CUSDDS       UNLESS DDS IS LOOKING                        
         BNZ   GLSCRO20                                                         
         CLI   IOKEY+(GRPKDTF-GRPKEYD),GRPKDTFQ                                 
         BE    GLSCRO16                                                         
                                                                                
GLSCRO20 GOTOR AFLTKEY             FILTER THE GROUP DIRECTORY KEY               
         BNE   GLSCRO16                                                         
                                                                                
         GOTOR AIO,'IOGET+IOGENFIS+IO1'                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    LSTTABD(LSTTABL),LSTTABD                                         
         NI    GLFLAG,FF-(GLFHAVG)                                              
                                                                                
         GOTOR LSTBLD,0            BUILD LIST ENTRY/TEST SECURITY               
         BNE   GLSCRO16                                                         
                                                                                
         GOTOR FLTREC              FILTER THE GROUP RECORD                      
         BNE   GLSCRO16                                                         
                                                                                
         SR    RE,RE               BUMP HIGH RECORD NUMBER                      
         ICM   RE,3,CSHIRECN                                                    
         AHI   RE,1                                                             
         STCM  RE,3,LSTTRECN                                                    
                                                                                
         GOTOR LSTADD,0            ADD ENTRY TO LIST                            
         BE    GLSCRO16                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY PAGE OF DATA                                                *         
***********************************************************************         
                                                                                
GLDISPLY GOTOR BLDCOL              BUILD COLUMN HEADING & DISPLACEMENTS         
         MVC   GLIHD1,BLDHEAD1                                                  
         OI    GLIHD1H+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   GLIHD2,BLDHEAD2                                                  
         OI    GLIHD2H+(FVOIND-FVIHDR),FVOXMT                                   
         TWAXC GLIACT1H,PROT=Y                                                  
                                                                                
         CLC   CSHIRECN,CSPSRECN   TEST ANY RECORDS FOUND                       
         BNE   GLDISP02                                                         
         MVC   FVMSGNO,=AL2(GI$NLENR)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R0,GLIGRPH                                                       
         ST    R0,FVADDR                                                        
         B     EXIT                                                             
                                                                                
GLDISP02 MVC   LSTTRECN,CSPAG#LO                                                
         CLC   LSTTRECN,CSSEL#HI   TEST > HIGH MULTIPLE SELECT                  
         BNH   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LINES TO DISPLAY                
         BNZ   *+6                                                              
         DC    H'0'                DIE FOR NOW IF NO DISPLAY LINES              
         LA    R3,GLIACT1H                                                      
         USING GLIACT1H,R3         R3=A(SCREEN LINE)                            
GLDISP04 GOTOR ATSARIO,TSAGET      GET GROUP LIST RECORD                        
         GOTOR BLDLIN,GLILIN1H                                                  
         MVI   GLIACT1H+(FVILEN-FVIHDR),0                                       
         LA    R3,GLIACT2H                                                      
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN                                                    
         AHI   R1,1                                                             
         STCM  R1,3,LSTTRECN                                                    
         BCT   R0,GLDISP04                                                      
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSSEL#LO   TEST < LOW MULTIPLE SELECT                   
         BNL   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         TM    CSLTINDS,CSLTIEOL   TEST SELECT TO END OF LIST                   
         BNZ   GLSELE02                                                         
         DROP  R3                                                               
                                                                                
GRPLSTX  LA    R0,GLIACT1H                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$LDSOE)                                           
         MVI   FVOMTYP,GTMINF                                                   
         TM    CSLTINDS,CSLTIEOF   TEST END-OF-FILE ENCOUNTERED                 
         BZ    EXIT                                                             
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         BNE   EXIT                                                             
         MVC   FVMSGNO,=AL2(GI$ELSEF)                                           
         B     EXIT                                                             
         DROP  RC                                                               
                                                                                
GLISTMAX EQU   (GLIPFKH-GLIACT1H)/(GLIACT2H-GLIACT1H)                           
                                                                                
GLWORKD  DSECT                     ** GRPLST LOCAL W/S **                       
GLBYTE1  DS    X                   WORK BYTE 1                                  
GLBYTE2  DS    X                   WORK BYTE 2                                  
GLSCRNUM DS    X                   SCROLL MAGNITUDE                             
GLOPTS   DS    XL(GLVALSL)         KEY & OPTIONS                                
GLFLAG   DS    X                   FLAG BYTE                                    
GLFINPT  EQU   X'80'               USER INPUT IN SELECT FIELD                   
GLFIPFK  EQU   X'40'               USER INPUT PFKEY ON THIS LINE                
GLFNEXT  EQU   X'20'               NOT FIRST TIME FOR CURRENT RECORD            
GLFHAVG  EQU   X'10'               GROUP VALIDATED                              
GLPAG#LO DS    XL2                 LOW RECORD ON CURRENT PAGE                   
GLPAG#HI DS    XL2                 HIGH RECORD ON CURRENT PAGE                  
GLASEL   DS    AL4                 A(UNIQUE SELECT TABLE ACTION)                
GLMASK   DS    XL(L'LSTTMASK)      MASK WORK AREA                               
RLP01    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER A GROUP RECORD                                    *         
*                                                                     *         
* NTRY - AIO1 CONTAINS GROUP RECORD                                   *         
* EXIT - CC=NOT EQUAL IF GROUP FILTERED OUT                           *         
***********************************************************************         
                                                                                
FLTREC   NTR1  ,                                                                
         L     R2,AIO1                                                          
         SR    RE,RE                                                            
         L     R3,ARFPIOB                                                       
         USING RFPD,R3                                                          
                                                                                
         ICM   RE,1,FREQFCC        FREQUENCY FILTER                             
         BZ    FLTREC04                                                         
         CLC   GRPLFREQ,FREQFVAL                                                
         EX    RE,*+8                                                           
         B     FLTREC04                                                         
         NOP   FLTRECN                                                          
                                                                                
FLTREC04 ICM   RE,1,OTYPFLEN       TEST OUTPUT TYPE FILTER SET                  
         BZ    FLTREC06                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BNE   FLTRECN                                                          
         CLC   GRPLOTYP(0),OTYPFVAL                                             
                                                                                
FLTREC06 CLI   DSTCFLEN,0          DESTINATION ID CODE FILTER                   
         BE    FLTREC08                                                         
         OC    GRPLDEST,GRPLDEST                                                
         BZ    FLTRECN                                                          
         GOTOR AGETUID,GRPLDEST    SET DESTINATION ID CODE                      
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RE,1,DSTCFLEN       DESTINATION ID CODE FILTER                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BNE   FLTRECN                                                          
         CLC   PCWORK+(GIDCODE-GIDTABD)(0),DSTCFVAL                             
                                                                                
FLTREC08 ICM   RE,1,NAMEFLEN       FILTER NAME FILTER                           
         BZ    FLTREC10                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BNE   FLTRECN                                                          
         CLC   GRPLNAME(0),NAMEFVAL                                             
                                                                                
FLTREC10 GOTOR AGETNXT             GET NEXT RUN DATE                            
         CLC   PCWORK(L'NXTRSTDT),NXTRSTDT                                      
         BL    FLTRECN                                                          
         CLC   PCWORK(L'NXTRENDT),NXTRENDT                                      
         BH    FLTRECN                                                          
                                                                                
         GOTOR AGETLST             GET LAST RUN DATE                            
         CLC   PCWORK(L'LSTRSTDT),LSTRSTDT                                      
         BL    FLTRECN                                                          
         CLC   PCWORK(L'LSTRENDT),LSTRENDT                                      
         BH    FLTRECN                                                          
                                                                                
         GOTOR AGETEND             GET END RUN DATE                             
         CLC   PCWORK(L'ENDSTDT),ENDSTDT                                        
         BL    FLTRECN                                                          
         CLC   PCWORK(L'ENDENDT),ENDENDT                                        
         BH    FLTRECN                                                          
                                                                                
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   FLTREC16                                                         
         CLI   XFGFTYP,XFGFTNUL    APPLY XFILE FILTER                           
         BE    FLTREC16                                                         
         CLI   XFGFTYP,XFGFTYES                                                 
         BNE   FLTREC12                                                         
         OC    GRPLXFIL,GRPLXFIL                                                
         BZ    FLTRECN                                                          
         B     FLTREC16                                                         
FLTREC12 CLI   XFGFTYP,XFGFTNO                                                  
         BNE   FLTREC14                                                         
         OC    GRPLXFIL,GRPLXFIL                                                
         BNZ   FLTRECN                                                          
         B     FLTREC16                                                         
FLTREC14 CLI   XFGFTYP,XFGFTFLT                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GRPLXFIL,XFGFTGRP                                                
         BNE   FLTRECN                                                          
                                                                                
FLTREC16 CLI   CSREC,RECGRP        TEST FOR GROUP RECORD                        
         BNE   FLTREC20                                                         
         CLI   KEYFILT,KEYFNUL     APPLY KEYWORD FILTER                         
         BE    FLTREC20                                                         
         GOTOR GETGRP              GET RFP GROUP VALUES                         
         CLI   KEYFILT,KEYFYES                                                  
         BNE   FLTREC18                                                         
         CLI   RFPVNUMS,0                                                       
         BE    FLTRECN                                                          
         B     FLTREC20                                                         
FLTREC18 CLI   RFPVNUMS,0                                                       
         BNE   FLTRECN                                                          
                                                                                
FLTREC20 CLI   CSREC,RECGRP        TEST FOR GROUP RECORD                        
         BNE   FLTREC24                                                         
         CLI   REQFILT,REQFNUL     APPLY REQUEST FILTER                         
         BE    FLTREC24                                                         
         GOTOR GETGRP              GET RFP GROUP VALUES                         
         CLI   REQFILT,REQFYES                                                  
         BNE   FLTREC22                                                         
         OC    RFPVNREQ,RFPVNREQ                                                
         BZ    FLTRECN                                                          
         B     FLTREC24                                                         
FLTREC22 CLI   REQFILT,REQFNO                                                   
         BNE   FLTREC24                                                         
         OC    RFPVNREQ,RFPVNREQ                                                
         BNZ   FLTRECN                                                          
                                                                                
FLTREC24 CLI   CALFILT,CALFNUL     APPLY CALENDAR FILTER                        
         BE    FLTREC28                                                         
         CLI   CALFILT,CALFYES                                                  
         BNE   FLTREC26                                                         
         OC    RFPVRNGE,RFPVRNGE   ?????????????????                            
         BZ    FLTRECN                                                          
         B     FLTREC28                                                         
FLTREC26 CLI   CALFILT,CALFNO                                                   
         BNE   FLTREC28                                                         
         OC    RFPVRNGE,RFPVRNGE   ?????????????????                            
         BNZ   FLTRECN                                                          
                                                                                
FLTREC28 CLI   SEFILT,0            APPLY SE NUMBER FILTER                       
         BE    FLTREC30                                                         
         GOTOR AGETUID,GRPLUSER    CALL GETUID TO BUILD LIST OF SE#'S           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,PCWORK+(GIDSEL-GIDTABD)                                       
         LHI   R0,GIDSELN                                                       
         CLC   SEFILT,0(RE)        MATCH ENTRY IN LIST TO FILTER                
         BE    FLTREC30                                                         
         AHI   RE,L'GIDSEL         BUMP TO NEXT                                 
         BCT   R0,*-14             AND DO FOR NUMBER IN LIST                    
         B     FLTRECN                                                          
                                                                                
FLTREC30 DS    0H                                                               
*&&UK                                                                           
         TM    RFPVHSTA,GRPHSPVT   TEST GROUP IS PRIVATE                        
         BZ    FLTREC32                                                         
         TM    CUSTAT,CUSDDS                                                    
         BNZ   FLTREC32                                                         
         CLC   CUPASS,RFPVPERS     TEST OWNER IS LISTER                         
         BE    FLTREC32                                                         
         TM    CUAUTH,X'80'        TET USER IS AUTHORIZED                       
         BZ    FLTRECN                                                          
                                                                                
FLTREC32 DS    0H                                                               
         CLI   PRVTFILT,0          TEST PRIVATE FILTER SET                      
         BE    FLTREC34                                                         
         LHI   RE,PRVTFXGR                                                      
         TM    RFPVHSTA,GRPHSPVT                                                
         BZ    *+8                                                              
         LHI   RE,PRVTFXPR                                                      
         EX    RE,*+8                                                           
         BZ    FLTRECN                                                          
         TM    PRVTFILT,0                                                       
*&&                                                                             
FLTREC34 CLI   REQFILT,REQFNRUN    TEST REQ=NORUN FILTER SET                    
         BNE   FLTREC38                                                         
         OI    RFPFFLAG,RFPF1STR                                                
                                                                                
FLTREC36 MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         MVI   RFPMODE,RFPRETRQ    SET TO RETURN REQUEST CARDS                  
         GOTOR VRFPIO,PCPARM,RFPD                                               
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   RFPRET,RFPEOF       TEST END OF REQUESTS                         
         BE    FLTRECN                                                          
         CLI   RFPVREQD,RFPVRDIS   TEST REQUEST IS DISABLED                     
         BNE   FLTREC36                                                         
                                                                                
FLTREC38 CLI   SOFTFILT,SOFTFNUL   TEST ANY SOFTDATE FILTER SET                 
         BE    FLTREC46                                                         
         GOTOR GETGRP              GET RFP GROUP VALUES                         
         SR    R0,R0                                                            
         ICM   R0,1,RFPVNUMS       R0=N'RFP SYMBOLS IN GROUP                    
         BZ    FLTRECN                                                          
         LA    R1,RFPD                                                          
         AHI   R1,RFPVSYMX-RFPD    R1=A(SYMBOL LIST)                            
         USING RFPVSYME,R1                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
                                                                                
FLTREC40 CLI   RFPVCVAL,C'|'       TEST SOFT DATE EXPRESSION                    
         BE    *+8                                                              
         CLI   RFPVCVAL,C'!'                                                    
         BNE   *+12                                                             
         AHI   RE,1                BUMP N'SOFT DATES                            
         B     FLTREC42                                                         
         CLI   RFPVCVAL,C' '       BUMP N'UNRESOLVED DATES                      
         BH    FLTREC42                                                         
         AHI   RF,1                                                             
FLTREC42 AHI   R1,RFPVSYML                                                      
         BCT   R0,FLTREC40                                                      
         DROP  R1                                                               
                                                                                
         CLI   SOFTFILT,SOFTFYES   TEST SOFTDATE=YES FILTER                     
         BNE   FLTREC44                                                         
         LTR   RE,RE                                                            
         BZ    FLTRECN                                                          
         B     FLTREC46                                                         
                                                                                
FLTREC44 CLI   SOFTFILT,SOFTFNOS   TEST SOFTDATE=NOSET FILTER SET               
         BNE   FLTREC46                                                         
         LTR   RE,RE                                                            
         BZ    FLTRECN                                                          
         LTR   RF,RF                                                            
         BZ    FLTRECN                                                          
                                                                                
FLTREC46 CLI   SOFTFILT,SOFTFNON   TEST SOFTDATE=NONE FILTER SET                
         BNE   FLTREC48                                                         
         LTR   RE,RE                                                            
         BNZ   FLTRECN                                                          
                                                                                
FLTREC48 DS    0H                                                               
                                                                                
FLTRECY  CLI   *+1,0               SET CC=EQUAL                                 
         B     EXIT                                                             
FLTRECN  CLI   *+0,0               SET CC=NOT EQUAL                             
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET RFP GROUP DETAILS FOR FILTER ROUTINE                            *         
***********************************************************************         
                                                                                
         USING GLWORKD,RC                                                       
GETGRP   NTR1  ,                                                                
         TM    GLFLAG,GLFHAVG      TEST GROUP ALREADY READ                      
         BNZ   GETGRPX                                                          
                                                                                
         L     R2,AIO1                                                          
         USING GRPKEYD,R2                                                       
         L     R3,ARFPIOB                                                       
         USING RFPD,R3                                                          
         MVC   CUUSER,GRPKUSER     CALL RFPIO TO VALIDATE GROUP                 
         MVC   CUAALF,GRPKAGY                                                   
         MVC   CUSYSL,GRPKSYST                                                  
         LA    R1,GRPKGRP                                                       
         ICM   R1,8,=AL1(RFPXRDUP)                                              
         GOTOR AVALGRP                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    RFPFRQID,RFPFRQID                                                
         XC    RFPFSORT,RFPFSORT                                                
         XC    RFPFSEQN,RFPFSEQN                                                
         MVI   RFPFFLAG,RFPF1STR                                                
         MVI   RFPFLAGS,RFPXRDUP+RFPXSYMS                                       
         MVI   RFPMODE,RFPRETRQ                                                 
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   RFPVSEQN,0                                                       
         CLI   RFPRET,RFPEOF                                                    
         BE    *+8                                                              
         MVI   RFPVSEQN,1                                                       
                                                                                
         OI    GLFLAG,GLFHAVG      TEST GROUP ALREADY READ                      
                                                                                
GETGRPX  B     EXIT                                                             
         DROP  R2,R3,RC                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CREATE A GROUP LIST TABLE ELEMENT                        *         
*                                                                     *         
* NTRY - R1=ZERO TO CREATE AN ENTRY, NON-ZERO TO POST AN ENTRY        *         
*        IOKEY CONTAINS GROUP RECORD KEY (IF R1=ZERO)                 *         
*        LSTTRECN IS RECORD NUMBER OR LIST ENTRY (IF R1=NON-ZERO)     *         
*        CSLSTNUM IS NUMBER OF ENTRIES IN PAGE SO FAR                 *         
* EXIT - CC=NOT EQUAL IF PAGE FULL - GLNXTKEY IS NEXT GROUP KEY       *         
***********************************************************************         
                                                                                
LSTADD   NTR1  ,                                                                
         LTR   R1,R1               SAVE GROUP KEY IF ADDING TO LIST             
         BNZ   *+10                                                             
         MVC   GLTBANXT,IOKEY      SAVE NEXT KEY                                
         SR    RE,RE                                                            
         ICM   RE,1,CSLSTNUM                                                    
         LA    R0,1(RE)                                                         
         CHI   R0,GLISTMAX         TEST TABLE FULL                              
         BH    LSTADDN                                                          
         STC   R0,CSLSTNUM                                                      
         LTR   R1,R1               TEST CREATE/POST                             
         BNZ   LSTADD02                                                         
         GOTOR ATSARIO,TSAADD      ADD LIST ENTRY TO TSAR                       
                                                                                
LSTADD02 OC    CSPAG#LO,CSPAG#LO   SET LOW & HIGH RECORDS FOR PAGE              
         BNZ   *+10                                                             
         MVC   CSPAG#LO,LSTTRECN                                                
         MVC   CSPAG#HI,LSTTRECN                                                
                                                                                
LSTADDY  CLI   *+1,0               SET CC=EQUAL                                 
         B     EXIT                                                             
LSTADDN  CLI   *+0,0               SET CC=NOT EQUAL FOR FULL PAGE               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD LIST TABLE ENTRY                                   *         
***********************************************************************         
                                                                                
LSTBLD   NTR1  ,                                                                
         LR    R4,R1               SAVE CALLING TYPE                            
         XC    LSTTKEY(LSTTDATL),LSTTKEY                                        
         L     R3,ARFPIOB                                                       
         USING RFPD,R3             R3=A(RFP INTERFACE BLOCK)                    
         LTR   R4,R4                                                            
         BNZ   LSTBLD02                                                         
         LA    R2,IOKEY                                                         
         USING GRPKEYD,R2          R2=A(GROUP/XFILE RECORD)                     
         MVC   LSTTDA,GRPKDA       SET DISK ADDRESS                             
         MVC   GRPSTAT,GRPKSTAT    EXTRACT FIRST STATUS BYTE                    
                                                                                
         CLI   CSREC,RECGRP        TEST XFILE GROUP RECORD                      
         BNE   LSTBLD04                                                         
                                                                                
         MVI   LSTTRTYP,RECGRP     SET GROUP LIST ENTRY                         
         MVC   GRPLSYST,GRPKSYST   SET SYSTEM                                   
         MVC   GRPLAGY,GRPKAGY     SET AGENCY                                   
         MVC   GRPLUSER,GRPKUSER   SET USER                                     
         MVC   GRPLGRP,GRPKGRP     SET GROUP CODE                               
         B     LSTBLD06                                                         
                                                                                
LSTBLD02 MVI   LSTTRTYP,RECGRP     SET GROUP LIST ENTRY                         
         MVC   GRPLSYST,RFPVSYST   SET SYSTEM                                   
         MVC   GRPLAGY,RFPVAGY     SET AGENCY                                   
         MVC   GRPLUSER,RFPVUSER   SET USER                                     
         MVC   GRPLGRP,RFPVGRP     SET GROUP CODE                               
         B     LSTBLD07                                                         
                                                                                
LSTBLD04 CLI   CSREC,RECXFG        SET DISK ADDRESS                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING XFILED,R2                                                        
         MVI   LSTTRTYP,RECXFG     SET XFILE LIST ENTRY                         
         MVC   GRPLAGY,XFAGY       SET AGENCY                                   
         MVC   GRPLUSER,XFUSER     SET USER                                     
         MVC   GRPLGRP,XFGRP       SET GROUP CODE                               
                                                                                
LSTBLD06 L     R2,AIO1                                                          
         CLI   XFFRSTEL,GRPHCDQ    FIRST ELEMENT MUST BE HEADER                 
         BNE   LSTBLDN                                                          
                                                                                
         GOTOR AXFGRFP             EXTRACT VALUES IN RFPD                       
                                                                                
LSTBLD07 MVC   GRPLFREQ,RFPVFREQ   SET FREQUENCY                                
         MVC   GRPLOTYP,RFPVOTYP   SET OUTPUT TYPE                              
         MVC   GRPLDEST,RFPVDEST   SET DESTINATION ID                           
         MVC   GRPLNAME,RFPVNAME   SET GROUP NAME                               
         MVC   GRPLNXTR,RFPVNXTR   SET NEXT RUN DATE                            
         MVC   GRPLLSTR,RFPVLSTR   SET LAST RUN DATE                            
                                                                                
         MVC   GRPLEND,RFPVENDR    SET END RUN DATE                             
         MVC   GRPLDESC,RFPVDESC   SET GROUP DESCRIPTION                        
         MVC   GRPLXFIL,RFPXFILE   SET XFILE GROUP CODE                         
         MVC   GRPLHSTA,RFPVHSTA   SET GROUP HEADER STATUS                      
         MVC   GRPLPERS,RFPVPERS   SET CREATOR PERSON ID                        
                                                                                
LSTBLD08 CLC   GRPLAGY,CUUALF      TEST CONNECTED USER FOR GROUP                
         BNE   LSTBLD12                                                         
         OC    GRPLXFIL,GRPLXFIL   TEST XFILE ATTACHED TO GROUP                 
         BNZ   LSTBLD12            YES - DON'T ALLOW SUBMIT                     
         OC    GRPLEND,GRPLEND     TEST EFFECTIVE END SET FOR GROUP             
         BZ    LSTBLD10                                                         
         CLC   GRPLEND,ASJDAT      TEST ENDED BEFORE TODAY                      
         BL    LSTBLD12                                                         
         GOTOR AGETNXT             GET NEXT RUN DATE FOR GROUP                  
         BNE   LSTBLD12                                                         
LSTBLD10 OC    LSTTMASK,=AL2(GRPMSUBQ)                                          
                                                                                
LSTBLD12 DS    0H                                                               
                                                                                
LSTBLDY  CLI   *+1,0                                                            
         B     EXIT                                                             
LSTBLDN  CLI   *+0,0                                                            
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD COLUMN HEADINGS AND DISPLAY DISPLACEMENTS          *         
***********************************************************************         
                                                                                
         USING BDWORKD,RC                                                       
BLDCOL   NTR1  WORK=(RC,BDWORKX-BDWORKD)                                        
         XC    BDWORKD(BDWORKX-BDWORKD),BDWORKD                                 
         MVC   BLDHEAD1,PCSPACES                                                
         MVC   BLDHEAD2,PCSPACES                                                
         XC    BLDSDSP(BLDSDSPL),BLDSDSP                                        
         SR    R0,R0                                                            
         IC    R0,GLODINUM         R0=NUMBER OF DISPLAYED COLUMNS               
         SR    R1,R1                                                            
         IC    R1,GLODINDX         R1=INDEX TO DISPLAYED COLUMNS                
                                                                                
         TM    PCSCROLL,PFKIHORZ   TEST HORIZONTAL SCROLL SPECIFIED             
         BZ    BLDCOL12                                                         
         TM    PCSCROLL,PFKIUPDN   TEST SCROLL LEFT                             
         BNZ   BLDCOL08                                                         
                                                                                
         TM    PCSCRNUM,PFKIPAGE   SCROLL RIGHT                                 
         BZ    *+10                                                             
         AR    R1,R0                                                            
         B     BLDCOL12                                                         
         TM    PCSCRNUM,PFKIHALF                                                
         BZ    BLDCOL02                                                         
         SRA   R0,1                                                             
         BNZ   *+6                                                              
         SR    R1,R1                                                            
         AR    R1,R0                                                            
         B     BLDCOL12                                                         
BLDCOL02 MVC   BDTEMP(1),PCSCRNUM                                               
         NI    BDTEMP,X'0F'                                                     
         IC    R0,BDTEMP                                                        
         AR    R1,R0                                                            
         B     BLDCOL12                                                         
                                                                                
BLDCOL08 TM    PCSCRNUM,PFKIMAXN   TEST MAXIMUM SCROLL LEFT                     
         BZ    *+10                                                             
         SR    R1,R1                                                            
         B     BLDCOL12                                                         
         TM    PCSCRNUM,PFKIHALF                                                
         BZ    *+14                                                             
         SRL   R0,1                                                             
         SR    R1,R0                                                            
         B     BLDCOL12                                                         
         TM    PCSCRNUM,PFKIPAGE                                                
         BZ    BLDCOL10                                                         
         LTR   R1,R1                                                            
         BZ    BLDCOL12                                                         
         LA    RE,BDTEMP           INVERT DISPLAY PROFILE                       
         LA    RF,GLODIS(R1)                                                    
         BCTR  RF,0                                                             
         MVC   0(1,RE),0(RF)                                                    
         AHI   RE,1                                                             
         BCT   R1,*-12                                                          
         OI    BDFLAG,BDFINVRT                                                  
         B     BLDCOL22                                                         
BLDCOL10 MVC   BDTEMP(1),PCSCRNUM                                               
         NI    BDTEMP,X'0F'                                                     
         IC    R0,BDTEMP                                                        
         SR    R1,R0                                                            
                                                                                
BLDCOL12 LA    RE,GLODIS+L'GLODIS-1                                             
         LHI   RF,L'GLODIS                                                      
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         DC    H'0'                                                             
         LTR   R1,R1                                                            
         BM    BLDCOL14                                                         
         CR    R1,RF                                                            
         BL    BLDCOL16                                                         
                                                                                
BLDCOL14 SR    R1,R1                                                            
                                                                                
BLDCOL16 STC   R1,GLODINDX                                                      
         SR    RF,R1                                                            
         LA    R1,GLODIS(R1)                                                    
         EX    RF,*+8                                                           
         B     BLDCOL22                                                         
         MVC   BDTEMP(0),0(R1)                                                  
                                                                                
BLDCOL22 LA    R1,BDTEMP           R1=A(DISPLAY COLUMN PROFILE)                 
         SR    R6,R6               R6=DISPLACEMENT TO DISPLAY VALUE             
                                                                                
BLDCOL24 CLI   0(R1),0             TEST END OF COLUMN DEFINITIONS               
         BE    BLDCOL42                                                         
         L     R3,OVADDR1                                                       
         USING DISTABD,R3          R3=A(DISPLAY COLUMN TABLE)                   
BLDCOL26 CLI   DISTABD,EOT         TEST EOT                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DISCOL,0(R1)        TEST COLUMN MATCH                            
         BE    *+12                                                             
         AHI   R3,DISTABL                                                       
         B     BLDCOL26                                                         
                                                                                
         TM    BDFLAG,BDFPASS2     TEST SECOND PASS                             
         BNZ   BLDCOL32                                                         
                                                                                
         SR    RE,RE                                                            
         IC    RE,DISWID                                                        
         AR    RE,R6               ADD DISPLACEMENT SO FAR                      
         CHI   RE,L'GLILIN1                                                     
         BH    BLDCOL42            OVERFLOW - DROP THIS COLUMN                  
         LA    R6,1(RE)            R6=DISPLACEMENT TO NEXT COLUMN               
         SR    RF,RF                                                            
         IC    RF,BDCOLS           BUMP NUMBER OF DISPLAY COLUMNS               
         AHI   RF,1                                                             
         STC   RF,BDCOLS                                                        
         B     BLDCOL40                                                         
                                                                                
BLDCOL32 LA    RF,L'FVIHDR(R6)     DISPLACEMENT SO FAR +L'HEADER                
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         STC   RF,BLDSDSP-1(RE)    SET SCREEN LINE DISPLACEMENT                 
         SR    RF,RF                                                            
         ICM   RF,3,DISNAM1                                                     
         BZ    BLDCOL34                                                         
         LA    RF,WORKD(RF)                                                     
         SR    RE,RE                                                            
         ICM   RE,1,DISHWI         HEADING WIDTH OVERRIDE                       
         BNZ   *+8                                                              
         IC    RE,DISWID           COLUMN WIDTH                                 
         BCTR  RE,0                                                             
         LA    R4,BLDHEAD1(R6)                                                  
         EX    RE,*+8                                                           
         B     BLDCOL34                                                         
         MVC   0(0,R4),0(RF)                                                    
                                                                                
BLDCOL34 SR    RF,RF                                                            
         ICM   RF,3,DISNAM2        TEST SECOND HEADING                          
         BZ    BLDCOL36                                                         
         LA    RF,WORKD(RF)        BUILD SECOND HEADING                         
         LA    R4,BLDHEAD2(R6)     INDEX TO SECOND HEADLINE                     
         SR    RE,RE                                                            
         ICM   RE,1,DISHWI         HEADING WIDTH OVERRIDE                       
         BNZ   *+8                                                              
         IC    RE,DISWID           COLUMN WIDTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     BLDCOL38                                                         
         MVC   0(0,R4),0(RF)                                                    
                                                                                
BLDCOL36 CLC   DISNAM1,BZEROES     TEST FIRST HEADING TO UNDERLINE              
         BE    BLDCOL38                                                         
         LA    RF,0(RE,R4)         RF=A(LAST POSSIBLE CHARACTER)                
         CLI   0(R4),SPACE         LOCATE FIRST CHARACTER                       
         BH    *+12                                                             
         AHI   R4,1                                                             
         B     *-12                                                             
         CLI   0(RF),SPACE         LOCATE LAST CHARACTER                        
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R4               RF=EXECUTE L'HEADING                         
         BM    BLDCOL38            NOTHING TO UNDERLINE                         
         AHI   R4,L'BLDHEAD2       INDEX TO SECOND HEADLINE                     
         MVI   0(R4),USCOREQ       UNDERLINE FIRST CHARACTER                    
         AHI   RF,-1               DROP A CHARACTER FOR SECOND MOVE             
         BM    BLDCOL38            SINGLE CHARACTER HEADING                     
         EX    RF,*+8                                                           
         B     BLDCOL38                                                         
         MVC   1(0,R4),0(R4)       UNDERLINE REST OF FIRST HEADING              
                                                                                
BLDCOL38 SR    RE,RE                                                            
         IC    RE,DISWID           RESET COLUMN WIDTH                           
         LA    R6,1(R6,RE)         R6=DISPLACEMENT TO NEXT COLUMN               
         A     R6,BDFULL1          ADD CONSTANT FACTOR FOR SPARE                
                                                                                
BLDCOL40 AHI   R1,1                BUMP TO NEXT FIELD                           
         B     BLDCOL24            PERFORM FOR NUMBER OF PROFILES               
                                                                                
BLDCOL42 TM    BDFLAG,BDFPASS2     TEST SECOND PASS                             
         BNZ   BLDCOLX             YES - WE HAVE FINISHED                       
         TM    BDFLAG,BDFINVRT     TEST INVERTED LIST                           
         BZ    BLDCOL44                                                         
         MVC   BDSAVE,BDTEMP       RE-INVERT LIST                               
         SR    R1,R1                                                            
         ICM   R1,1,BDCOLS                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RF,BDSAVE(R1)                                                    
         LA    RE,BDTEMP                                                        
         BCTR  RF,0                                                             
         MVC   0(1,RE),0(RF)                                                    
         AHI   RE,1                                                             
         BCT   R1,*-12                                                          
                                                                                
         SR    R1,R1               ADJUST INDEX VALUE                           
         IC    R1,BDCOLS                                                        
         SR    R0,R0                                                            
         IC    R0,GLODINDX                                                      
         SR    R0,R1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         STC   R0,GLODINDX                                                      
         NI    BDFLAG,FF-BDFINVRT                                               
                                                                                
BLDCOL44 OI    BDFLAG,BDFPASS2     SET SECOND TIME                              
         SR    R1,R1                                                            
         IC    R1,BDCOLS                                                        
         LA    R1,BDTEMP(R1)                                                    
         MVI   0(R1),0             SET END OF COLUMN DEFINITION                 
         LHI   R1,L'GLILIN1+1                                                   
         SR    R1,R6               GET REMAINING SPACE INTO R1                  
         BNP   BLDCOL22            NOTHING LEFT TO ADD                          
         CLI   BDCOLS,1            TEST SINGLE COLUMN                           
         BE    BLDCOL22            DON'T DIVIDE OR SET FACTOR                   
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    RF,BDCOLS           NO. OF COLS (THOUGH ONE IS FIXED)            
         BCTR  RF,0                                                             
         DR    R0,RF                                                            
         ST    R1,BDFULL1          SET CONSTANT FACTOR FOR SPARE                
         B     BLDCOL22            GO BACK AND SET DISPLACEMENTS                
                                                                                
BLDCOLX  MVC   GLODINUM,BDCOLS     SET NUMBER OF COLUMNS PROCESSED              
         B     EXIT                                                             
         DROP  R3,RC                                                            
                                                                                
BDWORKD  DSECT                     ** BLDCOL LOCAL W/S **                       
BDFULL1  DS    F                                                                
BDTEMP   DS    XL64                                                             
BDSAVE   DS    XL64                                                             
BDFLAG   DS    X                                                                
BDFPASS2 EQU   X'80'               ON=SECOND PASS                               
BDFINVRT EQU   X'40'               ON=INVERTED LIST                             
BDCOLS   DS    X                                                                
BDCOLUMN DS    X                                                                
BDWORKX  EQU   *                                                                
RLP01    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A DISPLAY LINE USING BLDCOL DISPLACEMENTS          *         
***********************************************************************         
                                                                                
BLDLIN   NTR1  ,                                                                
         LA    R3,0(,R1)           SAVE A(OUTPUT FIELD)                         
         MVC   IODAOVER,LSTTDA     GET XFILE RECORD INTO STORAGE                
         GOTOR AIO,'IOGET+IOGENFIS+IO1'                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AXFGRFP             FILL RFP BLOCK VALUES                        
                                                                                
         MVC   L'FVIHDR(L'GLILIN1,R3),PCSPACES                                  
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
         NI    FVATRB-FVIHDR(R3),FF-FVAHIGH                                     
         MVI   FVTLEN,L'FVIHDR+L'FVIFLD                                         
         SR    R4,R4                                                            
         ICM   R4,1,BLDDAD         DISK ADDRESS                                 
         BZ    BLDLIN02                                                         
         LA    R5,0(R4,R3)                                                      
         GOTOR VHEXOUT,PCPARM,LSTTDA,(R5),L'LSTTDA,=C'TOG'                      
                                                                                
BLDLIN02 ICM   R4,1,BLDRECND       RECORD NUMBER                                
         BZ    BLDLIN04                                                         
         LA    R5,0(R4,R3)                                                      
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN                                                    
         CVD   R1,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  0(4,R5),PCDUB                                                    
                                                                                
BLDLIN04 ICM   R4,1,BLDSYSTD       SYSTEM                                       
         BZ    BLDLIN06                                                         
         CLI   LSTTRTYP,RECGRP     TEST GROUP RECORD                            
         BNE   BLDLIN06                                                         
         LA    R5,0(R4,R3)                                                      
         GOTOR AGETSYS,GRPLSYST                                                 
         MVC   0(L'SYSLNAME,R5),PCWORK                                          
                                                                                
BLDLIN06 ICM   R4,1,BLDAGYD        AGENCY                                       
         BZ    BLDLIN08                                                         
         LA    R5,0(R4,R3)                                                      
         MVC   0(L'GRPLAGY,R5),GRPLAGY                                          
                                                                                
BLDLIN08 ICM   R4,1,BLDUSERD       USER#                                        
         BZ    BLDLIN10                                                         
         LA    R5,0(R4,R3)                                                      
         SR    R1,R1                                                            
         ICM   R1,3,GRPLUSER                                                    
         CVD   R1,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  0(5,R5),PCDUB                                                    
                                                                                
BLDLIN10 ICM   R4,1,BLDDESTD       DEST#                                        
         BZ    BLDLIN12                                                         
         LA    R5,0(R4,R3)                                                      
         SR    R1,R1                                                            
         ICM   R1,3,GRPLDEST                                                    
         CVD   R1,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  0(5,R5),PCDUB                                                    
                                                                                
BLDLIN12 ICM   R4,1,BLDUSRCD       USER ID                                      
         BZ    BLDLIN14                                                         
         LA    R5,0(R4,R3)                                                      
         GOTOR AGETUID,GRPLUSER    SET USER-ID CODE                             
*        BE    *+10                                                             
*        MVC   PCWORK+(GIDCODE-GIDTABD),=CL10'UNKNOWN'                          
         MVC   0(L'GIDCODE,R5),PCWORK+(GIDCODE-GIDTABD)                         
*                                                                               
BLDLIN14 ICM   R4,1,BLDGRPD        GROUP CODE                                   
         BZ    BLDLIN16                                                         
         LA    R5,0(R4,R3)                                                      
         MVC   0(L'GRPLGRP,R5),GRPLGRP                                          
         CLI   GRPLGRP,GRPKDTFQ    TEST DESKTOP FOLDER GROUP                    
         BNE   BLDLIN16                                                         
         MVC   PCWORK(L'GRPLAGY),GRPLAGY                                        
         MVC   PCWORK+L'GRPLAGY(L'GRPKDTFP),GRPLGRP+L'GRPKDTF                   
         GOTOR AGETPER,PCWORK                                                   
         MVC   0(8,R5),PCWORK                                                   
                                                                                
BLDLIN16 ICM   R4,1,BLDFRQND       FREQUENCY NAME                               
         BZ    BLDLIN20                                                         
         LA    R5,0(R4,R3)                                                      
         GOTOR AGETFRQ,GRPLFREQ                                                 
         MVC   0(FRQNAMLQ,R5),PCWORK                                            
                                                                                
BLDLIN20 ICM   R4,1,BLDOTYPD       OUTPUT TYPE                                  
         BZ    BLDLIN22                                                         
         LA    R5,0(R4,R3)                                                      
         MVC   0(L'GRPLOTYP,R5),GRPLOTYP                                        
                                                                                
BLDLIN22 ICM   R4,1,BLDDSTCD       DESTINATION ID                               
         BZ    BLDLIN24                                                         
         OC    GRPLDEST,GRPLDEST                                                
         BZ    BLDLIN24                                                         
         LA    R5,0(R4,R3)                                                      
         GOTOR AGETUID,GRPLDEST    SET DESTINATION ID CODE                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'GIDCODE,R5),PCWORK+(GIDCODE-GIDTABD)                         
                                                                                
BLDLIN24 ICM   R4,1,BLDNAMED       FILTER NAME                                  
         BZ    BLDLIN26                                                         
         LA    R5,0(R4,R3)                                                      
         MVC   0(L'GRPLNAME,R5),GRPLNAME                                        
         CLI   GRPLGRP,GRPKDTFQ    TEST DESKTOP FOLDER GROUP                    
         BNE   BLDLIN26                                                         
         MVC   PCWORK(L'GRPKDTFI),GRPLGRP+(GRPKDTFI-GRPKDTF)                    
         XC    PCWORK(L'GRPKDTFI),PCEFFS                                        
         SR    R1,R1                                                            
         ICM   R1,7,PCWORK                                                      
         CVD   R1,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,X'0F'                                            
         MVI   0(R5),C'#'                                                       
         UNPK  1(L'GRPLNAME-1,R5),PCDUB                                         
                                                                                
BLDLIN26 ICM   R4,1,BLDNXTRD       NEXT RUN DATE                                
         BZ    BLDLIN32                                                         
         LA    R5,0(R4,R3)                                                      
         GOTOR AGETNXT             GET NEXT RUN DATE                            
         BNE   BLDLIN32                                                         
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',FVIHDR),('SOFIIONE',0)                      
         MVC   0(BLDNXTRW,R5),FVIFLD                                            
                                                                                
BLDLIN32 ICM   R4,1,BLDLSTRD       LAST RUN DATE                                
         BZ    BLDLIN34                                                         
         GOTOR AGETLST             GET LAST RUN DATE                            
         BNE   BLDLIN34                                                         
         LA    R5,0(R4,R3)                                                      
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',FVIHDR),('SOFIIONE',0)                      
         MVC   0(BLDLSTRW,R5),FVIFLD                                            
                                                                                
BLDLIN34 ICM   R4,1,BLDENDD        END RUN DATE                                 
         BZ    BLDLIN36                                                         
         LA    R5,0(R4,R3)                                                      
         GOTOR AGETEND             GET END RUN DATE                             
         BNE   BLDLIN36                                                         
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',FVIHDR),('SOFIIONE',0)                      
         MVC   0(BLDENDW,R5),FVIFLD                                             
                                                                                
BLDLIN36 ICM   R4,1,BLDXFILD       XFILE GROUP                                  
         BZ    BLDLIN38                                                         
         CLI   LSTTRTYP,RECGRP     TEST GROUP RECORD                            
         BNE   BLDLIN38                                                         
         LA    R5,0(R4,R3)                                                      
         MVC   0(L'GRPLXFIL,R5),GRPLXFIL                                        
                                                                                
BLDLIN38 ICM   R4,1,BLDDESCD       DESCRIPTION                                  
         BZ    BLDLIN40                                                         
         LA    R5,0(R4,R3)                                                      
         MVC   0(L'GRPLDESC,R5),GRPLDESC                                        
                                                                                
BLDLIN40 ICM   R4,1,BLDFREQD       FREQUENCY CODE                               
         BZ    BLDLIN42                                                         
         LA    R5,0(R4,R3)                                                      
         MVC   0(L'GRPLFREQ,R5),GRPLFREQ                                        
                                                                                
BLDLIN42 ICM   R4,1,BLDEFDTD       EFFECTIVE DATES                              
         BZ    BLDLIN44                                                         
         OC    GRPLNXTR,GRPLNXTR   TEST NEXT RUN DATE SET                       
         BZ    BLDLIN44                                                         
         MVC   PCWORK(L'GRPLNXTR),GRPLNXTR                                      
         MVC   PCWORK+L'GRPLNXTR(L'GRPLEND),GRPLEND                             
         SR    R0,R0                                                            
         OC    GRPLEND,GRPLEND                                                  
         BNZ   *+8                                                              
         LHI   R0,SOFIIONE                                                      
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',FVIHDR),((R0),0)                            
         LA    R5,0(R4,R3)                                                      
         MVC   0(BLDEFDTW,R5),FVIFLD                                            
                                                                                
BLDLIN44 ICM   R4,1,BLDRUNSD       RUN SCHEDULE                                 
         BZ    BLDLIN46                                                         
         L     R1,ARFPIOB                                                       
         LA    R1,RFPVDAYS-RFPD(R1)                                             
         ICM   R1,8,=X'80'                                                      
         GOTOR AGETRSH,(R1)                                                     
         LA    R5,0(R3,R4)                                                      
         MVC   0(BLDRUNSW,R5),PCWORK                                            
                                                                                
BLDLIN46 ICM   R4,1,BLDACTSD       VALID ACTIONS                                
         BZ    BLDLIN56                                                         
         LA    R5,0(R3,R4)                                                      
         MVC   PCWORK,PCSPACES                                                  
         LA    RF,PCWORK                                                        
         L     R4,AOVERSEL                                                      
         USING SELTABD,R4                                                       
BLDLIN48 CLI   SELTABD,EOT         TEST EOT                                     
         BE    BLDLIN52                                                         
         LR    R0,RF                                                            
         GOTOR ATSTMIX,SELTRECA    VALIDATE RECORD/ACTION                       
         LR    RF,R0                                                            
         BNE   BLDLIN50                                                         
         MVC   PCHALF,SELTMASK                                                  
         NC    PCHALF,LSTTMASK                                                  
         CLC   PCHALF,SELTMASK                                                  
         BNE   BLDLIN50                                                         
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPM       MIXED CASE ACTION WORD                       
         LA    RE,WORKD(RE)                                                     
         MVC   0(3,RF),0(RE)                                                    
         AHI   RF,3                                                             
         MVC   0(L'PCCOMMA,RF),PCCOMMA                                          
         AHI   RF,L'PCCOMMA                                                     
BLDLIN50 AHI   R4,SELTABL                                                       
         B     BLDLIN48                                                         
         DROP  R4                                                               
                                                                                
BLDLIN52 SR    R4,R4                                                            
         CLC   PCWORK,PCSPACES                                                  
         BE    BLDLIN56                                                         
         LA    RF,PCWORK+BLDACTSW-1                                             
         CLC   0(L'PCCOMMA,RF),PCCOMMA                                          
         BE    BLDLIN54                                                         
         MVI   0(RF),SPACE                                                      
         BCTR  RF,0                                                             
         CLC   0(L'PCCOMMA,RF),PCCOMMA                                          
         BNE   *-12                                                             
BLDLIN54 MVI   0(RF),SPACE                                                      
         MVC   0(BLDACTSW,R5),PCWORK                                            
         CLI   PCWORK+BLDACTSW+1,SPACE                                          
         BNH   *+8                                                              
         MVI   BLDACTSW-1(R5),C'>'                                              
                                                                                
BLDLIN56 ICM   R4,1,BLDSTATD       STATUS                                       
         BZ    BLDLIN62                                                         
         LA    R5,0(R3,R4)                                                      
         L     R1,ARFPIOB                                                       
         LA    R1,RFPVGSTA-RFPD(R1)                                             
         TM    0(R1),RFPVGSDQ      TEST DELETED                                 
         BZ    BLDLIN58                                                         
         MVC   0(L'PCM3DEL,R5),PCM3DEL                                          
         MVC   L'PCM3DEL(L'PCCOMMA,R5),PCCOMMA                                  
         AHI   R5,L'PCM3DEL+L'PCCOMMA                                           
BLDLIN58 L     R1,ARFPIOB                                                       
         LA    R1,RFPVHSTA-RFPD(R1)                                             
         TM    0(R1),GRPHSPVT      TEST PRIVATE                                 
         BZ    BLDLIN60                                                         
         MVC   0(L'PCM3PRVT,R5),PCM3PRVT                                        
         MVC   L'PCM3PRVT(L'PCCOMMA,R5),PCCOMMA                                 
         AHI   R5,L'PCM3PRVT+L'PCCOMMA                                          
BLDLIN60 SHI   R5,L'PCCOMMA                                                     
         CLC   0(L'PCCOMMA,R5),PCCOMMA                                          
         BNE   BLDLIN62                                                         
         MVI   0(R5),SPACE                                                      
                                                                                
BLDLIN62 DS    0H                                                               
*&&UK                                                                           
         ICM   R4,1,BLDPERSD       PERSON                                       
         BZ    BLDLIN64                                                         
         L     R1,ARFPIOB                                                       
         OC    RFPVPERS-RFPD(,R1),RFPVPERS-RFPD(R1)                             
         BZ    BLDLIN64                                                         
         LA    R5,0(R3,R4)                                                      
         MVC   PCWORK(L'RFPVAGY),RFPVAGY-RFPD(R1)                               
         MVC   PCWORK+L'RFPVAGY(L'RFPVPERS),RFPVPERS-RFPD(R1)                   
         GOTOR AGETPER,PCWORK                                                   
         MVC   0(8,R5),PCWORK                                                   
*&&                                                                             
BLDLIN64 DS    0H                                                               
                                                                                
BLDLINX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MOVE SYSTEM NAMES TO TWA                                 *         
***********************************************************************         
                                                                                
SETSYS   NTR1  ,                                                                
                                                                                
         LA    R5,GRPSYS1H                                                      
         USING GRPSYS1H,R5                                                      
         LA    R6,SYSLST                                                        
         LHI   R0,SYSLSTN                                                       
                                                                                
SETSYS02 L     RE,ASWSTAB          TEST SYSTEM VALID FOR USER                   
         LHI   RF,SYSSWMAX                                                      
         USING SYSSWTAB,RE                                                      
SETSYS04 CLC   SYSSWSOV,0(R6)      MATCH SYSTEM TO SWITCH LIST                  
         BE    SETSYS06                                                         
         AHI   RE,SYSSWLEN                                                      
         BCT   RF,SETSYS04                                                      
         B     SETSYS08                                                         
                                                                                
SETSYS06 GOTOR AGETSYS,(R6)        LOOK UP SYSTEM NAME                          
         MVC   GRPSYS1(L'SYSLNAME),PCWORK                                       
         LA    R5,GRPSYS2H                                                      
                                                                                
SETSYS08 AHI   R6,L'SYSLST         BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,SETSYS02                                                      
                                                                                
SETSYSX  B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD GROUP LIST                                         *         
***********************************************************************         
                                                                                
BLDGRP   NTR1  WORK=(RC,BGWORKX-BGWORKD)                                        
         USING BGWORKD,RC                                                       
         USING RFPD,R3                                                          
         LA    R1,0(R1)            CLEAR HOB                                    
         ST    R1,BGAOUT           SAVE A(OUTPUT LIST)                          
         MVC   BGXGRP,LGRP         SAVE XFILE GROUP CODE                        
                                                                                
         L     R4,BGAOUT           R4=A(OUTPUT LIST)                            
         USING XFGTABD,R4                                                       
         LA    R5,GRPSYS1H                                                      
         USING GRPSYS1H,R5                                                      
         LHI   R6,SYSLSTN                                                       
                                                                                
BLDGRP02 OC    GRPSYS1,GRPSYS1                                                  
         BZ    BLDGRPY                                                          
         GOTOR AVALSYS,GRPSYS1H    GET SYSTEM LETTER                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BGSYSLET,PCWORK     AND SAVE                                     
                                                                                
         GOTOR AFVAL,GRPGRP1H      VALIDATE GROUP(S) LINE                       
         BNE   BLDGRP12                                                         
         MVC   PCPARM+08(2),=C',='                                              
         MVC   PCPARM+10(1),PCCOMMA                                             
         MVI   PCPARM+11,0                                                      
         GOTOR VSCANNER,PCPARM,FVIHDR,('BGSCNMAX',BGSCNTAB)                     
         MVC   PCBYTE1,4(R1)       SAVE NUMBER OF ITEMS INPUT                   
         CLI   PCBYTE1,0                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     BLDGRPN                                                          
                                                                                
         MVI   PCBYTE2,0                                                        
         LA    R2,BGSCNTAB         R2=A(SCANNER TABLE)                          
                                                                                
BLDGRP04 SR    R1,R1                                                            
         IC    R1,PCBYTE2          BUMP INPUT FIELD COUNT                       
         AHI   R1,1                                                             
         STC   R1,PCBYTE2                                                       
         CLC   PCBYTE2,PCBYTE1     TEST ALL ENTRIES PROCESSED                   
         BNH   *+12                                                             
         MVI   FVINDX,0            YES - RESET MULTIPLE FIELD INDEX             
         B     BLDGRP12                                                         
                                                                                
         CLI   PCBYTE1,1           TEST FIELD# 1                                
         BE    *+10                                                             
         MVC   FVINDX,PCBYTE2      NO - SET MULTIPLE FIELD INDEX                
                                                                                
         CLI   0(R2),0             TEST FIELD PRESENT                           
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$MIF)                                             
         B     BLDGRPN                                                          
                                                                                
         CLI   0(R2),L'RFPVGRP     TEST NOT TOO LONG                            
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ILTL)                                            
         B     BLDGRPN                                                          
                                                                                
         GOTOR GETUSR,CUUSER       TEST GROUP EXISTS                            
         MVC   CUAALF,TWAAGY                                                    
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    *+16                                                             
         MVC   CUUSER,GRPLUSER                                                  
         MVC   CUAALF,GRPLAGY                                                   
         MVC   CUSYSL,BGSYSLET                                                  
         GOTOR AVALGRP,12(R2)                                                   
         BL    BLDGRPN                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RECNF)                                           
         B     BLDGRPN                                                          
                                                                                
         MVI   RFPFLAGS,RFPXSYMS   SET TO USE EXTENDED SYMBOLS                  
         OI    RFPFFLAG,RFPF1STR+RFPXRDUP                                       
         XC    RFPFRQID(RFPFNUMR-RFPFRQID),RFPFRQID                             
         MVI   RFPMODE,RFPRETRQ    CHECK GROUP HAS REQUESTS                     
         GOTOR VRFPIO,PCPARM,RFPD                                               
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RFPRET,RFPEOF       OKAY TO DELETE IF NO REQUESTS                
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$NRITG)                                           
         B     BLDGRPN                                                          
                                                                                
         CLC   RFPXFILE,BGXGRP     TEST ATTACHED TO THIS GROUP                  
         BE    BLDGRP06                                                         
         OC    RFPXFILE,RFPXFILE   NO - MUSTN'T BE ATTACHED TO OTHER            
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IREC)                                            
         B     BLDGRPN                                                          
                                                                                
BLDGRP06 L     RE,BGAOUT           ENSURE NOT DUPLICATED GROUP                  
         MVC   PCWORK(L'RFPVSYST),RFPVSYST                                      
         MVC   PCWORK+L'RFPVSYST(L'RFPVGRP),RFPVGRP                             
BLDGRP08 CR    RE,R4                                                            
         BE    BLDGRP10                                                         
         CLC   0(XFGTABL,RE),PCWORK                                             
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$DUPE)                                            
         B     BLDGRPN                                                          
         AHI   RE,XFGTABL                                                       
         B     BLDGRP08                                                         
                                                                                
BLDGRP10 MVC   XFGTSYSL,BGSYSLET   BUILD OUTPUT TABLE ENTRY                     
         MVC   XFGTGRP,RFPVGRP                                                  
         GOTOR AGETSYS,XFGTSYSL                                                 
         MVC   XFGTSYS,PCWORK+L'SYSLNAME+L'SYSLUSLT                             
         AHI   R4,XFGTABL          BUMP TO NEXT TABLE ENTRY                     
                                                                                
         AHI   R2,L'BGSCNTAB       BUMP TO NEXT SCANNER TABLE ENTRY             
         B     BLDGRP04                                                         
                                                                                
BLDGRP12 LA    R5,GRPSYS2H         BUMP TO NEXT INPUT LINE                      
         BCT   R6,BLDGRP02         DO FOR NUMBER OF SYSTEMS                     
                                                                                
BLDGRPY  CLI   *+1,0                                                            
         B     EXIT                                                             
BLDGRPN  CLI   *+0,0                                                            
         B     EXIT                                                             
         DROP  R3,R4,R5,RC                                                      
                                                                                
BGWORKD  DSECT                     ** BLDGRP S/R LOCAL W/S **                   
                                                                                
BGAOUT   DS    A                   A(OUTPUT LIST)                               
                                                                                
BGXGRP   DS    CL(L'LGRP)          XFILE GROUP CODE                             
                                                                                
BGSYSLET DS    CL(L'XFGTSYSL)      SYSTEM LETTER                                
                                                                                
BGSCNMAX EQU   20                  MAXIMUM N'SCANNER TABLE ENTRIES              
BGSCNTAB DS    (BGSCNMAX)CL32      SCANNER TABLE                                
                                                                                
BGWORKX  EQU   *                                                                
                                                                                
RLP01    CSECT                                                                  
         EJECT                                                                  
*&&UK                                                                           
VALPVT   LR    R0,RE                                                            
         L     R1,ARFPIOB                                                       
         NI    RFPVHSTA-RFPD(R1),FF-(GRPHSPVT)                                  
         GOTOR AFVAL,GRPPRVTH      VALIDATE PRIVATE                             
         LA    RE,PCUNO            DEFAULT IS NO (GLOBAL)                       
         BNE   VALPVT02                                                         
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BE    VALPVT02                                                         
         CLC   FVIFLD(0),0(RE)                                                  
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         L     R1,ARFPIOB                                                       
         OI    RFPVHSTA-RFPD(R1),GRPHSPVT                                       
         LA    RE,PCUYES                                                        
         EX    RF,*+8                                                           
         BNE   EXIT                                                             
         CLC   FVIFLD(0),0(RE)                                                  
         CLC   CUUALF,CUAALF       TEST CONNECTED TO USER                       
         BNE   EXIT                                                             
         OC    CUPASS,CUPASS       TEST CONNECTED WITH PASSWORD                 
         BZ    EXIT                                                             
         MVC   RFPVPERS-RFPD(,R1),CUPASS                                        
                                                                                
VALPVT02 CLC   GRPPRVT(L'PCUYES),0(RE)                                          
         BE    VALPVTX                                                          
         MVC   GRPPRVT(L'PCUYES),0(RE)                                          
         OI    GRPPRVTH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
VALPVTX  LR    RE,R0                                                            
         BR    RE                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET USER-ID NUMBER                                       *         
***********************************************************************         
                                                                                
GETUSR   NTR1  LABEL=NO                                                         
         LR    R2,R1                                                            
         MVC   0(L'TWAUSRID,R2),TWAUSRID                                        
         CLI   GRPUSER,C' '                                                     
         JNH   EXIT                                                             
         MVC   OVWORK1(L'CTIKID),GRPUSER                                        
         OC    OVWORK1(L'CTIKID),PCSPACES                                       
         GOTOR AVALUID,OVWORK1                                                  
         JE    GETUSR30                                                         
         CLC   =CL33'RECORD NOT FOUND',PCWORK+(GIDNAME-GIDTABD)                 
         JNE   *+2                                                              
         CLI   CSACT,ACTCHA        ERROR IF ACTION CHANGE                       
         JNE   GETUSR10                                                         
         MVC   FVMSGNO,=AL2(GE$IID)                                             
         J     GETUSR30                                                         
                                                                                
GETUSR10 CLI   CSACT,ACTDEL        ALLOW DELETE IF USER ID NOT FOUND            
         JNE   *+2                                                              
                                                                                
GETUSR30 MVC   0(L'GIDNUM,R2),PCWORK+(GIDNUM-GIDTABD)                           
         J     EXIT                                                             
         EJECT                                                                  
CONLETQ  EQU   C'C'                CONTROL SYSTEM LETTER                        
                                                                                
         LTORG                                                                  
                                                                                
GENFIL   DC    C'GENFILE'                                                       
BZEROES  DC    XL4'00'                                                          
                                                                                
SYSLST   DS    0X                  LIST OF SYSTEMS FOR XFILE SCREEN             
*&&UK*&& DC    AL1(MEDNUMQ,MPLNUMQ,ACCNUMQ,FEENUMQ)                             
*&&US*&& DC    AL1(SPTNUMQ,NETNUMQ,PRTNUMQ,ACCNUMQ,REPNUMQ)                     
SYSLSTN  EQU   *-SYSLST                                                         
         EJECT                                                                  
DISTAB   DS    0X                  ** DISPLAY COLUMNS **                        
                                                                                
         DC    AL1(BLDUSRCQ,BLDUSRCD+1-BLDSDSP,BLDUSRCW)                        
         DC    AL1(L'PCMUSRID),AL2(PCMUSRID-WORKD,0)                            
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDGRPQ,BLDGRPD+1-BLDSDSP,BLDGRPW)                           
         DC    AL1(L'PCMGRPC1),AL2(PCMGRPC1-WORKD,PCMGRPC2-WORKD)               
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDDESCQ,BLDDESCD+1-BLDSDSP,BLDDESCW)                        
         DC    AL1(L'PCMDESC),AL2(PCMDESC-WORKD,0)                              
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDFREQQ,BLDFREQD+1-BLDSDSP,BLDFREQW)                        
         DC    AL1(L'GRPLFREQ),AL2(PCMFRQCY-WORKD,0)                            
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDNAMEQ,BLDNAMED+1-BLDSDSP,BLDNAMEW)                        
         DC    AL1(L'PCMGNAM1),AL2(PCMGNAM1-WORKD,PCMGNAM2-WORKD)               
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDLSTRQ,BLDLSTRD+1-BLDSDSP,BLDLSTRW)                        
         DC    AL1(L'PCMLRUN1),AL2(PCMLRUN1-WORKD,PCMLRUN2-WORKD)               
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDNXTRQ,BLDNXTRD+1-BLDSDSP,BLDNXTRW)                        
         DC    AL1(L'PCMNRUN1),AL2(PCMNRUN1-WORKD,PCMNRUN2-WORKD)               
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDENDQ,BLDENDD+1-BLDSDSP,BLDENDW)                           
         DC    AL1(L'PCMERUN1),AL2(PCMERUN1-WORKD,PCMERUN2-WORKD)               
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDOTYPQ,BLDOTYPD+1-BLDSDSP,BLDOTYPW)                        
         DC    AL1(L'PCMOTYP1),AL2(PCMOTYP1-WORKD,PCMOTYP2-WORKD)               
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDDSTCQ,BLDDSTCD+1-BLDSDSP,BLDDSTCW)                        
         DC    AL1(L'PCMDSTID),AL2(PCMDSTID-WORKD,0)                            
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDXFILQ,BLDXFILD+1-BLDSDSP,BLDXFILW)                        
         DC    AL1(L'PCMXFGC1),AL2(PCMXFGC1-WORKD,PCMXFGC2-WORKD)               
         DC    AL1(DISTIGRP)                                                    
                                                                                
         DC    AL1(BLDFRQNQ,BLDFRQND+1-BLDSDSP,BLDFRQNW)                        
         DC    AL1(L'PCMFRQCY),AL2(PCMFRQCY-WORKD,0)                            
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDEFDTQ,BLDEFDTD+1-BLDSDSP,BLDEFDTW)                        
         DC    AL1(L'PCMEFFDT),AL2(PCMEFFDT-WORKD,0)                            
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDRUNSQ,BLDRUNSD+1-BLDSDSP,BLDRUNSW)                        
         DC    AL1(L'PCMRUNSH),AL2(PCMRUNSH-WORKD,0)                            
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDSTATQ,BLDSTATD+1-BLDSDSP,BLDSTATW)                        
         DC    AL1(L'PCMSTAT),AL2(PCMSTAT-WORKD,0)                              
         DC    AL1(0)                                                           
*&&UK                                                                           
         DC    AL1(BLDPERSQ,BLDPERSD+1-BLDSDSP,BLDPERSW)                        
         DC    AL1(L'PCMPRSN1),AL2(PCMPRSN1-WORKD,PCMPRSN2-WORKD)               
         DC    AL1(0)                                                           
*&&                                                                             
         DC    AL1(BLDACTSQ,BLDACTSD+1-BLDSDSP,BLDACTSW)                        
         DC    AL1(L'PCMVACTS),AL2(PCMVACTS-WORKD,0)                            
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDDAQ,BLDDAD+1-BLDSDSP,BLDDAW)                              
         DC    AL1(L'PCMDSKAD),AL2(PCMDSKAD-WORKD,0)                            
         DC    AL1(DISTIDDS)                                                    
                                                                                
         DC    AL1(BLDRECNQ,BLDRECND+1-BLDSDSP,BLDRECNW)                        
         DC    AL1(L'PCMRNUM),AL2(PCMRNUM-WORKD,0)                              
         DC    AL1(DISTIDDS)                                                    
                                                                                
         DC    AL1(BLDSYSTQ,BLDSYSTD+1-BLDSDSP,BLDSYSTW)                        
         DC    AL1(L'PCMSYSTM),AL2(PCMSYSTM-WORKD,0)                            
         DC    AL1(DISTIGRP)                                                    
                                                                                
         DC    AL1(BLDAGYQ,BLDAGYD+1-BLDSDSP,BLDAGYW)                           
         DC    AL1(L'GRPKAGY),AL2(PCMCPY-WORKD,0)                               
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(BLDUSERQ,BLDUSERD+1-BLDSDSP,BLDUSERW)                        
         DC    AL1(L'PCMUSER#),AL2(PCMUSER#-WORKD,0)                            
         DC    AL1(DISTIDDS)                                                    
                                                                                
         DC    AL1(BLDDESTQ,BLDDESTD+1-BLDSDSP,BLDDESTW)                        
         DC    AL1(L'PCMDEST#),AL2(PCMDEST#-WORKD,0)                            
         DC    AL1(DISTIDDS)                                                    
                                                                                
DISTABX  DC    AL1(EOT)            END-OF-TABLE                                 
                                                                                
DISTABD  DSECT                     ** DISPLAY COLUMN TABLE **                   
DISCHR   DS    X                   COLUMN CHARACTER                             
DISCOL   DS    AL1                 COLUMN NUMBER                                
DISWID   DS    AL1                 COLUMN WIDTH                                 
DISHWI   DS    AL1                 HEADING WIDTH OVERRIDE                       
DISNAM1  DS    AL2                 DISPLACENT TO COLUMN NAME - 1                
DISNAM2  DS    AL2                 DISPLACENT TO COLUMN NAME - 2                
DISTINDS DS    X                   INDICATOR BYTE                               
DISTIDDS EQU   X'80'               DDS ONLY DISTAB ENTRY                        
DISTIGRP EQU   X'40'               GROUP LIST ONLY                              
DISTIXFG EQU   X'20'               XFILE LIST ONLY                              
DISTABL  EQU   *-DISTABD                                                        
RLP01    CSECT                                                                  
         EJECT                                                                  
GLOTAB   DS    0X                  ** GROUP LIST OPTION TABLE **                
                                                                                
         DC    AL2(PCUDIS-WORKD,PCUDIS-WORKD)                                   
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,GLODISL,GLODISL)                                 
         DC    AL1(1)                                                           
         DC    AL2(1,GLODIS-GLVALS)                                             
         DC    CL4'++'                                                          
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(PCUUSRID-WORKD,PCUUSRID-WORKD)                               
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'GIDCODE,UIDRNGEL)                              
         DC    AL1(2)                                                           
         DC    AL2(2,UIDFLTS-GLVALS)                                            
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(PCUSYSTM-WORKD,PCUSYSTM-WORKD)                               
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,RECGRP,0,0,1,SYSFMAXN,SYSFLTSL)                          
         DC    AL1(3)                                                           
         DC    AL2(3,SYSFLTS-GLVALS)                                            
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(PCUAGY-WORKD,PCUAGY-WORKD)                                   
         DC    AL1(OPTNRTN+OPTIDDS,0)                                           
         DC    AL1(0,0,0,0,0,1,L'PCUYES,AGYFLTSL)                               
         DC    AL1(4)                                                           
         DC    AL2(4,AGYFLTS-GLVALS)                                            
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(PCUIDGRL-WORKD,PCUIDGRS-WORKD)                               
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,RECGRP,0,0,1,L'GIDCODE,UIDFLTSL)                         
         DC    AL1(5)                                                           
         DC    AL2(5,UIDFLTS-GLVALS)                                            
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(PCUXFILE-WORKD,PCUXFILE-WORKD)                               
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,RECGRP,0,0,1,L'XFGTGRP,XFGFLTSL)                         
         DC    AL1(6)                                                           
         DC    AL2(6,XFGFLTS-GLVALS)                                            
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(PCUKEYWD-WORKD,PCUKEYWD-WORKD)                               
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,RECGRP,0,0,1,L'PCUYES,L'KEYFILT)                         
         DC    AL1(7)                                                           
         DC    AL2(7,KEYFILT-GLVALS)                                            
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(PCUREQST-WORKD,PCUREQST-WORKD)                               
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,RECGRP,0,0,1,L'PCUNORUN,L'REQFILT)                       
         DC    AL1(8)                                                           
         DC    AL2(8,REQFILT-GLVALS)                                            
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(PCUCALDR-WORKD,PCUCALDR-WORKD)                               
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'PCUYES,L'CALFILT)                              
         DC    AL1(9)                                                           
         DC    AL2(9,CALFILT-GLVALS)                                            
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(PCUSENAM-WORKD,PCUSENAM-WORKD)                               
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,RECGRP,0,0,1,L'SENAME,L'SEFILT)                          
         DC    AL1(10)                                                          
         DC    AL2(10,SEFILT-GLVALS)                                            
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
*&&UK                                                                           
         DC    AL2(PCUPRVT-WORKD,PCUPRVT-WORKD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'PCUONLY,L'PRVTFILT)                            
         DC    AL1(11)                                                          
         DC    AL2(11,PRVTFILT-GLVALS)                                          
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
*&&                                                                             
         DC    AL2(PCUSOFTD-WORKD,PCUSOFTD-WORKD)                               
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,RECGRP,0,0,1,L'PCUNOSET,L'SOFTFILT)                      
         DC    AL1(12)                                                          
         DC    AL2(12,SOFTFILT-GLVALS)                                          
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
                                                                                
GLOTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
KEYTGRP  DS    0X                  ** GROUP KEY DRIVER TABLE **                 
                                                                                
         DC    AL1(GRPKSYS-GRPKEY,L'GRPKSYS-1)                                  
         DC    AL1(GRPKSYSQ,0)                                                  
         DC    AL1(0)                                                           
         DC    AL1(KEYTILIT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKSTYP-GRPKEY,L'GRPKSTYP-1)                                
         DC    AL1(GRPKSTYQ,0)                                                  
         DC    AL1(0)                                                           
         DC    AL1(KEYTILIT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKSYST-GRPKEY,L'GRPKSYST-1)                                
         DC    AL2(SYSFLTS-TWAD)                                                
         DC    AL1(SYSFMAXN)                                                    
         DC    AL1(KEYTITAB+KEYTIATB)                                           
         DC    AL1(KEYTLIST)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKSYST-GRPKEY,L'GRPKSYST-1)                                
         DC    AL2(SYSFLTS-TWAD)                                                
         DC    AL1(SYSFMAXN)                                                    
         DC    AL1(KEYTIRNG+KEYTIATB)                                           
         DC    AL1(KEYTRNGE)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKAGY-GRPKEY,L'GRPKAGY-1)                                  
         DC    AL2(AGYFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKUSER-GRPKEY,L'GRPKUSER-1)                                
         DC    AL2(UIDFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG+KEYTIATB)                                           
         DC    AL1(KEYTRNGE)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKUSER-GRPKEY,L'GRPKUSER-1)                                
         DC    AL2(UIDFLTS-TWAD)                                                
         DC    AL1(UIDFMAXN)                                                    
         DC    AL1(KEYTITAB+KEYTIATB)                                           
         DC    AL1(KEYTLIST)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKGRP-GRPKEY,L'GRPKGRP-1)                                  
         DC    AL2(GRPFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
KEYTGRPX DC    AL1(KEYTEOTQ)                                                    
         EJECT                                                                  
KEYTXFG  DS    0X                  ** XFILE KEY DRIVER TABLE **                 
                                                                                
         DC    AL1(XFKSYS-XFILED,L'XFKSYS-1)                                    
         DC    AL1(XFKSYSQ,0)                                                   
         DC    AL1(0)                                                           
         DC    AL1(KEYTILIT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XFKSTYP-XFILED,L'XFKSTYP-1)                                  
         DC    AL1(XFKSTYPQ,0)                                                  
         DC    AL1(0)                                                           
         DC    AL1(KEYTILIT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XFAGY-XFKEY,L'XFAGY-1)                                       
         DC    AL2(AGYFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XFUSER-XFKEY,L'XFUSER-1)                                     
         DC    AL2(UIDFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG+KEYTIATB)                                           
         DC    AL1(KEYTRNGE)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XFUSER-XFKEY,L'XFUSER-1)                                     
         DC    AL2(UIDFLTS-TWAD)                                                
         DC    AL1(UIDFMAXN)                                                    
         DC    AL1(KEYTITAB+KEYTIATB)                                           
         DC    AL1(KEYTLIST)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XFGRP-XFKEY,L'XFGRP-1)                                       
         DC    AL2(GRPFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
KEYTXFGX DC    AL1(KEYTEOTQ)                                                    
         EJECT                                                                  
***********************************************************************         
* VALIDATE GROUP OPTIONS                                              *         
***********************************************************************         
                                                                                
         DROP  R7,R8,RB                                                         
GLOVAL   NMOD1 250,**GLOV**,CLEAR=YES                                           
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     GLOVDIS             DISPLAY=EXPRESSION                           
         B     GLOVUID             USER-ID=USER-ID                              
         B     GLOVSYS             SYSTEM=ABC...                                
         B     GLOVAGY             AGENCY=XX                                    
         B     GLOVGID             GROUPID=XXXXXXXX                             
         B     GLOVXFG             XFILE=XXXXXXXX                               
         B     GLOVKEY             KEYWORD=YES/NO                               
         B     GLOVREQ             REQUEST=YES/NO/NORUN                         
         B     GLOVCAL             CALENDAR=YES/NO                              
         B     GLOVSEN             SENAME=SENAME                                
*&&UK*&& B     GLOVPVT             PRIVATE=YES/NO/ONLY                          
*&&US*&& DC    AL4(0)                                                           
         B     GLOVSFT             SOFTDATE=NOSET/YES                           
                                                                                
GLOVALX  XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISPLAY=ABC OR +ABC OR ABC+ OR ++ (ALL COLUMNS)            *         
***********************************************************************         
                                                                                
GLOVDIS  SR    RE,RE                                                            
         ICM   RE,1,FVXLEN         TEST MORE THAN ONE CHARACTER INPUT           
         BZ    GLOVDIS4                                                         
         CLI   FVIFLD,ACTEOSQ      TEST FOR LEADING + SIGN (SUFFIX)             
         BNE   GLOVDIS3                                                         
         CLI   FVILEN,2            TEST FOR DIS=++                              
         BNE   GLOVDIS2                                                         
         CLI   FVIFLD+1,ACTEOSQ                                                 
         BNE   GLOVDIS2                                                         
                                                                                
         L     R1,OVADDR1                                                       
         USING DISTABD,R1                                                       
         LA    RF,ADDCOL                                                        
GLOVDIS1 CLI   DISTABD,EOT         TEST END OF TABLE                            
         BE    GLOVDISX                                                         
         BASR  RE,RF               CALL ADDCOL                                  
         AHI   R1,DISTABL                                                       
         B     GLOVDIS1                                                         
                                                                                
GLOVDIS2 EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),FVIFLD+1                                               
         STC   RE,FVILEN                                                        
                                                                                
         LA    R1,DEFCOL           OR DEFAULT DISPLAY COLUMNS                   
         LHI   R0,DEFCOLL                                                       
         GOTOR ADDCOL                                                           
         AHI   R1,L'DEFCOL                                                      
         BCT   R0,*-8                                                           
         B     GLOVDIS4                                                         
                                                                                
GLOVDIS3 LA    RF,FVIFLD(RE)       POINT TO END OF INPUT STRING                 
         CLI   0(RF),ACTEOSQ       TEST FOR TRAILING + SIGN (PREFIX)            
         BNE   GLOVDIS4                                                         
         MVI   0(RF),SPACE                                                      
         STC   RE,FVILEN                                                        
                                                                                
GLOVDIS4 SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
         LA    RF,ADDCOL                                                        
GLOVDIS5 BASR  RE,RF                                                            
         BE    GLOVDIS6                                                         
         MVC   FVXTRA(1),0(R1)                                                  
         MVC   FVMSGNO,=AL2(GE$ICOL)                                            
         B     GLOVALX                                                          
                                                                                
GLOVDIS6 AHI   R1,1                                                             
         BCT   R0,GLOVDIS5                                                      
                                                                                
         LA    R1,DEFCOL           OR DEFAULT DISPLAY COLUMNS                   
         LHI   R0,DEFCOLL                                                       
         GOTOR ADDCOL                                                           
         AHI   R1,L'DEFCOL                                                      
         BCT   R0,*-8                                                           
                                                                                
GLOVDISX B     GLOVALX                                                          
         DROP  R1                                                               
                                                                                
DEFCOL   DS    0AL1                ** GROUP DEFAULT DISPLAY COLUMNS **          
                                                                                
         DC    AL1(BLDUSRCQ)                                                    
         DC    AL1(BLDGRPQ)                                                     
         DC    AL1(BLDDESCQ)                                                    
         DC    AL1(BLDFREQQ)                                                    
         DC    AL1(BLDEFDTQ)                                                    
         DC    AL1(BLDRUNSQ)                                                    
         DC    AL1(BLDNAMEQ)                                                    
         DC    AL1(BLDLSTRQ)                                                    
         DC    AL1(BLDNXTRQ)                                                    
         DC    AL1(BLDENDQ)                                                     
         DC    AL1(BLDOTYPQ)                                                    
         DC    AL1(BLDDSTCQ)                                                    
         DC    AL1(BLDXFILQ)                                                    
         DC    AL1(BLDFRQNQ)                                                    
         DC    AL1(BLDSTATQ)                                                    
*&&UK*&& DC    AL1(BLDPERSQ)                                                    
                                                                                
         DC    AL1(BLDDAQ)                                                      
         DC    AL1(BLDRECNQ)                                                    
         DC    AL1(BLDSYSTQ)                                                    
         DC    AL1(BLDAGYQ)                                                     
         DC    AL1(BLDUSERQ)                                                    
         DC    AL1(BLDDESTQ)                                                    
                                                                                
DEFCOLL  EQU   *-DEFCOL                                                         
         EJECT                                                                  
***********************************************************************         
* ADD A COLUMN TO LIST OF DISPLAY COLUMNS                             *         
*                                                                     *         
* NTRY - R1=A(DISPLAY COLUMN CHARACTER)                               *         
* EXIT - CC=EQUAL IF OK, NOT EQUAL ON ERROR                           *         
***********************************************************************         
                                                                                
ADDCOL   STM   RE,R1,12(RD)                                                     
         CLI   0(R1),0             IGNORE NULL COLUMNS                          
         BE    ADDCOLY                                                          
                                                                                
         L     RE,OVADDR1                                                       
         USING DISTABD,RE          RE=A(DISPLAY COLUMN TABLE)                   
ADDCOL02 CLI   DISTABD,EOT         TEST EOT                                     
         BE    ADDCOLN                                                          
                                                                                
         TM    DISTINDS,DISTIGRP   GROUP ONLY DISPLAY COLUMN                    
         BZ    *+12                                                             
         CLI   CSREC,RECGRP                                                     
         BNE   ADDCOL03                                                         
                                                                                
         TM    DISTINDS,DISTIXFG   XFILE ONLY DISPLAY COLUMN                    
         BZ    *+12                                                             
         CLI   CSREC,RECXFG                                                     
         BNE   ADDCOL03                                                         
                                                                                
         TM    DISTINDS,DISTIDDS   TEST DDS ONLY COLUMN                         
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS USER                          
         BZ    ADDCOL03                                                         
                                                                                
         CLC   DISCHR,0(R1)        MATCH CHARACTER TO TABLE                     
         BE    *+12                                                             
ADDCOL03 AHI   RE,DISTABL          BUMP TO NEXT TABLE ENTRY                     
         B     ADDCOL02                                                         
                                                                                
         LA    R1,PCWORK                                                        
         LHI   R0,L'GLODIS                                                      
ADDCOL04 CLI   0(R1),0             TEST THIS IS A NEW COLUMN                    
         BE    ADDCOL06                                                         
         CLC   DISCOL,0(R1)        IGNORE DUPLICATE COLUMNS                     
         BE    ADDCOLY                                                          
         AHI   R1,L'DISCOL                                                      
         BCT   R0,ADDCOL04                                                      
         B     ADDCOLN                                                          
                                                                                
ADDCOL06 MVC   0(L'DISCOL,R1),DISCOL  SET INTERNAL VALUE                        
                                                                                
ADDCOLY  CLI   *+1,0                                                            
         B     ADDCOLX                                                          
ADDCOLN  CLI   *+0,0                                                            
                                                                                
ADDCOLX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE USERID=XXXXXXXXXX OR ALL (IF PRINCIPAL OR DDS)             *         
***********************************************************************         
                                                                                
GLOVUID  TM    CUSTAT,CUSDDS       TEST DDS TERMINAL                            
         BNZ   *+12                                                             
         TM    TWAINDS1,TWAIUPID   TEST USER CONNECTED TO PRINCIPAL ID          
         BZ    GLOVUID3                                                         
         SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BNE   GLOVUID2                                                         
         CLC   FVIFLD(0),PCUALL                                                 
         MVI   PCWORK+(UIDFLTY-UIDFLTS),KEYTRNGE                                
         MVC   PCWORK+(UIDFEND-UIDFLTS)(L'UIDFEND),PCEFFS                       
         B     GLOVUIDX                                                         
                                                                                
GLOVUID2 GOTOR AVALUID,FVIFLD      VALIDATE USER-ID                             
         BNE   GLOVUID3                                                         
UID      USING GIDTABD,PCWORK                                                   
         TM    CUSTAT,CUSDDS       TEST DDS TERMINAL                            
         BNZ   *+14                                                             
         CLC   UID.GIDALPH,CUAALF  NO - TEST SAME AGENCY ALPHA                  
         BNE   GLOVUID3                                                         
         ICM   R0,15,UID.GIDALPH                                                
         MVI   PCWORK+(UIDFLTY-UIDFLTS),KEYTRNGE                                
         STCM  R0,B'0011',PCWORK+(UIDFSTR-UIDFLTS)                              
         STCM  R0,B'0011',PCWORK+(UIDFEND-UIDFLTS)                              
                                                                                
         TM    CUSTAT,CUSDDS       TEST DDS TERMINAL                            
         BZ    GLOVUIDX                                                         
         L     RE,AOVEROUT         SET AGENCY VALUES TOO                        
         STCM  R0,B'1100',AGYFSTR-GLVALS(RE)                                    
         STCM  R0,B'1100',AGYFEND-GLVALS(RE)                                    
         B     GLOVUIDX                                                         
                                                                                
GLOVUID3 MVC   FVMSGNO,=AL2(GE$IID)                                             
                                                                                
GLOVUIDX B     GLOVALX                                                          
         DROP  UID                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE SYSTEM=ABC...                                              *         
***********************************************************************         
                                                                                
GLOVSYS  SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R3,FVIFLD                                                        
         SR    R4,R4                                                            
         XC    PCWORK(SYSFLTSL),PCWORK                                          
         MVI   PCWORK,KEYTLIST                                                  
                                                                                
         CLC   PCUALL,FVIFLD       TEST 'SYSTEM=ALL' SPECIFIED                  
         BNE   GLOVSYS0                                                         
         MVI   PCWORK,KEYTRNGE     YES - SET FILTER RANGE                       
         MVI   PCWORK+1,00                                                      
         MVI   PCWORK+2,FF                                                      
         B     GLOVSYSX                                                         
                                                                                
GLOVSYS0 L     R1,ASYSLST          LOOK UP SYSTEM IN SYSTEM LIST                
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6+SYSLLEN                                                     
         USING SYSLSTD,R1                                                       
                                                                                
GLOVSYS1 DS    0H                                                               
*&&US                                                                           
         CLI   0(R3),RFPFSSTL      TEST SPOT TRAFFIC                            
         BNE   *+12                                                             
         CLI   SYSLNUM,STRNUMQ     YES - MATCH SYSTEM NUMBER                    
         BE    GLOVSYS2                                                         
*&&                                                                             
         CLC   SYSLNAME(1),0(R3)   MATCH SYSTEM TO INPUT                        
         BE    GLOVSYS2                                                         
         BXLE  R1,RE,GLOVSYS1                                                   
         MVC   FVXTRA(L'SYSLUSLT),0(R3)                                         
         MVC   FVMSGNO,=AL2(GE$ISYST)                                           
         B     GLOVSYSX                                                         
                                                                                
GLOVSYS2 LA    RE,PCWORK+1         CHECK FOR DUPLICATES                         
         LHI   RF,SYSFMAXN                                                      
GLOVSYS3 CLI   0(RE),0                                                          
         BE    GLOVSYS4                                                         
         CLC   0(L'SYSLUSLT,RE),0(R3)                                           
         BE    GLOVSYS5                                                         
         AHI   RE,L'SYSLUSLT                                                    
         BCT   RF,GLOVSYS3                                                      
         DC    H'0'                                                             
                                                                                
GLOVSYS4 MVC   0(L'SYSLUSLT,RE),0(R3)                                           
         AHI   R4,1                BUMP N'ENTRIES IN TABLE                      
                                                                                
GLOVSYS5 AHI   R3,L'SYSLUSLT       BUMP TO NEXT INPUT SYSTEM                    
         BCT   R0,GLOVSYS0         DO FOR LENGTH OF INPUT                       
                                                                                
         LA    R1,PCWORK+1         SORT INTO ASCENDING SEQUENCE                 
         AHI   R4,-1                                                            
         BZ    GLOVSYSX                                                         
GLOVSYS6 LA    RF,1(R1)                                                         
         LR    RE,R4                                                            
GLOVSYS7 CLC   0(1,R1),0(RF)                                                    
         BNH   *+22                                                             
         XC    0(1,R1),0(RF)                                                    
         XC    0(1,RF),0(R1)                                                    
         XC    0(1,R1),0(RF)                                                    
         AHI   RF,1                                                             
         BCT   RE,GLOVSYS7                                                      
         AHI   R1,1                                                             
         BCT   R4,GLOVSYS6                                                      
                                                                                
GLOVSYSX B     GLOVALX                                                          
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGENCY=ALL OR XX                                           *         
***********************************************************************         
                                                                                
GLOVAGY  CLC   PCUALL,FVIFLD       TEST AGENCY=ALL                              
         BNE   GLOVAGY1                                                         
         XC    PCWORK(L'AGYFSTR),PCWORK                                         
         MVC   PCWORK+L'AGYFSTR(L'AGYFEND),PCEFFS                               
         B     GLOVAGY2                                                         
                                                                                
GLOVAGY1 MVC   PCWORK(L'AGYFSTR),FVIFLD                                         
         MVC   PCWORK+L'AGYFSTR(L'AGYFEND),FVIFLD                               
                                                                                
GLOVAGY2 L     RE,AOVEROUT         SET AGENCY VALUES TOO                        
         MVI   UIDFLTY-GLVALS(RE),KEYTRNGE                                      
         XC    UIDFSTR-GLVALS(,RE),UIDFSTR-GLVALS(RE)                           
         MVC   UIDFEND-GLVALS(,RE),PCEFFS                                       
                                                                                
GLOVAGYX B     GLOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE GROUPID=XXXXXXXX                                           *         
***********************************************************************         
                                                                                
         USING VGWORKD,RC                                                       
GLOVGID  MVI   VGGIDMAX,UIDFMAXN                                                
         GOTOR AVALGID,VGPARM,CUAALF,FVIFLD,VGGIDMAX                            
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IGID)                                            
         B     GLOVALX                                                          
                                                                                
         MVI   PCWORK,KEYTLIST     BUILD LIST OF USER-IDS                       
         LA    RF,PCWORK+1                                                      
         LA    RE,VGGIDTAB                                                      
         USING GIDTABD,RE                                                       
         SR    R0,R0                                                            
         ICM   R0,1,VGGIDCUR                                                    
GLOVGID2 MVC   0(L'GIDNUM,RF),GIDNUM                                            
         AHI   RE,GIDTABL                                                       
         AHI   RF,L'GIDNUM                                                      
         BCT   R0,GLOVGID2                                                      
                                                                                
GLOVGIDX B     GLOVALX                                                          
         DROP  RC                                                               
                                                                                
VGWORKD  DSECT                     ** GLOVGID S/R LOCAL W/S **                  
VGPARM   DS    6F                  PARAMETER LIST                               
VGGIDMAX DS    X                   MAXIMUM N'ENTRIES IN VGGIDTAB                
VGGIDCUR DS    X                   ACTUAL N'ENTRIES IN VGGIDTAB                 
VGGIDTAB DS    (UIDFMAXN)XL(GIDTABL)                                            
RLP01    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE XFILE=YES/NO/GROUP CODE                                    *         
***********************************************************************         
                                                                                
GLOVXFG  SR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         MVI   PCWORK,XFGFTYES                                                  
         EX    RE,*+8                                                           
         BE    GLOVXFGX                                                         
         CLC   FVIFLD(0),PCUYES                                                 
         MVI   PCWORK,XFGFTNO                                                   
         EX    RE,*+8                                                           
         BE    GLOVXFGX                                                         
         CLC   FVIFLD(0),PCUNO                                                  
         MVI   PCWORK,XFGFTFLT                                                  
         MVC   PCWORK+L'XFGFTYP(L'XFGFTGRP),FVIFLD                              
                                                                                
GLOVXFGX B     GLOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEYWORD=YES/NO                                             *         
***********************************************************************         
                                                                                
GLOVKEY  SR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         MVI   PCWORK,KEYFYES                                                   
         EX    RE,*+8                                                           
         BE    GLOVKEYX                                                         
         CLC   FVIFLD(0),PCUYES                                                 
         MVI   PCWORK,KEYFNO                                                    
         EX    RE,*+8                                                           
         BE    GLOVKEYX                                                         
         CLC   FVIFLD(0),PCUNO                                                  
         MVC   FVMSGNO,=AL2(GE$INV)                                             
                                                                                
GLOVKEYX B     GLOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE REQUEST=YES/NO                                             *         
***********************************************************************         
                                                                                
GLOVREQ  SR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         MVI   PCWORK,REQFYES                                                   
         EX    RE,*+8                                                           
         BE    GLOVREQX                                                         
         CLC   FVIFLD(0),PCUYES                                                 
         MVI   PCWORK,REQFNO                                                    
         EX    RE,*+8                                                           
         BE    GLOVREQX                                                         
         CLC   FVIFLD(0),PCUNO                                                  
         MVI   PCWORK,REQFNRUN                                                  
         EX    RE,*+8                                                           
         BE    GLOVREQX                                                         
         CLC   FVIFLD(0),PCUNORUN                                               
         MVC   FVMSGNO,=AL2(GE$INV)                                             
                                                                                
GLOVREQX B     GLOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE CALENDAR=YES/NO                                            *         
***********************************************************************         
                                                                                
GLOVCAL  SR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         MVI   PCWORK,CALFYES                                                   
         EX    RE,*+8                                                           
         BE    GLOVCALX                                                         
         CLC   FVIFLD(0),PCUYES                                                 
         MVI   PCWORK,CALFNO                                                    
         EX    RE,*+8                                                           
         BE    GLOVCALX                                                         
         CLC   FVIFLD(0),PCUNO                                                  
                                                                                
         MVC   FVMSGNO,=AL2(GE$INV)                                             
                                                                                
GLOVCALX B     GLOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE SENAME=SENAME                                              *         
***********************************************************************         
                                                                                
GLOVSEN  L     R1,ASYSFAC          R1=A(SYSFACS)                                
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6                                                             
         USING SELISTD,R1                                                       
         CLC   SENAME,FVIFLD       MATCH FULL SENAME TO INPUT                   
         BE    GLOVSEN2                                                         
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     GLOVSENX                                                         
*                                  SET SE, SYSTEM & USER-ID FILTERS             
GLOVSEN2 MVC   PCWORK(L'SESYS),SESYS                                            
         L     RE,AOVEROUT         SET AGENCY VALUES TOO                        
         XC    SYSFLTS-GLVALS(SYSFLTSL,RE),SYSFLTS-GLVALS(RE)                   
         MVI   SYSFLTY-GLVALS(RE),KEYTLIST                                      
         MVC   SYSFSYST-GLVALS(,RE),FVIFLD                                      
         MVI   UIDFLTY-GLVALS(RE),KEYTRNGE                                      
         XC    UIDFSTR-GLVALS(,RE),UIDFSTR-GLVALS(RE)                           
         MVC   UIDFEND-GLVALS(,RE),PCEFFS                                       
         XC    AGYFSTR-GLVALS(,RE),AGYFSTR-GLVALS(RE)                           
         MVC   AGYFEND-GLVALS(,RE),PCEFFS                                       
                                                                                
GLOVSENX B     GLOVALX                                                          
         DROP  R1                                                               
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* VALIDATE PRIVATE=NO/ONLY                                            *         
***********************************************************************         
                                                                                
GLOVPVT  SR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         MVI   PCWORK,PRVTFXGR     IF NO - SPUPPRESS PRIVATE                    
         EX    RE,*+8                                                           
         BE    GLOVPVTX                                                         
         CLC   FVIFLD(0),PCUNO                                                  
         MVI   PCWORK,PRVTFXPR     IF ONLY - SUPPRESS GLOBAL                    
         EX    RE,*+8                                                           
         BE    GLOVPVTX                                                         
         CLC   FVIFLD(0),PCUONLY                                                
         MVC   FVMSGNO,=AL2(GE$INV)                                             
                                                                                
GLOVPVTX B     GLOVALX                                                          
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SOFTDATE=NOSET/YES                                         *         
***********************************************************************         
                                                                                
GLOVSFT  SR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         MVI   PCWORK,SOFTFNOS                                                  
         EX    RE,*+8                                                           
         BE    GLOVSFTX                                                         
         CLC   FVIFLD(0),PCUNOSET                                               
         MVI   PCWORK,SOFTFYES                                                  
         EX    RE,*+8                                                           
         BE    GLOVSFTX                                                         
         CLC   FVIFLD(0),PCUYES                                                 
         MVI   PCWORK,SOFTFNON                                                  
         EX    RE,*+8                                                           
         BE    GLOVSFTX                                                         
         CLC   FVIFLD(0),PCUNONE                                                
         MVC   FVMSGNO,=AL2(GE$INV)                                             
                                                                                
GLOVSFTX B     GLOVALX                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
                                                                                
* GERLPWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE GERLPWORK                                                      
         PRINT ON                                                               
                                                                                
TWAD     DSECT                     ** TWAD DEFINITIONS **                       
                                                                                
         ORG   RLPOLY1H                                                         
       ++INCLUDE GERLPFED                                                       
         ORG   RLPOLY1H                                                         
       ++INCLUDE GERLPFDD                                                       
                                                                                
         ORG   OSVALS                                                           
                                                                                
LGRP     DS    CL(L'GRPKGRP)       SAVED GROUP CODE                             
                                                                                
GLVALS   DS    0X                  ** GROUP/LIST VALUES **                      
                                                                                
GLOKOPT  DS    0X                  ** KEY VALUES **                             
                                                                                
SYSFLTS  DS    0X                  ** SYSTEM FILTERS **                         
SYSFLTY  DS    X                   FILTER TYPE                                  
SYSFSYST DS    (SYSFMAXN)XL(L'GRPKSYS)                                          
SYSFMAXN EQU   8                                                                
SYSFLTSL EQU   *-SYSFLTS                                                        
                                                                                
AGYFLTS  DS    0X                  ** AGENCY ALPHA FILTERS **                   
AGYFSTR  DS    CL(L'GRPKAGY)       AGENCY ALPHA START                           
AGYFEND  DS    CL(L'GRPKAGY)       AGENCY ALPHA END                             
AGYFLTSL EQU   *-AGYFLTS                                                        
                                                                                
UIDFLTS  DS    0X                  ** USER-ID FILTERS **                        
UIDFMAXN EQU   60                  MAXIMUM N'USER IDS IN A LIST                 
UIDFLTY  DS    X                   FILTER TYPE                                  
UIDFSTR  DS    XL(L'GRPKUSER)      USER-ID START                                
UIDFEND  DS    XL(L'GRPKUSER)      USER ID END                                  
UIDRNGEL EQU   *-UIDFLTS                                                        
         ORG   UIDFLTS                                                          
         DS    (UIDFMAXN)XL(L'GRPKUSER)                                         
UIDFLTSL EQU   *-UIDFLTS                                                        
                                                                                
GRPFLTS  DS    0C                  ** GROUP CODE FILTERS **                     
GRPFSTR  DS    CL(L'GRPKGRP)       GROUP CODE START VALUE                       
GRPFEND  DS    CL(L'GRPKGRP)       GROUP CODE END VALUE                         
GRPFLTSL EQU   *-GRPFLTS                                                        
*&&UK                                                                           
PRVTFILT DS    X                   ** PRIVATE STATUS FILTER **                  
PRVTFXGR EQU   X'80'               DON'T WANT GLOBAL GROUPS                     
PRVTFXPR EQU   X'40'               DON'T WANT PRIVATE GROUPS                    
*&&                                                                             
FREQFLT  DS    0X                  ** FREQUENCY FILTER **                       
FREQFCC  DS    X                   CONDITION CODE                               
FREQFVAL DS    CL(L'GRPHFREQ)      FREQUENCY CODE                               
FREQFLTL EQU   *-FREQFLT                                                        
                                                                                
OTYPFLT  DS    0X                  ** OUTPUT TYPE FILTER **                     
OTYPFLEN DS    X                   L'INPUT FILTER                               
OTYPFVAL DS    CL(L'GRPHOTYP)      FILTER VALUE                                 
OTYPFLTL EQU   *-OTYPFLT                                                        
                                                                                
DSTCFLT  DS    0X                  ** DESTINATION CODE FILTER **                
DSTCFLEN DS    X                   L'INPUT FILTER                               
DSTCFVAL DS    CL(L'GIDCODE)       FILTER VALUE                                 
DSTCFLTL EQU   *-DSTCFLT                                                        
                                                                                
NXTRFLT  DS    0X                  ** NEXT RUN DATE FILTER **                   
NXTRSTDT DS    XL(L'GRPHNXTR)      NEXT RUN DATE START                          
NXTRENDT DS    XL(L'GRPHNXTR)      NEXT RUN DATE END                            
NXTRFLTL EQU   *-NXTRFLT                                                        
                                                                                
LSTRFLT  DS    0X                  ** LAST RUN DATE FILTER **                   
LSTRSTDT DS    XL(L'GRPHLSTR)      LAST RUN DATE START                          
LSTRENDT DS    XL(L'GRPHLSTR)      LAST RUN DATE END                            
LSTRFLTL EQU   *-LSTRFLT                                                        
                                                                                
ENDFLT   DS    0X                  ** END RUN DATE FILTER **                    
ENDSTDT  DS    XL(L'GRPHEND)       END RUN DATE START                           
ENDENDT  DS    XL(L'GRPHEND)       END RUN DATE END                             
ENDFLTL  EQU   *-ENDFLT                                                         
                                                                                
NAMEFLT  DS    0X                  ** FILTER NAME CODE FILTER **                
NAMEFLEN DS    X                   L'INPUT FILTER                               
NAMEFVAL DS    CL(L'GRPLNAME)      FILTER VALUE                                 
NAMEFLTL EQU   *-NAMEFLT                                                        
                                                                                
XFGFLTS  DS    0X                  ** XFILE GROUP FILTERS **                    
XFGFTYP  DS    X                   FILTER TYPE                                  
XFGFTNUL EQU   0                   NO FILTER                                    
XFGFTYES EQU   1                   XFILE GROUPS ONLY                            
XFGFTNO  EQU   2                   EXCLUDE XFILE GROUPS                         
XFGFTFLT EQU   3                   FILTER ON NAME                               
XFGFTGRP DS    CL(L'GRPLXFIL)      XFILE GROUP CODE                             
XFGFLTSL EQU   *-XFGFLTS                                                        
                                                                                
KEYFILT  DS    X                   ** KEYWORD FILTER **                         
KEYFNUL  EQU   0                   NO FILTER                                    
KEYFYES  EQU   1                   GROUPS WITH KEYWORDS ONLY                    
KEYFNO   EQU   2                   GROUPS WITHOUT KEYWORDS ONLY                 
                                                                                
REQFILT  DS    X                   ** REQUEST FILTER **                         
REQFNUL  EQU   0                   NO FILTER                                    
REQFYES  EQU   1                   GROUPS WITH REQUESTS ONLY                    
REQFNO   EQU   2                   GROUPS WITHOUT REQUESTS ONLY                 
REQFNRUN EQU   3                   GROUPS WITH NO RUN REQUESTS ONLY             
                                                                                
CALFILT  DS    X                   ** CALENDAR FILTER **                        
CALFNUL  EQU   0                   NO FILTER                                    
CALFYES  EQU   X'80'               GROUPS WITH CALENDARS ONLY                   
CALFNO   EQU   X'40'               GROUPS WITHOUT CALENDARS ONLY                
                                                                                
SEFILT   DS    X                   ** SE NUMBER FILTER **                       
                                                                                
SOFTFILT DS    X                   ** SOFTDATE FILTER **                        
SOFTFNUL EQU   0                   NO FILTER                                    
SOFTFNOS EQU   X'80'               GROUPS WITH UNRESOLVED SOFT DATE             
SOFTFYES EQU   X'40'               GROUPS WITH ANY SOFT DATES                   
SOFTFNON EQU   X'20'               GROUPS WITH NO RESOLVED DATES                
                                                                                
GLOKOPTL EQU   *-GLOKOPT                                                        
                                                                                
GLODOPT  DS    0X                  ** DISPLAY OPTIONS **                        
                                                                                
GLODIS   DS    XL32                DISPLAY COLUMNS                              
GLODISL  EQU   *-GLODIS                                                         
                                                                                
GLOUSER  DS    XL(L'GRPLUSER)      USER-ID                                      
                                                                                
GLODOPTL EQU   *-GLODOPT                                                        
GLVALSL  EQU   *-GLVALS                                                         
                                                                                
GLODVALS DS    0XL2                SAVED DISPLAY VALUES (FOR SCROLLING)         
GLODINDX DS    X                   INDEX TO CURRENT DISPLAY COLUMN              
GLODINUM DS    X                   NUMBER OF COLUMNS DISPLYED                   
                                                                                
GLTBANXT DS    XL(GRPKLENQ)        NEXT TIME GROUP KEY                          
                                                                                
BLDHEAD1 DS    CL(L'GLILIN1)       DISPLAY COLUMN HEADING                       
BLDHEAD2 DS    CL(L'GLILIN1)       DISPLAY COLUMN HEADING                       
                                                                                
BLDSDSP  DS    0X                  DISPLACEMENTS FOR SCREEN DISPLAY             
                                                                                
***********************************************************************         
* COLUMNS 1 TO 9 RESERVED FOR DDS USE                                 *         
***********************************************************************         
                                                                                
BLDDAQ   EQU   C'1'                                                             
BLDDAW   EQU   8                   TTTTBBRR                                     
BLDDAD   DS    X                   DISPLACEMENT TO DISK ADDRESS                 
                                                                                
BLDRECNQ EQU   C'2'                                                             
BLDRECNW EQU   4                   NNNN                                         
BLDRECND DS    X                   DISPLACEMENT TO RECORD NUMBER                
                                                                                
BLDUSERQ EQU   C'3'                                                             
BLDUSERW EQU   5                   NNNNN                                        
BLDUSERD DS    X                   DISPLACEMENT TO USER-ID#                     
                                                                                
BLDDESTQ EQU   C'4'                                                             
BLDDESTW EQU   5                   NNNNN                                        
BLDDESTD DS    X                   DISPLACEMENT TO DESTINATION ID#              
                                                                                
***********************************************************************         
* COLUMNS A TO Z AND SPECIAL CHARACTERS FOR USERS                     *         
***********************************************************************         
                                                                                
BLDUSRCQ EQU   C'A'                                                             
BLDUSRCW EQU   L'GIDCODE           CCCCCCCCCC                                   
BLDUSRCD DS    X                   DISPLACEMENT TO USER-ID CODE                 
                                                                                
BLDGRPQ  EQU   C'B'                                                             
BLDGRPW  EQU   L'GRPLGRP           CCCCCCCC                                     
BLDGRPD  DS    X                   DISPLACEMENT TO GROUP CODE                   
                                                                                
BLDDESCQ EQU   C'C'                                                             
BLDDESCW EQU   L'GRPLDESC          DDDDDDDDDDDDDDDDDDDDDDDD                     
BLDDESCD DS    X                   DISPLACEMENT TO DESCRIPTION                  
                                                                                
BLDFREQQ EQU   C'D'                                                             
BLDFREQW EQU   L'GRPLFREQ          F                                            
BLDFREQD DS    X                   DISPLACEMENT TO FREQUENCY CODE               
                                                                                
BLDNAMEQ EQU   C'E'                                                             
BLDNAMEW EQU   L'GRPLNAME          NNNNNNNN                                     
BLDNAMED DS    X                   DISPLACEMENT TO GROUP NAME                   
                                                                                
BLDLSTRQ EQU   C'F'                                                             
BLDLSTRW EQU   8                   MMMDD/YY OR DDMMMYY OR DD.MM.YY              
BLDLSTRD DS    X                   DISPLACEMENT TO LAST RUN DATE                
                                                                                
BLDNXTRQ EQU   C'G'                                                             
BLDNXTRW EQU   8                   MMMDD/YY OR DDMMMYY OR DD.MM.YY              
BLDNXTRD DS    X                   DISPLACEMENT TO NEXT RUN DATE                
                                                                                
BLDENDQ  EQU   C'H'                                                             
BLDENDW  EQU   8                   MMMDD/YY OR DDMMMYY OR DD.MM.YY              
BLDENDD  DS    X                   DISPLACEMENT TO END RUN DATE                 
                                                                                
BLDOTYPQ EQU   C'I'                                                             
BLDOTYPW EQU   L'GRPLOTYP          CCCCCC                                       
BLDOTYPD DS    X                   DISPLACEMENT TO OUTPUT TYPE                  
                                                                                
BLDDSTCQ EQU   C'J'                                                             
BLDDSTCW EQU   L'GIDCODE           CCCCCCCCCC                                   
BLDDSTCD DS    X                   DISPLACEMENT TO DESTINATION ID CODE          
                                                                                
BLDXFILQ EQU   C'K'                                                             
BLDXFILW EQU   L'GRPLXFIL          CCCCCCCC                                     
BLDXFILD DS    X                   DISPLACEMENT TO XFILE GROUP CODE             
                                                                                
BLDFRQNQ EQU   C'L'                                                             
BLDFRQNW EQU   FRQNAMLQ            FFFFFFFFFF                                   
BLDFRQND DS    X                   DISPLACEMENT TO FREQUENCY NAME               
                                                                                
BLDEFDTQ EQU   C'M'                                                             
BLDEFDTW EQU   L'PCMEFFDT          YMD(-YMD)                                    
BLDEFDTD DS    X                   DISPLACEMENT TO EFFECTIVE DATES              
                                                                                
BLDRUNSQ EQU   C'N'                                                             
BLDRUNSW EQU   20                  DDD(,DDD,..) OR NN(,NN,..)                   
BLDRUNSD DS    X                   DISPLACEMENT TO RUN SCHEDULE                 
                                                                                
BLDSYSTQ EQU   C'O'                                                             
BLDSYSTW EQU   L'ASSYSNAM          SSSSSSS                                      
BLDSYSTD DS    X                   DISPLACEMENT TO SYSTEM                       
                                                                                
BLDAGYQ  EQU   C'P'                                                             
BLDAGYW  EQU   L'GRPLAGY           AA                                           
BLDAGYD  DS    X                   DISPLACEMENT TO AGENCY                       
                                                                                
BLDSTATQ EQU   C'Q'                                                             
BLDSTATW EQU   21                  ST1(,ST2,..)                                 
BLDSTATD DS    X                   DISPLACEMENT TO STATUS                       
*&&UK                                                                           
BLDPERSQ EQU   C'R'                                                             
BLDPERSW EQU   22                  PERSON-ID                                    
BLDPERSD DS    X                   DISPLACEMENT TO PERSON                       
*&&                                                                             
BLDACTSQ EQU   C'?'                                                             
BLDACTSW EQU   32                  ACT(,ACT,..)                                 
BLDACTSD DS    X                   DISPLACEMENT TO VALID ACTIONS                
                                                                                
BLDSDSPL EQU   *-BLDSDSP                                                        
                                                                                
XFGTABD  DSECT                     ** SYSTEM/GROUP TABLE **                     
XFGTSYS  DS    XL(L'XFSGSYS)       SYSTEM NUMBER                                
XFGTGRP  DS    CL(L'XFSGGRP)       RFP GROUP CODE                               
XFGTSYSL DS    CL(L'XFSGSYSL)      SYSTEM LETTER                                
XFGTABL  EQU   *-XFGTABD           TABLE LENGTH                                 
XFGTABM  EQU   50                  MAXIMUM N'ENTRIES IN TABLE                   
                                                                                
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
                                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
                                                                                
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027GERLP01   01/26/21'                                      
         END                                                                    
