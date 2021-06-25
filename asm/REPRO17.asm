*          DATA SET REPRO17    AT LEVEL 028 AS OF 11/02/98                      
*&&      SET   NOP=N                                                            
*PHASE T80A17B                                                                  
T80A17   TITLE 'REPRO17 - REP PROPOSALS COSTS'                                  
PRO17    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO17*,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
*                                                                               
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
GSFRR    USING FRRELD,GSFRREL      CURRENT RECORD ELEMENT                       
GSFRA    USING FRAELD,GSFRAEL      CURRENT ACTION ELEMENT                       
GSFRP    USING FRPELD,GSFRPEL      CURRENT PFKEY ELEMENT                        
PSFRR    USING FRRELD,PSFRREL      PREVIOUS SESSION'S RECORD ELEMENT            
PSFRA    USING FRAELD,PSFRAEL      PREVIOUS SESSION'S ACTION ELEMENT            
*                                                                               
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         EJECT                                                                  
NUMSTAS  EQU   4                   NUMBER OF STATIONS                           
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)                        
*                          - EXPECTS R1 TO HOLD VERB                            
***********************************************************************         
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                            
*                                                                               
* P1 HOLDS EQUATED VERB                                                         
***********************************************************************         
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECRD)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFKEY)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(SUBACT)                              
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                                
***********************************************************************         
INIT     DS    0H                                                               
         OI    TWASRVH+1,X'01'     SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    TWASRVH+6,X'80'                                                  
         OI    GCINDS1,GCIPROT             UNPROT ON NTRSES                     
         OI    GSINDSL1,GSINOIO    WE'LL DO THE IO'S                            
         OI    GSINDSL1,GSIXKEY    NO ENTER KEY MESG                            
         OI    LSSTAT1,LSSMAIN     LIST PART OF MAINT. SCREEN                   
         OI    LSSTAT1,LSSBALL     BUILD ALL OF LIST IN ONE GO                  
         OI    LSSTAT1,LSSTSAR     TSAR ONLY LIST                               
*                                                                               
INITX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                                    
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 A(KEY OR WHERE TO BUILD THE KEY)                                           
* P4 HOLDS SUB-ACTION                                                           
***********************************************************************         
KEY      LM    R0,R2,SVPARMS                                                    
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE WE DO ANYTHING TO THE KEY FIELDS ON THE SCREEN                         
***********************************************************************         
KEYFRST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE VALIDATING THE KEY FIELDS                                              
***********************************************************************         
KFKVAL   DS    0H                                                               
* DON'T KNOW IF KEY OR VIEW WAS CHANGED YET                                     
         NI    MISCFLG1,X'FF'-MF1KYCHG-MF1VWCHG                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* AFTER WE'VE DONE EVERYTHING TO THE KEY FIELDS ON THE SCREEN                   
***********************************************************************         
KEYLAST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KLTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER VALIDATING THE KEY FIELDS                                               
***********************************************************************         
KLKVAL   DS    0H                                                               
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
         DROP  R2                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD OBJECT                                                                 
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 A(KEY OR WHERE TO BUILD THE KEY)                                           
* P4 HOLDS SUB-ACTION                                                           
***********************************************************************         
RECRD    LM    R0,R2,SVPARMS                                                    
         LA    RF,RECRDTBL                                                      
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
RECRDTBL DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER THE CONTROLLER CALLS THE I/O ACTION                                     
***********************************************************************         
RECLAST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RFTABL   DS    0H                                                               
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLRWRT)      WRITE                      
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER THE CALL TO DISPLAY THE RECORD                                          
***********************************************************************         
RLRWRT   DS    0H                                                               
         OC    CHGRATCS,CHGRATCS                                                
         BNZ   *+14                                                             
         OC    BASEPERC,BASEPERC                                                
         BZ    RLRWRTX                                                          
         NI    MISCFLG1,FF-MF1TMPBT                                             
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ    DETAIL CLUSTER                               
         BAS   RE,MINIOHI                                                       
         BNE   RLRW100                                                          
RLRW002  L     RE,MINELEM                                                       
         CLI   0(RE),RPRDTELQ                                                   
         BNE   RLRW100                                                          
*                                                                               
         BAS   RE,INTOAIO5         SO WE CAN USE RECUP                          
         NI    MISCFLG1,FF-MF1NWRT                                              
*                                                                               
         OC    CHGRATCS,CHGRATCS   RATE CHANGES?                                
         BZ    RLRW004             NO                                           
*                                                                               
         BAS   RE,FTCHRATE         FETCH RATES                                  
*                                                                               
RLRW004  XC    BOELEM,BOELEM                                                    
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         USING RPRDTELD,R6                                                      
         MVC   BOFULL1,RPRDTNC1                                                 
         OC    BOFULL1,BOFULL1                                                  
         BZ    RLRW020             NO COST SKIP LINE                            
         TM    BOFULL1,X'80'       N/A?                                         
         BNZ   RLRW020             YES SKIP LINE                                
         DROP  R6                                                               
*                                                                               
         OI    MISCFLG1,MF1NWRT                                                 
RLRW010  CLI   0(R6),0                                                          
         BE    RLRW014                                                          
         CLI   0(R6),RPRCSELQ                                                   
         BE    RLRW012                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RLRW010                                                          
*                                                                               
RLRW012  DS    0H                  COPY AND DELETE ELEMENT                      
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)                                                  
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
RLRW014  DS    0H                                                               
         MVI   BOELEM,RPRCSELQ                                                  
         MVI   BOELEM+1,RPRCSLNQ                                                
*                                                                               
         LA    RE,BASEPERC+1       NEVER DO COST 1                              
         LA    R0,L'BASEPERC-1                                                  
         LA    R4,BOELEM+(RPRCSNC2-RPRCSELD)                                    
*                                                                               
RLRW016  DS    0H                                                               
         CLI   0(RE),0             % OF BASE COST?                              
         BE    RLRW018             NO                                           
         TM    0(R4),X'80'         N/A?                                         
         BNZ   RLRW018             YES                                          
*                                                                               
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,=P'100'                                                 
         ZIC   R1,0(RE)                                                         
         CVD   R1,BODUB1                                                        
         MP    PCKOF16B,BODUB1                                                  
         DP    PCKOF16B,=PL8'100'                                               
         MVC   BODUB1,PCKOF16B                                                  
         SRP   BODUB1,64-2,5                                                    
         CP    BODUB1,=P'2000000000'    CHECK FOR 7FFFFFF                       
         BNH   RLRW017                                                          
*                                                                               
         ZAP   BODUB1,=P'0'                                                     
         OI    MISCFLG1,MF1TMPBT                                                
*                                                                               
RLRW017  DS    0H                                                               
         SRP   BODUB1,64-2,5                                                    
         CVB   R1,BODUB1                                                        
         MH    R1,=H'100'          MAKE IT DOLLARS                              
         ST    R1,0(R4)                                                         
*                                                                               
RLRW018  DS    0H                                                               
         LA    RE,1(RE)                                                         
         LA    R4,RPRCSNC3-RPRCSNC2(R4)                                         
         BCT   R0,RLRW016                                                       
*                                                                               
RLRW020  DS    0H                                                               
         TM    MISCFLG1,MF1NWRT                                                 
         BZ    RLRW050             DON'T NEED MINIOWRT                          
*                                                                               
         CLI   BOELEM,0            DON'T NEED RECUP                             
         BE    RLRW026                                                          
*                                                                               
         L     R6,AIO5             WHERE DOES IT GO?                            
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
*                                                                               
RLRW022  DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    RLRW024                                                          
         CLI   0(R6),RPRCSELQ                                                   
         BNL   RLRW024                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RLRW022                                                          
*                                                                               
RLRW024  DS    0H                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
RLRW026  DS    0H                                                               
         BAS   RE,FROMAIO5                                                      
*                                                                               
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MINEKEY,MINEKEY     SOMETHING SCREWY HERE                        
         MVI   MINEKEY,RPRDTELQ                                                 
         L     R6,MINELEM                                                       
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RLRW050  DS    0H                                                               
         BAS   RE,MINIOSEQ                                                      
         BE    RLRW002             MORE DETAILS                                 
*                                                                               
RLRW100  DS    0H                                                               
*                                                                               
RLRWRTX  DS    0H                                                               
         BAS   RE,MINIOCLS                                                      
*                                                                               
         TM    MISCFLG1,MF1TMPBT                                                
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(698)                                                
         B     EXITL                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(91)                                                 
         MVI   FVOMTYP,C'I'                                                     
         B     EXITL                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* FETCH RATE CARD FOR DETAIL LINE IN AIO5                                       
***********************************************************************         
FTCHRATE NTR1                                                                   
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         USING RPRDTELD,R6                                                      
*                                                                               
         CLC   RPRDTINM,BCSPACES   INV # FETCH?                                 
         BNH   FTRATX              NO                                           
*                                                                               
         L     R4,AIOREC           CLEAR THE BLOCK                              
         LH    R5,=Y(IOAREALN)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R4,RE                                                            
         L     R4,AIOREC                                                        
*                                                                               
         USING RFTBLKD,R4                                                       
         MVC   RFTACOM,ACOM                    A(COMFACS)                       
         MVC   RFTAIO1,AIO1                    A(2K IO AREA)                    
         MVC   RFTAIO2,AIO6                    A(2K IO AREA)                    
         MVC   RFTAWRK,AIO2                    A(6K WORK AREA)                  
*                                              USES AIO2,AIO3, & AIO4           
         LA    RE,RATEHOOK                                                      
         STCM  RE,15,RFTHOOKA                           HOOK ROUTINE            
         MVI   RFTCNTL,RFTCHDRQ+RFTCRTEQ                DATA FLAGS              
         MVC   RFTCREP,CUAALF                           REP CODE                
*                                                                               
         LA    RE,RFTCRTES                                                      
         LA    RF,CHGRATCS                                                      
         LA    R0,CHGRATCS+L'CHGRATCS                                           
FTRAT002 DS    0H                                                               
         OC    0(L'CHGRATC,RF),0(RF)           ANY RATE CARD?                   
         BZ    *+14                            NO                               
         MVC   0(RFTCRTSL,RE),0(RF)                                             
         LA    RE,RFTCRTSL(RE)                                                  
         LA    RF,RFTCRTSL(RF)                                                  
         CR    RF,R0                                                            
         BL    FTRAT002                                                         
         EJECT                                                                  
*                                                                               
*---------------------*                                                         
* STATION FOR CLUSTER *                                                         
*---------------------*                                                         
         ZIC   R1,RPRDTSTA                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
*                                                                               
         MVC   RFTCSTAT,0(R1)                 STATION CALL LETTERS              
         MVI   RFTCSRC,C'N'                   DEMO SOURCE                       
*                                                                               
         MVI   RFTAMODE,RFTAMSTQ              FETCH MODE                        
         MVC   RFTCINV,RPRDTINM               INVENTORY #                       
*                                                                               
         OC    RPRDTEFF,RPRDTEFF                                                
         BZ    FTRAT030                                                         
         GOTO1 VDATCON,BODMCB,(8,RPRDTEFF),(2,RFTCEFST)     EFF START &         
FTRAT030 OC    RPRDTEEF,RPRDTEEF                                                
         BZ    FTRAT032                                                         
         GOTO1 VDATCON,BODMCB,(8,RPRDTEEF),(2,RFTCEFEN)     END DATES           
FTRAT032 DS    0H                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------                       
*            FETCH CALL - FOR UPDATING EXISTING DETAIL CLUSTERS                 
*--------------------------------------------------------                       
         GOTO1 VFETCH,BODMCB,AIOREC                                             
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY     RESTORE READ SEQUENCE                        
         MVI   MINEKEY,RPRDTELQ                                                 
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FTRATX   DS    0H                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
***********************************************************************         
*  HOOK FOR UPDATING RATES BASED ON NEW RATE CARD                               
***********************************************************************         
RATEHOOK NTR1                                                                   
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         USING RPRDTELD,R6                                                      
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
*                                                                               
* CHECK FOR FIRST COST                                                          
*                                                                               
         OC    CHGRATC,CHGRATC     ANY RATE CARD?                               
         BZ    RATHK30             NO - NO COST THEN                            
         TM    RPRDTNC1,X'80'      N/A?                                         
         BNZ   RATHK30             YES                                          
*                                                                               
                                                                                
         LA    RE,RFTFRTES                                                      
         LA    R0,RFTFRTEN                                                      
RATHK20  DS    0H                                                               
         OC    0(RFTFRTSL,RE),0(RE)                                             
         BZ    RATHK30                                                          
         CLC   CHGRATC,0(RE)       MATCH?                                       
         BE    RATHK22             YES                                          
         LA    RE,RFTFRTSL(RE)                                                  
         BCT   R0,RATHK20                                                       
         B     RATHK30                                                          
*                                                                               
RATHK22  DS    0H                                                               
         OC    RFTFRRTE-RFTFRTES(L'RFTFRRTE,RE),RFTFRRTE-RFTFRTES(RE)           
         BZ    *+18                                                             
         MVC   RPRDTNC1,RFTFRRTE-RFTFRTES(RE)                                   
         NI    RPRDTNC1,X'7F'       CLEAR N/A FLAG ON LARGE COST                
         OI    MISCFLG1,MF1NWRT                                                 
         DROP  R6                                                               
*                                                                               
RATHK30  DS    0H                                                               
********                                                                        
*    SUPPLEMENTAL COSTS                                                         
********                                                                        
         OC    RFTFRTES(RFTFRTEN*RFTFRTSL),RFTFRTES                             
         BZ    RATHKX                                                           
*                                                                               
         LA    R2,CHGRATCS+L'CHGRATC        SUPPLEMENTAL RATE CARDS?            
         OC    0(3*L'CHGRATC,R2),0(R2)                                          
         BZ    RATHKX                       NO                                  
*                                                                               
         XC    BOELEM,BOELEM                                                    
RATHK32  CLI   0(R6),0                                                          
         BE    RATHK36                                                          
         CLI   0(R6),RPRCSELQ                                                   
         BE    RATHK34                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RATHK32                                                          
*                                                                               
RATHK34  DS    0H                  COPY AND DELETE ELEMENT                      
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)                                                  
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
RATHK36  DS    0H                                                               
         MVI   BOELEM,RPRCSELQ                                                  
         MVI   BOELEM+1,RPRCSLNQ                                                
*                                                                               
         LA    RE,BOELEM                                                        
         USING RPRCSELD,RE                                                      
         MVI   RPRCSEL,RPRCSELQ                                                 
         MVI   RPRCSLEN,RPRCSLNQ                                                
         LA    R3,RPRCSNC2                                                      
         DROP  RE                                                               
*                                                                               
RATHK38  DS    0H                                                               
         OC    0(L'CHGRATC,R2),0(R2)        ANY RATE CARD?                      
         BZ    RATHK60                      NO                                  
         TM    0(R3),X'80'                  N/A?                                
         BNZ   RATHK60                      YES                                 
*                                                                               
         LA    RE,RFTFRTES                                                      
         LA    R0,RFTFRTEN                                                      
RATHK50  DS    0H                                                               
         OC    0(RFTFRTSL,RE),0(RE)                                             
         BZ    RATHK60                                                          
         CLC   0(L'CHGRATC,R2),0(RE)        MATCH?                              
         BE    RATHK52                      YES                                 
         LA    RE,RFTFRTSL(RE)                                                  
         BCT   R0,RATHK50                                                       
         B     RATHK60                                                          
*                                                                               
RATHK52  DS    0H                                                               
         OC    RFTFRRTE-RFTFRTES(L'RFTFRRTE,RE),RFTFRRTE-RFTFRTES(RE)           
         BZ    *+18                                                             
         MVC   0(L'RPRCSNC2,R3),RFTFRRTE-RFTFRTES(RE)                           
         NI    0(R3),X'7F'           CLEAR N/A FLAG ON LARGE COST               
         OI    MISCFLG1,MF1NWRT                                                 
*                                                                               
RATHK60  DS    0H                                                               
         LA    R2,L'CHGRATC(R2)                                                 
         LA    R3,RPRCSNC3-RPRCSNC2(R3)                                         
         LA    R0,CHGRATCS+L'CHGRATCS                                           
         CR    R2,R0                                                            
         BL    RATHK38                                                          
*                                                                               
         L     R6,AIO5             WHERE DOES IT GO?                            
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
*                                                                               
RATHK70  DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    RATHK72                                                          
         CLI   0(R6),RPRCSELQ                                                   
         BNL   RATHK72                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RATHK70                                                          
*                                                                               
RATHK72  DS    0H                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
*                                                                               
RATHKX   DS    0H                                                               
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECTS FOR KEY DATA OR RECORD DATA                                      
*                                                                               
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                            
* P2 HOLDS EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION                   
* P3 BYTE  0   HOLDS EQUATED DATA VERB IF P2 IS ZERO                            
* P3 BYTES 1-3 HOLDS EQUATED ACTION VERB                                        
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                           
*                                                                               
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                                    
***********************************************************************         
DATA     ICM   R1,15,SVPARMS2      DOING ACTION ON SPECIFIC DATA OBJ?           
         BNZ   DATA10              YES                                          
***********************************************************************         
************** DOING A GLOBAL ACTION ON ENTIRE RECORD *****************         
***********************************************************************         
         L     R2,SVPARMS4         R2 = A(RECORD)                               
         SR    R1,R1                                                            
         IC    R1,SVPARMS3         R1 = GLOBAL ACTION                           
         LA    RF,DTATABL          TABLE OF GLOBAL VERBS                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE WE DO ANYTHING TO THE DATA FIELDS ON THE SCREEN                        
***********************************************************************         
DTAFRST  DS    0H                                                               
         L     R1,SVPARMS3         VERB                                         
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE DISPLAYING THE DATA FIELDS                                             
***********************************************************************         
DFDDIS   DS    0H                                                               
         MVC   SVRECDA,GSRECDA                                                  
         GOTO1 =A(D1STDDIS),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
***********************************************************************         
* AFTER  WE DO EVERYTHING TO THE DATA FIELDS ON THE SCREEN                      
***********************************************************************         
DTALAST  DS    0H                                                               
         L     R1,SVPARMS3         VERB                                         
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER VALIDATING THE DATA FIELDS                                              
***********************************************************************         
DLDVAL   DS    0H                                                               
         CLC   SVPARMS4,ATLST                                                   
         BNE   DLDVX               ONLY PROCESS IF TSAR LINE                    
*                                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
*                                                                               
         CLC   TLRRTCX,BCSPACES    RATE CARD?                                   
         BH    DLDV10              YES                                          
*                                                                               
         CLI   TLRSLN,0                                                         
         BNE   EXIXLEN             LENGTH ERROR                                 
*                                                                               
         CLI   TLRQTR,0                                                         
         BNE   EXIXQTR             QUARTER ERROR                                
*                                                                               
         CLI   TLRYR,0                                                          
         BNE   EXIXYR              YEAR ERRROR                                  
         B     DLDVX               OK FOR COST PERCENTAGE                       
*                                                                               
DLDV10   DS    0H                                                               
         CLI   TLRPC,FF            CHECK %?                                     
         BE    *+12                NO                                           
         CLI   TLRPC,0             PERCENT OF COST 1?                           
         BNE   EXIXRTCD            YES - RATE CARD ERROR                        
*                                                                               
         CLI   TLRSLN,0                                                         
         BE    EXITLEN             LENGTH ERROR                                 
*                                                                               
         CLI   TLRQTR,0                                                         
         BE    EXITQTR             QUARTER ERROR                                
*                                                                               
         CLI   TLRYR,0                                                          
         BE    EXITYR              YEAR ERRROR                                  
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RARTKEY,RE                                                       
         MVI   RARTKTYP,X'3E'      READ RATE RECORD                             
         MVC   RARTKREP,CUAALF                                                  
         MVC   RARTKCOD,TLRRTCX                                                 
         OC    RARTKCOD,BCSPACES                                                
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RARTKEY),IOKEYSAV                                        
         BNE   EXINRTCD            NO RATE CARD                                 
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGET)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXINRTCD            NO RATE CARD                                 
*                                                                               
         L     R4,AIO4                                                          
         LA    R4,RARTPEL-RARTREC(R4)                                           
         USING RALQELEM,R4                                                      
         MVC   BOBYTE2,TLRYR                                                    
         NI    BOBYTE2,X'7F'                                                    
         ZIC   R0,BOBYTE2                                                       
         CHI   R0,99                                                            
         BNH   *+8                                                              
         AHI   R0,-100                                                          
         MVC   IOKEY+2(4),=C'0101'                                              
         EDIT  (R0),(2,IOKEY),FILL=0,WRK=BOWORK1,DUB=BODUB1                     
         GOTO1 VDATCON,BODMCB,(0,IOKEY),(3,IOKEY+6)                             
         MVC   BOBYTE2,IOKEY+6                                                  
         NI    MISCFLG1,FF-(MF1RTYR+MF1RTLEN)                                   
*                                                                               
DLDV20   DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    DLDV24              NO LENGTH/QUARTER ELEMENT TO MATCH           
         CLI   0(R4),X'02'         NOT A LEGTH/QUARTER ELEMENT                  
         BNE   DLDV22                                                           
*                                                                               
         CLC   BOBYTE2,RALQYEAR                                                 
         BNE   DLDV22              WRONG YEAR                                   
         OI    MISCFLG1,MF1RTYR                                                 
*                                                                               
         TM    RALQSTAT,X'80'                                                   
         BNZ   DLDV22              NO MINUTES YET                               
         CLC   TLRSLN,RALQLEN                                                   
         BNE   DLDV22              WRONG LENGTH                                 
         OI    MISCFLG1,MF1RTLEN                                                
*                                                                               
         LA    RE,X'80'                                                         
         ZIC   RF,TLRQTR                                                        
         BCTR  RF,0                                                             
         SRL   RE,0(RF)                                                         
         STC   RE,BOBYTE1          MASK FOR QUARTER                             
*                                                                               
         NC    BOBYTE1,RALQQTR     IS THIS MASKED BIT ON?                       
         BNZ   DLDV26              YES RATE CARD OK                             
*                                                                               
DLDV22   DS    0H                  NEXT ELEMENT                                 
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     DLDV20                                                           
*                                                                               
DLDV24   DS    0H                  RATE CARD ERROR                              
         TM    MISCFLG1,MF1RTLEN   QUARTER ERROR?                               
         BNZ   EXINQTR             YES                                          
         TM    MISCFLG1,MF1RTYR    LENGTH ERROR?                                
         BNZ   EXINLEN             YES                                          
         B     EXINYR              NO - YEAR ERROR                              
*                                                                               
DLDV26   DS    0H                  RATE CARD CHECKED OUT                        
*                                                                               
DLDVX    DS    0H                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         LR    RF,RB               TABLE OF KNOWN OBJECTS                       
         AH    RF,=Y(KNOWTAB-PRO17)                                             
         USING KNOWTABD,RF                                                      
*                                                                               
DATA20   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA30              YES                                          
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA20                                                           
***********************************                                             
* WE KNOW OF THIS DATA OBJECT                                                   
***********************************                                             
DATA30   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING FRRRECD,R2          R2 HOLDS A(RECORD)                           
         L     R3,AFRREL                                                        
         USING FRRELD,R3           R3=A(FRREL ON RECORD)                        
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CONTRACT NUMBER                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
CONDTA   DS    0H                                                               
         LA    RF,CONTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CONTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALCON)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(NTRCON)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE A CONTRACT FIELD                                                     
***********************************************************************         
VALCON   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   FVILEN,0            THIS FIELD IS REQUIRED                       
         BE    EXITNO                                                           
*                                                                               
         GOTOX (VALCONQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
         OI    FVIIND,FVIVAL       VALIDATED                                    
         B     EXITOK                                                           
***********************************************************************         
* DISPLAY A CONTRACT FIELD                                                      
***********************************************************************         
NTRCON   DS    0H                                                               
         LR    RE,RA               DO WE HAVE ANY CONTRACT NUMBER?              
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         OC    0(L'SVCONNUM,RE),0(RE)                                           
         BZ    NTRCONX             NO                                           
*                                                                               
         ZAP   BOWORK1+10(5),=P'0'                                              
         MVO   BOWORK1+10(5),0(4,RE)                                            
         ZAP   BOWORK1(5),=P'99999999'                                          
         SP    BOWORK1(5),BOWORK1+10(5)                                         
         OI    BOWORK1+4,X'0F'                                                  
         UNPK  FVIFLD(8),BOWORK1(5)                                             
NTRCONX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROPOSAL NUMBER                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRODTA   DS    0H                                                               
         LA    RF,PROTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PROTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRO)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(NTRPRO)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE A PROPOSAL FIELD                                                     
***********************************************************************         
VALPRO   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         GOTOX (VALPROQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
         OI    FVIIND,FVIVAL       VALIDATED                                    
*                                                                               
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
*                                                                               
         GOTOX (MNIOINQ,AREPRO01),BOPARM INITIALIZE MINIO                       
*                                                                               
         GOTO1 =A(RDBKSDMS),BODMCB,(R9),RR=BORELO                               
*                                                                               
         XC    RPROKEY,RPROKEY                                                  
*                                                                               
VALPROX  B     EXITOK                                                           
***********************************************************************         
* PASS PROPOSAL NUMBER TO NEXT SESSION                                          
***********************************************************************         
NTRPRO   DS    0H                                                               
         LR    RE,RA               DO WE HAVE ANY PROPOSAL NUMBER?              
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         CLI   0(RE),0                                                          
         BE    NTRPROX             NO                                           
*                                                                               
         MVC   BOBYTE1,0(RE)                                                    
         XI    BOBYTE1,X'FF'                                                    
         EDIT  (B1,BOBYTE1),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,         X        
               DUB=BODUB1                                                       
NTRPROX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR AGENCY                                                        
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
AGYDTA   DS    0H                                                               
         MVC   FVIFLD(L'EAGYNAM1),EAGYNAM1                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR ADVERTISER                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
ADVDTA   DS    0H                                                               
         MVC   FVIFLD(L'EADVNAME),EADVNAME                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR PRODUCT                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRDDTA   DS    0H                                                               
         MVC   FVIFLD(L'EPRDNAME),EPRDNAME                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR SALESPERSON                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SALDTA   DS    0H                                                               
         MVC   FVIFLD(L'ESALNAME),ESALNAME                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR BUYER                                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
BYRDTA   DS    0H                                                               
         MVC   FVIFLD(L'ECONBUYR),ECONBUYR                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR FLIGHT DATES                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
FLTDTA   DS    0H                                                               
         GOTO1 VDATCON,BODMCB,(3,CCONDAT),(5,FVIFLD)                            
         MVI   FVIFLD+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,CCONDAT+3),(5,FVIFLD+9)                             
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR DISK ADDRESS                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DSKDTA   DS    0H                                                               
         GOTO1 VHEXOUT,BODMCB,GSRECDA,FVIFLD,L'GSRECDA                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEVELOPMENT SALESPERSON                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DVSDTA   DS    0H                                                               
         LA    RF,DVSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DVSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDVS)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEVELOPMENT SALESPERSON FIELD                                         
***********************************************************************         
DISDVS   DS    0H                                                               
         MVC   FVIFLD(L'EDVSNAME),EDVSNAME                                      
         B     DISDVSX                                                          
*                                                                               
DISDVS5  MVC   FVIFLD(L'CCONDVS),CCONDVS                                        
*                                                                               
DISDVSX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEVELOPMENT TYPE                                              
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DVTDTA   DS    0H                                                               
         LA    RF,DVTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DVTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDVT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEVELOPMENT TYPE                                                      
***********************************************************************         
DISDVT   DS    0H                                                               
         MVC   FVIFLD(L'CCONDVT),CCONDVT                                        
*                                                                               
DISDVTX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR STATION                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
STADTA   DS    0H                                                               
         LA    RF,STATBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
STATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTA)                                 
*        DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTA)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY STATION FIELD                                                         
***********************************************************************         
DISSTA   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
*                                                                               
         TM    MISCFLG1,MF1KYCHG      DID THE KEY CHANGE?                       
         BZ    *+8                    NO                                        
         MVI   0(RE),1                STATION FROM CONTRACT IS #1               
*                                                                               
         CLI   0(RE),0                ANY STATION?                              
         BE    DISSTAX                NONE                                      
         ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
         MVC   FVIFLD(L'SAVSTA),0(R1)                                           
*                                                                               
DISSTAX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAG                                                           
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LTAGDTA  DS     0H                                                              
         LA    RF,LTAGTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LTAGTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLTAG)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY TAG FIELD                                                             
***********************************************************************         
DISLTAG  DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
         ZIC   R0,TLKPG                                                         
         DROP  RE                                                               
         EDIT  (R0),(1,FVIFLD),WRK=BOWORK1,DUB=BODUB1                           
         MVI   FVIFLD+1,C'.'                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LABEL                                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LLBLDTA  DS     0H                                                              
         LA    RF,LLBLTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LLBLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLLBL)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLLBL)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY LABEL FIELD                                                           
***********************************************************************         
DISLLBL  DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
         MVC   FVIFLD(L'TLRLBL),TLRLBL                                          
         DROP  RE                                                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE LABEL FIELD                                                          
***********************************************************************         
VALLLBL  DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
*                                                                               
         MVC   TLRLBL,=CL20'COST'                                               
         EDIT  TLKPG,(1,TLRLBL+4),WRK=BOWORK1,DUB=BODUB1                        
*                                                                               
         CLI   FVILEN,0                                                         
         BE    VALLLBLX                                                         
*                                                                               
         XC    TLRLBL,TLRLBL                                                    
         ZIC   RF,FVXLEN                                                        
         CH    RF,=Y(L'TLRLBL-1)                                                
         BNH   *+8                                                              
         LA    RF,L'TLRLBL-1                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TLRLBL,FVIFLD                                                    
         OC    TLRLBL,BCSPACES                                                  
         DROP  RE                                                               
VALLLBLX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BOOK                                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LBKDTA   DS    0H                                                               
         LA    RF,LBKTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LBKTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLBK)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLBK)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY BOOK FIELD                                                            
***********************************************************************         
DISLBK   DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
         ZIC   R1,TLRBK                                                         
         LTR   R1,R1                                                            
         BZ    DISLBKX                                                          
         DROP  RE                                                               
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINLBLS-TWAD)                                              
         LR    RF,RA                                                            
         AH    RF,=Y(MINBKS-TWAD)                                               
*                                                                               
DISLBK3  CLM   R1,1,0(RF)          INTERNAL ORDER MATCH?                        
         BE    DISLBK5             YES                                          
         LA    RE,L'MINLBL(RE)                                                  
         LA    RF,L'MINBK(RF)                                                   
         LR    R0,RA                                                            
         AH    R0,=Y(MINBKS+L'MINBKS-TWAD)                                      
         CR    RF,R0                                                            
         BL    DISLBK3                                                          
         DC    H'0'                                                             
*                                                                               
DISLBK5  OC    0(L'MINLBL,RE),0(RE)          LABEL?                             
         BZ    *+14                          NO                                 
         MVC   FVIFLD(L'MINLBL),0(RE)                                           
         B     DISLBKX                                                          
*                                                                               
         OC    0(L'MINBK,RF),0(RF)                                              
         BZ    DISLBKX                                                          
*                                                                               
         USING BOOKLIN,RF                                                       
         XC    BOWORK2,BOWORK2                                                  
         MVC   BOWORK2(L'FVIHDR),FVIHDR                                         
         ZIC   RE,BOWORK2                                                       
         LA    RE,5(RE)                                                         
         STC   RE,BOWORK2                                                       
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(3),BKLNBK    JUST SHOW THE 1ST (PRIMARY BOOK)            
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(2),=X'0B07'      PUT OUT SOURCE                           
         MVC   BOELEM+2(1),BKLNSPBK                                             
         DROP  RF                                                               
         GOTOX (UNBOOKQ,AREPRO01),BODMCB,(1,BOWORK1),BOWORK2,          X        
               (C'L',BOELEM),(C'+',=CL6' ')                                     
*                                                                               
         ZIC   RE,BOWORK2                                                       
         LA    RE,BOWORK2(RE)                                                   
         TM    BOWORK2+1,X'02'       EXT FIELD HDR?                             
         BNE   *+8                                                              
         SH    RE,=H'8'                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   0(RE),C')'                                                       
         BNE   DISLBK10                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     DISLBK10                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
DISLBK10 DS    0H                                                               
         LA    RF,BOWORK2+8                                                     
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),BOWORK2+8                                              
*                                                                               
DISLBKX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE BOOK FIELD                                                           
***********************************************************************         
VALLBK   DS    0H                                                               
         GOTO1 =A(VALBOOK),BODMCB,(R9),RR=BORELO                                
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RATE CARD                                                     
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LRTCDTA  DS     0H                                                              
         LA    RF,LRTCTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LRTCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLRTC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLRTC)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY RATE CARD FIELD                                                       
***********************************************************************         
DISLRTC  DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
*                                                                               
         ZIC   R1,TLKPG                                                         
         BCTR  R1,0                                                             
         MH    R1,=Y(4*4)          LENGTH OF ADDRESS GROUP                      
         AR    R1,RA                                                            
         AH    R1,=Y(SVFLADDR-TWAD)                                             
         USING SVFLADDR,R1                                                      
         L     R0,FVADDR                                                        
         SR    R0,RA                                                            
         ST    R0,SVFARTCD         STORE THIS FOR VALIDATION                    
         DROP  R1                                                               
*                                                                               
         MVC   FVIFLD(L'TLRRTCX),TLRRTCX                                        
         DROP  RE                                                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE RATE CARD FIELD                                                      
***********************************************************************         
VALLRTC  DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
         XC    TLRRTCX,TLRRTCX                                                  
         CLI   FVILEN,0                                                         
         BE    VALLRTCX                                                         
*                                                                               
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TLRRTCX,FVIFLD                                                   
         OC    TLRRTCX,BCSPACES                                                 
         DROP  RE                                                               
VALLRTCX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LENGTH                                                        
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LLENDTA  DS    0H                                                               
         LA    RF,LLENTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LLENTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLLEN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLLEN)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY LENGTH FIELD                                                          
***********************************************************************         
DISLLEN  DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
*                                                                               
         ZIC   R1,TLKPG                                                         
         BCTR  R1,0                                                             
         MH    R1,=Y(4*4)          LENGTH OF ADDRESS GROUP                      
         AR    R1,RA                                                            
         AH    R1,=Y(SVFLADDR-TWAD)                                             
         USING SVFLADDR,R1                                                      
         L     R0,FVADDR                                                        
         SR    R0,RA                                                            
         ST    R0,SVFALEN          STORE THIS FOR VALIDATION                    
         DROP  R1                                                               
*                                                                               
         ZIC   R0,TLRSLN                                                        
         DROP  RE                                                               
         EDIT  (R0),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1                
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE LENGTH FIELD                                                         
***********************************************************************         
VALLLEN  DS    0H                                                               
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVI   TLRSLN,0                                                         
*                                                                               
         CLI   FVILEN,0                                                         
         BE    VALLLENX                                                         
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
*                                                                               
         L     R0,BCFULL                                                        
         CH    R0,=H'255'          SPOT LENGTH SHOULD FIT IN 1 BYTE             
         BH    EXITILEN                                                         
         CLI   BCFULL+3,0          AND CAN'T BE 0 SECONDS                       
         BE    EXITILEN                                                         
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINSLNS-TWAD)                                              
         LA    RF,L'MINSLNS(RE)                                                 
VALLEN10 CLC   0(L'MINSLN,RE),BCFULL+3   SEE IF ONE OF OUR CHOSEN SPOT          
         BE    VALLEN20                    LENGTHS FOR THIS PROPOSAL            
         LA    RE,L'MINSLN(RE)                                                  
         CR    RE,RF                                                            
         BL    VALLEN10                                                         
         B     EXITILEN                                                         
*                                                                               
VALLEN20 DS    0H                                                               
         MVC   TLRSLN,BCFULL+3                                                  
         DROP  R3                                                               
*                                                                               
VALLLENX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR QUARTER                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LQTRDTA  DS     0H                                                              
         LA    RF,LQTRTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LQTRTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLQTR)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLQTR)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY QUARTER FIELD                                                         
***********************************************************************         
DISLQTR  DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
*                                                                               
         ZIC   R1,TLKPG                                                         
         BCTR  R1,0                                                             
         MH    R1,=Y(4*4)          LENGTH OF ADDRESS GROUP                      
         AR    R1,RA                                                            
         AH    R1,=Y(SVFLADDR-TWAD)                                             
         USING SVFLADDR,R1                                                      
         L     R0,FVADDR                                                        
         SR    R0,RA                                                            
         ST    R0,SVFAQTR          STORE THIS FOR VALIDATION                    
         DROP  R1                                                               
*                                                                               
         ZIC   R0,TLRQTR                                                        
         DROP  RE                                                               
         EDIT  (R0),(1,FVIFLD),WRK=BOWORK1,DUB=BODUB1                           
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE QUARTER FIELD                                                        
***********************************************************************         
VALLQTR  DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
         MVI   TLRQTR,0                                                         
*                                                                               
         CLI   FVILEN,0                                                         
         BE    VALLQTRX                                                         
*                                                                               
         CLI   FVIFLD,C'1'                                                      
         BE    VALLQTR5                                                         
         CLI   FVIFLD,C'2'                                                      
         BE    VALLQTR5                                                         
         CLI   FVIFLD,C'3'                                                      
         BE    VALLQTR5                                                         
         CLI   FVIFLD,C'4'                                                      
         BNE   EXITNV                                                           
*                                                                               
VALLQTR5 MVC   TLRQTR,BCFULL+3                                                  
         DROP  RE                                                               
VALLQTRX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR YEAR                                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LYRDTA   DS     0H                                                              
         LA    RF,LYRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LYRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLYR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLYR)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY YEAR FIELD                                                            
***********************************************************************         
DISLYR   DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
*                                                                               
         ZIC   R1,TLKPG                                                         
         BCTR  R1,0                                                             
         MH    R1,=Y(4*4)          LENGTH OF ADDRESS GROUP                      
         AR    R1,RA                                                            
         AH    R1,=Y(SVFLADDR-TWAD)                                             
         USING SVFLADDR,R1                                                      
         L     R0,FVADDR                                                        
         SR    R0,RA                                                            
         ST    R0,SVFAYR           STORE THIS FOR VALIDATION                    
         DROP  R1                                                               
*                                                                               
         ZIC   R0,TLRYR                                                         
         TM    TLRYR,X'80'                                                      
         BZ    DISLYRX                                                          
*                                                                               
         N     R0,=XL4'0000007F'                                                
         DROP  RE                                                               
         EDIT  (R0),(2,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1,      X        
               FILL=0                                                           
DISLYRX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE YEAR FIELD                                                           
***********************************************************************         
VALLYR   DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
         MVI   TLRYR,0                                                          
*                                                                               
         CLI   FVILEN,0                                                         
         BE    VALLYRX                                                          
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
*                                                                               
         MVC   TLRYR,BCFULL+3                                                   
         OI    TLRYR,X'80'                                                      
         DROP  RE                                                               
*                                                                               
VALLYRX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR % OF BASE COST                                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LPCDTA   DS     0H                                                              
         LA    RF,LPCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LPCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLPC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLPC)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY % OF BASE COST FIELD                                                  
***********************************************************************         
DISLPC   DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
         CLI   TLRPC,0                                                          
         BE    DISLPCX                                                          
*                                                                               
         CLC   TLRPC,=X'FF'                                                     
         BNE   *+18                                                             
         MVC   FVIFLD(3),=C'100'                                                
         OI    FVATRB,FVAPROT                                                   
         B     DISLPCX                                                          
*                                                                               
         EDIT  (B1,TLRPC),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,           X        
               DUB=BODUB1                                                       
DISLPCX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE % OF BASE COST FIELD                                                 
***********************************************************************         
VALLPC   DS    0H                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
         MVI   TLRPC,0                                                          
*                                                                               
         CLI   FVILEN,0                                                         
         BE    VALLPCX                                                          
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
*                                                                               
         CLC   BCFULL,=F'200'                                                   
         BH    EXITNV                                                           
*                                                                               
         MVC   TLRPC,BCFULL+3                                                   
         DROP  RE                                                               
*                                                                               
VALLPCX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                                   
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 HOLDS CURRENT KEY BUILD AREA   (THIS)                                      
* P4 HOLDS PREVIOUS KEY BUILD AREA  (LAST)                                      
***********************************************************************         
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING RPROKEY,R2                                                       
LAST     USING RPROKEY,R3                                                       
*                                                                               
         LR    RF,RB                                                            
         AH    RF,=Y(LISTABL-PRO17)                                             
         USING OBJTABD,RF                                                       
LITER    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    LITER02             MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     LITER               ITERATE TABLE                                
*                                                                               
LITER02  CLC   OBJIND3,GSSMPAGE    CHECK PAGE OK (0 FOR LIST)                   
         BE    LITER04                                                          
         LA    RF,OBJTABL(RF)                                                   
         B     LITER                                                            
*                                                                               
LITER04  ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* SET DEFAULT COLUMNS                                                           
***********************************************************************         
DEFCLM1  DS    0H                                                               
         GOTO1 =A(DEFCLMNS),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE FOR LIST                                                           
***********************************************************************         
INITL1   DS    0H                                                               
**       OI    LSSTAT1,LSSSEL+LSSTSAR   SELECTABLE LIST OF TSAR RECS            
         OI    LSSTAT1,LSSTSAR                                                  
         OI    LSSTAT2,LSSIUPD          WE WANT TO DO OUR OWN UPDATES           
*        MVI   LSSUBLEN,2               LENGTH OF SUB-ACTION FIELD              
         MVI   LSSUBLEN,0               LENGTH OF SUB-ACTION FIELD              
*                                                                               
         NI    LSSTAT2,X'FF'-LSSADD   NOT VALID TO ADD NEW LIST LINES           
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* FIRST TIME FOR LIST                                                           
***********************************************************************         
FTFLST1  DS    0H                                                               
         XC    SVMINEKY,SVMINEKY                                                
         MVI   SVMINEKY,RPRCHELQ   NEED A COST HEADER ELEMENT                   
         B     EXITOK                                                           
***********************************************************************         
* FIRST FOR LIST                                                                
***********************************************************************         
FLST1    DS    0H                                                               
         LA    R3,1                                                             
         L     R4,ATLST                                                         
         USING TLSTD,R4                                                         
*                                                                               
FLST5    GOTOX AGENLST,BOPARM,OLIST,LTSARDIR,IOKEY                              
*                                                                               
         MVC   TLRLEN,=AL2(TSARLNQ)     MY TSAR RECORD LENGTH                   
         STC   R3,TLKPG                                                         
*                                                                               
         CLI   TLKPG,1                                                          
         BNE   *+8                                                              
         MVI   TLRPC,X'FF'                                                      
*                                                                               
         MVC   TLRLBL,=CL(20)'COST'                                             
         EDIT  TLKPG,(1,TLRLBL+4),WRK=BOWORK1,DUB=BODUB1                        
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRCHELQ                                                 
         STC   R3,MINEKEY+1                                                     
         BAS   RE,MINIOHI                                                       
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRCHELQ                                                   
         BNE   FLST15                                                           
         USING RPRCHELD,R6                                                      
         CLM   R3,1,RPRCHSEQ                                                    
         BNE   FLST15                                                           
*                                                                               
         MVC   TLRBK,RPRCHBK                                                    
         MVC   TLRRTCD,RPRCHRTC                                                 
         MVC   TLRRTC2,RPRCHRT2                                                 
         MVC   TLRQTR,RPRCHQTR                                                  
         MVC   TLRYR,RPRCHYR                                                    
         MVC   TLRSLN,RPRCHSLN                                                  
*                                                                               
         CLC   TLRPC,=X'FF'                                                     
         BE    *+10                                                             
         MVC   TLRPC,RPRCHPBC                                                   
*                                                                               
         ZIC   R1,RPRCHLEN                                                      
         XC    TLRLBL,TLRLBL                                                    
         SH    R1,=Y(RPRCHOVQ-1)                                                
         CH    R1,=Y(L'TLRLBL-1)                                                
         BNH   *+8                                                              
         LA    R1,L'TLRLBL-1                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TLRLBL(0),RPRCHLBL                                               
         OC    TLRLBL,BCSPACES                                                  
         DROP  R6                                                               
*                                                                               
FLST15   GOTOX AGENLST,BOPARM,OLIST,LTSARADD                                    
         LA    R3,1(R3)                                                         
         CH    R3,=H'4'                                                         
         BNH   FLST5                                                            
*                                                                               
         B     EXITL                                                            
         DROP  R4                                                               
***********************************************************************         
* NEXT FOR LIST                                                                 
***********************************************************************         
NLST1    DS    0H                                                               
         B     EXITL               PRETEND NOTHING TO FIND                      
         EJECT                                                                  
***********************************************************************         
* UPDATE RECORD FROM TSAR                                                       
***********************************************************************         
UPDREC   DS    0H                                                               
         L     R4,ATLST                                                         
         USING TLSTD,R4                                                         
         MVC   BOWORK1(L'TLRLBL),=CL(20)'COST'                                  
         EDIT  TLKPG,(1,BOWORK1+4),WRK=BOWORK2,DUB=BODUB1                       
*                                                                               
         OC    TLRLBL,TLRLBL                                                    
         BNZ   *+10                                                             
         MVC   TLRLBL,BOWORK1                                                   
*                                                                               
         ZIC   R3,TLKPG                                                         
         BCTR  R3,0                                                             
         LA    R3,BASEPERC(R3)                                                  
         MVI   0(R3),0                                                          
         CLC   TLRPC,=X'FF'                                                     
         BE    *+10                                                             
         MVC   0(1,R3),TLRPC                                                    
*                                                                               
         ZIC   RE,TLKPG                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'CHGRATC)                                                 
         LA    RE,CHGRATCS(RE)                                                  
         XC    0(L'CHGRATC,RE),0(RE)                                            
         USING RFTCRTES,RE                                                      
*                                                                               
         MVC   RFTCRTCD,TLRRTCX                                                 
         MVC   RFTCQQTR,TLRQTR                                                  
         MVC   RFTCQYR,TLRYR                                                    
         NI    RFTCQYR,X'7F'                                                    
         MVC   RFTCSLN,TLRSLN                                                   
         DROP  RE                                                               
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRCHELQ                                                 
         MVC   MINEKEY+1,TLKPG                                                  
         BAS   RE,MINIOHI                                                       
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRCHELQ                                                   
         BNE   UPDREC10                                                         
         USING RPRCHELD,R6                                                      
         CLC   RPRCHSEQ,TLKPG                                                   
         BNE   UPDREC10                                                         
*                                                                               
         CLC   TLRPC,RPRCHPBC                                                   
         BNE   *+8                                                              
         MVI   0(R3),0                                                          
*                                                                               
         ZIC   RE,TLKPG                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'CHGRATC)                                                 
         LA    RE,CHGRATCS(RE)                                                  
         USING RFTCRTES,RE                                                      
*                                                                               
         CLI   TLRPC,X'FF'                                                      
         BE    *+12                                                             
         CLI   TLRPC,0                                                          
         BNE   UPDREC02              COST %, NO RATE CARD                       
*                                                                               
         CLC   RPRCHRTC,TLRRTCD    RATE CARD CHANGE?                            
         BNE   UPDREC08            YES                                          
         CLC   RPRCHRT2,TLRRTC2    RATE CARD CHANGE?                            
         BNE   UPDREC08            YES                                          
         CLC   RPRCHQTR,TLRQTR     QUARTER CHANGE?                              
         BNE   UPDREC08            YES                                          
         CLC   RPRCHYR,TLRYR       YEAR CHANGE?                                 
         BNE   UPDREC08            YES                                          
         CLC   RPRCHSLN,TLRSLN     LENGTH CHANGE?                               
         BNE   UPDREC08            YES                                          
         DROP  RE                                                               
*                                                                               
UPDREC02 DS    0H                  SAVE CHANGED RATE CARD                       
         XC    0(L'CHGRATC,RE),0(RE)                                            
*                                                                               
UPDREC08 DS    0H                                                               
         BAS   RE,MINIODEL                                                      
*                                                                               
UPDREC10 L     R6,MINELEM                                                       
         XC    0(256,R6),0(R6)                                                  
         USING RPRCHELD,R6                                                      
         MVI   RPRCHEL,RPRCHELQ                                                 
         MVC   RPRCHSEQ,TLKPG                                                   
         MVC   RPRCHBK,TLRBK                                                    
         MVC   RPRCHRTC,TLRRTCD                                                 
         MVC   RPRCHRT2,TLRRTC2                                                 
         MVC   RPRCHQTR,TLRQTR                                                  
         MVC   RPRCHYR,TLRYR                                                    
         MVC   RPRCHSLN,TLRSLN                                                  
*                                                                               
         CLC   TLRPC,=X'FF'                                                     
         BE    *+10                                                             
         MVC   RPRCHPBC,TLRPC                                                   
*                                                                               
         MVC   RPRCHLBL(L'TLRLBL),TLRLBL                                        
         LA    RE,RPRCHOVQ+L'TLRLBL(R6)                                         
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RE,1(RE)                                                         
         XC    0(L'TLRLBL,RE),0(RE)                                             
         SR    RE,R6                                                            
         STC   RE,RPRCHLEN                                                      
*                                                                               
         OC    RPRCHBK(RPRCHOVQ-(RPRCHBK-RPRCHELD)),RPRCHBK                     
         BNZ   UPDREC20                                                         
         CLC   TLRLBL,BOWORK1                                                   
         BE    UPDRECX             NOT WORTH ADDING                             
*                                                                               
UPDREC20 BAS   RE,MINIOADD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
UPDRECX  B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                                 
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 HOLDS A(BLOCK) COVERED BY SSAVD                                            
***********************************************************************         
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITIN),AL1(0,0,0),AL4(NTRXIN)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                                      
***********************************************************************         
         PUSH  USING                                                            
         USING FSRRECD,GSRECKEY                                                 
NTROUT   DS    0H                                                               
         CLI   SREC,O#MAX          CHECK FOR CONTROLLER                         
         BNH   EXITOK                                                           
*                                                                               
         OI    SNINDS1,SNIPARMS    SO WE CAN GET DNTR                           
*                                                                               
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         XC    SDATA,SDATA                                                      
         MVC   SDATA(6),=C'COSLST'                                              
         MVC   SDATA+6,TLKPG                                                    
         DROP  R6                                                               
*                                                                               
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                                    
***********************************************************************         
         PUSH  USING                                                            
NTRIN    DS    0H                                                               
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS COMIMG BACK FROM CALLED SESSION                                       
***********************************************************************         
         PUSH  USING                                                            
NTRXIN   DS    0H                                                               
         OI    MISCFLG1,MF1PFRET                                                
*                                                                               
         CLC   =C'COSCHA',SDATA                                                 
         BNE   NTRXINX                                                          
*                                                                               
         LA    R1,GSRECKEY                                                      
         USING RPROKEY,R1                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         MVC   RPROKCON,0(RE)                                                   
         MVC   RPROKPRO,SVPRONUM-SVCONNUM(RE)                                   
         DROP  R1                                                               
         GOTOX (MNIOINQ,AREPRO01),BOPARM                                        
*                                                                               
* UPDATE TSAR                                                                   
         L     R4,ATLST                                                         
         USING TLSTD,R4                                                         
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRCHELQ                                                 
         MVC   MINEKEY+1,TLKPG                                                  
         BAS   RE,MINIOHI                                                       
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRCHELQ                                                   
         BNE   NTXI15                                                           
         USING RPRCHELD,R6                                                      
         CLC   TLKPG,RPRCHSEQ                                                   
         BNE   NTXI15                                                           
*                                                                               
         MVC   TLRBK,RPRCHBK                                                    
         MVC   TLRRTCD,RPRCHRTC                                                 
         MVC   TLRRTC2,RPRCHRT2                                                 
         MVC   TLRQTR,RPRCHQTR                                                  
         MVC   TLRYR,RPRCHYR                                                    
         MVC   TLRSLN,RPRCHSLN                                                  
         ZIC   R1,RPRCHLEN                                                      
         SH    R1,=Y(RPRCHOVQ-1)                                                
         CH    R1,=Y(L'TLRLBL-1)                                                
         BNH   *+8                                                              
         LA    R1,L'TLRLBL-1                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TLRLBL(0),RPRCHLBL                                               
         OC    TLRLBL,BCSPACES                                                  
         DROP  R6,R4,R5                                                         
*                                                                               
NTXI15   DS    0H                                                               
*                                                                               
NTRXINX  B     EXITOK                                                           
         POP   USING                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                                 
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
SCRN     LM    R0,R3,SVPARMS                                                    
         LA    RF,SCRNTBL                                                       
         B     ITER                                                             
*                                                                               
SCRNTBL  DC    AL1(SKSET),AL1(0,0,0),AL4(SETKSCR)                               
         DC    AL1(SSET),AL1(0,0,0),AL4(SETMSCR)                                
         DC    AL1(SMOD),AL1(0,0,0),AL4(MODSCR)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET THE KEY SCREEN CODE                                                       
***********************************************************************         
SETKSCR  DS    0H                                                               
         MVI   GSSKCODE,0                                                       
         B     EXITOK                                                           
***********************************************************************         
* SET THE MAINT SCREEN CODE                                                     
***********************************************************************         
SETMSCR  DS    0H                                                               
         MVI   GSSMCODE,0                                                       
         B     EXITOK                                                           
***********************************************************************         
* MODIFY THE SCREEN FIELDS (PULL OUT THE KEYS FROM AKYFLD)                      
***********************************************************************         
MODSCR   DS    0H                                                               
         L     R5,ATWA             SCREEN                                       
         LA    R5,64(R5)           SKIP HEADER                                  
*                                                                               
MODSCR10 CLI   0(R5),0             END OF SCREEN?                               
         BE    MODSCRX             YES, WE'RE DONE                              
*                                                                               
         TM    1(R5),X'02'         EXTENDED HEADER?                             
         BZ    MODSCRNX            NO, SKIP TO NEXT FIELD                       
*                                                                               
MODSCR20 LR    RF,R5               RF = A(EXTENDED FIELD HDR)                   
         ZIC   R0,0(R5)                                                         
         AR    RF,R0                                                            
         SH    RF,=H'8'                                                         
         USING FVIXHDR,RF                                                       
         ZIC   RE,FVIXUS2          RE = FIELD #                                 
         DROP  RF                                                               
         BCTR  RE,0                MAKE IT ZERO-BASED                           
         LTR   RE,RE                                                            
         BM    MODSCRNX            SKIP FLUFF                                   
*                                                                               
         SLL   RE,2                MULTIPLY BY 4                                
         L     R6,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R6,0(RE,R6)         A(THIS FIELD ENTRY)                          
         L     RF,0(R6)            THIS FIELD ENTRY                             
         USING FDRELD,RF                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
         ZICM  RE,FDRNUM,2                                                      
         DROP  RF                                                               
*                                                                               
         LA    RF,KNOWTAB2                                                      
         USING KNOWTABD,RF                                                      
MODSCR30 CLC   KNOWID,=AL2(EOT)    REACH END OF KEY WANTED LIST?                
         BE    MODSCRNX            YES, CHECK NEXT SCREEN FIELD                 
         CLM   RE,3,KNOWID         IS THIS A WANTED KEY?                        
         BE    MODSCR40            YES                                          
         LA    RF,KNOWLQ(RF)                                                    
         B     MODSCR30                                                         
         DROP  RF                                                               
*                                                                               
MODSCR40 SR    R0,R0               SEE IF WE HAVE DATA FOR THIS FIELD           
         L     RF,AKYFLD                                                        
         ZICM  R1,0(RF),2          R1 = A(AFTER LAST KEY FIELD ENTRY)           
         AR    R1,RF                                                            
         LA    RF,2(RF)            RF = A(1ST ENTRY IN KEY FIELD TBL)           
         USING KEYELD,RF                                                        
MODSCR45 CLI   KEYEL,KEYELQ                                                     
         BNE   MODSCRNX            NO DATA FOR THIS KEY FIELD                   
*                                                                               
         CLM   RE,3,KEYNUM         MATCH ON THIS FIELD?                         
         BE    MODSCR50                                                         
         IC    R0,KEYLN            NO                                           
         AR    RF,R0                                                            
         CR    RF,R1                                                            
         BNL   MODSCRNX                                                         
         B     MODSCR45                                                         
*                                                                               
MODSCR50 ZIC   R1,KEYLN            COPY THE DATA OVER TO THE FIELD              
         SH    R1,=Y(KEYLN1Q+1)                                                 
         BM    MODSCRNX                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    8(0,R5),8(R5)       IS THERE ANYTHING HERE?                      
         BNZ   MODSCRNX            YES                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R5),KEYDATA                                                  
         LA    R1,1(R1)                                                         
         STC   R1,5(R5)                                                         
         OI    6(R5),X'80'         AND TRANSMIT IT                              
         DROP  RF                                                               
*                                                                               
MODSCRNX ZIC   R0,0(R5)            BUMP TO NEXT SCREEN FIELD                    
         AR    R5,R0                                                            
         B     MODSCR10                                                         
*                                                                               
MODSCRX  B     EXITOK                                                           
***********************************************************************         
* TABLE OF WANTED KEY OBJECTS                                                   
***********************************************************************         
KNOWTAB2 DS    0XL(KNOWLQ)                                                      
         DC    AL2(00001),AL4(FLDPROT)   CONTRACT                               
         DC    AL2(00002),AL4(FLDPROT)   PROPOSAL                               
         DC    AL2(EOT)                                                         
*                                                                               
FLDPROT  DC    H'0'                DUMMY BRANCH                                 
         EJECT                                                                  
***********************************************************************         
* SUB-ACTION OBJECT                                                             
* -----------------                                                             
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
SUBACT   DS    0H                                                               
         B     EXITH                                                            
         EJECT                                                                  
***********************************************************************         
* PFKEY OBJECT                                                                  
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
PFKEY    LM    R0,R3,SVPARMS                                                    
         LA    RF,PFKYTBL                                                       
         B     ITER                                                             
*                                                                               
PFKYTBL  DC    AL1(PFREC),AL1(0,0,0),AL4(RECPFK)                                
         DC    AL1(PFACT),AL1(0,0,0),AL4(ACTPFK)                                
         DC    AL1(PFUSER),AL1(0,0,0),AL4(USRPFK)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* CAN SET THE RECORD FOR THE PFKEY                                              
***********************************************************************         
RECPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFAVAIL       AVAIL?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Ava'                                              
         B     RECPFKX                                                          
*                                                                               
         CLI   0(RE),PFPACKGE      PACKAGE?                                     
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Pack'                                             
         B     RECPFKX                                                          
*                                                                               
         CLI   0(RE),PFMBOOK       MBOOK?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Mbk'                                              
         B     RECPFKX                                                          
*                                                                               
         CLI   0(RE),PFMDEMO       MDEMO?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Mdem'                                             
         B     RECPFKX                                                          
*                                                                               
         B     NOTPFK              EVERYTHING ELSE - NO RECORD                  
*                                                                               
RECPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE ACTION FOR THE PFKEY                                              
***********************************************************************         
ACTPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         B     NOTPFK              EVERYTHING ELSE - NO ACTION                  
*                                                                               
ACTPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE USER DEFINED NAME FOR THE PFKEY                                   
***********************************************************************         
USRPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
*                                                                               
         CLI   0(RE),PFKYUP        UP?                                          
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Up'                                               
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFKYDOWN      DOWN?                                        
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Dwn'                                              
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFKYLEFT      LEFT?                                        
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Lft'                                              
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFKYRGHT      RIGHT?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Rt'                                               
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFRETURN      RETURN?                                      
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Re'                                               
         B     USRPFKX                                                          
*                                                                               
USRPFKX  B     EXITOK                                                           
***********************************************************************         
* PFKEY DEFINITION (RECORD, ACTION, OR USER) NOT WANTED                         
***********************************************************************         
NOTPFK   OI    SVPARMS3,X'80'                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SETS UP AIO5 SO WE CAN USE RECUP TO ADD ELEMENTS, DELETE ELEMENTS, OR         
* CHANGE THE SIZE OF EXISTING ELEMENTS OF THE CLUSTER IN MINELEM                
***********************************************************************         
INTOAIO5 NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         L     RE,AIO5             COPY KEY, LEN, STAT, & LINK                  
         L     RF,MINBUFF                                                       
         MVC   0(RPROR1ST-RPROKEY,RE),0(RF)                                     
         LA    RE,RPROR1ST-RPROKEY(RE)                CLUSTER GOES HERE         
         LA    RF,IOAREALN-(RPROR1ST-RPROKEY)                                   
*                                                                               
         L     R0,MINELEM          COPY THE ENTIRE CLUSTER                      
         LH    R1,MINELEML                                                      
         MVCL  RE,R0                                                            
*                                                                               
         LH    R1,MINELEML                                                      
         AH    R1,=Y(RPROR1ST-RPROKEY)     L(FAKE RECORD)                       
         L     RE,AIO5             COPY KEY, LEN, STAT, & LINK                  
         STCM  R1,3,RPRORLEN-RPROKEY(RE)                                        
         B     EXITOK                                                           
         DROP  R5                                                               
***********************************************************************         
* SETS UP MINELEM FROM AIO5 BECAUSE WE NEEDED RECUP                             
***********************************************************************         
FROMAIO5 NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         L     RE,AIO5                                                          
         ZICM  RF,RPRORLEN-RPROKEY(RE),2                                        
         SH    RF,=Y(RPROR1ST-RPROKEY)     L'CLUSTER                            
         LA    RE,RPROR1ST-RPROKEY(RE)                                          
*                                                                               
         L     R0,MINELEM                                                       
         LH    R1,MINMAXEL                                                      
         MVCL  R0,RE               COPY CLUSTER                                 
*                                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS FOR A MINIO ELEMENT                                        
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIORD  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINRD',(R5))                                     
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS HIGH FOR A MINIO ELEMENT.                                  
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIOHI  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINHI',(R5))                                     
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         B     EXITL               OTHERWISE RETURN 'NO'                        
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS SEQUENTIAL FOR A MINIO ELEMENT.                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOSEQ NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINSEQ',(R5))                                    
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    EXITL                                                            
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOWRT NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         GOTO1 VMINIO,BODMCB,('MINWRT',(R5))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LA    R5,PSSAV               COMING FROM WORK/UPDATE?                  
         USING SSAVD,R5                                                         
         CLC   =C'WORUPD',SDATA                                                 
         BNE   MINWRTX                NO                                        
         MVC   SDATA(7),=C'COSLSTY'   YES, DRASTIC CHANGE TO PROPOSAL           
*                                                                               
MINWRTX  B     EXITOK                                                           
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.                                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOADD NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         GOTO1 VMINIO,BODMCB,('MINADD',(R5))                                    
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    EXITL               YES, RETURN A NO                             
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LA    R5,PSSAV               COMING FROM WORK/UPDATE?                  
         USING SSAVD,R5                                                         
         CLC   =C'WORUPD',SDATA                                                 
         BNE   MINADDX                NO                                        
         MVC   SDATA(7),=C'COSLSTY'   YES, DRASTIC CHANGE TO PROPOSAL           
*                                                                               
MINADDX  B     EXITOK                                                           
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT.  CALLER IS RESPONSIBLE FOR              
* POINTING TO ELEMENT FIRST.                                                    
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIODEL NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINDEL',(R5))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         ZIC   RF,MINNBUF          NUMBER OF BUFFERS BEING USED                 
         L     RE,MINBUFF          A(1ST BUFFER BEING USED BY MINIO)            
         USING RPROHDRD,RE                                                      
MNIODEL2 CLC   RPRORLEN,=Y(RPROR1ST-RPROHDRD)   ANY ELEMENTS IN RECORD?         
         BH    MNIODEL4                         YES, CHECK NEXT BUFFER          
*                                                                               
         MVC   RPRORLEN,=Y(RPROR1ST-RPROHDRD+2) DMDALINK: MIN IS 36             
         OI    RPRORSTA,X'80'                   MARK FOR DELETE                 
         LA    R1,RPROR1ST                                                      
         XC    0(3,R1),0(R1)                    FAKE ELEMENT                    
*                                                                               
MNIODEL4 AH    RE,MINFRCLM         BUMP TO NEXT MINIO BUFFER                    
         BCT   RF,MNIODEL2         LOOP UNTIL ALL BUFFERS CHECKED               
*                                                                               
MNIODELX B     EXITOK                                                           
         DROP  R5,RE                                                            
***********************************************************************         
* THIS ROUTINE CLOSES MINIO AND FLUSHES OUT THE BUFFERS TO THE MINIO            
* RECORDS.                                                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOCLS NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINCLS',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
***************                                                                 
* INFO EXITS                                                                    
***************                                                                 
EXITENTR MVC   FVMSGNO,=AL2(GI$ENTER)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXITL               EXIT WITH ENTER DATA                         
***************                                                                 
* ERROR EXITS                                                                   
***************                                                                 
EXINRTCD DS    0H                                                               
         LA    R1,EXITRCNF         RECORD NOT FOUND                             
         B     EXI0RTCD                                                         
EXIXRTCD DS    0H                                                               
         LA    R1,EXITNV           INVALID                                      
         B     EXI0RTCD                                                         
EXITRTCD DS    0H                                                               
         LA    R1,EXITNO           MISING INPUT                                 
EXI0RTCD DS    0H                                                               
         L     RE,ATLST                                                         
         ZIC   RF,TLKPG-TLSTD(RE)                                               
         BCTR  RF,0                                                             
         MH    RF,=Y(4*4)          LENGTH OF ADDRESS GROUP                      
         AR    RF,RA                                                            
         AH    RF,=Y(SVFLADDR-TWAD)                                             
         L     RF,SVFARTCD-SVFLADDR(RF)                                         
         AR    RF,RA                                                            
         ST    RF,FVADDR                                                        
         BR    R1                                                               
*                                                                               
EXINLEN  DS    0H                                                               
         LA    R1,EXITRCNF         RECORD NOT FOUND                             
         B     EXI0LEN                                                          
EXIXLEN  DS    0H                                                               
         LA    R1,EXITNV           INVALID                                      
         B     EXI0LEN                                                          
EXITLEN  DS    0H                                                               
         LA    R1,EXITNO           MISING INPUT                                 
EXI0LEN  DS    0H                                                               
         L     RE,ATLST                                                         
         ZIC   RF,TLKPG-TLSTD(RE)                                               
         BCTR  RF,0                                                             
         MH    RF,=Y(4*4)          LENGTH OF ADDRESS GROUP                      
         AR    RF,RA                                                            
         AH    RF,=Y(SVFLADDR-TWAD)                                             
         L     RF,SVFALEN-SVFLADDR(RF)                                          
         AR    RF,RA                                                            
         ST    RF,FVADDR                                                        
         BR    R1                                                               
*                                                                               
EXINQTR  DS    0H                                                               
         LA    R1,EXITRCNF         RECORD NOT FOUND                             
         B     EXI0QTR                                                          
EXIXQTR  DS    0H                                                               
         LA    R1,EXITNV           INVALID                                      
         B     EXI0QTR                                                          
EXITQTR  DS    0H                                                               
         LA    R1,EXITNO           MISING INPUT                                 
EXI0QTR  DS    0H                                                               
         L     RE,ATLST                                                         
         ZIC   RF,TLKPG-TLSTD(RE)                                               
         BCTR  RF,0                                                             
         MH    RF,=Y(4*4)          LENGTH OF ADDRESS GROUP                      
         AR    RF,RA                                                            
         AH    RF,=Y(SVFLADDR-TWAD)                                             
         L     RF,SVFAQTR-SVFLADDR(RF)                                          
         AR    RF,RA                                                            
         ST    RF,FVADDR                                                        
         BR    R1                                                               
*                                                                               
EXINYR   DS    0H                                                               
         LA    R1,EXITRCNF         RECORD NOT FOUND                             
         B     EXI0YR                                                           
EXIXYR   DS    0H                                                               
         LA    R1,EXITNV           INVALID                                      
         B     EXI0YR                                                           
EXITYR   DS    0H                                                               
         LA    R1,EXITNO           MISING INPUT                                 
EXI0YR   DS    0H                                                               
         L     RE,ATLST                                                         
         ZIC   RF,TLKPG-TLSTD(RE)                                               
         BCTR  RF,0                                                             
         MH    RF,=Y(4*4)          LENGTH OF ADRESS GROUP                       
         AR    RF,RA                                                            
         AH    RF,=Y(SVFLADDR-TWAD)                                             
         L     RF,SVFAYR-SVFLADDR(RF)                                           
         AR    RF,RA                                                            
         ST    RF,FVADDR                                                        
         BR    R1                                                               
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITRCNF MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITL               EXIT WITH RECORD NOT ON FILE                 
EXITRCDL MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     EXITL               EXIT WITH RECORD IS DELETED                  
EXITRCAE MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     EXITL               EXIT WITH RECORD ALREADY EXISTS              
EXITCRES MVC   FVMSGNO,=AL2(FVFXRES)                                            
         B     EXITL               EXIT WITH RECORD CAN'T BE RESTORED           
EXITCCHG MVC   FVMSGNO,=AL2(CSTCHNGD)                                           
         B     EXITL               EXIT WITH COST WAS ALREADY CHANGED           
TABCHNGD MVC   FVMSGNO,=AL2(BCPPCHGD)                                           
         B     EXITL               EXIT WITH BUYER'S CPP WAS CHANGED            
EXITILEN MVC   FVMSGNO,=AL2(INVSPLEN)                                           
         B     EXITL               EXIT WITH INVALID LENGTH                     
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
PFAVAIL  EQU   PFK03               PFKEY FOR AVAIL                              
PFPACKGE EQU   PFK04               PFKEY FOR PACKAGE                            
PFMBOOK  EQU   PFK05               PFKEY FOR MULTI-BOOK                         
PFMDEMO  EQU   PFK06               PFKEY FOR MULTI-DEMO                         
PFKYUP   EQU   PFK07               PFKEY FOR SCROLL UP                          
PFKYDOWN EQU   PFK08               PFKEY FOR SCROLL DOWN                        
PFKYLEFT EQU   PFK09               PFKEY FOR SCROLL LEFT                        
PFKYRGHT EQU   PFK10               PFKEY FOR SCROLL RIGHT                       
PFRETURN EQU   PFK12               PFKEY FOR RETURN                             
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
* SET DEFAULT COLUMNS 1                                                         
***********************************************************************         
DEFCLMNS DS    0H                                                               
         NMOD1 0,**DEFC**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LA    RF,LSFIXCLM         RF = A(1ST FIXED COLUMN)                     
         USING DCTABD,RF                                                        
         XC    LSFIXNUM,LSFIXNUM   # OF FIX. COLUMNS                            
         LA    RE,0                                                             
*                                                                               
         MVC   DCTFLD#,=AL2(19)    HEADING                                      
         LA    RF,DCTABL(RF)                                                    
         LA    RE,1(RE)                                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(13)    LABEL                                        
         LA    RF,DCTABL(RF)                                                    
         LA    RE,1(RE)                                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(14)    BOOK                                         
         LA    RF,DCTABL(RF)                                                    
         LA    RE,1(RE)                                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(27)    % OF BASE COST(COST 1)                       
         LA    RF,DCTABL(RF)                                                    
         LA    RE,1(RE)                                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(15)    RATE CARD                                    
         LA    RF,DCTABL(RF)                                                    
         LA    RE,1(RE)                                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(16)    LEN                                          
         LA    RF,DCTABL(RF)                                                    
         LA    RE,1(RE)                                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(17)    QUARTER                                      
         LA    RF,DCTABL(RF)                                                    
         LA    RE,1(RE)                                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(18)    YEAR                                         
         LA    RE,1(RE)                                                         
*                                                                               
         STCM  RE,3,LSFIXNUM       # OF FIX. COLUMNS                            
         MVC   LSVARNUM,=AL2(0)    # OF VAR. COLUMNS                            
*                                                                               
DMLCX    B     EXITOK                                                           
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE BOOK FIELD                                                           
***********************************************************************         
VALBOOK  DS    0H                                                               
         NMOD1 0,*VALBK**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
         MVI   TLRBK,0                                                          
         DROP  RE                                                               
*                                                                               
         CLI   FVILEN,0            ANY BOOK ENTERED HERE?                       
         BE    VALBOKX             YES, VALIDATE IT                             
*                                                                               
         GOTO1 VBOOKVAL,BODMCB,(C'N',FVIHDR),(1,BOWORK1),              X        
               (C'B',VSCANNER),BODUB1                                           
         CLI   4(R1),0             GOOD BOOK?                                   
         BE    VALBOK50            NO, COULD BE USER-DEFINED BOOK               
*                                                                               
         LR    R2,RA               FIND THE PRIME BOOK NUMBER                   
         AH    R2,=Y(MINBKS-TWAD)                                               
         USING BOOKLIN,R2                                                       
*                                                                               
VALBOK20 CLC   BKLNBK,BOWORK1      MATCH ON THIS 3-BYTE BOOK?                   
         BNE   *+24                NO                                           
         CLC   BKLNSPBK,BODUB1     MATCH ON SPECIAL BOOK?                       
         BNE   *+14                NO                                           
         OC    BKLNUPGD,BKLNUPGD   UPGRADE?                                     
         BZ    VALBOK60            NO                                           
         LA    R2,L'MINBK(R2)      BUMP TO NEXT MINBKS ENTRY                    
         LR    R0,RA                                                            
         AH    R0,=Y(MINBKS+L'MINBKS-TWAD)                                      
         CR    R2,R0                                                            
         BNL   EXITNV              BOOK IS NOT PART OF OUR BOOKS LIST           
         B     VALBOK20                                                         
*                                                                               
VALBOK50 LR    R3,RA                                                            
         AH    R3,=Y(MINLBLS-TWAD)                                              
         LR    R2,RA               FIND THE PRIME BOOK NUMBER                   
         AH    R2,=Y(MINBKS-TWAD)                                               
         OC    FVIFLD(8),BCSPACES                                               
*                                                                               
VALBOK55 CLC   FVIFLD(L'MINLBL),0(R3)   MATCH ON THIS USER-DEFINED LBL?         
         BE    VALBOK60                 YES                                     
         LA    R3,L'MINLBL(R3)                                                  
         LA    R2,L'MINBK(R2)      BUMP TO NEXT MINBKS ENTRY                    
         LR    R0,RA                                                            
         AH    R0,=Y(MINLBLS+L'MINLBLS-TWAD)                                    
         CR    R3,R0                                                            
         BNL   EXITNV              BOOK IS NOT PART OF OUR BOOKS LIST           
         B     VALBOK55                                                         
*                                                                               
VALBOK60 DS    0H                  SAVE # OF WHERE IN THE DISPLAY LIST          
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
         MVC   TLRBK,BKLNIORD                                                   
         DROP  RE,R2                                                            
*                                                                               
VALBOKX  B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READS THE BOOK ELEMENTS INTO SAVBKS, SAVLBLS                                  
*                              MINBKS, MINLBLS                                  
***********************************************************************         
RDBKSDMS DS    0H                                                               
         NMOD1 0,**BKDM**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
***************                                                                 
* BOOK ELEMENT(S)                                                               
***************                                                                 
         XC    SAVBKS,SAVBKS                                                    
         XC    SAVLBLS,SAVLBLS                                                  
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ    GET THE BOOK ELEMENT                         
         BAS   RE,MINIOHI                                                       
         BNE   RDBDBKX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
*                                                                               
RDBDBK10 CLI   0(R6),RPRBKELQ                                                   
         BNE   RDBDBKX                                                          
*                                                                               
         ZIC   R1,RPRBKDOR         DISPLAY ORDER NUMBER                         
         BCTR  R1,0                                                             
         LR    RE,R1                                                            
         MH    R1,=Y(L'SAVBK)                                                   
         LA    R1,SAVBKS(R1)                                                    
         USING BOOKLIN,R1                                                       
*                                                                               
         CLI   RPRBKLEN,RPRBKOVQ   USER DEFINED BOOK?                           
         BH    RDBDBK20            YES                                          
*********                                                                       
* REGULAR BOOK                                                                  
*********                                                                       
         MVC   BKLNIORD,RPRBKIOR   INTERNAL ORDER #                             
         MVC   BKLNFLG,RPRBKFLG    FLAGS                                        
         MVC   BKLNBK,RPRBKSTT     BOOK                                         
         MVC   BKLNSPBK,RPRBKBKT   SPECIAL BOOK TYPE                            
         MVC   BKLNFIL,RPRBKFIL    BOOK SOURCE(I/T/P/4)                         
         B     RDBDBK50                                                         
*********                                                                       
* USER-DEFINED BOOK                                                             
*********                                                                       
RDBDBK20 DS    0H                                                               
         MVC   BKLNIORD,RPRBKIOR   INTERNAL ORDER #                             
         MVC   BKLNFLG,RPRBKFLG    FLAGS                                        
         MVC   BKLNBK,RPRBKSTT     BOOK                                         
         MVC   BKLNSPBK,RPRBKBKT   SPECIAL BOOK TYPE                            
         MVC   BKLNFIL,RPRBKFIL    BOOK SOURCE(I/T/P/4)                         
         MVC   BKLNUPGD,RPRBKBKS   UPGRADE FORMULA                              
         MVC   BKLNXBKS,RPRBKXBK   EXTRA BASE BOOKS                             
         DROP  R1                                                               
*                                                                               
         MH    RE,=Y(L'SAVLBL)                                                  
         LA    RE,SAVLBLS(RE)                                                   
         MVC   0(L'SAVLBL,RE),RPRBKUDF  SAVE THE LABEL                          
*                                                                               
RDBDBK50 BAS   RE,MINIOSEQ                                                      
         BE    RDBDBK10                                                         
*                                                                               
RDBDBKX  LR    RE,RA               SAVE THESE SO WE KNOW WHAT CHANGED           
         AH    RE,=Y(MINBKS-TWAD)                                               
         MVC   0(L'MINBKS,RE),SAVBKS                                            
         LR    RE,RA                                                            
         AH    RE,=Y(MINLBLS-TWAD)                                              
         MVC   0(L'MINLBLS,RE),SAVLBLS                                          
***************                                                                 
* STATION ELEMENT(S)                                                            
***************                                                                 
RDBDST00 XC    SAVSTAS,SAVSTAS                                                  
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRSTELQ    GET THE STATION ELEMENT                      
         BAS   RE,MINIOHI                                                       
         BNE   RDBDSTX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
*                                                                               
RDBDST10 CLI   0(R6),RPRSTELQ                                                   
         BNE   RDBDSTX                                                          
*                                                                               
         ZIC   R1,RPRSTICD                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
         MVC   0(L'SAVSTA,R1),RPRSTSTA                                          
*                                                                               
         CLI   0(R1),C' '          NEED 'T' SET?                                
         BNE   *+8                 NO                                           
         MVI   0(R1),C'T'                                                       
         TM    RPRSTFLG,RPRSTSTL   SATELLITE STATION?                           
         BZ    RDBDST20                                                         
         MVI   4(R1),C'1'          YES, C'1' AFTER STATION CALL LTRS            
*                                                                               
RDBDST20 BAS   RE,MINIOSEQ                                                      
         BE    RDBDST10                                                         
*                                                                               
RDBDSTX  LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD)                                              
         MVC   0(L'MINSTAS,RE),SAVSTAS                                          
         DROP  R6                                                               
*                                                                               
RDBKDMX  B     EXITOK                                                           
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BEFORE DISPLAYING THE DATA FIELDS                                             
***********************************************************************         
D1STDDIS DS    0H                                                               
         NMOD1 0,**DFDD**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LR    R1,RA                                                            
         AH    R1,=Y(SVFLADDR-TWAD)                                             
         CLC   SVPARMS4,ATLST                                                   
         BE    *+10                                                             
         XC    0(L'SVFLADDR,R1),0(R1)                                           
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         OC    0(L'SVCONNUM,RE),0(RE)    ANY CONTRACT NUMBER?                   
         BZ    DFDDISX                                                          
         CLI   SVPRONUM-SVCONNUM(RE),0    ANY PROPOSAL NUMBER?                  
         BZ    DFDDISX                                                          
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
***************                                                                 
* DESCRIPTION ELEMENT                                                           
***************                                                                 
DFDDIS20 DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINSLNS-TWAD)                                              
         XC    0(L'MINSLNS,RE),0(RE)                                            
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDSELQ    GET THE DESCRIPTION ELEMENT                  
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                BETTER HAVE ONE                              
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDSELD,R6                                                      
         LR    RE,RA                                                            
         AH    RE,=Y(MINSLNS-TWAD)                                              
         MVC   0(L'MINSLNS,RE),RPRDSSEC    COPY THE SPOT LENGTHS                
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(RDBKSDMS),BODMCB,(R9),RR=BORELO                               
*                                                                               
DFDDISX  B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
LISTABL  DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LDEFCLM),AL1(0,0,1),AL4(DEFCLM1)                             
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC)                              
         DC    AL1(EOT)                                                         
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* KEY (UNPROTECTED PORTION)                                                     
         DC    AL2(00001),AL4(CONDTA)    CONTRACT                               
         DC    AL2(00002),AL4(PRODTA)    PROPOSAL                               
* KEY (PROTECTED PORTION)                                                       
         DC    AL2(00003),AL4(AGYDTA)    AGENCY                                 
         DC    AL2(00004),AL4(ADVDTA)    ADVERTISER                             
         DC    AL2(00005),AL4(PRDDTA)    PRODUCT                                
         DC    AL2(00006),AL4(SALDTA)    SALESPERSON                            
         DC    AL2(00007),AL4(DVSDTA)    DEVELOPMENT SALESPERSON                
         DC    AL2(00008),AL4(DVTDTA)    DEVELOPMENT CONTRACT TYPE              
         DC    AL2(00009),AL4(STADTA)    STATION                                
         DC    AL2(00010),AL4(BYRDTA)    BUYER                                  
         DC    AL2(00011),AL4(FLTDTA)    FLIGHT DATES                           
         DC    AL2(00012),AL4(DSKDTA)    DISK ADDRESS                           
* RECORD LIST                                                                   
         DC    AL2(00019),AL4(LTAGDTA)   TAG                                    
         DC    AL2(00013),AL4(LLBLDTA)   LABEL                                  
         DC    AL2(00014),AL4(LBKDTA)    BOOK                                   
         DC    AL2(00015),AL4(LRTCDTA)   RATE CARD                              
         DC    AL2(00016),AL4(LLENDTA)   LENGTH                                 
         DC    AL2(00017),AL4(LQTRDTA)   QUARTER                                
         DC    AL2(00018),AL4(LYRDTA)    YEAR                                   
         DC    AL2(00027),AL4(LPCDTA)    % COST FIELD                           
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 FIELD NUMBER                                 
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
AVDIC    DS    A                                                                
ADDIC    DS    A                                                                
AFRREL   DS    A                                                                
AFVADDR  DS    A                                                                
ESCLEN   DS    XL1                                                              
ESCCHAR  DS    XL2                                                              
*                                                                               
BASEPERC DS    XL4                 LIST OF CHANGED BASE %'S                     
*                                                                               
CHGRATCS DS    0XL(4*RFTCRTSL)    RATE CARD                                     
CHGRATC  DS    (4)XL(RFTCRTSL)                                                  
*                                                                               
PCKOF16B DS    PL16                                                             
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS, SET #1                  
MF1KYCHG EQU   X'80'                - KEY FIELD CHANGED                         
MF1PFRET EQU   X'40'                - RETURNING FROM CALLED SESSION             
MF1VWCHG EQU   X'20'                - VIEW WAS CHANGED                          
MF1RTYR  EQU   X'10'                - MATCHED YEAR ONCE                         
MF1RTLEN EQU   X'08'                - MATCHED LENGTH ONCE IMPLIES YEAR          
MF1NWRT  EQU   X'04'                - NEED MINIOWRT                             
MF1GLOBR EQU   X'02'                - CAME BACK FROM GLOBBER                    
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
SAVSTAS  DS    0CL(NUMSTAS*5)            SAVED 5-BYTE CALL LETTERS              
SAVSTA   DS    (NUMSTAS)CL5                                                     
*                                                                               
SAVBKS   DS    0XL(7*(BKLNLENQ))                                                
SAVBK    DS    7XL(BKLNLENQ)                                                    
*                                                                               
SAVLBLS  DS    0CL(7*5)            SAVED LABELS FOR USER DEFINED BOOKS          
SAVLBL   DS    7CL5                 - NULL: EMPTY OR LABEL                      
*                                                                               
SVMINEKY DS    XL(L'RPROKMEL)                                                   
SVRECDA  DS    XL(L'GSRECDA)                                                    
*                                                                               
         EJECT                                                                  
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
***********************************                                             
* SAVE AREA EXCEPT BETWEEN NTRSES                                               
***********************************                                             
MINSTAS  DS    0XL(NUMSTAS*5)             - 5-BYTE CALL LETTERS                 
MINSTA   DS    (NUMSTAS)XL5                                                     
*                                                                               
MINBKS   DS    0XL(7*(BKLNLENQ))                                                
MINBK    DS    7XL(BKLNLENQ)                                                    
*                                                                               
MINLBLS  DS    0CL(7*5)            LABELS FOR USER DEFINED BKS (MINIO)          
MINLBL   DS    7CL5                 - NULL: EMPTY OR LABEL                      
*                                                                               
MINSLNS  DS    0CL(6*1)            SAVED 1-BYTE SPOT LENGTHS                    
MINSLN   DS    6XL1                                                             
*                                                                               
         DS    0A                                                               
SVFLADDR DS    0XL(4*(4*4))        SAVED FIELD ADDRESSES                        
SVFARTCD DS    A                   RATE CARD FIELD                              
SVFALEN  DS    A                   LENGTH FIELD                                 
SVFAQTR  DS    A                   QUARTER                                      
SVFAYR   DS    A                   YEAR                                         
         DS    3XL(4*4)                                                         
*                                                                               
         DS    XL(TWUSER+L'TWUSER-*)   # OF SPARE BEFORE TWSECBLK               
         DS    0X                                                               
         EJECT                                                                  
BOOKLIN  DSECT                     FOR SAVBK ENTRIES - MATCH RFTC LYOUT         
BKLNIORD DS    X                   INTERNAL ORDER #                             
BKLNBK   DS    CL3                 BOOK(BASE BOOK FOR UPGRD)                    
BKLNFIL  DS    C                   BOOK SOURCE(I/T/P/4)                         
BKLNSPBK DS    C                   SPECIAL BOOK SOURCE(O/H/B/M..)               
BKLNXBKS DS    XL(3*2)             EXTRA BASE BOKS FOR UPGD                     
BKLNUPGD DS    XL12                UPGRADE EXPRESSION                           
BKLNFLG  DS    XL1                 FLAGS                                        
BKLNLENQ EQU   *-BOOKLIN                                                        
         EJECT                                                                  
* FAFACTS                                                                       
* FASYSLSTD                                                                     
* DDDDEQUS                                                                      
* CTMSGEQUS                                                                     
* FASELIST                                                                      
* FASYSFAC                                                                      
* DDSCANBLKD                                                                    
* DDFLDHDR                                                                      
* DDCOMFACS                                                                     
* DDGLVXCTLD                                                                    
* DDGLOBEQUS                                                                    
* REGENARTE                                                                     
* REFETCHD                                                                      
         PRINT OFF                                                              
*********INCLUDE FAFACTS                                                        
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE DDDDEQUS                                                       
       ++INCLUDE CTMSGEQUS                                                      
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE REGENARTEA                                                     
       ++INCLUDE REFETCHD                                                       
         PRINT ON                                                               
         EJECT                                                                  
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKPG    DS    XL1                 PAGE #                                       
TLKEYD   DS    0X                                                               
         ORG   TLUSER                                                           
TLRECD   DS    0X                                                               
TLRBK    DS    XL1                 INTERNAL BOOK ORDER #                        
TLRSLN   DS    XL1                 LENGTH                                       
TLRQTR   DS    XL1                 QUARTER                                      
TLRYR    DS    XL1                 YEAR                                         
TLRRTCX  DS    0CL8                RATE CARD                                    
TLRRTCD  DS    CL4                 RATE CARD 1ST HALF                           
TLRRTC2  DS    CL4                 RATE CARD 2ND HALF                           
TLRPC    DS    XL1                 % OF BASE COST                               
TLRLBL   DS    CL18                LABEL                                        
TLRMLN   EQU   *-TLRECD                                                         
*                                                                               
TSARLNQ  EQU   *-TLSTD             LENGTH OF TSAR RECORD                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028REPRO17   11/02/98'                                      
         END                                                                    
