*          DATA SET ACBAT61    AT LEVEL 054 AS OF 06/05/13                      
*PHASE T61B61C                                                                  
BAT61    TITLE '- BATCH PROGRAM ITEM RECORD HANDLING'                           
BAT61    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BA61**                                                       
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA                                                          
         USING BSVALS,R5                                                        
         ST    RB,BCRELO                                                        
         BASR  R8,0                                                             
         AHI   R8,GLOBALS-*                                                     
         USING GLOBALS,R8                                                       
         L     RC,AOVERWRK                                                      
         L     R1,AMIXNTRY                                                      
         SRL   RF,32-8                                                          
         LTR   RF,RF                                                            
         BNZ   ITENTRY                                                          
         ICM   RF,1,MIXROUT-MIXTABD(R1)                                         
         CHI   RF,ITEROUTM                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ITEROUTS(RF)                                                     
*                                                                               
ITEROUTS DS    0XL4                                                             
         J     ITEINP                                                           
         J     ITEDSP                                                           
         J     ITELST                                                           
         J     ITESEL                                                           
         J     ITECOP                                                           
         J     ITEREV                                                           
         J     ITECHA                                                           
         J     ITEDEL                                                           
         J     ITEGEN                                                           
         J     SCRSEL                                                           
         J     BATGLU                                                           
ITEROUTM EQU   (*-ITEROUTS)/L'ITEROUTS                                          
         EJECT                                                                  
***********************************************************************         
* RETURN TO POINT AFTER NTRSES                                        *         
***********************************************************************         
         SPACE 1                                                                
ITENTRY  BCTR  RF,0                                                             
         CHI   RF,ITENTRYM                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ITENTRYS(RF)                                                     
*                                                                               
ITENTRYS DS    0XL4                                                             
ITELST#1 EQU   1                                                                
ITECOP#1 EQU   5                                                                
ITECOP#2 EQU   6                                                                
ITEGEN#1 EQU   7                                                                
ITEGEN#2 EQU   8                                                                
ITEINP#1 EQU   9                                                                
         J     ITELSTR1                                                         
         J     ITESELY                                                          
         J     ITESELN                                                          
         J     ITESELC                                                          
         J     ITECOPR1                                                         
         J     ITECOPR2                                                         
         J     ITEGENR1                                                         
         J     ITEGENR2                                                         
         J     ITEINPR1                                                         
         J     ITEACTR1                                                         
ITENTRYM EQU   (*-ITENTRYS)/L'ITENTRYS                                          
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RE-BUILD BATCH DETAILS ON QUIT                           *         
***********************************************************************         
         SPACE 1                                                                
ITEACTR1 GOTOR ABLDDET,0           REFRESH BATCH DETAILS                        
         MVC   FVMSGNO,=AL2(AI$PSRES)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE LINE SELECTION FOR ITEM/SELECT                             *         
* NTRY - R2=A(DISPLAY LINE HEADER)                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING LSTTABD,R3                                                       
         USING ILIACT1H,R2                                                      
ITESELY  BASE  ,                                                                
         LA    R2,ILIITEMH                                                      
         USING ILLINED,R2                                                       
         LA    R3,CSLSTCUR                                                      
         OC    LSTIREF2,LSTIREF2                                                
         BZ    *+12                                                             
         TM    ILLHDR2+(FVIIND-FVIHDR),FVITHIS                                  
         BZ    ITESELY2                                                         
         GOTOR AFVAL,ILLHDR2                                                    
         BE    ITESELY1                                                         
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         JL    EXIT                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         J     EXIT                                                             
ITESELY1 MVC   LSTIREF2,FVIFLD                                                  
ITESELY2 OC    LSTIDAT2,LSTIDAT2                                                
         BZ    *+12                                                             
         TM    ILLHDR3+(FVIIND-FVIHDR),FVITHIS                                  
         BZ    ITESELY4                                                         
         GOTOR AFVAL,ILLHDR3                                                    
         BE    ITESELY3                                                         
         LA    R1,ILLHDR3                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         J     EXIT                                                             
ITESELY3 GOTOR AVALDAT,ILLHDR3                                                  
         JNE   EXIT                                                             
         MVC   LSTIDAT2,BCWORK+2                                                
ITESELY4 OI    LSTIIND1,LSTI1SEL                                                
         SR    R1,R1                                                            
         ICM   R1,3,TWASELCT                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,TWASELCT                                                    
         J     EXIT                                                             
         DROP  R2,RB                                                            
         SPACE 2                                                                
         USING ILIACT1H,R2                                                      
ITESELN  LA    R2,ILIITEMH                                                      
         USING ILLINED,R2                                                       
         LA    R3,CSLSTCUR                                                      
         NI    LSTIIND1,FF-LSTI1SEL                                             
         SR    R1,R1                                                            
         ICM   R1,3,TWASELCT                                                    
         SH    R1,=H'1'                                                         
         JNM   *+6                                                              
         DC    H'0'                                                             
         STCM  R1,3,TWASELCT                                                    
         J     EXIT                                                             
         SPACE 2                                                                
         USING ILIACT1H,R2                                                      
ITESELC  LA    R2,ILIITEMH                                                      
         USING ILLINED,R2                                                       
         LA    R3,CSLSTCUR                                                      
         TM    ILLHDR2+(FVIIND-FVIHDR),FVITHIS                                  
         JNZ   ITESELC1                                                         
         TM    ILLHDR3+(FVIIND-FVIHDR),FVITHIS                                  
         JNZ   ITESELC1                                                         
         LA    R1,ILLHDR2                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         J     EXIT                                                             
ITESELC1 GOTOR AFVAL,ILLHDR2                                                    
         JE    ITESELC2                                                         
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         JL    EXIT                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         J     EXIT                                                             
ITESELC2 MVC   LSTIREF2,FVIFLD                                                  
         CLI   ILLHDR3+(FVILEN-FVIHDR),0                                        
         JNE   ITESELC3                                                         
         LA    R1,ILLHDR3                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         J     EXIT                                                             
ITESELC3 GOTOR AVALDAT,ILLHDR3                                                  
         JNE   EXIT                                                             
         MVC   LSTIDAT2,BCWORK+2                                                
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ADD A BATCH ITEM                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING IIWORKD,RC                                                       
ITEINP   BASE  ,                                                                
         TM    CSOIND1,CSOIPLII    TEST PROCESS LIST FOR ITEM/INPUT             
         BO    *+12                                                             
         TM    CSINDSG1,CSINDBHD   TEST BATCH HEADER OPENED                     
         BZ    ITEINP68                                                         
         LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2                                                       
*                                                                               
         MVC   IIIOKEY,IOKEY       READ DIRECTORY TO CHECK BATCH IS OK          
         GOTOR ABLDBAK,LSTTABD                                                  
         GOTOR AIO,IORDD+IOACCDIR+IO1                                           
         BE    *+14                                                             
         TM    IOERR,IOEDEL        DELETED IS PERMISSABLE ERROR                 
         BO    *+6                                                              
         DC    H'0'                                                             
         TM    IOKEY+(TBAKHSTA-TBARECD),FF-(TBAHSIIP+TBAHSIAD)                  
         BNZ   ITEINP68            INVALID BATCH STATUS                         
         MVC   IOKEY,IIIOKEY       RESTORE IOKEY                                
*                                                                               
         TM    LSTTSTAT,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BNZ   ITEINP02            SKIP ITEM/CASH CONTROLS CHECK                
         SR    R0,R0                                                            
         ICM   R0,3,LSTBITMA                                                    
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R0,BOHALF1          R0=NUMBER OF ITEMS IN BATCH                  
         CLM   R0,3,CSBMAXIT       TEST MAXIMUM ITEMS REACHED                   
         BL    *+12                                                             
         MVI   BOBYTE1,X'44'                                                    
         B     ITEINP54                                                         
ITEINP02 TM    CSOIND1,CSOIPLII    TEST PROCESS LIST FOR ITEM/INPUT             
         BZ    ITEINP40                                                         
         MVI   CSSUBACT,1          SET SUB-ACTION                               
         TM    CSOIND1,CSOIFRST    TEST FIRST FOR CSOIPLII                      
         BZ    ITEINP04                                                         
         NI    CSOIND1,FF-(CSOIFRST)                                            
         OI    CSINDSG1,CSINDBHD   SET BATCH HEADER OPENED                      
         MVC   CSSQRECN,CSHIRECN                                                
         MVC   CSHIRECN,CSNXRECN   PRESERVE SCREEN/SELECT LIST                  
ITEINP04 TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BO    ITEINP40                                                         
         SR    R1,R1               BUMP TSAR RECORD NUMBER                      
         ICM   R1,3,CSSQRECN                                                    
         LA    R1,1(R1)                                                         
         CLM   R1,3,CSHIRECN       TEST ALL ITEM RECORDS PROCESSED              
         BH    ITEINP64                                                         
         STCM  R1,3,CSSQRECN                                                    
         MVC   BCITECUR+(LSTTRECN-LSTTABD),CSSQRECN                             
         GOTOR ATSARIO,BOPARM,('TSAGET',BCITECUR)                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   BCITECUR+(LSTTRTYP-LSTTABD),RECITE                               
         BE    *+6                                                              
         DC    H'0'                NOT AN ITEM RECORD                           
         TM    BCITECUR+(LSTIIND1-LSTTABD),LSTI1SEL                             
         BZ    ITEINP04            NO - GET NEXT RECORD                         
         NI    BCITECUR+(LSTIIND1-LSTTABD),FF-(LSTI1SEL)                        
         GOTOR ATSARIO,BOPARM,('TSAPUT',BCITECUR)                               
         MVC   IODAOVER,BCITECUR+(LSTTDA-LSTTABD)                               
         GOTOR AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         L     R2,AIO1                                                          
         USING TBARECD,R2                                                       
         MVC   BCBYTE1,CSBITS      FIGURE OUT WHAT SCREEN TO LOAD               
         CLI   TBARESCR,0                                                       
         BE    *+10                                                             
         MVC   BCBYTE1,TBARESCR                                                 
         CLI   BCBYTE1,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   TWASCRN,0           SET VALUE TO SET CLEAR BEFORE/AFTER          
         GOTOR AOVRSCR,BCPARM,(BCBYTE1,BASOLY2H)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CSSPROG,TBARESPG    RESTORE SUB-PROGRAM                          
*                                                                               
         GOTOR ARESFLD,BCPARM,TBARFST,BASOLY2H                                  
         MVC   FVADDR,BCFULL       RESFLD RETURNS A(FIELD FOR CURSOR)           
*                                                                               
         TM    BCBATCUR+(LSTBIND2-LSTTABD),LSTBIMLT                             
         BZ    ITEINP36                                                         
         GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         L     R2,AIO1                                                          
         USING TBARECD,R2                                                       
         B     ITEINP22                                                         
*                                                                               
ITEINP20 SR    RF,RF               READ NEXT ITEM RECORD                        
         ICM   RF,3,TBAKTSEQ                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TBAKTSEQ                                                    
         MVC   IOKEY(L'TBAKEY),TBAKEY                                           
         GOTOR AIO,IOREAD+IOACCDIR+IO1                                          
         BNE   ITEINP36                                                         
         GOTOR AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ITEINP22 LA    R1,TBARFST          LOCATE BATCH ITEM AMOUNT ELEMENT             
         SR    R0,R0                                                            
         USING BIAELD,R1                                                        
ITEINP24 CLI   BIAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BIAEL,BIAELQ                                                     
         BE    *+14                                                             
         IC    R0,BIALN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     ITEINP24                                                         
         CLI   BIALN,BIALN2Q       TEST IF SAME LOGICAL SCREEN                  
         BNE   ITEINP36                                                         
         CLC   BIASNO,BCITECUR+(LSTISNO-LSTTABD)                                
         BNE   ITEINP36                                                         
         TM    TBARESTA,TBAESLDE   TEST THIS ITEM IS DELETED                    
         BZ    ITEINP20                                                         
         MVC   CSOLINE,BIASIS      SET LINE NUMBER FOR APPLICATION              
         MVI   CSOMODE,CSOMPLIN    CALL APPLICATION TO "ERASE" LINE             
         GOTOR APHOOK,0                                                         
         MVI   CSOMODE,0           CLEAR APPLICATION MODE                       
         B     ITEINP20                                                         
         DROP  R1                                                               
*                                                                               
ITEINP36 TM    CSBIND2,TYPIOSC     TEST OVERLAY PREPARES SCREEN                 
         BZ    ITEINP38                                                         
         GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         GOTOR BONTRYA,BCPARM,('TYPIOSC',TWAD),WORKD                            
*                                                                               
ITEINP38 OI    CSINDSL1,CSIRDSPC   SET RECORD DISPLAYED FOR CHANGE              
         MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         GOTOR ABLDDET,0                                                        
         J     SETCUR                                                           
*                                                                               
ITEINP40 LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2                                                       
         CLI   BCPFKEY,PFK12       TEST 'NEXT' ITEM FOR LIST                    
         BNE   *+16                                                             
         TM    CSOIND1,CSOIOVRS    TEST OVERLAY CALLED SUB SCREEN               
         BO    ITEINP42            AND NOW WANTS TO RECALL MAIN SCREEN          
         B     ITEINP60                                                         
         GOTOR ATSTINP,BCPARM,BASOLY2H,0                                        
         BE    ITEINP42                                                         
         TM    CSINDSL2,CSIACFRM   TEST APPLICATION AWAITS CONFIRMATION         
         BZ    *+12                                                             
         NI    CSINDSL2,FF-(CSIACFRM)                                           
         B     ITEINP42                                                         
         CLI   BCPFKEY,0           TEST PFKEY TO PASS                           
         BE    ITEINP60                                                         
*                                                                               
ITEINP42 XC    BASMSG,BASMSG       CLEAR MESSAGE FIELD                          
         GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         NI    BCINDS1,FF-(BCIFRST)                                             
         MVI   CSOLINE,0           RESET THIS TIME LINE NUMBER                  
         GOTOR APHOOK                                                           
         LA    R0,1                                                             
         BNE   *+6                                                              
         SR    R0,R0                                                            
         GOTOR ABLDDET,0                                                        
         LTR   R0,R0                                                            
         JNZ   EXIT                                                             
         NI    CSINDSL1,FF-(CSIRDSPC)                                           
         MVC   FVMSGNO,=AL2(AI$IOKNX)                                           
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
*                                                                               
         LA    R1,BASOLY2H         TRANSMIT UNPROTECTED FIELDS                  
         SR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
ITEINP44 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    ITEINP52                                                         
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   ITEINP46                                                         
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         NI    FVIIND-FVIHDR(R1),FF-FVITHIS                                     
         CLI   BCBP01,C'Y'         TEST OPTION TO CLEAR AMOUNTS SET             
         BNE   ITEINP46                                                         
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    ITEINP46                                                         
         LA    R3,0(RE,R1)                                                      
         SHI   R3,L'FVIXHDR        R3=A(EXTENDED FIELD HEADER)                  
         CLI   FVIXNU-FVIXHDR(R3),1                                             
         BL    ITEINP46                                                         
         CLI   FVIXNU-FVIXHDR(R3),10                                            
         BH    ITEINP46                                                         
         LR    R3,RE                                                            
         SHI   R3,L'FVIHDR+L'FVIXHDR+1                                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R1),L'FVIHDR(R1)                                      
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
*                                                                               
ITEINP46 BXLE  R1,RE,ITEINP44                                                   
*                                                                               
ITEINP52 TM    LSTTSTAT,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BNZ   ITEINP58            SKIP ITEM/CASH CONTROLS CHECK                
         SR    R0,R0                                                            
         ICM   R0,3,LSTBITMA                                                    
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R0,BOHALF1          R0=NUMBER OF ITEMS IN BATCH                  
         CLM   R0,3,CSBMAXIT       TEST MAXIMUM ITEMS REACHED                   
         BL    *+12                                                             
         MVI   BOBYTE1,X'44'                                                    
         B     ITEINP54                                                         
         GOTOR ATSTBTY,=AL1(ACTACL)                                             
         MVC   FVMSGNO,=AL2(AI$IOKNX)                                           
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         BNE   ITEINP58                                                         
         TM    CSBIND1,TYPICUMU    TEST TYPE ACCUMULATES TOTAL DR/CR            
         BZ    *+14                                                             
         CP    LSTBTDRS,LSTBTCRS   TEST DEBITS EQUAL CREDITS                    
         BNE   ITEINP58                                                         
         TM    LSTBINDS,LSTBINFY   TEST USER NOTIFIED                           
         BNZ   ITEINP58                                                         
         MVI   BOBYTE1,0                                                        
         LA    R1,BNHQ             BRANCH NOT HIGH                              
         CP    LSTBCSHC,BCPZERO    TEST NEGATIVE BATCH                          
         BNL   *+8                                                              
         LA    R1,BNLQ             BRANCH NOT LOW                               
         CP    LSTBCSHA,LSTBCSHC   TEST CASH CONTROLS EXCEEDED                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         NOP   *+8                                                              
         OI    BOBYTE1,X'80'                                                    
         BNE   *+8                                                              
         OI    BOBYTE1,X'40'                                                    
         CLM   R0,3,LSTBITMC       TEST ITEM COUNT EXCEEDED                     
         BNH   *+8                                                              
         OI    BOBYTE1,X'08'                                                    
         BNE   *+8                                                              
         OI    BOBYTE1,X'04'                                                    
*                                                                               
ITEINP54 LA    RE,LSTBINFY+LSTBIAUT                                             
         LA    R1,=AL1(RECBAT,ACTCLO,ITEINP#1,0,0,0)                            
         TM    BOBYTE1,X'44'       TEST CASH/ITEMS REACHED                      
         BNZ   ITEINP56                                                         
         LA    RE,LSTBINFY                                                      
         LA    R1,=AL1(RECBAT,ACTDSP,ITEINP#1,PFKQUITQ,0,0)                     
         TM    BOBYTE1,X'88'       TEST CASH/ITEMS EXCEEDED                     
         BZ    ITEINP58                                                         
*                                                                               
ITEINP56 EX    RE,*+8                                                           
         B     *+8                                                              
         OI    LSTBINDS,0                                                       
         GOTOR ANTRSES,(R1)                                                     
*                                                                               
ITEINPR1 BASR  RB,0                RETURN FROM CLOSE/DISPLAY SESSION            
         AHI   RB,ITEINP-*                                                      
         MVC   FVMSGNO,=AL2(AI$PSRES)                                           
         MVI   FVOMTYP,GTMINF                                                   
*                                                                               
ITEINP58 TM    CSOIND1,CSOIPLII    TEST PROCESS LIST FOR ITEM/INPUT             
         JZ    SETCUR                                                           
         CLC   CSSQRECN,CSHIRECN   TEST ANY LIST RECORD TO PROCESS              
         BL    ITEINP04                                                         
         B     ITEINP64                                                         
*                                                                               
ITEINP60 TM    CSOIND1,CSOIPLII    TEST PROCESS LIST FOR ITEM/INPUT             
         BZ    ITEINP62                                                         
         CLI   BCPFKEY,PFK12       TEST 'NEXT' FOR LIST                         
         BNE   ITEINP62                                                         
         MVI   BCPFKEY,0           CLEAR 'NEXT' FOR LIST                        
         CLC   CSSQRECN,CSHIRECN   TEST FURTHER RECORD(S) TO PROCESS            
         BNL   *+12                                                             
         NI    CSINDSL1,FF-(CSIRDSPC)                                           
         B     ITEINP04                                                         
         TM    CSOIND1,CSOIPLII    TEST LIST ALREADY EXHAUSTED                  
         BO    ITEINP64                                                         
*                                                                               
ITEINP62 MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         MVI   FVOMTYP,GTMERR      SET ERROR MESSAGE                            
         J     SETCUR                                                           
*                                                                               
ITEINP64 NI    CSOIND1,FF-(CSOIPLII)                                            
         MVI   CSSUBACT,0          RESET SUB-ACTION                             
         CLI   BCBP12,C'Y'         TEST CLEAR SCREEN AFTER GENERATE             
         BNE   ITEINP66                                                         
         MVC   BCBYTE1,TWASCRN     RE-LOAD CURRENT SCREEN                       
         MVI   TWASCRN,0           SET VALUE TO SET CLEAR BEFORE/AFTER          
         GOTOR AOVRSCR,BCPARM,(BCBYTE1,BASOLY2H)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CSBIND2,TYPIOSC     TEST OVERLAY PREPARES SCREEN                 
         BZ    ITEINP66                                                         
         GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         GOTOR BONTRYA,BCPARM,('TYPIOSC',TWAD),WORKD                            
*                                                                               
ITEINP66 MVC   FVMSGNO,=AL2(AI$NMISP)                                           
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         J     SETCUR                                                           
*                                                                               
ITEINP68 MVC   FVMSGNO,=AL2(AE$INACS)                                           
         LA    R0,BASACTH          SET CURSOR TO ACTION FIELD                   
         ST    R0,FVADDR                                                        
         J     EXIT                                                             
         DROP  RB,RC                                                            
         SPACE 1                                                                
IIWORKD  DSECT                                                                  
IIIOKEY  DS    XL(L'IOKEY)         SAVED/RESTORED IOKEY                         
BAT61    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY A BATCH ITEM                                                *         
***********************************************************************         
         SPACE 1                                                                
ITEDSP   BASE  ,                                                                
         LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2                                                       
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BNZ   ITEDSP04                                                         
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   ITEDSP02                                                         
         CLI   TWASCRN,HEADSCRN    TEST HEADER SCREEN LOADED                    
         BNE   *+12                                                             
         CLI   TWASCRF,1           AND FORMAT 1 (UNPROT KEY)                    
         BE    ITEDSP02                                                         
         GOTOR ADISHDR,1+X'80'     BUILD BATCH HEADER SCREEN                    
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         J     EXIT                                                             
*                                                                               
ITEDSP02 LA    R1,LSTTABD          VALIDATE BATCH KEY                           
         ICM   R1,8,BCEFFS         SET TO TEST BATCH TYPE SECURITY              
         GOTOR AGETBAT                                                          
         JNE   EXIT                                                             
         OC    LSTBITMA,LSTBITMA   TEST AT LEAST ONE ITEM                       
         JZ    EXIT                                                             
         CLI   LSTBBTYP,60         TYPE 60?                                     
         BNE   *+18                                                             
         MVC   FVMSGNO,=AL2(AE$USEAL)    USE ACTION LIST                        
         MVI   FVOMTYP,GTMERR      SET ERROR MESSAGE                            
         J     EXIT                                                             
*                                                                               
         GOTOR ADISHDR,1+X'40'     DISPLAY BATCH RECORD                         
         GOTOR ANTRSES,0           SAVE CURRENT SESSION                         
         OI    CSINDSL1,CSIUSELC   SET NESTED CALL                              
         OI    IDIND1,IDI1NATV     SET NATIVE CALL                              
*                                                                               
ITEDSP04 CLI   TWASCRN,HEADSCRN    TEST HEADER SCREEN                           
         BE    ITEDSP06                                                         
         CLI   TWASCRN,ILSTSCRN    OR ITEM LIST SCREEN                          
         BE    ITEDSP06                                                         
         TM    IDIND1,IDI1NATV     NO - TEST NATIVE CALL                        
         BO    ITEDSP08                                                         
         CLI   CSOMODE,CSOMSSCR       - TEST SUB-SCREEN ACTIVE                  
         BE    ITEDSP31                                                         
         B     ITEDSP36                                                         
*                                                                               
ITEDSP06 GOTOR AOVRSCR,BCPARM,('DETLSCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOR ABLDBAK,LSTTABD                                                  
         MVC   IDTBASAV,IOKEY      SAVE BATCH RECORD KEY                        
         MVC   IDTBASIT,LSTBITMA   SAVE NUMBER OF ITEMS                         
         XC    IDTBACUR,IDTBACUR   SET FOR FIRST BATCH ITEM                     
*                                                                               
ITEDSP08 MVC   IOKEY,IDTBASAV      SET SAVED KEY                                
         ICM   R1,3,BCITECUR+(LSTISEQ-LSTTABD)                                  
         TM    IDIND1,IDI1NATV     TEST NATIVE CALL                             
         BZ    ITEDSP10                                                         
         ICM   R1,3,IDTBACUR       TAKE CURRENT ITEM                            
         LA    R1,1(R1)                                                         
         CLM   R1,3,IDTBASIT       TEST LAST ITEM DISPLAYED                     
         BNH   *+8                                                              
         LA    R1,1                SET FOR FIRST                                
         STCM  R1,3,IDTBACUR       SET NEXT ITEM                                
         MVC   BCITECUR+(LSTISEQ-LSTTABD)(L'LSTISEQ),IDTBACUR                   
*                                                                               
ITEDSP10 STCM  R1,3,IOKEY+(TBAKTSEQ-TBARECD)                                    
         GOTOR AIO,IORDD+IOACCDIR+IO1                                           
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1                                                          
         USING TBARECD,R2                                                       
         MVC   BCBYTE1,CSBITS      FIGURE OUT WHAT SCREEN TO LOAD               
         CLI   TBARESCR,0                                                       
         BE    *+10                                                             
         MVC   BCBYTE1,TBARESCR                                                 
         CLI   BCBYTE1,0                                                        
         BE    ITEDSP36                                                         
         GOTOR AOVRSCR,BCPARM,(BCBYTE1,BASOLY2H)                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,BASOLY2H         TRANSMIT/PROTECT/SET FIELD INTENSITY         
         LA    RF,OSVALS-1                                                      
         SR    RE,RE                                                            
ITEDSP12 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    ITEDSP14                                                         
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         NI    FVATRB-FVIHDR(R1),FF-(FVAMODF)                                   
         TM    FVATRB-FVIHDR(R1),FVAZERO                                        
         BO    *+8                                                              
         NI    FVATRB-FVIHDR(R1),FF-FVAHIGH                                     
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BO    *+8                                                              
         OI    FVATRB-FVIHDR(R1),FVAHIGH+FVAPROT                                
         BXLE  R1,RE,ITEDSP12                                                   
*                                                                               
ITEDSP14 GOTOR ABLDDET,1           DISPLAY BATCH/ITEM DETAILS                   
         SR    R0,R0                                                            
         MVI   CSOLINE,0                                                        
         GOTOR ARESFLD,BCPARM,TBARFST,BASOLY2H                                  
         LA    R2,TBARFST                                                       
ITEDSP16 CLI   0(R2),0                                                          
         BE    ITEDSP28                                                         
         TM    CSBIND5,TYPIOVSS    TEST OVERLAY CALLS SUB-SCREEN                
         BO    ITEDSP18                                                         
         CLI   0(R2),BIAELQ        TEST BATCH ITEM/AMOUNT ELEMENT               
         BNE   ITEDSP26                                                         
         USING BIAELD,R2                                                        
         CLI   BIALN,BIALN2Q                                                    
         BL    ITEDSP26                                                         
         MVC   CSOLINE,BIASIS      SAVE ITEM SEQUENCE NUMBER                    
         B     ITEDSP28                                                         
ITEDSP18 CLI   0(R2),GINELQ        TEST GROUP INVOICE NUMBER ELEMENT            
         BNE   ITEDSP26                                                         
         USING GINELD,R2                                                        
         OC    GINHISN,GINHISN     ONLY SET IF DEBIT SCREEN USED                
         BZ    ITEDSP28                                                         
         MVC   CSOLINE,GINHISN+1                                                
         OC    CSSHIREC,CSSHIREC                                                
         BNZ   *+10                                                             
         MVC   CSSHIREC,GINHISN+1  HIGH RECORD NUMBER                           
         OC    CSSDELS,CSSDELS                                                  
         BNZ   *+10                                                             
         MVC   CSSDELS,GINDELS+1   DELETES                                      
         MVC   CSGIN,GININV        GROUP INVOICE NUMBER                         
         B     ITEDSP28                                                         
*                                                                               
ITEDSP26 IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     ITEDSP16                                                         
*                                                                               
ITEDSP28 TM    CSBIND2,TYPIOSC     TEST OVERLAY PREPARES SCREEN                 
         BZ    ITEDSP30                                                         
         GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         GOTOR BONTRYA,BCPARM,('TYPIOSC',TWAD),WORKD                            
*                                                                               
ITEDSP30 TM    CSBIND5,TYPIOVSS    TEST OVERLAY CAN CALL SUB-SCREEN             
         BZ    ITEDSP32                                                         
         CLI   CSOLINE,0                                                        
         BE    ITEDSP34                                                         
ITEDSP31 GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         GOTOR BONTRYA,BCPARM,('TYPIOVSS',TWAD),WORKD                           
         MVI   CSOMODE,CSOMSSCR    SET SUB-SCREEN ACTIVE                        
         CLI   FVMSGNO+1,X'FF'     TEST OVERLAY HAS SET MESSAGE                 
         JNE   EXIT                                                             
         B     ITEDSP34                                                         
*                                                                               
ITEDSP32 CLI   CSOLINE,0           TEST MULTIPLE SCREEN INPUT                   
         BE    ITEDSP34                                                         
         SR    RE,RE                                                            
         ICM   RE,1,TWASESNL       TAKE CURRENT NEST LEVEL                      
         BZ    ITEDSP33                                                         
         SLL   RE,1                TEST PREVIOUS SESSION SCREEN/SELECT          
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         CLC   0(L'TWASESRA,RE),=AL1(RECSCR,ACTSEL)                             
         BE    ITEDSP34                                                         
ITEDSP33 GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         MVI   CSOMODE,CSOMPCHA    PREPARE FOR ITEM/CHANGE                      
         GOTOR APHOOK                                                           
*                                                                               
ITEDSP34 LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2                                                       
         MVC   IDMSGNO,=AL2(AI$ITDSP)                                           
         TM    LSTBHDS1,BHDSCOPY                                                
         BZ    *+10                                                             
         MVC   IDMSGNO,=AL2(AI$COPID)                                           
         TM    LSTBHDS1,BHDSREVS                                                
         BZ    *+10                                                             
         MVC   IDMSGNO,=AL2(AI$REVID)                                           
         L     R1,AIO1                                                          
         TM    TBARESTA-TBARECD(R1),TBAESDEL+TBAESLDE                           
         BZ    *+10                                                             
         MVC   IDMSGNO,=AL2(AI$IDSPD)                                           
*                                                                               
ITEDSP36 MVC   FVMSGNO,IDMSGNO     MOVE OUT (SAVED) MESSAGE                     
         MVI   FVOMTYP,GTMINF                                                   
         LA    R0,BASACTH                                                       
         ST    R0,FVADDR                                                        
         J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* COPY/REVERSE ITEM RECORDS INTO A NEW BATCH                          *         
***********************************************************************         
         SPACE 1                                                                
         USING ICWORKD,RC                                                       
ITEREV   MVC   ICMASK,=AL2(LMITEREV)                                            
         J     ITECOP00                                                         
ITECOP   MVC   ICMASK,=AL2(LMITECOP)                                            
*                                                                               
ITECOP00 BASE  ,                                                                
         LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    *+8                                                              
         OI    CSINDSL1,CSIK1REQ+CSIK1VAL                                       
         TM    CSINDSL1,CSIK1REQ   TEST KEY 1 REQUESTED                         
         BNZ   ITECOP02                                                         
         GOTOR ADISHDR,1+X'80'     BUILD BATCH HEADER SCREEN                    
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIK1REQ                                                
         J     EXIT                                                             
*                                                                               
ITECOP02 TM    CSINDSL1,CSIK1VAL   TEST KEY 1 VALID                             
         BNZ   ITECOP04                                                         
         LA    R1,LSTTABD          VALIDATE BATCH KEY                           
         ICM   R1,8,BCEFFS         SET TO TEST BATCH TYPE SECURITY              
         GOTOR AGETBAT                                                          
         JNE   EXIT                                                             
         SR    R0,R0                                                            
         ICM   R0,3,LSTBITMA                                                    
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R0,BOHALF1                                                       
         BP    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BEMPT)                                           
         J     EXIT                                                             
         TM    LSTTSTAT,TBAHSIIP   TEST INPUT IN PROGRESS                       
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INPIP)                                           
         J     EXIT                                                             
         TM    LSTBINDS,LSTBBCBG   TEST TYPE CREATED BY ACBG                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IAFBT)                                           
         J     EXIT                                                             
         TM    LSTBINDS,LSTBACRV   TEST BATCH IS AN ACCRUAL REVERSAL            
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IAFBT)                                           
         J     EXIT                                                             
         MVC   BOWORK1(L'ICMASK),ICMASK                                         
         NC    BOWORK1(L'ICMASK),LSTTMASK                                       
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         J     EXIT                                                             
         OI    CSINDSL1,CSIK1VAL   SET KEY 1 IS VALID                           
         MVC   BCBATCUR,CSLSTCUR   SET CURRENT BATCH LIST RECORD                
*                                                                               
ITECOP04 TM    CSINDSL1,CSIK2REQ   TEST KEY 2 REQUESTED                         
         BNZ   ITECOP06                                                         
         GOTOR ADISHDR,5           BUILD BATCH HEADER 'FROM-TO' SCREEN          
         LA    R0,BATREF2H                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIK2REQ                                                
         J     EXIT                                                             
*                                                                               
ITECOP06 TM    CSINDSL1,CSIK2VAL   TEST KEY 2 VALID                             
         BZ    *+6                                                              
         DC    H'0'                YES - SHOULDN'T BE HERE                      
*                                                                               
         MVC   ICLSTSAV,CSLSTCUR   SAVE 'FROM' BATCH                            
*                                                                               
         MVI   FVMINL,1            BATCH REFERENCE                              
         GOTOR AVALBRF,BATREF2H                                                 
         BNE   ITECOPER                                                         
         MVI   FVMINL,1            BATCH NAME                                   
         GOTOR AVALBNA,BATNAM2H                                                 
         BNE   ITECOPER                                                         
         MVI   FVMINL,1            BATCH MONTH OF SERVICE                       
         GOTOR AVALBMO,BATMOA2H                                                 
         BNE   ITECOPER                                                         
*&&UK*&& GOTOR ACHKBMO                                                          
*&&UK*&& BNE   ITECOPER                                                         
         GOTOR AVALEFD,BATEFD2H    EFFECTIVE DATE                               
         BNE   ITECOPER                                                         
         GOTOR ATSTBMO,BATMOA2H    TEST BATCH MONTH LOCKED                      
         BNE   ITECOPER                                                         
         MVI   FVMINL,1            BATCH ITEM CONTROL                           
         GOTOR AVALITE,BATITC2H                                                 
         BNE   ITECOPER                                                         
         MVI   FVMINL,1            BATCH CASH CONTROL                           
         GOTOR AVALCSH,BATAMC2H                                                 
         BNE   ITECOPER                                                         
*                                                                               
         XC    LSTBDELI,LSTBDELI                                                
         MVI   LSTBHDS1,BHDSCOPY   SET THIS IS A COPY BATCH                     
         CLI   CSACT,ACTREV        TEST REVERSING ITEMS                         
         BNE   *+8                                                              
         MVI   LSTBHDS1,BHDSREVS   SET THIS IS A REVERSAL BATCH                 
         NI    LSTBHDS2,FF-(BHDSGENL+BHDSBDON)                                  
         GOTOR AFVAL,BATACR2H      VALIDATE ACCRUAL OPTION                      
         BE    ITECOP08                                                         
         BH    ITECOPER                                                         
         OC    CSBTYP2,CSBTYP2     TEST AUTOMATIC ACCRUAL                       
         BZ    ITECOP10                                                         
         TM    ICLSTSAV+(LSTBHDS1-LSTTABD),BHDSACRU                             
         BZ    ITECOP10            UNLESS ORIGINAL WAS NOT ACCRUED              
         OI    LSTBHDS1,BHDSACRU   SET THIS IS AN ACCRUAL BATCH                 
         B     ITECOP10                                                         
ITECOP08 SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8              TEST FOR 'NO'                                
         BE    ITECOP10                                                         
         CLC   BC@NO(0),FVIFLD     'NO' IS ALWAYS VALID                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   BC@YES(0),FVIFLD    TEST FOR 'YES'                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     ITECOPER                                                         
         TM    CSBIND1,TYPIACRL    TEST ACCRUAL VALID FOR BATCH TYPE            
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACRNV)                                           
         B     ITECOPER                                                         
         OI    LSTBHDS1,BHDSACRU   SET THIS IS AN ACCRUAL BATCH                 
*                                                                               
ITECOP10 GOTOR AADDHDR,BCPARM,BATREF2H,('FF',0)  TEST 'ADD' IS OK               
         BNE   ITECOPER            'TO' BATCH ALREADY EXISTS                    
*                                                                               
         OI    CSINDSL1,CSIK2VAL   SET KEY 2 IS VALID                           
         XC    CSLSTCUR,ICLSTSAV   RESTORE 'FROM' BATCH                         
         XC    ICLSTSAV,CSLSTCUR   AND SAVE 'TO' BATCH                          
         XC    CSLSTCUR,ICLSTSAV                                                
         TM    CSINDSL2,CSIRETRY   TEST RETRY AFTER ERROR                       
         BO    ITECOP22                                                         
*                                                                               
         XC    TWASELCT,TWASELCT   CLEAR SELECTED ITEM COUNT                    
         OI    CSINDSL1,CSIUSELC   SET NESTED CALL                              
         GOTOR ANTRSES,=AL1(RECITE,ACTSEL,ITECOP#1,0,0,0)                       
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* RETURN FROM ITEM/SELECT                                             *         
* NTRY - CSLSTCUR CONTAINS ENTRY FOR 'FROM' BATCH                     *         
*        ICLSTSAV CONTAINS ENTRY FOR 'TO' BATCH                       *         
***********************************************************************         
         SPACE 1                                                                
ITECOPR1 MVC   ICPFKEY,BCPFKEY     SAVE USER PFKEY                              
         CLI   ICPFKEY,PFKQUITQ    TEST QUITTING ITEM/SELECT                    
         JNE   ITECOP18                                                         
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     ITECOPX                                                          
*                                                                               
ITECOP18 OC    TWASELCT,TWASELCT   TEST ANY ITEMS SELECTED                      
         JNZ   ITECOP20                                                         
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         J     ITECOPX                                                          
*                                                                               
ITECOP20 XC    CSLSTCUR,ICLSTSAV   SET 'TO' BATCH IN CSLSTCUR                   
         XC    ICLSTSAV,CSLSTCUR   AND SAVE 'FROM' BATCH                        
         XC    CSLSTCUR,ICLSTSAV                                                
         GOTOR AADDHDR,BCPARM,('FF',BATREF2H),(2,BATCOM1H),ICLSTSAV             
         JE    ITECOP22                                                         
         NI    CSINDSL1,FF-(CSIK2VAL)                                           
         OI    CSINDSL2,CSIRETRY                                                
         J     ITECOPER                                                         
*                                                                               
ITECOP22 BASE  ,                                                                
         NI    CSINDSL2,FF-(CSIRETRY)                                           
         MVC   ICLSTCUR,CSLSTCUR   SAVE NEW BATCH DETAILS                       
         GOTOR AINIADT             INITIALISE ADDTRN CONTROL BLOCK              
         MVC   CSLSTCUR+(LSTTRECN-LSTTABD)(L'LSTTRECN),CSHIRECN                 
*                                                                               
         L     R3,ATIA                                                          
         USING ITENDXD,R3          BUILD INDEX OF SELECTED ITEM RECORDS         
         SR    R0,R0                                                            
         ICM   R0,3,TWASELCT                                                    
ITECOP24 SR    R1,R1               BUMP TSAR RECORD NUMBER                      
         ICM   R1,3,CSLSTCUR+(LSTTRECN-LSTTABD)                                 
         LA    R1,1(R1)                                                         
         CLM   R1,3,CSNXRECN       TEST ALL ITEM RECORDS PROCESSED              
         BH    ITECOP26                                                         
         STCM  R1,3,CSLSTCUR+(LSTTRECN-LSTTABD)                                 
         GOTOR ATSARIO,TSAGET      GET TSAR RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   CSLSTCUR+(LSTTRTYP-LSTTABD),RECITE                               
         BE    *+6                                                              
         DC    H'0'                NOT AN ITEM RECORD                           
         TM    CSLSTCUR+(LSTIIND1-LSTTABD),LSTI1SEL                             
         BZ    ITECOP24            NO - GET NEXT RECORD                         
         MVC   ITENDXSQ,CSLSTCUR+(LSTISEQ-LSTTABD)                              
         MVC   ITENDXDA,CSLSTCUR+(LSTTDA-LSTTABD)                               
         MVC   ITENDXRF,CSLSTCUR+(LSTIREF2-LSTTABD)                             
         MVC   ITENDXDT,CSLSTCUR+(LSTIDAT2-LSTTABD)                             
         MVI   ITENDXST,0                                                       
         LA    RE,CSLSTCUR+(LSTIDATE-LSTTABD)                                   
         LA    RF,CSLSTCUR+(LSTIDAT2-LSTTABD)                                   
         CLC   0(L'LSTIDATE,RE),0(RF)                                           
         BE    *+8                                                              
         OI    ITENDXST,ICBUFSRD                                                
         LA    RE,CSLSTCUR+(LSTIREF-LSTTABD)                                    
         LA    RF,CSLSTCUR+(LSTIREF2-LSTTABD)                                   
         CLC   0(L'LSTIREF,RE),0(RF)                                            
         BE    *+8                                                              
         OI    ITENDXST,ICBUFSRD                                                
         LA    R3,ITENDXL(R3)      BUMP TO NEXT INDEX ENTRY                     
         BCTR  R0,0                DECREMENT SELECTED RECORD COUNT              
         B     ITECOP24            GET NEXT RECORD                              
*                                                                               
ITECOP26 LTR   R0,R0               TEST ALL ITEMS PROCESSED                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTOR ATSARIO,TSASAV      SAVE CURRENT TSAR BUFFER TO DISK             
         L     R1,ATSABLK                                                       
         USING TSARD,R1                                                         
         MVI   TSRECI,0            INITIALISE FOR TRANSACTION BUFFER            
         MVI   TSINDS,TSINODSK+TSIKEYUP                                         
         MVI   TSIND2,0                                                         
         LA    R0,ICBUFREC                                                      
         ST    R0,TSAREC                                                        
         MVI   TSKEYL,ICKEYLEN                                                  
         MVC   TSRECL,=Y(ICBUFLEN)                                              
         MVI   TSACTN,TSAINI                                                    
         GOTOR VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSAADD       SET ACTION TO ADD                            
         DROP  R1                                                               
*                                                                               
         L     R3,ATIA             R3=A(ITEM INDEX)                             
         SR    R0,R0                                                            
         ICM   R0,3,TWASELCT       R0=NUMBER OF ITEMS SELECTED                  
ITECOP28 MVC   IODAOVER,ITENDXDA   SET DISK ADDRESS                             
         GOTOR AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO2             R4=A(BATCH ITEM RECORD)                      
         USING TBARECD,R4                                                       
         LA    R4,TBARFST                                                       
         USING ASKELD,R4           LOCATE ASKELS AND POST TO BUFFER             
ITECOP29 CLI   ASKEL,0             TEST EOR                                     
         BE    ITECOP34                                                         
*                                                                               
         CLI   ASKEL,GINELQ        TEST GROUP INVOICE NUMBER ELEMENT            
         BNE   ITECOP32                                                         
         GOTOR AGETGIN                                                          
         BNE   ITECOPX                                                          
         LA    RF,IOKEY            BUILD GROUP INVOICE PASSIVE KEY              
         USING GINPASD,RF                                                       
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,CUABIN                                                   
         MVC   GINPINV,GININV-GINELD(R4)                                        
         XC    ICBUFSEQ,ICBUFSEQ                                                
         LA    R1,IOHI+IOACCMST+IO2                                             
ITECOP30 GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
         CLC   IOKEY(GINPISN-GINPASD),IOKEYSAV                                  
         BNE   ITECOP34                                                         
         L     R1,AIO2                                                          
         USING TRNRECD,R1                                                       
         MVC   ICBUFTRN,TRNKEY     BUILD TRANSACTION BUFFER RECORD              
         MVC   ICBUFITE,ITENDXSQ                                                
         IC    RE,ICBUFSEQ                                                      
         LA    RE,1(RE)                                                         
         STC   RE,ICBUFSEQ                                                      
         MVC   ICBUFREF,ITENDXRF                                                
         MVC   ICBUFDAT,ITENDXDT                                                
         MVC   ICBUFGIN,CSGIN                                                   
         MVC   ICBUFSTA,ITENDXST                                                
         DROP  R1                                                               
         L     R1,ATSABLK                                                       
         GOTOR VTSAR,(R1)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOSQ+IOACCMST+IO2                                             
         B     ITECOP30                                                         
*                                                                               
ITECOP32 CLI   ASKEL,ASKELQ        TEST TRANSACTION KEY ELEMENT                 
         BNE   ITECOP33                                                         
         MVC   ICBUFTRN,ASKKEY     BUILD TRANSACTION BUFFER RECORD              
         MVC   ICBUFITE,ITENDXSQ                                                
         MVC   ICBUFSEQ,ASKSEQN                                                 
         MVC   ICBUFREF,ITENDXRF                                                
         MVC   ICBUFDAT,ITENDXDT                                                
         XC    ICBUFGIN,ICBUFGIN                                                
         MVC   ICBUFSTA,ITENDXST                                                
         L     R1,ATSABLK                                                       
         GOTOR VTSAR,(R1)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ITECOP33 SR    R1,R1               BUMP TO NEXT ELEMENT                         
         IC    R1,ASKLN                                                         
         AR    R4,R1                                                            
         B     ITECOP29                                                         
*                                                                               
ITECOP34 LA    R3,ITENDXL(R3)      BUMP TO NEXT ITEM INDEX ENTRY                
         BCT   R0,ITECOP28         DO FOR NUMBER OF SELECTED ITEMS              
*                                                                               
         L     R4,ATSABLK                                                       
         USING TSARD,R4                                                         
         XC    TSRNUM,TSRNUM       SET INITIAL RECORD NUMBER                    
         XC    ICLSTACT,ICLSTACT                                                
ITECOP35 NI    ICFLAG,FF-(ICFLAST) RESET LAST TIME FOR BUFFER                   
         LH    R1,TSRNUM           GET FIRST/NEXT BUFFER ENTRY                  
         LA    R1,1(R1)                                                         
         CLM   R1,3,TSPRECN        TEST ALL RECORDS READ                        
         BNH   *+12                                                             
         OI    ICFLAG,ICFLAST      YES - SET LAST TIME FOR BUFFER               
         B     ITECOP36                                                         
         STH   R1,TSRNUM           NO - GET NEXT SEQUENTIAL RECORD              
         MVI   TSACTN,TSAGET                                                    
         GOTOR VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    ICLSTACT,ICLSTACT   TEST FIRST TIME                              
         BZ    ITECOP37                                                         
         CLC   ICLSTACT,ICBUFTRN   TEST FOR CHANGE OF ACCOUNT                   
         BE    ITECOP37                                                         
ITECOP36 L     R1,AADTBLK          YES - FLUSH ADDTRN I/O & UNLOCK              
         MVI   TRNINDS-TRNBLK(R1),TRNICONV+TRNILAST+TRNIDUCN+TRNIDUCL           
         GOTOR VADDTRN,(R1)                                                     
         BNE   ITECOP40                                                         
         GOTOR VDMGR,BCPARM,DMUNLK,ACCDIR                                       
         GOTOR VDMGR,BCPARM,DMUNLK,ACCMST                                       
         TM    ICFLAG,ICFLAST      TEST LAST TIME FOR BUFFER                    
         BO    ITECOP43                                                         
*                                                                               
ITECOP37 MVC   ICLSTACT,ICBUFTRN   SAVE LAST ACCOUNT                            
         MVC   IOKEY,ICBUFTRN      READ 'FROM' TRANSACTION                      
         GOTOR AIO,IOREAD+IOACCDIR+IO3                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BCDLT)                                           
         B     ITECOP41                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGET+IOACCMST+IO3                                            
         TM    IOKEY+(TRNKSTAT-TRNRECD),TRNSARCH                                
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO3                                            
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         GOTOR REMNEW50            REMOVE NEW X'50' ELEMENTS                    
         OC    ICBUFGIN,ICBUFGIN   USE NEW GROUP INVOICE NO. IN 'TO' TX         
         BZ    ITECOP39                                                         
         USING TRNRECD,R1                                                       
         LA    R1,TRNRFST                                                       
         XR    R0,R0                                                            
ITECOP38 CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),GINELQ                                                     
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     ITECOP38                                                         
         MVC   GININV-GINELD(L'ICBUFGIN,R1),ICBUFGIN                            
         L     R1,AIO3                                                          
ITECOP39 GOTOR GENTRN                                                           
         L     R1,AADTBLK                                                       
         USING ADDTRND,R1                                                       
         MVC   TRNPUSER,ICLSTCUR+(LSTBUSER-LSTTABD)                             
         MVC   TRNBMOS,ICLSTCUR+(LSTBMOSP-LSTTABD)                              
         MVC   TRNEFDT,ICLSTCUR+(LSTBEFDT-LSTTABD)                              
         MVI   TRNINDS-TRNBLK(R1),TRNICONV+TRNIDRFT+TRNIDUCN+TRNIDUCL           
         GOTOR VADDTRN,(R1)                                                     
         BE    ITECOP42                                                         
         DROP  R1                                                               
*                                                                               
ITECOP40 L     R1,AADTBLK                                                       
         CLI   TRNERRS-TRNBLK(R1),TRNEACOL                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3             ACCOUNT IS LOCKED/CLOSED                     
         MVC   FVXTRA(L'TRNKULA),TRNKULA-TRNRECD(R1)                            
         MVC   FVMSGNO,=AL2(AE$ALOCL)                                           
*                                                                               
ITECOP41 LA    R1,BATREF2H         ABEND FROM UPDATIVE FUNCTION                 
         ST    R1,FVADDR                                                        
         NI    CSINDSL1,FF-(CSIK2VAL)  CLEAR KEY 2 VALID                        
         OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         L     RD,BCSVRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  EXIT TO ROOT CALL POINT                      
*                                                                               
ITECOP42 L     R1,AIO3                                                          
         MVC   ICBUFTRN,0(R1)      SET 'TO' TRANSACTION KEY                     
         MVI   TSACTN,TSAPUT                                                    
         GOTOR VTSAR,TSARD         PUT RECORD BACK WITH NEW KEY                 
         BE    *+6                                                              
         DC    H'0'                                                             
         B     ITECOP35                                                         
*                                                                               
ITECOP43 MVI   TSACTN,TSASRT       SORT BUFFER INTO ITEM SEQUENCE               
         MVI   TSRTKSEQ,0                                                       
         MVI   TSRTKDSP,ICBUFITE-ICBUFREC                                       
         MVI   TSRTKLEN,L'ICBUFITE+L'ICBUFSEQ                                   
         GOTOR VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    TSRNUM,TSRNUM       SET INITIAL RECORD NUMBER                    
         DROP  R4                                                               
         L     R3,ATIA             R3=A(ITEM INDEX)                             
         SR    R0,R0                                                            
         ICM   R0,3,TWASELCT       R0=NUMBER OF ITEM INDEX ENTRIES              
*                                                                               
ITECOP44 MVC   IODAOVER,ITENDXDA   SET D/A OF 'FROM' ITEM RECORD                
         GOTOR AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         USING TBARECD,R2          R2=A(EXISTING BATCH ITEM RECORD)             
         MVC   ICSEQSAV,TBAKTSEQ   SAVE OLD ITEM SEQUENCE NUMBER                
         L     R1,AIO1             R1=A(NEW BATCH HEADER)                       
         MVC   TBAKEY,0(R1)        EXTRACT KEY                                  
         SR    R1,R1                                                            
         ICM   R1,3,ICBITMA                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,ICBITMA        INCREMENT ITEM COUNT                         
         STCM  R1,3,TBAKTSEQ       SET ITEM# IN KEY                             
*                                                                               
         LA    R4,TBARFST                                                       
         USING BIAELD,R4                                                        
ITECOP46 CLI   BIAEL,0             TEST EOR                                     
         BE    ITECOP64                                                         
         CLI   BIAEL,BIAELQ                                                     
         BE    ITECOP50                                                         
         CLI   BIAEL,BTAELQ                                                     
         BE    ITECOP50                                                         
         CLI   BIAEL,BIOELQ                                                     
         BE    ITECOP58                                                         
         CLI   BIAEL,BICELQ                                                     
         BE    ITECOP60                                                         
         CLI   BIAEL,GINELQ                                                     
         BE    ITECOP61                                                         
         CLI   BIAEL,ASKELQ                                                     
         BE    ITECOP63                                                         
*                                                                               
ITECOP48 SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,BIALN                                                         
         AR    R4,RE                                                            
         B     ITECOP46                                                         
*                                                                               
ITECOP50 CLI   CSACT,ACTREV        TEST REVERSING ITEMS                         
         BNE   ITECOP52                                                         
         ZAP   BODUB1,BIAAMT                                                    
         MP    BODUB1,=P'-1'                                                    
         ZAP   BIAAMT,BODUB1                                                    
ITECOP52 MVC   BIAREF,ITENDXRF                                                  
         CLI   BIAEL,BIAELQ        TEST ITEM AMOUNT                             
         BNE   ITECOP48                                                         
         TM    CSBIND1,TYPICUMU    TEST ACCUMULATING TOTAL DRS/CRS              
         BO    ITECOP54                                                         
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS/TAX                  
         BO    *+14                                                             
         AP    ICBCSHA,BIAAMT      NO - ADD TO BATCH TOTAL                      
         B     ITECOP48                                                         
         TM    TBARESTA,TBAESIDR   TEST DEBIT ITEM                              
         BZ    *+14                                                             
         AP    ICBTDRS,BIAAMT      'DEBITS'=TAX                                 
         B     ITECOP48                                                         
         AP    ICBCSHA,BIAAMT      'CREDIT' - ADD TO BATCH TOTAL                
         AP    ICBTCRS,BIAAMT      'CREDITS'=HOURS/UNITS/CASH                   
         B     ITECOP48                                                         
*                                                                               
ITECOP54 TM    TBARESTA,TBAESDAC   TEST DR/CR PAIR                              
         BZ    ITECOP56                                                         
         BM    ITECOP48            (IGNORE DR/-DR & CR/-CR)                     
         AP    ICBCSHA,BIAAMT                                                   
         AP    ICBTDRS,BIAAMT                                                   
         AP    ICBTCRS,BIAAMT                                                   
         B     ITECOP48                                                         
*                                                                               
ITECOP56 LA    R1,ICBTCRS          ACCUMULATING TOTAL DRS/CRS                   
         TM    TBARESTA,TBAESIDR   TEST DEBIT ITEM                              
         BZ    *+14                                                             
         AP    ICBCSHA,BIAAMT      ADD TO BATCH TOTAL                           
         LA    R1,ICBTDRS                                                       
         AP    0(L'ICBTDRS,R1),BIAAMT                                           
         B     ITECOP48                                                         
*                                                                               
         USING BIOELD,R4                                                        
ITECOP58 DS    0H                  SUPPORT ORDERS                               
         CLI   CSACT,ACTREV        TEST REVERSING ITEMS                         
         BNE   ITECOP48                                                         
         ZAP   BODUB1,BIOAMNT                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   BIOAMNT,BODUB1                                                   
         CLI   BIOLN,BIOLNQ        CONTRACT ORDERS?                             
         BNH   ITECOP48            NO, GET OUTTA HERE                           
*                                                                               
         ZAP   BODUB1,BIORTAMT                                                  
         MP    BODUB1,=P'-1'                                                    
         ZAP   BIORTAMT,BODUB1                                                  
         ZAP   BODUB1,BIOUTAMT                                                  
         MP    BODUB1,=P'-1'                                                    
         ZAP   BIOUTAMT,BODUB1                                                  
         ZAP   BODUB1,BIOSTAMT                                                  
         MP    BODUB1,=P'-1'                                                    
         ZAP   BIOSTAMT,BODUB1                                                  
*                                                                               
         SR    RF,RF                                                            
         IC    RF,BIOLN                                                         
         LA    RE,0(RF,R4)         ADDRESS OF NEXT ELEMENT                      
         LA    RF,BIOSTOCK                                                      
         USING BIOSTOCK,RF                                                      
ITECOP59 ZAP   BODUB1,BIOSQTY      REVERSE QUANTITIES                           
         MP    BODUB1,=P'-1'                                                    
         ZAP   BIOSQTY,BODUB1                                                   
         LA    RF,BIOSUBQ(RF)      BUMP TO NEXT ONE                             
         CR    RF,RE                                                            
         BL    ITECOP59                                                         
*                                                                               
         B     ITECOP48                                                         
*                                                                               
         USING BICELD,R4                                                        
ITECOP60 MVC   BICCNO,ITENDXRF     REFERENCE/DATE MAY HAVE CHANGED              
         MVC   BICDAT,ITENDXDT                                                  
         CLI   CSACT,ACTREV        TEST REVERSING ITEMS                         
         BNE   ITECOP48                                                         
         ZAP   BODUB1,BICAMT                                                    
         MP    BODUB1,=P'-1'                                                    
         ZAP   BICAMT,BODUB1                                                    
         B     ITECOP48                                                         
*                                                                               
         USING GINELD,R4                                                        
ITECOP61 L     R1,ATSABLK          READ BUFFER FOR NEW GROUP INV. NO.           
         USING TSARD,R1                                                         
ITECOP62 LH    RE,TSRNUM           GET FIRST/NEXT BUFFER ENTRY                  
         LA    RE,1(RE)                                                         
         CLM   RE,3,TSPRECN        TEST ALL RECORDS READ                        
         BNH   *+6                                                              
         DC    H'0'                                                             
         STH   RE,TSRNUM           NO - GET NEXT SEQUENTIAL RECORD              
         MVI   TSACTN,TSAGET                                                    
         GOTOR VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ICBUFITE,ICSEQSAV   TEST SAME ITEM NUMBER                        
         BNE   ITECOP62                                                         
         MVC   GININV,ICBUFGIN                                                  
         B     ITECOP48                                                         
*                                                                               
         USING ASKELD,R4                                                        
ITECOP63 L     R1,ATSABLK                                                       
         USING TSARD,R1                                                         
         LH    RE,TSRNUM           GET FIRST/NEXT BUFFER ENTRY                  
         LA    RE,1(RE)                                                         
         CLM   RE,3,TSPRECN        TEST ALL RECORDS READ                        
         BNH   *+6                                                              
         DC    H'0'                                                             
         STH   RE,TSRNUM           NO - GET NEXT SEQUENTIAL RECORD              
         MVI   TSACTN,TSAGET                                                    
         GOTOR VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ICBUFITE,ICSEQSAV   TEST SAME ITEM NUMBER                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ICBUFSEQ,ASKSEQN    TEST SAME SEQUENCE NUMBER                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ASKKEY,ICBUFTRN     SET NEW TRANSACTION KEY                      
         B     ITECOP48                                                         
         DROP  R1                                                               
*                                                                               
ITECOP64 TM    ICLSTCUR+(LSTBINDS-LSTTABD),LSTBIAPD                             
         BZ    ITECOP66                                                         
         ST    R0,BOFULL1                                                       
         L     R0,AIO3             COPY ITEM RECORD INTO IO3                    
         SR    R1,R1                                                            
         ICM   R1,3,TBARLEN                                                     
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R0,BOFULL1                                                       
*                                                                               
         MVC   IOKEY(L'TBAKEY),TBAKEY                                           
         GOTOR AIO,IORDUPD+IOACCDIR+IO2                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOERNF        TEST RECORD NOT FOUND                        
         BNZ   ITECOP66                                                         
         TM    IOERR,IOEDEL        TEST RECORD FOUND BUT DELETED                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOGETRUP+IOACCMST+IO2                                        
*                                                                               
         ST    R0,BOFULL1                                                       
         L     R0,AIO2             RESTORE ITEM RECORD TO IO2                   
         L     RE,AIO3                                                          
         SR    RF,RF                                                            
         ICM   RF,3,TBARLEN-TBARECD(RE)                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         L     R0,BOFULL1                                                       
*                                                                               
         GOTOR AIO,IOPUTREC+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'TBAKEY),TBAKEY                                           
         MVC   IOKEY+L'TBAKEY(L'TBAKSTA),TBARSTA                                
         MVC   IOKEY+L'TBAKEY+L'TBARSTA(L'TBAKDA),IODA                          
         GOTOR AIO,IOWRITE+IOACCDIR+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     ITECOP68                                                         
*                                                                               
ITECOP66 NI    ICLSTCUR+(LSTBINDS-LSTTABD),FF-LSTBIAPD                          
         GOTOR AIO,IOADDREC+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ITECOP68 LA    R3,ITENDXL(R3)      BUMP TO NEXT ITEM INDEX ENTRY                
         BCT   R0,ITECOP44         DO FOR NUMBER OF ITEMS SELECTED              
*                                                                               
         GOTOR ABLDBAK,ICLSTCUR    BUILD BATCH HEADER KEY                       
         GOTOR AIO,IORDUP+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R1,TBARFST          UPDATE BATCH HEADER ELEMENT                  
         USING BHDELD,R1                                                        
         SR    R0,R0                                                            
ITECOP72 CLI   BHDEL,0             TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ        TEST BATCH HEADER ELEMENT                    
         BE    *+14                                                             
         IC    R0,BHDLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     ITECOP72                                                         
*                                                                               
         TM    CSBIND5,TYPIASIR    TEST AUTO REVERSE ALL                        
         BZ    ITECOP73                                                         
         ZAP   BCDUB,BCBATCUR+(LSTBCSHA-LSTTABD)(L'LSTBCSHA)                    
         MP    BCDUB,=P'-1'                                                     
         ZAP   ICBCSHA,BCDUB                                                    
*                                                                               
ITECOP73 MVC   BHDITEMA,ICBITMA    UPDATE ITEM COUNT                            
         ZAP   BHDCASHA,ICBCSHA    UPDATE CASH TOTAL                            
         TM    CSBIND1,TYPICUMU    TEST ACCUMULATING TOTAL DRS/CRS              
         BO    *+12                                                             
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS/TAX                  
         BZ    *+16                                                             
         ZAP   BHDTOTDR,ICBTDRS    UPDATE TOTAL DEBITS                          
         ZAP   BHDTOTCR,ICBTCRS    UPDATE TOTAL CREDITS                         
         GOTOR AIO,IOWRITE+IOACCMST+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    BCTSINDS,FF-BCTSIRES                                             
         GOTOR ATSARIO,TSARES      RESTORE TSAR BUFFER                          
*                                                                               
         MVC   CSLSTCUR,ICLSTCUR   SET 'TO' BATCH IN SESSION VALUES             
         GOTOR ASETMSK,CSLSTCUR                                                 
         OI    CSINDSL1,CSIUSELC                                                
         MVC   ICMSGNO,=AL2(AI$BACLO)                                           
         LA    R1,=AL1(RECBAT,ACTCLO,ITECOP#2,0,0,0)                            
         CLI   ICPFKEY,PFKCLOSQ                                                 
         BE    ITECOP74                                                         
         MVC   ICMSGNO,=AL2(AI$BAUPD)                                           
         LA    R1,=AL1(RECBAT,ACTUPD,ITECOP#2,0,0,0)                            
         CLI   ICPFKEY,PFKUPDTQ                                                 
         BE    ITECOP74                                                         
         MVC   ICMSGNO,=AL2(AI$BASAV)                                           
         LA    R1,=AL1(RECBAT,ACTSAV,ITECOP#2,0,0,0)                            
         CLI   ICPFKEY,PFKSAVEQ                                                 
         BE    ITECOP74                                                         
         MVC   ICMSGNO,=AL2(AI$BCOPN)                                           
         B     ITECOPR2            DON'T KNOW OR CARE HOW I GOT HERE            
*                                                                               
ITECOP74 GOTOR ANTRSES,(R1)        SAVE/CLOSE/UPDATE THE BATCH                  
*                                                                               
ITECOPR2 BASR  RB,0                                                             
         AHI   RB,ITECOP22-*                                                    
         CLI   TWASESNL,0          TEST NESTED CALL                             
         BE    ITECOP76                                                         
         MVC   CSLSTCUR,ICLSTSAV   RESTORE 'FROM' BATCH                         
         GOTOR AXITSES             RETURN TO LIST                               
*                                                                               
ITECOP76 GOTOR ADISHDR,1           DISPLAY 'TO' BATCH                           
         CLI   BCPFKEY,PFKQUITQ    TEST USER QUIT UPDATIVE ACTION               
         BNE   *+10                                                             
         MVC   ICMSGNO,=AL2(AI$BCOPN)                                           
         MVC   FVMSGNO,ICMSGNO                                                  
         MVI   FVOMTYP,GTMINF                                                   
         MVC   CSLSTCUR,ICLSTSAV   RESTORE 'FROM' BATCH                         
         NI    CSINDSL1,CSIUSELC   START AGAIN NEXT TIME                        
*                                                                               
ITECOPX  NI    CSINDSL1,FF-(CSIK2VAL)                                           
         J     SETCUR                                                           
*                                                                               
ITECOPER MVC   CSLSTCUR,ICLSTSAV   ERROR - RESTORE 'FROM' BATCH                 
         J     EXIT                                                             
         DROP  R2,R3,R4,RB,RC                                                   
         SPACE 1                                                                
ICWORKD  DSECT                     ** ITECOP LOCAL W/S **                       
ICBTYP   DS    XL1                 BATCH TYPE                                   
ICMASK   DS    XL2                 ACTION MASK                                  
ICLSTCUR DS    XL(LSTTABL)         NEW BATCH DETAILS                            
         ORG   ICLSTCUR+(LSTBITMA-LSTTABD)                                      
ICBITMA  DS    XL(L'LSTBITMA)                                                   
         ORG   ICLSTCUR+(LSTBCSHA-LSTTABD)                                      
ICBCSHA  DS    PL(L'LSTBCSHA)                                                   
         ORG   ICLSTCUR+(LSTBMOSC-LSTTABD)                                      
ICBMOSC  DS    XL(L'LSTBMOSC)                                                   
         ORG   ICLSTCUR+(LSTBBREF-LSTTABD)                                      
ICBBREF  DS    XL(L'LSTBBREF)                                                   
         ORG   ICLSTCUR+(LSTBTDRS-LSTTABD)                                      
ICBTDRS  DS    PL(L'LSTBTDRS)                                                   
         ORG   ICLSTCUR+(LSTBTCRS-LSTTABD)                                      
ICBTCRS  DS    PL(L'LSTBTCRS)                                                   
         ORG                                                                    
*                                                                               
ICBUFREC DS    0X                  ** TSAR TRANSACTION BUFFER RECORD **         
ICBUFTRN DS    XL(L'TRNKEY)        TRANSACTION KEY                              
ICBUFITE DS    XL(L'TBAKTSEQ)      ITEM RECORD NUMBER                           
ICBUFSEQ DS    XL(L'ASKSEQN)       TRANSACTION SEQUENCE NUMBER                  
ICKEYLEN EQU   *-ICBUFREC          LENGTH OF TSAR KEY                           
ICBUFREF DS    CL(L'ITENDXRF)      NEW REFERENCE NUMBER                         
ICBUFDAT DS    PL(L'ITENDXDT)      NEW DATE                                     
ICBUFGIN DS    XL(L'CSGIN)         NEW GROUP INVOICE NUMBER                     
ICBUFSTA DS    XL1                 STATUS                                       
ICBUFSRD EQU   X'80'               DATE AND/OR REFERENCE CHANGED                
ICBUFLEN EQU   *-ICBUFREC          LENGTH OF BUFFER RECORD                      
*                                                                               
ICSEQSAV DS    XL(L'TBAKTSEQ)      SAVED ITEM SEQUENCE NUMBER                   
*                                                                               
ICLSTACT DS    XL(ACTKEND)         LAST ACCOUT PASSED TO ADDTRN                 
*                                                                               
ICFLAG   DS    XL1                                                              
ICFLAST  EQU   X'80'               LAST TIME FOR TSAR BUFFER                    
*                                                                               
ICWORKX  EQU   *                                                                
         SPACE 1                                                                
ITENDXD  DSECT                     ** DSECT TO COVER ITEM INDEX **              
ITENDXSQ DS    XL(L'LSTISEQ)       ITEM SEQUENCE NUMBER                         
ITENDXDA DS    XL(L'LSTTDA)        DISK ADDRESS OF ITEM RECORD                  
ITENDXRF DS    CL(L'LSTIREF2)      ITEM DATE                                    
ITENDXDT DS    PL(L'LSTIDAT2)      ITEM REFERENCE                               
ITENDXST DS    XL1                 INDEX STATUS (SEE ICBUFSTA)                  
ITENDXL  EQU   *-ITENDXD           LENGTH OF INDEX ENTRY                        
BAT61    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* REMOVE THOSE NEW X'50' ELEMENTS WHEN COPYING / REVERSING            *         
***********************************************************************         
         USING TRNRECD,R2                                                       
         USING SCIELD,R3                                                        
REMNEW50 NTR1  LABEL=NO                                                         
         SR    R1,R1                                                            
         L     R2,AIO3                                                          
         LA    R3,TRNRFST                                                       
REMN50_5 CLI   SCIEL,0             EOR?                                         
         JE    EXIT                                                             
         CLI   SCIEL,SCIELQ        X'50' ELEMENT?                               
         JNE   REMN50_7                                                         
         CLI   SCITYPE,SCITPART    NEW ONES FOR TYPE 34 ONLY                    
         JNE   REMN50_7                                                         
         MVI   SCIEL,X'FF'                                                      
         J     EXIT                                                             
*                                                                               
REMN50_7 IC    R1,SCILN                                                         
         AR    R3,R1                                                            
         J     REMN50_5                                                         
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* CHANGE AN ITEM RECORD                                               *         
***********************************************************************         
         SPACE 1                                                                
ITECHA   BASE  ,                                                                
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BNZ   ITECHA02                                                         
         MVC   FVMSGNO,=AL2(AE$INACS)                                           
         LA    R0,BASACTH          SET CURSOR TO ACTION FIELD                   
         ST    R0,FVADDR                                                        
         J     EXIT                                                             
ITECHA02 CLI   CSOMODE,CSOMSSCR    TEST SUB-SCREEN ACTIVE                       
         BE    ITECHA18                                                         
         TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BNZ   ITECHA32                                                         
         LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2                                                       
         GOTOR AGETBTY,LSTBBTYP                                                 
         GOTOR AOVRSCR,BCPARM,('DETLSCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,BCITECUR                                                      
         MVC   IODAOVER,LSTTDA                                                  
         GOTOR AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING TBARECD,R2                                                       
*                                                                               
         MVC   BCBYTE1,CSBITS      FIGURE OUT WHAT SCREEN TO LOAD               
         CLI   TBARESCR,0                                                       
         BE    *+10                                                             
         MVC   BCBYTE1,TBARESCR                                                 
         CLI   BCBYTE1,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTOR AOVRSCR,BCPARM,(BCBYTE1,BASOLY2H)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR ABLDDET,1           DISPLAY BATCH/ITEM DETAILS                   
*                                                                               
         GOTOR ARESFLD,BCPARM,TBARFST,BASOLY2H                                  
*                                                                               
         MVI   CSOLINE,0                                                        
         TM    CSBIND5,TYPIOVSS    TEST OVERLAY CAN CALL SUB-SCREEN             
         BZ    ITECHA08                                                         
         LA    R2,TBARFST                                                       
         XR    R0,R0                                                            
ITECHA04 CLI   0(R2),0                                                          
         BE    ITECHA08                                                         
         CLI   0(R2),GINELQ                                                     
         BE    ITECHA06                                                         
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     ITECHA04                                                         
         USING GINELD,R2                                                        
ITECHA06 OC    GINHISN,GINHISN     TEST SUB-SCREEN ACTIVE                       
         BZ    ITECHA08                                                         
         MVC   CSOLINE,GINHISN+1                                                
         OC    CSSHIREC,CSSHIREC                                                
         BNZ   *+10                                                             
         MVC   CSSHIREC,GINHISN+1                                               
         OC    CSSDELS,CSSDELS                                                  
         BNZ   *+10                                                             
         MVC   CSSDELS,GINDELS+1                                                
         MVC   CSGIN,GININV                                                     
*                                                                               
ITECHA08 TM    CSBIND2,TYPIOSC     TEST OVERLAY PREPARES SCREEN                 
         BZ    ITECHA16                                                         
         GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         GOTOR BONTRYA,BCPARM,('TYPIOSC',TWAD),WORKD                            
*                                                                               
ITECHA16 CLI   CSLSTCUR+2,45       HANDLE TYPE 45S DIFFERENTLY                  
         BE    *+12                                                             
         TM    CSBIND5,TYPIOVSS    TEST OVERLAY CAN CALL SUB-SCREEN             
         BZ    ITECHA24                                                         
         CLI   CSOLINE,0                                                        
         BE    ITECHA22                                                         
ITECHA18 GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         CLI   CSOMODE,CSOMSSCR                                                 
         BNE   ITECHA19                                                         
         GOTOR BONTRYA,BCPARM,('TYPIOVSS',TWAD),WORKD                           
         CLI   FVMSGNO+1,X'FF'     TEST OVERLAY HAS SET MESSAGE                 
         JNE   EXIT                                                             
         B     ITECHAX                                                          
ITECHA19 LA    R1,BASOLY2H         SET UNPROTECTED FIELDS VALIDATED             
         LA    RF,OSVALS-1                                                      
         SR    RE,RE                                                            
ITECHA20 TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   *+8                                                              
         OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+8                                                              
         BXLE  R1,RE,ITECHA20                                                   
         MVI   CSOMODE,CSOMSSCR    SET SUB-SCREEN ACTIVE                        
         MVC   FVMSGNO,=AL2(AI$RDECH)  RECORD DISPLAYED - ENTER CHANGES         
         MVI   FVOMTYP,GTMINF                                                   
         B     ITECHA24                                                         
*                                                                               
ITECHA22 TM    CSLSTCUR+(LSTBIND2-LSTTABD),LSTBIMLT                             
         BZ    ITECHA24                                                         
         GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         MVC   CSOLINE,BCITECUR+(LSTISIS-LSTTABD)                               
         MVI   CSOMODE,CSOMPCHA    PREPARE FOR ITEM/CHANGE                      
         GOTOR APHOOK                                                           
*                                                                               
ITECHA24 LA    R1,BASOLY2H         SET CURSOR TO FIRST UNPROT FIELD             
         LA    RF,OSVALS-1                                                      
         SR    RE,RE                                                            
ITECHA26 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    *+10                                                             
         BXLE  R1,RE,ITECHA26                                                   
         DC    H'0'                                                             
         ST    R1,FVADDR                                                        
*                                                                               
         CLI   CSOMODE,CSOMSSCR    TEST SUB-SCREEN ACTIVE                       
         JE    EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIRDSPC                                                
         MVI   TWAMODE,0           CLEAR OVERLAY MODE                           
*                                                                               
ITECHA30 TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   *+8                                                              
         OI    FVIIND-FVIHDR(R1),FVIVAL SET PREVIOUSLY VALIDATED BITS           
         ICM   RE,1,FVTLEN-FVIHDR(R1)   IN ALL INPUT FIELDS                     
         BZ    *+8                                                              
         BXLE  R1,RE,ITECHA30                                                   
         J     EXIT                                                             
*                                                                               
ITECHA32 LA    R1,BASOLY2H         TEST FOR INPUT THIS TIME                     
         LA    RF,OSVALS-1                                                      
         SR    RE,RE                                                            
         SR    R3,R3                                                            
         XC    FVADDR,FVADDR                                                    
ITECHA34 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    ITECHA38                                                         
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   ITECHA36                                                         
         OC    FVADDR,FVADDR                                                    
         BNZ   *+8                                                              
         ST    R1,FVADDR           SET A(FIRST INPUT FIELD)                     
         TM    FVIIND-FVIHDR(R1),FVIVAL                                         
         BNZ   *+8                                                              
         LA    R3,1(R3)            BUMP NUMBER OF CHANGED FIELDS                
         NI    FVIIND-FVIHDR(R1),FF-FVIVAL                                      
ITECHA36 BXLE  R1,RE,ITECHA34                                                   
         DC    H'0'                                                             
ITECHA38 LTR   R3,R3               TEST ANY INPUT THIS TIME                     
         BNZ   ITECHA40                                                         
         TM    CSBIND5,TYPINODC    TEST NO DATA REQUIRED FOR CHANGE             
         BZ    *+12                                                             
         CLI   BCPFKEY,0           YES - TEST PF KEY ENTERED                    
         BNE   ITECHA40                                                         
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         J     EXIT                                                             
*                                                                               
ITECHA40 GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         TM    CSLSTCUR+(LSTBIND2-LSTTABD),LSTBIMLT                             
         BZ    ITECHA42                                                         
         MVC   CSOLINE,BCITECUR+(LSTISIS-LSTTABD)                               
         MVI   CSOMODE,CSOMDCHA    DO ITEM/CHANGE                               
*                                                                               
ITECHA42 GOTOR APHOOK                                                           
         LA    R0,1                                                             
         BNE   *+6                                                              
         SR    R0,R0                                                            
         GOTOR ABLDDET,1                                                        
         LTR   R0,R0                                                            
         JNZ   EXIT                                                             
ITECHAX  GOTOR AXITSES             RETURN TO LIST                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE AN ITEM RECORD (LOGICAL DELETE)                              *         
***********************************************************************         
         SPACE 1                                                                
ITEDEL   TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         JNZ   ITEDEL00                                                         
         MVC   FVMSGNO,=AL2(AE$INACS)                                           
         LA    R0,BASACTH          SET CURSOR TO ACTION FIELD                   
         ST    R0,FVADDR                                                        
         J     EXIT                                                             
*                                                                               
ITEDEL00 BASE  ,                                                                
         TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BNZ   ITEDEL30                                                         
         LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2                                                       
         GOTOR AGETBTY,LSTBBTYP                                                 
         GOTOR AOVRSCR,BCPARM,('DETLSCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,BCITECUR                                                      
         MVC   IODAOVER,LSTTDA                                                  
         GOTOR AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO1                                                          
         USING TBARECD,R4                                                       
*                                                                               
         MVC   BCBYTE1,CSBITS      FIGURE OUT WHAT SCREEN TO LOAD               
         CLI   TBARESCR,0                                                       
         BE    *+10                                                             
         MVC   BCBYTE1,TBARESCR                                                 
         CLI   BCBYTE1,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTOR AOVRSCR,BCPARM,(BCBYTE1,BASOLY2H)                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,BASOLY2H         TRANSMIT/PROTECT/SET FIELD INTENSITY         
         LA    RF,OSVALS-1                                                      
         SR    RE,RE                                                            
ITEDEL02 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    ITEDEL04                                                         
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         TM    FVATRB-FVIHDR(R1),FVAZERO                                        
         BO    *+8                                                              
         NI    FVATRB-FVIHDR(R1),FF-FVAHIGH                                     
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BO    *+8                                                              
         OI    FVATRB-FVIHDR(R1),FVAHIGH+FVAPROT                                
         BXLE  R1,RE,ITEDEL02                                                   
*                                                                               
ITEDEL04 GOTOR ABLDDET,1           DISPLAY BATCH/ITEM DETAILS                   
*                                                                               
         GOTOR ARESFLD,BCPARM,TBARFST,BASOLY2H                                  
*                                                                               
         LA    R2,TBARFST                                                       
         USING BIAELD,R2                                                        
         MVI   CSOLINE,0                                                        
         CLI   BIAEL,BIAELQ        TEST BATCH ITEM/AMOUNT ELEMENT               
         BNE   ITEDEL24                                                         
         CLI   BIALN,BIALN2Q                                                    
         BL    ITEDEL24                                                         
         MVC   CSOLINE,BIASIS      SAVE ITEM SEQUENCE NUMBER                    
*                                                                               
ITEDEL24 TM    CSBIND2,TYPIOSC     TEST OVERLAY PREPARES SCREEN                 
         BZ    ITEDEL26                                                         
         GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         GOTOR BONTRYA,BCPARM,('TYPIOSC',TWAD),WORKD                            
*                                                                               
ITEDEL26 CLI   CSOLINE,0           TEST MULTIPLE INPUT SCREEN                   
         BE    ITEDEL28                                                         
         GOTOR AOVRBIT,CSBITO      LOAD APPLICATION OVERLAY PROGRAM             
         MVI   CSOMODE,CSOMPCHA    PREPARE FOR ITEM/CHANGE                      
         GOTOR APHOOK                                                           
*                                                                               
         USING LSTTABD,R2                                                       
ITEDEL28 LA    R2,CSLSTCUR                                                      
         LA    R1,BASACTH          SET CURSOR TO ACTION FIELD                   
         ST    R1,FVADDR                                                        
         TM    LSTTSTAT,TBAHSDEL                                                
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BADEL)                                           
         J     EXIT                                                             
         TM    LSTTSTAT,TBAHSUPD                                                
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BAUPD)                                           
         J     EXIT                                                             
         TM    LSTTSTAT,TBAHSEND                                                
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BACLO)                                           
         J     EXIT                                                             
         L     R1,AIO1                                                          
         TM    TBARESTA-TBARECD(R1),TBAESDEL+TBAESLDE                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ITDEL)                                           
         J     EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$ETDEL)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIRDSPC                                                
         J     EXIT                                                             
*                                                                               
ITEDEL30 LA    R2,BCITECUR                                                      
         MVC   IODAOVER,LSTTDA                                                  
         GOTOR AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO1                                                          
         MVC   IOKEY,TBAKEY                                                     
         XC    IOKEY+TBAKTSEQ-TBARECD(L'TBAKTSEQ),IOKEY+TBAKTSEQ-TBAREC+        
               D                                                                
         GOTOR AIO,IORD+IOACCMST+IO2                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         LA    R2,TBARFST                                                       
         USING ASKELD,R2                                                        
ITEDEL32 CLI   ASKEL,0                                                          
         BE    ITEDEL40                                                         
         CLI   ASKEL,ASKELQ        TEST ACCOUNT SYSTEM KEY ELEMENT              
         BNE   ITEDEL36                                                         
*                                                                               
         MVC   BCINVREF,BCSPACES   CLEAR FOR INOICE PASSIVES                    
         MVC   IOKEY,ASKKEY        UPDATE TRANSACTION RECORD                    
         GOTOR AIO,IORDUP+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         USING TRNRECD,R1                                                       
         MVC   BCINVREF(L'TRNKREF),TRNKREF                                      
         MVC   BCINVDTE(L'TRNKDATE),TRNKDATE                                    
         TM    TRNRSTAT,TRNSDRFT   MUST BE A DRAFT TRANSACTION                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OI    TRNRSTAT,TRNSDELT   SET DELETED                                  
         GOTOR AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOKEY                                                         
         L     RF,AIO3                                                          
         MVC   TRNKEY,0(RF)                                                     
         MVC   TRNKSTA,TRNRSTA-TRNRECD(RF)                                      
         MVC   TRNKDA,IODA                                                      
         GOTOR AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,IOKEY            UPDATE ACCOUNT RECORD                        
         USING ACTRECD,R1                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,ASKKEY                                                  
         GOTOR AIO,IOREAD+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         LA    R1,ACTRFST                                                       
         USING ASTELD,R1                                                        
ITEDEL34 IC    R0,ASTLN                                                         
         AR    R1,R0                                                            
         CLI   ASTEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ASTEL,ASTELQ                                                     
         BNE   ITEDEL34                                                         
         SR    RE,RE                                                            
         ICM   RE,7,ASTDRAFT                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         STCM  RE,7,ASTDRAFT                                                    
         GOTOR AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     ITEDEL38                                                         
*                                                                               
         USING GINELD,R2                                                        
ITEDEL36 CLI   GINEL,GINELQ        DELETE POSTINGS VIA GIN PASSIVES             
         BNE   ITEDEL38                                                         
         LA    R1,IOKEY                                                         
         USING GINPASD,R1                                                       
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,CUABIN                                                   
         MVC   GINPINV,GININV                                                   
         MVC   GINPISN,=X'FFFF'                                                 
         GOTOR ADELGIN                                                          
*                                                                               
ITEDEL38 IC    R0,GINLN                                                         
         AR    R2,R0                                                            
         B     ITEDEL32                                                         
*                                                                               
ITEDEL40 TM    TBARESTA-TBARECD(R4),TBAESORD                                    
         BZ    ITEDEL49                                                         
         GOTO1 AORDAUD,BODMCB,(1,AIO1)                                          
         LA    R2,TBARFST          PROCESS ORDERS (MIXIUNDO)                    
         SR    R3,R3                                                            
         USING BIOELD,R2                                                        
ITEDEL42 CLI   BIOEL,0                                                          
         BE    ITEDEL49                                                         
         CLI   BIOEL,BIOELQ        TEST BATCH ITEM ORDER ELEMENT                
         BNE   ITEDEL44                                                         
         LA    R3,1(R3)            CURRENT BIOEL SEQUENCE NUMBER                
         STC   R3,BOBYTE1                                                       
         GOTOR AUPDORD,BODMCB,(X'0C',BIOELD),BOBYTE1,AIO1                       
ITEDEL44 IC    R0,BIOLN                                                         
         AR    R2,R0                                                            
         B     ITEDEL42                                                         
                                                                                
         USING BIAELD,R2                                                        
ITEDEL49 LA    R2,TBARFST          PROCESS ORDERS (MIXIUNDO)                    
ITEDEL50 CLI   BIAEL,0                                                          
         BE    ITEDEL56                                                         
         CLI   BIAEL,BIAELQ        TEST ITEM AMOUNT ELEMENT                     
         BE    *+12                                                             
         CLI   BIAEL,BTAELQ        OR TOTAL AMOUNT ELEMENT                      
         BNE   ITEDEL54                                                         
         LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3                                                       
         ICM   R1,3,LSTBDELI       INCREMENT DELETED ITEM COUNT                 
         LA    R1,1(R1)                                                         
         STCM  R1,3,LSTBDELI                                                    
         TM    CSBIND1,TYPICUMU    TEST ACCUMULATING TOTAL DRS/CRS              
         BO    ITEDEL52                                                         
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS/TAX                  
         BO    *+14                                                             
         SP    LSTBCSHA,BIAAMT     NO - UPDATE BATCH TOTAL                      
         B     ITEDEL54                                                         
         TM    TBARESTA,TBAESIDR   TEST DEBIT ITEM                              
         BO    ITEDEL51                                                         
         SP    LSTBTCRS,BIAAMT     'CREDITS'=HOURS                              
         SP    LSTBCSHA,BIAAMT      UPDATE BATCH TOTAL                          
         CLI   BIALN,BIALN3Q                                                    
         BL    ITEDEL54                                                         
         SP    LSTBTDRS,BIATAX      TAX(TYPE 10)                                
         B     ITEDEL54                                                         
*                                                                               
ITEDEL51 SP    LSTBTDRS,BIAAMT     'DEBITS'=TAX                                 
         B     ITEDEL54                                                         
*                                                                               
ITEDEL52 TM    TBARESTA,TBAESDAC   TEST DR/CR PAIR                              
         BZ    ITEDEL53                                                         
         BM    ITEDEL54            (IGNORE DR/-DR & CR/-CR)                     
         SP    LSTBTDRS,BIAAMT                                                  
         SP    LSTBTCRS,BIAAMT                                                  
         SP    LSTBCSHA,BIAAMT     UPDATE BATCH TOTAL                           
         B     ITEDEL54                                                         
*                                                                               
ITEDEL53 LA    RE,LSTBTCRS         ACCUMULATING TOTAL DRS/CRS                   
         TM    TBARESTA,TBAESIDR   TEST DEBIT ITEM                              
         BZ    *+14                                                             
         SP    LSTBCSHA,BIAAMT     UPDATE BATCH TOTAL                           
         LA    RE,LSTBTDRS                                                      
         SP    0(L'LSTBTDRS,RE),BIAAMT                                          
         DROP  R4                                                               
*                                                                               
ITEDEL54 IC    R0,BIALN                                                         
         AR    R2,R0                                                            
         B     ITEDEL50                                                         
*                                                                               
ITEDEL56 MVC   IODAOVER,LSTTDA     GET BATCH HEADER RECORD                      
         GOTOR AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         LA    R1,TBARFST-TBARECD(R1)                                           
         USING BHDELD,R1                                                        
ITEDEL57 CLI   BHDEL,0             LOCATE BATCH HEADER ELEMENT                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    R1,R0                                                            
         B     ITEDEL57                                                         
         LA    R3,CSLSTCUR                                                      
         ZAP   BHDCASHA,LSTBCSHA   UPDATE BATCH HEADER ELEMENT                  
         MVC   BHDDELIT,LSTBDELI   SAVE NUMBER OF DELETED ITEMS                 
         CLI   BHDLN,BHDLN2Q                                                    
         BL    *+16                                                             
         ZAP   BHDTOTDR,LSTBTDRS                                                
         ZAP   BHDTOTCR,LSTBTCRS                                                
         GOTOR AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1,R3                                                            
*                                                                               
         L     R2,AIO1             UPDATE ITEM RECORD                           
         USING TBARECD,R2                                                       
         OI    TBARESTA,TBAESLDE                                                
         GOTOR AIO,IOPUTREC+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         L     R1,AIO1                                                          
         MVC   TBAKEY,0(R1)                                                     
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA                                                      
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AI$ITDEL)                                           
         MVI   FVOMTYP,GTMINF                                                   
         NI    CSINDSL1,FF-(CSIRDSPC)                                           
         OI    BCITECUR+(LSTTSTAT-LSTTABD),TBAESLDE                             
         GOTOR ASETMSK,CSLSTCUR    SET BATCH ACTION MASK                        
         GOTOR ASETMSK,BCITECUR    SET ITEM ACTION MASK                         
         GOTOR AXITSES                                                          
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* GENERATE ITEM RECORDS FOR A NEW BATCH                               *         
***********************************************************************         
         SPACE 1                                                                
         USING IGWORKD,RC                                                       
ITEGEN   BASE  ,                                                                
         MVC   IGMASK,=AL2(LMITEGEN)                                            
         LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    *+8                                                              
         OI    CSINDSL1,CSIK1REQ+CSIK1VAL                                       
         TM    CSINDSL1,CSIK1REQ   TEST KEY 1 REQUESTED                         
         BNZ   ITEGEN02                                                         
         GOTOR ADISHDR,1+X'80'     BUILD BATCH HEADER SCREEN                    
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIK1REQ                                                
         J     EXIT                                                             
*                                                                               
ITEGEN02 TM    CSINDSL1,CSIK1VAL   TEST KEY 1 VALID                             
         BNZ   ITEGEN04                                                         
         LA    R1,LSTTABD          VALIDATE BATCH KEY                           
         ICM   R1,8,BCEFFS         SET TO TEST BATCH TYPE SECURITY              
         GOTOR AGETBAT                                                          
         JNE   EXIT                                                             
         SR    R0,R0                                                            
         ICM   R0,3,LSTBITMA                                                    
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R0,BOHALF1                                                       
         BP    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BEMPT)                                           
         J     EXIT                                                             
         TM    LSTTSTAT,TBAHSIIP   TEST INPUT IN PROGRESS                       
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INPIP)                                           
         J     EXIT                                                             
         TM    LSTBINDS,LSTBBCBG   TEST TYPE CREATED BY ACBG                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IAFBT)                                           
         J     EXIT                                                             
         TM    LSTBINDS,LSTBACRV   TEST BATCH IS AN ACCRUAL REVERSAL            
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IAFBT)                                           
         J     EXIT                                                             
         MVC   BOWORK1(L'IGMASK),IGMASK                                         
         NC    BOWORK1(L'IGMASK),LSTTMASK                                       
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         J     EXIT                                                             
         OI    CSINDSL1,CSIK1VAL   SET KEY 1 IS VALID                           
         MVC   BCBATCUR,CSLSTCUR   SET CURRENT BATCH LIST RECORD                
*                                                                               
ITEGEN04 TM    CSINDSL1,CSIK2REQ   TEST KEY 2 REQUESTED                         
         BNZ   ITEGEN06                                                         
         GOTOR ADISHDR,5           BUILD BATCH HEADER 'FROM-TO' SCREEN          
         LA    R0,BATREF2H                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIK2REQ                                                
         J     EXIT                                                             
*                                                                               
ITEGEN06 TM    CSINDSL1,CSIK2VAL   TEST KEY 2 VALID                             
         BZ    *+6                                                              
         DC    H'0'                YES - SHOULDN'T BE HERE                      
*                                                                               
         MVC   IGLSTSAV,CSLSTCUR   SAVE 'FROM' BATCH                            
*                                                                               
         MVI   FVMINL,1            BATCH REFERENCE                              
         GOTOR AVALBRF,BATREF2H                                                 
         BNE   ITEGENER                                                         
         MVI   FVMINL,1            BATCH NAME                                   
         GOTOR AVALBNA,BATNAM2H                                                 
         BNE   ITEGENER                                                         
         MVI   FVMINL,1            BATCH MONTH OF SERVICE                       
         GOTOR AVALBMO,BATMOA2H                                                 
         BNE   ITEGENER                                                         
*&&UK*&& GOTOR ACHKBMO                                                          
*&&UK*&& BNE   ITEGENER                                                         
         GOTOR AVALEFD,BATEFD2H    EFFECTIVE DATE                               
         BNE   ITEGENER                                                         
         GOTOR ATSTBMO,BATMOA2H    TEST BATCH MONTH LOCKED                      
         BNE   ITEGENER                                                         
         MVI   FVMINL,1            BATCH ITEM CONTROL                           
         GOTOR AVALITE,BATITC2H                                                 
         BNE   ITEGENER                                                         
         MVI   FVMINL,1            BATCH CASH CONTROL                           
         GOTOR AVALCSH,BATAMC2H                                                 
         BNE   ITEGENER                                                         
*                                                                               
         XC    LSTBDELI,LSTBDELI                                                
         XC    LSTBHISN,LSTBHISN                                                
         MVI   LSTBHDS1,BHDSGENE   SET THIS IS A GENERATED BATCH                
         NI    LSTBHDS2,FF-(BHDSGENL+BHDSBDON)                                  
         GOTOR AFVAL,BATACR2H      VALIDATE ACCRUAL OPTION                      
         BE    ITEGEN08                                                         
         BH    ITEGENER                                                         
         OC    CSBTYP2,CSBTYP2     TEST AUTOMATIC ACCRUAL                       
         BZ    ITEGEN10                                                         
         TM    IGLSTSAV+(LSTBHDS1-LSTTABD),BHDSACRU                             
         BZ    ITEGEN10            UNLESS ORIGINAL WAS NOT ACCRUED              
         OI    LSTBHDS1,BHDSACRU   SET THIS IS AN ACCRUAL BATCH                 
         B     ITEGEN10                                                         
ITEGEN08 SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8              TEST FOR 'NO'                                
         BE    ITEGEN10                                                         
         CLC   BC@NO(0),FVIFLD     'NO' IS ALWAYS VALID                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   BC@YES(0),FVIFLD    TEST FOR 'YES'                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     ITEGENER                                                         
         TM    CSBIND1,TYPIACRL    TEST ACCRUAL VALID FOR BATCH TYPE            
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACRNV)                                           
         B     ITEGENER                                                         
         OI    LSTBHDS1,BHDSACRU   SET THIS IS AN ACCRUAL BATCH                 
*                                                                               
ITEGEN10 XC    LSTBAPNO,LSTBAPNO   SET NO APPROVER FOR THIS BATCH               
         GOTOR AADDHDR,BCPARM,BATREF2H,('FF',0)  TEST 'ADD' IS OK               
         BNE   ITEGENER            'TO' BATCH ALREADY EXISTS                    
*                                                                               
         OI    CSINDSL1,CSIK2VAL   SET KEY 2 IS VALID                           
         XC    CSLSTCUR,IGLSTSAV   RESTORE 'FROM' BATCH                         
         XC    IGLSTSAV,CSLSTCUR   AND SAVE 'TO' BATCH                          
         XC    CSLSTCUR,IGLSTSAV                                                
         TM    CSINDSL2,CSIRETRY   TEST RETRY AFTER ERROR                       
         BO    ITEGEN22                                                         
*                                                                               
         XC    TWASELCT,TWASELCT   CLEAR SELECTED ITEM COUNT                    
         OI    CSINDSL1,CSIUSELC   SET NESTED CALL                              
         GOTOR ANTRSES,=AL1(RECSCR,ACTSEL,ITEGEN#1,0,0,0)                       
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* RETURN FROM SCREEN/SELECT                                           *         
* NTRY - CSLSTCUR CONTAINS ENTRY FOR 'FROM' BATCH                     *         
*        IGLSTSAV CONTAINS ENTRY FOR 'TO' BATCH                       *         
***********************************************************************         
         SPACE 1                                                                
ITEGENR1 MVC   IGPFKEY,BCPFKEY     SAVE USER PFKEY                              
         CLI   IGPFKEY,PFKINPTQ    TEST ENTER ITEM/INPUT                        
         JE    ITEGEN18                                                         
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     ITEGENX                                                          
*                                                                               
ITEGEN18 BASE  ,                                                                
         OC    TWASELCT,TWASELCT   TEST ANY ITEMS SELECTED                      
         BNZ   ITEGEN20                                                         
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         B     ITEGENX                                                          
*                                                                               
ITEGEN20 XC    CSLSTCUR,IGLSTSAV   SET 'TO' BATCH IN CSLSTCUR                   
         XC    IGLSTSAV,CSLSTCUR   AND SAVE 'FROM' BATCH                        
         XC    CSLSTCUR,IGLSTSAV                                                
         GOTOR AADDHDR,BCPARM,('FF',BATREF2H),(2,BATCOM1H),IGLSTSAV             
         BE    ITEGEN22                                                         
         NI    CSINDSL1,FF-(CSIK2VAL)                                           
         OI    CSINDSL2,CSIRETRY                                                
         B     ITEGENER                                                         
*                                                                               
ITEGEN22 NI    CSINDSL2,FF-(CSIRETRY)                                           
         GOTOR AGETBTY,CSLSTCUR+(LSTBBTYP-LSTTABD)                              
         GOTOR AOVRSCR,BCPARM,('DETLSCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TWAMODE,0           CLEAR OVERLAY MODE                           
         LA    R1,=AL1(RECITE,ACTINP,ITEGEN#2,0,CSOIPLII+CSOIFRST,0)            
         GOTOR ANTRSES,(R1)                                                     
*                                                                               
ITEGENR2 BASR  RB,0                                                             
         AHI   RB,ITEGEN18-*                                                    
         CLI   TWASESNL,0          TEST NESTED CALL                             
         BE    ITEGEN24                                                         
         MVC   CSLSTCUR,IGLSTSAV   RESTORE 'FROM' BATCH                         
         GOTOR AXITSES             RETURN TO LIST                               
*                                                                               
ITEGEN24 GOTOR ADISHDR,1           DISPLAY 'TO' BATCH                           
         MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         MVI   FVOMTYP,GTMINF                                                   
         MVC   CSLSTCUR,IGLSTSAV   RESTORE 'FROM' BATCH                         
         NI    CSINDSL1,CSIUSELC   START AGAIN NEXT TIME                        
*                                                                               
ITEGENX  NI    CSINDSL1,FF-(CSIK2VAL)                                           
         J     SETCUR                                                           
*                                                                               
ITEGENER MVC   CSLSTCUR,IGLSTSAV   ERROR - RESTORE 'FROM' BATCH                 
         J     EXIT                                                             
         DROP  RB,RC                                                            
         SPACE 1                                                                
IGWORKD  DSECT                     ** ITEGEN LOCAL W/S **                       
IGMASK   DS    XL2                 ACTION MASK                                  
IGWORKX  EQU   *                                                                
BAT61    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GENERATE GENERAL LEDGER POSTINGS FOR A NEW BATCH                    *         
***********************************************************************         
         SPACE 1                                                                
         USING BGWORKD,RC                                                       
BATGLU   BASE  ,                                                                
         MVC   BGMASK,=AL2(LMBATGLU)                                            
         LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    *+8                                                              
         OI    CSINDSL1,CSIK1REQ+CSIK1VAL                                       
         TM    CSINDSL1,CSIK1REQ   TEST KEY 1 REQUESTED                         
         BNZ   BATGLU02                                                         
         GOTOR ADISHDR,1+X'80'     BUILD BATCH HEADER SCREEN                    
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIK1REQ                                                
         J     EXIT                                                             
                                                                                
BATGLU02 TM    CSINDSL1,CSIK1VAL   TEST KEY 1 VALID                             
         BNZ   BATGLU04                                                         
         LA    R1,LSTTABD          VALIDATE BATCH KEY                           
         ICM   R1,8,BCEFFS         SET TO TEST BATCH TYPE SECURITY              
         GOTOR AGETBAT                                                          
         JNE   EXIT                                                             
         SR    R0,R0                                                            
         ICM   R0,3,LSTBITMA                                                    
         MVC   BOHALF1,LSTBDELI                                                 
         SH    R0,BOHALF1                                                       
         BP    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BEMPT)                                           
         J     EXIT                                                             
         TM    LSTTSTAT,TBAHSIIP   TEST INPUT IN PROGRESS                       
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INPIP)                                           
         J     EXIT                                                             
         TM    LSTBINDS,LSTBACRV   TEST BATCH IS AN ACCRUAL REVERSAL            
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IAFBT)                                           
         J     EXIT                                                             
         MVC   BOWORK1(L'BGMASK),BGMASK                                         
         NC    BOWORK1(L'BGMASK),LSTTMASK                                       
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         J     EXIT                                                             
         OI    CSINDSL1,CSIK1VAL   SET KEY 1 IS VALID                           
         MVC   BCBATCUR,CSLSTCUR   SET CURRENT BATCH LIST RECORD                
                                                                                
BATGLU04 TM    CSINDSL1,CSIK2REQ   TEST KEY 2 REQUESTED                         
         BNZ   BATGLU06                                                         
         GOTOR ADISHDR,9+X'10'     BUILD BATCH HEADER 'FROM-TO' SCREEN          
         LA    R0,BATREF2H                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIK2REQ                                                
         J     EXIT                                                             
                                                                                
BATGLU06 TM    CSINDSL1,CSIK2VAL   TEST KEY 2 VALID                             
         BZ    *+6                                                              
         DC    H'0'                YES - SHOULDN'T BE HERE                      
                                                                                
         MVC   BGLSTSAV,CSLSTCUR   SAVE 'FROM' BATCH                            
                                                                                
         MVI   FVMINL,1            BATCH REFERENCE                              
         GOTOR AVALBRF,BATREF2H                                                 
         BNE   BATGLUER                                                         
         MVI   FVMINL,1            BATCH NAME                                   
         GOTOR AVALBNA,BATNAM2H                                                 
         BNE   BATGLUER                                                         
         MVI   FVMINL,1            BATCH MONTH OF SERVICE                       
         GOTOR AVALBMO,BATMOA2H                                                 
         BNE   BATGLUER                                                         
         GOTOR AVALEFD,BATEFD2H    EFFECTIVE DATE                               
         BNE   BATGLUER                                                         
         GOTOR ATSTBMO,BATMOA2H    TEST BATCH MONTH LOCKED                      
         BNE   BATGLUER                                                         
         MVI   FVMINL,1            BATCH ITEM CONTROL                           
         GOTOR AVALITE,BATITC2H                                                 
         BNE   BATGLUER                                                         
         MVI   FVMINL,1            BATCH CASH CONTROL                           
         GOTOR AVALCSH,BATAMC2H                                                 
         BNE   BATGLUER                                                         
                                                                                
         XC    LSTBDELI,LSTBDELI                                                
         XC    LSTBHISN,LSTBHISN                                                
         XC    LSTBHDS1,LSTBHDS1                                                
         MVI   LSTBHDS2,BHDSGLUD   SET THIS IS A GLUPDATED BATCH                
         GOTOR AFVAL,BATACR2H      VALIDATE ACCRUAL OPTION                      
         BE    BATGLU08                                                         
         BH    BATGLUER                                                         
         OC    CSBTYP2,CSBTYP2     TEST AUTOMATIC ACCRUAL                       
         BZ    BATGLU10                                                         
         TM    BGLSTSAV+(LSTBHDS1-LSTTABD),BHDSACRU                             
         BZ    BATGLU10            UNLESS ORIGINAL WAS NOT ACCRUED              
         OI    LSTBHDS1,BHDSACRU   SET THIS IS AN ACCRUAL BATCH                 
         B     BATGLU10                                                         
                                                                                
BATGLU08 SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8              TEST FOR 'NO'                                
         BE    BATGLU10                                                         
         CLC   BC@NO(0),FVIFLD     'NO' IS ALWAYS VALID                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   BC@YES(0),FVIFLD    TEST FOR 'YES'                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     BATGLUER                                                         
         TM    CSBIND1,TYPIACRL    TEST ACCRUAL VALID FOR BATCH TYPE            
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACRNV)                                           
         B     BATGLUER                                                         
         OI    LSTBHDS1,BHDSACRU   SET THIS IS AN ACCRUAL BATCH                 
                                                                                
BATGLU10 XC    LSTBAPNO,LSTBAPNO   SET NO APPROVER FOR THIS BATCH               
         MVI   CSBTYP,BT25         SET THE TYPE                                 
         MVI   CSINDSG1,CSINDIUP                                                
         MVI   CSBGRUP,TBAGGENQ                                                 
         GOTOR AADDHDR,BCPARM,('FF',BATREF2H),0,BGLSTSAV                        
         BNE   BATGLUER            'TO' BATCH ALREADY EXISTS                    
                                                                                
         OI    CSINDSL1,CSIK2VAL   SET KEY 2 IS VALID                           
         MVC   BGLSTCUR,CSLSTCUR   SAVE NEW BATCH DETAILS                       
                                                                                
***********************************************************************         
* GET COMPANY LEVEL A25 PROFILES                                      *         
***********************************************************************         
                                                                                
         XC    BCWORK,BCWORK                                                    
         MVI   BCWORK,C'A'-X'40'                                                
         MVC   BCWORK+2(2),=C'25'                                               
         MVC   BCWORK+12(2),TWAAGY                                              
         GOTOR VGETPROF,BCPARM,(X'C0',BCWORK),BCBPROF1,VDMGR                    
                                                                                
         MVI   BGCORSW,0           CLEAR RUN LEVEL RULES                        
         CLI   BCBP06,C'Y'         POST BY OFFICE?                              
         BNE   *+8                 NO                                           
         OI    BGCORSW,BGCOROF     YES                                          
         CLI   BCBP06,C'T'         POST BY TRANSACTION OFFICE?                  
         BNE   *+8                 NO                                           
         OI    BGCORSW,BGCOROFT    YES                                          
                                                                                
         TM    BCCPYST4,CPYSOFF2   ON 2-BYTES OFFICES?                          
         BZ    *+8                 NO                                           
         OI    BGCORSW,BGCOROF     YES, FORCE POST BY OFFICE                    
                                                                                
***********************************************************************         
* READ COMPANY - SAVE G/L RULES ELEMENT & OFFICE                      *         
***********************************************************************         
         LA    R2,IOKEY                                                         
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,BCSPACES                                                  
         MVC   CPYKCPY,CUABIN                                                   
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T READ COMPANY RECORD                    
                                                                                
         XC    BGGLRC,BGGLRC       CLEAR AREA FOR E6 ELEMENT                    
         L     R2,AIO1                                                          
         LA    R2,CPYRFST                                                       
         USING GLRELD,R2                                                        
         SR    R0,R0                                                            
                                                                                
BATGLU12 CLI   GLREL,0             TEST EOR                                     
         BE    BATGLU14                                                         
         CLI   GLREL,GLRELQ        GET G/L OFFICE RULES ELEMENT (X'E6')         
         BE    *+14                                                             
         IC    R0,GLRLN                                                         
         AR    R2,R0                                                            
         B     BATGLU12                                                         
                                                                                
         IC    RF,GLRLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BGGLRC(0),GLREL     SAVE THE ELEMENT                             
                                                                                
BATGLU14 MVC   BGCOOFF,BCCPYOFF    SAVE COMPANY OFFICE ALSO                     
         DROP  R2                                                               
                                                                                
***********************************************************************         
* INITIALIZE TSAR                                                     *         
* SET TSAR PARAMETERS SO THAT ACCOUNT/OFFICE IS KEY OF RECORD         *         
***********************************************************************         
                                                                                
         L     R4,ATSABLK                                                       
         USING TSARD,R4                                                         
         MVC   TSACOM,ACOM                                                      
         MVI   TSRECI,0                                                         
         MVI   TSINDS,TSINODSK+TSIKEYUP                                         
         MVI   TSIND2,0                                                         
         MVI   TSKEYL,L'BGTKEY                                                  
         LHI   R1,BGTLNQ                                                        
         STCM  R1,3,TSRECL         SET RECORD LENGTH                            
         LA    R1,BGTSAV                                                        
         ST    R1,TSAREC           A(RECORD AREA)                               
         MVI   TSACTN,TSAINI       SET INITIALZE                                
         MVI   TSPAGN,TSNMAX                                                    
         GOTOR VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* READ BATCH HEADERS AND MARK UPDATED TO G/L                          *         
***********************************************************************         
                                                                                
         USING LSTTABD,R4                                                       
         LA    R4,BGLSTSAV                                                      
         MVC   BGTBASAV,IOKEY      SAVE BATCH RECORD KEY                        
         MVC   BGTBASIT,LSTBITMA   SAVE NUMBER OF ITEMS                         
         XC    BGTBACUR,BGTBACUR   SET FOR FIRST BATCH ITEM                     
                                                                                
         MVC   IODAOVER,LSTTDA                                                  
         GOTOR AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
                                                                                
         L     R2,AIO1                                                          
         USING TBARECD,R2                                                       
         LA    R2,TBARFST                                                       
         SR    R0,R0                                                            
                                                                                
         USING BHDELD,R2                                                        
BATGLU16 CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                GOT TO HAVE THIS                             
         CLI   0(R2),BHDELQ                                                     
         BE    BATGLU18                                                         
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     BATGLU16                                                         
                                                                                
BATGLU18 TM    BHDSTAT2,BHDSGENL   ALREADY UPDATED TO GL?                       
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ALRGL)                                           
         B     BATGLUAB                                                         
         OI    BHDSTAT2,BHDSGENL   TURN IT ON                                   
         GOTOR AIO,IOWRITE+IOACCMST+IO1                                         
                                                                                
***********************************************************************         
* READ BATCH HEADER USING AIO1                                        *         
* GET KEY OF RECORD, ASKKEY, FILTER FOR UNIT S, PASS TO ADDTSAR       *         
***********************************************************************         
                                                                                
         L     R2,AIO1             A(BATCH HEADER)                              
         USING TBARECD,R2                                                       
         B     BATGLU21                                                         
                                                                                
BATGLU20 SR    RF,RF               READ NEXT ITEM RECORD                        
         ICM   RF,3,TBAKTSEQ                                                    
         AHI   RF,1                                                             
         STCM  RF,3,TBAKTSEQ                                                    
                                                                                
         MVC   IOKEY(L'TBAKEY),TBAKEY                                           
         GOTOR AIO,IOREAD+IOACCDIR+IO1                                          
         BNE   BATGLU28                                                         
         TM    IOKEY+(TBAKESTA-TBARECD),TBAESLDE                                
         BNZ   BATGLU20                                                         
         GOTOR AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
BATGLU21 LA    R1,TBARFST          LOCATE SYSTEM KEY ELEMENT                    
         SR    R0,R0                                                            
                                                                                
         USING ASKELD,R1                                                        
BATGLU22 CLI   ASKEL,0                                                          
         BE    BATGLU20                                                         
         CLI   ASKEL,ASKELQ                                                     
         BE    BATGLU26                                                         
                                                                                
BATGLU24 IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BATGLU22                                                         
                                                                                
BATGLU26 CLI   ASKKEY+1,C'S'       UNIT S ONLY                                  
         BNE   BATGLU24                                                         
         CLC   ASKKEY+1(2),=C'S9'  SKIP THIS ONE THOUGH                         
         BE    BATGLU24                                                         
         GOTOR ADDTSAR             UPDATE RECORD AND ADD DATA TO TSAR           
         B     BATGLU24                                                         
                                                                                
***********************************************************************         
* GET RECORDS FROM TSAR AND DETERMINE WHAT LOOKUPS ARE NEEDED         *         
***********************************************************************         
                                                                                
BATGLU28 XC    BGSVLGR,BGSVLGR     CLEAR SAVE AREAS                             
         XC    BGSVACC,BGSVACC                                                  
         XC    BGSVOFF,BGSVOFF                                                  
         XC    BGSVPOF,BGSVPOF                                                  
                                                                                
         XC    BGBITMC,BGBITMC                                                  
         ZAP   BGBCSHC,=P'0'                                                    
                                                                                
         L     R4,ATSABLK                                                       
         USING TSARD,R4                                                         
         XC    TSRNUM,TSRNUM                                                    
                                                                                
BATGLU30 L     R4,ATSABLK                                                       
         LH    R1,TSRNUM                                                        
         AHI   R1,1                                                             
         OC    TSPRECN,TSPRECN                                                  
         BZ    BATGLU38            NOTHING TO POST                              
         CLM   R1,3,TSPRECN        ALL DONE?                                    
         BH    BATGLU38                                                         
         STH   R1,TSRNUM                                                        
         MVI   TSACTN,TSAGET                                                    
         GOTOR VTSAR,TSARD                                                      
         DROP  R4                                                               
                                                                                
         CLC   BGSVLGR,BGTPACC     CHECK LEGDER PROCESSED BEFORE                
         BE    BATGLU32                                                         
         MVC   BGSVLGR,BGTPACC                                                  
         GOTOR GETLED                                                           
         XC    BGSVACC,BGSVACC                                                  
         XC    BGSVOFF,BGSVOFF                                                  
         XC    BGSVPOF,BGSVPOF                                                  
                                                                                
BATGLU32 CLC   BGSVACC,BGTPACC                                                  
         BE    BATGLU34                                                         
         MVC   BGSVACC,BGTPACC                                                  
         GOTOR GETACN                                                           
         BNE   BATGLUAB                                                         
         XC    BGSVOFF,BGSVOFF                                                  
         XC    BGSVPOF,BGSVPOF                                                  
                                                                                
BATGLU34 CLC   BGSVOFF,BGTPOFF                                                  
         BE    BATGLU36                                                         
         MVC   BGSVOFF,BGTPOFF                                                  
         GOTOR GETOFF                                                           
                                                                                
BATGLU36 GOTOR READUP              READ ACCOUNTS UP FRONT                       
         BNE   BATGLUAB                                                         
         GOTOR POSTER                                                           
         B     BATGLU30            GET NEXT                                     
                                                                                
BATGLU38 GOTOR ABLDBAK,BGLSTCUR    GET BATCH HEADER                             
         GOTOR AIO,IORDUP+IOACCMST+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TBARECD,R2                                                       
         L     R2,AIO2                                                          
         MVI   TBARHSTA,TBAHSUPD   MARK UPDATED                                 
         MVC   TBAHRUDT,BCTODAYC   SET DATE UPDATED                             
                                                                                
         LA    R1,TBARFST          UPDATE BATCH HEADER ELEMENT                  
         USING BHDELD,R1                                                        
         SR    R0,R0                                                            
                                                                                
BATGLU40 CLI   BHDEL,0             TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ        TEST BATCH HEADER ELEMENT                    
         BE    BATGLU42                                                         
         IC    R0,BHDLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     BATGLU40                                                         
                                                                                
BATGLU42 MVC   BHDITEMC,BGBITMC    UPDATE ITEM COUNT                            
         ZAP   BHDCASHC,BGBCSHC    UPDATE CASH TOTAL                            
         GOTOR AIO,IOWRITE+IOACCMST+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,IOKEY                                                         
         L     R1,AIO2                                                          
         MVC   TBAKEY,0(R1)        GET KEY OF RECORD                            
         GOTOR AIO,IORDUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AIO2                                                          
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA                                                      
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR ABLDBAP,BGLSTCUR    GET PASSIVE POINTER                          
         GOTOR AIO,IORDUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AIO2                                                          
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA                                                      
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
BATGLUX  NI    CSINDSL1,FF-(CSIK2VAL)                                           
         MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         MVI   FVOMTYP,GTMINF                                                   
         MVC   CSLSTCUR,BGLSTSAV   RESTORE 'FROM' BATCH                         
         NI    CSINDSL1,CSIUSELC   START AGAIN NEXT TIME                        
         J     SETCUR                                                           
                                                                                
BATGLUER MVC   CSLSTCUR,BGLSTSAV   ERROR - RESTORE 'FROM' BATCH                 
         J     EXIT                                                             
                                                                                
BATGLUAB MVC   CSLSTCUR,BGLSTSAV   ERROR - RESTORE 'FROM' BATCH                 
         LA    R1,BATREF2H         ABEND FROM UPDATIVE FUNCTION                 
         ST    R1,FVADDR                                                        
         NI    CSINDSL1,FF-(CSIK2VAL)  CLEAR KEY 2 VALID                        
         OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         L     RD,BCSVRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  EXIT TO ROOT CALL POINT                      
         DROP  RB,RC                                                            
                                                                                
BGWORKD  DSECT                     ** BATCH GLU LOCAL W/S **                    
BGLSTCUR DS    XL(LSTTABL)         NEW BATCH DETAILS                            
         ORG   BGLSTCUR+(LSTBITMC-LSTTABD)                                      
BGBITMC  DS    XL(L'LSTBITMC)                                                   
         ORG   BGLSTCUR+(LSTBCSHC-LSTTABD)                                      
BGBCSHC  DS    PL(L'LSTBCSHC)                                                   
         ORG                                                                    
                                                                                
BGMASK   DS    XL2                 ACTION MASK                                  
BGCOOFF  DS    CL2                 COMPANY LEVEL OFFICE                         
BGLDOFF  DS    CL2                 LEDGER LEVEL OFFICE                          
BGACOFF  DS    CL2                 ACCOUNT LEVEL OFFICE                         
BGCLOFF  DS    CL2                 CLIENT LEVEL OFFICE                          
BGPOOFF  DS    CL2                 POSTING OFFICE                               
                                                                                
*        COMPANY LEVEL DATA                                                     
BGCORSW  DS    X                   COMPANY LEVEL STATUS                         
BGCOROF  EQU   X'80'               POST TO G/L BY OFFICE                        
BGCOROFT EQU   X'40'               POST TO G/L BY TRANSACTION OFFICE            
                                                                                
*        LEDGER LEVEL DATA                                                      
BGLGSW   DS    X                   LEDGER LEVEL STATUS                          
BGLGPR   EQU   X'80'               PRODUCTION OPTIONS                           
BGLGOF   EQU   X'40'               PRODUCTION RULES BY OFFICE                   
BGLGRL   EQU   X'20'               G/L RULES FOR OFFICE                         
BGLGRLT  EQU   X'10'               G/L RULES FOR TRANSACTION OFFICES            
                                                                                
BGLGNAME DS    CL36                LEDGER NAME                                  
BGLGMED  DS    X                   MEDIA DISPLACEMENT FOR LEDGER                
BGLGLVA  DS    X                   LEVEL A LENGTH                               
BGLGLVB  DS    X                   LEVEL B LENGTH                               
BGLGLVC  DS    X                   LEVEL C LENGTH                               
BGLGLVD  DS    X                   LEVEL D LENGTH                               
BGLGOFFP DS    XL1                 OFFICE POSITION                              
                                                                                
BGGLRC   DS    CL255               COMPANY LEVEL OFFICE OVERRIDE                
BGGLRL   DS    CL255               LEDGER LEVEL OFFICE OVERRIDE                 
                                                                                
*        POSTING LEVEL DATA                                                     
BGPSTAT  DS    X                   POSTING STATUS                               
BGPSPRD  EQU   X'80'               PRODUCTION LEDGER                            
BGPSLGS  EQU   X'40'               SOURCE IS THE LEDGER                         
BGPSACS  EQU   X'20'               SOURCE IS THE ACCOUNT                        
BGPSTRN  EQU   X'10'               POST BY TRANSACTION                          
BGPSOFL  EQU   X'08'               PUT OFFICE IN LAST BYTE OF ACCOUNT           
                                                                                
BGPTOA   DS    CL(L'TRNKULA)       ACCOUNT CODE                                 
BGPFRA   DS    CL(L'TRNKULC)       CONTRA ACCOUNT CODE                          
BGPNME   DS    CL(L'NAMEREC)                                                    
BGPOFF   DS    CL(L'TRNKOFF)                                                    
                                                                                
BGSTAR   DS    F                                                                
                                                                                
BGSVLGR  DS    CL(L'TRNKUNT+L'TRNKLDG)                                          
BGSVACC  DS    CL(L'TRNKULA)                                                    
BGSVOFF  DS    CL(L'TRNKOFF)                                                    
BGSVPOF  DS    CL(L'BGPOOFF)                                                    
                                                                                
BGRULTAB DS    (BGRULMAX)CL(BGRULLNQ) POSTING RULES                             
BGRULMAX EQU   100                                                              
BGRULSTR DS    A                                                                
                                                                                
BGKEY    DS    CL49                                                             
                                                                                
IOAREA   DS    CL400                                                            
                                                                                
         DS    0D                                                               
BGTSAV   DS    0XL(BGTLNQ)         TSAR RECORD                                  
BGTKEY   DS    0CL(L'TRNKULA+L'TRNKOFF+L'BGTPSEQ)                               
BGTPACC  DS    CL(L'TRNKULA)       ACCOUNT                                      
BGTPOFF  DS    CL(L'TRNKOFF)       OFFICE                                       
BGTPSEQ  DS    XL2                 SEQUENCE NUMBER                              
BGTPCAC  DS    CL(L'TRNKULC)       CONTRA ACCOUNT                               
BGTPREF  DS    CL(L'TRNREF)        REFERENCE                                    
BGTPMOS  DS    CL(L'TRNMOS)        MONTH OF SERVICE                             
BGTPDAT  DS    CL(L'TRNDATE)       DATE                                         
BGTPSTA  DS    CL(L'TRNSTAT)       STATUS                                       
BGTPAMT  DS    CL(L'TRNAMNT)       AMOUNT                                       
BGTLNQ   EQU   *-BGTKEY                                                         
                                                                                
BGWORKX  EQU   *                                                                
                                                                                
BGRULD   DSECT                                                                  
BGRULS   DS    0C                  POSTING RULES TABLE                          
BGRULFR  DS    CL10                FROM ACCOUNT                                 
BGRULFLN DS    CL1                 LENGTH-1 OF FROM ACCOUNT                     
BGRULTO  DS    CL14                TO ACCOUNT                                   
BGRULLNQ EQU   *-BGRULD                                                         
         EJECT                                                                  
BAT61    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LIST BATCH ITEM RECORDS                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING ILWORKD,RC                                                       
SCRSEL   TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         JNZ   SCRSEL02                                                         
         MVC   FVMSGNO,=AL2(AE$INACS)                                           
         LA    R0,BASACTH          SET CURSOR TO ACTION FIELD                   
         ST    R0,FVADDR                                                        
         J     EXIT                                                             
SCRSEL02 CLI   BCPFKEY,PFKAUTOQ    TEST SELECT ALL ITEMS                        
         JNE   SCRSEL04                                                         
         CLI   TWASCRN,ILSTSCRN    TEST NOT YET ENTERED SCREEN/SELECT           
         JE    SCRSEL04                                                         
         OI    ILFLAG,ILFAUTO      SET TO AUTO SELECT ALL                       
SCRSEL04 J     ITELST                                                           
*                                                                               
ITESEL   TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         JNZ   ITESEL02                                                         
         MVC   FVMSGNO,=AL2(AE$INACS)                                           
         LA    R0,BASACTH          SET CURSOR TO ACTION FIELD                   
         ST    R0,FVADDR                                                        
         J     EXIT                                                             
ITESEL02 TM    CSBIND5,TYPIASIR    TEST TYPE AUTO-SELECTS ALL                   
         JNZ   *+12                                                             
         CLI   BCPFKEY,PFKAUTOQ    TEST COPY/REVERSE ALL ITEMS                  
         JNE   ITESEL04                                                         
         CLI   TWASCRN,ILSTSCRN    TEST NOT YET ENTERED ITEM/SELECT             
         JE    ITESEL04                                                         
         OI    ILFLAG,ILFAUTO      SET TO AUTO COPY/REVERSE ALL                 
ITESEL04 J     ITELST                                                           
*                                                                               
ITELST   BASE  ,                                                                
         LA    R2,CSLSTCUR         R2=A(LIST TABLE ENTRY)                       
         USING LSTTABD,R2                                                       
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BNZ   ITELST04                                                         
         TM    CSINDSL1,CSIUENTK   TEST USER INVITED TO ENTER KEY               
         BNZ   ITELST02                                                         
         CLI   TWASCRN,HEADSCRN    TEST HEADER SCREEN LOADED                    
         BNE   *+12                                                             
         CLI   TWASCRF,1           AND FORMAT 1 (UNPROT KEY)                    
         BE    ITELST02                                                         
         GOTOR ADISHDR,1+X'80'     BUILD BATCH HEADER SCREEN                    
         LA    R0,BATREFH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         OI    CSINDSL1,CSIUENTK   SET USER INVITED TO ENTER KEY                
         J     EXIT                                                             
*                                                                               
ITELST02 LA    R1,LSTTABD          VALIDATE BATCH KEY                           
         ICM   R1,8,BCEFFS         SET TO TEST BATCH TYPE SECURITY              
         GOTOR AGETBAT                                                          
         JNE   EXIT                                                             
         GOTOR ADISHDR,1+X'40'     DISPLAY BATCH RECORD                         
         GOTOR ANTRSES,0           SAVE CURRENT SESSION                         
         OI    CSINDSL1,CSIUSELC   SET NESTED CALL                              
*                                                                               
ITELST04 CLI   TWASCRN,ILSTSCRN                                                 
         BE    ITELST06                                                         
         GOTOR ABLDBAK,LSTTABD                                                  
         MVC   ILTBASAV,IOKEY      SAVE BATCH RECORD KEY                        
         MVC   ILTBASIT,LSTBITMA   SAVE NUMBER OF ITEMS                         
         XC    ILTBACUR,ILTBACUR   SET FIRST BATCH ITEM                         
         GOTOR AOVRSCR,BCPARM,('ILSTSCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR ABLDDET,0                                                        
*                                                                               
ITELST06 LA    R3,ILOPTS                                                        
         ST    R3,AOVEROUT         SAVE A(OUTPUT AREA) FOR OPTIONS              
         LA    RF,ILODOPTL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ILODOPT-ILVALS(0,R3),BCSPACES                                    
         USING ILVALS,R3           R3=A(ITEM LIST OPTIONS)                      
         MVC   ILOREFND,BCEFFS     SET HIGH VALUES FOR END OF RANGES            
         MVC   ILODATND,BCEFFS                                                  
         MVC   ILOITEND,BCEFFS                                                  
         BASR  R1,0                                                             
         AHI   R1,ILOVAL-*                                                      
         ST    R1,AOVERVAL         SAVE A(OPTION VALIDATION ROUTINES)           
         XC    ILODIS,ILODIS                                                    
         GOTOR AFVAL,BASOPTH                                                    
         GOTOR AVALOPT,ILOTAB      VALIDATE OPTIONS                             
         JNE   EXIT                                                             
*                                                                               
         OC    ILODIS,ILODIS       TEST DISPLAY COLUMNS SET                     
         BNZ   ITELST08                                                         
         XC    BCWORK,BCWORK       SET PROFILE OR DEFAULT COLUMNS               
         LA    R1,DEFDISP          R1=A(DEFAULT DISPLAY COLUMNS)                
         LA    R0,L'DEFDISP                                                     
         GOTOR ADDCOL                                                           
         AHI   R1,1                                                             
         BCT   R0,*-8                                                           
         MVC   ILODISI(ILODISL),BCWORK                                          
*                                                                               
ITELST08 GOTOR READBTY             READ BATCH TYPE RECORD                       
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE LIST SUB-ACTIONS                                             *         
***********************************************************************         
         SPACE 1                                                                
ITELST20 MVI   FVINDX,0                                                         
         LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3          R3=A(LIST TABLE)                             
         NI    CSLTINDS,FF-(CSLTIFST+CSLTIHLD+CSLTIANY)                         
*                                                                               
         CLI   BCPFKEY,PFKRFSHQ    TEST REFRESH THE LIST                        
         BNE   ITELST22                                                         
         MVI   BCPFKEY,0           CLEAR PFKEY                                  
         OI    CSLTINDS,CSLTIFST   SET FIRST FOR LIST                           
         XC    TWASELCT,TWASELCT   CLEAR SELECTED RECORD COUNT                  
*                                                                               
ITELST22 CLC   ILOPTS(ILOKOPTL),ILOKOPTS                                        
         BE    *+14                                                             
         OI    CSLTINDS,CSLTIFST   SET FIRST FOR LIST IF KEY CHANGED            
         XC    TWASELCT,TWASELCT   CLEAR SELECTED RECORD COUNT                  
*                                                                               
         CLC   ILOPTS+ILOKOPTL(ILODOPTL),ILODOPT                                
         BE    *+8                                                              
         OI    CSLTINDS,CSLTIHLD   HOLD PAGE IF DISPLAY OPTIONS CHANGE          
*                                                                               
         TM    BCSCROLL,PFKIHORZ   TEST HORIZONTAL SCROLL                       
         BZ    *+8                                                              
         OI    CSLTINDS,CSLTIHLD   SET HOLD PAGE (NO VERTICAL SCROLL)           
*                                                                               
         MVC   ILVALS(ILVALSL),ILOPTS                                           
         TM    CSLTINDS,CSLTIFST   TEST CHANGE OF KEY FIELDS                    
         JNZ   ITELST64                                                         
*                                                                               
ITELST26 MVC   LSTTRECN,CSPAG#LO                                                
         SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LIST ENTRIES                    
         JZ    ITELST64                                                         
         LA    R2,ILIACT1H                                                      
         USING ILIACT1H,R2                                                      
         EJECT                                                                  
ITELST28 BASE  ,                                                                
         NI    ILFLAG,FF-ILFINPT   CLEAR USER INPUT FLAG BIT                    
         GOTOR ATSARIO,TSAGET      GET ITEM LIST RECORD                         
         TM    LSTIIND1,LSTI1DSP   TEST DISPLAY ONLY                            
         BO    ITELST54                                                         
         CLI   ILIACT1H+(FVILEN-FVIHDR),0                                       
         BE    *+12                                                             
         OI    ILFLAG,ILFINPT      SET USER INPUT                               
         B     ITELST30                                                         
         TM    CSLTINDS,CSLTIEOL+CSLTIEOP                                       
         BZ    ITELST54                                                         
         CLC   LSTTRECN,CSSEL#LO                                                
         BL    ITELST54                                                         
         CLC   LSTTRECN,CSSEL#HI                                                
         BH    ITELST54                                                         
         SR    R4,R4                                                            
         ICM   R4,3,CSSELMUL                                                    
         A     R4,AOVERSEL                                                      
         USING SELTABD,R4          R4=A(SELECT TABLE ENTRY)                     
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         MVC   ILIACT1,0(RE)                                                    
*                                                                               
ITELST30 GOTOR AFVAL,ILIACT1H                                                   
         BNE   ITELST54                                                         
         L     R4,AOVERSEL         R4=A(SELECT TABLE)                           
         CLI   FVIFLD,C'*'                                                      
         BE    ITELST56                                                         
         CLI   FVIFLD,C'-'                                                      
         BNE   *+12                                                             
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         B     ITELST56                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FVXLEN                                                      
         BZ    ITELST32                                                         
         LA    RE,FVIFLD(RF)       POINT TO END OF INPUT FIELD                  
         CLI   0(RE),C'+'                                                       
         BE    *+12                                                             
         CLI   0(RE),C'&&'                                                      
         BNE   ITELST32                                                         
         MVC   CSSEL#LO,LSTTRECN   SET LOW RECORD NUMBER                        
         MVC   CSSEL#HI,BCEFFS     SET DEFAULT HIGH VALUE                       
         LA    R1,CSLTIEOL                                                      
         CLI   0(RE),C'&&'                                                      
         BE    *+14                                                             
         MVC   CSSEL#HI,CSPAG#HI                                                
         LA    R1,CSLTIEOP                                                      
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         OI    CSLTINDS,0                                                       
         BCTR  RF,0                                                             
         STC   RF,FVXLEN                                                        
*                                                                               
ITELST32 CLI   SELTABD,EOT                                                      
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         J     EXIT                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RE)                                                  
         BNE   ITELST40                                                         
         MVC   BOWORK1(L'LSTBINDS),BCBATCUR+(LSTBINDS-LSTTABD)                  
         NC    BOWORK1(L'LSTBINDS),SELTRCI1                                     
         CLC   SELTRCI1,BOWORK1                                                 
         BE    ITELST34                                                         
         MVC   BOWORK1(L'LSTIIND1),LSTIIND1                                     
         NC    BOWORK1(L'LSTIIND1),SELTRCI2                                     
         CLC   SELTRCI2,BOWORK1                                                 
         BNE   ITELST40                                                         
*                                                                               
ITELST34 OC    SELTREC(L'SELTREC+L'SELTACT),SELTREC                             
         BZ    ITELST36                                                         
         GOTOR ATSTMIX,SELTPARM    VALIDATE RECORD/ACTION                       
         BNE   ITELST40                                                         
*                                                                               
ITELST36 CLC   LSTTRECN,CSSEL#LO   TEST SELECT MULTIPLE INPUT LINE              
         BNE   ITELST38                                                         
         L     RE,AOVERSEL                                                      
         LA    RF,SELTABD                                                       
         SR    RF,RE                                                            
         STCM  RF,3,CSSELMUL       SET DISPLACEMENT TO SELTAB ENTRY             
*                                                                               
ITELST38 MVC   ILMASK,SELTMASK     EXTRACT SELECT ACTION MASK                   
         NC    ILMASK,LSTTMASK     MERGE VALID ACTION MASK                      
         CLC   ILMASK,SELTMASK     TEST MASK MATCHES                            
         BE    ITELST42                                                         
*                                                                               
         CLC   LSTTRECN,CSSEL#LO                                                
         BE    ITELST54                                                         
         BL    ITELST40                                                         
         TM    ILFLAG,ILFINPT      TEST INPUT THIS TIME                         
         BZ    ITELST54                                                         
*                                                                               
ITELST40 LA    R4,SELTABL(R4)                                                   
         B     ITELST32                                                         
*                                                                               
ITELST42 MVC   CSLSTCUR,LSTTABD                                                 
         OI    CSLTINDS,CSLTIANY   SET LINE PROCESSED THIS SCREEN               
         OC    SELTREC(L'SELTREC+L'SELTACT),SELTREC                             
         JZ    ITELST50                                                         
         L     RE,ATWA                                                          
         LA    RF,ILIACT1H                                                      
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
         CLC   SELTRECA,=AL1(RECPST,ACTDSP)                                     
         BE    *+14                                                             
         CLC   SELTRECA,=AL1(RECPST,ACTANL)                                     
         BNE   ITELST48                                                         
         MVC   IODAOVER,LSTTDA     READ ITEM RECORD                             
         GOTOR AIO,IOGET+IOACCMST+IO1                                           
         L     R1,AIO1             R1=A(ITEM RECORD)                            
         LA    R1,TBARFST-TBARECD(R1)                                           
         USING ASKELD,R1                                                        
         SR    RF,RF                                                            
ITELST44 CLI   ASKEL,0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ASKEL,GINELQ        USE GINEL IF PRESENT                         
         BE    ITELST46                                                         
         CLI   ASKEL,ASKELQ        LOCATE FIRST ASKEL                           
         BE    ITELST46                                                         
         IC    RF,ASKLN                                                         
         AR    R1,RF                                                            
         B     ITELST44                                                         
ITELST46 GOTOR PSTBLD,ASKELD                                                    
         DROP  R1                                                               
ITELST48 GOTOR ANTRSES,SELTPARM                                                 
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* LINE ACTION COMPLETED                                               *         
***********************************************************************         
         SPACE 1                                                                
ITELSTR1 SR    R2,R2                                                            
         ICM   R2,3,CSSELACT                                                    
         A     R2,ATWA             R2=A(ACTION LINE)                            
         LA    R3,CSLSTCUR         R3=A(LIST TABLE ENTRY)                       
         SR    R4,R4                                                            
         ICM   R4,3,CSSELCUR                                                    
         A     R4,AOVERSEL         R4=A(SELECT TABLE ENTRY)                     
         SR    R0,R0                                                            
         ICM   R0,1,CSSELREM       R0=NUMBER OF LINES REMAINING                 
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   SELTRECA,=AL1(RECITE,ACTCHA)                                     
         JNE   ITELST52                                                         
         CLI   BCPFKEY,PFKQUITQ    TEST QUIT OR NEXT                            
         JE    ITELST52                                                         
         CLI   BCPFKEY,PFKNEXTQ                                                 
         JE    ITELST52                                                         
         MVC   IODAOVER,CSLSTCUR+(LSTTDA-LSTTABD)                               
         GOTOR AIO,IOGET+IOACCMST+IO1                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR LSTBLD,CSLSTCUR     NO - UPDATE LIST TABLE ENTRY                 
         J     ITELST52                                                         
*                                                                               
ITELST50 L     RF,BCRELO                                                        
         ICM   RF,8,SELTRTN                                                     
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         BASR  RE,RF                                                            
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         JNE   EXIT                                                             
*                                                                               
ITELST52 MVI   ILIACT1,C'*'                                                     
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         MVC   ILIACT1+1(L'ILIACT1-1),0(RE)                                     
         GOTOR ATSARIO,TSAPUT                                                   
         OI    ILIACT1H+(FVOIND-FVIHDR),FVOXMT                                  
         GOTOR BLDLIN,ILIACT1H                                                  
         CLI   BCPFKEY,PFKQUITQ                                                 
         JNE   ITELST56                                                         
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         J     ITELST60                                                         
*                                                                               
ITELST54 XC    ILIACT1,ILIACT1                                                  
         MVI   ILIACT1,C'*'                                                     
         OI    ILIACT1H+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
ITELST56 LA    R2,ILIACT2H                                                      
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,LSTTRECN                                                    
         BRCT  R0,ITELST28                                                      
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
         TM    CSLTINDS,CSLTIEOF   TEST END OF FILE REACHED                     
         JZ    ITELST58                                                         
         CLC   LSTTRECN,CSHIRECN   TEST THIS IS LAST RECORD                     
         JNE   ITELST58                                                         
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         DROP  R2                                                               
*                                                                               
ITELST58 TM    CSLTINDS,CSLTIANY                                                
         JZ    ITELST64                                                         
         TM    CSLTINDS,CSLTIEOL                                                
         JNZ   ITELST64                                                         
*                                                                               
ITELST60 MVC   CSLSTCUR,BCBATCUR                                                
         GOTOR ABLDDET,0                                                        
         MVC   CSLSTCUR,BCITECUR                                                
         CLC   ILODIS,BCSPACES     ARE ALTERNATIVE DISPLAY OPTIONS SET?         
         JNE   ITELST94            YES - REDISPLAY ENTIRE SCREEN                
         LA    R1,BASOPTH          CURSOR DEFAULTS TO OPTIONS FIELD             
         SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM                                                    
         JZ    ITELST62            NO LINES DISPLAYED                           
         LA    R1,ILIACT1H         LOOK FOR FIRST UNPROT ACTION FIELD           
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         JZ    ITELST62                                                         
         LA    R1,ILIACT2H-ILIACT1H(R1)                                         
         BRCT  R0,*-12                                                          
         LA    R1,BASSCRH          IF NONE, USE SCROLL FIELD                    
ITELST62 ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE SCROLLING                                                    *         
***********************************************************************         
         SPACE 1                                                                
ITELST64 BASE  ,                                                                
         LA    R2,IOKEY                                                         
         USING TBARECD,R2          R2=A(BATCH ITEM RECORD KEY)                  
         MVC   ILPAG#LO,CSPAG#LO   SAVE LAST LOW & HIGH RECORDS                 
         MVC   ILPAG#HI,CSPAG#HI                                                
         XC    CSPAG#LO,CSPAG#LO   CLEAR LOW & HIGH VALUES FOR PAGE             
         XC    CSPAG#HI,CSPAG#HI                                                
         TM    CSLTINDS,CSLTIFST   TEST FIRST FOR LIST                          
         BZ    ITELST66                                                         
         MVI   CSLTINDS,0          RESET LIST INDICATORS                        
         MVI   CSLSTNUM,0          CLEAR NUMBER OF LIST ENTRIES                 
         MVC   CSHIRECN,CSPSRECN   RESET HIGH RECORD NUMBER                     
         XC    ILTBACUR,ILTBACUR   CLEAR ITEM SEQUENCE NUMBER                   
         XC    ILHISNO,ILHISNO     CLEAR HIGH SCREEN NUMBER                     
         B     ITELST80                                                         
*                                                                               
ITELST66 TM    BCSCROLL,PFKIHORZ   TEST HORIZONTAL SCROLLING                    
         BNZ   *+14                                                             
         MVC   ILSCRNUM,BCSCRNUM   EXTRACT SCROLL MAGNITUDE                     
         B     *+8                                                              
         MVI   ILSCRNUM,PFKIPAGE                                                
         TM    CSLTINDS,CSLTIHLD   TEST HOLD PAGE IF DISPLAY CHANGED            
         BZ    ITELST68                                                         
         MVC   CSPAG#LO,ILPAG#LO   RESTORE LAST LOW & HIGH RECORDS              
         MVC   CSPAG#HI,ILPAG#HI                                                
         B     ITELST90                                                         
*                                                                               
ITELST68 TM    BCSCROLL,PFKIUPDN   TEST SCROLL UP OR DOWN                       
         BZ    ITELST72                                                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,CSPSRECN                                                    
         TM    ILSCRNUM,PFKIMAXN   TEST SCROLL FIRST                            
         BNZ   ITELST70                                                         
         LA    RF,ILISTMAX         SCROLL UP (BACKWARDS)                        
         TM    ILSCRNUM,PFKIHALF                                                
         BZ    *+8                                                              
         SRL   RF,1                                                             
         TM    ILSCRNUM,X'0F'                                                   
         BZ    *+8                                                              
         IC    RF,ILSCRNUM                                                      
         LA    RF,1(RF)                                                         
         SR    RE,RE                                                            
         ICM   RE,3,ILPAG#LO                                                    
         SR    RE,RF               BACK-UP TO RECORD NUMBER-1                   
         BM    *+12                                                             
         CLM   RE,3,CSPSRECN       TEST NOT < LOW RECORD FOR SESSION            
         BNL   ITELST70                                                         
         SR    RE,RE               SET TO START FROM LOW RECORD                 
         ICM   RE,3,CSPSRECN                                                    
*                                                                               
ITELST70 STCM  RE,3,LSTTRECN                                                    
         MVI   CSLSTNUM,0                                                       
         B     ITELST76                                                         
*                                                                               
ITELST72 SR    R1,R1                                                            
         ICM   R1,1,CSLSTNUM       PICK UP NUMBER OF ENTRIES IN PAGE            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   CSLSTNUM,0                                                       
         TM    ILSCRNUM,X'0F'      TEST SCROLL AMOUNT SPECIFIED                 
         BZ    *+16                                                             
         CLM   R1,1,ILSCRNUM       TEST SCROLL EXCEEDS ACTUAL AMOUNT            
         BL    ITELST74                                                         
         IC    R1,ILSCRNUM                                                      
         TM    ILSCRNUM,PFKIHALF   TEST HALF PAGE SCROLL                        
         BZ    *+8                                                              
         SRL   R1,1                                                             
         SH    R1,=H'1'                                                         
         BM    ITELST74                                                         
         SR    R0,R0                                                            
         ICM   R0,3,ILPAG#LO                                                    
         AR    R1,R0                                                            
         STCM  R1,3,LSTTRECN                                                    
         TM    CSLTINDS,CSLTIEOF   TEST END OF FILE ENCOUNTERED                 
         BZ    ITELST76                                                         
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         BL    ITELST76                                                         
*                                                                               
ITELST74 MVC   LSTTRECN,CSPSRECN   SET TO DISPLAY FIRST PAGE                    
*                                                                               
ITELST76 SR    RE,RE               BUMP TO NEXT RECORD                          
         ICM   RE,3,LSTTRECN                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSHIRECN   TEST IN TSAR BUFFER                          
         BH    ITELST78            NO - GET NEXT BATCH ITEM RECORD              
         GOTOR LSTADD,1            ADD ENTRY TO LSTTAB                          
         BE    ITELST76                                                         
         B     ITELST90                                                         
*                                                                               
ITELST78 TM    CSLTINDS,CSLTIEOF   TEST EOF ENCOUNTERED                         
         BNZ   ITELST90                                                         
*                                                                               
ITELST80 MVC   TBAKEY,ILTBASAV     SET NEXT BATCH ITEM KEY                      
         SR    R1,R1                                                            
         ICM   R1,3,ILTBACUR                                                    
         LA    R1,1(R1)                                                         
         CLM   R1,3,ILTBASIT                                                    
         BH    ITELST82            NO FURTHER ITEMS FOR THIS BATCH              
         STCM  R1,3,TBAKTSEQ                                                    
         GOTOR AIO,IORDD+IOACCDIR+IO1                                           
         BE    ITELST84                                                         
         TM    IOERR,IOEDEL                                                     
         BNZ   ITELST84                                                         
         DC    H'0'                                                             
*                                                                               
ITELST82 OI    CSLTINDS,CSLTIEOF   SET END OF FILE                              
         B     ITELST90                                                         
*                                                                               
ITELST84 GOTOR AIO,IOGET+IOACCMST+IO1                                           
         GOTOR FLTITE                                                           
         BNE   ITELST86                                                         
         XC    LSTTABD(LSTTABL),LSTTABD                                         
         SR    RE,RE                                                            
         ICM   RE,3,CSHIRECN                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,LSTTRECN                                                    
         GOTOR LSTADD,0            ADD ENTRY TO LIST                            
         BNE   ITELST90                                                         
*                                                                               
ITELST86 MVC   ILTBACUR,TBAKTSEQ                                                
         B     ITELST80                                                         
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY PAGE OF DATA                                                *         
***********************************************************************         
         SPACE 1                                                                
ITELST90 GOTOR BLDDIS              BUILD COLUMN HEADING & DISPLACEMENTS         
         MVCDD ILIIHDT,AC#ITEM,L                                                
         MVCDD ILIIHDU,AC#ITEM,LU                                               
*&&UK                                                                           
         TM    BCBATCUR+(LSTBIND2-LSTTABD),LSTBIMLT                             
         JZ    *+16                                                             
         MVCDD ILIIHDT,AC#SEQNO                                                 
         MVCDD ILIIHDU,AC#SEQNO,LU                                              
*&&                                                                             
         OI    ILIIHDTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    ILIIHDUH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   ILIHED1,ILDHEAD1                                                 
         OI    ILIHED1H+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   ILIHED2,ILDHEAD2                                                 
         OI    ILIHED2H+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
         LA    R1,ILIACT1H         CLEAR & TRANSMIT THE DISPLAY SCREEN          
         SR    RF,RF                                                            
ITELST92 ICM   RF,1,0(R1)                                                       
         JZ    ITELST94                                                         
         OI    6(R1),X'80'                                                      
         SHI   RF,9                                                             
         TM    1(R1),X'02'                                                      
         JZ    *+8                                                              
         SHI   RF,8                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         J     ITELST92                                                         
*                                                                               
ITELST94 CLC   CSHIRECN,CSPSRECN   TEST ANY RECORDS FOUND                       
         JNE   ITELST96                                                         
         MVC   FVMSGNO,=AL2(AI$NOITF)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R0,BASOPTH                                                       
         ST    R0,FVADDR                                                        
         XC    ILVALS(ILVALSL),ILVALS                                           
         J     EXIT                                                             
*                                                                               
ITELST96 MVC   LSTTRECN,CSPAG#LO                                                
         CLC   LSTTRECN,CSSEL#HI   TEST > HIGH MULTIPLE SELECT                  
         JNH   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LINES TO DISPLAY                
         JNZ   *+6                                                              
         DC    H'0'                DIE FOR NOW IF NO DISPLAY LINES              
         LA    R2,ILIACT1H                                                      
         USING ILIACT1H,R2         R2=A(SCREEN LINE)                            
*                                                                               
ITELST98 GOTOR ATSARIO,TSAGET      GET ITEM LIST RECORD                         
         GOTOR BLDLIN,ILIACT1H                                                  
         MVI   ILIACT1H+(FVILEN-FVIHDR),0                                       
         LA    R2,ILIACT2H                                                      
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN                                                    
         AHI   R1,1                                                             
         STCM  R1,3,LSTTRECN                                                    
         BRCT  R0,ITELST98                                                      
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSSEL#LO   TEST < LOW MULTIPLE SELECT                   
         JNL   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         TM    CSLTINDS,CSLTIEOL   TEST SELECT TO END OF LIST                   
         JNZ   ITELST26                                                         
         DROP  R2                                                               
*                                                                               
         TM    ILFLAG,ILFAUTO      TEST AUTO COPY/REVERSE ALL ITEMS             
         JZ    ITELSTX                                                          
         NI    ILFLAG,FF-ILFAUTO                                                
         MVC   ILIACT1,BC@YES                                                   
         MVI   ILIACT1+L'ILIACT1-1,C'&&'                                        
         OI    ILIACT1H+(FVOIND-FVIHDR),FVOXMT                                  
         MVI   ILIACT1H+(FVILEN-FVIHDR),L'ILIACT1                               
         J     ITELST26            RECURSE TO AUTO COPY/REVERSE ALL             
*                                                                               
ITELSTX  LA    R1,BASOPTH          CURSOR DEFAULTS TO OPTIONS FIELD             
         SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM                                                    
         JZ    ITELSTX2            NO LINES DISPLAYED                           
         LA    R1,ILIACT1H         LOOK FOR FIRST UNPROT ACTION FIELD           
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         JZ    ITELSTX2                                                         
         LA    R1,ILIACT2H-ILIACT1H(R1)                                         
         BRCT  R0,*-12                                                          
         LA    R1,BASSCRH          IF NONE, USE SCROLL FIELD                    
ITELSTX2 ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$IDINX)                                           
         MVI   FVOMTYP,GTMINF                                                   
         CLI   CSACT,ACTSEL                                                     
         JNE   *+10                                                             
         MVC   FVMSGNO,=AL2(AI$MKIEP)                                           
         TM    CSLTINDS,CSLTIEOF   TEST END-OF-FILE ENCOUNTERED                 
         JZ    EXIT                                                             
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         JNE   EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$ITDNM)                                           
         CLI   CSACT,ACTSEL                                                     
         JNE   EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$MKINM)                                           
         J     EXIT                                                             
         DROP  R3,RC                                                            
         SPACE 1                                                                
ILISTMAX EQU   (ILIPFKH-ILIACT1H)/(ILIACT2H-ILIACT1H)                           
         SPACE 1                                                                
ILWORKD  DSECT                     ** ITELST LOCAL W/S **                       
ILABIAEL DS    A                   A(BATCH ITEM AMOUNT ELEMENT)                 
ILAGINEL DS    A                   A(GROUP INVOICE NUMBER ELEMENT)              
ILAASK1  DS    A                   A(ACCOUNT SYSTEM KEY ELEMENT)                
ILABICEL DS    A                   A(BATCH ITEM CHEQUE ELEMENT)                 
ILPAG#LO DS    XL2                 LOW RECORD ON CURRENT PAGE                   
ILPAG#HI DS    XL2                 HIGH RECORD ON CURRENT PAGE                  
ILOPTS   DS    XL(ILVALSL)         KEY & OPTIONS                                
ILFLAG   DS    XL1                 INDICATOR                                    
ILFINPT  EQU   X'80'               USER INPUT THIS TIME                         
ILFAUTO  EQU   X'40'               AUTO COPY/REVERSE ALL ITEMS                  
ILFFIRST EQU   X'20'               USE FIRST ASKEL                              
ILFREAD  EQU   X'01'               BATCH TYPE RECORD HAS BEEN READ              
ILMASK   DS    XL(L'LSTTMASK)      VALID ACTION MASK WORK AREA                  
ILSCRNUM DS    XL1                 SCROLL MAGNITUDE                             
ILWORKX  EQU   *                                                                
BAT61    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CREATE AN ITEM LIST TABLE ELEMENT                        *         
*                                                                     *         
* NTRY - R1=ZERO TO CREATE AN ENTRY, NON-ZERO TO POST AN ENTRY        *         
*        IOKEY CONTAINS BATCH ITEM RECORD KEY (IF R1=ZERO)            *         
*        LSTTRECN IS RECORD NUMBER OR LIST ENTRY (IF R1=NON-ZERO)     *         
*        CSLSTNUM IS NUMBER OF ENTRIES IN PAGE SO FAR                 *         
* EXIT - CC=NOT EQUAL IF PAGE FULL                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING ILWORKD,RC          RC=A(LOCAL W/S)                              
LSTADD   NTR1  ,                                                                
         LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3          R3=A(CURRENT LIST ENTRY)                     
         SR    RE,RE                                                            
         ICM   RE,1,CSLSTNUM                                                    
         LA    R0,1(RE)                                                         
         CLM   R0,1,=AL1(ILISTMAX) TEST TABLE FULL                              
         JH    LSTADDN                                                          
         STC   R0,CSLSTNUM                                                      
         LTR   R1,R1               TEST CREATE/POST                             
         JNZ   LSTADD02                                                         
         MVI   LSTTRTYP,RECITE                                                  
         MVC   LSTTDA,IOKEY+(TBAKDA-TBARECD)                                    
         MVC   LSTISEQ,IOKEY+(TBAKTSEQ-TBARECD)                                 
*                                                                               
         GOTOR LSTBLD,LSTTABD                                                   
         GOTOR ATSARIO,TSAADD      ADD LIST ENTRY TO TSAR                       
*                                                                               
LSTADD02 OC    CSPAG#LO,CSPAG#LO   SET LOW & HIGH RECORDS FOR PAGE              
         JNZ   *+10                                                             
         MVC   CSPAG#LO,LSTTRECN                                                
         MVC   CSPAG#HI,LSTTRECN                                                
*                                                                               
LSTADDY  CR    RB,RB               SET CC=EQUAL                                 
         J     EXIT                                                             
*                                                                               
LSTADDN  LTR   RB,RB               SET CC=NOT EQUAL FOR FULL PAGE               
         J     EXIT                                                             
         DROP  R3,RC                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CREATE OR UPDATE A LIST ENTRY                            *         
***********************************************************************         
         SPACE 1                                                                
         USING ILWORKD,RC                                                       
LSTBLD   NTR1  ,                                                                
         MVC   BCWORK,IOKEY        SAVE IOKEY                                   
         LR    R3,R1                                                            
         USING LSTTABD,R3                                                       
*                                                                               
         XC    ILABIAEL,ILABIAEL                                                
         XC    ILABICEL,ILABICEL                                                
         XC    ILAGINEL,ILAGINEL                                                
         XC    ILAASK1,ILAASK1                                                  
*                                                                               
         L     RF,AIO1                                                          
         USING TBARECD,RF          RF=A(BATCH ITEM RECORD KEY)                  
         MVC   LSTTSTAT,TBARESTA                                                
         TM    TBARESTA,TBAESAUT   TEST/SET 'AUTO GENERATED' ITEM               
         JZ    *+8                                                              
         OI    LSTIIND1,LSTI1AUT                                                
*                                                                               
         LA    RF,TBARFST                                                       
         SR    R0,R0                                                            
         USING BIAELD,RF                                                        
LSTBLD02 CLI   BIAEL,0             TEST END OF RECORD                           
         JE    LSTBLD06                                                         
         CLI   BIAEL,BTAELQ        TEST BATCH TOTAL AMOUNT ELEMENT              
         JE    *+12                                                             
         CLI   BIAEL,BIAELQ        TEST BATCH ITEM AMOUNT ELEMENT               
         JNE   *+12                                                             
         ST    RF,ILABIAEL                                                      
         J     LSTBLD05                                                         
         CLI   BIAEL,BICELQ        TEST BATCH ITEM CHEQUE ELEMENT               
         JNE   *+12                                                             
         ST    RF,ILABICEL                                                      
         J     LSTBLD05                                                         
         CLI   BIAEL,GINELQ        TEST GROUP INVOICE NUMBER ELEMENT            
         JNE   *+16                                                             
         ST    RF,ILAGINEL                                                      
         OI    LSTIIND1,LSTI1SIN                                                
         J     LSTBLD04                                                         
         CLI   BIAEL,ASKELQ        TEST ACCOUNT SYSTEM KEY ELEMENT              
         JNE   LSTBLD04                                                         
         SR    RE,RE               INCREMENT NUMBER OF ASKELS ON RECORD         
         IC    RE,LSTINASK                                                      
         LA    RE,1(RE)                                                         
         STC   RE,LSTINASK                                                      
         TM    LSTIIND1,LSTI1SIN                                                
         JZ    *+6                                                              
         DC    H'0'                                                             
         OC    ILAASK1,ILAASK1     TEST FIRST ASKEL                             
         JNZ   LSTBLD05                                                         
         ST    RF,ILAASK1                                                       
         J     LSTBLD05                                                         
*                                                                               
         USING AFCELD,RF                                                        
LSTBLD04 CLI   AFCEL,AFCELQ        EXTRACT FOREIGN CURRENCY VALUES              
         JNE   LSTBLD05                                                         
         MVC   LSTIFCUR,AFCCURR                                                 
         ZAP   LSTIFAMT,AFCAMNT                                                 
*                                                                               
LSTBLD05 IC    R0,AFCLN                                                         
         AR    RF,R0                                                            
         J     LSTBLD02                                                         
*                                                                               
LSTBLD06 ICM   RF,15,ILABIAEL      RF=A(BATCH ITEM AMOUNT ELEMENT)              
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING BIAELD,RF                                                        
         ZAP   LSTIAMT,BIAAMT      EXTRACT VALUES                               
         MVC   LSTIREF,BIAREF                                                   
         CLI   CSACT,ACTSEL        TEST SELECT ACTION                           
         JNE   LSTBLD08                                                         
         TM    LSTIIND1,LSTI1AUT   SET DISPLAY ONLY IF 'AUTO' ITEM              
         JZ    LSTBLD08                                                         
         OI    LSTIIND1,LSTI1DSP                                                
*                                                                               
LSTBLD08 CLI   BIALN,BIALN2Q       TEST EXTENDED ELEMENT LENGTH                 
         JL    LSTBLD10                                                         
         MVC   LSTISNO,BIASNO      SET SCREEN & LINE NUMBER                     
         MVC   LSTISIS,BIASIS                                                   
*                                                                               
         CLC   CSRECACT,=AL1(RECSCR,ACTSEL)                                     
         JNE   LSTBLD10                                                         
         TM    BCBATCUR+(LSTBIND2-LSTTABD),LSTBIMLT                             
         JZ    LSTBLD10                                                         
         CLC   ILHISNO,BIASNO      TEST FIRST FOR NEW SCREEN                    
         JNL   *+14                                                             
         MVC   ILHISNO,BIASNO      SAVE NEW HIGH SCREEN NUMBER                  
         J     LSTBLD10                                                         
         OI    LSTIIND1,LSTI1DSP   SET DISPLAY ONLY                             
         DROP  RF                                                               
*                                                                               
LSTBLD10 ICM   RF,15,ILAGINEL      RF=A(GROUP INVOICE NUMBER ELEMENT)           
         JZ    LSTBLD12                                                         
         LA    R1,IOKEY            BUILD GROUP INVOICE PASSIVE KEY              
         USING GINPASD,R1                                                       
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,CUABIN                                                   
         MVC   GINPINV,GININV-GINELD(RF)                                        
         GOTOR AIO,IOHI+IOACCMST+IO2                                            
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO2                                                          
         USING TRNRECD,R1                                                       
         MVC   LSTIDATE,TRNKDATE                                                
         MVC   LSTIACC1,TRNKCULA                                                
         MVC   LSTIACC2,TRNKCULC                                                
         DROP  R1                                                               
         J     LSTBLD14                                                         
*                                                                               
LSTBLD12 ICM   RF,15,ILAASK1       RF=A(ACCOUNT SYSTEM KEY ELEMENT)             
         JZ    LSTBLD16                                                         
         MVC   LSTIDATE,ASKKEY+(TRNKDATE-TRNRECD)-ASKELD(RF)                    
         GOTOR GETASK,LULTACC1                                                  
         MVC   LSTIACC1,0(R1)                                                   
         GOTOR GETASK,LULTACC2                                                  
         MVC   LSTIACC2,0(R1)                                                   
*                                                                               
LSTBLD14 GOTOR ASETMSK,LSTTABD     SET ITEM MASK                                
         CLC   CSRECACT,=AL1(RECSCR,ACTSEL)                                     
         JNE   LSTBLD18                                                         
         MVC   BCWORK(L'LSTTMASK),LSTTMASK                                      
         NC    BCWORK(L'LSTTMASK),=AL2(LMISESEL)                                
         JNZ   LSTBLD18                                                         
         OI    LSTIIND1,LSTI1DSP   SET DISPLAY ONLY                             
         J     LSTBLD18                                                         
*                                                                               
         USING BICELD,RF                                                        
LSTBLD16 ICM   RF,15,ILABICEL      EXTRACT DATE FROM CHEQUE ELEMENT             
         JZ    LSTBLD18                                                         
         MVC   LSTIDATE,BICDAT                                                  
         DROP  RF                                                               
*                                                                               
LSTBLD18 DS    0H                                                               
*                                                                               
LSTBLDX  MVC   IOKEY,BCWORK        RESTORE IOKEY                                
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET THE A(ASKEL) AND THE A(ACCOUNT CODE MATCHED)         *         
*                                                                     *         
* NTRY: R1=LIST TYPE                                                  *         
* EXIT: R1=A(MATCHED C/U/L/ACCOUNT CODE)                              *         
*       R2=A(MATCHED ASKEL)                                           *         
***********************************************************************         
         SPACE 1                                                                
GETASK   NTR1  ,                                                                
         STC   R1,BCBYTE1          BCBYTE1=LIST TYPE                            
         TM    ILFLAG,ILFREAD      TEST BATCH TYPE RECORD HAS BEEN READ         
         JNZ   *+8                                                              
         GOTOR READBTY                                                          
*                                                                               
         TM    ILFLAG,ILFFIRST     TEST USE FIRST ASKEL                         
         JO    GETASKF                                                          
         L     R3,AIOA                                                          
         LA    R3,BTYDATA-BTYREC(R3)                                            
         USING LULELD,R3                                                        
         XR    RF,RF                                                            
GASK02   CLI   LULELD,EOT                                                       
         JE    GETASKF                                                          
         IC    RF,LULLN                                                         
         CLI   LULEL,LULELQ                                                     
         JNE   *+14                                                             
         CLC   LULTYPE,BCBYTE1                                                  
         JE    GASK04                                                           
         BRXH  R3,RF,GASK02                                                     
*                                                                               
GASK04   XC    BOELEM,BOELEM                                                    
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   BOELEM(0),LULELD                                                 
         LA    R3,BOELEM                                                        
*                                                                               
GASK12   CLI   LULDATA,0                                                        
         JE    GETASKF                                                          
*                                                                               
         L     R2,ILAASK1                                                       
         USING ASKEL,R2                                                         
         XR    RF,RF                                                            
*                                                                               
GASK14   CLI   ASKEL,ASKELQ                                                     
         JNE   GASK18                                                           
         LA    R1,ASKKEY+(TRNKCULA-TRNRECD)                                     
         CLI   LULDTYPE,LULDTCAC   TEST CONTRA ACCOUNT CODE                     
         JNE   *+8                                                              
         LA    R1,ASKKEY+(TRNKCULC-TRNRECD)                                     
         CLC   LULDUL,1(R1)        MATCH ON UNIT/LEDGER                         
         JE    GETASKY                                                          
*                                                                               
         IC    RF,ASKLN            BUMP R2 TO NEXT ELEMENT                      
         BRXH  R2,RF,GASK14                                                     
*                                                                               
GASK18   LA    R3,L'LULDATA(R3)    BUMP R3 TO NEXT SUB-ELEMENT                  
         J     GASK12                                                           
*                                                                               
GETASKF  L     R2,ILAASK1          USE FIRT ASKEL                               
         LA    R1,ASKKEY+(TRNKCULA-TRNRECD)                                     
         CLI   BCBYTE1,LULTACC2    USE CONTRA-ACCOUNT FOR ACCOUNT#2             
         JNE   GETASKY                                                          
         LA    R1,ASKKEY+(TRNKCULC-TRNRECD)                                     
*                                                                               
GETASKY  XIT1  REGS=(R1,R2)                                                     
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ BATCH TYPE RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
READBTY  NTR1  ,                                                                
         OI    ILFLAG,ILFFIRST+ILFREAD                                          
*                                                                               
         LA    R2,IOKEY                                                         
         USING BTYREC,R2                                                        
         XC    BTYKEY,BTYKEY                                                    
         MVI   BTYKTYP,BTYKTYPQ                                                 
         MVC   BTYKBTY,CSBTYP                                                   
         MVC   BTYKCTRY,CUCTRY                                                  
         MVC   IOADDR,AIOA                                                      
         GOTOR AIO,IORD+IOCTFILE                                                
         JNE   READBTYX                                                         
*                                                                               
         L     R2,AIOA                                                          
         LA    R3,BTYDATA                                                       
         USING LULELD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
RBTY02   CLI   LULEL,0                                                          
         JE    READBTYX                                                         
         CLI   LULEL,LULELQ                                                     
         JE    *+12                                                             
         IC    RF,LULLN                                                         
         BRXH  R3,RF,RBTY02                                                     
*                                                                               
         CLI   LULTYPE,LULTFRST                                                 
         JE    READBTYX                                                         
         NI    ILFLAG,FF-ILFFIRST                                               
*                                                                               
READBTYX J     EXIT                                                             
         DROP  R2,R3,RC                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL APPLICATION OVERLAY                                 *         
*                                                                     *         
* ON ENTRY TO APPLICATION OVERLAY THE FOLLOWING VALUES ARE SET        *         
***********************************************************************         
         SPACE 1                                                                
APHOOK   NTR1  ,                                                                
         STC   R1,BOMODE                                                        
         XC    FVMSGNO,FVMSGNO                                                  
         LR    R0,R6                                                            
         SR    R6,R6               CLEAR R6 FOR OLD GETACC CALLS                
         GOTOR BONTRYA,BCPARM,TWAD,WORKD                                        
         LR    R6,R0                                                            
         CLC   FVMSGNO,=AL2(FVFOK)                                              
APHOOKX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ TRANSACTIONS FROM ASKKEY 0(R2) USING AIO2                      *         
* MARK TRANSACTIONS UPDATED TO G/L AND WRITE BACK                     *         
* FORMAT KEY AND ADD RECORD TO TSAR                                   *         
***********************************************************************         
ADDTSAR  NTR1  ,                                                                
         USING BGWORKD,RC          RC=A(LOCAL W/S)                              
         USING ASKELD,R1                                                        
         MVC   IOKEY,ASKKEY        READ TRANSACTION RECORD                      
         GOTO1 AIO,IORDUP+IOACCMST+IO2                                          
         JE    *+6                                                              
         DC    H'0'                CAN'T READ ACCMST                            
                                                                                
         USING TRNRECD,R2                                                       
         L     R2,AIO2                                                          
         LA    R4,TRNRFST                                                       
         SR    R0,R0                                                            
                                                                                
         USING TRNELD,R4                                                        
ADDT02   CLI   TRNEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                MUST HAVE THESE ELEMENTS                     
         CLI   TRNEL,TRNELQ                                                     
         JE    ADDT06                                                           
         CLI   TRNEL,TRSELQ                                                     
         JE    ADDT08                                                           
                                                                                
ADDT04   IC    R0,TRNLN                                                         
         AR    R4,R0                                                            
         J     ADDT02                                                           
                                                                                
ADDT06   L     R1,ATSABLK                                                       
         USING TSARD,R1                                                         
         LH    RE,BGTPSEQ                                                       
         AHI   RE,1                                                             
         STH   RE,BGTPSEQ                                                       
         MVC   BGTPACC,TRNKULA                                                  
         MVC   BGTPOFF,TRNANAL                                                  
         MVC   BGTPCAC,TRNKULC                                                  
         MVC   BGTPREF,TRNREF                                                   
         MVC   BGTPMOS,TRNMOS                                                   
         MVC   BGTPDAT,TRNDATE                                                  
         ZAP   BGTPAMT,TRNAMNT                                                  
         MVC   BGTPSTA,TRNSTAT                                                  
         MVI   TSACTN,TSAADD       DON'T ADD UNTIL WE LOOK AT TRSEL             
         J     ADDT04                                                           
         DROP  R1,R4                                                            
                                                                                
         USING TRSELD,R4                                                        
ADDT08   CLI   TRSUPDT,0           SKIP IF ALREADY MARKED UPDATED               
         JNE   EXIT                                                             
         OI    TRSSTAT,TRSSGLUP    MARK AS UPDATED                              
         MVC   TRSUPDT,BCTODAYC                                                 
         OI    TRSSTAT,TRSSGLIP    INDICATE UPDATED VIA $INPUT                  
                                                                                
         GOTO1 AIO,IOWRITE+IOACCMST+IO2                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,ATSABLK                                                       
         USING TSARD,R1                                                         
         GOTOR VTSAR,TSARD         OK TO ADD NOW                                
         JE    EXIT                                                             
         DC    H'0'                                                             
                                                                                
         DROP  R1,R2,R4                                                         
         EJECT                                                                  
***********************************************************************         
* GET LEDGER LEVEL A25 PROFILES                                       *         
* GET LEDGER RECORD: SAVE MEDIA DISP, G/L RULES, OFFICE & HIERARCHY   *         
***********************************************************************         
                                                                                
GETLED   NTR1  ,                                                                
         USING BGWORKD,RC          RC=A(LOCAL W/S)                              
         XC    BCWORK,BCWORK       SAVE LEDGER LEVEL PROGRAM PROFILE            
         MVI   BCWORK,C'A'-X'40'                                                
         MVC   BCWORK+2(2),=C'25'                                               
         MVC   BCWORK+5(2),BGSVLGR                                              
         MVC   BCWORK+12(2),TWAAGY                                              
         GOTOR VGETPROF,BCPARM,(X'C0',BCWORK),BCBPROF1,VDMGR                    
                                                                                
         MVI   BGLGSW,0            CLEAR LEDGER LEVEL RULES                     
         CLI   BCBP07,C'Y'         USE G/L OFFICE RULES?                        
         JNE   *+8                 NO                                           
         OI    BGLGSW,BGLGRL       YES                                          
         CLI   BCBP08,C'Y'         USE G/L OFFICE RULES FOR TRANS?              
         JNE   *+8                 NO                                           
         OI    BGLGSW,BGLGRLT      YES                                          
                                                                                
         MVC   BGGLRL,BGGLRC       DEFAULT IS COMPANY OFFICE OVERRIDE           
                                                                                
         LA    R2,IOKEY            GET THE LEDGER RECORD                        
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(2),BGTPACC                                               
         GOTOR AIO,IOHIGH+IOACCMST+IO2                                          
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   BCCPYPRD,BGTPACC                                                 
         JNE   *+8                                                              
         OI    BGLGSW,BGLGPR       INDICATE PRODUCTION LEDGER                   
                                                                                
         LA    RE,BGRULTAB         CLEAR RULES TABLE                            
         LHI   RF,BGRULLNQ*BGRULMAX                                             
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         SR    R3,R3               CLEAR RULE COUNTER                           
                                                                                
         LA    RE,BGRULTAB         SAVE TABLE START                             
         ST    RE,BGRULSTR                                                      
                                                                                
         L     R2,AIO2             A(LEDGER RECORD)                             
         LA    R2,LDGRFST                                                       
         USING LDGELD,R2                                                        
         SR    R0,R0                                                            
                                                                                
GETL02   CLI   LDGEL,0             TEST EOR                                     
         JE    GETL16                                                           
         CLI   LDGEL,LDGELQ        LOOK FOR LEDGER ELEMENT (X'14')              
         JE    GETL06                                                           
         CLI   LDGEL,GLPELQ        G/L POSTING ELEMENT (X'15')                  
         JE    GETL08                                                           
         CLI   LDGEL,ACLELQ        LEDGER HIERARCHY ELEMENT (X'16')             
         JE    GETL10                                                           
         CLI   LDGEL,NAMELQ        LEDGER NAME (X'20')                          
         JE    GETL12                                                           
         CLI   LDGEL,GLRELQ        G/L OFFICE RULES ELEMENT (X'E6')             
         JE    GETL14                                                           
                                                                                
GETL04   IC    R0,LDGLN                                                         
         AR    R2,R0                                                            
         J     GETL02                                                           
                                                                                
         USING LDGELD,R2                                                        
GETL06   MVC   BGLDOFF,LDGOFFC     SAVE DEFAULT OFFICE                          
         MVC   BGLGOFFP,LDGOPOS    AND OFFICE POSITION                          
         J     GETL04                                                           
         DROP  R2                                                               
                                                                                
GETL08   BRAS  RE,GETRUL           BUILD TABLE OF LEDGER RULES                  
         J     GETL04                                                           
                                                                                
         USING ACLELD,R2                                                        
GETL10   MVC   BGLGLVA,ACLVALS     SAVE CLIENT & MEDIA DISPLACEMENTS            
         MVC   BGLGLVB,ACLVALS+(L'ACLVALS*1)                                    
         MVC   BGLGLVC,ACLVALS+(L'ACLVALS*2)                                    
         MVC   BGLGLVD,ACLVALS+(L'ACLVALS*3)                                    
         MVC   BGLGMED,ACLVALS+(L'ACLVALS*1)                                    
         J     GETL04                                                           
         DROP  R2                                                               
                                                                                
GETL12   LA    R4,BGLGNAME         GET NAME OF LEDGER                           
         GOTOR GETNME                                                           
         J     GETL04                                                           
                                                                                
         USING GLRELD,R2                                                        
GETL14   IC    RF,GLRLN            SAVE A(LEDGER G/L OFFICE RULES)              
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   BGGLRL(0),GLREL                                                  
         J     GETL04                                                           
         DROP  R2                                                               
                                                                                
GETL16   LTR   R3,R3                                                            
         JZ    GETLEDX            NO RULES                                      
         LHI   R1,BGRULMAX                                                      
         CR    R3,R1                                                            
         JNH   *+6                                                              
         DC    H'0'                TOO MANY POSTING INSTRUCTIONS                
                                                                                
         LHI   R0,BGRULLNQ         SORT THE TABLE                               
         LHI   RF,L'BGRULFR        LENGTH OF SORT FIELD                         
         GOTOR VQSORT,BCPARM,(1,BGRULTAB),(R3),(R0),(RF),0                      
                                                                                
GETLEDX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF RULES FROM GLPEL (X'15') ELEMENT                     *         
* R2=A(GLPEL)                                                                   
***********************************************************************         
                                                                                
GETRUL   NTR1  ,                                                                
         USING BGWORKD,RC          RC=A(LOCAL W/S)                              
         USING BGRULD,R4                                                        
         L     R4,BGRULSTR                                                      
         USING GLPELD,R2                                                        
         MVC   BGRULTO(10),GLPACC1 SAVE G/L ACCOUNT                             
         CLI   GLPLN,GLPLN1Q       IS THIS A NEW ELEMENT?                       
         JL    *+10                NO                                           
         MVC   BGRULTO(14),GLPACC1 YES,MOVE LONGER                              
         OC    BGRULTO,BCSPACES    SPACE FILL IT                                
                                                                                
                                                                                
         MVC   BGRULFR,GLPSUB      FROM ACCOUNT                                 
         TM    BGLGSW,BGLGPR       IS THIS A PRODUCTION LEDGER?                 
         JZ    GETR02              NO                                           
         CLI   BGRULFR,C'*'        YES, ARE WE POSTING BY OFFICE?               
         JNE   GETR02              NO                                           
         CLI   BGRULFR+1,C' '      OR BY MEDIA?                                 
         JE    GETR02              YES                                          
         OI    BGLGSW,BGLGOF       RULES BY OFFICE                              
                                                                                
         MVC   BGRULFR,BCSPACES    CLEAR FROM ACCOUNT                           
         SR    R1,R1                                                            
         IC    R1,BCOFFLEN                                                      
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   BGRULFR(0),GLPSUB+1 MOVE OFFICE TO FROM ACCOUNT                  
                                                                                
GETR02   LHI   R2,L'BGRULFR-1      GET LENGTH OF FROM DATA                      
         LA    RF,BGRULFR+(L'BGRULFR-1)                                         
         CLI   0(RF),C' '          FIND LAST NON-BLANK                          
         JNE   *+12                                                             
         BCTR  R2,0                                                             
         BCTR  RF,0                                                             
         J     *-12                                                             
         STC   R2,BGRULFLN         SAVE LENGTH FOR COMPARE                      
                                                                                
         AHI   R4,BGRULLNQ         GET NEXT TABLE ENTRY                         
         ST    R4,BGRULSTR         STARTING POINT FOR NEXT RULE                 
         AHI   R3,1                INCREMENT COUNT                              
         XIT1  REGS=(R3)           GO BACK FOR MORE                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* READ ACCOUNT AND CLIENT RECORD TO GET OFFICE CODES                  *         
* GET MEDIA FROM KEY AND READ FOR NAME                                *         
***********************************************************************         
                                                                                
GETACN   NTR1  ,                                                                
         USING BGWORKD,RC          RC=A(LOCAL W/S)                              
         MVI   BGPSTAT,0                                                        
                                                                                
         MVC   BGPTOA,BCSPACES                                                  
         MVC   BGPOFF,BCSPACES                                                  
         MVC   BGPFRA,BCSPACES                                                  
         MVC   BGPNME,BCSPACES                                                  
                                                                                
         MVC   BGKEY,BCSPACES        GET THE ACCOUNT RECORD                     
         MVC   BGKEY(1),CUABIN                                                  
         MVC   BGKEY+1(L'BGTPACC),BGTPACC                                       
         SR    R0,R0                                                            
         TM    BGLGSW,BGLGPR       IS THIS A PRODUCTION LEDGER?                 
         JZ    *+8                 NO                                           
         ICM   R0,8,=X'80'                                                      
         GOTOR AGETACC,BCPARM,BGKEY,(R0)                                        
         JE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
                                                                                
         MVC   BGACOFF,ACCOFF      ACCOUNT OFFICE                               
         MVC   BGPFRA,ACCODE+1     ACCOUNT CODE                                 
         MVC   BGPNME,ACNAME       ACCOUNT NAME                                 
                                                                                
         TM    BGLGSW,BGLGPR       IS THIS A PRODUCTION LEDGER?                 
         JZ    GETACN10            NO                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS A PRODUCTION ACCOUNT                                        *         
***********************************************************************         
                                                                                
         OI    BGPSTAT,BGPSPRD     INDICATE PRODUCTION                          
         OI    BGPSTAT,BGPSLGS     RULES FROM LEDGER                            
                                                                                
         GOTOR AMRGPRF                                                          
         LA    R2,PSCOMPPR                                                      
         USING PPRELD,R2                                                        
         MVC   BGCLOFF,PPRGAOFF                                                 
         MVC   BGACOFF,BCSPACES                                                 
         DROP  R2                                                               
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,BCSPACES     GET THE CLIENT RECORD                        
         MVC   ACTKCPY,CUABIN                                                   
         SR    RF,RF                                                            
         IC    RF,BGLGLVA                                                       
         AHI   RF,1                ADD 1 FOR UL MINUS 1                         
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   ACTKUNT(0),BGTPACC                                               
         GOTOR AIO,IOHIGH+IOACCMST+IO2                                          
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO2                                                          
         LA    R4,ACTRFST                                                       
         SR    R0,R0                                                            
                                                                                
         USING RSTELD,R4                                                        
GETACN04 CLI   RSTEL,0             TEST EOR                                     
         JE    GETACN06                                                         
         CLI   RSTEL,RSTELQ                                                     
         JE    *+14                                                             
         IC    R0,RSTLN                                                         
         AR    R4,R0                                                            
         J     GETACN04                                                         
                                                                                
         CLI   RSTLN,RSTLN2Q       CHECK IF OLD LENGTH                          
         JL    *+10                                                             
         MVC   BGACOFF,RSTOFFC     SAVE ACCOUNT LEVEL OFFICE                    
         DROP  R4                                                               
                                                                                
GETACN06 MVC   BGPFRA(2),BGTPACC   SAVE UL                                      
                                                                                
         LA    R2,BGTPACC+2        R2=A(ACCOUNT)                                
         SR    R1,R1                                                            
         IC    R1,BGLGMED                                                       
         AR    R1,R2                                                            
         MVC   BGPFRA+2(1),0(R1) MOVE MEDIA INTO ACCOUNT                        
                                                                                
         USING PMDRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   PMDKEY,BCSPACES      GET MEDIA RECORD                            
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUABIN                                                   
         MVC   PMDKMED,0(R1)                                                    
         GOTO1 AIO,IOHIGH+IOACCMST+IO2                                          
         CLC   IOKEY(L'PMDKEY),IOKEYSAV                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO2                                                          
         LA    R2,PMDRFST                                                       
         SR    R0,R0                                                            
                                                                                
         USING PMDELD,R2                                                        
GETACN08 CLI   PMDEL,0             TEST EOR                                     
         JE    GETACN18                                                         
         CLI   PMDEL,PMDELQ        GET MEDIA ELEMENT (X'11')                    
         JE    *+14                                                             
         IC    R0,PMDLN                                                         
         AR    R2,R0                                                            
         J     GETACN08                                                         
                                                                                
         MVC   BGPNME,BCSPACES                                                  
         MVC   BGPNME(L'PMDSPACE+L'PMDDESC),PMDSPACE                            
         J     GETACN18                                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS A NON-PRODUCTION ACCOUNT                                    *         
***********************************************************************         
                                                                                
GETACN10 LHI   R0,4                SET MAXIMUM NUMBER OF LEVELS                 
         LA    R4,BGLGLVD          POINT TO LAST LEVEL                          
                                                                                
GETACN12 SR    R1,R1                                                            
         ICM   R1,1,0(R4)                                                       
         JZ    GETACN16            NOTHING AT THIS LEVEL                        
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,BCSPACES      GET EACH LEVEL                              
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),BGTPACC                                               
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   ACTKACT(0),BGTPACC+2                                             
                                                                                
         GOTO1 AIO,IOHIGH+IOACCMST+IO2                                          
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO2                                                          
         LA    R2,ACTRFST                                                       
         SR    RF,RF                                                            
                                                                                
         USING GLPELD,R2                                                        
GETACN14 CLI   GLPEL,0                                                          
         JE    GETACN16                                                         
         CLI   GLPEL,GLPELQ        GET G/L POSTING ELEMENT (X'15')              
         JE    *+14                                                             
         IC    RF,GLPLN                                                         
         AR    R2,RF                                                            
         J     GETACN14                                                         
                                                                                
         OI    BGPSTAT,BGPSACS     SOURCE IS ACCOUNT RECORD                     
         MVC   BGPTOA(10),GLPACC1                                               
         CLI   GLPLN,26                                                         
         JL    *+10                                                             
         MVC   BGPTOA,GLPACC1                                                   
         CLI   BCBP01,C'P'                                                      
         JNE   GETACN24                                                         
         DROP  R2                                                               
                                                                                
         USING ACTRECD,R2                                                       
         L     R2,AIO2                                                          
         MVC   BGPFRA,1(R2)                                                     
         LA    R2,ACTRFST                                                       
         SR    RF,RF                                                            
                                                                                
         USING NAMELD,R2                                                        
GETACN15 CLI   NAMEL,0                                                          
         JE    GETACN24                                                         
         CLI   NAMEL,NAMELQ        GET THE NAME ELEMENT                         
         JE    *+14                                                             
         IC    RF,NAMLN                                                         
         AR    R2,RF                                                            
         J     GETACN15                                                         
                                                                                
         LA    R4,BGPNME                                                        
         GOTOR GETNME                                                           
         J     GETACN24                                                         
                                                                                
GETACN16 SHI   R4,1                                                             
         BRCT  R0,GETACN12                                                      
         OI    BGPSTAT,BGPSLGS     SOURCE IS LEDGER                             
                                                                                
***********************************************************************         
* COMMON CODE FOR PRODUCTION AND NON-PRODUCTION ACCOUNTS              *         
***********************************************************************         
                                                                                
GETACN18 SR    R1,R1                                                            
         LA    R4,BGRULTAB                                                      
         USING BGRULD,R4                                                        
                                                                                
GETACN20 CLC   BGRULFR,BCSPACES    IF IT'S THE DEFAULT, USE IT                  
         JE    GETACN22                                                         
         OC    BGRULFR,BGRULFR     IF AT END OF TABLE - ERROR                   
         JZ    GETACNER                                                         
         LA    RE,BGCLOFF                                                       
         LHI   R1,L'BGCLOFF-1                                                   
         TM    BGLGSW,BGLGOF                                                    
         JO    *+12                                                             
         LA    RE,BGPFRA+2                                                      
         IC    R1,BGRULFLN         LENGTH OF FROM DATA                          
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         JE    GETACN22                                                         
         CLC   BGRULFR(0),0(RE)    MATCH RULE TO FROM ACCOUNT                   
         AHI   R4,BGRULLNQ                                                      
         J     GETACN20                                                         
                                                                                
GETACN22 MVC   BGPTOA,BGRULTO                                                   
                                                                                
GETACN24 XC    BGSTAR,BGSTAR                                                    
         LA    R0,L'BGPTOA-1       FIND OFFICE OVERRIDE POSITION                
         LA    R5,BGPTOA+(L'BGPTOA-1)                                           
         CLI   0(R5),C'*'                                                       
         JNE   *+16                                                             
         ST    R5,BGSTAR                                                        
         OI    BGPSTAT,BGPSTRN+BGPSOFL                                          
         J     *+10                                                             
         BCTR  R5,0                                                             
         BRCT  R0,*-22                                                          
                                                                                
         TM    BGCORSW,BGCOROF     POSTING TO G/L BY OFFICE?                    
         JZ    GETACN26            NO                                           
         CLI   BGLGOFFP,C'T'       YES, OFFICE IN TRANSACTION?                  
         JE    *+12                YES                                          
         CLI   BGLGOFFP,0          DEFAULT IS ALSO TRANSACTION                  
         JNE   *+8                                                              
         OI    BGPSTAT,BGPSTRN     MUST POST AT TRANSACTION LEVEL               
         J     GETACNX                                                          
                                                                                
GETACN26 TM    BGCORSW,BGCOROFT    POSTING BY TRANSACTION OFFICE?               
         JZ    *+8                 NO                                           
         OI    BGPSTAT,BGPSTRN     YES, POST AT TRANSACTION LEVEL               
                                                                                
GETACNX  CR    RB,RB                                                            
         J     EXIT                                                             
                                                                                
GETACNER MVC   FVMSGNO,=AL2(AE$NORUL)                                           
         MVC   FVXTRA(2),BGTPACC                                                
         LTR   RB,RB                                                            
         J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DETERMINE POSTING OFFICE                                            *         
***********************************************************************         
                                                                                
GETOFF   NTR1  ,                                                                
         USING BGWORKD,RC          RC=A(LOCAL W/S)                              
         MVC   BGPOOFF,BGTPOFF                                                  
         TM    BGLGSW,BGLGPR       IS THIS A PRODUCTION LEDGER?                 
         JZ    *+10                NO                                           
         MVC   BGPOOFF,BGCLOFF                                                  
         OC    BGPOOFF,BCSPACES                                                 
         MVC   BGSVPOF,BGPOOFF                                                  
         TM    BGPSTAT,BGPSTRN                                                  
         JZ    GETOFF04                                                         
         BRAS  RE,GLRL             GET OFFICE OVERRIDE                          
         TM    BGPSTAT,BGPSOFL     REPLACE LAST CHARACTER OF ACCOUNT            
         JZ    GETOFF02            NO                                           
         ICM   R5,15,BGSTAR                                                     
         JZ    GETOFF02                                                         
         MVC   0(1,R5),BGPOOFF                                                  
         CLI   0(R5),X'40'                                                      
         JH    *+8                                                              
         MVI   0(R5),C'*'                                                       
                                                                                
GETOFF02 TM    BGLGSW,BGLGRLT      USE G/L OFFICE RULES FOR TRANS?              
         JO    GETOFF12            YES                                          
         MVC   BGPOOFF,BGSVPOF                                                  
         J     GETOFF12                                                         
                                                                                
GETOFF04 TM    BGPSTAT,BGPSPRD     IS THIS PRODUCTION?                          
         JZ    GETOFF06            NO                                           
         MVC   BGPOOFF,BGCLOFF     YES                                          
         OC    BGPOOFF,BCSPACES                                                 
         J     GETOFF10                                                         
                                                                                
GETOFF06 TM    BGCORSW,BGCOROF     POST TO G/L BY OFFICE?                       
         JZ    GETOFFX             NO                                           
         TM    BGLGOFFP,X'F0'      YES, IS IT IN FILTER?                        
         JNO   GETOFF08            NO                                           
         SR    R1,R1                                                            
         IC    R1,BGLGOFFP                                                      
         SHI   R1,241                                                           
         LA    R3,ACFLTS(R1)                                                    
         MVC   BGPOOFF(1),0(R3)                                                 
         OC    BGPOOFF,BCSPACES                                                 
         J     GETOFF10                                                         
                                                                                
GETOFF08 SR    R1,R1                                                            
         TM    BGLGOFFP,X'40'                                                   
         JZ    *+8                                                              
         LHI   R1,2                                                             
         MVC   BCBYTE1,BGLGOFFP                                                 
         NI    BCBYTE1,X'FF'-X'40'                                              
         CLI   BCBYTE1,1                                                        
         JNL   *+6                                                              
         DC    H'0'                INVALID OFFICE POSITION                      
         CLI   BCBYTE1,12                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R3,R3                                                            
         IC    R3,BCBYTE1                                                       
         LA    R4,BGTPACC                                                       
         LA    R4,1(R3,R4)                                                      
         BASR  RE,R0                                                            
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   BGPOOFF(0),0(R4)                                                 
                                                                                
GETOFF10 TM    BGLGSW,BGLGRLT      USE G/L OFFICE RULES FOR TRANS?              
         JZ    GETOFF12            NO                                           
         BRAS  RE,GLRL                                                          
         OC    BGPOOFF,BCSPACES                                                 
                                                                                
GETOFF12 LA    R1,BGACOFF                                                       
         CLI   0(R1),X'40'                                                      
         JH    GETOFF14                                                         
                                                                                
         LA    R1,BGLDOFF                                                       
         CLI   0(R1),X'40'                                                      
         JH    GETOFF14                                                         
                                                                                
         LA    R1,BGPOOFF                                                       
         CLI   0(R1),X'40'                                                      
         JH    GETOFF14                                                         
                                                                                
         LA    R1,BGCOOFF                                                       
                                                                                
GETOFF14 MVC   BGPOFF,0(R1)                                                     
         OC    BGPOFF,BCSPACES                                                  
                                                                                
         TM    BGPSTAT,BGPSPRD     IF IT'S PRODUCTION                           
         JO    GETOFFX             CONTRA REMAINS SJX X=MEDIA CODE              
         TM    BGPSTAT,BGPSACS     IS RULE FROM ACCOUNT                         
         JO    *+10                                                             
         MVC   BGPFRA+2(12),BCSPACES IF NOT, FORCE CONTRA BY U/L                
         CLI   BCBP01,C'Y'         POST BY CONTRA U/L ONLY                      
         JNE   *+10                                                             
         MVC   BGPFRA+2(12),BCSPACES                                            
         CLI   BCBP01,C'L'         POST BY CONTRA U/L ONLY                      
         JNE   *+10                                                             
         MVC   BGPFRA+2(12),BCSPACES                                            
                                                                                
GETOFFX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET OFFICE OVERRIDE FROM GLRULES                                    *         
***********************************************************************         
                                                                                
GLRL     NTR1  ,                                                                
         USING BGWORKD,RC          RC=A(LOCAL W/S)                              
         TM    BGLGSW,BGLGRL       USE G/L OFFICE RULES                         
         JZ    GLRLX                                                            
         CLI   BGGLRL,0            OFFICE OVERRIDE ELEMENT                      
         JE    GLRLX                                                            
                                                                                
         USING GLRELD,R2                                                        
         LA    R2,BGGLRL                                                        
         SR    R3,R3                                                            
         IC    R3,GLRPRS           NUMBER OF OFFICE CODES                       
         LA    R2,GLROFFP                                                       
         CLC   0(2,R2),BGPOOFF     MATCH TO SUBSIDIARY OFFICE                   
         JE    *+16                                                             
         LA    R2,4(R2)                                                         
         BRCT  R3,*-14                                                          
         J     *+10                NO OVERRIDE                                  
         MVC   BGPOOFF,2(R2)       CHANGE THE OFFICE CODE                       
                                                                                
GLRLX    J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ POSTINGS UP FRONT. GENERATE ERROR AND ABEND ON ERRORS          *         
***********************************************************************         
READUP   NTR1  ,                                                                
         USING BGWORKD,RC          RC=A(LOCAL W/S)                              
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,BCSPACES     VERIFY ACCOUNTS BEFORE POSTING               
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,BGPTOA                                                   
         GOTO1 AIO,IOHIGH+IOACCMST+IO2                                          
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         JNE   READUPER                                                         
                                                                                
READUPX  CR    RB,RB                                                            
         J     EXIT                                                             
                                                                                
READUPER MVC   FVMSGNO,=AL2(AE$IVACN)                                           
         LHI   R0,L'BGPTOA+1                                                    
         STC   R0,FVPARMS                                                       
         MVC   FVPARMS+1(L'BGPTOA),IOKEYSAV+1                                   
         LHI   R0,L'BGTPACC                                                     
         STC   R0,FVPARMS+1+L'BGPTOA                                            
         MVC   FVPARMS+2+L'BGPTOA(L'BGTPACC),BGTPACC                            
         LTR   RB,RB                                                            
         J     EXIT                                                             
         EJECT                                                                  
         DROP  R2                                                               
***********************************************************************         
* BUILD POSTINGS                                                      *         
***********************************************************************         
POSTER   NTR1  ,                                                                
         USING BGWORKD,RC          RC=A(LOCAL W/S)                              
         LA    R2,IOAREA           CLEAR IO AREA                                
         LHI   R3,L'IOAREA                                                      
         XR    R1,R1                                                            
         MVCL  R2,R0                                                            
                                                                                
         SR    R2,R2                                                            
         ICM   R2,3,BGBITMC                                                     
         AHI   R2,1                                                             
         STCM  R2,3,BGBITMC        INCREMENT ITEM COUNT                         
                                                                                
         AP    BGBCSHC,BGTPAMT                                                  
                                                                                
         LA    R2,IOAREA+2                                                      
         USING DLDESCD,R2                                                       
         MVI   DLDSEL,DLDSELQ                                                   
         MVI   DLDSSBRF,0                                                       
         MVC   DLDSREF,BGTPREF                                                  
         MVC   DLDSDATE,BGTPDAT                                                 
         MVI   DLDSSTAT,0                                                       
         MVI   DLDSNARR,C' '                                                    
         LHI   R1,1                                                             
                                                                                
         LA    RF,DLDSNARR                                                      
         SR    RF,R2               GET LENGTH UP TO NARRATIVE                   
         AR    RF,R1               ADD LENGTH OF NARRATIVE                      
         STC   RF,DLDSLEN                                                       
         AR    R2,RF                                                            
                                                                                
         USING TRSELD,R2                                                        
         XC    TRSEL(TRSLNQ),TRSEL                                              
         MVI   TRSEL,TRSELQ        BUILD TRANSACTION STATUS ELEMENT             
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSPMOS,BGTPMOS                                                  
         AHI   R2,TRSLNQ                                                        
                                                                                
*                                  ** GB POSTING **                             
         USING DLPOSTD,R2                                                       
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,BCSPACES                                                
         ZAP   DLPSAMNT,BGTPAMT                                                 
         MVC   DLPSANAL,BGPOFF                                                  
                                                                                
         TM    BGTPSTA,TRNSDR      DETERMINE IF DEBIT OR CREDIT NEEDED          
         JZ    POST02                                                           
         MVI   DLPSEL,DLPSEDRQ     SINGLE DEBIT                                 
         MVC   DLPSDBC,CUABIN                                                   
         MVC   DLPSDBU(L'BGPTOA),BGPTOA                                         
         MVC   DLPSCRAC,BGPNME                                                  
         J     POST04                                                           
                                                                                
POST02   MVI   DLPSEL,DLPSECRQ     SINGLE CREDIT                                
         MVC   DLPSCRC,CUABIN                                                   
         MVC   DLPSCRU(L'BGPTOA),BGPTOA                                         
         MVC   DLPSDBAC,BGPNME                                                  
                                                                                
POST04   TM    BGPSTAT,BGPSPRD     IS THIS PRODUCTION?                          
         JO    POST08              YES                                          
                                                                                
         TM    BGTPSTA,TRNSDR                                                   
         JZ    POST06                                                           
         MVC   DLPSCRC,CUABIN                                                   
         MVC   DLPSCRU(L'BGPFRA),BGPFRA                                         
         MVC   DLPSCRNM,BGPNME                                                  
         CLI   DLPSCRAC+3,C' '                                                  
         JNE   POST08                                                           
         MVC   DLPSCRNM,BGLGNAME                                                
         J     POST08                                                           
                                                                                
POST06   MVC   DLPSDBC,CUABIN                                                   
         MVC   DLPSDBU(L'BGPFRA),BGPFRA                                         
         MVC   DLPSDBNM,BGPNME                                                  
         CLI   DLPSDBAC+3,C' '                                                  
         JNE   POST08                                                           
         MVC   DLPSDBNM,BGLGNAME                                                
                                                                                
POST08   AHI   R2,DLPSLNQ          BUMP PAST END AND POST S9                    
                                                                                
         USING TRSELD,R2                                                        
         XC    TRSEL(TRSLNQ),TRSEL                                              
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSPMOS,BGTPMOS                                                  
         AHI   R2,TRSLNQ                                                        
                                                                                
         USING DLPOSTD,R2                                                       
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,BCSPACES                                                
         ZAP   DLPSAMNT,BGTPAMT                                                 
         MVC   DLPSANAL,BGPOFF                                                  
                                                                                
         TM    BGTPSTA,TRNSDR                                                   
         JO    POST10                                                           
                                                                                
         MVI   DLPSEL,DLPSEDRQ     SINGLE DEBIT                                 
         MVC   DLPSDBC,CUABIN                                                   
         MVC   DLPSDBU(2),=C'S9'                                                
         MVC   DLPSDBA(1),BGSVLGR+1                                             
         MVC   DLPSCRAC,BCSPACES                                                
         MVC   DLPSCRC,CUABIN                                                   
         MVC   DLPSCRU(2),BGSVLGR                                               
         MVC   DLPSCRNM,BCSPACES                                                
         J     POST12                                                           
                                                                                
POST10   MVI   DLPSEL,DLPSECRQ     SINGLE CREDIT                                
         MVC   DLPSCRC,CUABIN                                                   
         MVC   DLPSCRU(2),=C'S9'                                                
         MVC   DLPSCRA(1),BGSVLGR+1                                             
         MVC   DLPSDBAC,BCSPACES                                                
         MVC   DLPSDBC,CUABIN                                                   
         MVC   DLPSDBU(2),BGSVLGR                                               
         MVC   DLPSDBNM,BCSPACES                                                
                                                                                
POST12   AHI   R2,DLPSLNQ          BUMP PAST END                                
         MVI   0(R2),0             MARK END                                     
         AHI   R2,1                END ADDRESS                                  
         LA    R1,IOAREA           START ADDRESS                                
         SR    R2,R1                                                            
         STCM  R2,3,IOAREA         TOTAL LENGTH                                 
                                                                                
         SR    R3,R3                                                            
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(L'BGTPREF),BGTPREF                                       
                                                                                
         MVI   BOWORK1+11,25       TRANSACTION TYPE                             
         ZAP   BOPL61,BGTPAMT      AMOUNT                                       
                                                                                
         GOTO1 AADDITE,BOPARM,IOAREA,BOPL61,BOWORK1                             
         LA    R0,FVFOK            TEST ADD OK                                  
         CLM   R0,3,FVMSGNO                                                     
         JE    EXIT                                                             
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET NAME AND STORE OFF R4                                           *         
***********************************************************************         
                                                                                
         USING NAMELD,R2                                                        
GETNME   SR    R1,R1                                                            
         MVC   0(36,R4),BCSPACES                                                
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         BASR  RF,0                                                             
         EX    R1,6(RF)                                                         
         BR    RE                                                               
         MVC   0(0,R4),NAMEREC                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXITS FROM PROGRAM                                                  *         
***********************************************************************         
         SPACE 1                                                                
SETCUR   LA    R1,BASOLY1H         SET CURSOR TO FIRST UNPROT FIELD             
         SR    RE,RE                                                            
         LA    RF,OSVALS-1                                                      
SETCUR02 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         JNZ   *+6                                                              
         DC    H'0'                                                             
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         JZ    *+10                                                             
         BRXLE R1,RE,SETCUR02                                                   
         DC    H'0'                CAN'T FIND AN INPUT FIELD                    
         STCM  R1,15,FVADDR                                                     
         J     EXIT                                                             
         SPACE 1                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD COLUMN HEADINGS AND DISPLAY DISPLACEMENTS          *         
***********************************************************************         
         SPACE 1                                                                
         USING IDWORKD,RC                                                       
BLDDIS   NTR1  BASE=*,WORK=(RC,IDWORKX-IDWORKD)                                 
         XC    IDWORKD(IDWORKX-IDWORKD),IDWORKD                                 
         MVC   ILDHEAD1,BCSPACES                                                
         MVC   ILDHEAD2,BCSPACES                                                
         XC    ILDSDSP(ILDSDSPL),ILDSDSP                                        
         XC    ILDRDSP(ILDRDSPL),ILDRDSP                                        
         SR    R0,R0                                                            
         IC    R0,ILODINUM         R0=NUMBER OF DISPLAYED COLUMNS               
         SR    R1,R1                                                            
         IC    R1,ILODINDX         R1=INDEX TO DISPLAYED COLUMNS                
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
BLDDIS02 MVC   IDTEMP(1),BCSCRNUM                                               
         NI    IDTEMP,X'0F'                                                     
         IC    R0,IDTEMP                                                        
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
         LA    RE,IDTEMP           INVERT DISPLAY PROFILE                       
         LA    RF,ILODIS(R1)                                                    
         BCTR  RF,0                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         OI    IDFLAG,IDFINVRT                                                  
         B     BLDDIS22                                                         
BLDDIS10 MVC   IDTEMP(1),BCSCRNUM                                               
         NI    IDTEMP,X'0F'                                                     
         IC    R0,IDTEMP                                                        
         SR    R1,R0                                                            
*                                                                               
BLDDIS12 LA    RE,ILODIS+L'ILODIS-1                                             
         LA    RF,L'ILODIS                                                      
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
BLDDIS16 STC   R1,ILODINDX                                                      
         SR    RF,R1                                                            
         LA    R1,ILODIS(R1)                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   IDTEMP(0),0(R1)                                                  
*                                                                               
BLDDIS22 LA    R1,IDTEMP           R1=A(DISPLAY COLUMN PROFILE)                 
         SR    R3,R3               R3=DISPLACEMENT TO DISPLAY VALUE             
*                                                                               
BLDDIS24 CLI   0(R1),0             TEST END OF COLUMN DEFINITIONS               
         BE    BLDDIS42                                                         
         LA    R2,DISTAB                                                        
         USING DISTABD,R2          R2=A(DISPLAY COLUMN TABLE)                   
BLDDIS26 CLI   DISTABD,EOT         TEST EOT                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DISCOL,0(R1)        TEST COLUMN MATCH                            
         BE    *+12                                                             
         AHI   R2,DISTABL                                                       
         B     BLDDIS26                                                         
*                                                                               
         TM    IDFLAG,IDFPASS2     TEST SECOND PASS                             
         BNZ   BLDDIS32                                                         
         LA    RF,IDWORK                                                        
         CLI   0(RF),0                                                          
         BE    *+12                                                             
         AHI   RF,1                                                             
         B     *-12                                                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,DISWID                                                        
         AR    RE,R3               ADD DISPLACEMENT SO FAR                      
         CLM   RE,1,=AL1(L'ILILIN1)                                             
         BH    BLDDIS42            OVERFLOW - DROP THIS COLUMN                  
         LA    R3,1(RE)            R3=DISPLACEMENT TO NEXT COLUMN               
         MVC   0(1,RF),DISCOL                                                   
         IC    RF,IDCOLS           BUMP NUMBER OF DISPLAY COLUMNS               
         AHI   RF,1                                                             
         STC   RF,IDCOLS                                                        
         B     BLDDIS40                                                         
*                                                                               
BLDDIS32 LA    RF,L'FVIHDR(R3)     DISPLACEMENT SO FAR +L'HEADER                
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         STC   RF,ILDSDSP-1(RE)    SET SCREEN LINE DISPLACEMENT                 
         LA    RF,1(R3)            DISPLACEMENT SO FAR +1                       
         STC   RF,ILDRDSP-1(RE)    SET REPORT LINE DISPLACEMENT                 
         SR    RF,RF                                                            
         ICM   RF,3,DISNAM1                                                     
         BZ    BLDDIS34                                                         
         LA    RF,TWAD(RF)                                                      
         SR    RE,RE                                                            
         ICM   RE,1,DISHWI         HEADING WIDTH OVERRIDE                       
         BNZ   *+8                                                              
         IC    RE,DISWID           COLUMN WIDTH                                 
         BCTR  RE,0                                                             
         LA    R4,ILDHEAD1(R3)                                                  
         EX    RE,*+8                                                           
         B     BLDDIS34                                                         
         MVC   0(0,R4),0(RF)                                                    
*                                                                               
BLDDIS34 SR    RF,RF                                                            
         ICM   RF,3,DISNAM2        TEST SECOND HEADING                          
         BZ    BLDDIS36                                                         
         LA    RF,TWAD(RF)         BUILD SECOND HEADING                         
         LA    R4,ILDHEAD2(R3)     INDEX TO SECOND HEADLINE                     
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
         LA    R4,L'ILDHEAD2(R4)   INDEX TO SECOND HEADLINE                     
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
         A     R3,IDFULL1          ADD CONSTANT FACTOR FOR SPARE                
*                                                                               
BLDDIS40 LA    R1,1(R1)            BUMP TO NEXT FIELD                           
         B     BLDDIS24            PERFORM FOR NUMBER OF PROFILES               
*                                                                               
BLDDIS42 TM    IDFLAG,IDFPASS2     TEST SECOND PASS                             
         BNZ   BLDDISX             YES - WE HAVE FINISHED                       
         TM    IDFLAG,IDFINVRT     TEST INVERTED LIST                           
         BZ    BLDDIS44                                                         
         MVC   IDSAVE,IDTEMP       RE-INVERT LIST                               
         SR    R1,R1                                                            
         ICM   R1,1,IDCOLS                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RF,IDSAVE(R1)                                                    
         LA    RE,IDTEMP                                                        
         BCTR  RF,0                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         SR    R1,R1               ADJUST INDEX VALUE                           
         IC    R1,IDCOLS                                                        
         SR    R0,R0                                                            
         IC    R0,ILODINDX                                                      
         SR    R0,R1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         STC   R0,ILODINDX                                                      
         NI    IDFLAG,FF-IDFINVRT                                               
*                                                                               
BLDDIS44 OI    IDFLAG,IDFPASS2     SET SECOND TIME                              
         SR    R1,R1                                                            
         IC    R1,IDCOLS                                                        
         LA    R1,IDTEMP(R1)                                                    
         MVI   0(R1),0             SET END OF COLUMN DEFINITION                 
         LA    R1,L'ILILIN1+1                                                   
         SR    R1,R3               GET REMAINING SPACE INTO R1                  
         BNP   BLDDIS22            NOTHING LEFT TO ADD                          
         CLI   IDCOLS,1            TEST SINGLE COLUMN                           
         BE    BLDDIS22            DON'T DIVIDE OR SET FACTOR                   
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    RF,IDCOLS           NO. OF COLS (THOUGH ONE IS FIXED)            
         BCTR  RF,0                                                             
         DR    R0,RF                                                            
         ST    R1,IDFULL1          SET CONSTANT FACTOR FOR SPARE                
         B     BLDDIS22            GO BACK AND SET DISPLACEMENTS                
*                                                                               
BLDDISX  MVC   ILODINUM,IDCOLS     SET NUMBER OF COLUMNS PROCESSED              
         J     EXIT                                                             
         DROP  R2,RB,RC                                                         
         SPACE 1                                                                
IDWORKD  DSECT                     ** BLDDIS LOCAL W/S **                       
IDFULL1  DS    F                                                                
IDTEMP   DS    XL64                                                             
IDWORK   DS    XL64                                                             
IDSAVE   DS    XL64                                                             
IDFLAG   DS    XL1                                                              
IDFPASS2 EQU   X'80'               ON=SECOND PASS                               
IDFINVRT EQU   X'40'               ON=INVERTED LIST                             
IDCOLS   DS    XL1                                                              
IDCOLUMN DS    XL1                                                              
IDWORKX  EQU   *                                                                
BAT61    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A DISPLAY LINE USING BLDDIS DISPLACEMENTS          *         
***********************************************************************         
         SPACE 1                                                                
         USING LSTTABD,R3          R3=A(LSTTABD ENTRY)                          
BLDLIN   NTR1  BASE=*                                                           
         LR    R2,R1                                                            
         USING ILIACT1H,R2                                                      
         NI    ILIACT1H+(FVATRB-FVIHDR),FF-(FVAPROT+FVAHIGH)                    
         TM    CSBIND5,TYPIASIR    TEST AUTO SELECT ALL ITEMS                   
         BZ    *+12                                                             
         TM    LSTIIND1,LSTI1SEL   TEST THIS ITEM IS SELECTED                   
         BNZ   *+12                                                             
         TM    LSTIIND1,LSTI1DSP   TEST DISPLAY ONLY                            
         BNO   *+12                                                             
         OI    ILIACT1H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         MVI   ILIACT1,C'*'                                                     
         LA    R2,ILIITEMH                                                      
         USING ILLINED,R2                                                       
         OI    ILLHDR1+(FVOIND-FVIHDR),FVOXMT                                   
         OI    ILLHDR2+(FVOIND-FVIHDR),FVOXMT                                   
         OI    ILLHDR3+(FVOIND-FVIHDR),FVOXMT                                   
         OI    ILLHDR4+(FVOIND-FVIHDR),FVOXMT                                   
         CLC   CSRECACT,=AL1(RECITE,ACTSEL)                                     
         BNE   BLDLIN04                                                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,TWASESNL       FOR ITEM/SELECT                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SLL   RE,1                TEST PREVIOUS SESSION BATCH/REVERSE          
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         CLC   0(L'TWASESRA,RE),=AL1(RECBAT,ACTREV)                             
         BE    BLDLIN04                                                         
         NI    ILLHDR2+(FVATRB-FVIHDR),FF-FVAPROT                               
         NI    ILLHDR3+(FVATRB-FVIHDR),FF-FVAPROT                               
BLDLIN04 DS    0H                                                               
*&&UK                                                                           
         TM    BCBATCUR+(LSTBIND2-LSTTABD),LSTBIMLT                             
         BZ    BLDLIN08                                                         
         SR    R0,R0               FOR MULTI ITEM CHANGE BATCHES THE            
         ICM   R0,3,LSTISNO        SEQUENCE NUMBER IS IN THE FORMAT             
         CVD   R0,BODUB1           SSS/NN (SSS=SCREEN#, NN=LINE#)               
         OI    BODUB1+7,X'0F'                                                   
         UNPK  ILLITEM(3),BODUB1                                                
         MVC   ILLITEM+3(1),BCSLASH                                             
         SR    R0,R0                                                            
         ICM   R0,1,LSTISIS                                                     
         CVD   R0,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  ILLITEM+4(2),BODUB1                                              
         B     BLDLIN10                                                         
*&&                                                                             
BLDLIN08 CURED LSTISEQ,(L'ILLITEM,ILLITEM),0,ALIGN=LEFT,DMCB=BOPARM             
BLDLIN10 MVC   ILLREFN,LSTIREF                                                  
         GOTOR VDATCON,BOPARM,(1,LSTIDATE),(17,ILLDATE)                         
*                                                                               
         NI    ILLHDR1+(FVATRB-FVIHDR),FF-FVAHIGH                               
         NI    ILLHDR2+(FVATRB-FVIHDR),FF-FVAHIGH                               
         NI    ILLHDR3+(FVATRB-FVIHDR),FF-FVAHIGH                               
         NI    ILLHDR4+(FVATRB-FVIHDR),FF-FVAHIGH                               
         TM    LSTIIND1,LSTI1SEL   TEST SELECTED                                
         BZ    BLDLIN20                                                         
         MVC   ILLREFN,LSTIREF2                                                 
         GOTOR VDATCON,BOPARM,(1,LSTIDAT2),(17,ILLDATE)                         
         OI    ILLHDR1+(FVATRB-FVIHDR),FVAHIGH                                  
         OI    ILLHDR2+(FVATRB-FVIHDR),FVAHIGH                                  
         OI    ILLHDR3+(FVATRB-FVIHDR),FVAHIGH                                  
         OI    ILLHDR4+(FVATRB-FVIHDR),FVAHIGH                                  
*                                                                               
BLDLIN20 LA    R2,ILLHDR4                                                       
         DROP  R2                                                               
         XR    R4,R4                                                            
*                                                                               
BLAMNT   ICM   R4,1,ILDSAMNT                                                    
         BZ    BLAMNTX                                                          
         LA    RF,0(R2,R4)                                                      
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS/TAX                  
         BZ    BLAMNT02                                                         
*&&UK                                                                           
         LA    RE,ZDPTAB                                                        
         LA    R0,ZDPTABN                                                       
         CLC   CSBTYP,0(RE)        TEST TYPE SUPPORTS BIAAMT TO 0 DP            
         BE    *+16                                                             
         LA    RE,ZDPTABL(RE)                                                   
         BCT   R0,*-14                                                          
*&&                                                                             
         B     BLAMNT02                                                         
         TM    LSTTSTAT,TBAESIDR   'DEBIT'=TAX, 'CREDIT'=UNITS                  
         BO    BLAMNT02                                                         
*&&UK*&& CURED LSTIAMT,(WIDAMT-1,(RF)),0,DMCB=BOPARM,FLOAT=-                    
*&&US*&& CURED LSTIAMT,(WIDAMT-1,(RF)),0,DMCB=BOPARM,MINUS=YES                  
         B     BLAMNTX                                                          
BLAMNT02 DS    0H                                                               
*&&UK*&& CURED LSTIAMT,(WIDAMT-1,(RF)),2,DMCB=BOPARM,FLOAT=-                    
*&&US*&& CURED LSTIAMT,(WIDAMT-1,(RF)),2,DMCB=BOPARM,MINUS=YES                  
BLAMNTX  DS    0H                                                               
*                                                                               
BLACC1   ICM   R4,1,ILDSACC1                                                    
         BZ    BLACC1X                                                          
         LA    RE,LSTIACC1+L'ACTKCPY                                            
         LA    RF,L'LSTIACC1-L'ACTKCPY                                          
         CLI   0(RE),C' '          DROP LEADING SPACES                          
         BH    BLACC1A                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,*-12                                                          
         B     BLACC1X             NOTHING TO DISPLAY                           
BLACC1A  BCTR  RF,0                                                             
         LA    R1,0(R2,R4)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)                                                    
BLACC1X  DS    0H                                                               
*                                                                               
BLACC2   ICM   R4,1,ILDSACC2                                                    
         BZ    BLACC2X                                                          
         LA    RE,LSTIACC2+L'ACTKCPY                                            
         LA    RF,L'LSTIACC2-L'ACTKCPY                                          
         CLI   0(RE),C' '          DROP LEADING SPACES                          
         BH    BLACC2A                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,*-12                                                          
         B     BLACC2X             NOTHING TO DISPLAY                           
BLACC2A  BCTR  RF,0                                                             
         LA    R1,0(R2,R4)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)                                                    
BLACC2X  DS    0H                                                               
*                                                                               
BLSTA    ICM   R4,1,ILDSSTA                                                     
         BZ    BLSTAX                                                           
         LA    RF,0(R2,R4)                                                      
         TM    LSTTSTAT,TBAESLDE   DELETED                                      
         BZ    *+14                                                             
         MVC   0(L'LC3DELD,RF),LC3DELD                                          
         B     BLSTAX                                                           
         TM    CSBIND1,TYPICUMU    SHOW DR/CR IS TYPICUMU                       
         BZ    BLSTAX                                                           
         TM    LSTTSTAT,TBAESDAC                                                
         BZ    BLSTA06                                                          
         BM    BLSTA02                                                          
         MVC   0(1,RF),BC@DR                                                    
         MVC   1(1,RF),BCSLASH                                                  
         MVC   2(1,RF),BC@CR                                                    
         B     BLSTAX                                                           
BLSTA02  TM    LSTTSTAT,TBAESDMD                                                
         BZ    BLSTA04                                                          
         MVC   0(1,RF),BC@DR                                                    
         MVI   1(RF),C'-'                                                       
         MVC   2(1,RF),BC@DR                                                    
         B     BLSTAX                                                           
BLSTA04  MVC   0(1,RF),BC@CR                                                    
         MVI   1(RF),C'-'                                                       
         MVC   2(1,RF),BC@CR                                                    
         B     BLSTAX                                                           
BLSTA06  MVC   0(L'LC3DELD,RF),BC@CR                                            
         TM    LSTTSTAT,TBAESIDR                                                
         BZ    *+10                                                             
         MVC   0(L'LC3DELD,RF),BC@DR                                            
BLSTAX   DS    0H                                                               
*                                                                               
BLITEM   ICM   R4,1,ILDSITEM                                                    
         BZ    BLITEMX                                                          
         LA    RF,0(R2,R4)                                                      
*&&UK*&& CURED LSTISEQ,(WIDITEM,(RF)),0,ALIGN=LEFT,DMCB=BOPARM                  
*&&US                                                                           
         SR    R0,R0               FOR MULTI ITEM CHANGE BATCHES THE            
         ICM   R0,3,LSTISNO        SEQUENCE NUMBER IS IN THE FORMAT             
         CVD   R0,BODUB1           SSS/NN (SSS=SCREEN#, NN=LINE#)               
         OI    BODUB1+7,X'0F'                                                   
         UNPK  0(3,RF),BODUB1                                                   
         MVC   3(1,RF),BCSLASH                                                  
         SR    R0,R0                                                            
         ICM   R0,1,LSTISIS                                                     
         CVD   R0,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  4(2,RF),BODUB1                                                   
*&&                                                                             
BLITEMX  DS    0H                                                               
*                                                                               
BLACTS   ICM   R4,1,ILDSACTS                                                    
         BZ    BLACTSX                                                          
         GOTOR LSTADIS                                                          
         LA    RF,0(R2,R4)                                                      
         MVC   0(WIDACTS,RF),BOWORK1                                            
BLACTSX  DS    0H                                                               
*&&UK                                                                           
BLCURR   ICM   R4,1,ILDSCURR                                                    
         BZ    BLCURRX                                                          
         OC    LSTIFCUR,LSTIFCUR                                                
         BZ    BLCURRX                                                          
         ICM   RF,15,BOADDR1                                                    
         BNZ   BLCURR2                                                          
         GOTOR VBLDCUR,BOPARM,0,AIO9,ACOM                                       
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO9                                                          
         ST    RF,BOADDR1                                                       
         USING CURTABD,RF                                                       
BLCURR2  CLI   CURTABD,0           TEST END OF CURRENCY TABLE                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   CURTCUR,LSTIFCUR                                                 
         BE    *+12                                                             
         LA    RF,CURTABL(RF)                                                   
         B     BLCURR2                                                          
         LA    R4,0(R2,R4)                                                      
         CURED LSTIFAMT,(WIDCURR,(R4)),(RF),DMCB=BOPARM,MINUS=YES,     *        
               CURSYMB=Y                                                        
         SR    R4,R4                                                            
BLCURRX  DS    0H                                                               
*&&                                                                             
BLRNUM   ICM   R4,1,ILDSRNUM                                                    
         BZ    BLRNUMX                                                          
         LA    RF,0(R2,R4)                                                      
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN                                                    
         CVD   R1,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  0(WIDRNUM,RF),BODUB1                                             
BLRNUMX  DS    0H                                                               
*                                                                               
BLADDR   ICM   R4,1,ILDSADDR                                                    
         BZ    BLDLINX                                                          
         LA    RF,0(R2,R4)                                                      
         GOTOR VHEXOUT,BOPARM,LSTTDA,(RF),L'LSTTDA,=C'TOG'                      
BLADDRX  DS    0H                                                               
*                                                                               
BLDLINX  J     EXIT                                                             
         DROP  R3,RB                                                            
         SPACE 1                                                                
ZDPTAB   DS    0X                  TYPES WHICH SUPPORT 0 DP BIAAMT              
         DC    AL1(BT62)                                                        
ZDPTABL  EQU   *-ZDPTAB                                                         
ZDPTABN  EQU   (*-ZDPTAB)/ZDPTABL                                               
*                                                                               
ILLINED  DSECT                                                                  
ILLHDR1  DS    XL(L'FVIHDR)                                                     
ILLITEM  DS    CL6                                                              
ILLHDR2  DS    XL(L'FVIHDR)                                                     
ILLREFN  DS    CL6                                                              
ILLHDR3  DS    XL(L'FVIHDR)                                                     
ILLDATE  DS    CL8                                                              
ILLHDR4  DS    XL(L'FVIHDR)                                                     
ILLINE   DS    CL48                                                             
BAT61    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT ALTERNATIVE DISPLAY DATA (VALID ACTIONS) FOR LIST SCREEN    *         
***********************************************************************         
         SPACE 1                                                                
LSTADIS  NTR1  ,                                                                
         LA    R1,CSLSTCUR                                                      
         USING LSTTABD,R1                                                       
         LA    R4,BOWORK1                                                       
         MVC   BOWORK1,BCSPACES                                                 
*                                                                               
         L     R3,AOVERSEL                                                      
         USING SELTABD,R3                                                       
LSTADIS2 CLI   SELTABD,EOT                                                      
         JE    LSTADIS6                                                         
         STM   RE,R1,BOPARM                                                     
         GOTOR ATSTMIX,SELTPARM    VALIDATE RECORD/ACTION                       
         LM    RE,R1,BOPARM                                                     
         JNE   LSTADIS4                                                         
         MVC   BCHALF,SELTMASK                                                  
         NC    BCHALF,LSTTMASK                                                  
         CLC   BCHALF,SELTMASK                                                  
         JNE   LSTADIS4                                                         
         XR    RE,RE                                                            
         ICM   RE,3,SELTDSPM       MIXED CASE ACTION WORD                       
         LA    RE,TWAD(RE)                                                      
         MVC   0(ILVACTWD,R4),0(RE)                                             
         MVC   ILVACTWD(L'BCCOMMA,R4),BCCOMMA                                   
         LA    R4,L'BCCOMMA+ILVACTWD(R4)                                        
LSTADIS4 AHI   R3,SELTABL                                                       
         J     LSTADIS2                                                         
*                                                                               
LSTADIS6 CLC   BOWORK1,BCSPACES                                                 
         JE    LSTADISX                                                         
*                                                                               
         CLC   BOWORK1+WIDACTS(1),BCSPACES                                      
         JNE   *+16                                                             
         BCTR  R4,0                                                             
         MVC   0(1,R4),BCSPACES    REMOVE LAST COMMA                            
         J     LSTADISX                                                         
*                                                                               
         LA    R4,BOWORK1+WIDACTS-1                                             
         CLC   0(L'BCCOMMA,R4),BCCOMMA                                          
         MVC   0(1,R4),BCSPACES                                                 
         JE    *+8                                                              
         BRCT  R4,*-16                                                          
         MVI   BOWORK1+WIDACTS-1,C'>'                                           
*                                                                               
LSTADISX J     EXIT                                                             
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD POSTING LSTTAB ENTRY IN BCPSTCUR                              *         
* NTRY - R1=A(GINEL) OR A(ASKEL)                                      *         
***********************************************************************         
         SPACE 1                                                                
PSTBLD   NTR1  ,                                                                
         LA    R2,BCPSTCUR                                                      
         USING LSTTABD,R2                                                       
         XC    LSTTABD(LSTTABL),LSTTABD                                         
         CLI   0(R1),ASKELQ                                                     
         JE    PSTBLD00                                                         
         LA    RF,IOKEY           BUILD GROUP INVOICE PASSIVE KEY               
         USING GINPASD,RF                                                       
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,CUABIN                                                   
         MVC   GINPINV,GININV-GINELD(R1)                                        
         GOTOR AIO,IOHI+IOACCMST+IO2                                            
         JE    *+6                                                              
         DC    H'0'                                                             
         J     PSTBLD01                                                         
PSTBLD00 MVC   IOKEY,ASKKEY-ASKELD(R1)                                          
         MVC   LSTPSEQ,ASKSEQN-ASKELD(R1)                                       
         GOTOR AIO,IORD+IOACCMST+IO2                                            
         JE    PSTBLD01                                                         
         DC    H'0'                CAN'T READ ACCDIR/ACCMST                     
PSTBLD01 L     R4,AIO2                                                          
         USING TRNRECD,R4                                                       
         MVC   LSTTSTAT,TRNRSTAT                                                
         MVC   LSTTDA,IODA                                                      
         MVI   LSTTRTYP,RECPST                                                  
         MVC   LSTPACT,TRNKCULA                                                 
         MVC   LSTPKOF,TRNKOFF                                                  
         MVC   LSTPCAC,TRNKCULC                                                 
         LA    RF,TRNRFST                                                       
         SR    R0,R0                                                            
PSTBLD02 CLI   0(RF),0             TEST EOR                                     
         JE    PSTBLD09                                                         
         CLI   0(RF),TRNELQ        TEST TRANSACTION ELEMENT                     
         JE    PSTBLD06                                                         
         CLI   0(RF),TRSELQ        TEST TRANSACTION STATUS ELEMENT              
         JE    PSTBLD05                                                         
*                                                                               
PSTBLD04 IC    R0,1(RF)            BUMP TO NEXT ELEMENT                         
         AR    RF,R0                                                            
         J     PSTBLD02                                                         
*                                                                               
         USING TRSELD,RF                                                        
PSTBLD05 MVC   LSTPTRS1,TRSSTAT    EXTRACT TRANSACTION STATUS VALUES            
         MVC   LSTPTRS2,TRSSTAT2                                                
         MVC   LSTPTRS3,TRSSTAT3                                                
         J     PSTBLD04                                                         
*                                                                               
         USING TRNELD,RF                                                        
PSTBLD06 MVC   LSTPOFF,TRNOFFC     EXTRACT TRANSACTION VALUES                   
         MVC   LSTPSTAT,TRNSTAT                                                 
         ZAP   LSTPAMT,TRNAMNT                                                  
*                                                                               
PSTBLD09 GOTOR ASETMSK,LSTTABD                                                  
*                                                                               
PSTBLDX  J     EXIT                                                             
         DROP  R2,R4,RF                                                         
         EJECT                                                                  
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH ITEM OPTIONS                                         *         
***********************************************************************         
         SPACE 1                                                                
ILOVAL   NMOD1 250,**ILOV**,CLEAR=YES                                           
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     ILOVREF                                                          
         B     ILOVDAT                                                          
         B     ILOVAMT                                                          
         B     ILOVACC                                                          
         B     ILOVDIS                                                          
         B     ILOVITE                                                          
         B     ILOVDEL                                                          
         B     ILOVEXP                                                          
*                                                                               
ILOVALX  XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE REF=AAAAAA-999999                                          *         
***********************************************************************         
         SPACE 1                                                                
ILOVREF  MVC   BCWORK+L'ILOREFST(L'ILOREFND),BCEFFS                             
         LA    R1,FVIFLD                                                        
         LA    RF,1(R1)                                                         
         CLI   0(R1),C'-'                                                       
         BNE   ILOVRE02                                                         
         CLI   0(RF),C' '                                                       
         BH    ILOVRE08                                                         
         B     ERRSHRT                                                          
*                                                                               
ILOVRE02 CLI   0(RF),C' '                                                       
         BE    ILOVRE04                                                         
         CLI   0(RF),C'-'                                                       
         BE    ILOVRE04                                                         
         LA    RF,1(RF)                                                         
         B     ILOVRE02                                                         
*                                                                               
ILOVRE04 SR    RF,R1                                                            
         CHI   RF,L'ILOREFST                                                    
         BH    ERRLONG                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BCWORK(0),0(R1)                                                  
         LA    R1,1(RF,R1)                                                      
         CLI   0(R1),C'-'          TEST RANGE GIVEN                             
         BE    ILOVRE06                                                         
         EX    RF,*+8                                                           
         B     ILOVREFX                                                         
         MVC   BCWORK+L'ILOREFST(0),BCWORK                                      
*                                                                               
ILOVRE06 LA    RF,1(R1)                                                         
         CLI   0(RF),C' '          TEST END REF INPUT                           
         BNH   ILOVREFX                                                         
*                                                                               
ILOVRE08 LR    R1,RF                                                            
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    *-8                                                              
         SR    RF,R1                                                            
         CHI   RF,L'ILOREFND                                                    
         BH    ERRLONG                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BCWORK+L'ILOREFST(0),0(R1)                                       
*                                                                               
         CLC   BCWORK(L'ILOREFST),BCWORK+L'ILOREFST                             
         BH    ERRRANGE            START EXCEEDS END                            
*                                                                               
ILOVREFX B     ILOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATE=DDMMMYY-DDMMMYY                                       *         
***********************************************************************         
         SPACE 1                                                                
ILOVDAT  GOTOR AVALPER,BOPARM,FVIHDR,0                                          
         BH    ILOVALX                                                          
         ICM   R0,7,BCWORK+(PVALPSTA-PERVALD)                                   
         ICM   R1,7,BCWORK+(PVALPEND-PERVALD)                                   
         STCM  R0,7,BCWORK                                                      
         STCM  R1,7,BCWORK+L'PVALPSTA                                           
         B     ILOVALX                                                          
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AMOUNT=99999999999.99                                      *         
***********************************************************************         
         SPACE 1                                                                
ILOVAMT  GOTOR AVALAMT,BOPARM,(X'C2',FVIHDR),(L'ILOAMT,BCWORK)                  
         B     ILOVALX                                                          
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCOUNT=ULA(CCOUNT)                                        *         
***********************************************************************         
         SPACE 1                                                                
ILOVACC  MVC   BCWORK(L'ILOULAL),FVILEN                                         
         MVC   BCWORK+L'ILOULAL(L'ILOULA),FVIFLD                                
         B     ILOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISPLAY=ABC OR +ABC OR ABC+ OR ++ (ALL COLUMNS)            *         
***********************************************************************         
         SPACE 1                                                                
ILOVDIS  MVI   BCWORK,0                                                         
         SR    RE,RE                                                            
         ICM   RE,1,FVXLEN         TEST MORE THAN ONE CHARACTER INPUT           
         BZ    ILOVDI08                                                         
         CLI   FVIFLD,C'+'         TEST FOR LEADING + SIGN (SUFFIX)             
         BNE   ILOVDI06                                                         
         CLI   FVILEN,2            TEST FOR DIS=++                              
         BNE   ILOVDI04                                                         
         CLI   FVIFLD+1,C'+'                                                    
         BNE   ILOVDI04                                                         
         LA    R1,DEFDISP          R1=A(DEFAULT DISPLAY COLUMNS)                
         LHI   R0,L'DEFDISP                                                     
         GOTOR ADDCOL                                                           
         AHI   R1,1                                                             
         BCT   R0,*-8                                                           
ILOVDI01 LA    R1,DISTAB                                                        
         USING DISTABD,R1                                                       
ILOVDI02 CLI   DISTABD,EOT         TEST END OF TABLE                            
         BE    ILOVDISX                                                         
         GOTOR ADDCOL                                                           
         AHI   R1,DISTABL                                                       
         B     ILOVDI02                                                         
         DROP  R1                                                               
*                                                                               
ILOVDI04 MVI   BCWORK,ILODISIS                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),FVIFLD+1                                               
         STC   RE,FVILEN                                                        
*                                                                               
         LA    R1,DEFDISP          R1=A(DEFAULT DISPLAY COLUMNS)                
         LHI   R0,L'DEFDISP                                                     
         GOTOR ADDCOL                                                           
         AHI   R1,1                                                             
         BCT   R0,*-8                                                           
         B     ILOVDI08                                                         
*                                                                               
ILOVDI06 LA    RF,FVIFLD(RE)       POINT TO END OF INPUT STRING                 
         CLI   0(RF),C'+'          TEST FOR TRAILING + SIGN (PREFIX)            
         BNE   ILOVDI08                                                         
         MVI   BCWORK,ILODISIP                                                  
         MVI   0(RF),C' '                                                       
         STC   RE,FVILEN                                                        
*                                                                               
ILOVDI08 SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
ILOVDI10 GOTOR ADDCOL                                                           
         BE    *+14                                                             
         MVC   FVXTRA(1),0(R1)                                                  
         B     ERRINCOL                                                         
         AHI   R1,1                                                             
         BCT   R0,ILOVDI10                                                      
*                                                                               
         CLI   BCWORK,ILODISIP     TEST INPUT COLUMNS ARE PREFIX                
         BNE   ILOVDISX                                                         
         LA    R1,DEFDISP          R1=A(DEFAULT DISPLAY COLUMNS)                
         LHI   R0,L'DEFDISP                                                     
         GOTOR ADDCOL                                                           
         AHI   R1,1                                                             
         BCT   R0,*-8                                                           
*                                                                               
ILOVDISX B     ILOVALX                                                          
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE ITEM=NNNNN-NNNNN                                           *         
***********************************************************************         
         SPACE 1                                                                
ILOVITE  MVC   BCWORK+L'ILOITEST(L'ILOITEND),BCEFFS                             
         LA    R1,FVIFLD                                                        
         LA    RF,1(R1)                                                         
         CLI   0(R1),C'-'                                                       
         BNE   ILOVIT02                                                         
         CLI   0(RF),C' '                                                       
         BNH   ERRSHRT                                                          
         CLI   0(RF),C'0'                                                       
         BL    ERRNONIF                                                         
         CLI   0(RF),C'9'                                                       
         BH    ERRNONIF                                                         
         B     ILOVIT08                                                         
*                                                                               
ILOVIT02 CLI   0(R1),C'0'                                                       
         BL    ERRNONIF                                                         
         CLI   0(R1),C'9'                                                       
         BH    ERRNONIF                                                         
         CLI   0(RF),C' '                                                       
         BE    ILOVIT04                                                         
         CLI   0(RF),C'-'                                                       
         BE    ILOVIT04                                                         
         CLI   0(RF),C'0'                                                       
         BL    ERRNONIF                                                         
         CLI   0(RF),C'9'                                                       
         BH    ERRNONIF                                                         
         LA    RF,1(RF)                                                         
         B     ILOVIT02                                                         
*                                                                               
ILOVIT04 SR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  BCDUB,0(0,R1)                                                    
         CVB   R0,BCDUB                                                         
         C     R0,=F'65535'                                                     
         BH    ERRLONG                                                          
         STCM  R0,3,BCWORK                                                      
         LA    R1,1(RF,R1)                                                      
         CLI   0(R1),C'-'          TEST RANGE GIVEN                             
         BE    ILOVIT06                                                         
         STCM  R0,3,BCWORK+L'ILOITEST                                           
         B     ILOVITEX                                                         
*                                                                               
ILOVIT06 LA    RF,1(R1)                                                         
         CLI   0(RF),C' '          TEST END ITEM INPUT                          
         BNH   ILOVITEX                                                         
         CLI   0(RF),C'0'                                                       
         BL    ERRNONIF                                                         
         CLI   0(RF),C'9'                                                       
         BH    ERRNONIF                                                         
*                                                                               
ILOVIT08 LR    R1,RF                                                            
ILOVIT10 LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BNH   ILOVIT12                                                         
         CLI   0(RF),C'0'                                                       
         BL    ERRNONIF                                                         
         CLI   0(RF),C'9'                                                       
         BH    ERRNONIF                                                         
         B     ILOVIT10                                                         
ILOVIT12 SR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  BCDUB,0(0,R1)                                                    
         CVB   R0,BCDUB                                                         
         C     R0,=F'65535'                                                     
         BH    ERRLONG                                                          
         STCM  R0,3,BCWORK+L'ILOITEST                                           
*                                                                               
         CLC   BCWORK(L'ILOITEST),BCWORK+L'ILOITEST                             
         BH    ERRRANGE            START EXCEEDS END                            
*                                                                               
ILOVITEX B     ILOVALX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE DELETED= OPTION                                            *         
***********************************************************************         
         SPACE 1                                                                
ILOVDEL  SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    R1,DELTAB                                                        
ILOVDEL2 CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     ILOVALX                                                          
         CLM   RF,1,1(R1)                                                       
         BH    ILOVDEL4                                                         
         SR    RE,RE                                                            
         ICM   RE,3,2(R1)                                                       
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    ILOVDEL6                                                         
         CLC   FVIFLD(0),0(RE)                                                  
ILOVDEL4 LA    R1,L'DELTAB(R1)                                                  
         B     ILOVDEL2                                                         
ILOVDEL6 MVC   BCWORK(L'ILODEL),0(R1)                                           
         B     ILOVALX                                                          
         SPACE 2                                                                
DELTAB   DS    0XL4                ** DELETE KEYWORD TABLE **                   
         DC    AL1(ILODELYQ)                                                    
         DC    AL1(L'UC@YES-1),AL2(UC@YES-TWAD)                                 
         DC    AL1(ILODELNQ)                                                    
         DC    AL1(L'UC@NO-1),AL2(UC@NO-TWAD)                                   
         DC    AL1(ILODELOQ)                                                    
         DC    AL1(L'UC@ONLY-1),AL2(UC@ONLY-TWAD)                               
DELTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXPAND= OPTION                                             *         
***********************************************************************         
         SPACE 1                                                                
ILOVEXP  SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    R1,EXPTAB                                                        
ILOVEXP2 CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     ILOVALX                                                          
         CLM   RF,1,1(R1)                                                       
         BH    ILOVEXP4                                                         
         SR    RE,RE                                                            
         ICM   RE,3,2(R1)                                                       
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BE    ILOVEXP6                                                         
         CLC   FVIFLD(0),0(RE)                                                  
ILOVEXP4 LA    R1,L'EXPTAB(R1)                                                  
         B     ILOVEXP2                                                         
ILOVEXP6 MVC   BCWORK(L'ILOEXP),0(R1)                                           
         B     ILOVALX                                                          
         SPACE 2                                                                
EXPTAB   DS    0XL4                ** EXPAND KEYWORD TABLE **                   
         DC    AL1(ILOEXPNQ)                                                    
         DC    AL1(L'UC@NO-1),AL2(UC@NO-TWAD)                                   
EXPTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ADD A COLUMN TO LIST OF DISPLAY COLUMNS                             *         
*                                                                     *         
* NTRY - R1=A(DISPLAY COLUMN CHARACTER)                               *         
* EXIT - CC=EQUAL IF OK, NOT EQUAL ON ERROR                           *         
***********************************************************************         
         SPACE 1                                                                
ADDCOL   STM   RE,R1,12(RD)                                                     
         CLI   0(R1),0             IGNORE NULL COLUMNS                          
         JE    ADDCOLY                                                          
         LA    RE,DISTAB                                                        
         USING DISTABD,RE          RE=A(DISPLAY COLUMN TABLE)                   
ADDCOL02 CLI   DISTABD,EOT         TEST EOT                                     
         JE    ADDCOLN                                                          
         CLC   DISCHR,0(R1)        MATCH CHARACTER TO TABLE                     
         JE    *+12                                                             
         AHI   RE,DISTABL                                                       
         J     ADDCOL02                                                         
*                                                                               
         TM    DISINDS,DISIITEM    TEST ALTERNATIVE ITEM NO.                    
         JZ    *+12                                                             
         TM    BCBATCUR+(LSTBIND2-LSTTABD),LSTBIMLT                             
         JZ    ADDCOLN                                                          
         TM    DISINDS,DISIDDS     TEST DDS ONLY                                
         JZ    *+12                                                             
         TM    CUSTAT,CUSDDS                                                    
         JZ    ADDCOLN                                                          
         TM    DISINDS,DISIFCUR    TEST FOREIGN CURRENCY COLUMN                 
         JZ    ADDCOL04                                                         
         TM    CSBIND5,TYPIFCUR    TEST MAY BE FOREIGN CURRENCY                 
         JZ    ADDCOLN                                                          
         TM    BCCPYST6,CPYSFBIL+CPYSFMCR+CPYSFOCR                              
         JZ    ADDCOLN                                                          
*                                                                               
ADDCOL04 LA    R1,BCWORK+1                                                      
         LA    R0,L'ILODIS                                                      
ADDCOL06 CLI   0(R1),0             TEST THIS IS A NEW COLUMN                    
         JE    ADDCOL08                                                         
         CLC   DISCOL,0(R1)        IGNORE DUPLICATE COLUMNS                     
         JE    ADDCOLY                                                          
         LA    R1,L'DISCOL(R1)                                                  
         BRCT  R0,ADDCOL06                                                      
         J     ADDCOLN                                                          
ADDCOL08 MVC   0(L'DISCOL,R1),DISCOL  SET INTERNAL VALUE                        
*                                                                               
ADDCOLY  CR    RE,RE                                                            
         J     ADDCOLX                                                          
ADDCOLN  LTR   RE,RE                                                            
*                                                                               
ADDCOLX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* ILOVAL ERROR EXITS                                                  *         
***********************************************************************         
         SPACE 1                                                                
ERRSHRT  MVC   FVMSGNO,=AL2(AE$FLDTS)                                           
         B     ILOVALX                                                          
ERRLONG  MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         B     ILOVALX                                                          
ERRRANGE MVC   FVMSGNO,=AL2(AE$INVRG)                                           
         B     ILOVALX                                                          
ERRINCOL MVC   FVMSGNO,=AL2(AE$INCOL)                                           
         B     ILOVALX                                                          
ERRNONIF MVC   FVMSGNO,=AL2(AE$NONIF)                                           
         B     ILOVALX                                                          
         EJECT                                                                  
DISTAB   DS    0X                  ** DISPLAY COLUMNS **                        
*                                                                               
WIDAMT   EQU   14                                                               
         DC    C'0',AL1(ILDSAMNT+1-ILDSDSP),AL1(WIDAMT,L'LC@AMT,0)              
         DC    AL2(LC@AMT-TWAD,0)                                               
*                                                                               
WIDACC1  EQU   14                                                               
         DC    C'1',AL1(ILDSACC1+1-ILDSDSP)                                     
         DC    AL1(WIDACC1,0,0)                                                 
         DC    AL2(LC@ACC1-TWAD,0)                                              
*                                                                               
WIDACC2  EQU   14                                                               
         DC    C'2',AL1(ILDSACC2+1-ILDSDSP)                                     
         DC    AL1(WIDACC2,0,0)                                                 
         DC    AL2(LC@ACC2-TWAD,0)                                              
*                                                                               
WIDSTA   EQU   3                                                                
         DC    C'3',AL1(ILDSSTA+1-ILDSDSP)                                      
         DC    AL1(WIDSTA,0,0)                                                  
         DC    AL2(LC@STT-TWAD,0)                                               
*                                                                               
WIDITEM  EQU   6                                                                
         DC    C'4',AL1(ILDSITEM+1-ILDSDSP)                                     
         DC    AL1(WIDITEM,0,DISIITEM)                                          
*&&UK*&& DC    AL2(LC@ITEM-TWAD,0)                                              
*&&US*&& DC    AL2(LC@SEQNO-TWAD,0)                                             
*&&UK                                                                           
WIDCURR  EQU   17                                                               
         DC    C'5',AL1(ILDSCURR+1-ILDSDSP)                                     
         DC    AL1(WIDCURR,WIDCURR-1,DISIFCUR)                                  
         DC    AL2(LC@CURRY-TWAD,0)                                             
*&&                                                                             
WIDACTS  EQU   16                                                               
         DC    C'?',AL1(ILDSACTS+1-ILDSDSP)                                     
         DC    AL1(WIDACTS,0,0)                                                 
         DC    AL2(LC@VALAC-TWAD,0)                                             
*                                                                               
WIDRNUM  EQU   4                                                                
         DC    C'W',AL1(ILDSRNUM+1-ILDSDSP)                                     
         DC    AL1(WIDRNUM,0,DISIDDS)                                           
         DC    AL2(LC$NUM-TWAD,0)                                               
*                                                                               
WIDADDR  EQU   8                                                                
         DC    C'V',AL1(ILDSADDR+1-ILDSDSP)                                     
         DC    AL1(WIDADDR,0,DISIDDS)                                           
         DC    AL2(LC$ADR-TWAD,0)                                               
*                                                                               
DISTABX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
DISTABD  DSECT                                                                  
DISCHR   DS    CL1                 COLUMN CHARCTER                              
DISCOL   DS    AL1                 COLUMN NUMBER                                
DISWID   DS    AL1                 COLUMN WIDTH                                 
DISHWI   DS    AL1                 HEADING WIDTH OVERRIDE                       
DISINDS  DS    AL1                 * INDICATORS *                               
DISIDDS  EQU   X'80'               COLUMN IS DDS ONLY                           
DISIITEM EQU   X'40'               COLUMN IS ALTERNATIVE ITEM NO.               
DISIFCUR EQU   X'20'               COLUMN IS FOREIGN CURRENCY                   
DISNAM1  DS    AL2                 DISPLACEMENT TO COLUMN NAME - 1              
DISNAM2  DS    AL2                 DISPLACEMENT TO COLUMN NAME - 2              
DISTABL  EQU   *-DISTABD                                                        
BAT61    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER AN ITEM RECORD                                    *         
*                                                                     *         
* NTRY - IOKEY CONTAINS BATCH ITEM RECORD KEY                         *         
* EXIT - CC=NOT EQUAL IF BATCH ITEM FILTERED OUT                      *         
***********************************************************************         
         SPACE 1                                                                
FLTITE   NTR1  BASE=*,LABEL=*                                                   
         USING ILWORKD,RC          RC=A(LOCAL W/S)                              
         L     R2,AIO1                                                          
         USING TBARECD,R2          R2=A(BATCH ITEM KEY)                         
         OC    TBAKTSEQ,TBAKTSEQ   ONLY WANT ITEM RECORDS                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TBAKTSEQ,ILOITEST   TEST < START ITEM RANGE                      
         BL    FLTITEN                                                          
         CLC   TBAKTSEQ,ILOITEND   TEST > END ITEM RANGE                        
         BH    FLTITEN                                                          
         CLI   ILODEL,ILODELYQ     TEST INCLUDE DELETED ITEMS                   
         BE    FLTITE02                                                         
         LA    RF,BOQ              SET TO EXCLUDE IF DELETED                    
         CLI   ILODEL,ILODELOQ     TEST ONLY DELETED ITEMS                      
         BNE   *+8                                                              
         LA    RF,BZQ              SET TO INCLUDE IF DELETED                    
         TM    TBARESTA,TBAESLDE   TEST LOGICALLY DELETED                       
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   FLTITEN                                                          
*                                                                               
FLTITE02 SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R1,TBARFST                                                       
         SR    R0,R0                                                            
FLTITE04 CLI   0(R1),0                                                          
         BE    FLTITE08                                                         
         CLI   0(R1),BTAELQ                                                     
         BE    *+12                                                             
         CLI   0(R1),BIAELQ                                                     
         BNE   *+10                                                             
         LR    RE,R1                                                            
         B     FLTITE06                                                         
         CLI   0(R1),ASKELQ                                                     
         BNE   FLTITE06                                                         
         LTR   RF,RF                                                            
         BNZ   FLTITE06                                                         
         LR    RF,R1                                                            
FLTITE06 IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     FLTITE04                                                         
*                                                                               
FLTITE08 LTR   R1,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING BIAELD,R1                                                        
         CLC   BIAREF,ILOREFST                                                  
         BL    FLTITEN                                                          
         CLC   BIAREF,ILOREFND                                                  
         BH    FLTITEN                                                          
*                                                                               
         CLI   ILOEXP,ILOEXPNQ     TEST DON'T EXPAND                            
         BNE   FLTITE10                                                         
         CLC   CSRECACT,=AL1(RECSCR,ACTSEL)                                     
         BNE   FLTITE10                                                         
         TM    BCBATCUR+(LSTBIND2-LSTTABD),LSTBIMLT                             
         BZ    FLTITE10                                                         
         CLC   ILHISNO,BIASNO      TEST FIRST FOR NEW SCREEN                    
         BNL   FLTITEN                                                          
*                                                                               
FLTITE10 OC    ILOAMT,ILOAMT       TEST AMOUNT FILTER                           
         BZ    FLTITE14                                                         
         CP    BIAAMT,ILOAMT       TEST AMOUNT EQUAL                            
         BE    FLTITE14                                                         
         TM    ILOAMTI,OPTPLQ+OPTMIQ TEST EXPLICIT POSITIVE/NEGATIVE            
         BNZ   FLTITEN                                                          
         ZAP   BODUB1,ILOAMT       TEST NEGATIVE AMOUNT EQUAL                   
         MP    BODUB1,=P'-1'                                                    
         CP    BIAAMT,BODUB1                                                    
         BE    FLTITE14                                                         
         TM    ILOAMTI,OPTGEQ      TEST >= AMOUNT FILTER                        
         BZ    FLTITE12                                                         
         CP    BIAAMT,ILOAMT                                                    
         BL    FLTITEN                                                          
         B     FLTITE14                                                         
FLTITE12 TM    ILOAMTI,OPTLEQ      TEST <= AMOUNT FILTER                        
         BZ    FLTITEN                                                          
         CP    BIAAMT,ILOAMT                                                    
         BH    FLTITEN                                                          
*                                                                               
FLTITE14 LTR   R1,RF                                                            
         BZ    FLTITE16                                                         
         USING ASKELD,R1                                                        
         LA    R1,ASKKEY                                                        
         USING TRNKEY,R1                                                        
         CLC   TRNKDATE,ILODATST                                                
         BL    FLTITEN                                                          
         CLC   TRNKDATE,ILODATND                                                
         BH    FLTITEN                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,ILOULAL                                                     
         BZ    FLTITE16                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BNE   FLTITEN                                                          
         CLC   TRNKUNT(0),ILOULA                                                
*                                                                               
FLTITE16 DS    0H                                                               
*                                                                               
FLTITEY  MVI   BOBYTE1,1                                                        
         B     FLTITEX                                                          
FLTITEN  MVI   BOBYTE1,0                                                        
         B     FLTITEX                                                          
*                                                                               
FLTITEX  CLI   BOBYTE1,1           SET CONDITION CODE FOR CALLER                
         XIT1  ,                                                                
         DROP  R1,R2,RC                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE A COPY/REVERSE TRANSACTION                      *         
*                                                                     *         
* NTRY - R1=A(EXISTING TRANSACTION RECORD)                            *         
*        AIO2 CONTAINS NEW BATCH HEADER RECORD                        *         
*        CSLSTCUR CONTAINS TSAR RECORD FOR EXISTING BATCH ITEM        *         
*        ICLSTCUR CONTAINS NEW BATCH DETAILS                          *         
***********************************************************************         
         SPACE 1                                                                
GENTRN   NMOD1 0,**GTRN**                                                       
         L     RC,4(RD)                                                         
         L     RC,68(RC)           POINT BACK TO LOCAL W/S                      
         USING ICWORKD,RC          RC=A(LOCAL W/S)                              
         LR    R2,R1                                                            
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD)                     
         OC    BCCPYGLM,BCCPYGLM   IF USING NEW GL, CHECK MOA IF G              
         BZ    GENTRN01                                                         
         CLI   TRNKUNT,C'G'        TURN OFF BANK RECONCILED BIT ON              
         BNE   GENTRN01                                                         
         MVC   FVMSGNO,=AL2(AE$NOGEN)                                           
         CLC   ICLSTCUR+(LSTBMOSP-LSTTABD),BCCPYGLM                             
         JNL   GENTRNE                                                          
         CLI   ICLSTCUR+(LSTBBTYP-LSTTABD),55                                   
         BE    *+12                SUBTRACT A MONTH FOR TY 53 OR 55             
         CLI   ICLSTCUR+(LSTBBTYP-LSTTABD),53                                   
         JNE   GENTRN01                                                         
         MVC   BCWORK(L'BCCPYGLM),BCCPYGLM                                      
         MVI   BCWORK+2,X'01'                                                   
         GOTO1 VDATCON,BOPARM,(X'31',BCWORK),(1,BCWORK),(4,0)                   
         CLC   ICLSTCUR+(LSTBMOSP-LSTTABD),BCWORK                               
         JNL   GENTRNE                                                          
*                                                                               
GENTRN01 CLI   CSACT,ACTREV        TEST REVERSING                               
         BE    *+16                                                             
         MVC   TRNKDATE,ICBUFDAT   SET DATE                                     
         MVC   TRNKREF,ICBUFREF    SET REFERENCE                                
         MVI   TRNKSBR,0                                                        
         NI    TRNRSTAT,FF-(TRNSARCH)                                           
         NI    TRNRSTA2,FF-(TRNSGLUP+TRNSUSED+TRNSPEEL)                         
         XC    TRNRSUSE,TRNRSUSE                                                
         SR    R0,R0                                                            
*                                                                               
         LA    R4,TRNRFST                                                       
         USING TRNELD,R4                                                        
         CLI   TRNEL,TRNELQ        TEST TRANSACTION ELEMENT FIRST               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ICBTYP,TRNTYPE                                                   
         MVC   TRNDATE,TRNKDATE                                                 
         MVC   TRNMOS,ICBMOSC                                                   
         MVC   TRNBREF,ICBBREF                                                  
*&&UK                                                                           
         CLC   =C'SC',TRNKUNT      TURN OFF BANK RECONCILED BIT ON              
         BNE   *+8                 CASH LEDGER FOR COPY & REVERSE               
         NI    TRNSTAT,FF-(TRNSBREC)                                            
*&&                                                                             
         CLI   CSACT,ACTCOP        TEST COPY/REVERSE                            
         BNE   GENTRN02                                                         
         NI    TRNSTAT,CTRNBITS    COPY - CLEAR/PRESERVE BITS                   
         TM    BCCPYST4,CPYSIREG   TEST COMPANY ON INVOICE REGISTER             
         BZ    GENTRN08                                                         
         TM    CSBIND4,TYPIPAUT    TEST PRESERVE AUTH STATUS ON COPY            
         BNZ   *+8                                                              
         NI    TRNSTAT,FF-(TRNSAUTH)                                            
         B     GENTRN08                                                         
*                                                                               
GENTRN02 NI    TRNSTAT,RTRNBITS    REVERSE - CLEAR/PRESERVE BITS                
         ZAP   BODUB1,TRNAMNT                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   TRNAMNT,BODUB1      SWAP TRANSACTION SIGN                        
*                                                                               
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),TRNKUNT                      
         BNE   GENTRN08                                                         
         CLC   TRNKWORK,=C'99'                                                  
         BNE   GENTRN08                                                         
*&&UK                                                                           
         TM    BCCPYEL+(CPYSTAT5-CPYELD),CPYSNVAT                               
         BO    GENTRN08                                                         
         LA    RF,13                                                            
*&&                                                                             
*&&US*&& LA    RF,3                                                             
         LA    RE,TRNNARR+15                                                    
GENTRN06 ZAP   BODUB1,0(6,RE)                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   0(6,RE),BODUB1                                                   
         LA    RE,6(RE)                                                         
         BCT   RF,GENTRN06                                                      
*                                                                               
GENTRN08 IC    R0,TRNLN                                                         
         AR    R4,R0                                                            
         CLI   TRNEL,0                                                          
         BE    GENTRN90                                                         
         CLI   TRNEL,SCIELQ                                                     
         BE    GENTRN10                                                         
         CLI   TRNEL,MPYELQ                                                     
         BE    GENTRN14                                                         
         CLI   TRNEL,TRSELQ                                                     
         BE    GENTRN16                                                         
         CLI   TRNEL,MDTELQ                                                     
         BE    GENTRN22                                                         
         CLI   TRNEL,XPYELQ                                                     
         BE    GENTRN26                                                         
         CLI   TRNEL,BNDELQ                                                     
         BE    GENTRN28                                                         
         CLI   TRNEL,AFCELQ                                                     
         BE    GENTRN30                                                         
         CLI   TRNEL,PTAELQ                                                     
         BE    GENTRN32                                                         
*&&US*&& CLI   TRNEL,PRTELQ                                                     
*&&US*&& BE    GENTRN36                                                         
         CLI   TRNEL,UNPELQ                                                     
         BE    GENTRN38                                                         
         CLI   TRNEL,FFTELQ                                                     
         BE    GENTRN40                                                         
         CLI   TRNEL,TRXELQ                                                     
         BE    GENTRN42                                                         
         CLI   TRNEL,RBIELQ                                                     
         BE    GENTRN44                                                         
         CLI   TRNEL,CUBELQ                                                     
         BE    GENTRN46                                                         
         CLI   TRNEL,VBIELQ                                                     
         BE    GENTRN48                                                         
*&&US*&& CLI   TRNEL,PBIELQ                                                     
*&&US*&& BE    GENTRN50                                                         
*&&US*&& CLI   TRNEL,PIDELQ                                                     
*&&US*&& BE    GENTRN52                                                         
         B     GENTRN08                                                         
*                                                                               
         USING SCIELD,R4                                                        
GENTRN10 CLI   CSACT,ACTREV        TEST REVERSING                               
         BNE   GENTRN08                                                         
         ZAP   BODUB1,SCIAMNT      REVERSE SUBSIDIARY CASH                      
         MP    BODUB1,=P'-1'                                                    
         ZAP   SCIAMNT,BODUB1      REVERSE SUBSIDIARY CASH AMOUNT               
         CLI   SCILN,SCILN2Q       TEST SECOND AMOUNT HELD                      
         BL    GENTRN08                                                         
*                                  THIS IS TO HANDLE SOME BAD SCIEL'S           
         ZAP   BODUB1,=P'0'        INITIALIZE IN CASE SCIADMN MISSING           
         TM    SCIADMN+5,X'0C'     IS IT PACKED?                                
         BZ    *+10                NO, SKIP ZAP                                 
*                                                                               
         ZAP   BODUB1,SCIADMN      SCIADMN COVERS SECOND AMOUNT                 
         MP    BODUB1,=P'-1'                                                    
         ZAP   SCIADMN,BODUB1      REVERSE SECOND AMOUNT                        
         B     GENTRN08                                                         
*                                                                               
         USING MPYELD,R4                                                        
GENTRN14 MVC   MPYNO,BCSPACES                                                   
         XC    MPYDTE,MPYDTE                                                    
         ZAP   MPYAMNT,BCPZERO                                                  
         MVC   MPYBNK,BCSPACES                                                  
         CLI   MPYLN,MPYLN2Q                                                    
         BL    GENTRN08                                                         
         MVI   MPYSUB,0                                                         
         CLI   MPYLN,MPYLN3Q                                                    
         BL    GENTRN08                                                         
         ZAP   MPYPART,BCPZERO                                                  
         B     GENTRN08                                                         
*                                                                               
         USING TRSELD,R4                                                        
GENTRN16 ICM   RE,3,TRSPMOS        SAVE ORIGINAL MOS                            
         ICM   R1,8,TRSSTAT        SAVE STATUS BYTES 1-3                        
         ICM   R1,4,TRSSTAT2                                                    
         ICM   R1,2,TRSSTAT3                                                    
         IC    RF,TRSLN                                                         
         SHI   RF,TRSLN+2-TRSELD                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    TRSLN+1(0),TRSLN+1                                               
         MVC   TRSDATE,BCTODAYC                                                 
         STCM  R1,8,TRSSTAT        RESTORE STATUS BYTES 1-3                     
         STCM  R1,4,TRSSTAT2                                                    
         STCM  R1,2,TRSSTAT3                                                    
         CLI   CSACT,ACTCOP        TEST COPY/REVERSE                            
         BNE   GENTRN18                                                         
*        NI    TRSSTAT,CTRSBITS    COPY - CLEAR/PRESERVE BITS                   
         MVI   TRSSTAT,0           CLEAR ALL BITS                               
         NI    TRSSTAT2,CTRSBIT2                                                
         NI    TRSSTAT3,CTRSBIT3                                                
         B     GENTRN20                                                         
*ENTRN18 NI    TRSSTAT,RTRSBITS    REVERSE - CLEAR/PRESERVE BITS                
GENTRN18 MVI   TRSSTAT,0           CLEAR ALL BITS                               
         NI    TRSSTAT2,RTRSBIT2                                                
         NI    TRSSTAT3,RTRSBIT3                                                
         TM    ICBUFSTA,ICBUFSRD   TEST NEW REFERENCE/DATE                      
         BNZ   GENTRN20                                                         
         STCM  RE,3,TRSRMOS        SET REVERSING TRANSACTION MOS                
         OI    TRSSTAT,TRSSREVS    SET 'REVERSE ME'                             
GENTRN20 TM    ICLSTCUR+(LSTBHDS1-LSTTABD),BHDSACRU                             
         BZ    *+8                                                              
         OI    TRSSTAT2,TRSSACRL   SET ACCRUAL                                  
         TM    ICLSTCUR+(LSTBHDS1-LSTTABD),BHDSACRV                             
         BZ    *+8                                                              
         OI    TRSSTAT2,TRSSACRV   SET ACCRUAL REVERSAL                         
         B     GENTRN08                                                         
*                                                                               
         USING MDTELD,R4                                                        
GENTRN22 CLI   CSACT,ACTREV        TEST REVERSING                               
         BNE   GENTRN08                                                         
         LA    RE,MDTGRS                                                        
         LA    RF,(MDTFDTE-MDTGRS)/L'MDTGRS                                     
GENTRN24 ICM   R1,15,0(RE)                                                      
         BZ    *+12                                                             
         MH    R1,=H'-1'                                                        
         STCM  R1,15,0(RE)                                                      
         LA    RE,4(RE)                                                         
         BCT   RF,GENTRN24                                                      
         ICM   R1,15,MDTVAT                                                     
         BZ    *+12                                                             
         MH    R1,=H'-1'                                                        
         STCM  R1,15,MDTVAT                                                     
         B     GENTRN08                                                         
*                                                                               
         USING XPYELD,R4                                                        
GENTRN26 CLI   CSACT,ACTREV        TEST REVERSING                               
         BNE   GENTRN08                                                         
         ZAP   BODUB1,XPYCD                                                     
         MP    BODUB1,=P'-1'                                                    
         ZAP   XPYCD,BODUB1                                                     
         B     GENTRN08                                                         
*                                                                               
         USING BNDELD,R4                                                        
GENTRN28 SR    R1,R1               CLEAR BILL NUMBER/DATE ELEMENT               
         IC    R1,BNDLN                                                         
         SHI   R1,BNDLN+2-BNDELD                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    BNDLN+1(0),BNDLN+1                                               
*&&US*&& ZAP   BNDCMP,BCPZERO                                                   
*&&UK*&& MVC   BNDBNO,BCSPACES                                                  
         B     GENTRN08                                                         
*                                                                               
         USING AFCELD,R4                                                        
GENTRN30 CLI   CSACT,ACTREV        TEST REVERSING                               
         BNE   GENTRN08                                                         
         ZAP   BODUB1,AFCAMNT      REVERSE CURRENCY AMOUNT                      
         MP    BODUB1,=P'-1'                                                    
         ZAP   AFCAMNT,BODUB1                                                   
         B     GENTRN08                                                         
*                                                                               
         USING PTAELD,R4                                                        
GENTRN32 CLI   ICBTYP,BT06         REVERSE PTA AMOUNTS FOR BT6 & 26             
         BE    GENTRN34                                                         
         CLI   ICBTYP,BT26                                                      
         BE    GENTRN34                                                         
         MVI   PTAEL,X'FF'         DROP PTA ELEMENT IF NOT                      
         B     GENTRN08                                                         
*                                                                               
GENTRN34 CLI   CSACT,ACTREV        TEST REVERSING                               
         BNE   GENTRN08                                                         
         ZAP   BODUB1,PTANET                                                    
         MP    BODUB1,=P'-1'                                                    
         ZAP   PTANET,BODUB1                                                    
         ZAP   BODUB1,PTANETF                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   PTANETF,BODUB1                                                   
         ZAP   BODUB1,PTACDSC                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   PTACDSC,BODUB1                                                   
         SR    RE,RE                                                            
         ICM   RE,3,PTAHOURS                                                    
         LCR   RE,RE                                                            
         STCM  RE,3,PTAHOURS                                                    
         CLI   PTATYPE,PTATRAL     TEST REGULAR ALLOCATION                      
         BNE   GENTRN08                                                         
         ZAP   BODUB1,PTARCORT                                                  
         MP    BODUB1,=P'-1'                                                    
         ZAP   PTARCORT,BODUB1                                                  
         ZAP   BODUB1,PTARCOM                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   PTARCOM,BODUB1                                                   
         CLI   PTALN,PTARLN2Q                                                   
         BL    GENTRN08                                                         
         ZAP   BODUB1,PTARFCOM                                                  
         MP    BODUB1,=P'-1'                                                    
         ZAP   PTARFCOM,BODUB1                                                  
         B     GENTRN08                                                         
*&&US                                                                           
         USING PRTELD,R4                                                        
GENTRN36 CLI   CSACT,ACTREV        TEST REVERSING                               
         BNE   GENTRN08                                                         
         ZAP   BODUB1,PRTHOUR      REVERSE NUMBER OF HOURS                      
         MP    BODUB1,=P'-1'                                                    
         ZAP   PRTHOUR,BODUB1                                                   
         B     GENTRN08                                                         
*&&                                                                             
         USING UNPELD,R4                                                        
GENTRN38 CLI   CSACT,ACTREV        TEST REVERSING                               
         BNE   GENTRN08                                                         
         ZAP   BODUB1,UNPUNIT      REVERSE NUMBER OF UNITS                      
         MP    BODUB1,=P'-1'                                                    
         ZAP   UNPUNIT,BODUB1                                                   
         B     GENTRN08                                                         
*                                                                               
         USING FFTELD,R4                                                        
GENTRN40 CLI   FFTTYPE,FFTTKREF    ORIGINAL KEY REFERENCE VALUE                 
         BNE   GENTRN08                                                         
         CLI   CSACT,ACTCOP        TEST COPY/REVERSE                            
         BNE   *+14                                                             
         MVC   FFTDATA(L'TRNKREF),TRNKREF                                       
         B     GENTRN08                                                         
         MVC   TRNKREF,FFTDATA                                                  
         B     GENTRN08                                                         
*                                                                               
         USING TRXELD,R4                                                        
GENTRN42 SR    RE,RE               CLEAR TRANSACTION EXTRA STATUS BITS          
         IC    RE,TRXLN                                                         
         SHI   RE,TRXSTA1+1-TRXELD                                              
         EX    RE,*+8                                                           
         B     GENTRN08                                                         
         XC    TRXSTA1(0),TRXSTA1                                               
*                                                                               
         USING RBIELD,R4                                                        
GENTRN44 MVI   RBIEL,X'FF'         DROP OLD UNBILL ELEMENT                      
         B     GENTRN08                                                         
*                                                                               
         USING CUBELD,R4                                                        
GENTRN46 MVI   CUBEL,X'FF'         DROP NEW UNBILL ELEMENT                      
         B     GENTRN08                                                         
*                                                                               
         USING VBIELD,R4                                                        
GENTRN48 CLI   CSACT,ACTREV        TEST REVERSING                               
         BNE   GENTRN08                                                         
         ZAP   BODUB1,VBIVAT       REVERSE VAT AMOUNT                           
         MP    BODUB1,=P'-1'                                                    
         ZAP   VBIVAT,BODUB1                                                    
         ZAP   BODUB1,VBIGROSS     REVERSE GROSS AMOUNT                         
         MP    BODUB1,=P'-1'                                                    
         ZAP   VBIGROSS,BODUB1                                                  
         ZAP   BODUB1,VBICOMM      REVERSE COMM AMOUNT                          
         MP    BODUB1,=P'-1'                                                    
         ZAP   VBICOMM,BODUB1                                                   
         B     GENTRN08                                                         
*&&US                                                                           
         USING PBIELD,R4                                                        
GENTRN50 CLI   CSACT,ACTREV        TEST REVERSING                               
         BNE   GENTRN08                                                         
         ZAP   BODUB1,PBIPST       REVERSE PST AMOUNT                           
         MP    BODUB1,=P'-1'                                                    
         ZAP   PBIPST,BODUB1                                                    
         ZAP   BODUB1,PBIGROSS     REVERSE GROSS AMOUNT                         
         MP    BODUB1,=P'-1'                                                    
         ZAP   PBIGROSS,BODUB1                                                  
         ZAP   BODUB1,PBICOMM      REVERSE COMM AMOUNT                          
         MP    BODUB1,=P'-1'                                                    
         ZAP   PBICOMM,BODUB1                                                   
         B     GENTRN08                                                         
*&&                                                                             
         USING PIDEL,R4                                                         
GENTRN52 CLI   CSACT,ACTREV        ARE WE REVERSING?                            
         BE    *+12                                                             
         CLI   CSACT,ACTCOP        OR COPYING?                                  
         BNE   GENTRN08            NO                                           
         MVC   PIDNO,CUPASS        YES, UPDATE THE PID                          
         B     GENTRN08                                                         
*                                                                               
GENTRN54 DS    0H                  N/D                                          
         B     GENTRN08                                                         
*                                                                               
GENTRN90 LA    R4,TRNRFST                                                       
         USING TRNELD,R4                                                        
*&&US                                                                           
         CLI   TRNTYPE,BT53        TEST ACCRUAL                                 
         BE    *+12                                                             
         CLI   TRNTYPE,BT55                                                     
         BNE   *+8                                                              
         MVI   TRNKREF+L'TRNKREF-1,C'A'                                         
*&&                                                                             
         MVC   TRNREF,TRNKREF                                                   
         GOTOR VHELLO,BOPARM,(C'D',=C'ACCMST'),(X'FF',TRNRECD),0,0              
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GENTRNX  XIT1  ,                                                                
*                                                                               
GENTRNE  LA    R1,BATMOA2H         ABEND FROM UPDATIVE FUNCTION                 
         ST    R1,FVADDR                                                        
         NI    CSINDSL1,FF-(CSIK2VAL)  CLEAR KEY 2 VALID                        
         OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         L     RD,BCSVRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  EXIT TO ROOT CALL POINT                      
         DROP  R2,R4,RC                                                         
         SPACE 1                                                                
GLOBALS  DS    0D                                                               
         LTORG                                                                  
         SPACE 1                                                                
DMUNLK   DC    C'DMUNLK '                                                       
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
                                                                                
DEFDISP  DS    0CL8                                                             
         DC    C'0123'                                                          
         DC    4X'00'                                                           
                                                                                
***********************************************************************         
* OPTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
ILOTAB   DS    0X                                                               
*                                                                               
         DC    AL2(UC8REF-TWAD,UC3REF-TWAD)                                     
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,ILOREFL+1,ILOREFL)                               
         DC    AL1(1)                                                           
         DC    AL2(1,ILOREF-ILVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8DATE-TWAD,UC3DATE-TWAD)                                   
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'PVALCPER,ILODATL)                              
         DC    AL1(2)                                                           
         DC    AL2(2,ILODAT-ILVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8AMT-TWAD,UC3AMT-TWAD)                                     
         DC    AL1(OPTNRTN,OPTGEQ+OPTLEQ+OPTPLQ+OPTMIQ)                         
         DC    AL1(0,0,0,0,0,1,13,L'ILOAMT)                                     
         DC    AL1(3)                                                           
         DC    AL2(3,ILOAMTI-ILVALS)                                            
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8ACC-TWAD,UC3ACC-TWAD)                                     
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'ILOULA,L'ILOULAL+L'ILOULA)                     
         DC    AL1(4)                                                           
         DC    AL2(4,ILOULAL-ILVALS)                                            
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,ILODISL,ILODISL)                                 
         DC    AL1(5)                                                           
         DC    AL2(5,ILODISI-ILVALS)                                            
         DC    CL4'++'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC@ITEM-TWAD,UC@ITEM-TWAD)                                   
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,11,ILOITEL)                                      
         DC    AL1(6)                                                           
         DC    AL2(6,ILOITE-ILVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC@DELD-TWAD,UC3DELD-TWAD)                                   
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,10,L'ILODEL)                                     
         DC    AL1(7)                                                           
         DC    AL2(7,ILODEL-ILVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
         DC    AL2(UC@EXPAN-TWAD,UC@EXPAN-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,10,L'ILOEXP)                                     
         DC    AL1(8)                                                           
         DC    AL2(8,ILOEXP-ILVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
ILOTABX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
CTRNBITS EQU   TRNSDR+TRNSNOCM+TRNSURG+TRNSAUTH                                 
CTRSBITS EQU   FF-(TRSSGLIP)                                                    
CTRSBIT2 EQU   TRSSTADJ+TRSSTMSS+TRSSTIME                                       
CTRSBIT3 EQU   TRSSPROG+TRSSXJOB                                                
*                                                                               
RTRNBITS EQU   FF-(TRNSREV)                                                     
RTRSBITS EQU   FF-(TRSSGLIP)                                                    
RTRSBIT2 EQU   TRSSTADJ+TRSSTMSS+TRSSTIME                                       
RTRSBIT3 EQU   TRSSPROG+TRSSXJOB                                                
         EJECT                                                                  
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
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLY1H                                                         
       ++INCLUDE ACBATADD                                                       
         ORG   OSVALS                                                           
ILVALS   DS    0X                  ** ITEM/LIST VALUES **                       
ILOKOPTS DS    0X                  ** ITEM/LIST KEY OPTIONS **                  
ILOREF   DS    0X                  REFERENCE RANGE                              
ILOREFST DS    CL(L'BIAREF)                                                     
ILOREFND DS    CL(L'BIAREF)                                                     
ILOREFL  EQU   *-ILOREF                                                         
ILODAT   DS    0X                  DATE RANGE                                   
ILODATST DS    PL(L'TRNKDATE)                                                   
ILODATND DS    PL(L'TRNKDATE)                                                   
ILODATL  EQU   *-ILODAT                                                         
ILOULAL  DS    XL1                 LENGTH FOR ILOULA COMPARE                    
ILOULA   DS    CL(L'TRNKCULA-1)    UNIT/LEDGER/ACCOUNT CODE                     
ILOAMTI  DS    XL1                 AMOUNT FILTER                                
ILOAMT   DS    PL(L'BIAAMT)                                                     
ILOITE   DS    0X                  ITEM# RANGE                                  
ILOITEST DS    CL(L'TBAKTSEQ)                                                   
ILOITEND DS    CL(L'TBAKTSEQ)                                                   
ILOITEL  EQU   *-ILOITE                                                         
ILODEL   DS    XL1                                                              
ILODELYQ EQU   X'80'               INCLUDE DELETED ITEMS                        
ILODELNQ EQU   X'40'               EXCLUDE DELETED ITEMS                        
ILODELOQ EQU   X'20'               DELETED ITEMS ONLY                           
ILOEXP   DS    XL1                                                              
ILOEXPNQ EQU   X'80'               DON'T EXPAND SCREEN ITEMS                    
ILOKOPTL EQU   *-ILOKOPTS                                                       
*                                                                               
ILODOPT  DS    0X                                                               
ILODISI  DS    XL1                                                              
ILODISIS EQU   X'80'               ILODIS SUFFIXES DEFAULT COLUMNS              
ILODISIP EQU   X'40'               ILODIS PREFIXES DEFAULT COLUMNS              
ILODIS   DS    XL(DISTABL)         OPTION DISPLAY INFORMATION                   
ILODISL  EQU   *-ILODIS                                                         
ILODOPTL EQU   *-ILODOPT                                                        
ILVALSL  EQU   *-ILVALS                                                         
*                                                                               
ILODVALS DS    0XL2                SAVED DISPLAY VALUES (FOR SCROLLING)         
ILODINDX DS    XL1                 INDEX TO CURRENT DISPLAY COLUMN              
ILODINUM DS    XL1                 NUMBER OF COLUMNS DISPLAYED                  
*                                                                               
ILHISNO  DS    XL2                 HIGH MULTI-ITEM SCREEN NUMBER                
*                                                                               
ILTBASAV DS    XL(L'TBAKEY)        SAVED BATCH RECORD KEY                       
ILTBASIT DS    XL(L'LSTBITMA)      SAVED NUMBER OF ITEMS IN BATCH               
ILTBACUR DS    XL(L'TBAKTSEQ)      SAVED BATCH ITEM NUMBER                      
*                                                                               
ILDHEAD1 DS    CL(L'ILLINE)        DISPLAY COLUMN HEADING                       
ILDHEAD2 DS    CL(L'ILLINE)        DISPLAY COLUMN HEADING                       
ILDSDSP  DS    0X                  DISPLACEMENTS FOR SCREEN DISPLAY             
ILDSAMNT DS    XL1                 DISPLACEMENT TO AMOUNT                       
ILDSACC1 DS    XL1                 DISPLACEMENT TO ACCOUNT#1                    
ILDSACC2 DS    XL1                 DISPLACEMENT TO ACCOUNT#2                    
ILDSSTA  DS    XL1                 DISPLACEMENT TO STATUS                       
ILDSITEM DS    XL1                 DISPLACEMENT TO ALTERNATIVE ITEM NO.         
ILDSACTS DS    XL1                 DISPLACEMNT TO VALID ACTIONS                 
ILDSCURR DS    XL1                 DISPLACEMENT TO CURRENCY                     
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
         DS    XL1                 DISPLACEMENT TO N/D                          
ILDSADDR DS    XL1                 DISPLACEMNT TO DISK ADDRESS                  
ILDSRNUM DS    XL1                 DISPLACEMENT TO RECORD NUMBER                
ILDSDSPL EQU   *-ILDSDSP                                                        
ILDRDSP  DS    0X                  DISPLACEMENTS FOR REPORT DISPLAY             
         DS    XL32                                                             
ILDRDSPL EQU   *-ILDRDSP                                                        
         SPACE 1                                                                
ILVACTWD EQU   3                   CHRS IN VALID ACTIONS WORDS                  
         ORG   OSVALS+OSVALSL                                                   
         SPACE 1                                                                
         ORG   OSVALS                                                           
IDVALS   DS    0X                  ** ITEM/DISPLAY VALUES **                    
IDIND1   DS    XL1                 INDICATOR - 1                                
IDI1NATV EQU   X'80'               NATIVE CALL                                  
IDTBASAV DS    XL(L'TBAKEY)        SAVED BATCH RECORD KEY                       
IDTBASIT DS    XL(L'LSTBITMA)      SAVED NUMBER OF ITEMS IN BATCH               
IDTBACUR DS    XL(L'TBAKTSEQ)      SAVED BATCH ITEM NUMBER                      
IDMSGNO  DS    XL(L'FVMSGNO)       SAVED ITEM MESSAGE                           
         SPACE 1                                                                
         ORG   OSVALS                                                           
ICVALS   DS    0X                  ** ITEM/COPY VALUES **                       
ICPFKEY  DS    XL1                 PFKEY ON ENTRY/RETURN TO ITECOPR1            
ICMSGNO  DS    XL(L'FVMSGNO)       MESSAGE NUMBER FOR GOOD EXIT                 
ICLSTSAV DS    XL(LSTTABL)         SAVE AREA FOR CSLSTCUR                       
         SPACE 1                                                                
         ORG   OSVALS                                                           
IGVALS   DS    0X                  ** ITEM/GENERATE VALUES **                   
IGPFKEY  DS    XL1                 PFKEY ON ENTRY/RETURN TO ITEGENR1            
IGMSGNO  DS    XL(L'FVMSGNO)       MESSAGE NUMBER FOR GOOD EXIT                 
IGLSTSAV DS    XL(LSTTABL)         SAVE AREA FOR CSLSTCUR                       
         SPACE 1                                                                
         ORG   OSVALS                                                           
BGVALS   DS    0X                  ** BATCH/GENERAL VALUES **                   
BGLSTSAV DS    XL(LSTTABL)         SAVE AREA FOR CSLSTCUR                       
BGTBASAV DS    XL(L'TBAKEY)        SAVED BATCH RECORD KEY                       
BGTBASIT DS    XL(L'LSTBITMA)      SAVED NUMBER OF ITEMS IN BATCH               
BGTBACUR DS    XL(L'TBAKTSEQ)      SAVED BATCH ITEM NUMBER                      
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACBAT61   06/05/13'                                      
         END                                                                    
