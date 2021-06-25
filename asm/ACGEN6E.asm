*          DATA SET ACGEN6E    AT LEVEL 165 AS OF 07/10/18                      
*PHASE T00A6EA                                                                  
*INCLUDE ACRAPPER                                                               
*INCLUDE TMSUPD                                                                 
ROUT2    TITLE 'GENERAL FACILITIES - OVERLAY 2'                                 
ROUT2    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ROU2**,RR=R6                                                 
         USING R2WORKD,RC                                                       
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         BASR  R8,0                                                             
         AHI   R8,LITERALS-*                                                    
         USING LITERALS,R8         R8=A(GLOBAL LITERALS)                        
***********************************************************************         
* PATCH FIXIT TO Y TO RUN THE SPECIAL VERSION FOR BATCHES ALREADY     *         
* UPDATED.                                                            *         
***********************************************************************         
         SR    RE,RE                                                            
         SLDL  RE,8                BRANCH INDEX HELD IN HOB RF                  
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          ENSURE GOOD INDEX VALUE                      
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                ROUTINE NOT DEFINED                          
         LA    RF,ROUT2(RF)        RF=A(ROUTINE)                                
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=TEMPORARY W/S AMOUNT                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AHI   R5,7                ROUND W/S AMOUNT TO DOUBLEWORDS              
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               ACQUIRE STORAGE FROM W/S POOL                
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               AND CLEAR IT                                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R4,R2                                                            
                                                                                
         ST    R6,R2RELO           SAVE RELOCATION FACTOR                       
         LHI   R5,BSVALS-TWAD                                                   
         LA    R5,TWAD(R5)                                                      
         USING BSVALS,R5           R5=A(TWA SAVE AREA)                          
         BR    RF                                                               
         DROP  RB                                                               
                                                                                
ROUTAB   DS    0XL4                                                             
         DC    AL2(ADDHDR-ROUT2),AL2(AHWORKL)                                   
         DC    AL2(BLDBAK-ROUT2),AL2(R2WORKL)                                   
         DC    AL2(BLDBAP-ROUT2),AL2(R2WORKL)                                   
         DC    AL2(DISHDR-ROUT2),AL2(DHWORKL)                                   
         DC    AL2(PSTACR-ROUT2),AL2(PAWORKL)                                   
         DC    AL2(ADDITE-ROUT2),AL2(AIWORKL)                                   
         DC    AL2(UPDBAT-ROUT2),AL2(UBWORKL)                                   
         DC    AL2(INIADT-ROUT2),AL2(R2WORKL)                                   
         DC    AL2(CLOBAT-ROUT2),AL2(UBWORKL)                                   
         DC    AL2(BLDACR-ROUT2),AL2(R2WORKL)                                   
         DC    AL2(DISSTA-ROUT2),AL2(DSWORKL)                                   
         DC    AL2(RCLBAT-ROUT2),AL2(UBWORKL)                                   
         DC    AL2(SAVBAT-ROUT2),AL2(UBWORKL)                                   
         DC    AL2(GETBAT-ROUT2),AL2(GBWORKL)                                   
         DC    AL2(DELBAT-ROUT2),AL2(UBWORKL)                                   
         DC    AL2(ADDMON-ROUT2),AL2(AMWORKL)                                   
         DC    AL2(GETCUR-ROUT2),AL2(GCWORKL)                                   
         DC    AL2(VALHRS-ROUT2),AL2(VHWORKL)                                   
         DC    AL2(UPDORD-ROUT2),AL2(UOWORKL)                                   
         DC    AL2(SAVFLD-ROUT2),AL2(R2WORKL)                                   
         DC    AL2(RESFLD-ROUT2),AL2(R2WORKL)                                   
         DC    AL2(RESVAT-ROUT2),AL2(RVWORKL)                                   
         DC    AL2(CHKINV-ROUT2),AL2(CIWORKL)                                   
ROUTABL  EQU   *-ROUTAB                                                         
                                                                                
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         J     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         J     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
                                                                                
ROUTX    XIT1  ,                   EXIT WITH CC SET                             
                                                                                
R2WORKD  DSECT                     ** MODULE W/S **                             
                                                                                
R2GLOBAL DS    0F                  ** GLOBAL VALUES **                          
R2RELO   DS    F                   RELOCATION FACTOR                            
R2WORKL  EQU   *-R2WORKD                                                        
                                                                                
R2SHARED DS    0F                  ** SHARED VALUES **                          
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AND ADD A BATCH HEADER                          *         
*                                                                     *         
* NTRY - P1 BYTES 0    X'FF' P3 PRESENT                               *         
*                 1-3  A(REFERENCE FIELD HEADER) OR ZERO              *         
*        P2 BYTES 0    X'NN' NUMBER OF COMMENT LINES                  *         
*                 1-3  A(1ST COMMENT LINE FIELD HEADER)               *         
*        P3 BYTES 0    X'FF' TEST BATCH HEADER DOESN'T EXIST          *         
*                 1-3  A(ORIGINAL BATCH LIST ENTRY)                   *         
*        P4 BYTES 0    X'00' USER PASSING A(1ST FFTEL)                *         
*                 1-3  A(1ST FFTEL FROM EXISTING BATCH IN AIO2)       *         
*                                                                     *         
***********************************************************************         
                                                                                
ADDHDR   J     *+12                                                             
         DC    C'*ADDHDR*'                                                      
         LR    RB,RF                                                            
         USING ADDHDR,RB                                                        
         MVC   AHPARM,0(R1)        SAVE CALLING PARAMETER LIST                  
         LA    R4,CSLSTCUR                                                      
         USING LSTTABD,R4          R4=A(CURRENT LSTTAB ENTRY)                   
         OC    AHAREF,AHAREF       TEST INTERNAL CALL, LSTTAB BUILT             
         BZ    ADDHDR02                                                         
         MVI   LSTTRTYP,RECBAT     SET BATCH RECORD                             
         MVC   LSTBBTYP,CSBTYP     SET BATCH TYPE                               
         MVC   LSTBGRUP,CSBGRUP    SET BATCH TYPE GROUP                         
         MVC   LSTBBCHR,CUPASS     SET BATCHER NUMBER                           
         MVC   LSTBIBNO,CUPASS     SET INPUT BATCHER NUMBER                     
         MVC   LSTBPID,CSBPID      SET BATCHER PUBLIC-ID                        
         MVC   LSTBADDT,BCTODAYC   SET ADDED DATE                               
         MVC   LSTBUSER,CUUSER     SET USER-ID                                  
         CLI   CUACCS,C'*'         SET ONE/TWO CHARACTER OFFICE                 
         BNE   *+14                                                             
         MVC   LSTBOFFC(1),CUACCS+1                                             
         B     *+10                                                             
         MVC   LSTBOFFC,CUACCS                                                  
         TM    CSINDSG1,CSINDIUP   TEST INSTANT UPDATE BATCH                    
         BZ    *+8                                                              
         OI    LSTBINDS,LSTBIIUP                                                
         XC    LSTBITMA,LSTBITMA   SET ITEMS ADDED                              
         ZAP   LSTBCSHA,BCPZERO    SET CASH ADDED                               
         TM    CSBIND8,TYPIDCSA    TEST DR/CR SUBSIDIARILY ACCUMULATED          
         BO    *+20                                                             
         TM    CSBIND1,TYPICUMU    TEST ACCUMULATING TOTAL DRS/CRS              
         BO    *+12                                                             
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS/TAX                  
         BZ    *+16                                                             
         ZAP   LSTBTDRS,BCPZERO    SET TOTAL DEBITS                             
         ZAP   LSTBTCRS,BCPZERO    SET TOTAL CREDITS                            
         TM    CSBIND2,TYPIMLT     TEST/SET MULTIPLE ITEM/CHANGE                
         BZ    *+8                                                              
         OI    LSTBIND2,LSTBIMLT                                                
         TM    CSBIND3,TYPIMSIC    TEST/SET TYPIMSIC ITEM/CHANGE                
         BZ    *+8                                                              
         OI    LSTBIND2,LSTBIMSI                                                
         OI    LSTBIND2,LSTBADPP   ADD A PASSIVE POINTER                        
                                                                                
ADDHDR02 GOTOR ABLDBAK,LSTTABD     BUILD ACTIVE BATCH HEADER KEY                
         LA    R2,IOKEY                                                         
         USING TBAKEY,R2                                                        
         LHI   R1,IOHIUPD+IOACCDIR+IO1                                          
         L     RF,BCAUTL           TEST SCRIPT EXECUTION                        
         TM    TSTAT6-UTLD(RF),TST6SCRP                                         
         BNZ   ADDHDR10                                                         
         XC    TBAKBCHR(TBAKLAST-TBAKBCHR),TBAKBCHR                             
                                                                                
ADDHDR10 GOTOR AIO                 ISSUE I/O                                    
         JL    ROUTX               EXIT ON HARDWARE ERROR                       
         TM    IOERR,IOEEOF        OR END OF FILE                               
         JNZ   ROUTX                                                            
         LA    R2,IOKEY                                                         
         CLC   TBAKEY(TBAKBCHR-TBAKEY),IOKEYSAV                                 
         BNE   ADDHDR16                                                         
         L     RF,BCAUTL           TEST SCRIPT EXECUTION                        
         TM    TSTAT6-UTLD(RF),TST6SCRP                                         
         BZ    ADDHDR12                                                         
         CLC   TBAKEY(TBAKOFFC-TBAKEY),IOKEYSAV                                 
         BNE   ADDHDR16                                                         
         IC    RF,LSTBSEQN         DUPLICATE KEY - BUMP SUB-REFERENCE           
         AHI   RF,1                                                             
         STC   RF,LSTBSEQN                                                      
         CLI   LSTBSEQN,0          TEST FOR WRAP-AROUND                         
         BE    ADDHDRE1                                                         
         B     ADDHDR02            NO - GO AND TRY AGAIN                        
                                                                                
ADDHDR12 TM    TBAKHSTA,TBAHSDEL   TEST ACTIVE RECORD IS DELETED                
         BZ    ADDHDRE1                                                         
         CLC   TBAKBCHR,LSTBBCHR                                                
         BNE   ADDHDR14                                                         
         CLC   TBAKOFFC,LSTBOFFC                                                
         BNE   ADDHDR14                                                         
         OI    LSTBINDS,LSTBIAPD   SET ACTIVE RECORD FOUND BUT DELETED          
         MVC   LSTBSEQN,TBAKSEQN   SAVE SEQUENCE OF DELETED RECORD              
         MVC   AHIODA,IODA         SAVE D/A OF DUPLICATE BATCH HEADER           
         B     ADDHDR16                                                         
                                                                                
ADDHDR14 LHI   R1,IOSQUPD+IOACCDIR+IO1                                          
         B     ADDHDR10                                                         
                                                                                
ADDHDR16 GOTOR ABLDBAP,LSTTABD     BUILD PASSIVE BATCH HEADER KEY               
         LA    R2,IOKEY                                                         
         USING TBAPAS,R2                                                        
         LHI   R1,IOHIUPD+IOACCDIR+IO1                                          
                                                                                
ADDHDR18 GOTOR AIO                 ISSUE I/O                                    
         JL    ROUTX               EXIT ON HARDWARE ERROR                       
         TM    IOERR,IOEEOF        OR END OF FILE                               
         JNZ   ROUTX                                                            
         CLC   TBAPAS(TBAPBCHR-TBAPAS),IOKEYSAV                                 
         BNE   ADDHDR22                                                         
         CLC   TBAPTSEQ,LSTBADDT                                                
         BNE   ADDHDR22                                                         
         L     RF,BCAUTL           TEST SCRIPT EXECUTION                        
         TM    TSTAT6-UTLD(RF),TST6SCRP                                         
         BZ    ADDHDR20                                                         
         CLC   TBAKEY(TBAKOFFC-TBAKEY),IOKEYSAV                                 
         BNE   ADDHDR22                                                         
                                                                                
ADDHDR20 TM    TBAKHSTA,TBAHSDEL   TEST PASSIVE IS DELETED                      
         BZ    ADDHDRE1                                                         
         CLC   TBAPBCHR,LSTBBCHR                                                
         BNE   *+10                                                             
         CLC   TBAPOFFC,LSTBOFFC                                                
         BNE   *+12                                                             
         OI    LSTBINDS,LSTBIPPD   SET PASSIVE RECORD FOUND BUT DELETED         
         B     ADDHDR22                                                         
         LHI   R1,IOSQUPD+IOACCDIR+IO1                                          
         B     ADDHDR18                                                         
                                                                                
ADDHDR22 CLI   AHTEST,FF           TESTING ONLY                                 
         JE    ROUTE               GIVE GOOD EXIT                               
         TM    LSTBINDS,LSTBIAPD   TEST DELETED DUPLICATE RECORD FOUND          
         BZ    ADDHDR24                                                         
         MVC   IODAOVER,AHIODA                                                  
         GOTOR AIO,IOGETRUP+IOACCMST+IO1                                        
                                                                                
ADDHDR24 GOTOR ABLDBAK,LSTTABD     BUILD ACTIVE BATCH KEY                       
         L     R2,AIO1             R2=A(BATCH RECORD AREA)                      
         MVC   TBAKEY,IOKEY                                                     
         XC    TBARSTA(TBARFST-TBARSTA),TBARSTA                                 
         MVI   TBARHSTA,TBAHSIIP   INPUT IN PROGRESS                            
         TM    CSINDSG1,CSINDIUP   TEST INSTANT UPDATE BATCH                    
         BZ    *+8                                                              
         OI    TBARHSTA,TBAHSIAD   SET OPEN FOR INSTANT UPDATE                  
         TM    LSTBHDS2,BHDSGLUD   SKIP THESE IF GLUING                         
         BO    ADDHDR26                                                         
         TM    LSTBIND2,LSTBIMLT   TEST MULTIPLE ITEM/CHANGE BATCH              
         BZ    *+8                                                              
         OI    TBAHRIND,TBAHIMLT   SET MULTIPLE ITEM/CHANGE BATCH               
         CLI   AHAIND,AHAIOLST     TEST ORIGINAL BATCH LIST ENTRY               
         BNE   ADDHDR26                                                         
         L     RF,AHAOLST                                                       
         TM    LSTBINDS-LSTTABD(RF),LSTBBCBG                                    
         BZ    ADDHDR26                                                         
         OI    TBAHRIND,TBAHIGDJ                                                
                                                                                
ADDHDR26 MVC   TBAHRADT,BCTODAYC   ADDED TO FILE TODAY                          
         MVC   TBAHREDT,LSTBEFDT   EFFECTIVE DATE                               
         MVC   LSTTSTAT,TBARHSTA                                                
         OI    LSTBINDS,LSTBINPT   SET INPUT SESSION IN PROGRESS                
         GOTOR ASETMSK,LSTTABD     SET VALID ACTION MASK FOR RECORD             
         LA    R3,TBARFST                                                       
         XC    0(256,R3),0(R3)                                                  
                                                                                
         USING BHDELD,R3           R3=A(HEADER ELEMENT)                         
         MVI   BHDEL,BHDELQ                                                     
         MVI   BHDLN,BHDLNQ                                                     
         MVC   BHDNAME,LSTBNAME                                                 
         MVC   BHDSTAT1,LSTBHDS1                                                
         MVC   BHDSTAT2,LSTBHDS2                                                
         TM    CSBIND8,TYPIDETL    TEST DETAIL SCREEN                           
         BNO   *+8                                                              
         OI    BHDSTAT2,BHDSDETL                                                
         MVC   BHDITEMC,LSTBITMC                                                
         ZAP   BHDCASHC,LSTBCSHC                                                
         ZAP   BHDCASHA,BCPZERO                                                 
         MVC   BHDLUID,CUTSYM                                                   
         MVC   BHDIBNO,LSTBIBNO                                                 
         LHI   R0,BHDLNQ                                                        
         TM    CSBIND8,TYPIDCSA    TEST DR/CR SUBSIDIARILY ACCUMULATED          
         BO    *+20                                                             
         TM    CSBIND1,TYPICUMU    TEST ACCUMULATING TOTAL DRS/CRS              
         BO    *+12                                                             
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS/TAX                  
         BZ    ADDHDR28                                                         
         MVI   BHDLN,BHDLN2Q       EXTEND ELEMENT                               
         ZAP   BHDTOTDR,BCPZERO    SET TOTAL DEBITS                             
         ZAP   BHDTOTCR,BCPZERO    SET TOTAL CREDITS                            
         LHI   R0,BHDLN2Q                                                       
                                                                                
         USING FFTELD,R3           R3=A(FREE FORM TEXT ELEMENT)                 
ADDHDR28 AR    R3,R0               POINT TO NEXT ELEMENT                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q                                                    
         MVI   FFTTYPE,FFTTFREE                                                 
         MVI   FFTSEQ,1                                                         
         SR    R1,R1                                                            
         ICM   R1,7,AHACOM         R1=A(FIRST COMMENT FIELD IN TWA)             
         BZ    ADDHDR38                                                         
         SR    R0,R0                                                            
         ICM   R0,1,AHNCOM         R0=NUMBER OF COMMENT LINES OR ZERO           
         BNZ   ADDHDR32                                                         
                                                                                
ADDHDR30 CLI   0(R1),FFTELQ        R1=A(FFTEL IN EXISTING BATCH)                
         BNE   ADDHDR38                                                         
         SR    RF,RF                                                            
         IC    RF,FFTLN-FFTELD(R1)                                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FFTEL(0),0(R1)                                                   
         LA    R1,1(RF,R1)                                                      
         LA    R3,1(RF,R3)                                                      
         B     ADDHDR30                                                         
                                                                                
ADDHDR32 GOTOR AFVAL,(R1)                                                       
         BNE   ADDHDR34                                                         
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         LA    RF,FFTELD(RF)                                                    
         USING FFTDLEN,RF          RF=A(NEXT COMMENT)                           
         MVC   FFTDLEN,FVILEN      SET COMMENT LENGTH                           
         SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FFTDATA(0),FVIFLD   SET COMMENT                                  
         DROP  RF                                                               
         LA    RF,2(RE,RF)                                                      
         LA    RE,FFTELD                                                        
         SR    RF,RE                                                            
         STC   RF,FFTLN            SET NEW ELEMENT LENGTH                       
                                                                                
ADDHDR34 BCT   R0,*+8              BUMP TO NEXT COMMENT IN TWA                  
         B     ADDHDR38                                                         
         SR    RE,RE                                                            
ADDHDR36 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         AR    R1,RE                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   ADDHDR36                                                         
         B     ADDHDR32                                                         
                                                                                
ADDHDR38 CLI   FFTLN,FFTLN1Q       TEST ANY COMMENTS                            
         BNE   *+14                                                             
         XC    FFTELD(FFTLN1Q),FFTELD                                           
         B     ADDHDR40                                                         
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         AR    R3,RF                                                            
                                                                                
ADDHDR40 CLI   AHAIND,AHAIOLST     TEST ORIGINAL BATCH LIST ENTRY               
         BNE   ADDHDR42                                                         
         L     R1,AHAOLST                                                       
         GOTOR ABLDBAK             BUILD ORIGINAL BATCH KEY                     
         USING ASKELD,R3                                                        
         MVI   ASKEL,ASKELQ                                                     
         MVI   ASKLN,ASKLNQ                                                     
         MVI   ASKSEQN,0                                                        
         MVC   ASKKEY,IOKEY        SAVE KEY ON NEW BATCH HEADER                 
         LA    R3,ASKELD+ASKLNQ                                                 
                                                                                
ADDHDR42 MVI   0(R3),0             SET END OF RECORD                            
         LA    R0,TBARECD                                                       
         BCTR  R0,0                                                             
         SR    R3,R0                                                            
         STCM  R3,3,TBARLEN        SET RECORD LENGTH                            
                                                                                
ADDHDR44 LHI   R1,IOADDREC+IOACCMST+IO1                                         
         TM    LSTBINDS,LSTBIAPD   TEST RECORD EXISTS (THOUGH DELETED)          
         BZ    *+8                                                              
         LHI   R1,IOPUTREC+IOACCMST+IO1                                         
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY            R2=A(BATCH KEY)                              
         L     R1,AIO1             SAVE KEY AND DISK ADDRESS                    
         MVC   AHSTA,TBARSTA-TBARECD(R1)                                        
         MVC   LSTTDA,IODA         SET DISK ADDRESS IN LSTTAB ENTRY             
         TM    LSTBINDS,LSTBIAPD   TEST RECORD IS ON FILE (DELETED)             
         BZ    ADDHDR46                                                         
         MVC   TBAKEY,0(R1)        BUILD DIRECTORY KEY                          
         GOTOR AIO,IORDUPD+IOACCDIR+IO1                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TBAKSTA,AHSTA                                                    
         MVC   TBAKDA,LSTTDA                                                    
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
ADDHDR46 GOTOR ABLDBAP,LSTTABD     BUILD PASSIVE POINTER                        
         MVC   TBAKSTA,AHSTA                                                    
         MVC   TBAKDA,IODA                                                      
         LHI   R1,IOADD+IOACCDIR+IO1                                            
         TM    LSTBINDS,LSTBIPPD   TEST RECORD IS ON FILE (DELETED)             
         BZ    ADDHDR48                                                         
         GOTOR AIO,IORDUPD+IOACCDIR+IO1                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TBAKSTA,AHSTA                                                    
         MVC   TBAKDA,LSTTDA                                                    
         LHI   R1,IOWRITE+IOACCDIR+IO1                                          
                                                                                
ADDHDR48 GOTOR AIO                 ADD OR WRITE PASSIVE POINTER                 
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
ADDHDRX  J     ROUTE                                                            
                                                                                
ADDHDRE1 MVC   FVMSGNO,=AL2(AE$BATAE)  BATCH ALREADY EXISTS                     
         MVC   FVADDR,AHAREF                                                    
                                                                                
ADDHDREX XC    LSTTABD(LSTTABL),LSTTABD                                         
         J     ROUTH                                                            
         DROP  R2,R4,RB                                                         
                                                                                
R2WORKD  DSECT                     ** ADDHDR S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
AHPARM   DS    0XL12               ADDHDR PARAMETER LIST                        
AHAIND   DS    0X                  INDICATOR                                    
AHAIOLST EQU   FF                  P3 IS ORIGINAL BATCH LIST ENTRY              
AHAREF   DS    A                   A(REFERENCE FIELD HEADER) OR ZERO            
AHNCOM   DS    X                   NUMBER OF COMMENT LINES                      
         ORG   AHNCOM                                                           
AHTEST   DS    X                   X'FF' - TEST FOR BATCH ONLY                  
AHACOM   DS    AL3                 A(FIRST COMMENT LINE)                        
AHAOLST  DS    A                   A(ORIGINAL BATCH LIST ENTRY)                 
AHSTA    DS    XL(L'TBAKSTA)       BATCH STATUS                                 
AHWORK   DS    XL64                WORK AREA                                    
AHIODA   DS    XL(L'IODA)          DUPLICATE BATCH HEADER DISK ADDRESS          
AHWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD LSTTAB ENTRY FOR ACCRUAL REVERSAL BATCH                       *         
***********************************************************************         
                                                                                
BLDACR   LR    R4,R1               R4=A(ACCRUAL REVERSAL LSTTAB ENTRY)          
         USING LSTTABD,R4                                                       
         MVI   LSTBINDS,0                                                       
         MVI   LSTBHDS1,BHDSACRV   SET ACCRUAL REVERSAL BATCH                   
         MVC   LSTBADDT,BCTODAYC   ADDED TODAY                                  
         MVC   LSTBEFDT,BCTODAYC   EFFECTIVE TODAY                              
         ZAP   BCDUB,LSTBCSHA                                                   
         MP    BCDUB,=P'-1'                                                     
         ZAP   LSTBCSHA,BCDUB                                                   
         ZAP   BCDUB,LSTBCSHC                                                   
         MP    BCDUB,=P'-1'                                                     
         ZAP   LSTBCSHC,BCDUB                                                   
         SR    RE,RE                                                            
         ICM   RE,1,CSBTYP2        TEST/SET SECOND BATCH TYPE                   
         JZ    *+8                                                              
         STC   RE,LSTBBTYP                                                      
                                                                                
         MVC   BCWORK(L'LSTBMOSP),LSTBMOSP                                      
         MVI   BCWORK+L'LSTBMOSP,X'00'                                          
         GOTOR AADDMON,BCWORK                                                   
         MVC   LSTBMOSP,BCWORK                                                  
                                                                                
         MVC   LSTBMOSC(1),LSTBMOSP                                             
         OI    LSTBMOSC,X'F0'                                                   
         SR    RE,RE                                                            
         IC    RE,LSTBMOSP+1                                                    
         LA    RE,MOSTAB-1(RE)                                                  
         MVC   LSTBMOSC+1(1),0(RE)                                              
                                                                                
         GOTOR ATSTBMO,0           TEST LSTBMOSP IS VALID                       
         JE    BLDACR02                                                         
         MVC   FVMSGNO,=AL2(AE$ABMLK)  MONTH LOCKED                             
         CLI   BCWORK+(BMOERR-BMONVALD),BMOELOKQ                                
         JE    *+10                                                             
         MVC   FVMSGNO,=AL2(AE$ABMRG)  MONTH OUT OF RANGE                       
         MVC   FVXTRA,BCSPACES                                                  
         MVC   BCWORK(L'LSTBMOSP),LSTBMOSP                                      
         MVI   BCWORK+L'LSTBMOSP,X'01'                                          
         GOTOR VDATCON,BOPARM,(1,BCWORK),(9,FVXTRA)                             
         J     ROUTH                                                            
                                                                                
BLDACR02 GOTOR AADDHDR,BCPARM,0,('FF',0)                                        
         J     ROUTX               CC EQU IF OK, NEQ IF HEADER EXISTS           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD ACTIVE BATCH HEADER POINTER                                   *         
***********************************************************************         
                                                                                
         USING LSTTABD,R1                                                       
BLDBAK   LA    RF,IOKEY                                                         
         USING TBARECD,RF          RF=A(BATCH RECORD KEY)                       
         XC    TBAKEY,TBAKEY       CLEAR AND BUILD KEY                          
         MVI   TBAKTYP,TBAKTYPQ                                                 
         MVC   TBAKCPY,CUABIN                                                   
         MVC   TBAKUSER,LSTBUSER                                                
         MVC   TBAKBREF,LSTBBREF                                                
         MVC   TBAKGRUP,LSTBGRUP                                                
         MVC   TBAKBTYP,LSTBBTYP                                                
         MVC   TBAKBMOS,LSTBMOSP                                                
         MVC   TBAKADDT,LSTBADDT                                                
         XC    TBAKADDT,BCEFFS                                                  
         MVC   TBAKBCHR,LSTBBCHR                                                
         MVC   TBAKSEQN,LSTBSEQN                                                
         MVC   TBAKOFFC,LSTBOFFC                                                
         J     ROUTE                                                            
         DROP  R1,RF                                                            
                                                                                
***********************************************************************         
* BUILD PASSIVE BATCH HEADER POINTER                                  *         
***********************************************************************         
                                                                                
         USING LSTTABD,R1                                                       
BLDBAP   LA    RF,IOKEY                                                         
         USING TBARECD,RF          RF=A(BATCH RECORD KEY)                       
         XC    TBAPAS,TBAPAS       CLEAR AND BUILD KEY                          
         MVI   TBAPTYP,TBAPTYPQ                                                 
         MVC   TBAPCPY,CUABIN                                                   
         MVC   TBAPUSER,LSTBUSER                                                
         MVC   TBAPBREF,LSTBBREF                                                
         MVC   TBAPGRUP,LSTBGRUP                                                
         MVC   TBAPBTYP,LSTBBTYP                                                
         MVC   TBAPBMOS,LSTBMOSP                                                
         MVC   TBAPEFDT,LSTBEFDT                                                
         MVC   TBAPBCHR,LSTBBCHR                                                
         MVC   TBAPSEQN,LSTBSEQN                                                
         MVC   TBAPOFFC,LSTBOFFC                                                
         TM    LSTBIND2,LSTBADPP   IS ADDED DATE IN PASSIVE POINTER?            
         JZ    *+10                                                             
         MVC   TBAPTSEQ,LSTBADDT   YES - MOVE IT IN                             
         J     ROUTE                                                            
         DROP  R1,RF                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY A BATCH HEADER                                   *         
*                                                                     *         
* NTRY - R1 LOB=DISPLAY MENU NUMBER                                   *         
*               X'80' BIT ON BUILD SCREEN ONLY                        *         
*               X'40' BIT ON INHIBIT BATCH RECORD READ                *         
*               X'20' BIT ON READ ORIGIN BATCH RECORD                 *         
*               X'10' BIT ON DISPLAY ORIGIN BATCH AS "TO" RECORD      *         
***********************************************************************         
                                                                                
DISHDR   J     *+12                                                             
         DC    C'*DISHDR*'                                                      
         LR    RB,RF                                                            
         USING DISHDR,RB                                                        
         STC   R1,DHBYTE1                                                       
         STC   R1,DHBYTE2                                                       
         NI    DHBYTE1,FF-(DHBLDSCN+DHNOREAD+DHORIGIN+DHORG2)                   
         SR    R2,R2                                                            
         IC    R2,DHBYTE1                                                       
         BCTR  R2,0                                                             
         MHI   R2,HDRTABL                                                       
         A     R2,AHDRTAB                                                       
         USING HDRTABD,R2                                                       
         GOTOR AOVRSCR,BCPARM,('HEADSCRN',BASOLY1H)                             
         JNE   ROUTH                                                            
         MVC   TWASCRF,DHBYTE1     SET SCREEN FLAVOUR                           
                                                                                
         GOTOR ATSTACS,=AL1(RECBAT,ACTIUP)                                      
         BNE   *+8                                                              
         OI    DHFLAG,DHFIUPY                                                   
         GOTOR ATSTACS,=AL1(RECBAT,ACTACR)                                      
         BNE   *+8                                                              
         OI    DHFLAG,DHFACRY                                                   
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,HDRHED1                                                     
         BZ    *+14                                                             
         LA    RE,TWAD(RE)                                                      
         MVC   BATFKEY,0(RE)                                                    
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,HDRHED2                                                     
         BZ    *+14                                                             
         LA    RE,TWAD(RE)                                                      
         MVC   BATTKEY,0(RE)                                                    
                                                                                
         LA    R1,HDRNAME                                                       
         LA    R3,HDRDISP                                                       
         LHI   R0,HDRDISPN                                                      
DISHDR02 SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         LA    RF,TWAD(RF)         RF=A(FIELD HEADER OF NAME FIELD)             
         SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         BZ    DISHDR04                                                         
         LA    RE,TWAD(RE)         RE=A(DICTIONARY WORD)                        
         MVC   FVIFLD-FVIHDR(L'BATRHD1,RF),0(RE)                                
         TM    L'HDRNAME(R1),HDRNHIGH                                           
         BZ    DISHDR04                                                         
         OI    FVATRB-FVIHDR(RF),FVAHIGH                                        
DISHDR04 SR    RE,RE                                                            
         IC    RE,FVTLEN-FVIHDR(RF)                                             
         AR    RF,RE               RF=A(INPUT FIELD 1 HEADER)                   
         TM    L'HDRNAME(R1),HDR1PROT                                           
         BZ    *+8                                                              
         OI    FVATRB-FVIHDR(RF),FVAPROT                                        
         TM    L'HDRNAME(R1),HDR1HIGH                                           
         BZ    *+8                                                              
         OI    FVATRB-FVIHDR(RF),FVAHIGH                                        
         IC    RE,FVTLEN-FVIHDR(RF)                                             
         AR    RF,RE               RF=A(INPUT FIELD 2 HEADER)                   
         TM    L'HDRNAME(R1),HDR2PROT                                           
         BZ    *+8                                                              
         OI    FVATRB-FVIHDR(RF),FVAPROT                                        
         TM    L'HDRNAME(R1),HDR2HIGH                                           
         BZ    *+8                                                              
         OI    FVATRB-FVIHDR(RF),FVAHIGH                                        
         AHI   R1,L'HDRNAME+L'HDRINDS                                           
         AHI   R3,L'HDRDISP                                                     
         BCT   R0,DISHDR02                                                      
                                                                                
         TM    HDRCOMI,HDR1PROT                                                 
         BZ    *+12                                                             
         OI    BATCOM1H+(FVATRB-FVIHDR),FVAPROT                                 
         OI    BATCOM2H+(FVATRB-FVIHDR),FVAPROT                                 
         TM    HDRCOMI,HDR1HIGH                                                 
         BZ    *+12                                                             
         OI    BATCOM1H+(FVATRB-FVIHDR),FVAHIGH                                 
         OI    BATCOM2H+(FVATRB-FVIHDR),FVAHIGH                                 
                                                                                
         TM    DHFLAG,DHFIUPY      TEST INSTANT UPDATE ALLOWED                  
         BNZ   DISHDR06                                                         
         XC    BATRHD8,BATRHD8                                                  
         OI    BATIUPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    BATIUP2H+(FVATRB-FVIHDR),FVAPROT                                 
                                                                                
DISHDR06 TM    DHFLAG,DHFACRY      TEST ACCRUAL ALLOWED                         
         BNZ   DISHDR08                                                         
         XC    BATRHDF,BATRHDF                                                  
         OI    BATACRH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    BATACR2H+(FVATRB-FVIHDR),FVAPROT                                 
                                                                                
DISHDR08 TM    DHBYTE2,DHBLDSCN                                                 
         BNZ   DISHDRX                                                          
                                                                                
         LA    R4,CSLSTCUR                                                      
         USING LSTTABD,R4          R4=A(BATCH MONTH TABLE ENTRY)                
         TM    DHBYTE2,DHNOREAD    TEST INHIBIT BATCH RECORD READ               
         BNZ   DISHDR10                                                         
         GOTOR ABLDBAK,LSTTABD     BUILD BATCH RECORD KEY                       
         GOTOR AIO,IORDUPD+IOACCDIR+IO2                                         
         BE    *+14                                                             
         TM    IOERR,IOEDEL        BATCH HEADER MAY BE DELETED                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                BATCH RECORD ERROR                           
         DROP  R4                                                               
                                                                                
DISHDR10 L     R3,AIO2             DISPLAY DATA FROM BATCH RECORD KEY           
         USING TBARECD,R3          R2=A(BATCH RECORD)                           
                                                                                
         TM    DHBYTE2,DHORIGIN    TEST READING ORIGIN BATCH RECORD             
         BZ    DISHDR16                                                         
         LA    R4,TBARFST                                                       
         XR    R0,R0                                                            
DISHDR12 CLI   0(R4),0                                                          
         BE    DISHDR16                                                         
         CLI   0(R4),ASKELQ                                                     
         BE    DISHDR14                                                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DISHDR12                                                         
DISHDR14 MVC   IOKEY,ASKKEY-ASKELD(R4)                                          
         MVC   IOADDR,AIO4                                                      
         GOTOR AIO,IORDD+IOACCDIR                                               
         BE    *+12                                                             
         TM    IOERR,IOEDEL        DELETES ARE ALLOWED                          
         BNO   DISHDR16                                                         
         MVC   IOADDR,AIO4                                                      
         GOTOR AIO,IOGET+IOACCMST                                               
         BNE   DISHDR16                                                         
         OI    DHFLAG,DHFORIG      SHOWING ORIGIN BATCH                         
                                                                                
         LHI   R1,LC@CURBT-TWAD    CURRENT BATCH HEADLINE                       
         LHI   RE,L'LC@CURBT-1                                                  
         LA    R1,TWAD(R1)                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BATFKEY(0),0(R1)                                                 
         NI    BATFKEYH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
         LHI   R1,LC@ORGBT-TWAD    ORIGIN BATCH HEADLINE                        
         LHI   RE,L'LC@ORGBT-1                                                  
         LA    R1,TWAD(R1)                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BATTKEY(0),0(R1)                                                 
         NI    BATTKEYH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
                                                                                
         L     R4,AIO4             R4=A(ORIGIN BATCH RECORD)                    
                                                                                
DISHDR16 TM    HDRUIDI,HDR1DISP    USER-ID                                      
         BZ    DISHDR18                                                         
         GOTOR AGETUID,TBAKUSER                                                 
         MVC   BATUID,BCWORK                                                    
         TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    *+12                                                             
         OI    BATUID2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         B     *+12                                                             
         TM    HDRREFI,HDR2DISP    USER-ID 2                                    
         BZ    DISHDR18                                                         
         MVC   BATUID2,BCWORK                                                   
                                                                                
DISHDR18 TM    HDRREFI,HDR1DISP    BATCH REFERENCE                              
         BZ    *+10                                                             
         MVC   BATREF,TBAKBREF                                                  
         TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    *+18                                                             
         OI    BATREF2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         MVC   BATREF2,TBAKBREF-TBAKEY(R4)                                      
         B     DISHDR20                                                         
         TM    HDRREFI,HDR2DISP    BATCH REFERENCE 2                            
         BZ    DISHDR20                                                         
         MVC   BATREF2,CSLSTCUR+(LSTBBREF-LSTTABD)                              
                                                                                
DISHDR20 TM    HDRITYI,HDR1DISP    INPUT TYPE                                   
         BZ    DISHDR23                                                         
         LA    RF,TBARFST                                                       
         XR    R0,R0                                                            
         USING BHDELD,RF                                                        
DISHDR21 CLI   BHDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    DISHDR22                                                         
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     DISHDR21                                                         
                                                                                
DISHDR22 LA    R1,TBAKBTYP                                                      
         TM    BHDSTAT2,BHDSDETL   TEST DETAIL SCREEN                           
         BNO   *+8                                                              
         ICM   R1,8,=AL1(TYPIDETL)  SET DETAIL FLAG                             
         GOTOR AGETBTY                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CURED TBAKBTYP,(3,BATTYP),0,DMCB=BCPARM,ALIGN=LEFT                     
         MVC   BATTYP+4(L'BATTYP-4),CSBTYPN                                     
                                                                                
         TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR23                                                         
         LA    R1,TBAKBTYP-TBAKEY(R4)                                           
         TM    BHDSTAT2,BHDSDETL   TEST DETAIL SCREEN                           
         BNO   *+8                                                              
         ICM   R1,8,=AL1(TYPIDETL)  SET DETAIL FLAG                             
         GOTOR AGETBTY                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,TBAKBTYP-TBAKEY(R4)                                         
         CURED (RF),(3,BATTYP2),0,DMCB=BCPARM,ALIGN=LEFT                        
         MVC   BATTYP2+4(L'BATTYP2-4),CSBTYPN                                   
         DROP  RF                                                               
                                                                                
DISHDR23 TM    HDRMOAI,HDR1DISP    MONTH OF ACTIVITY                            
         BZ    DISHDR24                                                         
         MVC   BCDUB(L'TBAKBMOS),TBAKBMOS                                       
         MVI   BCDUB+L'TBAKBMOS,X'01'                                           
         GOTOR VDATCON,BCPARM,(1,BCDUB),(9,BATMOA)                              
DISHDR24 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR25                                                         
         OI    BATMOA2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         MVC   BCDUB(L'TBAKBMOS),TBAKBMOS-TBAKEY(R4)                            
         MVI   BCDUB+L'TBAKBMOS,X'01'                                           
         GOTOR VDATCON,BCPARM,(1,BCDUB),(9,BATMOA2)                             
         B     DISHDR26                                                         
DISHDR25 TM    DHBYTE2,DHORG2      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR26                                                         
         GOTOR VDATCON,BCPARM,(1,BCDUB),(9,BATMOA2)                             
                                                                                
DISHDR26 TM    HDRCRDI,HDR1DISP    CREATED DATE                                 
         BZ    DISHDR28                                                         
         GOTOR VDATCON,BCPARM,(2,TBAHRADT),(17,BATCRD)                          
DISHDR28 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    *+12                                                             
         OI    BATCRD2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         B     *+12                                                             
         TM    HDRCRDI,HDR2DISP    CREATED DATE 2                               
         BZ    DISHDR30                                                         
         GOTOR VDATCON,BCPARM,(2,TBAHRADT-TBAKEY(R4)),(17,BATCRD2)              
                                                                                
DISHDR30 TM    HDREFDI,HDR1DISP    EFFECTIVE DATE                               
         BZ    DISHDR32                                                         
         GOTOR VDATCON,BCPARM,(2,TBAHREDT),(17,BATEFD)                          
         TM    DHFLAG,DHFORIG                                                   
         BO    DISHDR34                                                         
         TM    HDREFDI,HDR2DISP    EFFECTIVE DATE 2                             
         BZ    DISHDR36                                                         
         GOTOR (RF),(R1),(2,BCTODAYC),(17,BATEFD2)                              
         B     DISHDR36                                                         
DISHDR32 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR36                                                         
DISHDR34 OI    BATEFD2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         GOTOR VDATCON,BCPARM,(2,TBAHREDT-TBAKEY(R4)),(17,BATEFD2)              
                                                                                
DISHDR36 TM    DHFLAG,DHFIUPY      TEST INSTANT UPDATE ALLOWED                  
         BZ    DISHDR40                                                         
         TM    HDRIUPI,HDR1DISP    INSTANT UPDATE                               
         BZ    DISHDR38                                                         
         LHI   R1,UC@NO-TWAD                                                    
         TM    TBARHSTA,TBAHSIAD                                                
         BZ    *+8                                                              
         LHI   R1,UC@YES-TWAD                                                   
         LA    R1,TWAD(R1)                                                      
         MVC   BATIUP,0(R1)                                                     
DISHDR38 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR40                                                         
         OI    BATIUP2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         LHI   R1,UC@NO-TWAD                                                    
         TM    TBARHSTA-TBAKEY(R4),TBAHSIAD                                     
         BZ    *+8                                                              
         LHI   R1,UC@YES-TWAD                                                   
         LA    R1,TWAD(R1)                                                      
         MVC   BATIUP2,0(R1)                                                    
                                                                                
DISHDR40 TM    HDRPERI,HDR1DISP    PERSON                                       
         BZ    DISHDR42                                                         
         GOTOR AGETPID,TBAKBCHR                                                 
         MVC   BATPER,BCWORK                                                    
DISHDR42 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR44                                                         
         OI    BATPER2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         GOTOR AGETPID,TBAKBCHR-TBAKEY(R4)                                      
         MVC   BATPER2,BCWORK                                                   
                                                                                
DISHDR44 TM    HDRSTAI,HDR1DISP    BATCH STATUS                                 
         BZ    DISHDR46                                                         
         GOTOR ADISSTA,BCPARM,(L'BATSTA,BATSTA),(X'80',AIO2)                    
DISHDR46 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR48                                                         
         OI    BATSTA2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         GOTOR ADISSTA,BCPARM,(L'BATSTA2,BATSTA2),(X'80',AIO4)                  
                                                                                
DISHDR48 LA    R3,TBARFST          DISPLAY DATA FROM BATCH RECORD BODY          
DISHDR50 CLI   0(R3),0             TEST END OF RECORD                           
         BE    DISHDRX                                                          
         CLI   0(R3),BHDELQ        TEST BATCH HEADER ELEMENT                    
         BE    DISHDR52                                                         
         CLI   0(R3),FFTELQ        TEST FREE FORM TEXT ELEMENT                  
         BE    DISHDRA8                                                         
         B     DISHDRB4                                                         
         DROP  R3                                                               
                                                                                
         USING TBARECD,R4                                                       
DISHDR52 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR56                                                         
         LA    R4,TBARFST                                                       
         XR    R0,R0                                                            
DISHDR54 CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),BHDELQ                                                     
         BE    DISHDR56                                                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DISHDR54                                                         
         DROP  R4                                                               
                                                                                
         USING BHDELD,R3           PROCESS BATCH HEADER ELEMENT                 
DISHDR56 TM    HDRNAMI,HDR1DISP    BATCH NAME                                   
         BZ    *+10                                                             
         MVC   BATNAM,BHDNAME                                                   
         TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    *+18                                                             
         OI    BATNAM2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         MVC   BATNAM2,BHDNAME-BHDELD(R4)                                       
         B     DISHDR58                                                         
         TM    HDRNAMI,HDR2DISP    BATCH NAME 2                                 
         BZ    DISHDR58                                                         
         MVC   BATNAM2,BATNAM                                                   
         B     DISHDR58                                                         
                                                                                
DISHDR58 TM    HDRITCI,HDR1DISP    ITEM COUNT                                   
         BZ    DISHDR60                                                         
         CURED BHDITEMC,(L'BATITC,BATITC),0,ALIGN=LEFT                          
         TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BO    DISHDR62                                                         
         TM    HDRITCI,HDR2DISP    ITEM COUNT 2                                 
         BZ    DISHDR64                                                         
         CLI   CSACT,ACTGEN        TEST GENERATE                                
         BE    DISHDR64                                                         
         SR    R0,R0                                                            
         ICM   R0,3,BHDITEMA                                                    
         MVC   BOHALF1,BHDDELIT                                                 
         SH    R0,BOHALF1                                                       
         CURED (R0),(L'BATITC2,BATITC2),0,ALIGN=LEFT                            
         B     DISHDR64                                                         
DISHDR60 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR64                                                         
DISHDR62 OI    BATITC2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         PUSH  USING                                                            
         DROP  R3                                                               
         USING BHDELD,R4                                                        
         CURED BHDITEMC,(L'BATITC2,BATITC2),0,ALIGN=LEFT                        
         POP   USING                                                            
                                                                                
DISHDR64 TM    HDRITAI,HDR1DISP    ITEMS ADDED                                  
         BZ    DISHDR66                                                         
         SR    R0,R0                                                            
         ICM   R0,3,BHDITEMA                                                    
         MVC   BOHALF1,BHDDELIT                                                 
         SH    R0,BOHALF1                                                       
         CURED (R0),(L'BATITA,BATITA),0,ALIGN=LEFT                              
DISHDR66 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR68                                                         
         OI    BATITA2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         SR    R0,R0                                                            
         ICM   R0,3,BHDITEMA-BHDELD(R4)                                         
         MVC   BOHALF1,BHDDELIT-BHDELD(R4)                                      
         SH    R0,BOHALF1                                                       
         CURED (R0),(L'BATITA2,BATITA2),0,ALIGN=LEFT                            
                                                                                
DISHDR68 TM    HDRAMCI,HDR1DISP    BATCH TOTAL                                  
         BZ    DISHDR70                                                         
*&&UK*&& CURED BHDCASHC,(L'BATAMC,BATAMC),CSCURTAM,FLOAT=-,ALIGN=LEFT           
*&&US*&& CURED BHDCASHC,(L'BATAMC,BATAMC),CSCURTAM,MINUS=YES,ALIGN=LEFT         
         TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BO    DISHDR72                                                         
         TM    HDRAMCI,HDR2DISP    BATCH TOTAL 2                                
         BZ    DISHDR74                                                         
         CLI   CSACT,ACTGEN        TEST GENERATE                                
         BE    DISHDR74                                                         
         ZAP   BCDUB,BHDCASHA                                                   
         CLI   CSACT,ACTREV        TEST REVERSE                                 
         BNE   *+10                                                             
         MP    BCDUB,=P'-1'                                                     
*&&UK*&& CURED BCDUB,(L'BATAMC2,BATAMC2),CSCURTAM,FLOAT=-,ALIGN=LEFT            
*&&US*&& CURED BCDUB,(L'BATAMC2,BATAMC2),CSCURTAM,MINUS=YES,ALIGN=LEFT          
         B     DISHDR74                                                         
DISHDR70 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR74                                                         
DISHDR72 OI    BATAMC2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         PUSH  USING                                                            
         DROP  R3                                                               
         USING BHDELD,R4                                                        
*&&UK*&& CURED BHDCASHC,(L'BATAMC2,BATAMC2),CSCURTAM,FLOAT=-,ALIGN=LEFT         
*&&US*&& CURED BHDCASHC,(L'BATAMC2,BATAMC2),CSCURTAM,MINUS=Y,ALIGN=LEFT         
         POP   USING                                                            
                                                                                
DISHDR74 TM    HDRAMAI,HDR1DISP    TOTAL SO FAR                                 
         BZ    DISHDR80                                                         
         TM    HDRREFI,HDR2DISP    TEST TWO-UP SCREEN                           
         BNZ   DISHDR78                                                         
         CLI   BHDLN,BHDLN2Q       TEST TOTAL DRS/CRS PRESENT                   
         BL    DISHDR78                                                         
         TM    CSBIND1,TYPICUMU    TEST ACCUMULATING DEBITS AND CREDITS         
         BO    DISHDR76                                                         
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS & TAX                
         BZ    DISHDR78                                                         
         OI    DHFLAG,DHFCRDR      SET TO SHOW CREDITS BEFORE DEBITS            
DISHDR76 GOTOR EDTTWO,BHDELD                                                    
         MVC   BATAMA,BCWORK                                                    
         B     DISHDR80                                                         
DISHDR78 DS    0H                                                               
*&&UK*&& CURED BHDCASHA,(L'BATAMA,BATAMA),CSCURTAM,FLOAT=-,ALIGN=LEFT           
*&&US*&& CURED BHDCASHA,(L'BATAMA,BATAMA),CSCURTAM,MINUS=Y,ALIGN=LEFT           
                                                                                
         PUSH  USING                                                            
         DROP  R3                                                               
         USING BHDELD,R4                                                        
DISHDR80 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR86                                                         
         OI    BATAMA2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         CLI   BHDLN,BHDLN2Q       TEST TOTAL DRS/CRS PRESENT                   
         BL    DISHDR84                                                         
         TM    CSBIND1,TYPICUMU    TEST ACCUMULATING DEBITS AND CREDITS         
         BO    DISHDR82                                                         
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS & TAX                
         BZ    DISHDR84                                                         
         OI    DHFLAG,DHFCRDR      SET TO SHOW CREDITS BEFORE DEBITS            
DISHDR82 GOTOR EDTTWO,BHDELD                                                    
         MVC   BATAMA2,BCWORK                                                   
         B     DISHDR86                                                         
DISHDR84 DS    0H                                                               
*&&UK*&& CURED BHDCASHA,(L'BATAMA2,BATAMA2),CSCURTAM,FLOAT=-,ALIGN=LEFT         
*&&US*&& CURED BHDCASHA,(L'BATAMA2,BATAMA2),CSCURTAM,MINUS=Y,ALIGN=LEFT         
         POP   USING                                                            
                                                                                
DISHDR86 TM    HDRPERI,HDR1DISP    PERSON                                       
         BZ    DISHDR88                                                         
         OC    BHDIBNO,BHDIBNO     TEST CURRENT INPUT BATCHER NUMBER            
         BZ    DISHDR88                                                         
         GOTOR AGETPID,BHDIBNO                                                  
         MVC   BATPER,BCWORK       SUBSTITUTE PERSON NAME                       
DISHDR88 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR90                                                         
         OI    BATPER2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         LA    R1,BHDIBNO-BHDELD(R4)                                            
         OC    0(L'BHDIBNO,R1),0(R1)  TEST CURRENCT INPUT BATCHER NO.           
         BZ    DISHDR90                                                         
         GOTOR AGETPID,(R1)                                                     
         MVC   BATPER2,BCWORK      SUBSTITUTE PERSON NAME                       
                                                                                
DISHDR90 TM    BCCPYST5,CPYSBAPR   TEST COMPANY USING BATCH APPROVAL            
         BZ    DISHDR94                                                         
         TM    HDRAPRI,HDR1DISP    APPROVER                                     
         BZ    DISHDR92                                                         
         OC    BHDAPRVR,BHDAPRVR   TEST APPROVER PRESENT                        
         BZ    DISHDR92                                                         
         GOTOR AGETPID,BHDAPRVR                                                 
         MVC   BATAPR,BCWORK       SET APPROVER                                 
DISHDR92 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    DISHDR96                                                         
         OI    BATAPR2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         LA    R1,BHDAPRVR-BHDELD(R4)                                           
         OC    0(L'BHDAPRVR,R1),0(R1)  TEST APPROVER PRESENT                    
         BZ    DISHDR96                                                         
         GOTOR AGETPID,(R1)                                                     
         MVC   BATAPR2,BCWORK      SET APPROVER                                 
         B     DISHDR96                                                         
                                                                                
DISHDR94 XC    BATRHDG,BATRHDG     CLEAR APPROVER FIELD NAME                    
         OI    BATAPRH+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                          
         B     DISHDR96                                                         
                                                                                
DISHDR96 TM    HDRACRI,HDR1PROT+HDR2PROT   TEST ACCRUAL DISPLAY ONLY            
         BNO   DISHDR98                                                         
         TM    BHDSTAT1,BHDSACRU+BHDSACRV  TEST ACCRUAL/REVERSAL                
         BNZ   DISHDR98                                                         
         XC    BATRHDF,BATRHDF     CLEAR ACCRUAL FIELD NAME                     
         B     DISHDRA6                                                         
                                                                                
DISHDR98 TM    DHFLAG,DHFACRY      TEST ACCRUAL ALLOWED                         
         BZ    DISHDRA6                                                         
         TM    HDRACRI,HDR1DISP    ACCRUAL                                      
         BZ    DISHDRA2                                                         
         SR    RE,RE                                                            
         LHI   R1,UC@NO-TWAD                                                    
         LHI   RE,L'UC@NO-1                                                     
         TM    BHDSTAT1,BHDSACRU   TEST ACCRUAL                                 
         BZ    *+12                                                             
         LHI   R1,UC@YES-TWAD                                                   
         LHI   RE,L'UC@YES-1                                                    
         TM    BHDSTAT1,BHDSACRV   TEST ACCRUAL REVERSAL                        
         BZ    *+12                                                             
         LHI   R1,UC@RVRSL-TWAD                                                 
         LHI   RE,L'UC@RVRSL-1                                                  
         LA    R1,TWAD(R1)                                                      
         MVC   BATACR,BCSPACES                                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BATACR(0),0(R1)                                                  
         TM    HDRACRI,HDR2DISP    ACCRUAL 2                                    
         BZ    DISHDRA6                                                         
         TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BO    DISHDRA4                                                         
         OC    CSBTYP2,CSBTYP2     TEST AUTOMATIC ACCRUAL                       
         BNZ   DISHDRA6                                                         
         TM    BHDSTAT1,BHDSACRU   NO - TEST USER ACCRUAL                       
         BZ    DISHDRA6                                                         
         MVC   BATACR2,BATACR      SET USER ACCRUAL                             
         B     DISHDRA6                                                         
DISHDRA2 TM    DHFLAG,DHFORIG      TEST SHOWING ORIGIN BATCH                    
         BZ    *+12                                                             
         OI    BATACR2H+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         B     *+12                                                             
         TM    HDRACRI,HDR2DISP    ACCRUAL 2                                    
         BZ    DISHDRA6                                                         
DISHDRA4 SR    RE,RE                                                            
         LHI   R1,UC@NO-TWAD                                                    
         LHI   RE,L'UC@NO-1                                                     
         TM    BHDSTAT1-BHDELD(R4),BHDSACRU  TEST ACCRUAL                       
         BZ    *+12                                                             
         LHI   R1,UC@YES-TWAD                                                   
         LHI   RE,L'UC@YES-1                                                    
         TM    BHDSTAT1-BHDELD(R4),BHDSACRV  TEST ACCRUAL REVERSAL              
         BZ    *+12                                                             
         LHI   R1,UC@RVRSL-TWAD                                                 
         LHI   RE,L'UC@RVRSL-1                                                  
         LA    R1,TWAD(R1)                                                      
         MVC   BATACR2,BCSPACES                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BATACR2(0),0(R1)                                                 
                                                                                
DISHDRA6 B     DISHDRB4                                                         
                                                                                
         USING FFTELD,R3           PROCESS FREE FORM TEXT ELEMENT               
DISHDRA8 TM    HDRCOMI,HDR1DISP    BATCH COMMENTS                               
         BZ    DISHDRB4                                                         
         IC    R0,FFTLN                                                         
         SHI   R0,FFTDLEN-FFTELD                                                
         LA    R1,FFTDLEN                                                       
         USING FFTDLEN,R1                                                       
         LA    RF,BATCOM1H                                                      
         SR    RE,RE                                                            
DISHDRB2 IC    RE,FFTDLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD-FVIHDR(0,RF),FFTDATA                                      
         AHI   RE,L'FFTDLEN+1                                                   
         SR    R0,RE                                                            
         BNP   DISHDRB4                                                         
         AR    R1,RE               R1=A(NEXT INPUT CHUNK)                       
         LA    RF,BATCOM2H                                                      
         B     DISHDRB2                                                         
                                                                                
DISHDRB4 SR    R0,R0               BUMP TO NEXT BATCH RECORD ELEMENT            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DISHDR50                                                         
                                                                                
DISHDRX  J     ROUTE                                                            
         DROP  R1,R2,R3                                                         
                                                                                
         USING BHDELD,R2                                                        
EDTTWO   ST    R2,BCFULL           SUBROUTINE TO DISPLAY TWIN AMOUNTS           
         ST    RE,DHRTRN1                                                       
         LR    R2,R1                                                            
         MVC   BCWORK,BCSPACES                                                  
         TM    DHFLAG,DHFCRDR      TEST SHOWING CREDITS BEFORE DEBITS           
         BO    *+12                                                             
         GOTOR EDTCRP                                                           
         B     *+8                                                              
         GOTOR EDTDRP                                                           
         STH   R0,BCHALF                                                        
         MVC   BCWORK+L'BATAMA(L'BATAMA),BCWORK                                 
         TM    DHFLAG,DHFCRDR                                                   
         BO    *+12                                                             
         GOTOR EDTDRP                                                           
         B     *+8                                                              
         GOTOR EDTCRP                                                           
         LH    RF,BCHALF           TEST ENOUGH SPACE FOR BOTH AMOUNTS           
         AR    RF,R0               (IF NOT SHOW WITHOUT DECIMAL PLACES)         
         LHI   RE,L'BATAMA-1                                                    
         CR    RE,RF                                                            
         BNL   EDTTWO02                                                         
                                                                                
         MVC   BCWORK,BCSPACES                                                  
         TM    DHFLAG,DHFCRDR      TEST SHOWING CREDITS BEFORE DEBITS           
         BO    *+12                                                             
         GOTOR EDTCRD                                                           
         B     *+8                                                              
         GOTOR EDTDRD                                                           
         STH   R0,BCHALF                                                        
         MVC   BCWORK+L'BATAMA(L'BATAMA),BCWORK                                 
         TM    DHFLAG,DHFCRDR      TEST SHOWING CREDITS BEFORE DEBITS           
         BO    *+12                                                             
         GOTOR EDTDRD                                                           
         B     *+8                                                              
         GOTOR EDTCRD                                                           
                                                                                
EDTTWO02 LH    RF,BCHALF           SHUNT UP THE SECOND AMOUNT                   
         LA    RE,BCWORK                                                        
         AR    RE,R0                                                            
         MVI   0(RE),C'/'                                                       
         AHI   RE,1                                                             
         LA    R1,BCWORK+L'BATAMA                                               
         CR    R1,RE                                                            
         BNH   EDTTWOX                                                          
         MVC   0(1,RE),0(R1)                                                    
         AHI   RE,1                                                             
         AHI   R1,1                                                             
         BCT   RF,*-14                                                          
         LA    RF,BCWORK+L'BCWORK-1                                             
         SR    RF,R1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),BCSPACES                                                 
EDTTWOX  L     R2,BCFULL                                                        
         L     RE,DHRTRN1                                                       
         BR    RE                                                               
                                                                                
EDTCRP   ST    RE,DHRTRN2                                                       
*&&UK*&& CURED BHDTOTCR,(L'BATAMA,BCWORK),CSCURTCR,FLOAT=-,ALIGN=LEFT           
*&&US*&& CURED BHDTOTCR,(L'BATAMA,BCWORK),CSCURTCR,MINUS=Y,ALIGN=LEFT           
         L     RE,DHRTRN2                                                       
         BR    RE                                                               
                                                                                
EDTDRP   ST    RE,DHRTRN2                                                       
*&&UK*&& CURED BHDTOTDR,(L'BATAMA,BCWORK),CSCURTDR,FLOAT=-,ALIGN=LEFT           
*&&US*&& CURED BHDTOTDR,(L'BATAMA,BCWORK),CSCURTDR,MINUS=Y,ALIGN=LEFT           
         L     RE,DHRTRN2                                                       
         BR    RE                                                               
                                                                                
EDTCRD   ST    RE,DHRTRN2                                                       
*&&UK*&& CURED BHDTOTCR,(L'BATAMA,BCWORK),CSCURTCR,FLOAT=-,ALIGN=LEFT, X        
               DECS=ROUND                                                       
*&&US*&& CURED BHDTOTCR,(L'BATAMA,BCWORK),CSCURTCR,MINUS=Y,ALIGN=LEFT, X        
               DECS=ROUND                                                       
         L     RE,DHRTRN2                                                       
         BR    RE                                                               
                                                                                
EDTDRD   ST    RE,DHRTRN2                                                       
*&&UK*&& CURED BHDTOTDR,(L'BATAMA,BCWORK),CSCURTDR,FLOAT=-,ALIGN=LEFT, X        
               DECS=ROUND                                                       
*&&US*&& CURED BHDTOTDR,(L'BATAMA,BCWORK),CSCURTDR,MINUS=Y,ALIGN=LEFT, X        
               DECS=ROUND                                                       
         L     RE,DHRTRN2                                                       
         BR    RE                                                               
         DROP  RB                                                               
                                                                                
HDRDISP  DS    0AL2                                                             
         DC    AL2(BATRHD1H-TWAD)                                               
         DC    AL2(BATRHD2H-TWAD)                                               
         DC    AL2(BATRHD3H-TWAD)                                               
         DC    AL2(BATRHD4H-TWAD)                                               
         DC    AL2(BATRHD5H-TWAD)                                               
         DC    AL2(BATRHD6H-TWAD)                                               
         DC    AL2(BATRHD7H-TWAD)                                               
         DC    AL2(BATRHD8H-TWAD)                                               
         DC    AL2(BATRHD9H-TWAD)                                               
         DC    AL2(BATRHDAH-TWAD)                                               
         DC    AL2(BATRHDBH-TWAD)                                               
         DC    AL2(BATRHDCH-TWAD)                                               
         DC    AL2(BATRHDDH-TWAD)                                               
         DC    AL2(BATRHDEH-TWAD)                                               
         DC    AL2(BATRHDFH-TWAD)                                               
         DC    AL2(BATRHDGH-TWAD)                                               
HDRDISPN EQU   (*-HDRDISP)/L'HDRDISP                                            
                                                                                
R2WORKD  DSECT                     ** ADDHDR S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
DHBYTE1  DS    X                                                                
DHBYTE2  DS    X                                                                
DHBLDSCN EQU   X'80'               BUILD SCREEN ONLY                            
DHNOREAD EQU   X'40'               INHIBIT BATCH RECORD READ                    
DHORIGIN EQU   X'20'               READ FOR ORIGIN BATCH RECORD                 
DHORG2   EQU   X'10'               SHOW ORIGIN BATCH AS "TO"                    
DHFLAG   DS    X                                                                
DHFIUPY  EQU   X'80'               INSTANT UPDATE=YES                           
DHFACRY  EQU   X'40'               ACCRUAL OPTION=YES                           
DHFCRDR  EQU   X'20'               SHOW CREDITS BEFORE DEBITS                   
DHFORIG  EQU   X'10'               SHOW ORIGIN BATCH                            
DHRTRN1  DS    A                   RETURN ADDRESS 1                             
DHRTRN2  DS    A                   RETURN ADDRESS 2                             
DHWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD A BATCH ITEM RECORD                                             *         
*                                                                     *         
* NTRY - P1/B0  =X'80' ON MEANS EXTRA ITEM ELEMENTS PASSED - SEE P4   *         
*               =X'40' ON MEANS PASS BACK TABLE OF ADDED TXS - SEE P5 *         
*               =X'20' ON MEANS ADDRESS OF TAX ACCUMULATOR IN P6      *         
*        P1/B1-3=A(ACCDAY RECORD)                                     *         
*        P2     =A(ITEM AMOUNT)                                       *         
*        P3/B0  =INDICATORS AS FOLLOWS:-                              *         
*                X'80' ON MEANS DON'T ADD AN ITEM RECORD              *         
*                X'40' ON MEANS SET TBAESORD IN ITEM RECORD           *         
*                X'2A' BITS USED FOR DR/CR, DR/-DR AND CR/-CR PAIRS   *         
*                X'10' ON MEANS P7 USED                               *         
*        P3/B1-3=A(WORK AREA)                                         *         
*                +00 ITEM REFERENCE                                   *         
*                +10 ITEM STATUS                                      *         
*                +11 TRANSACTION TYPE                                 *         
*        P4     =A(A LIST OF EXTRA ELEMENTS (DELIMITED WITH A X'00')  *         
*                TO BE ADDED TO ITEM RECORD)                          *         
*        P5     =A(AREA FOR TABLE OF ADDED TX KEYS : (20*42)+1 BYTES) *         
*        P6     =A(AMOUNT OF USE TAX OR GST/PST FOR ITEM)             *         
*        P7     =INTERCOMPANY OFFICE CODE                                       
*        CSVALS SET FOR BATCH ITEM                                    *         
***********************************************************************         
                                                                                
ADDITE   J     *+12                                                             
         DC    C'*ADDITE*'                                                      
         LR    RB,RF                                                            
         USING ADDITE,RB                                                        
         ST    R1,AIPARM           SAVE A(CALLER'S PARAMETER LIST)              
         LA    R4,CSLSTCUR                                                      
         USING LSTTABD,R4          R4=A(BATCH MONTH OF SERVICE TABLE)           
                                                                                
         L     R1,4(R1)                                                         
         ZAP   BCDUB,LSTBCSHA      CHECK IF WE WILL EXCEED CASH AMOUNT          
         AP    BCDUB,0(6,R1)                                                    
         NI    BCDUB+7,X'FE'            STRIP OFF NEGATIVE VALUE                
         CP    BCDUB,=P'100000000000'   LIMIT 1 BILLION DOLLARS                 
         BL    ADDIT002                                                         
         MVC   FVMSGNO,=AL2(AE$AMTHI)   AMOUNT TOO HIGH                         
         J     ROUTH                                                            
                                                                                
ADDIT002 L     R1,AIPARM                                                        
         L     R2,0(R1)                                                         
         ST    R2,AIAREC           A(ACCDAY RECORD)                             
         AHI   R2,2                BUMP PAST RECORD LENGTH                      
         TM    0(R1),X'80'         TEST EXTRA ITEM ELEMENTS PASSED              
         BZ    *+10                                                             
         MVC   AIAXTRAS,12(R1)     YES - SET A(EXTRA ELEMENTS)                  
         TM    0(R1),X'40'         TEST PASS BACK TX KEYS TABLE                 
         BZ    *+10                                                             
         MVC   AIATXKEY,16(R1)     YES - SET A(TX KEYS TABLE)                   
         TM    0(R1),X'20'         PASSED A(TAX ACCUMULATOR)                    
         BZ    *+10                                                             
         MVC   AIATAXES,20(R1)     YES - SET A(TAX ACCUMULATOR)                 
         TM    0(R1),X'10'         P7 PASSED W/INTERCOMPANY OFFICE              
         BZ    *+10                                                             
         MVC   AIAINOFF,24(R1)     YES - SET A(INOFF)                           
         TM    8(R1),X'80'         TEST DON'T ADD ITEM                          
         BZ    *+8                                                              
         OI    AIFLAG,AIFNOADD     YES - SET FLAG                               
         TM    8(R1),X'40'         TEST SET ITEM CARRIES ORDER                  
         BZ    *+8                                                              
         OI    AIFLAG,AIFORDER     YES - SET FLAG                               
         L     RF,8(R1)            RF=A(REFERENCE)                              
                                                                                
         OC    AIITESTA,8(R1)      SET/TEST ITEM STATUS                         
         BNZ   *+16                                                             
         TM    10(RF),X'80'        TEST REGULAR DEBIT                           
         BZ    *+8                                                              
         OI    AIITESTA,TBAESIDR   SET DEBIT ITEM                               
         NI    AIITESTA,TBAESIDR+TBAESDAC                                       
                                                                                
         L     R1,4(R1)            INCREMENT BATCH CASH                         
         SR    R0,R0                                                            
         MVC   AITRNTYP,LSTBBTYP   DEFAULT BATCH TYPE                           
         TM    CSBIND5,TYPIOVTY    TEST TRANSACTION TYPE PROVIDED               
         BZ    *+10                                                             
         MVC   AITRNTYP,11(RF)     SECOND BYTE OF OLD DISK ADDRESS              
                                                                                
         TM    AIFLAG,AIFNOADD     TEST DON'T ADD ITEM                          
         BNZ   ADDIT014                                                         
                                                                                
         CLI   CSACT,ACTCHA        TEST ITEM/CHANGE                             
         BNE   ADDIT008                                                         
         LA    RE,BCITECUR                                                      
         TM    CSBIND1,TYPICUMU    TEST ACCUMULATING TOTAL DRS/CRS              
         BO    ADDIT006                                                         
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS/TAX                  
         BZ    *+12                                                             
         TM    LSTTSTAT-LSTTABD(RE),TBAESIDR                                    
         BO    *+10                TAX EXCLUDED FROM GENERAL TOTAL              
         SP    LSTBCSHA,LSTIAMT-LSTTABD(,RE)                                    
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS/TAX                  
         BO    ADDIT004            DON'T TOTAL YET                              
         AP    LSTBCSHA,0(6,R1)                                                 
         ZAP   LSTIAMT-LSTTABD(,RE),0(6,R1)                                     
         MVC   LSTIREF-LSTTABD(,RE),0(RF)                                       
         B     ADDIT014                                                         
                                                                                
ADDIT004 TM    LSTTSTAT-LSTTABD(RE),TBAESIDR                                    
         BO    *+14                                                             
         SP    LSTBTCRS,LSTIAMT-LSTTABD(,RE)                                    
         B     *+10                                                             
         SP    LSTBTDRS,LSTIAMT-LSTTABD(,RE)                                    
         ZAP   LSTIAMT-LSTTABD(,RE),0(6,R1)                                     
         MVC   LSTIREF-LSTTABD(,RE),0(RF)                                       
         NI    LSTTSTAT-LSTTABD(RE),FF-(TBAESIDR+TBAESDAC)                      
         TM    AIITESTA,TBAESIDR   TEST THIS IS (NOW) A DEBIT POSTING           
         BZ    *+12                                                             
         OI    LSTTSTAT-LSTTABD(RE),TBAESIDR                                    
         B     ADDIT014                                                         
         AP    LSTBCSHA,0(6,R1)    NOT (NOW) TAX - ADD TO GENERAL TOTAL         
         B     ADDIT014                                                         
                                                                                
ADDIT006 TM    LSTTSTAT-LSTTABD(RE),TBAESDAC                                    
         BO    *+12                                                             
         TM    LSTTSTAT-LSTTABD(RE),TBAESIDR                                    
         BNZ   *+10                                                             
         SP    LSTBTCRS,LSTIAMT-LSTTABD(,RE)                                    
                                                                                
         TM    LSTTSTAT-LSTTABD(RE),TBAESDAC                                    
         BO    *+12                                                             
         TM    LSTTSTAT-LSTTABD(RE),TBAESIDR                                    
         BZ    *+16                                                             
         SP    LSTBTDRS,LSTIAMT-LSTTABD(,RE)                                    
         SP    LSTBCSHA,LSTIAMT-LSTTABD(,RE)                                    
                                                                                
         NI    LSTTSTAT-LSTTABD(RE),FF-(TBAESIDR+TBAESDAC)                      
         OC    LSTTSTAT-LSTTABD(,RE),AIITESTA                                   
                                                                                
         TM    AIITESTA,TBAESDAC   TEST THIS IS (NOW) A DR/CR PAIR              
         BO    *+12                                                             
         TM    AIITESTA,TBAESIDR   TEST THIS IS (NOW) A DR POSTING              
         BZ    *+10                                                             
         AP    LSTBCSHA,0(6,R1)                                                 
         ZAP   LSTIAMT-LSTTABD(,RE),0(6,R1)                                     
         MVC   LSTIREF-LSTTABD(,RE),0(RF)                                       
         B     ADDIT014                                                         
                                                                                
ADDIT008 ICM   RF,15,AIATAXES      ADDRESS OF TAXES PASSED                      
         BZ    ADDIT009                                                         
         TP    LSTBTDRS                                                         
         BE    *+10                                                             
         ZAP   LSTBTDRS,BCPZERO                                                 
         AP    LSTBTDRS,0(6,RF)    TAXES AS DEBITS                              
ADDIT009 TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS/TAX                  
         BZ    *+12                                                             
         TM    AIITESTA,TBAESIDR   TEST THIS IS A (DEBIT) TAX POSTING           
         BO    ADDIT012            TAX EXCLUDED FROM GENERAL TOTAL              
         TM    CSBIND1,TYPICUMU    TEST ACCUMULATE DR & CR                      
         BZ    ADDIT010                                                         
         TM    AIITESTA,TBAESDAC   TEST DEBIT/CREDIT PAIR                       
         BNZ   ADDIT010                                                         
         TM    AIITESTA,TBAESIDR   OR A DEBIT POSTING                           
         BZ    ADDIT012                                                         
                                                                                
ADDIT010 AP    LSTBCSHA,0(6,R1)                                                 
                                                                                
ADDIT012 SR    R1,R1               INCREMENT BATCH ITEM COUNT                   
         ICM   R1,3,LSTBITMA                                                    
         AHI   R1,1                                                             
         STCM  R1,3,LSTBITMA                                                    
         XC    AIWCODES,AIWCODES                                                
                                                                                
         USING DLPOSTD,R2          R2=A(FIRST ACCDAY RECORD ELEMENT)            
ADDIT014 CLI   DLPSEL,0            TEST END OF RECORD                           
         BE    ADDIT028                                                         
                                                                                
         CLI   DLPSEL,DLPSEDRQ     TEST SINGLE DEBIT                            
         BNE   ADDIT020                                                         
         CLI   AITRNTYP,BT34       TEST TYPE 34                                 
         BNE   ADDIT016                                                         
         CLC   =C'SJ',DLPSDBU      FROM/TO JOB                                  
         BNE   ADDIT016                                                         
         SR    RE,RE                                                            
         IC    RE,AIACTSTN         INCREMENT SET NUMBER                         
         AH    RE,=H'1'                                                         
         STC   RE,AIACTSTN                                                      
                                                                                
ADDIT016 GOTOR APEADR                                                           
*&&US                                                                           
         CLC   =C'SJ',DLPSDBU                                                   
         BNE   ADDIT017                                                         
         GOTOR ADDWCS                                                           
                                                                                
ADDIT017 CLC   =C'1C',DLPSDBU                                                   
         BNE   ADDIT026                                                         
         CLC   =C'11',DLPSCRU                                                   
         BE    ADDIT018                                                         
         CLC   =C'12',DLPSCRU                                                   
         BE    ADDIT018                                                         
         CLC   =C'13',DLPSCRU                                                   
         BNE   ADDIT026                                                         
                                                                                
ADDIT018 GOTOR APEACR                                                           
*&&                                                                             
         B     ADDIT026                                                         
                                                                                
ADDIT020 CLI   DLPSEL,DLPSECRQ     TEST SINGLE CREDIT                           
         BNE   ADDIT024                                                         
         GOTOR APEACR                                                           
*&&US                                                                           
         CLC   =C'1C',DLPSCRU                                                   
         BNE   ADDIT026                                                         
         CLC   =C'11',DLPSDBU                                                   
         BE    ADDIT022                                                         
         CLC   =C'12',DLPSDBU                                                   
         BE    ADDIT022                                                         
         CLC   =C'13',DLPSDBU                                                   
         BNE   ADDIT026                                                         
                                                                                
ADDIT022 GOTOR APEADR                                                           
*&&                                                                             
         B     ADDIT026                                                         
                                                                                
ADDIT024 CLI   DLPSEL,DLPSEDCQ     TEST DEBIT/CREDIT                            
         BNE   ADDIT026                                                         
         GOTOR APEADR                                                           
         GOTOR APEACR                                                           
                                                                                
ADDIT026 IC    R0,DLPSLEN          BUMP TO NEXT ACCDAY ELEMENT                  
         AR    R2,R0                                                            
         CLI   DLPSEL,X'FF'        TEST FOR END OF CHUNK                        
         BNE   ADDIT014                                                         
         AHI   R2,1                                                             
         CLI   DLPSEL,DLDSELQ      TEST POSTING DESCRIPTION ELEMENT             
         BE    ADDIT014                                                         
         DC    H'0'                                                             
                                                                                
ADDIT028 GOTOR AINIADT             INITIALISE ADDTRN CONTROL BLOCK              
         L     R2,AIAREC                                                        
         AHI   R2,2                                                             
         USING DLDESCD,R2                                                       
         L     R3,AADTBLK                                                       
         USING ADDTRND,R3          R3=A(ADDTRN BLOCK)                           
         MVC   TRNBSEQN,LSTBITMA   SET ITEM SEQUENCE NUMBER                     
         CLI   CSACT,ACTCHA        TEST ITEM/CHANGE                             
         BNE   *+14                                                             
         LA    RE,BCITECUR                                                      
         MVC   TRNBSEQN,LSTISEQ-LSTTABD(RE)                                     
         MVC   TRNPUSER,LSTBUSER                                                
         MVC   TRNBMOS,LSTBMOSP    PWOS P'YYMM'                                 
         MVC   TRNEFDT,LSTBEFDT    COMPRESSED BINARY EFFECTIVE DATE             
                                                                                
ADDIT030 LA    R0,AITRNEL          CLEAR TRANSACTION ELEMENT AREA               
         LHI   R1,L'AITRNEL                                                     
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R1,AITRNEL                                                       
         USING TRNELD,R1           CREATE A TRANSACTION ELEMENT                 
         MVI   TRNEL,TRNELQ        ELEMENT CODE                                 
         MVC   TRNTYPE,AITRNTYP    BATCH TYPE                                   
         MVC   TRNMOS,LSTBMOSC     BATCH MONTH                                  
         MVC   TRNBREF,LSTBBREF    BATCH REFERENCE                              
         MVC   TRNDATE,DLDSDATE    TRANSACTION DATE                             
         MVC   TRNREF,DLDSREF      TRANSACTION REFERENCE                        
         MVC   BCINVDTE,DLDSDATE                                                
         MVC   TRNSTAT,DLDSSTAT    TRANSACTION STATUS                           
         MVC   BCINVREF,BCSPACES                                                
         MVC   BCINVREF(L'DLDSREF),DLDSREF                                      
                                                                                
         SR    RF,RF                                                            
         IC    RF,DLDSLEN                                                       
         SHI   RF,DLDSLN1Q+1                                                    
         BNM   *+12                                                             
         LA    RE,TRNELD+TRNLN1Q-1                                              
         B     ADDIT032                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TRNNARR(0),DLDSNARR TRANSACTION NARRATIVE                        
         LA    RE,TRNNARR(RF)      RE=A(END OF NARRATIVE)                       
         LTR   RF,RF                                                            
         BZ    ADDIT032                                                         
         CLI   TRNTYPE,BT06        MANUAL BILLING ?                             
         BE    ADDIT032            YES, LENGTH IS CORRECT                       
         CLI   0(RE),C' '          LOCATE END OF NARRATIVE                      
         BH    ADDIT032                                                         
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
                                                                                
ADDIT032 AHI   RE,1                                                             
         LA    RF,TRNELD                                                        
         SR    RE,RF                                                            
         STC   RE,TRNLN            SET LENGTH OF TRANSACTION ELEMENT            
         LA    RF,TRNELD(RE)                                                    
         XC    0(256,RF),0(RF)     CLEAR OUT ANY GARBAGE                        
         XC    AIADDRS(AIADDRSL),AIADDRS                                        
         MVI   AIACTSTN,0          CLEAR SET NUMBER                             
                                                                                
ADDIT034 SR    R0,R0               POINT TO NEXT POSTING ELEMENT                
         IC    R0,DLDSLEN                                                       
         AR    R2,R0                                                            
         TM    DLDSEL,X'FF'        TEST FOR X'00' (END OF RECORD)               
         BZ    ADDIT060                     X'FF' (END OF CHUNK)                
         BM    ADDIT036                     X'XX' (ELEMENT)                     
         AHI   R2,1                                                             
         CLI   DLDSEL,DLDSELQ      TEST POSTING DESCRIPTION ELEMENT             
         BE    ADDIT030                                                         
         DC    H'0'                                                             
                                                                                
ADDIT036 CLI   DLDSEL,NAMELQ       SAVE A(NAME ELEMENT) IF FOUND                
         BNE   *+8                                                              
         ST    R2,AIANAMEL                                                      
         CLI   DLDSEL,ADRELQ       SAVE A(ADDRESS ELEMENT) IF FOUND             
         BNE   *+8                                                              
         ST    R2,AIAADREL                                                      
         CLI   DLDSEL,GINELQ       SAVE A(GIN ELEMENT) IF FOUND                 
         BNE   *+8                                                              
         ST    R2,AIAGINEL                                                      
         CLI   DLDSEL,FFNELQ       SAVE A(FFN ELEMENT) IF FOUND                 
         BNE   *+8                                                              
         ST    R2,AIAFFNEL                                                      
         CLI   DLDSEL,FFTELQ       LOOKING FOR FFTTWRKC ONLY                    
         BNE   ADDIT037                                                         
         CLI   DLDSEL+2,FFTTWRKC                                                
         BNE   *+12                                                             
         ST    R2,AIAFFTEL         SAVE IT IF FOUND                             
         B     ADDIT037                                                         
         CLI   FFTTYPE-FFTELD(R2),FFTTINVN                                      
         BNE   ADDIT037                                                         
         LLC   RF,FFTDLEN-FFTELD(R2)                                            
         SHI   RF,1                                                             
         MVC   BCINVREF(0),FFTDATA-FFTELD(R2)                                   
         EX    RF,*-6                                                           
         OC    BCINVREF,BCSPACES                                                
                                                                                
ADDIT037 CLI   DLDSEL,AFCELQ       SAVE A(FIRST AFC ELEMENT) IF FOUND           
         BNE   ADDIT038                                                         
         TM    AFCXSTAT-AFCELD(R2),AFCXPRIM                                     
         BO    *+14                IF PRIME ALWAYS SAVE                         
         OC    AIAAFCEL,AIAAFCEL                                                
         BNZ   ADDIT038            NOT FIRST NON-PRIME AFCEL                    
         ST    R2,AIAAFCEL                                                      
                                                                                
ADDIT038 CLI   DLDSEL,DLPSEDCQ     REGULAR POSTING ELEMENTS                     
         BL    *+12                                                             
         CLI   DLDSEL,DLPSECRQ     ARE 68,69 AND 6A                             
         BNH   ADDIT040                                                         
         ST    R2,AIALAST          SET A(LAST EXTRA ELEMENT)                    
         OC    AIAFRST,AIAFRST                                                  
         BNZ   *+8                                                              
         ST    R2,AIAFRST          SET A(FIRST EXTRA ELEMENT)                   
         B     ADDIT034                                                         
                                                                                
ADDIT040 TM    CSBIND1,TYPIORDS    TEST PROCESSING ORDERS                       
         BZ    ADDIT044                                                         
         TM    CSBIND1,TYPIOMEM    TEST CAN HAVE MEMO ORDERS                    
         BZ    ADDIT042                                                         
         L     R1,AIPARM           R1=A(CALLER'S PARAMETER LIST)                
         L     R1,8(R1)            R1=A(OLD BATCH ITEM ENTRY)                   
         TM    10(R1),X'80'        TEST TAX ITEM (ORDER MEMO)                   
         BNZ   ADDIT044                                                         
                                                                                
ADDIT042 OC    AIAFFNEL,AIAFFNEL   BUILD ORDER ELEMENT IF REQUIRED              
         BZ    ADDIT044                                                         
         LR    R0,R2                                                            
         L     R2,AIAFFNEL                                                      
         GOTOR BLDBIO                                                           
         LR    R2,R0                                                            
                                                                                
         USING DLPOSTD,R2                                                       
ADDIT044 LA    R1,AITRNEL                                                       
         L     RE,TRNREC                                                        
         USING TRNRECD,RE                                                       
         MVC   TRNKCULA,DLPSDBAC   ASSUME DEBIT(69) OR 68 (DOUBLE)              
         MVC   TRNKCULC,DLPSCRAC                                                
         MVC   TRNKDATE,TRNDATE                                                 
         MVC   TRNKREF,TRNREF                                                   
         MVI   TRNKSBR,0                                                        
         XC    TRNKSTA,TRNKSTA                                                  
         NI    TRNSTAT,FF-TRNSHOLD-TRNSBREC-TRNSNOCM                            
         TM    DLPSTYPE,DLPSTNCM                                                
         BZ    *+8                                                              
         OI    TRNSTAT,TRNSNOCM    NON-COMMISSIONABLE                           
         TM    DLPSTYPE,DLPSTBRC                                                
         BZ    *+8                                                              
         OI    TRNSTAT,TRNSBREC    RECONCILED ADVANCE PAYMENT                   
         TM    DLPSTYPE,DLPSTHCQ                                                
         BZ    *+8                                                              
         OI    TRNSTAT,TRNSHOLD    HOLD FOR GERMAN CHEQUES                      
         ZAP   TRNAMNT,DLPSAMNT                                                 
         MVC   TRNOFFC,DLPSANAL                                                 
         OI    TRNSTAT,TRNSDR      SET DEBIT                                    
         CLI   DLPSEL,DLPSECRQ                                                  
         BNE   ADDIT046                                                         
         NI    TRNSTAT,FF-TRNSDR                                                
         MVC   TRNKCULA,DLPSCRAC                                                
         MVC   TRNKCULC,DLPSDBAC                                                
                                                                                
ADDIT046 MVC   TRNKWORK,BCSPACES   SET ANALYSIS CODE FOR PRODUCTION             
         CLC   TRNKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         BNE   ADDIT048                                                         
         MVC   TRNKWORK,DLPSANAL                                                
         CLI   TRNTYPE,BT34                                                     
         BNE   ADDIT048                                                         
         SR    RF,RF                                                            
         IC    RF,AIACTSTN         INCREMENT SET NUMBER                         
         AH    RF,=H'1'                                                         
         STC   RF,AIACTSTN                                                      
         DROP  RE                                                               
                                                                                
ADDIT048 MVC   TRNCACNM,DLPSCRNM   IF DEBIT CONTRA NAME IS CREDIT NAME          
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+10                                                             
         MVC   TRNCACNM,DLPSDBNM   IF CREDIT CONTRA NAME IS DEBIT NAME          
         TM    LSTTSTAT,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BNZ   ADDIT054                                                         
                                                                                
         TM    CSBIND1,TYPIADVP    TEST ADVANCE PAYMENT BATCH                   
         BZ    ADDIT050                                                         
         TM    TRNSTAT,TRNSDR      TEST FOR DEBIT                               
         BNZ   ADDIT054                                                         
         CLC   BCCPYEL+(CPYBANK-CPYELD)(L'CPYBANK),DLPSCRU                      
         BE    ADDIT052                                                         
         B     ADDIT054                                                         
                                                                                
ADDIT050 TM    CSBIND1,TYPICSHR    TEST CASH RECEIPT BATCH                      
         BZ    ADDIT054                                                         
         TM    TRNSTAT,TRNSDR      AND A DEBIT                                  
         BZ    ADDIT054                                                         
         CLC   BCCPYEL+(CPYRECV-CPYELD)(L'CPYRECV),DLPSDBU                      
         BE    ADDIT054                                                         
                                                                                
ADDIT052 GOTOR BLDBIC              BUILD CHEQUE ITEM ELEMENT                    
         B     ADDIT056            AND DON'T POST DRAFT TRANSACTION             
                                                                                
ADDIT054 GOTOR UPDTRN,LSTTABD      UPDATE FILES                                 
         LA    R1,AITRNEL                                                       
                                                                                
ADDIT056 CLI   DLPSEL,DLPSEDCQ     DO CREDIT SIDE OF A DOUBLE                   
         BNE   ADDIT058                                                         
         L     RE,TRNREC                                                        
         USING TRNRECD,RE                                                       
         NI    TRNSTAT,FF-TRNSDR                                                
         MVC   TRNKCULA,DLPSCRAC                                                
         MVC   TRNKCULC,DLPSDBAC                                                
         DROP  RE                                                               
         MVC   TRNCACNM,DLPSDBNM   CONTRA NAME IS DEBIT NAME                    
         GOTOR UPDTRN,LSTTABD      UPDATE FILES                                 
                                                                                
ADDIT058 XC    AIADDRS(AIADDRSL),AIADDRS                                        
         B     ADDIT034                                                         
                                                                                
ADDIT060 MVI   TRNINDS,TRNICONV+TRNILAST                                        
         OI    TRNINDS2,TRNIUPDG                                                
         GOTOR VADDTRN,ADDTRND     CALL ADDTRN TO WRITE ACCOUNT ETC.            
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERRORS                            
         TM    AIFLAG,AIFNOADD     TEST DON'T ADD ITEM                          
         BNZ   ADDIT126                                                         
         DROP  R1                                                               
                                                                                
         GOTOR ABLDBAK,LSTTABD     BUILD BATCH HEADER KEY                       
         LA    R2,IOKEY                                                         
         USING TBAKEY,R2                                                        
         MVC   TBAKTSEQ,LSTBITMA   SET ITEM NUMBER IN KEY                       
         CLI   CSACT,ACTCHA        TEST ITEM/CHANGE                             
         BNE   ADDIT078                                                         
         LA    RE,BCITECUR                                                      
         MVC   TBAKTSEQ,LSTISEQ-LSTTABD(RE)                                     
         GOTOR AIO,IORDUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         MVC   AISSTA,TBARESTA                                                  
         LA    R2,TBARFST                                                       
         SR    R0,R0                                                            
                                                                                
         USING BIAELD,R2                                                        
ADDIT062 CLI   BIAEL,BIAELQ        TEST ITEM AMOUNT ELEMENT                     
         BNE   ADDIT064                                                         
         CLI   BIALN,BIALN2Q                                                    
         BL    ADDIT064                                                         
         CLC   BIASIS,CSOLINE      LINE NUMBERS MUST MATCH                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AISNO,BIASNO        SAVE SCREEN AND LINE NUMBERS                 
         B     ADDIT074                                                         
                                                                                
         USING ASKELD,R2                                                        
ADDIT064 CLI   ASKEL,0                                                          
         BE    ADDIT076                                                         
         CLI   ASKEL,ASKELQ        TEST ACCOUNT SYSTEM KEY ELEMENT              
         BNE   ADDIT066                                                         
                                                                                
         MVC   IOKEY,ASKKEY        DELETE TRANSACTION RECORD                    
         GOTOR AIO,IORDUP+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR DELTRN                                                           
                                                                                
         USING GINELD,R2                                                        
ADDIT066 CLI   GINEL,GINELQ        DELETE OLD TXS/PASSIVES                      
         BNE   ADDIT072                                                         
         LA    R3,AITRNS                                                        
                                                                                
ADDIT068 MVC   IOKEY,0(R3)                                                      
         GOTOR AIO,IORD+IOACCMST+IO3                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO3                                                          
         USING TRNRECD,RE                                                       
         LA    RF,TRNRFST                                                       
         USING TRNELD,RF                                                        
         LA    R1,IOKEY                                                         
         USING GINPASD,R1                                                       
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,CUABIN                                                   
         MVC   GINPINV,GININV                                                   
         CLC   CSGIN,GININV        GIN CHANGES IF NOT A SPLIT INVOICE           
         BE    *+14                                                             
         MVC   GINPISN,=X'FFFF'    SO DELETE ALL OLD TXS/PASSIVES               
         B     ADDIT070                                                         
         MVI   GINPPTYP,GINTMAC    ELSE DELETE ALL EXCEPT CURRENT               
         CLC   =C'SG',TRNKULA      CREDITOR/VAT TXS/PASSIVES                    
         BNE   *+8                 (PASS KEYS NOT TO BE DELETED)                
         MVI   GINPPTYP,GINTSUB                                                 
         MVC   GINPULA,TRNKULA                                                  
         MVC   GINPWORK,TRNANAL                                                 
         MVC   GINPDA,IODA                                                      
                                                                                
ADDIT070 GOTOR ADELGIN                                                          
         AHI   R3,L'AITRNS                                                      
         OC    0(L'AITRNS,R3),0(R3)                                             
         BZ    ADDIT074                                                         
         B     ADDIT068                                                         
         DROP  R1,RE,RF                                                         
                                                                                
ADDIT072 DS    0H                                                               
         USING BIOELD,R2                                                        
ADDIT074 IC    R0,BIOLN                                                         
         AR    R2,R0                                                            
         B     ADDIT062                                                         
                                                                                
ADDIT076 GOTOR ABLDBAK,LSTTABD     BUILD BATCH HEADER KEY                       
         LA    R2,IOKEY                                                         
         USING TBAKEY,R2                                                        
         LA    RE,BCITECUR                                                      
         MVC   TBAKTSEQ,LSTISEQ-LSTTABD(RE)                                     
         B     ADDIT082                                                         
                                                                                
ADDIT078 GOTOR AIO,IORDUPD+IOACCDIR+IO1                                         
         BE    ADDITEAB            UNWIND VIA ABEND                             
         TM    IOERR,IOERNF        TEST NOT FOUND (OK TO ADD)                   
         BNZ   ADDIT080                                                         
         TM    IOERR,IOEDEL        TEST RECORD DELETED                          
         BZ    ADDITEAB            UNWIND VIA ABEND                             
         OI    AIFLAG,AIFITDEL     SET DELETED ITEM RECORD EXISTS               
         GOTOR AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
ADDIT080 LA    R2,IOKEY                                                         
         GOTOR ABLDBAK,LSTTABD     BUILD BATCH HEADER KEY                       
         MVC   TBAKTSEQ,LSTBITMA   SET ITEM NUMBER IN KEY                       
                                                                                
ADDIT082 L     R2,AIO1                                                          
         MVC   TBAKEY,IOKEY        SET BATCH ITEM KEY                           
         XC    TBARSTA(TBARFST-TBARSTA),TBARSTA                                 
         L     RE,BCAUTL                                                        
         MVC   TBARESCR,TSCRNUM+6-UTLD(RE)                                      
         CLC   TBARESCR,CSBITS     TEST LOADED SCREEN IS ACTUAL                 
         BNE   *+8                                                              
         MVI   TBARESCR,0          YES - CLEAR SCREEN NUMBER                    
*        MVC   TBARECFN,TCFN-UTLD(RE)   NO LONGER USED                          
         TM    CSBIND5,TYPIOVTY    TEST OVERLAY SUPPLIES TYPE                   
         BZ    *+10                                                             
         MVC   TBARETYP,AITRNTYP   SET ENTRY TRANSACTION TYPE                   
         MVC   TBARESPG,CSSPROG    SET ENTRY OVERLAY SUB-PROGRAM                
                                                                                
         LA    R3,TBARFST                                                       
         USING BIAELD,R3           BUILD ITEM AMOUNT ELEMENT                    
         XC    BIAELD(BIALN3Q),BIAELD                                           
         MVI   BIAEL,BIAELQ                                                     
         MVI   BIALN,BIALNQ                                                     
         L     R1,AIPARM           R1=A(CALLER'S PARAMETER LIST)                
         L     RF,4(R1)            RF=A(ITEM AMOUNT)                            
         ZAP   BIAAMT,0(6,RF)                                                   
         L     RF,8(R1)            RF=A(ITEM REFERENCE)                         
         MVC   BIAREF,0(RF)                                                     
                                                                                
         ICM   RF,15,AIATAXES      ADDRESS OF TAXES PASSED                      
         BZ    ADDIT083                                                         
         CP    0(6,RF),=P'0'                                                    
         BZ    ADDIT083                                                         
         ZAP   BIATAX,0(6,RF)                                                   
         MVI   BIALN,BIALN3Q                                                    
                                                                                
ADDIT083 TM    CSBIND2,TYPIMLT     TEST MULTI ITEM/CHANGE SUPPORTED             
         BZ    ADDIT086                                                         
         TM    LSTBIND2,LSTBIMLT   TEST NEW STYLE BATCH RECORD                  
         BZ    ADDIT086                                                         
         MVI   BIALN,BIALN2Q       SET EXTENDED ELEMENT LENGTH                  
         MVC   BIASIS,CSOLINE      SET SCREEN LINE NUMBER                       
         CLI   CSACT,ACTCHA        TEST ACTION CHANGE                           
         BNE   *+14                                                             
         MVC   BIASNO,AISNO        YES - USE ORIGINAL SCREEN NUMBER             
         B     ADDIT086                                                         
         ICM   R1,3,LSTBHISN       INCREMENT INPUT SCREEN NUMBER                
         AHI   R1,1                                                             
         TM    BCINDS1,BCIFRST     TEST FIRST LINE FOR NEW SCREEN               
         BNZ   ADDIT084                                                         
         OI    BCINDS1,BCIFRST     SET NOT FIRST TIME NEXT TIME                 
         OC    LSTBHISN,LSTBHISN   TEST FIRST ITEM RECORD                       
         BZ    *+14                                                             
         CLC   CSOLINE,CSOLINEL    TEST FIRST LINE THIS SCREEN                  
         BH    ADDIT084            IS HIGHER THAN LAST ON PREVIOUS              
         STCM  R1,3,LSTBHISN       ELSE THIS IS A NEW SCREEN                    
                                                                                
ADDIT084 MVC   BIASNO,LSTBHISN     SET INPUT SCREEN NUMBER                      
         MVC   CSOLINEL,CSOLINE    SAVE HIGHEST LINE THIS SCREEN                
                                                                                
ADDIT086 ICM   RF,15,AIAINOFF      ADDRESS OF INTERCOMPANY OFFICE               
         BZ    ADDIT087                                                         
         CLC   0(2,RF),BCSPACES                                                 
         BE    ADDIT087                                                         
         MVC   BIAIOFF,0(RF)       SAVE THE INTERCOMPANY OFFICE                 
         MVI   BIASTAT,BIAELG      INDICATE ELIGIBLE FOR BALANCING              
         MVI   BIALN,BIALN4Q       SET NEW LENGTH                               
                                                                                
ADDIT087 TM    CSBIND1,TYPICUMU    TEST ACCUMULATING TOTAL DRS/CRS              
         BO    *+12                                                             
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS/TAX                  
         BZ    ADDIT090                                                         
         MVC   TBARESTA,AIITESTA                                                
         TM    AIITESTA,TBAESDAC   TEST DR/-DR OR CR/-CR (NO CHANGE)            
         BM    ADDIT090                                                         
         BZ    ADDIT088                                                         
         AP    LSTBTCRS,BIAAMT     UPDATE TOTAL DR & CR TOTAL IF A PAIR         
         AP    LSTBTDRS,BIAAMT                                                  
         B     ADDIT090                                                         
                                                                                
ADDIT088 LA    RE,LSTBTCRS                                                      
         TM    AIITESTA,TBAESIDR   TEST DEBIT ITEM                              
         BZ    *+8                                                              
         LA    RE,LSTBTDRS                                                      
         TP    0(L'LSTBTDRS,RE)                                                 
         BE    *+10                                                             
         ZAP   0(L'LSTBTDRS,RE),BCPZERO                                         
         AP    0(L'LSTBTDRS,RE),BIAAMT                                          
                                                                                
ADDIT090 SR    R0,R0                                                            
         IC    R0,BIALN                                                         
         AR    R3,R0               BUMP TO NEXT ELEMENT                         
                                                                                
         USING SFSELD,R3                                                        
         GOTOR ASAVFLD,BCPARM,BASOLY2H,SFSELD                                   
         L     R3,BCFULL           SAVFLD RETURNS A(NEXT) IN BCFULL             
                                                                                
         ICM   R0,1,AITRNSN        R0=NUMBER OF TRANSACTION KEYS                
         BZ    ADDIT096                                                         
         LA    R1,AITRNS           R1=A(TRANSACTION KEYS)                       
         OC    AIAGINEL,AIAGINEL   TEST GROUP INVOICE NO. ELMT SAVED            
         BZ    ADDIT092                                                         
         XC    AIAGINEL,AIAGINEL                                                
         AHI   R1,L'AITRNS                                                      
         B     ADDIT096                                                         
                                                                                
ADDIT092 MVC   BCWORK(L'ASKKEY),0(R1)  SAVE FIRST TX FOR ORDER DETAILS          
                                                                                
         USING ASKELD,R3           BUILD ACCOUNT SYSTEM KEY ELEMENTS            
         SR    RE,RE               RE=ELEMENT SEQUENCE NUMBER                   
ADDIT094 MVI   ASKEL,ASKELQ        BUILD SYSTEM KEY ELEMENT                     
         MVI   ASKLN,ASKLNQ                                                     
         AHI   RE,1                INCREMENT SEQUENCE NUMBER                    
         STC   RE,ASKSEQN                                                       
         MVC   ASKKEY,0(R1)        SET KEY VALUE                                
                                                                                
         AHI   R3,ASKLNQ                                                        
         AHI   R1,L'AITRNS         BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,ADDIT094                                                      
                                                                                
ADDIT096 OC    AIBICEL1,AIBICEL1   TEST CHEQUE ELEMENT SAVED                    
         BZ    ADDIT098                                                         
         USING BICELD,R3           BUILD BATCH ITEM CHEQUE ELEMENT              
         SR    RE,RE                                                            
         IC    RE,AIBICEL1+(BICLN-BICELD)                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BICEL(0),AIBICEL1   MOVE SAVED ELEMENT TO RECORD                 
         LA    R3,1(RE,R3)                                                      
         XC    AIBICEL1,AIBICEL1                                                
                                                                                
ADDIT098 OC    AIBICEL2,AIBICEL2   TEST CHEQUE ELEMENT 2 SAVED                  
         BZ    ADDIT100                                                         
         USING BICELD,R3           BUILD BATCH ITEM CHEQUE ELEMENT              
         SR    RE,RE                                                            
         IC    RE,AIBICEL2+(BICLN-BICELD)                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BICEL(0),AIBICEL2   MOVE SAVED ELEMENT TO RECORD                 
         LA    R3,1(RE,R3)                                                      
         XC    AIBICEL2,AIBICEL2                                                
         DROP  R3                                                               
                                                                                
ADDIT100 SR    RE,RE                                                            
         ICM   R1,15,AIANAMEL                                                   
         BZ    ADDIT102                                                         
         IC    RE,NAMLN-NAMELD(R1)                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)                                                    
         AR    R3,RE                                                            
                                                                                
ADDIT102 ICM   R1,15,AIAADREL                                                   
         BZ    ADDIT104                                                         
         IC    RE,ADRLN-ADRELD(R1)                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)                                                    
         AR    R3,RE                                                            
                                                                                
ADDIT104 ICM   R1,15,AIAAFCEL                                                   
         BZ    ADDIT106                                                         
         IC    RE,AFCLN-AFCELD(R1)                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)                                                    
         AR    R3,RE                                                            
                                                                                
ADDIT106 ICM   R1,15,AIAXTRAS      TEST A(EXTRA ELEMENTS PASSED)                
         BZ    ADDIT112                                                         
         SR    R0,R0                                                            
         SR    RF,RF               RF=TOTAL L'EXTRA ELEMENTS                    
                                                                                
ADDIT108 CLI   0(R1),0             TEST E-O-L                                   
         BE    ADDIT110                                                         
         IC    R0,1(R1)            NO - BUMP TO NEXT ELEMENT                    
         AR    RF,R0                                                            
         AR    R1,R0                                                            
         B     ADDIT108                                                         
                                                                                
ADDIT110 LTR   RF,RF               TEST ANY EXTRA ELEMENTS PASSED               
         BZ    ADDIT112                                                         
         LA    R0,0(R3)                                                         
         AR    R3,RF                                                            
         LR    R1,RF                                                            
         L     RE,AIAXTRAS                                                      
         MVCL  R0,RE                                                            
*                                  *** ACBG REQUIRES BIOEL AT END ***           
ADDIT112 TM    AIFLAG,AIFORDER     TEST ITEM CARRIES ORDERS                     
         BZ    *+8                                                              
         OI    TBARESTA,TBAESORD   SET ORDER CARRIED                            
         LA    R0,AIBIOELM         EXTRACT ANY SAVED ORDER ELEMENTS             
         LA    R1,AIBIOEL                                                       
                                                                                
ADDIT114 CLI   0(R1),BIOELQ                                                     
         BNE   ADDIT116            FINISHED                                     
         OI    TBARESTA,TBAESORD   SET ORDER CARRIED                            
         USING BIOELD,R3           BUILD BATCH ITEM ORDER ELEMENT               
*&&UK                                                                           
         MVC   BIOELD(BIOLNQ),0(R1)                                             
         AHI   R1,BIOLNQ           BUMP TO NEXTS                                
         AHI   R3,BIOLNQ                                                        
         BCT   R0,ADDIT114                                                      
ADDIT116 XC    AIBIOEL(AIBIOELN),AIBIOEL                                        
*&&                                                                             
*&&US                                                                           
         SR    RF,RF                                                            
         IC    RF,1(R1)            LENGTH OF BIOEL                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BIOELD(0),0(R1)                                                  
         AHI   RF,1                                                             
         AR    R1,RF                                                            
         AR    R3,RF                                                            
         BCT   R0,ADDIT114                                                      
                                                                                
ADDIT116 LA    RE,AIBIOEL                                                       
         LHI   RF,AIBIOELN                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*&&                                                                             
         DROP  R3                                                               
                                                                                
         MVI   0(R3),0             SET END OF ITEM RECORD                       
         AHI   R3,1                                                             
         L     R0,AIO1                                                          
         SR    R3,R0                                                            
         STCM  R3,3,TBARLEN        SET ITEM RECORD LENGTH                       
         CLI   CSACT,ACTCHA        TEST ITEM/CHANGE                             
         BNE   ADDIT118                                                         
         GOTOR AIO,IOPUTREC+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   AISSTA,TBARESTA     TEST ITEM STATUS CHANGED                     
         BE    ADDIT126                                                         
         LR    R1,R2               BUILD DIRECTORY KEY                          
         LA    R2,IOKEY                                                         
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA                                                      
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     ADDIT126                                                         
                                                                                
ADDIT118 TM    AIFLAG,AIFITDEL     TEST (DELETED) ITEM RECORD FOUND             
         BNZ   ADDIT120                                                         
                                                                                
         L     R1,AIO1             MAKE SURE KEY IS VALID                       
         OC    0(2,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR AIO,IOADDREC+IOACCMST+IO1                                        
         BE    ADDIT122                                                         
         DC    H'0'                                                             
                                                                                
ADDIT120 GOTOR AIO,IOPUTREC+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R1,R2               BUILD DIRECTORY KEY                          
         LA    R2,IOKEY                                                         
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA                                                      
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
ADDIT122 TM    LSTTSTAT,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BZ    ADDIT126                                                         
         L     R2,AIO1                                                          
         TM    TBARESTA,TBAESORD   TEST RECORD CARRIES ORDER(S)                 
         BZ    ADDIT126                                                         
         MVI   AICBIOSN,0                                                       
         LA    R1,TBARFST                                                       
         USING BIOELD,R1                                                        
         SR    R0,R0                                                            
                                                                                
ADDIT124 IC    R0,BIOLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   BIOEL,0             TEST EOR                                     
         BE    ADDIT126                                                         
         CLI   BIOEL,BIOELQ        TEST BATCH ITEM ORDER ELEMENT                
         BNE   ADDIT124                                                         
         IC    R0,AICBIOSN         CURRENT BIOEL SEQUENCE NUMBER                
         LA    R0,1(R0)                                                         
         STC   R0,AICBIOSN                                                      
         GOTOR AUPDORD,BODMCB,BIOELD,AICBIOSN,AIO1                              
         B     ADDIT124                                                         
                                                                                
ADDIT126 GOTO1 AORDAUD,BCPARM,(2,AIO1)                                          
         GOTOR ABLDBAK,LSTTABD     BUILD BATCH HEADER KEY                       
         XC    IODAOVER,IODAOVER                                                
         GOTOR AIO,IORDUP+IOACCMST+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO3                                                          
         USING TBARECD,R2                                                       
         LA    R1,TBARFST          UPDATE BATCH HEADER ELEMENT                  
         USING BHDELD,R1                                                        
         SR    R0,R0                                                            
                                                                                
ADDIT128 CLI   BHDEL,0             TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ        TEST BATCH HEADER ELEMENT                    
         BE    *+14                                                             
         IC    R0,BHDLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     ADDIT128                                                         
                                                                                
         TM    AIFLAG,AIFNOADD     TEST NOT ADDING ITEM                         
         BO    ADDIT130                                                         
         CLI   CSACT,ACTCHA        DON'T UPDATE ITEM COUNT IF CHANGE            
         BE    ADDIT130                                                         
         SR    RE,RE                                                            
         ICM   RE,3,BHDITEMA       ENSURE BATCH ITEM COUNT IS CORRECT           
         AHI   RE,1                                                             
         CLM   RE,3,LSTBITMA                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         STCM  RE,3,BHDITEMA       UPDATE ITEM COUNT                            
         TM    CSBIND2,TYPIMLT     TEST MULTIPLE CHANGE BATCH                   
         BZ    *+10                                                             
         MVC   BHDHISNO,LSTBHISN                                                
                                                                                
ADDIT130 ZAP   BHDCASHA,LSTBCSHA   UPDATE CASH TOTAL                            
         TM    CSBIND8,TYPIDCSA    TEST DR/CR SUBSIDIARILY ACCUMULATED          
         BO    *+20                                                             
         TM    CSBIND1,TYPICUMU    TEST ACCUMULATING TOTAL DRS/CRS              
         BO    *+12                                                             
         TM    CSBIND5,TYPIHRTX    TEST ACCUMULATING HOURS/TAX                  
         BZ    *+16                                                             
         ZAP   BHDTOTDR,LSTBTDRS   UPDATE TOTAL DEBITS                          
         ZAP   BHDTOTCR,LSTBTCRS   UPDATE TOTAL CREDITS                         
         TM    LSTBIND2,LSTBISIN   SET BATCH CONTIANS SPLIT INVOICES            
         BZ    *+8                                                              
         OI    BHDSTAT2,BHDSSPIN                                                
         GOTOR AIO,IOWRITE+IOACCMST+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR ASETMSK,LSTTABD                                                  
                                                                                
ADDITEX  ICM   R0,15,AIATXKEY      TEST PASSING BACK TX KEYS TABLE              
         JZ    ROUTE                                                            
         LHI   R1,AITRNSQ                                                       
         LA    RE,AITRNSN                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     ROUTE                                                            
                                                                                
ADDITEAB LA    R1,BASACTH          ABEND IF NEW ITEM IS INVALID                 
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$INVIT)                                           
         OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         L     RD,BCSVRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  EXIT TO ROOT CALL POINT                      
         DROP  R1,R2,R4,RB                                                      
                                                                                
R2WORKD  DSECT                     ** ADDITE S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
AIPARM   DS    A                   A(CALLING PARAMETER LIST)                    
AIAREC   DS    A                   A(ACCDAY RECORD)                             
AIAXTRAS DS    A                   A(EXTRA ITEM ELEMENTS)                       
AIATXKEY DS    A                   A(TX KEYS TABLE)                             
AIATAXES DS    A                   A(TAX AMOUNT)                                
AIAINOFF DS    A                   A(INTERCOMPANY OFFICE)                       
AIADDRS  DS    0A                  ** ELEMENT ADDRESSES **                      
AIAFRST  DS    A                   A(FIRST TRANSACTION EXTRA ELEMENT)           
AIALAST  DS    A                   A(LAST TRANSACTION EXTRA ELEMENT)            
AIAFFNEL DS    A                   A(FREE FORM NUMBER ELEMENT)                  
AIAFFTEL DS    A                   A(FREE FORM TEXT ELEMENT)                    
AIADDRSL EQU   *-AIADDRS                                                        
AIANAMEL DS    A                   A(NAME ELEMENT ON ACCDAY RECORD)             
AIAADREL DS    A                   A(ADDRESS ELEMENT ON ACCDAY RECORD)          
AIAAFCEL DS    A                   A(1ST AFC ELEMENT ON ACCDAY RECORD)          
AIAGINEL DS    A                   A(GROUP INVOICE ELEMENT)                     
AIASJXP  DS    A                   A(X-JOB AMOUNT ELEM OR 0)                    
AIAFSDR  DS    A                   A(FIRST SINGLE DEBIT ELEMENT)                
AIACSHD  DS    A                   A(CASH DISCOUNT ELEM OR 0)                   
AIABAT   DS    A                   A(CURRENT LSTTAB ENTRY)                      
AIIOKEY  DS    XL(L'IOKEY)         I/O KEY                                      
AIWORK   DS    XL256               WORK AREA                                    
AITRNEL  DS    XL512                                                            
AIBICEL1 DS    XL(BICLN1Q+L'BICCACN)                                            
AIBICEL2 DS    XL(BICLN1Q+L'BICCACN)                                            
AIBIOEL  DS    (AIBIOELM)XL(BIOLNQ)                                             
AIBIOELN EQU   *-AIBIOEL                                                        
*&&UK                                                                           
AIBIOELM EQU   8                                                                
*&&                                                                             
*&&US                                                                           
AIBIOELM EQU   40                                                               
*&&                                                                             
AISNO    DS    XL(L'BIASNO)        SAVED SCREEN NUMBER FROM BIAEL               
AISSTA   DS    XL(L'TBAKSTA)       SAVED ITEM STATUS FOR ITEM/CHANGE            
AIITESTA DS    XL(L'TBAKESTA)      ITEM STATUS                                  
AIFLAG   DS    X                   FLAG BYTE                                    
AIFITDEL EQU   X'80'               DELETED ITEM EXISTS                          
AIFNOADD EQU   X'40'               DON'T ADD ITEM                               
AIFORDER EQU   X'20'               SET TBAESORD IN ITEM RECORD                  
AIFFNTRN EQU   X'10'               FFNEL ON THIS TRANSACTION                    
AIACTI   DS    X                   INDICATOR FOR AIACTS                         
AIACTISE EQU   X'80'               SE ACCOUNT PRESENT IN AIACTS                 
AIACTISB EQU   X'40'               SB ACCOUNT PRESENT IN AIACTS                 
AIACTISI EQU   X'20'               SI ACCOUNT PRESENT IN AIACTS                 
AIACTSN  DS    X                   NUMBER OF ENTRIES IN AIACTS                  
AIACTSTN DS    X                   CURRENT SET NUMBER                           
AICBIOSN DS    XL1                 CURRENT BIOEL SEQUENCE NUMBER                
                                                                                
AIACT    DS    0C                  ATTRIBUTE ENTRY                              
AIACTSET DS    X                   SET NUMBER FOR TYPE 34(OR ZERO)              
AIACTSTA DS    XL(L'APENSTAT)      STATUS                                       
AIACTSAC DS    CL(L'APENACT)       ACCOUNT                                      
AIACTSL  EQU   *-AIACT                                                          
AIACTSM  EQU   32                  MAXIMUM NUMBER OF AIACTS ENTRIES             
         ORG   AIACT                                                            
AIACTS   DS    (AIACTSM)XL(AIACTSL)                                             
                                                                                
AITRNSM  EQU   30                  MAXIMUM NUMBER OF AITRNS ENTRIES             
AITRNSN  DS    X                   NUMBER OF ENTRIES IN AITRNS                  
AITRNS   DS    (AITRNSM)XL(L'TRNKEY)                                            
AITRNSQ  EQU   *-AITRNSN                                                        
AITRNTYP DS    XL(L'TRNTYPE)       TRANSACTION TYPE FROM OVERLAY                
                                                                                
AIWCNUM  DS    X                   NUMBER OF ENTRIES IN AIWCNUM                 
AIWCTAB  DS    0C                                                               
AIWCODE  DS    CL(L'TRNKWORK)                                                   
AIWCAMT  DS    PL(L'TRNAMNT)                                                    
AIWCLNQ  EQU   *-AIWCTAB                                                        
AIWCMAX  EQU   8                                                                
         ORG   AIWCTAB                                                          
AIWCODES DS    (AIWCMAX)XL(AIWCLNQ)                                             
                                                                                
AIWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A TRANSACTION AND UPDATE DRAFT TRANSACTION COUNT  *         
* IN ACCOUNT HEADER                                                   *         
*                                                                     *         
* NTRY - IOKEY CONTAINS KEY OF TRANSACTION TO BE DELETED              *         
***********************************************************************         
                                                                                
DELTRN   NTR1  LABEL=NO                                                         
         GOTOR AIO,IOGETRUP+IOACCMST+IO3                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         USING TRNRECD,R1                                                       
         TM    TRNRSTAT,TRNSDRFT                                                
         JO    *+6                                                              
         DC    H'0'                ERROR - TRANS IS LIVE                        
         OI    TRNRSTAT,TRNSDELT                                                
         GOTOR AIO,IOPUTREC+IOACCMST+IO3                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOKEY                                                         
         L     RF,AIO3                                                          
         MVC   TRNKEY,0(RF)                                                     
         GOTOR AIO,IORDUP+IOACCDIR+IO3                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOKEY                                                         
         L     RF,AIO3                                                          
         MVC   TRNKSTA,TRNRSTA-TRNRECD(RF)                                      
         MVC   TRNKDA,IODA                                                      
         GOTOR AIO,IOWRITE+IOACCDIR+IO3                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R1,IOKEY            UPDATE DRAFT COUNT IN ACCOUNT RECORD         
         USING ACTRECD,R1                                                       
         MVC   ACTKEY+ACTKEND(L'ACTKEY-ACTKEND),BCSPACES                        
         GOTOR AIO,IOREAD+IOACCDIR+IO3                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOGETRUP+IOACCMST+IO3                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         LA    R1,ACTRFST                                                       
         USING ASTELD,R1                                                        
DELTRN02 IC    R0,ASTLN                                                         
         AR    R1,R0                                                            
         CLI   ASTEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ASTEL,ASTELQ                                                     
         JNE   DELTRN02                                                         
         SR    RE,RE                                                            
         ICM   RE,7,ASTDRAFT                                                    
         JZ    DELTRNX                                                          
         BCTR  RE,0                                                             
         STCM  RE,7,ASTDRAFT                                                    
         GOTOR AIO,IOPUTREC+IOACCMST+IO3                                        
         JE    *+6                                                              
         DC    H'0'                                                             
DELTRNX  J     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD WORKCODE AND AMOUNT TO TABLE                         *         
***********************************************************************         
                                                                                
         USING DLPOSTD,R2          R2=A(FIRST ACCDAY ELEMENT)                   
ADDWCS   LA    RF,AIWCODES         RF=A(TABLE OF WORKCODE AND AMOUNTS)          
         LA    R0,AIWCMAX          R0=MAX NUMBER OF ENTRIES IN TABLE            
                                                                                
ADDWC02  OC    0(AIWCLNQ,RF),0(RF) TEST END OF TABLE                            
         JZ    ADDWC04                                                          
                                                                                
         AHI   RF,AIWCLNQ                                                       
         BRCT  R0,ADDWC02                                                       
         BR    RE                                                               
                                                                                
ADDWC04  MVC   AIWCODE-AIWCTAB(L'AIWCODE,RF),DLPSANAL                           
         ZAP   AIWCAMT-AIWCTAB(L'AIWCAMT,RF),DLPSAMNT                           
                                                                                
         IC    RF,AIWCNUM                                                       
         AHI   RF,1                                                             
         STC   RF,AIWCNUM                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD AN ENTRY TO TABLE OF PRIMARY ACCOUNTS FOR POSTING    *         
***********************************************************************         
                                                                                
         USING DLPOSTD,R2          R2=A(FIRST ACCDAY ELEMENT)                   
APEADR   LA    R1,DLPSDBU          R1=A(DEBIT ACCOUNT CODE)                     
         MVI   BCFLAG,APENSDR      SET STATUS TO DEBIT                          
         J     APEADD                                                           
                                                                                
APEACR   LA    R1,DLPSCRU          R1=A(CREDIT ACCOUNT CODE)                    
         MVI   BCFLAG,0            SET STATUS TO CREDIT                         
                                                                                
APEADD   LA    RF,AIACTS           RF=A(TABLE OF PRIMARY ACCOUNTS)              
         LHI   R0,AIACTSM          R0=MAX NUMBER OF ENTRIES IN TABLE            
APEADD02 OC    0(AIACTSL,RF),0(RF) TEST END OF TABLE                            
         JZ    APEADD06                                                         
         CLC   AIACTSET-AIACT(L'AIACTSET,RF),AIACTSTN TEST SET NUMBER           
         JNE   APEADD04                                                         
         CLC   AIACTSTA-AIACT(L'AIACTSTA,RF),BCFLAG   TEST STATUS               
         JNE   APEADD04                                                         
         CLC   AIACTSAC-AIACT(L'AIACTSAC,RF),0(R1)    TEST ACCOUNT              
         BER   RE                                                               
APEADD04 AHI   RF,AIACTSL          BUMP TO NEXT TABLE ENTRY                     
         BRCT  R0,APEADD02                                                      
         BR    RE                                                               
                                                                                
APEADD06 MVC   AIACTSET-AIACT(L'AIACTSET,RF),AIACTSTN   SET NUMBER              
         MVC   AIACTSTA-AIACT(L'AIACTSTA,RF),BCFLAG     STATUS                  
         MVC   AIACTSAC-AIACT(L'AIACTSAC,RF),0(R1)      ACCOUNT                 
         CLC   =C'SE',0(R1)        TEST PRIMARY SE POSTING                      
         JNE   *+8                                                              
         OI    AIACTI,AIACTISE     SET SE INDICATOR                             
*&&US                                                                           
         CLC   =C'SI',0(R1)        TEST PRIMARY SI POSTING                      
         JNE   *+8                                                              
         OI    AIACTI,AIACTISI     SET SI INDICATOR                             
         CLC   =C'SB',0(R1)        TEST PRIMARY SB POSTING                      
         JNE   APEADD08                                                         
         TM    ACOPSTAT,ACOXJOB    TEST X-JOB                                   
         JZ    *+8                                                              
         OI    AIACTI,AIACTISB                                                  
*&&                                                                             
APEADD08 IC    RF,AIACTSN          INCREMENT NUMBER OF TABLE ENTRIES            
         AHI   RF,1                                                             
         STC   RF,AIACTSN                                                       
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE/RECALL/CLOSE/UPDATE A BATCH                                    *         
***********************************************************************         
                                                                                
SAVBAT   OI    UBINDS,UBISAV                                                    
         J     UPDBAT                                                           
                                                                                
RCLBAT   OI    UBINDS,UBIRCL                                                    
         J     UPDBAT                                                           
                                                                                
DELBAT   OI    UBINDS,UBIDEL                                                    
         J     UPDBAT                                                           
                                                                                
CLOBAT   OI    UBINDS,UBICLO                                                    
                                                                                
UPDBAT   J     *+12                                                             
         DC    C'*UPDBAT*'                                                      
         BASR  RB,0                                                             
         SHI   RB,*-UPDBAT                                                      
         USING UPDBAT,RB                                                        
                                                                                
         NI    BCINDS1,FF-BCIFRST                                               
         TM    UBINDS,UBISAV       TEST BATCH SAVE                              
         BNZ   UPDBAT04                                                         
         TM    UBINDS,UBIRCL       TEST BATCH RECALL                            
         BNZ   UPDBAT04                                                         
         TM    UBINDS,UBIDEL       TEST BATCH DELETE                            
         BNZ   UPDBAT02                                                         
         TM    UBINDS,UBICLO       TEST BATCH CLOSE                             
         BNZ   UPDBAT02                                                         
                                                                                
         L     R1,AIO8             MUST BE BATCH UPDATE THEN                    
         XC    0(CHQTABL,R1),0(R1)                                              
         OI    UBINDS,UBIUPD                                                    
         GOTOR ATSARIO,TSASAV      SAVE CURRENT TSAR BUFFER TO DISK             
         L     R1,ATSABLK                                                       
         USING TSARD,R1                                                         
         MVI   TSRECI,0            INITIALISE FOR TRANSACTION BUFFER            
         MVI   TSINDS,TSINODSK                                                  
         MVI   TSIND2,0                                                         
         LA    R0,UBTSAREC                                                      
         ST    R0,TSAREC                                                        
         MVI   TSKEYL,L'UBTSAKEY                                                
         MVC   TSRECL,=Y(UBTSARLN)                                              
         MVI   TSACTN,TSAINI                                                    
         GOTOR VTSAR,TSARD         INITIALISE FOR TRANSACTION BUFFER            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSAADD       SET ACTION TO ADD                            
                                                                                
UPDBAT02 L     R0,ATIA             CLEAR TIA FOR CHEQUE/ASKKEY TABLE            
         LHI   R1,TWAMAXRL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
UPDBAT04 LA    R4,CSLSTCUR                                                      
         USING LSTTABD,R4          R4=A(BATCH MONTH TABLE ENTRY)                
         TM    UBINDS,UBIRCL+UBISAV+UBIDEL                                      
         BNZ   UPDBAT08                                                         
         L     R3,AADTBLK          INITIALISE ADDTRN CONTROL BLOCK              
         USING ADDTRND,R3          R3=A(ADDTRN BLOCK)                           
         GOTOR AINIADT             INITIALISE ADDTRN CONTROL BLOCK              
         MVC   TRNBSEQN,LSTBITMA   SET ITEM SEQUENCE NUMBER                     
         MVC   TRNPUSER,LSTBUSER                                                
         MVC   TRNBMOS,LSTBMOSP    PWOS P'YYMM'                                 
         MVC   TRNEFDT,LSTBEFDT    COMPRESSED BINARY EFFECTIVE DATE             
                                                                                
UPDBAT08 GOTOR ABLDBAK,LSTTABD     BUILD BATCH RECORD KEY                       
         MVC   UBTBAKEY,IOKEY      SAVE BATCH KEY                               
         GOTOR AIO,IORDUPD+IOACCMST+IO2                                         
         BE    *+6                                                              
         DC    H'0'                BATCH RECORD ERROR                           
                                                                                
         L     R2,AIO2                                                          
         USING TBARECD,R2                                                       
         TM    TBARHSTA,TBAHSIAD   INSTANT - JUST UPDATE BATCH STATUS           
         BO    UPDBAT52                                                         
         LA    R1,TBARFST          UPDATE CONTROL TOTALS                        
         USING BHDELD,R1                                                        
         SR    R0,R0                                                            
UPDBAT10 CLI   BHDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    R1,R0                                                            
         B     UPDBAT10                                                         
         TM    UBINDS,UBIDEL       TEST DELETE                                  
         BZ    UPDBAT12                                                         
         MVC   BHDDELDT,BCTODAYC   SET DATE DELETED                             
         MVC   BHDDELER,CUPASS     SET PERSON                                   
         MVC   LSTBITMA,BHDITEMA   SET BATCH ITEM COUNT                         
         B     UPDBAT14                                                         
UPDBAT12 XC    BHDIBNO,BHDIBNO     CLEAR INPUT BATCHER NUMBER                   
         TM    UBINDS,UBIRCL                                                    
         BZ    *+10                                                             
         MVC   BHDIBNO,CUPASS      RECALL - RESET INPUT BATCHER NUMBER          
         MVC   LSTBIBNO,BHDIBNO                                                 
         TM    UBINDS,UBISAV       SAVE - SKIP ITEM RECORDS                     
         BO    UPDBAT52                                                         
         TM    UBINDS,UBIRCL       RECALL - DON'T RESET CONTROL TOTALS          
         BO    UPDBAT14                                                         
         MVC   BHDITEMC,LSTBITMC                                                
         ZAP   BHDCASHC,LSTBCSHC                                                
                                                                                
UPDBAT14 LA    R2,UBTBAKEY         USE SAVED BATCH KEY                          
         CLC   LSTBITMA,TBAKTSEQ   TEST ALL ITEMS ACCOUNTED FOR                 
         BE    UPDBAT50            END OF BATCH                                 
         ICM   R1,3,TBAKTSEQ                                                    
         AHI   R1,1                                                             
         STCM  R1,3,TBAKTSEQ                                                    
         MVC   IOKEY(L'TBAKEY),UBTBAKEY                                         
         GOTOR AIO,IORDUPD+IOACCMST+IO1                                         
         BE    *+14                                                             
         TM    IOERR,IOEDEL        GET NEXT IF ITEM DELETED                     
         BNZ   UPDBAT14                                                         
         DC    H'0'                BATCH RECORD ERROR                           
         TM    UBINDS,UBIDEL       TEST DELETE                                  
         BNZ   *+12                                                             
         TM    IOKEY+(TBAKESTA-TBARECD),TBAESLDE                                
         BNZ   UPDBAT14                                                         
                                                                                
         L     R2,AIO1             R2=A(ITEM RECORD)                            
         TM    UBINDS,UBIDEL       TEST DELETE                                  
         BZ    *+12                                                             
         TM    TBARESTA,TBAESLDE   TEST ALREADY LOGICALLY DELETED               
         BNZ   UPDBAT49            SET TO PHYSICALLY DELETE THE RECORD          
         LA    R2,TBARFST          R2=A(FIRST ELEMENT)                          
         MVI   UBCBIOSN,0                                                       
         USING ASKELD,R2                                                        
*&&UK*&& XC    BCWORK,BCWORK                                                    
                                                                                
UPDBAT16 CLI   ASKEL,0             TEST EOR                                     
         BE    UPDBAT48                                                         
         CLI   ASKEL,ASKELQ        TEST ACCOUNT SYSTEM KEY ELEMENT              
         BNE   UPDBAT24                                                         
*&&UK                                                                           
         OC    BCWORK,BCWORK       SAVE FIRST TX FOR ORDER DETAILS              
         BNZ   *+16                                                             
         MVC   BCWORK(L'ASKKEY),ASKKEY                                          
*&&                                                                             
         TM    UBINDS,UBIDEL       TEST DELETE                                  
         BZ    UPDBAT22                                                         
         MVC   BCINVREF,BCSPACES                                                
         MVC   IOKEY,ASKKEY        READ TRANSACTION                             
         GOTOR AIO,IORDUP+IOACCMST+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         TM    TRNRSTA-TRNRECD(RF),TRNSDRFT                                     
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    TRNRSTA-TRNRECD(RF),TRNSDELT                                     
         MVC   BCINVDTE,TRNKDATE-TRNRECD(RF)                                    
         CLC   TRNKULA-TRNRECD(2,RF),=C'ST'                                     
         BE    *+10                                                             
         CLC   TRNKULA-TRNRECD(2,RF),=C'SX'                                     
         BE    *+14                                                             
         CLC   TRNKULA-TRNRECD(2,RF),=C'SV'                                     
         BNE   UPDBAT17                                                         
         MVC   BCINVREF(L'TRNKREF),TRNKREF-TRNRECD(RF)                          
         LA    RF,TRNRFST-TRNRECD(RF)                                           
         XR    R0,R0                                                            
         USING FFTELD,RF                                                        
UPDBA16A IC    R0,FFTLN            FIRST LOOK FOR INVOICE# FFTEL                
         AR    RF,R0                                                            
         CLI   FFTEL,0                                                          
         BE    UPDBAT17                                                         
         CLI   FFTEL,FFTELQ                                                     
         BNE   UPDBA16A                                                         
         CLI   FFTTYPE,FFTTINVN                                                 
         BNE   UPDBA16A                                                         
         MVC   BCINVREF,BCSPACES                                                
         XR    RE,RE                                                            
         IC    RE,FFTLN                                                         
         SHI   RE,FFTLN1Q+1                                                     
         CHI   RE,L'INVPINV-1                                                   
         BNH   *+8                                                              
         LHI   RE,L'INVPINV-1                                                   
         MVC   BCINVREF(0),FFTDATA                                              
         EX    RE,*-6                                                           
         DROP  RF                                                               
*                                                                               
UPDBAT17 GOTOR AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         MVC   IOKEY,0(RF)         READ DIRECTORY RECORD                        
         GOTOR AIO,IORDUP+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3             SET STATUS AND WRITE BACK                    
         MVC   IOKEY+(TRNKSTA-TRNRECD)(L'TRNKSTA),TRNRSTA-TRNRECD(RF)           
         MVC   IOKEY+(TRNKDA-TRNRECD)(L'TRNKDA),IODA                            
         GOTOR AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ATIA             UPDATE DRAFT TRANSACTION COUNT               
         USING ASKTABD,R1                                                       
         LHI   R0,ASKTMAX          MAXIMUM ASKTAB ENTRIES                       
UPDBAT18 CLI   ASKTULA,0           TEST EMPTY SLOT                              
         BNE   *+10                                                             
         MVC   ASKTULA,IOKEY+(ACTKUNT-ACTRECD)  SET ACCOUNT ENTRY               
         CLC   ASKTULA,IOKEY+(ACTKUNT-ACTRECD)  TEST MATCHING ENTRY             
         BE    UPDBAT20                                                         
         AHI   R1,ASKTABL          BUMP TO NEXT ENTRY/EMPTY SLOT                
         BCT   R0,UPDBAT18                                                      
         DC    H'0'                TOO MANY ACCOUNTS TO PROCESS                 
                                                                                
UPDBAT20 SR    RF,RF               INCREMENT DELETED TRANSACTION COUNT          
         ICM   RF,3,ASKTDEL                                                     
         AHI   RF,1                                                             
         STCM  RF,3,ASKTDEL                                                     
         B     UPDBAT46                                                         
         DROP  R1                                                               
                                                                                
UPDBAT22 TM    UBINDS,UBIUPD       TEST UPDATE                                  
         BZ    UPDBAT46                                                         
         MVC   UBTSAKEY,ASKKEY     SET TSAR RECORD KEY                          
         L     R1,ATSABLK                                                       
         GOTOR VTSAR,(R1)          ADD KEY TO TSAR BUFFER                       
         BE    *+6                                                              
         DC    H'0'                                                             
         B     UPDBAT46                                                         
                                                                                
         USING GINELD,R2                                                        
UPDBAT24 CLI   GINEL,GINELQ                                                     
         BNE   UPDBAT33                                                         
         LA    R1,IOKEY                                                         
         USING GINPASD,R1                                                       
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,CUABIN                                                   
         MVC   GINPINV,GININV                                                   
         TM    UBINDS,UBIDEL       TEST DELETE                                  
         BZ    UPDBAT26                                                         
         MVC   GINPISN,=X'FFFF'    DELETE POSTINGS VIA GIN PASSIVES             
         GOTOR ADELGIN                                                          
         B     UPDBAT46                                                         
                                                                                
UPDBAT26 TM    UBINDS,UBIUPD+UBICLO TEST CLOSE OR UPDATE                        
         BZ    UPDBAT46                                                         
         LHI   R1,IOHI+IOACCDIR+IO3                                             
UPDBAT28 GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOKEY                                                         
         CLC   GINPASD(GINPISN-GINPASD),IOKEYSAV                                
         BNE   UPDBAT46                                                         
         TM    UBINDS,UBICLO       TEST CLOSING THE BATCH                       
         BZ    UPDBAT31                                                         
         CLC   =C'SJ',GINPULA                                                   
         BNE   UPDBAT32                                                         
         MVC   BCWORK(L'CUABIN),CUABIN                                          
         MVC   BCWORK+L'CUABIN(L'GINPULA),GINPULA                               
         B     UPDBAT46                                                         
                                                                                
UPDBAT31 GOTOR AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         MVC   UBTSAKEY,0(RF)      SET TSAR RECORD KEY                          
         L     R1,ATSABLK                                                       
         GOTOR VTSAR,(R1)          ADD KEY TO TSAR BUFFER                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
UPDBAT32 LHI   R1,IOSEQ+IOACCDIR+IO3                                            
         B     UPDBAT28                                                         
         DROP  R2                                                               
                                                                                
         USING BICELD,R2           R2=A(BATCH ITEM CHEQUE ELEMENT)              
UPDBAT33 CLI   BICEL,BICELQ        TEST BATCH ITEM CHEQUE ELEMENT               
         BNE   UPDBAT40                                                         
         TM    UBINDS,UBIUPD       TEST UPDATE                                  
         BZ    UPDBAT46                                                         
         LHI   R0,CHQTMAX                                                       
         L     R1,AIO8             ADD TO/UPDATE CHEQUE TABLE                   
         USING CHQTABD,R1          R1=A(CHEQUE TABLE)                           
UPDBAT34 OC    CHQITEM,CHQITEM     TEST EMPTY SLOT                              
         BZ    UPDBAT38                                                         
         CLC   CHQDATA(CHQKEYL),BICACT TEST KEY MATCH                           
         BNE   UPDBAT36                                                         
         CLC   BCCPYEL+(CPYBANK-CPYELD)(L'CPYBANK),BICACTU  TEST BANK           
         BNE   UPDBAT36                                                         
         AP    BICAMT-BICELD(L'BICAMT,R1),BICAMT            ADD AMOUNT          
         B     UPDBAT46                                                         
UPDBAT36 AHI   R1,CHQTABL                                                       
         BCT   R0,UPDBAT34                                                      
         DC    H'0'                                                             
UPDBAT38 L     RE,AIO1                                                          
         MVC   CHQITEM,TBAKTSEQ-TBARECD(RE)                                     
         MVC   CHQDATA,BCSPACES    SPACE-FILL DATA                              
         SR    RF,RF                                                            
         IC    RF,BICLN            BICEL IS VARIABLE LENGTH                     
         SHI   RF,(BICACT-BICELD)+1                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CHQDATA(0),BICACT   EXTRACT ELEMENT BEYOND BICACT                
         B     UPDBAT46                                                         
                                                                                
         USING BIOELD,R2                                                        
UPDBAT40 CLI   BIOEL,BIOELQ        TEST BATCH ITEM ORDER ELEMENT                
         BNE   UPDBAT46                                                         
         SR    RF,RF                                                            
         IC    RF,UBCBIOSN         CURRENT BIOEL SEQUENCE NUMBER                
         LA    RF,1(RF)                                                         
         STC   RF,UBCBIOSN                                                      
         GOTOR AUPDORD,BODMCB,(UBINDS,BIOELD),UBCBIOSN,AIO1                     
                                                                                
UPDBAT46 SR    R0,R0                                                            
         IC    R0,BIOLN            BUMP TO NEXT ITEM RECORD ELEMENT             
         AR    R2,R0                                                            
         B     UPDBAT16                                                         
                                                                                
UPDBAT48 TM    UBINDS,UBIDEL       TEST DELETE                                  
         BZ    UPDBAT14                                                         
         GOTO1 AORDAUD,BCDMCB,(1,AIO1)                                          
UPDBAT49 L     RF,AIO1                                                          
         OI    TBARESTA-TBARECD(RF),TBAESDEL+TBAESLDE                           
         GOTOR AIO,IOPUTREC+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1                                                          
         MVC   IOKEY,0(RF)         READ DIRECTORY RECORD                        
         GOTOR AIO,IORDUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1             SET STATUS AND WRITE BACK                    
         MVC   IOKEY+(TBAKSTA-TBARECD)(L'TBAKSTA),TBARSTA-TBARECD(RF)           
         MVC   IOKEY+(TBAKDA-TBARECD)(L'TBAKDA),IODA                            
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     UPDBAT14                                                         
                                                                                
UPDBAT50 TM    UBINDS,UBIUPD       TEST UPDATE                                  
         BZ    UPDBAT52                                                         
         GOTOR UPDCHQ                                                           
                                                                                
UPDBAT52 L     R2,AIO2             UPDATE BATCH RECORD                          
         USING TBARECD,R2                                                       
                                                                                
         TM    UBINDS,UBIDEL                                                    
         BZ    *+12                                                             
         MVI   TBARHSTA,TBAHSDEL   SET DELETED                                  
         B     UPDBAT63                                                         
                                                                                
         TM    UBINDS,UBICLO                                                    
         BZ    *+8                                                              
         MVI   TBARHSTA,TBAHSEND   SET CLOSED                                   
                                                                                
         TM    UBINDS,UBIUPD                                                    
         BZ    UPDBAT54                                                         
         MVI   TBARHSTA,TBAHSUPD   SET UPDATED                                  
         MVC   TBAHRUDT,BCTODAYC   SET DATE UPDATED                             
         MVC   TBAHREDT,LSTBEFDT   SET EFFECTIVE DATE (TODAY)                   
                                                                                
UPDBAT54 TM    UBINDS,UBICLO+UBIUPD                                             
         BZ    UPDBAT62                                                         
                                                                                
         LA    R1,TBARFST          LOCATE BATCH HEADER ELEMENT                  
         USING BHDELD,R1                                                        
         SR    R0,R0                                                            
UPDBAT56 CLI   BHDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    R1,R0                                                            
         B     UPDBAT56                                                         
         ST    R1,UBABHDEL         SAVE A(BATCH HEADER ELEMENT)                 
                                                                                
         CLC   BHDITEMA,LSTBITMA   BATCH ITEM COUNT SHOULD MATCH                
         BNE   *+14                                                             
         CLC   BHDDELIT,LSTBDELI   DELETE COUNT SHOULD MATCH ALSO               
         BE    UPDBAT58                                                         
         LA    R1,BASACTH          ABEND IF TOTALS DON'T MATCH                  
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$BTCBR)                                           
                                                                                
UPDBAT57 OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         L     RD,BCSVRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  EXIT TO ROOT CALL POINT                      
                                                                                
UPDBAT58 TM    UBINDS,UBIUPD       TEST UPDATE                                  
         BZ    UPDBAT60                                                         
         TM    CSBIND1,TYPIADVP    TEST ADVANCE PAYMENT BATCH                   
         BZ    UPDBAT60                                                         
         MVC   LSTBITMC,LSTBITMA   YES - FRIG THE CONTROL TOTALS                
         MVC   BHDITEMC,LSTBITMC                                                
                                                                                
UPDBAT60 XC    LSTBAPNO,LSTBAPNO   CLEAR APPROVER                               
         LA    RE,CPYSBAPR                                                      
         CLC   LSTBEFDT,LSTBADDT   TEST REGULAR OR EFFECTIVE DATE BATCH         
         BE    *+8                                                              
         LA    RE,CPYSBAPE                                                      
         EX    RE,*+8              TEST APPROVAL REQUIRED FOR BATCH             
         BZ    UPDBAT62                                                         
         TM    BCCPYST5,0          TEST APPROVING REGULAR BATCHES               
         ICM   R0,7,FVOMTYP        SAVE MESSAGE TYPE/NUMBER                     
         GOTOR ATSTBTY,=AL1(ACTAPR)                                             
         STCM  R0,7,FVOMTYP        RESTORE MESSAGE TYPE/NUMBER                  
         BNE   UPDBAT62                                                         
         TM    UBINDS,UBIUPD       IF UPDATE DON'T SET STATUS                   
         BNZ   *+8                                                              
         OI    TBARHSTA,TBAHSAPR   SET APPROVED IF USER ALLOWED TO              
         L     R1,UBABHDEL                                                      
         MVC   BHDAPRVR,CUPASS     AND SET APPROVER                             
         MVC   LSTBAPNO,BHDAPRVR                                                
         DROP  R1                                                               
                                                                                
UPDBAT62 NI    LSTBINDS,FF-LSTBINPT                                             
         TM    UBINDS,UBIRCL+UBISAV                                             
         BZ    UPDBAT63                                                         
         NI    TBARHSTA,FF-TBAHSEND-TBAHSSAV                                    
         OI    TBARHSTA,TBAHSIIP   SET INPUT IN PROGRESS                        
         TM    TBARHSTA,TBAHSUPD   IS BATCH UPDATED?                            
         BNZ   UPDBAT70            ABEND WITH MESSAGE                           
         OI    LSTBINDS,LSTBINPT   SET INPUT SESSION IN PROGRESS                
         TM    UBINDS,UBISAV                                                    
         BZ    UPDBAT63                                                         
         NI    TBARHSTA,FF-TBAHSIIP                                             
         NI    LSTBINDS,FF-LSTBINPT                                             
         OI    TBARHSTA,TBAHSSAV   SET SAVED                                    
                                                                                
UPDBAT63 MVC   IODAOVER,LSTTDA                                                  
         GOTOR AIO,IORDUPD+IOACCMST+IO1                                         
         BE    *+6                                                              
         DC    H'0'                BATCH RECORD ERROR                           
         GOTOR AIO,IOPUTREC+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY            R2=A(BATCH KEY)                              
         L     R1,AIO2                                                          
         MVC   TBAKEY,0(R1)        READ DIRECTORY RECORD                        
         GOTOR AIO,IORDUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO2             SET STATUS AND WRITE BACK                    
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA                                                      
         MVC   LSTTSTAT,TBARSTA-TBARECD(R1)                                     
         NI    LSTBINDS,FF-(LSTBINFY+LSTBIAUT)                                  
         GOTOR ASETMSK,LSTTABD     SET VALID ACTION MASK FOR RECORD             
         MVC   LSTTDA,IODA                                                      
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    UPDBAT64                                                         
         DC    H'0'                                                             
                                                                                
UPDBAT64 GOTOR ABLDBAP,LSTTABD     READ DIRECTORY PASSIVE POINTER               
         GOTOR AIO,IORDUP+IOACCDIR+IO1                                          
         BE    UPDBAT65            GOT RECORD THE FIRST TIME                    
         TM    LSTBIND2,LSTBADPP   HAVE WE READ RECORD WITH DATE                
         BZ    *+6                 NO - SO WE SHOULD TRY THAT                   
         DC    H'0'                                                             
         OI    LSTBIND2,LSTBADPP   GO AND RE-READ RECORD WITH DATE              
         B     UPDBAT64                                                         
                                                                                
UPDBAT65 L     R1,AIO2             SET STATUS AND WRITE BACK                    
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,LSTTDA                                                    
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    UBINDS,UBIUPD       TEST UPDATE                                  
         BZ    UPDBAT76                                                         
         L     R2,ATSABLK                                                       
         USING TSARD,R2            R2=A(TSAR BLOCK)                             
         XC    TSRNUM,TSRNUM       CLEAR CURRENT RECORD NUMBER                  
         XC    UBTRNACT,UBTRNACT   CLEAR CURRENT ACCOUNT CODE                   
                                                                                
UPDBAT66 SR    R1,R1                                                            
         ICM   R1,3,TSRNUM                                                      
         AHI   R1,1                INCREMENT TSAR RECORD NUMBER                 
         CLM   R1,3,TSPRECN        TEST ALL RECORDS PROCESSED                   
         BH    UPDBAT74                                                         
         STH   R1,TSRNUM                                                        
         MVI   TSACTN,TSAGET       GET NEXT TRANSACTION KEY                     
         GOTOR VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERRORS                            
*                                                                               
         CLC   UBTRNACT,UBTSAKEY   TEST CHANGE OF ACCOUNT CODE                  
         BE    UPDBAT68                                                         
         OC    UBTRNACT,UBTRNACT   TEST FIRST TRANSACTION                       
         BZ    UPDBAT68                                                         
         MVI   TRNINDS,TRNILAST    FLUSH ALL ADDTRN I/O'S                       
         GOTOR VADDTRN,ADDTRND                                                  
*NOP     GOTOR VDMGR,BCPARM,=C'DMUNLK',=C'ACCDIR'                               
*NOP     GOTOR VDMGR,BCPARM,=C'DMUNLK',=C'ACCMST'                               
                                                                                
UPDBAT68 MVC   UBTRNACT,UBTSAKEY   SET CURRENT ACCOUNT CODE                     
         ICM   RF,15,TRNREC                                                     
         MVC   0(TRNKEND,RF),UBTSAKEY                                           
         MVI   TRNINDS,TRNICONV+TRNILIVE                                        
         OI    TRNINDS2,TRNIADDG                                                
         GOTOR VADDTRN,ADDTRND     CALL ADDTRN TO MAKE TRANSACTION LIVE         
         BE    UPDBAT72            NO ERROR                                     
         CLI   FIXIT,C'Y'                                                       
         BE    UPDBAT66            IGNORE ERROR                                 
         CLI   TRNERRS,TRNETRNS    IS ERROR BATCH ALREADY UPDATED?              
         BNE   *+14                NO, DUMP                                     
                                                                                
UPDBAT70 MVC   FVMSGNO,=AL2(AE$BAUPD)                                           
         B     UPDBAT57            YES, PRINT MESSAGE AND UNWIND                
         DC    H'0'                DIE ON OTHER ERRORS                          
                                                                                
UPDBAT72 L     RF,=V(TMSUPD)                                                    
         A     RF,R2RELO                                                        
         GOTOR (RF),BCDMCB,(X'C0',AIO3),ACOM,ATMSUBLK,0 "ADD & UPDATE"          
         B     UPDBAT66                                                         
                                                                                
UPDBAT74 MVI   TRNINDS,TRNICONV+TRNILAST                                        
         OI    TRNINDS2,TRNIUPDG                                                
         GOTOR VADDTRN,ADDTRND     CALL ADDTRN TO WRITE ACCOUNT ETC.            
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERRORS                            
                                                                                
         NI    BCTSINDS,FF-(BCTSIRES)                                           
         TM    BCTSINDS,BCTSIINI   TEST INITIALISED                             
         BZ    UPDBAT76                                                         
         GOTOR ATSARIO,TSARES      RESTORE TSAR BUFFER                          
                                                                                
UPDBAT76 TM    UBINDS,UBIDEL       TEST DELETE                                  
         BZ    UPDBATX                                                          
         LHI   R0,ASKTMAX          MAXIMUM ASKTAB ENTRIES                       
         L     R2,ATIA                                                          
         USING ASKTABD,R2                                                       
         LA    R3,IOKEY                                                         
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
                                                                                
UPDBAT78 CLI   ASKTULA,0           TEST EOT                                     
         BE    UPDBATX                                                          
         MVC   ACTKUNT(L'ASKTULA),ASKTULA                                       
         GOTOR AIO,IOREAD+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         AHI   R1,ACTRFST-ACTRECD                                               
         USING ASTELD,R1                                                        
         SR    RF,RF                                                            
UPDBAT80 CLI   ASTEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ASTEL,ASTELQ                                                     
         BE    *+14                                                             
         IC    RF,ASTLN                                                         
         AR    R1,RF                                                            
         B     UPDBAT80                                                         
         ICM   RF,7,ASTDRAFT       RF=ACCOUNT DRAFT TRANSACTION COUNT           
         SR    RE,RE                                                            
         ICM   RE,3,ASKTDEL        RE=DELETED DRAFT TRANSACTION COUNT           
         SR    RF,RE                                                            
         BNM   *+6                                                              
         SR    RF,RF               ENSURE COUNT HAS NOT BECOME NEGATIVE         
         STCM  RF,7,ASTDRAFT       UPDATE DRAFT TRANSACTION COUNT               
         GOTOR AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         AHI   R2,ASKTABL          BUMP TO NEXT ASKTAB ENTRY                    
         BCT   R0,UPDBAT78                                                      
                                                                                
UPDBATX  J     ROUTE                                                            
         DROP  R1,R2,R3,R4,RB                                                   
                                                                                
R2WORKD  DSECT                     ** ADDHDR S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
UBABHDEL DS    A                   A(BATCH HEADER ELEMENT)                      
UBTRNACT DS    XL(L'ACTKCULA)      CURRENT ACCOUNT BEING UPDATED                
UBTSAREC DS    0X                  TSAR RECORD                                  
UBTSAKEY DS    XL(L'TRNKEY)        TRANSACTION KEY                              
UBTSADTA DS    X                   NULL DATA FOR TSAR                           
UBTSARLN EQU   *-UBTSAREC          LENGTH OF TSAR RECORD                        
UBTBAKEY DS    XL(L'TBAKEY)        SAVED BATCH RECORD KEY                       
UBCHQNAR DS    CL(L'TRNNARR)       SAVED CHEQUE NARRATIVE (TYPIADVP)            
UBCHQDAT DS    CL8                 CHEQUE DATE                                  
UBCHQDEP DS    CL8                 CHEQUE DEPOSIT DATE                          
UBBITMA  DS    XL2                 COUNTER FOR EXTRA CHEQUE ITEMS               
UBINDS   DS    X                   INDICATORS                                   
UBIUPD   EQU   X'80'               ACTION IS UPDATE                             
UBICLO   EQU   X'40'               ACTION IS CLOSE                              
UBIRCL   EQU   X'20'               ACTION IS RECALL                             
UBISAV   EQU   X'10'               ACTION IS SAVE                               
UBIDEL   EQU   X'08'               ACTION IS DELETE                             
UBIITM   EQU   X'04'               ACTION IS AT ITEM LEVEL                      
UBWORK   DS    CL7                                                              
UBCBIOSN DS    XL1                 CURRENT BIOEL SEQUENCE NUMBER                
UBWORKL  EQU   *-R2WORKD                                                        
                                                                                
ASKTABD  DSECT                     ** DSECT COVERS ASKTAB **                    
ASKTULA  DS    CL(L'ACTKULA)       UNIT/LEDGER/ACCOUNT                          
ASKTDEL  DS    XL2                 DELETED DRAFT TRANSACTION COUNT              
ASKTABL  EQU   *-ASKTABD                                                        
ASKTMAX  EQU   TWAMAXRL/ASKTABL    MAXIMUM ASKTAB ENTRIES                       
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS A BATCH ITEM ORDER ELEMENT                                  *         
* NTRY - R1 PARM 1 BYTE 0  INDICATOR                                  *         
*                  BYTE 1-3  A(BATCH ITEM ORDER ELEMENT)              *         
*           PARM 2 BYTE 1-3  A(BATCH ITEM ORDER SEQUENCE)             *         
*           PARM 3 BYTE 1-3  A(BATCH ITEM RECORD)                     *         
***********************************************************************         
                                                                                
         USING TBARECD,R5                                                       
         USING BIOELD,R3                                                        
UPDORD   J     *+12                                                             
         DC    C'*UPDORD*'                                                      
         LR    RB,RF                                                            
         USING UPDORD,RB                                                        
         TM    CSBIND5,TYPIDORU    TEST DON'T UPDATE ORDER RECORDS              
         JO    ROUTE               YES - EXIT WITH CC EQUAL                     
         LM    R3,R5,0(R1)                                                      
         MVC   UOIND1,0(R1)                                                     
         MVC   UOCBIOSN,0(R4)      CURRENT BIOEL SEQUENCE NUMBER                
         XC    UOTYPE,UOTYPE                                                    
*                                                                               
         LA    R2,IOKEY            BUILD ORDER RECORD KEY                       
         USING ORDRECD,R2                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUABIN                                                   
         MVC   ORDKORD,BIOONUM                                                  
         GOTOR AIO,IOREAD+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   UORDA,IODA          SAVE ORDER RECORD DISK ADDRESS               
         USING CPTRBLK,BOWORK1     DELETE ALL PASSIVES                          
         XC    CPTRBLK,CPTRBLK                                                  
         GOTO1 VPADDLE,DMCB,(C'D',AIO3),(C'K',CPTRBLK),0,0,ACOM                 
         L     R2,AIO3                                                          
         MVC   UOSTAT,ORDRSTAT     STORE OLD ORDER STATUS                       
         MVC   UONSTA2,ORDRSTA2                                                 
*                                                                               
UPDORD08 LA    R4,ORDRFST                                                       
         USING ORDELD,R4                                                        
         SR    R0,R0                                                            
UPDORD10 CLI   ORDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                ORDER ELEMENT MISSING                        
         CLI   ORDEL,ORDELQ                                                     
         BE    *+14                                                             
         IC    R0,ORDLN                                                         
         AR    R4,R0                                                            
         B     UPDORD10                                                         
*                                                                               
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),ORDACCU                      
         BNE   UPDORD18                                                         
         OI    UOTYPE,UOTPROD      SET PRODUCTION ORDER                         
         LA    RF,TBARFST                                                       
         USING SFSELD,RF                                                        
*                                                                               
UPDORD12 CLI   SFSEL,0                                                          
         JE    UPDORD18                                                         
         CLI   SFSEL,SFSELQ                                                     
         JE    UPDORD16                                                         
*                                                                               
UPDORD14 LLC   R0,SFSLN                                                         
         AR    RF,R0                                                            
         J     UPDORD12                                                         
*                                                                               
UPDORD16 CLI   SFSFLDN,X'40'       IS THERE A JOB ON THE SCREEN?                
         JNE   *+8                 NO                                           
         OI    UOTYPE,UOTPJOB      YES, SET PRODUCTION W/JOB                    
*                                                                               
         CLI   SFSFLDN,X'44'       IS THERE AN EXPENSE ON THE SCREEN?           
         JNE   UPDORD14            NO                                           
         DROP  RF                                                               
*                                                                               
         TM    UOTYPE,UOTPROD      IS THIS A PRODUCTION ORDER?                  
         JZ    *+12                NO, MUST BE EXPENSE THEN                     
         TM    UOTYPE,UOTPJOB      YES, IS THERE A JOB ON THE SCREEN?           
         BO    UPDORD18            YES, LEAVE AS PRODUCTION                     
         OI    UOTYPE,UOTPEXP      NO JOB, SET IT TO EXPENSE                    
                                                                                
UPDORD18 DS    0H                                                               
*&&US*&& CLI   ORDLN,ORDLN2Q       TEST IF ORDER EL LONG ENOUGH                 
*&&UK*&& CLI   ORDLN,ORDLN3Q                                                    
         BL    UPDORD20                                                         
                                                                                
         TM    ORDSTAT,ORDSPRES    TEST IF A PRESTO ORDER                       
         BZ    *+8                                                              
         OI    UOTYPE,UOTPRES      YES - SET INDICATOR                          
         CLI   ORDLN,ORDLN3Q                                                    
         BL    UPDORD20                                                         
         OI    ORDSTAT2,ORDSSTAT                                                
                                                                                
UPDORD20 GOTOR UPDOAM,0            UPDATE ORDER AMOUNT ELEMENT                  
         OI    ORDRSTA2,ORDSSTAT                                                
                                                                                
UPDORD22 TM    UOTYPE,UOTPRES      TEST PRESTO ORDER                            
         BNO   UPDORD24                                                         
                                                                                
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM    POINTER ELEMENT MAINTENANCE                  
         MVC   RAPCPY,CUABIN                                                    
         MVI   RAPEMU,C'N'                                                      
         MVI   RAPRTYP,RAPKRORD                                                 
         MVC   RAPACOM,ACOM                                                     
         MVC   RAPAREC,AIO3                                                     
         MVC   RAPRDA,UORDA        SET DISK ADDRESS OF ORDER                    
         L     RF,=V(ACRAPPER)                                                  
         A     RF,R2RELO                                                        
         GOTOR (RF),RAPBLK                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
UPDORD24 MVC   UONSTAT,ORDRSTAT     SAVE NEW ORDER STATUS                       
         MVC   UONSTA2,ORDRSTA2                                                 
         GOTOR AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDORD25 GOTO1 VPADDLE,BOPARM,(C'A',AIO3),(C'A',CPTRBLK),UORDA,0,ACOM           
*                                                                               
UPDORD26 TM    UOTYPE,UOTPRES      TEST STATUS CHANGE TO PRESTO ORDER           
         BZ    UPDORD28                                                         
                                                                                
         MVI   RAPACTN,RAPAPTR     PASSIVE POINTER MAINTENANCE                  
         L     RF,=V(ACRAPPER)                                                  
         A     RF,R2RELO                                                        
         GOTOR (RF),RAPBLK                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDORD28 DS    0H                                                               
*&&US                                                                           
         LA    RF,IOKEY                                                         
         TM    ORDKSTAT-ORDRECD(RF),ORDSCON    CONTRACT ORDER?                  
         BZ    *+12                                                             
         GOTOR UPDCNT                                                           
         B     UPDORD70                                                         
*&&                                                                             
         USING TRNRECD,R2                                                       
         LA    R2,IOKEY            BUILD KEY FOR ORDER TRANSACTION              
*&&UK*&& MVC   TRNKCULA,BCWORK                                                  
*&&UK*&& MVC   TRNKCULC,BCWORK+L'TRNKCULA                                       
*&&US*&& MVC   TRNKCULA,ORDJOB                                                  
*&&US*&& MVC   TRNKCULC,ORDSUP                                                  
         MVC   TRNKWORK,=C'**'                                                  
         CLC   TRNKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         BE    *+10                                                             
         MVC   TRNKWORK(L'TRNKWORK+L'TRNKCCPY),BCSPACES                         
         MVC   TRNKDATE,ORDDATE                                                 
         MVC   TRNKREF,BIOONUM                                                  
         MVI   TRNKSBR,0                                                        
                                                                                
         GOTOR AIO,IORDUPD+IOACCDIR+IO3                                         
         BE    *+14                                                             
         CLI   IOERR,IOEDEL        ALLOW DELETES, BUT NO OTHER ERROR            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         GOTOR UPDOAM,1            UPDATE ORDER AMOUNT ELEMENT                  
         GOTOR AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY+TRNKSTAT-TRNRECD(L'TRNKSTAT),TRNRSTAT                      
         GOTOR AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* STCEL UPDATE (BRANDOCEAN AUDIT TRAIL)                                         
*                                                                               
UPDORD70 CLI   UOCBIOSN,1          ONLY DO THIS ONCE                            
         JNE   UPDORDX                                                          
         CLC   UOSTAT,UONSTAT      CHECK THE ORDER MATCHING STATUS              
         JE    UPDORDX                                                          
*                                                                               
UPDORD72 GOTOR UPDAUD                                                           
UPDORDX  J     ROUTE                                                            
***********************************************************************         
* ROUTINE TO UPDATE ORDER AUDIT                                       *         
***********************************************************************         
         SPACE 1                                                                
UPDAUD   NTR1  ,                                                                
         USING BIOELD,R3                                                        
         USING STCELD,R4                                                        
UPDAUD58 LA    R4,UOELEM                                                        
         XC    STCEL(L'UOELEM),STCEL                                            
         MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIORD2                                                  
         MVI   STCOTYP,STCOCHGQ                                                 
         OI    STCOTYP,STCOPSMQ    SET INPUT/POSTMAN AS APPLICATION             
         MVI   STCLN,STCOLN2Q                                                   
         MVC   STCOUSR,CUUSER                                                   
         MVC   STCOPID,CUPASS                                                   
         MVC   STCODTE,BCTODAYP                                                 
         OI    STCOIND3,STCOWSTQ SET WORKFLOW STATUS HAS CHANGED                
         MVI   STCOFRST,STCOAPPD                                                
         TM    UOSTAT,ORDSFMCH   IF ORDER WAS PREVIOUSLY FULLY MATCHED          
         JZ    *+8                                                              
         MVI   STCOFRST,STCOCOMP IT SHOULD BE MARKED AS COMPLETE                
         MVI   STCOTOST,STCOAPPD APPROVED TO STATUS                             
         ZAP   STCOAMT,BCPZERO                                                  
         MVC   STCOCURC,CSCPYCUR AGENCY CURRENCY CODE                           
                                                                                
         XC    BODUB1,BODUB1                                                    
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         AHI   R0,X'0C'                                                         
         STCM  R0,15,BODUB1+4                                                   
         ZAP   STCOTIM,BODUB1                                                   
*&&US*&& AP    STCOTIM,=P'60000'   ADJUST TO REAL (EST) TIME IN                 
                                                                                
         LA    R2,IOKEY            BUILD ORDER RECORD KEY                       
         USING ORDRECD,R2                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUABIN                                                   
         MVC   ORDKORD,BIOONUM                                                  
         MVC   UOORDNUM,BIOONUM                                                 
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         LA    R3,ORDRFST                                                       
         USING ORDELD,R3                                                        
UPDAUD60 CLI   ORDEL,0                                                          
         JE    UPDAUD92                                                         
         CLI   ORDEL,ORDELQ                                                     
         JE    UPDAUD64                                                         
         CLI   ORDEL,AFCELQ                                                     
         JE    UPDAUD68                                                         
         CLI   ORDEL,FFTELQ                                                     
         JE    UPDAUD74                                                         
         CLI   ORDEL,SPAELQ                                                     
         JE    UPDAUD70                                                         
         CLI   ORDEL,SORELQ                                                     
         JE    UPDAUD78                                                         
         CLI   ORDEL,OAMELQ                                                     
         JE    UPDAUD80                                                         
UPDAUD62 LLC   RF,ORDLN                                                         
         AR    R3,RF                                                            
         J     UPDAUD60                                                         
                                                                                
UPDAUD64 MVI   STCOSTAT,STCOIUMA  SET THE DEFAULT OF UNMATCHED                  
         TM    UOIND1,UBIRCL      IF RECALL ALWAYS UNMATCHED                    
         JNZ   UPDAUD65                                                         
         TM    ORDSTAT,ORDSPART   IF ORDER IS NOW PART MATCHED                  
         JZ    *+8                                                              
         MVI   STCOSTAT,STCOIPMA  IT SHOULD BE MARKED AS PART MATCHED           
         TM    ORDRSTAT,ORDSFMCH  IF ORDER IS NOW FULLY MATCHED                 
         JZ    *+12                                                             
         MVI   STCOTOST,STCOCOMP  IT SHOULD BE MARKED AS COMPLETE               
         MVI   STCOSTAT,STCOIFMA  IT SHOULD BE MARKED AS FULLY MATCHED          
UPDAUD65 MVC   STCODATE,ORDDATE                                                 
         TM    ORDRSTAT,ORDGDRCV  GOODS RECIEVED                                
         JZ    *+8                                                              
         OI    STCOSTAT,STCOGDRD  YES                                           
         MVC   STCORQDT,ORDRQBD                                                 
         MVC   STCOSULA,ORDSUPU   COPY SUPPLIER                                 
         TM    UOTYPE,UOTPROD                                                   
         JZ    UPDAUD66                                                         
         MVC   STCOSJAC,ORDACCA   COPY JOB                                      
         J     UPDAUD62                                                         
                                                                                
UPDAUD66 MVC   STCOXULA,ORDACCU   COPY EXPENSE ACCOUNT                          
         J     UPDAUD62                                                         
                                                                                
         USING AFCELD,R3                                                        
UPDAUD68 ZAP   STCOFCAM,AFCAMNT   COPY CURRENCY INFORMATION                     
         MVC   STCOCURC,AFCCURR                                                 
         J     UPDAUD62                                                         
                                                                                
         USING SPAELD,R3                                                        
UPDAUD70 CLI   SPATYPE,SPATDEPT   COPY DEPARTMENT INFORMATION                   
         JNE   UPDAUD72                                                         
         MVC   STCO2DAC,SPAAACT                                                 
         J     UPDAUD62                                                         
                                                                                
UPDAUD72 CLI   SPATYPE,SPATPERS   COPY PERSON INFORMATION                       
         JNE   UPDAUD62                                                         
         MVC   STCO2PAC,SPAAACT                                                 
         J     UPDAUD62                                                         
                                                                                
         USING FFTELD,R3                                                        
UPDAUD74 CLI   FFTTYPE,FFTTEXTY   COPY ETYPE INFORMATION                        
         JNE   UPDAUD76                                                         
                                                                                
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         MVC   STCOETYP(0),FFTDATA                                              
         EX    RF,*-6                                                           
         OC    STCOETYP,BCSPACES                                                
         J     UPDAUD62                                                         
                                                                                
UPDAUD76 CLI   FFTTYPE,FFTTOFFC                                                 
         JNE   UPDAUD62                                                         
         OC    STCO2DAC,STCO2DAC                                                
         JNZ   UPDAUD62                                                         
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         MVC   STCO2DAC(0),FFTDATA                                              
         EX    RF,*-6                                                           
         OC    STCO2DAC,BCSPACES                                                
         J     UPDAUD62                                                         
                                                                                
         USING SORELD,R3                                                        
UPDAUD78 MVC   STCOSJAC,SORAACT   COPY JOB INFORMATION                          
         J     UPDAUD62                                                         
                                                                                
         USING OAMELD,R3                                                        
UPDAUD80 AP    STCOAMT,OAMAMNT    ADD UP TOTAL FOR ORDER                        
         J     UPDAUD62                                                         
                                                                                
         USING AUDRECD,R2                                                       
UPDAUD92 LA    R2,IOKEY           BUILD ORDER AUDIT RECORD                      
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,CUABIN                                                   
         MVI   AUDKAUDT,AUDKORD                                                 
         MVC   AUDKORDN,UOORDNUM                                                
         GOTO1 AIO,IORD+IOACCDIR+IO3                                            
         BNE   UPDAUDX                                                          
         MVC   UOSVKEY,IOKEY       SAVE OFF KEY AND SEQUENCE                    
*                                                                               
UPDAUD94 GOTO1 AIO,IOSEQ+IOACCDIR+IO3 FIND LAST AUDIT RECORD                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   UOSVKEY(AUDKSEQ-AUDRECD),IOKEY                                   
         BNE   UPDAUD96                                                         
         MVC   UOSVKEY,IOKEY                                                    
         B     UPDAUD94                                                         
*                                                                               
UPDAUD96 MVC   IOKEY,UOSVKEY                                                    
         GOTO1 AIO,IORDUPD+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,AUDRLEN        GET CURRENT RECORD LENGTH                    
         JNZ   *+8                                                              
         LHI   R0,AUDRFST+1-AUDRECD                                             
         LLC   R1,STCLN            R1=L'ELEMENT TO BE ADDED                     
         AR    R0,R1               UPDATE RECORD LENGTH                         
                                                                                
         CHI   R0,2000                                                          
         JH    UPDAUD98            NO ROOM ADD A NEW AUDIT RECORD               
*                                                                               
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,BCDMCB,(C'P',=C'ACCMST'),AUDRECD,UOELEM,(RF)              
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOPUTREC+IOACCMST+IO3                                        
         JE    UPDAUDX                                                          
         DC    H'0'                                                             
*                                                                               
UPDAUD98 L     R2,AIO3             CLEAR AIO AREA AND ADD NEW AUDIT             
         LR    R0,R2                                                            
         LHI   R1,2048                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    AUDKEY,AUDKEY                                                    
         MVC   AUDKEY,UOSVKEY                                                   
         MVC   AUDRSTAT,UONSTAT                                                 
         MVC   AUDRSTA2,UONSTA2                                                 
         LLC   RF,AUDKSEQ                                                       
         AHI   RF,1                                                             
         STC   RF,AUDKSEQ                                                       
         LHI   R0,AUDRFST-AUDRECD                                               
         STCM  R0,3,AUDRLEN                                                     
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,BCDMCB,(C'P',=C'ACCMST'),AUDRECD,UOELEM,(RF)              
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOADDREC+IOACCMST+IO3                                        
         JE    *+6                                                              
         DC    H'0'                                                             
UPDAUDX  XIT1  ,                                                                
ACCMST   DC    C'ACCMST  '                                                      
         DROP  R2,R3,R4,R5,RB                                                   
                                                                                
R2WORKD  DSECT                     ** ADDHDR S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
UOBYTE1  DS    X                   WORK BYTE                                    
UOBYTE   DS    XL1                 WORK BYTE                                    
UODUB    DS    D                   DUB                                          
UOORDNUM DS    CL6                 ORDER NUMBER                                 
UOTREF   DS    CL6                 TRX REFERENCE                                
UOTDAT   DS    PL3                 TRX DATE                                     
UOELEM   DS    XL255               ELEMENT                                      
UOSVKEY  DS    XL64                SAVED KEY                                    
UOSTAT   DS    XL1                 OLD ORDER STATUS                             
UONSTAT  DS    XL1                 NEW ORDER STATUS BYTE 1                      
UONSTA2  DS    XL1                 NEW ORDER STATUS BYTE 2                      
UOCBIOSN DS    XL1                 CURRENT BOIEL INDEX NUMBER                   
UOIND1   DS    X                   INDICATOR - 1                                
UOIND2   DS    X                   INDICATOR - 2                                
UOTYPE   DS    X                   ORDER TYPE                                   
UOTPROD  EQU   X'80'               PRODUCTION ORDER                             
UOTPRES  EQU   X'40'               PRESTO ORDER                                 
UOTMISS  EQU   X'20'               MISSING WORKCODE - DUE TO BATCHING           
*                                  WORKCODES NOT ON THE ORIGINAL ORDER          
UOTPEXP  EQU   X'10'               EXPENSE ORDER                                
UOTPJOB  EQU   X'08'               JOB PRESENT                                  
*                                                                               
UORDA    DS    XL4                 ORDER RECORD DISK ADDRESS                    
CFAPPQ   EQU   C'A'                                                             
CGRCVQ   EQU   C'G'                                                             
CFMATQ   EQU   C'F'                                                             
CPMATQ   EQU   C'M'                                                             
                                                                                
* ACRAPPERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRAPPERD                                                      
         PRINT ON                                                               
                                                                                
UOWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE ADDTRN CONTROL BLOCK                                     *         
***********************************************************************         
                                                                                
         USING ADDTRND,R2          R2=A(ADDTRN BLOCK)                           
INIADT   L     R2,AADTBLK          INITIALISE ADDTRN CONTROL BLOCK              
         LA    R0,ADDTRND                                                       
         LHI   R1,TRNBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   TRNCTRY,CUCTRY      COUNTRY                                      
         MVC   TRNCOMF,ACOM        A(COMMON FACILITIES)                         
         MVC   TRNCPYS1,BCCPYST1                                                
         MVC   TRNCPYS2,BCCPYST2                                                
         MVC   TRNCPYS3,BCCPYST3                                                
         MVC   TRNCPYS4,BCCPYST4                                                
         MVC   TRNCPYS5,BCCPYST5                                                
         MVC   TRNCPYS6,BCCPYST6                                                
         MVC   TRNCPYS7,BCCPYST7                                                
         MVC   TRNCPYS8,BCCPYST8                                                
         MVC   TRNCPYS9,BCCPYST9                                                
         MVC   TRNCPYSA,BCCPYSTA                                                
         MVC   TRNGLMOA,BCCPYGLM                                                
         LA    R0,WORKD                                                         
         AHI   R0,PALAREA-WORKD                                                 
         STCM  R0,15,TRNPAL                                                     
         XC    TRNGLBLK,TRNGLBLK                                                
         MVC   TRNREC,AIO3                                                      
         MVC   TRNACC,AIO4                                                      
         MVC   TRNBUK,AIO5                                                      
         MVC   TRNCAC,AIO6                                                      
         MVC   TRNOFA,AIO7                                                      
         LA    R0,BCLDGTAB                                                      
         STCM  R0,15,TRNLDG        A(LEDGER TABLE)                              
         MVI   TRN#LDGS,20         SET NUMBER OF LEDGER TABLE ENTRIES           
         MVI   TRNINDS1,TRNIVDAT   SET DON'T CHECK DATE FLAG                    
         MVC   TRNUDATE,BCTODAYC   SET TODAY'S DATE AS FOUND                    
         J     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY BATCH STATUS                                                *         
* NTRY - BATCH RECORD                                                 *         
***********************************************************************         
                                                                                
DISSTA   J     *+12                                                             
         DC    C'*DISSTA*'                                                      
         LR    RB,RF                                                            
         USING DISSTA,RB                                                        
         L     R2,0(R1)                                                         
         STCM  R2,8,DSOUTL         SAVE L'OUTPUT                                
         STCM  R2,7,DSOUTA+1       SAVE A(OUTPUT)                               
         IC    RF,DSOUTL                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)       CLEAR OUTPUT AREA                            
         MVI   DSINDS,DSILONG      USE LONG OUTPUT EXPRESSIONS                  
         L     R2,4(R1)            R2=A(LSTTAB ENTRY/BATCH RECORD)              
         TM    4(R1),X'80'         TEST BATCH RECORD PASSED                     
         BO    DISSTA02                                                         
         USING LSTTABD,R2                                                       
         MVC   DSTSTAT,LSTTSTAT                                                 
         MVC   DSBHDS1,LSTBHDS1                                                 
         MVC   DSBHDS2,LSTBHDS2                                                 
         MVC   DSBEFDT,LSTBEFDT    SET EFFECTIVE DATE                           
         MVC   DSBADDT,LSTBADDT    SET ADDED DATE                               
         B     DISSTA06                                                         
                                                                                
         USING TBARECD,R2                                                       
DISSTA02 MVC   DSBEFDT,TBAHREDT    SET EFFECTIVE DATE                           
         MVC   DSBADDT,TBAHRADT    SET ADDED DATE                               
         MVC   DSTSTAT,TBARHSTA                                                 
         LA    R2,TBARFST                                                       
         SR    R0,R0                                                            
         USING BHDELD,R2                                                        
DISSTA04 CLI   BHDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    R2,R0                                                            
         B     DISSTA04                                                         
         MVC   DSBHDS1,BHDSTAT1                                                 
         MVC   DSBHDS2,BHDSTAT2                                                 
         DROP  R2                                                               
                                                                                
DISSTA06 LA    R2,DSTEMP                                                        
         TM    DSTSTAT,TBAHSDEL                                                 
         BZ    DISSTA08                                                         
         LHI   R1,L'LC@DELD-1                                                   
         LHI   RF,LC@DELD-TWAD                                                  
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC3DELD-1                                                   
         LHI   RF,LC3DELD-TWAD                                                  
         GOTOR DISMOV                                                           
                                                                                
DISSTA08 TM    DSTSTAT,TBAHSEND                                                 
         BZ    DISSTA10                                                         
         LHI   R1,L'LC@CLSD-1                                                   
         LHI   RF,LC@CLSD-TWAD                                                  
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC3CLSD-1                                                   
         LHI   RF,LC3CLSD-TWAD                                                  
         GOTOR DISMOV                                                           
                                                                                
DISSTA10 TM    DSTSTAT,TBAHSUPD                                                 
         BZ    DISSTA12                                                         
         LHI   R1,L'LC@UPDTD-1                                                  
         LHI   RF,LC@UPDTD-TWAD                                                 
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC3UPDTD-1                                                  
         LHI   RF,LC3UPDTD-TWAD                                                 
         GOTOR DISMOV                                                           
                                                                                
DISSTA12 TM    DSTSTAT,TBAHSIIP                                                 
         BZ    DISSTA14                                                         
         LHI   R1,L'LC@OPEN2-1                                                  
         LHI   RF,LC@OPEN2-TWAD                                                 
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC4OPEN2-1                                                  
         LHI   RF,LC4OPEN2-TWAD                                                 
         GOTOR DISMOV                                                           
                                                                                
DISSTA14 TM    DSTSTAT,TBAHSIAD                                                 
         BZ    DISSTA16                                                         
         LHI   R1,L'LC8INSUP-1                                                  
         LHI   RF,LC8INSUP-TWAD                                                 
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC3INSUP-1                                                  
         LHI   RF,LC3INSUP-TWAD                                                 
         GOTOR DISMOV                                                           
                                                                                
DISSTA16 TM    DSTSTAT,TBAHSSAV                                                 
         BZ    DISSTA18                                                         
         LHI   R1,L'LC@SAVED-1                                                  
         LHI   RF,LC@SAVED-TWAD                                                 
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC3SAVED-1                                                  
         LHI   RF,LC3SAVED-TWAD                                                 
         GOTOR DISMOV                                                           
                                                                                
DISSTA18 TM    DSTSTAT,TBAHSEND                                                 
         BZ    DISSTA22                                                         
         TM    DSTSTAT,TBAHSAPR                                                 
         BZ    DISSTA20                                                         
         LHI   R1,L'LC@APRVD-1                                                  
         LHI   RF,LC@APRVD-TWAD                                                 
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC3APRVD-1                                                  
         LHI   RF,LC3APRVD-TWAD                                                 
         GOTOR DISMOV                                                           
         B     DISSTA22                                                         
                                                                                
DISSTA20 LA    RE,CPYSBAPR                                                      
         CLC   DSBEFDT,DSBADDT                                                  
         BE    *+8                                                              
         LA    RE,CPYSBAPE                                                      
         EX    RE,*+8                                                           
         BZ    DISSTA22                                                         
         TM    BCCPYST5,0                                                       
         LHI   R1,L'LC@UAPR-1                                                   
         LHI   RF,LC@UAPR-TWAD                                                  
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC3UAPR-1                                                   
         LHI   RF,LC3UAPR-TWAD                                                  
         GOTOR DISMOV                                                           
                                                                                
DISSTA22 TM    DSBHDS1,BHDSCOPY                                                 
         BZ    DISSTA24                                                         
         LHI   R1,L'LC@COPY2-1                                                  
         LHI   RF,LC@COPY2-TWAD                                                 
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC3COPY2-1                                                  
         LHI   RF,LC3COPY2-TWAD                                                 
         GOTOR DISMOV                                                           
                                                                                
DISSTA24 TM    DSBHDS1,BHDSREVS                                                 
         BZ    DISSTA26                                                         
         LHI   R1,L'LC@RVRSL-1                                                  
         LHI   RF,LC@RVRSL-TWAD                                                 
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC3RVRSL-1                                                  
         LHI   RF,LC3RVRSL-TWAD                                                 
         GOTOR DISMOV                                                           
                                                                                
DISSTA26 TM    DSBHDS1,BHDSGENE                                                 
         BZ    DISSTA28                                                         
         LHI   R1,L'LC@GENED-1                                                  
         LHI   RF,LC@GENED-TWAD                                                 
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC3GENED-1                                                  
         LHI   RF,LC3GENED-TWAD                                                 
         GOTOR DISMOV                                                           
                                                                                
DISSTA28 TM    DSBHDS2,BHDSGENL                                                 
         BZ    DISSTA30                                                         
         LHI   R1,L'LC@GLUED-1                                                  
         LHI   RF,LC@GLUED-TWAD                                                 
         TM    DSINDS,DSISHRT                                                   
         BZ    *+12                                                             
         LHI   R1,L'LC3GLUED-1                                                  
         LHI   RF,LC3GLUED-TWAD                                                 
         GOTOR DISMOV                                                           
                                                                                
DISSTA30 LA    R1,DSTEMP           TEST IF WE BUILT ANYTHING                    
         CR    R2,R1                                                            
         JNH   ROUTX               NO                                           
                                                                                
DISSTA32 BCTR  R2,0                DROP BACK AND CLEAR LAST CHARACTER           
         CLC   0(L'BCCOMMA,R2),BCCOMMA                                          
         MVI   0(R2),C' '          CLEAR COMMA/CHARACTER                        
         BNE   DISSTA32                                                         
         LR    RF,R2               R2=A(DSTEMP+NN)                              
         SR    RF,R1               R1=A(DSTEMP)                                 
         CLM   RF,1,DSOUTL                                                      
         BNH   DISSTA34                                                         
         TM    DSINDS,DSILONG      TEST USING LONG OUTPUT EXPRESSIONS           
         BZ    DISSTA32                                                         
         MVI   DSINDS,DSISHRT      TRY USING SHORT OUTPUT EXPRESSIONS           
         XC    DSTEMP,DSTEMP       CLEAR WORK AREA                              
         B     DISSTA06                                                         
                                                                                
DISSTA34 L     R1,DSOUTA                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),DSTEMP                                                   
         J     ROUTE                                                            
                                                                                
DISMOV   LA    RF,TWAD(RF)                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
         AR    R2,R1                                                            
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'BCCOMMA,R2),BCCOMMA                                          
         AHI   R2,1+L'BCCOMMA                                                   
         BR    RE                                                               
         DROP  RB                                                               
                                                                                
R2WORKD  DSECT                     ** ADDHDR S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
DSBEFDT  DS    XL2                 BATCH EFFECTIVE DATE                         
DSBADDT  DS    XL2                 BATCH ADDED DATE                             
DSOUTA   DS    A                   A(OUTPUT)                                    
DSOUTL   DS    X                   MAX L'OUTPUT                                 
DSTSTAT  DS    XL(L'TBAKHSTA)      BATCH RECORD KEY STATUS - 1                  
DSBHDS1  DS    XL(L'BHDSTAT1)      BATCH HEADER ELEMENT STATUS - 1              
DSBHDS2  DS    XL(L'BHDSTAT2)      BATCH HEADER ELEMENT STATUS - 2              
DSINDS   DS    X                   INDICATORS                                   
DSILONG  EQU   X'80'               USE LONG OUTPUT EXPRESSIONS                  
DSISHRT  EQU   X'40'               USE SHORT OUTPUT EXPRESSIONS                 
DSTEMP   DS    CL64                WORK AREA                                    
DSWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE BATCH KEY FIELDS AND READ BATCH RECORD          *         
***********************************************************************         
                                                                                
GETBAT   J     *+12                                                             
         DC    C'*GETBAT*'                                                      
         LR    RB,RF                                                            
         USING GETBAT,RB                                                        
         LR    R2,R1                                                            
         MVI   GBFLAG,0                                                         
         CLM   R2,8,BCEFFS         TEST CALLER WANTS SECURITY CHECK             
         BNE   *+8                                                              
         MVI   GBFLAG,GBFTBTSC     TEST BATCH TYPE SECURITY                     
         USING LSTTABD,R2                                                       
         XC    LSTTKEY(LSTTDATL),LSTTKEY                                        
         MVI   FVMINL,1            BATCH REFERENCE                              
         GOTOR AVALBRF,BATREFH                                                  
         JNE   ROUTH                                                            
         MVI   FVMINL,1            BATCH TYPE                                   
         GOTOR AVALBTY,BATTYPH                                                  
         JNE   ROUTH                                                            
         TM    GBFLAG,GBFTBTSC     TEST BATCH TYPE SECURITY REQUIRED            
         BZ    GETBAT01                                                         
         GOTOR ATSTBTY,CSACT                                                    
         JNE   ROUTH                                                            
                                                                                
GETBAT01 CLC   BATTYP,BCWORK                                                    
         BE    *+14                                                             
         MVC   BATTYP,BCWORK                                                    
         OI    BATTYPH+(FVOIND-FVIHDR),FVOXMT                                   
         MVI   FVMINL,1            BATCH MONTH OF SERVICE                       
         GOTOR AVALBMO,BATMOAH     VALIDATE MOS FIELD                           
         JNE   ROUTH                                                            
         GOTOR ATSTBMO,BATMOAH     TEST/SET MOS VALUES                          
         CLI   BATCRDH+(FVILEN-FVIHDR),0                                        
         BE    GETBAT02                                                         
         GOTOR AVALDAT,BATCRDH     VALIDATE CREATED DATE, IF PRESENT            
         JNE   ROUTH                                                            
         MVC   LSTBADDT,BCWORK                                                  
GETBAT02 CLI   BATEFDH+(FVILEN-FVIHDR),0                                        
         BE    GETBAT04                                                         
         GOTOR AVALDAT,BATEFDH     VALIDATE EFFECTIVE DATE, IF PRESENT          
         JNE   ROUTX                                                            
         MVC   LSTBEFDT,BCWORK                                                  
         B     GETBAT06                                                         
GETBAT04 MVC   LSTBEFDT,BCTODAYC   ELSE SET EFFECTIVE TODAY                     
         OC    LSTBADDT,LSTBADDT   TEST CREATED DATE PRESENT                    
         BNZ   GETBAT06                                                         
         MVC   LSTBADDT,BCTODAYC   SET CREATED TODAY                            
                                                                                
GETBAT06 MVC   LSTBBTYP,CSBTYP     SET BATCH TYPE                               
         MVC   LSTBGRUP,CSBGRUP    SET BATCH TYPE GROUP                         
         MVC   LSTBUSER,CUUSER     SET USER-ID                                  
         CLI   CUACCS,C'*'         SET ONE/TWO CHARACTER OFFICE                 
         BNE   *+14                                                             
         MVC   LSTBOFFC(1),CUACCS+1                                             
         B     *+10                                                             
         MVC   LSTBOFFC,CUACCS                                                  
         L     RF,ABLDBAK                                                       
         OC    LSTBADDT,LSTBADDT   TEST CREATED DATE GIVEN                      
         BNZ   *+8                                                              
         L     RF,ABLDBAP                                                       
         GOTOR (RF),LSTTABD        BUILD BATCH HEADER KEY                       
         LA    R3,IOKEY                                                         
         USING TBARECD,R3          R3=A(BATCH HEADER KEY)                       
         XC    TBAKBCHR(TBAKLAST-TBAKBCHR),TBAKBCHR                             
         GOTOR AIO,IOHIUP+IOACCDIR+IO2                                          
         JNE   ROUTH                                                            
         CLC   TBARECD(TBAKGRUP-TBARECD),IOKEYSAV                               
         BE    GETBAT08                                                         
         MVC   FVMSGNO,=AL2(AE$BATNF)                                           
         LA    R0,BATREFH                                                       
         CLI   BATCRDH+(FVILEN-FVIHDR),0                                        
         BE    *+8                                                              
         LA    R0,BATCRDH                                                       
         CLI   BATEFDH+(FVILEN-FVIHDR),0                                        
         BE    *+8                                                              
         LA    R0,BATEFDH                                                       
         ST    R0,FVADDR                                                        
         J     ROUTH                                                            
                                                                                
GETBAT08 CLC   TBARECD(TBAKBCHR-TBARECD),IOKEYSAV                               
         BNE   *+14                                                             
         OC    TBAKTSEQ,TBAKTSEQ                                                
         BZ    GETBAT10                                                         
         LA    R0,BATREFH                                                       
         CLI   BATCRDH+(FVILEN-FVIHDR),0                                        
         BE    *+8                                                              
         LA    R0,BATCRDH                                                       
         CLI   BATEFDH+(FVILEN-FVIHDR),0                                        
         BE    *+8                                                              
         LA    R0,BATEFDH                                                       
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$BATNF)                                           
         J     ROUTH                                                            
                                                                                
GETBAT10 TM    TBAKHSTA,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BZ    *+8                                                              
         OI    LSTBINDS,LSTBIIUP   SET INSTANT UPDATE BATCH                     
         TM    CSBIND3,TYPIMSIC    TEST TYPIMSCR ITEM/CHANGE SUPPORTED          
         BZ    *+8                                                              
         OI    LSTBIND2,LSTBIMSI   SET TYPIMSCR ITEM/CHANGE SUPPORTED           
         TM    TBAHKIND,TBAHIMLT   TEST MULTI ITEM/CHANGE VALID                 
         BZ    *+8                                                              
         OI    LSTBIND2,LSTBIMLT   SET MULTI ITEM/CHANGE VALID                  
         TM    TBAHKIND,TBAHIGDJ   TEST BATCH CREATED OFFLINE                   
         BZ    *+8                                                              
         OI    LSTBINDS,LSTBBCBG   SET BATCH CREATED OFFLINE                    
         TM    CSBIND1,TYPIACRV    TEST BATCH IS AN ACCRUAL REVERSAL            
         BZ    *+8                                                              
         OI    LSTBINDS,LSTBACRV   SET BATCH IS AN ACCRUAL REVERSAL             
         MVC   LSTTSTAT,TBAKHSTA                                                
         MVI   LSTTRTYP,RECBAT                                                  
         MVC   LSTBBCHR,TBAKBCHR                                                
         MVC   LSTBOFFC,TBAKOFFC                                                
         MVC   LSTBADDT,TBAHKADT                                                
         MVC   LSTBEFDT,TBAHKEDT                                                
         MVC   LSTTDA,IODA                                                      
         GOTOR AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,AIO2                                                          
         SR    R0,R0               SET BATCH TOTALS                             
         LA    R1,TBARFST                                                       
         USING BHDELD,R1                                                        
GETBAT12 CLI   BHDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ                                                     
         BE    *+14                                                             
         IC    R0,BHDLN                                                         
         AR    R1,R0                                                            
         B     GETBAT12                                                         
         MVC   LSTBITMC,BHDITEMC   ITEM CONTROL                                 
         MVC   LSTBITMA,BHDITEMA   ITEMS ADDED                                  
         ZAP   LSTBCSHC,BHDCASHC   CASH CONTROL                                 
         ZAP   LSTBCSHA,BHDCASHA   CASH ADDED                                   
         MVC   LSTBIBNO,BHDIBNO    INPUT BATCHER NUMBER                         
         MVC   LSTBNAME,BHDNAME    BATCH NAME                                   
         MVC   LSTBHDS1,BHDSTAT1   BATCH HEADER ELEMENT STATUS - 1              
         MVC   LSTBHDS2,BHDSTAT2                               - 2              
         MVC   LSTBDELI,BHDDELIT   NUMBER OF DELETED ITEMS IN BATCH             
         MVC   LSTBHISN,BHDHISNO   NUMBER OF INPUT SCREENS                      
         CLI   BHDLN,BHDLN2Q                                                    
         BL    *+16                                                             
         ZAP   LSTBTDRS,BHDTOTDR                                                
         ZAP   LSTBTCRS,BHDTOTCR                                                
         MVC   LSTBAPNO,BHDAPRVR                                                
         DROP  R1                                                               
         LA    R1,LSTBBTYP         SET BATCH TYPE                               
         TM    LSTBHDS2,BHDSDETL   TEST DETAIL SCREEN                           
         BNO   *+8                                                              
         ICM   R1,8,=AL1(TYPIDETL) SET DETAIL FLAG                              
         GOTOR AGETBTY             GET BATCH TYPE VALUES                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR ASETMSK,LSTTABD     SET VALID ACTION MASK FOR RECORD             
         LA    R1,LSTBIBNO         SET INPUT BATCHER NAME, IF PRESENT           
         OC    LSTBIBNO,LSTBIBNO                                                
         BNZ   *+8                                                              
         LA    R1,LSTBBCHR         ELSE SET ORIGINAL BATCHER NAME               
         GOTOR AGETPID,(R1)                                                     
         MVC   LSTBPID,BCWORK                                                   
         J     ROUTE                                                            
         DROP  R2,R3,RB                                                         
                                                                                
R2WORKD  DSECT                     ** ADDHDR S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
GBFLAG   DS    X                   FLAG BYTE                                    
GBFTBTSC EQU   X'80'               TEST BATCH TYPE SECURITY                     
GBWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO POST AN ACCRUAL REVERSAL BATCH AT UPDATE                 *         
* NTRY - P1=A(PSTACR PARAMETERS)                                      *         
***********************************************************************         
                                                                                
PSTACR   J     *+12                                                             
         DC    C'*PSTACR*'                                                      
         LR    RB,RF                                                            
         USING PSTACR,RB                                                        
         MVC   PAPARMS(PAPARMSL),0(R1)                                          
         LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3          R3=A(ACCRUAL LSTTAB ENTRY)                   
         GOTOR AADDHDR,BCPARM,0,(0,PAAFFTEL)                                    
         BE    *+6                                                              
         DC    H'0'                CANNOT ADD ACCRUAL BATCH HEADER              
         GOTOR AINIADT             INITIALISE ADDTRN CONTROL BLOCK              
         L     R2,AADTBLK                                                       
         USING ADDTRND,R2          R2=A(ADDTRN BLOCK)                           
         MVC   TRNPUSER,LSTBUSER   USER-ID                                      
         MVC   TRNBMOS,LSTBMOSP    PWOS P'YYMM'                                 
         MVC   TRNEFDT,LSTBEFDT    COMPRESSED BINARY EFFECTIVE DATE             
                                                                                
         GOTOR ABLDBAK,PABATCUR    BUILD KEY FOR ORIGINAL BATCH                 
         MVC   PATBAKEY,IOKEY      SAVE ORIGINAL KEY FOR ITEM READING           
         GOTOR ABLDBAK,LSTTABD     BUILD KEY FOR ACCRUAL REVERSAL               
         LA    R2,IOKEY            READ NEW BATCH HEADER INTO IO1               
         USING TBARECD,R2                                                       
         GOTOR AIO,IORDUP+IOACCMST+IO1                                          
         BE    PSTACR10                                                         
         DC    H'0'                                                             
                                                                                
PSTACR10 CLC   LSTBITMA,PATBAKEY+(TBAKTSEQ-TBARECD)                             
         BE    PSTACR70            ALL ITEMS ACCOUNTED FOR                      
         SR    R1,R1                                                            
         ICM   R1,3,PATBAKEY+(TBAKTSEQ-TBARECD)                                 
         AHI   R1,1                                                             
         STCM  R1,3,PATBAKEY+(TBAKTSEQ-TBARECD)                                 
         LA    R2,IOKEY                                                         
         MVC   TBAKEY,PATBAKEY     SET FIRST/NEXT SOURCE ITEM KEY               
         GOTOR AIO,IOREAD+IOACCMST+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         TM    TBARESTA,TBAESLDE   TEST LOGICALLY DELETED                       
         BNZ   PSTACR10            YES - DROP THIS ITEM                         
         TM    TBARESTA,TBAESORD   TEST ITEM CARRIES ORDER(S)                   
         BZ    *+6                                                              
         DC    H'0'                CANNOT ACCRUE BATCHES WITH ORDERS            
                                                                                
         L     R1,AIO1             R1=A(NEW BATCH HEADER)                       
         MVC   TBAKEY,0(R1)        SET NEW KEY                                  
         ICM   R1,3,PAITESEQ                                                    
         AHI   R1,1                                                             
         STCM  R1,3,PAITESEQ                                                    
         STCM  R1,3,TBAKTSEQ                                                    
         MVI   PAFLAG,0            SET ITEM RECORD NOT FOUND                    
         MVC   IOKEY(L'TBAKEY),TBAKEY                                           
         GOTOR AIO,IORDUPD+IOACCDIR+IO3                                         
         BNE   *+6                                                              
         DC    H'0'                MUST BE NOT FOUND OR DELETED                 
         TM    IOERR,IOERNF        TEST RECORD NOT FOUND                        
         BNZ   PSTACR14                                                         
         TM    IOERR,IOEDEL        TEST RECORD DELETED                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOGETRUP+IOACCMST+IO3                                        
         OI    PAFLAG,PAFITDEL     SET RECORD FOUND AND DELETED                 
         LHI   RE,IOAREA2-WORKD                                                 
         LA    RE,WORKD(RE)        COPY D/A AND WORK AREA INTO IO2              
         LHI   RF,IOAREA3-WORKD                                                 
         LA    RF,WORKD(RF)                                                     
         MVC   0(L'IODA+L'IOWORK,RE),0(RF)                                      
                                                                                
PSTACR14 LA    R4,TBARFST                                                       
         USING BIAELD,R4                                                        
         SR    R0,R0                                                            
PSTACR16 CLI   BIAEL,0             TEST EOR                                     
         BE    PSTACR62                                                         
         CLI   BIAEL,BIAELQ                                                     
         BE    PSTACR20                                                         
         CLI   BIAEL,BTAELQ                                                     
         BE    PSTACR20                                                         
         CLI   BIAEL,BIOELQ                                                     
         BE    PSTACR24                                                         
         CLI   BIAEL,BICELQ                                                     
         BE    PSTACR26                                                         
         CLI   BIAEL,SFSELQ                                                     
         BE    PSTACR28                                                         
         CLI   BIAEL,ASKELQ                                                     
         BE    PSTACR30                                                         
                                                                                
PSTACR18 IC    R0,BIALN                                                         
         AR    R4,R0                                                            
         B     PSTACR16                                                         
                                                                                
PSTACR20 ZAP   BODUB1,BIAAMT                                                    
         MP    BODUB1,=P'-1'                                                    
         ZAP   BIAAMT,BODUB1                                                    
         B     PSTACR18                                                         
                                                                                
         USING BIOELD,R4                                                        
PSTACR24 DC    H'0'                CANNOT SUPPORT ORDERS                        
                                                                                
         USING BICELD,R4                                                        
PSTACR26 ZAP   BODUB1,BICAMT                                                    
         MP    BODUB1,=P'-1'                                                    
         ZAP   BICAMT,BODUB1                                                    
         B     PSTACR18                                                         
                                                                                
         USING SFSELD,R4                                                        
PSTACR28 B     PSTACR18            NO ACTION FOR SFSELS                         
                                                                                
         USING ASKELD,R4                                                        
PSTACR30 LA    R2,IOKEY                                                         
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,ASKKEY                                                    
         GOTOR AIO,IOREAD+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,IOGET+IOACCMST+IO3                                            
         TM    TRNKSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         LHI   R1,IOGET+IOACCARC+IO3                                            
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3             R2=A(TRANSACTION RECORD)                     
*&&US*&& GOTOR AADDMON,TRNKDATE    ADD ONE MONTH (SAME/LAST DAY)                
*&&US*&& MVI   TRNKREF+L'TRNKREF-1,C'R'                                         
         MVI   TRNKSBR,0                                                        
         NI    TRNRSTAT,FF-(TRNSARCH+TRNSDRFT+TRNSREVS)                         
         NI    TRNRSTA2,FF-(TRNSGLUP+TRNSUSED+TRNSPEEL)                         
         XC    TRNRSUSE,TRNRSUSE                                                
         SR    R0,R0                                                            
         LA    R1,TRNRFST                                                       
         USING TRNELD,R1                                                        
         CLI   TRNEL,TRNELQ        TEST TRANSACTION ELEMENT FIRST               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRNDATE,TRNKDATE    REFRESH DATE                                 
         MVC   TRNREF,TRNKREF      REFRESH REFERENCE FROM KEY                   
         MVC   TRNSUB,TRNKSBR      REFRESH SUB-REFERENCE FROM KEY               
         NI    TRNSTAT,FF-(TRNSREV)                                             
         MVC   TRNMOS,LSTBMOSC                                                  
         MVC   TRNBREF,LSTBBREF                                                 
         TM    CSBIND5,TYPIOVTY    TEST OVERLAY SUPPLIED TYPE                   
         BNZ   *+10                LEAVE TRANSACTION TYPE INTACT                
         MVC   TRNTYPE,LSTBBTYP    MAY BE SECOND BATCH TYPE                     
         ZAP   BODUB1,TRNAMNT                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   TRNAMNT,BODUB1      REVERSE TRANSACTION AMOUNT                   
                                                                                
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),TRNKUNT                      
         BNE   PSTACR34                                                         
         CLC   TRNKWORK,=C'99'     TEST PRODUCTION BILL                         
         BNE   PSTACR34                                                         
*&&UK                                                                           
         TM    BCCPYEL+(CPYSTAT5-CPYELD),CPYSNVAT                               
         BO    PSTACR34                                                         
         LHI   R0,13                                                            
*&&                                                                             
*&&US*&& LHI   R0,3                                                             
         LA    RE,TRNNARR+15       REVERSE AMOUNTS HIDDEN IN NARRATIVE          
PSTACR32 ZAP   BODUB1,0(6,RE)                                                   
         MP    BODUB1,=P'-1'                                                    
         ZAP   0(6,RE),BODUB1                                                   
         AHI   RE,6                                                             
         BCT   R0,PSTACR32                                                      
                                                                                
PSTACR34 SR    R0,R0                                                            
         IC    R0,TRNLN                                                         
         AR    R1,R0                                                            
         CLI   TRNEL,0                                                          
         BE    PSTACR60                                                         
         CLI   TRNEL,SCIELQ                                                     
         BE    PSTACR36                                                         
         CLI   TRNEL,MPYELQ                                                     
         BE    PSTACR38                                                         
         CLI   TRNEL,TRSELQ                                                     
         BE    PSTACR40                                                         
         CLI   TRNEL,AFCELQ                                                     
         BE    PSTACR42                                                         
         CLI   TRNEL,MDTELQ                                                     
         BE    PSTACR44                                                         
         CLI   TRNEL,XPYELQ                                                     
         BE    PSTACR48                                                         
*&&US*&& CLI   TRNEL,PRTELQ                                                     
*&&US*&& BE    PSTACR50                                                         
         CLI   TRNEL,UNPELQ                                                     
         BE    PSTACR52                                                         
*&&US*&& CLI   TRNEL,VBIELQ                                                     
*&&US*&& BE    PSTACR54                                                         
*&&US*&& CLI   TRNEL,PBIELQ                                                     
*&&US*&& BE    PSTACR56                                                         
         B     PSTACR34                                                         
                                                                                
         USING SCIELD,R1                                                        
PSTACR36 ZAP   BODUB1,SCIAMNT      REVERSE SUBSIDIARY CASH                      
         MP    BODUB1,=P'-1'                                                    
         ZAP   SCIAMNT,BODUB1      REVERSE SUBSIDIARY CASH AMOUNT               
         CLI   SCILN,SCILN2Q       TEST SECOND AMOUNT HELD                      
         BL    PSTACR34                                                         
         ZAP   BODUB1,SCIADMN      SCIADMN COVERS SECOND AMOUNT                 
         MP    BODUB1,=P'-1'                                                    
         ZAP   SCIADMN,BODUB1      REVERSE SECOND AMOUNT                        
         B     PSTACR34                                                         
                                                                                
         USING MPYELD,R1                                                        
PSTACR38 MVC   MPYNO,BCSPACES                                                   
         XC    MPYDTE,MPYDTE                                                    
         ZAP   MPYAMNT,BCPZERO                                                  
         MVC   MPYBNK,BCSPACES                                                  
         CLI   MPYLN,MPYLN2Q                                                    
         BL    PSTACR34                                                         
         MVI   MPYSUB,0                                                         
         CLI   MPYLN,MPYLN3Q                                                    
         BL    PSTACR34                                                         
         ZAP   MPYPART,BCPZERO                                                  
         B     PSTACR34                                                         
                                                                                
         USING TRSELD,R1                                                        
PSTACR40 L     RE,AADTBLK                                                       
         MVC   TRNBSEQN-ADDTRND(,RE),PAITESEQ                                   
         IC    RF,TRSLN                                                         
         SHI   RF,TRSDATE+1-TRSELD                                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    TRSDATE(0),TRSDATE  CLEAR TRSEL DATA                             
         MVC   TRSDATE,BCTODAYC    SET TODAY'S DATE                             
         OI    TRSSTAT,TRSSREVS    SET 'REVERSE ME'                             
         B     PSTACR34                                                         
                                                                                
         USING AFCELD,R1                                                        
PSTACR42 ZAP   BODUB1,AFCAMNT      REVERSE CURRENCY AMOUNT                      
         MP    BODUB1,=P'-1'                                                    
         ZAP   AFCAMNT,BODUB1                                                   
         B     PSTACR34                                                         
                                                                                
         USING MDTELD,R1                                                        
PSTACR44 LA    RE,MDTGRS           REVERSE ALL MEDIA TRANSFER AMOUNTS           
         LHI   RF,(MDTFDTE-MDTGRS)/L'MDTGRS                                     
PSTACR46 ICM   R0,15,0(RE)                                                      
         BZ    *+12                                                             
         MH    R0,=H'-1'                                                        
         STCM  R0,15,0(RE)                                                      
         AHI   RE,4                                                             
         BCT   RF,PSTACR46                                                      
         ICM   R0,15,MDTVAT                                                     
         MH    R0,=H'-1'                                                        
         STCM  R0,15,MDTVAT                                                     
         B     PSTACR34                                                         
                                                                                
         USING XPYELD,R1                                                        
PSTACR48 ZAP   BODUB1,XPYCD        REVERSE EXTRA PAYMENT CASH DISCOUNT          
         MP    BODUB1,=P'-1'                                                    
         ZAP   XPYCD,BODUB1                                                     
         B     PSTACR34                                                         
*&&US                                                                           
         USING PRTELD,R1                                                        
PSTACR50 ZAP   BODUB1,PRTHOUR      REVERSE HOURS                                
         MP    BODUB1,=P'-1'                                                    
         ZAP   PRTHOUR,BODUB1                                                   
         B     PSTACR34                                                         
*&&                                                                             
         USING UNPELD,R1                                                        
PSTACR52 ZAP   BODUB1,UNPUNIT      REVERSE NUMBER OF UNITS                      
         MP    BODUB1,=P'-1'                                                    
         ZAP   UNPUNIT,BODUB1                                                   
         B     PSTACR34                                                         
                                                                                
         USING VBIELD,R1                                                        
PSTACR54 DS    0H                  NEXT ELEMENT TO REVERSE                      
*&&US                                                                           
         ZAP   BODUB1,VBIVAT       REVERSE VAT AMOUNT                           
         MP    BODUB1,=P'-1'                                                    
         ZAP   VBIVAT,BODUB1                                                    
         ZAP   BODUB1,VBIGROSS     REVERSE GROSS AMOUNT                         
         MP    BODUB1,=P'-1'                                                    
         ZAP   VBIGROSS,BODUB1                                                  
         ZAP   BODUB1,VBICOMM      REVERSE COMM AMOUNT                          
         MP    BODUB1,=P'-1'                                                    
         ZAP   VBICOMM,BODUB1                                                   
*&&                                                                             
         B     PSTACR34                                                         
                                                                                
         USING PBIELD,R1                                                        
PSTACR56 DS    0H                  NEXT ELEMENT TO REVERSE                      
*&&US                                                                           
         ZAP   BODUB1,PBIPST       REVERSE PST AMOUNT                           
         MP    BODUB1,=P'-1'                                                    
         ZAP   PBIPST,BODUB1                                                    
         ZAP   BODUB1,PBIGROSS     REVERSE GROSS AMOUNT                         
         MP    BODUB1,=P'-1'                                                    
         ZAP   PBIGROSS,BODUB1                                                  
         ZAP   BODUB1,PBICOMM      REVERSE COMM AMOUNT                          
         MP    BODUB1,=P'-1'                                                    
         ZAP   PBICOMM,BODUB1                                                   
*&&                                                                             
         B     PSTACR34                                                         
                                                                                
PSTACR58 B     PSTACR34            NEXT ELEMENT TO REVERSE                      
                                                                                
         USING ADDTRND,R1                                                       
PSTACR60 L     R1,AADTBLK                                                       
         MVI   TRNINDS,TRNICONV+TRNIDUCN                                        
         OI    TRNINDS2,TRNIADDG                                                
         OI    TRNTIND1,TRNTIARV                                                
         GOTOR VADDTRN,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ASKKEY,TRNKEY       SET NEW KEY IN ASKEL                         
         B     PSTACR18                                                         
         DROP  R1,R4                                                            
                                                                                
PSTACR62 TM    PAFLAG,PAFITDEL     TEST ITEM FOUND AND DELETED                  
         BZ    PSTACR64                                                         
         GOTOR AIO,IOPUTREC+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         USING TBARECD,R2                                                       
         MVC   IOKEY(L'TBAKEY),TBAKEY                                           
         GOTOR AIO,IORDUPD+IOACCDIR+IO3                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'TBAKEY),TBAKEY                                           
         MVC   IOKEY+(TBAKSTA-TBARECD)(L'TBAKSTA),TBARSTA                       
         MVC   IOKEY+(TBAKDA-TBARECD)(L'TBAKDA),IODA                            
         GOTOR AIO,IOWRITE+IOACCDIR+IO2                                         
         B     PSTACR10                                                         
                                                                                
PSTACR64 GOTOR AIO,IOADDREC+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PSTACR10                                                         
                                                                                
PSTACR70 L     R4,AADTBLK          PROCESSING COMPLETE - LAST TIME CALL         
         USING ADDTRND,R4                                                       
         MVI   TRNINDS,TRNICONV+TRNILAST+TRNIDUCN                               
         OI    TRNINDS2,TRNIUPDG                                                
         GOTOR VADDTRN,ADDTRND     CALL ADDTRN TO WRITE ACCOUNT ETC.            
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERRORS                            
                                                                                
         L     R2,AIO1             R2=A(ACCRUAL REVERSAL BATCH RECORD)          
         USING TBARECD,R2                                                       
         MVI   TBARHSTA,TBAHSUPD   SET UPDATED                                  
         MVC   TBAHRUDT,BCTODAYC   SET DATE UPDATED                             
         LA    R1,TBARFST          UPDATE BATCH HEADER ELEMENT                  
         USING BHDELD,R1                                                        
         SR    R0,R0                                                            
PSTACR72 CLI   BHDEL,0             TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BHDEL,BHDELQ        TEST BATCH HEADER ELEMENT                    
         BE    *+14                                                             
         IC    R0,BHDLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     PSTACR72                                                         
                                                                                
         MVC   BHDITEMC,LSTBITMC   SET CONTROL & INPUT TOTALS                   
         MVC   BHDITEMA,PAITESEQ                                                
         ZAP   BHDCASHA,LSTBCSHA                                                
         ZAP   BHDCASHC,LSTBCSHC                                                
         MVC   BHDAPRVR,PABATCUR+(LSTBAPNO-LSTTABD)                             
         OC    BHDAPRVR,BHDAPRVR   TEST APPROVER SET                            
         BZ    *+8                                                              
         OI    TBARHSTA,TBAHSAPR   YES - SET APPROVED STATUS BIT ON             
         CLI   BHDLN,BHDLN2Q       TEST ACCUMULATING TOTAL DRS/CRS              
         BL    PSTACR74                                                         
         ZAP   BODUB1,LSTBTDRS     REVERSE TOTAL DEBITS                         
         MP    BODUB1,=P'-1'                                                    
         ZAP   BHDTOTDR,BODUB1                                                  
         ZAP   BODUB1,LSTBTCRS     REVERSE TOTAL CREDITS                        
         MP    BODUB1,=P'-1'                                                    
         ZAP   BHDTOTCR,BODUB1                                                  
                                                                                
PSTACR74 GOTOR AIO,IOPUTREC+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY            R2=A(BATCH KEY)                              
         L     R1,AIO1                                                          
         MVC   TBAKEY,0(R1)        READ DIRECTORY RECORD                        
         GOTOR AIO,IORDUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1             SET STATUS AND WRITE BACK                    
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA                                                      
         MVC   LSTTSTAT,TBARSTA-TBARECD(R1)                                     
         GOTOR ASETMSK,LSTTABD     SET VALID ACTION MASK FOR RECORD             
         MVC   LSTTDA,IODA                                                      
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR ABLDBAP,LSTTABD     READ DIRECTORY PASSIVE POINTER               
         GOTOR AIO,IORDUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1             SET STATUS AND WRITE BACK                    
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,LSTTDA                                                    
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         J     ROUTE                                                            
         DROP  R1,R2,R3,R4,RB                                                   
                                                                                
R2WORKD  DSECT                     ** ADDHDR S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
PAPARMS  DS    0A                                                               
PAAFFTEL DS    A                   A(1ST FFTEL IN EXISTING BATCH, AIO2)         
PABATCUR DS    XL(LSTTABL)         SAVED ORIGINAL BATCH LSTTAB ENTRY            
PAPARMSL EQU   *-PAPARMS                                                        
PATBAKEY DS    XL(L'TBAKEY)        SAVED ORIGINAL BATCH KEY                     
PAITESEQ DS    XL(L'TBAKTSEQ)      ACCRUAL BATCH ITEM SEQUENCE#                 
PAFLAG   DS    X                   FLAG BYTE                                    
PAFITDEL EQU   X'01'               ITEM RECORD FOUND AND DELETED                
PAWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET NEXT MONTH.  YEAR IS ADJUSTED IF NECESSARY.  IF A    *         
* NON-ZERO DAY IS PASSED ADDMON CHECKS IT IS VALID FOR THE NEW MONTH  *         
* AND IF INVALID, ADJUSTS IT TO THE LAST DAY OF THE NEW MONTH         *         
*                                                                     *         
* NTRY - R1=A(INPUT/OUTPUT PWOS PL3'YYMMDD' OR PL3'YYMM00')           *         
* EXIT - INPUT/OUTPUT DATE SET TO NEW DATE                            *         
***********************************************************************         
                                                                                
ADDMON   ST    R1,AMAOUT           SAVE A(OUTPUT)                               
         MVC   AMDATE,0(R1)        MOVE DATE INTO W/S                           
         GOTOR VDATCON,DMCB,(1,AMDATE),(0,AMWDATE)                              
         GOTOR VADDAY,DMCB,(C'M',AMWDATE),AMWODATE,1                            
         GOTOR VDATCON,DMCB,(0,AMWODATE),(1,AMDATE)                             
         L     R1,AMAOUT                                                        
         MVC   0(L'AMDATE,R1),AMDATE                                            
         J     ROUTE                                                            
                                                                                
R2WORKD  DSECT                     ** ADDHDR S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
AMAOUT   DS    A                                                                
AMDATE   DS    PL3                 PWOS YMD                                     
AMWDATE  DS    CL6                 EBCIDIC DATE                                 
AMWODATE DS    CL6                 EBCIDIC DATE                                 
AMWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET CURRENCY DETAILS                                     *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' ON TO RETURN TABLE ENTRY                    *         
*                   X'40' ON TO RETURN CURRENCY NAME                  *         
*                   X'20' ON TO RETURN MIN/MAX EXCHNAGE RATES         *         
*             1-3 = A(CURRENCY CODE)                                  *         
*       P2        = A(AREA FOR CURRENCY TABLE ENTRY)                  *         
*       P3        = A(AREA FOR CURRENCY NAME)                         *         
*       P4        = A(AREA FOR MINIMUM EXCHANGE RATE)                 *         
*       P5        = A(AREA FOR MAXIMUM EXCHANGE RATE)                 *         
***********************************************************************         
                                                                                
GETCUR   J     *+12                                                             
         DC    C'*GETCUR*'                                                      
         LR    RB,RF                                                            
         USING GETCUR,RB                                                        
         MVC   GCIPARM(GCIPARML),0(R1)                                          
         XR    RF,RF                                                            
         ICM   RF,7,GCIPACUR                                                    
         XR    R0,R0                                                            
         TM    GCIPINDS,GCIPINAM                                                
         BZ    *+8                                                              
         LHI   R0,X'C0'                                                         
         GOTOR VBLDCUR,GCPARM,(RF),((R0),GCWORK),ACOM                           
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCUR)                                           
         J     ROUTL                                                            
         L     R2,0(R1)            SET MIN/MAX EXCHANGE RATES                   
         USING GCURD,R2                                                         
         CLI   GCREL,GCRELQ                                                     
         BNE   GETCUR02                                                         
         MVC   GCMINEXC,GCRMNEXC                                                
         MVC   GCMAXEXC,GCRMXEXC                                                
         DROP  R2                                                               
                                                                                
GETCUR02 TM    GCIPINDS,GCIPITAB   COPY TABLE ENTRY                             
         BZ    *+14                                                             
         L     RF,GCIPATAB                                                      
         MVC   0(CURTABL,RF),GCWORK                                             
         TM    GCIPINDS,GCIPINAM   COPY LONG NAME                               
         BZ    *+14                                                             
         L     RF,GCIPANAM                                                      
         MVC   0(L'CURTLONG,RF),GCWORK+(CURTSHRT-CURTABD+L'CURTSHRT)            
                                                                                
         TM    GCIPINDS,GCIPIMNX   TEST MIN/MAX EXCHANGE RATES REQUIRED         
         BZ    GETCURX                                                          
         OC    GCMINEXC,GCMINEXC                                                
         BZ    GETCUR06                                                         
         CLC   CSCPYCUR,=C'GBP'    TEST COMPANY CURRENCY = GBP                  
         BE    GETCUR08            YES - MIN/MAX IS CORRECT                     
         GOTOR VBLDCUR,GCPARM,CSCPYCUR,GCWORK,ACOM                              
         L     R2,0(R1)                                                         
         USING GCURD,R2                                                         
         CLI   GCREL,GCRELQ                                                     
         BNE   GETCUR06                                                         
         ZAP   GCPL81,BCPZERO                                                   
         MVO   GCPL81,GCRMNEXC     GCPL81=MIN. COMPANY CURRENCY->GBP            
         ZAP   GCPL82,BCPZERO                                                   
         MVO   GCPL82,GCRMXEXC     GCPL82=MAX. COMPANY CURRENCY->GBP            
                                                                                
         ZAP   GCDUB,BCPZERO       CALCULATE AVERAGE                            
         MVO   GCDUB,GCMINEXC        ALLOWABLE EXCHANGE RATE                    
         CVB   RE,GCDUB                 FOR BILLING CURRENCY->GBP               
         MVO   GCDUB,GCMAXEXC                                                   
         CVB   RF,GCDUB                                                         
         AR    RE,RF                                                            
         SRL   RE,1                                                             
         CVD   RE,GCDUB            GCDUB=AVERAGE RATE                           
                                                                                
         ZAP   GCPL16,GCDUB        MAX. RATE =                                  
         SRP   GCPL16,10,0                     AVERAGE (BILLING->GBP)           
         DP    GCPL16,GCPL81                   / MINIMUM (COMPANY->GBP)         
         MVC   GCMAXEXC,GCPL16                                                  
                                                                                
         ZAP   GCPL16,GCDUB        MIN. RATE =                                  
         SRP   GCPL16,10,0                     AVERAGE (BILLING->GBP)           
         DP    GCPL16,GCPL82                   / MAXIMUM (COMPANY->GBP)         
         MVC   GCMINEXC,GCPL16                                                  
         B     GETCUR08                                                         
         DROP  R2                                                               
                                                                                
GETCUR06 MVC   GCMINEXC,=X'0000000001'                                          
         MVC   GCMAXEXC,=X'9999999999'                                          
                                                                                
GETCUR08 LM    RE,RF,GCIPAMIN                                                   
         MVC   0(L'GCMINEXC,RE),GCMINEXC                                        
         MVC   0(L'GCMAXEXC,RF),GCMAXEXC                                        
                                                                                
GETCURX  J     ROUTE                                                            
         DROP  RB                                                               
                                                                                
R2WORKD  DSECT                     ** ADDHDR S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
GCIPARM  DS    0A                  * INPUT PARAMETERS *                         
GCIPINDS DS    X                   INPUT INDICATOR BYTE                         
GCIPITAB EQU   X'80'               TABLE ENTRY REQUIRED                         
GCIPINAM EQU   X'40'               CURRENCY NAME REQUIRED                       
GCIPIMNX EQU   X'20'               MIN/MAX EXCHANGE RATES REQUIRED              
GCIPACUR DS    AL3                 A(CURRENCY CODE)                             
GCIPATAB DS    A                   A(OUTPUT FOR TABLE ENTRY)                    
GCIPANAM DS    A                   A(OUTPUT FOR CURRENCY NAME)                  
GCIPAMIN DS    A                   A(OUTPUT FOR MINUMUM EXCHANGE RATE)          
GCIPAMAX DS    A                   A(OUTPUT FOR MAXIMUM EXCHANGE RATE)          
GCIPARML EQU   *-GCIPARM                                                        
                                                                                
GCDUB    DS    D                                                                
GCPARM   DS    6A                                                               
GCWORK   DS    XL80                                                             
GCMINEXC DS    PL5                                                              
GCMAXEXC DS    PL5                                                              
GCPL81   DS    PL8                                                              
GCPL82   DS    PL8                                                              
GCPL16   DS    PL16                                                             
GCWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE NUMBER OF HOURS                                 *         
*                                                                     *         
* NTRY:- P1    = A(INPUT FIELD) OR 0 IF AFVAL ALREADY CALLED          *         
*        P2    = A(PRO RATA BLOCK)                                    *         
* EXIT:- BODUB1 CONTAINS NUMBER OF HOURS                              *         
***********************************************************************         
                                                                                
VALHRS   J     *+12                                                             
         DC    C'*VALHRS*'                                                      
         LR    RB,RF                                                            
         USING VALHRS,RB                                                        
         LM    R3,R4,0(R1)                                                      
         USING PRORATAD,R4                                                      
         LTR   R1,R3               TEST AFVAL CALLED                            
         BZ    VALHRS02                                                         
         GOTOR AFVAL                                                            
         JH    ROUTL                                                            
                                                                                
VALHRS02 SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         JZ    ROUTE               NO INPUT - FINE                              
*&&UK                                                                           
         MVC   FVMSGNO,=AL2(AE$NOHAL)                                           
         TM    PG$STAT,PG$CASHA                                                 
         JO    ROUTL               THIS ITEM ALLOCATED IN CASH                  
*&&                                                                             
         MVC   FVMSGNO,=AL2(AE$IVHRS)                                           
         LA    RE,FVIFLD-1(RF)     RE =A(LAST INPUT CHAR)                       
         LA    R2,FVIFLD                                                        
         CLI   FVIFLD,C'-'                                                      
         BNE   VALHRS04                                                         
         BCTR  RF,0                REDUCE COUNT                                 
         STC   RF,FVILEN           RESET INPUT LENGTH                           
         AHI   R2,1                                                             
VALHRS04 CLI   0(RE),C'/'                                                       
         BNE   VALHRS06                                                         
         CLI   VHWORK,0                                                         
         JNE   ROUTL                                                            
         MVI   VHWORK,C'/'                                                      
         STCM  RE,15,VHWORK+2                                                   
         B     VALHRS12                                                         
                                                                                
VALHRS06 CLC   0(1,RE),BCDECMAL                                                 
         BNE   VALHRS08                                                         
         CLI   VHWORK,0                                                         
         JNE   ROUTL                                                            
         MVI   VHWORK,X'FF'                                                     
         B     VALHRS12                                                         
                                                                                
VALHRS08 CLI   0(RE),C' '          SPACE SEP. INTEGER FROM FRACTION             
         BNE   VALHRS10                                                         
         CLI   VHWORK+1,0                                                       
         JNE   ROUTL               MORE THAN 1 SPACE IS INVALID                 
         CLI   VHWORK,C'/'                                                      
         JNE   ROUTL               AND IT MUST PRECEED '/'                      
         MVI   VHWORK+1,X'FF'                                                   
         B     VALHRS12                                                         
                                                                                
VALHRS10 TM    0(RE),X'F0'                                                      
         JNO   ROUTL                                                            
VALHRS12 BCTR  RE,0                                                             
         BCT   RF,VALHRS04                                                      
                                                                                
         CLI   VHWORK,0                                                         
         BE    VALHRS20            NONE - FINE                                  
         ICM   RF,15,VHWORK+2      RF=A(/)                                      
         BZ    VALHRS20            NO FRACTIONAL INPUT                          
         AHI   RF,1                RF=A(DIVISOR)                                
         SR    RE,RE                                                            
         IC    RE,FVILEN           INPUT LENGTH                                 
         LA    RE,0(R2,RE)                                                      
         SR    RE,RF               RE=L'DIVISOR                                 
         JZ    ROUTL               NO DIVISOR INPUT                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  VHDUB3,0(0,RF)      DUB=DIVISOR                                  
         BCTR  RF,0                RF=A(/)                                      
         LR    RE,R2                                                            
         SR    RF,RE               RF=L'INPUT BEFORE '/'                        
         LR    R0,RF                                                            
         AR    RE,RF                                                            
VALHRS14 BCTR  RE,0                                                             
         BCT   R0,*+8                                                           
         B     VALHRS16            FRACTION ONLY INPUT                          
         CLI   0(RE),C' '                                                       
         BNE   VALHRS14                                                         
         AHI   RE,1                RE=A(DIVIDEND)                               
         AHI   R0,1                                                             
         SR    RF,R0               RF=L'DIVIDEND                                
         BCTR  R0,0                                                             
VALHRS16 BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  VHDUB4,0(0,RE)                                                   
         ZAP   VHPL81,=P'0'                                                     
         LTR   RF,R0               RF=REMAINING LHS INPUT                       
         BZ    VALHRS18            NONE - FRACTION ONLY                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  VHPL81,0(0,R2)                                                   
         MP    VHPL81,=P'100'      WHOLE HOURS                                  
VALHRS18 ZAP   VHDUB1(16),VHDUB4                                                
         MP    VHDUB1(16),=P'100'                                               
         DP    VHDUB1(16),VHDUB3                                                
         ZAP   VHDUB2,VHDUB2                                                    
         JNZ   ROUTL               REMAINDER IS ERROR                           
         ZAP   VHDMCB+4(8),VHDUB1                                               
         AP    VHDMCB+4(8),VHPL81  ADD IN THE WHOLE HOURS                       
         CLI   FVIFLD,C'-'         REVERSE SIGN IF APPROPRIATE                  
         BNE   VALHRS22                                                         
         MP    VHDMCB+4(8),=P'-1'                                               
         B     VALHRS22                                                         
                                                                                
VALHRS20 SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         CLI   FVIFLD,C'-'                                                      
         BNE   *+8                                                              
         AHI   RF,1                RESTORE ACTUAL LENGTH                        
         STC   RF,FVILEN                                                        
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         GOTOR VCASHVAL,VHDMCB,(X'80',FVIFLD),(RF)                              
         CLI   0(R1),X'FF'                                                      
         JE    ROUTL               INPUT MUST BE STRAIGHT HOURS                 
                                                                                
VALHRS22 MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         ZAP   VHDUB4,VHDMCB+4(8)  ENSURE RESULT DIVISIBLE BY 5                 
*&&UK*&& DP    VHDUB4,=PL2'05'     (UNITS OF 3 MINUTES)                         
*&&US*&& DP    VHDUB4,=PL2'25'     (UNITS OF 15 MINUTES)                        
         ZAP   VHDUB4+6(2),VHDUB4+6(2)                                          
         JNZ   ROUTL                                                            
                                                                                
         MVC   FVMSGNO,=AL2(AE$OALLC)                                           
         CP    PA$HOURS,=P'0'                                                   
         JE    ROUTL               NO HOURS                                     
         ZAP   BODUB1,VHDMCB+4(8)                                               
         J     ROUTE                                                            
         DROP  R4,RB                                                            
                                                                                
R2WORKD  DSECT                     ** VALHRS S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
VHDUB1   DS    D                                                                
VHDUB2   DS    D                                                                
VHDUB3   DS    D                                                                
VHDUB4   DS    D                                                                
VHPL81   DS    PL8                                                              
VHPL82   DS    PL8                                                              
VHPL16   DS    PL16                                                             
VHDMCB   DS    6F                                                               
VHWORK   DS    XL6                                                              
VHWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CREATE SFS ELEMENTS FROM A TWA                           *         
*                                                                     *         
* NTRY:- P1    = A(FIRST TWA FIELD HEADER)                            *         
*        P2    = A(OUTPUT AREA)                                       *         
* EXIT:- BCFULL CONTAINS ADDRESS OF LAST BYTE OF SIGNIFICANT DATA     *         
***********************************************************************         
                                                                                
SAVFLD   J     *+12                                                             
         DC    C'*SAVFLD*'                                                      
         LR    RB,RF                                                            
         USING SAVFLD,RB                                                        
         L     R2,4(R1)            R2=A(OUTPUT AREA)                            
         USING SFSELD,R2           BUILD SCREEN FIELD SAVE ELEMENTS             
         L     R1,0(R1)                                                         
         SR    R0,R0                                                            
SAVFLD02 TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    SAVFLD06                                                         
         SR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R1)                                             
         AR    RF,R1                                                            
         SHI   RF,L'FVIHDR         RF=A(EXTENDED FIELD HEADER)                  
         CLI   0(RF),0             TEST FIELD NUMBER SET                        
         BE    SAVFLD06                                                         
         CLI   0(RF),PFKFLDNO      TEST PFKEY FIELD                             
         BE    SAVFLD06                                                         
         MVI   SFSEL,SFSELQ        BUILD SCREEN SAVE ELEMENT                    
         MVC   SFSFLDN,0(RF)                                                    
         LA    RE,L'FVIHDR(R1)     RE=A(FIELD DATA)                             
SAVFLD04 BCTR  RF,0                LOCATE END OF FIELD                          
         CR    RF,RE                                                            
         BL    SAVFLD06                                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         B     SAVFLD04                                                         
         SR    RF,RE               RF=L'DATA-1                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SFSFIELD(0),0(RE)                                                
         AHI   RF,SFSLN1Q+1        RF=L'ELEMENT                                 
         STC   RF,SFSLN                                                         
         AR    R2,RF                                                            
SAVFLD06 IC    R0,FVTLEN-FVIHDR(R1) BUMP TO NEXT TWA FIELD                      
         AR    R1,R0                                                            
         CLI   0(R1),0             TEST END OF TWA                              
         BNE   SAVFLD02                                                         
         MVI   0(R2),0             SET END OF OUTPUT AREA                       
         ST    R2,BCFULL                                                        
SAVFLDX  J     ROUTE                                                            
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RESTORE SFS ELEMENT DATA TO TWA                          *         
*                                                                     *         
* NTRY:- P1    = A(FIRST ELEMENT ON ITEM RECORD)                      *         
*        P2    = A(FIRST OUTPUT TWA FIELD HEADER)                     *         
* EXIT:- BCFULL CONTAINS ADDRESS OF FIRST OUTPUT FIELD THAT HAS HAD   *         
*        DATA SET                                                     *         
***********************************************************************         
                                                                                
RESFLD   J     *+12                                                             
         DC    C'*RESFLD*'                                                      
         LR    RB,RF                                                            
         USING RESFLD,RB                                                        
         L     R2,0(R1)                                                         
         USING SFSELD,R2           R2=A(LIST OF ELEMENTS)                       
         XC    BCFULL,BCFULL                                                    
         SR    R0,R0                                                            
RESFLD02 CLI   SFSEL,0             TEST ANY MORE ELEMENTS                       
         BE    RESFLDX                                                          
         CLI   SFSEL,SFSELQ        TEST SCREEN FIELD SAVE ELEMENT               
         BNE   RESFLD10                                                         
         L     R3,4(R1)            R3=A(FIRST TWA OUTPUT FIELD)                 
         LA    RF,OSVALS-1                                                      
         TM    CSBIND8,TYPIXOVL    TEST EXTRA AREA FOR SCREEN                   
         BNO   *+8                                                              
         LA    RF,OSSAVE-1                                                      
         SR    RE,RE                                                            
RESFLD04 ICM   RE,1,FVTLEN-FVIHDR(R3)                                           
         BZ    RESFLD10                                                         
         TM    FVATRB-FVIHDR(R3),FVAXTND                                        
         BZ    RESFLD06                                                         
         LA    RE,0(R3,RE)                                                      
         SHI   RE,L'FVIHDR         RE=A(EXTENDED FIELD HEADER)                  
         CLC   0(L'SFSFLDN,RE),SFSFLDN                                          
         BE    RESFLD08                                                         
         SR    RE,RE                                                            
         IC    RE,FVTLEN-FVIHDR(R3)                                             
RESFLD06 BXLE  R3,RE,RESFLD04                                                   
         DC    H'0'                MUST FIND FIELD FOR ELEMENT                  
                                                                                
RESFLD08 SR    RE,RE                                                            
         IC    RE,FVTLEN-FVIHDR(R3)                                             
         SHI   RE,(L'FVIHDR*2)+1                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R3),L'FVIHDR(R3)                                      
         SR    RF,RF                                                            
         IC    RF,SFSLN                                                         
         SHI   RF,SFSLN1Q+1                                                     
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,R3),SFSFIELD                                          
         AHI   RF,1                                                             
         STC   RF,FVILEN-FVIHDR(R3)                                             
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
         OC    BCFULL,BCFULL       TEST FIRST FIELD ADDRESS SET                 
         BZ    *+12                                                             
         C     R3,BCFULL                                                        
         BH    RESFLD10                                                         
         ST    R3,BCFULL           SAVE A(FIRST OR LOWEST FIELD)                
                                                                                
RESFLD10 IC    R0,SFSLN            BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         B     RESFLD02                                                         
                                                                                
RESFLDX  J     ROUTE                                                            
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RESOLVE VAT CODE                                         *         
*                                                                     *         
* NTRY:- P1    = A(KEY OF CREDITOR ACCOUNT RECORD)                    *         
*        P2    = A(VAT FIELD HEADER OR ZERO)                          *         
* EXIT:- ACVATCOD CONTAINS VAT CODE                                   *         
* NOTE:- THIS ROUTINE ASSUMES THAT A GETACC (OR GETACT) CALL HAS BEEN *         
*        ISSUED ON THE ACCOUNT RECORD AND ACVATCOD MAY THEREFORE HAVE *         
*        ALREADY BEEN SET.                                            *         
***********************************************************************         
                                                                                
*&&US                                                                           
RESVAT   DC    H'0'                                                             
*&&                                                                             
*&&UK                                                                           
RESVAT   J     *+12                                                             
         DC    C'*RESVAT*'                                                      
         LR    RB,RF                                                            
         USING RESVAT,RB                                                        
         TM    BCCPYST5,CPYSNVAT   TEST COMPANY USES NEW VAT RECORD             
         BZ    RESVATX                                                          
         LM    R2,R3,0(R1)         R2=A(CREDITOR A/C), R3=A(TWA HEADER)         
         CLI   ACVATCOD,0          TEST VAT CODE RESOLVED (VIA GETACC)          
         BNE   RESVAT04                                                         
         ICM   R4,15,ACALDG        TEST LEDGER DEFAULT SET                      
         BZ    RESVATX                                                          
         USING LDGTABD,R4                                                       
         CLC   LDGTUL,ACTKUNT-ACTRECD(R2)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LDGTVATC,0          TEST DEFAULT VAT CODE ON LEDGER              
         BE    RESVATX                                                          
         MVC   ACVATCOD,LDGTVATC   SET LEDGER DEFAULT VALUE                     
         CLI   LDGTLVA,L'ACTKACT   TEST ONE LEVEL CREDITOR LEDGER               
         BE    RESVAT04                                                         
         SR    RE,RE                                                            
         IC    RE,LDGTLVA                                                       
         AHI   RE,ACTKACT-ACTRECD-1                                             
         MVC   RVKEYSV,IOKEY                                                    
         MVC   RVDASV,IODA                                                      
         MVC   RVWORKSV,IOWORK                                                  
         MVC   IOKEY(L'ACTKEY),BCSPACES                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   IOKEY(0),0(R2)      BUILD KEY OF HIGH LEVEL CREDITOR A/C         
         LA    R0,RVIOAREA                                                      
         ST    R0,IOADDR                                                        
         GOTOR AIO,IOREAD+IOACCMST                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    IOADDR,IOADDR       ENSURE IOADDR NOT LEFT INITIALIZED           
         MVC   IOKEY,RVKEYSV       AND RESTORE CALLER'S I/O VALUES              
         MVC   IODA,RVDASV                                                      
         MVC   IOWORK,RVWORKSV                                                  
                                                                                
         LA    R1,RVIOAREA         LOCATE VAT CODE ON HIGH LEVEL A/C            
         USING ACTRECD,R1                                                       
         LA    R1,ACTRFST                                                       
         SR    R0,R0                                                            
         USING FFTELD,R1                                                        
RESVAT02 IC    R0,FFTLN                                                         
         AR    R1,R0                                                            
         CLI   FFTEL,0             TEST END OF RECORD                           
         BE    RESVAT04                                                         
         CLI   FFTEL,FFTELQ        TEST FREE FORM TEXT ELEMENT                  
         BNE   RESVAT02                                                         
         CLI   FFTTYPE,FFTTVATC    TEST VAT CODE DATA TYPE                      
         BNE   RESVAT02                                                         
         MVC   ACVATCOD,FFTDATA                                                 
         B     RESVAT04                                                         
                                                                                
RESVAT04 LTR   R3,R3               TEST OUTPUT TWA FIELD SET                    
         BZ    RESVATX                                                          
         SR    RE,RE                                                            
         IC    RE,FVTLEN-FVIHDR(R3)                                             
         TM    FVATRB-FVIHDR(R3),FVAXTND                                        
         BZ    *+8                                                              
         SHI   RE,L'FVIHDR                                                      
         SHI   RE,L'FVIHDR                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R3),L'FVIHDR(R3)                                      
         MVC   L'FVIHDR(L'ACVATCOD,R3),ACVATCOD                                 
         MVI   FVILEN-FVIHDR(R3),L'ACVATCOD                                     
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
                                                                                
RESVATX  J     ROUTE                                                            
         DROP  R1,R4,RB                                                         
*&&                                                                             
                                                                                
R2WORKD  DSECT                     ** ADDHDR S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
RVKEYSV  DS    XL(L'IOKEY)         SAVED KEY AREA                               
RVDASV   DS    XL(L'IODA)          SAVED DISK ADDRESS                           
RVWORKSV DS    XL(L'IOWORK)        SAVED DATAMGR WORK AREA                      
RVIOAREA DS    2000X               TO READ HIGH LEVEL CREDITOR A/C              
RVWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK FOR DUPLICATE INVOICE NUMBER                       *         
*                                                                     *         
* NTRY:- P1    = A(LEDGER CODE)                                       *         
*        P2    = A(SUPPLIER ACCOUNT)                                  *         
*        P3    = A(20 BYTE INVOICE NUMBER)                            *         
*        P4    = A(PWOS INVOICE DATE)                                 *         
* EXIT:- P1    = A(I/O AREA CONTAINING TRANSACTION RECORD - IF FOUND) *         
*        CC    = LOW IF A DRAFT TRANSACTION RECORD FOUND              *         
*              = EQUAL IF NO DUPLICATE INVOICE FOUND                  *         
*              = HIGH IF A LIVE TRANSACTION RECORD FOUND              *         
***********************************************************************         
                                                                                
CHKINV   J     *+12                                                             
         DC    C'*CHKINV*'                                                      
         LR    RB,RF                                                            
         USING CHKINV,RB                                                        
         MVC   CIKEYSV,IOKEY       SAVE CURRENT I/O VALUES                      
         MVC   CIDASV,IODA                                                      
         MVC   CIWORKSV,IOWORK                                                  
         LA    R2,IOKEY                                                         
         USING INVPASD,R2          BUILD KEY OF INVOICE PASSIVE                 
         XC    INVPKEY,INVPKEY                                                  
         MVI   INVPTYP,INVPTYPQ                                                 
         MVC   INVPCPY,CUABIN                                                   
         LM    RE,RF,0(R1)                                                      
         MVC   INVPLDG,0(RE)                                                    
         MVC   INVPACT,0(RF)                                                    
         LM    RE,RF,8(R1)                                                      
         MVC   INVPINV,0(RE)                                                    
         MVC   INVPDAT,0(RF)                                                    
         LA    R0,CIIOAREA         RETURN A(I/O AREA) IN P1 FOR CALLER          
         ST    R0,0(R1)                                                         
         MVI   BCDUB,1             SET NO DUPLICATE FOUND                       
                                                                                
CHKINV02 GOTOR AIO,IOHI+IOACCDIR                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   INVPKEY(INVPDA-INVPKEY),IOKEYSAV                                 
         BNE   CHKINV06                                                         
         LA    R0,CIIOAREA                                                      
         ST    R0,IOADDR                                                        
         GOTOR AIO,IOGET+IOACCMST                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CIIOAREA                                                      
         USING TRNRECD,R2                                                       
         TM    TRNRSTAT,TRNSDELT   TEST THIS RECORD IS DELETED                  
         BNZ   *+16                                                             
         TM    TRNRSTAT,TRNSDRFT   TEST THIS IS A DRAFT TRANSACTION             
         BZ    CHKINV04                                                         
         MVI   BCDUB,0             SET DRAFT DUPLICATE FOUND                    
         LA    R2,IOKEY                                                         
         USING INVPASD,R2          BUILD KEY OF INVOICE PASSIVE                 
         IC    RE,INVPKEY+L'INVPKEY-1                                           
         AHI   RE,1                                                             
         STC   RE,INVPKEY+L'INVPKEY-1                                           
         B     CHKINV02                                                         
                                                                                
CHKINV04 MVI   BCDUB,2             SET LIVE DUPLICATE FOUND                     
                                                                                
CHKINV06 XC    IOADDR,IOADDR       ENSURE IOADDR NOT LEFT INITIALIZED           
         MVC   IOKEY,CIKEYSV       AND RESTORE CALLER'S I/O VALUES              
         MVC   IODA,CIDASV                                                      
         MVC   IOWORK,CIWORKSV                                                  
                                                                                
CKHINVX  J     ROUTCC              SET CONDITION CODE FOR CALLER & EXIT         
         DROP  R2,RB                                                            
                                                                                
R2WORKD  DSECT                     ** ADDHDR S/R LOCAL W/S **                   
         ORG   R2SHARED                                                         
CIKEYSV  DS    XL(L'IOKEY)         SAVED KEY AREA                               
CIDASV   DS    XL(L'IODA)          SAVED DISK ADDRESS                           
CIWORKSV DS    XL(L'IOWORK)        SAVED DATAMGR WORK AREA                      
CIIOAREA DS    2000X               TO READ TRANSACTION RECORD                   
CIWORKL  EQU   *-R2WORKD                                                        
ROUT2    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD A BATCH ITEM ORDER ELEMENT IF REQUIRED                        *         
*                                                                     *         
* NTRY - R2=A(FFN ELEMENT)                                            *         
***********************************************************************         
                                                                                
BLDBIO   NTR1  BASE=*,LABEL=*                                                   
         USING FFNELD,R2                                                        
         LHI   RF,UC@NOORD-TWAD                                                 
         LA    RF,TWAD(RF)                                                      
         CLC   FFNONUM,0(RF)       TEST DUMMY ORDER TRANSACTION                 
         BE    BLDBIOX                                                          
         XC    AIASJXP,AIASJXP     ZERO A(X-JOB AMOUNT SCIEL)                   
         XC    AIAFSDR,AIAFSDR     ZERO A(FIRST SINGLE DEBIT ELEMENT)           
         XC    AIACSHD,AIACSHD     ZERO A(CASH DISCOUNT SCIEL)                  
                                                                                
BLDBIO02 L     R1,AIAFRST                                                       
         SR    R0,R0                                                            
         NI    AIFLAG,X'FF'-AIFFNTRN                                            
         B     *+10                                                             
         USING DLPOSTD,R1                                                       
BLDBIO04 IC    R0,DLPSLEN          BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   DLPSEL,X'FF'        TEST END OF CHUNK                            
         BE    BLDBIOX                                                          
         CLI   DLPSEL,0            TEST END OF RECORD                           
         BE    BLDBIOX                                                          
         CR    R1,R2                                                            
         BNE   BLDBIO07                                                         
         OI    AIFLAG,AIFFNTRN     YES SET FLAG                                 
         B     *+12                                                             
BLDBIO07 CLI   DLPSEL,FFNELQ       TEST SUBSEQUENT FFN ELEMENT                  
         BE    BLDBIOX                                                          
         CLI   DLPSEL,DLPSEDRQ     TEST SINGLE DEBIT ELEMENT                    
         BE    BLDBIO06                                                         
*&&US                                                                           
         USING SCIELD,R1                                                        
         CLI   SCIEL,SCIELQ        IS THIS A SUBSIDIARY ELEMENT ?               
         BNE   BLDBIO04            NO                                           
         CLI   SCITYPE,SCITSJXP    YES, IS IT FOR AN X-JOB ?                    
         BNE   *+12                NO                                           
         ST    R1,AIASJXP          YES, SAVE THE ADDRESS                        
         B     BLDBIO04                                                         
                                                                                
         CLI   SCITYPE,SCITCDSC    YES, IS IT FOR CASH DISCOUNT?                
         BNE   BLDBIO04            NO                                           
         ST    R1,AIACSHD          YES, SAVE THE ADDRESS                        
*&&                                                                             
         B     BLDBIO04                                                         
         DROP  R1                                                               
                                                                                
         USING DLPOSTD,R1                                                       
BLDBIO06 ICM   RF,15,AIAFSDR       SUBSEQUENT SINGLE DEBITS ONLY                
         BNZ   *+12                GENERATE BIOELS IF FOR THE SAME              
         STCM  R1,15,AIAFSDR       UNIT & LEDGER AS THE FIRST ONE               
         B     *+14                                                             
         CLC   DLPSDBU(L'DLPSDBU+L'DLPSDBL),DLPSDBU-DLPOSTD(RF)                 
         BNE   BLDBIO04                                                         
         SR    RF,RF                                                            
         LHI   R0,AIBIOELM         LOCATE SLOT FOR BIO ELEMENT                  
         LA    R3,AIBIOEL                                                       
         USING BIOELD,R3                                                        
BLDBIO08 CLI   BIOEL,BIOELQ        TEST FREE ENTRY                              
         BNE   BLDBIO10                                                         
         IC    RF,BIOLN                                                         
         AR    R3,RF                                                            
         BCT   R0,BLDBIO08                                                      
         DC    H'0'                TOO MANY BATCH ITEM ORDER ELEMENTS           
                                                                                
BLDBIO10 MVI   BIOEL,BIOELQ        SET ELEMENT CODE                             
         MVI   BIOLN,BIOLN2Q       AND LENGTH                                   
         MVC   BIOONUM,FFNONUM                                                  
                                                                                
         MVC   BIOSTAT,FFNSTAT                                                  
         ZAP   BIOAMNT,DLPSAMNT                                                 
*&&US                                                                           
         USING SCIELD,R4                                                        
         ICM   R4,15,AIACSHD       TEST FOR CASH DISCOUNT SCIEL                 
         BZ    *+10                                                             
         AP    BIOAMNT,SCIAMNT     ADD TO GET TRUE AMOUNT FOR ORDER             
         DROP  R4                                                               
*&&                                                                             
         MVC   BIOWORK,DLPSANAL                                                 
*&&US                                                                           
         CLI   FFNLN,FFNLN2Q       SPECIAL?                                     
         BNH   BLDBIO12                                                         
         SR    RE,RE                                                            
         IC    RE,FFNLN                                                         
         SHI   RE,FFNLN2Q+1                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BIORTAMT(0),FFNELD+FFNLN2Q                                       
                                                                                
BLDBIO12 ICM   R4,15,AIASJXP       TEST FOR X-JOB AMOUNT SCIEL                  
         BZ    BLDBIOX             NO                                           
         USING SCIELD,R4                                                        
         ZAP   BIOAMNT,SCIAMNT     YES-REAL AMOUNT IS IN SCIEL                  
         DROP  R4                                                               
*&&                                                                             
*&&UK*&& B     BLDBIO04                                                         
BLDBIOX  J     ROUTE                                                            
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
***********************************************************************         
* CALL ADDTRN TO ADD A DRAFT OR LIVE TRANSACTION                      *         
*                                                                     *         
* NTRY - R1=A(BATCH MONTH TABLE ENTRY)                                *         
***********************************************************************         
                                                                                
         USING BSVALS,R5           R5=A(TWA SAVE AREA)                          
UPDTRN   NTR1  BASE=*,LABEL=*                                                   
         ST    R1,AIABAT           SAVE A(LSTTAB ENTRY)                         
                                                                                
         L     R4,AADTBLK                                                       
         USING ADDTRND,R4          R4=A(ADDTRN BLOCK)                           
         L     R2,TRNREC                                                        
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD KEY)                 
         XC    TRNRLEN,TRNRLEN     CLEAR RECORD LENGTH                          
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3           R3=A(TRANSACTION ELEMENT)                    
                                                                                
         LR    RE,R3                                                            
         LHI   RF,L'AITRNEL                                                     
         LR    R1,RF                                                            
         LA    R0,AITRNEL                                                       
         MVCL  RE,R0                                                            
                                                                                
         TM    CSBIND3,TYPIMANB    TEST MANUAL BILLING NARRATIVE                
         BZ    UPDTRN10                                                         
         SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         LA    RF,TRNELD(RF)                                                    
         SHI   RF,TRNLNBDQ                                                      
         CLC   0(L'TRNDESC,RF),UC@MANBL                                         
         BNE   UPDTRN06                                                         
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),TRNKUNT                      
         BNE   UPDTRN02                                                         
         CLC   =C'99',TRNKWORK     TEST BILL POSTING                            
         BNE   UPDTRN02                                                         
                                                                                
         SR    R1,R1                                                            
         IC    R1,TRNLN            SAVE ORIGINAL LENGTH                         
         MVC   TRNNARR(TRNLNBDQ),0(RF)                                          
         MVI   TRNLN,TRNLNBQ                                                    
         SHI   R1,TRNLNBQ          FIND DIFFERENCE IN LENGTHS                   
         BNP   UPDTRN10            ELEMENT WAS NOT SHORTENED                    
         SR    RE,RE                                                            
         IC    RE,TRNLN            GET NEW LENGTH                               
         LA    RE,TRNELD(RE)       POINT TO END OF ELEMENT                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,RE),0(RE)       CLEAR END OF OLD ELEMENT                     
         B     UPDTRN10                                                         
                                                                                
UPDTRN02 SR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SHI   RF,TRNLNBDQ                                                      
         BCTR  RF,0                DROP TRAILING CHARACTER                      
         STC   RF,TRNLN                                                         
         LA    RF,TRNELD(RF)                                                    
         LHI   RE,TRNLNBDQ                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,RF),0(RF)       CLEAR END OF ELEMENT                         
         B     UPDTRN10                                                         
                                                                                
UPDTRN06 DS    0H                                                               
*&&UK                                                                           
         AHI   RF,TRNLNBDQ-L'TRNDESC                                            
         CLC   0(L'TRNDESC,RF),UC@MANBL                                         
         BNE   UPDTRN10                                                         
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),TRNKUNT                      
         BNE   UPDTRN08                                                         
         MVC   TRNNARR(L'TRNDESC),UC@MANBL                                      
         SR    RF,RF                                                            
         IC    RF,TRNLN            TAKE ORIGINAL LENGTH                         
         SHI   RF,TRNLNDQ+1                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    TRNNARR+L'TRNDESC(0),TRNNARR+L'TRNDESC                           
         MVI   TRNLN,TRNLNDQ                                                    
         B     UPDTRN10                                                         
                                                                                
UPDTRN08 BCTR  RF,0                GO BACK ONE SPACE                            
         XC    0(L'TRNDESC+1,RF),0(RF)                                          
         SR    RF,RF                                                            
         IC    RF,TRNLN            TAKE ORIGINAL LENGTH                         
         SHI   RF,L'TRNDESC+1                                                   
         STC   RF,TRNLN            SET TRANSACTION LENGTH                       
*&&                                                                             
UPDTRN10 SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         LA    RE,TRNELD(RE)                                                    
         USING PIDEL,RE                                                         
         XC    PIDEL(PIDLNQ),PIDEL                                              
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,CUPASS                                                     
         AHI   RE,PIDLNQ                                                        
         MVI   0(RE),0                                                          
         ST    RE,BCFULL           SAVE A(END OF RECORD)                        
         DROP  RE                                                               
                                                                                
         USING FFTELD,RE                                                        
         L     RE,BCFULL                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'TRNKREF                                
         MVI   FFTTYPE,FFTTKREF                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'TRNKREF                                                
         MVC   FFTDATA(L'TRNKREF),TRNKREF                                       
         AHI   RE,FFTLN1Q+L'FFTDLEN+L'TRNKREF                                   
         MVI   0(RE),0                                                          
         DROP  R3,RE                                                            
                                                                                
         ST    RE,BCFULL           SAVE A(END OF RECORD)                        
         ICM   R0,15,AIAFRST       TEST/SET A(FIRST EXTRA ELEMENT)              
         BZ    UPDTRN16                                                         
         ICM   R1,15,AIALAST       SET A(LAST EXTRA ELEMENT)                    
         SR    RF,RF                                                            
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         SR    R1,R0               R1=L'EXTRA ELEMENTS                          
         LA    RF,0(R1,RE)                                                      
         ST    RF,BCFULL           SAVE A(NEW END OF RECORD)                    
         MVI   0(RF),0                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0               MOVE EXTRA ELEMENT(S) TO RECORD              
                                                                                
UPDTRN16 DS    0H                                                               
*&&UK                                                                           
         TM    AIACTI,AIACTISE     TEST SE ACCOUNT INVOLVED                     
         BZ    UPDTRN32                                                         
         CLC   =C'SE',TRNKUNT                                                   
         BE    UPDTRN24                                                         
         CLC   =C'SE',TRNKCUNT                                                  
         BE    UPDTRN24                                                         
                                                                                
         CLC   =C'1C',TRNKUNT                                                   
         BNE   UPDTRN32                                                         
         CLC   =C'11',TRNKCUNT                                                  
         BE    UPDTRN24                                                         
         CLC   =C'12',TRNKCUNT                                                  
         BE    UPDTRN24                                                         
         CLC   =C'13',TRNKCUNT                                                  
         BNE   UPDTRN32                                                         
*&&                                                                             
UPDTRN24 CLC   =C'SJ',TRNKUNT                                                   
         BE    UPDTRN26                                                         
         OC    AIWCODE,AIWCODE     NO WORKCODE, SKIP ELEMENT                    
         BZ    UPDTRN26                                                         
         SR    R1,R1                                                            
         ICM   R1,1,AIWCNUM                                                     
         BZ    UPDTRN26                                                         
         OC    AIAFFTEL,AIAFFTEL                                                
         BNZ   UPDTRN26                                                         
         L     RF,BCFULL                                                        
         USING FFTELD,RF           RF=WORK CODE ELEMENT(S)                      
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTWRKC                                                 
         MVI   FFTSEQ,0                                                         
         MHI   R1,AIWCLNQ                                                       
         STC   R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FFTDATA(0),AIWCODE                                               
         LA    R1,1(R1)                                                         
         AHI   R1,FFTLN1Q+L'FFTDLEN                                             
         STC   R1,FFTLN                                                         
         AR    RF,R1                                                            
         MVI   0(RF),0                                                          
         ST    RF,BCFULL           SAVE A(END OF RECORD)                        
         DROP  RF                                                               
                                                                                
UPDTRN26 L     RF,BCFULL                                                        
         USING APEELD,RF           RF=A(ANALYSIS POINTER ELEMENT)               
         MVI   APEEL,APEELQ                                                     
         MVI   APELN,APELN1Q                                                    
         MVI   APENUM,0                                                         
         LA    R1,AIACTS           R1=A(TABLE OF ANALYSIS ACCOUNTS)             
         SR    R0,R0                                                            
         ICM   R0,1,AIACTSN        R0=NUMBER OF TABLE ENTRIES                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
UPDTRN28 CLC   AIACTSAC-AIACT(L'AIACTSAC,R1),TRNKUNT                            
         BE    UPDTRN30                                                         
         CLC   AIACTSAC-AIACT(L'AIACTSAC,R1),TRNKCUNT                           
         BE    UPDTRN30                                                         
         CLC   AIACTSET-AIACT(L'AIACTSET,R1),AIACTSTN TEST SET NUMBER           
         BNE   UPDTRN30                                                         
         SR    RE,RE                                                            
         IC    RE,APELN                                                         
         LA    RE,APEELD(RE)                                                    
         USING APENTRY,RE          RE=A(APEEL ENTRY)                            
         MVC   APENSTAT,AIACTSTA-AIACT(R1)                                      
         MVC   APENACT,AIACTSAC-AIACT(R1)                                       
         LA    R3,APENACT+L'APENACT-1                                           
         CLI   0(R3),C' '          LOCATE END OF ACCOUNT CODE                   
         BH    *+12                                                             
         MVI   0(R3),0             AND DROP TRAILING SPACES                     
         BCT   R3,*-12                                                          
         AHI   R3,1                                                             
         SR    R3,RE               R3=L'SUB-ELEMENT                             
         STC   R3,APENLEN                                                       
         DROP  RE                                                               
                                                                                
         SR    RE,RE               INCREMENT ELEMENT LENGTH                     
         IC    RE,APELN                                                         
         AR    RE,R3                                                            
         STC   RE,APELN                                                         
         IC    RE,APENUM           INCREMENT NUMBER OF SUB-ELEMENTS             
         AHI   RE,1                                                             
         STC   RE,APENUM                                                        
                                                                                
UPDTRN30 AHI   R1,AIACTSL          BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDTRN28                                                      
         CLI   APELN,APELN1Q       TEST ANY POINTERS IN ELEMENT                 
         BNE   UPDTRN32                                                         
         XC    APEELD(APELN1Q),APEELD                                           
         DROP  RF                                                               
                                                                                
UPDTRN32 MVI   TRNINDS,TRNICONV    SET CONVERTED TRANSACTION RECORD             
         OI    TRNINDS2,TRNIADDG                                                
         L     RE,AIABAT           RE=A(LSTTAB ENTRY)                           
         USING LSTTABD,RE                                                       
         TM    LSTTSTAT,TBAHSIAD   TEST INSTANT UPDATE BATCH                    
         BNZ   *+8                                                              
         OI    TRNINDS,TRNIDRFT    NO - SET TO ADD DRAFT TRANSACTION            
         TM    LSTBHDS1,BHDSACRU   TEST ACCRUAL                                 
         BZ    *+8                                                              
         OI    TRNTIND1,TRNTIACR   SET ACCRUAL                                  
         TM    CSBIND8,TYPIDCSA    TEST DR/CR SUBSIDIARILY ACCUMULATED          
         BZ    UPDTRN34                                                         
         L     R2,TRNREC                                                        
         LA    RF,TRNRFST                                                       
         USING TRNELD,RF                                                        
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+14                                                             
         AP    LSTBTDRS,TRNAMNT                                                 
         B     UPDTRN34                                                         
         AP    LSTBTCRS,TRNAMNT                                                 
         DROP  RE,RF                                                            
                                                                                
UPDTRN34 GOTOR VADDTRN,ADDTRND     CALL ADDTRN TO ADD TRANSACTION               
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERRORS                            
                                                                                
         SR    RE,RE                                                            
         IC    RE,AITRNSN                                                       
         LA    RF,1(RE)                                                         
         CLM   RF,1,=AL1(AITRNSM)  TEST TOO MANY KEYS GENERATED                 
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   RF,AITRNSN                                                       
         MHI   RE,L'AITRNS                                                      
         LA    RE,AITRNS(RE)       RE=A(NEXT AVAILABLE KEY SLOT)                
         MVC   0(L'AITRNS,RE),TRNRECD                                           
         B     UPDTRNX                                                          
                                                                                
UPDTRNX  J     ROUTE                                                            
         DROP  R2,R4,RB                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A BATCH ITEM CHEQUE ELEMENT                        *         
*                                                                     *         
* NTRY - R1=A(TRANSACTION ELEMENT)                                    *         
*        R2=A(POSTING ELEMENT)                                        *         
***********************************************************************         
                                                                                
BLDBIC   NTR1  BASE=*,LABEL=*                                                   
         USING DLPOSTD,R2          R2=A(POSTING ELEMENT)                        
         LA    R3,AIBICEL1         POINT TO FIRST ELEMENT                       
         OC    0(L'AIBICEL1,R3),0(R3)                                           
         BZ    *+8                                                              
         LA    R3,AIBICEL2         USE SECOND IF FIRST IN USE                   
         USING BICELD,R3           R3=A(BATCH CHEQUE ELEMENT)                   
         LR    R4,R1                                                            
         USING TRNELD,R4           R4=A(TRANSACTION ELEMENT)                    
         XC    BICELD(L'AIBICEL1),BICELD                                        
         MVI   BICEL,BICELQ                                                     
         MVC   BICCNO,TRNREF                                                    
         MVC   BICDAT,TRNDATE                                                   
         MVC   BICSTA,TRNSTAT                                                   
                                                                                
         TM    CSBIND1,TYPIADVP    SKIP FOLLOWING IF ADVANCE PAYEMNT            
         BNZ   BLDBIC04                                                         
         MVC   BICDAT,BCTODAYP                                                  
         GOTOR VDATVAL,BCDMCB,(0,TRNNARR+27),AIWORK                             
         OC    0(4,R1),0(R1)                                                    
         BZ    BLDBIC02                                                         
         GOTOR VDATCON,BCDMCB,(0,AIWORK),(1,BICDAT)                             
                                                                                
BLDBIC02 MVC   BICDEP,BCTODAYP                                                  
         GOTOR VDATVAL,BCDMCB,(0,TRNNARR+48),AIWORK                             
         OC    0(4,R1),0(R1)                                                    
         BZ    BLDBIC04                                                         
         GOTOR VDATCON,BCDMCB,(0,AIWORK),(1,BICDEP)                             
         MVC   BICCNO,TRNNARR+14                                                
                                                                                
BLDBIC04 L     RF,AADTBLK          EXTRACT KEY VALUES                           
         MVC   BICCACN,TRNCACNM-ADDTRND(RF)                                     
         ICM   RF,15,TRNREC-ADDTRND(RF)                                         
         USING TRNRECD,RF                                                       
         MVC   BICACT,TRNKCULA                                                  
         MVC   BICCAC,TRNKCULC                                                  
         DROP  RF                                                               
                                                                                
         MVC   BICOFF,TRNOFFC                                                   
         ZAP   BICAMT,TRNAMNT                                                   
         LA    R1,BICCACN+L'BICCACN-1                                           
         CLI   0(R1),C' '          CALCULATE ELEMENT LENGTH                     
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,BICEL                                                         
         SR    R1,R0                                                            
         AHI   R1,1                                                             
         STC   R1,BICLN                                                         
                                                                                
BLDBICX  J     ROUTE                                                            
         DROP  R2,R3,R4,RB                                                      
         EJECT                                                                  
***********************************************************************         
* BUILD A TRANSACTION FOR EACH CHQTAB ENTRY AND CALL ADDTRN TO UPDATE *         
*                                                                     *         
* NTRY - R4=A(BATCH TABLE ENTRY)                                      *         
***********************************************************************         
                                                                                
UPDCHQ   NTR1  BASE=*,LABEL=*                                                   
         USING LSTTABD,R4          R4=A(BATCH TABLE ENTRY)                      
         LHI   R0,CHQTMAX          R0=MAXIMUM CHEQUE TABLE ENTRIES              
         L     R2,AIO8                                                          
         USING CHQTABD,R2          R2=A(CHEQUE TABLE)                           
         MVC   UBBITMA,LSTBITMA    TAKE PRESENT HIGH ITEM NUMBER                
                                                                                
UPDCHQ02 L     R3,AADTBLK                                                       
         USING ADDTRND,R3          R3=A(ADDTRN BLOCK)                           
         OC    CHQITEM,CHQITEM     TEST NO MORE ENTRIES                         
         BZ    UPDCHQX                                                          
         XC    UBCHQNAR,UBCHQNAR   CLEAR NARRATIVE                              
         TM    CSBIND5,TYPIOVTY    TEST OVERLAY SET TRANSACTION TYPE            
         BZ    *+6                                                              
         DC    H'0'                TYPIOVTY CANNOT SUPPORT BICELS               
         GOTOR ABLDBAK,LSTTABD                                                  
         LA    R1,IOKEY                                                         
         MVC   TBAKTSEQ-TBARECD(L'TBAKTSEQ,R1),CHQITEM                          
         GOTOR AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         AHI   R1,TBARFST-TBARECD                                               
         SR    RE,RE                                                            
         USING ASKELD,R1                                                        
UPDCHQ04 IC    RE,ASKLN                                                         
         AR    R1,RE                                                            
         CLI   ASKEL,0                                                          
         BE    UPDCHQ06                                                         
         CLI   ASKEL,ASKELQ                                                     
         BNE   UPDCHQ04                                                         
         MVC   IOKEY,ASKKEY                                                     
         GOTOR AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         AHI   R1,TRNRFST-TRNRECD                                               
         USING TRNELD,R1                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    RE,TRNLN                                                         
         SHI   RE,TRNLN1Q+1                                                     
         XC    UBCHQNAR,UBCHQNAR                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   UBCHQNAR(0),TRNNARR                                              
         MVC   UBCHQDAT,BCSPACES                                                
         MVC   UBCHQDEP,BCSPACES                                                
                                                                                
         USING BICELD,R2           CHEQUE ENTRIES MAP TO BICACT                 
UPDCHQ06 GOTOR VDATCON,BCPARM,(1,BICDAT),(8,UBCHQDAT)                           
         GOTOR (RF),(R1),(1,BICDEP),(8,UBCHQDEP)                                
         MVC   TRNCACNM,BICCACN    CONTRA ACCOUNT NAME (SPACE-FILLED)           
         L     RF,TRNREC                                                        
         USING TRNRECD,RF          RF=A(TRANSACTION RECORD)                     
         MVC   TRNKCULA,BICACT     KEY ACCOUNT                                  
         MVC   TRNKOFF,BCSPACES    SPACES IN OFFICE/WORKCODE                    
         MVC   TRNKCULC,BICCAC     KEY CONTRA-ACCOUNT                           
         MVC   TRNKDATE,BICDEP     KEY DATE (DEPOSIT DATE)                      
         TM    CSBIND1,TYPIADVP    TEST ADVANCE PAYMENT MERGING                 
         BNO   *+10                                                             
         MVC   TRNKDATE,BICDAT     KEY DATE (CHEQUE DATE)                       
         MVC   TRNKREF,BICCNO      KEY REFERENCE                                
         MVI   TRNKSBR,0           KEY SUB-REFERENCE                            
         XC    TRNRSTA,TRNRSTA     CLEAR STATUS AREA                            
                                                                                
         LA    R1,TRNRFST          R1=A(FIRST ELEMENT)                          
         USING TRNELD,R1                                                        
         XC    TRNELD(256),TRNELD  CLEAR SOME SPACE                             
         MVI   TRNEL,TRNELQ        BUILD TRANSACTION                            
         MVC   TRNDATE,TRNKDATE    DATE                                         
         MVC   TRNREF,TRNKREF      REFERENCE                                    
         MVC   TRNSUB,TRNKSBR      SUB-REFERENCE                                
         MVC   TRNTYPE,LSTBBTYP    BATCH TYPE                                   
         MVI   TRNSTAT,TRNSDR      ASSUME DEBIT                                 
         TM    CSBIND1,TYPIADVP    TEST ADVANCE PAYMENT MERGING                 
         BNO   *+10                                                             
         MVC   TRNSTAT,BICSTA      USE STATUS FROM BICEL                        
         MVC   TRNMOS,LSTBMOSC     CHARACTER MOS                                
         MVC   TRNBREF,LSTBBREF    BATCH REFERENCE                              
         ZAP   TRNAMNT,BICAMT      AMOUNT                                       
         MVC   TRNOFFC,BICOFF      OFFICE/WORKCODE                              
         MVC   TRNNARR,UBCHQNAR                                                 
         TM    CSBIND1,TYPIADVP    TEST ADVANCE PAYMENT MERGING                 
         BO    UPDCHQ08                                                         
*&&US                                                                           
         MVC   TRNNARR+13(L'BICCNO),BICCNO                                      
         MVC   TRNNARR+26(L'UBCHQDAT),UBCHQDAT                                  
*&&                                                                             
*&&UK                                                                           
         MVC   TRNNARR+14(L'BICCNO),BICCNO                                      
         MVC   TRNNARR+27(L'UBCHQDAT),UBCHQDAT                                  
*&&                                                                             
         MVC   TRNNARR+48(L'UBCHQDEP),UBCHQDEP                                  
         DROP  RF                                                               
                                                                                
UPDCHQ08 LA    RE,TRNEL+TRNLN1Q+L'TRNNARR                                       
         CLI   0(RE),0                                                          
         BH    *+8                 RE=A(LAST CHARACTER OF TRNEL)                
         BCT   RE,*-8                                                           
         AHI   RE,1                                                             
         SR    RE,R1               RE=L'TRNEL                                   
         STC   RE,TRNLN            TRANSACTION LENGTH                           
                                                                                
         MVI   TRNINDS,TRNICONV                                                 
         TM    LSTBHDS1,BHDSACRU                                                
         BZ    *+8                                                              
         OI    TRNTIND1,TRNTIACR   SET ACCRUAL                                  
         OI    TRNINDS2,TRNIADDG                                                
         GOTOR VADDTRN,ADDTRND                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR ABLDBAK,LSTTABD     BUILD BATCH HEADER KEY                       
         LA    RF,IOKEY                                                         
         USING TBAKEY,RF                                                        
         ICM   R1,3,UBBITMA        TAKE CURRENT HIGH ITEM NUMBER                
         AHI   R1,1                                                             
         STCM  R1,3,UBBITMA                                                     
         MVC   TBAKTSEQ,UBBITMA    SET ITEM NUMBER IN KEY                       
         L     RF,AIO1                                                          
         MVC   TBAKEY,IOKEY        SET BATCH ITEM KEY                           
         XC    TBARSTA(TBARFST-TBARSTA),TBARSTA                                 
         MVI   TBARESTA,TBAESAUT   SET 'AUTO GENERATED' ITEM                    
                                                                                
         L     R1,TRNREC           R1=A(TRANSACTION RECORD)                     
         AHI   R1,TRNRFST-TRNRECD                                               
         USING TRNELD,R1           R1=A(TRANSACTION ELEMENT WE ADDED)           
         LA    R3,TBARFST                                                       
         USING BIAELD,R3           BUILD TOTAL AMOUNT ELEMENT                   
         XC    BIAELD(BIALNQ),BIAELD                                            
         MVI   BIAEL,BTAELQ        NOTE - SPECIAL ELEMENT CODE                  
         MVI   BIALN,BIALNQ                                                     
         ZAP   BIAAMT,TRNAMNT                                                   
         MVC   BIAREF,TRNREF                                                    
         SR    RE,RE                                                            
         IC    RE,BIALN                                                         
         AR    R3,RE               BUMP TO NEXT ELEMENT                         
                                                                                
         USING ASKELD,R3                                                        
         MVI   ASKEL,ASKELQ        BUILD SYSTEM KEY ELEMENT                     
         MVI   ASKLN,ASKLNQ                                                     
         MVI   ASKSEQN,1                                                        
         L     R1,AADTBLK                                                       
         ICM   R1,15,TRNREC-ADDTRND(R1)                                         
         MVC   ASKKEY,0(R1)        SET KEY FROM TRANSACTION RECORD              
         MVI   ASKLNQ(R3),0        SET EOR                                      
         AHI   R3,ASKLNQ+1                                                      
         L     R1,AIO1                                                          
         SR    R3,R1                                                            
         STCM  R3,3,TBARLEN        SET ITEM RECORD LENGTH                       
                                                                                
         GOTOR AIO,IORDUPD+IOACCDIR+IO1                                         
         BNE   *+6                                                              
         DC    H'0'                RECORD MUST NOT EXIST UNDELETED              
         TM    IOERR,IOERNF                                                     
         BZ    UPDCHQ10                                                         
         GOTOR AIO,IOADDREC+IOACCMST+IO1                                        
         B     UPDCHQ12                                                         
                                                                                
UPDCHQ10 TM    IOERR,IOEDEL        RECORD EXISTS - CHECK DELETED                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOPUTREC+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1             BUILD DIRECTORY KEY                          
         LA    RF,IOKEY                                                         
         MVC   TBAKSTA,TBARSTA-TBARECD(R1)                                      
         MVC   TBAKDA,IODA                                                      
         DROP  RF                                                               
         GOTOR AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
UPDCHQ12 SR    R1,R1               INCREMENT BATCH ITEM COUNT                   
         ICM   R1,3,LSTBITMA                                                    
         AHI   R1,1                                                             
         STCM  R1,3,LSTBITMA                                                    
                                                                                
         AHI   R2,CHQTABL          BUMP TO NEXT CHEQUE TABLE ENTRY              
         BCT   R0,UPDCHQ02                                                      
                                                                                
UPDCHQX  J     ROUTE                                                            
         DROP  R1,R2,R3,R4,RB                                                   
         EJECT                                                                  
***********************************************************************         
* UPDATE ORDER AMOUNT ELEMENT IN ORDER RECORD/TRANSACTION             *         
* NTRY - R1=0 IF ORDER RECORD PASSED                                  *         
*        R1=1 IF ORDER TRANSACTION PASSED                             *         
*        R3=A(BATCH ITEM ORDER ELEMENT)                               *         
*        ORDER RECORD/TRANSACTION IN IOAREA 3                         *         
*        BATCH HEADER RECORD IN IOAREA 2                              *         
***********************************************************************         
                                                                                
         USING ORDRECD,R5                                                       
         USING BIOELD,R3                                                        
UPDOAM   NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1                                                            
         L     R5,AIO3                                                          
         LA    R1,ORDRFST                                                       
         SR    R0,R0                                                            
         USING OAMELD,R1                                                        
         TM    ORDRSTAT,ORDSCON    CONTRACT ORDER                               
         BZ    *+12                                                             
         CLI   CSACT,ACTCLO        CLOSING BATCH W/ CONTRACT ORDER?             
         BE    UPDOAMX             NOT ALLOWED TO DO THAT, LEAVE                
         TM    UONSTA2,ORDSEXEX    DO THIS ONLY FOR BRANDO ORDERS               
         BZ    UPDOAM14                                                         
         TM    UOTYPE,UOTPROD      AND ONLY FOR PRODUCTION ORDERS               
         BZ    UPDOAM14                                                         
         CLI   CSACT,ACTDEL        DON'T CREATE EXTRA WORKCODES FOR             
         BE    UPDOAM14                             DELETE ACTION               
                                                                                
         USING OAMELD,R1                                                        
UPDOAM02 CLI   OAMEL,0                                                          
         BE    UPDOAM08            NOT THERE, ADD IT                            
         CLI   OAMEL,OAMELQ                                                     
         BNE   UPDOAM06                                                         
         TM    UOTYPE,UOTPEXP      IF EXPENSE ORDER                             
         BZ    UPDOAM04                                                         
         CLC   OAMWORK,BCSPACES    DOES WORKCODE MATCH SPACES FOR               
         BNE   UPDOAM06                              EXPENSE                    
         B     UPDOAM12                                                         
UPDOAM04 CLC   OAMWORK,BIOWORK     DOES WORKCODE MATCH?                         
         BE    UPDOAM12                                                         
                                                                                
UPDOAM06 IC    R0,OAMLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     UPDOAM02            LOOP BACK UP FOR NEXT ELEMENT                
*                                                                               
UPDOAM08 LA    R1,UOELEM                                                        
         XC    UOELEM,UOELEM                                                    
         MVI   OAMEL,OAMELQ                                                     
         MVI   OAMLN,OAMLN2Q                                                    
         MVI   OAMSTAT,OAMSXTRA                                                 
         ZAP   OAMAMNT,BCPZERO                                                  
         ZAP   OAMINUM,BCPZERO                                                  
         ZAP   OAMIVAL,BCPZERO                                                  
         ZAP   OAMTVAL,BCPZERO                                                  
         MVC   OAMWORK,BIOWORK                                                  
         TM    UOTYPE,UOTPEXP      IF EXPENSE ORDER                             
         BZ    UPDOAM10                                                         
         MVC   OAMWORK,BCSPACES    SET WORKCODE AS SPACES                       
                                                                                
UPDOAM10 LA    RF,=CL8'ADD=CODE'                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCMST'),AIO3,UOELEM,(RF)                   
         CLI   12(R1),0                                                         
         BE    UPDOAM12            GO BACK AND FALL INTO THE CODE               
         DC    H'0'                                                             
*                                                                               
UPDOAM12 LA    R1,ORDRFST                                                       
         SR    R0,R0                                                            
         USING OAMELD,R1                                                        
UPDOAM14 CLI   OAMEL,0             IS WORKCODE MISSING                          
         BNE   UPDOAM16            NO                                           
         TM    CSMIX2,MIXIMARK     YES - TEST MARK/UNDO                         
         BNZ   UPDOAM42                                                         
         OI    UOTYPE,UOTMISS                                                   
         B     UPDOAM60                                                         
UPDOAM16 CLI   OAMEL,OAMELQ                                                     
         BE    UPDOAM20                                                         
                                                                                
UPDOAM18 IC    R0,OAMLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     UPDOAM14            LOOP BACK UP FOR NEXT ELEMENT                
                                                                                
UPDOAM20 TM    UOTYPE,UOTPROD      TEST PRODUCTION ORDER                        
         BZ    UPDOAM36                                                         
         TM    UOTYPE,UOTPEXP      IF EXPENSE ORDER                             
         BZ    UPDOAM22                                                         
         CLC   OAMWORK,BCSPACES    DOES WORKCODE MATCH SPACES FOR               
         BE    UPDOAM36                              EXPENSE                    
         B     UPDOAM18                                                         
UPDOAM22 CLC   OAMWORK,BIOWORK     TEST WORKCODE MATCHES                        
         BNE   UPDOAM18                                                         
*                                                                               
UPDOAM36 LTR   R2,R2               TEST THIS IS THE ORDER RECORD                
         BNZ   UPDOAM38            NO - TRANSACTION                             
         TM    UOIND1,UBIUPD+UBIDEL UPDATE REDUCE PENDING                       
         JZ    UPDOAM38                  IF BATCH IS SAVED OR CLOSED            
         LLC   R4,OAMIPND          REDUCE PENDING COUNT                         
         SHI   R4,1                                                             
         STC   R4,OAMIPND                                                       
*                                                                               
UPDOAM38 TM    CSMIX2,MIXIMARK     TEST MARK/UNDO                               
         BZ    UPDOAM60                                                         
         LTR   R2,R2               TEST THIS IS THE ORDER RECORD                
         BNZ   UPDOAM40            NO - TRANSACTION                             
*                                                                               
* ORDER RECORD PROCESSING                                                       
*                                                                               
         TM    ORDRSTAT,ORDSCON    CONTRACT ORDER                               
         BZ    UPDOAM40                                                         
         SP    OAMTVAL,BIOAMNT     YES - ADD BACK TO AMOUNT MATCHED             
         AP    OAMTVAL,BIOSTAMT    DON'T INCLUDE TAX/SHIP                       
         MVC   OAMLAST,BCTODAYP                                                 
         AP    OAMIVAL,BIOAMNT                                                  
         SP    OAMIVAL,BIOSTAMT                                                 
         AP    OAMINUM,=P'1'                                                    
         SR    R4,R4                                                            
         GOTOR UPDCPO              UPDATE CONTRACT PO ELEMENT                   
         B     UPDOAM42                                                         
                                                                                
UPDOAM40 L     RF,AIO2             TEST IF BATCH IS CLOSED                      
         TM    TBARHSTA-TBARECD(RF),TBAHSEND                                    
         BNZ   UPDOAM42            IF CLOSED DON'T REPEAT WHEN UPDATING         
         LTR   R2,R2               TEST THIS IS THE ORDER RECORD                
         BNZ   *+10                NO - TRANSACTION                             
         SP    OAMTVAL,BIOAMNT     YES - SUBTRACT FROM AMOUNT MATCHED           
         AP    OAMIVAL,BIOAMNT     ADD TO INVOICED TO DATE                      
         AP    OAMINUM,=P'1'                                                    
*                                                                               
UPDOAM42 LTR   R2,R2                                                            
         BNZ   UPDOAM50                                                         
         LA    R4,ORDRFST                                                       
         SR    RE,RE                                                            
         USING ORDELD,R4                                                        
UPDOAM44 CLI   ORDEL,ORDELQ        FIND THE ORDEL                               
         BE    UPDOAM46                                                         
         IC    RE,ORDLN                                                         
         AR    R4,RE                                                            
         B     UPDOAM44                                                         
*                                                                               
UPDOAM46 CLI   BIOSTAT,BIOSPTQ     IS THIS A PART MATCHED ORDER                 
         BNE   UPDOAM48            NO                                           
         TM    ORDSTAT,ORDSMNUP    HAS IT BEEN FULLY MATCHED BY ANOTHER         
         BNZ   UPDOAMX                           BATCH - DON'T UNMATCH          
         OI    ORDSTAT,ORDSPART    YES                                          
         NI    ORDSTAT,FF-ORDSMNUP                                              
         NI    ORDRSTAT,FF-(ORDSFMCH+ORDCLOSE)                                  
         B     UPDOAMX                                                          
*                                                                               
UPDOAM48 NI    ORDSTAT,FF-ORDSPART                                              
         OI    ORDSTAT,ORDSMNUP                                                 
         OI    ORDRSTAT,ORDSFMCH                                                
         B     UPDOAMX                                                          
         DROP  R4                                                               
*                                                                               
* ORDER TRANSACTION - MARK (UPDATE, CLOSE)                                      
*                                                                               
UPDOAM50 TM    UOIND1,UBIUPD                                                    
         BZ    UPDOAMX                                                          
         NI    ORDRSTAT,FF-TRNSDELT                                             
         CLI   BIOSTAT,BIOSPTQ                                                  
         BE    UPDOAMX             PARTMATCHED                                  
         OI    ORDRSTAT,TRNSDELT   DELETE IF ORDER IS FULLY MATCHED             
         B     UPDOAMX                                                          
*                                                                               
* BATCH ACTIONS - DELETE, OPEN, RECALL                                          
*                                                                               
UPDOAM60 TM    UOIND1,UBIDEL       TEST FORCE UNMARK BAT/DEL OR ITE/DEL         
         BO    UPDOAM62                                                         
         L     RF,AIO2             TEST IF BATCH IS CLOSED                      
         TM    TBARHSTA-TBARECD(RF),TBAHSEND                                    
         BNO   UPDOAMX             DON'T UNDO THAT WHICH WASN'T DONE            
*                                                                               
UPDOAM62 TM    UOTYPE,UOTMISS      MISSING WORKCODE OAMELD ELEMENT              
         BNZ   UPDOAM72            YES - SKIP UPDATE                            
         LTR   R2,R2               TEST THIS IS THE ORDER RECORD                
         BNZ   UPDOAM70                                                         
         TM    ORDRSTAT,ORDSCON    CONTRACT ORDER                               
         BZ    UPDOAM68                                                         
         L     RF,AIO2                                                          
         TM    TBARHSTA-TBARECD(RF),TBAHSEND      IS BATCH CLOSED?              
         BZ    UPDOAM66                                                         
         TM    UOIND1,UBIDEL       BAT/DEL OR ITE/DEL?                          
         BZ    UPDOAM64                                                         
         MVC   OAMLAST,BCTODAYP                                                 
         SP    OAMTVAL,BIOAMNT                                                  
         AP    OAMTVAL,BIOSTAMT                                                 
         B     UPDOAM66                                                         
                                                                                
UPDOAM64 AP    OAMTVAL,BIOAMNT     YES SUBTRACT FROM AMOUNT MATCHED             
         SP    OAMTVAL,BIOSTAMT    DON'T INCLUDE TAX/SHIP                       
         MVC   OAMLAST,BCTODAYP                                                 
         SP    OAMIVAL,BIOAMNT                                                  
         AP    OAMIVAL,BIOSTAMT                                                 
         SP    OAMINUM,=P'1'                                                    
UPDOAM66 LHI   R4,1                                                             
         GOTOR UPDCPO              UPDATE CONTRACT PO ELEMENT                   
         B     UPDOAM72                                                         
                                                                                
UPDOAM68 TM    UOIND1,UBIDEL       BAT/DEL OR ITE/DEL?                          
         BO    *+14                                                             
         AP    OAMTVAL,BIOAMNT     YES ADD BACK TO AMOUNT MATCHED               
         B     UPDOAM70                                                         
                                                                                
         L     RF,AIO2                                                          
         TM    TBARHSTA-TBARECD(RF),TBAHSEND      IS BATCH CLOSED?              
         BO    UPDOAM70                                                         
         SP    OAMTVAL,BIOAMNT     JUST GET RID OF AMOUNT FROM TODAY            
         B     UPDOAM72                                                         
                                                                                
UPDOAM70 L     RF,AIO2                                                          
         TM    TBARHSTA-TBARECD(RF),TBAHSEND    IS BATCH CLOSED?                
         BZ    UPDOAM72            NO, DO NOTHING                               
         SP    OAMIVAL,BIOAMNT     ADD BACK AMOUNT INVOICED TO DATE             
         SP    OAMINUM,=P'1'                                                    
*                                                                               
* ORDER STATUS                                                                  
*                                                                               
UPDOAM72 LTR   R2,R2               TEST THIS IS THE ORDER RECORD                
         BNZ   UPDOAM82            NO - BT12 ORDER TRANSACTION REC              
         LA    R4,ORDRFST                                                       
         SR    RE,RE                                                            
         USING ORDELD,R4                                                        
UPDOAM74 CLI   ORDEL,ORDELQ        FIND THE ORDEL                               
         BE    UPDOAM76                                                         
         IC    RE,ORDLN                                                         
         AR    R4,RE                                                            
         B     UPDOAM74                                                         
*                                                                               
UPDOAM76 CLI   BIOSTAT,BIOSPTQ     IS THIS A PART MATCHED ORDER                 
         BNE   UPDOAM78            NO                                           
         TM    ORDSTAT,ORDSMNUP    IF WAS IT FULLY MATCHED                      
         BNZ   UPDOAM82            ELSEWHERE - DO NOTHING                       
UPDOAM78 GOTOR CHKBT10             CHECK FOR BATCH TYPE 10                      
         BNE   UPDOAM82                                                         
         TM    UOIND1,UBIRCL       TEST BATCH RECALL                            
         BNZ   UPDOAM80            IF SO DON'T ALTER ORDSTAT                    
         OI    ORDSTAT,ORDSPART                                                 
         NI    ORDSTAT,FF-ORDSMNUP                                              
                                                                                
UPDOAM80 NI    ORDRSTAT,FF-(ORDSFMCH) ASSUME PART MATCHED NOW                   
         TM    UOTYPE,UOTMISS      MISSING WORKCODE OAMELD ELEMENT              
         BNZ   UPDOAMX             YES - SKIP                                   
         CP    OAMINUM,BCPZERO     ANY INVOICES LEFT ON ORDER?                  
         BNE   UPDOAM82                                                         
         OC    OAMIPND,OAMIPND                                                  
         BNZ   UPDOAM82                                                         
         NI    ORDSTAT,FF-ORDSPART    UNMATCH IF NOTHING LEFT                   
                                                                                
UPDOAM82 TM    UOTYPE,UOTMISS      MISSING WORKCODE OAMELD ELEMENT              
         BNZ   UPDOAMX             YES - SKIP                                   
         TM    OAMSTAT,OAMSXTRA    IS THIS AN EXTRA WORKCODE                    
         BZ    UPDOAMX                                                          
         OC    OAMIPND,OAMIPND     IF ALL NUMBERS ARE ZERO                      
         BNZ   UPDOAMX                 DELETE ELEMENT                           
         CP    OAMIVAL,BCPZERO                                                  
         BNE   UPDOAMX                                                          
         CP    OAMINUM,BCPZERO                                                  
         BNE   UPDOAMX                                                          
         CP    OAMTVAL,BCPZERO                                                  
         BNE   UPDOAMX                                                          
         MVI   OAMEL,X'FF'                                                      
         GOTO1 VHELLO,DMCB,(C'D',=C'ACCMST'),('FF',ORDRECD),0                   
         CLI   12(R1),0                                                         
         BE    UPDOAMX                                                          
         DC    H'0'                                                             
*                                                                               
UPDOAMX  J     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK WHETHER THERE ARE OTHER ITEMS FOR THE BT10                    *         
***********************************************************************         
                                                                                
CHKBT10  NTR1  ,                                                                
IT       USING TBARECD,R2                                                       
         USING BIOELD,R3                                                        
         USING TBARECD,R4                                                       
         L     R2,AIO1                                                          
         CLI   IT.TBAKBTYP,X'0A'   BATCH TYPE 10                                
         JNE   ROUTE                                                            
         TM    UOIND1,UBIITM       TEST IF ITEM LEVEL ACTION                    
         JZ    ROUTE               IF NOT EXIT GOOD                             
CHKBT002 MVC   IOKEY,IT.TBAKEY     USE BATCH KEY                                
         LA    R4,IOKEY                                                         
         XR    R1,R1                                                            
*        ICM   R1,3,TBAKTSEQ                                                    
         AHI   R1,1                                                             
         STCM  R1,3,TBAKTSEQ                                                    
         MVC   IOADDR,AIO4                                                      
         GOTOR AIO,IOHI+IOACCDIR                                                
         B     CHKBT006                                                         
                                                                                
CHKBT004 MVC   IOADDR,AIO4                                                      
         LA    R4,IOKEY                                                         
         GOTOR AIO,IOSEQ+IOACCDIR                                               
                                                                                
CHKBT006 CLC   TBAKEY(TBAKTSEQ-TBAKEY),IT.TBAKEY  HAVE WE READ ALL RECS         
         JNE   ROUTE               YES - NOTHING FOUND GOOD EXIT                
         CLC   TBAKEY,IT.TBAKEY                                                 
         BE    CHKBT004                                                         
         TM    TBAKESTA,TBAESLDE   IF RECORD DELETED IGNORE IT                  
         BNZ   CHKBT004                                                         
                                                                                
         MVC   IOADDR,AIO4                                                      
         GOTOR AIO,IOGET+IOACCMST                                               
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,AIO4                                                          
         AHI   R4,TBARFST-TBARECD                                               
N        USING BIOELD,R4                                                        
CHKBT008 CLI   N.BIOEL,0                                                        
         BE    CHKBT004                                                         
         CLI   N.BIOEL,BIOELQ                                                   
         BE    CHKBT012                                                         
CHKBT010 LLC   RF,N.BIOLN                                                       
         AR    R4,RF                                                            
         B     CHKBT008                                                         
                                                                                
CHKBT012 CLC   BIOONUM,N.BIOONUM                                                
         BNE   CHKBT010                                                         
         CLI   N.BIOSTAT,BIOSPTQ    IS IT PART MATCHED                          
         JE    CHKBT010                                                         
         J     ROUTH                                                            
         DROP  N,IT                                                             
         EJECT                                                                  
***********************************************************************         
* UPDATE CONTRACT PURCHASE ORDER ELEMENT                              *         
***********************************************************************         
                                                                                
         USING CPOELD,R1                                                        
UPDCPO   NTR1  ,                                                                
         CLI   BIOLN,BIOLNQ        HAS TO HAVE EXTRA INFO                       
         JNH   ROUTE                                                            
                                                                                
         SR    R0,R0                                                            
         SR    RF,RF               FIND LAST BYTE OF ELEMENT                    
         IC    RF,BIOLN                                                         
         LA    RF,0(RF,R3)         STRIP OFF HIGH ORDER BYTE                    
         BCTR  RF,0                                                             
         LA    RE,BIOSUBQ          INC REGISTER                                 
                                                                                
         LA    R2,BIOSTOCK                                                      
         USING BIOSTOCK,R2                                                      
UPDCPO02 CLI   CPOEL,0             EOR, LEAVE                                   
         BE    UPDCPOX                                                          
         CLI   CPOEL,CPOELQ                                                     
         BNE   UPDCPO04                                                         
         CLC   CPOSTOCK,BIOSTOCK   STOCK NUMBERS HAVE TO MATCH                  
         BE    UPDCPO06                                                         
UPDCPO04 IC    R0,CPOLN                                                         
         AR    R1,R0                                                            
         B     UPDCPO02                                                         
                                                                                
UPDCPO06 MVC   CPOLAST,BCTODAYP    LAST ACTIVITY DATE                           
         ZAP   BCDUB,BIOSAMT                                                    
         MP    BCDUB,BIOSQTY                                                    
         LTR   R4,R4                                                            
         BNZ   UPDCPO08                                                         
         AP    CPOQTYIN,BIOSQTY                                                 
         AP    CPOAMTIN,BCDUB                                                   
         SP    CPOQTYTD,BIOSQTY                                                 
         SP    CPOAMTTD,BCDUB                                                   
         B     UPDCPO12                                                         
                                                                                
UPDCPO08 TM    UOIND1,UBIDEL       BAT/DEL OR ITE/DEL?                          
         BZ    UPDCPO10                                                         
         SP    CPOQTYTD,BIOSQTY                                                 
         SP    CPOAMTTD,BCDUB                                                   
         B     UPDCPO12                                                         
                                                                                
UPDCPO10 SP    CPOQTYIN,BIOSQTY                                                 
         AP    CPOQTYTD,BIOSQTY                                                 
         SP    CPOAMTIN,BCDUB                                                   
         AP    CPOAMTTD,BCDUB                                                   
                                                                                
UPDCPO12 BXLE  R2,RE,UPDCPO02                                                   
                                                                                
UPDCPOX  J     ROUTE                                                            
         DROP  R1,R3,R5,RB                                                      
         EJECT                                                                  
***********************************************************************         
* UPDATE CONTRACT                                                     *         
* NTRY - R3=A(BATCH ITEM ORDER ELEMENT)                               *         
*        ORDER RECORD IN IOAREA 3                                     *         
***********************************************************************         
                                                                                
UPDCNT   NTR1  BASE=*,LABEL=*,WORK=(R6,256)                                     
         XC    0(256,R6),0(R6)                                                  
         USING BIOELD,R3                                                        
         L     R4,AIO3             POINT TO ORDER RECORD                        
         AHI   R4,ORDRFST-ORDRECD                                               
         SR    R0,R0                                                            
         USING OAMELD,R4                                                        
UPDCNT02 CLI   OAMEL,0             EOR?                                         
         BE    UPDCNTX                                                          
         CLI   OAMEL,OAMELQ        ORDER AMOUNT ELEMENT?                        
         BE    UPDCNT04                                                         
         IC    R0,OAMLN            NO, BUMP TO NEXT ELEMENT                     
         AR    R4,R0                                                            
         B     UPDCNT02                                                         
                                                                                
         USING ACTRECD,R2                                                       
UPDCNT04 GOTOR CALOPO              CALCULATE TRUE AMT FROM ORDER REC            
         SR    RF,RF                                                            
         IC    RF,OAMLN            GET LENGTH OF ELEMENT                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),OAMEL       SAVE IT, CUZ WE'RE KILLING IO3               
         LR    R4,R6                                                            
                                                                                
         LA    R2,IOKEY            BUILD CONTRACT KEY                           
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'OAMCONT),OAMCONT                                       
         OC    ACTKULA(L'OAMCONT),BCSPACES                                      
         GOTOR AIO,IOREAD+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   UORDA,IODA          SAVE DISK ADDRESS                            
                                                                                
         L     R5,AIO3                                                          
         AHI   R5,ACTRFST-ACTRECD                                               
         USING CNTELD,R5                                                        
UPDCNT06 CLI   CNTEL,0             EOR?                                         
         BE    UPDCNTX                                                          
         CLI   CNTEL,CNTELQ        UPDATE CTA CONTRACT ELEMENT                  
         BE    UPDCNT08                                                         
         IC    R0,CNTLN                                                         
         AR    R5,R0                                                            
         B     UPDCNT06                                                         
                                                                                
UPDCNT08 TM    CSMIX2,MIXIMARK     TEST MARK/UNDO                               
         BZ    UPDCNT10                                                         
         AP    CNTCOST,BIOAMNT     + ACTUAL (INCLUDING SALES TAX/SHIP)          
         AP    CNTCOST,BIOUTAMT    +   USE TAX                                  
         SP    CNTOPNPO,BCDUB      - RETAIL                                     
         AP    CNTINVPO,BIORTAMT   + (RETAIL + S&T)                             
         MVC   CNTDELDT,OAMLAST    LAST ACTIVITY DATE                           
         B     UPDCNT12                                                         
                                                                                
UPDCNT10 L     RF,AIO2             TEST IF BATCH IS CLOSED                      
         TM    TBARHSTA-TBARECD(RF),TBAHSEND                                    
         BNO   UPDCNTX             DON'T UNDO THAT WHICH WASN'T DONE            
                                                                                
         SP    CNTCOST,BIOAMNT     - ACTUAL (INCLUDING SALES TAX/SHIP)          
         SP    CNTCOST,BIOUTAMT    -   USE TAX                                  
         AP    CNTOPNPO,BCDUB      + RETAIL                                     
         SP    CNTINVPO,BIORTAMT   - (RETAIL + S&T)                             
         MVC   CNTDELDT,OAMLAST    LAST ACTIVITY DATE                           
                                                                                
         USING CTGELD,R5                                                        
UPDCNT12 IC    R0,CTGLN                                                         
         AR    R5,R0                                                            
         CLI   CTGEL,0             EOR?                                         
         BE    UPDCNTX                                                          
         CLI   CTGEL,CTGELQ        UPDATE CTA CATEGORY ELEM                     
         BNE   UPDCNT12                                                         
         CLC   CTGCTGY,OAMWORK     CATEGORY=WORKCODE?                           
         BNE   UPDCNT12                                                         
         TM    CSMIX2,MIXIMARK     TEST MARK/UNDO                               
         BZ    UPDCNT14                                                         
         SP    CTGOPNPO,BCDUB      - RETAIL                                     
         AP    CTGINVPO,BIORTAMT   + (RETAIL + S&T)                             
         AP    CTGCOST,BIOAMNT     + ACTUAL (INCLUDING SALES TAX/SHIP)          
         AP    CTGCOST,BIOUTAMT    +   USE TAX                                  
         B     UPDCNT16                                                         
                                                                                
UPDCNT14 AP    CTGOPNPO,BCDUB      + RETAIL                                     
         SP    CTGINVPO,BIORTAMT   - (RETAIL + S&T)                             
         SP    CTGCOST,BIOAMNT     - ACTUAL (INCLUDING SALES TAX/SHIP)          
         SP    CTGCOST,BIOUTAMT    -   USE TAX                                  
                                                                                
UPDCNT16 GOTOR AIO,IOPUTREC+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
UPDCNTX  J     ROUTE                                                            
         DROP  R2,R3,R4,R5,RB                                                   
         EJECT                                                                  
***********************************************************************         
* CALCULATE AMOUNT TO UPDATE CONTRACT OPEN PO'S                       *         
***********************************************************************         
                                                                                
         USING CPOELD,R2                                                        
         USING BIOSTOCK,R5                                                      
CALOPO   NTR1  ,                                                                
         ZAP   0(8,R6),=P'0'       INIT                                         
         SR    R1,R1                                                            
         LA    R5,BIOLN2Q(R3)      POINT R5 TO SEPARATE DETAILS                 
         LR    R2,R4                                                            
CALOPO02 CLI   0(R2),0             POINT R2 TO CONTRACT PO ELEMENTS             
         JE    CALOPO10                                                         
         CLI   CPOEL,CPOELQ                                                     
         JE    CALOPO04                                                         
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     CALOPO02                                                         
                                                                                
CALOPO04 CLI   0(R5),0             EOR?                                         
         JE    CALOPO10                                                         
         CP    BIOSQTY,=P'0'       ANY INVOICED?                                
         JNE   CALOPO08                                                         
CALOPO06 AHI   R5,BIOSUBQ                                                       
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     CALOPO04                                                         
                                                                                
CALOPO08 ZAP   BCDUB,CPOURET       UNIT RETAIL * QUANTITY * RATIO               
         MP    BCDUB,BIOSQTY                                                    
         MP    BCDUB,CPORATIO                                                   
         SRP   BCDUB,64-2,5        GET RID OF 2 DECIMALS                        
         AP    0(8,R6),BCDUB       KEEP RUNNING TOTAL                           
         J     CALOPO06            BUMP TO NEXT ONE                             
                                                                                
CALOPO10 ZAP   BCDUB,0(8,R6)                                                    
         J     ROUTX                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
FF       EQU   X'FF'                                                            
DMCB     EQU   BOPARM                                                           
                                                                                
LITERALS DS    0D                                                               
*                                                                               
         LTORG                                                                  
                                                                                
MOSTAB   DC    C'123456789......ABC'                                            
FIXIT    DC    C'N'                                                             
         EJECT                                                                  
CHQTABD  DSECT                     ** DSECT COVERS CHEQUE TABLE **              
CHQITEM  DS    XL(L'TBAKTSEQ)      FIRST MERGED ITEM NUMBER                     
CHQDATA  DS    XL(BICLN1Q+L'BICCACN)                                            
CHQKEYL  EQU   (BICSTA-BICACT)                                                  
CHQTABL  EQU   *-CHQTABD                                                        
CHQTMAX  EQU   64                  MAXIMUM ENTRIES                              
                                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
                                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
                                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
                                                                                
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
                                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
                                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
* ACBATWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBATWORK                                                      
         PRINT ON                                                               
                                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
                                                                                
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
                                                                                
* GEGENCUR                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENCUR                                                       
         PRINT ON                                                               
                                                                                
* ACPRORATAD                                                                    
PRORATAD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACPRORATAD                                                     
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'165ACGEN6E   07/10/18'                                      
         END                                                                    
