*          DATA SET SPNWS19    AT LEVEL 115 AS OF 02/27/07                      
*PHASE T20719C,*                                                                
         TITLE 'NWS19 - NEW BUYERS WORK SHEET - COMMENT OVERLAY'                
T20719   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20719**,RA,RR=RE                                              
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         LA    R2,IOKEY                                                         
         USING COMRECD,R2                                                       
*                                                                               
         CLI   APMODE,APMVALK     VALIDATE KEY                                  
         BE    VALKEY                                                           
         CLI   APMODE,APMVALR     VALIDATE RECORD                               
         BE    VALREC                                                           
         CLI   APMODE,APMDISR     DISPLAY RECORD                                
         BE    DISREC                                                           
         CLI   APMODE,APMDELR     DELETE RECORD                                 
         BE    DELREC                                                           
         CLI   APMODE,APMRESR     RESTORE RECORD                                
         BE    RESREC                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*====================================================================*          
* VALIDATE KEY - VALID COMBOS ARE:                                   *          
* 1) MED,ALL,ALL,ALL (2) MED,BYR,ALL,ALL,ALL  (3) MED,BYR,CMP,ALL,ALL*          
* 4) MED,BYR,CMP,MKT,ALL (5) MED,BYR,CMP,MKT,STA                     *          
*====================================================================*          
*                                                                               
VALKEY   XC    BAGYMD(BVALSX-BAGYMD),BAGYMD                                     
         XC    QMED(QVALSX-QMED),QMED   ALWAYS FORCE RE-VALIDATION              
*                                                                               
         LA    R3,COMMEDH                                                       
         XC    COMMDN,COMMDN      VALIDATE MEDIA FIELD                          
         OI    COMMDNH+6,FVOXMT   SET TRANSMIT BYTE                             
         GOTO1 AVALMED,COMMEDH                                                  
         BNE   VALKX                                                            
         MVC   COMMDN,MEDNM       DISPLAY MEDIA NAME TO SCREEN                  
*                                                                               
VALBYR   LA    R3,COMBYRH         VALIDATE BUYER FIELD                          
         XC    COMBYN,COMBYN                                                    
         OI    COMBYNH+6,FVOXMT                                                 
         CLC   =C'ALL',8(R3)                                                    
         BE    VALCAM                                                           
         GOTO1 AVALBYR,COMBYRH                                                  
         BNE   VALKX                                                            
         MVC   COMBYN,BYRNM       DISPLAY BUYER NAME TO SCREEN                  
         OC    BYRPW,BYRPW        ANY SPECIAL PASSWORD?                         
         BZ    VALCAM                                                           
         GOTO1 AVALPWD                                                          
         BNE   VALKX              INCORRECT PASSWORD                            
*                                                                               
VALCAM   LA    R3,COMCAMH         VALIDATE CAMPAIGN FIELD                       
         XC    COMCMN,COMCMN                                                    
         OI    COMCMNH+6,FVOXMT                                                 
         CLC   =C'ALL',8(R3)                                                    
         BE    VALMKT                                                           
         CLI   BBYR,0             IF ALL BUYER, CAN'T HAVE ONE CAMPAIGN         
         BNE   VALCAM2                                                          
         MVC   FVMSGNO,=AL2(FVICAM)                                             
         B     VALKNO                                                           
VALCAM2  GOTO1 AVALCAM,COMCAMH                                                  
         BNE   VALKX                                                            
         MVC   COMCMN,CMPNM      DISPLAY CAMPAIGN NAME                          
*                                                                               
VALMKT   LA    R3,COMMKTH         VALIDATE MARKET FIELD                         
         XC    COMMTN,COMMTN                                                    
         OI    COMMTNH+6,FVOXMT                                                 
         CLC   =C'ALL',8(R3)                                                    
         BE    VALSTA                                                           
         OC    BCAM,BCAM          IF ALL CAMP, CAN'T HAVE ONE MARKET            
         BNZ   VALMKT2                                                          
         MVC   FVMSGNO,=AL2(FVIMKT)                                             
         B     VALKNO                                                           
VALMKT2  GOTO1 AVALMKT,COMMKTH                                                  
         BNE   VALKX                                                            
         MVC   COMMTN,MKTNM       DISPLAY MARKET NAME                           
*                                                                               
VALSTA   LA    R3,COMSTAH         VALIDATE STATION FIELD                        
         XC    COMSTN,COMSTN                                                    
         OI    COMSTNH+6,FVOXMT                                                 
         CLC   =C'ALL',8(R3)                                                    
         BE    VALK10                                                           
         OC    BMKT,BMKT          IF ALL MKTS, CAN'T HAVE ONE STATION           
         BNZ   VALSTA2                                                          
         MVC   FVMSGNO,=AL2(FVISTA)                                             
         B     VALKNO                                                           
VALSTA2  GOTO1 AVALSTA,COMSTAH                                                  
         BNE   VALKX                                                            
******** MVC   COMSTN,QSTA        DISPLAY STATION NAME                          
*                                                                               
VALK10   XC    IOKEY,IOKEY                                                      
         MVI   COMKTYP,COMKTYPQ                                                 
         MVI   COMKSUB,COMKSUBQ                                                 
         MVC   COMKAGMD,BAGYMD                                                  
         OC    COMKAGMD,BBYRMASK                                                
         MVC   COMKBYR,BBYR                                                     
         MVC   COMKCAM,BCAM                                                     
         MVC   COMKMKT,BMKT                                                     
         MVC   COMKSTA,BSTA                                                     
*                                                                               
         LA    R3,COMMEDH         PT TO MEDIA HEADER AGAIN                      
         MVC   APRECKEY,IOKEY                                                   
         GOTO1 AIO,DIRRDD+IO1                                                   
         BL    VALKX              HARD IO ERROR                                 
         BAS   RE,IOCHECK         CHECK IO CONDITIONS                           
         BNE   VALKX                                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   APACTN,ACTADD                                                    
         BE    VALKX                                                            
         TM    APINDS,APIOKADD                                                  
         BO    VALKX                                                            
         LA    R1,FILGET1                                                       
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BNE   VALKX                                                            
         MVC   APRECDA,IODA                                                     
*                                                                               
VALKX    ST    R3,APCURSOR        SET CURSOR ON EXIT                            
         B     EXIT                                                             
*                                                                               
VALKNO   LTR   RB,RB                                                            
         B     VALKX                                                            
         EJECT                                                                  
*================================*                                              
* VALIDATE RECORD                *                                              
*================================*                                              
*                                                                               
VALREC   L     R2,AIOAREA1        VALIDATE COMMENT RECORD                       
         XC    COMRECD(256),COMRECD                                             
         MVC   COMKEY,APRECKEY    SET KEY                                       
*                                                                               
         MVI   APELEM,CCMELCDQ    DELETE THE COMMENT ELEMENTS                   
         GOTO1 ADELELS,(R2)                                                     
*                                                                               
         LA    R4,1                                                             
         LA    R8,COMCM1H                                                       
         CLI   5(R8),0            FIRST LINE MUST HAVE INPUT                    
         BE    VALRNO                                                           
*                                                                               
VALR20   SR    R1,R1                                                            
         ICM   R1,1,5(R8)         GET LENGTH OF COMMENT LINE                    
         BZ    VALR30             NO MORE COMMENTS                              
         BCTR  R1,0               SET IT UP FOR EX MOVE OF COMMENTS             
         LA    R3,APELEM                                                        
         USING CCMEL,R3                                                         
         MVI   CCMELCD,CCMELCDQ                                                 
         STC   R4,CCMELSEQ        SEQUENCE NUMBER                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CCMELCOM(0),8(R8)                                                
         LA    R1,1(R1)           RESTORE LENGTH OF COMMENT                     
         LA    R1,3(R1)           + LENGTH + ELE CODE + SEQ #                   
         STC   R1,CCMELLN         = TOTAL LENGTH OF ELEMENT                     
         GOTO1 AADDELS,COMRECD    ADD ELEMENT TO RECORD                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,0(R8)                                                         
         AR    R8,R0                                                            
         LA    R4,1(R4)                                                         
         CH    R4,=H'4'                                                         
         BNH   VALR20                                                           
*                                                                               
VALR30   LA    R1,FILADD1                                                       
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,FILPUT1                                                       
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALRX    B     EXIT                                                             
*                                                                               
VALRNO   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         ST    R8,APCURSOR                                                      
         LTR   RB,RB                                                            
         B     VALRX                                                            
         DROP  R3                                                               
         EJECT                                                                  
*================================*                                              
* DISPLAY RECORD                 *                                              
*================================*                                              
*                                                                               
DISREC   XC    COMCM1,COMCM1      CLEAR THE LINES                               
         OI    COMCM1H+6,FVOXMT                                                 
         XC    COMCM2,COMCM2                                                    
         OI    COMCM2H+6,FVOXMT                                                 
         XC    COMCM3,COMCM3                                                    
         OI    COMCM3H+6,FVOXMT                                                 
         XC    COMCM4,COMCM4                                                    
         OI    COMCM4H+6,FVOXMT                                                 
*                                                                               
         L     R2,AIOAREA1        VALIDATE COMMENT RECORD                       
         LA    R8,COMCM1H         FIRST COMMENT LINE ON SCREEN                  
         LA    R3,CCMEL           FIRST COMMENT ELEMENT                         
         USING CCMEL,R3                                                         
*                                                                               
DISR10   CLI   0(R3),0                                                          
         BE    DISRX              END OF RECORD                                 
         CLI   0(R3),CCMELCDQ                                                   
         BNE   DISR20                                                           
         ZIC   R1,CCMELLN         COMMENT ELEMENT                               
         SH    R1,=H'2'                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R8),CCMELCOM   DISPLAY IT                                    
         OI    6(R8),FVOXMT       TRANSMIT THE LINE                             
         ZIC   R0,0(R8)                                                         
         AR    R8,R0              NEXT COMMENT LINE ON SCREEN                   
*                                                                               
DISR20   ZIC   R0,1(R3)                                                         
         AR    R3,R0              NEXT ELEMENT IN RECORD                        
         B     DISR10                                                           
DISRX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*================================*                                              
* DELETE RECORD                  *                                              
*================================*                                              
*                                                                               
DELREC   MVC   IOKEY(L'APRECKEY),APRECKEY                                       
         GOTO1 AIO,DIRRDUP            READ DIRECTORY FOR UPDATE                 
         BNE   DELRX                                                            
         LA    R2,IOKEY                                                         
         TM    COMKCNTL,X'80'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    COMKCNTL,X'80'         TURN ON DELETE BIT                        
         GOTO1 AIO,DIRWRT             WRITE DIRECTORY POINTER                   
         BNE   DELRX                                                            
         GOTO1 AIO,FILGETU1           READ FILE FOR UPDATE                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR                                                        
         TM    COMCNTL,X'80'                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    COMCNTL,X'80'          TURN ON DELETE BIT                        
         GOTO1 AIO,FILPUT1            WRITE THE RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
DELRX    B     EXIT                                                             
         EJECT                                                                  
*================================*                                              
* RESTORE RECORD                 *                                              
*================================*                                              
*                                                                               
RESREC   MVC   IOKEY(L'APRECKEY),APRECKEY                                       
         GOTO1 AIO,DIRRDUP         READ DIRECTORY FOR UPDATE                    
         BL    RESRX               HARD ERROR                                   
         BE    RESR9               ALREADY EXISTS                               
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    RESRX               NO - ERROR                                   
         LA    R2,IOKEY                                                         
         TM    COMKCNTL,X'80'                                                   
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    COMKCNTL,X'7F'      TURN OFF DELETE BIT                          
         GOTO1 AIO,DIRWRT          WRITE DIRECTORY POINTER                      
         BNE   RESRX                                                            
         GOTO1 AIO,FILGETU1        READ FILE FOR UPDATE                         
         BNL   *+6                                                              
         DC    H'0'                HARD ERROR                                   
         BE    *+12                ALREADY EXISTS (OK)                          
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    RESRX               NO - ERROR                                   
         L     R2,IOADDR                                                        
         TM    COMCNTL,X'80'                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    COMCNTL,X'80'       TURN ON DELETE BIT                           
         GOTO1 AIO,FILPUT1         WRITE THE RECORD                             
         BE    RESRX                                                            
         DC    H'0'                                                             
*                                                                               
RESR9    MVC   FVMSGNO,=AL2(FVFERAE)     RECORD ALREADY EXISTS                  
RESRX    B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* ROUTINE TO CHECK IO ERROR BITS                                      *         
* IN : IOERR  - IO ERROR RETURN BYTE                                  *         
* OUT: APINDS - APPLICATION INDICATORS BYTE                           *         
*      CC     - EQ  OK                                                *         
*             - NE  NON RECOVERABLE ERROR                             *         
*=====================================================================*         
*                                                                               
IOCHECK  TM    IOERR,IOEDSK        NON-RECOVERABLE DISK ERROR                   
         BO    IOCH9                                                            
         OI    APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
*                                                                               
         TM    IOERR,IOEEOF        END-OF-FILE                                  
         BO    *+12                                                             
         TM    IOERR,IOERNF        RECORD NOT FOUND                             
         BZ    *+12                                                             
         NI    APINDS,255-APIOKDIS-APIOKCHA-APIOKDEL                            
         OI    APINDS,APIOKADD                                                  
*                                                                               
         TM    IOERR,IOEDEL         RECORD IS DELETED                           
         BZ    *+12                                                             
         NI    APINDS,255-APIOKADD-APIOKCHA-APIOKDEL                            
         OI    APINDS,APIOKRES                                                  
         CR    RE,RE                                                            
         B     IOCHX                                                            
*                                                                               
IOCH9    MVC   FVMSGNO,=AL2(FVFIOER)  IO ERROR                                  
         LA    R1,COMMEDH                                                       
         ST    R1,FVADDR                                                        
         LTR   RE,RE                                                            
IOCHX    BR    RE                                                               
         EJECT                                                                  
*==================*                                                            
* LITERAL POOL     *                                                            
*==================*                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SPNWSWRK                                                                      
       ++INCLUDE SPNWSWRK                                                       
         EJECT                                                                  
* LOCAL STORAGE AREA                                                            
LOCALD   DSECT                                                                  
LOCALX   EQU   *                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSDBD                                                       
*                                                                               
       ++INCLUDE SPNWSCOM                                                       
         SPACE 1                                                                
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115SPNWS19   02/27/07'                                      
         END                                                                    
