*          DATA SET PPCON50    AT LEVEL 019 AS OF 11/05/03                      
*PHASE T40D50A                                                                  
*INCLUDE NUMED                                                                  
*INCLUDE XSORT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T40D50 - PRINTPAK CONTRACT COPY'                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 09/18/01 ADD/CHG SPACE DESCRIPTION                                       
*                                                                               
* KWAN 12/99    REVISE CODES FOR TEL AND FAX FIELDS                             
*                                                                               
* KWAN 08/99    ADD CODES FOR NEW FIELDS (TEL AND FAX)                          
*                                                                               
* BPLA 11/98    CHANGE FOR OPEN RATES (X'24')                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T40D50   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40D50                                                         
         LA    R6,4095(RB)                                                      
         LA    R6,1(R6)                                                         
         USING T40D50+4096,R6                                                   
*                                                                               
         L     RC,0(R1)                                                         
         LA    R7,1(RC)                                                         
         LA    R7,4095(R7)                                                      
         USING GENOLD,RC,R7                                                     
         USING T40DFFD,RA                                                       
*                                                                               
         RELOC RELO10                                                           
*                                                                               
         FOUT  KBAPAGH,SPACES,2                                                 
         XC    NEXTNUM,NEXTNUM                                                  
         CLC   KBAACT(4),=C'COPY'   SPECIAL CODE FOR COPY TESTING               
         BE    *+6                                                              
         DC    H'0'                SOMETHING VERY WRONG                         
*              GET K FOR COPY                                                   
         LA    R2,KBAACTH                                                       
         LA    R3,12               INVALID ACTION                               
         TM    4(R2),X'20'         DID USER CHANGE CONTRACT NUMBER?             
         BZ    ERROR                                                            
*                                                                               
*                                                                               
         MVC   KEY+27(4),SAVKKEY+27   SAVE K DISK ADDR                          
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
*******                                                                         
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    CON10                                                            
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    CON10                                                            
         TM    SADVDATA+15,X'20'    OR IF I HAVE MY OWN CONTRACTS               
         BNO   CON10                                                            
         B     ERROR                       INVALID ACTION                       
*                                                                               
*                                                                               
CON10    DS    0H                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    CON12                                                            
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    CON12                                                            
         TM    SADVDATA+15,X'20'    OR IF I HAVE MY OWN CONTRACTS               
         BNO   CON12                                                            
         B     ERROR                      INVALID ACTION                        
*                                                                               
CON12    DS    0H                                                               
*                                                                               
*                                                                               
CON50    CLC   PCONMOD,TODAY                                                    
         BE    CON100                                                           
         SR    RE,RE                                                            
         IC    RE,PCONMODN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,PCONMODN                                                      
         MVC   PCONMOD,TODAY                                                    
*                                                                               
CON100   DS    0H                                                               
         LA    R3,1                MISSING                                      
         LA    R2,CPYTOCNH         CHK FOR INPUT IN "TO" CONTRACT               
         CLI   5(R2),0                                                          
         BE    ERROR               IF MISSING -                                 
         LA    R3,2                INVALID INPUT                                
         CLI   5(R2),3             MAX LENGHT FOR CONTRACT                      
         BH    ERROR                                                            
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
*              BUILD K KEY                                                      
         LA    R3,2                INVALID                                      
         CH    R0,=H'999'          MAX CONTRACT NUMBER IS 999                   
         BH    ERROR                                                            
         STH   R0,HALF                                                          
         LA    R3,52               DUPLICATE KEY ON ADD                         
         MVC   PCONKEY(13),SAVKKEY                                              
         MVC   PCONNUM,HALF                                                     
         MVC   KEY,PCONKEY                                                      
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ERROR                                                            
         LH    R2,HALF                                                          
         BCTR  R2,0                MUST DECREMENT BY ONE                        
         STH   R2,HALF                                                          
         MVC   NEXTNUM,HALF        SAVE AS NEXT AVAIL CON NUM                   
*                                                                               
CON110   DS    0H                                                               
*              TURN OFF VALID BITS IN CASE PREVIOUS ADD ERROR                   
         LA    R2,CPYSDTH          FIRST K FIELD                                
         SR    RE,RE                                                            
         NI    4(R2),X'DF'                                                      
         IC    RE,0(R2)                                                         
         LA    R2,0(RE,R2)         NEXT FIELD                                   
         CLI   0(R2),0             LAST?                                        
         BNE   *-16                                                             
         MVC   PCONKEY(13),SAVKKEY                                              
         MVI   PCONMODN,0          SET MOD NO. TO 0                             
         MVC   PCONMOD,TODAY                                                    
         EJECT                                                                  
*              VALIDATE START-END DATES                                         
EDITCON  DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(X'85',PCONREC)   DELETE ANY X'85' ELEM            
*                                                                               
         LA    R2,CPYSPRH               AUTO SPACE RES FIELD MUST BE            
         CLI   CPYSPR,C'Y'              YES, AUTO SPACE RESV                    
         BE    EDSPR5                                                           
         CLI   CPYSPR,C'N'                                                      
         BE    EDITCTF                                                          
         CLI   5(R2),0                  NO INPUT, TREAT AS 'N'                  
         BE    EDITCTF                                                          
         LA    R3,2                                                             
         B     ERROR                                                            
*                                                                               
*                                                                               
EDSPR5   GOTO1 VDATCON,DMCB,(5,0),(3,SVTODAY)                                   
         B     EDSPR40                                                          
*                                                                               
EDSPR40  LA    R4,WORK                     IF NO X'85' EL EXISTS                
         XC    WORK,WORK                   ADD NEW ONE WITH TODAY IN            
EDSPR42  MVC   WORK(2),=X'850D'            CURRENT DATE                         
         GOTO1 VADDELEM,DMCB,PCONREC,0(R4)                                      
*                                                                               
*                                                                               
*                                                                               
EDITCTF  DS    0H                  EDITING CONTRACT TEL & FAX                   
         LA    R4,PCONREC+33                                                    
EDCTF10  CLI   0(R4),X'55'         TEL & FAX ELEM ALREADY EXIST?                
         BE    EDCTF30             EXIST                                        
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'00'                                                      
         BNE   EDCTF10                                                          
         B     EDCTF50             ELEM NOT FOUND, BUILD NEW ONE                
*                                                                               
EDCTF30  GOTO1 VDELELEM,DMCB,(X'55',PCONREC)                                    
*                                                                               
EDCTF50  DS    0H                                                               
         XC    WORK,WORK           BUILD NEW PCTFELEM (X'55')                   
         LA    R4,WORK                                                          
         USING PCTFELEM,R4                                                      
*                                                                               
         MVI   0(R4),X'55'         ELEM CODE                                    
         MVI   PCTFLEN,34          ELEM LENGTH                                  
*                                                                               
         LA    R2,CPYTELH                                                       
         LA    R3,2                FIELD INVALID ERROR MSG                      
         CLI   5(R2),0                                                          
         BE    EDCTF60                                                          
         CLC   =C'NONE',8(R2)                                                   
         BNE   EDCTF55H                                                         
         CLI   5(R2),4             "NONE" IS 4 CHARS                            
         BNE   ERROR                                                            
         MVC   PCONTELE(4),8(R2)                                                
         B     EDCTF55V                                                         
EDCTF55H CLC   =C'DEFAULT',8(R2)                                                
         BNE   EDCTF55M                                                         
         CLI   5(R2),7             "DEFAULT" IS 7 CHARS                         
         BNE   ERROR                                                            
         MVC   PCONTELE(7),8(R2)                                                
         B     EDCTF55V                                                         
EDCTF55M ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PCONTELE(0),8(R2)                                                
EDCTF55V CLI   CPYFAXH+5,0         FAX FIELD MUST ALSO BE PRESENT               
         BNE   EDCTF60                                                          
         LA    R2,CPYFAXH                                                       
         LA    R3,1                MISSING INPUT ERROR MSG                      
         B     ERROR                                                            
*                                                                               
EDCTF60  DS    0H                                                               
         LA    R2,CPYFAXH                                                       
         LA    R3,2                FIELD INVALID ERROR MSG                      
         CLI   5(R2),0                                                          
         BE    EDCTF70                                                          
         CLC   =C'NONE',8(R2)                                                   
         BNE   EDCTF65H                                                         
         CLI   5(R2),4             "NONE" IS 4 CHARS                            
         BNE   ERROR                                                            
         MVC   PCONFAX(4),8(R2)                                                 
         B     EDCTF65V                                                         
EDCTF65H CLC   =C'DEFAULT',8(R2)                                                
         BNE   EDCTF65M                                                         
         CLI   5(R2),7             "DEFAULT" IS 7 CHARS                         
         BNE   ERROR                                                            
         MVC   PCONFAX(7),8(R2)                                                 
         B     EDCTF65V                                                         
EDCTF65M ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PCONFAX(0),8(R2)                                                 
EDCTF65V CLI   CPYTELH+5,0         FAX FIELD MUST ALSO BE PRESENT               
         BNE   EDCTF70                                                          
         LA    R2,CPYTELH                                                       
         LA    R3,1                MISSING INPUT ERROR MSG                      
         B     ERROR                                                            
*                                                                               
EDCTF70  OC    WORK+2(12),WORK+2   ANYTHING IN THERE?                           
         BZ    EDITATN             NO, EDIT NEXT ITEM                           
*                                                                               
         GOTO1 VADDELEM,DMCB,PCONREC,0(R4)                                      
*                                                                               
*                                                                               
*                                                                               
EDITATN  DS    0H                                                               
         LA    R4,PCONREC+33                                                    
EDATN10  CLI   0(R4),X'50'         DOES ATTENTION OF ELEMENT ALREADY            
         BE    EDATN30             EXIST                                        
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'00'                                                      
         BNE   EDATN10                                                          
*                                                                               
EDATN20  DS    0H                                                               
         LA    R2,CPYATTNH                                                      
         XC    WORK,WORK                BUILD NEW PCATELE X'50' EL              
         LA    R4,WORK                                                          
         USING PCATELEM,R4                                                      
         MVC   WORK(2),=X'502A'                                                 
         CLI   5(R2),0                                                          
         BE    EDATN25                                                          
         MVC   PCATNAM,CPYATTN          ATTENTION OF FIELD                      
********************************************************************            
* IF NOTHING ENTERED IN CD OVERRIDE FIELD ON CON SCREEN ,PCATPCT   *            
* IN X'50' EL WILL BE LEFT X'0000'.  IF 0.0 ENTERED WILL BE SET TO *            
* X'FFFF'.  DONE THIS WAY SO CHECK AT EDATN28 WILL WORK AND        *            
* WILL ELIMINATE ADDING ELEMENT IF NO DATA TO ADD.                 *            
********************************************************************            
EDATN25  LA    R2,CPYCDPH                                                       
         CLI   5(R2),0                                                          
         BE    EDATN28                  CONVERT PERCENT TO BINARY               
         SR    R8,R8                                                            
         IC    R8,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(1,8(R2)),(R8)                                     
         OI    DMCB,0                                                           
         BNZ   EDATERR                                                          
         OC    DMCB+6(2),DMCB+6                                                 
         BNZ   EDATN27                                                          
         MVC   PCATPCT,=2X'FF'                                                  
         B     EDATN28                                                          
EDATN27  MVC   PCATPCT,DMCB+6                                                   
*                                                                               
EDATN28  DS    0H                                                               
         LA    R2,CPYMAXH                                                       
         CLI   5(R2),0                                                          
         BE    EDATN28D                                                         
         BAS   RE,PACK                                                          
         LTR   R0,R0                FIELD NON NUMERIC OR ZERO                   
         BZ    EDATERR              IS INVALID                                  
         STH   R0,HALF                                                          
         MVC   PCATMAX,HALF                                                     
*                                                                               
EDATN28D DS    0H                                                               
         LA    R2,CPYMIZEH       ACROSS ZONES/EDITIONS                          
         CLI   5(R2),0                                                          
         BE    EDATN29                                                          
         BAS   RE,PACK                                                          
         LTR   R0,R0                FIELD NON NUMERIC OR ZERO                   
         BZ    EDATERR              IS INVALID                                  
         STH   R0,HALF                                                          
         MVC   PCATMAXZ,HALF                                                    
*                                                                               
EDATN29  OC    WORK+2(40),WORK+2       CHECK FOR ANY DATA IN ELEM               
         BZ    EDITSDT                                                          
         GOTO1 VADDELEM,DMCB,PCONREC,0(R4)                                      
         B     EDITSDT                                                          
*                                                                               
EDATERR  LA    R3,2                                                             
         B     ERROR                                                            
*                                                                               
EDATN30  MVC   PCATNAM,CPYATTN                                                  
         LA    R2,CPYCDPH                                                       
         CLI   5(R2),0         WAS ANYTHING INPUT IN CD OVERRIDE                
         BNE   EDATN3A                                            BUG01         
         XC    PCATPCT,PCATPCT     CLEAR ANY PREVIOUS NUMBER      BUG01         
         B     EDATN50                                            BUG01         
EDATN3A  SR    R8,R8                                                            
         IC    R8,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(1,8(R2)),(R8)                                     
         OI    DMCB,0                                                           
         BNZ   EDATERR                                                          
         OC    DMCB+6(2),DMCB+6                                                 
         BNZ   EDATN40                                                          
         MVC   PCATPCT,=2X'FF'                                                  
         B     EDATN50                                                          
EDATN40  MVC   PCATPCT,DMCB+6                                                   
*                                                                               
EDATN50  LA    R2,CPYMAXH                                                       
         XC    PCATMAX,PCATMAX                                                  
         CLI   5(R2),0                                                          
         BE    EDATN50D                                                         
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    EDATERR                                                          
         STH   R0,HALF                                                          
         MVC   PCATMAX,HALF                                                     
*                                                                               
EDATN50D DS    0H                                                               
         LA    R2,CPYMIZEH       ACROSS ZONES/EDITIONS                          
         XC    PCATMAXZ,PCATMAXZ                                                
         CLI   5(R2),0                                                          
         BE    EDATN50G                                                         
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    EDATERR                                                          
         STH   R0,HALF                                                          
         MVC   PCATMAXZ,HALF                                                    
*                                                                               
EDATN50G DS    0H                                                               
*                                                                               
EDITSDT  LA    R2,CPYSDTH                                                       
         LA    R3,20               INVALID DATE                                 
         TM    4(R2),X'20'                                                      
         BO    EDITEDT                                                          
         GOTO1 VDATVAL,DMCB,CPYSDT,DUB       START DATE                         
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         NI    CPYEDTH+4,X'DF'     END DATE - REVALIDATE                        
*                                                                               
         GOTO1 VDATCON,DMCB,(0,DUB),(3,PCONSDT)                                 
*                                                                               
         OC    SPUBKILL,SPUBKILL      SEE IF I HAVE A PUB KILL DATE             
         BZ    EDITS5                                                           
*                                                                               
         CLC   PCONSDT,SPUBKILL                                                 
         BNH   EDITS5                                                           
         LA    R3,217          PAST KILL DATE                                   
         B     ERROR                                                            
*                                                                               
EDITS5   DS    0H                                                               
*                                                                               
*                                                                               
*              VALIDATE END DATE                                                
EDITEDT  LA    R2,CPYEDTH                                                       
         LA    R3,20                                                            
         TM    4(R2),X'20'                                                      
         BO    EDITREV                                                          
*              EDIT END DATE                                                    
         GOTO1 VDATVAL,DMCB,CPYEDT,DUB                                          
*                                                                               
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,DUB),(3,PCONEDT)                                 
*                                                                               
         OC    SPUBKILL,SPUBKILL      SEE IF I HAVE A KILL DATE                 
         BZ    EDITE5                                                           
*                                                                               
         CLC   PCONEDT,SPUBKILL                                                 
         BNH   EDITE5                                                           
         LA    R3,217          PAST KILL DATE                                   
         B     ERROR                                                            
*                                                                               
EDITE5   DS    0H                                                               
         LA    R3,80               START DATE LATER THAN END                    
         CLC   PCONSDT,PCONEDT                                                  
         BH    ERROR                                                            
*              TEST FOR K DATE OVERLAP                                          
*                                  NEXTNUM WAS CLEARED AT START                 
         MVC   KEY(13),SAVKKEY     K KEY                                        
         XC    KEY+13(19),KEY+13                                                
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         BAS   RE,HIGH             PRTDIR                                       
         B     *+8                                                              
NEXTK    BAS   RE,SEQ                                                           
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EDT100                                                           
         B     NEXTK1                                                           
*                                                                               
NEXTK1   TM    KEY+25,X'80'        DELETED?                                     
         BO    NEXTK                                                            
         MVC   AREC,APUBIO         READ INTO PUBIO                              
         BAS   RE,GETREC           PRTFILE - K                                  
         LA    RF,IOAREA           RESTORE AREC                                 
         ST    RF,AREC                                                          
         L     R5,APUBIO                                                        
         MVI   0(R5),0                                                          
         CLI   50(R5),C'A'      IS THIS A PRD-CONTRACT                          
         BL    NEXTK2              NO - MUST CHECK FOR OVERLAP                  
         CLI   SAVPRD,0                                                         
         BE    NEXTK2                                                           
         CLC   SAVPRD,50(R5)          MATCH PRDS                                
         BNE   NEXTK               CHECK FOR OVERLAP ON MATCH                   
*                                                                               
NEXTK2   DS    0H                                                               
         LA    R3,49               K DATE OVERLAP                               
         CLC   PCONSDT,38(R5)   NEW K START V EXISTING K END                    
         BH    NEXTK                                                            
         CLC   PCONEDT,35(R5)   NEW K END V EXISTING K START                    
         BL    NEXTK                                                            
         B     ERROR                                                            
*              TEST IF K PERIOD GREATER THAN YEAR                               
EDT100   LA    R3,20               INVALID DATE                                 
         NI    DMINBTS,X'F7'       ELIM. PASS DELETES                           
         B     EDT110              ** NOP YEAR TEST **                          
****************                                                                
*****    GOTO1 VDTCNV,DMCB,(1,PCONSDT),WORK                                     
*****    GOTO1 VADDAY,DMCB,WORK,WORK+6,366                                      
*****    CLC   DUB(6),WORK+6       K END V K START + 366 DAYS                   
*****    BH    ERROR                                                            
EDT110   DS    0H                                                               
*                                                                               
         B     EDITREV                                                          
         EJECT                                                                  
*              EDIT REVISION DATE                                               
EDITREV  LA    R2,CPYREQH                                                       
         TM    4(R2),X'20'                                                      
         BO    EDITRV                                                           
* MOVE AUTHORIZED SIGNATURE                                                     
         BAS   RE,MOVE                                                          
         MVC   PCONREQ,WORK                                                     
EDITRV   LA    R2,CPYREVH                                                       
         LA    R3,20               INVALID DATE                                 
         TM    4(R2),X'20'                                                      
         BO    EDITCV                                                           
         XC    PCONREV,PCONREV                                                  
         CLI   5(R2),0                                                          
         BE    EDITCV                                                           
         GOTO1 VDATVAL,DMCB,CPYREV,DUB                                          
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,DUB),(3,PCONREV)                                 
*                                                                               
*              VALIDATE CONTRIBUTING VOLUME                                     
EDITCV   LA    R2,CPYCONH                                                       
         LA    R3,2                INVALID INPUT FIELD                          
         TM    4(R2),X'20'                                                      
         BO    EDITTYP                                                          
         ZAP   PCONCON,=P'0'                                                    
         CLI   5(R2),0                                                          
         BE    EDITTYP                                                          
*                                                                               
         SR    R8,R8                                                            
         IC    R8,5(R2)            INPUT LENGTH                                 
         GOTO1 VCASHVAL,DMCB,CPYCON,(R8)                                        
         CLI   DMCB,0              ERROR?                                       
         BNE   ERROR                                                            
         L     R1,DMCB+4           CONTRIB VOLUME                               
         LTR   R1,R1                                                            
         BM    ERROR                                                            
         SR    R0,R0                                                            
         D     R0,=F'100'          ELIM DECIMALS                                
         CVD   R1,DUB                                                           
*                                                                               
         MVC   PCONCON,DUB+3                                                    
*                                                                               
*              VALIDATE TYPE                                                    
EDITTYP  LA    R2,CPYTYPH                                                       
         LA    R3,2                                                             
         TM    4(R2),X'20'                                                      
         BO    EDITRATE                                                         
         MVC   PCONTYP,CPYTYP                                                   
         CLI   8(R2),C'M'          MASTER?                                      
         BE    EDITRATE                                                         
         CLI   8(R2),C'R'          RESERVATION                                  
         BE    EDITRATE                                                         
         CLI   8(R2),X'40'                                                      
         BE    EDITRATE                                                         
         CLI   8(R2),0                                                          
         BNE   ERROR                                                            
         TITLE 'T40D50 - PRINTPAK CONTRACT RATE BASIS LINE EDIT'                
*                                                                               
EDITRATE DS    0H              SEE IF COPYING RATES                             
         MVC   DELCODES(4),=X'2021222400' TABLE OF ELEMNETS CODES               
*                            X'24' IS OPEN                                      
*                                         TO BE DELETED                         
         LA    R2,CPYCPYRH                                                      
         CLI   5(R2),0                                                          
         BE    EDITRMIS            NO INPUT - SEND MISSING ERROR                
         CLI   8(R2),C'N'          N - MEANS DELETE ALL                         
         BE    EDITRDEL                                                         
         MVC   DELCODES(5),=X'2122000000'                                       
         CLI   8(R2),C'Y'          Y - MEANS ONLY COPY CURRENT+OPEN             
         BE    EDITRDEL                                                         
         MVC   DELCODES(5),=X'2200000000'                                       
         CLI   8(R2),C'L'          L - MEANS ONLY DELETE HIGHER                 
         BE    EDITRDEL                                                         
         MVC   DELCODES(5),=X'2100000000'                                       
         CLI   8(R2),C'H'          H - MEANS ONLY DELETE LOWER                  
         BE    EDITRDEL                                                         
         CLI   8(R2),C'B'          B- MEANS DELETE NONE                         
         BE    EDITSTD             (COPY CURRENT,HIGHER,LOWER,OPEN)             
         LA    R3,2                INVALID                                      
         B     ERROR                                                            
*                                                                               
EDITRDEL DS    0H    DELETE ALL ELEMENTS WITH CODES IN DELCODES                 
         LA    R3,DELCODES                                                      
EDITRD3  CLI   0(R3),0    END OF CODES                                          
         BE    EDITRDX                                                          
         CLI   0(R3),X'20'                                                      
         BNE   EDITRD10                                                         
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'20',PCONREC)                                    
         B     EDITRD30                                                         
*                                                                               
EDITRD10 CLI   0(R3),X'21'                                                      
         BNE   EDITRD15                                                         
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'21',PCONREC)                                    
         B     EDITRD30                                                         
*                                                                               
EDITRD15 CLI   0(R3),X'22'                                                      
         BNE   EDITRD17                                                         
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'22',PCONREC)                                    
         B     EDITRD30                                                         
*                                                                               
EDITRD17 CLI   0(R3),X'24'                                                      
         BNE   EDITRD20                                                         
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'24',PCONREC)                                    
         B     EDITRD30                                                         
*                                                                               
EDITRD20 DC    H'0'         SOMETHING SCREWY                                    
*                                                                               
EDITRD30 LA    R3,1(R3)                                                         
         B     EDITRD3                                                          
*                                                                               
EDITRMIS LA    R3,1          RATE OPTION IS REQUIRED                            
         B     ERROR                                                            
*                                                                               
EDITRDX  DS    0H                                                               
*                                                                               
         TITLE 'T40D50 - PRINTPAK CONTRACT STANDARD COMMENT EDIT'               
*              VALIDATE STANDARD COMMENTS                                       
EDITSTD  LA    R2,CPYSTDH                                                       
         LA    R3,36               INVALID COMMENT                              
*                                                                               
         LA    R4,CPYSTD           STD COMMENTS                                 
         TM    4(R2),X'20'                                                      
         BO    COM100                                                           
         SR    R8,R8                                                            
         IC    R8,5(R2)            INPUT LEN                                    
*              DELETE PREVIOUS STANDARD COMMENT ELEMENTS                        
         GOTO1 VDELELEM,DMCB,(X'30',PCONREC)                                    
         CLI   5(R2),0             INPUT?                                       
         BE    COM100                                                           
*                                                                               
         MVC   WORK2(2),=X'3008'   ELEM CODE + LEN                              
         LA    R8,8(R8,R2)         INPUT END                                    
COM5     SR    R9,R9                                                            
         LR    R5,R4                                                            
*                                                                               
COM10    CLI   0(R4),C','          STOP CHAR                                    
         BE    COM25                                                            
         LA    R9,1(R9)                                                         
         LA    R4,1(R4)                                                         
         CR    R4,R8               LAST CHAR INPUT?                             
         BL    COM10                                                            
*                                                                               
COM25    MVC   WORK2+2(6),SPACES                                                
         LTR   R9,R9                                                            
         BZ    ERROR                                                            
         LA    RE,6                                                             
         SR    RE,R9                                                            
         LA    RE,WORK2+2(RE)      TO RIGHT ALIGN                               
         BCTR  R9,R0                                                            
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R5)                                                    
         CH    R9,=H'5'                                                         
         BH    ERROR                                                            
*              VALIDATE STANDARD COMMENT                                        
         XC    IOAREA(32),IOAREA                                                
         MVC   PCOMKAGY,PCONKAGY                                                
         MVC   PCOMKMED,KBAMED                                                  
         MVI   PCOMKRCD,X'40'      REC CODE                                     
         MVC   PCOMKNUM,WORK2+2    NUMBER                                       
         MVC   KEY,IOAREA                                                       
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         MVC   WORK2+2(6),PCOMKNUM                                              
*              ADD STD COMMENT ELEMENT                                          
         GOTO1 VADDELEM,DMCB,PCONREC,WORK2                                      
         CLI   DMCB,X'FF'                                                       
         BE    OVFERR                                                           
*                                                                               
         CLI   0(R4),C','          COMMA STOP?                                  
         BNE   COM100                                                           
         LA    R4,1(R4)            NEXT                                         
         B     COM5                                                             
         TITLE 'T40D50 - PRINTPAK CONTRACT SPECIAL COMMENTS EDIT'               
*              VALIDATE SPECIAL COMMENTS FOR THIS CONTRACT                      
COM100   LA    R2,CPYCM1H                                                       
*              TEST FOR CHANGES                                                 
         SR    R8,R8                                                            
COM150   TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BZ    COM200                                                           
         IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)         NEXT FIELD                                   
         CLI   0(R2),0             LAST?                                        
         BE    COM400                                                           
         B     COM150                                                           
*              EDIT SPECIAL COMMENTS                                            
COM200   LA    R2,CPYCM1H                                                       
         LA    R3,36               INVALID COMMENT                              
*                                                                               
*              ELIMINATE PREVIOUS COMMENTS                                      
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'40',PCONREC)                                    
         MVI   WORK2,X'40'         ELEM CODE                                    
*                                                                               
COM250   CLI   5(R2),0             LEN                                          
         BE    COM300                                                           
         IC    R8,5(R2)            INPUT LEN                                    
         BCTR  R8,R0                                                            
         EX    R8,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SPACES                                                   
         BE    COM300                                                           
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+2(0),8(R2)                                                 
         LA    R8,3(R8)                                                         
         LA    RE,WORK2-1(R8)                                                   
* ELIMINATE TRAILING BLANKS                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  R8,R0                                                            
         BCT   RE,*-10                                                          
         STC   R8,WORK2+1          ELEM LEN                                     
         LA    R3,223                                                           
         CLC   WORK2+2(3),=C'RC='                                               
         BNE   *+12                                                             
         CLI   WORK2+1,35                                                       
         BH    ERROR                                                            
         LA    R3,222              SPECIAL COMMENT ERROR                        
         CLC   WORK2+2(2),=C'E+'                                                
         BE    *+14                                                             
         CLC   WORK2+2(2),=C'E-'   SPECIAL ESTIMATE ONLY COMMENT?               
         BNE   COM275                                                           
         CLI   WORK2+1,32                                                       
         BH    ERROR                                                            
COM275   DS    0H                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,PCONREC,WORK2                                      
         CLI   DMCB,X'FF'                                                       
         BE    OVFERR                                                           
*                                                                               
COM300   IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)         NEXT FIELD                                   
         CLI   0(R2),0             LAST?                                        
         BNE   COM250                                                           
*                                                                               
COM400   DS    0H                                                               
*                                  NOW ADD CONTRACT                             
*                                  AND SET PUT NEW NUMBER IN KBANUM             
         LA    R0,PCONREC                                                       
         ST    R0,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(25),PCONREC                                                  
         BAS   RE,ADDREC                                                        
         MVC   SAVKKEY+27(4),KEY                                                
         XC    KBANUM,KBANUM                                                    
         EDIT  (2,PCONNUM),(3,KBANUM),ALIGN=LEFT                                
         FOUT  KBANUMH                                                          
         NI    KBANUMH+4,X'DF'     UNVALIDATE NUMBER                            
*                                  UPON RETURN TO BASE                          
*                                  THE CONTRACT JUST COPIED WILL                
*                                  BE DISPLAYED BY T40D20                       
         B     EXXMOD              FINIS                                        
         SPACE 3                                                                
*                                                                               
OVFERR   DS    0H                                                               
         LA    R3,208              RECORD OVERFLOW                              
         B     ERROR                                                            
*                                                                               
SVTODAY  DS    CL3                                                              
MYTEMP   DS    CL20                                                             
*                                                                               
LEVADDR  DS    F                                                                
RATADDR  DS    F                                                                
         EJECT                                                                  
*                                                                               
DELCODES DC    5X'00'                                                           
*                                                                               
       ++INCLUDE PPGENEROL         IN-LINE CODES                                
*                                                                               
         LTORG                                                                  
SPACES   DC    CL70' '                                                          
PATCH    DS    CL30                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PPCONWRK                                                       
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019PPCON50   11/05/03'                                      
         END                                                                    
