*          DATA SET CTGEN0E    AT LEVEL 031 AS OF 05/01/02                      
*PHASE TA0B0EA                                                                  
         TITLE 'CTGEN0E - NARRATIVE RECORD MAINT'                               
*                                                                               
* JMUM 028 25JAN95 - ADD MEFF TO MTYPTAB (FOOTLINES)                            
* TGUT 027 01DEC93 - ADD MEOO TO VALID TYPES (PROGRAMS) FOR MEDIA               
* TGUT 026 15NOV93 - ADD MEOD TO VALID TYPES (PROGRAMS) FOR MEDIA               
*                                                                               
GEN0E    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GENE**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GNARD,R2            R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELREC                                                           
         B     RESREC                                                           
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF NARRATIVE RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   EQU   *                                                                
         LA    R2,APRECKEY                                                      
         XC    APRECKEY,APRECKEY                                                
         MVI   GNKREC,GNKRECQ                                                   
         MVI   GNKLANG,LANGEUK                                                  
*                                                                               
VKUSR    EQU   *                                                                
         MVI   FVMINL,3                                                         
         GOTO1 AFVAL,NARUSERH      * VALIDATE USER *                            
         BNE   VALKEYX                                                          
         CLC   FVIFLD(10),=C'ALL       '                                        
         BE    VKUSRX                                                           
         LA    R4,IOKEY                                                         
         USING CTIREC,R4           BUILD AN ID KEY                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIREC,C'I'                                                      
         MVC   CTIKID,FVIFLD                                                    
         LA    R1,IORD+IOCONFIL+IO2                                             
         GOTO1 AIO                 READ ID RECORD                               
         BNE   VALKEYNV            RECORD MUST BE PRESENT & CORRECT             
         L     R4,AIOAREA2                                                      
         LA    R3,CTIDATA                                                       
         DROP  R4                                                               
         SR    RF,RF                                                            
VKUSR1   CLI   0(R3),0             LOOK FOR ID# ELEMENT                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VKUSR1                                                           
         MVC   GNKAGY,2(R3)                                                     
         MVC   NARUSER,FVIFLD                                                   
VKUSRX   CLC   CUUSER,GNKAGY       TEST USER IS SAME AGY AS CONNECT             
         BE    VKUSRX1                                                          
         TM    CUSTAT,CUSDDS       ONLY AVAILABLE FOR DDS                       
         BNO   VALKEYNV                                                         
VKUSRX1  OI    NARUSERH+6,FVOXMT                                                
*                                                                               
VKSYS    EQU   *                   * VALIDATE SYSTEM *                          
         MVI   FVMINL,1            MANDATORY                                    
         GOTO1 AFVAL,NARSYSH                                                    
         BNE   VALKEYX                                                          
         LA    RE,SYSTAB           CHECK IN TABLE OF VALID SYSTEMS              
         ZIC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
VKSYS1   CLI   0(RE),X'FF'                                                      
         BE    VALKEYNV                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),1(RE)                                                  
         BE    VKSYS2                                                           
         LA    RE,L'SYSTAB(,RE)                                                 
         B     VKSYS1                                                           
VKSYS2   MVC   GNKSYS,0(RE)        OK - PLUG INTO RECORD KEY                    
         MVC   NARSYS,1(RE)             & REDISPLAY                             
         OI    NARSYSH+6,FVOXMT                                                 
VKSYSX   EQU   *                                                                
*                                                                               
VKTYP    EQU   *            * VALIDATE TYPE & MEDIA ACCORDING TO SYSTEM         
         ZIC   RF,L'SYSTAB-1(RE)   (RE STILL -> SYSTAB ENTRY)                   
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     VKMED                                                            
         B     VKACC                                                            
*                                                                               
VKMED    EQU   *                   SYSTEM=MEDIA                                 
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,NARTYPH                                                    
         BNE   VALKEYX                                                          
         LA    RE,MTYPTAB          - TYPES VALID FOR MEDIA SYSTEM               
         ZIC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
VKMED1   CLI   0(RE),X'FF'                                                      
         BE    VALKEYNV                                                         
         CLC   FVIFLD(2),0(RE)                                                  
         BE    VKMED2                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),2(RE)                                                  
         BE    VKMED2                                                           
         LA    RE,L'MTYPTAB(,RE)                                                
         B     VKMED1                                                           
VKMED2   MVC   GNKTYP,0(RE)        OK - PLUG INTO RECORD KEY                    
         MVC   NARTYP,2(RE)             & REDISPLAY                             
         OI    NARTYPH+6,FVOXMT                                                 
*                                                                               
         MVI   FVMINL,1            VALIDATE MEDIA                               
         GOTO1 AFVAL,NARMEDH                                                    
         BNE   VALKEYX                                                          
         TM    FVIIND,FVIALF+FVINUM  - ALPHA OR NUMERIC                         
         BZ    VALKEYNV                                                         
         MVC   GNKMED,FVIFLD       OK - PLUG INTO RECORD KEY                    
         B     VKTYPX                                                           
         SPACE 2                                                                
VKACC    EQU   *                   SYSTEM=ACCOUNTING                            
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,NARTYPH                                                    
         BNE   VALKEYX                                                          
         LA    RE,ATYPTAB          - TYPES VALID FOR ACC SYSTEM                 
         ZIC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
VKACC1   CLI   0(RE),X'FF'                                                      
         BE    VALKEYNV                                                         
         CLC   FVIFLD(3),0(RE)                                                  
         BE    VKACC2                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),3(RE)                                                  
         BE    VKACC2                                                           
         LA    RE,L'ATYPTAB(,RE)                                                
         B     VKACC1                                                           
VKACC2   MVC   GNKATYP,0(RE)       OK - PLUG INTO RECORD KEY                    
         MVC   NARTYP,3(RE)             & REDISPLAY                             
         OI    NARTYPH+6,FVOXMT                                                 
*                                  VALIDATE MEDIA                               
         GOTO1 AFVAL,NARMEDH                                                    
         BE    VALKEYNV            - NOT VALID FOR ACC SYSTEM                   
*                                                                               
VKTYPX   EQU   *                                                                
*                                                                               
VKCLI    EQU   *                   * VALIDATE CLIENT *                          
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,NARCLIH                                                    
         BNE   VALKEYX                                                          
         CLC   FVIFLD(5),=C'ALLE ' SET ALLE TO ALL                              
         BNE   *+10                                                             
         MVC   FVIFLD(5),=C'ALL  '                                              
         CLC   FVIFLD(5),=C'ALL  '                                              
         BE    VKCLIX                                                           
         OC    GNKAGY,GNKAGY                                                    
         BZ    VALKEYNV            NOT VALID IF USER=ALL                        
         MVC   GNKCLI,FVIFLD                                                    
VKCLIX   MVC   NARCLI,FVIFLD                                                    
         OI    NARCLIH+6,FVOXMT                                                 
         SPACE 2                                                                
         MVC   IOKEY,GNKEY                                                      
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BH    VKDEL               NRF - CHECK IF DELETED                       
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         CLI   UPDATE,C'N'                                                      
         BNE   *+8                                                              
         MVI   APINDS,APIOKDIS                                                  
         B     VKGET                                                            
VKDEL    TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BNZ   VKDEL1                                                           
         MVI   APINDS,APIOKADD     - NOT DEL THEREFORE NO REC                   
         CLI   UPDATE,C'N'                                                      
         BE    VALKEYX             CAN'T ADD IF UPDATE = NO                     
         B     VALKEYY                                                          
VKDEL1   MVI   APINDS,APIOKDIS+APIOKRES                                         
         CLI   UPDATE,C'N'                                                      
         BNE   *+8                                                              
         MVI   APINDS,APIOKDIS                                                  
*                                                                               
VKGET    LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET RECORD                                   
         TM    IOERR,IOERRS-IOEDEL DELETED IS ONLY SAFE ERR                     
         BZ    *+6                                                              
         DC    H'0'                ERROR ON GET OF D/A RECORD                   
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
*                                                                               
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
*                                                                               
VALKEYNV MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A NARRATIVE RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         XC    APCURSOR,APCURSOR                                                
         XC    CHG,CHG                                                          
         CLI   APACTN,ACTADD                                                    
         BNE   VRDSC                                                            
         MVC   GNKEY,APRECKEY                                                   
         MVC   GNFLEN,=AL2(GNFIRST)                                             
         XC    GNFSTAT(GNFIRST-GNKEYL),GNFSTAT                                  
*                                                                               
VRDSC    EQU   *                   * VALIDATE DESCRIPTION *                     
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,NARDESCH                                                   
         BNE   VALRECX                                                          
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GNDSCELQ     GET DESCRIPTION ELEM                         
         CLI   APACTN,ACTADD                                                    
         BE    VRDSC1                                                           
         GOTO1 AGETELS,GNARD                                                    
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    VRDSC1                                                           
*                                                                               
         USING GNDSCD,R3                                                        
*                                                                               
         ZIC   RF,FVILEN                                                        
         ZIC   RE,GNDSCLEN                                                      
         SH    RE,=AL2(GNDSCFXL)                                                
         CR    RE,RF                                                            
         BNE   VRDSC0                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),GNDESC    COMPARE DESCRIPTION                          
         BE    VRDSCX                                                           
VRDSC0   GOTO1 ADELELS,GNARD       DELETE OLD DESC ELEM                         
VRDSC1   LA    R3,APELEM           BUILD NEW DESC ELEM                          
         ZIC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   GNDESC(0),FVIFLD    - DESCRIPTION                                
         LA    RF,GNDSCFXL+1(,RF)                                               
         STC   RF,GNDSCLEN         - ELEMENT LENGTH                             
         OI    CHG,1               INDICATE RECORD HAS CHANGED                  
         GOTO1 AADDELS,GNARD       AND ADD NEW DESC ELEM                        
VRDSCX   OI    NARDESCH+6,FVOXMT                                                
*                                                                               
VRDATA   EQU   *                   * VALIDATE DATA LINES *                      
         LA    R4,NARHDRH                                                       
         MVI   SEQ,1                                                            
         XC    ELEM,ELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GNDATELQ     GET DATA ELEMS                               
         CLI   APACTN,ACTADD                                                    
         BE    VRDATA20                                                         
VRDATA10 MVI   APELEM+1,1                                                       
         MVC   APELEM+2(1),SEQ                                                  
         XC    ELEM,ELEM                                                        
         GOTO1 AGETELS,GNARD                                                    
         ICM   R3,15,APPARM                                                     
         BZ    *+8                                                              
         MVI   ELEM,GOT1                                                        
*                                                                               
         USING GNDATD,R3                                                        
         USING IPLINE,R4                                                        
*                                                                               
VRDATA20 EQU   *                                                                
         GOTO1 AFVAL,IPDATAH       ANY DATA ?                                   
         BNE   VRDATA50                                                         
         CLI   IPSTDH+5,0          YES, IGNORE STD/BLANK                        
         BE    VRDATA30                                                         
         XC    IPSTD,IPSTD                                                      
         OI    IPSTDH+6,FVOXMT                                                  
VRDATA30 CLI   APACTN,ACTADD                                                    
         BE    VRDATA40                                                         
         TM    ELEM,GOT1           GOT AN ELEMENT?                              
         BZ    VRDATA40                                                         
         ZIC   RF,FVILEN                                                        
         ZIC   RE,GNDATLEN                                                      
         SH    RE,=AL2(GNDATFXL)                                                
         CR    RE,RF                                                            
         BNE   VRDATA35                                                         
         BCTR  RF,0                CHECK IF DATA CHANGED                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),GNDATA                                                 
         BE    VRDATA80            (NO CHG, GOTO NEXT LINE)                     
VRDATA35 GOTO1 ADELELS,GNARD       DELETE OLD DATA ELEM                         
VRDATA40 EQU   *                                                                
         LA    R3,APELEM           BUILD NEW DATA ELEM                          
         ZIC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   GNDATA(0),FVIFLD    - DATA LINE                                  
         LA    RF,GNDATFXL+1(,RF)                                               
         STC   RF,GNDATLEN         - ELEMENT LENGTH                             
         MVC   GNDATSEQ,SEQ        - SEQ NO                                     
         MVC   GNDATHDR,IPHDR      - LINE HEADER                                
         OI    CHG,1               INDICATE RECORD HAS CHANGED                  
         GOTO1 AADDELS,GNARD       AND ADD NEW DATA ELEM                        
         MVC   IPDATA,FVIFLD                                                    
         OI    IPDATAH+6,FVOXMT                                                 
         B     VRDATA80                                                         
*                                                                               
VRDATA50 EQU   *                   NO DATA ENTERED                              
         OI    IPSTDH+6,FVOXMT                                                  
         GOTO1 AFVAL,IPSTDH        IF NOTHING IN STD FLD                        
         BE    VRDATA52                                                         
         MVC   IPSTD,=C'BLA'          DEFAULT TO BLANK                          
         B     VRDATA65                                                         
VRDATA52 ZIC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   IPSTD(0),=C'STD'                                                 
         BNE   VRDATA60                                                         
         CLC   NARUSER(10),=C'ALL       '  STD NOT VALID IF USER=ALL            
         BNE   VRDATA55                                                         
         MVC   IPSTD,=C'BLA'       - CHANGE IT TO BLANK                         
         B     VRDATA65                                                         
VRDATA55 CLI   APACTN,ACTADD       IF ADD+STD                                   
         BE    VRDATA80               THERES NOWT TO DO                         
         TM    ELEM,GOT1           IF NO ELEM EXISTS                            
         BZ    VRDATA80               THERES NOWT TO DO                         
         GOTO1 ADELELS,GNARD       DEL OLD ELEM FOR "STD"                       
         OI    CHG,1               SET RECORD CHANGED                           
         B     VRDATA80                                                         
*                                                                               
VRDATA60 EX    RF,*+8              BLANK LINE?                                  
         B     *+10                                                             
         CLC   IPSTD(0),=C'BLA'                                                 
         BE    VRDATA65                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
VRDATA65 CLI   APACTN,ACTADD                                                    
         BE    VRDATA70                                                         
         TM    ELEM,GOT1                                                        
         BZ    VRDATA70                                                         
         CLI   GNDATLEN,GNDATFXL                                                
         BE    VRDATA80                                                         
         GOTO1 ADELELS,GNARD       DELETE OLD DATA ELEM                         
VRDATA70 EQU   *                                                                
         LA    R3,APELEM           BUILD NEW DATA ELEM                          
         MVI   GNDATLEN,GNDATFXL                                                
         MVC   GNDATSEQ,SEQ                                                     
         MVC   GNDATHDR,IPHDR                                                   
         OI    CHG,1               INDICATE RECORD HAS CHANGED                  
         GOTO1 AADDELS,GNARD       AND ADD NEW DATA ELEM                        
*                                                                               
VRDATA80 LA    R4,IPLINEL(,R4)     -> NEXT I/P LINE                             
         CLI   SEQ,12              ALL DUN?                                     
         BNL   VRUPD                                                            
         ZIC   RF,SEQ              UPDATE SEQ NO                                
         LA    RF,1(,RF)                                                        
         STC   RF,SEQ                                                           
         B     VRDATA10            GO GET NEXT ELEM                             
*                                                                               
*                                                                               
VRUPD    EQU   *                   * UPDATE RECORD AS NECESSARY *               
         OC    CHG,CHG             ANY CHANGES MADE?                            
         BNZ   VRUPD1                                                           
         MVC   FVMSGNO,=AL2(INFCHA1) NO CHANGES MADE                            
         MVI   FVOMTYP,C'I'            REDISPLAY "ENTER CHANGES"                
         MVI   APMODE,APMFMOK                                                   
         LA    RF,NARDESCH                                                      
         ST    RF,APCURSOR                                                      
         B     VALRECX                                                          
*                                                                               
VRUPD1   GOTO1 ASETACT,GNARD       UPDATE ACTIVITY ELEMENT                      
*                                                                               
         LA    R1,IOADD+IOGENFIL+IO1   UPDATE GENFIL                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   APACTN,ACTADD       DON'T UPDATE GENDIR ON ADD                   
         BE    VALRECX                                                          
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF NARRATIVE RECORD                                    
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
*                                                                               
DKUSR    EQU   *                   * DISPLAY USER ID *                          
         OC    GNKAGY,GNKAGY                                                    
         BNZ   DKUSR2                                                           
         MVC   NARUSER(10),=C'ALL       '                                       
         B     DKUSRX                                                           
DKUSR2   LA    R4,IOKEY                                                         
         USING CTIREC,R4           BUILD AN ID KEY                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIREC,C'I'                                                      
         MVC   CTIKID+8(2),GNKAGY                                               
         LA    R1,IORD+IOCONFIL+IO2                                             
         GOTO1 AIO                 READ ID RECORD                               
         BE    DKUSR4                                                           
         MVC   NARUSER(3),=C'*?*'                                               
         B     DKUSRX                                                           
DKUSR4   L     R4,AIOAREA2                                                      
         LA    R3,CTIDATA                                                       
         DROP  R4                                                               
         SR    RF,RF                                                            
DKUSR6   CLI   0(R3),0             LOOK FOR ID NAME ELEMENT                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DKUSR6                                                           
         MVC   NARUSER,2(R3)                                                    
DKUSRX   EQU   *                                                                
*                                                                               
DKSYS    LA    RE,SYSTAB           * DISPLAY SYSTEM *                           
DKSYS2   CLI   0(RE),X'FF'                                                      
         BNE   DKSYS4                                                           
         MVC   NARSYS(1),GNKSYS    (NOT IN SYSTEM TABLE)                        
         MVC   NARTYP(3),GNKTYP    (SO CANT DO TYPE/MEDIA EITHER)               
         B     DKTYPX                                                           
DKSYS4   CLC   GNKSYS,0(RE)                                                     
         BE    DKSYS6                                                           
         LA    RE,L'SYSTAB(,RE)                                                 
         B     DKSYS2                                                           
DKSYS6   MVC   NARSYS,1(RE)                                                     
DKSYSX   EQU   *                                                                
*                                                                               
DKTYP    EQU   *            * DISPLAY TYPE & MEDIA ACCORDING TO SYSTEM          
         ZIC   RF,L'SYSTAB-1(RE)   (RE STILL -> SYSTAB ENTRY)                   
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     DKMED                                                            
         B     DKACC                                                            
*                                                                               
DKMED    EQU   *                   SYSTEM=MEDIA                                 
         LA    RE,MTYPTAB                                                       
DKMED10  CLI   0(RE),X'FF'                                                      
         BNE   DKMED20                                                          
         MVC   NARTYP(2),GNKTYP                                                 
         B     DKMED40                                                          
DKMED20  CLC   GNKTYP,0(RE)                                                     
         BE    DKMED30                                                          
         LA    RE,L'MTYPTAB(,RE)                                                
         B     DKMED10                                                          
DKMED30  MVC   NARTYP,2(RE)                                                     
DKMED40  MVC   NARMED,GNKMED                                                    
DKMEDX   B     DKTYPX                                                           
*                                                                               
DKACC    EQU   *                   SYSTEM=ACCOUNTING                            
         LA    RE,ATYPTAB                                                       
DKACC2   CLI   0(RE),X'FF'                                                      
         BNE   DKACC4                                                           
         MVC   NARTYP(3),GNKATYP                                                
         B     DKACCX                                                           
DKACC4   CLC   GNKATYP,0(RE)                                                    
         BE    DKACC6                                                           
         LA    RE,L'ATYPTAB(,RE)                                                
         B     DKACC2                                                           
DKACC6   MVC   NARTYP,3(RE)                                                     
DKACCX   EQU   *                                                                
*                                                                               
DKTYPX   EQU   *                                                                
*                                                                               
DKCLI    EQU   *                   * DISPLAY CLIENT                             
         OC    GNKCLI,GNKCLI                                                    
         BNZ   DKCLI1                                                           
         MVC   NARCLI(5),=C'ALL  '                                              
         B     DKCLIX                                                           
DKCLI1   MVC   NARCLI,GNKCLI                                                    
DKCLIX   EQU   *                                                                
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY NARRATIVE RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC NARDESCH                                                         
*                                                                               
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GNDSCELQ     GET DESCRIPTION ELEM                         
         GOTO1 AGETELS,GNARD                                                    
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    DRDATA                                                           
*                                                                               
         USING GNDSCD,R3                                                        
*                                                                               
         ZIC   RF,GNDSCLEN                                                      
         SH    RF,=Y(GNDSCFXL+1)                                                
         BM    DRDATA                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NARDESC(0),GNDESC   DISPLAY DESCRIPTION                          
*                                                                               
         USING GNDATD,R3                                                        
*                                                                               
DRDATA   EQU   *                   * DISPLAY DATA LINES *                       
         LA    R4,NARHDRH                                                       
         MVI   SEQ,1                                                            
         XC    ELEM,ELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GNDATELQ     GET DATA ELEMS                               
         MVI   APELEM+1,1                                                       
DRDATA10 MVC   APELEM+2(1),SEQ                                                  
         XC    ELEM,ELEM                                                        
         GOTO1 AGETELS,GNARD                                                    
         ICM   R3,15,APPARM                                                     
         BNZ   DRDATA20                                                         
*                                                                               
         USING GNDATD,R3                                                        
         USING IPLINE,R4                                                        
*                                                                               
         MVC   IPSTD,=C'STD'       (NO ELEM SO MUST BE STD LINE)                
         B     DRDATA30                                                         
DRDATA20 ZIC   RF,GNDATLEN         MOVE DATA LINE TO SCREEN                     
         SH    RF,=Y(GNDATFXL+1)                                                
         BNM   DRDATA25                                                         
         MVC   IPSTD,=C'BLA'       (NO DATA MEANS BLANK LINE)                   
         B     DRDATA30                                                         
DRDATA25 EX    RF,*+8                                                           
         B     DRDATA30                                                         
         MVC   IPDATA(0),GNDATA                                                 
DRDATA30 LA    R4,IPLINEL(,R4)     -> NEXT LINE ON SCREEN                       
         CLI   SEQ,12              FINISHED?                                    
         BNL   DRDATAX                                                          
         ZIC   RF,SEQ              UPDATE ELEM SEQ NO                           
         LA    RF,1(,RF)                                                        
         STC   RF,SEQ                                                           
         B     DRDATA10            GO GET NEXT ELEM                             
*                                                                               
DRDATAX  CLI   APACTN,ACTRES                                                    
         BNE   DISRECX                                                          
         OI    GENACTH+6,X'01'     REMODIFY ACTION                              
*                                                                               
DISRECX  EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A NARRATIVE RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
DELREC   LA    R2,IOKEY                                                         
         OI    GNDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GNARD                                                    
         OI    GNFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED NARRATIVE RECORD                       *         
***********************************************************************         
         SPACE 1                                                                
RESREC   LA    R2,IOKEY                                                         
         NI    GNDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GNARD                                                    
         NI    GNFSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY                                                      
         XC    GNKEY,GNKEY                                                      
         MVI   GNKREC,X'FF'        FLAG FOR FIRST PASS                          
         XC    SELKEY,SELKEY                                                    
*                                                                               
         LA    R8,LSTUSERH                                                      
         BAS   RE,VALPARS          GO VALIDATE I/P PARAMETERS                   
*                                  (ONLY RETURNS HERE IF ALL VALID)             
*                                                                               
         CLI   SELUSER,X'FF'       BUILD AN INITIAL KEY                         
         BE    *+10                                                             
         MVC   GNKAGY,SELUSER                                                   
         MVC   GNKSYS,SELSYS                                                    
         MVC   GNKATYP,SELTYP                                                   
         CLI   SELCLI,X'FF'                                                     
         BE    *+10                                                             
         MVC   GNKCLI,SELCLI                                                    
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY                                                         
         MVC   GNKEY,APRECKEY                                                   
         CLI   GNKREC,X'FF'        TEST FIRST TIME FLAG                         
         BNE   GSGREC1                                                          
         MVI   GNKREC,GNKRECQ                                                   
         B     GSGREC3                                                          
GSGREC1  TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BZ    GSGREC2                                                          
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    GSGREC4                                                          
         B     GETSELN                                                          
GSGREC2  TM    APINDS,APILNSEQ     TEST READ OR READ HIGH                       
         BNZ   GSGREC4                                                          
GSGREC3  LA    R1,IOGENDIR+IOHI+IO1                                             
         LA    RE,GSGREC5                                                       
         ST    RE,RETURN                                                        
         B     GETRECIO            (I/O PARAMS ALREADY SET)                     
GSGREC4  BAS   RE,GETREC           GO SELECT NEXT RECORD                        
GSGREC5  BNE   GETSELN             (EOF)                                        
*                                                                               
GETSELY  MVC   APRECKEY(L'GNKEY),GNKEY                                          
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   EQU   *                                                                
*                                                                               
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         L     R2,AIOAREA1                                                      
*                                                                               
DSUSR    EQU   *                   * USER ID *                                  
         OC    GNKAGY,GNKAGY                                                    
         BNZ   DSUSR1                                                           
         MVC   LISTUSER(10),=C'ALL       '                                      
         B     DSUSRX                                                           
DSUSR1   LA    R8,IOKEY                                                         
         USING CTIREC,R8           BUILD AN ID KEY                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIREC,C'I'                                                      
         MVC   CTIKID+8(2),GNKAGY                                               
         LA    R1,IORD+IOCONFIL+IO2                                             
         GOTO1 AIO                 READ ID RECORD                               
         BNE   DSUSRX                                                           
         L     R8,AIOAREA2                                                      
         LA    R3,CTIDATA                                                       
         DROP  R8                                                               
         SR    RF,RF                                                            
DSUSR2   CLI   0(R3),0             LOOK FOR ID NAME ELEMENT                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DSUSR2                                                           
         MVC   LISTUSER,2(R3)                                                   
DSUSRX   EQU   *                                                                
*                                                                               
DSSYS    MVC   LISTSYS,GNKSYS      * SYSTEM *                                   
*                                                                               
DSTYP    CLI   LISTSYS,C'M'        * TYPE & MEDIA *                             
         BNE   DSTYPA                                                           
         MVC   LISTTYP(L'GNKTYP),GNKTYP                                         
         MVC   LISTMED,GNKMED                                                   
         B     DSTYPX                                                           
DSTYPA   MVC   LISTTYP,GNKATYP                                                  
*                                                                               
DSTYPX   EQU   *                                                                
*                                                                               
DSCLI    EQU   *                                                                
         MVC   LISTCLI,GNKCLI      * CLIENT *                                   
         OC    LISTCLI,LISTCLI                                                  
         BNZ   DSCLIX                                                           
         MVC   LISTCLI(5),=C'ALL  '                                             
DSCLIX   EQU   *                                                                
*                                                                               
DSDSC    EQU   *                   * DESCRIPTION *                              
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GNDSCELQ     GET DESCRIPTION ELEM                         
         GOTO1 AGETELS,GNARD                                                    
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    DSDSCX                                                           
*                                                                               
         USING GNDSCD,R3                                                        
*                                                                               
         ZIC   RF,GNDSCLEN                                                      
         SH    RF,=Y(GNDSCFXL+1)                                                
         BM    DSDSCX                                                           
         CH    RF,=Y(L'LISTDESC-1) (MAKE SURE THERES ENUF ROOM)                 
         BNH   *+8                                                              
         LH    RF,=Y(L'LISTDESC-1)                                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LISTDESC(0),GNDESC                                               
DSDSCX   EQU   *                                                                
         DROP  R4                                                               
*                                                                               
DISSELX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         LA    R2,APRECKEY                                                      
         XC    GNKEY,GNKEY                                                      
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REQREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REQWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REQDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         XC    SELKEY,SELKEY                                                    
         LA    R8,REQUSERH                                                      
         BAS   RE,VALPARS          GO VALIDATE I/P PARAMETERS                   
*                                                                               
         CLC   SELUSER,HIGH        BUILD INITIAL KEY                            
         BNE   VRKEY1                                                           
         OC    SELALFA,SELALFA                                                  
         BZ    VRKEY2                                                           
         OI    GNKAGY+1,1          (IF ALPHA AGENCY ENTERED                     
         B     VRKEY2               SET TO SKIP "ALL-USER" RECORDS)             
VRKEY1   MVC   GNKAGY,SELUSER                                                   
VRKEY2   MVC   GNKSYS,SELSYS                                                    
         MVC   GNKATYP,SELTYP                                                   
         CLI   SELCLI,X'FF'                                                     
         BE    *+10                                                             
         MVC   GNKCLI,SELCLI                                                    
*                                                                               
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT NARRATIVE RECORDS                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   EQU   *                                                                
         L     R9,AREP                                                          
         LA    R2,IOKEY                                                         
         MVC   GNKEY,APRECKEY                                                   
         MVI   GNKREC,GNKRECQ                                                   
         LA    R1,IOHI+IOGENDIR+IO1                                             
         LA    RE,PRGREC1          GO GET REC WIV                               
         ST    RE,RETURN             PARAMS SET TO READ HI                      
         B     GETRECIO                                                         
*                                                                               
PRGREC   EQU   *                                                                
         BAS   RE,GETREC           GO GET NEXT REC                              
PRGREC1  BNE   PRTREPX                                                          
*                                                                               
PRUSR    EQU   *                   * DISPLAY USER ID *                          
         MVC   REPP1(4),=C'User'                                                
         OC    GNKAGY,GNKAGY                                                    
         BNZ   PRUSR2                                                           
         MVC   REPP1+5(3),=C'ALL'                                               
         MVC   REPP2(8),DASHES                                                  
         B     PRUSRX                                                           
PRUSR2   LA    R4,IOKEY                                                         
         USING CTIREC,R4           BUILD AN ID KEY                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIREC,C'I'                                                      
         MVC   CTIKID+8(2),GNKAGY                                               
         LA    R1,IORD+IOCONFIL+IO2                                             
         GOTO1 AIO                 READ ID RECORD                               
         BE    PRUSR4                                                           
         OC    SELALFA,SELALFA                                                  
         BNZ   PRGREC              (DITCH REC IF CANT CHECK ALFA-AGY)           
         MVC   REPP1+5(3),=C'*?*'                                               
         MVC   REPP2(8),DASHES                                                  
         B     PRUSRX                                                           
PRUSR4   L     R4,AIOAREA2                                                      
         LA    R3,CTIDATA                                                       
         SR    RF,RF                                                            
         OC    SELALFA,SELALFA                                                  
         BZ    PRUSR6                                                           
PRUSR5   CLI   0(R3),0             CHECK ALFA-AGY                               
         BE    PRGREC                                                           
         CLI   0(R3),X'06'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     PRUSR5                                                           
         CLC   SELALFA,2(R3)                                                    
         BNE   PRGREC                                                           
         LA    R3,CTIDATA                                                       
PRUSR6   CLI   0(R3),0             LOOK FOR ID NAME ELEMENT                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     PRUSR6                                                           
         MVC   REPP1+5(10),2(R3)                                                
         MVC   REPP2(15),DASHES                                                 
         LA    RF,REPP1+14         (STRIP EXCESS DASHES)                        
         LA    RE,REPP2+14                                                      
PRUSR7   CLI   0(RF),C' '                                                       
         BNE   PRUSRX                                                           
         MVI   0(RE),C' '                                                       
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         B     PRUSR7                                                           
PRUSRX   EQU   *                                                                
         DROP  R4                                                               
*                                                                               
PRSYS    EQU   *                   * DISPLAY SYSTEM *                           
         MVC   REPP1+16(6),=C'System'                                           
         LA    RE,SYSTAB                                                        
PRSYS2   CLI   0(RE),X'FF'                                                      
         BNE   PRSYS4                                                           
         MVC   REPP1+23(1),GNKSYS (NOT IN SYSTEM TABLE)                         
         MVC   REPP2+16(8),DASHES                                               
         MVC   REPP1+37(3),GNKTYP (SO CANT DO TYPE/MEDIA EITHER)                
         MVC   REPP2+32(8),DASHES                                               
         B     PRTYPX                                                           
PRSYS4   CLC   GNKSYS,0(RE)                                                     
         BE    PRSYS6                                                           
         LA    RE,L'SYSTAB(,RE)                                                 
         B     PRSYS2                                                           
PRSYS6   MVC   REPP1+23(8),1(RE)                                                
PRSYSX   EQU   *                                                                
*                                                                               
PRTYP    EQU   *            * DISPLAY TYPE & MEDIA ACCORDING TO SYSTEM          
         MVC   REPP1+32(4),=C'Type'                                             
         ZIC   RF,L'SYSTAB-1(RE)   (RE STILL -> SYSTAB ENTRY)                   
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     PRMED                                                            
         B     PRACC                                                            
*                                                                               
PRMED    EQU   *                   SYSTEM=MEDIA                                 
         CLC   REPTKEY(L'REPTKEY-1),GNKEY                                       
         BE    PRMED05                                                          
         OI    REPHEADI,REPHFRCE   (THROW PAGE ON CHANGE OF TYPE)               
         MVC   REPTKEY,GNKEY                                                    
PRMED05  LA    RE,MTYPTAB                                                       
         MVC   REPP2+16(12),DASHES                                              
         MVC   REPP1+49(5),=C'Media'                                            
PRMED10  CLI   0(RE),X'FF'                                                      
         BNE   PRMED20                                                          
         MVC   REPP1+37(2),GNKTYP                                               
         MVC   REPP2+32(7),DASHES                                               
         B     PRMED40                                                          
PRMED20  CLC   GNKTYP,0(RE)                                                     
         BE    PRMED30                                                          
         LA    RE,L'MTYPTAB(,RE)                                                
         B     PRMED10                                                          
PRMED30  MVC   REPP1+37(11),2(RE)                                               
         MVC   REPP2+32(16),DASHES                                              
         LA    RF,REPP1+47         (STRIP EXCESS DASHES)                        
         LA    RE,REPP2+47                                                      
PRMED35  CLI   0(RF),C' '                                                       
         BNE   PRMED40                                                          
         MVI   0(RE),C' '                                                       
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         B     PRMED35                                                          
PRMED40  MVC   REPP1+55(1),GNKMED                                               
         MVC   REPP2+49(7),DASHES                                               
PRMEDX   B     PRTYPX                                                           
*                                                                               
PRACC    EQU   *                   SYSTEM=ACCOUNTING                            
         MVC   REPP2+16(14),DASHES                                              
         CLC   REPTKEY,GNKEY                                                    
         BE    PRACC1                                                           
         OI    REPHEADI,REPHFRCE   (THROW PAGE ON CHANGE OF TYPE)               
         MVC   REPTKEY,GNKEY                                                    
PRACC1   LA    RE,ATYPTAB                                                       
PRACC2   CLI   0(RE),X'FF'                                                      
         BNE   PRACC4                                                           
         MVC   REPP1+37(3),GNKATYP                                              
         MVC   REPP2+32(8),DASHES                                               
         B     PRACCX                                                           
PRACC4   CLC   GNKATYP,0(RE)                                                    
         BE    PRACC6                                                           
         LA    RE,L'ATYPTAB(,RE)                                                
         B     PRACC2                                                           
PRACC6   MVC   REPP1+37(11),3(RE)                                               
         MVC   REPP2+32(16),DASHES                                              
         LA    RF,REPP1+47         (STRIP EXCESS DASHES)                        
         LA    RE,REPP2+47                                                      
PRACC8   CLI   0(RF),C' '                                                       
         BNE   PRACCX                                                           
         MVI   0(RE),C' '                                                       
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         B     PRACC8                                                           
PRACCX   EQU   *                                                                
*                                                                               
PRTYPX   EQU   *                                                                
*                                                                               
PRCLI    EQU   *                   * DISPLAY CLIENT                             
         MVC   REPP1+58(6),=C'Client'                                           
         OC    GNKCLI,GNKCLI                                                    
         BNZ   PRCLI1                                                           
         MVC   REPP1+65(3),=C'ALL'                                              
         MVC   REPP2+58(10),DASHES                                              
         B     PRCLIX                                                           
PRCLI1   MVC   REPP1+65(5),GNKCLI                                               
         MVC   REPP2+58(12),DASHES                                              
         LA    RF,REPP1+69         (STRIP EXCESS DASHES)                        
         LA    RE,REPP2+69                                                      
PRCLI5   CLI   0(RF),C' '                                                       
         BNE   PRCLIX                                                           
         MVI   0(RE),C' '                                                       
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         B     PRCLI5                                                           
PRCLIX   EQU   *                                                                
*                                                                               
PRDSC    EQU   *                   * DESCRIPTION                                
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GNDSCELQ     GET DESCRIPTION ELEM                         
         GOTO1 AGETELS,GNARD                                                    
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    PRDSCX                                                           
*                                                                               
         USING GNDSCD,R3                                                        
*                                                                               
         ZIC   RF,GNDSCLEN                                                      
         SH    RF,=Y(GNDSCFXL+1)                                                
         BM    PRDSCX                                                           
         EX    RF,PRMVDSC                                                       
         MVI   REPP2+73,C'-'                                                    
         BCTR  RF,0                                                             
         EX    RF,PRMVDSH                                                       
         B     PRDSCX                                                           
PRMVDSC  MVC   REPP1+73(0),GNDESC  DISPLAY DESCRIPTION                          
PRMVDSH  MVC   REPP2+74(0),REPP2+73                                             
PRDSCX   EQU   *                                                                
*                                                                               
PRDATA   EQU   *                   * DISPLAY DATA LINES *                       
         LA    R8,LINEHDRS                                                      
         MVI   REPP3,0             SPACE AFTER KEY FIELDS                       
         LA    R4,REPP4                                                         
         XC    LINEFLG,LINEFLG                                                  
         MVI   SEQ,1                                                            
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GNDATELQ     GET DATA ELEMS                               
         MVI   APELEM+1,1                                                       
PRDATA10 MVC   APELEM+2(1),SEQ                                                  
         GOTO1 AGETELS,GNARD                                                    
         ICM   R3,15,APPARM                                                     
         BNZ   PRDATA20                                                         
*                                                                               
         USING GNDATD,R3                                                        
*                                                                               
         LA    R3,=AL2(GNDATFXL)   (NO ELEM, SET ZERO DATA LENGTH)              
PRDATA20 CLI   GNDATLEN,GNDATFXL   ANY DATA?                                    
         BH    PRDATA25            (BR IF THERE IS)                             
         TM    SEQ,1                                                            
         BNZ   PRDATA50                                                         
         TM    LINEFLG,LEFT                                                     
         BZ    PRDATA50                                                         
         NI    LINEFLG,255-LEFT    (IF ALREADY DATA ON THIS LINE                
         LA    R4,132(,R4)          THEN -> NEXT LINE)                          
         B     PRDATA50                                                         
*                                                                               
PRDATA25 TM    LINEFLG,HDR         DO LINE HEADING IF NOT DONE YET              
         BNZ   PRDATA30                                                         
         MVC   0(11,R4),0(R8)                                                   
         LA    R4,132(,R4)                                                      
         OI    LINEFLG,HDR                                                      
PRDATA30 ZIC   RF,GNDATLEN         MOVE DATA LINE TO SCREEN                     
         SH    RF,=Y(GNDATFXL+1)                                                
PRDATA35 TM    SEQ,1               (LEFT OR RIGHT?)                             
         BZ    PRDATA40                                                         
         OI    LINEFLG,LEFT                                                     
         EX    RF,*+8                                                           
         B     PRDATA50                                                         
         MVC   0(0,R4),GNDATA      MOVE LEFT SIDE OF LINE                       
PRDATA40 EX    RF,*+8                                                           
         B     PRDATA45                                                         
         MVC   66(0,R4),GNDATA     MOVE RIGHT SIDE OF LINE                      
PRDATA45 LA    R4,132(,R4)         -> NEXT LINE                                 
         NI    LINEFLG,255-LEFT    & RESET "DATA-ON-THIS-LINE"                  
*                                                                               
PRDATA50 CLI   SEQ,12              ALL FINISHED?                                
         BNL   PRDATAX                                                          
         ZIC   RF,SEQ              UPDATE ELEM SEQ NO                           
         LA    RF,1(,RF)                                                        
         STC   RF,SEQ                                                           
         CLI   SEQ,5               HEADLINES FINISHED?                          
         BE    PRDATA60                                                         
         CLI   SEQ,9               MIDLINES FINISHED?                           
         BNE   PRDATA10                                                         
PRDATA60 NI    LINEFLG,255-HDR     RESET "HDR-ALREADY-DONE"                     
         LA    R8,11(,R8)           AND -> NEXT LINE HEADING                    
         B     PRDATA10            GO GET NEXT ELEM                             
*                                                                               
PRDATAX  EQU   *                                                                
*                                                                               
         MVI   0(R4),0             SPACE AFTER DATA                             
         GOTO1 VREPORT,REPD                                                     
         B     PRGREC                                                           
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* VALIDATE I/P PARAMETERS FOR LIST/REPORT                            *          
**********************************************************************          
         SPACE 1                                                                
         USING LSTUSERH,R8                                                      
VALPARS  ST    RE,RETURN                                                        
*                                                                               
VPUSR    EQU   *                                                                
         CLI   APMODE,APMVALQ                                                   
         BNE   VPUSR0                                                           
         CLC   LSTUSER(3),=C'AGY=' REPORT MODE ONLY ALLOW ALPHA ID              
         BNE   VPUSR0                                                           
         MVI   FVMINL,6            MUST BE AGY=AA                               
         MVI   FVMAXL,6                                                         
         GOTO1 AFVAL,LSTUSERH                                                   
         BNE   VPERREX                                                          
         MVC   SELUSER,HIGH                                                     
         MVC   SELALFA,FVIFLD+4                                                 
         CLC   SELALFA,CUAALF      MUST BE SAME AGY                             
         BE    VPUSRX1                                                          
         CLI   CUOFFC,C'*'                                                      
         BNE   VPERROR                                                          
         B     VPUSRX                                                           
VPUSR0   GOTO1 AFVAL,LSTUSERH      * VALIDATE USER *                            
         BE    VPUSR1              (IF ENTERED)                                 
         TM    CUSTAT,CUSDDS       ONLY DDS CAN SEE ALL USERS                   
         BNO   VPERROR                                                          
         MVC   SELUSER,HIGH        SET TO FF IF NOT ENTERED                     
         B     VPUSRX                                                           
VPUSR1   CLC   FVIFLD(10),=C'ALL       '                                        
         BE    VPUSRX                                                           
         LA    R4,IOKEY                                                         
         USING CTIREC,R4           BUILD AN ID KEY                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIREC,C'I'                                                      
         MVC   CTIKID,FVIFLD                                                    
         LA    R1,IORD+IOCONFIL+IO2                                             
         GOTO1 AIO                 READ ID RECORD                               
         BNE   VPERROR             RECORD MUST BE PRESENT & CORRECT             
         L     R4,AIOAREA2                                                      
         LA    R3,CTIDATA                                                       
         DROP  R4                                                               
         SR    RF,RF                                                            
VPUSR2   CLI   0(R3),0             LOOK FOR ID# ELEMENT                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VPUSR2                                                           
         MVC   SELUSER,2(R3)                                                    
VPUSRX   CLC   SELUSER,CUUSER      MUST BE SAME USER                            
         BE    VPUSRX1                                                          
         TM    CUSTAT,CUSDDS       UNLESS DDS TERMINAL                          
         BNO   VPERROR                                                          
VPUSRX1  OI    LSTUSERH+6,FVOXMT                                                
*                                                                               
VPSYS    EQU   *                   * VALIDATE SYSTEM *                          
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,LSTSYSH                                                    
         BE    VPSYS0                                                           
         CLI   LSTTYPH+5,0         (IF NO SYSTEM ENTERED                        
         BNE   VPERREX                MUSTNT BE TYPE/MEDIA EITHER)              
         CLI   LSTMEDH+5,0                                                      
         BNE   VPERREX                                                          
         B     VPTYPX                                                           
VPSYS0   LA    RE,SYSTAB           CHECK IN TABLE OF VALID SYSTEMS              
         ZIC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
VPSYS1   CLI   0(RE),X'FF'                                                      
         BE    VPERROR                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),1(RE)                                                  
         BE    VPSYS2                                                           
         LA    RE,L'SYSTAB(,RE)                                                 
         B     VPSYS1                                                           
VPSYS2   MVC   SELSYS,0(RE)        OK - STORE SYSTEM                            
         MVC   LSTSYS,1(RE)             & REDISPLAY                             
         OI    LSTSYSH+6,FVOXMT                                                 
VPSYSX   EQU   *                                                                
*                                                                               
VPTYP    EQU   *            * VALIDATE TYPE & MEDIA ACCORDING TO SYSTEM         
         ZIC   RF,L'SYSTAB-1(RE)   (RE STILL -> SYSTAB ENTRY)                   
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     VPMED                                                            
         B     VPACC                                                            
*                                                                               
VPMED    EQU   *                   SYSTEM=MEDIA                                 
         GOTO1 AFVAL,LSTTYPH                                                    
         BNE   VPMED25                                                          
         LA    RE,MTYPTAB          - TYPES VALID FOR MEDIA SYSTEM               
         ZIC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
VPMED1   CLI   0(RE),X'FF'                                                      
         BE    VPERROR                                                          
         CLC   FVIFLD(2),0(RE)                                                  
         BE    VPMED2                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),2(RE)                                                  
         BE    VPMED2                                                           
         LA    RE,L'MTYPTAB(,RE)                                                
         B     VPMED1                                                           
VPMED2   MVC   SELTYP,0(RE)        OK - STORE TYPE                              
         MVC   LSTTYP,2(RE)             & REDISPLAY                             
         OI    LSTTYPH+6,FVOXMT                                                 
*                                                                               
VPMED25  GOTO1 AFVAL,LSTMEDH                                                    
         BNE   VPTYPX                                                           
         TM    FVIIND,FVIALF+FVINUM                                             
         BZ    VPERROR                                                          
         MVC   SELMED,FVIFLD       OK - STORE MEDIA                             
         B     VPTYPX                                                           
         SPACE 2                                                                
VPACC    EQU   *                   SYSTEM=ACCOUNTING                            
         GOTO1 AFVAL,LSTTYPH                                                    
         BNE   VPTYPX                                                           
         LA    RE,ATYPTAB          - TYPES VALID FOR ACC SYSTEM                 
         ZIC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
VPACC1   CLI   0(RE),X'FF'                                                      
         BE    VPERROR                                                          
         CLC   FVIFLD(3),0(RE)                                                  
         BE    VPACC2                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),3(RE)                                                  
         BE    VPACC2                                                           
         LA    RE,L'ATYPTAB(,RE)                                                
         B     VPACC1                                                           
VPACC2   MVC   SELATYP,0(RE)       OK -STORE                                    
         MVC   LSTTYP,3(RE)             & REDISPLAY                             
         OI    LSTTYPH+6,FVOXMT                                                 
*                                  VALIDATE MEDIA                               
         CLI   LSTMEDH+5,0         - NOT VALID FOR ACC SYSTEM                   
         BNE   VPERROR                                                          
*                                                                               
VPTYPX   EQU   *                                                                
*                                                                               
VPCLI    EQU   *                   * VALIDATE CLIENT *                          
         GOTO1 AFVAL,LSTCLIH                                                    
         BE    VPCLI1                                                           
         MVC   SELCLI,HIGH         SET TO FF IF NOT ENTERED                     
         B     VPCLIX                                                           
VPCLI1   CLC   FVIFLD(5),=C'ALL  '                                              
         BE    VPCLIX                                                           
         OC    SELUSER,SELUSER                                                  
         BZ    VPERROR             NOT VALID IF USER=ALL                        
         MVC   SELCLI,FVIFLD                                                    
VPCLIX   OI    LSTCLIH+6,FVOXMT                                                 
*                                                                               
VALPARSX EQU   *                                                                
         L     RE,RETURN                                                        
         BR    RE                                                               
         DROP  R8                                                               
         SPACE 3                                                                
VPERROR  EQU   *                                                                
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
VPERREX  B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET NEXT RECORD FOR LIST/REPORT, FILTERING ON I/P PARAMETERS       *          
**********************************************************************          
         SPACE 1                                                                
GETREC   ST    RE,RETURN                                                        
*                                                                               
GETRECSQ LA    R1,IOGENDIR+IOSQ+IO1                                             
GETRECIO GOTO1 AIO                                                              
         BNE   GETRECX                                                          
         LA    R2,IOKEY                                                         
         CLI   GNKREC,GNKRECQ      CHECK STILL NARRATIVE RECORD                 
         BNE   GETRECX                                                          
         CLC   GNKAGY,SELUSER      DONT READ BEYOND                             
         BH    GETRECX                HIGHEST RELEVENT KEY                      
         SPACE 1                                                                
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         SPACE                                                                  
*                                  * FILTER ON SELECTION CRITERIA *             
         SPACE 1                                                                
GRUSR    CLI   SELUSER,X'FF'       USER                                         
         BE    GRUSRX              (IF ENTERED)                                 
         CLC   GNKAGY,SELUSER                                                   
         BNE   GETRECSQ                                                         
GRUSRX   EQU   *                                                                
*                                                                               
GRSYS    OC    SELSYS,SELSYS       SYSTEM                                       
         BZ    GRSYSX              (IF ENTERED)                                 
         CLC   SELSYS,GNKSYS                                                    
         BNE   GETRECSQ                                                         
GRSYSX   EQU   *                                                                
*                                                                               
GRTYP    OC    SELTYP,SELTYP       TYPE                                         
         BZ    GRTYPX              (IF ENTERED)                                 
         CLC   SELTYP,GNKTYP                                                    
         BNE   GETRECSQ                                                         
GRTYPX   EQU   *                                                                
*                                                                               
GRMED    OC    SELMED,SELMED       MEDIA                                        
         BZ    GRMEDX              (IF ENTERED)                                 
         CLC   SELMED,GNKMED                                                    
         BNE   GETRECSQ                                                         
GRMEDX   EQU   *                                                                
*                                                                               
GRCLI    CLI   SELCLI,X'FF'        CLIENT                                       
         BE    GRCLIX              (IF ENTERED)                                 
         CLC   GNKCLI,SELCLI                                                    
         BNE   GETRECSQ                                                         
GRCLIX   EQU   *                                                                
*                                                                               
GETRECX  L     RE,RETURN                                                        
         BR    RE                  RETURN WITH CC= IF REC IS OK                 
         EJECT                                                                  
SYSTAB   DS    0CL10               VALID SYSTEMS                                
*                                                                               
*        BYTE  0       SYSTEM CODE                                              
*        BYTES 1 - 8   SYSTEM NAME                                              
*        BYTE  9       ROUTINE NO (FOR VAL/DISP ETC)                            
*                                                                               
         DC    C'M',CL8'MEDIA',X'01'                                            
         DC    C'A',CL8'ACCOUNT',X'02'                                          
         DC    X'FF'                                                            
*                                                                               
MTYPTAB  DS    0CL13               TYPES VALID FOR MEDIA SYSTEM                 
         DC    CL13'12ORDERS'                                                   
         DC    CL13'13COPY ADVICE'                                              
         DC    CL13'14BILLS'                                                    
         DC    CL13'1ABUYING SCH.'                                              
         DC    CL13'1BBOOKING DET'                                              
         DC    CL13'CADDB C/A'                                                  
         DC    CL13'MIMI BILLS'                                                 
         DC    CL13'MOGER ORDERS'                                               
         DC    CL13'D4GERMAN BILL'                                              
         DC    CL13'D7SPLIT BILLS'                                              
         DC    CL13'DSDELIV SHEET'                                              
         DC    CL13'CPCONTRACTS'                                                
         DC    CL13'H4DUTCH BILLS'                                              
         DC    CL13'CICOPY INSTR.'                                              
         DC    CL13'ODDELIV NOTCE'                                              
         DC    CL13'OOPOST.ORDERS'                                              
         DC    CL13'FFFFF-SCHEDLE'                                              
         DC    X'FF'                                                            
*                                                                               
ATYPTAB  DS    0CL14               TYPES VALID FOR ACC SYSTEM                   
         DC    CL14'14 ESTIMATES'                                               
         DC    CL14'21 AUTO BILLS'                                              
         DC    CL14'22 DRAFT BILLS'                                             
         DC    CL14'27 NIC SCHED'                                               
         DC    CL14'83 RECEIVABLES'                                             
         DC    CL14'8A PROD. REC.'                                              
         DC    CL14'HP DUTCH PMNTS'                                             
         DC    CL14'HV DUTCH VNOTE'                                             
         DC    CL14'BILPROD BILLS'                                              
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
REPDESCL DC    C'NARRATIVE LIST'                                                
*                                                                               
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'NARRATIVE LIST'                                          
         SPEC  H2,57,C'--------------'                                          
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
         LTORG                                                                  
         SPACE 5                                                                
HIGH     DC    5X'FF'                                                           
*                                                                               
INFCHA1  EQU   04+X'FF00'          RECORD DISPLAYED - ENTER CHANGES             
*                                                                               
LINEHDRS DC    C'Headlines:-'                                                   
         DC    C'Midlines:- '                                                   
         DC    C'Footlines:-'                                                   
*                                                                               
DASHES   DC    16C'-'                                                           
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGEND1D                                                       
         EJECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENF1D                                                       
         ORG                                                                    
         EJECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENB1D                                                       
         ORG                                                                    
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTUSER DS    CL10                                                             
         DS    CL2                                                              
LISTSYS  DS    CL1                                                              
         DS    CL2                                                              
LISTTYP  DS    CL3                                                              
         DS    CL2                                                              
LISTMED  DS    CL1                                                              
         DS    CL2                                                              
LISTCLI  DS    CL5                                                              
         DS    CL1                                                              
LISTDESC DS    CL45                                                             
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
*                                                                               
SELKEY   DS    0XL(SELKEYL)        SELECTION CRITERIA                           
SELUSER  DS    CL2                                                              
SELALFA  DS    CL2                                                              
SELSYS   DS    C                                                                
SELATYP  DS    CL3                                                              
         ORG   SELATYP                                                          
SELTYP   DS    CL2                                                              
SELMED   DS    C                                                                
SELCLI   DS    CL5                                                              
SELKEYL  EQU   *-SELUSER                                                        
DUB      DS    D                   BOG ODDMENTS                                 
WORK     DS    CL17                                                             
UPDATE   DS    X                                                                
RETURN   DS    F                                                                
*                                                                               
SEQ      DS    X                   FOR ELEM SEQ NO                              
CHG      DS    X                   SET '01' IF RECORD NEEDS WRITING             
ELEM     DS    X                                                                
GOT1     EQU   1                   GOT AN ELEMENT                               
LINEFLG  DS    X                                                                
HDR      EQU   1                   LINE HEADING ALREADY DONE                    
LEFT     EQU   2                   DATA ON LEFT-SIDE OF LINE                    
*                                                                               
REPTKEY  DS    CL(GNKCLI-GNKEY)    REPORT KEY FOR PAGE THROW                    
*                                                                               
IPLINE   DSECT                     INPUT LINES                                  
IPHDRH   DS    CL8                                                              
IPHDR    DS    CL(L'NARHDR)                                                     
IPDATAH  DS    CL8                                                              
IPDATA   DS    CL(L'NARDATA)                                                    
IPSTDH   DS    CL8                                                              
IPSTD    DS    CL(L'NARSTD)                                                     
IPLINEL  EQU   *-IPLINE                                                         
*                                                                               
LOCALX   EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE GEGENNAR                                                       
         SPACE 5                                                                
* DDLANGEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031CTGEN0E   05/01/02'                                      
         END                                                                    
