*          DATA SET NEBUY21S   AT LEVEL 101 AS OF 05/01/02                      
*PHASE T31121A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - INFO UNITS OVERLAY - T31121'               
T31121   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**INFU**,RA,RR=RE                                              
         L     R9,0(R1)                                                         
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK                                                       
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
         SPACE 2                                                                
* DISPLAY UNITS BY PROGRAM                                                      
*                                                                               
UNIT     LA    R2,BUYESTH          LIMIT ESTIMATE=ALL REQUESTS                  
         ST    R2,FADDR                                                         
         TM    ESTTYP,IALL+DEFAULT TEST FOR EST=ALL                             
         BZ    UNIT1               NO                                           
         TM    NETTYP,ISINGLE      YES-MAKE SURE ONLY ONE NETWORK               
         BZ    *+12                                                             
         TM    PROGTYP,IALL        PROGRAM CANNOT EQ ALL                        
         BZ    UNIT1                                                            
         MVI   FERN,INVERR                                                      
         MVC   XTRA(15),=C'REQUEST TOO BIG'                                     
         B     ERROR                                                            
         SPACE 1                                                                
UNIT1    LA    R2,BUYACTH          SET CURSOR POSITION                          
         ST    R2,FADDR                                                         
         TM    MODE,DISPLAY        TEST FOR BASE FIELD CHANGES                  
         BZ    *+8                 NO                                           
         OI    MODE,FIRST          FORCE RE-START OF READ                       
         TM    SVLMODE,EOF         TEST FOR EOF ON LAST DISPLAY                 
         BZ    *+8                                                              
         OI    MODE,FIRST          FORCE FIRST TIME                             
*                                                                               
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVC   DEMO,ESTDEMS                                                     
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,X'FF'         TEST IF ANYTHING FOLLOWS ACTION              
         BE    UNIT12              NO                                           
         MVI   FNDX,1                                                           
         SPACE                                                                  
* EDIT ACTION FIELD FOR FILTERS                                                 
*                                                                               
UNIT2    XC    FTERM,FTERM                                                      
         MVC   FTERM(2),=C'=,'                                                  
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BNE   UNIT3                                                            
         CLI   FSTOP,X'FF'         TEST FOR END OF FIELD                        
         BE    UNIT12                                                           
         B     UNITR               LONE TERMINATOR                              
         SPACE                                                                  
UNIT3    ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         CLI   FSTOP,EQUAL         TEST FOR EQUALS SIGN                         
         BNE   UNIT7               NO                                           
         CLI   FLDH+5,1                                                         
         BNE   UNIT4                                                            
         CLI   FLD,C'P'            TEST FOR PRODUCT FILTER                      
         BNE   UNITR                                                            
*                                                                               
         XC    FTERM,FTERM         NOW SCAN FOR END OF BRAND CODE               
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,2            BRAND CODE IS 2 OR 3 CHARS                   
         BE    *+12                                                             
         CLI   FLDH+5,3                                                         
         BNE   UNITR                                                            
*        TM    FLDH+4,X'04'        AND ALPHA                                    
*        BZ    ERROR                                                            
         MVC   THREE,FLD           EXTRACT PRODUCT CODE                         
         BAS   RE,VALPRD           GO VALIDATE IT                               
         B     UNIT10              NEXT FIELD                                   
         SPACE                                                                  
UNIT4    CLI   FLDH+5,2                                                         
         BNE   UNIT5                                                            
         CLC   FLD(2),=C'DP'       TEST FOR DAYPART                             
         BNE   UNITR                                                            
         XC    FTERM,FTERM         LOOK FOR DAYPART VALUE                       
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,1                                                         
         BNE   UNITR                                                            
         TM    FLDH+4,X'04'        TEST FOR ALPHA VALUE                         
         BZ    UNITR                                                            
         CLI   DAYPART,0           TEST FOR DUPLICATE VALUE                     
         BE    *+14                                                             
         MVC   XTRA(L'DUPFILT),DUPFILT                                          
         B     UNITR                                                            
*                                                                               
         MVC   DAYPART,FLD                                                      
         B     UNIT10                                                           
*                                                                               
UNIT5    CLI   FLDH+5,3                                                         
         BNE   UNIT6                                                            
         CLC   FLD(2),=C'TOT'      TOT WITH DEMO OVERRIDE                       
         BNE   UNIT6                                                            
         MVI   REPOPT,C'T'                                                      
         XC    FTERM,FTERM         LOOK FOR DAYPART VALUE                       
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
*                                                                               
         LA    RE,DBLOCKA                                                       
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'EVN'      SET FOR EVN-ESTIMATED                        
         MVI   DBSELMED,C'V'                                                    
         MVC   DBFILE,=C'NTI'      NO-SET FOR NTI-ACTUALS                       
         MVI   DBSELMED,C'N'                                                    
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         GOTO1 CDEMOVAL,DMCB,FLDH,(1,FULL),DBLOCKA                              
         MVC   DEMO,FULL           SAVE DEMO CODE                               
*                                                                               
         CLI   FULL,X'FF'           ERROR                                       
         BNE   UNIT5B                                                           
         MVC   XTRA(L'DEMERR),DEMERR                                            
         B     UNITR                                                            
* CHECK TO SEE THAT DEMO EXISTS ON THE ESTIMATE                                 
UNIT5B   LA    RE,ESTDEMS                                                       
         LA    RF,20                                                            
UNIT5D   CLC   DEMO(1),0(RE)                                                    
         BNE   UNIT5F                                                           
         CLC   DEMO+2(1),2(RE)                                                  
         BE    UNIT10                                                           
UNIT5F   LA    RE,3(RE)                                                         
         BCT   RF,UNIT5D                                                        
         MVC   XTRA(L'DEMEST),DEMEST                                            
         B     UNITR                                                            
*                                                                               
         SPACE                                                                  
UNIT6    CLI   FLDH+5,3            TEST FOR MIN LEN OF 3                        
         BL    UNITR                                                            
         CLI   FLDH+5,7            TEST FOR MAX LEN OF 7                        
         BH    UNITR                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,DAYTCOM          DAY(TIME)=Y(ES)                              
         BNE   UNITR                                                            
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    UNITR                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,YESCOMP                                                       
         BNE   UNITR                                                            
         MVI   REPOPT,C'D'         SET DAY/TIME W/IN PROG AS OPT                
         B     UNIT10                                                           
         SPACE 1                                                                
* NOT KEYWORD-PARAMETER CHECK FOR PACKAGE STATUS OR DATES                       
*                                                                               
UNIT7    BAS   RE,PACKST           VALIDATE FOR PACKAGE STATUS                  
         BE    UNIT10              FOUND VALID PACKAGE STATUS                   
         SPACE                                                                  
UNIT8    MVI   FLEN,0              RE-EDIT FIELD LOOKING FOR                    
         XC    FTERM,FTERM         MMMDDYY-MMMDDYY                              
         MVI   FTERM,DASH                                                       
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,DASH                                                       
         BNE   UNITR                                                            
         CLI   FLDH+5,0                                                         
         BE    UNITR                                                            
         GOTO1 VDATVAL,DMCB,FLD,DUB                                             
         OC    0(4,R1),0(R1)                                                    
         BZ    UNITR                                                            
         GOTO1 VDATCON,DMCB,DUB,(2,HALF)                                        
         OC    STDATE,STDATE                                                    
         BNZ   UNITR                                                            
         MVC   STDATE,HALF                                                      
         SPACE                                                                  
UNIT9    XC    FTERM,FTERM         FOUND START DATE-NOW GET                     
         MVI   FTERM,COMMA         END DATE                                     
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    UNITR                                                            
         GOTO1 VDATVAL,DMCB,FLD,DUB                                             
         OC    0(4,R1),0(R1)                                                    
         BZ    UNITR                                                            
         GOTO1 VDATCON,(R1),DUB,(2,ENDATE)                                      
         CLC   STDATE,ENDATE       TEST FOR DATE SEQUENCE ERROR                 
         BNH   UNIT10              NO                                           
         MVI   FERN,SEQERR                                                      
         B     ERROR                                                            
         SPACE                                                                  
UNIT10   B     UNIT2                                                            
         SPACE 2                                                                
UNITR    MVI   FERN,INVERR                                                      
         B     ERROR                                                            
         SPACE                                                                  
* TEST FOR CHANGE IN READ CONTROLS AND SET UP FOR READ                          
*                                                                               
UNIT12   CLI   REPOPT,0            TEST FOR DEFAULT REPORT OPTION               
         BNE   *+8                                                              
         MVI   REPOPT,C'P'         SET IT TO PROGRAM                            
         CLC   THISVALS,SVVALS     TEST CHANGE IN CONTROL VALUES                
         BE    *+8                                                              
         OI    MODE,FIRST                                                       
         MVC   SVVALS(SVVALSLN),THISVALS                                        
         GOTO1 VCLEARF,DMCB,(1,INUDAT1H),INULAST                                
         CLI   REPOPT,C'P'         TEST FOR PROGRAM REPORT                      
         BE    UNIT17                                                           
         CLI   REPOPT,C'T'         TEST FOR PROGRAM REPORT                      
         BE    UNIT30                                                           
*                                                                               
* CONTROL FOR DAY/TIME WITHIN PROGRAM REPORT                                    
*                                                                               
UNIT13   TM    MODE,FIRST          TEST FOR FIRST TIME                          
         BO    UNIT14              YES-DO INQUIRY FIRST                         
         BAS   RE,GETSAVE          RETRIEVE SAVE AREA                           
         B     UNIT60              NOW DO THE SCREEN DISPLAY                    
*                                                                               
UNIT14   XC    INUHED1,INUHED1                                                  
         MVC   INUHED1(L'DAYHED1),DAYHED1 SET UP HEADLINE                       
         MVC   INUHED1+L'DAYHED1(L'DAYHED2),DAYHED2                             
         OI    INUHED1H+6,X'80'    XMIT                                         
         BAS   RE,READF            READ THE RECORDS FOR THE INQUIRY             
         CLI   ENTRIES,0           TEST FOR ANY DATA                            
         BNE   UNIT15              YES                                          
*                                                                               
         MVC   BUYMSG(L'NODATA),NODATA                                          
         XC    SVDATA(SVDATAL),SVDATA                                           
         MVI   SVLMODE,EOF                                                      
         B     UNITX               GO OUT TO USER                               
*                                                                               
UNIT15   ZIC   R0,ENTRIES          SORT THE TABLE ENTRIES                       
         GOTO1 VXSORT,DMCB,AIOAREA2,(R0),PRTABL,PRKEYL,0                        
         BAS   RE,PUTSAVE          SAVE THE TABLE                               
         MVC   SVTABNTR,ENTRIES    SAVE ENTRY COUNT                             
         MVI   SVLNTRY,0           LAST ENTRY IS ZERO                           
         B     UNIT60                                                           
         SPACE                                                                  
* CONTROL FOR PROGRAM REPORT                                                    
*                                                                               
UNIT17   TM    MODE,FIRST          TEST FOR FIRST TIME                          
         BO    UNIT18              YES                                          
         BAS   RE,RESTBLK          NO-RESTORE BLOCK                             
         MVI   NBFUNCT,NBFRDHI     AND SET TO RESUME READ                       
         B     UNIT20                                                           
         SPACE 1                                                                
UNIT18   XC    INUHED1,INUHED1                                                  
         XC    INUHED2,INUHED2                                                  
         MVC   INUHED1(L'PROGHED1),PROGHED1 SET UP HEADLINE                     
         MVC   INUHED1+L'PROGHED1(L'PROGHED2),PROGHED2                          
         OI    INUHED1H+6,X'80'    XMIT                                         
         OI    INUHED2H+6,X'80'    XMIT                                         
         BAS   RE,CLRBLOCK                                                      
         SPACE 1                                                                
UNIT20   BAS   RE,READF            READ THE RECORDS FOR THIS SCREEN             
         CLI   ENTRIES,0           TEST FOR ANY DATA                            
         BNE   UNIT60              YES                                          
*                                                                               
         MVC   BUYMSG(L'NODATA),NODATA                                          
         XC    SVDATA(SVDATAL),SVDATA                                           
         MVI   SVLMODE,EOF                                                      
         B     UNITX               GO OUT TO USER                               
*                                                                               
* CONTROL FOR TOTAL REPORT                                                      
*                                                                               
UNIT30   TM    MODE,FIRST          TEST FOR FIRST TIME                          
         BO    UNIT35              YES-DO INQUIRY FIRST                         
         BAS   RE,GETSAVE          RETRIEVE SAVE AREA                           
         B     UNIT60              NOW DO THE SCREEN DISPLAY                    
*                                                                               
UNIT35   XC    INUHED1,INUHED1                                                  
         XC    INUHED2,INUHED2                                                  
         MVC   INUHED2(L'TOTHED1),TOTHED1 SET UP HEADLINE                       
         MVC   INUHED2+L'TOTHED1(L'TOTHED2),TOTHED2                             
*                                                                               
         LA    RE,DBLOCKA          INIT  DBLOCK                                 
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DEMO+1,C'I'                                                      
         GOTO1 VDEMOCON,DMCB,DEMO,(10,WORK),(C'S',DBLOCKA),ESTUSNS              
* FIND THE BLANKS                                                               
         LA    RE,WORK+9                                                        
         SR    RF,RF                                                            
         CLI   0(RE),X'40'                                                      
         BH    *+14                                                             
         LA    RF,1(RF)                                                         
         BCTR  RE,0                                                             
         B     *-14                                                             
*                                                                               
         LA    RE,INUHED1+42                                                    
         AR    RE,RF                                                            
         MVC   0(10,RE),WORK     DEMO CATEGORY                                  
         LA    RE,INUHED1+57                                                    
         AR    RE,RF                                                            
         MVC   0(10,RE),WORK     DEMO CATEGORY                                  
*                                                                               
         OI    INUHED1H+6,X'80'    XMIT                                         
         OI    INUHED2H+6,X'80'    XMIT                                         
         BAS   RE,READF            READ THE RECORDS FOR THE INQUIRY             
         B     UNIT60                                                           
         SPACE 2                                                                
* OUTPUT SCREEN DISPLAY                                                         
*                                                                               
UNIT60   CLI   REPOPT,C'T'         TEST FOR PROGRAM REPORT                      
         BE    UNIT70                                                           
         CLI   REPOPT,C'P'         TEST FOR PROGRAM REPORT                      
         BNE   UNIT65                                                           
         BAS   RE,PRODT                                                         
         MVI   MORESW,NO                                                        
         CLI   NBMODE,NBREQLST     TEST FOR EOF                                 
         BE    *+8                                                              
         MVI   MORESW,YES                                                       
         B     UNIT80                                                           
         SPACE                                                                  
UNIT65   BAS   RE,DISDT            DAY/TIME REPORT                              
         MVI   MORESW,YES                                                       
         CLC   SVLNTRY,SVTABNTR    TEST IF MORE ENTRIES TO DO                   
         BL    UNIT80                                                           
         MVI   MORESW,NO                                                        
         SPACE                                                                  
UNIT70   BAS   RE,DISTOT           DAY/TIME REPORT                              
         MVI   MORESW,NO                                                        
         SPACE                                                                  
* END OF REPORT PROCESSING                                                      
*                                                                               
         SPACE                                                                  
UNIT80   MVC   BUYMSG(L'INFOMSG),INFOMSG                                        
         CLI   MORESW,YES          TEST FOR MORE TO COME                        
         BNE   *+14                                                             
         MVC   BUYMSG+L'INFOMSG+1(14),=C'(MORE TO COME)'                        
         B     *+10                                                             
         MVC   BUYMSG+L'INFOMSG+1(20),=C'- ENTER NEXT REQUEST'                  
         CLI   MORESW,YES          TEST FOR END OF DISPLAY                      
         BE    UNIT90              NO                                           
*                                                                               
         XC    SVDATA(SVDATAL),SVDATA CLEAR SAVE AREA                           
         MVI   SVLMODE,EOF         FORCE FIRST TIME NEXT                        
         B     UNITX                                                            
         SPACE                                                                  
UNIT90   OI    BUYACTH+6,X'01'     MODIFIED NEXT TIME                           
         MVI   SVLMODE,PROCESS     PROCESSED THIS TIME                          
         SPACE                                                                  
UNITX    LA    R2,BUYACTH          MODULE EXIT                                  
         ST    R2,FADDR                                                         
         NI    MODE,X'FF'-FIRST-DISPLAY                                         
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE A PRODUCT CODE AGAINST THE BRAND LIST                 
* AT ENTRY, THREE CONTAINS ALPHA CODE                                           
*                                                                               
VALPRD   ST    RE,SAVEREG                                                       
         LA    R0,220              VALIDATE THE CODE AGAINST BRAND LIST         
         LA    RF,CLILIST                                                       
         SPACE                                                                  
VALPRD2  OC    0(4,RF),0(RF)       TEST FOR EOL                                 
         BZ    VALPRDR                                                          
         CLC   THREE,0(RF)                                                      
         BE    VALPRD4                                                          
         LA    RF,4(RF)                                                         
         BCT   R0,VALPRD2                                                       
*                                                                               
VALPRDR  MVI   FERN,PRDERR                                                      
         B     ERROR                                                            
         SPACE                                                                  
VALPRD4  CLI   PROD,0              TEST FOR DUPLICATE FILTER                    
         BE    *+14                                                             
         MVC   XTRA(L'DUPFILT),DUPFILT                                          
         B     UNITR                                                            
*                                                                               
         MVC   PROD,3(RF)          EXTRACT BRAND NUMBER                         
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR VALID PACKAGE STATUS                                 
* ON EXIT, CC=EQ IF VALID PACKAGE STATUS, NEQ IF NOT A VALID STATUS             
*                                                                               
PACKST   ST    RE,SAVEREG                                                       
         ZIC   R1,FLDH+5           VALIDATE PACKAGE STATUS                      
         BCTR  R1,0                                                             
         LA    R0,PAKENT                                                        
         LA    RE,PAKTAB                                                        
         SPACE                                                                  
PACKST2  CLC   FLDH+5(1),8(RE)     TEST FOR MINIMUM INPUT LENGTH                
         BL    PACKST3             NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RE)                                                     
         BE    PACKST4                                                          
*                                                                               
PACKST3  LA    RE,L'PAKTAB(RE)                                                  
         BCT   R0,PACKST2                                                       
         B     PACKSTX                                                          
         SPACE                                                                  
PACKST4  ICM   R1,7,9(RE)          DISPLACEMENT TO FIELD FOR VALUE              
         LA    R1,TEMPD(R1)                                                     
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         MVC   XTRA(L'DUPFILT),DUPFILT                                          
         B     UNITR                                                            
*                                                                               
         MVC   0(1,R1),FLD         SET PARM VALUE                               
         CR    RB,RB               FORCE EXIT WITH CC=EQ                        
         B     PACKSTX                                                          
         SPACE                                                                  
PACKSTX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO READ UNIT FILE AND TO BUILD A TABLE OF DATA BY                 
* PROGRAM/DAY/TIME                                                              
*                                                                               
* FOR PROGRAM READ, TABLE IS BUILT UNTIL SCREEN CAPACITY IS REACHED.            
* FOR DAY/TIME WITHIN PROGRAM READ, ENTIRE INQUIRY IS DONE ON THE               
* FIRST TRANSACTION, FILLING THE TABLE WITH THE NECESSARY DATA.                 
* SUBSEQUENT TRANSACTIONS PICK UP THE DATA FROM THE NEXT LOCATION               
* IN THE TABLE.                                                                 
*                                                                               
READF    NTR1                                                                   
         MVC   NBSELEST(2),EST                                                  
         MVC   NBSELNET,NET                                                     
         MVC   NBSELDP,DAYPART                                                  
         MVC   NBSELPAK,PACK                                                    
         MVC   NBSELPRG,PROG                                                    
         TM    PROGTYP,IFILTER     TEST IF PROGRAM FILTER CODES INPUT           
         BZ    *+16                                                             
         XC    NBSELPRG,NBSELPRG                                                
         MVC   NBSELPFL,PROG       SET FILTER VALUES                            
         OC    STDATE,STDATE       TEST FOR                                     
         BZ    READ2                                                            
         GOTO1 VDATCON,DMCB,(2,STDATE),NBSELSTR                                 
         GOTO1 (RF),(R1),(2,ENDATE),NBSELEND                                    
*                                                                               
READ2    MVI   NBSELMOD,NBPROCUN                                                
         MVI   NBDATA,C'U'                                                      
         CLI   REPOPT,C'T'                                                      
         BNE   READ3                                                            
*                                                                               
         CLI   PACKTYP,IALL                                                     
         BNE   READ2A                                                           
         LA    R2,BUYPAKH                                                       
         ST    R2,FADDR                                                         
         MVC   BUYMSG(L'PAKALLER),PAKALLER                                      
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
READ2A   CLI   NBPOSTYP,C'N'                                                    
         BE    READ2C                                                           
         CLI   NBPOSTYP,C'H'                                                    
         BE    READ2C                                                           
         CLI   NBPOSTYP,C'S'                                                    
         BE    READ2C                                                           
         MVI   NBHUNOPT,C'Y'                                                    
READ2C   MVI   NBDATA,C'B'                                                      
         MVI   NBSELMOD,0                                                       
         MVI   NBRESUME,NBPROCPK                                                
         MVI   NBESTOPT,C'N'                                                    
         MVI   NBACTOPT,C'Y'                                                    
*SET FOR DEMO READ                                                              
         MVC   NBADEM,AIOAREA3     IO3 CONTAINS NETWORK DEMO BLOCK              
         L     RE,NBADEM                                                        
         USING NDDEMBLK,RE                                                      
         MVC   NDDEMOS(60),ESTDEMS                                              
         MVC   NDWGTLST,ESTWLST    NOT CURRENTLY SUPPORTING WEIGTHS             
         MVC   NDUSRNMS,ESTUSNS                                                 
         DROP  RE                                                               
*                                                                               
READ3    MVI   NBSEQ,C'Q'          READ IN PROGRAM SEQUENCE                     
         MVI   NBESTOPT,C'M'                                                    
         MVI   NBUSER+13,NO        FORCE BACK PRE-EMPTS                         
         MVC   NBAIO,AIOAREA1                                                   
         MVC   NBACOM,ACOMFACS                                                  
         MVI   SVLNTRY,0           CLEAR NEXT ENTRY FIELD                       
         SPACE 2                                                                
* READ RECORDS AND POST TO TABLE                                                
*                                                                               
READ4    CLI   REPOPT,C'P'         TEST FOR PROGRAM ONLY REPORT                 
         BNE   *+8                                                              
         BAS   RE,SAVBLOCK                                                      
         MVI   NBNOWRIT,C'N'                                                    
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         BAS   RE,CHKMAXIO         CHECK IO COUNT                               
         XC    NBNOWRIT,NBNOWRIT                                                
         CLI   NBERROR,NBGOOD                                                   
         BE    READ6                                                            
         CLI   NBERROR,NBINVEST    INTERCEPT NO ESTIMATES                       
         BE    *+6                                                              
         DC    H'0'                ELSE TAKE A HIT ON NETIO ERROR               
         MVI   NBMODE,NBREQLST     FORCE EOF                                    
         SPACE 1                                                                
READ6    CLI   NBMODE,NBREQLST     TEST FOR EOF                                 
         BE    READ50              YES                                          
         CLI   NBMODE,NBPROCPK                                                  
         BE    READ20                                                           
         CLI   NBMODE,NBPROCUN                                                  
         BNE   READ4                                                            
         CLI   PROD,0              TEST FOR PRODUCT FILTER                      
         BE    READ7               NO                                           
         CLC   PROD,NBPRD          APPLY IT                                     
         BE    *+14                                                             
         CLC   PROD,NBPRD2                                                      
         BNE   READ4                                                            
         SPACE                                                                  
READ7    CLI   LOCK,C'L'           TEST FOR LOCKED ONLY FILTER                  
         BNE   *+16                                                             
         TM    NBPACKST,X'20'      TEST IF LOCKED UNIT                          
         BO    READ8               YES-INCLUDE ON REPORT                        
         B     READ4               NO-SKIP IT                                   
         CLI   LOCK,C'U'           TEST FOR UNLOCKED ONLY                       
         BNE   READ8                                                            
         TM    NBPACKST,X'20'      FILTER OUT LOCKED UNITS                      
         BO    READ4                                                            
         SPACE                                                                  
READ8    CLI   REPOPT,C'T'         IF TOTAL JUST ACCUMULATE                     
         BE    READ30                                                           
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING PRTABD,R2                                                        
         MVC   PRNET,NBACTNET      BUILD AN ENTRY KEY                           
         MVC   PRPROG,NBACTPRG                                                  
         MVC   PRNAME,NBPROGNM                                                  
         OC    PRNAME,SPACES                                                    
*                                                                               
         LA    RE,DAYTAB           CONVERT DAY BITS TO SORT VALUE               
         LA    R0,DAYS                                                          
         CLC   NBDAY,0(RE)         TEST BITS VS. TABLE                          
         BE    *+12                                                             
         LA    RE,L'DAYTAB(RE)                                                  
         BCT   R0,*-14                                                          
*                                                                               
         CLI   REPOPT,C'P'         TEST FOR PROGRAM REPORT                      
         BE    *+16                YES-ONLY ONE ENTRY FOR PROGRAM               
         MVC   PRSRTDAY,1(RE)      SORT VALUE                                   
         MVC   PRTIME,NBTIME                                                    
         MVC   PRUNITS,=H'1'       INITIALIZE UNIT COUNT                        
         CLI   REPOPT,C'P'                                                      
         BE    *+10                                                             
         MVC   PRDAY,NBDAY                                                      
         MVC   PRCOST,NBACTUAL                                                  
         MVC   PRINTG,NBINTEG                                                   
         MVC   PRGRP+2(2),NBESTHOM+2  HOMES RATING                              
         TM    NBUNITST,X'42'      TEST FOR MISSING/PRE-EMPT                    
         BZ    READ9               NEITHER                                      
         XC    PRUNITS,PRUNITS     COUNTS AS ZERO UNITS                         
         XC    PRCOST,PRCOST       ZERO COST                                    
         XC    PRINTG,PRINTG       ZERO INTEGRATION                             
         XC    PRGRP,PRGRP         AND ZERO GRPS                                
         SPACE 1                                                                
READ9    ICM   RF,15,PRCOST                                                     
         ICM   RE,15,SVTOTC                                                     
         AR    RE,RF                                                            
         STCM  RE,15,SVTOTC        ACCUM COST                                   
*                                                                               
         LA    R0,LINES            MAX SCREEN LINES                             
         CLI   REPOPT,C'P'         TEST FOR PROGRAM REPORT                      
         BE    *+8                 YES                                          
         LA    R0,MAXENTRY         MAXIMUM TABLE ENTRIES                        
         L     R3,AIOAREA2         POINT TO START OF TABLE                      
         SPACE                                                                  
READ10   OC    0(PRKEYL,R3),0(R3)  TEST FOR END OF TABLE                        
         BZ    READ14                                                           
         CLC   PRTABD(PRKEYL),0(R3) TEST IF ENTRY IS IN TABLE                   
         BE    READ12              YES                                          
         LA    R3,PRTABL(R3)                                                    
         BCT   R0,READ10                                                        
*                                                                               
         CLI   REPOPT,C'P'         TEST FOR PROGRAM REPORT                      
         BE    READ50              YES-EXIT                                     
*                                  TABLE IS FILLED - INQUIRY TOO LARGE          
         MVC   BUYMSG(L'TOOMUCH),TOOMUCH                                        
         XC    SVDATA(SVDATAL),SVDATA                                           
         MVI   SVLMODE,EOF         FORCE FIRST TIME NEXT                        
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         NI    MODE,X'FF'-FIRST-DISPLAY                                         
         GOTO1 VEXIT                                                            
*                                                                               
         DROP  R2                                                               
         USING PRTABD,R3                                                        
READ12   SR    R1,R1                                                            
         ICM   R1,3,PRUNITS                                                     
         SR    RE,RE                                                            
         ICM   RE,3,PRUNITS-PRTABD(R2)                                          
         AR    R1,RE                                                            
         STCM  R1,3,PRUNITS                                                     
         MVC   PRDAY,PRDAY-PRTABD(R2)                                           
         ICM   R1,15,PRCOST                                                     
         ICM   RE,15,PRCOST-PRTABD(R2)                                          
         AR    R1,RE                                                            
         STCM  R1,15,PRCOST                                                     
         ICM   R1,15,PRINTG                                                     
         ICM   RE,15,PRINTG-PRTABD(R2)                                          
         AR    R1,RE                                                            
         STCM  R1,15,PRINTG                                                     
         ICM   R1,15,PRGRP                                                      
         ICM   R0,15,PRGRP-PRTABD(R2)                                           
         AR    R1,R0                                                            
         STCM  R1,15,PRGRP                                                      
         B     READ40              READ NEXT RECORD                             
         SPACE                                                                  
READ14   MVC   0(PRTABL,R3),0(R2)  ADD TABLE ENTRY                              
*                                                                               
         ZIC   R1,ENTRIES                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ENTRIES                                                       
         B     READ40                                                           
         SPACE                                                                  
*--IF REPOPT=T THEN GET PACKAGE INFO                                            
READ20   L     RE,NBAIO                                                         
         USING NPRECD,RE                                                        
         MVC   PKGCOST,NPAKCOST                                                 
         MVC   PKGCPM,NPAKGCPM                                                  
         B     READ40                                                           
         DROP  RE                                                               
         SPACE                                                                  
*--IF REPOPT=T THEN ACCUM TOTALS                                                
READ30   TM    NBUNITST,X'42'      IS UNIT A MISSED OR MAKEGOOD                 
         BNZ   READ40              BYPASS                                       
         ICM   RE,15,UNITCT                                                     
         LA    RE,1(RE)                                                         
         STCM  RE,15,UNITCT                                                     
*                                                                               
         L     R1,NBADEM                                                        
         USING NDDEMBLK,R1                                                      
*  POSITION RF TO CORRECT DEMO                                                  
         LA    RE,NDDEMOS                                                       
         LA    RF,NDACTDEM                                                      
         LA    R2,20                                                            
READ32   CLC   DEMO(1),0(RE)                                                    
         BNE   *+14                                                             
         CLC   DEMO+2(1),2(RE)                                                  
         BE    READ36                                                           
         LA    RE,3(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R2,READ32                                                        
         DC    H'0'                                                             
*                                                                               
READ36   ICM   RE,15,4(RF)                                                      
         ICM   RF,15,UIMPS                                                      
         AR    RE,RF                                                            
         STCM  RE,15,UIMPS                                                      
*                                                                               
         ICM   RE,15,UCOST                                                      
         ICM   RF,15,NBACTUAL                                                   
         AR    RE,RF                                                            
         STCM  RE,15,UCOST                                                      
         B     READ40                                                           
         DROP  R1                                                               
         SPACE                                                                  
READ40   B     READ4               READ NEXT RECORD                             
         SPACE                                                                  
READ50   DS    0H                                                               
         SPACE                                                                  
READX    B     EXXMOD                                                           
         EJECT                                                                  
* SET MAXIOCTR TO 90% OF MAX IO'S SHOWN BY GETFACT                              
* CHECK IF I/O OVER                                                             
* RETURN VIA R4OVER                                                             
CHKMAXIO NTR1                                                                   
*                                                                               
         L     R5,ACOMFACS         GET MAX IO                                   
         USING COMFACSD,R5                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
                                                                                
         CLI   MAXIOCTR,0          HAVE WE SET MAXIO ?                          
         BNE   MAX20                           YES                              
         MVC   MAXIOCTR,FATMAXIO-FACTSD(R1)    NO                               
         SR    R2,R2                                                            
         LH    R3,MAXIOCTR                                                      
         LA    R4,9                MULTIPLY MAX IO BY 9                         
         MR    R2,R4                                                            
         LA    R4,10               DIVIDE MAXIMUM BY 10                         
         DR    R2,R4                                                            
         STH   R3,MAXIOCTR                                                      
         B     MAXOK                                                            
MAX20    DS    0H                                                               
         CLC   MAXIOCTR,FATIOCNT-FACTSD(R1)   MAXED OUT ?                       
         BH    MAXOK                                                            
                                                                                
         MVI   FERN,MAXIOERR                                                    
         B     ERROR                                                            
                                                                                
MAXOK    B     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
* SUB-ROUTINE TO OUTPUT PROGRAM DISPLAY SCREEN                                  
*                                                                               
PRODT    NTR1                                                                   
         LA    R2,INUDAT1H                                                      
         USING PLIND,R2                                                         
         L     R4,AIOAREA2         POINT TO DATA AREA                           
         USING PRTABD,R4                                                        
         ZIC   R3,ENTRIES                                                       
*                                                                               
PRODT2   MVC   PLINNET,PRNET                                                    
         MVC   PLINCOD,PRPROG                                                   
         MVC   PLINNAME,PRNAME                                                  
         SR    R0,R0                                                            
         ICM   R0,3,PRUNITS                                                     
         EDIT  (R0),(4,PLINUN)                                                  
         ICM   R1,15,PRCOST                                                     
         SR    R0,R0                                                            
         LTR   R1,R1                                                            
         BNM   *+10                                                             
         BCTR  R0,0                                                             
         B     *+8                                                              
         AH    R1,=H'50'                                                        
         D     R0,=F'100'          ROUND TO NEAREST DOLLAR                      
         LR    R0,R1                                                            
         EDIT  (R0),(8,PLINCOST),MINUS=YES                                      
         ICM   R1,15,PRINTG                                                     
         SR    R0,R0                                                            
         LTR   R1,R1                                                            
         BNM   *+10                                                             
         BCTR  R0,0                                                             
         B     *+8                                                              
         AH    R1,=H'50'                                                        
         D     R0,=F'100'          ROUND TO NEAREST DOLLAR                      
         LR    R0,R1                                                            
         EDIT  (R0),(8,PLININTG),MINUS=YES                                      
         ICM   R0,15,PRGRP                                                      
         EDIT  (R0),(6,PLINGRP),1                                               
         CLI   DEMPREC+1,X'82'                                                  
         BNE   PRODT5                                                           
         EDIT  (R0),(6,PLINGRP),2      CABLE PRECISSION                         
*                                                                               
PRODT5   LA    R2,LINELEN(R2)                                                   
         LA    R4,PRTABL(R4)                                                    
         BCT   R3,PRODT2                                                        
         B     EXXMOD                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO OUTPUT DAY/TIME WITHIN PROGRAM DISPLAY SCREEN                  
*                                                                               
DISDT    NTR1                                                                   
         LA    R2,INUDAT1H                                                      
         USING DLIND,R2                                                         
         L     R4,AIOAREA2         POINT TO DATA AREA                           
         USING PRTABD,R4                                                        
         ZIC   RE,SVLNTRY          INDEX INTO DATA TABLE                        
         LR    R1,RE               SAVE THE VALUE                               
         MH    RE,=Y(PRTABL)                                                    
         LA    R4,0(R4,RE)         INDEX TO NEXT ENTRY                          
         ZIC   R3,SVTABNTR         GET COUNT OF ENTRIES                         
         SR    R3,R1               LESS LAST ENTRY DONE=ENTRIES LEFT            
         LA    R0,LINES            CANNOT BE MORE THAN LINES                    
         CR    R3,R0               TEST ENTRIES LEFT VS. MAX ON SCRN            
         BL    *+6                 IF LESS-USE THAT NUMBER                      
         LR    R3,R0               NO-USE MAX LINES                             
*                                                                               
DISDT2   MVC   DLINNET,PRNET                                                    
         MVC   DLINCOD,PRPROG                                                   
         MVC   DLINNAME,PRNAME                                                  
         GOTO1 VUNDAY,DMCB,PRDAY,DUB                                            
         MVC   DLINDAY,DUB                                                      
         XC    WORK(L'DLINTIME),WORK                                            
         GOTO1 VUNTIME,DMCB,PRTIME,WORK                                         
         MVC   DLINTIME,WORK                                                    
         SR    R0,R0                                                            
         ICM   R0,3,PRUNITS                                                     
         EDIT  (R0),(4,DLINUN)                                                  
         ICM   R1,15,PRCOST                                                     
         SR    R0,R0                                                            
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         BCTR  R0,0                                                             
         AH    R1,=H'50'                                                        
         D     R0,=F'100'          ROUND TO NEAREST DOLLAR                      
         LR    R0,R1                                                            
         EDIT  (R0),(8,DLINCOST),MINUS=YES                                      
         ICM   R1,15,PRINTG                                                     
         SR    R0,R0                                                            
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         BCTR  R0,0                                                             
         AH    R1,=H'50'                                                        
         D     R0,=F'100'          ROUND TO NEAREST DOLLAR                      
         LR    R0,R1                                                            
         EDIT  (R0),(8,DLININTG),MINUS=YES                                      
         ICM   R0,15,PRGRP                                                      
         EDIT  (R0),(6,DLINGRP),1                                               
         CLI   DEMPREC+1,X'82'                                                  
         BNE   DISDT5                                                           
         EDIT  (R0),(6,DLINGRP),2      CABLE PRECISSION                         
*                                                                               
DISDT5   LA    R2,LINELEN(R2)                                                   
         LA    R4,PRTABL(R4)                                                    
         ZIC   R1,SVLNTRY          BUMP LAST ENTRY INDEX                        
         LA    R1,1(R1)                                                         
         STC   R1,SVLNTRY                                                       
         BCT   R3,DISDT2                                                        
         B     EXXMOD                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO OUTPUT TOTAL LINE                                              
*                                                                               
DISTOT   NTR1                                                                   
         LA    R2,INUDAT2H                                                      
         USING TLIND,R2                                                         
*                                                                               
         EDIT  (4,PKGCOST),(8,TLINPCST)                                         
         EDIT  (4,PKGCPM),(11,TLINPCPM),2                                       
         EDIT  (4,UNITCT),(4,TLINUNIT)                                          
         EDIT  (4,UCOST),(11,TLINUCST),2                                        
         EDIT  (4,UIMPS),(11,TLINUIMP)                                          
         CLI   NBHUNOPT,C'Y'                                                    
         BNE   DISTOT20                                                         
         EDIT  (4,UIMPS),(11,TLINUIMP),1                                        
*                                                                               
DISTOT20 BAS   RE,CALCCPM                                                       
         EDIT  (4,UCPMS),(11,TLINUCPM),2                                        
*                                                                               
DISTOTEX B     EXXMOD                                                           
         DROP  R2                                                               
         SPACE 3                                                                
* SUB-ROUTINE TO CALCULATE THE TAREGET CPM                                      
*                                                                               
CALCCPM  NTR1                                                                   
         SR    R0,R0                                                            
         ICM   R1,15,UCOST                                                      
         M     R0,=F'10'                                                        
         CLI   NBHUNOPT,C'Y'                                                    
         BNE   *+8                                                              
         M     R0,=F'10'                                                        
*                                                                               
         ICM   RF,15,UIMPS                                                      
         LTR   RF,RF                                                            
         BZ    CLCPMEX                                                          
         DR    R0,RF                                                            
*                                                                               
         SR    R0,R0                                                            
         A     R1,=F'5'                                                         
         D     R0,=F'10'                                                        
         STCM  R1,15,UCPMS                                                      
CLCPMEX  B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO PUT SAVED AREA FROM IO2                                        
*                                                                               
PUTSAVE  ST    RE,SAVEREG                                                       
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,1            FIRST SAVED PAGE                             
         MVC   DMCB+10(2),TERM                                                  
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,,AIOAREA2                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET SAVED AREA TO IO2                                          
*                                                                               
GETSAVE  ST    RE,SAVEREG                                                       
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,1                                                         
         MVC   DMCB+10(2),TERM                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,,AIOAREA2                           
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CLEAR SAVED BLOCK                                              
*                                                                               
CLRBLOCK ST    RE,SAVEREG          SAVE RETURN POINT                            
         LA    RE,SVNBLOCK                                                      
         LA    RF,NEBLOCKL                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO RESTORE SAVED BLOCK                                            
*                                                                               
RESTBLK  ST    RE,SAVEREG                                                       
         LA    RE,NEBLOCKA                                                      
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,SVNBLOCK                                                      
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE NETBLOCK IN TWA SAVE AREA                                 
*                                                                               
SAVBLOCK ST    RE,SAVEREG                                                       
         LA    RE,SVNBLOCK                                                      
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,NEBLOCKA                                                      
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* MODULE AND ROUTINE EXIT                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* EXECUTED INSTRUCTIONS                                                         
*                                                                               
DAYTCOM  CLC   FLD(0),=C'DAYTIME'                                               
YESCOMP  CLC   FLD(0),=C'YES'                                                   
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
DMWRT    DC    CL6'DMWRT'                                                       
DMREAD   DC    CL6'DMREAD'                                                      
TEMPSTR  DC    C'TEMPSTR'                                                       
INFOMSG  DC    C'** PROGRAMS DISPLAYED'                                         
NODATA   DC    C'** NO DATA TO DISPLAY - ENTER NEXT REQUEST **'                 
TOOMUCH  DC    C'** INQUIRY IS TOO LARGE - PLEASE CHANGE REQUEST **'            
PAKALLER DC    C'** SINGLE PACKAGE REQUIRED FOR TOTAL ACTION **'                
DUPFILT  DC    C'DUPLICATE FILTER'                                              
DEMERR   DC    C'DEMO INPUT INVALID'                                            
DEMEST   DC    C'DEMO NOT ON ESTIMATE'                                          
DAYHED1  DC    C' NET  CODE    NAME              DAY '                          
DAYHED2  DC    C'TIME       UNITS    COST     INTG   GRPS'                      
PROGHED1 DC    C' NET   CODE    NAME              '                             
PROGHED2 DC    C'UNITS    COST      INTG    GRPS'                               
TOTHED1  DC    C' PKG COST      ACT COST       PKG CPM'                         
TOTHED2  DC    C'       UNIT CPM       UNIT IMP  UNITS'                         
         SPACE 2                                                                
* TABLE OF PACKAGE STATUS FILTERS                                               
*                                                                               
* BYTES 0-7 = CHARACTER STATUS FILTER                                           
* BYTE  8   = MININUM NUMBER OF CHARACTERS                                      
* BYTES 9-11= DISPLACEMENT INTO LOCAL STORAGE FOR FILTER VALUE                  
*                                                                               
PAKTAB   DS    0CL12                                                            
         DC    CL8'LOCKED',X'01',AL3(LOCK-TEMPD)                                
         DC    CL8'UNLOCKED',X'03',AL3(LOCK-TEMPD)                              
         DC    CL8'TOTAL',X'03',AL3(REPOPT-TEMPD)                               
PAKENT   EQU   (*-PAKTAB)/L'PAKTAB                                              
         SPACE 2                                                                
* TABLE OF DAY BITS AND THEIR CORRESPONDING SORT VALUES                         
*                                                                               
DAYTAB   DS    0CL2                                                             
         DC    X'40',X'01'         MONDAY                                       
         DC    X'20',X'02'         TUESDAY                                      
         DC    X'10',X'03'                                                      
         DC    X'08',X'04'                                                      
         DC    X'04',X'05'                                                      
         DC    X'02',X'06'                                                      
         DC    X'01',X'07'         SUNDAY                                       
         DC    X'7C',X'08'         M-F                                          
         DC    X'7F',X'09'         M-SUN                                        
DAYS     EQU   (*-DAYTAB)/L'DAYTAB                                              
         DC    X'FF',X'0A'         DEFAULT FOR VARIABLE DAY CODES               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
       ++INCLUDE NETDEMOD                                                       
* INFO UNIT SCREEN                                                              
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYF7D                                                       
         SPACE 2                                                                
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVDATA   DS    0D                                                               
SVLMODE  DS    X                   LAST TIME PROCESS MODE                       
SVLNTRY  DS    X                   POINTER TO NEXT ENTRY                        
SVTABNTR DS    X                   COUNT OF DATA TABLE ENTRIES                  
SVTOTC   DS    XL4                 TOTAL UNIT COST                              
*                                                                               
SVVALS   DS    0C                  LAST TIME CONTROL VALUES                     
SVPROD   DS    X                                                                
SVSTDATE DS    XL2                                                              
SVENDATE DS    XL2                                                              
SVDP     DS    C                                                                
SVLOCK   DS    C                                                                
SVLOPT   DS    C                   LAST TIME REPORT OPTION (D,P)                
SVVALSLN EQU   *-SVVALS                                                         
*                                                                               
SVDATAL  EQU   *-SVDATA                                                         
*                                                                               
SVNBLOCK DS    XL(NEBLOCKL)                                                     
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
MORESW   DS    C                                                                
DEMO     DS    CL3                                                              
*                                                                               
*--ACCUMULATORS FOR TYPE T REPORT                                               
PKGCOST  DS    F                   PACKAGE COST                                 
PKGCPM   DS    F                   PACKAGE CPM                                  
UNITCT   DS    F                   UNIT COUNT                                   
UCOST    DS    F                   UNIT COST                                    
UIMPS    DS    F                   UNIT IMPRESSIONS                             
UCPMS    DS    F                   UNIT CPM                                     
*                                                                               
THISVALS DS    0CL(SVVALSLN)       THIS TIME CONTROL VALUES                     
PROD     DS    X                                                                
STDATE   DS    XL2                                                              
ENDATE   DS    XL2                                                              
DAYPART  DS    C                                                                
LOCK     DS    C                                                                
REPOPT   DS    C                                                                
MAXIOCTR DS    H                                                                
*                                                                               
ENTRIES  DS    X                                                                
*                                                                               
         DS    0D                                                               
BLOCK    DS    CL256                                                            
         SPACE 2                                                                
* DSECT TO COVER DAY/TIME W/IN PROGRAM SCREEN LINE                              
*                                                                               
DLIND    DSECT                                                                  
DLINFLDH DS    CL8                                                              
         DS    C                                                                
DLINNET  DS    CL4                                                              
         DS    CL1                 SPARE                                        
DLINCOD  DS    CL6                                                              
         DS    CL2                                                              
DLINNAME DS    CL16                                                             
         DS    CL2                                                              
DLINDAY  DS    CL3                                                              
         DS    CL1                                                              
DLINTIME DS    CL10                                                             
         DS    C                                                                
DLINUN   DS    CL4                                                              
         DS    CL2                                                              
DLINCOST DS    CL8                                                              
         DS    CL1                                                              
DLININTG DS    CL8                                                              
         DS    CL1                                                              
DLINGRP  DS    CL6                                                              
         SPACE 2                                                                
* DSECT TO COVER UNITS BY PROGRAM SCREEN LINE                                   
*                                                                               
PLIND    DSECT                                                                  
PLINFLDH DS    CL8                                                              
         DS    C                                                                
PLINNET  DS    CL4                                                              
         DS    CL2                 SPARE                                        
PLINCOD  DS    CL6                                                              
         DS    CL2                                                              
PLINNAME DS    CL16                                                             
         DS    CL2                                                              
PLINUN   DS    CL4                                                              
         DS    CL2                                                              
PLINCOST DS    CL8                                                              
         DS    CL2                                                              
PLININTG DS    CL8                                                              
         DS    CL2                                                              
PLINGRP  DS    CL6                                                              
         SPACE 2                                                                
* DSECT TO COVER TOTAL UNITS LINE                                               
*                                                                               
TLIND    DSECT                                                                  
TLINFLDH DS    CL8                                                              
         DS    C                                                                
TLINPCST DS    CL8                                                              
         DS    CL3                 SPARE                                        
TLINUCST DS    CL11                                                             
         DS    CL3                                                              
TLINPCPM DS    CL11                                                             
         DS    CL4                                                              
TLINUCPM DS    CL11                                                             
         DS    CL4                                                              
TLINUIMP DS    CL11                                                             
         DS    CL3                                                              
TLINUNIT DS    CL4                                                              
* DSECT TO COVER TABLE OF UNIT DATA BY PROGRAM                                  
*                                                                               
PRTABD   DSECT                                                                  
PRNET    DS    CL4                                                              
PRPROG   DS    CL6                                                              
PRNAME   DS    CL16                                                             
PRSRTDAY DS    X                   DAY VALUE FOR SORTING                        
PRTIME   DS    XL4                                                              
PRKEYL   EQU   *-PRTABD            LENGTH OF SORT KEY                           
PRUNITS  DS    XL2                 START OF ENTRY DATA                          
PRDAY    DS    X                                                                
PRCOST   DS    XL4                                                              
PRINTG   DS    XL4                                                              
PRGRP    DS    XL4                                                              
PRTABL   EQU   *-PRTABD                                                         
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
SLASH    EQU   C'/'                                                             
EQUAL    EQU   C'='                                                             
LINELEN  EQU   INUDAT2H-INUDAT1H                                                
LINES    EQU   (INULAST-INUDAT1H)/LINELEN                                       
SAVELEN  EQU   6144                LENGTH OF SAVE AREA                          
MAXENTRY EQU   SAVELEN/PRTABL      MAXIMUM ENTRIES IN TABLE                     
PROCESS  EQU   X'01'               PROCESSED DISPLAY LAST TIME                  
EOF      EQU   X'02'               FINISHED DISPLAY LAST TIME                   
         SPACE 2                                                                
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'101NEBUY21S  05/01/02'                                      
         END                                                                    
