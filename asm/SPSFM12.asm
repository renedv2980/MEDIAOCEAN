*          DATA SET SPSFM12    AT LEVEL 043 AS OF 08/11/11                      
*PHASE T21712A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T21712 - SIR NSID PERIOD COPY                         *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS PCOPY ACTION ONLY                            *         
*                                                                     *         
*  INPUTS       SCREEN T217AC (PCOPY SCREEN)                          *         
*                                                                     *         
*  OUTPUTS      NEW NSID AND DETAIL RECORDS                           *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- SIRRECD                                         *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- SECOND BASE                                     *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - NSID RECORD                                     *         
*               IO2 - 'FROM' SCHEME                                   *         
*               IO3 - 'TO' SCHEME                                     *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21712 - SIR NSID PERIOD COPY'                                  
T21712   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21712**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
*                                                                               
         CLI   MODE,VALREC         COPY RECORDS ONLINE                          
         BE    ONCOPY                                                           
*                                                                               
         CLI   MODE,PRINTREP       COPY RECORDS OFFLINE                         
         BE    OFFCOPY                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,=X'0900000000010000E3'                                        
         GOTO1 VALIMED             FAKE THE MEDIA FIELD TO 'T'                  
*                                                                               
         LA    R2,SIRFRYRH         'FROM' YEAR                                  
         XC    WORK,WORK                                                        
         GOTO1 PERVAL,DMCB,(SIRFRYRH+5,SIRFRYR),('PVINSGLO',WORK)               
*                                                                               
         CLI   DMCB+4,PVRCOK       YEAR WAS OK?                                 
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR             NO                                           
*                                                                               
         LA    RF,WORK             PERVAL OUTPUT AREA                           
         MVC   FROMYEAR,PVALBSTA-PERVALD(RF)  BINARY YEAR                       
*                                                                               
         XC    FROMSCH,FROMSCH     'FROM' SCHEME                                
         LA    R2,SIRFRSCH                                                      
         CLI   SIRFRSCH+5,0        REQUIRED                                     
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         CLC   =C'ALL',SIRFRSC     TEST SCHEME FOR ENTIRE AGENCY                
         BE    VK10                                                             
         OC    SIRFRSC,SPACES                                                   
         GOTO1 CLPACK,DMCB,SIRFRSC,FROMSCH                                      
         CLI   DMCB,0              TEST VALID SCHEME CODE                       
         BE    VK10                YES                                          
         MVI   ERROR,INVSCH                                                     
         B     TRAPERR                                                          
*                                                                               
VK10     MVI   USEREP,C'N'         ASSUME SCHEME DOESN'T USE REP MKTS           
         XC    WORK,WORK           READ SID PROFILE                             
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         CLC   SIRFRSC,=C'ALL'                                                  
         BE    *+10                                                             
         MVC   WORK+23(3),SIRFRSC                                               
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    *+16                NO                                           
         CLI   WORK+1,C'Y'         TEST READ REP MARKET RECORDS                 
         BNE   *+8                 NO                                           
         MVI   USEREP,C'Y'         SET REP FLAG                                 
*                                                                               
         LA    R4,KEY              CREATE THE SCHEME KEY                        
         USING SIRRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   SIRKTYPE,SIRKTYPQ                                                
         MVC   SIRKAM,BAGYMD                                                    
         MVC   SIRKCODE,FROMSCH                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST SCHEME EXISTS                           
         BE    *+12                                                             
         MVI   ERROR,NOSCHM                                                     
         B     TRAPERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC              GET THE SCHEME RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EPNCODEQ     PERIOD NAMES ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'5'            R1 = NO. OF PERIODS                          
         LA    R5,2(R5)            FIRST PERIOD NAME                            
*                                                                               
         LA    R2,SIRFRPRH         'FROM' PERIOD                                
         CLI   SIRFRPRH+5,0                                                     
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         OC    SIRFRPR,SPACES      PAD NAME WITH BLANKS                         
*                                                                               
VK20     CLC   SIRFRPR(4),1(R5)    LOOK FOR MATCH OF PERIOD NAME                
         BE    VK30                                                             
         LA    R5,5(R5)                                                         
         BCT   R1,VK20                                                          
         MVI   ERROR,INVBUYP                                                    
         B     TRAPERR                                                          
*                                                                               
VK30     MVC   FROMPER,0(R5)       SAVE THE PERIOD NUMBER                       
         MVI   LASTFPER,C'N'       ASSUME IT'S NOT THE LAST PERIOD              
         CH    R1,=H'1'            TEST THIS IS THE LAST PERIOD                 
         BNE   *+8                 NO                                           
         MVI   LASTFPER,C'Y'                                                    
*                                                                               
         MVC   SIRKYEAR,FROMYEAR   BUILD THE PERIOD KEY                         
         XI    SIRKYEAR,X'FF'      YEAR IN ONE'S COMPLEMENT FORM                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR PERIOD KEY                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERROR,NOPERREC                                                   
         B     TRAPERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET THE PERIOD RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EPDCODEQ     LOOK FOR PERIOD ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EPDELEM,R5                                                       
VK40     CLC   FROMPER,EPDNUM      LOOK FOR THE GIVEN PERIOD                    
         BE    VK50                                                             
         BAS   RE,NEXTEL                                                        
         BE    VK40                                                             
         MVI   ERROR,INVBUYP                                                    
         B     TRAPERR                                                          
         DROP  R5                                                               
*                                                                               
VK50     LA    R2,SIRTOYRH         'TO' YEAR                                    
         XC    WORK,WORK                                                        
         GOTO1 PERVAL,DMCB,(SIRTOYRH+5,SIRTOYR),('PVINSGLO',WORK)               
*                                                                               
         CLI   DMCB+4,PVRCOK       YEAR WAS OK?                                 
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR             NO                                           
*                                                                               
         LA    RF,WORK             PERVAL OUTPUT AREA                           
         MVC   TOYEAR,PVALBSTA-PERVALD(RF)  BINARY YEAR                         
*                                                                               
         XC    TOSCH,TOSCH         'TO' SCHEME                                  
         LA    R2,SIRTOSCH                                                      
         CLI   SIRTOSCH+5,0        REQUIRED                                     
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         CLC   =C'ALL',SIRTOSC     TEST SCHEME FOR ENTIRE AGENCY                
         BE    VK60                                                             
         OC    SIRTOSC,SPACES                                                   
         GOTO1 CLPACK,DMCB,SIRTOSC,TOSCH                                        
         CLI   DMCB,0              TEST VALID SCHEME CODE                       
         BE    VK60                YES                                          
         MVI   ERROR,INVSCH                                                     
         B     TRAPERR                                                          
*                                                                               
VK60     MVI   BYTE,C'N'           ASSUME SCHEME DOESN'T USE REP MKTS           
         XC    WORK,WORK           READ SID PROFILE                             
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         CLC   SIRTOSC,=C'ALL'                                                  
         BE    *+10                                                             
         MVC   WORK+23(3),SIRTOSC                                               
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    *+16                NO                                           
         CLI   WORK+1,C'Y'         TEST READ REP MARKET RECORDS                 
         BNE   *+8                 NO                                           
         MVI   BYTE,C'Y'           SET REP FLAG                                 
         CLC   USEREP,BYTE         TEST SCHEMES HAVE SAME PROFILE VALUE         
         BNE   NOTREP              NO                                           
*                                                                               
         XC    KEY,KEY             CREATE THE SCHEME KEY                        
         MVI   SIRKTYPE,SIRKTYPQ                                                
         MVC   SIRKAM,BAGYMD                                                    
         MVC   SIRKCODE,TOSCH                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST SCHEME EXISTS                           
         BE    *+12                                                             
         MVI   ERROR,NOSCHM                                                     
         B     TRAPERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC              GET THE SCHEME RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EPNCODEQ     PERIOD NAMES ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'5'            R1 = NO. OF PERIODS                          
         LR    R0,R1                                                            
         LA    R5,2(R5)            FIRST PERIOD NAME                            
*                                                                               
         LA    R2,SIRTOPRH         'TO' PERIOD                                  
         CLI   SIRTOPRH+5,0                                                     
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         MVI   PERFLAG,C'N'        ASSUME NO "/Y" OPTION                        
         MVI   ERROR,INVBUYP                                                    
         GOTO1 SCANNER,DMCB,SIRTOPRH,BLOCK,C',=/ '                              
         CLI   DMCB+4,0                                                         
         BE    TRAPERR                                                          
         CLI   DMCB+4,2                                                         
         BH    TRAPERR             TOO MANY FIELDS                              
         BL    VK70                NO "/Y" OPTION                               
         CLI   BLOCK,4                                                          
         BH    TRAPERR             PERIOD NAME TOO LONG                         
         CLI   BLOCK+32,0          SECOND FIELD IS AT +32 IN BLOCK              
         BE    VK70                NO "/Y" OPTION                               
         CLI   BLOCK+32,1                                                       
         BNE   TRAPERR             SUB-OPTION TOO LONG                          
         CLI   BLOCK+44,C'Y'                                                    
         BNE   TRAPERR             UNKNOWN OPTION                               
         MVI   PERFLAG,C'Y'        "/Y" MEANS ANY COPY IS ALLOWED               
*                                                                               
VK70     CLC   BLOCK+12(4),1(R5)   LOOK FOR MATCH OF PERIOD NAME                
         BE    VK80                                                             
         LA    R5,5(R5)                                                         
         BCT   R0,VK70                                                          
         B     TRAPERR                                                          
*                                                                               
VK80     MVC   TOPER,0(R5)         SAVE THE PERIOD NUMBER                       
*                                                                               
         MVC   SIRKYEAR,TOYEAR     BUILD THE PERIOD KEY                         
         XI    SIRKYEAR,X'FF'      YEAR IN ONE'S COMPLEMENT FORM                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR PERIOD KEY                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERROR,NOPERREC                                                   
         B     TRAPERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET THE PERIOD RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EPDCODEQ     LOOK FOR PERIOD ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EPDELEM,R5                                                       
VK90     CLC   TOPER,EPDNUM        LOOK FOR THE GIVEN PERIOD                    
         BE    VK100                                                            
         BAS   RE,NEXTEL                                                        
         BE    VK90                                                             
         MVI   ERROR,INVBUYP                                                    
         B     TRAPERR                                                          
*                                                                               
VK100    MVC   SVPERST,EPDSTART    SAVE PERIOD START AND END DATES              
         MVC   SVPEREND,EPDEND                                                  
         DROP  R5                                                               
*                                                                               
         CLC   FROMSCH,TOSCH       TEST SCHEMES ARE THE SAME                    
         BNE   VK120               NO - COPY ANY PERIOD TO ANY OTHER            
         CLI   PERFLAG,C'Y'        USER SAID "/Y"?                              
         BE    VK120               YES -- ANY COPY OK                           
*                                                                               
         ZIC   RE,TOYEAR                                                        
         ZIC   R0,FROMYEAR                                                      
         SR    RE,R0               RE = DIFFERENCE IN YEARS                     
         ZIC   RF,TOPER                                                         
         ZIC   R0,FROMPER                                                       
         SR    RF,R0               RF = DIFFERENCE IN PERIODS                   
*                                                                               
         CLI   LASTFPER,C'Y'       TEST 'FROM' PERIOD WAS THE LAST              
         BE    VK110                                                            
*                                                                               
         CLC   FROMYEAR,TOYEAR     TEST YEARS ARE THE SAME                      
         BNE   *+16                                                             
         CH    RF,=H'1'            TEST ONE PERIOD DIFFERENCE                   
         BE    VK120               YES                                          
         B     INVTARG             INVALID                                      
*                                                                               
         CH    RE,=H'1'            TEST ONE YEAR DIFFERENCE                     
         BNE   INVTARG             NO                                           
         CLC   FROMPER,TOPER       TEST PERIODS ARE THE SAME                    
         BE    VK120                                                            
         B     INVTARG                                                          
*                                                                               
VK110    CH    RE,=H'1'            TEST ONE YEAR DIFFERENCE                     
         BNE   INVTARG                                                          
         CLC   FROMPER,TOPER       TEST PERIODS ARE THE SAME                    
         BE    VK120                                                            
         CLI   TOPER,1             TEST 'TO' PERIOD IS THE FIRST                
         BNE   INVTARG                                                          
*                                                                               
VK120    XC    BMKTSTA,BMKTSTA     STATION                                      
         LA    R2,SIRFRSTH                                                      
         CLI   SIRFRSTH+5,0                                                     
         BNE   *+12                                                             
         MVI   ERROR,MISSING       FILTER IS REQUIRED                           
         B     TRAPERR                                                          
*                                                                               
         CLI   SIRFRSTH+5,3        TEST ALL STATION REQUEST                     
         BNE   VK125                                                            
         CLC   =C'ALL',SIRFRST                                                  
         BNE   VK125               NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         CLC   WORK+6(3),SPACES    TEST VALID DAY OF WEEK                       
         BNE   *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         CLI   OFFLINE,C'Y'        DON'T BOTHER CHECKING DAY IF OFFLINE         
         BE    *+12                                                             
         CLI   DMCB,5              TEST TODAY IS FRIDAY                         
         BNE   NOTFRI              NO                                           
*                                                                               
         TM    WHEN,X'80'          TEST ONLINE COPY                             
         BO    NOTNOW              MUST HAVE FILTER FOR ONLINE                  
         B     VK140                                                            
*                                                                               
VK125    TM    SIRFRSTH+4,X'08'    TEST NUMERIC                                 
         BZ    VK130               NO - IT'S A STATION CODE                     
*                                                                               
         TM    WHEN,X'80'          TEST ONLINE COPY                             
         BO    NOTNOW              MUST HAVE FILTER FOR ONLINE                  
*                                                                               
         CLI   USEREP,C'Y'         TEST READING REP MARKETS                     
         BNE   VK127               NO                                           
         GOTO1 VALIRMKT            YES                                          
         B     VK140                                                            
VK127    GOTO1 VALIMKT             IT'S A MARKET CODE                           
         B     VK140               NO STATION FILTER                            
*                                                                               
VK130    CLI   USEREP,C'Y'         TEST READING REP MARKETS                     
         BNE   VK137               NO                                           
         GOTO1 VALIRSTA            YES                                          
         B     VK140                                                            
VK137    GOTO1 VALISTA             VALIDATE STATION CODE                        
*                                                                               
VK140    L     R5,AIO2             'FROM' SCHEME RECORD                         
         MVI   ELCODE,EDCCODEQ     LOOK FOR DAYPART CODES                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF DAYPARTS                         
         LR    R0,R1                                                            
         LA    R5,2(R5)            FIRST DAYPART                                
         MVC   WORK,SPACES                                                      
         SR    R2,R2                                                            
*                                                                               
VK150    TRT   0(1,R5),TRTABLE     TEST VALID DAYPART                           
         BZ    *+14                NO - IGNORE NON-ALPHA CODE IN RECORD         
         LA    R1,WORK-1(R2)       R2 = (DISP INTO WORK) + 1                    
         MVC   0(1,R1),0(R5)       PUT DAYPART CODE IN WORK                     
         LA    R5,8(R5)            NEXT DAYPART                                 
         BCT   R0,VK150                                                         
*                                                                               
         CLI   SIRFRDPH+5,0        TEST FILTERS WERE GIVEN                      
         BNE   *+14                YES                                          
         MVC   DAYPARTS,WORK       NO - USE EVERYTHING IN THE SCHEME            
         B     VK180                                                            
*                                                                               
         LA    R3,SIRFRDP          R3 = BEGINNING OF FILTER FIELD               
         CLI   SIRFRDP,C'-'        TEST NEGATIVE FILTERS                        
         BE    *+14                YES                                          
         MVC   DAYPARTS,SPACES     NO - BEGIN WITH NOTHING                      
         B     VK160                                                            
*                                                                               
         MVC   DAYPARTS,WORK       BEGIN WITH ENTIRE LIST                       
         LA    R3,1(R3)            BUMP PAST MINUS SIGN                         
*                                                                               
VK160    TRT   0(1,R3),TRTABLE     TEST VALID DAYPART                           
         BZ    INVDAYPT            NO                                           
         LA    R1,WORK-1(R2)       R2 = (DISP INTO WORK) + 1                    
         CLI   0(R1),C' '          TEST DAYPART IS IN 'FROM' SCHEME             
         BE    INVDAYPT            NO                                           
*                                                                               
         LA    R1,DAYPARTS-1(R2)   A(DAYPARTS) OF THIS CHARACTER                
         CLI   SIRFRDP,C'-'        TEST NEGATIVE FILTERS                        
         BNE   *+12                NO                                           
         MVI   0(R1),C' '          REMOVE DAYPART FROM LIST                     
         B     *+10                                                             
         MVC   0(1,R1),0(R3)       PUT DAYPART IN LIST                          
*                                                                               
         LA    R3,1(R3)            BUMP                                         
         CLI   0(R3),C' '          TEST ANY MORE DATA                           
         BNH   VK170               NO                                           
*                                                                               
         CLI   0(R3),C','          TEST COMMA                                   
         BE    *+16                YES - OK                                     
         LA    R2,SIRFRDPH                                                      
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
         LA    R3,1(R3)            BUMP TO NEXT FILTER                          
         CLI   SIRFRDP,C'-'        TEST NEGATIVE FILTERS                        
         BNE   VK160               NO                                           
         CLI   0(R3),C'-'          IF FIRST FILTER WAS NEG, ALL MUST BE         
         BNE   BADFILT                                                          
         LA    R3,1(R3)            BUMP PAST MINUS SIGN                         
         B     VK160                                                            
*                                                                               
VK170    CLC   DAYPARTS,SPACES     TEST ANY DAYPARTS LEFT TO FILTER ON          
         BE    NODPTLEF            NO                                           
*                                                                               
VK180    CLC   FROMSCH,TOSCH       TEST SAME SCHEMES                            
         BE    VK200               YES                                          
*                                                                               
         L     R5,AIO3             'TO' SCHEME RECORD                           
         MVI   ELCODE,EDCCODEQ     LOOK FOR DAYPART CODES                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF DAYPARTS                         
         LR    R0,R1                                                            
         LA    R5,2(R5)            FIRST DAYPART                                
         MVC   WORK,SPACES                                                      
         SR    R2,R2                                                            
*                                                                               
VK190    TRT   0(1,R5),TRTABLE     TEST VALID DAYPART                           
         BZ    *+14                NO - IGNORE NON-ALPHA CODE IN RECORD         
         LA    R1,WORK-1(R2)       R2 = (DISP INTO WORK) + 1                    
         MVC   0(1,R1),0(R5)       PUT DAYPART CODE IN WORK                     
         LA    R5,8(R5)            NEXT DAYPART                                 
         BCT   R0,VK190                                                         
*                                                                               
         XC    WORK(26),DAYPARTS   THOSE LEFT OVER COULD BE INVALID             
         NC    WORK(26),DAYPARTS   LEAVES ONLY THOSE WHICH ARE INVALID          
         OC    WORK(26),SPACES     CONVERT LOWER TO UPPER                       
         CLC   WORK(26),SPACES     TEST ANY INVALID                             
         BE    VK200               NO                                           
*                                                                               
         LA    R3,WORK                                                          
         CLI   0(R3),C' '                                                       
         BNE   INVDAYP2            R3 POINTS TO INVALID DAYPART                 
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
VK200    CLI   SIRFRDPH+5,0        TEST FILTERS WERE GIVEN                      
         BNE   *+14                YES                                          
         XC    DAYPARTS,DAYPARTS                                                
         B     VK230                                                            
*                                                                               
         LA    R1,DAYPARTS         SQUASH ALL DAYPARTS TO THE LEFT              
         CLI   0(R1),C' '          BUMP TO FIRST BLANK POSITION                 
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         LA    R0,DAYPARTS+26                                                   
         LA    R3,1(R1)                                                         
*                                                                               
VK210    CLI   0(R3),C' '          TEST THERE'S A DAYPART HERE                  
         BE    *+18                NO                                           
         MVC   0(1,R1),0(R3)       MOVE IT TO THE FAR LEFT                      
         MVI   0(R3),C' '                                                       
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         CR    R3,R0               TEST END OF LIST                             
         BNE   VK210                                                            
*                                                                               
         CLC   DAYPARTS+16(10),SPACES   TEST MORE THAN 16 DAYPARTS              
         BE    VK230               NO                                           
         LA    R2,SIRFRDPH                                                      
         CLI   SIRFRDP,C'-'        TEST NEGATIVE FILTERS                        
         BE    TOOFEWN             YES - WE NEED MORE                           
         B     TOOMANY             NO - THERE ARE TOO MANY                      
*                                                                               
VK230    L     R5,AIO2             'FROM' SCHEME RECORD                         
         MVI   ELCODE,EPCCODEQ     LOOK FOR PROGTYPE CODES                      
         BAS   RE,GETEL                                                         
         BE    VK235                                                            
*                                                                               
         CLI   SIRFRPTH+5,0        TEST ANY PROGTYPES GIVEN                     
         BNE   NOPROGST            YES - BUT NONE ARE DEFINED                   
         XC    PRGTYPES,PRGTYPES                                                
         B     VKX                                                              
*                                                                               
VK235    SR    R0,R0                                                            
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF PROGTYPES                        
         LR    R0,R1                                                            
         LA    R5,2(R5)            FIRST PROGTYPE                               
         MVC   WORK,SPACES                                                      
         SR    R2,R2                                                            
*                                                                               
VK240    TRT   0(1,R5),TRTABLE     TEST VALID PROGTYPE                          
         BZ    *+14                NO - IGNORE NON-ALPHA CODE IN RECORD         
         LA    R1,WORK-1(R2)       R2 = (DISP INTO WORK) + 1                    
         MVC   0(1,R1),0(R5)       PUT PROGTYPE CODE IN WORK                    
         LA    R5,8(R5)            NEXT PROGTYPE                                
         BCT   R0,VK240                                                         
*                                                                               
         CLI   SIRFRPTH+5,0        TEST FILTERS WERE GIVEN                      
         BNE   *+14                YES                                          
         MVC   PRGTYPES,WORK       NO - USE EVERYTHING IN THE SCHEME            
         B     VK270                                                            
*                                                                               
         LA    R3,SIRFRPT          R3 = BEGINNING OF FILTER FIELD               
         CLI   SIRFRPT,C'-'        TEST NEGATIVE FILTERS                        
         BE    *+14                YES                                          
         MVC   PRGTYPES,SPACES     NO - BEGIN WITH NOTHING                      
         B     VK250                                                            
*                                                                               
         MVC   PRGTYPES,WORK       BEGIN WITH ENTIRE LIST                       
         LA    R3,1(R3)            BUMP PAST MINUS SIGN                         
*                                                                               
VK250    TRT   0(1,R3),TRTABLE     TEST VALID PROGTYPE                          
         BZ    INVPROGT            NO                                           
         LA    R1,WORK-1(R2)       R2 = (DISP INTO WORK) + 1                    
         CLI   0(R1),C' '          TEST PROGTYPE IS IN 'FROM' SCHEME            
         BE    INVPROGT            NO                                           
*                                                                               
         LA    R1,PRGTYPES-1(R2)   A(PRGTYPES) OF THIS CHARACTER                
         CLI   SIRFRPT,C'-'        TEST NEGATIVE FILTERS                        
         BNE   *+12                NO                                           
         MVI   0(R1),C' '          REMOVE PROGTYPE FROM LIST                    
         B     *+10                                                             
         MVC   0(1,R1),0(R3)       PUT PROGTYPE IN LIST                         
*                                                                               
         LA    R3,1(R3)            BUMP                                         
         CLI   0(R3),C' '          TEST ANY MORE DATA                           
         BNH   VK260               NO                                           
*                                                                               
         CLI   0(R3),C','          TEST COMMA                                   
         BE    *+16                YES - OK                                     
         LA    R2,SIRFRPTH                                                      
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
         LA    R3,1(R3)            BUMP TO NEXT FILTER                          
         CLI   SIRFRPT,C'-'        TEST NEGATIVE FILTERS                        
         BNE   VK250               NO                                           
         CLI   0(R3),C'-'          IF FIRST FILTER WAS NEG, ALL MUST BE         
         BNE   BADFILT                                                          
         LA    R3,1(R3)            BUMP PAST MINUS SIGN                         
         B     VK250                                                            
*                                                                               
VK260    CLC   PRGTYPES,SPACES     TEST ANY PROGTYPES LEFT TO FILTER ON         
         BE    NOPGTLEF            NO                                           
*                                                                               
VK270    CLC   FROMSCH,TOSCH       TEST SAME SCHEMES                            
         BE    VK300               YES                                          
*                                                                               
         L     R5,AIO3             'TO' SCHEME RECORD                           
         MVI   ELCODE,EPCCODEQ     LOOK FOR PROGTYPE CODES                      
         BAS   RE,GETEL                                                         
         BNE   NOPROGSF            ERROR - THERE ARE NONE                       
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF PROGTYPES                        
         LR    R0,R1                                                            
         LA    R5,2(R5)            FIRST PROGTYPE                               
         MVC   WORK,SPACES                                                      
         SR    R2,R2                                                            
*                                                                               
VK280    TRT   0(1,R5),TRTABLE     TEST VALID PROGTYPE                          
         BZ    *+14                NO - IGNORE NON-ALPHA CODE IN RECORD         
         LA    R1,WORK-1(R2)       R2 = (DISP INTO WORK) + 1                    
         MVC   0(1,R1),0(R5)       PUT PROGTYPE CODE IN WORK                    
         LA    R5,8(R5)            NEXT PROGTYPE                                
         BCT   R0,VK280                                                         
*                                                                               
         XC    WORK(26),PRGTYPES   THOSE LEFT OVER COULD BE INVALID             
         NC    WORK(26),PRGTYPES   LEAVES ONLY THOSE WHICH ARE INVALID          
         OC    WORK(26),SPACES     CONVERT LOWER TO UPPER                       
         CLC   WORK(26),SPACES     TEST ANY INVALID                             
         BE    VK300               NO                                           
*                                                                               
         LA    R3,WORK                                                          
         CLI   0(R3),C' '                                                       
         BNE   INVPRGT2            R3 POINTS TO INVALID PROGTYPE                
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
VK300    CLI   SIRFRPTH+5,0        TEST FILTERS WERE GIVEN                      
         BNE   *+14                YES                                          
         XC    PRGTYPES,PRGTYPES                                                
         B     VKX                                                              
*                                                                               
         LA    R1,PRGTYPES         SQUASH ALL PROGTYPES TO THE LEFT             
         CLI   0(R1),C' '          BUMP TO FIRST BLANK POSITION                 
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         LA    R0,PRGTYPES+26                                                   
         LA    R3,1(R1)                                                         
*                                                                               
VK310    CLI   0(R3),C' '          TEST THERE'S A PROGTYPE HERE                 
         BE    *+18                NO                                           
         MVC   0(1,R1),0(R3)       MOVE IT TO THE FAR LEFT                      
         MVI   0(R3),C' '                                                       
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         CR    R3,R0               TEST END OF LIST                             
         BNE   VK310                                                            
*                                                                               
         CLC   PRGTYPES+8(18),SPACES   TEST MORE THAN 8 PROGTYPES               
         BE    VKX                 NO                                           
         LA    R2,SIRFRPTH                                                      
         CLI   SIRFRPT,C'-'        TEST NEGATIVE FILTERS                        
         BE    TOOFEWN             YES - WE NEED MORE                           
         B     TOOMANY             NO - THERE ARE TOO MANY                      
*                                                                               
VKX      MVC   AIO,AIO1            RESTORE AIO                                  
         TM    WHEN,X'80'          TEST ONLINE COPY                             
         BZ    *+8                 NO                                           
         OI    WHENOK,X'01'        CALL WITH VALREC                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* OFFLINE COPY INITIALIZATION                                                   
*                                                                               
OFFCOPY  LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   HDHOOKOK,C'Y'                                                    
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         B     ONCOPY                                                           
         SPACE 3                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=4100'                                  
         EJECT                                                                  
HEDSPECS SSPEC H1,1,C'MEDIA     SPOT T.V.'                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'PCOPY REPORT'                                            
         SSPEC H2,52,C'------------'                                            
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,1,C'PERIOD    '                                               
         SSPEC H5,1,C'DAYPARTS  '                                               
         SSPEC H6,1,C'PRGTYPES  '                                               
         SSPEC H4,52,C'SCHEME '                                                 
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H5,112,PAGE                                                      
         SSPEC H9,1,C'MARKET                          STATION    DPT'           
         SSPEC H10,1,C'------                          -------    ---'          
         SSPEC H9,58,C'NO. COPIED'                                              
         SSPEC H10,58,C'----------'                                             
         DC    X'00'                                                            
         SPACE 3                                                                
HOOK     NTR1  ,                   HEADLINE ROUTINE                             
*                                                                               
         CLI   HDHOOKOK,C'N'       TEST OK TO DO HEADHOOK                       
         BE    HOOKX                                                            
*                                                                               
         MVI   H3,0                NEED THIS TO SKIP LINES                      
         MVI   H7,0                                                             
*                                                                               
         MVC   H4+10(4),SIRTOPR    PERIOD                                       
         GOTO1 DATCON,DMCB,(3,SVPERST),(8,H4+16)                                
         MVI   H4+24,C'-'                                                       
         GOTO1 DATCON,DMCB,(3,SVPEREND),(8,H4+25)                               
         MVC   H4+58(3),SIRTOSC    SCHEME                                       
         MVI   H4+61,C'/'                                                       
         MVC   H4+62(2),SIRTOYR    YEAR                                         
         MVC   H5+10(L'SIRFRDP),SIRFRDP                                         
         MVC   H6+10(L'SIRFRPT),SIRFRPT                                         
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
* COPY NSID (AND DETAIL) RECORDS                                                
*                                                                               
ONCOPY   MVI   COPYFLAG,C'N'       NO RECORDS COPIED YET                        
         XC    OLDMKT,OLDMKT                                                    
         XC    OLDSTA,OLDSTA                                                    
         LA    RE,SRBLK            A(RANSID BLOCK)                              
         LA    RF,SRBLKLN                                                       
         XCEF  ,                   CLEAR RANSID BLOCK                           
*                                                                               
         MVC   SRASIR,AIO1         RETURN NSID RECORDS IN IO1                   
         MVC   SRACOM,ACOMFACS                                                  
         MVC   SRACLPAC,CLPACK                                                  
         MVC   SRAMSUNP,MSUNPK                                                  
         MVC   SRADYUNP,UNDAY                                                   
         MVC   SRAUNTIM,UNTIME                                                  
*                                                                               
         MVC   SRSELSCH,SIRFRSC    SCHEME                                       
         MVC   SRSELAM,BAGYMD      AGENCY/MEDIA                                 
         MVC   SRSELAGY,AGENCY     ALPHA AGENCY CODE                            
         MVI   SRSELMED,C'T'       MEDIA (ALWAYS TELEVISION FOR NOW)            
         MVC   SRSELPER,SIRFRPR    PERIOD                                       
         MVC   SRSELMKT,BMKT       MARKET                                       
         MVC   SRSELSTA,BSTA       STATION                                      
         MVC   SRSELDPT,DAYPARTS   DAYPART FILTERS                              
         CLC   DAYPARTS+8(8),SPACES                                             
         BE    *+10                                                             
         MVC   SRSELDP2,DAYPARTS+8 MORE DAYPART FILTERS IF NECESSARY            
         MVC   SRSELPRG,PRGTYPES   PROGRAM TYPE FILTERS                         
         MVC   SRSELYR,FROMYEAR    YEAR                                         
         OI    SRSELFLG,SRNSID     RETURN NSID RECORDS ONLY                     
         MVC   SRSELCTY,SVAPROF+7  COUNTRY CODE                                 
*                                                                               
COPY10   GOTO1 RANSID,DMCB,SRBLK   GOTO RANSID                                  
         CLI   SRERROR,0           TEST ERROR CONDITION                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SRMODE,SRNORECS     TEST ANY RECORDS FOUND                       
         BE    NODATA              NO                                           
         CLI   SRMODE,SRONEREC     TEST ANOTHER RECORD WAS RETURNED             
         BNE   COPY40              NO - WE'RE FINISHED                          
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY             BUILD NEW KEY                                
         MVC   KEY(13),SRACTKEY    RETURNED KEY                                 
         MVI   SIRKSEQ,0           BECAUSE X'FF' WAS PLACED HERE                
         MVC   FROMKEY,KEY         SAVE OLD NSID KEY                            
         MVI   NUMDETS,0           CLEAR DETAIL RECORD COUNTER                  
*                                                                               
         MVC   SIRKCODE,TOSCH      'TO' SCHEME                                  
         MVC   SIRKYEAR,TOYEAR     'TO' YEAR                                    
         XI    SIRKYEAR,X'FF'                                                   
         MVC   SIRKMON,TOPER       'TO' PERIOD                                  
         OI    SIRKMON,SIRKBUYQ                                                 
         MVC   TOKEY,KEY                                                        
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST NSID RECORD IS ALREADY THERE            
         BNE   COPY12              NO, SO IT'S OK TO COPY                       
         BAS   RE,PRTLINE          YES, SO WE CAN'T COPY IT                     
         B     COPY10              GET NEXT NSID RECORD                         
*                                                                               
COPY12   MVC   AIO,AIO1                                                         
         L     R5,AIO              A(OLD RECORD)                                
         MVC   0(13,R5),TOKEY      PUT NEW KEY IN RECORD                        
*                                                                               
         MVI   COPYFLAG,C'Y'       WE'VE DONE A COPY                            
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BNE   *+16                                                             
         MVI   SORTFLAG,C'O'       YES -- AND RECORD IS OK                      
         BAS   RE,PUTSORT          PUT RECORD TO SORTER                         
         B     COPY15                                                           
*                                                                               
         GOTO1 ADDREC              ADD NEW NSID RECORD                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COPY15   MVI   ELCODE,EDPCODEQ     COUNT DAY/TIME ELEMENTS                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,1                R3 = NO. OF DETAIL RECORDS                   
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
         STC   R3,NUMDETS                                                       
         BAS   RE,PRTLINE          PRINT DETAIL LINE ON REPORT                  
         SR    R3,R3                                                            
*                                                                               
         XC    KEY,KEY             RESTORE OLD KEY                              
         MVC   KEY(13),FROMKEY                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR NSID KEY                            
         CLC   KEY(13),KEYSAVE     TEST KEY WAS FOUND                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COPY20   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 LOOK FOR DETAIL KEYS                         
*                                                                               
         CLC   KEY(10),FROMKEY     TEST SAME TYPE/A-M/SCH/MKTSTA/DPT            
         BE    COPY25              YES                                          
*                                                                               
         ZIC   R0,NUMDETS          NO - MAKE SURE NUMBER OF ELEMENTS...         
         CR    R0,R3                ...IN NSID MATCHES NO. OF DETAILS           
         BE    COPY10              IT DOES                                      
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BE    *+6                                                              
         DC    H'0'                DIE ONLINE                                   
*                                                                               
         MVC   P1(45),=C'** RECORD NOT COPIED -- PLEASE CONTACT DDS **'         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SORTFLAG,C'E'       RECORD IS BAD                                
         BAS   RE,PUTSORT          PUT WARNING RECORD TO SORTER                 
         B     COPY10                                                           
*                                                                               
COPY25   CLI   SIRKSEQ,0           TEST DETAIL RECORD                           
         BE    COPY20              NO                                           
         CLC   KEY+11(2),FROMKEY+11 TEST SAME YR/PERIOD                         
         BNE   COPY20              NO                                           
*                                                                               
         MVC   DETKEY,KEY          SAVE DETAIL KEY                              
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO2            USE IO2 FOR DETAIL RECORDS                   
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVC   0(10,R5),TOKEY      PUT NEW KEY IN DETAIL RECORD. . .            
         MVC   11(2,R5),TOKEY+11   . . . BUT LEAVE SEQNUM ALONE                 
         MVI   ELCODE,EDPCODEQ     LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0               BUMP PAST DAY/TIME ELEMENT                   
COPY30   IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0             TEST END OF RECORD                           
         BE    *+12                YES                                          
         MVI   0(R5),X'FF'         MARK ALL OTHER ELEMENTS FOR DELETION         
         B     COPY30                                                           
*                                                                               
         MVI   ELCODE,X'FF'        SAVE ONLY THE DAY/TIME ELEMENT               
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BNE   *+16                                                             
         MVI   SORTFLAG,C'O'       YES -- AND RECORD IS OK                      
         BAS   RE,PUTSORT          PUT RECORD TO SORTER                         
         B     COPY35                                                           
*                                                                               
         GOTO1 ADDREC              ADD NEW DETAIL RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                 LOOK FOR NEXT DETAIL RECORD                  
         DC    H'0'                                                             
*                                                                               
COPY35   LA    R3,1(R3)            INCREMENT DETAIL COUNTER                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),DETKEY      RESTORE READ SEQUENCE                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    COPY20                                                           
         DC    H'0'                                                             
*                                                                               
COPY40   CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   *+12                NO                                           
         BAS   RE,GETSORT                                                       
         B     COPYX                                                            
*                                                                               
         CLI   COPYFLAG,C'N'       TEST ANY RECORDS COPIED                      
         BE    NODATA              NO                                           
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(19),=C'NSID records copied'                              
*                                                                               
COPYX    B     XIT                                                              
         EJECT                                                                  
PUTSORT  NTR1                                                                   
*                                                                               
         L     R4,AIO              A(RECORD TO ADD)                             
         SR    RF,RF                                                            
         ICM   RF,3,SIRRLEN        RECORD LENGTH                                
         LA    R0,SORTHEDQ(RF)     LENGTH OF VARIABLE LENGTH SORT REC           
         STH   R0,SORTRECL                                                      
         MVC   SORTMKST,SIRKMS     MARKET/STATION                               
         MVC   SORTDPT,SIRKDPT     DAYPART                                      
         LA    R0,SORTREC          A(SORT RECORD)                               
         LR    R1,RF               LENGTH OF SORT RECORD                        
         LR    RE,R4                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTRECL                                     
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
GETSORT  NTR1                                                                   
*                                                                               
         CLI   COPYFLAG,C'N'       TEST ANY RECORDS COPIED                      
         BNE   GS10                YES                                          
         MVI   HDHOOKOK,C'N'                                                    
         MVI   P1,0                                                             
         MVC   P2(26),=C'**************************'                            
         MVC   P3(26),=C'* NO NSID RECORDS COPIED *'                            
         MVC   P4(26),=C'**************************'                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     GSX                                                              
*                                                                               
GS10     LA    RE,SORTREC                                                       
         ST    RE,AIO                                                           
*                                                                               
GS20     GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RE,15,4(R1)         A(RECORD)                                    
         BZ    GS60                NO MORE RECORDS                              
*                                                                               
GS30     LH    RF,0(RE)            SORT RECORD LENGTH                           
         LA    R0,SORTRECL                                                      
         LR    R1,RF               LENGTH OF SORT RECORD                        
         MVCL  R0,RE               MOVE RECORD TO MY BUFFER                     
*                                                                               
         CLI   SORTFLAG,C'O'       IS RECORD OK?                                
         BE    GS50                YES -- ADD IT                                
         MVC   WORK(SORTKEYQ),SORTMKST                                          
         MVC   P1(15),=C'UNCOPIED KEY = '                                       
         GOTO1 HEXOUT,DMCB,SORTREC,P+15,13,=C'TOG'                              
         CLC   DMCB+16(4),=F'26'                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
GS40     GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RE,15,4(R1)         A(RECORD)                                    
         BZ    GS60                NO MORE RECORDS                              
         CLC   WORK(SORTKEYQ),4(RE)  MATCH ON BAD KEY?                          
         BE    GS40                YES -- SKIP THIS RECORD                      
         B     GS30                NO -- PROCESS IT                             
*                                                                               
GS50     GOTO1 ADDREC                                                           
         CLI   DMCB+8,0                                                         
         BE    GS20                GET THE NEXT RECORD                          
         DC    H'0'                                                             
*                                                                               
GS60     GOTO1 SORTER,DMCB,=C'END' NO MORE RECORDS TO ADD                       
*                                                                               
GSX      B     XIT                                                              
         EJECT                                                                  
* PRINT DETAIL LINE                                                             
*                                                                               
PRTLINE  NTR1                                                                   
*                                                                               
         CLI   OFFLINE,C'Y'        ONLY PRODUCE REPORT OFFLINE                  
         BNE   PRTLINEX                                                         
*                                                                               
         LA    R2,P1                                                            
         USING PLINED,R2                                                        
         CLC   OLDSTA,SRERSTA      TEST CHANGE IN STATION                       
         BE    PRTLIN10            NO                                           
*                                                                               
         MVI   P1,0                SKIP A LINE                                  
         LA    R2,P2                                                            
         MVC   OLDSTA,SRERSTA      SAVE STATION CALL LETTERS                    
         MVC   PSTA,SRERSTA        STATION CALL LETTERS                         
*                                                                               
PRTLIN10 CLC   OLDMKT,SRERMNO      TEST CHANGE IN MARKET                        
         BE    PRTLIN40            NO                                           
*                                                                               
         MVC   OLDMKT,SRERMNO      SAVE THE MARKET NUMBER                       
         MVC   PMKT,SRERMNO        PRINT MARKET NUMBER                          
*                                                                               
         CLI   USEREP,C'Y'         TEST READ REP MARKETS                        
         BNE   PRTLIN30            NO                                           
*                                                                               
         L     R3,TWAMASTC         GET A(MASTC)                                 
         L     R3,MCUTL-MASTD(R3)  GET A(UTL)                                   
         MVC   SVUTLSYS,4(R3)      SAVE SPOT SENUM                              
         MVI   4(R3),X'08'         PUT REP SENUM IN UTL                         
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',REPFLIST,AIO3                    
*                                                                               
         LA    R3,KEY              READ MARKET KEY                              
         USING RMKTRECD,R3                                                      
         XC    KEY,KEY                                                          
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         MVC   RMKTKMKT,SRERMNO                                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'REPDIR',KEY,KEY,0                 
         CLI   8(R1),0             TEST VALID MARKET                            
         BE    *+14                YES                                          
         MVC   PMKTNM,=CL24'*** UNKNOWN ***'                                    
         B     PRTLIN20                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'REPFIL',KEY+28,          +        
               AIO3,DMWORK                                                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3             DISPLAY MARKET NAME                          
         MVC   PMKTNM(L'RMKTNAME),RMKTNAME                                      
         DROP  R3                                                               
*                                                                               
PRTLIN20 GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'REP'                                  
         L     R3,TWAMASTC         GET A(MASTC)                                 
         L     R3,MCUTL-MASTD(R3)  GET A(UTL)                                   
         MVC   4(1,R3),SVUTLSYS    SWITCH BACK TO SPOT SYSTEM                   
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',SPTFLIST,AIO3                   
         B     PRTLIN40                                                         
*                                                                               
PRTLIN30 LA    R3,KEY              BUILD MARKET RECORD KEY                      
         USING MKTRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVC   MKTKEY,=C'00000000000000000'                                     
         MVI   MKTKTYPE,C'M'                                                    
         MVI   MKTKMED,C'T'        MEDIA 'T'                                    
         MVC   MKTKMKT,SRERMNO     MARKET NUMBER                                
         MVC   MKTKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO3                 
         LA    R1,=CL24'*** UNKNOWN ***'                                        
         L     R3,AIO3                                                          
         CLC   MKTKEY,0(R3)                                                     
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
         MVC   PMKTNM,0(R1)        PRINT MARKET NAME                            
         DROP  R3                                                               
*                                                                               
PRTLIN40 MVC   PDPT,SRACTDPT       DAYPART CODE                                 
         MVC   PDPTNM,SRDPTNM      DAYPART NAME                                 
         EDIT  (B1,NUMDETS),(3,PNUMDETS),ZERO=NOBLANK                           
*                                                                               
         CLI   NUMDETS,0           TEST ANY DETAILS COPIED                      
         BNE   *+10                YES                                          
         MVC   PWARN,WARNING       NO - PRINT WARNING LINE                      
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R2                                                               
*                                                                               
PRTLINEX B     XIT                                                              
         SPACE 3                                                                
SPTFLIST DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE                                                                  
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
         GETEL R5,DATADISP,ELCODE                                               
*                                                                               
RELO     DS    F                                                                
*                                                                               
TRTABLE  DS    0F             TRANSLATE ALPHABET TO (ARRAY DISP + 1)            
         DC    193X'00'                                                         
         DC    AL1(1,2,3,4,5,6,7,8,9)           A,B,C,D,E,F,G,H,I               
         DC    7X'00'                                                           
         DC    AL1(10,11,12,13,14,15,16,17,18)  J,K,L,M,N,O,P,Q,R               
         DC    8X'00'                                                           
         DC    AL1(19,20,21,22,23,24,25,26)     S,T,U,V,W,X,Y,Z                 
         DC    22X'00'                                                          
*                                                                               
WARNING  DC    C'*** RECORD ALREADY EXISTS ***'                                 
         SPACE 3                                                                
NOTNOW   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOTNOWM),NOTNOWM                                       
         GOTO1 ERREX2                                                           
NOTNOWM  DC    C'* ERROR * Online copy requires station filter *'               
*                                                                               
NOTFRI   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOTFRIM),NOTFRIM                                       
         GOTO1 ERREX2                                                           
NOTFRIM  DC    C'* ERROR * ''All'' request allowed only on Friday *'            
*                                                                               
INVTARG  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVTARGM),INVTARGM                                     
         LA    R2,SIRFRYRH                                                      
         GOTO1 ERREX2                                                           
INVTARGM DC    C'* ERROR * Invalid ''From''/''To'' combination *'               
*                                                                               
NOTREP   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOTREPM),NOTREPM                                       
         GOTO1 ERREX2                                                           
NOTREPM  DC    C'* ERROR * Both schemes must use REP markets *'                 
*                                                                               
INVDAYPT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVDAYPM),INVDAYPM                                     
         MVC   CONHEAD+19(1),0(R3)                                              
         LA    R2,SIRFRDPH                                                      
         GOTO1 ERREX2                                                           
INVDAYPM DC    C'* ERROR * Daypart '' '' is invalid *'                          
*                                                                               
INVDAYP2 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVDPT2M),INVDPT2M                                     
         MVC   CONHEAD+19(1),0(R3)                                              
         LA    R2,SIRFRDPH                                                      
         GOTO1 ERREX2                                                           
INVDPT2M DC    C'* ERROR * Daypart '' '' is not in ''To'' scheme *'             
*                                                                               
NODPTLEF XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODPTLEM),NODPTLEM                                     
         LA    R2,SIRFRDPH                                                      
         GOTO1 ERREX2                                                           
NODPTLEM DC    C'* ERROR * No dayparts remain to copy *'                        
*                                                                               
TOOMANY  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOOMANYM),TOOMANYM                                     
         GOTO1 ERREX2                                                           
TOOMANYM DC    C'* ERROR * Too many filters specified *'                        
*                                                                               
TOOFEWN  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOOFEWNM),TOOFEWNM                                     
         GOTO1 ERREX2                                                           
TOOFEWNM DC    C'* ERROR * Too few negative filters *'                          
*                                                                               
INVPROGT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVPROGM),INVPROGM                                     
         MVC   CONHEAD+24(1),0(R3)                                              
         LA    R2,SIRFRPTH                                                      
         GOTO1 ERREX2                                                           
INVPROGM DC    C'* ERROR * Program type '' '' is invalid *'                     
*                                                                               
NOPROGST XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOPROGSM),NOPROGSM                                     
         LA    R2,SIRFRPTH                                                      
         GOTO1 ERREX2                                                           
NOPROGSM DC    C'* ERROR * No program types defined in ''To'' scheme *'         
*                                                                               
NOPROGSF XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOPROGFM),NOPROGFM                                     
         LA    R2,SIRFRPTH                                                      
         GOTO1 ERREX2                                                           
NOPROGFM DC  C'* ERROR * No program types defined in ''From'' scheme *'         
*                                                                               
INVPRGT2 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVPRG2M),INVPRG2M                                     
         MVC   CONHEAD+24(1),0(R3)                                              
         LA    R2,SIRFRPTH                                                      
         GOTO1 ERREX2                                                           
INVPRG2M DC   C'* ERROR * Program type '' '' is not in ''To'' scheme *'         
*                                                                               
NOPGTLEF XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOPGTLEM),NOPGTLEM                                     
         LA    R2,SIRFRPTH                                                      
         GOTO1 ERREX2                                                           
NOPGTLEM DC    C'* ERROR * No program types remain to copy *'                   
*                                                                               
BADFILT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADFILTM),BADFILTM                                     
         GOTO1 ERREX2                                                           
BADFILTM DC    C'* ERROR * All filters must be negative *'                      
*                                                                               
NODATA   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODATAM),NODATAM                                       
         GOTO1 ERREX2                                                           
NODATAM  DC    C'* ERROR * No NSID records to copy *'                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
* THIS NON-REENTRANT AREA IS ONLY USED OFFLINE, SO IT'S OK                      
*                                                                               
SORTRECL DC    F'0'                VARIABLE LENGTH OF RECORD                    
SORTMKST DS    0XL5                                                             
SORTMKT  DS    XL2                 MARKET                                       
SORTSTA  DS    XL3                 STATION                                      
SORTDPT  DS    X                   DAYPART                                      
SORTKEYQ EQU   *-SORTMKST          L'SORT RECORD KEY                            
SORTFLAG DS    X                   C'O' IF OK, C'E' IF ERROR                    
SORTHEDQ EQU   *-SORTRECL          L'SORT RECORD HEADER                         
SORTREC  DS    4000X               THE NEW RECORD TO BE ADDED                   
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMACD                                                       
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         SPACE 5                                                                
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
FROMSCH  DS    XL2                 'FROM' SCHEME                                
TOSCH    DS    XL2                 'TO' SCHEME                                  
FROMPER  DS    X                   'FROM' PERIOD                                
TOPER    DS    X                   'TO' PERIOD                                  
FROMYEAR DS    X                   'FROM' YEAR                                  
TOYEAR   DS    X                   'TO' YEAR                                    
USEREP   DS    C                   'Y' IF WE SHOULD READ REP MARKETS            
DAYPARTS DS    CL26                DAYPART CODES                                
PRGTYPES DS    CL26                PROGRAM TYPE CODES                           
LASTFPER DS    C                   'Y' IF LAST 'FROM' PERIOD                    
COPYFLAG DS    C                   'Y' IF WE'VE COPIED ANY RECORDS              
HDHOOKOK DS    C                   'Y' IF OK TO DO HEADHOOK                     
FROMKEY  DS    XL13                'FROM' NSID KEY                              
TOKEY    DS    XL13                'TO' NSID KEY                                
DETKEY   DS    XL13                DETAIL KEY                                   
SVPERST  DS    XL3                 'TO' PERIOD START DATE                       
SVPEREND DS    XL3                 'TO' PERIOD END DATE                         
OLDMKT   DS    CL4                 PREVIOUS MARKET NUMBER                       
OLDSTA   DS    CL8                 PREVIOUS STATION CALL LETTERS                
NUMDETS  DS    X                   DETAIL RECORD COUNTER                        
SVUTLSYS DS    X                   SAVED UTL SENUM                              
PERFLAG  DS    C                   'Y' IF OK TO DO ANY PERIOD COPY              
         EJECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDREPMASTD                                                     
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
RMKTRECD DSECT                                                                  
       ++INCLUDE REGENMKT                                                       
         PRINT ON                                                               
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         SPACE 5                                                                
PLINED   DSECT                                                                  
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PMKTNM   DS    CL24                                                             
         DS    CL2                                                              
PSTA     DS    CL8                                                              
         DS    CL3                                                              
PDPT     DS    CL1                                                              
         DS    CL2                                                              
PDPTNM   DS    CL7                                                              
         DS    CL7                                                              
PNUMDETS DS    CL3                                                              
         DS    CL8                                                              
PWARN    DS    CL(L'WARNING)                                                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043SPSFM12   08/11/11'                                      
         END                                                                    
