*          DATA SET CTSFM10    AT LEVEL 062 AS OF 05/01/02                      
*PHASE TA0A10A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A10 - ADDS DIRECTORY RECORD MAINTENANCE/LIST             *         
*                                                                     *         
*  COMMENTS: MAINTAINS ADDS DIRECTORY RECORDS.                        *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMDC (TA0ADC) -- MAINTENANCE                    *         
*                  CTSFMEC (TA0AEC) -- LIST                           *         
*                                                                     *         
*  OUTPUTS: UPDATED ADDS DIRECTORY RECORDS                            *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A10 ADDS DIRECTORY RECORD MAINTENANCE/LIST'                  
TA0A10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A10**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETREP           SETUP REP TABLE                              
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY ROUTINE                                                          
***********************************************************************         
VK       DS    0H                                                               
*                                                                               
         MVI   ADDFLAG,C'F'                                                     
         LA    R2,ADDMEDH                                                       
         CLI   ACTNUM,ACTADD       ACTION ADD DISPLAYS DEFAULTS                 
         BNE   VK03                  FIRST TIME THRU                            
         TM    4(R2),X'80'                                                      
         BZ    VK08                                                             
*                                                                               
VK03     MVC   GERROR,=AL2(MISSING)                                             
         CLI   5(R2),0                                                          
         BE    VSFMERR                                                          
         MVC   GERROR,=AL2(INVMED)                                              
         CLI   8(R2),C'T'         MEDIA MUST BE T OR R                          
         BE    VK05                                                             
         CLI   8(R2),C'R'                                                       
         BNE   VSFMERR                                                          
*                                                                               
VK05     DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK08A                                                            
         CLI   ACTNUM,ACTCHA                                                    
         BE    VK08                                                             
         BAS   RE,DISREP           CLEAR AND FORMAT SCREEN                      
*                                                                               
VK08     DS    0H                                                               
         LA    R2,ADDIDH                                                        
         CLI   5(R2),0             MISSING AGENCY ID                            
         BNE   VK09                                                             
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
*                                                                               
VK08A    XC    SAVEKEY2,SAVEKEY2                                                
         LA    R4,SAVEKEY2                                                      
         USING DIRKEY,R4                                                        
         MVI   DIRKSYS,DIRKSYSQ    KEY SYSTEM X'05' FOR ALL SYSTEMS             
         MVI   DIRTYPE,DIRTYPEQ    RECORD TYPE X'04' FOR ADDS DIR REC.          
         MVC   DIRMED,ADDMED       MEDIA                                        
         MVC   SVAGYID,ADDID                                                    
         DROP  R4                                                               
         B     VKLSTX                                                           
*                                                                               
VK09     DS    0H                  CHECK IF AGENCY ID EXISTS                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
*                                                                               
         TM    4(R2),X'08'         IS INPUT VALID NUMERIC?                      
         BZ    VK10                NO, MUST BE ALPHANUMERIC                     
*                                                                               
         MVI   CTIKTYP,CTIKTYPQ    RECORD TYPE C'I'                             
*                                                                               
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),ADDID(0)                                                  
         CVB   R3,DUB                                                           
         C     R3,=F'65535'        MUST BE 2-BYTE X'FFFF' MAX                   
         BH    BADID                                                            
         STCM  R3,3,CTIKNUM                                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     LOOK FOR MATCHING AGY ID REC                 
         BNE   VKBADID             INVALID AGENCY ID                            
         B     VK20                                                             
*                                                                               
VK10     MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,ADDID                                                     
         OC    CTIKID,SPACES       BLANK PAD WITH SPACES                        
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     LOOK FOR MATCHING AGY ID REC                 
         BNE   VKBADID                                                          
*                                                                               
VK20     L     R6,AIO                                                           
         MVI   ELCODE,X'36'        GET ID NUM/ID NAME                           
         BAS   RE,GETEL                                                         
         BE    VK25                                                             
         DC    H'0'                MUST HAVE X'36' ELEMENT                      
*                                                                               
VK25     MVC   ADDNAME,2(R6)       FULL NAME                                    
         OI    ADDNAMEH+6,X'80'    XMIT                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        GET ID NUM/ID NAME                           
         BAS   RE,GETEL                                                         
         BE    VK30                                                             
         DC    H'0'                MUST HAVE X'02' ELEMENT                      
*                                                                               
VK30     DS    0H                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R5,SVKEY                                                         
         USING DIRKEY,R5                                                        
         MVI   DIRKSYS,DIRKSYSQ    KEY SYSTEM X'05' FOR ALL SYSTEMS             
         MVI   DIRTYPE,DIRTYPEQ    RECORD TYPE X'04' FOR ADDS DIR REC.          
         MVC   DIRMED,ADDMED       MEDIA                                        
*                                                                               
         TM    4(R2),X'08'         IS INPUT NUMERIC?                            
         BNZ   VK35                                                             
*                                                                               
         MVC   DIRID,2(R6)         NO                                           
*                                                                               
         ZIC   RF,ADDIDH+5         GET AGENCY ID INPUT LENGTH AND               
         LA    RF,13(RF,R4)        PARSE BACKWARD FOR CITY                      
         MVC   LOCATION,0(RF)                                                   
         B     VK38                                                             
*                                                                               
VK35     MVC   DIRID,CTIKNUM       YES                                          
*                                                                               
         MVC   ADDID,2(R6)                                                      
         OI    ADDIDH+6,X'80'      XMIT                                         
         LA    RF,10               GET ELEMENT LENGTH                           
         LA    R1,11(R6)           PARSE BACKWARD FOR CITY                      
VK36     CLI   0(R1),C' '          FIND FIRST NON-BLANK CHARACTER               
         BNE   VK37                                                             
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         B     VK36                                                             
*                                                                               
VK37     BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LA    RF,2(RF,R6)         PARSE BACKWARD FOR CITY                      
         MVC   LOCATION,0(RF)                                                   
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
VK38     DS    0H                                                               
         OC    ADDLOVE,ADDLOVE     LOCATION OVERRIDE??                          
         BZ    VK39                                                             
         MVC   LOCATION,ADDLOVE                                                 
*                                                                               
VK39     DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VKX                                                              
         EJECT                                                                  
***********************************************************************         
* IF ALL INPUT FIELDS EMPTY AND ACTION IS ADD, DISPLAY DEFAULT REPS             
***********************************************************************         
*                                                                               
         LA    R2,ADDREPH                                                       
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               POINT TO FIRST INPUT FIELD                   
VK40     DS    0H                                                               
*                                                                               
         LA    RF,ADDREPLH         END OF SCREEN?                               
         CR    R2,RF                                                            
         BNL   VK50                                                             
         CLI   5(R2),0             IS FIELD EMPTY??                             
         BNE   VKX                                                              
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
         B     VK40                                                             
*                                                                               
VK50     DS    0H                  DISPLAY DEFAULTS                             
*                                                                               
         LA    R2,ADDREPH                                                       
         L     R7,AREPTAB                                                       
         MVC   REPPREFX,12(R7)                                                  
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               POINT TO FIRST INPUT FIELD                   
*                                                                               
VK60     DS    0H                                                               
*                                                                               
         MVC   8(2,R2),LOCATION                                                 
         MVI   5(R2),2                                                          
         BAS   RE,ISEDICT          EDICT REC DEFINED FOR THIS REP??             
         BE    VK70                                                             
         XC    8(2,R2),8(R2)       NO EDICT REC ATTACHED                        
         MVI   5(R2),0                                                          
         B     VK80                                                             
*                                                                               
VK70     MVI   ADDFLAG,C'T'                                                     
*                                                                               
VK80     OI    6(R2),X'80'         XMIT                                         
*                                                                               
         LA    RF,ADDREPLH         END OF SCREEN?                               
         CR    R2,RF                                                            
         BNL   VKX                                                              
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
         OC    8(1,R2),8(R2)                                                    
         BZ    VKX                                                              
*                                                                               
         ZIC   RF,0(R7)            A(NEXT REP ENTRY)                            
         AR    R7,RF                                                            
         MVC   REPPREFX,12(R7)                                                  
*                                                                               
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
         B     VK60                                                             
*                                                                               
VKBADID  DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   BADID                                                            
         B     VKX                                                              
*                                                                               
VKX      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         MVC   SAVEKEY2,KEY        FOR LIST                                     
VKLSTX   XC    SVKEY,SVKEY                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD ROUTINE                                                       
***********************************************************************         
VR       DS    0H                                                               
         CLI   ADDFLAG,C'T'                                                     
         BNE   VR05                                                             
         LA    R2,ADDREPH                                                       
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               R2 = A(1ST REP INPUT FIELD)                  
         OI    6(R2),X'41'         POSITION CURSOR, MODIFIED FOR NEXT           
         MVC   GERROR,=AL2(PROMPT)                                              
         B     VSFMERR               NEXT INPUT                                 
*                                                                               
VR05     MVC   AIO,AIO1                                                         
         L     R6,AIO              REMOVE ANY X'05' ELEMENTS                    
         MVI   ELCODE,DIRLOVEQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         OC    ADDLOVE,ADDLOVE     ANY LOCATION OVERRIDE?                       
         BZ    VR08                                                             
         MVC   LOCATION,ADDLOVE                                                 
*                                                                               
         LA    R6,ELEM             SAVE LOCATION OVERRIDE TO ELEMENT            
         USING DIRLOVD,R6                                                       
         XC    ELEM,ELEM                                                        
         MVI   DIRLOVEL,DIRLOVEQ                                                
         MVI   DIRLOVLN,DIRLOVOV                                                
         MVC   DIRLOVER,ADDLOVE                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR08     DS    0H                                                               
         L     R6,AIO              REMOVE ANY X'10' ELEMENTS                    
         MVI   ELCODE,DIRREPEQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   REPNDX,0            REP INDEX INTO THE REP TABLE                 
         L     R7,AREPTAB          R7 = A(1ST ENTRY IN REP TABLE)               
         LA    R2,ADDREPH          R2 = A(1ST REP INPUT FIELD)                  
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
VR10     CLI   5(R2),0             IF NO INPUT FOR THIS REP                     
         BE    VR80                                                             
*                                                                               
         CLI   5(R2),2             LOCATION ONLY                                
         BE    VR30                                                             
         CLI   5(R2),4             2 BYTE REP AND LOCATION                      
         BE    VR20                                                             
         MVC   GERROR,=AL2(INVREPOF)                                            
         CLI   5(R2),5             3 BYTE REP AND LOCATION                      
         BNE   VSFMERR                                                          
*                                                                               
VR20     ST    R7,SAVER7           SAVE A(REP ENTRY WE'RE ON)                   
         MVC   SAVER7(L'REPNDX),REPNDX     SAVE INDEX IN HOB(SAVER7)            
         MVI   REPNDX,0            REP INDEX INTO THE REP TABLE                 
         L     R7,AREPTAB          R7 = A(1ST ENTRY IN REP TABLE)               
*                                                                               
VR20LP   MVC   GERROR,=AL2(INVREPOF)                                            
         CLI   0(R7),0             END OF TABLE??                               
         BE    VSFMERR             CAN'T FIND PREFIX                            
*                                                                               
VR25     CLI   5(R2),4             IF 4 BYTE INPUT, MUST BE 2 BYTE REP          
         BNE   *+14                                                             
         CLC   8(2,R2),12(R7)                                                   
         B     *+10                                                             
         CLC   8(3,R2),12(R7)      ELSE 3 BYTE REP                              
         BE    VR30                                                             
*                                                                               
         ZIC   R1,0(R7)            KEEP BUMPING UNTIL END OF REP TABLE          
         AR    R7,R1                                                            
         ZIC   R1,REPNDX                                                        
         LA    R1,1(R1)                                                         
         STC   R1,REPNDX                                                        
         B     VR20LP                                                           
*                                                                               
VR30     MVC   REPPREFX,12(R7)                                                  
*                                                                               
         CLI   1(R7),0             NO LOCATION CODES?                           
         BE    VR40                NONE, TAKE ANYTHING                          
*                                                                               
         LA    R4,15(R7)           R4 = A(LOCATION CODES)                       
         ZIC   R0,1(R7)            R0 = NUMBER OF LOCATION CODES                
*                                                                               
VR30LP   CLI   5(R2),2             CITY CODE ENTERED ONLY                       
         BH    VR35                                                             
         CLC   8(2,R2),0(R4)       IF NO MATCH                                  
         BE    VR40                                                             
         B     VR30NEXT            THEN CHECK NEXT LOCATION CODE                
*                                                                               
VR35     CLI   5(R2),4                                                          
         BH    *+14                                                             
         CLC   10(2,R2),0(R4)      2 BYTE REP PREFIX                            
         B     *+10                                                             
         CLC   11(2,R2),0(R4)      3 BYTE REP PREFIX                            
         BE    VR40                FOUND A MATCH                                
*                                                                               
VR30NEXT LA    R4,2(R4)            R4 = A(NEXT LOCATION CODE)                   
         BCT   R0,VR30LP           LOOP BACK UNTIL NO MORE LOCATIONS            
         B     BADID               BAD LOCATION CODE                            
*                                                                               
VR40     DS    0H                                                               
         MVC   GERROR,=AL2(NOEDICT)                                             
         BAS   RE,ISEDICT          CHECK REP FOR EDICT RECORD                   
         BNE   VSFMERR             REP EDICT REC NOT FOUND                      
*                                                                               
         MVC   AIO,AIO2            USE AIO2 FOR ID REC                          
         XC    KEY,KEY             SETUP KEY FOR ID RECORD                      
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
*                                                                               
         CLI   5(R2),2                                                          
         BH    VR45                                                             
         MVC   CTIKID(3),REPPREFX  INPUT IS CITY CODE                           
         CLI   CTIKID+2,C' '       2-CHAR AGENCY??                              
         BE    *+14                                                             
         MVC   CTIKID+3(2),8(R2)                                                
         B     VR45A                                                            
         MVC   CTIKID+2(2),8(R2)                                                
         B     VR45A                                                            
*                                                                               
VR45     MVC   CTIKID,8(R2)        INPUT IS AGENCY AND CITY CODE                
VR45A    OC    CTIKID,SPACES       BLANK PAD WITH SPACES                        
         DROP  R4                                                               
         XC    REPIDNUM,REPIDNUM                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'CTIKEY),KEYSAVE   IF NOT DDS USER                          
         BNE   VR50                THEN NO ID NUM                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        GET ID NUM/ID NAME                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NO X'02' ELEMENT                      
         MVC   REPIDNUM,2(R6)      COPY ID NUM OF REP                           
*                                                                               
VR50     MVC   AIO,AIO1                                                         
*                                  LOOK FOR ELEMENT WITH SAME INDEX             
         MVC   GERROR,=AL2(DUPREPER)                                            
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(X'10',AIO),(1,REPNDX),0                
         CLI   DMCB+12,0           IF ELEMENT WITH SAME INDEX FOUND             
         BE    VSFMERR             THEN ERROR                                   
*                                  FOUND A MATCH, ADD TO ELEMENT                
         LA    R6,ELEM                                                          
         USING DIRREPD,R6                                                       
         XC    ELEM,ELEM           PREPARE ELEMENT TO BE ADDED                  
         MVI   DIRREPEL,DIRREPEQ                                                
         MVI   DIRREPLN,DIRREPOV                                                
         MVC   DIRREPIN,REPNDX     MOVE IN SEQUENCE NUMBER                      
*                                                                               
         CLI   5(R2),2             IF LOCATION ENTERED ONLY                     
         BH    VR50A                                                            
         MVC   DIRREPID(3),REPPREFX                                             
         CLI   DIRREPID+2,C' '     2-CHAR AGENCY??                              
         BE    *+14                                                             
         MVC   DIRREPID+3(2),8(R2) NO                                           
         B     VR50B                                                            
         MVC   DIRREPID+2(2),8(R2) YES                                          
         B     VR50B                                                            
VR50A    MVC   DIRREPID,8(R2)      INPUT IS AGENCY AND CITY CODE                
*                                                                               
VR50B    OC    DIRREPID,SPACES     BLANK-PAD                                    
         MVC   DIRREPNO,REPIDNUM                                                
         DROP  R6                                                               
*                                                                               
VR60     L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    VR80                GO VALIDATE NEXT INPUT FIELD                 
         DC    H'0'                                                             
*                                                                               
VR80     CLI   0(R7),0             IF END OF REP TABLE                          
         BE    VRX                                                              
*                                                                               
         CLI   5(R2),4             YES, CHECK IF WE CAN GET ID NUM              
         BL    VR90                                                             
*                                                                               
         L     R7,SAVER7           REPOSITION R7                                
         MVC   REPNDX,SAVER7                                                    
*                                                                               
VR90     ZIC   R0,0(R7)            R7 = A(NEXT REP ENTRY)                       
         AR    R7,R0                                                            
         CLI   0(R7),0             EXIT IF END OF TABLE                         
         BE    VRX                                                              
*                                                                               
         ZIC   RF,0(R2)            R2 = A(NEXT PROTECTED FIELD)                 
         AR    R2,RF                                                            
*                                                                               
         LA    RF,ADDREPLH         AFTER LAST FIELD?                            
         CR    R2,RF                                                            
         BH    VRX                 YES, EXIT                                    
*                                                                               
         CLI   8(R2),0             IF BLANK, THIS REP MAY BE DELETED            
         BNE   VR110               SO SKIP                                      
         ZIC   RF,0(R2)            R2 = A(NEXT PROTECTED FIELD)                 
         AR    R2,RF                                                            
         ZIC   RF,REPNDX           INCREMENT SEQUENCE NUMBER                    
         LA    RF,1(RF)                                                         
         STC   RF,REPNDX                                                        
         B     VR90                                                             
*                                                                               
VR110    DS    0H                                                               
         ZIC   RF,0(R2)            R2 = A(NEXT INPUT FIELD)                     
         AR    R2,RF                                                            
*                                                                               
         ZIC   RF,REPNDX           INCREMENT SEQUENCE NUMBER                    
         LA    RF,1(RF)                                                         
         STC   RF,REPNDX                                                        
         B     VR10                LOOP                                         
*                                                                               
VRX      B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE REP AGAINST EDICT RECORD                                             
*                                                                               
* ENTRY:                                                                        
* R2: SCREEN FIELD CONTAINING LOCATION                                          
* REPPREFX: REP PREFIX                                                          
*                                                                               
* EXIT:                                                                         
* TRUE OR FALSE                                                                 
***********************************************************************         
ISEDICT  NTR1                                                                   
         L     R7,AIO              SAVE AIO                                     
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING EDIKEY,R6                                                        
         MVI   EDIKSYS,EDIKSYSQ    KEY SYS '05'                                 
         MVI   EDITYPE,EDITYPEQ    REC TYPE '07'                                
*                                                                               
         CLI   5(R2),2                                                          
         BH    ISEDIC10                                                         
         MVC   EDINAME(3),REPPREFX  INPUT IS CITY CODE                          
         CLI   EDINAME+2,C' '       2-CHAR AGENCY??                             
         BE    *+14                                                             
         MVC   EDINAME+3(2),8(R2)                                               
         B     ISEDIC20                                                         
         MVC   EDINAME+2(2),8(R2)                                               
         B     ISEDIC20                                                         
*                                                                               
ISEDIC10 MVC   EDINAME(6),8(R2)     INPUT IS AGENCY AND CITY CODE               
ISEDIC20 OC    EDINAME,SPACES       BLANK PAD WITH SPACES                       
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'EDIKEY),KEYSAVE                                            
         BE    ISEDIC30                                                         
*                                                                               
         ST    R7,AIO              RESTORE AIO                                  
         CR    RB,RD                                                            
         B     ISEDICX                                                          
*                                                                               
ISEDIC30 ST    R7,AIO              RESTORE AIO                                  
         CR    RB,RB                                                            
ISEDICX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF REPS THERE EXISTS BEFORE THE REMELEM                            
***********************************************************************         
BLDODLST NTR1                                                                   
         LA    R0,BLOCK            CLEAR ANY DATA IN BLOCK                      
         LA    R1,480                                                           
         LA    RE,BLOCK                                                         
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R3,BLOCK                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,DIRREPEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         MVI   0(R3),X'FF'         X'FF' IS END OF THIS LIST                    
         B     BODX                                                             
*                                                                               
BODX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RESTORE ELEMENTS THAT WERE DELETED                                            
***********************************************************************         
REDORLST NTR1                                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
*                                                                               
         BAS   RE,DISREP                                                        
*                                                                               
         XC    ADDLOVE,ADDLOVE     CLEAR LOCATION OVERRIDE                      
*                                                                               
         L     R6,AIO                                                           
         USING DIRLOVD,R6                                                       
         MVI   ELCODE,X'05'        GET LOCATION OVERRIDE                        
         BAS   RE,GETEL                                                         
         BNE   DR05                                                             
         MVC   ADDLOVE,DIRLOVER                                                 
         DROP  R6                                                               
*                                                                               
DR05     OI    ADDLOVEH+6,X'80'    XMIT                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         LA    R2,ADDREPH                                                       
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               POINT TO FIRST DISPLAY FIELD                 
         XC    REPNDX,REPNDX                                                    
*                                                                               
DR10     CLC   REPNDX,2(R6)        MATCHING SEQUENCE NUMBER??                   
         BNE   DR20                                                             
*                                                                               
         CLI   7(R6),C' '            2-CHAR AGENCY??                            
         BE    DR15                                                             
         MVC   8(2,R2),6(R6)                                                    
         B     DR18                                                             
DR15     MVC   8(2,R2),5(R6)                                                    
*                                                                               
DR18     OI    6(R2),X'80'         XMIT CITY                                    
         BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
DR20     ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
*                                                                               
         ZIC   RF,REPNDX           INCREMENT SEQUENCE NUMBER                    
         LA    RF,1(RF)                                                         
         STC   RF,REPNDX                                                        
         BCT   R3,DR10             LOOP                                         
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
         MVC   PREVKEY,KEY         FOR LIST                                     
         MVC   SAVEKEY,KEY                                                      
         MVC   SVAIO,AIO                                                        
         XC    SVKEY,SVKEY                                                      
         LA    R5,SVKEY                                                         
         USING CTIKEY,R5                                                        
         MVI   CTIKTYP,CTIKTYPQ    GET ID REC                                   
*                                                                               
         LA    R4,KEY                                                           
         USING DIRKEY,R4                                                        
*                                                                               
         CLI   ACTNUM,ACTSEL       LIST SELECT NEEDS TO ASSIGN TABLES           
         BNE   DK08                                                             
*                                                                               
         CLI   DIRMED,C'T'         MEDIA MUST BE T OR R                         
         BE    DK08                                                             
         LA    R2,ADDMEDH                                                       
         MVC   GERROR,=AL2(INVMED)                                              
         CLI   DIRMED,C'R'                                                      
         BNE   VSFMERR                                                          
*                                                                               
DK08     MVC   ADDMED,DIRMED       DISPLAY MEDIA                                
         OI    ADDMEDH+6,X'80'     XMIT                                         
         MVC   CTIKNUM,DIRID                                                    
         MVC   KEY,SVKEY                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   BADID                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    DK10                                                             
         DC    H'0'                                                             
*                                                                               
DK10     DS    0H                                                               
         MVC   ADDID,2(R6)                                                      
         LA    RF,10               GET ELEMENT LENGTH                           
         LA    R1,11(R6)           PARSE BACKWARD FOR CITY                      
DK15     CLI   0(R1),C' '          FIND FIRST NON-BLANK CHARACTER               
         BNE   DK20                                                             
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         B     DK15                                                             
*                                                                               
DK20     BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LA    RF,2(RF,R6)         PARSE BACKWARD FOR CITY                      
         MVC   LOCATION,0(RF)                                                   
         OI    ADDIDH+6,X'80'      XMIT AGY ID                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'36'        FULL NAME                                    
         BAS   RE,GETEL                                                         
         BE    DK30                                                             
         DC    H'0'                                                             
*                                                                               
DK30     MVC   ADDNAME,2(R6)                                                    
         OI    ADDNAMEH+6,X'80'    XMIT FULL NAME                               
*                                                                               
DKX      DS    0H                                                               
         MVC   AIO,SVAIO                                                        
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ONLINE LIST ROUTINE                                                           
***********************************************************************         
LR       DS    0H                                                               
         LA    R5,LISTAR                                                        
         USING PLINED,R5                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
*                                                                               
         LA    R3,HEADING          SET UP REPORT HEADINGS                       
         ST    R3,SPECS                                                         
         LA    R3,HDRTN                                                         
         ST    R3,HEADHOOK                                                      
*                                                                               
LR02     DS    0H                                                               
         LA    R4,KEY                                                           
         USING DIRKEY,R4                                                        
         OC    PREVKEY,PREVKEY     DISPLAY FROM SELECT?                         
         BZ    LR05                OVERRIDE GENCON KEY                          
         MVC   KEY,PREVKEY         USE SELECTED KEY                             
         XC    PREVKEY,PREVKEY                                                  
         B     LR10                                                             
*                                                                               
LR05     OC    KEY(DIRKLENQ),KEY   FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
         MVI   DIRKSYS,DIRKSYSQ    X'0504'                                      
         MVI   DIRTYPE,DIRTYPEQ    DIR REC                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         B     LR40                                                             
*                                                                               
LR20     GOTO1 SEQ                                                              
*                                                                               
LR40     CLI   DIRKSYS,DIRKSYSQ    X'0504'                                      
         BNE   LRX                                                              
         CLI   DIRTYPE,DIRTYPEQ    DIR RECORD KEY                               
         BNE   LRX                                                              
         CLI   SAVEKEY2+10,X'00'    FILTER ON MEDIA                             
         BE    LR45                                                             
         CLC   SAVEKEY2+10(1),KEY+10    FILTER ON MEDIA                         
         BNE   LR20                                                             
*                                                                               
LR45     MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         EDIT  (B2,DIRID),(5,PAGNO),ALIGN=LEFT    MOVE IN AGY NO                
*                                                                               
         MVC   SVAIO,AIO                                                        
         MVC   SAVEKEY,KEY                                                      
         XC    SVKEY,SVKEY                                                      
         LA    R7,SVKEY                                                         
         USING CTIKEY,R7                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
*                                                                               
         MVC   CTIKNUM,DIRID                                                    
         MVC   KEY,SVKEY                                                        
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    LR50                                                             
         DC    H'0'                                                             
*                                                                               
LR50     DS    0H                                                               
         OC    SVAGYID,SVAGYID          FILTER ON AGENCY ID                     
         BZ    LR60                                                             
         ZIC   R1,ADDIDH+5                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVAGYID(0),2(R6)         FILTER ON AGENCY ID                     
         BE    LR60                                                             
         MVC   AIO,SVAIO           RESTORE                                      
         MVC   KEY,SAVEKEY                                                      
         XC    SAVEKEY,SAVEKEY                                                  
         GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR60     MVC   PAGID,2(R6)         PUT AGENCY SIGNON NAME IN LIST LINE          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'36'        GET AGENCY FULL NAME                         
         BAS   RE,GETEL                                                         
         BE    LR65                                                             
         DC    H'0'                                                             
*                                                                               
LR65     MVC   PAGNAME,2(R6)       PUT AGENCY FULL NAME IN LIST LINE            
*                                                                               
         MVC   AIO,SVAIO                                                        
         MVC   KEY,SAVEKEY                                                      
         XC    SAVEKEY,SAVEKEY                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR70                GO PRINT REPORT                              
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LR20                                                             
***********************************************************************         
* PRINT REPORT ROUTINE                                                          
***********************************************************************         
LR70     DS    0H                                                               
*** MOVE IN REST OF REP OFFICES                                                 
         MVC   P+10(PEND-PAGID),PAGID    KEY DATA FROM LIST TO P                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4,R5,R7                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY TV/RADIO REPS                                                         
***********************************************************************         
DISREP   NTR1                                                                   
         LA    R2,ADDREPH          R2 = A(1ST PROTECTED DISPLAY FIELD)          
DISR05   XC    8(L'ADDREP,R2),8(R2)  CLEAR EVERYTHING                           
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         XMIT                                         
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
         XC    8(6,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         XMIT                                         
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
         LA    RF,ADDREPLH         END OF SCREEN?                               
         CR    R2,RF                                                            
         BH    DISR08                                                           
         B     DISR05                                                           
*                                                                               
DISR08   L     R5,AREPTAB          R5 = A(1ST ENTRY IN REP TABLE)               
         LA    R2,ADDREPH          R2 = A(1ST PROTECTED DISPLAY FIELD)          
*                                                                               
DISR10   CLI   0(R5),0             NO MORE REPS?                                
         BE    DISR20              YES                                          
         MVC   8(10,R2),2(R5)      YES, DISPLAY REP                             
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         CLC   2(10,R5),SPACES     IF EXPANDED REP NAME IS BLANK,               
         BNE   DISR20                THIS MEANS THE REP IS DELETED              
         ZIC   RF,0(R2)              AND WE SHOULD PROTECT THE OFF FLD          
         AR    R2,RF               WE STILL NEED THIS SLOT AS A                 
         OI    6(R2),X'20'           PLACE HOLDER                               
         B     DISR25                                                           
*                                                                               
DISR20   ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
*                                                                               
DISR25   DS    0H                                                               
         CLI   0(R5),0             END OF TABLE?                                
         BNE   DISR30                                                           
         OI    6(R2),X'20'         NO MORE REPS, MAKE INPUT FIELD               
*                                     PROTECTED                                 
DISR30   DS    0H                                                               
         LA    RF,ADDREPLH         END OF SCREEN?                               
         CR    R2,RF                                                            
         BH    DISRX                                                            
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
         CLI   0(R5),0                                                          
         BE    DISR20                                                           
*                                                                               
DISR40   ZIC   RF,0(R5)                                                         
         AR    R5,RF                                                            
         B     DISR10                                                           
*                                                                               
DISRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET UP VARIABLES POINTING TO APPROPRIATE REP TABLES                           
***********************************************************************         
SETREP   NTR1                                                                   
         LA    RF,TREPTAB          GET ADDRESS OF REP CODE TABLE                
         ST    RF,AREPTAB                                                       
*                                                                               
         LA    R2,ADDMEDH                                                       
         CLI   8(R2),C'T'          MEDIA MUST BE T/R                            
         BE    SETREPX                                                          
*                                  ASSUME MEDIA R, VK WILL VERIFY               
         LA    RF,RREPTAB                                                       
         ST    RF,AREPTAB                                                       
*                                                                               
SETREPX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* HEADER ROUTINE                                                                
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,46,C'ADDS DIRECTORY RECORDS'                                  
         SSPEC H2,46,C'----------------------'                                  
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'0'                                                             
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         LA    R4,H8+10                                                         
         USING PLINED,R4                                                        
         MVC   PAGNO(7),=C'AGY NO.'                                             
         MVC   PAGNO+132(7),=80C'-'                                             
         MVC   PAGID(8),=C'AGY CODE'                                            
         MVC   PAGID+132(8),=80C'-'                                             
         MVC   PAGNAME(8),=C'AGY NAME'                                          
         MVC   PAGNAME+132(8),=80C'-'                                           
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
BADID    MVC   GERROR,=AL2(INVAGYID)                                            
         OC    ADDLOVE,ADDLOVE     BAD LOCATION OVERRIDE??                      
         BZ    BADIDX                                                           
         MVC   GERROR,=AL2(INVLOCOV)                                            
BADIDX   B     VSFMERR                                                          
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
*                                                                               
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
       ++INCLUDE SPADREPS                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENADDIR                                                     
       ++INCLUDE CTGENEDICT                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMDCD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMECD                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* TA0A10 STORAGE DSECT    * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
         ORG   SYSSPARE                                                         
MYWORK   DS    XL96                                                             
SVKEY    DS    XL48                ADDS DIR RECORD KEY                          
SAVEKEY  DS    XL48                                                             
SAVEKEY2 DS    XL48                                                             
PREVKEY  DS    XL48                PREVIOUS KEY                                 
SVAGYID  DS    CL10                SAVES AGENCY ID INPUT FIELD                  
LOCATION DS    CL2                 REP OFFICE LOCATION                          
REPIDNUM DS    XL2                 REP OFFICE ID NUMBER                         
SVAIO    DS    A                   SAVE AIO                                     
AREPTAB  DS    A                   A(REP TABLE)                                 
REPPREFX DS    CL3                 REP PREFIX                                   
REPNDX   DS    XL1                 SEQUENCE NUMBER                              
ADDFLAG  DS    CL1                 ACTION ADD XMIT FLAG                         
SAVER7   DS    F                   SAVES CURRENT ADD OF REP TABLE               
*                                                                               
* ONLINE LIST LINE   * * * * * * * * * * * * * * * * * * * * * * * * *          
*                                                                               
PLINED   DSECT                                                                  
PAGNO    DS    CL5                                                              
         DS    CL3                                                              
PAGID    DS    CL10                                                             
         DS    CL2                                                              
PAGNAME  DS    CL33                                                             
PEND     EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062CTSFM10   05/01/02'                                      
         END                                                                    
