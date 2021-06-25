*          DATA SET RERMP05S   AT LEVEL 028 AS OF 05/01/02                      
*PHASE T81005B,*                                                                
*INCLUDE BINSRCH2                                                               
*        TITLE 'T81005 - TEXT RECORDS'                                          
         TITLE 'T81005 - TEXT RECORDS - HISTORY'                                
**********************************************************************          
*                                                                    *          
*        RERMP05 (T81005) --- TEXT RECORDS                           *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
*   DEC96  (BOBY)--- ADD BOOK TYPE HANDLING                          *          
*                                                                    *          
*   AUG96  (BOBY)--- ADD COPY FUNCTION                               *          
*                                                                    *          
* 08OCT90  (EFJ) --- TOMBSTONE ADDED, PHASE CARD CHANGED TO 'A'      *          
*                                                                    *          
**********************************************************************          
         TITLE 'T81005 - TEXT RECORDS - INIT'                                   
**********************************************************************          
*                                                                    *          
*        RERMP05 (T81005) --- TEXT RECORDS                           *          
*                                                                    *          
*        INITIALIZATION                                              *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
T81005   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81005**,RR=R3                                                 
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST    R3,RELO                                                          
*                                                                               
         L     R7,=A(SUBROUTS)     ESTABLISH SUBROUTINES                        
         A     R7,RELO             RELOCATE ADDRESS                             
         USING SUBROUTS,R7                                                      
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
*===>                                                                           
         MVI   IPSTAT,0            INIT INPUT STATUS                            
         OI    GENSTAT5,GENSELVR   ALWAYS GO TO VALREC                          
*                                                                               
         MVI   ACTELOPT,C'N'       DON'T ADD GENCON ACTIVITY ELEMENT            
*                                                                               
         DS    0H                  MOVE PROFILE TO LOCAL WORKNG STORAG          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,SFMPROFS-CONHEADH                                             
*                                                                               
         USING SVDSECT,RF                                                       
*                                                                               
         MVC   RMPPROF,SVPGPBIT                                                 
*                                                                               
         DROP  RF                                                               
*                                                                               
         TITLE 'T81005 - TEXT RECORDS - VALMODE'                                
**********************************************************************          
*                                                                    *          
*        RERMP05 (T81005) --- TEXT RECORDS                           *          
*                                                                    *          
*        DETERMINE CALLING MODE                                      *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
VALMODE  DS    0H                                                               
*                                                                               
         CLI   MODE,PROCPFK        PFKEY HIT                                    
         BE    PKEY                                                             
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,RECDEL         DELETE   RECORD                              
         BE    RDEL                                                             
         CLI   MODE,XRECADD        ADD TEXT TO INVENTORY (FORCE)                
         BE    XREC                                                             
         CLI   MODE,XRECPUT        ADD TEXT TO INVENTORY (FORCE)                
         BE    XREC                                                             
         CLI   MODE,XRECDEL        ISSUE LTRANS REQUEST                         
         BE    XRLTRANS                                                         
         CLI   MODE,XRECREST       ISSUE LTRANS REQUEST                         
         BE    XRLTRANS                                                         
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LIST                                                             
*                                  UNKNOWN MODE                                 
         B     XIT                                                              
*                                                                               
         TITLE 'RERMP05 - TEXT RECORDS - PKEY'                                  
***********************************************************************         
*                                                                     *         
*        PFKEY HIT                                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PKEY     DS    0H                                                               
*                                                                               
         OI    GENSTAT2,RETEQSEL      DEFAULT IS RETURN HERE NEXT TIME          
*                                                                               
         CLI   PFAID,12            IF PF12 GO TO NEXT SELECTION                 
         BE    *+8                                                              
         CLI   PFAID,24                                                         
         BNE   PKEY12N                                                          
*                                                                               
         NI    GENSTAT2,X'FF'-RETEQSEL   DON'T RETURN HERE                      
*                                                                               
PKEY12N  DS    0H                                                               
*                                                                               
PKEYX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T81005 - TEXT RECORDS - VKEY'                                   
**********************************************************************          
*                                                                    *          
*        VALIDATE KEY ROUTINE                                        *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
VKEY     DS    0H                                                               
* <<<<<<<<<<<                                                                   
         DS    0H                  CHECK IF PHASE IS CALLED CORRECTLY           
         ZICM  R1,CALLSP,(1)       CAME FROM INV/ROVER?                         
         BZ    VKEY1               NO                                           
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,CALLSTCK(R1)                                                  
         CLI   0(R1),SCINVTRA                                                   
         BNE   VKEY1                                                            
*  <<<<<<<<<<<<<                                                                
         CLI   TWASCR,X'D1'        SKIP IF NOT TEXT DETAIL SCREEN               
         BNE   VKEY1                                                            
*                                                                               
         MVC   TXTSRC(3),CCONRSVC  MOVE IN SOURCE                               
         OI    TXTSRCH+6,X'80'                                                  
         MVC   TXTBOOK,CCONKBK     MOVE IN BOOK                                 
         OI    TXTBOOKH+6,X'80'                                                 
*                                                                               
VKEY1    XC    TOKEY,TOKEY         INIT COPY TO KEY                             
*                                                                               
         CLI   ACTNUM,ACTADD       IF ACTION IS NOT COPY                        
         BNE   *+8                                                              
         CLI   ORIGACT,C'C'                                                     
         BE    *+10                                                             
         XC    FRKEY,FRKEY            INIT FROM KEY                             
*                                                                               
         XC    HDRKEY,HDRKEY                                                    
         XC    STATION(14),STATION                                              
         XC    KFSRC,KFSRC                                                      
         XC    KFBOOK,KFBOOK                                                    
         XC    KFLOC,KFLOC                                                      
         XC    KFINT,KFINT                                                      
*                                                                               
         NI    STATUS,X'FF'-X'80'  TURN OFF INV. NUMBER ONLY                    
*                                                                               
         LA    R6,HDRKEY           ESTABLISH INVENTORY HEADER KEY               
         USING RINVKEY,R6                                                       
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
*                                                                               
         MVI   NEWKEY,C'N'         ASSUME KEY UNCHANGED                         
*                                                                               
         CLI   ACTNUM,ACTADD          SKIP IF COPYING                           
         BNE   *+8                                                              
         CLI   ORIGACT,C'C'                                                     
         BE    VKNEWX                                                           
*                                                                               
         CLI   ACTNUM,ACTADD       FORCE NEWKEY IF ADDING                       
         BNE   *+12                                                             
         MVI   NEWKEY,C'Y'            INDICATE NEW KEY                          
         B     VKNEWX                                                           
*                                                                               
         TM    TXTSTAH+4,X'80'     IF STATION INPUT THIS TIME                   
         BO    *+8                                                              
         TM    TXTTYPH+4,X'80'     IF TYPE    INPUT THIS TIME                   
         BO    *+8                                                              
         TM    TXTNUMH+4,X'80'     IF NUMBER  INPUT THIS TIME                   
         BNO   VKNEWX                                                           
*                                                                               
         MVI   NEWKEY,C'Y'            INDICATE NEW KEY                          
*                                                                               
VKNEWX   DS    0H                                                               
*                                                                               
         TITLE 'T81005 - TEXT RECORDS - VKSTA'                                  
***********************************************************************         
*                                                                     *         
*              VALIDATE STATION (REQUIRED)                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKSTA    DS    0H                                                               
*                                                                               
         LA    R2,TXTMRKTH         CLEAR OUT MARKET NAME                        
         MVC   8(20,R2),SPACES                                                  
         OI    TXTMRKTH+6,X'80'     TRANSMIT FIELD                              
*                                                                               
         LA    R2,TXTSTAH          NOW VALIDATE STATION                         
         GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
*                                                                               
         MVC   STATION,WORK        SAVE STATION                                 
         MVC   MEDIA,WORK+4        SAVE MEDIA                                   
         MVC   RINVKSTA,WORK       ADD TO KEY                                   
*                                                                               
         CLI   WORK+4,C' '         VALISTA RETURNS BLANK FOR                    
         BNE   VK3                 TV AND SATELLITE STATIONS                    
*                                                                               
         MVI   RINVKSTA+4,C'T'     KEY NEEDS T FOR TV                           
         MVI   MEDIA,C'T'                                                       
*                                                                               
         CLI   WORK+40,C' '        IF SATELLITE, WORK+40=1 OR 2                 
         BE    VK3                                                              
*                                                                               
         MVC   RINVKSTA+4(1),WORK+40                                            
         MVC   MEDIA,WORK+40                                                    
*                                                                               
VK3      LA    R2,TXTMRKTH                                                      
         MVC   8(20,R2),WORK+10    MARKET NAME                                  
*                                                                               
VKSTAX   DS    0H                                                               
*                                                                               
         TITLE 'T81005 - TEXT RECORDS - VKTYP'                                  
***********************************************************************         
*                                                                     *         
*              VALIDATE TYPE (SOURCE) - OPTIONAL FOR LIST AND REPORT  *         
*    VALID ENTRIES ARE 'MKT'         FOR MARKET TEXT        C'M'      *         
*                OR    'STA'         FOR STATION TEXT       C'S'      *         
*                OR    IIII          FOR INVENTORY TEXT     X'FF'     *         
*                      IIII          WHERE IIII IS AN INVENTORY NUM   *         
*                OR    IIII,MMMDDYY  FOR INVENTORY TEXT AND DATE      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKTYP    DS    0H                                                               
*                                                                               
         LA    R2,TXTTYPH          POINT TO TEXT TYPE FIELD                     
         XC    DATE,DATE           INIT EFFECTIVE DATE FIELD                    
         XC    KDATE,KDATE         INIT EFFECTIVE DATE FIELD                    
*                                                                               
         CLI   ACTNUM,ACTLIST      TYPE NOT REQUIRED FOR LIST                   
         BE    VK5                                                              
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BE    VK5                                                              
*                                                                               
         GOTO1 ANY                 PRODUCES ERROR MSG IF NOT ENTERED            
*                                                                               
VK5      CLI   5(R2),0                                                          
         BNE   VK7                                                              
*                                                                               
         XC    SOURCE(17),SOURCE   CLEAR SAVE AREAS                             
*                                                                               
         B     VKLSTSRC            ADDITIONAL FIELDS FOR LIST/REPORT            
*                                                                               
VK7      CLI   5(R2),3                                                          
         BH    VK20                                                             
*                                                                               
         ZIC   R1,5(R2)            GET TYPE INPUT LENGTH                        
*                                                                               
         BCTR  R1,0                DECREMENT FOR EXECUTE                        
         EX    R1,*+8              MARKET FACT                                  
         B     *+10                                                             
         CLC   8(0,R2),=C'MKT'                                                  
*                                                                               
         BE    VK10                                                             
*                                                                               
         EX    R1,*+8              STATION FACT                                 
         B     *+10                                                             
         CLC   8(0,R2),=C'STA'                                                  
*                                                                               
         BNE   VK20                                                             
*                                                                               
VK10     MVC   SOURCE,8(R2)        SAVE SOURCE CODE                             
*                                                                               
         B     VK50                                                             
*                                                                               
VK20     DS    0H                                                               
*                                                                               
*        TRANSLATE '\' TO ','                                                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          FIELD LENGTH                                 
         LR    RE,RF               SAVE INPUT LENGTH                            
*                                                                               
         LA    RF,8-1(RF,R2)       POINT TO LAST BYTE OF INPUT                  
*                                                                               
         CLI   0(RF),C'\'          TRANSLATE BACK SLASHS                        
         BNE   *+12                                                             
         MVI   0(RF),C','          TO COMMAS                                    
         OI    6(R2),X'80'         FORCE RE-DISPLAY OF FIELD                    
         BCTR  RF,0                BACK UP A BYTE                               
         BCT   RE,*-18                                                          
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(8),0(R2)       COPY R2 HEADER INTO WORK                     
*                                                                               
         CLI   8(R2),C'='          INDICATES LIST ONLY THIS INV NO.             
         BE    VK21                                                             
*                                                                               
         ZIC   RE,5(R2)            COPY R2 DATA INTO WORK                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),8(R2)                                                  
*                                                                               
         B     VK30                                                             
*                                                                               
VK21     CLI   ACTNUM,ACTLIST      PREFIX OF = ONLY OK FOR LIST/REPORT          
         BE    VK22                                                             
         CLI   ACTNUM,ACTREP                                                    
         BE    VK22                                                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
*                                                                               
VK22     OI    STATUS,X'80'        INDICATE LISTING WITH '='                    
*                                                                               
         ZIC   RE,5(R2)            COPY R2 DATA INTO WORK WITHOUT =             
         BCTR  RE,0                                                             
         STC   RE,WORK+5           ADJUST SIZE OF DATA                          
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),9(R2)                                                  
*                                                                               
VK30     MVI   ERROR,INVALID       INVENTORY/DATE EDIT                          
*                                                                               
         MVI   SOURCE,X'FF'        INDICATE RATIONALE RECORD                    
*                                                                               
         LA    R0,5                MAX CHUNKS TO INIT                           
         LA    R4,BLOCK                                                         
*                                                                               
         XC    0(32,R4),0(R4)                                                   
         LA    R4,32(R4)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         GOTO1 SCANNER,DMCB,WORK,(2,BLOCK)                                      
*                                                                               
         ZIC   R5,DMCB+4           NUMBER OF CHUNKS                             
         LTR   R5,R5               MUST BE AT LEAST ONE CHUNK                   
         BZ    ERREND                                                           
*                                                                               
         CH    R5,=H'2'            MAX TWO CHUNKS                               
         BH    ERREND                                                           
*                                                                               
         LA    R4,BLOCK                                                         
*                                                                               
*!!!     CLI   0(R4),3             INVENTORY NUMBER MUST BE 3                   
*!!!     BL    ERREND                                                           
         CLI   0(R4),4             OR 4 CHARACTERS                              
         BH    ERREND                                                           
*                                                                               
         MVC   RINVKINV,12(R4)     SAVE INVENTORY NUMBER                        
*                                                                               
         OC    RINVKINV,=4X'40'    BLANK FILL                                   
*                                                                               
         MVC   INVNO,RINVKINV                                                   
*                                                                               
         CH    R5,=H'2'            SECOND CHUNK IS EFFECTIVE DATE               
         BNE   VK50                                                             
*                                                                               
         MVI   ERROR,INVDATE                                                    
*                                                                               
         LA    R4,32(R4)           NOW VALIDATE DATE                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,12(R4)),WORK                                      
*                                                                               
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,RINVKSTD)                                
*                                                                               
         MVC   DATE,RINVKSTD                                                    
         MVC   KDATE,RINVKSTD                                                   
*                                                                               
VK50     MVC   RINVKSRC,SOURCE                                                  
*                                                                               
         TITLE 'T81005 - TEXT RECORDS - VKNUM'                                  
***********************************************************************         
*                                                                     *         
*        VALIDATE TEXT NUMBER (EXCEPT FOR LIST/REPORT)                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKNUM    DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTLIST      NO TEXT NUMBER FOR LIST                      
         BE    VKLSTSRC                                                         
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BE    VKLSTSRC                                                         
*                                                                               
         LA    R2,TXTNUMH                                                       
*                                                                               
         MVI   ERROR,NOTNUM                                                     
*                                                                               
         XC    HALF,HALF           INIT TEXT NUMBER WORKAREA                    
*                                                                               
         TM    4(R2),X'08'         NUMERIC                                      
         BNO   ERREND                                                           
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
*                                                                               
         CVB   R0,DUB                                                           
*                                                                               
         CH    R0,=H'9999'                                                      
         BH    ERREND                                                           
*                                                                               
         STH   R0,HALF                                                          
*                                                                               
VKTXTOK  DS    0H                                                               
*                                                                               
         MVC   NUMBER,HALF         SAVE TEXT NUMBER                             
         MVC   RINVKTXT,HALF                                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
         CLI   SOURCE,X'FF'        DON'T HAVE HEADER FOR MKT/STA                
         BNE   VKHDRX                                                           
*                                                                               
         MVC   AIO,AIO2            GET HEADER RECORD IN AIO2                    
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
*                                                                               
         MVC   KEY(24),HDRKEY      REP/STA/INV NO./(START DATE)                 
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VK90                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         B     VK120                                                            
*                                                                               
VK90     MVI   ERROR,NOTFOUND                                                   
*                                                                               
         OC    DATE,DATE                                                        
         BNZ   ERREND              INPUT A DATE, RECORD NOT FOUND               
*                                                                               
         CLC   KEYSAVE(RINVKSTD-RINVKEY),KEY  DID I FIND INV NO.                
         BNE   ERREND                                                           
*                                                                               
VK110    DS    0H                                                               
*                                                                               
         OC    KEY+24(3),KEY+24    MAKE SURE IT'S A HEADER                      
         BNZ   VK115                                                            
*                                                                               
         MVC   KDATE,RINVKSTD-RINVKEY+KEY  EFF DATE FROM KEY                    
*                                                                               
VK115    DS    0H                                                               
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEYSAVE(RINVKSTD-RINVKEY),KEY  CONTINUE IF SAME INV NO.          
         BE    VK110                                                            
*                                                                               
VK120    DS    0H                                                               
*                                                                               
         LA    R6,HDRKEY                                                        
         MVC   RINVKSTD,KDATE      SAVE IN HEADER KEY                           
*                                                                               
         LA    R2,TXTTYPH          PUT IT TO SCREEN                             
         LA    R1,8(R2)                                                         
         BAS   RE,INVDISP                                                       
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         MVC   DATE,KDATE                                                       
*                                                                               
VKDTEX   DS    0H                                                               
*                                                                               
VKHDRX   DS    0H                                                               
*                                                                               
         CLI   ORIGACT,C'C'        IF COPYING                                   
         BNE   VKHDRX10                                                         
*                                                                               
         MVC   KEY,FRKEY              READ IN FROM KEY                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
VKHDRX10 DS    0H                                                               
         B     VKXIT                                                            
*                                                                               
         TITLE 'T81005 - TEXT RECORDS - VKLSTSRC'                               
***********************************************************************         
*                                                                     *         
*        VALIDATE SOURCE FILTER (FOR LIST/REPORT)                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKLSTSRC LA    R2,LTXKSRCH                                                      
*                                                                               
         CLI   5(R2),0             OKAY IF NO FILTER SOURCE ENTERED             
         BE    VK160                                                            
*                                                                               
         MVI   ERROR,INVALID                                                    
*                                                                               
         MVC   KFSRC,8(R2)         SAVE ENTERED SOURCE                          
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
*                                                                               
         OC    WORK,SPACES                                                      
*                                                                               
*        VALIDATE SOURCE                                                        
*                                                                               
         EX    R1,*+12                                                          
         BE    VK160                                                            
         B     *+10                                                             
         CLC   WORK(0),=C'MFX'                                                  
*                                                                               
         EX    R1,*+12                                                          
         BE    VK160                                                            
         B     *+10                                                             
         CLC   WORK(0),=C'NSI'                                                  
*                                                                               
         EX    R1,*+12                                                          
         BE    VK160                                                            
         B     ERREND                                                           
         CLC   WORK(0),=C'SRC'                                                  
*                                                                               
****************************************************************                
*              VALIDATE FILTER BOOK (FOR LIST/REPORT)          *                
****************************************************************                
         SPACE 2                                                                
VK160    LA    R2,LTXKBKH          POINT TO FILTER BOOK FIELD                   
*                                                                               
         CLI   5(R2),0             NO ENTRY OKAY                                
         BE    VKBKX                                                            
*                                                                               
         MVI   BYTE,C'N'           DEFAULT TO NIELSEN AS SOURCE                 
*                                                                               
         CLI   KFSRC,0             IF SOURCE GIVEN                              
         BE    *+10                                                             
         MVC   BYTE,KFSRC             USE IT                                    
*                                                                               
         MVI   ERROR,INVALID                                                    
*                                                                               
         GOTO1 BOOKVAL,DMCB,(BYTE,(R2)),(1,WORK),(C'B',SCANNER),KFBKTP          
*                                                                               
         CLI   DMCB+4,1            EXIT IF THERE ARE ERRORS                     
         BNE   ERREND                                                           
*                                                                               
         TM    WORK,X'80'          DO NOT ALLOW '-' PREFIX                      
         BO    ERREND                WHICH MEANS SUPPRESS CPM'S                 
*                                                                               
         MVC   KFBKBITS,WORK       SAVE BOOKVAL BITS                            
         MVC   KFBKYM,WORK+1       SAVE BOOK                                    
*                                                                               
VKBKX    DS    0H                                                               
*                                                                               
****************************************************************                
*    VALIDATE FILTER FOR LOCAL TEXT ONLY ( FOR LIST/REPORT)    *                
****************************************************************                
         SPACE 1                                                                
VK170    LA    R2,LTXKLOCH                                                      
         CLI   5(R2),0                                                          
         BE    VKLOCX                                                           
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'N'                                                       
         BE    VK180                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
VK180    MVC   KFLOC,8(R2)         SAVE FILTER                                  
VKLOCX   DS    0H                                                               
         SPACE 2                                                                
****************************************************************                
*    VALIDATE FILTER FOR INTERNAL TEXT   ( FOR LIST/REPORT)    *                
****************************************************************                
         SPACE 1                                                                
VKINT    LA    R2,LTXKINTH                                                      
         CLI   5(R2),0                                                          
         BE    VKINTX                                                           
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'N'                                                       
         BE    VKINT1                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
VKINT1   MVC   KFINT,8(R2)         SAVE FILTER                                  
VKINTX   DS    0H                                                               
         SPACE 2                                                                
VKXIT    DS    0H                  BUILD TXT KEY IN AIO1                        
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY,HDRKEY                                                       
*                                                                               
         CLI   ACTNUM,ACTADD       IF COPYING                                   
         BNE   *+8                                                              
         CLI   ORIGACT,C'C'                                                     
         BNE   *+14                                                             
         MVC   TOKEY,HDRKEY           SAVE AS TO KEY                            
         B     *+10                                                             
         MVC   FRKEY,HDRKEY        ELSE SAVE AS FROM KEY                        
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T81005 - TEXT RECORDS - VREC'                                   
***********************************************************************         
*                                                                     *         
*        VALIDATE RECORD ROUTINE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VREC     DS    0H                                                               
*                                                                               
*        ANALYZE COPY FUNCTION                                                  
*                                                                               
         CLI   ACTNUM,ACTADD       IF COPYING                                   
         BNE   VRCPYX                                                           
         CLI   ORIGACT,C'C'                                                     
         BNE   VRCPYX                                                           
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,FRKEY           READ FROM RECORD INTO I/O AREA 1             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     RF,AIO                                                           
         MVC   0(L'RINVKEY,RF),TOKEY REPLACE KEY                                
*                                                                               
         MVC   KEY,TOKEY           RE-POINT FILE                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
VRCPYX   DS    0H                                                               
*                                                                               
         XC    FSRC,FSRC           INIT FILTER FIELDS                           
         XC    FBOOK,FBOOK                                                      
         XC    FBKTP,FBKTP                                                      
*                                                                               
         MVI   ELCODE,2                                                         
*                                                                               
         GOTO1 REMELEM             AND OLD X'02' ELEMENT                        
*                                                                               
         XC    ELEM,ELEM           AND REBUILD                                  
         LA    R6,ELEM                                                          
         USING RINVFEL,R6                                                       
*                                                                               
         MVC   RINVFCOD(2),=X'020A'                                             
*                                                                               
****************************************************************                
*              VALIDATE FILTER SOURCE                          *                
****************************************************************                
         SPACE 1                                                                
         LA    R2,TXTSRCH                                                       
*                                                                               
         MVI   ERROR,INVALID                                                    
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    VRSRCX              NO SOURCE                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
*                                                                               
         OC    WORK,SPACES                                                      
*                                                                               
         MVC   RINVFSRC,WORK                                                    
*                                                                               
         MVC   FSRC,WORK                                                        
*                                                                               
*        VALIDATE SOURCE                                                        
*                                                                               
         EX    R1,*+12                                                          
         BE    VRSRCX                                                           
         B     *+10                                                             
         CLC   WORK(0),=C'MFX'                                                  
*                                                                               
         EX    R1,*+12                                                          
         BE    VRSRCX                                                           
         B     *+10                                                             
         CLC   WORK(0),=C'NSI'                                                  
*                                                                               
         EX    R1,*+12                                                          
         BE    VRSRCX                                                           
         B     ERREND                                                           
         CLC   WORK(0),=C'SRC'                                                  
*                                                                               
VRSRCX   DS    0H                                                               
*                                                                               
****************************************************************                
*              VALIDATE INTERNAL TEXT   -DEFAULT IS NO         *                
****************************************************************                
         SPACE 1                                                                
VRINT    LA    R2,TXTINTH                                                       
         MVI   RINVFTYP,0          SET UP FOR DEFAULT                           
         CLI   5(R2),0                                                          
         BE    VRINTX                                                           
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'N'          NO IS DEFAULT                                
         BE    VRINTX                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         MVI   RINVFTYP,RINVFTIQ                                                
         MVI   KFINT,C'Y'                                                       
VRINTX   DS    0H                                                               
         SPACE 1                                                                
****************************************************************                
*              VALIDATE BOOK                                   *                
****************************************************************                
         SPACE 1                                                                
VR10     LA    R2,TXTBOOKH                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BE    VR15                                                             
*                                                                               
         MVI   BYTE,C'N'           DEFAULT TO NIELSEN                           
*                                                                               
         CLI   FSRC,0                                                           
         BE    *+10                                                             
         MVC   BYTE,FSRC                                                        
*                                                                               
         GOTO1 BOOKVAL,DMCB,(BYTE,(R2)),(1,WORK),(C'B',SCANNER),FBKTP           
         CLI   DMCB+4,1                                                         
         BNE   ERREND                                                           
*                                                                               
         TM    WORK,X'80'          DO NOT ALLOW '-' PREFIX                      
         BO    ERREND                WHICH MEANS SUPPRESS CPM'S                 
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         MVC   GSIRSVC,FSRC        SET RATING SERVICE                           
         MVC   GSIBITS,WORK        BOOKVAL BITS                                 
         MVC   GSIBKTYP,FBKTP      BOOK TYPE                                    
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GSRCIN),GSRCOUT                              
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BNE   ERREND                                                           
*                                                                               
         MVC   RINVFBKT,GSOBITS    BOOKVAL BITS                                 
         MVC   RINVFBK,WORK+1      BOOK                                         
         MVC   RINVFBTP,GSOBKTYP   BOOK TYPE                                    
*                                                                               
         MVC   FBKBITS,GSOBITS     BOOKVAL BITS                                 
         MVC   FBKYM,WORK+1        BOOK                                         
*                                                                               
****************************************************************                
*              VALIDATE LOCAL TEXT ONLY -DEFAULT IS NO         *                
****************************************************************                
         SPACE 1                                                                
VR15     LA    R2,TXTLOCH                                                       
         MVI   RINVFLOC,0          SET UP FOR DEFAULT                           
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'N'          NO IS DEFAULT                                
         BE    VR20                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         MVI   RINVFLOC,C'Y'                                                    
         MVI   KFLOC,C'Y'                                                       
         SPACE 1                                                                
****************************************************************                
*         VALIDATE PRINT WITH WORD WRAP                                         
****************************************************************                
         SPACE 1                                                                
VR20     LA    R2,TXTWRPH                                                       
*                                                                               
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VRWRP10                                                          
*                                                                               
         MVI   5(R2),1             SET DATA LENGTH                              
         OI    6(R2),X'80'         FORCE TRANSMISSION                           
*                                                                               
         TM    RMPPROF+RMPWWRPB,RMPWWRPA    IF 'Y' IS DEFAULT                   
         BNO   *+12                                                             
         MVI   8(R2),C'Y'                   SET 'Y' AS OVERRIDE                 
         B     *+8                                                              
         MVI   8(R2),C'N'                   ELSE, SET 'N'                       
         B     VRWRPX                                                           
*                                                                               
VRWRP10  DS    0H                                                               
*                                                                               
         MVI   ERROR,INVALID       SET ERROR                                    
*                                                                               
         CLI   5(R2),1             INPUT LENGTH OKAY?                           
         BNE   ERREND               NO,ERROR                                    
*                                                                               
         CLI   8(R2),C'N'          IF OPTION IS 'N'                             
         BNE   *+12                                                             
         MVI   RINVFWRP,C'N'        SAVE FLAG                                   
         B     VRWRPX                                                           
*                                                                               
         CLI   8(R2),C'Y'          IF OPTION IS 'Y'                             
         BNE   *+12                                                             
         MVI   RINVFWRP,C'Y'        SAVE FLAG                                   
         B     VRWRPX                                                           
*                                                                               
         B     ERREND              INVALID ENTRY                                
*                                                                               
VRWRPX   DS    0H                                                               
         SPACE 1                                                                
****************************************************************                
*              VALIDATE DEMOS - MAXIMUM 6 ALLOWED              *                
****************************************************************                
         SPACE 1                                                                
VR21     LA    R2,TXTDEMOH                                                      
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         LA    R4,BLOCK                                                         
         USING DEMOD,R4                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 DEMOVAL,DMCB,(R2),(6,WORK),(0,DBLOCK)                            
         DROP  R4                                                               
         MVI   ERROR,INVALID                                                    
         CLI   DMCB+4,0                                                         
         BE    ERREND                                                           
         ZIC   R4,DMCB+4                                                        
         AH    R4,=H'10'                                                        
         STC   R4,RINVFLEN                                                      
         SH    R4,=H'10'                                                        
         LA    R3,WORK                                                          
         LA    RE,RINVFDEM                                                      
         MVC   0(1,RE),2(R3)       DEMO NUMBERS TO ELEMENT                      
         LA    RE,1(RE)                                                         
         LA    R3,3(R3)                                                         
         BCT   R4,*-14                                                          
         SPACE 1                                                                
VR50     ZIC   R4,RINVFLEN                                                      
         SH    R4,=H'3'                                                         
         EX    R4,*+12                                                          
         BZ    VR90                DON'T ADD ZERO ELEMENT                       
         B     VR60                                                             
         OC    RINVFSRC(0),RINVFSRC                                             
         DROP  R6                                                               
         SPACE 1                                                                
VR60     GOTO1 ADDELEM                                                          
*                                                                               
VR90     DS    0H                                                               
*                                                                               
****************************************************************                
*              VALIDATE FORCE                                  *                
****************************************************************                
         SPACE 2                                                                
VRFRC    DS    0H                                                               
*                                                                               
         LA    R2,TXTFRCH          POINT TO FORCE FIELD                         
*                                                                               
         CLI   5(R2),0             INPUT NOT REQUIRED                           
         BE    VRFRC10                TREATED AS 'N'                            
*                                                                               
         MVI   ERROR,INVALID       DEFAULT ERROR MESSAGE ID                     
*                                                                               
         CLI   5(R2),1             INPUT LENGTH MUST BE 1                       
         BNE   ERREND                                                           
*                                                                               
         CLI   8(R2),C'N'          'N' IS THE DEFAULT                           
         BE    VRFRC10                                                          
*                                                                               
         CLI   8(R2),C'Y'          'Y' ONLY ALTERNATIVE TO 'N'                  
         BNE   ERREND                                                           
*                                                                               
         CLI   SOURCE,X'FF'                                                     
         BNE   ERREND               CAN'T FORCE STA OR MKT TEXT                 
*                                                                               
VRFRC10  DS    0H                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,RINVTCCQ     FIND TEXT CONTROL ELEMENT                    
         BRAS  RE,GETEL                                                         
         JNE   VRFRC40                NONE FOUND                                
*                                                                               
         USING RINVTCEL,R6         ESTABLISH TEXT CONTROL ELEMENT               
*                                                                               
         CLI   5(R2),0             IF NO ENTRY                                  
         BE    *+8                                                              
         CLI   8(R2),C'N'          OR OPTION IS NO                              
         BNE   VRFRC15                                                          
*                                                                               
         TM    RINVTCN1,RINVTCFQ          IF TEXT NOT CURRENTLY FORCED          
         BNO   VRFRCX                        NOTHING TO DO                      
         NI    RINVTCN1,X'FF'-RINVTCFQ    ELSE TURN OFF FORCE INDICATOR         
         B     VRFRCX                                                           
*                                                                               
VRFRC15  DS    0H                  OPTION IS YES                                
*                                                                               
         OI    RINVTCN1,RINVTCFQ   TURN ON FORCE INDICATOR                      
         B     VRFRCX                                                           
*                                                                               
VRFRC40  DS    0H                  NO TEXT CONTROL ELEMENT                      
*                                                                               
         CLI   5(R2),0             SKIP IF NO ENTRY                             
         BE    VRFRCX                                                           
         CLI   8(R2),C'N'          SKIP IF OPTION NO                            
         BE    VRFRCX                                                           
*                                  ELSE ADD AN ELEMENT                          
         XC    ELEM,ELEM           BUILD TEXT CONTROL ELEMENT IN ELEM           
         LA    R6,ELEM                                                          
         MVI   RINVTCCD,RINVTCCQ   ELEMENT CODE                                 
         MVI   RINVTCLN,RINVTCLQ   ELEMENT LENGTH                               
         OI    RINVTCN1,RINVTCFQ   SET FORCE INDICATOR                          
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO TEXT RECORD                   
*                                                                               
VRFRCX   DS    0H                                                               
*                                                                               
****************************************************************                
*        VALIDATE TEXT - MAXIMUM 40 LINES ALLOWED              *                
****************************************************************                
*                                                                               
         BAS   RE,FNDLST           FIND LAST LINE WITH ACTUAL DATA              
*                                                                               
***********************************************************************         
*                                                                     *         
* VALIDATE DETAIL INPUT VIA LINUP                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         MVC   SVLSVTAB,LSVTAB     SAVE SAVE COPY OF LINUP SAVE TABLE           
*                                  TO RESTORE AFTER VALIDATION ERRORS           
         GOTO1 =A(LINSET),RR=RELO  LINUP INTERFACE                              
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF ADDING                               
         BNE   *+12                                                             
         CLI   ORIGACT,C'C'        BUT NOT COPYING                              
         BNE   VR160                                                            
*                                                                               
         XC    SVLSTDT,SVLSTDT     INIT LAST ACTIVITY DATE SAVEAREA             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'EF'        GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   VR160                                                            
         USING RINVAEL,R6                                                       
         MVC   SVLSTDT,RINVALST    SAVE LAST ACTIVITY DATE                      
         MVC   RINVALST,BTODAY                                                  
         MVI   RINVAWHY,C'C'                                                    
         B     VRATVX                                                           
         DROP  R6                                                               
         SPACE 1                                                                
VR160    XC    ELEM,ELEM                                                        
         LA    R6,ELEM             BUILD ACTIVITY ELEMENT                       
         USING RINVAEL,R6                                                       
         MVC   RINVACOD(2),=X'EF0C'                                             
         MVC   RINVAFST,BTODAY                                                  
         MVC   RINVALST,BTODAY                                                  
         MVI   RINVAWHY,C'A'                                                    
         XC    RINVAWHY+1(3),RINVAWHY+1 SPARE                                   
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
VRATVX   DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       SKIP IF NOT COPYING                          
         BNE   *+8                                                              
         CLI   ORIGACT,C'C'                                                     
         BNE   VRCPACTX                                                         
*                                                                               
         FOUT  CONACTH,=C'COPY'       CHANGE TO 'COPY'                          
*                                                                               
         LA    R2,CONACTH                                                       
         ST    R2,ACURFORC         FORCE CURSOR TO THIS FIELD                   
*                                                                               
         MVC   RERROR,=AL2(RI#CPYTX)   INDICATE SUCCESSFUL COPY                 
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
*                                                                               
         BAS   RE,MSGOK            SET UP FOR INFORMATIONAL MSG EXIT            
*                                                                               
VRCPACTX DS    0H                                                               
*                                                                               
VRECX    DS    0H                                                               
*                                                                               
VRXIT    B     XIT                                                              
*                                                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
*                                                              *                
*    RDEL - DELETE RECORD - TELL GENCON TO PROCESS NEXT SELECT *                
*                                                              *                
****************************************************************                
         SPACE 2                                                                
RDEL     DS    0H                                                               
*                                                                               
RDELX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
****************************************************************                
****************************************************************                
*    XREC - IF FORCE=Y, ADD TEXT TO INVENTORY THAT MATCHES     *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
XREC     DS    0H                                                               
*                                                                               
         BRAS  RE,XRECROUT                                                      
*                                                                               
XRECX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
XRLTRANS DS    0H                                                               
*                                                                               
         CLC   CSTAT,SPACES        MAKE SURE WE HAVE A STATION                  
         BNH   XRLTRANX                                                         
*                                                                               
         NI    DMINBTS,X'FF'-X'80'     TURN OFF DELETED REC READ                
*                                                                               
         GOTO1 VLTRANS             ISSUE LTRANS REQUEST IF NEEDED               
*                                                                               
XRLTRANX DS    0H                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE 2                                                                
DKEY     DS    0H                                                               
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY AND                                 
         MVC   SVDMWORK,DMWORK+4   SAVE D/A (LATER GETREC IN VALISTA)           
*                                                                               
         L     R6,AIO              RECORD SELECTED                              
         USING RINVKEY,R6                                                       
*                                                                               
         LA    R2,TXTSTAH          STATION                                      
         LA    RE,8(R2)                                                         
         MVC   0(4,RE),RINVKSTA                                                 
*                                                                               
         LA    RE,3(RE)                                                         
*                                                                               
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
*                                                                               
         CLI   RINVKSTA+4,C'T'                                                  
         BE    DK20                                                             
*                                                                               
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),RINVKSTA+4                                               
*                                                                               
DK20     OI    TXTSTAH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 VALISTA             GET MARKET NAME                              
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,TXTMRKTH         MARKET NAME                                  
         MVC   8(20,R2),WORK+10                                                 
         OI    TXTMRKTH+6,X'80'                                                 
*                                                                               
         LA    R2,TXTTYPH                                                       
         MVC   SOURCE,RINVKSRC                                                  
*                                                                               
         CLI   RINVKSRC,X'FF'                                                   
         BE    DK30                                                             
*                                                                               
         MVC   8(3,R2),=C'MKT'                                                  
*                                                                               
         CLI   RINVKSRC,C'M'                                                    
         BE    DK40                                                             
*                                                                               
         MVC   8(3,R2),=C'STA'                                                  
*                                                                               
         B     DK40                                                             
*                                                                               
DK30     LA    R1,8(R2)            OR                                           
         MVC   INVNO,RINVKINV                                                   
         MVC   KDATE,RINVKSTD                                                   
         MVC   DATE,RINVKSTD                                                    
*                                                                               
         BAS   RE,INVDISP          INVENTORY NUMBER, DATE                       
*                                                                               
DK40     OI    TXTTYPH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
         LA    R2,TXTNUMH          TEXT NUMBER                                  
         MVC   NUMBER,RINVKTXT                                                  
*                                                                               
         EDIT  (2,RINVKTXT),(5,8(R2)),ALIGN=LEFT                                
*                                                                               
         OI    TXTNUMH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
DKXIT    DS    0H           NEED TO DO ANOTHER GETREC BEFORE PUTREC             
*                             (DID GETREC IN VALISTA FOR MKT NAME)              
         MVC   AIO,AIO2            PUT IT IN AIO2                               
         MVC   KEY(27),SVKEY                                                    
         MVC   KEY+28(4),SVDMWORK                                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AIO1            NEW RECORD IS IN AIO1                        
*                                                                               
         CLC   OLDKEY,SVKEY        IF NEW KEY                                   
         BE    *+14                                                             
         MVI   NEWKEY,C'Y'            INDICATE NEW KEY                          
         MVC   OLDKEY,SVKEY           SAVE NEW KEY                              
*                                                                               
         MVC   FRKEY,SVKEY    SAVE AS FROM KEY                                  
*                                                                               
DKXX     B     XIT                                                              
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DREC     DS    0H                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RINVTEL,R6                                                       
*                                                                               
         LA    R2,TXTSRCH          CLEAR OUT SOURCE                             
         BAS   RE,CLEAR                                                         
         LA    R2,TXTINTH          CLEAR OUT INTERNAL TEXT                      
         BAS   RE,CLEAR                                                         
         LA    R2,TXTBOOKH         BOOK                                         
         BAS   RE,CLEAR                                                         
         LA    R2,TXTLOCH          LOCAL TEXT ONLY                              
         BAS   RE,CLEAR                                                         
         LA    R2,TXTFRCH          FORCE                                        
         BAS   RE,CLEAR                                                         
         LA    R2,TXTWRPH          WORD WRAP                                    
         BAS   RE,CLEAR                                                         
         LA    R2,TXTDEMOH         AND DEMO FIELDS                              
         BAS   RE,CLEAR                                                         
         SPACE 1                                                                
***********************************************************************         
*                                                                     *         
*  DISPLAY DETAILS VIA LINUP                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         GOTO1 =A(LINSET),RR=RELO    INTERFACE WITH LINUP                       
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
         LA    R2,TXTINTH          PRESET INTERNAL TEXT TO NO                   
         MVI   8(R2),C'N'                                                       
*                                                                               
         LA    R2,TXTLOCH          PRESET LOCAL TEXT TO NO                      
         MVI   8(R2),C'N'                                                       
*                                                                               
         L     R6,AIO              IS THERE A FILTER ELEMENT?                   
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRFLTX                                                           
*                                                                               
         USING RINVFEL,R6                                                       
*                                                                               
         CLI   RINVFSRC,0                                                       
         BE    DR20                                                             
*                                                                               
         LA    R2,TXTSRCH                                                       
*                                                                               
         CLI   RINVFSRC,C'M'                                                    
         BNE   *+10                                                             
         MVC   8(3,R2),=C'MFX'                                                  
*                                                                               
         CLI   RINVFSRC,C'N'                                                    
         BNE   *+10                                                             
         MVC   8(3,R2),=C'NSI'                                                  
*                                                                               
         CLI   RINVFSRC,C'S'                                                    
         BNE   *+10                                                             
         MVC   8(3,R2),=C'SRC'                                                  
*                                                                               
DR20     OC    RINVFBK,RINVFBK                                                  
         BZ    DR25                                                             
*                                                                               
         LA    R2,TXTBOOKH                                                      
*                                                                               
         MVC   WORK(1),RINVFBKT                                                 
         MVC   WORK+1(2),RINVFBK                                                
         MVC   WORK+3(1),RINVFBTP                                               
*                                                                               
         BAS   RE,FMTBOOK                                                       
*                                                                               
         MVC   8(09,R2),WORK+10                                                 
*                                                                               
DR25     DS    0H                                                               
*                                                                               
         LA    R2,TXTINTH          INTERNAL TEXT?                               
*                                                                               
         CLI   RINVFTYP,RINVFTIQ                                                
         BNE   *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
         LA    R2,TXTLOCH          LOCAL TEXT ONLY?                             
*                                                                               
         CLI   RINVFLOC,C'Y'                                                    
         BNE   *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
DR27     MVC   TXTWRP(1),RINVFWRP  PLACE WORD WRAP OPTION ON SCREEN             
         OI    TXTWRPH+6,X'80'     XMIT                                         
*                                                                               
DR30     ZIC   R3,RINVFLEN                                                      
         SH    R3,=H'10'                                                        
         BNP   DRDEMX                                                           
         STC   R3,BYTE             R3 AND BYTE HAVE NUMBER OF DEMOS             
         LA    R2,TXTDEMOH                                                      
         LA    RE,RINVFDEM                                                      
         XC    WORK,WORK           WORK WILL HAVE FULL DEMO EXPRESSIONS         
         LA    R4,WORK                                                          
DR50     MVI   1(R4),C'T'                                                       
         MVC   2(1,R4),0(RE)                                                    
         LA    R4,3(R4)                                                         
         LA    RE,1(RE)            POINT TO NEXT DEMO                           
         BCT   R3,DR50             ANY MORE                                     
         SPACE 1                                                                
         LA    R4,BLOCK                                                         
         USING DEMOD,R4                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 DEMOCON,DMCB,(BYTE,WORK),(9,8(R2)),(0,DBLOCK)                    
         DROP  R4                                                               
*                                                                               
DRDEMX   DS    0H                                                               
*                                                                               
DRFLTX   DS    0H                                                               
*                                                                               
*        DISPLAY FORCE OPTION                                                   
*                                                                               
DRFRC    DS    0H                                                               
*                                                                               
         LA    R2,TXTFRCH          POINT TO FORCE FIELD                         
*                                                                               
         MVI   8(R2),C' '          INIT FORCE OPTION DISPLAY                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,RINVTCCQ     FIND TEXT CONTROL ELEMENT                    
         BRAS  RE,GETEL                                                         
         JNE   DRFRCX                 NONE FOUND                                
*                                                                               
         USING RINVTCEL,R6         ESTABLISH TEXT CONTROL ELEMENT               
*                                                                               
         TM    RINVTCN1,RINVTCFQ  SKIP IF TEXT NOT CURRENTLY FORCED             
         BNO   DRFRCX                        NOTHING TO DO                      
*                                                                               
         MVI   8(R2),C'Y'          INDICATE TEXT IS FORCED                      
         MVI   5(R2),1             SET INPUT LENGTH                             
         OI    6(R2),X'80'         FORCE TRANSMISSION                           
*                                                                               
         B     DRFRCX                                                           
*                                                                               
DRFRCX   DS    0H                                                               
*                                                                               
DRECX    DS    0H                                                               
*                                                                               
DRXIT    DS    0H                                                               
         CLI   ACTNUM,ACTCPY       IF COPYING                                   
         BNE   *+8                                                              
         MVI   CPYACTSW,CPYACTFQ      THEN FROM PHASE COMPLETE                  
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
         TITLE 'RERMP05 - TEXT RECORD - CPYREC'                                 
***********************************************************************         
*                                                                     *         
*        LIST AND PRINT ROUTINE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LIST     DS    0H                                                               
*                                                                               
         GOTO1 =A(LISTREC),RR=RELO                                              
*                                                                               
LISTX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
                                                                                
         TITLE 'RERMP05 - TEXT RECORD - CPYREC'                                 
***********************************************************************         
*                                                                     *         
*        COPY TEXT RECORDS                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CPYREC   DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         XC    KEY,KEY             ESTABLISH FROM TEXT KEY                      
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
*                                                                               
         MVI   RINVKTYP,X'12'      SET RECORD ID                                
         MVC   RINVKREP,AGENCY     SET REP CODE                                 
         MVC   RINVKSTA,CPYFSTA    SET FROM STATION                             
         MVC   RINVKINV,CPYFINV    SET FROM INVENTORY NUMBER                    
         MVC   RINVKSTD,CPYFDTE    SET FROM EFFECTIVE DATE                      
         MVC   RINVKSRC,CPYFTYP    SET FROM TEXT TYPE                           
         MVC   RINVKTXT,CPYFNUM    SET FROM TEXT NUMBER                         
*                                                                               
         GOTO1 HIGH                READ FOR KEY                                 
*                                                                               
CPRLOOP  DS    0H                                                               
*                                                                               
         CLC   RINVKEY(RINVKTXT-RINVKEY),KEYSAVE   DONE ON KEY CHANGE           
         JNE   CPRDONE                                                          
*                                                                               
         OC    CPYFNUM,CPYFNUM     OKAY IF COPYING ALL TEXT                     
         JZ    CPR10                                                            
*                                                                               
         CLC   RINVKEY,KEYSAVE     ERROR IF FROM KEY NOT FOUND                  
         JNE   CPRNOTFE                                                         
*                                                                               
CPR10    DS    0H                                                               
*                                                                               
         MVC   FRKEY,KEY           SAVE FROM KEY                                
*                                                                               
         MVC   AIO,AIO1            READ RECORD INTO I/O1                        
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT TO FOUND RECORD                        
*                                                                               
         XC    KEY,KEY             ESTABLISH TO   TEXT KEY                      
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
*                                                                               
         MVI   RINVKTYP,X'12'      SET RECORD ID                                
         MVC   RINVKREP,AGENCY     SET REP CODE                                 
         MVC   RINVKSTA,CPYTSTA    SET TO STATION                               
         MVC   RINVKINV,CPYTINV    SET TO INVENTORY NUMBER                      
         MVC   RINVKSTD,CPYTDTE    SET TO EFFECTIVE DATE                        
         MVC   RINVKSRC,CPYTTYP    SET TO TEXT TYPE                             
         MVC   RINVKTXT,CPYTNUM    SET TO TEXT NUMBER                           
*                                                                               
         OC    RINVKTXT,RINVKTXT   IF TEXT NOT GIVEN                            
         JNZ   *+10                                                             
         MVC   RINVKTXT,RINVKTXT-RINVKEY(R6)   USE FROM NUMBER                  
*                                                                               
         GOTO1 HIGH                READ FOR KEY                                 
*                                                                               
         CLC   RINVKEY,KEYSAVE     IF TO KEY FOUND                              
         JNE   CPR20                                                            
*                                                                               
         TM    RINVKEY+27,X'80'    THEN IT MUST BE DELETED                      
         JNO   CPRDUPE                                                          
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC              READ IN RECORD                               
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   0(L'RINVKEY,R6),RINVKEY COPY TO KEY                              
*                                                                               
         GOTO1 PUTREC              ADD TEXT RECORD                              
*                                                                               
         NI    RINVKEY+27,X'FF'-X'80'    TURN OFF DELETE BIT                    
*                                                                               
         GOTO1 WRITE               RE-WRITE KEY                                 
*                                                                               
         J     CPRCONT                                                          
*                                                                               
CPR20    DS    0H                                                               
*                                                                               
         MVC   RINVKEY,KEYSAVE     RESTORE TO KEY                               
*                                                                               
         MVC   0(L'RINVKEY,R6),RINVKEY COPY TO KEY                              
*                                                                               
         GOTO1 ADDREC              ADD TEXT RECORD                              
*                                                                               
CPRCONT  DS    0H                                                               
*                                                                               
         MVC   KEY,FRKEY           RESTORE FROM KEY                             
*                                                                               
         GOTO1 HIGH                RE-POSITION FILE POINTERS                    
         GOTO1 SEQ                 READ NEXT POINTER ON FILE                    
*                                                                               
         J     CPRLOOP                                                          
*                                                                               
CPRDONE  DS    0H                                                               
*                                                                               
CPYRECX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
CPRDUPE  DS    0H                  TO TEXT ALREADY EXISTS                       
         MVI   ERROR,NOTFOUND                                                   
*                                                                               
CPRNOTFE DS    0H                  FROM TEXT NOT FOUND                          
         MVI   ERROR,NOTFOUND                                                   
*                                                                               
CPRERR   DS    0H                  ERROR EXIT                                   
         GOTO1 ERREX                                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'RERMP05 - TEXT RECORD - MSGOK'                                  
SUBROUTS DS    0D                  SUBROUTINES                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        PRINT INFORMATIONAL MESSAGE                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MSGOK    NTR1  LABEL=*                                                          
*                                                                               
         OI    GENSTAT2,USGETTXT+USMYOK+STLCOK LOWERCASE                        
*                                  GENCON MUST CALL GETTXT, NOT GETMSG          
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         MVC   GTINDX,RINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,RERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,RMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,RTXTLEN      LENGTH OF OPTIONAL TEXT                      
         MVC   GTATXT,RTXTADR      A(OPTIONAL TEXT)                             
*                                                                               
         DROP  RF                                                               
*                                                                               
MSGOKX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'RERMP05 - TEXT RECORD - FNDLST'                                 
***********************************************************************         
*                                                                     *         
*        FIND LAST LINE WITH ACTUAL DATA                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FNDLST   NTR1  LABEL=*                                                          
*                                                                               
         LA    R2,TXTTXT1H         A(FIRST FIELD)                               
         LA    R4,NLINS            NUMBER OF LINES                              
*                                                                               
         ST    R2,LINLAST          INIT A(LAST LINE OF TEXT)                    
*                                                                               
FNDLSTLP DS    0H                                                               
*                                                                               
         CLC   8(L'TXTTXT1,R2),SPACES FIND A LINE WITH DATA                     
         BNH   FNDLSTCN            EMPTY LINE                                   
*                                                                               
         ST    R2,LINLAST          UPDATE A(LAST LINE OF TEXT)                  
*                                                                               
FNDLSTCN DS    0H                                                               
*                                                                               
         BAS   RE,BUMPU            BUMP TO START OF NEXT LINE                   
         BCT   R4,FNDLSTLP                                                      
*                                                                               
FNDLSTDN DS    0H                                                               
*                                                                               
FNDLSTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
** ROUTINE TO DISPLAY INVENTORY NUMBER AND DATE                                 
*  ON ENTRY, R1 POINTS TO OUTPUT AREA                                           
*            INVNO HAS INVENTORY NUMBER                                         
*            KDATE HAS DATE                                                     
         SPACE 1                                                                
INVDISP  NTR1                                                                   
         LR    R3,R1                                                            
         SR    RE,RE                                                            
*        IC    RE,INVNO                                                         
*        CVD   RE,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  DUB(3),DUB+6(2)                                                  
*        MVC   0(2,R3),DUB+1       QTR HOUR                                     
*        IC    RE,INVNO+1                                                       
*        STC   RE,2(R3)                                                         
*        LA    R3,3(R3)                                                         
*        CLI   INVNO+2,C'0'                                                     
*        BE    *+14                                                             
*        MVC   0(1,R3),INVNO+2                                                  
         MVC   0(4,R3),INVNO                                                    
         LA    RF,4                                                             
INVD050  CLI   0(R3),X'40'                                                      
         BNH   INVD070                                                          
         LA    R3,1(R3)                                                         
         BCT   RF,INVD050                                                       
INVD070  MVI   0(R3),C','          DATE                                         
         GOTO1 DATCON,DMCB,(3,KDATE),(8,1(R3))                                  
         XIT1                                                                   
         EJECT                                                                  
* THIS ROUTINE CLEARS OUT A FIELD                                               
* ON ENTRY, R2 POINTS TO FIELD HEADER                                           
         SPACE 1                                                                
CLEAR    DS    0H                                                               
         ZIC   R1,0(R2)            GET LENGTH OF FIELD                          
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'         EXTENDED HEADER                              
         BZ    CL10                                                             
         SH    R1,=H'8'                                                         
         SPACE 1                                                                
CL10     EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         BR    RE                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
         EJECT                                                                  
*  THIS ROUTINE FORMATS THE BOOK TYPE, MONTH AND YEAR                           
*  ON ENTRY,                                                                    
*            WORK   HAS BOOKCAL BITS                                            
*            WORK+1 HAS YEAR                                                    
*            WORK+2 HAS MONTH                                                   
*            WORK+3 HAS BOOKTYPE                                                
*  ON EXIT, WORK+10(9) HAS OUTPUT IE PJUL87(M)                                  
*                                                                               
         SPACE 2                                                                
FMTBOOK  NTR1                                                                   
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         MVC   GSIBITS,WORK        BOOKVAL BITS                                 
         MVC   GSIBKTYP,WORK+3     BOOK TYPE                                    
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GSRCIN),GSRCOUT                              
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BNE   FMTBKERR                                                         
*                                                                               
         MVC   WORK+10(9),SPACES                                                
         MVC   WORK+10(1),GSOQLF   BOOK QUALIFIER                               
*                                                                               
         LA    RE,WORK+10          START OF OUTPUT                              
*                                                                               
         CLI   WORK+10,C' '        IF NOT NIELSN NOR BLANK                      
         BNH   *+16                                                             
         CLI   WORK+10,C'N'                                                     
         BE    *+8                                                              
         LA    RE,1(RE)               BUMP TO NEXT PRINT POSITION               
*                                                                               
         SR    R1,R1                                                            
         IC    R1,WORK+2           NOW DO MONTH                                 
         MH    R1,=H'3'                                                         
         LA    R1,MONTH(R1)                                                     
         MVC   0(3,RE),0(R1)                                                    
         CLC   =C'ST ',0(RE)       IF SPECIAL ESTIMATE BOOK                     
         BNE   *+6                                                              
         BCTR  RE,0                   RECTIFY COUNT                             
         LA    RE,3(RE)                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,WORK+1           AND YEAR                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   0(2,RE),DUB+1                                                    
         LA    RE,2(RE)            BUMP POINTER                                 
*                                                                               
         CLI   GSOBKTYP,C' '       SKIP IF NO BOOK TYPE                         
         BNH   *+18                                                             
         MVI   0(RE),C'('          DISPLAY BOOK TYPE                            
         MVC   1(1,RE),GSOBKTYP                                                 
         MVI   2(RE),C')'                                                       
*                                                                               
FMTBOOKX DS    0H                                                               
         XIT1                                                                   
FMTBKERR DS    0H                                                               
         GOTO1 ERREX               ERROR EXIT                                   
*                                                                               
         SPACE 2                                                                
MONTH    DC    C'ST JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                       
         EJECT                                                                  
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
****************************************************************                
*  HEDSPECS                                                    *                
****************************************************************                
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H3,1,C'STATION -'                                                
         SSPEC H1,52,C'TEXT LISTING'                                            
         SSPEC H2,52,C'------------'                                            
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
         SPACE 4                                                                
HOOK     NTR1  LABEL=*                                                          
         LA    R2,TXTSTAH                                                       
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   H3+11(0),8(R2)      STATION                                      
         MVC   H3+20(20),TXTMRKT   MARKET NAME                                  
         SPACE 1                                                                
         LA    R2,TXTTYPH          INVENTORY NUMBER IS OPTIONAL FILTER          
         CLI   5(R2),0                                                          
         BE    HK50                                                             
         MVC   H3+92(6),=C'TYPE -'                                              
         CLI   8(R2),C'='                                                       
         BNE   HK40                                                             
         ZIC   RE,5(R2)                                                         
         SH    RE,=H'2'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   H3+99(0),9(R2)                                                   
         B     HK50                                                             
         SPACE 1                                                                
HK40     MVC   H3+99(13),=C'STARTING WITH'                                      
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   H3+113(0),8(R2)                                                  
         SPACE 1                                                                
HK50     LA    R2,LTXKSRCH          SOURCE IS OPTIONAL FILTER                   
         LA    R3,H4+92                                                         
         CLI   5(R2),0                                                          
         BE    HK60                                                             
         MVC   0(8,R3),=C'SOURCE -'                                             
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   9(0,R3),8(R2)                                                    
         LA    R3,H4+108                                                        
         SPACE 1                                                                
HK60     LA    R2,LTXKBKH          BOOK IS OPTIONAL FILTER                      
         CLI   5(R2),0                                                          
         BE    HK100                                                            
         MVC   0(6,R3),=C'BOOK -'                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   7(0,R3),8(R2)                                                    
         SPACE 1                                                                
HK100    LA    R2,LTXKLOCH         LOCAL TEXT ONLY IS OPTIONAL FILTER           
         MVC   H4(20),SPACES                                                    
         CLI   5(R2),0                                                          
         BE    HK150                                                            
         MVC   H4(20),=CL20'EXCLUDING LOCAL TEXT'                               
         CLI   8(R2),C'N'                                                       
         BE    HK150                                                            
         MVC   H4(20),=CL20'LOCAL TEXT ONLY'                                    
         SPACE 1                                                                
HK150    LA    R3,H6               FILTER HEADLINES                             
         MVC   18(42,R3),LTXHDS                                                 
         LA    R3,H7                                                            
         MVC   18(77,R3),DASH                                                   
         XIT1                                                                   
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
DASH     DC    132C'-'                                                          
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER LITTLE ROUTINES                                               *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*        DISPLAY DATA IN  FLD IN FIELD POINTED TO BY R1               *         
***********************************************************************         
         SPACE 2                                                                
DSPFLD   NTR1                      BUMP TO NEXT SCREEN FIELD                    
*                                                                               
         USING FLDHDRD,R1          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         SH    RF,=H'8'            HEADER LENGTH                                
*                                                                               
         TM    FLDATB,X'02'        IF THERE IS EXTENED HEADER                   
         BNO   *+8                                                              
         SH    RF,=H'8'               TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),FLD         MOVE DATA TO OUTPUT                          
*                                                                               
         OI    FLDOIND,X'80'       TRANSMIT FIELD                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
DUPERR   MVC   RERROR,=AL2(DUPEDATA)     DUPLICATE DATA                         
         B     ERRX                                                             
*                                                                               
MISSERR  MVC   RERROR,=AL2(MISSING)                                             
         B     ERRX                                                             
*                                                                               
ERRX     GOTO1 MYERROR                                                          
*                                                                               
DUPEDATA EQU   524                 DUPLICATE DATA                               
RECFULL  EQU   523                 RECORD FULL                                  
*                                                                               
         LTORG                                                                  
*                                                                               
****************************************************************                
*    XREC - IF FORCE=Y, ADD TEXT TO INVENTORY THAT MATCHES     *                
****************************************************************                
         SPACE 2                                                                
XRECROUT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
****************************************************************                
*              VALIDATE FORCE                                  *                
****************************************************************                
         SPACE 1                                                                
         LA    R2,TXTFRCH                                                       
         CLI   5(R2),0                                                          
         BE    XRXIT                                                            
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   XRECERR                                                          
         CLI   8(R2),C'N'                                                       
         BE    XRXIT                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   XRECERR                                                          
         CLI   SOURCE,X'FF'                                                     
         BNE   XRECERR              CAN'T FORCE STA OR MKT TEXT                 
         SPACE 1                                                                
****************************************************************                
*              FIND MATCHING INVENTORY                         *                
****************************************************************                
         SPACE 1                                                                
         USING RINVKEY,R5                                                       
*                                                                               
         L     R5,AIO              TEXT RECORD                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RINVKEY     COPY TEXT RECORD KEY                         
*                                                                               
         MVC   AIO,AIO2            DO INVENTORY STUFF IN IO2                    
*                                                                               
*        TRANSLATE FILTER INTO KSRC FORMAT                                      
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
         MVC   GSIRSVC,FSRC        SET RATING SERVICE                           
         CLI   FSRC,0              IF RATING SERVICE NOT GIVEN                  
         BNE   *+8                                                              
         MVI   GSIRSVC,C'N'           DEFAULT IS NIELSEN                        
*                                                                               
         MVC   GSIBITS,FBKBITS     SET BOOKVAL BITS                             
         MVC   GSIBKTYP,FBKTP      SET BOOKTYPE                                 
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GSRCIN),GSRCOUT                              
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BNE   XRECERR                                                          
*                                                                               
         LA    R5,KEY              POINT TO KEY BUILD AREA                      
         XC    RINVKSRC(3),RINVKSRC                                             
*                                                                               
         MVC   RINVKSRC,FSRC       IF FILTER SOURCE, USE IT                     
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
XR20     CLC   RINVKEY(RINVKSRC-RINVKEY),KEYSAVE                                
         BNE   XRXIT               CHANGE OF INVENTORY NUMBER                   
*                                                                               
         CLI   RINVKSRC,X'FF'                                                   
         BE    XRXIT               STOP AT BEGINNING OF TEXT                    
*                                                                               
         CLI   RINVKSRC,0                                                       
         BE    XR100               SKIP THE HEADER                              
*                                                                               
*        FILTER ON BOOK                                                         
*                                                                               
         CLC   RINVKSRC,GSOKSRC    MATCH ON KSRC                                
         BNE   XR100                                                            
*                                                                               
         OC    FBOOK,FBOOK         OKAY IF NO BOOK SPECIFIED                    
         BZ    XR39                                                             
*                                                                               
         CLC   RINVKBK,FBKYM       SKIP IF BOOK DOESN'T MATCH FILTER            
         BNE   XR100                                                            
*                                                                               
XR39     DS    0H                                                               
*                                                                               
****************************************************************                
*              ADD TEXT TO THIS INVENTORY                      *                
****************************************************************                
         SPACE 1                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
         BNE   XR40                                                             
         USING RINVCEL,R6                                                       
         CLC   RINVCTXT,NUMBER                                                  
         BE    XR100               ALREADY THERE                                
         MVC   RINVCTXT,NUMBER                                                  
         B     XR50                                                             
         SPACE 1                                                                
XR40     XC    ELEM,ELEM           NO CD ELEMENT - ADD ONE                      
         LA    R6,ELEM                                                          
         MVC   RINVCCOD(2),=X'CD0A'                                             
         MVC   RINVCTXT,NUMBER                                                  
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
XR50     GOTO1 PUTREC                                                           
         SPACE 1                                                                
XR100    GOTO1 SEQ                 NEXT INVENTORY RECORD                        
         B     XR20                                                             
*                                                                               
XRXIT    MVC   AIO,AIO1            RESTORE IO AREA                              
         XIT1                                                                   
*                                                                               
XRECERR  DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
**********************************************************************          
*   LINUP INTERFACE                                                  *          
*     - BUILD LUBLK, CREATE ELEMENT TABLE, CALL LINUP                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
         DS    0D                                                               
LINSET   NMOD1 0,**LINSET                                                       
*                                                                               
         LA    RC,SPOOLEND         POINT TO WORKING STORAGE                     
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
         LA    R5,LUBLK            POINT TO  LINUP CONTROL BLOCK                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         XC    LUBLKD(LUBLKL),LUBLKD   CLEAR LINUP CONTROL BLOCK                
*                                                                               
         MVC   LUNEW,NEWKEY        SET NEW OR OLD RECORD INDICATOR              
*                                                                               
         MVC   LUATWA,ATWA         PASS A(TWA)                                  
         L     R1,SYSPARMS         GET A(TIOB)                                  
         L     R1,0(R1)                                                         
         ST    R1,LUATIOB                                                       
*                                                                               
         MVI   LUNLINS,NLINS       SET NUMBER OF LINES ON SCREEN                
         MVI   LUNFLDS,2               FIELDS PER LINE                          
*                                                                               
*                                  BUILD LIST OF FIELD DISPLACEMENTS            
*                                                                               
         LA    R3,LINDSPS          POINT TO LIST OF DISPLACEMENTS               
         ST    R3,LUADSPS          A(LIST OF DISPLACEMENTS)                     
*                                                                               
         LA    R2,TXTTXT1H         A(FIRST FIELD)                               
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
*                                                                               
LS04     DS    0H                                                               
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON NEXT LINE            
         S     RF,ATWA             GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
*                                                                               
         BAS   RE,BUMPU            BUMP TO START OF NEXT LINE                   
*                                                                               
         BCT   R4,LS04             ADD NEXT LINE TO LIST                        
*                                                                               
         XC    0(2,R3),0(R3)       FORCE NULLS AT END OF LIST                   
*                                                                               
         MVI   LUSCROLL,LUHALFQ    SCROLL FACTOR OF A HALF IS DEFAULT           
*                                                                               
         CLI   TXTSCRL,C'P'        CHECK FOR PAGE SCROLL                        
         BE    *+8                                                              
         CLI   TXTSCRL,X'97'         LOWERCASE 'P'                              
         BNE   *+8                                                              
         MVI   LUSCROLL,LUPAGEQ                                                 
*                                                                               
         CLI   TXTSCRL,C'H'        CHECK FOR HALF PAGE SCROLL                   
         BE    *+8                                                              
         CLI   TXTSCRL,X'88'            LOWERCASE 'H'                           
         BNE   *+8                                                              
         MVI   LUSCROLL,LUHALFQ                                                 
*                                                                               
         TM    TXTSCRLH+4,X'08'    SKIP IF NOT A NUMERIC FIELD                  
         BNO   LS051                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,TXTSCRLH+5     FIELD INPUT LENGTH                           
         BZ    LS051               NO ENTRY                                     
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,TXTSCRL(0)      CONVERT SCROLL AMOUNT TO NUMBER              
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,LUSCROLL         PASS SCROLL AMOUNT                           
*                                                                               
LS051    DS    0H                                                               
*                                                                               
         CLI   PFAID,19            CHECK FOR UP KEY                             
         BE    *+8                                                              
         CLI   PFAID,7             CHECK FOR UP KEY                             
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFUPQ                                                  
*                                                                               
         CLI   PFAID,20            CHECK FOR DOWN KEY                           
         BE    *+8                                                              
         CLI   PFAID,8             CHECK FOR DOWN KEY                           
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFDNQ                                                  
*                                                                               
         CLI   MODE,VALREC         SET LINUP MODE                               
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPVALQ   VALIDATE                                     
*                                                                               
         CLI   MODE,DISPREC        SET LINUP MODE - DISPREC                     
         BE    *+8                                                              
         CLI   MODE,XRECADD        RE-DISPLAY AFTER ADD                         
         BE    *+8                                                              
         CLI   MODE,XRECPUT        RE-DISPLAY AFTER CHANGE                      
         BE    *+8                                                              
         CLI   MODE,XRECDEL        RE-DISPLAY AFTER DELETE                      
         BE    *+8                                                              
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    *+8                                                              
         CLI   MODE,XRECREST       RE-DISPLAY AFTER RESTORE                     
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPDSPQ   DISPLAY                                      
*                                                                               
         MVI   LUCNTL,LUBACKQ      WINDOW SUPPORTS BACKWARD SCROLLING           
*                                                                               
         TM    IPSTAT,LUWCURSQ     CHECK IF CURSOR FOUND YET                    
         BNO   *+8                 NO - NO CURSOR SELECT                        
         OI    LUCNTL,LUCURSQ      YES - MAKE CURSOR SENSITIVE                  
*                                                                               
         LA    RF,LINHOOK          PROCESSING ROUTINE                           
         ST    RF,LUHOOK                                                        
*                                                                               
         MVC   LUSVLEN,=AL2(LSVTABL) SAVED BYTES PER LINE                       
*                                                                               
         LA    RF,LSVTAB           LINUP SAVE AREA                              
         ST    RF,LUSVTAB                                                       
*                                                                               
         XC    SVELTKEY,SVELTKEY   INIT ELEMENT KEY SAVEAREA                    
*                                                                               
*              BUILD TABLE OF ELEMENTS                                          
*                                                                               
         BAS   RE,LSBLDTAB            BUILD ELEM TABLE                          
*                                                                               
         CLI   MODE,VALREC         IF VALIDATING RECORD                         
         BNE   LSPFKX                                                           
*                                     ANALYZE PFKEYS                            
         CLI   PFAID,0             IF PFKEY ENTERED                             
         BE    LSPFKX                                                           
*                                                                               
         BAS   RE,PFKEYS              GO ANALYZE                                
         BE    LSPFKX                 NO ERRORS                                 
*                                                                               
         GOTO1 ERREX                  CHECK FOR ERRORS                          
*                                                                               
LSPFKX   DS    0H                                                               
*                                                                               
*                                     FIRST LINE UP CALL                        
*                                     ------------------                        
*                                                                               
         MVI   SVDIR,0             INIT DIRECTION SAVEAREA                      
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD     LINUP                                     
*                                                                               
         OC    IPSTAT,LUWSTAT      OR IN WINDOW INPUT STATUS                    
*                                                                               
         CLI   LUAPMODE,LUAPVALQ      TEST VALIDATING                           
         BNE   LSMOR                                                            
*                                                                               
         TM    IPSTAT,LUWVERRQ     CONTINUE IF NO ERRORS                        
         BNO   LS22                                                             
*                                  ELSE                                         
         MVC   LSVTAB,SVLSVTAB     RESTORE LINUP SAVE TABLE                     
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LSX                 AND DON'T UPDATE ELEMS                       
*                                                                               
LS22     DS    0H                                                               
*                                                                               
*        ALWAYS UPDATE THE RECORD                                               
*                                                                               
****     TM    IPSTAT,LUSNPVQ      UPDATE RECORD IF THERE WAS AT LEAST          
****     BNO   *+8                 ONE NON-PREVIOUSLY VALIDATED FIELD           
****     TM    IPSTAT,LUSDATQ      AND DATA ENTERED IN SOME FIELD               
****     BNO   LS23                                                             
*                                                                               
         GOTO1 =A(LSWRTTAB),RR=RELO  WRITES CHANGES TO RECORD                   
*                                                                               
LS23     DS    0H                                                               
*                                                                               
         TM    IPSTAT,LUSNPVQ      IF ALL PREVIOUSLY VALIDATED                  
         BO    LSNCHA                                                           
         CLI   LUAPMODE,LUAPVALQ   AND VALIDATING                               
         BNE   LSNCHA              THEN WANT TO RE-DISPLAY IN CASE OF           
*                                    NEED TO SCROLL                             
         TM    LUSTAT,LUCLEARQ     SKIP IF CLEAR COMMAND ISSUED                 
         BO    LSNCHA                                                           
*                                    NEED TO SCROLL                             
         MVI   LUAPMODE,LUAPDSPQ     SET FOR DISPLAY                            
         MVI   MODE,DISPREC          SET FOR DISPLAY RECORD                     
         MVI   LUWSTAT,0             RESET WINDOW STAT                          
*                                                                               
         MVI   SVDIR,0               INIT DIRECTION SAVEAREA                    
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD    SCROLL IF NEEDED                           
*                                                                               
         B     LSMOR                                                            
*                                                                               
LSNCHA   DS    0X                                                               
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
LSMOR    DS    0X                  SET 'MORE' FIELDS                            
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOW                 CLEARED - LEAVE UPPER AS IS                
*                                                                               
         CLI   LUAPMODE,LUAPDSPQ   ONLY IF IN DISPLAY MODE                      
         BNE   LSMORX                                                           
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,TXTMOR1H         POINT TO FIRST MORE FIELD                    
*                                                                               
         L     R3,LUSVTAB          POINT TO FIRST SAVED ENTRY                   
         USING LSVTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST TABLE ENTRY                   
         USING ELTABD,R4           ESTABLISH ENTRY                              
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT START AT START            
         BNH   *+10                OF TABLE THEN SET UP INDICATOR               
         MVC   FLD(2),=C'<<'                                                    
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSLOW    DS    0H                                                               
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,TXTMORLH         POINT TO LAST MORE FIELD                     
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOWOUT              CLEARED                                    
*                                                                               
         ZIC   RF,LUNLINS          GET NUMBER OF LINES ON SCREEN                
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MH    RF,=Y(LSVTABL)      GET INDEX                                    
         AR    R3,RF               POINT TO LAST ELEMENT IN TABLE               
         L     R4,ELTLAST          POINT TO LAST ELEMENT IN TABLE               
*                                                                               
         OC    LSVKEY,LSVKEY       NULLS INDICATE END OF TABLE ALREADY          
         BZ    LSLOWOUT            ON SCREEN                                    
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT END AT END                
         BNL   *+10                OF TABLE THEN SET DOWN INDICATOR             
         MVC   FLD(2),=C'>>'                                                    
*                                                                               
LSLOWOUT DS    0H                                                               
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSMORX   DS    0X                                                               
*                                                                               
LSX      DS    0H                                                               
*                                                                               
         CLI   LUERR,0             RESET CC                                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                  LINES IN WINDOW                              
NLINS    EQU   ((TXTTXTLH-TXTTXT1H)/(TXTTXT2H-TXTTXT1H))+1                      
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP PROCESSING HOOK                                  *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LINHOOK  NTR1                                                                   
         CLI   LUMODE,LUVALQ       VALIDATE                                     
         BE    LHVAL                                                            
         CLI   LUMODE,LUDSPQ       DISPLAY                                      
         BE    LHDIS                                                            
         CLI   LUMODE,LUMOREQ      MORE TO DISPLAY                              
         BE    LHMORE                                                           
         DC    H'0'                INVALID MODE                                 
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP VALIDATION HOOK ROUTINE                          *          
**********************************************************************          
         SPACE 2                                                                
LHVAL    DS    0H                                                               
*                                                                               
*              IF FIRST INPUT FIELD HAS '++' THEN USER WANTS TO CLEAR           
*              OUT WINDOW FROM THIS POINT ON                                    
*                                                                               
         L     R2,LUACLIN          TEST FOR SCREEN CLEAR REQUEST                
*                                                                               
         CLC   8(2,R2),=C'++'      CHECK FOR CLEARING INDICATOR                 
         BNE   LHV04                                                            
*                                                                               
         NI    LULSTAT,X'FF'-LUSNPVQ  TURN OFF NOT VALIDATED INDICATOR          
         OI    LUSTAT,LUCLEARQ     TELL LINUP TO CLEAR FROM HERE ON             
*                                                                               
         OC    ACURFORC,ACURFORC   SET CURSOR IF IT IS NOT SET ALREADY          
         BNZ   *+8                                                              
         ST    R2,ACURFORC         FORCE CURSOR TO THIS FIELD                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LHVALX                                                           
*                                                                               
LHV04    DS    0H                                                               
*                                                                               
         BAS   RE,LHVALLIN         YES, VALIDATE LINE AND ADD TO TABLE          
*        BNE   LHVALX              IF ERROR DONT DELETE OLD AND DONT            
*                                  CLEAR SAVE TABLE ENTRY                       
         EJECT                                                                  
LHV06    DS    0H                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED TABLE ENTRY           
         USING LSVTABD,R2          ESTABLISH ENTRY                              
*                                                                               
         OC    LSVKEY,LSVKEY       WAS ANYTHING THERE BEFORE                    
         BZ    LHV10               NO                                           
*                                                                               
         MVC   HALF,LSVKEY                                                      
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                  YES, MARK OLD ENTRY DELETED                  
*                                  NOTE- ADD+DEL=CHANGE, THIS ENTRY             
*                                        MAY BE SAME AS ABOVE                   
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',HALF),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BE    LHV10                                                            
*                                                                               
****     BNE   *+6                                                              
****     DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          POINT TO FOUND ELEMENT                       
         USING ELTABD,RF           ESTABLISH ELEMENT                            
*                                                                               
         OI    ELTCTL,ELTDELQ      SET ENTRY DELETED                            
         DROP  RF                                                               
*                                                                               
LHV10    DS    0H                  SET NEW SAVE TABLE ENTRY                     
*                                                                               
         XC    LSVKEY,LSVKEY       INIT SAVE TABLE ENTRY                        
         XC    LSVKEYNW,LSVKEYNW   INIT SAVE TABLE ENTRY                        
         ICM   RF,15,ELTENT        GET ADDRESS OF ELEMENT                       
         BZ    *+16                PUT IN TABLE IF FOUND                        
         MVC   LSVKEY,ELTKEY-ELTABD(RF)                                         
         MVC   LSVKEYNW,ELTKEYNW-ELTABD(RF)                                     
*                                                                               
LHVALX   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP DISPLAY HOOK ROUTINE                             *          
**********************************************************************          
         SPACE 2                                                                
LHDIS    DS    0H                                                               
         MVI   LUSTAT,0            INIT STATUS                                  
         L     R4,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,R4                                                       
         XC    0(LSVTABL,R4),0(R4)  CLEAR IT                                    
*                                                                               
         BAS   RE,LHSRCH           FIND ELEM TO DISPLAY                         
         BAS   RE,LHDISLIN         BUILD SCREEN LINE                            
*                                                                               
LHDISX   DS    0H                                                               
         B     LHOOKX                                                           
         EJECT                                                                  
*                                  DO 'MORE' MESSAGE                            
*                                  -----------------                            
LHMORE   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
LHOOKX   DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         TITLE 'PRSFM0C - BUILD TABLE OF ELEMENTS ON FILE'                      
***********************************************************************         
*                                                                     *         
* ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LSBLDTAB NTR1                                                                   
         XC    BSPPRMS(BSPPRML),BSPPRMS INIT BINSRCH PARAMETERS                 
         L     R1,AIO2             STORE TABLE IN I/O2 & I/O3                   
         ST    R1,BSPATAB          PASS TABLE ADDRESS                           
         LA    R1,ELTABL           PASS ENTRY LENGTH                            
         ST    R1,BSPLENR                                                       
         LA    R1,ELTKEYL          PASS KEY LENGTH                              
         ST    R1,BSPLENK                                                       
         MVI   BSPKEYD,0           PASS KEY DISPLACEMENT                        
         LA    R1,ELTMAX           PASS MAXIMUM COUNT                           
         ST    R1,BSPMAX                                                        
*                                                                               
*        INITIALIZE TABLE AREA                                                  
*                                                                               
         LA    R0,ELTMAX           MAX ENTRIES IN TABLE                         
         L     R1,AIO2             TABLE AREA                                   
         LA    RF,ELTABL           LENGTH OF ONE ENTRY IN TABLE                 
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)       CLEAR ENTRY                                  
         LA    R1,1(RF,R1)         NEXT ENTRY                                   
         BCT   R0,*-18                                                          
*                                                                               
         LA    R4,WRKELTAB         POINT TO ELTAB WORK ELEMENT                  
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RINVTEL,R6                                                       
         B     LSBT20                                                           
*                                                                               
LSBT10   BAS   RE,NEXTEL                                                        
*                                                                               
LSBT20   BNE   LSBTX               NO ELEMENTS LEFT                             
*                                                                               
         XC    ELTABD(ELTABL),ELTABD INIT ENTRY                                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RINVTLEN         ELEMENT LENGTH                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELTELEM(0),RINVTEL  ADD ELEMENT TO TABLE                         
*                                                                               
         MVC   ELTSORT(1),RINVTLIN SET KEY                                      
*                                                                               
LSBT30   DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',WRKELTAB),RR=RELO                  
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FIT INTO TABLE               
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
         B     LSBT10                                                           
*                                                                               
LSBTX    DS    0H                                                               
*                                                                               
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,R0               MINUS ONE                                    
         MH    R1,=Y(ELTABL)       TIMES ENTRY LENGTH                           
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
*                                                                               
         XIT1                                                                   
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO SEARCH TABLE FOR ELEMENT                                 *         
* AND SET ADDRESS IN ELTENT                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHSRCH   NTR1                                                                   
         XC    ELTENT,ELTENT       INIT ELEMENT ADDRESS RETURN                  
         NI    LUSTAT,255-LUEOLQ   SET OFF END OF LIST INDICATOR                
*                                                                               
         OC    BSPNOR,BSPNOR       IF NO ENTRIES                                
         BZ    LHSRCHX             RETURN EMPTY-HANDED                          
*                                                                               
         L     R4,BSPATAB          DEFAULT TO FIRST ENTRY IN TABLE              
         USING ELTABD,R4                                                        
*                                                                               
         L     R3,LUAPTAB          A(PREVIOUS SAVE TABLE)                       
         USING LSVTABD,R3                                                       
*                                                                               
         MVC   HALF,LSVKEY         COPY KEY                                     
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                                                               
         OC    HALF,HALF           NO PREVIOUS ENTRY MEANS FIRST TIME           
         BNZ   LHSRCH02            OR SCROLLING FROM A NON-FILLED               
*                                  SCREEN - USE DEFAULT                         
*                                                                               
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         BNE   LHSRCH11               AFTER AN UP SCROLL                        
*                                                                               
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         B     LHSRCHX                                                          
*                                                                               
LHSRCH02 DS    0H                                                               
*                                                                               
         CLC   HALF,HIVALS         IF PREVIOUS IS X'FF'S                        
         BNE   LHSRCH05                                                         
*                                                                               
         L     RE,BSPNOR              USE LAST ENTRY IN TABLE                   
******   BCTR  RE,0                   DECREMENT FOR INDEXING                    
         L     RF,BSPLENR             RECORD LENGTH                             
         MR    RE,RE                  INDEX TO LAST ENTRY                       
         LA    R4,0(RF,R4)            A(LAST TABLE ENTRY)                       
         B     LHSRCH10                                                         
*                                                                               
LHSRCH05 DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',HALF),RR=RELO                     
*                                                                               
         L     R4,BSPAREC          GET TABLE ENTRY ADDRESS                      
*                                                                               
         CLI   BSPCNTL,BSPNF       DEFAULT TO FIRST OF TABLE IF                 
         BNE   LHSRCH10            PREVIOUS ENTRY WAS NOT FOUND                 
*                                                                               
         L     R4,BSPATAB          NO, POINT TO FIRST-(END OF TABLE)            
         B     LHSRCH11            DONE (NO MOVEMENT)                           
         EJECT                                                                  
LHSRCH10 DS    0H                                                               
         CLI   LUDIR,C'-'          CHECK FOR BACKWARD SCROLLING                 
         BE    LHSRCH16                                                         
*                                                                               
         CLI   LUDIR,C'='          SKIP BUMPING ENTRY IF RE-DISPLAYING          
         BE    *+8                                                              
         LA    R4,ELTABL(R4)       BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
LHSRCH11 DS    0H                                                               
*                                                                               
         C     R4,ELTLAST          DONE IF NOT AT END OF TABLE                  
         BL    LHSRCH30                                                         
         BE    LHSRCH12            AT END OF TABLE TELL LINUP                   
*                                  PAST END - ONLY IF SCROLLING DOWN            
*                                  AND PRIOR SCREEN ENDED WITH LAST             
*                                  ELEMENT IN TABLE - USE DEFAULT               
*                                  OR UP SCROLLING AND TABLE HAS ONLY           
*                                  ONE ELEMENT - STOP WITH NO DISPLAY           
         CLI   SVDIR,C'-'          IF FILLING BOTTOM OF A SCREEN                
         BNE   *+12                AFTER AN UP SCROLL                           
         OI    LUSTAT,LUEOLQ       EXIT WITHOUT FINDING ELEMENT                 
         B     LHSRCHX                                                          
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST ENTRY IN TABLE                
         CLC   BSPNOR,=F'1'        DONE IF MORE THAN ONE ENTRY IN TABLE         
         BH    LHSRCH30                                                         
*                                  EXIT WITHOUT FINDING ELEMENT                 
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCHX                                                          
*                                                                               
LHSRCH12 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCH30                                                         
         EJECT                                                                  
LHSRCH16 DS    0H                  GOING BACKWARDS                              
         C     R4,BSPATAB          IF AT START                                  
         BNH   LHSRCH18            DONT GO FURTHER                              
         SH    R4,=Y(ELTABL)       BACK UP AN ENTRY                             
LHSRCH17 DS    0H                                                               
         C     R4,BSPATAB          IF AT START                                  
         BH    *+8                                                              
LHSRCH18 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
*                                                                               
LHSRCH30 DS    0H                                                               
         TM    ELTCTL,ELTDELQ+ELTADDQ DISPLAY CHANGED ELEMENTS                  
         BO    LHSRCH40                                                         
         TM    ELTCTL,ELTDELQ      BYPASS DELETED ELEMENTS                      
         BNO   LHSRCH40                                                         
         TM    LUSTAT,LUEOLQ       EXIT IF AT END OF LIST                       
         BO    LHSRCHX                                                          
         CLI   LUDIR,C'='          QUIT IF RE-DISPLAY                           
         BE    LHSRCH40                                                         
         B     LHSRCH10            ELSE GO FIND NEXT ELEMENT                    
*                                                                               
LHSRCH40 DS    0H                                                               
         ST    R4,ELTENT           RETURN TABLE ENTRY ADDRESS                   
         L     R3,LUACTAB          POINT TO CURRENT SAVE TABLE ENTRY            
         MVC   LSVKEY,ELTKEY       SAVE APPROPRIATE DATA                        
         MVC   LSVKEYNW,ELTKEYNW   SAVE APPROPRIATE DATA                        
*                                                                               
LHSRCHX  DS    0H                                                               
         MVC   SVDIR,LUDIR         SAVE LAST TIME DIRECTION                     
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         DROP  R4                                                               
*                                                                               
HIVALS   DC    32X'FF'             HIGH VALUES                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO VALIDATE WINDOW LINE                                     *         
* BUILD ENTRY IN WORK AND ADD TO ELEM TABLE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHVALLIN NTR1                                                                   
*                                                                               
         XC    ELTENT,ELTENT       CLEAR A(ENTRY)                               
*******                                                                         
*******  TM    LULSTAT,LUSNPVQ     SKIP IF ALL FIELDS PREVIOUSLY                
*******  BO    *+8                   VALIDATED                                  
*******  TM    LULSTAT,LUSDATQ     AND NO DATA IN FIELDS ON THIS LINE           
*******  BZ    LHVLX                                                            
*                                                                               
         XC    WRKELTAB,WRKELTAB   INIT WORK AREA                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
         USING ELTABD,R4           IN WORK AREA                                 
*                                                                               
         LA    R3,ELTELEM          INITIALIZE ELEMENT                           
         USING RINVTEL,R3          ESTABLISH TEXT ELEMENT                       
*                                                                               
         SLR   R4,R4               INIT TABLE ELEMENT POINTER                   
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE                 
*                                                                               
LHVCLRN  DS    0H                                                               
*                                                                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
*                                                                               
         MVI   RINVTCOD,X'01'      ELEMENT CODE                                 
         MVI   RINVTLEN,RINVTEXT-RINVTEL  MINIMUM ELEMENT LENGTH                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          ENTERED TEXT LENGTH                          
         BZ    *+6                                                              
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RINVTEXT(0),8(R2)   SAVE ENTERED TEXT                            
*                                                                               
         LA    RE,RINVTEXT(RF)     POINT TO LAST BYTE                           
         LA    RF,1(RF)            RECIFY COUNT                                 
*                                                                               
         CLI   0(RE),C' '          STRIP TRAILING SPACES                        
         BH    LHVSTRP1                                                         
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
*                                                                               
         C     R2,LINLAST          DROP IF PAST LAST TEXT LINE                  
         BL    *+10                                                             
         SR    R4,R4                                                            
         B     LHVTXTOK                                                         
*                                  ELSE                                         
         LA    RF,1                KEEP ONE BYTE FOR LINE SPACING               
*                                                                               
LHVSTRP1 DS    0H                                                               
*                                                                               
         LA    RF,RINVTEXT-RINVTEL(RF)   ELEMENT LENGTH                         
         STC   RF,RINVTLEN         SET ELEMENT LENGTH                           
*                                                                               
         L     RF,LUACTAB          POINT TO CURRENT SAVED ELEMENT               
         USING LSVTABD,RF                                                       
*                                                                               
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+8                                                              
         LA    RF,LSVKEYNW            USE IT                                    
*                                                                               
         OC    LSVKEY,LSVKEY       IF THERE WAS NOTHING LAST TIME               
         BNZ   LHVTLINX                                                         
*                                                                               
         B     LHVSTRP2                                                         
*                                                                               
*        DEACTIVATED                                                            
*                                                                               
         ICM   RF,15,LUAPTAB          POINT TO PREVIOUS TABLE ENTRY             
         BZ    LHVSTRP2               NONE AVAILABLE                            
*                                                                               
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+8                                                              
         LA    RF,LSVKEYNW            USE IT                                    
*                                                                               
         B     LHVTLINX                  PREVIOUS ENTRY                         
*                                                                               
LHVSTRP2 DS    0H                                                               
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SVELTKEY+1         BUMP SUB LINE NUMBER                       
         LA    R1,1(R1)               OF LAST USED ELEMENT KEY                  
         STC   R1,SVELTKEY+1                                                    
*                                                                               
         LA    RF,SVELTKEY            POINT TO LAST USED KEY                    
*                                                                               
LHVTLINX DS    0H                                                               
*                                                                               
         MVC   ELTKEY,LSVKEY       SET SORT KEY TO LINE NUMBER                  
*                                                                               
         MVC   SVELTKEY,ELTKEY     SAVE CURRENT KEY                             
*                                                                               
         DROP  RF                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      ADD ELEMENT                                  
*                                                                               
LHVTXTOK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
LHVCMP   DS    0H                                                               
*                                                                               
         LTR   R4,R4               DONE IF NOT BUILDING AN ELEMENT              
         BZ    LHVLX                                                            
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE CURRENT MESSAGE NUMBER                  
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',ELTABD),RR=RELO                    
*                                                                               
         OC    1(3,R1),1(R1)       TEST ROOM                                    
         BZ    LHVCMPE1                                                         
*                                                                               
         L     R4,BSPAREC          POINT TO FOUND TABLE ELEMENT                 
*                                                                               
         STCM  R4,7,ELTENT+1                                                    
*                                                                               
         CLI   BSPCNTL,BSPNF       TEST IF NO MATCH FOUND                       
         BE    LHVL92              YES - NEW ENTRY FOR TABLE                    
*                                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED ELEMENT               
         USING LSVTABD,R2                                                       
*                                                                               
         CLC   LSVKEY,ELTKEY       ALSO OK IF ENTRY KEY NOT CHANGED             
         BE    *+10                                                             
         CLC   LSVKEYNW,ELTKEY     ALSO OK IF ENTRY KEY NOT CHANGED             
         BE    *+10                                                             
         CLC   LSVKEYNW,ELTKEYNW   ALSO OK IF ENTRY KEY NOT CHANGED             
         BNE   LHVCMPE2                                                         
*                                                                               
LHVL92   DS    0H                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      SET ADDED (NB- ADD+DEL=CHA)                  
         MVC   ELTELEM,RINVTEL     SET NEW ELEM IN TABLE                        
*                                  SET NEW ELTLAST                              
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MH    R1,=Y(ELTABL)                                                    
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
         B     LHVLX                                                            
*                                                                               
         EJECT                                                                  
LHVCMPE1 DS    0H                                                               
         MVC   RERROR,=AL2(RECFULL) TOO MANY DETAIL LINES                       
         B     LHVCMPER                                                         
*                                                                               
LHVCMPE2 DS    0H                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE                 
         MVC   RERROR,=AL2(DUPEDATA)   DUPLICATE                                
         B     LHVCMPER                                                         
*                                                                               
LHVCMPER DS    0H                                                               
*                                                                               
         NI    4(R2),X'FF'-X'20'   TURN OFF VALIDATED STATUS                    
         GOTO1 MYERROR             HANDLE FIELD IN ERROR                        
*                                                                               
LHVLX    DS    0H                                                               
         CLI   ERROR,0             SET CC                                       
         XIT1                                                                   
         DROP  R4                                                               
         DROP  R3                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD WINDOW LINE                                 *         
*        FROM TABLE ENTRY IN ELTENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
LHDISLIN NTR1                                                                   
*                                                                               
         L     R2,LUACLIN          A(FIRST FIELD)                               
*                                                                               
*****    MVC   FLD,SPACES          DEFAULT TO CLEARING                          
         XC    FLD,FLD             DEFAULT TO CLEARING                          
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         BZ    LHDTXTX             CHECK IF NONE FOUND                          
*                                                                               
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING RINVTEL,R3                                                       
*                                                                               
*              DISPLAY TEXT                                                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,RINVTLEN       ELEMENT LENGTH                               
         SH    RF,=Y(RINVTEXT-RINVTEL)   TEXT LENGTH                            
         BNP   LHDTXTX             NOTHING TO DISPLAY                           
         STC   RF,5(R2)            SET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),RINVTEXT     MOVE LINE OF TEXT TO SCREEN                  
*                                                                               
LHDTXTX  DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
         LR    R1,R2                                                            
         BAS   RE,DSPFLD           DISPLAY FIELD                                
*                                                                               
LHDLX    DS    0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        ANALYZE PFKEYS                                               *         
*              PF4  - INSERT A LINE                                   *         
*              PF6  - DELETE A LINE                                   *         
*              PF2  - SPLIT  A LINE                                   *         
*              PF3  - JOIN     LINES                                  *         
*                                                                     *         
*NTRY    R5 ==>  LINUP CONTROL BLOCK                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYS   NTR1                                                                   
*                                                                               
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
*                                                                               
*****    L     R3,SYSPARMS                                                      
         ICM   R3,15,ATIOB         POINT TO TIOB                                
         USING TIOBD,R3                                                         
*                                                                               
         CLC   CONACT(3),=C'ADD'   IF ACTION 'ADD'                              
         BNE   PFKEYS1                                                          
*                                                                               
         FOUT  CONACTH,=C'CHA'        CHANGE TO 'CHANGE'                        
*                                                                               
PFKEYS1  DS    0H                                                               
*                                                                               
         LA    R6,LNTBL            POINT TO START OF TABLE                      
         LA    R4,LSVTAB           POINT TO START OF LINUP SAVEAREA             
         USING LSVTABD,R4          ESTABLISH LINUP SAVEAREA ENTRY               
*                                                                               
*        FIND LINE WITH CURSOR                                                  
*                                                                               
         CLC   TIOBCURD,0(R6)      MUST BE AFTER START OF LINE                  
         BL    PFKEYSX                                                          
         CLC   TIOBCURD,2(R6)      AND BEFORE START OF NEXT LINE                
         BL    *+16                                                             
         LA    R6,2(R6)            BUMP TO NEXT TABLE ENTRY                     
         LA    R4,LSVTABL(R4)      BUMP TO NEXT SAVEAREA                        
         B     *-28                                                             
*                                                                               
         MVC   ALINCUR,0(R6)       SAVE A(LINE WITH CURSOR)                     
*                                                                               
         DROP  R5                                                               
*                                                                               
         CLI   TIOBAID,4           PF4 OR PF16                                  
         BE    *+8                                                              
         CLI   TIOBAID,16                                                       
         BE    LNADD                  ADD A LINE                                
*                                                                               
         CLI   TIOBAID,6           PF6 OR PF18                                  
         BE    *+8                                                              
         CLI   TIOBAID,18                                                       
         BE    LNDEL                  DELETE A LINE                             
*                                                                               
         CLI   TIOBAID,2           PF2 OR PF14                                  
         BE    *+8                                                              
         CLI   TIOBAID,14                                                       
         BE    LNADD                  SPLIT  LINE                               
*                                                                               
         CLI   TIOBAID,3           PF3 OR PF15                                  
         BE    *+8                                                              
         CLI   TIOBAID,15                                                       
         BE    LNJOIN                 JOIN   LINE                               
*                                                                               
         B     PFKEYSX             IGNORE ALL OTHER KEYS                        
*                                                                               
         TITLE 'RERMP05 - TEXT RECORD - LNADD'                                  
***********************************************************************         
*                                                                     *         
*        ADD OR SPLIT A LINE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNADD    DS    0H                  ADD LINE BEFORE CURSOR LINE                  
*                                                                               
         LA    R4,NLINS            NUMBER OF LINES ON SCREEN                    
         BCTR  R4,0                DECREMENT FOR INDEXING                       
         BCTR  R4,0                DECREMENT FOR NEXT TO LAST ENTRY             
         MH    R4,=Y(LSVTABL)      DISP  TO NEXT TO LAST IN LINUP SAVE          
         LA    R4,LSVTAB(R4)       POINT TO NEXT TO LAST IN LINUP SAVE          
*                                                                               
         LA    R6,LNTBLLS          POINT TO LAST ENTRY IN TABLE                 
         LR    R5,R6                                                            
         SH    R5,=H'2'            BACK UP A TABLE ENTRY                        
         SR    R1,R1                                                            
*                                                                               
LNADDLP  DS    0H                                                               
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R5)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         LA    R0,1                NUMBER OF FIELDS ON A LINE                   
         SR    R2,R2                                                            
*                                                                               
LNADDLP1 DS    0H                                                               
*                                                                               
         IC    R2,0(RF)            GET LENGTH OF RECEIVING FIELD                
         SH    R2,=H'8'            ALLOW FOR HEADER                             
         TM    1(RF),X'02'         IF THERE IS EXTENDED HEADER                  
         BNO   *+8                                                              
         SH    R2,=H'8'               TAKE OUT ITS LENGTH                       
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),8(RE)       COPY LINE DOWN ONE                           
*                                                                               
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNADDCN1 DS    0H                                                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNADDLP1                                                      
*                                                                               
LNADDDN1 DS    0H                                                               
*                                                                               
         MVC   LSVTABD+LSVTABL(LSVTABL),LSVTABD  COPY LINUP SAVE                
*                                                                               
LNADDCN  DS    0H                                                               
*                                                                               
         SH    R4,=Y(LSVTABL)      BACK UP LINUP SAVE ENTRY                     
*                                                                               
         LR    R6,R5               BACK UP ENTRIES IN TABLE                     
         SH    R5,=H'2'                                                         
*                                                                               
         CLC   0(2,R5),ALINCUR     STOP IF PASSED LINE WITH CURSOR              
         BNL   LNADDLP                                                          
*                                                                               
LNADDDN  DS    0H                                                               
*                                                                               
         AH    R4,=Y(LSVTABL)      POINT TO DATA FOR CURSOR LINE                
*                                                                               
*        SET SAVED KEYS FOR NEW AND OLD LINE                                    
*                                                                               
         MVC   HALF,LSVKEY         DEFAULT IS CURRENT LINE NUMBER               
*                                                                               
         OC    LSVKEYNW,LSVKEYNW   USE NEW KEY IF PRESENT                       
         BZ    *+10                                                             
         MVC   HALF,LSVKEYNW                                                    
*                                                                               
         LA    R4,LSVTABL(R4)      POINT TO NEXT ENTRY IN LSVTAB                
*                                                                               
         OC    HALF,HALF           SKIP IF NO OLD LINE NUMBER EXISTS            
         BZ    *+18                                                             
         SR    RF,RF                                                            
         IC    RF,HALF+1                                                        
         LA    RF,1(RF)                                                         
         STC   RF,HALF+1           UPDATE SAVED KEY                             
*                                                                               
         MVC   LSVKEYNW,HALF                                                    
*                                                                               
         LH    R6,0(R6)            POINT TO FIRST OF CURSOR LINE                
         LA    R6,0(R6,RA)                                                      
*                                                                               
         NI    4(R6),X'FF'-X'20'   SET AS NOT VALIDATED                         
*                                                                               
         CLI   TIOBAID,2           SKIP IF SPLITTING LINE                       
         BE    *+8                                                              
         CLI   TIOBAID,14                                                       
         BE    LNADDSP                                                          
*                                                                               
         ST    R6,ACURFORC         PLACE CURSOR HERE                            
*                                                                               
         LA    R0,1                NUMBER OF FIELDS ON LINE                     
         SR    RF,RF                                                            
*                                                                               
LNADXCLP DS    0H                                                               
*                                                                               
         TWAXC (R6),(R6),PROT=Y    CLEAR FIELD ON SCREEN                        
         MVC   8(2,R6),SPACES      PUT A COUPLE OF SPACES OUT                   
*                                                                               
         MVI   5(R6),2             CLEAR INPUT LENGTH                           
         NI    4(R6),X'FF'-X'20'   SET AS NOT VALIDATED                         
         OI    6(R6),X'80'         TRANSMIT FIELD                               
         MVI   7(R6),2             INDICATE NO OUTPUT                           
*                                                                               
LNADXCCN DS    0H                                                               
*                                                                               
         IC    RF,0(R6)            FIELD LENGTH                                 
         LA    R6,0(RF,R6)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNADXCLP                                                      
*                                                                               
         B     LNADDX                                                           
*                                                                               
LNADDSP  DS    0H                                                               
*                                                                               
         OI    TIOBINDS,TIOBSETC    LEAVES CURSOR WHERE IT WAS                  
*                                                                               
         LA    RF,L'TXTTXT1-1+8(R6) POINT TO LAST BYTE OF FIELD                 
*                                                                               
         SR    RE,RE                                                            
*                                                                               
         IC    RE,TIOBCURI         DISPLACEMENT OF CURSOR INTO FLD              
         LA    RE,8(RE,R6)         CURSOR ADDRESS                               
*                                                                               
         CR    RF,RE               DONE IF CURSOR PASSED                        
         BL    *+12                                                             
         MVI   0(RF),C' '          CLEAR OUT END OF FIELD                       
         BCT   RF,*-10                                                          
*                                                                               
         MVC   5(1,R6),TIOBCURI    NEW FIELD LENGTH                             
*                                                                               
         CLI   5(R6),0             IF NO LENGTH                                 
         BNZ   *+14                                                             
         MVI   5(R6),2                DEFAULT TO 2 SPACES                       
         MVC   8(2,R6),SPACES         DEFAULT TO 2 SPACES                       
*                                                                               
         LA    RE,TXTTXT2H-TXTTXT1H(RE)    POINT TO CURSOR IN NEXT LINE         
         LA    RF,8+TXTTXT2H-TXTTXT1H(R6)  START OF NEXT FIELD                  
*                                                                               
         CR    RF,RE               CLEAR OUT TO CURSOR                          
         BNL   *+16                                                             
         MVI   0(RF),C' '                                                       
         LA    RF,1(RF)            NEXT POSITION                                
         B     *-14                                                             
*                                                                               
         NI    4(R6),X'FF'-X'20'   SET AS NOT VALIDATED                         
         OI    6(R6),X'80'         TRANSMIT SPLIT LINE - OTHERS DONE            
*                                                                               
LNADDX   DS    0H                                                               
*                                                                               
         BAS   RE,FNDLST           RESET LAST LINE ON SCREEN                    
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'RERMP05 - TEXT RECORD - LNDEL'                                  
***********************************************************************         
*                                                                     *         
*        DELETE A LINE                                                *         
*                                                                     *         
*NTRY    R6==> TABLE ENTRY FOR LINE WITH CURSOR                       *         
*        R4==> LINUP TABLE ENTRY FOR LINE WITH CURSOR                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNDEL    DS    0H                  DELETE LINE AT CURSOR                        
*                                                                               
         LH    R5,0(R6)            POINT TO FIRST FIELD OF CURSOR LINE          
         LA    R5,0(R5,RA)                                                      
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         TM    1(R5),X'20'         IF PROTECTED FIELD                           
         BNO   *+16                                                             
         IC    RF,0(R5)                                                         
         LA    R5,0(RF,R5)            BUMP TO NEXT FIELD                        
         B     *-16                                                             
*                                                                               
         ST    R5,ACURFORC         PLACE CURSOR HERE                            
*                                                                               
*        FIND DATA FOR THIS LINE                                                
*                                                                               
         OC    LSVTABD(LSVTABL),LSVTABD   SKIP IF NO LINE DISPLAYED             
         BZ    LNDEL1                                                           
*                                                                               
         MVC   HALF,LSVKEY                                                      
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',HALF),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BE    LNDEL1                                                           
*                                                                               
****     BNE   *+6                                                              
****     DC    H'0'                                                             
*                                                                               
         L     R1,BSPAREC          POINT TO FOUND ELEMENT                       
         OI    ELTCTL-ELTABD(R1),ELTDELQ   FLAG FOR DELETE                      
*                                                                               
         XC    LSVTABD(LSVTABL),LSVTABD   CLEAR TABLE ENTRY                     
*                                                                               
LNDEL1   DS    0H                  DELETE LINE AT CURSOR                        
*                                                                               
         LR    R5,R6                                                            
         AH    R5,=H'2'            POINT TO NEXT TABLE ENTRY                    
         SR    R1,R1                                                            
         SR    R2,R2                                                            
*                                                                               
LNDELLP  DS    0H                                                               
*                                                                               
         CLC   0(2,R5),=X'FFFF'    STOP IF PASSED END OF TABLE                  
         BNL   LNDELDN                                                          
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R5)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         LA    R0,1                NUMBER OF FIELDS ON A LINE                   
*                                                                               
LNDELLP1 DS    0H                                                               
*                                                                               
         IC    R2,0(RF)            GET LENGTH OF RECEIVING FIELD                
         SH    R2,=H'8'            ALLOW FOR HEADER                             
         TM    1(RF),X'02'         IF THERE IS EXTENDED HEADER                  
         BNO   *+8                                                              
         SH    R2,=H'8'               TAKE OUT ITS LENGTH                       
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),8(RE)       COPY LINE UP ONE                             
*                                                                               
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         NI    4(RF),X'FF'-X'20'   SET AS NOT VALIDATED                         
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNDELCN1 DS    0H                                                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNDELLP1                                                      
*                                                                               
LNDELDN1 DS    0H                                                               
*                                                                               
         MVC   LSVTABD(LSVTABL),LSVTABD+LSVTABL  COPY LINUP SAVEAREA            
*                                                                               
LNDELCN  DS    0H                                                               
*                                                                               
         LA    R4,LSVTABL(R4)      BUMP TO NEXT SAVEAREA                        
         LR    R6,R5               ADVANCE ENTRIES IN TABLE                     
         AH    R5,=H'2'                                                         
*                                                                               
         B     LNDELLP                                                          
*                                                                               
LNDELDN  DS    0H                                                               
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,LNTBLLS        POINT TO LAST LINE IN TABLE                  
         AR    R6,RA               POINT TO FIELD HEADER                        
*                                                                               
         LA    R0,1                NUMBER OF FIELDS ON LINE                     
         SR    R3,R3                                                            
*                                                                               
LNDLXCLP DS    0H                                                               
*                                                                               
         TWAXC (R6),(R6),PROT=Y    CLEAR FIELD ON SCREEN                        
         MVC   8(2,R6),SPACES                                                   
*                                                                               
         MVI   5(R6),2             CLEAR INPUT LENGTH                           
         NI    4(R6),X'FF'-X'20'   SET AS NOT VALIDATED                         
*                                                                               
*        FILL LAST LINE  WITH NEXT ITEM IN TABLE                                
*        R4 ==> SAVE ENTRY FOR LAST LINE OF TABLE                               
*                                                                               
         OC    LSVTABD(LSVTABL),LSVTABD SKIP IF NO ENTRY                        
         BZ    LNDLSTX                                                          
*                                                                               
         SR    RF,RF                                                            
         MVC   HALF,LSVKEY         COPY KEY                                     
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                                                               
         IC    RF,HALF+1           BUMP KEY BY 1                                
         LA    RF,1(RF)                                                         
         STC   RF,HALF+1           FIND NEXT ITEM IN TABLE                      
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',HALF),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MAY HAVE BEEN AT END OF TABLE                
         BE    LNDLSTX                                                          
*                                                                               
         L     R1,BSPAREC          POINT TO FOUND RECORD                        
         MVC   LSVKEY,ELTKEY-ELTABD(R1)       SET SAVED KEY                     
         MVC   LSVKEYNW,ELTKEYNW-ELTABD(R1)   SET SAVED KEY                     
*                                                                               
         LA    R1,ELTELEM-ELTABD(R1) ESTABLISH RECORD ELEMENT PART              
         USING RINVTEL,R1                                                       
*                                                                               
*              DISPLAY TEXT                                                     
*                                                                               
         MVC   FLD,SPACES          INIT DISPLAY AREA                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,RINVTLEN       ELEMENT LENGTH                               
         SH    RF,=Y(RINVTEXT-RINVTEL)   TEXT LENGTH                            
         BNP   LNDLST1             NOTHING TO DISPLAY                           
         STC   RF,5(R6)            SET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),RINVTEXT     MOVE LINE OF TEXT TO SCREEN                  
*                                                                               
LNDLST1  DS    0H                                                               
*                                                                               
         NI    4(R6),X'FF'-X'20'    INDICATE FIELD NOT VALIDATED                
*                                                                               
         LR    R1,R6                                                            
         BAS   RE,DSPFLD           DISPLAY FIELD                                
*                                                                               
LNDLSTX  DS    0H                                                               
*                                                                               
LNDLXCCN DS    0H                                                               
*                                                                               
         SR    R3,R3                                                            
         IC    R3,0(R6)            FIELD LENGTH                                 
         LA    R6,0(R3,R6)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNDLXCLP                                                      
*                                                                               
         BAS   RE,FNDLST           RESET LAST LINE ON SCREEN                    
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         DROP  R1                                                               
*                                                                               
         TITLE 'RERMP05 - TEXT RECORD - LNJOIN'                                 
***********************************************************************         
*                                                                     *         
*        JOIN LINE WITH CURSOR TO ONE BELOW                           *         
*              FIND LAST NON-BLANK AND FILL REMAINDER OF LINE WITH    *         
*              DATA FROM NEXT LINE :*                                           
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNJOIN   DS    0H                                                               
*                                                                               
*        ON ENTRY R6==> TABLE ENTRY FOR LINE WITH CURSOR                        
*                                                                               
         LH    RF,0(R6)            POINT TO CURSOR LINE                         
         LA    RF,0(RF,RA)                                                      
         OI    6(RF),X'80'         FORCE RE-DISPLAY OF LINE                     
         MVI   4(RF),X'80'         INDICATE FIELD INPUT THIS TIME               
         NI    4(RF),X'FF'-X'20'   SET AS NOT VALIDATED                         
*                                                                               
*        MOVE RIGHT PART OF LINE OVER TO CURSOR                                 
*                                                                               
         ICM   RE,15,ATIOB         GET DISPLACEMENT OF CURSOR INTO LINE         
         SR    R1,R1                                                            
         ICM   R1,1,TIOBCURI-TIOBD(RE)                                          
*                                                                               
         OI    TIOBINDS-TIOBD(RE),TIOBSETC LEAVES CURSOR WHERE IT WAS           
*                                                                               
         LA    R1,8(R1,RF)         POINT TO CURSOR POSITION                     
*                                                                               
         LA    RF,L'TXTTXT1-1+8(RF) POINT TO LAST BYTE OF CURSOR LINE           
*                                                                               
         CLI   0(R1),C' '          FIND FIRST BLANK AT OR AFTER CURSOR          
         BNH   *+18                                                             
         LA    R1,1(R1)                                                         
         CR    R1,RF               CHECK FOR END OF LINE                        
         BL    *-14                                                             
         B     LNJOIN0             LINE FULL                                    
*                                                                               
         LA    R1,1(R1)            MOVE EVERYTHING UP TO NEXT POSITION          
*                                                                               
         CR    R1,RF               DONE IF END OF LINE REACHED                  
         BH    LNJOIN0                                                          
*                                                                               
         CLI   0(R1),C' '          DONE IF NON-BLANK                            
         BH    LNJOIN0                                                          
*                                                                               
         LA    RE,1(R1)            SEARCH FOR NEXT NON-BLANK                    
*                                                                               
         CR    RE,RF               DONE IF END OF LINE REACHED                  
         BH    LNJOIN0                                                          
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         LA    RE,1(RE)            BUMP POINTER                                 
         B     *-18                                                             
*                                                                               
         MVC   0(1,R1),0(RE)       MOVE RIGHT END OVER TO THE LEFT              
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BNH   *-16                                                             
*                                  END OF LINE REACHED                          
         MVI   0(R1),C' '          FILL OUT REST OF LINE WITH BLANKS            
         LA    R1,1(R1)                                                         
         CR    R1,RF                                                            
         BNH   *-10                                                             
*                                                                               
LNJOIN0  DS    0H                                                               
*                                  RF POINTS TO LAST BYTE OF LINE               
         LA    R0,L'TXTTXT1        NUMBER OF BYTES ON LINE                      
*                                                                               
         CLI   0(RF),C' '          IF LINE FULL NOTHING TO DO                   
         BH    LNJOIN1                                                          
*                                                                               
         CLI   0(RF),C' '          FIND LAST NON-BLANK                          
         BH    *+14                                                             
         BCTR  RF,0                BACK UP A BYTE                               
         BCT   R0,*-10                                                          
         B     LNJOIN1             EMPTY LINE NO SPACE TO START                 
*                                                                               
         LA    RF,1(RF)            BUMP TO FIRST AVAILABLE SPOT                 
         MVI   1(RF),C' '          FORCE SPACE SEPARATOR                        
         AH    R0,=H'1'            RECTIFY BYTE COUNT                           
*                                                                               
LNJOIN1  DS    0H                                                               
*                                                                               
         LH    RE,0(R6)            DISPLACEMENT OF LINE                         
         LA    RE,0(RE,RA)         START        OF LINE                         
         STC   R0,5(RE)            RESET LINE LENGTH                            
         CLI   5(RE),0             MAKE SURE WE HAVE AT LEAST 2 SPACES          
         BNE   *+14                                                             
         MVC   8(2,RE),SPACES                                                   
         MVI   5(RE),2                                                          
*                                                                               
         LA    RF,1(RF)            BUMP TO FIRST AVAILABLE SPOT                 
*                                                                               
         CLC   2(2,R6),=X'FFFF'    SKIP IF AT END OF TABLE                      
         BE    LNJOINXX                                                         
*                                                                               
         LH    RE,2(R6)            DISPLACEMENT OF NEXT LINE                    
         LA    RE,0(RE,RA)         START OF NEXT LINE                           
         OI    6(RE),X'80'         FORCE RE-DISPLAY OF LINE                     
         MVI   4(RE),X'80'         INDICATE FIELD INPUT THIS TIME               
         NI    4(RE),X'FF'-X'20'   SET AS NOT VALIDATED                         
         LA    RE,8(RE)            FIRST BYTE OF LINE                           
*                                                                               
         LA    R2,L'TXTTXT1        LENGTH OF LINE                               
*                                                                               
         CLI   0(RE),C' '          FIND FIRST NON-SPACE                         
         BH    *+16                                                             
         LA    RE,1(RE)                                                         
         BCT   R2,*-12                                                          
         B     LNJOINX              LINE IS BLANK                               
*                                                                               
         LR    R3,RE               SAVE DATA START ADDRESS                      
*                                                                               
         LA    R1,L'TXTTXT1        CALCULATE SPACE LEFT ON CURSOR LINE          
*                                                                               
         SR    R1,R0                                                            
         BNP   LNJOIN3             NO ROOM ON CURSOR LINE                       
*                                                                               
         CR    R2,R1                                                            
         BH    *+10                                                             
         LR    R1,R2               USE SMALLER LENGTH                           
         B     LNJOIN2             GO MOVE EVERYTHING                           
*                                                                               
         LA    RE,0(R1,RE)         POINT TO LAST BYTE PLUS ONE                  
*                                    TO BE MOVED TO CURSOR LINE                 
*                                                                               
         CLI   0(RE),C' '          MUST BE A SPACE                              
         BNH   *+14                                                             
         BCTR  RE,0                BACK UP A BYTE                               
         BCT   R1,*-10                                                          
         B     LNJOIN3             NO ROOM TO MOVE ANYTHING                     
*                                                                               
LNJOIN2  DS    0H                                                               
*                                  R1 HAS NUMBER OF BYTES TO MOVE               
         BCTR  R1,0                DECREMENT FOR EXECUTE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R3)       MOVES NEXT LINE TO CURSOR LINE               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R3),0(R3)       CLEARS START OF NEXT LINE                    
*                                                                               
         LA    R1,1(R1)            RESTORE BYTE COUNTER                         
*                                                                               
         AR    R0,R1               NUMBER OF BYTES OF FIRST LINE                
*                                                                               
         LH    RF,0(R6)            DISPLACEMENT OF LINE                         
         LA    RF,0(RF,RA)         START        OF LINE                         
         STC   R0,5(RF)            RESET LINE LENGTH                            
         CLI   5(RF),0             MAKE SURE WE HAVE AT LEAST 2 SPACES          
         BNE   *+14                                                             
         MVC   8(2,RF),SPACES                                                   
         MVI   5(RF),2                                                          
*                                                                               
*                                  R2 HAS REMAINING BYTES ON LINE               
*                                                                               
LNJOIN3  DS    0H                                                               
*                                                                               
         SR    R2,R1               NUMBER OF BYTES TO BE JUSTIFIED              
         BNP   LNJOINX             NONE - ALL MOVED                             
*                                                                               
         CLI   0(RE),C' '          FIND NEXT NON-BLANK                          
         BH    *+16                                                             
         LA    RE,1(RE)            BUMP POINTER                                 
         BCT   R2,*-12             CONTINUE IF MORE ON LINE                     
         B     LNJOINX             NOTHING LEFT ON LINE TO MOVE                 
*                                                                               
         LH    RF,2(R6)            DISPLACEMENT OF NEXT LINE                    
         LA    RF,8(RF,RA)         START OF NEXT LINE                           
*                                                                               
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)       LEFT JUSTIFIES 'NEXT' LINE                   
*                                                                               
         LA    RF,1(R2,RF)         END OF WHAT WAS JUSTIFIED                    
*                                                                               
         LA    R2,1(R2)            RESTORE BYTE COUNTER                         
*                                                                               
         LA    R1,L'TXTTXT1                                                     
         SR    R1,R2               AMOUNT TO BE CLEARED AFTERWARDS              
         BNP   LNJOINX             NOTHING                                      
*                                                                               
         BCTR  R1,0                DECREMENT FOR EXECUTE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,RF),0(RF)       CLEAR OUT REST OF 'NEXT' LINE                
*****    MVC   0(0,RF),SPACES      CLEAR OUT REST OF 'NEXT' LINE                
*                                                                               
LNJOINX  DS    0H                                                               
*                                                                               
         LH    RF,2(R6)            DISPLACEMENT OF NEXT LINE                    
         LA    RF,0(RF,RA)         START OF NEXT LINE                           
         STC   R2,5(RF)            NEW LENGTH OF LINE                           
         CLI   5(RF),0             MAKE SURE WE HAVE AT LEAST 2 SPACES          
         BNE   *+14                                                             
         MVC   8(2,RF),SPACES                                                   
         MVI   5(RF),2                                                          
*                                                                               
LNJOINXX DS    0H                                                               
*                                                                               
         BAS   RE,FNDLST           RESET LAST LINE ON SCREEN                    
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'RERMP05 - TEXT POSTING - LNRPT'                                 
***********************************************************************         
*                                                                     *         
*        REPEAT DATA FROM LINE ABOVE                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNSPLT   DS    0H                                                               
*                                                                               
*        ON ENTRY R6==> TABLE ENTRY FOR LINE WITH CURSOR                        
*                                                                               
         SR    R1,R1                                                            
         SR    R2,R2                                                            
*                                                                               
LNRPTLP  DS    0H                                                               
*                                                                               
         LA    R5,2(R6)            POINT TO NEXT LINE IN TABLE                  
*                                                                               
         CLC   0(2,R5),=X'FFFF'    DONE IF END OF TABLE REACHED                 
         BE    LNRPTDN                                                          
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R5)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         CLI   5(RE),0             DONE IF NO ISSUE DATE ON                     
         BE    LNRPTDN                RECEIVING LINE                            
*                                                                               
*        BUMP PAST ISSUE DATE FIELD ON BOTH LINES                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         LA    R0,4                NUMBER OF NUMERIC FIELDS ON LINE             
*                                                                               
LNRPTLP1 DS    0H                                                               
*                                                                               
         CLI   5(RE),0             SKIP IF DATA ALREADY IN NEXT LINE            
         BNE   LNRPTCN1                                                         
*                                                                               
         CLI   5(RF),0             SKIP IF NOTHING TO MOVE                      
         BE    LNRPTCN1                                                         
*                                                                               
         IC    R2,0(RE)            GET LENGTH OF RECEIVING FIELD                
         SH    R2,=H'8'            ALLOW FOR HEADER                             
         TM    1(RE),X'02'         IF THERE IS EXTENDED HEADER                  
         BNO   *+8                                                              
         SH    R2,=H'8'               TAKE OUT ITS LENGTH                       
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),8(RF)       COPY LINE DOWN ONE                           
*                                                                               
         MVC   4(2,RE),4(RF)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RE),X'80'         TRANSMIT FIELD                               
         MVC   7(1,RE),5(RE)       SET OUTPUT LENGTH                            
         NI    4(RE),X'FF'-X'20'   SET TO NOT VALIDATED                         
*                                                                               
LNRPTCN1 DS    0H                                                               
*                                                                               
         IC    R1,0(RF)            FIELD LENGTH                                 
         LA    RF,0(R1,RF)         BUMP TO NEXT FIELD                           
         IC    R1,0(RE)            FIELD LENGTH                                 
         LA    RE,0(R1,RE)         BUMP TO NEXT FIELD                           
*                                                                               
         BCT   R0,LNRPTLP1                                                      
*                                                                               
LNRPTCN  DS    0H                                                               
*                                                                               
         LA    R6,2(R6)            BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
         B     LNRPTLP                                                          
*                                                                               
LNRPTDN  DS    0H                                                               
*                                                                               
LNRPTX   DS    0H                                                               
*                                                                               
         BAS   RE,FNDLST           RESET LAST LINE ON SCREEN                    
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'RERMP05 - TEXT POSTING - EXITS'                                 
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYSX  DS    0H                  NORMAL EXIT                                  
         CR    RB,RB               FORCE EQ CC                                  
         XIT1                                                                   
*                                                                               
PFKEYERX DS    0H                  ERROR EXIT                                   
         LTR   RB,RB               FORCE NE CC                                  
         XIT1                                                                   
*                                                                               
         TITLE 'RERMP05 - TEXT POSTING - LNTBL'                                 
***********************************************************************         
*                                                                     *         
*        TABLE OF LINE DISPLACEMENTS INTO SCREEN                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNTBL    DS    0D                                                               
         DC    Y(TXTTXT1H-T810FFD)  TEXT 1                                      
         DC    Y(TXTTXT2H-T810FFD)  TEXT 2                                      
         DC    Y(TXTTXT3H-T810FFD)  TEXT 3                                      
         DC    Y(TXTTXT4H-T810FFD)  TEXT 4                                      
         DC    Y(TXTTXT5H-T810FFD)  TEXT 5                                      
         DC    Y(TXTTXT6H-T810FFD)  TEXT 6                                      
         DC    Y(TXTTXT7H-T810FFD)  TEXT 7                                      
         DC    Y(TXTTXT8H-T810FFD)  TEXT 8                                      
         DC    Y(TXTTXT9H-T810FFD)  TEXT 9                                      
         DC    Y(TXTTXTAH-T810FFD)  TEXT 10                                     
LNTBLLS  DC    Y(TXTTXTLH-T810FFD)  TEXT LAST                                   
         DC    4X'FF'               END OF TABLE                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO UPDATE RECORD BASED ON ELEMENT TABLE CHANGES             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LSWRTTAB NMOD1 0,**#LSWTB                                                       
*                                                                               
         LA    RC,SPOOLEND         POINT TO WORKING STORAGE                     
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE MESSAGE NUMBER                          
*                                                                               
*        FIRST DELETE ALL CURRENT ELEMENTS                                      
*                                                                               
         MVC   AIO,AIO1            GET RECORD ADDRESS                           
*                                                                               
         MVI   ELCODE,1            DELETE TEXT ELEMENTS                         
*                                                                               
LSWDELLP DS    0H                  DELETE OLD ELEMENTS                          
*                                                                               
         GOTO1 REMELEM             DELETE OLD X'01' ELEMENTS                    
*                                                                               
LSWDELCN DS    0H                  DELETE OLD ELEMENTS                          
*                                                                               
         OC    ELEMENT,ELEMENT     CONTINUE IF ONE WAS DELETED                  
         BNZ   LSWDELLP                                                         
*                                                                               
LSWDELDN DS    0H                  DELETE OLD ELEMENTS                          
*                                                                               
*        ADD ELEMENTS IN TABLE TO RECORD                                        
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         ICM   R0,15,BSPNOR        SKIP IF NO ENTRIES                           
         BZ    LSWTLPDN                                                         
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'R'           INDICATE REP SYSTEM                          
*                                                                               
         LA    R6,RINVPEL-RINVREC(R6) POINT TO FIRST ELEMENT IN REC             
*                                                                               
         LA    R1,DMCB             POINT TO PARAMETER LIST                      
         SR    R2,R2               INIT TEXT LINE NUMBER                        
*                                                                               
LSWTLOOP DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTADDQ      ADD ELEMENTS FLAGGED FOR ADD                 
         BO    LSWTADD                                                          
         TM    ELTCTL,ELTDELQ      AND THOSE NOT TO BE DELETED                  
         BO    LSWTNADD                                                         
*                                                                               
LSWTADD  DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR WORKAREA                               
*                                                                               
         LA    R3,ELTELEM          POINT TO ELEMENT IN TABLE                    
         USING RINVTEL,R3          ESTABLISH TEXT ELEMENT                       
*                                                                               
         CLI   RINVTLEN,0          GET ELEMENT LENGTH                           
         BZ    LSWTADDX            SKIP IF NO ENTRY                             
*                                                                               
         LA    R2,1(R2)            BUMP TEXT LINE NUMBER                        
         STC   R2,RINVTLIN         SET TEXT LINE NUMBER                         
*                                                                               
         XC    ELTKEYNW,ELTKEYNW   SAVE THIS NEW KEY                            
         STC   R2,ELTKEYNW                                                      
*                                                                               
         L     RF,AIO                                                           
         MVC   HALF,27(RF)         SAVE AWAY REC LENGTH                         
         LH    RF,HALF                                                          
*                                                                               
         ZIC   RE,RINVTLEN         ELEM LENGTH                                  
         AR    RF,RE               GET NEW RECORD LENGTH                        
         CH    RF,=H'2048'         REC LENGTH > 2K?                             
         BH    NOMORE                                                           
*                                                                               
         L     RF,VRECUP                                                        
         GOTO1 (RF),(R1),,(R3),(C'R',(R6))  ADD ELEMENT                         
*                                                                               
         CLI   DMCB+8,0            ONLY ERROR CAN BE NO ROOM IN REC             
         BE    NOMORE              NO ERROR TOLERATED                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(R6)            ELEMENT LENGTH                               
         AR    R6,RF               NEXT INSERTION POINT                         
*                                                                               
LSWTADDX DS    0H                                                               
*                                                                               
         B     LSWTLPCN                                                         
*                                                                               
LSWTNADD DS    0H                                                               
*                                                                               
LSWTLPCN DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         BCT   R0,LSWTLOOP         LOOP IF NOT LAST ENTRY                       
*                                                                               
LSWTLPDN DS    0H                                                               
*                                                                               
         LA    R0,NLINS            NUMBER OF LINES IN TABLE                     
         LA    R5,LSVTAB           POINT TO SAVED KEYS                          
*                                                                               
LSWLSVLP DS    0H                                                               
*                                                                               
         USING LSVTABD,R5          ESTABLISH SAVED KEYS AREA                    
*                                                                               
         OC    LSVKEY,LSVKEY       SKIP IF NO ENTRY SAVED                       
         BNZ   *+10                                                             
         OC    LSVKEYNW,LSVKEYNW                                                
         BZ    LSWLSVCN                                                         
*                                                                               
         MVC   HALF,LSVKEY                                                      
         OC    LSVKEYNW,LSVKEYNW   IF THERE IS A NEW KEY                        
         BZ    *+10                                                             
         MVC   HALF,LSVKEYNW          USE IT                                    
*                                  FIND TABLE ENTRY                             
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',HALF),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,BSPAREC          POINT TO FOUND ELEMENT                       
         USING ELTABD,R4           ESTABLISH ELEMENT                            
*                                                                               
         MVC   LSVKEY,ELTKEYNW     COPY NEW KEY                                 
         XC    LSVKEYNW,LSVKEYNW                                                
*                                                                               
LSWLSVCN DS    0H                                                               
*                                                                               
         LA    R5,LSVTABL(R5)      BUMP TO NEXT ITEM IN TABLE                   
         BCT   R0,LSWLSVLP                                                      
*                                                                               
LSWLSVDN DS    0H                                                               
*                                                                               
*        SET TABLE KEYS TO NEW ONES                                             
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         ICM   R0,15,BSPNOR        SKIP IF NO ENTRIES                           
         BZ    LSWTRSDN                                                         
*                                                                               
LSWTRSLP DS    0H                                                               
*                                                                               
         MVC   ELTKEY,ELTKEYNW     RESET TO NEW KEY                             
         XC    ELTKEYNW,ELTKEYNW                                                
*                                                                               
LSWTRSCN DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         BCT   R0,LSWTRSLP         LOOP IF NOT LAST ENTRY                       
*                                                                               
LSWTRSDN DS    0H                                                               
*                                                                               
LSWRTX   DS    0H                                                               
         MVC   ERROR,SAVMSGNO      RESTORE MESSAGE NUMBER                       
         XIT1                                                                   
*                                                                               
NOMORE   MVC   RERROR,=AL2(RECFULL)     NO ROOM IN RECORD OR TABLE              
         LA    R2,TXTTXT1H                                                      
         GOTO1 MYERROR                                                          
*                                                                               
         DROP  R3                                                               
         DROP  R4                                                               
         DROP  R5                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP05 - TEXT POSTING - LISTREC'                               
***********************************************************************         
*                                                                     *         
*        LIST AND PRINT ROUTINE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LISTREC  NTR1  BASE=*,LABEL=*                                                   
         XC    OLDKEY,OLDKEY       CLEAR SELECTION KEY                          
         CLI   MODE,PRINTREP                                                    
         BNE   LR5                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         LA    R1,BUFF             CLEAR BUFF TO SPACES                         
         LA    RE,45                                                            
LR3      MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   RE,LR3                                                           
         SPACE 1                                                                
LR5      LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         OC    KEY(27),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STATION                                                 
         CLI   SOURCE,0                                                         
         BE    LR7                                                              
         MVC   RINVKSRC,SOURCE                                                  
         OC    INVNO,INVNO                                                      
         BZ    LR7                                                              
         MVC   RINVKINV,INVNO                                                   
         OC    DATE,DATE                                                        
         BZ    LR7                                                              
         MVC   RINVKSTD,DATE                                                    
LR7      MVC   SAVEKEY,KEY                                                      
LR10     GOTO1 HIGH                                                             
LR15     CLC   KEY(17),SAVEKEY     CORRECT REP AND STATION                      
         BNE   LRXIT                                                            
         CLI   RINVKSRC,C'M'       ONLY WANT TEXT - MARKET                      
         BE    LR20                                                             
         CLI   RINVKSRC,C'S'       STATION                                      
         BE    LR20                                                             
         CLI   RINVKSRC,X'FF'      OR INVENTORY TEXT                            
         BNE   LR200                                                            
         SPACE 1                                                                
LR20     CLI   SOURCE,0                                                         
         BE    LR30                                                             
         CLC   RINVKSRC(1),SOURCE                                               
         BNE   LRXIT                                                            
         CLI   SOURCE,X'FF'                                                     
         BNE   LR30                                                             
         TM    STATUS,X'80'        LIST THIS INVENTORY NUMBER ONLY              
         BZ    LR30                                                             
         CLC   RINVKINV(4),INVNO                                                
         BNE   LRXIT                                                            
         OC    DATE,DATE                                                        
         BZ    LR30                                                             
         CLC   RINVKSTD(3),DATE                                                 
         BNE   LRXIT                                                            
         SPACE 1                                                                
LR30     MVC   LISTAR,SPACES       CLEAR OUT LIST LINE                          
         LA    R2,LISTAR                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR35                                                             
         LA    R2,P                OR USE P IF HARDCOPY                         
         MVC   P,SPACES                                                         
         USING LISTD,R2                                                         
LR35     GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        TEXT FILTER ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    LR40                                                             
*                                  NOT FOUND                                    
         CLI   KFLOC,C'Y'          IF THEY WANT ONLY LOCAL TEXT                 
         BE    *+8                                                              
         CLI   KFINT,C'Y'          IF THEY WANT ONLY INTERNAL TEXT              
         BE    LR200                  THEY DON'T WANT THIS ONE                  
*                                                                               
         MVI   LFLOC,C'N'                                                       
         MVI   LFINT,C' '                                                       
         B     LR100               ELSE, INCLUDE THIS TEXT                      
*                                                                               
         USING RINVFEL,R6                                                       
*                                                                               
LR40     OC    KFSRC,KFSRC         SOURCE FILTER                                
         BZ    LR45                                                             
         CLC   KFSRC,RINVFSRC       LIST MATCHING SOURCE                        
         BE    LR45                                                             
         CLI   RINVFSRC,0          AND NO SOURCE                                
         BNE   LR200                                                            
*                                  BOOK FILTER                                  
LR45     OC    KFBOOK,KFBOOK                                                    
         BZ    LRFBKX                 NO SCREEN FILTERS                         
*                                                                               
         OC    RINVFBK,RINVFBK                                                  
         BZ    LRFBKX                 NO SCREEN FILTERS                         
*                                                                               
         CLC   KFBKYM,RINVFBK      DOES YEAR/MONTH MATCH                        
         BNE   LR200                                                            
*                                                                               
         MVC   BYTE,KFBKBITS       COPY BOOKVAL BITS                            
*                                                                               
         CLC   BYTE,RINVFBKT       DO BOOKVAL BITS MATCH                        
         BE    LRFBK10             YES                                          
*                                                                               
         CLI   KFSRC,0             NO, BUT NO SOURCE ASSUMES MFX                
         BNE   LR200                                                            
*                                                                               
         OI    BYTE,X'40'          SO CHECK NSI ALSO                            
*                                                                               
         CLC   BYTE,RINVFBKT                                                    
         BE    LRFBK10             YES                                          
*                                                                               
         OI    BYTE,X'01'          AND CHECK SRC ALSO                           
*                                                                               
         CLC   BYTE,RINVFBKT                                                    
         BNE   LR200               NO MATCH - SKIP TEXT RECORD                  
*                                                                               
LRFBK10  DS    0H                                                               
*                                                                               
         CLC   KFBKTP,RINVFBTP     MATCH TO BOOK TYPE                           
         BNE   LR200               NO MATCH - SKIP TEXT RECORD                  
*                                                                               
LRFBKX   DS    0H                                                               
*                                                                               
LR50     OC    KFLOC,KFLOC         LOCAL TEXT ONLY FILTER                       
         BZ    LRLOCX                                                           
*                                                                               
         CLI   KFLOC,C'Y'          Y=ONLY LOCAL, N=EXLCLUDE LOCAL               
         BE    LRLOC1                                                           
*                                                                               
         CLI   RINVFLOC,0                                                       
         BE    LRLOCX                                                           
*                                                                               
         B     LR200                                                            
*                                                                               
LRLOC1   CLI   RINVFLOC,C'Y'                                                    
         BNE   LR200               DROP                                         
*                                                                               
LRLOCX   DS    0H                                                               
*                                                                               
         OC    KFINT,KFINT         INTERNAL TEXT FILTER                         
         BZ    LRINTX                                                           
*                                                                               
         CLI   KFINT,C'Y'          Y=ONLY INTERNAL, N=EXLCLUDE INTERNAL         
         BE    LRINT1                                                           
*                                                                               
         CLI   RINVFTYP,0                                                       
         BE    LRINTX              KEEP                                         
*                                                                               
         B     LR200               DROP                                         
*                                                                               
LRINT1   CLI   RINVFTYP,RINVFTIQ                                                
         BNE   LR200               DROP                                         
*                                                                               
LRINTX   DS    0H                                                               
*                                                                               
LR60     MVC   LFSRC,RINVFSRC                                                   
         OC    RINVFBK,RINVFBK                                                  
         BZ    LR65                                                             
         MVC   WORK(1),RINVFBKT                                                 
         MVC   WORK+1(2),RINVFBK                                                
         MVC   WORK+3(1),RINVFBTP                                               
         BAS   RE,FMTBOOK                                                       
         MVC   LFBOOK,WORK+10                                                   
*                                                                               
LR65     MVI   LFLOC,C'N'                                                       
         CLI   RINVFLOC,C'Y'                                                    
         BNE   *+8                                                              
         MVI   LFLOC,C'Y'                                                       
*                                                                               
         MVI   LFINT,C' '                                                       
         CLI   RINVFTYP,RINVFTIQ                                                
         BNE   *+8                                                              
         MVI   LFINT,C'Y'                                                       
*                                                                               
         ZIC   R3,RINVFLEN                                                      
         SH    R3,=H'10'                                                        
         BNP   LR100                                                            
         CLI   MODE,PRINTREP       6 DEMOS FIT ON PRINT LINE,                   
         BE    LR70                                                             
         CH    R3,=H'5'            BUT ONLY 5 DEMOS CAN FIT ON SCREEN           
         BNH   LR70                SO USE THE SMALLER                           
         LA    R3,5                                                             
         MVI   LFDEMOX,C'*'        INDICATES MORE DEMOS THAN CAN FIT            
         SPACE 1                                                                
LR70     STC   R3,BYTE             R3 AND BYTE HAVE NUMBER OF DEMOS             
         LA    RE,RINVFDEM                                                      
         XC    WORK,WORK           WORK WILL HAVE FULL DEMO EXPRESSIONS         
         LA    R5,WORK                                                          
LR80     MVI   1(R5),C'T'                                                       
         MVC   2(1,R5),0(RE)                                                    
         LA    R5,3(R5)                                                         
         LA    RE,1(RE)            POINT TO NEXT DEMO                           
         BCT   R3,LR80             ANY MORE                                     
         DROP  R6                                                               
         SPACE 1                                                                
         LA    R4,BLOCK                                                         
         USING DEMOD,R4                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 DEMOCON,DMCB,(BYTE,WORK),(9,LFDEMO),(0,DBLOCK)                   
         DROP  R4                                                               
         SPACE 1                                                                
LR100    L     R6,AIO                                                           
         USING RINVKEY,R6                                                       
         CLI   RINVKSRC,X'FF'                                                   
         BE    *+14                                                             
         MVC   LTYPE(1),RINVKSRC                                                
         B     LR105                                                            
         SPACE 1                                                                
         LA    R1,LTYPE                                                         
         MVC   INVNO,RINVKINV                                                   
         MVC   KDATE,RINVKSTD                                                   
         BAS   RE,INVDISP                                                       
         SPACE 1                                                                
LR105    EDIT  (2,RINVKTXT),(5,LTXTNO)                                          
         DROP  R6                                                               
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LR150                                                            
         LA    R4,BUFF                                                          
         SR    R5,R5               LINE COUNTER                                 
         MVC   18(114,R4),P       FOR REPORT-PRINT LIST STUFF                   
         LA    R4,132(R4)                                                       
         MVC   0(132,R4),SPACES                                                 
         LA    R5,2(R5)                                                         
         LA    R4,132(R4)                                                       
         SPACE 1                                                                
         L     R6,AIO              PLUS THE TEXT                                
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RINVTEL,R6                                                       
         SPACE 1                                                                
         XR    R3,R3                                                            
LR110    IC    R3,RINVTLEN         ELEMENT LENGTH                               
         SH    R3,=H'7'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   23(0,R4),RINVTEXT                                                
         LA    R5,1(R5)                                                         
         LA    R4,132(R4)                                                       
         IC    R3,1(R6)            NEXT ELEMENT                                 
         AR    R6,R3                                                            
         CLI   0(R6),1                                                          
         BE    LR110                                                            
         MVC   0(132,R4),SPACES    2 SPACING LINES BETWEEN TEXTS                
         LA    R4,132(R4)                                                       
         MVC   0(132,R4),SPACES                                                 
         LA    R5,2(R5)                                                         
         SPACE                                                                  
         STC   R5,ALLOWLIN         MAKE SURE TEXT FITS ON 1 PAGE                
         LA    R4,BUFF             AND PRINT IT                                 
LR130    MVC   P,0(R4)                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   0(132,R4),SPACES    RECLEAR BUFF TO SPACES                       
         LA    R4,132(R4)                                                       
         BCT   R5,LR130                                                         
         B     LR200                                                            
         DROP  R6                                                               
         SPACE 1                                                                
LR150    GOTO1 LISTMON             FOR LIST                                     
         SPACE 1                                                                
LR200    GOTO1 SEQ                 NEXT RECORD                                  
         LA    R6,KEY                                                           
         B     LR15                                                             
         SPACE 1                                                                
LRXIT    XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP05 - TEXT POSTING - LISTD'                                 
***********************************************************************         
*                                                                     *         
*        LIST LINE DSECT                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LISTD    DSECT                                                                  
LTYPE    DS    CL13                                                             
         DS    CL1                                                              
LTXTNO   DS    CL5                                                              
         DS    CL2                                                              
LFSRC    DS    CL1                                                              
         DS    CL2                                                              
LFBOOK   DS    CL9                                                              
         DS    CL1                                                              
LFLOC    DS    CL1                                                              
LFINT    DS    CL1                                                              
         DS    CL1                                                              
LFDEMO   DS    CL36                                                             
LFDEMOX  DS    CL1                                                              
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPFFD                                                                      
* DDGENTWA                                                                      
* RERMPD1D                                                                      
* RERMPD2D                                                                      
* RERMPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPD1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPD2D                                                       
         ORG   CONHEADH-64+X'900'                                               
LSVTAB   DS    XL(NLINS*LSVTABL)   LINE UP SAVE TABLE                           
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
       ++INCLUDE RERMPWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
       ++INCLUDE RERMPITSYP                                                     
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
SCINVTRA EQU   X'E8'               SCREEN NUMBER FOR INV/TRACKS                 
RELO     DS    A                                                                
MYWORK   DS    0CL1024                                                          
SAVEKEY  DS    CL27                                                             
SVKEY    DS    CL27                FOR INTERVENING GETRECS                      
HDRKEY   DS    CL27                FOR INVENTORY HEADER                         
SVELTKEY DS    CL2                 FOR ELEMENT KEY                              
SVDMWORK DS    F                                                                
STATION  DS    CL4                 STATION CALL LETTERS                         
MEDIA    DS    CL1                   MEDIA - TV=BLANK                           
SOURCE   DS    CL1                 M=MARKET, S=STATION, X'FF'=INV. NO           
NUMBER   DS    CL2                 TEXT NUMBER                                  
INVNO    DS    CL4                 INVENTORY NUMBER                             
DATE     DS    XL3                 EFFECTIVE DATE-INPUT                         
FSRC     DS    CL1                 FILTER SOURCE FROM RECORD                    
FBOOK    DS    0CL4                FILTER BOOK         FROM RECORD              
FBKBITS  DS    XL1                        BOOKVAL BITS FROM RECORD              
FBKYM    DS    XL2                        BOOK YM      FROM RECORD              
FBKTP    DS    CL1                        BOOK TYPE    FROM RECORD              
*                                                                               
INVSRC   DS    CL1                 INVENTORY SOURCE                             
KDATE    DS    XL3                 EFFECTIVE DATE-FROM HEADER                   
*                                                                               
KFSRC    DS    CL1                 FILTER SOURCE       FOR LIST/REPORT          
*                                                                               
KFBOOK   DS    0CL4                FILTER BOOK         FOR LIST/REPORT          
KFBKBITS DS    XL1                        BOOKVAL BITS FOR LIST/REPORT          
KFBKYM DS      XL2                        BOOK YM      FOR LIST/REPORT          
KFBKTP DS      CL1                        BOOK TYPE    FOR LIST/REPORT          
*                                                                               
KFLOC    DS    CL1                 FLTR LOCAL TEXT FOR LIST/RPT (0,Y,N)         
KFINT    DS    CL1                 FLTR INTERNAL   FOR LIST/RPT (0,Y,N)         
STATUS   DS    XL1                 X'80' LIST INVENTORY NUMBER ONLY             
FLD      DS    CL80                FIELD WORKAREA                               
ALINCUR  DS    A                   A(LINE WITH CURSOR)                          
LINLAST  DS    A                   A(LAST LINE WITH TEXT)                       
*                                                                               
*        COPY SAVEAREAS                                                         
*                                                                               
ACTCPY   EQU   15                  INTERNAL VALUE FOR ACTION COPY               
*                                                                               
CPYACTSW DS    CL1                 COPY ACTION SWITCH                           
CPYACTFQ EQU   C'1'                FIRST  PHASE COMPLETED                       
CPYACTTQ EQU   C'2'                SECOND PHASE COMPLETED                       
*                                                                               
*                                                                               
CPYFAREA DS    0X                  COPY FROM AREA                               
CPYFSTA  DS    CL5                 FROM STATION                                 
CPYFMED  DS    CL1                 FROM MEDIA                                   
CPYFTYP  DS    CL1                 FROM TYPE                                    
CPYFINV  DS    CL4                 FROM INVENTORY NUMBER                        
CPYFDTE  DS    XL3                 FROM INVENTORY EFF DATE                      
CPYFNUM  DS    XL2                 FROM NUMBER                                  
CPYFSRC  DS    CL1                 FROM SOURCE                                  
CPYFBK   DS    CL3                 FROM BOOK                                    
CPYFLOC  DS    CL1                 FROM LOCAL INDICATOR                         
CPYFDEM  DS    16XL3               FROM DEMOS                                   
CPYFSPR  DS    CL32                SPARE FROM AREA                              
CPYFLENQ EQU   *-CPYFAREA          COPY FROM AREA LENGTH                        
*                                                                               
CPYTAREA DS    0X                  COPY TO   AREA                               
CPYTSTA  DS    CL5                 TO   STATION                                 
CPYTMED  DS    CL1                 TO   MEDIA                                   
CPYTTYP  DS    CL1                 TO   TYPE                                    
CPYTINV  DS    CL4                 TO   INVENTORY NUMBER                        
CPYTDTE  DS    XL3                 TO   INVENTORY EFF DATE                      
CPYTNUM  DS    XL2                 TO   NUMBER                                  
CPYTSRC  DS    CL1                 TO   SOURCE                                  
CPYTBK   DS    CL3                 TO   BOOK                                    
CPYTLOC  DS    CL1                 TO   LOCAL INDICATOR                         
CPYTDEM  DS    16XL3               TO   DEMOS                                   
CPYTSPR  DS    CL32                SPARE TO   AREA                              
CPYTLENQ EQU   *-CPYTAREA          COPY TO   AREA LENGTH                        
*                                                                               
FRKEY    DS    CL27                FROM KEY                                     
TOKEY    DS    CL27                TO   KEY                                     
*                                                                               
WRKELTAB DS    XL(ELTABL)          WORK TABLE ELEMENT                           
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
         DS    0F                                                               
*                                                                               
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
         DS    0D                                                               
LUBLK    DS    XL(LUBLKL)          LINUP CONTROL BLOCK                          
         DS    0F                                                               
LINDSPS  DS    XL((NLINS+1)*2)                                                  
SVLSVTAB DS    XL(NLINS*LSVTABL)      HOLD COPY OF LINUP SAVE TABLE             
ELTMAX   EQU   40                     MAX NUMBER OF ELEMENTS IN TABLE           
DISPSW   DS    X                   DISPLAY SWITCH                               
DISPREDO EQU   X'80'               RE-DISPLAY SCREEN                            
SVDIR    DS    XL1                 DIRECTION SAVEAREA                           
WBOOKTYP DS    XL16                BOOK TYPES                                   
*                                                                               
*        INPUT CONTROL BLOCK FOR GETKSRC                                        
*                                                                               
GSRCIN   DS    0C                  GETKSRC INPUT BLOCK                          
GSIRSVC  DS    CL1                 RATING SERVICE                               
GSIQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSIKSRC  DS    CL1                 RINVKSRC FOR KEY                             
GSIBITS  DS    XL1                 BOOKVAL BITS                                 
GSIBKTYP DS    CL1                 BOOKTYPE                                     
GSRCINL  EQU   *-GSRCIN            INPUT BLOCK LENGTH                           
*                                                                               
*        OUTPUT CONTROL BLOCK FOR GETKSRC                                       
*                                                                               
GSRCOUT  DS    0C                  GETKSRC OUTPUT BLOCK                         
GSORSVC  DS    CL1                 RATING SERVICE                               
GSOQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSOKSRC  DS    CL1                 RINVKSRC FOR KEY                             
GSOBITS  DS    XL1                 BOOKVAL BITS                                 
GSOBKTYP DS    CL1                 BOOKTYPE                                     
GSRCOUTL EQU   *-GSRCOUT           OUTPUT BLOCK LENGTH                          
*                                                                               
SVLSTDT  DS    XL(L'BTODAY)        LAST CHANGED DATE SAVEAREA                   
*                                                                               
RMPPROF  DS    XL16                RMP PROGRAM PROFILE                          
*                                                                               
OLDKEY   DS    XL32                KEY SAVEAREA                                 
*                                                                               
ITERSW   DS    XL1                 X'00' - FIRST TIME TO DISREC/VALREC          
*                                    ONLY VALID IF PFKEY HIT                    
*                                                                               
       ++INCLUDE RERMPPROF                                                      
         SPACE 4                                                                
*DEMO DSECT INCLUDING DEDBLOCK                                                  
DEMOD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
*                                                                               
*DDFLDIND                                                                       
*DDFLDHDR                                                                       
*FATIOB                                                                         
*REMSGEQUS                                                                      
*FAGETTXTD                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REMSGEQUS                                                      
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE DDLINUPD                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
LSVTABD  DSECT                     LINUP SAVE AREA DSECT                        
LSVKEY   DS    0XL(L'LSVSORT)                                                   
LSVSORT  DS    XL2                 SORT VALUE - LINE # & LINE SUB #             
LSVKEYL  EQU   *-LSVTABD                                                        
LSVKEYNW DS    XL2                 NEW KEY AFTER WRITING RECORD                 
LSVTABL  EQU   *-LSVTABD                                                        
*                                                                               
ELTABD   DSECT                     DSECT FOR ELEM TABLE                         
ELTKEY   DS    0XL(L'ELTSORT)                                                   
ELTSORT  DS    XL2                 SORT VALUE- LINE # & LINE SUB #              
ELTKEYL  EQU   *-ELTABD            KEY LENGTH                                   
ELTKEYNW DS    XL2                 NEW KEY AFTER WRITING RECORD                 
ELTCTL   DS    XL1                 CONTROL BYTE                                 
ELTDELQ  EQU   X'80'                 DELETE                                     
ELTADDQ  EQU   X'40'                 ADD                                        
ELTELEM  DS    CL86                ELEMENT                                      
ELTABL   EQU   *-ELTABD            ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028RERMP05S  05/01/02'                                      
         END                                                                    
