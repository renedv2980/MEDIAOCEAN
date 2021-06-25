*          DATA SET CTSFM0A    AT LEVEL 085 AS OF 05/01/02                      
*PHASE TA0A0AA,*                                                                
*INCLUDE RECUP                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM0A -- PANEL ENTRY MAINTENANCE                   *         
*                                                                     *         
*  COMMENTS:     CREATES AND MAINTAINS THE PANEL RECORDS FOR          *         
*                SCROLLER ON GENDIR/GENFIL.                           *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00).                             *         
*                                                                     *         
*  CALLS TO:     DATAMGR, MINIO.                                      *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMFA (MAINTENANCE), ENTRY RECORDS         *         
*                                                                     *         
*  OUTPUTS:      UPDATED PANEL RECORDS.                               *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER POINTER                    *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- MINBLKD                                        *         
*                R6 -- MINELEM - PNLELTD/PNLPFD FOR WHOLE PROG        *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- PANKEYD                                        *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A0A - PANEL ENTRY MAINTENANCE'                               
TA0A0A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A0A**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         OI    CONSERVH+6,X'81'    MODIFIED SO PFKEY CAN BE PRESSED             
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO             RELOCATION FACTOR                            
         CLI   MODE,VALREC         MODE NOT VALREC                              
         BNE   EXIT1                                                            
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
* GET A(MINIO)                                                                  
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A74'   MINIO IS CORE-RES PHASE                 
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   MINIO,DMCB                                                       
* INITIALIZE MINIO                                                              
         LA    RE,MINBLOCK         CLEAR MINBLOCK                               
         LA    RF,MINBLKL                                                       
         XCEF                                                                   
         L     RF,=V(RECUP)                                                     
         A     RF,RELO                                                          
         ST    RF,MINRECUP         A(RECUP)                                     
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=CL8'GENFIL  ' FILE NAME                                  
         MVC   MINDIR,=CL8'GENDIR  ' DIR NAME                                   
         MVI   MINFKLEN,PNLKLENQ   KEY LENGTH                                   
         MVI   MINNCTL,4           NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=AL2(1976) MAXIMUM RECORD LENGTH                        
         MVI   MINEKLEN,6          ELEMENT KEY LENGTH                           
         MVI   MINEKDSP,L'PNLKMAST ELEM KEY DISP                                
         MVC   MINBUFF,AIO2        A(FIRST BUFFER)                              
         MVI   MINNBUF,2           USE TWO BUFFERS                              
         MVC   MINRTAB,ATIA        A(AREA FOR RECORD TABLE)                     
         MVC   MINRTABL,=AL2(6144) LENGTH OF RECORD TABLE                       
         LA    RE,MELEM                                                         
         ST    RE,MINELEM          A(AREA FOR ELEM OR CLUSTER)                  
         MVC   MINMAXEL,=AL2(100)  MAX LENGTH OF ELEM OF CLUSTER                
         XC    0(100,RE),0(RE)     CLEAR MINELEM AREA                           
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
* BUILD KEY                                                                     
         XC    MINMKEY,MINMKEY     CLEAR MASTER KEY FOR MINIO                   
         CLI   ACTNUM,15           COPY PANELS?                                 
         BNE   TA0A0A1                                                          
         GOTO1 =A(COPYPNLE),DMCB,(RC),(RA),(R5),(R6),(R9),RR=RELO               
TA0A0A1  XC    MODE,MODE           CLEAR MODE                                   
         LA    R8,MINMKEY                                                       
         USING PNLKEYD,R8                                                       
         MVI   PNLKSYS,PNLKSYSQ    SYSTEM                                       
         MVI   PNLKSTYP,PNLKSTYQ   KEY TYPE                                     
* SYSTEM.PROGRAM DICTIONARY                                                     
         LA    R2,SFMSYPGH         GET SYS.PRG FOR KEY                          
         CLI   5(R2),0             MUST BE PRESENT                              
         BE    MISSERR                                                          
         CLI   5(R2),5             MUST BE 5 CHARS                              
         BNE   BADERR                                                           
         MVC   PNLKSYPG,8(R2)                                                   
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BZ    *+8                                                              
         MVI   MODE,VALKEY         YES, VALIDATE THE KEY                        
* FOR WHICH PANEL                                                               
         LA    R2,SFMPANLH         GET PANEL NAME FOR KEY                       
         CLI   5(R2),0             MUST BE PRESENT                              
         BE    MISSERR                                                          
         MVC   PNLKNAME,8(R2)                                                   
         OC    PNLKNAME,MYSPACES   BLANK PAD                                    
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BZ    *+8                                                              
         MVI   MODE,VALKEY         YES, VALIDATE THE KEY                        
* WHICH TYPE?  SINGLE OR MULTIPLE                                               
         MVC   PNLKTYPE,CONREC     SINGLE OR MULTIPLE IN RECORD TYPE            
* WHAT AGENCY, IF ANY?                                                          
         LA    R2,SFMAGCYH                                                      
         TM    SFMAGCYH+4,X'80'    INPUT THIS TIME?                             
         BZ    *+8                                                              
         MVI   MODE,VALKEY         YES, VALIDATE THE KEY                        
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   PNLKAGY,SFMAGCY     AGENCY                                       
* WHAT MEDIA, IF ANY?                                                           
         LA    R2,SFMMEDIH                                                      
         TM    SFMMEDIH+4,X'80'    INPUT THIS TIME?                             
         BZ    *+8                                                              
         MVI   MODE,VALKEY         YES, VALIDATE THE KEY                        
         CLI   5(R2),0             MEDIA HAS SOMETHING                          
         BE    CKCLNT              NO                                           
         CLI   SFMAGCYH+5,0        YES, AGENCY DOES NOT?                        
         BNE   *+14                                                             
         MVC   GERROR,=AL2(NDAGENCY) YES, NEED AGENCY BEFORE MEDIA              
         B     SFMERROR                                                         
         MVC   PNLKMED,SFMMEDI     MEDIA                                        
* WHAT CLIENT, IF ANY?                                                          
CKCLNT   LA    R2,SFMCLNTH                                                      
         TM    SFMCLNTH+4,X'80'    INPUT THIS TIME?                             
         BZ    *+8                                                              
         MVI   MODE,VALKEY         YES, VALIDATE THE KEY                        
         CLI   5(R2),0             CLIENT HAS SOMETHING                         
         BE    CKSCRL                                                           
         CLI   SFMMEDIH+5,0        YES, MEDIA DOES NOT?                         
         BNE   *+14                                                             
         MVC   GERROR,=AL2(NDMEDIA) YES, NEED MEDIA BEFORE CLIENT               
         B     SFMERROR                                                         
         MVC   PNLKCLT,SFMCLNT     CLIENT                                       
*                                                                               
CKSCRL   MVI   NWINSERT,0          NO INSERT LINE YET                           
         MVI   NWINSRTN,0                                                       
         LA    R2,SFMSCRLH                                                      
         CLI   5(R2),0                                                          
         BNE   *+8                                                              
         MVI   8(R2),C'P'                                                       
         CLI   8(R2),C'P'                                                       
         BNE   CKSCRL10                                                         
         MVC   8(L'SFMSCRL,R2),=C'PG'                                           
         OI    6(R2),X'80'                                                      
         MVI   SCRLAMNT,SCRLINES                                                
         B     CKPFKEYS                                                         
CKSCRL10 CLI   8(R2),C'H'                                                       
         BNE   CKSCRL20                                                         
         MVC   8(L'SFMSCRL,R2),=C'HF'                                           
         OI    6(R2),X'80'                                                      
         MVI   SCRLAMNT,SCRLINES/2                                              
         B     CKPFKEYS                                                         
CKSCRL20 TM    4(R2),X'08'                                                      
         BO    *+14                                                             
         MVC   GERROR,=AL2(NOTNUM)                                              
         B     SFMERROR                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
         CVB   R1,DUB                                                           
         CH    R1,=H'14'                                                        
         BNH   *+14                                                             
ERNUMLIN MVC   GERROR,=AL2(NUMLINER)                                            
         B     SFMERROR                                                         
         LTR   R1,R1                                                            
         BZ    ERNUMLIN                                                         
         STC   R1,SCRLAMNT                                                      
* ANY PFKEYS?                                                                   
CKPFKEYS CLI   PFKEY,5             TOP PFKEY?                                   
         BE    PFTOP                                                            
         CLI   PFKEY,6             BOTTOM PFKEY?                                
         BE    PFBOT                                                            
         CLI   PFKEY,7             UP PFKEY?                                    
         BE    PFUP                                                             
         CLI   PFKEY,8             DOWN PFKEY?                                  
         BE    PFDOWN                                                           
         CLI   PFKEY,9             ERASE DETAILS PFKEY?                         
         BE    PFERASE                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         B     VR                  VALIDATE RECORD IF NOT VALIDATE KEY          
         DROP  R8                  FOR THE MINIO MASTER KEY                     
         EJECT                                                                  
*                                                                               
* VK: VALIDATE KEY                                                              
*                                                                               
VK       DS    0H                                                               
         MVI   ELCDBEG,PNLELTCQ    EVERY NEW KEY HAS A NEW BEGINNING            
         XC    BEGSEQ,BEGSEQ                                                    
         MVI   ELCDEND,PNLELTCQ    BUT NO END UNTIL DISPLAYED                   
         XC    ENDSEQ,ENDSEQ                                                    
         MVI   INSERTLN,0          NO INSERT POSSIBLE                           
         MVI   INSERTNM,0                                                       
         XC    AWHERE,AWHERE       NO A(WHERE) OF ACTION                        
         MVI   BEFAFTER,0          NO BEFORE/AFTER                              
         MVI   ARNG1COD,0                                                       
         XC    ARANGE1,ARANGE1     NO A(TOP) SEQUENCE                           
         MVI   ARNG2COD,0                                                       
         XC    ARANGE2,ARANGE2     NO A(BOTTOM) SEQUENCE                        
         MVI   SELACTN,0           NO SELECTION ACTION                          
         GOTO1 =A(DR),DMCB,(RC),(RA),(R5),(R6),(R9),RR=RELO                     
         EJECT                                                                  
*                                                                               
* VR: VALIDATE RECORD                                                           
*                                                                               
VR       DS    0H                                                               
         XC    PREVSEQ,PREVSEQ     STARTING SO NO PREVIOUS SEQUENCE             
         MVI   CHANGED,0           NOTHING WAS CHANGED                          
         MVI   LINENO,1            CHECK THE FIRST LINE                         
         L     R6,MINELEM          POINT TO THE ELEMENT                         
         USING PNLELTD,R6                                                       
         LA    R2,SFMSTH           POINT TO THE FIRST LINE                      
*                                                                               
VRLP     XC    0(100,R6),0(R6)     CLEAR THE ELEMENT TABLE                      
         NI    CHANGED,X'80'       LEAVE RECORD CHANGED BIT ALONE               
         MVI   TYPEFLAG,0          NO TYPE YET                                  
         MVI   PNLELTC,PNLELTCQ    SET EQUATES OF ELEMENT CODES                 
         MVI   PNLELTL,PNLELTLQ       AND LENGTH                                
* TEST DATATYPE                                                                 
VR00     CLI   CONREC,C'S'         SPANEL                                       
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMENTH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULENTH                                                       
         CLI   8(R3),0             ANY DATA FOR DATATYPE?                       
         BNE   VR00A               YES                                          
         CLC   LINENO,INSERTLN     BLANKED OUT INSERT LINE?                     
         BL    CKSELECT            NO, REGULAR BLANK.  CHECK SELECTS            
         ZICM  R1,INSERTLN,1                                                    
         BZ    CKSELECT                                                         
         ZIC   R0,INSERTNM                                                      
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         CLM   R1,1,LINENO                                                      
         BL    CKSELECT            NO, AFTER IT                                 
         OI    TYPEFLAG,X'80'      NO SEQ #, SO NO PREV SEQ # LATER             
         B     VRNEXT                                                           
VR00A    TM    4(R3),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                 YES, NO CHANGE                               
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
*     CHECK IF PFKEY                                                            
         CLC   =C'PF',8(R3)        PFKEY?                                       
         BNE   VRCK00              NO, USER LINE?                               
         OI    TYPEFLAG,X'80'      INDICATE WE HAVE A PFKEY                     
         B     VR05                                                             
*     CHECK IF TEXT ONLY                                                        
VRCK00   CLC   =C'TEXT',8(R3)      LABEL ONLY, DATA LENGTH = 0                  
         BNE   VRCK01              NO, CHECK IF USER FIELD                      
         CLI   CONREC,C'S'         MULTIPLE?                                    
         BE    VRCK00A             YES                                          
         USING MULFAD,R2                                                        
         LA    R2,MULENTH                                                       
         MVC   GERROR,=AL2(SPANONLY)  TEXT ONLY LABELS FOR SPANEL               
         B     SFMERROR            ONLY MULTIPLE PANEL USER LINES               
VRCK00A  OI    TYPEFLAG,X'08'      INDICATE WE HAVE A USER LINE                 
         B     VR05                                                             
*     CHECK IF USER LINE                                                        
VRCK01   CLC   =C'USER',8(R3)      USER LINE?                                   
         BNE   VRCK02              NO, CHECK IF SELECT FIELD                    
         CLI   CONREC,C'M'         MULTIPLE?                                    
         BE    VRCK01A             YES                                          
         USING SFMFAD,R2                                                        
         LA    R2,SFMENTH                                                       
         MVC   GERROR,=AL2(MPANONLY)  USER LINES ARE ONLY FOR MPANEL            
         B     SFMERROR            ONLY MULTIPLE PANEL USER LINES               
VRCK01A  OI    TYPEFLAG,X'40'      INDICATE WE HAVE A USER LINE                 
         B     VR05                                                             
*     CHECK IF SELECT FIELD                                                     
VRCK02   CLC   =C'SEL',8(R3)       SELECT FIELD?                                
         BNE   VRCK03              NO, CHECK IF KEY                             
         CLI   CONREC,C'M'         MULTIPLE?                                    
         BE    VRCK02A             YES                                          
         USING SFMFAD,R2                                                        
         LA    R2,SFMENTH                                                       
         MVC   GERROR,=AL2(MPANONLY)  SELECT FIELD IS ONLY FOR MPANEL           
         B     SFMERROR            ONLY MULTIPLE PANEL USER LINES               
VRCK02A  OI    TYPEFLAG,X'20'      INDICATE WE HAVE A SELECT FIELD              
         B     VR05                                                             
*     CHECK IF KEY                                                              
VRCK03   CLC   =C'KEY',8(R3)       KEY?                                         
         BNE   VRCK04              NO, REGULAR VALIDATION                       
         CLI   CONREC,C'M'         MULTIPLE?                                    
         BE    VRCK03A             YES                                          
         USING SFMFAD,R2                                                        
         LA    R2,SFMENTH                                                       
         MVC   GERROR,=AL2(MPANONLY)  KEY FIELD IS ONLY FOR MPANEL              
         B     SFMERROR                                                         
VRCK03A  OI    TYPEFLAG,X'10'      INDICATE WE HAVE A KEY FIELD                 
         B     VR05                                                             
*     CHECK IF KEY FIELD IN SUB-PANEL                                           
VRCK04   CLI   CONREC,C'S'         SPANEL                                       
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMSUBH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULSUBH                                                       
         CLI   8(R3),C'K'          KEY IN SUB-PANEL                             
         BNE   VR05                NO                                           
         CLI   CONREC,C'M'         MULTIPLE?                                    
         BE    VRCK04A             YES                                          
         LR    R2,R3                                                            
         MVC   GERROR,=AL2(MPANONLY)  KEY FIELD IS ONLY FOR MPANEL              
         B     SFMERROR                                                         
VRCK04A  OI    TYPEFLAG,X'04'      INDICATE WE HAVE A KEY IN SUB-PANEL          
* CHECK IF DELETED                                                              
VR05     CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMSELH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULSELH                                                       
         CLC   =C'DD',8(R3)        DELETE BLOCK?                                
         BE    VR10                YES, LEAVE THIS TO SELECT CHECKING           
         CLI   8(R3),C'D'          DELETE LINE OR # OF LINES?                   
         BE    VRNEXT              TREAT IT AS IF IT DOESN'T EXIST              
* TEST ROW                                                                      
VR10     DS    0H                                                               
         CLI   CONREC,C'M'         MPANEL?                                      
         BE    VR20                YES, NO ROW NOR COL                          
         TM    TYPEFLAG,X'F0'      THESE TYPES HAVE NO ROW OR COL               
         BO    VR20                CHECK REPLICATION ID                         
         USING SFMFAD,R2           ONLY SPANEL RECORD HAVE ROW AND COL          
         TM    SFMROWH+4,X'20'     PREVIOUSLY VALIDATED?                        
         BNZ   *+8                 YES, NOTHING CHANGED HERE                    
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
* TEST COLUMN                                                                   
         TM    SFMCOLH+4,X'20'     PREVIOUSLY VALIDATED?                        
         BNZ   *+8                 YES, NOTHING CHANGED HERE                    
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
* TEST REPLICATION ID          SFMRPI                                           
VR20     DS    0H                                                               
         TM    TYPEFLAG,X'FC'      THESE TYPES HAVE NO REPLICATION ID           
         BNZ   VR30                                                             
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMRPIH          YES                                          
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULRPIH                                                       
         TM    4(R3),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VR30                                                             
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
* TEST NAME LENGTH             SFMNLN                                           
VR30     DS    0H                                                               
         TM    TYPEFLAG,X'F8'      THESE TYPES HAVE NO NAME LENGTH              
         BNZ   VR40                                                             
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   VR40                NO                                           
         USING SFMFAD,R2                                                        
         TM    SFMNLNH+4,X'20'     PREVIOUSLY VALIDATED?                        
         BNZ   VR40                                                             
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
* TEST NAME                                                                     
VR40     DS    0H                                                               
         TM    TYPEFLAG,X'50'      THESE FIELDS HAVE NO NAME                    
         BNZ   VR60                                                             
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMOVRH          YES                                          
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULTOPH                                                       
         TM    4(R3),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VR50                                                             
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
VR50     CLI   CONREC,C'S'         SPANEL?                                      
         BE    VR60                YES, NO BOTTOM                               
         TM    TYPEFLAG,X'80'      PFKEY HAS NO BOTTOM NAME                     
         BNZ   VR60                                                             
         TM    MULBOTH+4,X'20'     PREVIOUSLY VALIDATED?                        
         BNZ   VR60                YES                                          
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
* TEST PROTECTED/UNPROTECTED    SFMPRO                                          
VR60     DS    0H                                                               
         TM    TYPEFLAG,X'E8'      THESE FIELDS HAVE OWN PROTECTION             
         BNZ   VR70                                                             
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMPROH          YES                                          
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULPROH                                                       
         TM    4(R3),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VR70                YES                                          
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
* TEST VALID NAME INTENSITY    SFMNIN                                           
VR70     DS    0H                                                               
         TM    TYPEFLAG,X'40'      USER LINE?                                   
         BNZ   VR80                YES, NO NAME.  CHECK DATA INTENSITY          
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMNINH          YES                                          
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULNINH                                                       
         TM    4(R3),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VR80                                                             
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
* TEST VALID DATA INTENSITY    SFMDIN                                           
VR80     DS    0H                                                               
         TM    TYPEFLAG,X'88'      THESE HAVE NO DATA SO NO INTENSITY           
         BNZ   VR90                                                             
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMDINH          YES                                          
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULDINH                                                       
         TM    4(R3),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VR90                                                             
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
* TEST VALID UPPER/LOWER CASE          SFMLOW                                   
VR90     DS    0H                                                               
         TM    TYPEFLAG,X'88'      THESE HAVE NO DATA SO NO UPPER/LOWER         
         BNZ   VR100                                                            
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMLOWH          YES                                          
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULLOWH                                                       
         TM    4(R3),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VR100               YES                                          
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
* TEST SUB-PANEL                                                                
VR100    DS    0H                                                               
         TM    TYPEFLAG,X'F0'      THESE HAVE NO SUB-PANEL                      
         BNZ   VR110                                                            
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMSUBH                                                       
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R3,MULSUBH                                                       
         TM    4(R3),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VR110                                                            
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
* TEST FIXED/SCROLL                                                             
VR110    DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BE    VR115               CHECK FOR ID NUMBER                          
         TM    TYPEFLAG,X'F4'      THESE ARE ALWAYS FIXED                       
         BNZ   VR110A              CHECK BELOW                                  
         USING MULFAD,R2                                                        
         TM    MULFIXH+4,X'20'                                                  
         BNZ   *+8                                                              
         OI    CHANGED,X'01'                                                    
* TEST BELOW LINE                                                               
VR110A   TM    TYPEFLAG,X'F0'      THESE ARE NOT BELOW                          
         BNZ   VR115                                                            
         TM    MULBELH+4,X'20'                                                  
         BNZ   *+8                                                              
         OI    CHANGED,X'01'                                                    
* EXTENDED HEADER ID NUMBER                                                     
VR115    TM    TYPEFLAG,X'C8'      THESE HAVE NO HEADER ID NUMBER               
         BNZ   VR120               PROTECTED                                    
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMIDNH                                                       
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R3,MULIDNH                                                       
         TM    4(R3),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VR120                                                            
         OI    CHANGED,X'01'       SET ELEMENT CHANGED/ADDED                    
*                                                                               
* TEST IF NEW LINE.  IF NO SEQUENCE NUMBER GIVE IT ONE, ELSE IT IS A            
* CHANGE.                                                                       
*                                                                               
VR120    DS    0H                                                               
         TM    CHANGED,X'01'       DID THE LINE CHANGE?                         
         BNZ   VR123               YES IT DID                                   
         CLC   LINENO,INSERTLN     NO CHANGE IN INSERT LINE?                    
         BL    VR126               NOT AN INSERT LINE                           
         ZICM  R1,INSERTLN,1                                                    
         BZ    VR126                                                            
         ZIC   R0,INSERTNM                                                      
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         CLM   R1,1,LINENO                                                      
         BL    VR126               AFTER INSERT LINE                            
         OI    TYPEFLAG,X'80'      BLANK INSERT LINES HAVE NO SEQ #             
         B     VRNEXT              DEL. INSERTED LINE BECAUSE NO CHANGE         
* CHANGED SO BUILD THE ELEMENT                                                  
VR123    GOTO1 =A(BLDELEM),DMCB,(RC),(RA),(R2),(R5),(R6),(R9),RR=RELO           
VR126    DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMSEQH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULSEQH                                                       
         CLI   12(R3),0            LAST BYTE OF SEQ TELLS IF OLD.  NEW?         
         BE    ADDITION            YES                                          
         TM    TYPEFLAG,X'80'      PFKEY ENTERED OR CHANGED?                    
         BNZ   VR128               YES, NO SEQUENCE                             
         GOTO1 HEXIN,DMCB,8(R3),PNLESEQ,L'SFMSEQ-1                              
VR128    TM    CHANGED,X'01'       IS THIS A CHANGED LINE?                      
         BNZ   CHANGE              YES? READ ELEM, CHANGE, THEN WRITE           
*                                                                               
VRNEXT   DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+16                                                             
         USING SFMFAD,R2                                                        
         LA    R1,SFMLSLNH         A(LAST LINE)                                 
         LA    R0,SFMFADX                                                       
         B     *+12                                                             
         USING MULFAD,R2                                                        
         LA    R1,MULLSLNH         A(LAST LINE)                                 
         LA    R0,MULFADX                                                       
         CR    R1,R2               CHECK IF NO MORE LINES AVAILABLE             
         BE    CKSELECT            END DISPLAY - OVER SCREEN                    
         IC    R1,LINENO           NEXT LINE                                    
         LA    R1,1(R1)                                                         
         STC   R1,LINENO                                                        
         AR    R2,R0                                                            
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BNZ   VRLP                YES, PFKEYS HAVE NO SEQ #                    
         MVC   PREVSEQ,PNLESEQ     NEW PREVIOUS SEQUENCE                        
         B     VRLP                                                             
         EJECT                                                                  
* ADDITION: PREVSEQ + 1                                                         
*            IN THE CASE OF A COLLISION, RESEQUENCE ALL THE ELEMENTS            
ADDITION DS    0H                                                               
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BNZ   ADD20               YES, NO SEQUENCE NUMBER                      
         OC    PREVSEQ,PREVSEQ     ANY SEQUENCE PREVIOUS TO THIS                
         BNZ   ADD15                                                            
         MVC   MELEM2,MELEM        MAKE A COPY FIRST                            
         MVI   MINEKEY,PNLPFDEQ    GET ELEMENT AFTER LAST SEQUENCE              
         XC    MINEKEY+1(2),MINEKEY+1                                           
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            ANY?                                         
         BE    ADD05               YES                                          
         CLI   MINERR,MINERNF      RECORD NON-EXISTANT?                         
         BE    ADD10               YES                                          
         CLI   MINERR,MINESNF      RECORD SET NON-EXISTANT?                     
         BE    ADD10               YES                                          
         CLI   MINERR,MINEEOF      NO X'30' ELEMENTS AND NO PREV SEQ            
         BE    ADD10               MUST BE FIRST ELEMENT IN RECORD              
         DC    H'0'                                                             
ADD05    GOTO1 MINIO,DMCB,('MINBSQ',(R5))                                       
         CLI   MINERR,0            ANY X'20' ELEMENTS?                          
         BE    ADD08               YES                                          
         CLI   MINERR,MINEEOF      NO X'20' ELEMENTS                            
         BE    ADD10               MUST BE FIRST ELEMENT IN RECORD              
         DC    H'0'                                                             
ADD08    MVC   PREVSEQ,PNLESEQ     COPY LAST SEQUENCE NUMBER                    
ADD10    MVC   MELEM,MELEM2                                                     
ADD15    ZICM  R1,PREVSEQ,2        LOAD THE PREVIOUS SEQUENCE NUMBER            
         LA    R1,1(R1)            ADD 1, CAN RESEQUENCE IF NEEDED              
         STCM  R1,3,PNLESEQ                                                     
ADD20    GOTO1 MINIO,DMCB,('MINADD',(R5))  ADD THE ELEMENT                      
         CLI   MINERR,0            NO ERRORS?                                   
         BE    ADD30               NONE                                         
         CLI   MINERR,MINEDUP      DUPLICATE?                                   
         BE    *+6                                                              
         DC    H'0'                SMOE OTHER ERROR                             
         TM    TYPEFLAG,X'80'      PFKEY DUPLICATE?                             
         BNZ   DUPPFKEY            YES                                          
         MVC   MELEM2,MELEM        MAKE A COPY OF THE ELEMENT                   
         IC    R0,LINENO                                                        
         GOTO1 =A(RESEQNCE),DMCB,(RC),(RA),(R5),(R6),(R9),RR=RELO               
         STC   R0,LINENO                                                        
         B     ADD10               TRY ADDING IT AGAIN                          
ADD30    CLI   CONREC,C'S'         SPANEL                                       
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMSEQH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULSEQH                                                       
         USING PNLPFD,R6                                                        
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BZ    ADD35               NO                                           
         MVC   8(2,R3),=C'PF'                                                   
         ZIC   R1,PNLPFNUM         LOAD UP PFKEY NUMBER                         
         CVD   R1,DUB                                                           
         UNPK  10(2,R3),DUB(8)                                                  
         OI    11(R3),X'F0'                                                     
         B     VRNEXT                                                           
         USING PNLELTD,R6                                                       
ADD35    GOTO1 HEXOUT,DMCB,PNLESEQ,8(R3),L'PNLESEQ                              
         B     VRNEXT                                                           
DUPPFKEY DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL                                       
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R2,SFMENTH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R2,MULENTH                                                       
         MVC   GERROR,=AL2(DUPPFNN)  DUPLICATE PFKEY                            
         B     SFMERROR                                                         
         EJECT                                                                  
CHANGE   DS    0H                                                               
         MVC   MELEM2,MELEM        MAKE A COPY                                  
         CLI   CONREC,C'S'         SPANEL                                       
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMSEQH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULSEQH                                                       
         TM    TYPEFLAG,X'80'      PFKEY NOW?                                   
         BZ    CHNGSEQ             NO, SEQUENCE                                 
         CLI   8(R3),C'P'          YES, WAS LINE CHANGED A PFKEY?               
         BE    CHNGPFK             YES                                          
* PFKEY OVER A SEQUENCE.  REALLY DELETE THE SEQ AND ADD PFKEY                   
         MVI   MINEKEY,PNLELTCQ    ELCODE=20                                    
         GOTO1 HEXIN,DMCB,8(R3),MINEKEY+1,L'SFMSEQ-1                            
         GOTO1 MINIO,DMCB,('MINRD',(R5))   READ THE RECORD                      
         CLI   MINERR,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         GOTO1 MINIO,DMCB,('MINDEL',(R5))  DELETE THE ELEMENT                   
         CLI   MINERR,0            NO ERRORS?                                   
         BE    *+6                 NONE                                         
         DC    H'0'                                                             
         USING PNLPFD,R6                                                        
         MVC   MELEM,MELEM2        COPY WHAT IT IS CHANGED TO                   
         GOTO1 MINIO,DMCB,('MINADD',(R5))  ADD THE ELEMENT                      
         CLI   MINERR,0            NO ERRORS?                                   
         BE    CHANGE01            NONE                                         
         CLI   MINERR,MINEDUP      ONLY A DUPLICATE PFKEY?                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DUPPFKEY            YES                                          
CHANGE01 MVC   8(2,R3),=C'PF'      SIGNIFY PFKEY                                
         ZIC   R1,PNLPFNUM         LOAD UP PFKEY NUMBER                         
         CVD   R1,DUB                                                           
         UNPK  10(2,R3),DUB(8)                                                  
         OI    11(R3),X'F0'                                                     
         B     VRNEXT              NONE                                         
         EJECT                                                                  
* PFKEY OVER PFKEY.  IS IT THE SAME PFKEY?                                      
CHNGPFK  DS    0H                                                               
         PACK  DUB(8),10(2,R3)     SEE WHAT PFKEY OLD ONE WAS                   
         CVB   R0,DUB                                                           
         CLM   R0,1,PNLPFNUM                                                    
         BE    CHNGPFKY            SAME PFKEY WERE CHANGING                     
* PFKEY OVER DIFF PFKEY.  REALLY DELETE OLD PFKEY ADD NEW PFKEY                 
         MVI   MINEKEY,PNLPFDEQ    ELCODE=30                                    
         STC   R0,MINEKEY+1                                                     
         MVI   MINEKEY+2,0                                                      
         GOTO1 MINIO,DMCB,('MINRD',(R5))   READ THE RECORD                      
         CLI   MINERR,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         GOTO1 MINIO,DMCB,('MINDEL',(R5))  DELETE THE ELEMENT                   
         CLI   MINERR,0            NO ERRORS?                                   
         BE    *+6                 NONE                                         
         DC    H'0'                                                             
         MVC   MELEM,MELEM2        COPY WHAT IT IS CHANGED TO                   
         GOTO1 MINIO,DMCB,('MINADD',(R5))  ADD THE ELEMENT                      
         CLI   MINERR,0            NO ERRORS?                                   
         BE    CHANGE02            NONE                                         
         CLI   MINERR,MINEDUP      DUPLICATE PFKEY?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DUPPFKEY            YES                                          
CHANGE02 MVC   8(2,R3),=C'PF'      SIGNIFY PFKEY                                
         ZIC   R1,PNLPFNUM         LOAD UP PFKEY NUMBER                         
         CVD   R1,DUB                                                           
         UNPK  10(2,R3),DUB(8)                                                  
         OI    11(R3),X'F0'                                                     
         B     VRNEXT              NONE                                         
         SPACE 2                                                                
* NEW INFORMATION FOR THAT PFKEY                                                
CHNGPFKY MVI   MINEKEY,PNLPFDEQ                                                 
         MVC   MINEKEY+1(1),PNLPFNUM                                            
         MVI   MINEKEY+2,0                                                      
         GOTO1 MINIO,DMCB,('MINRD',(R5))   READ THE RECORD                      
         CLI   MINERR,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         MVC   MELEM,MELEM2        COPY NEW DATA                                
         GOTO1 MINIO,DMCB,('MINWRT',(R5))  ADD THE ELEMENT                      
         CLI   MINERR,0            NO ERRORS?                                   
         BE    VRNEXT              NONE                                         
         DC    H'0'                                                             
         EJECT                                                                  
         USING PNLELTD,R6                                                       
CHNGSEQ  DS    0H                  R3=A(???SEQH) FROM ABOVE                     
         CLI   8(R3),C'P'          PFKEY WAS HERE?                              
         BNE   CHNGSEQN            NO                                           
* SEQUENCE OVER PFKEY.  REALLY DELETE OLD PFKEY ADD SEQUENCE                    
         PACK  DUB(8),10(2,R3)     SEE WHAT OLD PFKEY WAS                       
         CVB   R0,DUB                                                           
         MVI   MINEKEY,PNLPFDEQ                                                 
         STC   R0,MINEKEY+1                                                     
         MVI   MINEKEY+2,0                                                      
         GOTO1 MINIO,DMCB,('MINRD',(R5))   READ THE RECORD                      
         CLI   MINERR,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         GOTO1 MINIO,DMCB,('MINDEL',(R5))  DELETE THE ELEMENT                   
         CLI   MINERR,0            NO ERRORS?                                   
         BE    *+6                 NONE                                         
         DC    H'0'                                                             
         MVC   MELEM,MELEM2        COPY WHAT IT IS CHANGED TO                   
         ICM   R1,3,PREVSEQ        WHAT WAS THE LAST SEQUENCE USED?             
         AH    R1,=H'100'          CALCULATE NEXT SEQUENCE                      
         STCM  R1,3,PNLESEQ                                                     
         GOTO1 MINIO,DMCB,('MINADD',(R5))  ADD THE ELEMENT                      
         CLI   MINERR,0            NO ERRORS?                                   
         BE    *+6                 NONE                                         
         DC    H'0'                                                             
         GOTO1 HEXOUT,DMCB,PNLESEQ,8(R3),L'PNLESEQ                              
         B     VRNEXT                                                           
         SPACE 2                                                                
* NEW INFORMATION FOR THAT SEQUENCE NUMBER                                      
CHNGSEQN MVI   MINEKEY,PNLELTCQ    ELCODE=20                                    
         MVC   MINEKEY+1(2),PNLESEQ                                             
         GOTO1 MINIO,DMCB,('MINRD',(R5))   READ THE RECORD                      
         CLI   MINERR,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         MVC   MELEM,MELEM2        COPY NEW DATA                                
         GOTO1 MINIO,DMCB,('MINWRT',(R5))  ADD THE ELEMENT                      
         CLI   MINERR,0            NO ERRORS?                                   
         BE    VRNEXT              NONE                                         
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* CHECK THE SELECT FIELDS                                                       
*                                                                               
CKSELECT DS    0H                                                               
         XC    PREVSEQ,PREVSEQ     STARTING SO NO PREVIOUS SEQUENCE             
         MVI   LINENO,1            CHECK THE FIRST LINE                         
         L     R6,MINELEM          POINT TO THE ELEMENT                         
         USING PNLELTD,R6                                                       
         LA    R2,SFMSTH           POINT TO THE FIRST LINE                      
*                                                                               
CKSELLP  XC    0(100,R6),0(R6)     CLEAR THE ELEMENT TABLE                      
         NI    CHANGED,X'80'       LEAVE RECORD CHANGED BIT ALONE               
         MVI   TYPEFLAG,0          NO TYPE YET                                  
* TEST DATATYPE                                                                 
         CLI   CONREC,C'S'         SPANEL                                       
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMENTH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULENTH                                                       
         CLI   8(R3),0             ANY DATA FOR DATATYPE?                       
         BNE   CKSELLP1            YES                                          
         CLC   LINENO,INSERTLN     BLANK OUT INSERT LINE?                       
         BL    VRX                 NO, REGULAR BLANK.  EXIT NOW                 
         ZICM  R1,INSERTLN,1                                                    
         BZ    VRX                                                              
         ZIC   R0,INSERTNM                                                      
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         CLM   R1,1,LINENO                                                      
         BL    VRX                 NO, AFTER IT                                 
         OI    TYPEFLAG,X'80'      NO SEQ #, SO NO PREV SEQ # LATER             
         B     CKSELNXT                                                         
*     CHECK IF PFKEY (ONLY THING NEEDED FOR SELECTS)                            
CKSELLP1 CLC   =C'PF',8(R3)        PFKEY?                                       
         BNE   CKSEL05             NO, USER LINE?                               
         OI    TYPEFLAG,X'80'      INDICATE WE HAVE A PFKEY                     
* CHECK THE SELECT ENTRY                                                        
CKSEL05  CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMSELH                                                       
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R3,MULSELH                                                       
         CLI   8(R3),0             NULL?                                        
         BE    CKSELNXT            NOTHING SPECIAL TO DO                        
         CLI   8(R3),C'A'          AFTER?                                       
         BE    BEFRAFTR            YES                                          
         CLI   8(R3),C'B'          BEFORE?                                      
         BE    BEFRAFTR            YES                                          
         CLC   =C'CC',8(R3)        COPY BLOCK?                                  
         BE    CPMVBLCK            YES                                          
         CLI   8(R3),C'C'          COPY?                                        
         BE    COPYMOVE            YES                                          
         CLC   =C'DD',8(R3)        DELETE BLOCK?                                
         BE    DELEBLCK            YES                                          
         CLI   8(R3),C'D'          DELETE?                                      
         BE    DELETE              YES                                          
         CLI   8(R3),C'I'          INSERT?                                      
         BE    INSERT              YES                                          
         CLC   =C'MM',8(R3)        MOVE BLOCK?                                  
         BE    CPMVBLCK            YES                                          
         CLI   8(R3),C'M'          MOVE?                                        
         BE    COPYMOVE            YES                                          
         CLC   =C'RR',8(R3)        REPLICATE BLOCK?                             
         BE    REPLBLCK            YES                                          
         CLI   8(R3),C'R'          REPLICATE?                                   
         BE    REPLICTE            YES                                          
CKSELERR LR    R2,R3                                                            
         B     BADERR              NO OTHER SELECT AVAILABLE NOW                
CKSELNXT DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+16                                                             
         USING SFMFAD,R2                                                        
         LA    R1,SFMLSLNH         A(LAST LINE)                                 
         LA    R0,SFMFADX                                                       
         B     *+12                                                             
         USING MULFAD,R2                                                        
         LA    R1,MULLSLNH         A(LAST LINE)                                 
         LA    R0,MULFADX                                                       
         CR    R1,R2               CHECK IF NO MORE LINES AVAILABLE             
         BE    VRX                 END OF DISPLAY                               
         IC    R1,LINENO           NEXT LINE                                    
         LA    R1,1(R1)                                                         
         STC   R1,LINENO                                                        
         AR    R2,R0                                                            
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BNZ   CKSELLP             YES, PFKEYS HAVE NO SEQ #                    
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMSEQH                                                       
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R3,MULSEQH                                                       
         GOTO1 HEXIN,DMCB,0(R3),PREVSEQ,L'SFMSEQ-1 NEW REVIOUS SEQUENCE         
         B     CKSELLP                                                          
VRX      DS    0H                                                               
         OC    ARANGE1,ARANGE1     A(FROM)?                                     
         BZ    VRX1                NOTHING                                      
         CLI   SELACTN,C'C'                                                     
         BE    VRXWHERE                                                         
         CLI   SELACTN,C'M'                                                     
         BE    VRXWHERE                                                         
         OC    ARANGE2,ARANGE2     MUST BE A BLOCK COMMAND                      
         BZ    VRX1                INCOMPLETE BLOCK COMMAND                     
         CLI   SELACTN,C'R'        REPLICATE BLOCK?                             
         BE    DORPLBLK                                                         
         CLI   SELACTN,C'D'        DELETE BLOCK?                                
         BE    DODELBLK                                                         
VRXWHERE OC    AWHERE,AWHERE       A(WHERE TO GO)?                              
         BZ    VRX1                NOTHING                                      
         CLI   SELACTN,C'C'        COPY LINE(S)?                                
         BE    DOCPMOVE                                                         
         CLI   SELACTN,C'M'        MOVE LINE(S)?                                
         BE    DOCPMOVE                                                         
         CLI   SELACTN,C'B'        COPY BLOCK?                                  
         BE    CPYMVBLK                                                         
         CLI   SELACTN,C'N'        MOVE BLOCK?                                  
         BE    CPYMVBLK                                                         
         DC    H'0'                                                             
DOCPMOVE BAS   RE,DOCPYMOV         COPY/MOVE LINE(S)                            
         B     VRX1                                                             
CPYMVBLK LA    R3,SFMSTH                                                        
         CLC   AWHERE,ARANGE1      DESTINATION BEFORE THE TOP?                  
         BL    CPYMVBK1            YES                                          
         BNE   *+16                GREATER THAN TOP                             
         CLI   BEFAFTER,C'B'       IT IS THE TOP, SEND BEFORE THE TOP?          
         BE    CPYMVBK1            YES                                          
         B     DTAILERR            NO AFTER, CAUSES A LOOP                      
         CLC   AWHERE,ARANGE2      DESTINATION AFTER BOTTOM?                    
         BH    CPYMVBK1            YES                                          
         BNE   DTAILERR            IN BETWEEN, TOP AND BOTTOM.                  
         CLI   BEFAFTER,C'A'       IT'S THE BOTTOM, AFTER THE BOTTOM?           
         BNE   DTAILERR            NO BEFORE, CAUSES A LOOP                     
CPYMVBK1 BAS   RE,DOCPMVBK         COPY/MOVE BLOCK                              
         B     VRX1                                                             
DORPLBLK MVI   SELACTN,C'B'        REPLICATE BLOCK IS COPY BLOCK                
         MVC   AWHERE,ARANGE2        AFTER LAST LINE OF REPLICATE BLOCK         
         MVI   BEFAFTER,C'A'                                                    
         B     CPYMVBK1                                                         
DODELBLK BAS   RE,DELBLOCK         DELETE BLOCK                                 
VRX1     TM    CHANGED,X'80'       DID RECORD CHANGED AT ALL?                   
         BZ    VREXIT              NO                                           
         GOTO1 MINIO,DMCB,('MINCLS',(R5))  CLOSE THE RECORD BUFFER              
         CLI   MINERR,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                                                             
* REDISPLAY RECORD                                                              
VREXIT   TM    CHANGED,X'08'       ERROR CONDITION?                             
         BZ    VREXIT1                                                          
         GOTO1 MINIO,DMCB,('MINOPN',(R5))  YES, DISPLAY UNCHANGED ONE           
         B     PFERASE                                                          
VREXIT1  GOTO1 =A(DR),DMCB,(RC),(RA),(R5),(R6),(R9),RR=RELO                     
         EJECT                                                                  
PFDOWN   DS    0H                                                               
         CLI   SFMSCRL,C'P'                                                     
         BNE   *+14                                                             
         MVC   ELCDBEG(L'ELCDBEG+L'BEGSEQ),ELCDEND   NEXT PAGE                  
         B     PFCLEAR                                                          
         CLC   =X'0000',BEGSEQ     NO ENTRIES?                                  
         BE    PFCLEAR             FORGET ABOUT IT                              
         ZIC   R4,SCRLAMNT         # OF LINES TO SCROLL                         
         L     R6,MINELEM          POINT TO THE ELEMENT                         
         USING PNLELTD,R6                                                       
         MVC   MINEKEY(L'ELCDBEG+L'BEGSEQ),ELCDBEG   FIRST LINE                 
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
PFDNLOOP GOTO1 MINIO,DMCB,('MINSEQ',(R5))  GO TIL EOF() OR LINES                
         CLI   MINERR,0                                                         
         BE    PFDNNEXT                                                         
         CLI   MINERR,MINEEOF      HIT END OF RECORD?                           
         BE    PFDNSET             YES, SET THE BEGINNING THEN                  
         DC    H'0'                                                             
PFDNSET  CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BE    PFDN00              YES                                          
         MVI   ELCDBEG,PNLELTCQ    NO, REGULAR SEQUENCE ELEMENT                 
         MVC   BEGSEQ,PNLESEQ      NEW BEGINNING IS LAST ELEMENT                
         B     PFCLEAR             DISPLAY AND DON'T DO ANYTHING ELSE           
PFDN00   MVI   ELCDBEG,PNLPFDEQ    YES                                          
         USING PNLPFD,R6                                                        
         MVC   BEGSEQ(1),PNLPFNUM  NEW BEGINNING IS LAST ELEMENT                
         MVI   BEGSEQ+1,X'00'                                                   
         B     PFCLEAR             DISPLAY AND DON'T DO ANYTHING ELSE           
         USING PNLELTD,R6                                                       
PFDNNEXT BCT   R4,PFDNLOOP                                                      
         B     PFDNSET                                                          
PFCLEAR  MVI   PFKEY,0             CLEAR PFKEY                                  
* DISPLAY AND DON'T DO ANYTHING ELSE                                            
         GOTO1 =A(DR),DMCB,(RC),(RA),(R5),(R6),(R9),RR=RELO                     
         SPACE 2                                                                
PFUP     DS    0H                                                               
         CLC   =X'0000',BEGSEQ     NO ENTRIES?                                  
         BE    PFCLEAR             FORGET ABOUT IT                              
         ZIC   R4,SCRLAMNT         # OF LINES TO GO UP                          
         L     R6,MINELEM          POINT TO THE ELEMENT                         
         USING PNLELTD,R6                                                       
         MVC   MINEKEY(1),ELCDBEG    ELEMENT CODE OF FIRST LINE                 
         MVC   MINEKEY+1(2),BEGSEQ   START FROM THE FIRST LINE UP               
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
PFUPLOOP GOTO1 MINIO,DMCB,('MINBSQ',(R5))  BACK TIL BOF() OR 13 LINES           
         CLI   MINERR,0                                                         
         BE    PFUPNEXT                                                         
         CLI   MINERR,MINEEOF      HIT BEGINNING OF RECORD?                     
         BE    PFUPSET             YES, SET THE BEGINNING THEN                  
         DC    H'0'                                                             
PFUPSET  CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BE    PFUP00                                                           
         MVI   ELCDBEG,PNLELTCQ    NO, REGULAR SEQUENCE ELEMENT                 
         MVC   BEGSEQ,PNLESEQ      NEW BEGINNING IS LAST ELEMENT                
         B     PFCLEAR             DISPLAY AND DON'T DO ANYTHING ELSE           
PFUP00   MVI   ELCDBEG,PNLPFDEQ    YES                                          
         USING PNLPFD,R6                                                        
         MVC   BEGSEQ(1),PNLPFNUM  NEW BEGINNING IS LAST ELEMENT                
         MVI   BEGSEQ+1,X'00'                                                   
         B     PFCLEAR             DISPLAY AND DON'T DO ANYTHING ELSE           
         USING PNLELTD,R6                                                       
PFUPNEXT BCT   R4,PFUPLOOP                                                      
         B     PFUPSET                                                          
         EJECT                                                                  
PFTOP    DS    0H                                                               
         MVI   ELCDBEG,PNLELTCQ    DEFAULT IS REGULAR                           
         XC    BEGSEQ,BEGSEQ       START AT VERY TOP                            
         B     PFCLEAR             DISPLAY AND DON'T DO ANYTHING ELSE           
         SPACE 2                                                                
PFBOT    DS    0H                                                               
         CLC   =X'0000',ENDSEQ     NO ENTRIES?                                  
         BE    PFCLEAR             FORGET ABOUT IT                              
         L     R6,MINELEM          POINT TO THE ELEMENT                         
         USING PNLELTD,R6                                                       
         MVC   MINEKEY(1),ELCDEND                                               
         MVC   MINEKEY+1(2),ENDSEQ   START FROM THE LAST LINE SEQ               
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
PFBOTLP  GOTO1 MINIO,DMCB,('MINSEQ',(R5))  READ UNTIL EOF()                     
         CLI   MINERR,0                                                         
         BE    PFBOTLP                                                          
         CLI   MINERR,MINEEOF      HIT EOF()?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BE    PFBOT00                                                          
         MVI   ELCDBEG,PNLELTCQ    NO, REGULAR SEQUENCE ELEMENT                 
         MVC   BEGSEQ,PNLESEQ      NEW BEGINNING IS LAST ELEMENT                
         B     PFCLEAR             DISPLAY AND DON'T DO ANYTHING ELSE           
PFBOT00  MVI   ELCDBEG,PNLPFDEQ    YES                                          
         USING PNLPFD,R6                                                        
         MVC   BEGSEQ(1),PNLPFNUM  NEW BEGINNING IS LAST ELEMENT                
         MVI   BEGSEQ+1,X'00'                                                   
         B     PFCLEAR             DISPLAY AND DON'T DO ANYTHING ELSE           
         SPACE 2                                                                
PFERASE  XC    AWHERE,AWHERE       NO A(WHERE)                                  
         MVI   BEFAFTER,0          NO BEFORE/AFTER                              
         MVI   ARNG1COD,0                                                       
         XC    ARANGE1,ARANGE1     NO A(TOP)                                    
         MVI   ARNG2COD,0                                                       
         XC    ARANGE2,ARANGE2     NO A(BOTTOM)                                 
         MVI   SELACTN,0           NO SELECTION ACTION                          
         B     PFCLEAR             DISPLAY AND DON'T DO ANYTHING ELSE           
         USING PNLELTD,R6                                                       
         EJECT                                                                  
DELETE   DS    0H                                                               
         CLI   5(R3),1             JUST A 'D'?                                  
         BNE   *+12                                                             
         MVI   NUMLINES,1          JUST ONE LINE                                
         B     DODELETE                                                         
         LA    R4,9(R3)            POINT AFTER THE 'D'                          
         ZIC   R1,5(R3)                                                         
         BCTR  R1,0                -1 FOR 'D'                                   
DELTSTLP CLI   0(R4),C'0'                                                       
         BL    CKSELERR            NOT VALID NUMERIC                            
         CLI   0(R4),C'9'                                                       
         BH    CKSELERR                                                         
         LA    R4,1(R4)            NEXT CHARACTER                               
         BCT   R1,DELTSTLP                                                      
         IC    R1,5(R3)            VALID NUMERIC                                
         SH    R1,=H'2'            -1 FOR 'D', -1 FOR PACK                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),9(0,R3)                                                   
         CVB   R1,DUB                                                           
         STC   R1,NUMLINES         # OF LINES TO DELETE                         
DODELETE CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R4,SFMSEQH                                                       
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R4,MULSEQH                                                       
         CLI   8(R4),0             ANYTHING?                                    
         BE    DELETE30            NOTHING, RECORD WASN'T CHANGED               
         OI    CHANGED,X'80'       RECORD HAS BEEN CHANGED                      
         CLC   =C'PF',8(R4)        PFKEYS?                                      
         BNE   DELETE10                                                         
         MVI   MINEKEY,PNLPFDEQ    ELCODE=30 FOR PFKEYS                         
         PACK  DUB(8),10(2,R4)                                                  
         CVB   R0,DUB                                                           
         STC   R0,MINEKEY+1                                                     
         MVI   MINEKEY+2,0         PFKEY HAVE NO BYTE FOLLOWING                 
         B     DELETE20                                                         
DELETE10 MVI   MINEKEY,PNLELTCQ    ELCODE=20                                    
         GOTO1 HEXIN,DMCB,8(R4),MINEKEY+1,L'SFMSEQ-1                            
DELETE20 GOTO1 MINIO,DMCB,('MINRD',(R5))   READ THE RECORD                      
         CLI   MINERR,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                                                             
DELET20A CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BE    DELETE25            YES, JUST DELETE                             
         CLC   AWHERE,PNLESEQ      IF A(WHERE) IS DELETED                       
         BNE   DELET20B                                                         
         XC    AWHERE,AWHERE       NO MORE DETAIL                               
         MVI   BEFAFTER,0                                                       
DELET20B CLC   ARANGE1,PNLESEQ                                                  
         BNE   DELET20C                                                         
         MVC   ARANGE1,ARANGE2     NEW A(TOP) SEQUENCE                          
         XC    ARANGE2,ARANGE2                                                  
         B     DELETE25                                                         
DELET20C CLC   ARANGE2,PNLESEQ                                                  
         BNE   DELETE25                                                         
         XC    ARANGE2,ARANGE2                                                  
DELETE25 GOTO1 MINIO,DMCB,('MINDEL',(R5))  DELETE THE ELEMENT                   
         CLI   MINERR,0            NO ERRORS?                                   
         BE    *+6                 NONE                                         
         DC    H'0'                                                             
DELETE30 ZIC   R1,LINENO           DELETING PULLS ANY INSERTS UP ONE            
         BCTR  R1,0                                                             
         STC   R1,LINENO                                                        
         IC    R1,NUMLINES                                                      
         BCT   R1,*+8                                                           
         B     CKSELNXT            NO MORE LINES TO DELETE                      
         STC   R1,NUMLINES         SAVE THE NUMBER OF LINES LEFT                
         IC    R1,LINENO                                                        
         LA    R1,1(R1)                                                         
         STC   R1,LINENO                                                        
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+16                                                             
         USING SFMFAD,R2           YES                                          
         LA    R1,SFMLSLNH                                                      
         LA    R0,SFMFADX                                                       
         B     *+12                                                             
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R1,MULLSLNH                                                      
         LA    R0,MULFADX                                                       
         CR    R2,R1               LAST LINE?                                   
         BE    DELEMORE            NO MORE ON SCREEN, MORE ON FILE              
         AR    R2,R0               NEXT LINE                                    
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMSELH                                                       
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R3,MULSELH                                                       
         CLI   8(R3),0             NOTHING IN THE SELECT FIELD                  
         BNE   CKSELERR            SHOULDN'T HAVE ANYTHING                      
         B     DODELETE                                                         
DELEMORE CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BNE   DELEMR10                                                         
         USING PNLPFD,R6           YES                                          
         MVI   MINEKEY,PNLPFDEQ                                                 
         MVI   MINEKEY+2,0                                                      
         MVC   MINEKEY+1,PNLPFNUM                                               
         B     DELEMR20                                                         
         USING PNLELTD,R6                                                       
DELEMR10 MVI   MINEKEY,PNLELTCQ    NO                                           
         MVC   MINEKEY+1(2),PNLESEQ                                             
DELEMR20 GOTO1 MINIO,DMCB,('MINHI',(R5))  READ NEXT ELEM AFTER DELETED          
         CLI   MINERR,0                                                         
         BE    DELET20A                                                         
         CLI   MINERR,MINEEOF                                                   
         BE    CKSELNXT                                                         
         DC    H'0'                SOME STRANGE ERROR                           
         EJECT                                                                  
* REPLICATE: SEQUENCE + 1                                                       
*            IN THE CASE OF A COLLISION, RESEQUENCE ALL THE ELEMENTS            
REPLICTE DS    0H                                                               
         TM    TYPEFLAG,X'80'      REPLICATE PFKEY?                             
         BNZ   CKSELERR            CAN'T, WOULD HAVE SAME NUMBER                
         CLI   5(R3),1             JUST AN 'R'?                                 
         BNE   *+12                                                             
         MVI   NUMLINES,1          JUST ONE LINE                                
         B     DOREPLCT                                                         
         LA    R4,9(R3)            POINT AFTER THE 'R'                          
         ZIC   R1,5(R3)                                                         
         BCTR  R1,0                -1 FOR 'R'                                   
REPLCTLP CLI   0(R4),C'0'                                                       
         BL    CKSELERR            NOT VALID NUMERIC                            
         CLI   0(R4),C'9'                                                       
         BH    CKSELERR                                                         
         LA    R4,1(R4)            NEXT CHARACTER                               
         BCT   R1,REPLCTLP                                                      
         IC    R1,5(R3)            VALID NUMERIC                                
         SH    R1,=H'2'            -1 FOR 'R', -1 FOR PACK                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),9(0,R3)                                                   
         CVB   R1,DUB                                                           
         STC   R1,NUMLINES         # OF LINES TO DELETE                         
DOREPLCT OI    CHANGED,X'80'       RECORD HAS BEEN CHANGED                      
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R4,SFMSEQH                                                       
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R4,MULSEQH                                                       
REPLCT00 MVI   MINEKEY,PNLELTCQ                                                 
         GOTO1 HEXIN,DMCB,8(R4),MINEKEY+1,L'SFMSEQ-1  #S WERE CHANGED           
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
REPLCT05 ZICM  R1,PNLESEQ,2        GET THE SEQUENCE NUMBER                      
         LA    R1,1(R1)            ADD 1                                        
         STCM  R1,3,PNLESEQ                                                     
         GOTO1 MINIO,DMCB,('MINADD',(R5))  ADD THE ELEMENT                      
         CLI   MINERR,0            NO ERRORS?                                   
         BE    REPLCT10            NONE                                         
         CLI   MINERR,MINEDUP      DUPLICATE SEQUENCE #?                        
         BE    *+6                 YES                                          
         DC    H'0'                ANY OTHER ERRORS THEN DIE                    
         IC    R0,LINENO           SAVE CURRENT LINE NO                         
         GOTO1 =A(RESEQNCE),DMCB,(RC),(RA),(R5),(R6),(R9),RR=RELO               
         STC   R0,LINENO                                                        
         B     REPLCT00            TRY REPLICATING AGAIN                        
REPLCT10 ZIC   R1,LINENO                                                        
         LA    R1,1(R1)                                                         
         STC   R1,LINENO                                                        
         MVC   PREVSEQ,PNLESEQ                                                  
         IC    R1,NUMLINES                                                      
         BCT   R1,*+8                                                           
         B     CKSELNXT                                                         
         STC   R1,NUMLINES                                                      
         B     REPLCT05                                                         
         EJECT                                                                  
*                                                                               
* INSERT: JUST REMEMBER WHICH LINE WE INSERTED ON                               
*                                                                               
INSERT   DS    0H                                                               
         TM    TYPEFLAG,X'80'      INSERT AFTER A PFKEY?                        
         BNZ   CKSELERR            DON'T NEED TO, PFKEYS AUTO-SORT              
         CLI   NWINSERT,0          ANY NEW INSERT LINE BEFORE THIS?             
         BNE   CKSELERR            YES, ONE AT A TIME                           
         CLI   5(R3),1             JUST AN 'I'?                                 
         BNE   *+12                                                             
         MVI   NWINSRTN,1          JUST ONE LINE                                
         B     DOINSERT                                                         
         LA    R4,9(R3)            POINT AFTER THE 'I'                          
         ZIC   R1,5(R3)                                                         
         BCTR  R1,0                -1 FOR 'I'                                   
INSERTLP CLI   0(R4),C'0'                                                       
         BL    CKSELERR            NOT VALID NUMERIC                            
         CLI   0(R4),C'9'                                                       
         BH    CKSELERR                                                         
         LA    R4,1(R4)            NEXT CHARACTER                               
         BCT   R1,INSERTLP                                                      
         IC    R1,5(R3)            VALID NUMERIC                                
         SH    R1,=H'2'            -1 FOR 'I', -1 FOR PACK                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),9(0,R3)                                                   
         CVB   R1,DUB                                                           
         STC   R1,NWINSRTN         # OF LINES TO INSERT                         
DOINSERT CLI   CONREC,C'S'         SPANEL                                       
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R0,SFMFADX                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R0,MULFADX                                                       
         CLI   LINENO,14           LAST LINE? EVERYTHING VERIFIED               
         BH    CKSELNXT            OVER LAST LINE, IGNORE IT                    
         BNE   INSERT10            NO                                           
         MVC   BEGSEQ,ENDSEQ       YES, NEXT PAGE FOR INSERT                    
         MVC   ELCDBEG,ELCDEND                                                  
         MVI   NWINSERT,2          INSERT ON 2ND LINE OF NEXT PAGE              
         LA    R1,SFMSTH                                                        
         AR    R1,R0                                                            
         OI    6(R1),X'40'         POSITION CURSOR TO THIS LINE                 
         B     CKSELNXT                                                         
INSERT10 IC    R1,LINENO           INSERT ON NEXT LINE                          
         LA    R1,1(R1)                                                         
         STC   R1,NWINSERT                                                      
         LA    R3,SFMSTH                                                        
         ZIC   R4,LINENO           NUMBER OF LINES-1 OF NEXT LINE               
         LR    R1,R0               ???FADX - LENGTH OF LINE                     
         MR    R0,R4               GET OFFSET INTO LINES                        
         AR    R3,R1               WHERE TO PUT CURSOR                          
         OI    6(R3),X'40'         POSITION CURSOR TO THIS LINE                 
         B     CKSELNXT                                                         
         EJECT                                                                  
BEFRAFTR DS    0H                                                               
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BNZ   DTAILERR            YES                                          
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R4,SFMSEQ                                                        
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R4,MULSEQ                                                        
         CLI   0(R4),0             NOTHING?                                     
         BE    DTAILERR            YES                                          
         GOTO1 HEXIN,DMCB,0(R4),ATEMP,L'SFMSEQ-1   SEQ # FOR WHERE              
         OC    AWHERE,AWHERE       SEQ. NUMBER FOR WHERE SPECIFIED?             
         BZ    BEFRAF10            NONE YET                                     
         CLC   AWHERE,ATEMP        DIFFERENT ADDRESSES?                         
         BNE   DTAILERR            YES, CONFLICT                                
BEFRAF10 MVC   AWHERE,ATEMP                                                     
         MVC   BEFAFTER,8(R3)      SIGNIFY HOW, BEFORE/AFTER                    
         B     CKSELNXT            NOTHING WAS SPECIFIED                        
*                                                                               
DTAILERR LR    R2,R3               POINT TO THE SELECT FIELD                    
         MVC   GERROR,=AL2(DETAILER)  DETAIL ERRORS                             
         B     SFMERROR                                                         
         EJECT                                                                  
COPYMOVE DS    0H                                                               
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BNZ   DTAILERR            YES                                          
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R4,SFMSEQ                                                        
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R4,MULSEQ                                                        
         CLI   0(R4),0             NOTHING?                                     
         BE    DTAILERR            YES                                          
         GOTO1 HEXIN,DMCB,0(R4),ATEMP,L'SFMSEQ-1  SEQ # FOR COPY/MOVE           
         OC    ARANGE2,ARANGE2     BLOCK COMMAND FILLED?                        
         BNZ   DTAILERR            YES, CAN'T BYPASS THAT COMMAND               
         OC    ARANGE1,ARANGE1     SEQ. NUMBER SPECIFIED?                       
         BZ    CPYMVCHK            YES, THERE IS.  CHECK ADDRESSES              
         CLC   ARANGE1,ATEMP       DIFFERENT ADDRESSES?                         
         BNE   DTAILERR            YES, CONFLICT                                
CPYMVCHK CLI   5(R3),1                                                          
         BNE   *+12                                                             
         MVI   CPYMOVLN,1                                                       
         B     CPMVSTOR                                                         
         LA    R4,9(R3)                                                         
         ZIC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
CPMVCKLP CLI   0(R4),C'0'                                                       
         BL    CKSELERR                                                         
         CLI   0(R4),C'9'                                                       
         BH    CKSELERR                                                         
         LA    R4,1(R4)                                                         
         BCT   R1,CPMVCKLP                                                      
         IC    R1,5(R3)                                                         
         SH    R1,=H'2'            -1 FOR 'C' OR 'M', -1 FOR PACK               
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),9(0,R3)                                                   
         CVB   R1,DUB                                                           
         CH    R1,=H'0'            COPY NOTHING?  (C0)?                         
         BNE   *+12                                                             
         MVI   CPYMOVLN,1          YES, COPY ONLY ONE LINE THEN                 
         B     *+8                                                              
         STC   R1,CPYMOVLN                                                      
*                                                                               
CPMVSTOR MVC   ARANGE1,ATEMP       GOOD COPY/MOVE                               
         MVC   SELACTN,8(R3)                                                    
         B     CKSELNXT                                                         
         EJECT                                                                  
CPMVBLCK DS    0H                                                               
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BNZ   DTAILERR            YES                                          
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R4,SFMSEQ                                                        
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R4,MULSEQ                                                        
         CLI   0(R4),0             NOTHING?                                     
         BE    DTAILERR            YES                                          
         CLC   =C'CC',8(R3)        COPY BLOCK?                                  
         BNE   *+12                                                             
         MVI   TMPACTN,C'B'        YES                                          
         B     *+8                                                              
         MVI   TMPACTN,C'N'        NO, MOVE BLOCK                               
         GOTO1 HEXIN,DMCB,0(R4),ATEMP,L'SFMSEQ-1  SEQ # FOR COPY/MOVE           
         OC    ARANGE1,ARANGE1     SEQ. NUMBER SPECIFIED?                       
         BZ    CPMVBLK1            NO, SAVE THE ADDRESS                         
         CLC   ARANGE1,ATEMP       DIFFERENT ADDRESSES?                         
         BNE   CPMVBLK2            YES, CHECK IF SECOND ADDRESS                 
         CLI   SELACTN,C'C'        OVERRIDE COPY LINE COMMAND?                  
         BE    CPMVBLK1            YES                                          
         CLI   SELACTN,C'M'        OVERRIDE MOVE LINE COMMAND?                  
         BE    CPMVBLK1            YES                                          
         CLC   SELACTN,TMPACTN     SAME BLOCK COMMAND?                          
         BE    CPMVBLK1            YES                                          
         OC    ARANGE2,ARANGE2     NO, BLOCK COMMAND COMPLETE?                  
         BNZ   DTAILERR            YES, CAN'T OVERRIDE IT                       
CPMVBLK1 MVC   ARANGE1,ATEMP       GOOD COPY/MOVE                               
         MVC   SELACTN,TMPACTN                                                  
         B     CKSELNXT                                                         
CPMVBLK2 CLC   SELACTN,TMPACTN     SAME BLOCK COMMAND?                          
         BNE   DTAILERR                                                         
         OC    ARANGE2,ARANGE2     YES                                          
         BZ    *+14                                                             
         CLC   ARANGE2,ATEMP                                                    
         BNE   DTAILERR                                                         
         CLC   ARANGE1,ATEMP       REVERSE THE ADDRESSES?                       
         BH    *+14                                                             
         MVC   ARANGE2,ATEMP       NO, ARANGE1 IS TOP                           
         B     CKSELNXT                                                         
         MVC   ARANGE2,ARANGE1     YES, ADDRESS IS ABOVE ARANGE1                
         MVC   ARANGE1,ATEMP                                                    
         B     CKSELNXT                                                         
         EJECT                                                                  
REPLBLCK DS    0H                                                               
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BNZ   DTAILERR            YES                                          
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R4,SFMSEQ                                                        
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R4,MULSEQ                                                        
         CLI   0(R4),0             NOTHING?                                     
         BE    DTAILERR            YES                                          
         GOTO1 HEXIN,DMCB,0(R4),ATEMP,L'SFMSEQ-1  SEQ # FOR REPLICATE           
         OC    ARANGE1,ARANGE1     SEQ. NUMBER SPECIFIED?                       
         BZ    REPLBLK1            NO, SAVE THE ADDRESS                         
         CLC   ARANGE1,ATEMP       DIFFERENT ADDRESSES?                         
         BNE   REPLBLK2            YES, CHECK IF SECOND ADDRESS                 
         CLI   SELACTN,C'C'        OVERRIDE COPY LINE COMMAND?                  
         BE    REPLBLK1            YES                                          
         CLI   SELACTN,C'M'        OVERRIDE MOVE LINE COMMAND?                  
         BE    REPLBLK1            YES                                          
         CLI   SELACTN,C'R'        SAME BLOCK COMMAND?                          
         BE    REPLBLK1            YES                                          
         OC    ARANGE2,ARANGE2     NO, BLOCK COMMAND COMPLETE?                  
         BNZ   DTAILERR            YES, CAN'T OVERRIDE IT                       
REPLBLK1 MVC   ARANGE1,ATEMP       GOOD REPLICATE                               
         MVI   SELACTN,C'R'                                                     
         B     CKSELNXT                                                         
REPLBLK2 CLI   SELACTN,C'R'        SAME BLOCK COMMAND?                          
         BNE   DTAILERR            NO, ERROR                                    
         OC    ARANGE2,ARANGE2     YES                                          
         BZ    *+14                                                             
         CLC   ARANGE2,ATEMP                                                    
         BNE   DTAILERR                                                         
         CLC   ARANGE1,ATEMP       REVERSE THE ADDRESSES?                       
         BH    *+14                                                             
         MVC   ARANGE2,ATEMP       NO, ARANGE1 IS TOP                           
         B     CKSELNXT                                                         
         MVC   ARANGE2,ARANGE1     YES, ADDRESS IS ABOVE ARANGE1                
         MVC   ARANGE1,ATEMP                                                    
         B     CKSELNXT                                                         
         EJECT                                                                  
DELEBLCK DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R4,SFMSEQ                                                        
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R4,MULSEQ                                                        
         CLI   0(R4),0             NOTHING?                                     
         BE    DTAILERR            YES                                          
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BZ    DELBLK10            YES                                          
         MVI   ATEMPCOD,PNLPFDEQ                                                
         PACK  DUB(8),2(2,R4)                                                   
         CVB   R0,DUB                                                           
         STC   R0,ATEMP                                                         
         MVI   ATEMP+1,0                                                        
         B     DELBLK15                                                         
DELBLK10 MVI   ATEMPCOD,PNLELTCQ                                                
         GOTO1 HEXIN,DMCB,0(R4),ATEMP,L'SFMSEQ-1  SEQ # FOR DELETE              
DELBLK15 CLI   ARNG1COD,0          TOP SPECIFIED?                               
         BE    DELBLK20            NO, SAVE THE ADDRESS                         
         CLC   ARNG1COD,ATEMPCOD   YES, SEE IF SAME ELEMENT TYPE                
         BNE   DELBLK30            NOT, MUST BE A SECOND ADDRESS                
         CLC   ARANGE1,ATEMP       DIFFERENT ADDRESSES?                         
         BNE   DELBLK30            YES, CHECK IF SECOND ADDRESS                 
         CLI   SELACTN,C'C'        OVERRIDE COPY LINE COMMAND?                  
         BE    DELBLK20            YES                                          
         CLI   SELACTN,C'M'        OVERRIDE MOVE LINE COMMAND?                  
         BE    DELBLK20            YES                                          
         CLI   SELACTN,C'D'        SAME BLOCK COMMAND?                          
         BE    DELBLK20            YES                                          
         OC    ARANGE2,ARANGE2     NO, BLOCK COMMAND COMPLETE?                  
         BNZ   DTAILERR            YES, CAN'T OVERRIDE IT                       
DELBLK20 MVC   ARANGE1,ATEMP       GOOD REPLICATE                               
         MVC   ARNG1COD,ATEMPCOD                                                
         MVI   SELACTN,C'D'                                                     
         B     CKSELNXT                                                         
DELBLK30 CLI   SELACTN,C'D'        SAME BLOCK COMMAND?                          
         BNE   DTAILERR            NO, ERROR                                    
         CLI   ARNG2COD,0          YES, ANYTHING IN BOTTOM?                     
         BE    DELBLK40            NOTHING                                      
         CLC   ARNG2COD,ATEMPCOD                                                
         BNE   DTAILERR                                                         
         CLC   ARANGE2,ATEMP                                                    
         BNE   DTAILERR                                                         
DELBLK40 CLC   ARNG1COD,ATEMPCOD   SAME ELEMENT TYPE?                           
         BE    DELBLK45            YES                                          
         CLI   ARNG1COD,PNLPFDEQ   PFKEY FIRST, REGULAR SECOND?                 
         BE    DELBLK50            YES, SWAP                                    
         B     DELBLK47            NO, STORE PFKEY AS SECOND                    
DELBLK45 CLC   ARANGE1,ATEMP       REVERSE THE ADDRESSES?                       
         BH    DELBLK50                                                         
DELBLK47 MVC   ARNG2COD,ATEMPCOD                                                
         MVC   ARANGE2,ATEMP       NO, ARANGE1 IS TOP                           
         B     CKSELNXT                                                         
DELBLK50 MVC   ARNG2COD,ARNG1COD   YES, ADDRESS IS ABOVE ARANGE1                
         MVC   ARANGE2,ARANGE1                                                  
         MVC   ARNG1COD,ATEMPCOD                                                
         MVC   ARANGE1,ATEMP                                                    
         B     CKSELNXT                                                         
         EJECT                                                                  
DOCPYMOV NTR1                                                                   
         CLI   BEFAFTER,C'B'       BEFORE?                                      
         BE    *+14                                                             
         MVC   ADDRESS,AWHERE                                                   
         B     CPYMV10             NO                                           
         MVI   MINEKEY,PNLELTCQ    YES, CHANGE BEFORE IN TERMS OF AFTER         
         MVC   MINEKEY+1(2),AWHERE                                              
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MINIO,DMCB,('MINBSQ',(R5))                                       
         CLI   MINERR,0            NO PROBLEMS?                                 
         BE    CPYMV05             NONE                                         
         CLI   MINERR,MINEEOF      BEGINNING OF RECORD?                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         XC    ADDRESS,ADDRESS   **STORED IN ADDRESS SO AWHERE WON'T            
         B     CPYMV10                CHANGE IF THERE IS AN ERROR               
CPYMV05  MVC   ADDRESS,PNLESEQ     BEFORE NOW IN TERMS OF AFTER                 
*                                                                               
CPYMV10  ICM   R1,3,ADDRESS                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,FIRSTSEQ       SEQUENCE OF FIRST ONE                        
         MVI   MINEKEY,PNLELTCQ    ONLY REGULAR SEQUENCES DONE                  
         MVC   MINEKEY+1(2),ARANGE1  READ WHAT IS TO BE MOVED                   
         MVC   NUMLINES,CPYMOVLN   NUMBER OF LINES TO COPY/MOVE                 
         MVC   PREVSEQ,ARANGE1     SEQUENCE OF WHERE IT'S FROM                  
         MVC   NEWSEQ,ADDRESS      SEQUENCE OF WHERE IT'S GOING                 
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
CPYMVWHR ICM   R1,3,NEWSEQ         SEQ NUMBER OF AFTER                          
         LA    R1,1(R1)            EXACTLY AFTER                                
         STCM  R1,3,PNLESEQ                                                     
CPYMVADD GOTO1 MINIO,DMCB,('MINADD',(R5))  ADD THE ELEMENT                      
         CLI   MINERR,0            NO ERRORS?                                   
         BE    CPYMVMRE            NONE, ANY MORE TO COPY/MOVE                  
         CLI   MINERR,MINEDUP                                                   
         BE    *+6                                                              
         DC    H'0'                SOME OTHER ERROR                             
         MVC   MELEM2,MELEM        MAKE A COPY OF THE ELEMENT                   
         IC    R0,LINENO                                                        
         GOTO1 =A(RESEQNCE),DMCB,(RC),(RA),(R5),(R6),(R9),RR=RELO               
         STC   R0,LINENO                                                        
         MVC   MELEM,MELEM2        RESTORE THE ELEMENT                          
         B     CPYMVWHR                                                         
CPYMVMRE ZIC   R0,NUMLINES                                                      
         BCT   R0,*+8                                                           
         B     CPYMVDNE            DONE COPYING AND MOVING                      
         STC   R0,NUMLINES                                                      
         ZICM  R1,PREVSEQ,2                                                     
         LA    R1,1(R1)            GET NEXT ELEMENT TO COPY/MOVE                
         MVI   MINEKEY,PNLELTCQ                                                 
         STCM  R1,3,MINEKEY+1                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))  GOT NEXT ELEMENT                      
         CLI   MINERR,0            NO ERRORS?                                   
         BE    GOTMORE             NONE                                         
         CLI   MINERR,MINEEOF      NO MORE RECORDS?                             
         BE    CPYMVERR            YES, STILL WANT MORE                         
         DC    H'0'                SOME OTHER ERROR                             
GOTMORE  CLI   PNLELTC,PNLPFDEQ    READ A PFKEY?                                
         BE    CPYMVERR            YES, CAN'T COPY A PFKEY                      
         CLC   FIRSTSEQ,PNLESEQ    READ A COPY/MOVE SEQUENCE?                   
         BE    CPYMVERR            YES, WOULD HAVE DUPLICATES                   
         MVC   PREVSEQ,PNLESEQ                                                  
         ZICM  R1,NEWSEQ,2                                                      
         LA    R1,1(R1)            EXACTLY AFTER                                
         STCM  R1,3,NEWSEQ                                                      
         B     CPYMVWHR                                                         
*                                                                               
CPYMVDNE CLI   SELACTN,C'C'        COPY?                                        
         BE    CPYMVCLR            YES, DON'T NEED TO DELETE                    
         MVC   NUMLINES,CPYMOVLN                                                
         MVI   MINEKEY,PNLELTCQ    ONLY REGULAR SEQUENCES DONE                  
         MVC   MINEKEY+1(2),ARANGE1  READ WHAT WAS MOVED                        
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
CPMVDELP GOTO1 MINIO,DMCB,('MINDEL',(R5))  MOVED SEQ HAS TO BE DELETED          
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,NUMLINES                                                      
         BCT   R1,*+8                                                           
         B     CPYMVCLR            NO MORE                                      
         STC   R1,NUMLINES                                                      
         ICM   R1,3,PNLESEQ                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,MINEKEY+1                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BE    CPMVDELP                                                         
         DC    H'0'                                                             
CPYMVCLR MVI   SELACTN,0           NO ACTION                                    
         XC    ARANGE1,ARANGE1     NO SEQ # FOR COPY/MOVE                       
         MVI   CPYMOVLN,0          NO COPY/MOVE LINES                           
         MVI   BEFAFTER,0                                                       
         XC    AWHERE,AWHERE       NO SEQ # FOR WHERE                           
         OI    CHANGED,X'80'       RECORD CHANGED                               
         B     EXIT1                                                            
CPYMVERR MVC   CONHEAD(L'CPMVMESS),CPMVMESS                                     
         OI    CONHEADH+6,X'80'                                                 
         NI    CHANGED,X'FF'-X'80' DON'T SAVE THE DATA                          
         OI    CHANGED,X'08'       SIGNIFY ERROR, JUST LEAVE                    
         B     EXIT1               JUST DISPLAY ERROR AND REDISPLAY             
         EJECT                                                                  
DOCPMVBK NTR1                                                                   
         CLI   BEFAFTER,C'B'       BEFORE?                                      
         BE    *+14                                                             
         MVC   ADDRESS,AWHERE                                                   
         B     BKCPMV10            NO                                           
         MVI   MINEKEY,PNLELTCQ    YES, CHANGE BEFORE IN TERMS OF AFTER         
         MVC   MINEKEY+1(2),AWHERE                                              
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MINIO,DMCB,('MINBSQ',(R5))                                       
         CLI   MINERR,0            NO PROBLEMS?                                 
         BE    BKCPMV05            NONE                                         
         CLI   MINERR,MINEEOF      BEGINNING OF RECORD?                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         XC    ADDRESS,ADDRESS   **STORED IN ADDRESS SO AWHERE WON'T            
         B     BKCPMV10               CHANGE IF THERE IS AN ERROR               
BKCPMV05 MVC   ADDRESS,PNLESEQ     BEFORE NOW IN TERMS OF AFTER                 
*                                                                               
BKCPMV10 MVI   MINEKEY,PNLELTCQ    ONLY REGULAR SEQUENCES DONE                  
         MVC   MINEKEY+1(2),ARANGE1  READ WHAT IS TO BE MOVED                   
         MVC   PREVSEQ,ARANGE1     SEQUENCE OF WHERE IT'S FROM                  
         MVC   NEWSEQ,ADDRESS      SEQUENCE OF WHERE IT'S GOING                 
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
BKCPMVWH ICM   R1,3,NEWSEQ         SEQ NUMBER OF AFTER                          
         LA    R1,1(R1)            EXACTLY AFTER                                
         STCM  R1,3,PNLESEQ                                                     
BKCPMVAD GOTO1 MINIO,DMCB,('MINADD',(R5)) ADD THE ELEMENT                       
         CLI   MINERR,0            NO ERRORS?                                   
         BE    BKCPMVMR            NONE, ANY MORE TO COPY/MOVE                  
         CLI   MINERR,MINEDUP                                                   
         BE    *+6                                                              
         DC    H'0'                SOME OTHER ERROR                             
         MVC   MELEM2,MELEM        MAKE A COPY OF THE ELEMENT                   
         IC    R0,LINENO                                                        
         GOTO1 =A(RESEQNCE),DMCB,(RC),(RA),(R5),(R6),(R9),RR=RELO               
         STC   R0,LINENO                                                        
         MVC   MELEM,MELEM2        RESTORE THE ELEMENT                          
         B     BKCPMVWH                                                         
BKCPMVMR CLC   ARANGE2,PREVSEQ                                                  
         BE    BKCPMVDN            DONE COPY/MOVE BLOCK                         
         ZICM  R1,PREVSEQ,2                                                     
         LA    R1,1(R1)            GET NEXT ELEMENT TO COPY/MOVE                
         MVI   MINEKEY,PNLELTCQ                                                 
         STCM  R1,3,MINEKEY+1                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))  GOT NEXT ELEMENT                      
         CLI   MINERR,0            NO ERRORS?                                   
         BE    *+6                 NONE                                         
         DC    H'0'                SOME OTHER ERROR                             
         CLI   PNLELTC,PNLPFDEQ    READ A PFKEY?                                
         BE    DTAILERR            YES, CAN'T COPY A PFKEY                      
         MVC   PREVSEQ,PNLESEQ                                                  
         ZICM  R1,NEWSEQ,2                                                      
         LA    R1,1(R1)            EXACTLY AFTER                                
         STCM  R1,3,NEWSEQ                                                      
         B     BKCPMVWH                                                         
*                                                                               
BKCPMVDN CLI   SELACTN,C'B'        COPY?                                        
         BE    BKCPMVCL            YES, DON'T NEED TO DELETE                    
         MVI   MINEKEY,PNLELTCQ    ONLY REGULAR SEQUENCES DONE                  
         MVC   MINEKEY+1(2),ARANGE1  READ WHAT WAS MOVED                        
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
BKCPMVDL GOTO1 MINIO,DMCB,('MINDEL',(R5))  MOVED SEQ HAS TO BE DELETED          
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ARANGE2,PNLESEQ                                                  
         BE    BKCPMVCL            NO MORE                                      
         ICM   R1,3,PNLESEQ                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,MINEKEY+1                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BE    BKCPMVDL                                                         
         DC    H'0'                                                             
BKCPMVCL MVI   SELACTN,0           NO ACTION                                    
         XC    ARANGE1,ARANGE1     NO SEQ # FOR COPY/MOVE BLOCK                 
         XC    ARANGE2,ARANGE2                                                  
         MVI   BEFAFTER,0                                                       
         XC    AWHERE,AWHERE       NO SEQ # FOR WHERE                           
         OI    CHANGED,X'80'       RECORD CHANGED                               
         B     EXIT1                                                            
         EJECT                                                                  
DELBLOCK NTR1                                                                   
         MVC   MINEKEY(L'ARNG1COD),ARNG1COD                                     
         MVC   MINEKEY+1(L'ARANGE1),ARANGE1  READ TOP ELEMENT                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
BKDELOOP GOTO1 MINIO,DMCB,('MINDEL',(R5))  MOVED SEQ HAS TO BE DELETED          
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PNLELTC,PNLPFDEQ    PFKEY READ?                                  
         BE    BKDELLP1            YES, NO AWHERE                               
         CLC   AWHERE,PNLESEQ                                                   
         BNE   BKDELLP1                                                         
         XC    AWHERE,AWHERE                                                    
         MVI   BEFAFTER,0                                                       
BKDELLP1 CLC   ARNG2COD,PNLELTC    SAME ELEMENT TYPE?                           
         BNE   BKDELLP2            NO, KEEP DELETING TILL WE GET THERE          
         CLC   ARANGE2,PNLESEQ     SAME SEQUENCE OR PFKEY?                      
         BE    BKDELCLR            YES, NO MORE                                 
BKDELLP2 ICM   R1,3,PNLESEQ                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,MINEKEY+1                                                   
         MVC   MINEKEY(L'PNLELTC),PNLELTC   COPY THE ELEMENT CODE               
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BE    BKDELOOP                                                         
         DC    H'0'                                                             
BKDELCLR MVI   SELACTN,0           NO ACTION                                    
         MVI   ARNG1COD,0                                                       
         XC    ARANGE1,ARANGE1     NO SEQ # FOR COPY/MOVE BLOCK                 
         MVI   ARNG2COD,0                                                       
         XC    ARANGE2,ARANGE2                                                  
         OI    CHANGED,X'80'       RECORD CHANGED                               
         B     EXIT1                                                            
         EJECT                                                                  
RELO     DS    F                                                                
MYSPACES DC    C'        '                                                      
CPMVMESS DC    CL60'INVALID COPY/MOVE DETAILS.  PLEASE REDO.'                   
*                                                                               
         SPACE 3                                                                
BADERR   MVC   GERROR,=AL2(INVALID)                                             
         B     SFMERROR                                                         
MISSERR  MVC   GERROR,=AL2(MISSING)                                             
SFMERROR GOTO1 SFMERR                                                           
*                                                                               
EXIT     OI    6(R2),X'40'         POSN CURSOR                                  
EXIT1    XIT1                                                                   
         DROP  R2                                                               
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DR: DISPLAY RECORD                                                            
*                                                                               
DR       DS    0H                                                               
         NMOD1 0,**DREC**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R5,8(R1)                                                         
         L     R6,12(R1)                                                        
         L     R9,16(R1)                                                        
         MVC   INSERTLN,NWINSERT   INSERTION LINE NUMBER                        
         MVC   INSERTNM,NWINSRTN   NUMBER OF INSERTION LINES                    
         MVI   ELCDEND,PNLELTCQ    DEFAULT ELEMENT CODE                         
         XC    ENDSEQ,ENDSEQ       NO END UNTIL RECORD IS DISPLAYED             
         LA    R2,SFMSTH           STARTING POS. FOR SPANEL & MPANEL            
* SLIGHT CHANGES TO THE TWAXC MACRO                                             
*   TO CLEAR INTENSITY AND TO VALIDATE ALL THE FIELDS                           
         SR    RE,RE                                                            
         LA    R1,SFMSTH           STARTING POSITION FOR BOTH                   
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         LA    RF,SFMENDH          YES                                          
         B     *+8                                                              
         LA    RF,MULENDH          NO, MPANEL                                   
DRTWAXC  IC    RE,0(R1)            GET LENGTH                                   
         SH    RE,=H'9'                                                         
         TM    1(R1),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                 NO                                           
         SH    RE,=H'8'            YES                                          
         LTR   RE,RE                                                            
         BM    DRFRST                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)       NULL OUT THE DATA                            
         OI    6(R1),X'80'         TRANSMIT THE FIELDS                          
         OI    4(R1),X'20'         VALIDATE THE FIELDS                          
         NI    1(R1),X'FF'-X'0C'   SET TO NORMAL INTENSITY                      
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,DRTWAXC                                                    
DRFRST   MVC   MINEKEY(1),ELCDBEG     ELEMENT CODE OF BEGINNING                 
         MVC   MINEKEY+1(2),BEGSEQ    USE SEQUENCE THAT STARTED BEFORE          
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            ANY ERRORS?                                  
         BE    DR10                NO                                           
         CLI   MINERR,MINEEOF      ANY ELEMENTS IN RECORD?                      
         BE    DR05                NO                                           
         CLI   MINERR,MINESNF      RECORD NOT FOUND?                            
         BE    *+6                 YES                                          
         DC    H'0'                DIE, MINIO ERROR                             
DR05     XC    BEGSEQ,BEGSEQ       NO ELEMENTS, NO SEQUENCE                     
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                NO                                           
         USING SFMFAD,R2           SPANEL DSECT FOR LINE                        
         LA    R2,SFMENTH          DATATYPE IS FIRST DATA COLUMN                
         B     DREXIT              WANT DATA                                    
         USING MULFAD,R2           MPANEL DSECT FOR LINE                        
         LA    R2,MULENTH          DATATYPE IS FIRST DATA COLUMN                
DREXIT   XMOD1 2                   WANT DATA                                    
         EJECT                                                                  
DR10     DS    0H                                                               
         MVI   LINENO,1            START FROM THE FIRST LINE                    
*                                                                               
DRLP     L     R6,MINELEM          POINT TO THE ELEMENT READ                    
         USING PNLELTD,R6          ELEMENT TEMPLATE                             
*                                                                               
DR15     CLC   LINENO,INSERTLN     THE INSERT LINE?                             
         BL    DR17                NO, BEFORE IT                                
         ZICM  R1,INSERTLN,1                                                    
         BZ    DR17                                                             
         ZIC   R0,INSERTNM                                                      
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         CLM   R1,1,LINENO                                                      
         BL    DR17                NO, AFTER IT                                 
         CLI   CONREC,C'S'                                                      
         BNE   DR15A                                                            
         USING SFMFAD,R2           SPANEL                                       
         MVC   SFMENT,=8C'.'       THE INFAMOUS DOTS                            
         LA    R1,SFMLSLNH         A(LAST LINE)                                 
         LA    R0,SFMFADX          LENGTH OF SPANEL LINE                        
         B     DR15B                                                            
         USING MULFAD,R2           MPANEL                                       
DR15A    MVC   MULENT,=8C'.'                                                    
         LA    R1,MULLSLNH         A(LAST LINE)                                 
         LA    R0,MULFADX          LENGTH OF MPANEL LINE                        
DR15B    CR    R1,R2               INSERT ON LAST LINE                          
         BE    DR100               YES, THAT'S ALL                              
         IC    R1,LINENO           NO, NEXT LINE                                
         LA    R1,1(R1)                                                         
         STC   R1,LINENO                                                        
         AR    R2,R0               NEXT SCREEN LINE                             
         B     DRLP                THERE WAS A GOOD ELEMENT BEFORE              
*                                                                               
DR17     CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BE    DR30                YES, SKIP ROW AND COLUMN                     
         CLI   CONREC,C'M'         MPANEL?                                      
         BE    DR30                YES, NO ROW OR COLUMN IN MPANEL              
* DISPLAY ROW                                                                   
         USING SFMFAD,R2           SPANEL'S LINE DSECT                          
         CLI   PNLEROW,0           NULL (SAME ROW)?                             
         BNE   *+12                NO                                           
         MVI   SFMROW,C'*'         YES, DISPLAY '*'                             
         B     DR20                                                             
         MVI   SFMROW,C'+'         DISPLAY RELATIVE ROW #                       
         EDIT  (B1,PNLEROW),(2,SFMROW+1),ALIGN=LEFT                             
* DISPLAY COL                                                                   
DR20     DS    0H                                                               
         CLI   PNLECOL,0           NULL (NEXT AVALIABLE COLUMN)?                
         BNE   *+12                NO                                           
         MVI   SFMCOL,C'*'         YES, DISPLAY '*'                             
         B     DR30                                                             
         EDIT  (B1,PNLECOL),(3,SFMCOL),ALIGN=LEFT                               
* DISPLAY DATATYPE NAME                                                         
DR30     DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMENT                                                        
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R3,MULENT                                                        
         CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BNE   DR35                NO                                           
         USING PNLPFD,R6           DSECT FOR PFKEY ELEMENTS                     
         MVC   0(2,R3),=C'PF'      SHOW PF##                                    
         EDIT  (B1,PNLPFNUM),(2,2(R3)),ALIGN=LEFT                               
         B     DR40                CHECK OVERRIDE NAME                          
         USING PNLELTD,R6          ELEMENT TEMPLATE                             
DR35     MVC   0(L'PNLEDTYP,R3),PNLEDTYP  COPY DATATYPE TO SCREEN LINE          
         CLC   =C'SEL',PNLEDTYP                                                 
         BE    *+14                SELECT FIELD                                 
         CLC   =C'KEY',PNLEDTYP    KEY FIELD                                    
         BNE   DR37                                                             
         EDIT  (B1,PNLEDLEN),(2,3(R3)),ALIGN=LEFT                               
DR37     CLC   =C'TEXT',PNLEDTYP   TEXT FIELD?                                  
         BE    DR40                CHECK OVERRIDE NAME                          
* DISPLAY REPLICATION ID                                                        
         CLC   =C'SEL',PNLEDTYP    SELECT FIELD?                                
         BE    DR40                YES, NO REP ID OR NAME LENGTH                
         CLC   =C'KEY',PNLEDTYP    KEY FIELD?                                   
         BE    DR40                YES, NO REP ID OR NAME LENGTH                
         CLI   PNLESUBP,C'K'       KEY FIELD THROUGH SUB-PANEL?                 
         BE    DR40                YES, NO REP ID OR NAME LENGTH                
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMRPI                                                        
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R3,MULRPI                                                        
         EDIT  (B1,PNLERPID),(3,0(R3)),ALIGN=LEFT                               
         CLI   CONREC,C'M'         MPANEL?                                      
         BE    DR40                      NO NAME LENGTH IN MPANEL               
* DISPLAY OVERRIDE NAME LENGTH                                                  
         USING SFMFAD,R2                                                        
         CLI   PNLEFLEN,0          DEFAULT LENGTH TO DATATYPE?                  
         BE    DR40                YES                                          
         CLI   PNLEFLEN,X'FF'      NO NAME? JUST ENTRY?                         
         BNE   *+12                                                             
         MVI   SFMNLN,C'0'         YES                                          
         B     DR50                DON'T DISPLAY NAME IF 0                      
         EDIT  (B1,PNLEFLEN),(2,SFMNLN),ALIGN=LEFT                              
* DISPLAY OVERRIDE NAME                                                         
DR40     DS    0H                                                               
         CLC   =C'USER',PNLEDTYP   USER FIELD?                                  
         BE    DR50                NO OVERRIDE NAME                             
         CLC   =C'KEY',PNLEDTYP    KEY FIELD?                                   
         BE    DR50                NO OVERRIDE NAME                             
         CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BNE   DR45                NO                                           
         USING PNLPFD,R6           DSECT FOR PFKEY ELEMENTS                     
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   DR40A               NO                                           
         USING SFMFAD,R2           YES                                          
         MVC   SFMOVR(8),PNLPFTXT  DISPLAY WHAT PFKEY DOES IF ANY               
         B     DR50                DISPLAY PROTECTED                            
         USING MULFAD,R2           MPANEL                                       
DR40A    MVC   MULTOP(8),PNLPFTXT  DISPLAY WHAT PFKEY DOES IF ANY               
         B     DR50                DISPLAY PROTECTED                            
         USING PNLELTD,R6                                                       
DR45     CLI   CONREC,C'S'         SPANEL?                                      
         BNE   DR450                                                            
         CLI   PNLERWNL,0          NO LENGTH IN OVERRIDE?                       
         BNE   DR45A               THERE IS AN OVERRIDE                         
         CLI   PNLERWNM,0          NULL?  DEFAULT?                              
         BNE   DR45A               NO, NOT DEFAULT                              
         B     DR455                                                            
DR450    CLI   PNLECL1L,0          -- MPANEL                                    
         BNE   DR45Z               SOMETHING IN THE TOP                         
         CLI   PNLECL1N,0          NULL?  DEFAULT?                              
         BNE   DR45Z               NO, NOT DEFAULT                              
         CLI   PNLECL2L,0                                                       
         BNE   DR45Z               SOMETHING IN THE BOTTOM                      
         CLI   PNLECL2N,0          NULL?  DEFAULT?                              
         BNE   DR45Z               NO, NOT DEFAULT                              
DR455    CLC   =C'USER',PNLEDTYP   KEY WORD?                                    
         BE    DR50                YES, DON'T HAVE TO LOOKUP                    
         CLC   =C'SEL',PNLEDTYP                                                 
         BE    DR50                                                             
         CLC   =C'KEY',PNLEDTYP                                                 
         BE    DR50                                                             
         CLC   =C'TEXT',PNLEDTYP                                                
         BE    DR50                                                             
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R4,KEY                                                           
         USING DTYPKEYD,R4                                                      
         MVI   DTYPSYS,DTYPSYSQ    RECORD IDENTIFIER EQUATES                    
         MVI   DTYPTYP,DTYPTYPQ                                                 
         MVC   DTYPSYPG,SFMSYPG    DICTIONARY NAME SPACE FILLED                 
         MVC   DTYPCODE,PNLEDTYP   DATATYPE NAME                                
         OC    DTYPCODE,MYSPACES   SPACE FILL AND CAPITALIZE IF NEEDED          
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE     DOES THE ENTRY RECORD EXIST?                 
         BE    *+6                                                              
         DC    H'0'                HAVE TO BE THERE                             
* READ THE RECORD FROM THE DA HALF OF ISDA PAIR WITH GETREC                     
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,AIO,DMWORK         
         CLI   DMCB+8,0                                                         
         BE    *+6                 SHOULDN'T GET ANY ERRORS GETTING REC         
         DC    H'0'                                                             
         XC    DATADISP,DATADISP                                                
         MVI   DATADISP+1,DTYPFRST-DTYPKEYD   DATADISP TO 1ST ELEMENT           
         L     R4,AIO              POINT TO THE DATATYPE RECORD READ            
         CLI   CONREC,C'S'         SPANEL                                       
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         MVI   ELCODE,DTNMRELQ     SPANEL, ROW TEXT                             
         B     *+8                                                              
         USING MULFAD,R2                                                        
         MVI   ELCODE,DTNMCELQ     MPANEL, COLUMN TEXT                          
         BAS   RE,GETEL            GET THE TEXT ELEMENT                         
         BE    *+8                                                              
         B     DR50                NO NAME ELEMENT, NO DEFAULT TEXT             
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+14                NO                                           
         USING SFMFAD,R2           SPANEL                                       
         USING DTNMRD,R4           NAME DSECT FOR ROWS                          
         MVC   SFMOVR,DTNMRTXT     SHOW THE DEFAULT                             
         B     DR50                                                             
         USING MULFAD,R2           MPANEL                                       
         USING DTNMCD,R4                                                        
         MVC   MULTOP,DTNMCTX1                                                  
         MVC   MULBOT,DTNMCTX2                                                  
         B     DR50                                                             
         DROP  R4                                                               
*                                                                               
         USING SFMFAD,R2                                                        
DR45A    MVC   SFMOVR,PNLERWNM     COPY IT TO SCREEN                            
         CLC   =C'TEXT',PNLEDTYP                                                
         BE    DR50                                                             
         OI    SFMOVRH+1,X'08'     OVERRIDES GET HIGH INTENSITY                 
         B     DR50                                                             
*                                                                               
         USING MULFAD,R2                                                        
DR45Z    MVC   MULTOP,PNLECL1N     COPY TOP                                     
         OI    MULTOPH+1,X'08'                                                  
         MVC   MULBOT,PNLECL2N      AND BOTTOM                                  
         OI    MULBOTH+1,X'08'     OVERRIDES GET HIGH INTENSITY                 
* DISPLAY IF PROTECTED                                                          
DR50     DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMPRO                                                        
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R3,MULPRO                                                        
         CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BE    *+12                YES, PFKEYS ARE ALWAYS PROTECTED             
         TM    PNLEATTR,X'20'      PROTECTED OR UN-PROTECTED                    
         BZ    *+12                                                             
         MVI   0(R3),C'P'          PROTECTED                                    
         B     *+8                                                              
         MVI   0(R3),C'U'          UNPROTECTED                                  
* DISPLAY IF UPPER CASE                                                         
         CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BE    DR60                PFKEYS HAVE NO UPPER/LOWER DATA              
         CLC   =C'TEXT',PNLEDTYP                                                
         BE    DR60                                                             
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMLOWH                                                       
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R3,MULLOWH                                                       
         CLC   =C'USER',PNLEDTYP   KEY WORD?                                    
         BE    DR58                                                             
         CLC   =C'SEL',PNLEDTYP                                                 
         BE    DR58                                                             
         CLC   =C'KEY',PNLEDTYP                                                 
         BE    DR58                                                             
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R4,KEY                                                           
         USING DTYPKEYD,R4                                                      
         MVI   DTYPSYS,DTYPSYSQ    RECORD IDENTIFIER EQUATES                    
         MVI   DTYPTYP,DTYPTYPQ                                                 
         MVC   DTYPSYPG,SFMSYPG    DICTIONARY NAME SPACE FILLED                 
         OC    DTYPSYPG,MYSPACES   SPACE FILL AND CAPITALIZE IF NEEDED          
         MVC   DTYPCODE,PNLEDTYP   DATATYPE NAME                                
         OC    DTYPCODE,MYSPACES   SPACE FILL AND CAPITALIZE IF NEEDED          
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE     DOES THE ENTRY RECORD EXIST?                 
         BE    DR55A               CHECK FOR DEFAULTS                           
         LR    R2,R3               NO, BAD DATATYPE. R3=A(DATATYPE HDR)         
         MVC   GERROR,=AL2(NODTYPE) DATATYPE NOT IN SYS.PROG                    
         B     SFMERROR                                                         
DR55A    DS    0H                                                               
* READ THE RECORD FROM THE DA HALF OF ISDA PAIR WITH GETREC                     
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,AIO,DMWORK         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DATADISP,DATADISP                                                
         MVI   DATADISP+1,DTYPFRST-DTYPKEYD   DATADISP TO 1ST ELEMENT           
         L     R4,AIO              POINT TO THE DATATYPE RECORD READ            
         MVI   ELCODE,DTGENELQ     GET GENERAL INFO ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NO GENERAL INFO ELEMENT               
         ST    R4,AGENINFO         STORE THE ADDRESS FOR LATER USE              
         OI    1(R3),X'08'         HIGH INTS, CHANGE TO NORM IF DEFAULT         
         TM    PNLEDEFA,PNLEUPLO   UPPER/LOWER DEFAULT?                         
         BZ    DR58                NO                                           
         USING DTGEND,R4                                                        
         MVC   8(1,R3),DTGENDUL    COPY THE DEFAULT                             
         NI    1(R3),X'FF'-X'08'   DEFAULT HAS NORMAL INTENSITY                 
         B     DR60                                                             
         DROP  R4                                                               
DR58     TM    PNLEATTR,X'40'      UPPER OR UPPER/LOWER CASE                    
         BZ    *+12                                                             
         MVI   8(R3),C'L'          UPPER/LOWER                                  
         B     *+8                                                              
         MVI   8(R3),C'U'          UPPER ONLY                                   
* CHECK INTENSITIES...                                                          
DR60     DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+16                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMNIN                                                        
         LA    R4,SFMDIN                                                        
         B     *+12                                                             
         USING MULFAD,R2                                                        
         LA    R3,MULNIN                                                        
         LA    R4,MULDIN                                                        
         CLC   =C'USER',PNLEDTYP                                                
         BE    DR70                USER LINE HAS NO NAME                        
         CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BE    DR65                YES                                          
         TM    PNLEATTR,X'08'                                                   
         BZ    *+12                                                             
         MVI   0(R3),C'H'          NAME FIELD HIGH                              
         B     DR70                CHECK DATA FIELD                             
         TM    PNLEATTR,X'04'                                                   
         BZ    DR60A                                                            
         MVI   0(R3),C'Z'          NAME FIELD ZERO                              
         B     DR70                CHECK DATA FIELD                             
DR65     DS    0H                                                               
         USING PNLPFD,R6                                                        
         TM    PNLPFATT,X'08'                                                   
         BZ    *+12                                                             
         MVI   0(R3),C'H'          NAME FIELD HIGH                              
         B     DR70                CHECK DATA FIELD                             
         TM    PNLPFATT,X'04'                                                   
         BZ    *+12                                                             
         MVI   0(R3),C'Z'          NAME FIELD ZERO                              
         B     DR70                CHECK DATA FIELD                             
DR60A    MVI   0(R3),C'N'          NAME FIELD NORMAL                            
         USING PNLELTD,R6                                                       
*                                                                               
DR70     CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BE    DR90                YES, DISPLAY SEQUENCE                        
         CLC   =C'TEXT',PNLEDTYP                                                
         BE    DR90                                                             
         TM    PNLEATTR,X'01'                                                   
         BZ    *+12                                                             
         MVI   0(R4),C'H'                                                       
         B     DR80                                                             
         TM    PNLEATTR,X'02'                                                   
         BZ    *+12                                                             
         MVI   0(R4),C'Z'                                                       
         B     DR80                                                             
         MVI   0(R4),C'N'          DATA FIELD NORMAL                            
* DISPLAY SUB-PANEL                                                             
DR80     DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL                                       
         BNE   *+12                NO                                           
         USING SFMFAD,R2                                                        
         LA    R3,SFMSUBH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULSUBH                                                       
         CLI   PNLESUBP,0                                                       
         BE    DR85                                                             
         MVC   8(1,R3),PNLESUBP                                                 
* DISPLAY FIXED/SCROLL                                                          
         USING MULFAD,R2                                                        
DR85     DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL                                       
         BE    DR85A               YES, DISPLAY ID NUMBER                       
         CLC   =C'USER',PNLEDTYP                                                
         BE    DR90                                                             
         TM    PNLEFLAG,PNLEFIXQ   FIXED?                                       
         BNZ   *+12                                                             
         MVI   MULFIX,C'N'         NO                                           
         B     *+8                                                              
         MVI   MULFIX,C'Y'         YES                                          
* DISPLAY BELOW LINE (Y/N)                                                      
         TM    PNLEFLAG,PNLEBELO   BELOW?                                       
         BNZ   *+12                                                             
         MVI   MULBEL,C'N'         NO                                           
         B     *+8                                                              
         MVI   MULBEL,C'Y'         YES                                          
* EXTENDED HEADER ID NUMBER                                                     
DR85A    DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL                                       
         BNE   *+12                NO                                           
         USING SFMFAD,R2                                                        
         LA    R3,SFMIDNH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULIDNH                                                       
         CLC   =C'SEL',PNLEDTYP                                                 
         BE    DR85B                                                            
         CLC   =C'KEY',PNLEDTYP                                                 
         BE    DR85B                                                            
         OI    1(R3),X'08'         HIGH INTS, CHANGE TO NORM IF DEFAULT         
         TM    PNLEDEFA,PNLEIDNM   DATA LENGTH DEFAULT?                         
         BZ    DR85B               NO                                           
         L     R4,AGENINFO         A(GENERAL INFO ELEMENT)                      
         USING DTGEND,R4                                                        
         MVC   PNLEIDNO,DTGENIDN                                                
         NI    1(R3),X'FF'-X'08'   DEFAULT HAS NORMAL INTENSITY                 
         DROP  R4                                                               
DR85B    EDIT  (B1,PNLEIDNO),(3,8(R3)),ALIGN=LEFT                               
* DISPLAY SEQUENCE NUMBER                                                       
DR90     DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMSEQH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULSEQH                                                       
         OI    1(R3),X'0C'         ZERO OR LOW INTENSITY                        
         CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BNE   DR92                NO                                           
         USING PNLPFD,R6           DSECT FOR PFKEY ELEMENTS                     
* DISPLAY PF##                                                                  
         MVC   8(2,R3),=C'PF'      PFKEY INDICATOR                              
         ZIC   R1,PNLPFNUM                                                      
         CVD   R1,DUB                                                           
         UNPK  10(2,R3),DUB                                                     
         OI    11(R3),X'F0'                                                     
         MVI   12(R3),C'Y'         INDICATE IN FILE                             
         MVI   ELCDEND,PNLPFDEQ    PFKEY ELEMENT CODE                           
         MVC   ENDSEQ(1),PNLPFNUM                                               
         MVI   ENDSEQ+1,X'00'      SECOND BYTE HAS NOTHING                      
         B     DR94                                                             
         USING PNLELTD,R6                                                       
DR92     OI    6(R3),X'80'         TRANSMIT FOR NOW                             
         MVI   ELCDEND,PNLELTCQ    REGULAR SEQUENCE                             
         MVC   ENDSEQ,PNLESEQ      NEW ENDING SEQUENCE NUMBER                   
         GOTO1 HEXOUT,DMCB,PNLESEQ,8(R3),L'PNLESEQ                              
         MVI   12(R3),C'Y'         INDICATE IN FILE                             
DR94     CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMSELH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULSELH                                                       
         CLC   PNLESEQ,AWHERE      IS IT EQUAL TO WHERE?                        
         BNE   *+14                                                             
         MVC   8(1,R3),BEFAFTER    YES, COPY THE 'B' OR 'A'                     
         B     DR95                                                             
         CLI   SELACTN,C'B'        CC?                                          
         BNE   *+14                                                             
         MVC   TMPACTN,=C'CC '                                                  
         B     DR94CK                                                           
         CLI   SELACTN,C'N'        MM?                                          
         BNE   *+14                                                             
         MVC   TMPACTN,=C'MM '                                                  
         B     DR94CK                                                           
         CLI   SELACTN,C'R'        RR?                                          
         BNE   *+14                                                             
         MVC   TMPACTN,=C'RR '                                                  
         B     DR94CK                                                           
         CLI   SELACTN,C'D'        DD?                                          
         BNE   *+14                                                             
         MVC   TMPACTN,=C'DD '                                                  
         B     DR94CK                                                           
         MVC   TMPACTN(L'SELACTN),SELACTN                                       
         MVC   TMPACTN+1(2),=C'  '                                              
         CLI   CPYMOVLN,1          COPY ONE LINE?                               
         BE    DR94CK              YES, JUST A C                                
         LA    R1,TMPACTN                                                       
         EDIT  (B1,CPYMOVLN),(2,1(R1)),ALIGN=LEFT  AND THE NUMBER               
DR94CK   CLC   PNLESEQ,ARANGE1                                                  
         BNE   DR94CK1                                                          
         CLI   SELACTN,C'D'        ONLY DELETE BLOCK NEEDS TO CHECK             
         BNE   DR94CKA                ELEMENT CODE                              
         CLC   PNLELTC,ARNG1COD                                                 
         BNE   DR94CK1                                                          
DR94CKA  MVC   8(L'TMPACTN,R3),TMPACTN   SAY WHICH                              
         B     DR95                                                             
DR94CK1  CLC   PNLESEQ,ARANGE2                                                  
         BNE   DR95                                                             
         CLI   SELACTN,C'D'        ONLY DELETE BLOCK NEEDS TO CHECK             
         BNE   DR94CK1A               ELEMENT CODE                              
         CLC   PNLELTC,ARNG2COD                                                 
         BNE   DR95                                                             
DR94CK1A MVC   8(L'TMPACTN,R3),TMPACTN   SAY WHICH                              
DR95     CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+16                                                             
         USING SFMFAD,R2           YES                                          
         LA    R0,SFMLSLNH                                                      
         LA    R4,SFMFADX                                                       
         B     *+12                                                             
         USING MULFAD,R2           MPANEL                                       
         LA    R0,MULLSLNH                                                      
         LA    R4,MULFADX                                                       
         CR    R2,R0               CHECK IF NO MORE LINES AVAILABLE             
         BE    DR100               END DISPLAY - OVER SCREEN                    
         IC    R1,LINENO           INCREMENT THE LINE NUMBER                    
         LA    R1,1(R1)                                                         
         STC   R1,LINENO                                                        
* NEXT LINE AND ELEMENT OF RECORD                                               
         AR    R2,R4               GOTO THE NEXT LINE OF THE SCREEN             
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))  READ NEXT ELEMENT                    
         CLI   MINERR,0            ANY ERRORS?                                  
         BE    DRLP                NONE, CONTINUE PRINTING                      
*                                                                               
DR100    LA    R2,SFMSTH           STARTING POS. FOR SPANEL & MPANEL            
         CLI   CONREC,C'S'         SPANEL                                       
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMSEQ                                                        
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R3,MULSEQ                                                        
         CLI   0(R3),0             NOTHING IN THE BEGINNING?                    
         BNE   DR105                                                            
         MVI   ELCDBEG,PNLELTCQ    DEFAULT IS SEQUENCE ELEMENT                  
         XC    BEGSEQ,BEGSEQ                                                    
         B     DREXIT                                                           
DR105    CLI   0(R3),C'P'          PFKEY?                                       
         BNE   DR108               NO                                           
         PACK  DUB(8),2(2,R3)                                                   
         CVB   R0,DUB                                                           
         STC   R0,BEGSEQ                                                        
         MVI   BEGSEQ+1,X'00'                                                   
         MVI   ELCDBEG,PNLPFDEQ    PFKEY                                        
         B     DREXIT                                                           
DR108    GOTO1 HEXIN,DMCB,0(R3),BEGSEQ,L'SFMSEQ-1                               
         MVI   ELCDBEG,PNLELTCQ    REGULAR SEQUENCE ELEMENT                     
         B     DREXIT              OTHERWISE, EXIT                              
         DROP  R2,R6                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
BLDELEM  DS    0H                                                               
         NMOD1 0,**BLDE**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R2,8(R1)                                                         
         L     R5,12(R1)                                                        
         L     R6,16(R1)                                                        
         L     R9,20(R1)                                                        
         USING PNLELTD,R6          SEQUENCE DSECT                               
         OI    CHANGED,X'80'       RECORD CHANGED IF LINE DID                   
* DATATYPE                                                                      
BE00     CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMENTH                                                       
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R3,MULENTH                                                       
         TM    TYPEFLAG,X'40'      USER LINE?                                   
         BZ    BE00A               NO                                           
         CLI   5(R3),4             YES, JUST THE LETTERS 'USER'?                
         BNH   BEUSERR             YES                                          
         MVC   PNLEDTYP,8(R3)                                                   
         OC    PNLEDTYP,MYSPACES                                                
         B     BE00G               GET ROW                                      
BEUSERR  LR    R2,R3               BAD USER LINE                                
         MVC   GERROR,=AL2(INVUSER)                                             
         B     BLDERROR                                                         
BE00A    TM    TYPEFLAG,X'80'      PFKEY?                                       
         BZ    BE00G               NO                                           
         CLI   5(R3),2             ONLY 'PF'?                                   
         BNH   BEPFERR             YES                                          
         CLI   5(R3),4             MORE THAN NEEDED? (PF##)                     
         BH    BEPFERR                                                          
         ZIC   R1,5(R3)                                                         
         SH    R1,=H'2'                                                         
         LR    R0,R1                                                            
         LA    R4,10(R3)                                                        
BE00ALP  CLI   0(R4),C'0'                                                       
         BL    BEPFERR                                                          
         CLI   0(R4),C'9'                                                       
         BH    BEPFERR                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,BE00ALP                                                       
         BCTR  R1,0                -1 FOR PACK                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),10(0,R3)                                                  
         CVB   R1,DUB                                                           
         CH    R1,=H'24'           ONLY 24 PFKEYS                               
         BH    BEPFERR                                                          
         XC    MELEM,MELEM                                                      
         USING PNLPFD,R6                                                        
         MVI   PNLPFDEL,PNLPFDEQ                                                
         MVI   PNLPFDLN,PNLPFDLQ                                                
         STC   R1,PNLPFNUM                                                      
         B     BE00G                                                            
BEPFERR  LR    R2,R3               BAD PFKEY                                    
         MVC   GERROR,=AL2(INVPFKEY)                                            
         B     BLDERROR                                                         
         USING PNLELTD,R6                                                       
BE00G    DS    0H                                                               
         TM    TYPEFLAG,X'30'      SELECT OR KEY FIELD?                         
         BZ    BE00H               NO, OTHER                                    
         CLI   5(R3),3             ONLY 'SEL' OR 'KEY'?                         
         BNH   BESKERR             YES                                          
         CLI   5(R3),5             MORE THAN NEEDED? (???##)                    
         BH    BESKERR                                                          
         ZIC   R1,5(R3)                                                         
         SH    R1,=H'3'                                                         
         LR    R0,R1                                                            
         LA    R4,11(R3)                                                        
BE00GLP  CLI   0(R4),C'0'                                                       
         BL    BESKERR                                                          
         CLI   0(R4),C'9'                                                       
         BH    BESKERR                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,BE00GLP                                                       
         BCTR  R1,0                -1 FOR PACK                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),11(0,R3)                                                  
         CVB   R1,DUB                                                           
         STC   R1,PNLEDLEN                                                      
         B     BE00H                                                            
BESKERR  LR    R2,R3               BAD SELECT OR KEY LENGTH                     
         MVC   GERROR,=AL2(INVSELKY)                                            
         B     BLDERROR                                                         
BE00H    MVC   PNLEDTYP,8(R3)      SAVE DATATYPE NAME                           
         OC    PNLEDTYP,MYSPACES   SPACE FILL AND CAPITALIZE IF NEEDED          
         TM    TYPEFLAG,X'F8'      SPECIAL DATATYPE?                            
         BNZ   BE10                YES, DON'T CHECK IF DATATYPE EXISTS          
* TEST IF DATATYPE RECORD EXISTS                                                
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R4,KEY                                                           
         USING DTYPKEYD,R4                                                      
         MVI   DTYPSYS,DTYPSYSQ    RECORD IDENTIFIER EQUATES                    
         MVI   DTYPTYP,DTYPTYPQ                                                 
         MVC   DTYPSYPG,SFMSYPG    DICTIONARY NAME SPACE FILLED                 
         OC    DTYPSYPG,MYSPACES   SPACE FILL AND CAPITALIZE IF NEEDED          
         MVC   DTYPCODE,8(R3)      DATATYPE NAME                                
         OC    DTYPCODE,MYSPACES   SPACE FILL AND CAPITALIZE IF NEEDED          
         GOTO1 HIGH                                                             
         CLC   KEY(L'DTYPKEY),KEYSAVE  DOES THE ENTRY RECORD EXIST?             
         BE    BE00I               CHECK FOR DEFAULTS                           
         LR    R2,R3               NO, BAD DATATYPE. R3=A(DATATYPE HDR)         
         MVC   GERROR,=AL2(NODTYPE) DATATYPE NOT IN SYS.PROG                    
         B     BLDERROR                                                         
BE00I    DS    0H                                                               
* READ THE RECORD FROM THE DA HALF OF ISDA PAIR WITH GETREC                     
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,AIO,DMWORK         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DATADISP,DATADISP                                                
         MVI   DATADISP+1,DTYPFRST-DTYPKEYD   DATADISP TO 1ST ELEMENT           
         L     R4,AIO              POINT TO THE DATATYPE RECORD READ            
         MVI   ELCODE,DTGENELQ     GET GENERAL INFO ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NO GENERAL INFO ELEMENT               
         ST    R4,AGENINFO         STORE THE ADDRESS FOR LATER USE              
         DROP  R4                                                               
* ROW                                                                           
BE10     DS    0H                                                               
         CLI   CONREC,C'M'         MPANEL?                                      
         BE    BE30                YES, NO ROW NOR COL                          
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BNZ   BE30                YES, PFKEY HAVE NO ROW NOR COL               
         USING SFMFAD,R2                                                        
         CLI   SFMROW,0            NOTHING IN THE ROW?                          
         BE    *+12                                                             
         CLI   SFMROW,C'*'         SAME ROW?                                    
         BNE   *+12                NO                                           
         MVI   PNLEROW,X'00'       YES, VALUE FOR SAME                          
         B     BE20                CHECK COLUMN                                 
         CLI   SFMROW,C'+'         IF NOT '*' MUST BE '+'                       
         BE    BE10A                                                            
BE10ERR  LA    R2,SFMROWH                                                       
         B     BADERR              CHECK COLUMN                                 
BE10A    CLI   SFMROWH+5,1         JUST 1 CHARACTER?                            
         BE    BE10ERR             YES, NEED A NUMBER ALSO                      
         ZIC   RE,SFMROWH+5        GET RELATIVE ROW                             
         BCTR  RE,0                -1 FOR THE '+'                               
         LA    RF,SFMROW+1                                                      
BE10LP   CLI   0(RF),C'0'          SEE IF VALID NUMERIC                         
         BL    BE10ERR                                                          
         CLI   0(RF),C'9'                                                       
         BH    BE10ERR                                                          
         LA    RF,1(RF)            NEXT CHARACTER                               
         BCT   RE,BE10LP           CHECK ALL CHARACTERS INPUTTED                
         ZIC   RE,SFMROWH+5        GET RELATIVE ROW                             
         SH    RE,=H'2'            -1 FOR THE '+', -1 FOR PACK                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),SFMROW+1(0)  SKIP THE '+' SIGN                            
         CVB   R0,DUB                                                           
         STC   R0,PNLEROW          STORE RELATIVE ROW                           
* COLUMN                                                                        
BE20     DS    0H                                                               
         CLI   SFMCOLH+5,0         FIELD INPUT?                                 
         BE    *+12                NO                                           
         CLI   SFMCOL,C'*'         NEXT AVAILABLE COL?                          
         BNE   *+12                NO                                           
         MVI   PNLECOL,X'00'       YES, VALUE FOR NEXT AVAILABLE                
         B     BE30                CHECK DATATYPE                               
         TM    SFMCOLH+4,X'08'     VALID NUMERIC?                               
         BNZ   *+12                YES                                          
         LA    R2,SFMCOLH          NO, ERROR                                    
         B     BADERR                                                           
         ZIC   RE,SFMCOLH+5        GET ABSOLUTE COL LENGTH                      
         BCTR  RE,0                -1 FOR PACK                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),SFMCOL(0)                                                 
         CVB   R0,DUB                                                           
         STC   R0,PNLECOL          STORE ABSOLUTE COL                           
* REPLICATION ID                                                                
BE30     DS    0H                                                               
         TM    TYPEFLAG,X'FC'      THESE HAVE NO REPLICATION ID                 
         BNZ   BE35                                                             
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMRPIH                                                       
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R3,MULRPIH                                                       
         SR    R1,R1                                                            
         CLI   5(R3),0                                                          
         BE    BE30A               NOTHING                                      
         TM    4(R3),X'08'         NUMERIC?                                     
         BZ    BENOTNUM            NO, INVALID NUMERIC                          
         ZIC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R3)                                                   
         CVB   R1,DUB                                                           
         CH    R1,=H'255'                                                       
         BH    BLDINVLD                                                         
BE30A    STC   R1,PNLERPID                                                      
* NAME LENGTH                                                                   
BE35     DS    0H                                                               
         CLI   CONREC,C'M'         MPANEL?                                      
         BE    BE40                YES                                          
         USING SFMFAD,R2           YES                                          
         TM    TYPEFLAG,X'FC'      THESE HAVE NO NAME LENGTH                    
         BNZ   BE40                                                             
         CLI   SFMNLNH+5,0         NAME LENGTH INPUT?                           
         BE    BE40                NO, NONE                                     
         TM    SFMNLNH+4,X'08'     IS FIELD VALID NUMERIC?                      
         BNZ   BE35A               YES                                          
         LA    R2,SFMNLNH                                                       
         MVC   GERROR,=AL2(NOTNUM) NOT NUMERIC                                  
         B     BLDERROR                                                         
BE35A    ZIC   RE,SFMNLNH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),SFMNLN(0)                                                 
         CVB   R0,DUB                                                           
         OR    R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,X'FF'                                                         
         STC   R0,PNLEFLEN                                                      
* NAME                                                                          
BE40     DS    0H                                                               
         TM    TYPEFLAG,X'50'      USER LINE OR KEY?                            
         BNZ   BE50                NO NAMES                                     
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BZ    BE45                NO                                           
         USING PNLPFD,R6                                                        
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMOVRH                                                       
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R3,MULTOPH                                                       
         MVC   PNLPFTXT,8(R3)      JUST COPY 8 CHARACTERS                       
         B     BE50                                                             
         USING PNLELTD,R6                                                       
BE45     DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   BE45M               NO                                           
         USING SFMFAD,R2                                                        
         CLI   SFMOVRH+5,0         OVERRIDE NAME INPUT?                         
         BE    BE50                NO, NULL LENGTH AND NAME                     
         MVC   MELEM2,MELEM                                                     
         TM    SFMOVRH+4,X'20'     VALIDATED BEFORE?                            
         BZ    BE45A               NO, RECENTLY CHANGED                         
         CLI   SFMSEQ+4,0          IS IT A NEW ELEMENT?                         
         BE    BE45A               YES, ACCEPT WHAT USER TYPED                  
         TM    TYPEFLAG,X'08'      TEXTONLY FIELD?                              
         BNZ   BE45A               YES, FIELD HAS NO DEFAULTS                   
         MVI   MINEKEY,PNLELTCQ                                                 
         GOTO1 HEXIN,DMCB,SFMSEQ,MINEKEY+1,L'SFMSEQ-1                           
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PNLERWNL,0          ANY NAME LENGTH?                             
         BNE   BE45A               YES, THERE IS                                
         CLI   PNLERWNM,0          ANYTHING IN OVERRIDE NAME?                   
         BNE   BE45A               YES, THERE IS                                
         MVC   MELEM,MELEM2        NO, MUST BE A DEFAULT                        
         B     BE50                                                             
BE45A    MVC   MELEM,MELEM2                                                     
         CLC   =C'*NONE*',SFMOVR   SPECIFIED BLANKS?                            
         BNE   BE45B               NO                                           
         MVI   PNLERWNL,0          NOTHING IN LENGTH                            
         MVC   PNLERWNM(6),=C'*NONE*'  SPECIAL FOR BLANKS                       
         B     BE50                DONE WITH NAME                               
BE45B    MVC   PNLERWNL,SFMOVRH+5  GET LENGTH OF OVERRIDE LBL                   
         MVC   PNLERWNM,SFMOVR                                                  
         B     BE50                DONE WITH THE NAME                           
* TOP OF MULTIPLE LABEL                                                         
         USING MULFAD,R2                                                        
BE45M    DS    0H                                                               
         CLI   MULTOPH+5,0         TOP NAME INPUT?                              
         BNE   *+12                YES, THERE IS                                
         CLI   MULBOTH+5,0         OVERRIDE NAME INPUT?                         
         BE    BE50                NO NAME AT ALL                               
         MVC   MELEM2,MELEM                                                     
         TM    MULTOPH+4,X'20'     TOP VALIDATED BEFORE?                        
         BZ    BE45M1              NO, RECENTLY CHANGED                         
         TM    MULBOTH+4,X'20'     BOTTOM VALIDATED BEFORE?                     
         BZ    BE45M1              NO, RECENTLY CHANGED                         
         TM    TYPEFLAG,X'20'      SELECT FIELD?                                
         BNZ   BE45M1              YES, SELECT FIELD HAS NO DEFAULTS            
         CLI   MULSEQ,0            NO, IS IT A NEW ELEMENT?                     
         BE    BE45M1              YES, ACCEPT WHAT USER TYPED                  
         MVI   MINEKEY,PNLELTCQ                                                 
         GOTO1 HEXIN,DMCB,MULSEQ,MINEKEY+1,L'SFMSEQ-1                           
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PNLECL1L,0          ANY OVERRIDE?                                
         BNE   BE45M1              YES, THERE IS A TOP                          
         CLI   PNLECL2L,0          NO TOP, ANY BOTTOM?                          
         BNE   BE45M1              YES, THERE IS A BOTTOM                       
         MVC   MELEM,MELEM2        NO, MUST BE DEFAULT                          
         B     BE50                                                             
BE45M1   MVC   MELEM,MELEM2        NO, NOT DEFAULT                              
         CLI   MULTOPH+5,0         NOTHING?  IT MEANS BLANK OR *NONE*           
         BE    BE45M1A             YES, THE OTHER HAS SOMETHING                 
         CLC   =C'*NONE*',MULTOP   SPECIFIED BLANKS?                            
         BNE   BE45M2              NO                                           
BE45M1A  MVI   PNLECL1L,0          NOTHING IN LENGTH                            
         MVC   PNLECL1N(6),=C'*NONE*'  SPECIAL FOR BLANKS                       
         B     BE45M3              DONE WITH NAME                               
BE45M2   MVC   PNLECL1L,MULTOPH+5  GET LENGTH                                   
         MVC   PNLECL1N,MULTOP     COPY TOP                                     
BE45M3   CLI   MULBOTH+5,0         NOTHING?  IT MEANS BLANK OR *NONE*           
         BE    BE45M3A             YES, THE OTHER HAS SOMETHING                 
         CLC   =C'*NONE*',MULBOT   SPECIFIED BLANKS?                            
         BNE   BE45M4              NO                                           
BE45M3A  MVI   PNLECL2L,0          NOTHING IN LENGTH                            
         MVC   PNLECL2N(6),=C'*NONE*'  SPECIAL FOR BLANKS                       
         B     BE50                DONE WITH NAME                               
BE45M4   MVC   PNLECL2L,MULBOTH+5  GET LENGTH                                   
         MVC   PNLECL2N,MULBOT     COPY BOTTOM                                  
* PROTECTED/UNPROTECTED                                                         
BE50     DS    0H                                                               
         TM    TYPEFLAG,X'C8'      PFKEY, USERLINE, OR TEXTONLY?                
         BNZ   BE60                YES, ALWAYS PROTECTED                        
         TM    TYPEFLAG,X'20'      SELECT FIELD ALWAYS UNPROTECTED              
         BNZ   BE70                                                             
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMPROH                                                       
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R3,MULPROH                                                       
         CLI   5(R3),0             PROTECTED FIELD INPUT?                       
         BNE   *+16                YES                                          
         TM    TYPEFLAG,X'14'      KEY?                                         
         BNZ   BE60                YES, DEFAULT IS PROTECTED                    
         B     BE70                DEFAULT FOR REST IS UNPROTECTED              
         CLI   8(R3),C'U'          UNPROTECTED?                                 
         BE    BE70                                                             
         CLI   8(R3),C'P'          PROTECTED                                    
         BE    BE60                                                             
         LR    R2,R3                                                            
         MVC   GERROR,=AL2(PROORUN) (P)ROTECTED OR (U)NPROTECTED                
         B     BLDERROR                                                         
BE60     OI    PNLEATTR,X'20'      SET PROTECTED ATTR BIT                       
* NAME INTENSITY                                                                
BE70     DS    0H                                                               
         TM    TYPEFLAG,X'40'      USER LINE?                                   
         BNZ   BE80                USER LINE HAS NO NAME                        
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMNINH                                                       
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R3,MULNINH                                                       
         CLI   5(R3),0             NAME INTENSITY FIELD INPUT?                  
         BE    BE80                                                             
         CLI   8(R3),C'N'          NAME NORMAL?                                 
         BE    BE80                                                             
         CLI   8(R3),C'H'          NAME HIGH?                                   
         BNE   BE75                                                             
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BZ    *+12                NO                                           
         USING PNLPFD,R6                                                        
         OI    PNLPFATT,X'08'                                                   
         B     BE80                                                             
         USING PNLELTD,R6                                                       
         OI    PNLEATTR,X'08'      SET NAME-HIGH ATTR BIT                       
         B     BE80                                                             
BE75     CLI   8(R3),C'Z'          NAME ZERO?                                   
         BNE   NHZERR                                                           
         TM    TYPEFLAG,X'80'      PFKEY?                                       
         BZ    *+12                NO                                           
         USING PNLPFD,R6           PFKEY DSECT                                  
         OI    PNLPFATT,X'04'                                                   
         B     BE80                                                             
         USING PNLELTD,R6          SEQUENCE DSECT                               
         OI    PNLEATTR,X'04'      SET NAME-ZERO ATTR BIT                       
         B     BE80                                                             
NHZERR   LR    R2,R3                                                            
         MVC   GERROR,=AL2(NHZINTN) NORMAL, HIGH, OR ZERO INTENSITY             
         B     BLDERROR                                                         
* DATA INTENSITY                                                                
BE80     DS    0H                                                               
         TM    TYPEFLAG,X'88'      PFKEY OR TEXTONLY?                           
         BNZ   BLDEXIT             YES, THAT'S ALL FOLKS                        
BE80A    CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMDINH                                                       
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R3,MULDINH                                                       
         CLI   5(R3),0             DATA INTENSITY FIELD INPUT?                  
         BE    BE90                                                             
         CLI   8(R3),C'N'          DATA NORMAL?                                 
         BE    BE90                                                             
         CLI   8(R3),C'H'          DATA HIGH?                                   
         BNE   *+12                                                             
         OI    PNLEATTR,X'01'      SET DATA-HIGH ATTR BIT                       
         B     BE90                                                             
         CLI   8(R3),C'Z'          DATA ZERO?                                   
         BNE   NHZERR              NORMAL, HIGH, OR ZERO INTENSITY              
         OI    PNLEATTR,X'02'      SET DATA-ZERO ATTR BIT                       
* UPPER/LOWER CASE                                                              
BE90     DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMLOWH                                                       
         B     *+8                                                              
         USING MULFAD,R2           NO, MPANEL                                   
         LA    R3,MULLOWH                                                       
         CLI   5(R3),0             U/L FIELD INPUT?                             
         BNE   *+12                YES                                          
         OI    PNLEDEFA,PNLEUPLO   UPPER/LOWER DEFAULT                          
         B     BE100               DONE                                         
         TM    TYPEFLAG,X'70'      SELECT, KEY, OR USER FIELD?                  
         BNZ   BE95                YES, THEY HAVE NO ELEMENT                    
         L     R4,AGENINFO         POINT TO GENERAL INFO ELEMENT                
         USING DTGEND,R4                                                        
         CLC   8(1,R3),DTGENDUL    IS INPUT SAME AS DEFAULT?                    
         DROP  R4                                                               
         BNE   *+12                NO                                           
         OI    PNLEDEFA,PNLEUPLO   YES                                          
         B     BE100               DONE                                         
BE95     CLI   8(R3),C'U'          UPPERCASE ONLY?                              
         BE    BE100                                                            
         CLI   8(R3),C'L'          UPPER/LOWER?                                 
         BE    BE90A                                                            
         LR    R2,R3                                                            
         MVC   GERROR,=AL2(UPPLOWCS)                                            
         B     BLDERROR                                                         
BE90A    OI    PNLEATTR,X'40'      SET ATTR BIT FOR LOWER CASE                  
* SUB-PANEL                                                                     
BE100    TM    TYPEFLAG,X'40'      USER LINE?                                   
         BNZ   BLDEXIT             THAT'S ALL FOR USER LINES                    
BE110    DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                NO, MPANEL                                   
         USING SFMFAD,R2                                                        
         LA    R3,SFMSUBH                                                       
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R3,MULSUBH                                                       
         CLI   5(R3),0             LENGTH FIELD INPUT?                          
         BE    BE115               YES                                          
         TM    4(R3),X'0C'         ALPHA OR NUMERIC?                            
         BZ    BLDINVLD            NEITHER                                      
         MVC   PNLESUBP,8(R3)                                                   
* FIXED/SCROLL                                                                  
BE115    DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BE    BE130               YES, CHECK ID NUM                            
         USING MULFAD,R2                                                        
         TM    TYPEFLAG,X'34'      SELECT OR KEY?                               
         BNZ   BE115B              YES, ALWAYS FIXED                            
         CLI   MULFIXH+5,0         ANY INPUT?                                   
         BE    BE120               DEFAULT SCROLLABLE                           
         CLI   MULFIX,C'N'         FIXED?                                       
         BE    BE120               NO                                           
         CLI   MULFIX,C'Y'         YES                                          
         BE    BE115B                                                           
         LA    R2,MULFIXH                                                       
         B     BYESORNO                                                         
BE115B   OI    PNLEFLAG,PNLEFIXQ   SET FLAG BIT                                 
* BELOW LINE?                                                                   
BE120    DS    0H                                                               
         TM    TYPEFLAG,X'30'      SELECT OR KEY(NOT SUB-PANEL)?                
         BNZ   BE130               YES, NEVER BELOW                             
         CLI   MULBELH+5,0         ANY INPUT?                                   
         BE    BE130               DEFAULT IS NOT BELOW LINE                    
         CLI   MULBEL,C'N'         BELOW LINE?                                  
         BE    BE130               NO                                           
         CLI   MULBEL,C'Y'         YES                                          
         BE    BE120A                                                           
         LA    R2,MULBELH                                                       
BYESORNO MVC   GERROR,=AL2(YESORNO)                                             
         B     BLDERROR                                                         
BE120A   TM    PNLEATTR,X'20'      PROTECTED FIELD?                             
         BNZ   BE120B              YES                                          
         MVC   GERROR,=AL2(BELOPROT)   BELOW FIELDS ARE PROTECTED               
         LA    R2,MULPROH                                                       
         B     BLDERROR                                                         
BE120B   TM    PNLEFLAG,PNLEFIXQ   FIXED?                                       
         BNZ   BE120C              YES                                          
         MVC   GERROR,=AL2(BELOFIXD)   BELOW FIELDS ARE FIXED                   
         LA    R2,MULFIXH                                                       
         B     BLDERROR                                                         
BE120C   OI    PNLEFLAG,PNLEBELO   SET FLAG BIT                                 
* EXTENDED HEADER ID NUMBER                                                     
BE130    DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2           YES                                          
         LA    R3,SFMIDNH                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULIDNH                                                       
         CLI   5(R3),0             LENGTH FIELD INPUT?                          
         BNE   *+12                YES                                          
         OI    PNLEDEFA,PNLEIDNM                                                
         B     BLDEXIT             NO, THAT'S ALL FOLKS                         
         TM    4(R3),X'08'         IS FIELD VALID NUMERIC?                      
         BNZ   BE130A              YES                                          
BENOTNUM LR    R2,R3                                                            
         MVC   GERROR,=AL2(NOTNUM)   NOT NUMERIC                                
         B     BLDERROR                                                         
BE130A   ZIC   RE,5(R3)            GET LENGTH                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R3)                                                   
         CVB   R0,DUB                                                           
         TM    TYPEFLAG,X'30'      SELECT OR KEY FIELD?                         
         BNZ   BE130A1             YES, IT HAS NO ELEMENT                       
         L     R4,AGENINFO         POINT TO GENERAL INFO ELEMENT                
         USING DTGEND,R4                                                        
         CLM   R0,1,DTGENIDN       IS ID NUMBER SAME AS DEFAULT?                
         DROP  R4                                                               
         BNE   *+12                NO                                           
         OI    PNLEDEFA,PNLEIDNM   YES                                          
         B     BLDEXIT             DONE                                         
BE130A1  CH    R0,=H'255'          ABOVE MAXIMUM ID NUMBER?                     
         BNH   BE130B                                                           
BLDINVLD LR    R2,R3                                                            
         MVC   GERROR,=AL2(INVALID)  YES, INVALID ID NUMBER                     
         B     BLDERROR                                                         
BE130B   STC   R0,PNLEIDNO                                                      
BLDEXIT  XMOD1                                                                  
BLDERROR GOTO1 SFMERR                                                           
         DROP  R2,R6                                                            
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
RESEQNCE DS    0H                                                               
         NMOD1 0,**RSEQ**                                                       
         L     RC,0(R1)            USE REGISTERS OF CALLER                      
         L     RA,4(R1)                                                         
         L     R5,8(R1)                                                         
         L     R6,12(R1)                                                        
         L     R9,16(R1)                                                        
*                                                                               
         USING PNLELTD,R6                                                       
         LA    R2,SFMSTH           STARTING POSITION FOR BOTH                   
         MVI   LINENO,1                                                         
         SR    R4,R4               COUNTER OF X'20' ELEMENTS IN RECORD          
         MVI   MINEKEY,PNLELTCQ    GET FIRST ELEMENT IN RECORD                  
         XC    MINEKEY+1(2),MINEKEY+1                                           
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
RESEQ00  CLI   MINERR,0            ANY ERRORS?                                  
         BE    RESEQ05             NO, NONE                                     
         CLI   MINERR,MINEEOF      DONE GOING FORWARD?                          
         BE    RESEQ10             YES                                          
         DC    H'0'                                                             
RESEQ05  CLI   PNLELTC,PNLPFDEQ    READ A PFKEY?                                
         BE    RESEQ10             YES, DONE GOING FORWARD                      
         LA    R4,1(R4)            ONE MORE ELEMENT TO DEAL WITH                
*        CLM   R4,3,PNLESEQ        SAME NUMBER AS COUNTER?                      
*        BE    RESEQ05N            YES, DON'T NEED TO CHANGE                    
*                                                                               
RESEQ0A  DS    0H                                                               
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMSEQH                                                       
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R3,MULSEQH                                                       
         CLI   8(R3),C'P'          PFKEY ON SCREEN LINE?                        
         BE    RESEQ0AZ            YES, PROCEED WITH CHANGE                     
         CLI   8(R3),0             NO SEQUENCE?                                 
         BNE   RESEQ0A5            THERE IS                                     
         CLC   LINENO,INSERTLN     NO SEQUENCE. INSERTED?                       
         BL    RESEQ0AZ            BLANK LINE, PROCEED WITH CHANGE              
         ZICM  R1,INSERTLN,1                                                    
         BZ    RESEQ0AZ                                                         
         ZIC   R0,INSERTNM                                                      
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         CLM   R1,1,LINENO                                                      
         BL    RESEQ0AZ            NO, AFTER IT                                 
*                                                                               
RESEQ0A0 CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+16                                                             
         USING SFMFAD,R2                                                        
         LA    R0,SFMFADX                                                       
         LA    R1,SFMLSLNH                                                      
         B     *+12                                                             
         USING MULFAD,R2                                                        
         LA    R0,MULFADX                                                       
         LA    R1,MULLSLNH                                                      
         CR    R2,R1               LAST LINE ON SCREEN?                         
         BE    RESEQ0AZ            YES, PROCEED WITH CHANGE                     
         AR    R2,R0               NEXT LINE                                    
         IC    R1,LINENO                                                        
         LA    R1,1(R1)                                                         
         STC   R1,LINENO                                                        
         B     RESEQ0A             CHECK THIS LINE                              
*                                                                               
RESEQ0A5 GOTO1 HEXIN,DMCB,8(R3),HEXNUM,L'SFMSEQ-1                               
         CLC   HEXNUM,PNLESEQ      COMPARE LINE SEQ WITH RECORD SEQ             
         BH    RESEQ0AZ            THE LINE'S IS GREATER                        
         BL    RESEQ0A0            LESS, CHECK NEXT LINE IF ANY                 
*                                                                               
RESEQ0AA STCM  R4,3,HEXNUM         SAME, CHANGE IT ON THE SCREEN                
         GOTO1 HEXOUT,DMCB,HEXNUM,8(R3),L'HEXNUM                                
*                                                                               
RESEQ0AZ GOTO1 MINIO,DMCB,('MINDEL',(R5))   DELETE THE ELEMENT                  
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PREVSEQ,PNLESEQ     PREVIOUS SEQUENCE SAME                       
         BNE   *+8                                                              
         STCM  R4,3,PREVSEQ                                                     
         CLC   FIRSTSEQ,PNLESEQ    FIRST SEQUENCE SAME                          
         BNE   *+8                                                              
         STCM  R4,3,FIRSTSEQ                                                    
         CLC   NEWSEQ,PNLESEQ      SEQUENCE OF COPY/MOVE                        
         BNE   *+8                                                              
         STCM  R4,3,NEWSEQ                                                      
         CLC   BEGSEQ,PNLESEQ      BEGINNING SEQUENCE SAME?                     
         BNE   RESEQ0B1                                                         
         CLI   ELCDBEG,PNLPFDEQ    PFKEY FOR BEGINNING?                         
         BE    RESEQ0B1                                                         
         STCM  R4,3,BEGSEQ                                                      
RESEQ0B1 CLC   ENDSEQ,PNLESEQ      END SEQUENCE SAME?                           
         BNE   RESEQ0B2                                                         
         CLI   ELCDEND,PNLPFDEQ    PFKEY FOR END?                               
         BE    RESEQ0B2                                                         
         STCM  R4,3,ENDSEQ                                                      
RESEQ0B2 CLC   AWHERE,PNLESEQ                                                   
         BNE   RESEQ0B3                                                         
         STCM  R4,3,AWHERE                                                      
*        B     RESEQ0BX                                                         
RESEQ0B3 CLC   ARANGE1,PNLESEQ                                                  
         BNE   RESEQ0B4                                                         
         STCM  R4,3,ARANGE1                                                     
*        B     RESEQ0BX                                                         
RESEQ0B4 CLC   ARANGE2,PNLESEQ                                                  
         BNE   *+8                                                              
         STCM  R4,3,ARANGE2                                                     
RESEQ0BX STCM  R4,3,PNLESEQ        SEQUENCE # IS ELEMENT # IN RECORD            
         GOTO1 MINIO,DMCB,('MINADD',(R5))   CHANGED THE SEQUENCE #              
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MINEKEY+1(2),PNLESEQ   SET UP FOR NEXT ELEMENT                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESEQ05N GOTO1 MINIO,DMCB,('MINSEQ',(R5))  GETNEXT ELEMENT IF ANY               
         B     RESEQ00             CHECK THIS ELEMENT                           
         EJECT                                                                  
RESEQ10  CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R1,SFMFADX                                                       
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R1,MULFADX                                                       
         LA    R0,SFMSTH                                                        
RESEQ15  CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMSEQ                                                        
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R3,MULSEQ                                                        
         CLI   0(R3),0             ANYTHING IN THIS LINE?                       
         BE    RESEQ15A            NO                                           
         CLI   0(R3),C'P'          PFKEY LINE?                                  
         BNE   RESEQ20             NO, MUST BE A REGULAR SEQUENCE               
RESEQ15A CR    R2,R0               IS IT POINTING TO THE FIRST LINE?            
         BE    RESEQ20             YES, CAN'T DO ANYTHING ABOUT IT              
         SR    R2,R1                                                            
         B     RESEQ15             NO, LOOP TIL WE GET LINE WITH SEQ #          
*                                                                               
RESEQ20  STCM  R4,3,MINEKEY+1      SET UP FOR GOING BACKWARDS                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))   LAST X'20' ELEMENT                   
RESEQ30  CLI   MINERR,0            ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RESEQ40  LR    R0,R4               GET THE NUMERIC LINE EQUIVALENT              
         MH    R0,=H'100'          FIND NEW NUMBER OF ELEMENT                   
RESEQ45  CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMSEQ                                                        
         B     *+8                                                              
         USING MULFAD,R2                                                        
         LA    R3,MULSEQ                                                        
         CLI   0(R3),C'P'          1ST LINE IS PFKEY?                           
         BE    RESEQ60             YES, PROCEED TO DELETE                       
         CLI   0(R3),0             INSERTED LINE HAS NOTHING                    
         BE    RESEQ47                                                          
         GOTO1 HEXIN,DMCB,0(R3),HEXNUM,L'SFMSEQ-1                               
         CLC   HEXNUM,PNLESEQ      COMPARE #'S NOT EBCDIC                       
         BE    RESEQ50             REPLACE NUMBER ON SCREEN WITH NEW #          
         BL    RESEQ60             NO # TO REPLACE ON SCREEN                    
RESEQ47  LA    R1,SFMSTH           HIGHER                                       
         CR    R2,R1               DON'T GO BEYOND THE LINES                    
         BE    RESEQ60             NO MORE LINES                                
         CLI   CONREC,C'S'         SPANEL?                                      
         BNE   *+12                                                             
         USING SFMFAD,R2                                                        
         LA    R3,SFMFADX                                                       
         B     *+8                                                              
         USING MULFAD,R2           MPANEL                                       
         LA    R3,MULFADX                                                       
         SR    R2,R3               GO TO PREVIOUS LINE                          
         B     RESEQ45             LOOP UNTIL WE GET NUMBER                     
*                                                                               
RESEQ50  STCM  R0,3,HEXNUM         OUTPUT NUMBER TO SCREEN                      
         GOTO1 HEXOUT,DMCB,HEXNUM,0(R3),L'HEXNUM                                
*                                                                               
RESEQ60  GOTO1 MINIO,DMCB,('MINDEL',(R5))  DELETE THE ELEMENT                   
         CLI   MINERR,0            NO ERRORS?                                   
         BE    *+6                 NONE                                         
         DC    H'0'                                                             
         CLC   PREVSEQ,PNLESEQ     PREVIOUS SEQUENCE SAME                       
         BNE   *+8                                                              
         STCM  R0,3,PREVSEQ                                                     
         CLC   FIRSTSEQ,PNLESEQ    FIRST SEQUENCE SAME                          
         BNE   *+8                                                              
         STCM  R0,3,FIRSTSEQ                                                    
         CLC   NEWSEQ,PNLESEQ      SEQUENCE OF COPY/MOVE                        
         BNE   *+8                                                              
         STCM  R0,3,NEWSEQ                                                      
         CLC   BEGSEQ,PNLESEQ      BEGINNING SEQUENCE SAME?                     
         BNE   RESEQ601                                                         
         CLI   ELCDBEG,PNLPFDEQ    PFKEY FOR BEGINNING?                         
         BE    RESEQ601                                                         
         STCM  R0,3,BEGSEQ                                                      
RESEQ601 CLC   ENDSEQ,PNLESEQ      END SEQUENCE SAME?                           
         BNE   RESEQ602                                                         
         CLI   ELCDEND,PNLPFDEQ    PFKEY FOR END?                               
         BE    RESEQ602                                                         
         STCM  R0,3,ENDSEQ                                                      
RESEQ602 CLC   AWHERE,PNLESEQ                                                   
         BNE   RESEQ603                                                         
         STCM  R0,3,AWHERE                                                      
*        B     RESEQ60X                                                         
RESEQ603 CLC   ARANGE1,PNLESEQ                                                  
         BNE   RESEQ604                                                         
         STCM  R0,3,ARANGE1                                                     
*        B     RESEQ60X                                                         
RESEQ604 CLC   ARANGE2,PNLESEQ                                                  
         BNE   *+8                                                              
         STCM  R0,3,ARANGE2                                                     
RESEQ60X STCM  R0,3,PNLESEQ        NEW SEQUENCE NUMBER FOR ELEMENT              
         GOTO1 MINIO,DMCB,('MINADD',(R5))  ADD NEW ELEMENT                      
         CLI   MINERR,0            NO ERRORS?                                   
         BE    *+6                 NONE                                         
         DC    H'0'                                                             
         BCT   R4,RESEQ70                                                       
         B     RSEQEXIT            RETURN TO CALLER                             
RESEQ70  STCM  R4,3,MINEKEY+1      GET SEQUENCE # TO READ BACKWARDS             
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         B     RESEQ30             CHECK                                        
RSEQEXIT XMOD1                                                                  
RSEQERR  GOTO1 SFMERR                                                           
         DROP  R2,R6                                                            
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
COPYPNLE DS    0H                                                               
         NMOD1 0,**CPYP**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R5,8(R1)                                                         
         L     R6,12(R1)                                                        
         L     R9,16(R1)                                                        
         LA    R8,MINMKEY                                                       
         USING PNLKEYD,R8                                                       
         MVI   PNLKSYS,PNLKSYSQ    SYSTEM                                       
         MVI   PNLKSTYP,PNLKSTYQ   KEY TYPE                                     
* SYSTEM.PROGRAM DICTIONARY                                                     
         LA    R2,SPCSYP1H         GET SYS.PRG                                  
         CLI   5(R2),0             MUST BE PRESENT                              
         BE    CPMISSNG                                                         
         CLI   5(R2),5             MUST BE 5 CHARS                              
         BNE   CPINVLD                                                          
         MVC   PNLKSYPG,8(R2)                                                   
* FOR WHICH PANEL                                                               
         LA    R2,SPCPNL1H         GET PANEL NAME                               
         CLI   5(R2),0             MUST BE PRESENT                              
         BE    CPMISSNG                                                         
         MVC   PNLKNAME,8(R2)                                                   
         OC    PNLKNAME,CPSPACES   BLANK PAD                                    
* WHICH TYPE?  SINGLE OR MULTIPLE                                               
         MVC   PNLKTYPE,CONREC     SINGLE OR MULTIPLE IN RECORD TYPE            
* WHAT AGENCY, IF ANY?                                                          
         LA    R2,SPCAGY1H                                                      
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   PNLKAGY,8(R2)       AGENCY                                       
* WHAT MEDIA, IF ANY?                                                           
         LA    R2,SPCMED1H                                                      
         CLI   5(R2),0             MEDIA HAS SOMETHING                          
         BE    CPYPNL10            NO                                           
         CLI   SPCAGY1H+5,0        YES, AGENCY DOES NOT?                        
         BNE   *+14                                                             
         MVC   GERROR,=AL2(NDAGENCY) YES, NEED AGENCY BEFORE MEDIA              
         B     CPERROR                                                          
         MVC   PNLKMED,8(R2)       MEDIA                                        
* WHAT CLIENT, IF ANY?                                                          
CPYPNL10 LA    R2,SPCCLT1H                                                      
         CLI   5(R2),0             CLIENT HAS SOMETHING                         
         BE    CPYPNL20                                                         
         CLI   SPCMED1H+5,0        YES, MEDIA DOES NOT?                         
         BNE   *+14                                                             
         MVC   GERROR,=AL2(NDMEDIA) YES, NEED MEDIA BEFORE CLIENT               
         B     CPERROR                                                          
         MVC   PNLKCLT,8(R2)       CLIENT                                       
*                                                                               
CPYPNL20 DS    0H                                                               
         LA    R2,SPCSYP1H                                                      
         GOTO1 MINIO,DMCB,('MINOPN',(R5))   GET PANEL                           
         CLI   MINERR,0                                                         
         BE    CPYPNL30            IT EXISTS                                    
         CLI   MINERR,MINESNF                                                   
         BE    *+6                 IT DOESN'T                                   
         DC    H'0'                                                             
         MVC   GERROR,=AL2(NOTFOUND)  IT DOES, CAN'T COPY                       
         B     CPERROR                                                          
*                                                                               
* SYSTEM.PROGRAM DICTIONARY                                                     
CPYPNL30 LA    R2,SPCSYP2H         GET SYS.PRG FOR FROM                         
         CLI   5(R2),0             MUST BE PRESENT                              
         BE    CPMISSNG                                                         
         CLI   5(R2),5             MUST BE 5 CHARS                              
         BNE   CPINVLD                                                          
         MVC   SYSPRG1,8(R2)                                                    
         MVC   SYSPRG2,8(R2)       MAKE A COPY                                  
* FOR WHICH PANEL                                                               
         LA    R2,SPCPNL2H         GET PANEL NAME FOR FROM                      
         CLI   5(R2),0             MUST BE PRESENT                              
         BE    CPMISSNG                                                         
         MVC   PANEL1,8(R2)                                                     
         OC    PANEL1,CPSPACES     BLANK PAD                                    
* WHAT AGENCY, IF ANY?                                                          
         LA    R2,SPCAGY2H                                                      
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   AGENCY1,8(R2)       AGENCY                                       
* WHAT MEDIA, IF ANY?                                                           
         LA    R2,SPCMED2H                                                      
         CLI   5(R2),0             MEDIA HAS SOMETHING                          
         BE    CPYPNL40            NO                                           
         CLI   SPCAGY2H+5,0        YES, AGENCY DOES NOT?                        
         BNE   *+14                                                             
         MVC   GERROR,=AL2(NDAGENCY) YES, NEED AGENCY BEFORE MEDIA              
         B     CPERROR                                                          
         MVC   MEDIA1,8(R2)        MEDIA                                        
* WHAT CLIENT, IF ANY?                                                          
CPYPNL40 LA    R2,SPCCLT2H                                                      
         CLI   5(R2),0             CLIENT HAS SOMETHING                         
         BE    CPYPNL50                                                         
         CLI   SPCMED2H+5,0        YES, MEDIA DOES NOT?                         
         BNE   *+14                                                             
         MVC   GERROR,=AL2(NDMEDIA) YES, NEED MEDIA BEFORE CLIENT               
         B     CPERROR                                                          
         MVC   CLIENT1,8(R2)       CLIENT                                       
*                                                                               
CPYPNL50 DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R2,SPCSYP2H                                                      
         LA    R8,KEY                                                           
         USING PNLKEYD,R8                                                       
         MVI   PNLKSYS,PNLKSYSQ                                                 
         MVI   PNLKSTYP,PNLKSTYQ                                                
         MVC   PNLKSYPG,SYSPRG1                                                 
         MVC   PNLKNAME,PANEL1                                                  
         MVC   PNLKAGY,AGENCY1                                                  
         MVC   PNLKMED,MEDIA1                                                   
         MVC   PNLKCLT,CLIENT1                                                  
         MVC   PNLKTYPE,CONREC                                                  
         GOTO1 HIGH                PANEL EXISTS ALREADY?                        
         CLC   KEY(L'PNLKMAST),KEYSAVE                                          
         BNE   *+14                NO                                           
         MVC   GERROR,=AL2(RECEXIST)  YES                                       
         B     CPERROR                                                          
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         LA    R8,MINMKEY                                                       
         USING PNLKEYD,R8                                                       
         CLC   SYSPRG2,PNLKSYPG                                                 
         BNE   DIFFSYPG            DIFFERENT SYS.PRG                            
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'PNLKMAST),KEY                                          
         GOTO1 MINIO,DMCB,('MINCPY',(R5))                                       
         CLI   MINERR,0                                                         
         BE    CPYPNLEX            DONE COPYING                                 
         DC    H'0'                                                             
*                                                                               
DIFFSYPG DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         LA    R6,MELEM                                                         
         USING PNLELTD,R6                                                       
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
CPYPNL60 CLI   MINERR,0                                                         
         BE    CPYPNL70            NO ERRORS                                    
         CLI   MINERR,MINEEOF                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
CPYPNL65 MVC   SYSPRG2,PNLKSYPG    FAKE OUT CODE TO DO COPY                     
         B     CPYPNL50                                                         
CPYPNL70 CLI   PNLELTC,PNLPFDEQ    PFKEY?                                       
         BE    CPYPNL65            NO MORE VALIDATION                           
         TM    PNLEFLAG,X'5C'      KEYWORDS?                                    
         BNZ   CPYPNLNX            GET NEXT ELEMENT                             
         CLC   =C'USER',PNLEDTYP   KEYWORDS?                                    
         BE    CPYPNLNX            GET NEXT ELEMENT                             
         CLC   =C'SEL',PNLEDTYP                                                 
         BE    CPYPNLNX                                                         
         CLC   =C'KEY',PNLEDTYP                                                 
         BE    CPYPNLNX                                                         
         CLC   =C'TEXT',PNLEDTYP                                                
         BE    CPYPNLNX                                                         
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R4,KEY                                                           
         USING DTYPKEYD,R4                                                      
         MVI   DTYPSYS,DTYPSYSQ    RECORD IDENTIFIER EQUATES                    
         MVI   DTYPTYP,DTYPTYPQ                                                 
         MVC   DTYPSYPG,SYSPRG1    DICTIONARY NAME                              
         MVC   DTYPCODE,PNLEDTYP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'DTYPKEY),KEYSAVE   DATATYPE EXISTS?                        
         BNE   CPNOTIN                NO                                        
CPYPNLNX GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         B     CPYPNL60                                                         
*                                                                               
CPYPNLEX XMOD1 2                                                                
*                                                                               
CPSPACES DC    C'        '                                                      
SYSPRG1  DS    CL5                                                              
SYSPRG2  DS    CL5                                                              
PANEL1   DS    CL8                                                              
AGENCY1  DS    CL2                                                              
MEDIA1   DS    CL1                                                              
CLIENT1  DS    CL3                                                              
*                                                                               
CPNOTIN  MVC   GERROR,=AL2(NTINSYPG)   NO                                       
         MVI   GLTXT,L'PNLEDTYP                                                 
         LA    R1,PNLEDTYP                                                      
         STCM  R1,7,GATXT                                                       
         B     CPERROR                                                          
CPINVLD  MVC   GERROR,=AL2(INVALID)                                             
         B     CPERROR                                                          
CPMISSNG MVC   GERROR,=AL2(MISSING)                                             
CPERROR  GOTO1 SFMERR                                                           
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ANY CHANGES MADE TO THE DETAIL LINES OF CTSFMFA (TA0AFA)                      
* MUST BE MIRRORED HERE.  THIS DSECT COVERS THE DETAIL LINES.                   
* FOR SPANEL RECORDS.                                                           
SFMFAD   DSECT                                                                  
*                                                                               
SFMSELH  DS    CL8                 SELECT                                       
SFMSEL   DS    CL3                                                              
SFMENTH  DS    CL8                 DATATYPE ENTRY                               
SFMENT   DS    CL7                                                              
SFMROWH  DS    CL8                 RELATIVE ROW                                 
SFMROW   DS    CL3                                                              
SFMCOLH  DS    CL8                 ABSOLUTE OR RELATIVE COLUMN                  
SFMCOL   DS    CL3                                                              
SFMRPIH  DS    CL8                 REPLICATION ID                               
SFMRPI   DS    CL3                                                              
SFMNLNH  DS    CL8                 NAME'S LENGTH                                
SFMNLN   DS    CL2                                                              
SFMOVRH  DS    CL8                 OVERRIDE NAME                                
SFMOVR   DS    CL24                                                             
SFMPROH  DS    CL8                 PROTECTED?                                   
SFMPRO   DS    CL1                                                              
SFMNINH  DS    CL8                 NAME'S INTENSITY                             
SFMNIN   DS    CL1                                                              
SFMDINH  DS    CL8                 DATA'S INTENSITY                             
SFMDIN   DS    CL1                                                              
SFMLOWH  DS    CL8                 UPPER/LOWER CASE                             
SFMLOW   DS    CL1                                                              
SFMSUBH  DS    CL8                 SUB-PANEL                                    
SFMSUB   DS    CL1                                                              
SFMIDNH  DS    CL8                 EXTENDED HDR ID NUMBER                       
SFMIDN   DS    CL3                                                              
SFMSEQH  DS    CL8                 SEQUENCE NUMBER                              
SFMSEQ   DS    CL5                                                              
*                                                                               
SFMFADX  EQU   *-SFMFAD                                                         
         EJECT                                                                  
* ANY CHANGES MADE TO THE DETAIL LINES OF CTSFMFE (TA0AFE)                      
* MUST BE MIRRORED HERE.  THIS DSECT COVERS THE DETAIL LINES                    
* FOR MPANEL RECORDS.                                                           
MULFAD   DSECT                                                                  
*                                                                               
MULSELH  DS    CL8                 SELECT                                       
MULSEL   DS    CL3                                                              
MULENTH  DS    CL8                 DATATYPE ENTRY                               
MULENT   DS    CL7                                                              
MULRPIH  DS    CL8                 REPLICATION ID                               
MULRPI   DS    CL3                                                              
MULTOPH  DS    CL8                 TOP NAME                                     
MULTOP   DS    CL12                                                             
MULBOTH  DS    CL8                 BOTTOM NAME                                  
MULBOT   DS    CL12                                                             
MULPROH  DS    CL8                 PROTECTED?                                   
MULPRO   DS    CL1                                                              
MULNINH  DS    CL8                 NAME'S INTENSITY                             
MULNIN   DS    CL1                                                              
MULDINH  DS    CL8                 DATA'S INTENSITY                             
MULDIN   DS    CL1                                                              
MULLOWH  DS    CL8                 UPPER/LOWER CASE                             
MULLOW   DS    CL1                                                              
MULSUBH  DS    CL8                 SUB-PANEL                                    
MULSUB   DS    CL1                                                              
MULFIXH  DS    CL8                 FIXED/SCROLLABLE                             
MULFIX   DS    CL1                                                              
MULBELH  DS    CL8                 BELOW LINE                                   
MULBEL   DS    CL1                                                              
MULIDNH  DS    CL8                 EXTENDED HDR ID NUMBER                       
MULIDN   DS    CL3                                                              
MULSEQH  DS    CL8                 SEQUENCE NUMBER                              
MULSEQ   DS    CL5                                                              
*                                                                               
MULFADX  EQU   *-MULFAD                                                         
         EJECT                                                                  
*++INCLUDE DDSPLWORKD                                                           
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* MORE ERROR MESSAGES   IN C/GEN                                                
NDAGENCY EQU   230                 NEED AGENCY BEFORE MEDIA INPUT               
NDMEDIA  EQU   231                 NEED MEDIA BEFORE CLIENT INPUT               
MPANONLY EQU   232                 MPANEL ONLY FIELDS                           
DUPPFNN  EQU   233                 DUPLICATE PFKEY                              
NODTYPE  EQU   234                 DATATYPE NOT IN SYS.PROG                     
PROORUN  EQU   235                 (P)ROTECTED OR (U)NPROTECTED                 
NHZINTN  EQU   236                 (N)ORMAL, (H)IGH, OR (Z)ERO INTENS.          
UPPLOWCS EQU   237                 (U)PPERCASE OR (L)OWERCASE                   
YESORNO  EQU   238                 (Y)ES OR (N)O                                
INVPFKEY EQU   239                 INVALID PFKEY                                
INVUSER  EQU   240                 INVALID USER LINE                            
INVSELKY EQU   241                 INVALID LENGTH FOR SEL OR KEY                
DLENKEY  EQU   242                 A NON-ZERO LENGTH MUST BE ENTERED            
DETAILER EQU   243                 ERROR IN DETAIL SPECIFICATION                
SPANONLY EQU   244                 SPANEL ONLY FIELDS                           
NUMLINER EQU   245                 # OF LINES TO SCROLL 1-14                    
NTINSYPG EQU   246                 DATATYPE  &T  NOT IN SYS.PRG                 
BELOPROT EQU   249                 BELOW FIELDS ARE PROTECTED                   
BELOFIXD EQU   250                 BELOW FIELDS ARE FIXED                       
         SPACE 2                                                                
*++INCLUDE DDSPOOLD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*++INCLUDE CTSFMFFD                                                             
         PRINT OFF                                                              
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         SPACE 2                                                                
         ORG   CONTAGH                                                          
*++INCLUDE CTSFMFED                                                             
         PRINT OFF                                                              
       ++INCLUDE CTSFMFED                                                       
         PRINT ON                                                               
         SPACE 2                                                                
         ORG   CONTAGH                                                          
*++INCLUDE CTSFMFAD                                                             
         PRINT OFF                                                              
       ++INCLUDE CTSFMFAD                                                       
         PRINT ON                                                               
         SPACE 2                                                                
         ORG   CONTAGH                                                          
*++INCLUDE CTSFMEAD                                                             
         PRINT OFF                                                              
       ++INCLUDE CTSFMEAD                                                       
         PRINT ON                                                               
         ORG   SFMWORK+16                                                       
ELCDBEG  DS    XL1                 ELEMENT CODE OF BEGINNING SEQ                
BEGSEQ   DS    XL2                 WHAT SEQUENCE WE STARTED WITH                
ELCDEND  DS    XL1                 ELEMENT CODE OF END SEQUENCE                 
ENDSEQ   DS    XL2                 WHAT SEQUENCE WAS ENDED WITH                 
INSERTLN DS    XL1                 SAVED LINE NUMBER OF INSERTION               
INSERTNM DS    XL1                 NUMBER OF LINES TO INSERT                    
SELACTN  DS    CL1                 SELECTION ACTION                             
*                                  B - CC                                       
*                                  C - COPY LINE                                
*                                  D - DD                                       
*                                  M - MOVE LINE                                
*                                  N - MM                                       
*                                  R - RR                                       
ARNG1COD DS    XL1                 ELEMENT CODE OF TOP                          
ARANGE1  DS    XL2                 A(TOP) SEQUENCE                              
ARNG2COD DS    XL1                 ELEMENT CODE OF BOTTOM                       
ARANGE2  DS    XL2                 A(BOTTOM) SEQUENCE                           
CPYMOVLN DS    XL1                 # OF LINES FOR COPY/MOVE                     
BEFAFTER DS    CL1                 BEFORE/AFTER                                 
AWHERE   DS    XL2                 A(SEQUENCE)                                  
         SPACE 2                                                                
*++INCLUDE CTSFMWORKD                                                           
         PRINT OFF                                                              
       ++INCLUDE CTSFMWORKD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**  MY STORAGE                                                                  
         ORG   SYSSPARE                                                         
ATEMPCOD DS    XL1                 ELEMENT CODE OF TEMPORARY                    
ATEMP    DS    XL2                 A(TEMPORARY)                                 
PREVSEQ  DS    XL2                 SEQUENCE NUMBER OF PREV ELEM                 
FIRSTSEQ DS    XL2                 SEQUENCE NUMBER OF FIRST ELEM CPY/MV         
NEWSEQ   DS    XL2                 SEQUENCE NUMBER OF COPY/MOVE                 
ADDRESS  DS    XL2                 SEQUENCE NUMBER FOR WHERE                    
CHANGED  DS    X                   FLAG FOR DATA CHANGED                        
*                                  X'80' - RECORD CHANGED                       
*                                  X'01' - LINE CHANGED                         
TYPEFLAG DS    X                                                                
*                                  X'80' - PFKEY## DEFINED                      
*                                  X'40' - USER LINE DEFINED                    
*                                  X'20' - SELECT FIELD DEFINED                 
*                                  X'10' - KEY FIELD DEFINED                    
*                                  X'08' - TEXT ONLY FIELD DEFINED              
*                                  X'04' - SUB-PANEL KEY                        
NWINSERT DS    XL1                 NEW INSERT LINE NUMBER                       
NWINSRTN DS    XL1                 NUMBER OF INSERT LINES                       
LINENO   DS    XL1                 LINE NUMBER OF SCREEN                        
NUMLINES DS    XL1                 NUMBER OF LINES FOR THE ACTION               
SCRLAMNT DS    XL1                                                              
SCRLINES EQU   14                  MAXIMUM NUMBER OF LINES                      
TMPACTN  DS    CL3                 TEMPORARY STORAGE FOR ACTION                 
HEXNUM   DS    XL2                 HEX EQUIV OF SEQ NUMBER ON SCREEN            
MELEM    DS    XL100                                                            
MELEM2   DS    XL100                                                            
MINBLOCK DS    XL420                                                            
MINIO    DS    A                                                                
AGENINFO DS    A                   A(GEN INFO ELEMENT) -> R4                    
         EJECT                                                                  
*++INCLUDE DDMINBLK                                                             
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*++INCLUDE CTGENDTYPE                                                           
         PRINT OFF                                                              
       ++INCLUDE CTGENDTYPE                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE CTGENPNL                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085CTSFM0A   05/01/02'                                      
         END                                                                    
