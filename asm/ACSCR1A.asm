*          DATA SET ACSCR1A    AT LEVEL 086 AS OF 11/15/16                      
*PHASE T60C1AC                                                                  
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 84 AS OF 05/09/06         *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'RECAP SCREEN'                                                   
T60C1A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C1A,RA,RR=RE                                                 
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         L     RC,APALOCAL                                                      
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         CLC   ATWA,ACURSOR        Insure cursor was in a field                 
         BE    SCR01               NO,   SET  CURSOR                            
         CLI   TWALREC,RECRECAP    IF    1ST  TIME IN,  THEN SET CURSOR         
         BE    SCR02               NO,   SKIP                                   
*                                                                               
SCR01    LA    RE,RCACODEH         FORMAT     CODE FIELD                        
         ST    RE,ACURSOR                                                       
*                                                                               
SCR02    DS    0H                                                               
         CLI   APMODE,APMVALK                                                   
         BNE   SCR10                                                            
         LA    R2,SCRTXT           TABLE OF   SCREEN    UPDATES                 
*                                                                               
SCR05    CLI   0(R2),X'FF'         END   OF   TABLE?                            
         BE    SCR10               FINISHED                                     
         L     R4,0(,R2)           GET   HEADER    ADDRESS                      
         AR    R4,R5                                                            
         OI    6(R4),FVOXMT        TRANSMIT   FIELD                             
         L     R3,4(,R2)           GET   SCREEN    TEXT NUMBER                  
         GOTO1 TEXTGET,APPARM,(R3),(R4),0                                       
         LA    R2,8(,R2)           BUMP  TO   NEXT                              
         B     SCR05                                                            
         EJECT ,                                                                
SCR10    DS    0H                                                               
         CLI   APMODE,APMVALK      VALKEY ?                                     
         BE    SCR100              YES, SKIP THIS ROUTINE                       
         CLI   APMODE,APMDISK      DISKEY ?                                     
         BE    SCR100              YES, SKIP THIS ROUTINE                       
         CLI   APACTN,ACTCPY       ACTION    COPY ?                             
         BE    SCR100              YES, SKIP THIS ROUTINE                       
*                                                                               
         L     RE,ACURSOR          CURRENT   CURSOR    LOCATION                 
         LA    R1,RCACODEH         DEFAULT   CURSOR    LOCATION                 
         CR    RE,R1               BEFORE    DEFAULT   LOCATION ?               
         BL    SCR20               YES, USE  DEFAULT   LOCATION                 
         LA    RF,RCAF1RHH         ->        1ST  ROW  HEADER                   
         CR    RE,RF               BEFORE    1ST  ROW  HEADER ?                 
         BNL   SCR20               NO,  USE  DEFAULT   LOCATION                 
*                                                                               
         TM    1(RE),X'20'         IN   A    PROTECTED FIELD ?                  
         BZ    SCR30               NO,  OKAY AS   IS                            
*                                                                               
SCR20    DS    0H                                                               
         ST    R1,ACURSOR          USE  DEFAULT   CURSOR                        
*                                                                               
*                                  ************************************         
SCR30    DS    0H                  * MOVE CURSOR TO FORMAT LINE       *         
*                                  ************************************         
         L     R1,ACURSOR          GET  CURSOR    FIELD                         
         LA    R6,MAXRPT#          MAX  NUM  OF   REPORTS                       
         LA    R2,RCACODEH         ->   FORMAT'S  FORMAT    HEADER              
         LA    R3,RCAF1PRH         ->   FORMAT'S  PROFILE   HEADER              
         LA    R4,RCAF1INH         ->   FORMAT'S  INTERLEAVE     HEADER         
*                                                                               
SCR40    DS    0H                                                               
         LA    RE,RCAPFLNQ-1(,R2) ->   END  OF   FORMAT    FIELD                
         CR    R1,R2               CURSOR    IN   THIS FIELD ?                  
         BL    SCR50               LOW, NOT  IN   THIS REPORT                   
         CR    R1,RE               CURSOR    IN   THIS FIELD ?                  
         BNH   SCR60               NOT  HIGH,     SET  CURSOR                   
*                                                                               
         LA    RE,RCAPRLNQ-1(,R3)  ->   END  OF   PROFILE   FIELD               
         CR    R1,R3               CURSOR    IN   THIS FIELD ?                  
         BL    SCR50               LOW, NOT  IN   THIS REPORT                   
         CR    R1,RE               CURSOR    IN   THIS FIELD ?                  
         BNH   SCR60               NOT  HIGH,     SET  CURSOR                   
*                                                                               
         LA    RE,RCAINLNQ-1(,R4)  ->   END  OF   INTERLEAVE     FIELD          
         CR    R1,R4               CURSOR    IN   THIS FIELD ?                  
         BL    SCR50               LOW, NOT  IN   THIS REPORT                   
         CR    R1,RE               CURSOR    IN   THIS FIELD ?                  
         BNH   SCR60               NOT  HIGH,     SET  CURSOR                   
*                                                                               
SCR50    DS    0H                  NOT  IN   FIELD                              
         LA    R2,RCAPFLNQ(,R2)    ->   NEXT FORMAT    FIELD                    
*                                  ->   NEXT PROFILE   FIELD                    
         LA    R3,RCAPRLNQ(,R3)    ->   NEXT PROFILE   FIELD                    
         LA    R4,RCAINLNQ(,R4)    ->   NEXT INTERLEAVE     FIELD               
         BCT   R6,SCR40            TRY  NEXT FORMAT                             
*                                                                               
SCR60    DS    0H                                                               
         ST    R2,ACURSOR          SET  THE  CURSOR                             
         EJECT ,                                                                
         USING RESRECD,R2                                                       
SCR100   LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     EXIT                                                             
         B     EXIT                RESTORE                                      
         B     EXIT                VALSEL                                       
         B     EXIT                GETSEL                                       
         B     EXIT                DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  MODULE/SUBROUTINE EXIT LOGIC                                       *         
***********************************************************************         
         SPACE 1                                                                
EXIT     CLI   APMODE,APMVALR      IN    VALIDATE  RECORD                       
         BNE   EXIT90              NO,   SKIP                                   
         CLC   FVMSGNO,=AL2(FVFOK) ANY   ERRORS    FOUND ?                      
         BNE   EXIT80              YES,  SKIP                                   
         CLI   APPFKEY,0           ANY   PF   KEY  DEPRESSED ?                  
         BNE   EXIT90              YES,  SKIP                                   
         TM    TWASWPST,TWASWAP    SWAP  TO   NEW  RECORD    ACTION ?           
         BO    EXIT90              YES,  SKIP                                   
         MVC   APCURSOR,ACURSOR    NO,   SET  APPLICATION    CURSOR             
         B     EXIT90              EXIT                                         
*                                                                               
*                                  ERRORS     FOUND                             
EXIT80   CLI   APPFKEY,0           ANY   PF   KEY  DEPRESSED ?                  
         BE    EXIT90              NO,   SKIP                                   
         CLI   APPFKEY,PFK14       REQUEST    PF   KEY ?                        
         BE    EXIT90              YES,  STOP SWAP                              
         TM    TWASWPST,TWASWAP    SWAP  TO   NEW  RECORD    ACTION ?           
         BZ    EXIT90              NO,   SKIP                                   
         MVC   FVMSGNO,=AL2(FVFOK) ALLOW SWAP                                   
*                                                                               
EXIT90   CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
*                                                                               
         TM    TWASWPST,TWASWAP    SWAP  TO   NEW  RECORD ACTION ?              
         BZ    EXIT95              NO                                           
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
         MVI   APMODE,APMSWP             SWAP                                   
         MVC   APPARM(1),TWASWPRE        SWAP RECORD                            
         MVC   APPARM+1(1),TWASWPAC      SWAP ACTION                            
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   CLC   FVMSGNO,=AL2(FVFOK) ANY   ERRORS    FOUND ?                      
         BE    XIT                 NO,   EXIT                                   
         CLI   APMODE,APMDISR      MODE  IS   DISPLAY   RECORD ?                
         BNE   XIT                                                              
         OI    APINDS2,APIOVROK    SET   OVERLAY   IS   HAPPY                   
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE KEY                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   DS    0H                                                               
         MVCDD MAIN,AC#RSMAN                                                    
         MVCDD REPORT,AC#RSRPT                                                  
         GOTO1 VDICTAT,APPARM,C'TU  ',(L'MAIN,MAIN),0                           
         GOTO1 VDICTAT,APPARM,C'TU  ',(L'REPORT,REPORT),0                       
*                                                                               
         MVI   NEWKEY,NO                                                        
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY    CODE                              
         GOTO1 AFVAL,RCACODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    RCACODE+4,FVITHIS   ANY   INPUT?                                 
         BZ    *+10                NO                                           
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   RCACODE,SAVFORM                                                  
         OI    RCACODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 DS    0H                                                               
         MVC   SAVFORM,RESKFORM                                                 
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORD+IOACCFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ  FOR  UPDATE                            
         GOTO1 AIO                                                              
         BL    VALKEY99            IO    ERROR                                  
         BNE   VALKEY20                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
*                                                                               
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    TWAMODE,TWAMDFR     2ND   PASS IN   ACTION    COPY ?             
         BZ    VALKEY98                                                         
         CLI   APACTN,ACTCPY                                                    
         BNE   VALKEY98                                                         
         OI    APINDS,APIOKADD     TURN  ON   TO   TRICK    ACTION COPY         
         B     VALKEY98                                                         
*                                                                               
VALKEY20 TM    IOERR,IOEDEL        IS    RECORD    MARKED    DELETED            
         BNZ   VALKEY99            OK    TO   ADD  RECORD                       
         MVI   APINDS,APIOKADD     RECORD     NOT ON FILE, SO OK TO ADD         
         L     R2,AIOAREA1                                                      
         XC    RESKEY(256),RESKEY  RESET AIO  AREA                              
*                                                                               
VALKEY98 DS    0H                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 L     R3,=A(PROFTEXT)                                                  
         A     R3,APRELO                                                        
         LA    R0,MAXPROF                                                       
VALKY99A CLC   APREPJCL,0(R3)      MATCH ON JCL (REPORT TYPE)                   
         BE    VALKY99C                                                         
         LA    R3,PROFLNQ(,R3)                                                  
         BCT   R0,VALKY99A         IF FALL THOUGH, THEN SET TO DEFAULT          
*                                                                               
         USING RECTABD,R2                                                       
VALKY99C L     R2,ACRECTAB                                                      
         SR    R1,R1                                                            
*                                                                               
VALKY99D ICM   R1,1,RECELEN                                                     
         BNZ   *+6                                                              
         DC    H'00'               NO MATCH, SOMETHING WRONG                    
*                                                                               
         CLC   RECNUMB,1(R3)       MATCH RECORD NUMBER                          
         BE    VALKY99F                                                         
         AR    R2,R1                                                            
         B     VALKY99D                                                         
*                                                                               
VALKY99F MVC   RCAPROF,SPACES                                                   
         MVC   RCAPROF(L'RECNAME),RECNAME                                       
         OI    RCAPROFH+6,FVOXMT                                                
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   DS    0H                                                               
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE RECORD                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALREC   DS    0H                                                               
*                                  EXPAND    MAIN AND  REPORT                   
*                                                                               
*                                  ************************************         
*                                  *  DELETE OLD RECAP ELEMENTS       *         
*                                  ************************************         
         L     R1,AIOAREA1         ->   RECORD                                  
         MVC   RPTKYWLN,SPACES                                                  
         MVI   APELCODE,RCPELQ     RECAP     ELEMENT                            
         GOTO1 GETEL               ANY  RECAP     ELEMENTS ?                    
         BNE   VALREC10            NO,  SKIP DELETE                             
*                                                                               
         L     R1,AIOAREA1         ->   RECORD                                  
         MVI   APELCODE,RCPELQ     RECAP     ELEMENT                            
*        MVI   ELEMSEQ,X'00'                                                    
         GOTO1 DELEL               DELETE    ALL  RECAP     ELEMENTS            
*                                  ************************************         
*                                  * VALIDATE FORMAT,PROFILE,INTERLV  *         
*                                  ************************************         
VALREC10 L     R2,AIOAREA1         ->   RECORD                                  
         SR    R6,R6                                                            
         LA    R0,MAXRPT#          GET  NUM  OF   REPORTS                       
         MVI   RCP#RPTS,0          CLEAR     NUM  OF   REPORTS                  
*                                                                               
VALREC20 GOTO1 VALRPTN,(R6)        VALIDATE  BASIC,    DISPLAY ROWS             
         BNE   VALREC99            NOT  OKAY,     EXIT                          
         LA    R6,1(,R6)                                                        
         BCT   R0,VALREC20         TRY  NEXT FORMAT                             
*                                                                               
         CLI   RCP#RPTS,1          ONLY ONE  VALID FORMAT ?                     
         BNH   VALREC95            YES, DO   NOT   CREATE RECAP EL.S            
         EJECT ,                                                                
*                                  ************************************         
*                                  *  ADD RECAP ELEMENTS              *         
*                                  ************************************         
         LA    R0,MAXRPT#          GET  MAX  NUM  OF   REPORTS                  
         SR    R8,R8               NUM  OF   REPORT                             
         LA    R3,APELEM           ->   APELEM                                  
         LA    R2,RCACODEH         ->   1ST  FORMAT -  FORMAT    HDR            
         LA    R4,RCAF1PRH         ->   1ST  FORMAT -  PROFILE                  
         LA    R6,RCAF1INH         ->   1ST  FORMAT -  INTERLEAVE               
*                                                                               
         USING RCPELD,R3           MAP  RECAP     ELEMENT                       
VALREC22 DS    0H                                                               
         GOTO1 AFVAL,(R2)          ANY   FORMAT CODE ?                          
         BNE   VALREC25            NONE, CHECK NEXT                             
         XC    APELEM,APELEM       CLEAR     APELEM                             
         MVI   RCPEL,RCPELQ        ELEMENT   CODE                               
         MVI   RCPLN,RCPLNQ        ELEMENT   LENGTH                             
         STC   R8,RCPSEQ           SEQUENCE  NUMBER                             
         LA    R8,1(,R8)           NEXT REPORT                                  
         MVC   RCPCODE,FVIFLD      FORMAT    CODE                               
*                                                                               
         GOTO1 AFVAL,(R4)          Profile field                                
         CLC   REPORT,FVIFLD       Use profile option ?                         
         BNE   *+8                 No , use main                                
         OI    RCPOPT1,RCPPROF     Yes                                          
*                                                                               
         GOTO1 AFVAL,(R6)          Print within field                           
         CLC   APNO,FVIFLD         Print within ?                               
         BNE   *+8                 Yes                                          
         OI    RCPOPT1,RCPSEP      No, report is separate                       
*                                                                               
         CLI   FVIFLD,C'S'         Print within but separate page ?             
         BNE   *+8                 No                                           
         OI    RCPOPT1,RCPSPAGE    Yes, recap but start on next page            
*                                                                               
         TM    FVIIND,FVINUM       Is it a number ?                             
         BZ    VALREC24            No                                           
         CLI   FVIFLD,C'6'                                                      
         BH    IVALISET            Invalid setting                              
         MVC   RCPMRGE#,FVIFLD                                                  
         NI    RCPMRGE#,TURNOFF-X'F0'     Make binary                           
         ZIC   R1,RCPMRGE#                                                      
         BCTR  R1,0                                                             
         CLM   R1,1,RCPSEQ                                                      
         BE    IVALISET                                                         
         DROP  R3                                                               
*                                                                               
VALREC24 L     R1,AIOAREA1         A(Format record)                             
         GOTO1 ADDEL               Add recap element                            
*                                                                               
VALREC25 LA    R2,RCAPFLNQ(,R2)    ->   NEXT FORMAT -  FORMAT    HDR            
         LA    R4,RCAPRLNQ(,R4)    ->   NEXT FORMAT -  PROFILE                  
         LA    R6,RCAINLNQ(,R6)    ->   NEXT FORMAT -  INTERLEAVE               
         BCT   R0,VALREC22         TEST NEXT FORMAT                             
         EJECT ,                                                                
*=====================================================================*         
*        Verify interleave                                            *         
*=====================================================================*         
                                                                                
VALREC30 DS    0H                                                               
         LA    R0,MAXRPT#-1        MAX  NUM  OF   REPORTS                       
         LA    R2,RCACODEH         ->   1ST  FORMAT -  FORMAT HDR               
         LA    R4,RCAF2INH         ->   2ND  FORMAT -  INTERLEAVE               
         LA    R6,RPTKYWS          SAVED ROW/COL DATA                           
*                                                                               
VALREC40 DS    0H                                                               
         GOTO1 AFVAL,(R4)                                                       
         CLC   APYES,FVIFLD        Interleave requested ?                       
         BE    VALREC41            Yes                                          
         TM    FVIIND,FVINUM                                                    
         BO    VALREC41                                                         
         CLI   FVIFLD,C'S'         Interleave requested, sepearte page?         
         BNE   VALREC45            No,  SKIP                                    
*                                                                               
VALREC41 DS    0H                                                               
         CLC   0(L'RPTKYWS,R6),SPACES                                           
         BE    IVALINLV            Row not matching, can't print within         
         BL    VALREC45            NO FORMAT                                    
         LR    RF,R0                                                            
         LR    RE,R6                                                            
*                                                                               
VALREC42 LA    RE,L'RPTKYWS(,RE)                                                
         OC    0(L'RPTKYWS,RE),0(RE)  NULLS INDICATE ABSENSE OF FORMAT          
         BNZ   VALREC43                                                         
         BCT   RF,VALREC42                                                      
         B     VALREC50            NO MORE TO CHECK                             
*                                                                               
VALREC43 CLC   0(L'RPTKYWS,R6),0(RE)                                            
         BNE   IVALINLV            CANNOT    INTERLEAVE     REPORTS             
*                                                                               
VALREC45 DS    0H                                                               
         LA    R2,RCAPFLNQ(,R2)    ->   NEXT FORMAT -  CODE                     
         LA    R4,RCAINLNQ(,R4)    ->   NEXT FORMAT -  INTERLEAVE               
         LA    R6,L'RPTKYWS(,R6)   ->   NEXT ROW/COL DATA                       
         BCT   R0,VALREC40         TRY  NEXT FORMAT                             
         EJECT ,                                                                
*                                  ************************************         
VALREC50 DS    0H                  * CHECK FOR DUPLICATE FORMAT CODES *         
*                                  ************************************         
         LA    R0,MAXRPT#-1        GET  MAX  NUM  OF   REPORTS                  
         LA    R2,RCACODEH         ->   FORMAT CODE HEADER                      
VALREC55 GOTO1 AFVAL,(R2)                                                       
         BNE   VALREC60            No input, skip this one                      
         LR    R8,R0                                                            
         LR    R3,R2                                                            
*                                                                               
VALREC56 LA    R3,RCAPFLNQ(,R3)         LOOK AT NEXT FORMAT CODE                
         CLC   8(L'RCACODE,R3),SPACES                                           
         BNH   VALREC58                 SKIP EMPTY SPOTS                        
         CLC   FVIFLD(L'RCACODE),8(R3)  DO THEY MATCH ?                         
         BE    IVALDUPF                 Yes, duplicate format                   
*                                                                               
VALREC58 BCT   R8,VALREC56                                                      
                                                                                
VALREC60 LA    R2,RCAPFLNQ(,R2)    NEXT SET                                     
         BCT   R0,VALREC55                                                      
*                                  ************************************         
VALREC95 DS    0H                  * UPDATE THE RECORD WITH ALL THE   *         
*                                  * NEW ELEMENTS                     *         
*                                  ************************************         
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADD  A    RECORD ?                           
         BO    VALREC97                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGE    A    RECORD ?                      
         BO    VALREC97                                                         
         DC    H'0'                WHAT THE  HELL ?                             
*                                                                               
VALREC97 GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD   WRITE     OR   SOMETHING DUDE          
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK) VALIDATED OKAY ?                             
         BNE   EXIT                NO,  EXIT                                    
         MVC   APCURSOR,ACURSOR    SET  APPLICATION    CURSOR                   
*        B     DISREC                                                           
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY RECAP DATA                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING RROWD,R6                                                         
DISREC   DS    0H                                                               
         L     R2,AIOAREA1         PROFILE   ELEMENT                            
         TWAXC RCAF2PFH,RCAF6PFH                                                
         TWAXC RCAF2PRH,RCAF6PRH                                                
         TWAXC RCAF2INH,RCAF6INH                                                
*                                                                               
         LA    R0,ROWLINES         TOTAL NUMBER OF ROWS TO PROCESS              
         LA    R6,RCAF1RHH         FIRST ROW                                    
DISREC01 TWAXC RROWRP1H,RROWRP6H,PROT=Y                                         
         LA    R6,RROWLNQ(,R6)                                                  
         BCT   R0,DISREC01                                                      
         DROP  R6                                                               
*                                                                               
         MVI   SWITCHES,0          CLEAR     THE  SWITCHES                      
         MVC   RCAF1PR,MAIN        USE  MAIN FORMAT'S  PROFILE                  
         OI    RCAF1PRH+6,FVOXMT   TRANSMIT                                     
         MVC   RCAF1IN,APYES       INTERLEAVED                                  
         OI    RCAF1INH+6,FVOXMT   TRANSMIT                                     
*                                                                               
         USING RCPELD,R4                                                        
         SR    R6,R6               1ST  FORMAT                                  
         MVI   APELCODE,RCPELQ     RECAP     ELEMENT   X'C8'                    
         GOTO1 GETEL,(R2)          GET  AN   ELEMENT                            
         BE    DISREC10            FOUND,    CONTINUE                           
         OI    SWITCHES,NOELFND    REMEMBER, NO   ELEMENT   FOUND               
         B     DISREC30            DISPLAY   THE  ROWS                          
*=====================================================================*         
*       Display format codes                                          *         
*=====================================================================*         
                                                                                
DISREC10 DS    0H                  NEXT RECAP     ELEMENT                       
         LR    R4,R1               ->   ELEMENT                                 
         LR    RF,R6               R6 = Format number                           
         MHI   RF,RCAPFLNQ         Bump to FORMAT header                        
         LA    RE,RCACODEH(RF)     ->   FORMAT header                           
         OI    6(RE),FVOXMT             Transmit                                
         MVC   8(L'RCACODE,RE),RCPCODE                                          
                                                                                
*=====================================================================*         
*       Display PROFILE optin                                         *         
*=====================================================================*         
         LR    RF,R6               R6 = Format number                           
         MHI   RF,RCAPRLNQ         Bump to PROFILE header                       
         LA    RE,RCAF1PRH(RF)     ->   PROFILE   HEADER                        
         OI    6(RE),FVOXMT        Transmit                                     
         MVC   8(L'RCAF1PR,RE),MAIN                                             
         TM    RCPOPT1,RCPPROF     use main fromat's profile ?                  
         BZ    *+10                Yes                                          
         MVC   8(L'RCAF1PR,RE),REPORT                                           
                                                                                
*=====================================================================*         
*       Display PRINT WITHIN option                                   *         
*=====================================================================*         
         LR    RF,R6               R6 = Format number                           
         MHI   RF,RCAINLNQ         Bump to associated print within hdr          
         LA    RE,RCAF1INH(RF)     ->   PRINT WITHIN                            
         OI    6(RE),FVOXMT        Transmit data                                
         MVC   8(1,RE),APYES       Default not to print within                  
         TM    RCPOPT1,RCPSEP      Print within                                 
         BZ    *+10                Yes                                          
         MVC   8(1,RE),APNO        No                                           
         TM    RCPOPT1,RCPSPAGE    Print within , separate page ?               
         BZ    *+8                 Yes                                          
         MVI   8(RE),C'S'                                                       
*                                                                               
DISREC22 CLI   RCPMRGE#,0                                                       
         BE    DISREC30                                                         
         MVC   8(1,RE),RCPMRGE#                                                 
         OI    8(RE),X'F0'         Make it a number                             
*                                                                               
DISREC30 DS    0H                                                               
         GOTO1 DISPROWS,(R6)       DISPLAY   ROWS                               
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS ?                                
         BNE   DISREC99            YES, EXIT                                    
*                                                                               
         TM    SWITCHES,NOELFND    NO   ELEMENT   FOUND ?                       
         BO    DISREC99            YES, FINISHED                                
         AHI   R6,1                Increate format number working on            
         MVI   APELCODE,RCPELQ     RECAP     ELEMENT   X'C8'                    
         GOTO1 NEXTEL,(R4)         ANY  MORE FORMATS ?                          
         BE    DISREC10            YES, PROCESS   NEXT                          
         MVC   FVMSGNO,=AL2(FVFOK) GOOD EXIT                                    
*                                                                               
DISREC99 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS ?                                
         BNE   DISRC999            YES, SKIP                                    
*                                                                               
         MVC   APCURSOR,ACURSOR    SET  APPLICATION    CURSOR                   
*                                                                               
DISRC999 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK) SET  CONDITION CODE                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE REPORT                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALRPTN  NTR1  ,                   VALIDATE  A    REPORT                        
         LR    R6,R1               REPORT # WORKING ON                          
         LA    R2,RCACODEH         CALCULATE ADDRESS OF FORMAT REPORT           
         LR    RF,R6                                                            
         MHI   RF,RCAPFLNQ                                                      
         AR    R2,RF                                                            
         ST    R2,AFORMAT          A(FORMAT REPORT HEADER)                      
         OI    6(R2),FVOXMT        TRANSMIT  THE  FIELD                         
*                                                                               
         LA    R3,RCAF1PRH                                                      
         LR    RF,R6                                                            
         MHI   RF,RCAPRLNQ                                                      
         AR    R3,RF                                                            
         ST    R3,APROFILE         A(FORMAT PROFILE HEADER)                     
         OI    6(R3),FVOXMT        TRANSMIT  THE  FIELD                         
*                                                                               
         LA    R3,RCAF1INH                                                      
         LR    RF,R6                                                            
         MHI   RF,RCAINLNQ                                                      
         AR    R3,RF                                                            
         ST    R3,AINTERLV         A(FORMAT PRINT WITHIN HEADER)                
         OI    6(R3),FVOXMT        TRANSMIT  THE  FIELD                         
*                                  ************************************         
         GOTO1 AFVAL,(R2)          * VALIDATE REPORT FORMAT *                   
         BE    VRPT10              ************************************         
*                                  YES,  INPUT                                  
         MVC   8(L'RCACODE,R2),SPACES    NONE, CLEAR INPUT FIELDS               
         L     R3,APROFILE                                                      
         MVC   8(L'RCAF1PR,R3),SPACES                                           
         L     R3,AINTERLV                                                      
         MVC   8(L'RCAF1IN,R3),SPACES                                           
         GOTO1 DISPROWS,(R6)                                                    
         LR    R3,R6                                                            
         MHI   R3,L'RPTKYWS                                                     
         LA    R3,RPTKYWS(R3)                                                   
         XC    0(L'RPTKYWS,R3),0(R3)                                            
         MVC   FVMSGNO,=AL2(FVFOK) GOOD INPUT                                   
         B     VRPTEX              EXIT                                         
*                                                                               
         USING RESRECD,R2          MAP  SCRIBE    RECORDS                       
VRPT10   DS    0H                                                               
         MVC   8(L'RCACODE,R2),FVIFLD                                           
         LA    R2,IOKEY            GET FORMAT INTO IO2                          
         MVC   RESKEY,SPACES       CLEAR     KEY                                
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY   CODE                               
         MVC   RESKFORM,FVIFLD     FORMAT    CODE                               
         LA    R1,IORDD+IOACCFIL+IO2    READ THE  RECORD                        
         GOTO1 AIO                                                              
*MN                                                                             
         BE    VRPT13                                                           
*                                                                               
         CLC   RCACODE(4),=CL4'FBAL'                                            
         BE    *+14                                                             
         CLC   RCACODE(4),=C'FP&&L'                                             
         BNE   VRPTRNF                                                          
*                                                                               
         USING FMTLSTD,RF                                                       
         LA    RF,FMTLIST                                                       
VRPT12   CLI   0(RF),X'FF'                                                      
         BE    VRPTRNF                                                          
         CLC   FMTLCDE,FVIFLD                                                   
         BE    VRPTOK                                                           
         LA    RF,FMTLLNQ(RF)                                                   
         B     VRPT12                                                           
         DROP  RF                                                               
*MN                                                                             
*MN      BNE   VRPTRNF             BAD  READ,     RCD  NOT  FOUND               
         DROP  R2                                                               
*                                                                               
VRPT13   TM    IOERR,IOEDEL        IS RECORD MARKED DELETED                     
         BO    VRPTRDEL                                                         
*                                                                               
         L     R4,AIOAREA2         ->   RECORD                                  
         LR    R1,R4                                                            
         MVI   APELCODE,STYELQ     FREE FORM SCRIBE    ELEMENT  (X'25')         
         GOTO1 GETEL               GET  THE  ELEMENT                            
         BE    *+6                 FOUND,    CONTINUE                           
         DC    H'00'               ELSE,     ABEND                              
*                                                                               
         USING STYELD,R1           MAP  FREE FORM ELEMENT                       
         CLC   STYNAME,APREPCDE    SAME REPORT    TYPE ?                        
         BNE   VRPTNSRT            NO,  NOT  SAME REPORT    TYPE                
         DROP  R1                                                               
*                                                                               
*                                  ************************************         
VRPT15   DS    0H                  * VALIDATE PROFILE FIELD           *         
*                                  ************************************         
         L     R3,APROFILE                                                      
         GOTO1 AFVAL,(R3)          ANY  PROFILE   INPUT  ?                      
         BE    VRPT20                   YES, CONTINUE                           
         MVC   8(L'RCAF1PR,R3),MAIN     NO,  DEFAULT   TO   MAIN                
         B     VRPT30                                                           
*                                                                               
VRPT20   DS    0H                                                               
         ZIC   RF,FVXLEN           GET  THE  PROFILE'S EX   LENGTH              
         EXCLC RF,FVIFLD,MAIN      IS   IT   MAIN ?                             
         BNE   VRPT25              NO,  TRY  REPORT                             
         MVC   8(L'RCAF1PR,R3),MAIN                                             
         B     VRPT30                                                           
*                                                                               
VRPT25   DS    0H                                                               
         EXCLC RF,FVIFLD,REPORT                                                 
         BNE   VRPTIPUT            NO,  INVALID   INPUT                         
         MVC   8(L'RCAF1PR,R3),REPORT                                           
*                                                                               
*                                  ************************************         
VRPT30   DS    0H                  * VALIDATE INTERLEAVE FIELD        *         
*                                  ************************************         
         L     R3,AINTERLV                                                      
         GOTO1 AFVAL,(R3)          ANY  INTERLEAVE     INPUT  ?                 
         BE    VRPT35                   YES, CONTINUE                           
         MVC   8(L'RCAF1IN,R3),APNO     SO,  DEFAULT   TO   NO                  
         BNE   VRPT40                                                           
*                                                                               
VRPT35   DS    0H                                                               
         CLC   FVIFLD(1),APYES     Print within ?                               
         BE    VRPT40              Yes, continue                                
         CLC   FVIFLD(1),APNO      Print within ?                               
         BE    VRPT40              No,  continue                                
         CLI   FVIFLD,C'S'         Print recap on separate page ?               
         BE    VRPT40              No,  continue                                
         TM    FVIIND,FVINUM       Number input                                 
         BZ    VRPTIPUT            No options were valid                        
         CLI   FVIFLD,C'6'                                                      
         BH    VRPTIPUT                                                         
*                                                                               
VRPT40   DS    0H                                                               
         GOTO1 DISPROWS,(R6)                                                    
*                                                                               
         USING RRWELD,R1                                                        
         L     R4,AIOAREA2                                                      
         LR    R3,R6                                                            
         MHI   R3,L'RPTKYWS                                                     
         LA    R3,RPTKYWS(R3)                                                   
*                                                                               
VPRT44   LR    R1,R4                                                            
         MVI   APELCODE,RRWELQ     X'C2' ROW ELEMENT                            
         GOTO1 GETEL               GET FIRST ROW                                
         BNE   VRPT50                                                           
         LA    RF,RRWNDATA                                                      
         SR    R0,R0                                                            
         IC    R0,RRWDATLN         LENGTH OF DATA                               
         GOTO1 GETFST,APPARM,(RF),(R0),(R3)                                     
         B     VRPTOK              EXIT                                         
*                                                                               
         USING RCLELD,R1                                                        
VRPT50   MVI   APELCODE,RCLELQ     X'C3' COLUMN ELEMENT                         
         GOTO1 GETEL               GET FIRST COLUMN                             
*                                                                               
VRPT52   BNE   VRPTOK                                                           
         TM    RCLOPT,RCLACCM+RCLEQU                                            
         BZ    VRPT55              FIND FIRST NONE $, IF NO ROWS                
         GOTO1 NEXTEL                                                           
         B     VRPT52                                                           
*                                                                               
VRPT55   LA    RF,RCLNDATA                                                      
         SR    R0,R0                                                            
         IC    R0,RCLDATLN         LENGTH OF DATA                               
         GOTO1 GETFST,APPARM,(RF),(R0),(R3)                                     
         B     VRPTOK                                                           
         DROP  R1                                                               
*                                                                               
VRPTIPUT DS    0H                        INVALID   INPUT                        
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VRPTEX                                                           
*                                                                               
VRPTRDEL DS    0H                        RECORD DELETED                         
         MVC   FVMSGNO,=AL2(77)                                                 
         MVC   FVXTRA(L'RCACODE),FVIFLD  ACTUAL FORMAT  CODE                    
         B     VRPTEX                                                           
*                                                                               
VRPTRNF  DS    0H                        RECORD NOT FOUND                       
         MVC   FVMSGNO,=AL2(ACEIVFT)     INVALID FORMAT CODE                    
         MVC   FVXTRA(L'RCACODE),FVIFLD  ACTUAL FORMAT  CODE                    
         B     VRPTEX                                                           
*                                                                               
VRPTNSRT DS    0H                        MUST HAVE SAVE REPORT TYPE             
         MVC   FVMSGNO,=AL2(ACEMRSRT)                                           
         MVC   FVXTRA(L'RCACODE),FVIFLD                                         
         B     VRPTEX                                                           
*                                                                               
VRPTIDWN DS    0H                        DOWN-LOAD CONFLICT                     
         MVC   FVMSGNO,=AL2(ACERCPDW)                                           
         MVC   FVXTRA(L'RCACODE),FVIFLD                                         
         B     VRPTEX                                                           
*                                                                               
VRPTOK   MVC   FVMSGNO,=AL2(FVFOK) GOOD INPUT                                   
         ZIC   RF,RCP#RPTS         BUMP NUMBER OF GOOD REPORTS                  
         LA    RF,1(,RF)                                                        
         STC   RF,RCP#RPTS                                                      
*                                                                               
VRPTEX   DS    0H                  EXIT                                         
         CLC   FVMSGNO,=AL2(FVFOK) SET  CONDITION CODE                          
         B     XIT                 RETURN                                       
         EJECT ,                                                                
GETFST   NTR1                                                                   
         L     R2,0(,R1)           START  OF DATA                               
         LR    R3,R2                                                            
         L     R0,4(,R1)           LENGTH OF DATA                               
         L     R4,8(,R1)           WHERE  TO STORE DATA                         
         SR    RF,RF               KEEP TRACK OF LENGTH                         
*                                                                               
GETFST10 CLC   0(1,R3),SCCOMMA                                                  
         BE    GETFST20                                                         
         LA    R3,1(,R3)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,GETFST10                                                      
*                                                                               
GETFST20 BCTR  RF,0                MOVE DATA INTO TABLE                         
         EXMVC RF,0(R4),0(R2)                                                   
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY ROWS FOR A FORMAT                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2          MAP  SCRIBE    RECORDS                       
         USING RROWD,R3                                                         
         USING RRWELD,R4           MAP  ROW       DATA                          
DISPROWS NTR1  ,                   DISPLAY   ROWS                               
         LR    R6,R1               GET  RECAP     LINE NUMBER                   
         MVC   SAVEKEY1,IOKEY      SAVE IOKEY                                   
*                                                                               
         LR    RF,R6               GET  FORMAT    NUMBER                        
         MHI   RF,RPTNEXTQ         ADJUST    FOR  ROW  RPT  NUMBER              
         STH   RF,RPTADJST         SAVE REPORT    ADJUSTMENT     FACTOR         
*                                                                               
         LA    R0,ROWLINES         NUMBER    OF   LINES                         
         LA    R3,RCAF1RHH         POINT TO BEGINING OF FIRST ROW               
DROW08   LA    R8,RROWRP1H         POINT TO FIRST REPORT IN ROW                 
         AH    R8,RPTADJST         BUMP TO APPROPRIATE REPORT                   
         SR    RF,RF                                                            
         IC    RF,0(,R8)           GET LENGTH OF HEADER FIELD                   
         AHI   RF,-9                                                            
         EXXC  RF,8(R8),8(R8)                                                   
         LA    R3,RROWLNQ(,R3)     NEXT ROW                                     
         BCT   R0,DROW08                                                        
*                                  CLEAR     ROW  KEYWORDS  AND                 
*                                            ROW  TYPES                         
         LR    R1,R6               GET  FORMAT    NUMBER                        
         MHI   R1,RCAPFLNQ         TIMES     CODE FIELDS    LENGTH              
         LA    R1,RCACODEH(R1)     ->   FORMAT    HEADER                        
*                                                                               
IO       USING RESRECD,R4                                                       
         GOTO1 AFVAL,(R1)          ANY  FORMAT CODE ?                           
         BNE   DROWEX              NO,  EXIT                                    
         LA    R2,IOKEY            ->   I/O  KEY                                
         L     R4,AIOAREA2         ->   RECORD                                  
         MVC   RESKEY,SPACES       CLEAR     KEY                                
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY   CODE                               
         MVC   RESKFORM,FVIFLD                                                  
         CLC   RESKFORM,IO.RESKFORM                                             
         BE    DROW15              ALREADY HAVE RECORD                          
         LA    R1,IORDD+IOACCFIL+IO2    READ THE  RECORD                        
         GOTO1 AIO                                                              
*MN                                                                             
         BE    DROW15                                                           
*                                                                               
         CLC   RCACODE(4),=CL4'FBAL'                                            
         BE    *+14                                                             
         CLC   RCACODE(4),=C'FP&&L'                                             
         BNE   DROWRNF                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK) GOOD EXIT                                    
         USING FMTLSTD,RF                                                       
         LA    RF,FMTLIST                                                       
DROW10   CLI   0(RF),X'FF'                                                      
         BE    DROWRNF                                                          
         CLC   FMTLCDE,FVIFLD                                                   
         BE    DROWEX                                                           
         LA    RF,FMTLLNQ(RF)                                                   
         B     DROW10                                                           
         DROP  RF                                                               
*MN                                                                             
*MN      BNE   DROWRNF             BAD  READ,     RCD  NOT  FOUND               
         DROP  IO                                                               
*                                                                               
DROW15   MVI   APELCODE,RRWELQ     FIND ROW  ELEMENT   X'C2'                    
         GOTO1 GETEL,(R4)                                                       
         BNE   DROWEX              NOT  FOUND,    DONE                          
*                                                                               
         SR    R8,R8               ROW  NUMBER    1                             
         LA    R3,RCAF1RHH         POINT TO FIRST ROW                           
*                                                                               
DROW20   DS    0H                  DISPLAY   ROW  INFORMATION                   
         LA    R8,RROWRP1H         POINT TO FIRST REPORT IN ROW                 
         AH    R8,RPTADJST         BUMP TO APPROPRIATE REPORT                   
         LR    R4,R1               ->   ROW  ELEMENT                            
         SR    RE,RE               CLEAR     KEYWORD  LENGTH                    
         LA    RF,RRWNDATA         ->   KEYWORD                                 
*                                                                               
         SR    R0,R0                                                            
         IC    R0,RRWDATLN         LENGTH OF DATA                               
*                                                                               
DROW30   CLC   0(1,RF),SCCOMMA     COMMA ?                                      
         BE    DROW40              YES, FOUND     END  OF   KEYWORD             
         LA    RE,1(,RE)           ADD  1    TO   LENGTH                        
         LA    RF,1(,RF)           NEXT CHARACTER                               
         BCT   R0,DROW30                                                        
*                                                                               
DROW40   BCTR  RE,0                SUBTRACT  ONE  FOR  EXECUTE                  
         OI    6(R8),FVOXMT        TRANSMIT                                     
         EXMVC RE,8(R8),RRWNDATA   INSERT    KEYWORD                            
*                                                                               
DROW50   LA    R3,RROWLNQ(,R3)                                                  
         GOTO1 NEXTEL,(R4)         ANY  MORE ELEMENTS ?                         
         BE    DROW20              YES, PROCESS   NEXT ROW                      
         B     DROWEX              NO,  RETURN                                  
*                                                                               
DROWRNF  DS    0H                  RECORD    NOT  FOUND,    SAY                 
*                                  INVALID   FORMAT    CODE                     
         MVC   FVMSGNO,=AL2(ACEIVFT)                                            
*                                  FORMAT    CODE                               
         MVC   FVXTRA(L'RCACODE),FVIFLD                                         
*                                                                               
DROWEX   DS    0H                  EXIT                                         
         MVC   IOKEY,SAVEKEY1      RESTORE   IOKEY                              
         B     XIT                 RETURN                                       
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
***********************************************************************         
* ERRORS TO DISPLAY ON TOP OF SCREEN                                  *         
***********************************************************************         
         SPACE 1                                                                
IVALEKEY DS    0H                         ENTER KEY                             
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALISET DS    0H                         Invalid setting                       
         MVC   FVMSGNO,=AL2(1671)                                               
         B     IVALEXIT                                                         
                                                                                
IVALINLV DS    0H                         REPORT CANNOT BE INTERLEAVED          
         MVC   FVMSGNO,=AL2(2103)                                               
         B     IVALEXIT                                                         
                                                                                
IVALDUPF DS    0H                         DUPLICATE FORMAT CODES                
         MVC   FVMSGNO,=AL2(2104)                                               
         MVC   FVXTRA(L'RCACODE),8(R2)                                          
         B     IVALEXIT                                                         
                                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
SCRTXT   DS    0F                                                               
*        DC    AL4(PNLPL02H-TWAD,1652)                                          
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  CONSTANTS                                                          *         
***********************************************************************         
*MN                                                                             
FMTLIST  DC     CL8'FPL1'                                                       
*&&UK*&& DC     CL8'FPL3'                                                       
*&&US*&& DC     CL8'FPL3'                                                       
*&&US*&& DC     CL8'FPL5'                                                       
         DC     AL1(EOT)                                                        
*MN                                                                             
         SPACE 1                                                                
DCMAIN   DCDD  AC#RSMAN,L'MAIN     MAIN                                         
DCREPORT DCDD  AC#RSRPT,L'REPORT   REPORT                                       
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 4                                                                
         DROP  RA                                                               
         EJECT ,                                                                
PROFTEXT DS    0XL2                                                             
         DC    AL1(REPJCLR,RECRCVPF)  RCV                                       
PROFLNQ  EQU   *-PROFTEXT                                                       
         DC    AL1(REPJCLI,RECINCPF)  INC                                       
         DC    AL1(REPJCLP,RECPAYPF)  PAY                                       
         DC    AL1(REPJCLX,RECEXPPF)  EXP                                       
         DC    AL1(REPJCLV,RECJOBPF)  PROD                                      
         DC    AL1(REPJCL1,RECCSTPF)  PERSON                                    
         DC    AL1(REPJCL2,RECPNLPF)  P&L                                       
         DC    AL1(REPJCLB,RECCSHPF)  CASH                                      
         DC    AL1(REPJCLG,RECGNLPF)  GL                                        
*        DC    AL1(REPJCLZ,RECMEDPF)  MEDIA                                     
MAXPROF  EQU   (*-PROFTEXT)/PROFLNQ                                             
         DC    AL1(0,RECPRF)          PROFILE - (DEFAULT)                       
*                                                                               
         EJECT ,                                                                
*MN                                                                             
*        ACSCRFMTS                                                              
*        PRINT OFF                                                              
*      ++INCLUDE ACSCRFMTS                                                      
*        PRINT ON                                                               
*        EJECT ,                                                                
*MN                                                                             
*MN                                                                             
***********************************************************************         
*  DSECT TO COVER FORMAT LIST FOR ACCENT RECAPS                       *         
***********************************************************************         
FMTLSTD  DSECT                                                                  
FMTLCDE  DS    CL8                 Format code                                  
FMTLLNQ  EQU   *-FMTLCDE                                                        
*MN                                                                             
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
*                                                                               
RPTNEXTQ EQU   RROWRP2H-RROWRP1H   TO GET TO NEXT REPORT ON ROW                 
ROWLINES EQU   (RCATABH-RCAF1RHH)/RROWLNQ                                       
*                                                                               
MAXRPT#  EQU   MAXRCP#+1           MAXIMUM   REPORT    NUMBER                   
*                                                                               
RCAPFLNQ EQU   RCAF2PFH-RCACODEH   LENGTH    OF   FORMAT      SET               
RCAPRLNQ EQU   RCAF2PRH-RCAF1PRH   LENGTH    OF   PROFILE     SET               
RCAINLNQ EQU   RCAF2INH-RCAF1INH   LENGTH    OF   INTERLEAVE  SET               
*                                                                               
MAXPARM  EQU   20                                                               
*                                                                               
SVREGS   DS    6A                                                               
SVACURSR DS    A                   SAVE ADDR OF   CURSOR                        
*                                                                               
AFORMAT  DS    A                   A(CURRENT FORMAT    HEADER)                  
APROFILE DS    A                   A(CURRENT PROFILE   HEADER)                  
AINTERLV DS    A                   A(CURRENT INTERLEVE HEADER)                  
*                                                                               
RPTADJST DS    H                   REPORT    ADJUSTMENT     FACTOR              
ROWADJST DS    H                   ROW       ADJUSTMENT     FACTOR              
*                                                                               
HALF     DS    H                                                                
BYTE     DS    CL1                                                              
RCP#RPTS DS    X                   NUMBER OF RECAPED REPORTS                    
*                                                                               
RPTKYWLN DS    0CL(MAXRPT#*6)      USED TO CLEAR FIELD                          
RPTKYWS  DS    (MAXRPT#)CL6                                                     
*                                                                               
MAIN     DS    CL(L'RCAF1PR)       MAIN                                         
REPORT   DS    CL(L'RCAF2PR)       REPORT                                       
                                                                                
RCARPCDE DS    CL(L'REPCODE)                                                    
RCARPTYP DS    CL(L'REPSTYPE)                                                   
RCARPLNQ EQU   *-RCARPCDE                                                       
*                                                                               
RCABLOCK DS    (MAXPARM)CL32                                                    
SAVEKEY1 DS    CL(L'RESKEY)                                                     
*                                                                               
SWITCHES DS    XL1                 SWITCHES                                     
NOELFND  EQU   X'80'               NO   ELEMENT   FOUND                         
*                                                                               
LWSX     DS    0C                                                               
         EJECT ,                                                                
RROWD    DSECT                                                                  
RROWNAMH DS    CL8                                                              
RROWNAM  DS    CL3                                                              
RROWNUMH DS    CL8                                                              
RROWNUM  DS    CL2                                                              
RROWRP1H DS    CL8                                                              
RROWRP1  DS    CL6                                                              
RROWRP2H DS    CL8                                                              
RROWRP2  DS    CL6                                                              
RROWRP3H DS    CL8                                                              
RROWRP3  DS    CL6                                                              
RROWRP4H DS    CL8                                                              
RROWRP4  DS    CL6                                                              
RROWRP5H DS    CL8                                                              
RROWRP5  DS    CL6                                                              
RROWRP6H DS    CL8                                                              
RROWRP6  DS    CL6                                                              
RROWLNQ  EQU   *-RROWD                                                          
         EJECT ,                                                                
*        ACSCRWRK                                                               
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR RECAP SCREEN DEFINITIONS                                 *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRE1D                                                       
         PRINT ON                                                               
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086ACSCR1A   11/15/16'                                      
         END                                                                    
