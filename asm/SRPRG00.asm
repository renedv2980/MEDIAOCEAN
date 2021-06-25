*          DATA SET SRPRG00    AT LEVEL 008 AS OF 02/23/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE T14100A                                                                  
*INCLUDE SQUASHER                                                               
         TITLE '$PROG - DISPLAY FACPAK PROGRAM LIST'                            
PROG     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**$PROG*,R9,RR=RE,CLEAR=YES                            
         USING WRKD,RC             RC=A(W/S)                                    
         ST    RE,RELO                                                          
         USING SRPARMD,R1                                                       
         L     RA,SRPARM6          RA=A(TWA)                                    
         USING SRPRGFFD,RA                                                      
         L     R8,SRPARM4          EXTRACT COMFACTS DATA                        
         USING COMFACSD,R8                                                      
         MVC   PHEXIN,CHEXIN                                                    
         MVC   PHEXOUT,CHEXOUT                                                  
         MVC   PGETTXT,CGETTXT                                                  
         MVC   PDATAMGR,CDATAMGR                                                
         MVC   PGETFACT,CGETFACT                                                
         MVC   PXSORT,CXSORT                                                    
         L     RF,28(R1)                                                        
         ST    RF,ATIOB                                                         
         L     RF,SRQATIA                                                       
         ST    RF,ATIA                                                          
         DROP  R8                                                               
         L     R8,SRPARM1          EXTRACT SYSFACTS DATA                        
         USING SYSFACD,R8                                                       
         L     RF,VCALLOV                                                       
         ST    RF,PCALLOV                                                       
         L     RF,VSSB             EXTRACT SSB DATA                             
         ST    RF,PVSSB                                                         
         L     RF,VSELIST                                                       
         ST    RF,PSELIST                                                       
         L     RF,VDMOD000                                                      
         ST    RF,PDMOD000                                                      
         L     RF,VTICTOC                                                       
         ST    RF,PTICTOC                                                       
         L     RF,VWCTYPE                                                       
         ST    RF,PWCTYPE                                                       
         DROP  R8                                                               
         L     RF,=A(IOAREA-WRKD)                                               
         LA    RF,WRKD(RF)                                                      
         ST    RF,AIOREC                                                        
         XC    SYNTAB,SYNTAB       SET SYNONYM TABLE TO ZEROS                   
         XC    SRVMSG,SRVMSG       BLANK MESSAGE LINE                           
         XC    SAVEDSTR,SAVEDSTR   BLANK SAVED STORAGE                          
         MVI   FLAG,0              SET FLAG TO ZEROS                            
         EJECT                                                                  
**********************************************************************          
* INITIALISE FOR DDS AND NON-DDS TERMINALS                           *          
**********************************************************************          
         SPACE 1                                                                
INIT     MVI   DDS,0               TEST/SET DDS TERMINAL                        
         L     R5,8(R1)                                                         
         USING UTLD,R5                                                          
         L     RF,TBUFF                                                         
         ST    RF,PBUFF                                                         
         MVC   LANG,TLANG          SAVE LANGUAGE                                
         MVC   TRM,TNUM                                                         
         TM    TSTAT1,TSTATDDS                                                  
         BZ    *+8                                                              
         MVI   DDS,1                                                            
         MVC   OVSYS,TOVSYS        SAVE CONNECT SYSTEM                          
         MVC   CTRY,TAGCTRY        SAVE COUNTRY CODE                            
         MVC   AGY,TAGY            SAVE AGENCY CODE                             
         DROP  R5                                                               
*                                                                               
INIT1    L     R5,PVSSB                                                         
         MVC   RECLEN,SSBTWAL-SSBD(R5)                                          
         L     R5,SSBACTRY-SSBD(R5) GET ADDRESS OF COUNTRY TABLE                
         ST    R5,ACTRYTAB                                                      
*                                                                               
INIT2    LA    RE,SRVSRV            CHANGE FROM DDS TO NON DDS                  
         LA    RF,L'SRVSRV-1                                                    
         CLC   0(2,RE),=C',C'                                                   
         BE    INIT2X                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,*-14                                                          
         B     *+8                                                              
INIT2X   MVI   DDS,0                                                            
         EJECT                                                                  
**********************************************************************          
*SET HEADERS ETC FOR DDS AND NON-DDS TERMINALS                       *          
**********************************************************************          
         SPACE 1                                                                
HDRSET   CLI   DDS,0                                                            
         BNE   HDRDDS                                                           
*                                                                               
HDRCLI   LA    R0,28               SET CLIENT HEADERS                           
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',HDREC),(C'T',0),,(X'88',0)              
         MVC   SRVPM,HDREC                                                      
*                                                                               
         LA    R0,29               HEADLINE                                     
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',HDREC),(C'T',0),,(X'88',0)              
         MVC   SRVH1,HDREC                                                      
*                                                                               
         LA    R0,30               FOOTLINE                                     
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVLL,DESCR                                                      
         MVI   SRVLLH+6,X'08'      SET HIGH INTENSITY                           
*                                                                               
         LA    R0,39               FOOTLINE                                     
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVPFK,DESCR                                                     
*NOP     XC    SRVP2,SRVP2         CLEAR P2 P3,NOT VALID FOR NON DDS            
*NOP     XC    SRVP3,SRVP3                                                      
         OI    SRVP2H+6,X'8C'      MAKE THEM DISSAPEAR                          
         OI    SRVP3H+6,X'8C'                                                   
         B     HDRSETX                                                          
*                                                                               
HDRDDS   LA    R0,25               SET DDS HEADINGS                             
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(78,HDREC),(C'T',0),,(X'88',0)                 
         MVC   SRVPM,HDREC                                                      
*                                                                               
         LA    R0,26               HEADLINE                                     
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',HDREC),(C'T',0),,(X'88',0)              
         MVC   SRVH1,HDREC                                                      
*                                                                               
HDRSETX  EQU   *                                                                
         EJECT                                                                  
**********************************************************************          
* RETRIEVE SAVED STORAGE AND SET SYSTEM NAME IF NECESSARY            *          
**********************************************************************          
         SPACE 1                                                                
         BAS   RE,READSTR                                                       
         CLI   SAVEDSTR+7,C'Y'                                                  
         BNE   SYSTM                                                            
         MVC   SRVP1(3),SAVEDSTR                                                
         MVI   SRVP1H+5,3                                                       
         SPACE 1                                                                
***********************************************************************         
* RETRIEVE CURRENT SYSTEM NAME                                        *         
***********************************************************************         
         SPACE 1                                                                
SYSTM    CLI   OVSYS,0             ALREADY CONNECTED ?                          
         BNE   SYSTMA                                                           
         MVC   PSYSNME(7),=C'SERVICE'  DEFAULT TO SERVICE                       
         B     VALP1                                                            
SYSTMA   GOTO1 PGETFACT,DMCB,0     P1 - SYSTEM NAME                             
         L     R3,0(R1)                                                         
         L     R3,FASYSLST-FACTSD(R3)                                           
         LA    R3,6(R3)                                                         
         USING SYSLSTD,R3          R3=A(SYSTEM LIST)                            
SYSTMB   CLC   OVSYS,SYSLNUM       YES DEFAULT TO CURRENT SYSTEM                
         BNE   SYSTMC                                                           
         MVC   PSYSNME(L'SYSLNAME),SYSLNAME                                     
         MVC   RCSYSLNM,SYSLNAME                                                
         B     VALP1                                                            
SYSTMC   LA    R3,SYSLLEN(R3)                                                   
         B     SYSTMB                                                           
***********************************************************************         
* VALIDATE INPUT PARAMETERS                                           *         
***********************************************************************         
         SPACE 1                                                                
VALP1    GOTO1 PGETFACT,DMCB,0     P1 - SYSTEM NAME                             
         L     R3,0(R1)                                                         
         L     R3,FASYSLST-FACTSD(R3)                                           
         LA    R3,6(R3)                                                         
         USING SYSLSTD,R3          R3=A(SYSTEM LIST)                            
*                                                                               
         LA    R2,SRVP1H           TEST SYSTEM NAME ENTERED                     
         CLI   SRVP1H+5,0                                                       
         BNE   VALP1C                                                           
         CLI   OVSYS,0             ALREADY CONNECTED ?                          
         BNE   VALP1A                                                           
         MVC   SRVP1(3),=C'SER'    NO  DEFAULT TO SERVICE                       
         IC    R1,=AL1(2)                                                       
         B     VALP1D                                                           
VALP1A   CLC   OVSYS,SYSLNUM       YES DEFAULT TO CURRENT SYSTEM                
         BNE   VALP1B                                                           
         MVC   SRVP1(L'SYSLNAME),SYSLNAME                                       
         B     VALP1F                                                           
VALP1B   LA    R3,SYSLLEN(R3)                                                   
         B     VALP1A                                                           
*                                                                               
VALP1C   CLC   SRVP1(1),=C'?'      SYSTEM HELP KEY ?                            
         BE    SYSQRY                                                           
         SR    R1,R1               SET UP FOR EXECUTED COMPARE                  
         IC    R1,SRVP1H+5                                                      
         BCTR  R1,0                                                             
VALP1D   CLI   SYSLNUM,0           ERROR IF NOT IN SYSTEM TABLE                 
         BE    ERROR2                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),SRVP1                                                
         BE    VALP1E                                                           
         CLC   SYSLSHRT(3),SRVP1   CHECK SHORT NAME                             
         BE    VALP1E                                                           
         LA    R3,SYSLLEN(R3)                                                   
         B     VALP1D                                                           
VALP1E   MVC   SRVP1(L'SYSLNAME),SYSLNAME                                       
         MVC   RCSYSLNM,SYSLNAME                                                
*                                                                               
VALP1F   MVC   RCSYSLNM,SYSLNAME                                                
         L     R5,PSELIST          SEARCH SE LIST FOR SYSTEM                    
         BAS   RE,SETBXLE                                                       
         USING SELISTD,R5                                                       
         CLC   SEOVSYS,SYSLNUM                                                  
         BE    VALP1G                                                           
         BXLE  R5,R6,*-10                                                       
         BE    ERROR2                                                           
*                                                                               
VALP1G   MVC   APGMLST,SEPGMS      SAVE A(PROGRAM LIST FOR SYSTEM)              
         MVC   SYSCODE,SYSLNUM                                                  
*                                                                               
         CLC   SRVP1(7),SAVEDSTR                                                
         BE    *+10                                                             
         MVC   SAVEDSTR+17(7),=C'       ' SET LAST NAME                         
         DROP  R3,R5                                                            
**********************************************************************          
* STORE CURSOR POSITION AND KEY HIT                                  *          
**********************************************************************          
         SPACE 1                                                                
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R5,3,TIOBCURS                                                    
         D     R4,=F'78'           CALC LINE NO FROM ABSOLUTE ADDR              
         C     R5,=F'6'                                                         
         BL    PFKEYX                                                           
         S     R5,=F'5'                                                         
         ST    R5,CURADR                                                        
*                                                                               
         SR    R0,R0               READ PF KEY VALUE                            
         IC    R0,TIOBAID                                                       
         CH    R0,=H'12'           CONVERT 12-24 TO 1-12                        
         BNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY            STORE IN KEY                                 
*                                                                               
         CLI   PFKEY,1                                                          
         BNE   PFKEYA              DISPLAY EXTENDED TEXT                        
         CLI   DDS,1               ONLY AVAILABLE FOR CLIENTS                   
         BE    PFKEYX                                                           
         CLI   SAVEDSTR+16,1       CANNOT HIT PF1 FROM TEXT SCREEN              
         BE    PFKEYX                                                           
         MVC   SAVEDSTR+17(7),SAVEDSTR+34                                       
         BAS   RE,FULTXT           RETRIEVE EXTENDED TEXT                       
         BO    ERROR5                                                           
         TM    FLAG,FOUND                                                       
         BO    PFKEYB              DISPLAY HELP SCREEN                          
*                                                                               
         LA    R0,281                                                           
         XC    DMCB(24),DMCB       NO HELP AVAILABLE                            
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'3C',ERRMSG),(C'E',0),,(X'88',0)             
         MVC   SRVMSG,ERRMSG                                                    
         MVI   SAVEDSTR+16,0       RESET LAST KEY HIT TO 0                      
         XC    SRVP4,SRVP4         CLEAR P4                                     
         B     WSCOPY              IF NO HELP REDISPLAY LIST                    
*                                                                               
PFKEYA   CLI   PFKEY,2             CONNECT TO ANOTHER PROGRAM ?                 
         BNE   PFKEYX                                                           
         CLI   DDS,1               ONLY AVAILABLE FOR CLIENTS                   
         BE    PFKEYX                                                           
         CLI   OVSYS,0             DO WE NEED TO CONNECT                        
         BE    CONINIT                                                          
         CLI   SAVEDSTR+16,1       HIT CONNECT FROM HELP SCREEN ?               
         BE    CONNECT                                                          
         CLI   SAVEDSTR+16,2       CANNOT CONNECT PAST END OF LIST              
         BE    PFKEYZ                                                           
         BAS   RE,PSCNTL           HIT CONNECT FROM PROGRAM LIST                
         B     EXIT                                                             
         DROP  RF                                                               
*                                                                               
PFKEYB   MVC   SAVEDSTR+12(4),CURADR                                            
         MVI   SAVEDSTR+24,C'Y'                                                 
         LA    R2,SRVL1H           POSITION CURSOR                              
         LA    R0,125                                                           
         XC    DMCB(24),DMCB       HELP DISPLAYED MESSAGE                       
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVMSG,DESCR                                                     
         LA    R0,300              ALT PFKEY LINE                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVOV3,DESCR                                                     
         B     PRINTY                                                           
*                                                                               
PFKEYX   MVI   SAVEDSTR+16,0       SET PREVIOUS KEY HIT TO 0                    
PFKEYZ   CLI   DDS,0               IF NOT DDS NO OTHER PARAMETERS               
         BE    WSCOPY                                                           
         EJECT                                                                  
**********************************************************************          
*P2 - SORT TYPE                                                      *          
**********************************************************************          
         SPACE 1                                                                
VALP2    LA    R2,SRVP2H          R2=A(SRVP2)                                   
         LA    R3,SORTTYPE        R3=A(SORT OPTIONS TABLE)                      
         ZIC   R1,SRVP2H+5                                                      
         LTR   R1,R1              DEFAULT IS ALPHA SORT                         
         BZ    VALP2B                                                           
         BCTR  R1,0                                                             
*                                                                               
VALP2A   EX    R1,*+8             SEARCH TABLE OF SORT TYPES                    
         B     *+10                                                             
         CLC   SRVP2(0),8(R3)                                                   
         BE    VALP2B                                                           
         LA    R3,L'SORTTYPE(R3)                                                
         CLI   8(R3),C' '                                                       
         BNE   VALP2A                                                           
         B     ERROR1             INVALID SORT OPTION                           
*                                                                               
VALP2B   XC    SRVP2,SRVP2        ECHO BACK FULL VERSION                        
         MVC   SRVP2(7),8(R3)                                                   
         L     R2,4(R3)           ROUTINE TO VALIDATE P4                        
         A     R2,RELO                                                          
         ST    R2,AP4VAL                                                        
         L     R2,0(R3)           ROUTINE TO BUILD SORT KEY                     
         A     R2,RELO                                                          
         ST    R2,ASORTKEY                                                      
*                                                                               
VALP2C   CLI   SRVP2,C'T'         IF TABLE                                      
         BE    *+12                                                             
         CLI   SRVP2,C'C'         OR COUNT                                      
         BNE   *+8                                                              
         OI    FLAG,FLAGT         SET TABLE FLAG                                
*                                                                               
         CLC   SRVP2(2),=C'RE'    RETSTRICTED OR UNRESTRICTED                   
         BE    VALP3                                                            
         CLC   SRVP2(2),=C'UN'                                                  
         BE    VALP3                                                            
         CLC   SRVP2(2),=C'UP'    P3 PROGRAM NAME IF UP/DOWN                    
         BE    VALP3                                                            
         CLC   SRVP2(4),=C'DOWN'                                                
         BE    VALP3                                                            
         CLC   SRVP2(2),=C'C1'                                                  
         BNE   *+12                                                             
         OI    FLAG,FLAGC1                                                      
         B     VALP2X                                                           
VALP2X   LA    R5,SRVP3H          P3 COUNTRY NAME                               
         CLI   SRVP3H+5,0                                                       
         BNE   VALP3I                                                           
         B     WSCOPY                                                           
         EJECT                                                                  
**********************************************************************          
*P3  - SHOULD BE EITHER 'ALL' OR A PROGRAM NAME                      *          
**********************************************************************          
         SPACE 1                                                                
VALP3    LA    R2,SRVP3H          R2=A(SRVP3)                                   
         L     R5,APGMLST         R5=A(PROGRAM LIST FOR SYSTEM)                 
         BAS   RE,SETBXLE                                                       
         USING PGMLSTD,R5                                                       
         CLC   SRVP3(3),=C'ALL'                                                 
         BE    VALP3C                                                           
         SR    R1,R1                                                            
         IC    R1,SRVP3H+5                                                      
         BCTR  R1,0                                                             
*                                                                               
VALP3A   EX    R1,*+8              SEARCH PROGRAM LIST FOR NAME                 
         B     *+10                                                             
         CLC   PGMNAME(0),SRVP3    CHECK PROGRAM IN LIST                        
         BE    *+12                                                             
         BXLE  R5,R6,VALP3A                                                     
         B     ERROR3              PROGRAM DOES NOT EXIST                       
*                                                                               
         MVC   BYTE,PGMNUM         SAVE PROGRAM NUMBER                          
         L     R5,APGMLST                                                       
         BAS   RE,SETBXLE          POSITION BACK TO TOP OF LIST                 
*                                                                               
VALP3B   CLC   SRVP3(3),=C'ALL'    LOOP THRU PROG LIST FOR MATCH                
         BE    VALP3C                                                           
         CLC   BYTE,PGMNUM         LOOP THRU PROG LIST FOR MATCH                
         BNE   VALP3H              ON PROG NO                                   
*                                                                               
VALP3C   CLC   SRVP2(2),=C'RE'                                                  
         BE    VALP3D                                                           
         CLC   SRVP2(2),=C'UN'                                                  
         BE    VALP3E                                                           
         CLC   SRVP2(2),=C'UP'                                                  
         BE    VALP3F                                                           
         OI    PGMIND,PGMINOP      SET NO-OP                                    
         LA    R0,33               SET NO-OP MESSAGE                            
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVMSG,DESCR                                                     
         MVC   MESSAGE+25(8),=CL8'STOPPED'                                      
         B     VALP3G                                                           
*                                                                               
VALP3D   OI    PGMIND,PGMIACC      SET RESTRICTED                               
         LA    R0,35               SET MESSAGE                                  
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVMSG,DESCR                                                     
         MVC   MESSAGE+25(8),=CL8'DISABLED'                                     
         B     VALP3G                                                           
*                                                                               
VALP3E   NI    PGMIND,255-PGMIACC  SET UNRESTRICTED                             
         LA    R0,36               SET MESSAGE                                  
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVMSG,DESCR                                                     
         MVC   MESSAGE+25(8),=CL8'ENABLED'                                      
         B     VALP3G                   '                                       
*                                                                               
VALP3F   NI    PGMIND,255-PGMINOP  CLR NO OP                                    
         LA    R0,34               SET MESSAGE                                  
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVMSG,DESCR                                                     
         MVC   MESSAGE+25(8),=CL8'STARTED'                                      
*                                                                               
VALP3G   GOTO1 PTICTOC,DUB,C'SSET'                                              
         L     RE,PVSSB                                                         
         MVC   MESSAGE+4(3),SSBSYSNA-SSBD(RE)                                   
         MVC   MESSAGE+9(L'SYSLNAME),RCSYSLNM                                   
         MVC   MESSAGE+17(L'PGMNAME),PGMNAME                                    
         GOTO1 PDMOD000,DMCB,PWCTYPE,MESSAGE,LMESSAGE,C'LVL1'                   
         GOTO1 PTICTOC,DUB,C'RSET'                                              
*                                                                               
VALP3H   BXLE  R5,R6,VALP3B                                                     
         B     WSCOPY                                                           
         EJECT                                                                  
**********************************************************************          
*P3  - SHOULD BE A VALID COUNTRY                                     *          
**********************************************************************          
         SPACE 1                                                                
VALP3I   LA    R2,SRVP3H                                                        
         L     R5,ACTRYTAB                                                      
         BAS   RE,SETBXLE                                                       
         USING CTRYTABD,R5                                                      
VALP3J   SR    R1,R1               SET UP FOR EXECUTED COMPARE                  
         IC    R1,SRVP3H+5                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTRYSHRN(0),SRVP3   CHECK COUNTRY IN LIST                        
         BE    VALP3K                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTRYSHR(0),SRVP3    CHECK COUNTRY IN LIST                        
         BE    VALP3K                                                           
         BXLE  R5,R6,VALP3J                                                     
         B     ERROR1              COUNTRY NOT FOUND                            
VALP3K   MVC   SRVP3(3),CTRYSHR                                                 
         ZIC   RF,CTRYCODE                                                      
         STC   RF,CTRY                                                          
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CREATE A WORKING STORAGE COPY OF THIS SYSTEM'S PROGRAM LIST         *         
***********************************************************************         
         SPACE 1                                                                
WSCOPY   L     R5,APGMLST                                                       
         BAS   RE,SETBXLE          INITIALISE TABLE POINTERS                    
         USING PGMLSTD,R5                                                       
         LA    R4,TABLE                                                         
         USING TABLED,R4                                                        
         SR    R3,R3                                                            
         XC    SYNTAB,SYNTAB       SET SYNONYM TABLE TO ZEROS                   
*                                                                               
WSCOPYA  TM    FLAG,FLAGT          DISPLAY ALL PROGRAMS IN TABLE ORDER          
         BNO   *+12                WHEN CTRY=0                                  
         CLI   CTRY,0                                                           
         BE    WSCOPYC                                                          
*                                                                               
         CLI   PGMCTRY,0                                                        
         BE    WSCOPYC             HAS ALL PROGRAMS BEEN ADDED TO TABLE         
         CLC   CTRY,PGMCTRY                                                     
         BE    WSCOPYB                                                          
         TM    FLAG,FOUND                                                       
         BO    WSCOPYG             NO MORE TABLE ENTRIES                        
         B     WSCOPYF                                                          
WSCOPYB  CLC   CTRY,PGMCTRY        WHILE COUNTRY CODES MATCH                    
         BNE   WSCOPYG                                                          
         OI    FLAG,FOUND          SET FLAG ON - PROGRAMS FOUND                 
         B     *+12                                                             
WSCOPYC  TM    FLAG,FOUND          ACCESS GENERAL TABLE ENTRIES ?               
         BO    WSCOPYG                                                          
*                                                                               
WSCOPYC1 CLI   DDS,1               RESTRICTED ACCESS ?                          
         BE    WSCOPYC2                                                         
         TM    PGMIND,PGMIACC                                                   
         BO    WSCOPYF                                                          
         TM    PGMIND4,PGMIACCI                                                 
         BO    WSCOPYF                                                          
WSCOPYC2 SR    RF,RF               RESTRICTED AGENCY LIST PRESENT ?             
         ICM   RF,7,PGMAGYLA                                                    
         BZ    WSCOPYC4                                                         
         LA    R0,20                                                            
         OC    AGY,AGY             TEST IF LOGGED ON                            
         BNZ   WSCOPYC3                                                         
         CLI   DDS,1               DDS SEES IT ANYWAY                           
         BE    WSCOPYC4                                                         
         B     WSCOPYF             AGENCY DOESNT SEE IT                         
WSCOPYC3 CLC   0(2,RF),SPACES      DONT SHOW IF AGENCY NOT IN LIST              
         BE    WSCOPYF                                                          
         CLC   0(2,RF),AGY                                                      
         BE    WSCOPYC4                                                         
         LA    RF,2(RF)                                                         
         BCT   R0,WSCOPYC3                                                      
         B     WSCOPYF                                                          
*                                                                               
WSCOPYC4 TM    FLAG,FLAGT          INCLUDE SYNONYMS FOR TABLE ORDER             
         BO    WSCOPYD                                                          
         CLI   DDS,0               INCLUDE SYNONYMS FOR CLIENT DISPLAY          
         BE    WSCOPYD                                                          
         ZIC   RF,PGMNUM                                                        
         BCTR  RF,0                                                             
         LA    RF,SYNTAB(RF)                                                    
         CLI   0(RF),0             IS THIS A SYNONYM                            
         BNE   WSCOPYF             YES READ NEXT                                
         MVC   0(1,RF),ONE         SET FOR SYNONYM CHECK                        
*                                                                               
WSCOPYD  MVC   TPGMID,PGMNAME      COPY EACH TABLE ENTRY                        
         MVC   TPGMNO,PGMNUM                                                    
         MVC   TPGMIND,PGMIND                                                   
         MVC   TPGMIND2,PGMIND2                                                 
         MVC   TPGMIND3,PGMIND3                                                 
         MVC   TPGMIND4,PGMIND4                                                 
         MVC   TPGMCTY,PGMCTRY                                                  
         MVC   TPGMTXT,PGMTEXT                                                  
         MVC   TPGMCNT1,PGMCNT1                                                 
         XC    TPGMCNT2,TPGMCNT2                                                
         STC   R3,TPGMPOS                                                       
         CLI   DDS,1                                                            
         BNE   WSCOPYE                                                          
         L     R2,ASORTKEY                                                      
         BASR  RE,R2               LOAD SORT KEY IN TABLE ENTRY                 
         B     *+10                                                             
WSCOPYE  MVC   TKEY,TPGMID         ALWAYS ALPHA FOR NON DDS                     
         LA    R4,TENTLN(R4)                                                    
         LA    R3,1(R3)                                                         
WSCOPYF  BXLE  R5,R6,WSCOPYA                                                    
*                                                                               
         DROP  R4,R5                                                            
WSCOPYG  STC   R3,TENTNO           # OF TABLE ENTRIES                           
*                                                                               
         LA    R3,TENTLN           SET UP BXLE 6 BYTE PREFIX AREA               
         SR    R4,R3                                                            
         STH   R3,TABBXLE                                                       
         ST    R4,TABBXLE+2                                                     
         EJECT                                                                  
***********************************************************************         
*      SORT WORKING STORAGE TABLE ACCORDING TO KEY                    *         
***********************************************************************         
         SPACE 1                                                                
         XC     DMCB+4(3),DMCB+4   SET UP PARAMETER 2 TO BE THE                 
         MVC    DMCB+7(1),TENTNO   | NUMBER OF TABLE ENTRIES                    
         GOTO1  PXSORT,DMCB,(0,TABLE),,TENTLN,L'TKEY,0                          
         SPACE  1                                                               
***********************************************************************         
*      PRINT OUT PROGRAM LIST FROM WORKING STORAGE TABLE              *         
***********************************************************************         
         SPACE 1                                                                
PRINT    LA    R5,TABBXLE          GET ADDRESS OF TABLE                         
         BAS   RE,SETBXLE                                                       
         XC    COUNT,COUNT         SCREEN FIELD COUNT                           
*                                                                               
         CLI   DDS,0               DIFFERENT DISPLAY FOR NON DDS                
         BE    PRINT2                                                           
         L     R3,AP4VAL           APPROPRIATE P4 VALIDATION ROUTINE            
         BASR  RE,R3               DO WE HAVE A MATCH ON P4?                    
         BL    *+8                 YES ->                                       
         B     PRINTA                                                           
         BXLE  R5,R6,*-10          NO -- TRY NEXT PROGRAM                       
         LA    R2,SRVP4H           POSITION CURSOR                              
         B     ERROR3              PROGRAM DOES NOT EXIST                       
         SPACE 1                                                                
***********************************************************************         
* RETRIEVE OVERLAY SCREEN AND CREATE EACH LINE                        *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
PRINTA   LA    R1,SRVL3H                                                        
         ST    R1,DMCB             RETRIEVE SCREEN OVERLAY                      
         MVC   DMCB+4,=X'D90141FE'                                              
         GOTO1 PCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRNTA2   BAS   RE,GETADDR          SETS R1 TO CORRECT OUTPUT POSITION           
         BZ    PRINTC              LAST LINE?  YES ->                           
         LR    R2,R1                                                            
         BAS   RE,DISPLY           NO -> DISPLAY LINE, RETURN PRINTB            
PRINTB   BXLE  R5,R6,PRNTA2        NEXT LIST ITEM                               
*                                                                               
         LA    R0,27               FOOTLINE                                     
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',HDREC),(C'T',0),,(X'88',0)              
         MVC   SRVOV3,HDREC                                                     
         MVI   SRVOV3H+6,X'08'                                                  
*                                                                               
         USING TABLED,R5                                                        
         MVC   SRVP4(7),SPACES                                                  
         LA    R0,31               SET MESSAGE                                  
         LA    R2,SRVSRVH          SET CURSOR                                   
         B     PRINTX2                                                          
*                                                                               
PRINTC   XC    SRVP4,SRVP4                                                      
         CLI   SRVP2,C'A'          OUTPUT LAST ALPHA PROGRAM                    
         BNE   PRINTD                                                           
         MVC   SRVP4(7),TPGMID                                                  
         LA    R0,32               SET MESSAGE                                  
         LA    R2,SRVP5H           SET CURSOR                                   
         B     PRINTX2                                                          
*                                                                               
PRINTD   CLI   SRVP2,C'N'          OUTPUT LAST HEX PROGRAM                      
         BNE   PRINTE                                                           
         LA    R0,32               SET MESSAGE                                  
         LA    R2,SRVP5H           SET CURSOR                                   
         GOTO1 PHEXOUT,DMCB,TPGMNO,SRVP4,1,=C'TOG'                              
         B     PRINTX2                                                          
*                                                                               
PRINTE   TM    FLAG,FLAGT          OUTPUT LAST TABLE POSITION                   
         BNO   PRINTX2                                                          
         LA    R0,32               SET MESSAGE                                  
         LA    R2,SRVP5H           SET CURSOR                                   
         SR    RF,RF                                                            
         IC    RF,TPGMPOS                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SRVP4(3),DUB        SHOW AS NNN TO IDENTIFY IT                   
         B     PRINTX2                                                          
*                                                                               
PRINTX   CLI   SAVEDSTR+24,C'Y'    CARRY ON DISPLAY AFTER HELP SCREEN           
         BNE   PRINTX2                                                          
         XC    DMCB(24),DMCB       SET MESSAGE LINE                             
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVMSG,DESCR                                                     
         SR    R1,R1                                                            
         L     R1,SAVEDSTR+12      CALCULATE CURSOR POSITION                    
         S     R1,=F'1'                                                         
         M     R0,=F'92'                                                        
         S     R1,=F'8'                                                         
         LA    R1,SRVL1(R1)        SET CURSOR TO FIELD PREVIOUSLY               
         OI    6(R1),X'40'         SET CURSOR ON                                
         MVI   SAVEDSTR+7,C'N'     SET ERROR = NO IN SAVED STORAGE              
         MVI   SAVEDSTR+24,C'N'    SET HELP DISPLAYED BACK T0 N                 
         MVC   SAVEDSTR(7),SRVP1   SET SYSTEM IN SAVED STORAGE                  
         BAS   RE,WRITESTR         SET SAVED STORAGE                            
         B     SRXMOD                                                           
*                                                                               
PRINTX2  CLI   SRVMSG,0            HAS MESSAGE ALREADY BEEN SET                 
         BNE   PRINTY                                                           
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVMSG,DESCR                                                     
*                                                                               
*********************************************************************           
* IF NO HELP AVAILABLE RETURN TO PROGRAM LIST AND SET CURSOR        *           
*********************************************************************           
         SPACE 1                                                                
PRINTY   CLI   SAVEDSTR+7,C'Y'     HELP SCREEN WASNT AVAILABLE                  
         BNE   PRINTZ                                                           
         SR    R1,R1                                                            
         L     R1,CURADR           CALCULATE CURSOR POSITION                    
         S     R1,=F'1'                                                         
         M     R0,=F'92'                                                        
         S     R1,=F'8'                                                         
         LA    R1,SRVL1(R1)        SET CURSOR TO FIELD PREVIOUSLY               
         OI    6(R1),X'40'         SET CURSOR ON                                
         MVI   SAVEDSTR+7,C'N'     SET ERROR = NO IN SAVED STORAGE              
*        MVI   SAVEDSTR+24,C'N'    SET HELP DISPLAYED BACK TO N                 
         MVC   SAVEDSTR(7),SRVP1   SET SYSTEM IN SAVED STORAGE                  
         BAS   RE,WRITESTR         SET SAVED STORAGE                            
         B     SRXMOD                                                           
*                                                                               
PRINTZ   MVC   SAVEDSTR(7),SRVP1   SET SYSTEM IN SAVED STORAGE                  
         MVI   SAVEDSTR+7,C'N'     SET ERROR = NO IN SAVED STORAGE              
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CONNECT TO PROGRAM FROM THE HELP SCREEN                             *         
***********************************************************************         
         SPACE 1                                                                
CONNECT  CLC   SAVEDSTR(7),=C'SERVICE'                                          
         BE    *+14                                                             
         CLC   SAVEDSTR(7),PSYSNME                                              
         BNE   ERROR6                                                           
         MVC   NAMEX(7),SAVEDSTR+25                                             
         L     R1,=F'7'            PROG NAME 7 CHARS LONG                       
         L     R3,PBUFF            LOCATE TERMINAL BUFFER                       
         LR    R4,R1                                                            
         A     R4,=F'5'                                                         
         STC   R4,0(R3)            INSERT LINE LENGTH                           
         S     R1,=F'1'            ACTUAL LENGTH OF NAME                        
         S     R4,=F'1'            ACTUAL LENGTH OF LINE                        
         MVC   1(2,R3),SRVSRVH+2   INSERT SCREEN ADDRESSES                      
         MVC   3(1,R3),=C'='                                                    
         EX    R1,*+8              INSERT PROGRAM NAME                          
         B     *+10                                                             
         MVC   4(0,R3),NAMEX                                                    
         AR    R3,R4               INSERT 0 AT END OF LINE                      
         MVI   0(R3),X'0'                                                       
         SR    R3,R3                                                            
*                                                                               
         MVC   SRVSRV(8),=C'=GOBACK '                                           
         XC    SAVEDSTR,SAVEDSTR   BLANK SAVED STOREAGE                         
         BAS   RE,WRITESTR                                                      
         B     SRXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY PROGRAM NAME, PROGRAM NUMBER AND STATUS INDICATORS          *         
***********************************************************************         
         SPACE 1                                                                
         USING LINED,R2            FORMAT LINE                                  
         USING TABLED,R5                                                        
DISPLY   ST    RE,SAVEREG                                                       
         MVC   LINNAM,TPGMID       CLEAR P4 AND POSITION CURSOR                 
         GOTO1 PHEXOUT,DMCB,TPGMNO,LINNUM,1,=C'TOG'                             
         MVI   SRVP4,C' '                                                       
         MVC   SRVP4+1(L'SRVP4-1),SRVP4                                         
*                                                                               
         TM    FLAG,FLAGC1         TEST FOR COUNT FLAGS                         
         BZ    DISPLY1                                                          
         ICM   R0,15,TPGMCNT1                                                   
*                                                                               
         EDIT  (R0),(7,LININD),ZERO=NOBLANK                                     
         B     DISPLYX                                                          
*                                                                               
DISPLY1  LA    R3,LININD           DEAL WITH EACH BIT IN TURN                   
         LA    RE,INDTAB                                                        
DISPLA   ZIC   R1,0(RE)            R1=TM EXECUTE MASK (PGMIND)                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    TPGMIND,0                                                        
         BZ    *+14                                                             
         MVC   0(1,R3),1(RE)       BIT ON OUTPUT VALUE                          
         B     *+10                                                             
         MVC   0(1,R3),2(RE)       BIT OFF OUTPUT VALUE                         
         LA    RE,L'INDTAB(RE)                                                  
         LA    R3,1(R3)                                                         
         CLI   0(RE),0                                                          
         BNE   DISPLA              NEXT BIT                                     
*                                                                               
DISPLB   TM    TPGMIND,PGMIACC     TEST RESTRICTED                              
         BO    DISPLC              YES                                          
         TM    TPGMIND4,PGMIACCI   TEST IF INDIRECT ACCESS ONLY                 
         BZ    DISPLC                                                           
         MVI   LININD+2,C'X'                                                    
*                                                                               
DISPLC   TM    FLAG,FLAGT          IS SORT CODE = TABLE                         
         BNO   DISPLYX             YES                                          
         CLC   LININD(1),=C'*'     IS PROGRAM NO OP                             
         BE    DISPLYX             NO                                           
         CLI   TPGMCTY,0           YES IS COUNTRY NOT UK                        
         BE    DISPLYX             NO                                           
         OI    TPGMCTY,C'0'        YES,DISPLAY COUNTRY CODE IN 1ST BIT          
         ZIC   RE,TPGMCTY                                                       
         STC   RE,LININD                                                        
*                                                                               
DISPLYX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* GET A(NEXT TWA OUTPUT FIELD) IN R1                                  *         
***********************************************************************         
         SPACE 1                                                                
GETADDR  L     R1,COUNT                                                         
         LA    RF,1(R1)            ADD 1 TO LINE COUNT                          
         ST    RF,COUNT                                                         
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         D     R0,=F'16'           17 ROWS OF DATA ON SCREEN                    
         CH    R1,=H'3'            4 COLLUMNS                                   
         BH    GETADDRX                                                         
         MH    R0,=H'86'           86=78+8 LENGTH OF SRVL1H+SRVLI               
         MH    R1,=H'20'           20 = LENGTH OF EACH COLLUMN                  
         LA    R1,SRVOV1(R1)       LA R1,(CONTENTS OF R1)PAST SRVL1             
         AR    R1,R0                                                            
         LR    RF,R1                                                            
GETADDRX LTR   R1,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET UP DISPLAY FOR NON DDS TERMINAL                                 *         
***********************************************************************         
         SPACE 1                                                                
PRINT2   LA    R2,SRVL1H                                                        
         USING LNTWOD,R2                                                        
         USING TABLED,R5                                                        
         LA    R4,SAVEDSTR                                                      
         USING SAVED,R4                                                         
         L     R8,=F'0'                                                         
*                                                                               
PRINT2A  CLI   SAVEDSTR+17,C' '    NEXT PAGE ?                                  
         BNH   *+12                                                             
         BAS   RE,POSSCN                                                        
         BL    PRINT2B                                                          
         XC    SRVP4,SRVP4         CLEAR P4                                     
         SR    R0,R0               GET PROGRAM TEXT NUMBER                      
         ICM   R0,3,TPGMTXT                                                     
         BZ    PRINT2B                                                          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(70,DESCR),(C'T',0),,(X'8A',0)                 
         CLC   DESCR(4),NONE                                                    
         BNE   *+14                                                             
         CLC   DESCR+4(20),SPACES                                               
         BE    PRINT2B             IGNORE IF NONE                               
*                                                                               
         MVC   LNAME,TPGMID        MOVE PROG NAME TO SCREEN                     
         MVC   STXTID,TPGMTXT      MOVE TEXT ID TO SAVED STORAGE                
         MVC   SNAME,TPGMID        MOVE PROG NAME TO SAVED STORAGE              
*                                                                               
         MVC   LDESCR,DESCR        MOVE TEXT TO SCREEN                          
         OI    FLHDR+6,X'80'       SET TRANSMIT IND                             
         LA    R2,8+L'SRVL1(R2)    SET NEXT SCREEN POSITION                     
         LA    R2,8+L'SRVL2(R2)                                                 
         A     R8,=F'1'                                                         
         LA    R4,SAVEDL(R4)       SET NEXT SAVED STORAGE POSITION              
         LA    R0,SRVLLH                                                        
         CR    R2,R0               END OF SCREEN ?                              
         BNL   PRINT2C                                                          
*                                                                               
PRINT2B  BXLE  R5,R6,PRINT2A                                                    
         LA    R4,SAVEDSTR                                                      
         USING SAVED,R4                                                         
         MVC   SLNAME(7),=C'       '                                            
         ST    R8,STOTAL                                                        
         DROP  R4                                                               
         LA    R0,31               SET MESSAGE LINE                             
         LA    R2,SRVL1H           SET CURSOR                                   
         B     PRINTX                                                           
*                                                                               
PRINT2C  BXLE  R5,R6,PRINT2D       SET P4 NEXT PROG TO DISPLAY                  
         MVC   SAVEDSTR+17(7),=C'       '                                       
         LA    R0,31               SET MESSAGE LINE                             
         LA    R2,SRVL1H           SET CURSOR                                   
         B     PRINT2Z                                                          
*                                                                               
PRINT2D  ICM   R0,3,TPGMTXT                                                     
         BZ    PRINT2C                                                          
         MVC   SAVEDSTR+17(7),TPGMID                                            
         LA    R0,32               SET MESSAGE LINE                             
         LA    R2,SRVL1H           SET CURSOR                                   
*                                                                               
PRINT2Z  LA    R4,SAVEDSTR                                                      
         USING SAVED,R4                                                         
         ST    R8,STOTAL                                                        
         DROP  R4                                                               
         B     PRINTX                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP R5 - R7 FOR A TABLE SEARCH USING BXLE, ASSUMING 6 BYTE PREFIX*         
***********************************************************************         
         SPACE 1                                                                
SETBXLE  LH    R6,0(R5)            SET BXLE REGS                                
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO MOVE SORT CODE ELEMENT INTO KEY   - 0(R4)               *         
***********************************************************************         
         SPACE 1                                                                
         DROP  R5                                                               
         USING TABLED,R4                                                        
ALPKEY   MVC   TKEY,TPGMID                                                      
         BR    RE                                                               
*                                                                               
NUMKEY   MVC   TKEY(1),TPGMNO                                                   
         MVC   TKEY+1(L'TPGMID-1),TPGMID                                        
         BR    RE                                                               
*                                                                               
TABKEY   XC    TKEY,TKEY                                                        
         MVC   TKEY(1),TPGMPOS                                                  
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO VARIFY P4 MATCHES THE SORT CRITERIA - AP4VAL            *         
***********************************************************************         
         SPACE 1                                                                
         USING TABLED,R5                                                        
ALPP4    LA    R2,SRVP4H           POSITION CURSOR                              
         LA    RF,SRVP4H           STARTING PROGRAM ID                          
         CLI   5(RF),7             MAX LENGTH 7                                 
         BH    ERROR1                                                           
         ZIC   R1,5(RF)                                                         
         LTR   R1,R1               ANY CHARACTERS GIVEN?                        
         BZ    ALPP4X              NO ->                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PNAME(*-*),SRVP4                                                 
         CLC   TPGMID,PNAME        ARE WE ALPHABETICALLY HERE                   
ALPP4X   BR    RE                                                               
*                                                                               
NUMP4    LA    R2,SRVP4H           POSITION CURSOR                              
         LA    RF,SRVP4H           STARTING PROGRAM NUMBER                      
         CLI   5(RF),0             ANY NUMBER GIVEN?                            
         BE    NUMP4X              NO ->                                        
         CLI   5(RF),2             MUST BE TWO CHARACTER PROGRAM NUMBER         
         BH    ERROR1              IT ISN'T                                     
         BE    *+14                                                             
         MVC   SRVP4+1(1),SRVP4                                                 
         MVI   SRVP4,C'0'                                                       
         ST    RE,SAVEREG                                                       
         GOTO1 PHEXIN,DMCB,SRVP4,BYTE,2                                         
         L     RE,SAVEREG                                                       
         OC    DMCB+12(4),DMCB+12  WAS INPUT VALID HEX?                         
         BZ    ERROR3              NO                                           
         CLC   TPGMNO,BYTE         DO WE HAVE A MATCH ON PROGRAM NUM?           
NUMP4X   BR    RE                                                               
*                                                                               
POSP4    LA    R2,SRVP4H           POSITION CURSOR                              
         LA    RF,SRVP4H           STARTING TABLE POSITION                      
         CLI   5(RF),0             ANY NUMBER GIVEN?                            
         BE    POSP4X              NO ->                                        
*                                                                               
         SR    R1,R1               MUST BE NUMERIC                              
         LA    R1,SRVP4                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         CLI   0(R1),C'0'                                                       
         BL    ERROR4                                                           
         CLI   1(R1),C' '                                                       
         BNH   *+12                                                             
         CLI   1(R1),C'0'                                                       
         BL    ERROR4                                                           
         CLI   2(R1),C' '                                                       
         BNH   *+12                                                             
         CLI   2(R1),C'0'                                                       
         BL    ERROR4                                                           
*                                                                               
         CLI   5(RF),3             MUST BE THREE CHAR TABLE POSITION            
         BH    ERROR1              ITS MORE ->                                  
         BE    POSP4B                                                           
         CLI   5(RF),2             IS IT 2 CHARS                                
         BL    POSP4A                                                           
         B     POSP4B                                                           
POSP4A   MVC   SRVP4+2(1),SRVP4    PAD WITH 00                                  
         MVC   SRVP4(2),=C'00'                                                  
         SR    R1,R1               ADD 2 TO THE FIELD LENGTH                    
         IC    R1,SRVP4H+5                                                      
         LA    R1,2(R1)                                                         
         STC   R1,SRVP4H+5                                                      
*                                                                               
POSP4B   SR    R1,R1                                                            
         IC    R1,SRVP4H+5                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),SRVP4(0)                                                  
         CVB   R1,DUB                                                           
         STC   R1,BYTE                                                          
         CLC   TPGMPOS,BYTE        DO WE HAVE A MATCH ON PROGRAM POS?           
POSP4X   BR    RE                                                               
         DROP  R5                                                               
*                                                                               
         USING TABLED,R5                                                        
POSSCN   CLC   TPGMID(7),SAVEDSTR+17                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST OF OPTIONS AVAILABLE FOR PARAMETER 3                   *         
***********************************************************************         
         SPACE 1                                                                
QUERY    LA    RF,17                                                            
         ST    RF,COUNT                                                         
         LA    R3,SORTTYPE                                                      
EACHTYPE BAS   RE,GETADDR           GET SCREEN ADDRESS                          
         MVC   0(8,R1),8(R3)                                                    
         LA    R3,L'SORTTYPE(R3)    LOOP THROUGH SORTTYPE TABLW                 
         CLI   8(R3),C' '                                                       
         BNE   EACHTYPE                                                         
         LA    R2,SRVP2H            POSITION CURSOR                             
         LA    R0,38                                                            
         XC    DMCB(24),DMCB        SET MESSAGE LINE                            
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVMSG,DESCR                                                     
         B     EXIT                                                             
*                                                                               
***********************************************************************         
* DISPLAY LIST OF SYSTEMS FROM THE SYSTEM TABLE                       *         
***********************************************************************         
         SPACE 1                                                                
         USING SYSLSTD,R3                                                       
SYSQRY   LA    R2,SRVL1H                                                        
         USING LNTWOD,R2                                                        
SYSQRYA  MVC   LNAME,SYSLNAME       MOVE TABLE NAME TO SCREEN                   
         OI    FLHDR+6,X'80'                                                    
         LA    R3,SYSLLEN(R3)       POSITION TO NEXT TABLE ENTRY                
         LA    R2,8+L'SRVL1(R2)                                                 
         CLI   SYSLNUM,0            END OF TABLE ?                              
         BNE   SYSQRYA                                                          
         XC    SRVH1,SRVH1          CLEAR HEAD LINE                             
         LA    R2,SRVP1H            POSITION CURSOR                             
         LA    R0,37                                                            
         XC    DMCB(24),DMCB        SET MESSAGE LINE                            
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',DESCR),(C'T',0),,(X'88',0)              
         MVC   SRVMSG,DESCR                                                     
         B     EXIT                                                             
*                                                                               
ERROR1   LA    R0,02                                                            
         B     ERRORX                                                           
*                                                                               
ERROR2   LA    R0,32                                                            
         B     ERRORX                                                           
*                                                                               
ERROR3   LA    R0,33                                                            
         B     ERRORX                                                           
*                                                                               
ERROR4   LA    R0,3                                                             
         B     ERRORX                                                           
*                                                                               
ERROR5   LA    R0,281                                                           
         B     ERRORX                                                           
ERROR6   LA    R0,30                                                            
         LA    R2,SRVL1H                                                        
         B     ERRORX                                                           
*                                                                               
ERRORX   XC    DMCB(24),DMCB       R0=SERVICE ERROR MESSAGE NUMBER              
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'3C',ERRMSG),(C'E',0),,(X'88',0)             
         MVC   SRVMSG,ERRMSG                                                    
*                                                                               
EXIT     NI    SRVSRVH+6,X'FF'-X'40'                                            
         OI    6(R2),X'40'         SET CURSOR ON                                
         BAS   RE,WRITESTR         SET SAVED STORAGE                            
SRXMOD   XMOD1                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* RETRIEVE EXTENDED TEXT FOR DISPLAY                                  *         
***********************************************************************         
         SPACE 1                                                                
FULTXT   NTR1                                                                   
         SR    R0,R0                                                            
         LA    R8,SAVEDSTR                                                      
         USING SAVED,R8                                                         
         MVI   SPFKEY,1            SET PREVIOUS KEY HIT TO 1                    
         ICM   R1,15,CURADR                                                     
         ICM   R2,15,STOTAL                                                     
         CR    R1,R2               CHECK NOT HITTING HELP KEY AFTER             
         BH    FULTXTX             END OF PROGRAM LIST                          
         LA    R8,32(R8)           BUMP PAST SYSTEM NAME ETC                    
         L     R0,=F'1'                                                         
FULTXTA  CR    R0,R1                                                            
         BE    FULTXTB                                                          
         LA    R8,SAVEDL(R8)                                                    
         A     R0,=F'1'                                                         
         B     FULTXTA                                                          
*                                                                               
FULTXTB  MVC   TEXTNO(2),0(R8)                                                  
         ICM   R0,3,TEXTNO         SET R0 FOR LINE DESCRIPTION                  
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(70,DESCR),(C'T',0),,(X'88',0)                 
         MVC   SRVL2,DESCR         MOVE TEXT TO SCREEN                          
*                                                                               
********************************************************************            
* CALL GETTEXT TO RETRIEVE EXTENDED TEXT - IF ANY                  *            
********************************************************************            
*                                                                               
         LA    R1,DMCB             DEFINE GETTXT CONTROL BLOCK                  
         USING GETTXTD,R1                                                       
         XC    DMCB(24),DMCB       SET DSECT TO RETRIEVE KEY                    
         MVI   DMCB+21,1                                                        
         MVC   GTMSGNO(2),0(R8)                                                 
         MVC   SRVL1(7),2(R8)      SET PROG NAME ON FIRST SCREEN                
         MVI   GTMTYP,C'T'                                                      
         MVI   GTMSYS,1            SET TO SERVICE SYSTEM                        
         MVC   GTMLANG,LANG                                                     
         OI    GT1INDS,GT1REF+GT1OHDR                                           
         OI    GT2INDS,GT2SRPF1    INFORM GETTXT SPECIAL SVC CALL               
*                                                                               
         GOTO1 PGETTXT,GETTXTD                                                  
         SPACE 1                                                                
         TM    GT1INDS,GT1RETM0    IGNORE IF RETURNED WITH MSG ZERO             
         BO    FULTXTE                                                          
*                                                                               
         ICM   R3,7,GTAIO                                                       
         LA    R3,GMSGEL-GMSGD(R3) A(SHORT MESSAGE ELEMENT)                     
         ZIC   R1,GMSGELL-GMSGEL(R3)                                            
FULTXTC  AR    R3,R1               SCAN IO AREA FOR TEXT ELEMENT                
         ICM   R1,1,1(R3)                                                       
         BZ    FULTXTE             NO EXPANDED TEXT ELEMENT                     
         CLI   0(R3),GMTXTELC                                                   
         BNE   FULTXTC                                                          
         USING GMTXTD,R3                                                        
*                                                                               
********************************************************************            
* MOVE EXPANDED TEXT TO WORKING STORAGE                            *            
********************************************************************            
*                                                                               
         L     R5,AIOREC                                                        
NEXTLN   ZIC   RF,GMTXTELL                                                      
         SH    RF,=Y(GMTXTFXD+1)   RF=EX L'TEXT                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),GMTXTLIN                                                 
         LA    R5,74(R5)                                                        
         ZIC   R1,GMTXTELL                                                      
         AR    R3,R1               SCAN IO AREA FOR TEXT ELEMENT                
         ICM   R1,1,GMTXTELL                                                    
         BZ    *+12                DONE                                         
         CLI   GMTXTEL,GMTXTELC                                                 
         BE    NEXTLN              MOVE IN NEXT LINE                            
         MVI   0(R5),X'FF'         END OF TEXT                                  
*                                                                               
**********************************************************************          
* RETRIEVE HELP SCREEN OVERLAY AND MOVE IN TEXT                      *          
**********************************************************************          
*                                                                               
         LA    R1,SRVL3H                                                        
         ST    R1,DMCB             RETRIEVE SCREEN OVERLAY                      
         MVC   DMCB+4,=X'D90141FE'                                              
         GOTO1 PCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,SRVOV1H                                                       
         USING DISPLD,R6                                                        
         MVC   DSLINE(L'DSLINE),STARS SET TOP LINE OF BOX                       
         LA    R6,DISPLEN(R6)    SET NEXT SCREEN POSITION                       
         MVC   DSLINE(L'DSLINE),SPACES                                          
         MVC   DSLSTAR,STAR                                                     
         MVC   DSRSTAR,STAR                                                     
         LA    R6,DISPLEN(R6)    SET NEXT SCREEN POSITION                       
         L     R5,AIOREC         TEXT IN W/S                                    
*                                                                               
LINE2    CLI   0(R5),X'FF'                                                      
         BE    ENDTXT                                                           
         MVC   DSLINE(L'DSLINE),SPACES                                          
         MVC   DSDATA(74),0(R5)                                                 
         MVC   DSLSTAR(1),STAR                                                  
         MVC   DSRSTAR(1),STAR                                                  
         LA    R6,DISPLEN(R6)    SET NEXT SCREEN POSITION                       
         LA    R5,74(R5)         NEXT TEXT LINE                                 
         B     LINE2                                                            
*                                                                               
ENDTXT   MVC   SRVMSG(L'SRVMSG),SPACES                                          
         MVC   DSLINE(L'DSLINE),SPACES                                          
         MVC   DSLSTAR(1),STAR                                                  
         MVC   DSRSTAR,STAR                                                     
         LA    R6,DISPLEN(R6)    SET NEXT SCREEN POSITION                       
         MVC   DSLINE(L'DSLINE),STAR                                            
         MVI   SAVEDSTR+7,C'N'                                                  
         MVI   SAVEDSTR+24,C'Y'    HELP SCREEN TO BE DISPLAYED                  
         MVC   SAVEDSTR+25(7),SRVL1 SET HELP TEXT PROGRAM NAME                  
         OI    FLAG,FOUND        SET FLAG FOR HELP TEXT                         
*                                                                               
         LA    R0,39               FOOTLINE                                     
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+21,1                                                        
         GOTO1 PGETTXT,DMCB,(R0),(X'4E',HDREC),(C'T',0),,(X'88',0)              
         MVC   SRVOV3,HDREC                                                     
         MVI   SRVOV3H+6,X'08'                                                  
         B     FULTXTX                                                          
*                                                                               
FULTXTE  MVI   SAVEDSTR+7,C'Y'                                                  
         MVC   SAVEDSTR(7),SRVP1                                                
FULTXTX  XIT1                                                                   
         DROP  R3                                                               
         DROP  R6                                                               
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PASS CONTROL TO ANOTHER PROGRAM                                     *         
***********************************************************************         
         SPACE 1                                                                
PSCNTL   NTR1                                                                   
         SPACE 1                                                                
         LA    R8,SAVEDSTR         BUMP PAST SYSTEM NAME                        
         USING SAVED,R8                                                         
         CLC   SYSNME,=C'SERVICE'  CONNECT TO SERVICE PROGRAM                   
         BE    *+14                                                             
         CLC   SYSNME,PSYSNME      CANNOT CONNECT TO ANOTHER SYSTEM             
         BNE   ERROR6                                                           
         ICM   R1,15,CURADR                                                     
         ICM   R3,15,STOTAL                                                     
         CR    R1,R3               CANNOT CONNECT PAST END OF LIST              
         BH    PSCNTLX                                                          
         LA    R8,32(R8)                                                        
         L     R0,=F'1'                                                         
PSCNTLA  CR    R0,R1                                                            
         BE    PSCNTLB                                                          
         LA    R8,SAVEDL(R8)                                                    
         A     R0,=F'1'                                                         
         B     PSCNTLA                                                          
*                                                                               
PSCNTLB  CLC   2(2,R8),=C'  '                                                   
         BE    PSCNTLZ                                                          
         CLI   2(R8),X'40'                                                      
         BE    PSCNTLZ                                                          
         CLI   2(R8),X'F0'                                                      
         BE    PSCNTLZ                                                          
         CLI   2(R8),C'0'                                                       
         BE    PSCNTLZ                                                          
         MVC   NAMEX(7),2(R8)                                                   
         L     R1,=F'0'                                                         
         L     R3,=F'8'                                                         
         LA    R8,2(R8)            POSITION ON PROGRAM NAME                     
PSCNTLC  CR    R1,R3                                                            
         BE    PSCNTLD                                                          
         CLI   0(R8),C' '          CALCULATE PROG NAME LENGTH                   
         BE    PSCNTLE                                                          
         A     R1,=F'1'                                                         
         LA    R8,1(R8)                                                         
         B     PSCNTLC                                                          
PSCNTLD  L     R1,=F'7'            PROG NAME 7 CHARS LONG                       
PSCNTLE  L     R3,PBUFF            LOCATE TERMINAL BUFFER                       
         LR    R4,R1                                                            
         A     R4,=F'5'                                                         
         STC   R4,0(R3)            INSERT LINE LENGTH                           
         S     R1,=F'1'            ACTUAL LENGTH OF NAME                        
         S     R4,=F'1'            ACTUAL LENGTH OF LINE                        
         MVC   1(2,R3),SRVSRVH+2   INSERT SCREEN ADDRESSES                      
         MVC   3(1,R3),=C'='                                                    
         EX    R1,*+8              INSERT PROGRAM NAME                          
         B     *+10                                                             
         MVC   4(0,R3),NAMEX                                                    
         AR    R3,R4               INSERT 0 AT END OF LINE                      
         MVI   0(R3),X'0'                                                       
         SR    R3,R3                                                            
*                                                                               
         MVC   SRVSRV(8),=C'=GOBACK '                                           
         XC    SAVEDSTR,SAVEDSTR   BLANK OUT SAVED STORAGE                      
         BAS   RE,WRITESTR                                                      
         B     SRXMOD                                                           
         EJECT                                                                  
PSCNTLZ  DC    H'0'                                                             
PSCNTLX  XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        BUILD CONNECT INFO                                 *                   
*************************************************************                   
         SPACE 1                                                                
CONINIT  EQU   *                                                                
         CLC   SAVEDSTR(7),=C'SERVICE'                                          
         BE    ERROR6                                                           
         LA    R8,SAVEDSTR         BUMP PAST SYSTEM NAME                        
         ICM   R1,15,CURADR                                                     
         ICM   R3,15,STOTAL                                                     
         CR    R1,R3               CANNOT CONNECT PAST END OF LIST              
         BH    CONINZ                                                           
         LA    R8,32(R8)                                                        
         L     R0,=F'1'                                                         
CONINA   CR    R0,R1                                                            
         BE    CONINB                                                           
         LA    R8,SAVEDL(R8)                                                    
         A     R0,=F'1'                                                         
         B     CONINA                                                           
*                                                                               
CONINB   CLC   2(2,R8),=C'  '                                                   
         BE    CONINZ                                                           
         CLI   2(R8),X'40'                                                      
         BE    CONINZ                                                           
         CLI   2(R8),X'F0'                                                      
         BE    CONINZ                                                           
         CLI   2(R8),C'0'                                                       
         BE    CONINZ                                                           
         MVC   NAMEX(7),2(R8)                                                   
*        CLC   SAVEDSTR(7),PSYSNME                                              
*        BNE   ERROR6                                                           
         CLI   SAVEDSTR+24,C'Y'                                                 
         BNE   *+10                                                             
         MVC   NAMEX(7),SAVEDSTR+25                                             
         L     R3,PBUFF            LOCATE TERMINAL BUFFER                       
         MVI   0(R3),6                                                          
         MVC   1(2,R3),=X'0000'                                                 
         MVC   3(3,R3),=C'=CT'                                                  
         MVI   6(R3),13                                                         
         MVC   7(2,R3),=X'0000'                                                 
         MVC   9(10,R3),SRVP2                                                   
         MVI   19(R3),13                                                        
         MVC   20(2,R3),=X'0000'                                                
         MVC   22(10,R3),SRVP1                                                  
         MVI   32(R3),10                                                        
         MVC   33(2,R3),=X'0000'                                                
         MVC   35(7,R3),NAMEX                                                   
         MVI   42(R3),13                                                        
         MVC   43(2,R3),=X'0000'                                                
         MVC   45(10,R3),SRVP3                                                  
         MVI   55(R3),0                                                         
*                                                                               
         MVC   SRVSRV(8),=C'=GOBACK '                                           
         XC    SAVEDSTR,SAVEDSTR   BLANK SAVED STOREAGE                         
         BAS   RE,WRITESTR                                                      
CONINZ   B     SRXMOD                                                           
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        READ IN SAVED STORAGE                              *                   
*************************************************************                   
         SPACE 1                                                                
READSTR  NTR1                                                                   
         SPACE 1                                                                
         L     R5,ATIA                                                          
         USING SRSD,R5                                                          
         LA    R2,SRPAGENO         READ IN TWA11                                
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 PDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R2),SRSD                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DMCB(24),DMCB       REMOVE THE SPECIAL BITS                      
         CLC   SRCOMWRK(4),=C'T141'                                             
         BNE   EXIT2                                                            
         LA    R1,SRCOMWRK                                                      
         MVC   SAVEDSTR(L'SAVEDSTR),4(R1)                                       
EXIT2    XIT1                                                                   
         DROP  R5                                                               
*************************************************************                   
*        WRITE OUT SAVED STORAGE                            *                   
*************************************************************                   
         SPACE 1                                                                
WRITESTR NTR1                                                                   
         L     R8,ATIA                                                          
         USING SRSD,R8                                                          
         LA    R1,SRCOMWRK                                                      
         MVC   0(4,R1),=C'T141'                                                 
         MVC   4(L'SAVEDSTR,R1),SAVEDSTR                                        
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         GOTO1 PDATAMGR,DMCB,DMWRT,TEMPSTR,(R2),SRSD                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R8                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS ETC.                                                       *         
***********************************************************************         
         LTORG                                                                  
         SPACE 1                                                                
MESSAGE  DC    C'*FACXXX* SSSSSSS PPPPPPP XXXXXXXX'                             
LMESSAGE EQU   *-MESSAGE                                                        
         SPACE 1                                                                
ERRMSG   DC    CL60' '                                                          
ERRMSG2  DC    CL60' NO EXTENDED TEXT AVAILABLE'                                
HDREC    DC    CL78' '                                                          
DESCR    DC    CL70' '                                                          
DOTS     DC    8CL1'.'                                                          
ONE      DC    CL1'1'                                                           
STAR     DC    CL1'*'                                                           
STARS    DC    78CL1'*'                                                         
SPACES   DC    78CL1' '                                                         
PNAME    DC    7C' '                                                            
MSGCODE  DS    CL1'T'                                                           
NONE     DC    C'NONE'                                                          
LINELEN  DC    Y(SRVL2-SRVL1)      L' LINE                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
DMWRT    DC    CL8'DMWRT'                                                       
DMREAD   DC    CL8'DMREAD'                                                      
         SPACE 1                                                                
INDTAB   DS    0XL3                                                             
         DC    AL1(PGMINOP),CL2'*.'                                             
         DC    AL1(PGMIAHW),CL2'H.'                                             
         DC    AL1(PGMIACC),CL2'R.'                                             
         DC    AL1(PGMIIOB),CL2'I.'                                             
         DC    AL1(PGMIAOR),CL2'O.'                                             
         DC    AL1(PGMIRFU),CL2'C.'                                             
         DC    AL1(PGMIROP),CL2'.U'                                             
         DC    AL1(0)                                                           
         SPACE 1                                                                
         DS    0F                                                               
SORTTYPE DS    0XL16                                                            
         DC    AL4(ALPKEY),AL4(ALPP4),CL8'ALPHA   '                             
         DC    AL4(NUMKEY),AL4(NUMP4),CL8'NUMERIC '                             
         DC    AL4(TABKEY),AL4(POSP4),CL8'TABLE   '                             
         DC    AL4(ALPKEY),AL4(POSP4),CL8'UP      '                             
         DC    AL4(ALPKEY),AL4(POSP4),CL8'DOWN    '                             
         DC    AL4(ALPKEY),AL4(ALPP4),CL8'RESTRIC '                             
         DC    AL4(ALPKEY),AL4(ALPP4),CL8'UNRESTR '                             
         DC    AL4(TABKEY),AL4(POSP4),CL8'C1      '                             
         DC    AL4(QUERY),AL4(POSP4),CL8'?       '                              
         DC    AL4(ERROR1),AL4(POSP4),CL8'        '                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER WORKING STORAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
WRKD     DSECT                                                                  
DUB      DS    D                                                                
CURADR   DS    F                                                                
RELO     DS    F                                                                
SAVEREG  DS    F                                                                
AP4VAL   DS    F                                                                
ASORTKEY DS    F                                                                
ASYNON   DS    F                                                                
COUNT    DS    F                                                                
DMCB     DS    6F                                                               
AFULTXT  DS    A                                                                
AIOREC   DS    A                                                                
AREADST  DS    A                                                                
APGM1    DS    A                                                                
ATIOB    DS    A                                                                
ATIA     DS    A                                                                
APGMLST  DS    A                                                                
ACTRYTAB DS    A                                                                
PBUFF    DS    A                                                                
PCALLOV  DS    A                                                                
PDATAMGR DS    A                                                                
PDMOD000 DS    A                                                                
PGETTXT  DS    A                                                                
PGETFACT DS    A                                                                
PHEXIN   DS    A                                                                
PHEXOUT  DS    A                                                                
PSELIST  DS    A                                                                
PTICTOC  DS    A                                                                
PVSSB    DS    A                                                                
PWCTYPE  DS    A                                                                
PXSORT   DS    A                                                                
WORK     DS    XL32                                                             
SAVEDSTR DS    XL178                                                            
RCSYSLNM DS    CL7                                                              
PRGNME   DS    CL7                                                              
PSYSNME  DS    CL7                                                              
NAMEX    DS    CL7                                                              
SYNTAB   DS    CL256                                                            
PLNGTH   DS    XL1                                                              
MSGNO    DS    XL2                                                              
HIGHNO   DS    XL1                                                              
SYSCODE  DS    XL1                                                              
PGMLNTH  DS    XL1                                                              
TEXTNO   DS    XL2                                                              
BYTE     DS    X                                                                
TENTNO   DS    X                                                                
DDS      DS    X                                                                
OVSYS    DS    X                                                                
CTRY     DS    X                                                                
LANG     DS    X                                                                
AGY      DS    CL2                                                              
FLAG     DS    X                                                                
PFKEY    DS    X                                                                
FOUND    EQU   X'01'                                                            
FLAGC1   EQU   X'02'                                                            
FLAGT    EQU   X'08'                                                            
RECLEN   DS    H                                                                
TRM      DS    H                                                                
         DS    0F                  ALIGN BXLE PREFIX 2 BYTES AFTER              
         DS    H                   | A FULL WORD BOUNDARY.                      
TABBXLE  DS    0CL6                                                             
         DS    H                                                                
         DS    F                                                                
TABLE    DS    256CL(TENTLN)                                                    
IOAREA   DS    1024C                                                            
WRKX     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TABLE                                                *         
***********************************************************************         
         SPACE 1                                                                
TABLED   DSECT                                                                  
TKEY     DS    CL7                                                              
TPGMID   DS    CL7                                                              
TPGMNO   DS    C                                                                
TPGMPOS  DS    X                                                                
TPGMIND  DS    X                                                                
TPGMIND2 DS    X                                                                
TPGMIND3 DS    X                                                                
TPGMIND4 DS    X                                                                
         DS    X                                                                
TPGMCTY  DS    X                                                                
TPGMTXT  DS    XL2                                                              
TPGMCNT1 DS    XL4                                                              
TPGMCNT2 DS    XL4                                                              
TENTLN   EQU   *-TKEY                                                           
         SPACE 1                                                                
***********************************************************************         
* DSECT TO COVER SAVED STORAGE                                        *         
***********************************************************************         
SAVED    DSECT                                                                  
SYSNME   DS    CL7                 SYSTEM NAME                                  
SERRTXT  DS    CL1                 ERROR INDICATOR                              
STOTAL   DS    F                   TOTAL NO OF LINES DISPLAYED                  
SCURADR  DS    F                                                                
SPFKEY   DS    X                   PREVIOUS KEY HIT                             
SLNAME   DS    CL7                 LAST NAME DISPLAYED ON SCREEN                
SDISPL   DS    CL1                 HELP SCREEN DISPLAYED                        
SHNAME   DS    CL7                 HELP SCREEN PROGRAM NAME                     
STXTID   DS    XL2                 PROGRAM TEXT ID                              
SNAME    DS    CL7                 PROGRAM NAME                                 
SAVEDL   EQU   *-STXTID                                                         
***********************************************************************         
* DSECT TO COVER OUTPUT LINE                                          *         
***********************************************************************         
         SPACE 1                                                                
LINED    DSECT                                                                  
LINNAM   DS    CL7                                                              
         DS    CL1                                                              
LINNUM   DS    CL2                                                              
         DS    CL1                                                              
LININD   DS    CL7                                                              
         SPACE 1                                                                
***********************************************************************         
* DSECT TO COVER OUTPUT LINE TWO - NON DDS                            *         
***********************************************************************         
         SPACE 1                                                                
LNTWOD   DSECT                                                                  
FLHDR    DS    CL8                                                              
LNAME    DS    CL7                                                              
FLHDR2   DS    CL8                                                              
LDESCR   DS    CL69                                                             
         SPACE 1                                                                
***********************************************************************         
* DSECT TO COVER OUTPUT LINE FOR EXTENDED TEXT                        *         
***********************************************************************         
         SPACE 1                                                                
DISPLD   DSECT                                                                  
DSHDR1   DS    CL8                                                              
DSLINE   DS    CL78                                                             
         ORG   DSLINE                                                           
DSLSTAR  DS    CL1                                                              
DSFIL1   DS    CL1                                                              
DSDATA   DS    CL74                                                             
DSFIL2   DS    CL1                                                              
DSRSTAR  DS    CL1                                                              
         ORG                                                                    
DISPLEN  EQU   *-DISPLD                                                         
         SPACE 1                                                                
***********************************************************************         
* FADSECTS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDGETTXT                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFHD                                                                         
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* GEGENMSG                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENMSG                                                       
         PRINT ON                                                               
SRPRGFFD DSECT                                                                  
         DS    CL64                                                             
* SRPRGFFD                                                                      
       ++INCLUDE SRPRGFFD            A VERSION WHILE IN TEST                    
*                                                                               
         ORG     SRVL3H                                                         
* SRPRGFED                                                                      
       ++INCLUDE SRPRGFED            A VERSION WHILE IN TEST                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SRPRG00   02/23/15'                                      
         END                                                                    
