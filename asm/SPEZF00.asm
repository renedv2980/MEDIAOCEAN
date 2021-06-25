*          DATA SET SPEZF00    AT LEVEL 146 AS OF 05/01/18                      
*PHASE T23000A                                                                  
*INCLUDE CLPACK                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE SPEZCOM                                                                
*INCLUDE BINSR31                                                                
*INCLUDE KHDUMMY                                                                
*        TITLE 'T23000 - EASI FILE MAINTENANCE BASE'                            
***********************************************************************         
*                                                                     *         
*  LEV 25    JAN22/90 ALLOW 2 CHAR REQ AND MOVE REP BEFORE RESTORE    *         
*  LEV 26    FEB09/90 CHANGE SYSTEM FROM SPOT TO Z                    *         
*  LEV 27    FEB22/90 ADD INVOICE CONVERT                             *         
*  LEV 28    MAR01/90 SET SPTDIR/SPTFIL FOR SW TO MPL OR SPT          *         
*  LEV 29    MAY17/90 GIVE NOT OP OR NO ACCESS MSG, NOT PROG CK       *         
*  LEV 30    MAY29/90 SW TO CONTROL FILE FOR AGYNAME                  *         
*  LEV 31    AUG15/90 ADD CONVERT REPORT (Z9)                         *         
*  LEV 32    NOV19/90 CHANGE TO TWA0 FOR LESS I/O THANKS TO DAVID H.  *         
*  LEV 33    JUN28/91 ADD BATCH CLOSE 11 SCREEN F6                    *         
*  LEV 34    AUG05/91 ADD OFFNAME TO GENDIR/FIL                       *         
*  LEV 35-36 AUG14/91 ADD CK FOR NETWORK                              *         
*  LEV 37    SEP16/91 SHOW WHICH SYSTEM NOT UP                        *         
*  LEV 38-39 NOV07/91 ADD STATION EQUIV, DELETE INVOICE LIST          *         
*                     CHANGE FASWITCH                                 *         
*  LEV 40    JAN13/92 FIX MPL SWITCH TO POINT BY ADV SYSTEM           *         
*  LEV 41    FEB06/92 ALLOW ALL MEDIAS-VALISTA IF MEDIA=A, FIX MPLSW  *         
*  LEV 42    FEB20/92 FIX EQV STATION EDITING                         *         
*  LEV 43    FEB25/92 FORCE -C AND -S TO -N                           *         
*  LEV 44    MAR11/92 BYPASS MARKET LEVEL SECURITY LOCKOUT            *         
*  LEV 45    MAR31/92 ADD OFFICER SECURITY AND CHANGE VSTA FOR MED A  *         
*  LEV 46-47 JUN09/92 ADD INVOICE GEN - SPOT INVOICE TO EASI FORMAT   *         
*  LEV 48-50 JUL15/92 ADD MPLQ REP                                    *         
*  LEV 51    AUG06/92 ADD OFFLINE CONTROL FILE OPEN                   *         
*  LEV 52    AUG31/92 ALLOW CANADIAN AGENCIES TO SEE NET ON SPOT      *         
*  LEV 53    OCT02/92 ADD RADIO STATION CHECKING                      *         
*  LEV 54-55 OCT05/92 ADD VALIDATE ESTIMATE                           *         
*  LEV 56    JAN04/93 ADD VALIDATE ESTIMATE                           *         
*  LEV 57    JAN31/93 ADD MPL6                                        *         
*  LEV 58    FEB10/93 FIX MEDIA                                       *         
*  LEV 59    APR07/93 ADD CABLE HEAD AND ACTIVE STATION TOTALS PROG   *         
*  LEV 60    APR16/93 MOVE WORKER FILE NAME TO THIS MODULE            *         
*  LEV 61    APR21/93 ADD INVOICE COUNTS                              *         
*  LEV 62    APR28/93 CHANGE TO EASIWK, ADD ACTIVE STATION            *         
*  LEV 63    MAY05/93 RESET STBERR TO ZERO                            *         
*  LEV 64    MAY13/93 MOVE CLEAR EQVSTATB                             *         
*  LEV 65    JUN03/93 ADD BATCH MOVE                                  *         
*  LEV 66    JUN08/93 ADD ZC EOD JCL                                  *         
*  LEV 67    AUG17/93 NEW SCREENS FOR INV COUNT/STA COUNT             *         
*  LEV 68    SEP22/93 FIX CONSEMPL TO DEFAULT 05 - ADV1               *         
*  LEV 69    OCT04/93 ADD EIEXPR BILL/DUMP,ONLY FASW TO CON IF OFFLINE*         
*  LEV 70    MAR22/94 FIX SAVE CONSEMPL                               *         
*  LEV 71    MAY04/94 ADD ADV5                                        *         
*  LEV 72    MAY27/94 ADD BILL RECORDS AND PROGRAM DIRECTORY          *         
*  LEV 73    AUG18/94 ADD NEW NWK WRKFIL AND GFILE CALL               *         
*                     FIX LWORK (AND LSYSD)                           *         
*  LEV 74    JUN02/95 FIX WRKF PUTTING PERIOD IN FOR BLANK            *         
*  LEV 75    MAY18/96 FIX UTL FOR CONTROL OFFLINE                     *         
*                     FORCE COUNTS TO OFFLINE ONLY                    *         
*  LEV 76    MAY22/96 CHANGE USE AIO1, NOT AIO2                       *         
*  LEV 77    AUG01/96 FIX PERIOD IN 3 CHAR STATION                    *         
*  LEV 78    AUG05/96 ADD MOVE LIST (AGAIN)                           *         
*  LEV 79    AUG27/96 MPLV                                            *         
*  LEV 80    SEP05/96 FIX OFFLINE SW, SAVSYS                          *         
*  LEV 81    SEP09/96 ADD BILL CLOSE                                  *         
*  LEV 82    OCT03/96 ADD REP7, ALLOW FOR NO MPL FILES REPB/C         *         
*  LEV 83    OCT15/96 ADD TO MPLSW                                    *         
*  LEV 84    NOV06/96 ADD ADVNAME                                     *         
*  LEV 85    MAR05/97 FIX STATION EQV CHANGE OPTION                   *         
*  LEV 86    SEP12/97 ADD LOW POWER TV (L)                            *         
*  LEV 87    SEP22/97 ALLOW REPB/REPC TO MPL FILES                    *         
*  LEV 88    NOV19/98 ADD BILLING FILE TRACE                          *         
*  LEV 89    JAN18/99 MOVE EQVSTATB & GO FROM 301 TO 451              *         
*  LEV 90    FEB02/99 CHANGE INIT TO CONTROL SYSTEM                   *         
*  LEV 91    NOV01/99 ADD CONVERT LIST SOON                           *         
*  LEV 92    NOV08/99 CHG MPLW TO MPLY FOR REPC                       *         
*  LEV 93    MAR31/00 RESET BAGYMD FOR EQUIV STATIONS                 *         
*  LEV 94    MAY01/00 MOVE STATION EQUIVALENT RECS TO GENDIR/FIL      *         
*  LEV 95    JUN02/00 MADE EQVSTATB LARGER                            *         
*  LEV 96    AUG16/01 MOVED EQVSTATB TO TWA CONHEADH+2500 FOR 1000    *         
*  LEV 96    AUG16/01 MOVED EQVSTATB TO TWA CONHEADH+2500 FOR 1000    *         
*  ...                                                                          
*  LEV 102   SEP03/02 VUSER SYSTEM IS NOW SAVED AND RESTORED          *         
*                     BEFORE SUBROUTINE EXITS                                   
*                                                                     *         
***********************************************************************         
*                                                                               
***********************************************************************         
*                                                                     *         
*  BILL, AGYNAME, OFFNAME, ADVNAME RECORDS ON CONTROL FILE - GENDIR   *         
*  STATION EQUIVALENCY ALSO ON GENDIR/FIL                             *         
*  CLINAME  & PRDNAME ON MPL FILE, BUT NOT USED                       *         
*   ADV1 USES MPL1, ADV2=2, ADV3=3, ADV4=4, ADV5=5 TST=MPL1           *         
*                                                                     *         
***********************************************************************         
*                                                                               
T23000   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,T23000,R7,RR=R2,CLEAR=YES                                
*                                                                               
* SYSPARMS (R1) ARE SET IN A GOTO1 CALL IN FAMONITOR AT MONAP52                 
*        GOTO1 (RF),DMCB,(TAGYB,TRNOUT),TCBTWA,SYSFACS,TCBTIA,COMFACS,          
*              XTRAINFO                                                         
*                                                                               
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         LA    R9,IO                                                            
         AFI   R9,LENIOAS          GRABBING 3 I/O AREAS PLUS LABELS             
         USING SYSD,R9                                                          
         ST    R9,ASYSD                                                         
         L     RA,4(R1)            A(TWA)                                       
         USING T230FFD,RA                                                       
*                                                                               
         ST    R1,SYSPARMS                                                      
         ST    R2,RELO                                                          
         ST    RA,ATWA                                                          
         ST    RB,SYSRB                                                         
*        ST    RD,SYSRD                                                         
         ST    R7,BASER7                                                        
*                                                                               
         BRAS  RE,SYSINIT       <==INITIALIZE SYSTEM DEPENDENT==>               
*                                                                               
         OI    CONSERVH+1,X'01'    SERVISE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
GOGENCON DS    0H                                                               
         CLI   GOAGAIN,C'Y'                                                     
         BNE   *+8                                                              
         MVI   USEIO,C'Y'                                                       
*                                                                               
         MVI   GOAGAIN,C'N'         INITIALIZE RETURN SWITCH                    
*                                                                               
         CLI   TWALREC,X'05'        FOR OFFNAME RECORDS ONLY                    
         BNE   *+12                 =                                           
         OI    GENSTAT5,GENSELVR    =                                           
         MVI   USEIO,C'N'           =                                           
*                                                                               
         CLI   TWALREC,X'0F'        CONVERT/CONLIST ONLY                        
         BE    *+12                 =                                           
         CLI   TWALREC,X'08'        =                                           
         BNE   GOGENC10             =                                           
*                                   =                                           
         CLI   TWALACT,ACTCHA       =                                           
         BNE   *+8                  =                                           
         MVI   USEIO,C'Y'           =                                           
*                                   =                                           
         CLI   TWALACT,ACTDIS       =                                           
         BNE   *+8                  =                                           
         MVI   USEIO,C'Y'           =                                           
*                                                                               
GOGENC10 DS    0H                                                               
*                                                                               
         GOTO1 GENCON,DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)         
*                                                                               
GOGENC20 DS    0H                                                               
         CLI   GOAGAIN,C'Y'        REQUEST BY APPLIC. TO GO BACK                
         BE    GOGENCON                                                         
*                                                                               
         B     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
*                                                                               
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB,LABEL=N                                               
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,BASER7                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     VUSER               VALIDATE USER                                
         B     VMED                         MEDIA                               
         B     VCLI                         CLIENT                              
         B     VPROD                        PRODUCT                             
         B     VSTAT                        STATION                             
         B     VFASWTCH            SWITCH TO M=MEDIA (SPOT/NET)                 
*                                            P=MEDIA PLANNING (MPL)             
*                                            C=CONTROL (GEN)                    
         B     VEQSTA              FIND EQUIVALENT STATIONS                     
         B     VEST                VALIDATE ESTIMATE                            
         B     VINITPFK                                                         
*                                                                               
         B     VFILTSS                                                          
         B     VLKAGYY                                                          
         B     VRDSTA                                                           
         B     VPRSTA                                                           
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
* TEST TO SEE IF FILE NEEDS TO BE SWITCHED CONTROL/SPOT                         
*                                                                               
VFASWTCH CLI   CURSYST,C'M'        SWITCH TO MEDIA SYSTEM                       
         BNE   VFASWT10                                                         
*                                                                               
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         LA    R2,=C'SPT'                                                       
*                                                                               
         CLI   SPOTNETS,C'N'       IS THIS NETWORK                              
         BNE   SW100                                                            
         LA    R2,=C'NET'                                                       
*                                                                               
         B     SW100                                                            
*                                                                               
* USE FASWITCH TO SWITCH TO CONTROL                                             
*                                                                               
VFASWT10 CLI   CURSYST,C'C'        SWITCH TO CONTROL SYSTEM                     
         BNE   VFASWT20                                                         
*                                                                               
         LA    R2,=C'CON'                                                       
         MVC   LKEY,=H'32'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         B     SW100                                                            
*                                                                               
* USE FASWITCH TO SWITCH TO MEDIA PLANNING                                      
*                                                                               
VFASWT20 DC    H'0'                                                             
*                                                                               
SW100    L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         LTR   RF,RF               IF OFFLINE, NOT AVAILABLE OR NEEDED          
         BZ    SW140                                                            
*                                                                               
         GOTO1 (RF),DMCB,(R2),0                                                 
         CLI   4(R1),0             ALL SWITCHES COME HERE                       
         BNE   SWERR                                                            
*                                                                               
SW140    CLI   0(R2),C'C'          DON'T CHANGE FOR CON SYSTEM                  
         BE    SW250                                                            
         CLC   =C'NET',0(R2)                                                    
         BNE   SW200                                                            
         LA    R2,=C'SPT'                                                       
SW200    MVC   SYSDIR(3),0(R2)                                                  
         MVC   SYSFIL(3),0(R2)                                                  
         CLI   OFFLINE,C'Y'                                                     
         BNE   EXIT                                                             
         ICM   RF,15,UTL           MUST BE A UTL ADDRESS                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   4(1,RF),SAVSYS                                                   
         B     EXIT                                                             
*                                                                               
SW250    CLI   OFFLINE,C'Y'                                                     
         BNE   EXIT                                                             
         ICM   RF,15,UTL           MUST BE A UTL ADDRESS                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   4(RF),CONSECTL                                                   
         B     EXIT                                                             
*                                                                               
* GET AGENCY NAME/ADDRESS FROM CONTROL FILE ID RECORD *                         
*                                                                               
*                                                                               
VUSER    DS    0H                                                               
         L     RA,ATWA                                                          
         L     R1,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(,R1)                                        
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         MVI   SVSYSBT,C'M'        INDICATE MEDIA SYSTEM                        
         CLI   FAOVSYS,X'0A'       CONTROL?                                     
         BNE   *+8                                                              
         MVI   SVSYSBT,C'C'                                                     
*                                                                               
         CLI   FAOVSYS,02          SPOT                                         
         BNE   *+8                                                              
         MVI   SPOTNETS,C'S'                                                    
         CLI   FAOVSYS,03          NET                                          
         BNE   *+8                                                              
         MVI   SPOTNETS,C'N'                                                    
         DROP  RE                                                               
*                                                                               
         OC    SVUSER,SVUSER                                                    
         BZ    *+14                YES - READ DATA                              
         MVC   USERNAME(66),SVUSER ELSE MOVED SAVED DATA                        
         B     VUSER10                                                          
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   VUSER06                                                          
*                                                                               
         ZIC   R2,CURSYST                                                       
*                                                                               
         MVI   CURSYST,C'C'        SWITCH TO CONTROL FILE                       
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
VUSER06  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(RA)  FROM TWA                                     
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R4)                    
*                                                                               
         CLI   DMCB+8,0            ANY ERROR                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'        ORIGIN DETAILS                               
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSER(66),USERNAME SAVE FOR FUTURE REF                          
         DROP  R6                                                               
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   VUSER10                                                          
*                                                                               
         LTR   R2,R2                                                            
         BZ    VUSER10                                                          
*                                                                               
         STC   R2,CURSYST                                                       
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
VUSER10  L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         LTR   RF,RF               IF OFFLINE, CK IF MPL FILE OPEN              
         BZ    VUSER14                                                          
         LA    R1,DMCB                                                          
         MVC   0(4,R1),=X'FEFFFFFF'    SYS=FE GETS SYSFAC ADDRESS               
         BASR  RE,RF                                                            
*                                                                               
         L     RE,0(R1)                GET SYSFAC ADDRESS                       
         L     RF,VSSB-SYSFACD(,RE)                                             
         MVC   FACPAKRS,SSBSYSN1-SSBD(R1)                                       
VUSER14  LA    R2,CONRECH                                                       
         CLI   CONREC,C'?'         THIS A HELP REQUEST                          
         BE    VUSER60                                                          
         CLI   CONRECH+5,2         MUST BE 2 POSTION REC ID                     
         BL    RECSIZER                                                         
*                                                                               
         ZIC   RF,CONRECH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,VUSERCLE         THIS STATION EQUIVALENCY                     
         BE    VUSER16                                                          
         EX    RF,VUSERCLC         THIS AGYNAME                                 
         BE    VUSER16                                                          
         EX    RF,VUSERCLG         THIS ADVNAME                                 
         BE    VUSER16                                                          
         EX    RF,VUSERCLD         THIS OFFNAME                                 
         BE    VUSER16                                                          
         EX    RF,VUSERCLM         THIS MOVE LIST                               
         BNE   VUSER60                                                          
*                                                                               
VUSER16  DS    0H                                                               
         MVI   SVSYSBT,X'00'                                                    
         MVC   SYSFIL(3),=C'GEN'                                                
         MVC   SYSDIR(3),=C'GEN'                                                
         LA    R2,=C'CON'                                                       
         MVI   CURSYST,C'C'        INDICATE CONTROL SYSTEM                      
         CLI   OFFLINE,C'Y'                                                     
         BNE   VUSER50                                                          
*                                                                               
* ONLY IF OFFLINE, OPEN CONTROL FILE IF NOT OPEN ALREADY *                      
*                                                                               
         ICM   RF,15,UTL           MUST BE A UTL ADDRESS                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                  OPEN CONTROL                                 
         CLI   CTLOPEN,C'Y'                                                     
         BE    VUSER50                                                          
*                                                                               
         MVI   4(RF),CONSECTL                                                   
         L     R4,AIO1             USE AS WORK AREA FOR OPEN                    
         GOTO1 DATAMGR,DMCB,=C'OPEN',=CL7'CONTROL',FILELST,(R4)                 
         MVI   CTLOPEN,C'Y'                                                     
*                                                                               
VUSER50  CLI   OFFLINE,C'Y'                                                     
         BE    VUSER60                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(R2),0                                                 
         CLI   4(R1),0             WAS SWITCH OK                                
         BNE   SWERR                                                            
*                                                                               
VUSER60  DS   0H                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(2),10(R3)                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GFILE'),=C'WRKFIL',WORK,AIO1,ATIA             
*                                                                               
         LA    R1,WORK                                                          
         USING UKRECD,R1                                                        
         MVC   EASIWK,UKUSRINF                                                  
         DROP  R1                                                               
*                                                                               
         CLI   SVSYSBT,X'00'                                                    
         BE    VUSER80                                                          
*                                                                               
         MVC   CURSYST,SVSYSBT                                                  
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
VUSER80  DS    0H                                                               
         CLI   TWAMODE,0                                                        
         BNE   VUSER90                                                          
*                                                                               
* RUNNING ONLINE HERE                                                           
*                                                                               
         CLI   TWAFIRST,0          ONLINE, FIRST TIME IN?                       
         BNE   EXIT                                                             
*                                                                               
         LARL  R3,WSSVAREA                                                      
         USING FAWSSVRD,R3                                                      
         MVI   FAWSACTN,FAWSUCLR   CLEAR WSSVR BUFFER                           
         GOTOR WSSVR,FAWSSVRD                                                   
         DROP  R3                                                               
*                                                                               
         B     VUSER100                                                         
*                                                                               
* RUNNING OFFLINE HERE                                                          
*                                                                               
VUSER90  DS    0H                                                               
         CLI   FIRSTFL,X'00'                                                    
         BNE   EXIT                                                             
*                                                                               
*        ICM   R0,15,=AL4(XAPASSRL) SAME SIZE AS WSSVR BUFFER                   
         ICM   R0,15,=AL4(BUFFSIZQ)                                             
         STORAGE OBTAIN,LENGTH=(R0),LOC=31,COND=YES                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ASTATAB                                                       
         AHI   R1,4                                                             
         ST    R1,BP2                                                           
*                                                                               
VUSER100 DS    0H                                                               
         BRAS  RE,BLDEQVTB                                                      
*                                                                               
         CLI   TWAMODE,0                                                        
         BNE   *+12                                                             
         MVI   TWAFIRST,1                                                       
         B     EXIT                                                             
         MVI   FIRSTFL,X'01'                                                    
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
VUSERCLC CLC   CONREC(0),AGYNAMED                                               
VUSERCLD CLC   CONREC(0),OFFNAMED                                               
VUSERCLE CLC   CONREC(0),STAEQUIV                                               
VUSERCLG CLC   CONREC(0),ADVNAME                                                
VUSERCLM CLC   CONREC(0),MOVEREC                                                
*                                                                               
* VALIDATE MEDIA CODE *                                                         
*                                                                               
VMED     L     R1,ACOMFACS                                                      
*                                                                               
         L     RF,CGETFACT-COMFACSD(,R1)                                        
*                                                                               
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
*                                                                               
         MVI   QMED,C'T'           SET FOR SPOT                                 
         MVI   SPOTNETS,C'S'                                                    
         CLI   FAOVSYS,02          SPOT                                         
         BE    VMED10                                                           
         CLI   FAOVSYS,03          NET                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   QMED,C'N'           SET FOR NET                                  
         MVI   SPOTNETS,C'N'                                                    
*                                                                               
VMED10   MVI   CURSYST,C'M'        SWITCH TO MEDIA (NET/SPOT)                   
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AGYEL,R6                                                         
         MVI   SPOTCAN,0                                                        
         CLI   AGYPROF+7,C'C'                                                   
         BNE   *+8                                                              
         MVI   SPOTCAN,C'C'                                                     
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'02'                                                     
*                                                                               
VMED20   BAS   RE,NEXTEL                                                        
         BNE   VMEDERR                                                          
         CLC   QMED,2(R6)                                                       
         BNE   VMED20                                                           
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   MEDNM,4(R6)         MEDIA NAME                                   
         MVC   MEDCAPT,14(R6)      AND CAPTION                                  
         B     EXIT                                                             
VMEDERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MEDERRMS),MEDERRMS                                     
         LA    R2,CONRECH                                                       
         DS    0H                                                               
         GOTO1 ERREX2                                                           
MEDERRMS DC    C'* ERROR * ONLY MEDIA T SUPPORTED BY EASI *'                    
         DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
* VALIDATE CLIENT - ON EXIT QCLT AND BCLT CONTAIN VALUES                        
*                                                                               
VCLI     GOTO1 ANY                 CLIENT                                       
         MVC   SVERROPT,ERROPT                                                  
*                                                                               
         MVI   ERROR,INVCLI                                                     
         MVC   QCLT(3),WORK                                                     
         CLI   5(R2),3                                                          
         BH    TRAPERR                                                          
         CLI   5(R2),2                                                          
         BL    TRAPERR                                                          
*                                                                               
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   TRAPERR                                                          
*                                                                               
* READ CLIENT HEADER *                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD & BCLT                                           
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 READ                                                             
         MVC   ERROPT,SVERROPT                                                  
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         MVC   SVAGYM,0(R6)                                                     
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         MVC   ERROPT,SVERROPT                                                  
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
* SAVE CLIENT PRODUCT LIST *                                                    
*                                                                               
         LA    R4,CLIST                                                         
         LA    R5,880                                                           
         LA    RE,SVCLIST                                                       
         LR    RF,R5                                                            
         MVCL  RE,R4                                                            
*                                                                               
         LA    R4,CLIST2                                                        
         LHI   R5,140              LENGTH OF CLIST2                             
         LR    RF,R5                                                            
         CLC   CKEY+13(2),=H'1280' TEST CLIST2 IN RECORD                        
         BH    *+8                 YES                                          
         SR    R5,R5               ELSE JUST CLEAR REST OF LIST                 
         SR    RF,RF                                                            
         MVCL  RE,R4                                                            
*                                                                               
         MVC   CLTNM,CNAME         AND CLIENT NAME                              
         MVC   SVCACCS,CACCESS                                                  
         MVC   SVOFFC,COFFICE                                                   
         MVC   SVCOPT4,COPT4                                                    
*                                                                               
         CLI   T230FFD+6,C'+'          TEST MARKET LOCKOUT                      
         BE    VCLIX                   YES - IGNORE                             
         MVI   SVMACCS,X'FF'                                                    
         OC    T230FFD+6(2),T230FFD+6   OR HAVE LIMIT ACCESS                    
         BZ    VCLIX                                                            
         BRAS  RE,CALLOFCR                                                      
         CLI   ERROR,0                                                          
         BNE   TRAPERR                                                          
*                                                                               
VCLIX    MVI   ERROR,0                                                          
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
* VALIDATE PRD  - ON EXIT WORK(3)    = EBCDIC PRODUCT                           
*                         WORK+3(1)  = PRODUCT CODE                             
*                         WORK+4(20) = PRODUCT NAME                             
*                                                                               
VPROD    OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    MISSCLT                                                          
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 ANY                 PRODUCT                                      
*                                                                               
         MVI   ERROR,INVPROD                                                    
         CLI   5(R2),3                                                          
         BH    VPRDERR                                                          
         CLI   5(R2),2                                                          
         BL    VPRDERR                                                          
         CLC   =C'AAA',WORK                                                     
         BE    TRAPERR                                                          
         LA    RE,SVCLIST                                                       
         LA    R0,255                                                           
*                                                                               
PR100    OC    0(4,RE),0(RE)                                                    
*        BZ    VPRDERR                                                          
         BZ    PR120               PRD DOES NOT HAVE TO BE IN CLIST             
         CLC   0(3,RE),WORK                                                     
         BE    PR110                                                            
         LA    RE,4(RE)                                                         
         BCT   R0,PR100                                                         
*        B     VPRDERR                                                          
         B     PR120                                                            
*                                                                               
PR110    MVC   WORK(4),0(RE)       RETURN EBCDIC/BINARY PRD CODE                
         B     *+12                                                             
*                                                                               
PR120    DS    0H                                                               
         CLI   SPOTNETS,C'N'       IS THIS NETWORK                              
         BNE   VPRDERR                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),WORK                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         MVI   ERROR,NOTFOUND                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VPRDERR                                                          
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         USING PRDHDR,R6                                                        
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         MVC   WORK+4(20),PNAME                                                 
         MVC   WORK+3(1),PCODE+1                                                
         MVI   ERROR,0                                                          
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
VPRDERR  CLI   ERROPT,C'Y'                                                      
         BE    EXIT                                                             
         B     TRAPERR                                                          
*                                                                               
* VALIDATE ESTIMATE                                                             
*                                                                               
VEST     OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    MISSCLT                                                          
*                                                                               
         CLI   5(R2),1                                                          
         BL    ESTSIZER                                                         
         CLI   5(R2),3                                                          
         BH    ESTSIZER                                                         
*                                                                               
         GOTO1 VALINUM                                                          
*                                                                               
         MVC   BEST,ACTUAL                                                      
         ZIC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVC   EKEYAM(3),BAGYMD                                                 
         MVC   EKEYPRD,QPRD                                                     
*                                                                               
         CLI   BPRD,0              WAS PRODUCT ENTERED                          
         BNE   VEST20               YES                                         
*                                                                               
         CLI   SPOTNETS,C'N'                                                    
         BNE   VEST10                                                           
         CLC   QPRD,SPACES                                                      
         BH    VEST20                                                           
*                                                                               
VEST10   DS    0H                                                               
         MVC   EKEYPRD,=C'POL'     USE POL                                      
*                                                                               
VEST20   DS    0H                                                               
         MVC   EKEYEST,BEST                                                     
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ESTCDER                                                          
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
VESTX    B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
***********************************************************************         
* PFKEY INITIALIZATION                                                          
*                                                                               
* ON ENTRY:    PARAM 1             A(PFKEY VAL. TABLE) OR ZEROS                 
***********************************************************************         
VINITPFK DS    0H                                                               
         BRAS  RE,PFKYINIT                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
* VALIDATE STATION CALL LETTERS - ON EXIT QSTA AND STAPRNT ARE SET              
*                                                                               
VSTAT    CLI   5(R2),3                                                          
         BNE   *+14                                                             
         CLC   =C'ALL',8(R2)       IS THIS AN ALL REQUEST                       
         BE    ALLERR                                                           
*                                                                               
         XC    BLOCK(64),BLOCK                                                  
         LA    R4,BLOCK                                                         
         USING STABLKD,R4                                                       
         ST    R2,STBADDR                                                       
*                                                                               
         ZIC   R6,QMED             SAVE QMED                                    
*                                                                               
         CLI   QMED,C'A'           ACCEPT ALL MEDIA                             
         BE    VSTAT02                                                          
*                                                                               
         CLI   SPOTCAN,C'C'        IF CANADA, ALLOW ALL                         
         BNE   VSTAT04                                                          
*                                                                               
VSTAT02  LA    R3,MEDTBL           ALLOW ALL MEDIAS - T, R, X, N                
         LA    R5,4                                                             
         B     VSTAT10                                                          
VSTAT04  CLI   SPOTNETS,C'S'                                                    
         BNE   VSTAT06                                                          
         LA    R3,MEDTBL           ONLY ALLOW T, R, X                           
         LA    R5,3                                                             
         B     VSTAT10                                                          
VSTAT06  CLI   SPOTNETS,C'N'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MEDTBL+3         ONLY ALLOW N                                 
         LA    R5,1                                                             
*                                                                               
VSTAT10  MVC   STBMED,0(R3)                                                     
         MVC   QMED,0(R3)                                                       
*                                                                               
         MVI   STBCTRY,C'U'                                                     
*                                                                               
         CLI   SPOTCAN,C'C'        IF CANADA, PASS ON                           
         BNE   *+8                                                              
         MVI   STBCTRY,C'C'                                                     
*                                                                               
         MVC   STBACOM,ACOMFACS                                                 
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QSTAVAL                                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)            PICK UP A(STAVAL)                            
*                                                                               
         GOTO1 (RF),DMCB,(R4)      GO TO STAVAL                                 
*                                                                               
         CLI   STBERR,0            NO ERROR                                     
         BE    VSTAT20                                                          
*                                                                               
         MVI   STBERR,0            RESET                                        
         LA    R3,1(,R3)           TRY NEXT MEDIA                               
         BCT   R5,VSTAT10                                                       
*                                                                               
         B     BDSTAERR                                                         
*                                                                               
VSTAT20  MVC   QNET,STBNET                                                      
*                                                                               
         MVC   QSTA(5),STBSTA      SAVE CALL LETTERS                            
         DROP  R4                                                               
*                                                                               
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
         NI    BAGYMD,X'F0'                                                     
*                                                                               
         LA    R1,QMED                                                          
         ICM   R1,8,=AL1(EZMTMEDQ)                                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    BAGYMD,EZMTHEX-EZMEDTBD(RF)                                      
*                                                                               
         STC   R6,BYTE                                                          
         CLI   BYTE,C'A'           WAS QMED A                                   
         BNE   EXIT                                                             
         MVI   QMED,C'A'                                                        
         B     EXIT                                                             
*                                                                               
MEDTBL   DC    C'TRXN'                                                          
*                                                                               
*                                                                               
*                                                                               
* FIND EQUIVALENT STATION (IF ANY) FIRST TIME THRU, BUILD TABLE *               
*                                                                               
VEQSTA   DS   0H                                                                
         CLC   SRCESTA,SPACES      FAKE CALL JUST TO POPULATE TABLE             
         JNH   EXIT                                                             
*                                                                               
*NUMSTAQ  EQU   (XAPASSRL-WSLNQ)/10                                             
NUMSTAQ  EQU   (BUFFSIZQ-WSLNQ)/10                                              
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         JE    VEQSTA05                                                         
*                                                                               
* SET UP WSSVR PARAMETERS                                                       
* SEE IF TABLE ALREADY IN WSSVR                                                 
*                                                                               
         LARL  R3,WSSVAREA                                                      
         USING FAWSSVRD,R3                                                      
         XC    FAWSSVRD(FAWSSVRL),FAWSSVRD                                      
         MVC   FAWSTOKN,=C'EQST'   TOKEN                                        
         MVC   FAWSADR,ASTATAB     A(XA9 BUFFER)                                
         MVI   FAWSACTN,FAWSURST   DEFAULT ACTION = RESTORE                     
         GOTO1 WSSVR,FAWSSVRD                                                   
         CLI   FAWSRTN,0           RECORD FOUND OK?                             
         JE    VEQSTA05                                                         
         CLI   FAWSRTN,FAWSRNF     RECORD NOT FOUND?                            
         JNE   *+2                 NOT IN WSSVR - POPULATE TABLE & SAVE         
         DROP  R3                                                               
*                                                                               
* NOW LOOK UP STATION IN TABLE, IF FIND, PUT REPLACEMENT IN RSTA                
VEQSTA05 DS    0H                                                               
         CLI   SRCESTA+4,C'C'                                                   
         BNE   *+8                                                              
         MVI   SRCESTA+4,C'N'                                                   
*                                                                               
         CLI   SRCESTA+3,C'.'                                                   
         BNE   *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
*                                                                               
         MVC   EQUISTA,SRCESTA                                                  
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(5),SRCESTA                                                  
         MVI   BP4,X'00'           READ THE RECORD                              
         L     RF,ASTATAB                                                       
         SAM31                                                                  
         L     RF,0(RF)                                                         
         ST    RF,BP3                                                           
         GOTO1 VBINSR31,BSPARS,WORK                                             
         SAM24                                                                  
         CLI   BP1,X'80'                                                        
         JE    VEQSTA10                                                         
         L     RF,BP1              A(WHERE RECORD IS)                           
*                                                                               
         SAM31                                                                  
         CLC   WORK(5),0(RF)       FOUND THE STATION?                           
         JNE   *+10                                                             
         MVC   EQUISTA,5(RF)                                                    
         SAM24                                                                  
*                                                                               
VEQSTA10 DS    0H                                                               
*        LA    R1,SRCESTA+4                                                     
*        ICM   R1,8,=AL1(EZMTBNDQ)                                              
*        GOTO1 VGETMED                                                          
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        MVC   SRCESTA+4(2),EZMTPMED-EZMEDTBD(RF)                               
*                                                                               
         GOTO1 VPRTSTA,DMCB,EQUISTA,PRTSTA7C                                    
*                                                                               
         LA    R1,EQUISTA+4                                                     
         ICM   R1,8,=AL1(EZMTBNDQ)                                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R2,RF               SAVE A(TABLE ENTRY)                          
*                                                                               
         MVC   EQUISTA+4(2),EZMTPMED-EZMEDTBD(R2)                               
*                                                                               
         MVC   EQUISTA5,EQUISTA                                                 
         MVC   EQUISTA5+4(1),EZMTBAND-EZMEDTBD(R2)                              
*                                                                               
         MVC   EQVMED,EZMTMED-EZMEDTBD(R2)                                      
         MVC   EQVSYS,EZMTSYS-EZMEDTBD(R2)                                      
         NI    BAGYMD,X'F0'                                                     
         OC    BAGYMD,EZMTHEX-EZMEDTBD(R2)                                      
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
VFILTSS  DS    0H                                                               
         CLI   SPOTCAN,C'C'                                                     
         BE    EQXIT                                                            
         CLC   SPOTNETS,EQVSYS                                                  
         BE    EQXIT                                                            
         B     NEQXIT                                                           
*                                                                               
*                                                                               
*                                                                               
VLKAGYY  DS    0H                                                               
         LR    R2,R1                                                            
         USING LKAGYD,R2                                                        
*                                                                               
* IF BOTH BINARY AND CHAR ID ARE PASSED, BINARY IS USED FOR LOOKUP              
*                                                                               
         CLC   LKAGYUID,=CL10' '   10-CHAR ID PASSED?                           
         BH    *+14                                                             
         OC    LKAGYBID,LKAGYBID   BINARY ID PASSED?                            
         BZ    NEQXIT                                                           
*                                                                               
         OC    LKAGYUID,=CL10' '                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CTIKEY,R6                                                        
         MVI   CTIKTYP,C'I'                                                     
*                                                                               
         MVC   CTIKID+8(2),LKAGYBID                                             
         OC    LKAGYBID,LKAGYBID                                                
         BNZ   *+10                                                             
         MVC   CTIKID,LKAGYUID                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,AIO3,WORK                 
         CLI   8(R1),0                                                          
         BNE   NEQXIT                                                           
*                                                                               
         L     R6,AIO3                                                          
         LA    R6,CTIDATA-CTIREC(R6)                                            
*                                                                               
LKAGY30  DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    LKAGY100                                                         
*                                                                               
         CLI   0(R6),CTDSCELQ      DESCRIPTION ELEMENT?                         
         BNE   LKAGY35                                                          
*                                                                               
* DESCRIPTION ELEMENT HERE                                                      
*                                                                               
         USING CTDSCD,R6                                                        
         CLI   CTDSCLEN,X'04'      BINARY UID IN DESC ELEM?                     
         BNE   *+14                                                             
         MVC   LKAGYBID,2(R6)                                                   
         B     LKAGY40             GET NEXT ELEMENT                             
*                                                                               
         CLI   1(R6),X'0C'         CHAR UID IN DESC ELEM?                       
         BNE   NEQXIT              SOMETHING ELSE - RETURN ERROR                
         MVC   LKAGYUID,2(R6)                                                   
         OC    LKAGYUID,=CL10' '                                                
         B     LKAGY40             GET NEXT ELEMENT                             
*                                                                               
LKAGY35  DS    0H                                                               
         CLI   0(R6),CTAGYELQ      AGY ID ELEM                                  
         BNE   LKAGY40                                                          
         MVC   LKAGYAGY,2(R6)                                                   
*                                                                               
LKAGY40  DS    0H                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LKAGY30                                                          
*                                                                               
* EOR - MAKE SURE ALL VALUES WERE LOOKED UP CORRECTLY                           
*                                                                               
LKAGY100 DS    0H                                                               
         OC    LKAGYBID,LKAGYBID                                                
         BZ    NEQXIT                                                           
         OC    LKAGYAGY,LKAGYAGY                                                
         BZ    NEQXIT                                                           
         CLC   LKAGYUID,=CL10' '                                                
         BNH   NEQXIT                                                           
*                                                                               
* LOOK UP SYSTEM ACCESS RECORD TO GET COUNTRY                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CT5REC,R6                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,LKAGYAGY                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,AIO3,WORK                 
         CLI   8(R1),0                                                          
         BNE   NEQXIT                                                           
*                                                                               
         L     R6,AIO3                                                          
         LA    R6,CT5DATA-CT5REC(R6)                                            
*                                                                               
LKAGY130 DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    LKAGY150                                                         
*                                                                               
         CLI   0(R6),CTAGDELQ                                                   
         BNE   LKAGY140                                                         
*                                                                               
         USING CTAGDD,R6                                                        
         MVI   LKAGYCTR,C'U'                                                    
         TM    CTAGDCTY,X'08'                                                   
         BZ    LKAGY150                                                         
         MVI   LKAGYCTR,C'C'                                                    
         B     LKAGY150                                                         
*                                                                               
LKAGY140 DS    0H                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LKAGY130                                                         
*                                                                               
LKAGY150 DS    0H                                                               
         OC    LKAGYCTR,LKAGYCTR                                                
         BZ    NEQXIT                                                           
         B     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
* R2 - INPUT FIELD HEADER                                                       
* ON RETURN - QSTA POPULATED WITH 5-CHAR STATION                                
* I.E. WXY-FM -> WXY F                                                          
* IF INPUT IS GARBAGE - CC UNEQUAL IS SET                                       
* MEDIA/BAND WILL BE VALIDATED                                                  
* CALL LETTERS ARE ACCEPTED W/O VALIDATION                                      
VRDSTA   LA    R1,FLDRDSTA         OUTPUT FIELD                                 
         XC    FLDRDSTA,FLDRDSTA   CLEAR PREVIOUS VALUE                         
*                                                                               
         LA    R4,8(R2)            A(INPUT)                                     
         LR    R3,R1               A(OUTPUT)                                    
*                                                                               
         LLC   R0,5(R2)            INPUT LENGTH                                 
         LHI   RF,5                FLDRDSTA CHARACTER COUNTER                   
*                                                                               
VRDSTA10 CLI   0(R4),C' '                                                       
         BE    VRDSTA30                                                         
         CLI   0(R4),C'-'                                                       
         BE    VRDSTA30                                                         
*                                                                               
         MVC   0(1,R3),0(R4)       COPY CHAR FROM SCREEN TO FLDRDSTA            
         LA    R3,1(R3)            FLDRDSTA POINTER                             
         LA    R4,1(R4)            SCREEN POINTER                               
         BCT   R0,*+8              DEC REMAINING INPUT LENGTH                   
         B     VRDSTAX             END OF INPUT HERE - CHECK MEDIA              
         BCT   RF,VRDSTA10         DEC FLDRDSTA COUNTER, GET NXT CHAR           
*                                                                               
* FLDRDSTA FULL HERE                                                            
         LTR   R0,R0               ANY CHARACTERS LEFT ON SCREEN?               
         BZ    VRDSTAX             NO - VALIDATE INPUT                          
         B     VRDSTNQX            YES - INPUT IS INVALID                       
*                                                                               
* SPACE OR DASH HERE                                                            
* MUST BE FOLLOWED BY 1- OR 2-CHARACTER BAND                                    
VRDSTA30 LA    R4,1(R4)            SKIP DASH/SPACE                              
         BCT   R0,*+8              DECREMENT INPUT COUNTER                      
         B     VRDSTNQX            NOTHING AFTER DASH: INPUT INVALID            
*                                                                               
* HAVE CHARACTERS AFTER DASH/SPACE HERE.  PRESUME IT IS MEDIA/BAND              
         CHI   R0,1                CHARACTERS LEFT TO READ                      
         BNE   *+14                                                             
         MVC   FLDRDSTA+4(1),0(R4)     1 CHAR LEFT, JUST COPY IT                
         B     VRDSTAX                                                          
*                                                                               
         CHI   R0,2                                                             
         BNE   VRDSTNQX            MORE THAN 2 - INVALID INPUT                  
         LR    R1,R4                                                            
         ICM   R1,8,=AL1(EZMTPMDQ) SEARCH FOR PRINTABLE MEDIA                   
         GOTO1 VGETMED                                                          
         BNE   VRDSTNQX            PRINTABLE MEDIA/BAND NOT FOUND               
         MVC   FLDRDSTA+4(1),EZMTBAND-EZMEDTBD(RF) BAND FROM EZMEDTAB           
         OC    FLDRDSTA,SPACES                                                  
         B     EQXIT                                                            
*                                                                               
* CHECK IF BAND HAS BEEN POPULATED                                              
VRDSTAX  OC    FLDRDSTA,SPACES                                                  
         CLI   FLDRDSTA+4,C' '     BAND BYTE PRESENT?                           
         BH    VRDSTAX5            YES - MAKE SURE IT IS VALID                  
*                                                                               
         MVI   FLDRDSTA+4,C'T'     NO, DEFAULT MEDIA = TV                       
         CLI   SPOTNETS,C'N'       IS THIS NETWORK                              
         BNE   EQXIT                                                            
         MVI   FLDRDSTA+4,C'N'     YES, DEFAULT MEDIA = N                       
         B     EQXIT                                                            
*                                                                               
VRDSTAX5 LA    R1,FLDRDSTA+4                                                    
         ICM   R1,8,=AL1(EZMTBNDQ)                                              
         GOTO1 VGETMED             MAKE SURE BAND IS VALID                      
         BE    EQXIT                                                            
*                                                                               
VRDSTNQX XC    FLDRDSTA,FLDRDSTA                                                
         B     NEQXIT                                                           
*                                                                               
*                                                                               
*                                                                               
* R1 expected to address 5-char station field (e.g. WABCA)                      
* PRTSTA7C field will be populated                                              
VPRSTA   LR    RF,R1                                                            
         GOTO1 VPRTSTA,DMCB,0(RF),PRTSTA7C                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
* WASTED SPACE                                                                  
WS       EQU   4096-*                                                           
*                                                                               
*                                                                               
**********************************************************************          
* STUFF BELOW WILL BE ADDRESSED BY 2ND BASE REGISTER - R7                       
* ERROR MESSAGES, CONSTANTS, LTORG, ETC                                         
**********************************************************************          
WSLNQ    EQU   12                                                               
*                                                                               
         ORG   T23000+4096                                                      
*                                                                               
SWERR    CLI   4(R1),1             TEST USER NOT AUTHORIZED FOR SYSTEM          
         BE    SWERR10                                                          
         CLI   4(R1),2             TEST SYSTEM NOT OP                           
         BE    SWERR20                                                          
         DC    H'0'                                                             
SWERR10  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'USER NOT AUTHORIZED FOR SYSTEM'                   
         MVC   CONHEAD+31(3),0(R2)                                              
         B     SWERR30                                                          
*                                                                               
SWERR20  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(3),0(R2)                                                 
         MVC   CONHEAD+5(13),=C'SYSTEM NOT UP'                                  
         CLC   DUB(3),0(R2)        IF MPL, SHOW WHICH ONE                       
         BNE   SWERR30                                                          
         MVC   CONHEAD+3(1),DUB+3                                               
SWERR30  MVI   GENSTAT2,USMYOK                                                  
         B     ERREXIT2                                                         
MISSCLT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MISSCLTM),MISSCLTM                                     
         B     ERREXIT2                                                         
ESTSIZER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ESTSIZMS),ESTSIZMS                                     
         B     ERREXIT2                                                         
ESTCDER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ESTCDMS),ESTCDMS                                       
         B     ERREXIT2                                                         
RADIOERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'RADIOMS),RADIOMS                                       
         B     ERREXIT2                                                         
ALLERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ALLMS),ALLMS                                           
         B     ERREXIT2                                                         
RECSIZER L     RA,ATWA                                                          
         USING T230FFD,RA                                                       
         MVC   CONHEAD,RECSIZMS                                                 
ERREXIT2 GOTO1 ERREX2                                                           
*                                                                               
BDSTAERR MVI   ERROR,NOSTAFND                                                   
*                                                                               
TRAPERR  CLI   ERROPT,C'Y'                                                      
         BE    EXIT                                                             
         GOTO1 ERREX                                                            
*                                                                               
         DC    H'0'                                                             
*                                                                               
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
*                                                                               
* CONSTANTS TABLES, ETC *                                                       
*                                                                               
GENCON   DS    V                                                                
WSSVR    DS    V                                                                
ASTATAB  DS    A                                                                
*                                                                               
FIRSTFL  DC    X'00'                                                            
SVSYSBT  DC    X'00'                                                            
SVERROPT DS    XL1                 SAVED ERROPT                                 
SVAGYM   DS    XL1                 SAVED AGENCY/MEDIA BYTE                      
SVCACCS  DS    CL3                 CLIENT LIMIT ACCESS CODES                    
SVMACCS  DS    CL3                 MARKET LIMIT ACCESS CODES                    
SVOFFC   DS    CL1                 1 CHAR OFFICE CODE                           
SVOFFC2  DS    CL2                 2 CHAR OFFICE CODE                           
*                                                                               
         DS    0F                                                               
WSSVAREA DS    XL(FAWSSVRL)        ** WSSVR CONTROL BLOCK **                    
*                                                                               
         DS    0F                                                               
BSPARS   DS    0XL32               BINSEARCH PARAMETERS                         
BP1      DS    F                                                                
BP2      DS    F                                                                
BP3      DS    F                                                                
BP4      DS    F                                                                
BP5      DS    F                                                                
BP6      DS    F                                                                
BP7      DS    F                                                                
BP8      DS    F                                                                
*                                                                               
RECSIZMS DC    C'* ERROR * ENTER AT LEAST 3 CHAR OF RECORD TYPE *'              
MISSCLTM DC    C'* ERROR * MUST ENTER CLIENT TO ENTER ESTIMATE *'               
ESTSIZMS DC    C'* ERROR * ESTIMATE MUST BE 1-3 DIGITS VALUE 1-255 *'           
ESTCDMS  DC    C'* ERROR * ESTIMATE NOT FOUND *'                                
RADIOMS  DC    C'* ERROR * ENTER A OR F FOR A RADIO STATION *'                  
ALLMS    DC    C'* ERROR * TO GET ALL STATIONS, LEAVE BLANK *'                  
*                                                                               
FILELST  DS    0D                                                               
         DC    CL8'UGENDIR'                                                     
         DC    CL8'UGENFIL'                                                     
         DC    CL10'X'                                                          
CTLOPEN  DC    X'00'                                                            
SAVSYS   DC    X'00'                                                            
*                                                                               
CONSECTL EQU   X'0A'                                                            
*                                                                               
*                                                                               
*                                                                               
**********************************************************************          
* RECACT TABLE                                                                  
**********************************************************************          
* REPORT - PROGRAM(S) USING           15 USED  21 FREE 24 DUPES                 
* CODE                                                                          
*                                                                               
*  ZB      EASI BILLING - NO LONGER USED - STD CONTRACT AMT                     
*  ZC      INVOICE COUNTS BY STATION                                            
*  ZD      DUMP INOICE(S) FROM WORKER FILE TO SEQUENTIAL DATA SET               
*  ZI      INVOICE COUNTS BY AGENCY                                             
*  ZM      LIST OF INVOICES MOVED - BATCH MOVE PROGRAM                          
*  ZV      CONVERT LIST SOON                                                    
*  Z8      INVOICE GENERATE REPORT - GENERATE SEQ FLAT FILE FROM AN             
*          INTO EZLOAD - USED TO TRANSFER INVOICES BETWEEN AGENCIES             
*  Z9      INVOICE FACSIMILE REPORT                                             
*                                                                               
* DIRECTORY OF PROGRAMS                                                         
*                                                                               
*        PROGRAM          ACTION            PROG SCRN                           
*                                                                               
*        AGYNAME          MAINT              01   FE                            
*                         LIST               01   EE                            
*        ADVNAME          MAINT              0A   F7                            
*                         LIST               0A   EF                            
*        CLINAME          MAINT              02   FD                            
*                         LIST               02   ED                            
*        PRDNAME          MAINT              03   FC                            
*                         LIST               03   EC                            
*        OFFNAME          MAINT              04   FB                            
*                         LIST               04   EB                            
*        BATCH            MAINT              05   FA                            
*                         LIST               05   EA                            
*        INVOICE (DEFUNCT)MAINT              06   F9                            
*                         LIST               06   E9                            
*        CONVERT          MAINT              07   F8                            
*                         LIST               07   E8                            
*        ?????            MAINT              08                                 
*                         LIST               08                                 
*        CONVERT          REPORT             09   E6                            
*        BATCH            CLOSE              11   F6                            
*        STATION (EQUIV)  MAINT              12   F5                            
*                         LIST               12   E5                            
*        INVOICE          COUNTS             13   E4                            
*        STATION          COUNTS             14   E4                            
*        BATCH            MOVE               15   E7                            
*        EIXPR            BILLING            16   E3                            
*        BATCH            DUMP               17   E2                            
*        INVOICE          GENERATE           21   B1                            
*                                            22                                 
*        BILL             MAINT              23   B3                            
*                         LIST               23   A3                            
*                                                                               
*        FREE PROGRAMS    0A-0F 10 18-1F  23-2F                                 
*        FREE SCREENS     F7 F4 F3 F2 B2-BF A1-AF                               
*                                                                               
*              DIRECTORY OF RECORDS AND ACTIONS                                 
*                                                                               
RECACT   DS    0D                                                               
*                                                                               
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
*        GENSTAT7 = GES7DDS        XL1 STATUS BYTE  X'80'=DDS ONLY              
*                                                                               
AGYNAMED EQU   *+1                                                              
         DC    X'01',C'AGYNAME ',AL1(02),X'00C2',X'80'                          
*CLINAME  EQU   *+1                                                             
*         DC    X'01',C'CLINAME ',AL1(03),X'00C2',X'00'                         
*PRDNAME  EQU   *+1                                                             
*         DC    X'01',C'PRDNAME ',AL1(04),X'00C2',X'00'                         
OFFNAMED EQU   *+1                                                              
         DC    X'01',C'OFFNAME ',AL1(05),X'00C2',X'80'                          
         DC    X'01',C'BATCH   ',AL1(06),X'00C3',X'00'                          
         DC    X'01',C'INVOICE ',AL1(07),X'00C4',X'00'                          
         DC    X'01',C'CONVERT ',AL1(08),X'00C4',X'00'                          
STAEQUIV EQU   *+1                                                              
         DC    X'01',C'STATION ',AL1(09),X'00C2',X'00'                          
*        DC    X'01',C'EIEXPR  ',AL1(10),X'00C2',X'00'                          
*BILLREC  EQU   *+1                                                             
*         DC    X'01',C'BILL    ',AL1(11),X'00C2',X'00'                         
*         DC    X'01',C'MMPLUS  ',AL1(12),X'00C2',X'00'                         
MOVEREC  EQU   *+1                                                              
         DC    X'01',C'MOVE    ',AL1(13),X'00C2',X'80'                          
ADVNAME  EQU   *+1                                                              
         DC    X'01',C'ADVNAME ',AL1(14),X'00C2',X'00'                          
         DC    X'01',C'CONLIST ',AL1(15),X'00C4',X'00'                          
         DC    X'01',C'CPE     ',AL1(16),X'00C2',X'00'                          
         DC    X'01',C'BUY     ',AL1(17),X'00C2',X'00'                          
         DC    X'01',C'CONSUM  ',AL1(18),X'00C4',X'00'                          
         DC    X'01',C'INVSUM  ',AL1(19),X'00C4',X'00'                          
*        DC    X'01',C'HELP    ',AL1(00),X'00CF',X'00'                          
*                                                                               
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                  CL1 SPARE                                    
*        GENSTAT7 = GES7DDS        XL1 STATUS BYTE  X'80'=DDS ONLY              
*                                                                               
* 09/02/08 ACTEQU = ACTNUM                                                      
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00),X'00'                            
         DC    X'02',C'CHANGE  ',AL1(02,02,00),X'00'                            
         DC    X'02',C'DISPLAY ',AL1(03,03,00),X'00'                            
         DC    X'02',C'DELETE  ',AL1(04,04,00),X'00'                            
         DC    X'02',C'SELECT  ',AL1(05,05,00),X'00'                            
         DC    X'02',C'REPORT  ',AL1(12,12,00),X'00'                            
         DC    X'02',C'RESTORE ',AL1(06,06,00),X'00'                            
         DC    X'02',C'CLOSE   ',AL1(11,11,00),X'00'                            
         DC    X'02',C'LIST    ',AL1(10,10,00),X'00'                            
         DC    X'02',C'HELP    ',AL1(00,00,00),X'00'                            
         DC    X'02',C'GENERATE',AL1(13,13,00),X'00'                            
         DC    X'02',C'COUNTS  ',AL1(14,14,00),X'00'                            
         DC    X'02',C'MOVE    ',AL1(15,15,00),X'00'                            
         DC    X'02',C'BILL    ',AL1(16,16,00),X'00'                            
         DC    X'02',C'DUMP    ',AL1(17,17,00),X'00'                            
         DC    X'02',C'TRACE   ',AL1(18,18,00),X'00'                            
*                                                                               
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
*                                                                               
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                      01=USER MAINTENANCE                      
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
*        GENSTAT7 = GES7DDS        XL1 STATUS BYTE  X'80'=DDS ONLY              
*                                                                               
*                                                      AGYNAME RECORDS          
         DC    X'03',AL1(02,01),X'FE01000080',C'    ',X'80'     ADD             
         DC    X'03',AL1(02,02),X'FE01000080',C'    ',X'80'     CHANGE          
         DC    X'03',AL1(02,03),X'FE01000080',C'    ',X'80'     DISPLAY         
         DC    X'03',AL1(02,04),X'FE01000080',C'    ',X'80'     DELETE          
         DC    X'03',AL1(02,05),X'FE01000080',C'    ',X'80'     SELECT          
         DC    X'03',AL1(02,06),X'FE01000080',C'    ',X'80'     RESTORE         
         DC    X'03',AL1(02,10),X'EE010000C0',C'    ',X'80'     LIST            
*                                                      OFFNAME RECORDS          
         DC    X'03',AL1(05,01),X'FB04000080',C'    ',X'80'     ADD             
         DC    X'03',AL1(05,02),X'FB04000080',C'    ',X'80'     CHANGE          
         DC    X'03',AL1(05,03),X'FB04000080',C'    ',X'80'     DISPLAY         
         DC    X'03',AL1(05,04),X'FB04000080',C'    ',X'80'     DELETE          
         DC    X'03',AL1(05,05),X'FB04000080',C'    ',X'80'     SELECT          
         DC    X'03',AL1(05,06),X'FB04000080',C'    ',X'80'     RESTORE         
         DC    X'03',AL1(05,10),X'EB040000C0',C'    ',X'80'     LIST            
*                                                      ADVNAME RECORDS          
         DC    X'03',AL1(14,01),X'F70A000080',C'    ',X'00'     ADD             
         DC    X'03',AL1(14,02),X'F70A000080',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(14,03),X'F70A000080',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(14,04),X'F70A000080',C'    ',X'00'     DELETE          
         DC    X'03',AL1(14,05),X'F70A000080',C'    ',X'00'     SELECT          
         DC    X'03',AL1(14,06),X'F70A000080',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(14,10),X'EF0A0000C0',C'    ',X'00'     LIST            
*                                                      BATCH RECORDS            
         DC    X'03',AL1(06,05),X'FA05000080',C'    ',X'00'     SELECT          
         DC    X'03',AL1(06,10),X'EA050000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(06,15),X'E715000080',C'    ',X'00'     MOVE            
         DC    X'03',AL1(06,11),X'F611000058',C'    ',X'80'     CLOSE           
         DC    X'03',AL1(06,17),X'E217001718',C'ZDZD',X'80'     DUMP            
*                                                      INVOICE RECORDS          
*        DC    X'03',AL1(07,13),X'B121002118',C'Z8Z8',X'80'     GENER           
*        DC    X'03',AL1(07,14),X'E413001318',C'ZIZI',X'80'     COUNTS          
         DC    X'03',AL1(07,12),X'C550005078',C'ZZZZ',X'80'     REPORT          
*                                                      CONVERT RECORDS          
         DC    X'03',AL1(08,10),X'E8070000E0',C'ZVZV',X'00'     LIST            
         DC    X'03',AL1(08,02),X'F807000080',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(08,03),X'F807000080',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(08,05),X'F807000080',C'    ',X'00'     SELECT          
         DC    X'03',AL1(08,12),X'E609000978',C'Z9Z9',X'00'     REPORT          
*                                                      STATION RECORDS          
         DC    X'03',AL1(09,01),X'F512000080',C'    ',X'00'     ADD             
         DC    X'03',AL1(09,02),X'F512000080',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(09,03),X'F512000080',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(09,04),X'F512000080',C'    ',X'00'     DELETE          
         DC    X'03',AL1(09,05),X'F512000080',C'    ',X'00'     SELECT          
         DC    X'03',AL1(09,06),X'F512000080',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(09,10),X'E5120000C0',C'    ',X'00'     LIST            
*        DC    X'03',AL1(09,14),X'E414001418',C'ZCZC',X'80'     COUNTS          
*                                                      MOVE RECORDS             
         DC    X'03',AL1(13,10),X'A4240024D8',C'ZMZM',X'00'     LIST            
         DC    X'03',AL1(13,03),X'B424000080',C'    ',X'00'     DISP            
         DC    X'03',AL1(13,05),X'B424000080',C'    ',X'00'     SELECT          
*                                                      CONLIST RECORDS          
         DC    X'03',AL1(15,10),X'E1270000E0',C'ZVZV',X'00'     LIST            
         DC    X'03',AL1(15,02),X'F807000080',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(15,03),X'F807000080',C'    ',X'00'     DISPLAY         
*                                                      CONSUM   RECORDS         
         DC    X'03',AL1(18,10),X'DE280000E0',C'    ',X'00'     LIST            
         DC    X'03',AL1(18,02),X'F807000080',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(18,03),X'F807000080',C'    ',X'00'     DISPLAY         
*                                                      CPE RECORDS              
         DC    X'03',AL1(16,12),X'A220002038',C'ZFZF',X'00'     REPORT          
*                                                      BUY RECORDS              
         DC    X'03',AL1(17,10),X'F425000080',C'    ',X'00'     LIST            
*                                                      CPE RECORDS              
         DC    X'03',AL1(19,12),X'A535003538',C'ISIS',X'00'     REPORT          
*                                                      INVSUM REPORT            
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
$$LITER  LOCTR                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
$$CODE   LOCTR                                                                  
***********************************************************************         
* PFKEY INITIALIZATION                                                          
* ON ENTRY     PARAM 1             HAD BETTER HAVE A(GEND) !!!                  
* ON ENTRY:    PARAM 2             A(PFKEY VAL. TABLE) OR ZEROS                 
* WALTER -- TAKE NOTE  --                                                       
* BRANCHING TO ADDRESSES COVERED BY BASER7 IS HAZARDOUS !!!                     
***********************************************************************         
PFKYINIT NTR1  BASE=*,LABEL=*                                                   
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         USING T230FFD,RA                                                       
*                                                                               
         ICM   R3,7,1(R1)          IF PFKEY VALIDATION TABLE PASSED             
         BZ    INIT10                                                           
         BAS   RE,TESTSEL          TEST FOR SPECIAL SELECT CODE                 
         GOTO1 PFVAL,DMCB,(R3)     HANDLE LOCAL PFKEY PRESENCE                  
         BNE   INITX               TAKE DUMMY ERROR EXIT FOR GOAGAIN            
* THIS CODE REPLICATED FROM UNREACHABLE INSTRUCTION IN DUMMYERR                 
         MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
         L     RA,ATWA             CURSOR TO RECORD FIELD                       
         USING T230FFD,RA                                                       
         LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         GOTO1 ERREX2                                                           
*                                                                               
INIT10   DS    0H                                                               
INITX    B     XIT2                                                             
*                                                                               
*              LOCAL ROUTINE TO HANDLE PFKEY PRESENCE                           
*                                                                               
*                                  P1  BYTES 1-3 = A(PFKEY VAL. TABLE)          
PFVAL    NTR1                                                                   
         CLI   PFKEY,0             USER HIT ENTER?                              
         BE    NO2                 YES                                          
*                                                                               
         L     RF,0(R1)            RF=A(PFKEY TABLE)                            
         USING PFTABD,RF           LOOK UP PFKEY NUMBER IN TABLE                
PFV2     CLI   0(RF),X'FF'                                                      
         BE    PFERR2                                                           
*                                                                               
         CLC   PFKEY,PFTAID        MATCH ON NUMBER                              
         BE    PFV3                                                             
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     PFV2                                                             
*                                                                               
PFV3     TM    PFTSTAT2,PFTRETRN   TEST RETURN TO APPLICATION                   
         BO    NO2                                                              
*                                                                               
         BAS   RE,PFINVOKE         OK TO INVOKE PFKEY                           
         B     YES2                IF RETURNS, RETURN CC EQUAL                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * *                         
* ROUTINE TO PROCESS PFKEY REQUEST                                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * *                         
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
PFINVOKE NTR1                                                                   
         TM    PFTSTAT2,PFTCLRKY   DO WE CLEAR THE PFKEY?                       
         BNZ   *+8                 NO                                           
         MVI   PFKEY,0             CLEAR PFKEY FOR NEXT SCREEN                  
*                                                                               
         L     R1,SYSPARMS         RE=A(TRANSLATOR I/O BLOCK)                   
         L     RE,0(R1)                                                         
         MVI   TIOBAID-TIOBD(RE),0 CLEAR PF KEY HERE AS WELL                    
*                                                                               
         TM    PFTSTAT,PFTCPROG    TEST PFKEY GENERATES CALLPROG CALL           
         BZ    *+8                                                              
         BAS   RE,CPROG                                                         
*                                                                               
         CLI   PFTNKEYS,0          TEST KEY FIELDS PRESENT                      
         BE    *+8                                                              
         BAS   RE,EXPNDKEY         EXPAND THEM INTO 'KEY' FIELD                 
*                                                                               
         TM    PFTSTAT,PFTRPROG    POP NESTED CALL SEQUENCE                     
         BZ    *+12                                                             
         BAS   RE,RPROG            ROUTINE TO RESTORE PREV. SCREEN              
         B     DUMYERR2            TAKE DUMMY ERROR XIT FOR GOAGAIN             
*                                                                               
         CLI   PFTREC,C' '         IF NEW RECORD TYPE DEFINED                   
         BE    PFI8                                                             
         MVC   CONREC,PFTREC       MOVE IT OUT                                  
         OI    CONRECH+6,X'80'     TRANSMIT                                     
         MVI   CONRECH+5,8         SET L'I/P                                    
*                                                                               
         L     RE,EFHKEY           RE=A(KEY FIELD)                              
         CLI   5(RE),0             IF THERE'S NO INPUT IN KEY FIELD             
         BNE   *+12                                                             
         MVI   8(RE),C','          MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   5(RE),1             APPLICATION GETS CONTROL                     
*                                                                               
PFI8     CLI   PFTACT,C' '         TEST FOR ACTION CHANGE                       
         BE    PFIX                                                             
         MVC   CONACT,PFTACT       MOVE IT OUT                                  
         OI    CONACTH+6,X'80'     TRANSMIT                                     
         MVI   CONACTH+5,5         SET L'I/P - NOTE ONLY 5                      
*                                                                               
PFIX     B     XIT2                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*              ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS                 
*              R3=A(PFKEY TABLE)                                                
TESTSEL  NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TSELX                                                            
*                                                                               
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
*                                                                               
TSEL2    STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    TSEL6                                                            
         OC    8(3,R2),SPACES                                                   
*                                                                               
         LR    RF,R3               RF=A(START OF TABLE)                         
         USING PFTABD,RF                                                        
TSEL4    CLI   0(RF),X'FF'                                                      
         BE    TSEL6                                                            
         CLC   PFTSEL,8(R2)        MATCH ON EXACT SELECT CODE                   
         BE    TSEL8                                                            
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     TSEL4                                                            
*                                                                               
TSEL6    ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    TSELX               (E-O-S)                                      
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   TSEL2               SELECT FIELD                                 
         B     TSEL6                                                            
*                                                                               
TSEL8    MVC   8(3,R2),SPACES      FOUND A MATCH - CLEAR SELECT FIELD           
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    RE,R2               SAVE A(FIELD)                                
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   7(R2),0             TEST THERE'S SOMETHING TO SELECT             
         BE    TSEL6               (NO, SO IGNORE)                              
*                                                                               
         MVC   PFKEY,PFTAID        SET CORRESPONDING PFKEY NUMBER               
         SR    RE,RA                                                            
         STH   RE,CURDISP          SAVE DISP. TO FIELD                          
*                                                                               
TSELX    B     XIT2                                                             
*                                                                               
* THIS ROUTINE SAVES THE CURRENT TWA IN THE FIRST HALF OF TEMPSTR               
* RECORD NUMBERS 2.  IT THEN SAVES THE SCREEN NUMBER FOUND IN                   
* TWASCR ONTO A STACK.  THE USE OF THIS ROUTINE IN CONJUNCTION WITH             
* THE CHANGING OF THE RECORD, ACTION, AND KEY FIELDS ALLOWS USERS TO            
* CALL UP A NEW SCREEN AND THEN LATER RETURN TO THE SCREEN THEY WERE            
* WORKING ON.  WHEN THE USER WANTS TO RETURN TO A SCREEN, RETPROG WILL          
* BE CALLED TO RESTORE THE SCREEN.                                              
*                                                                               
CPROG    NTR1                                                                   
         MVC   CALLSTCK,TWASCR     SAVE SCREEN NUMBER ON STACK                  
         MVI   CALLSP,1                                                         
**       LR    R1,RA               WE HAVE TO DO THIS INSTEAD BECAUSE           
**       AHI   R1,TWAENDLQ-2       SPSFM HAS NO ROOM IN ITS SAVE AREA           
**       MVC   0(1,R1),TWASCR        FOR CALLSTCK AND CALLSP                    
**       MVI   1(R1),1                                                          
*                                                                               
         L     RE,ATIA             SAVE SCREEN IN FIRST HALF OF TWA             
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,X'82'            WRITE TWA RECORD #2                          
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         LA    R2,X'83'            WRITE TWA RECORD #3                          
         GOTO1 GETTWA,DMCB,((R2),ASTARTSV)                                      
*                                                                               
         B     XIT2                                                             
*                                                                               
* THIS ROUTINE RESTORES THE USER TO THE SCREEN THEY WERE WORKING ON             
* BEFORE CALLING ANOTHER SCREEN WHICH WAS SAVED IN TEMPSTR BY CALLPROG.         
*                                                                               
RPROG    NTR1                                                                   
**       LR    R1,RA               WE HAVE TO DO THIS INSTEAD BECAUSE           
**       AHI   R1,TWAENDLQ-2         SPSFM HAS NO ROOM IN ITS SAVE AREA         
**       CLI   1(R1),0                                                          
         CLI   CALLSP,0                                                         
         BE    PFERR2              ERROR IF STACK IS EMPTY                      
*                                                                               
         LA    R2,2                READ TWA RECORD #2                           
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         L     RE,ATIA             RESTORE SCREEN FROM 1ST HALF OF TWA          
         LHI   RF,TWAMXLEN                                                      
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   ACTNUM,TWALACT      SPECIAL CODE TO KEEP SELECT GOING            
         MVC   RECNUM,TWALREC                                                   
         MVC   CONHEAD(30),=CL30'Came back from another screen.'                
         MVC   CONHEAD+30(30),=CL30'  Please continue ...'                      
*                                                                               
         LA    R2,3                READ TWA RECORD #3                           
*^^NOP   GOTO1 GETTWA,DMCB,((R2),ASTARTSV)                                      
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
         L     RE,ATIA             RESTORE SAVE AREA FROM TIA                   
         LH    RF,=Y(SYSDEND-SVSTART)                                           
         L     R0,ASTARTSV                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   CALLSP,0            DECREMENT STACK POINTER                      
         MVC   TWASCR,CALLSTCK     EXTRACT TWASCR                               
**       LR    R1,RA               WE HAVE TO DO THIS INSTEAD BECAUSE           
**       AHI   R1,TWAENDLQ-2         SPSFM HAS NO ROOM IN ITS SAVE AREA         
**       MVI   1(R1),0               FOR CALLSTCK AND CALLSP                    
**       MVC   TWASCR,0(R1)        EXTRACT TWASCR                               
*                                                                               
         L     R2,ATWA             MUST SET INDICTOR TO XMIT ALL FIELDS         
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         OI    TRNSTAT,RETURNED    SET THAT RETPROG HAS BEEN CALLED             
*                                                                               
         B     XIT2                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*              ROUTINE TO EXPAND KEY FIELDS INTO TMPKEY FIELD                   
*                                                                               
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
EXPNDKEY NTR1                                                                   
         MVC   WORK,SPACES         BUILD KEY FIELD IN WORK FIRST                
         LA    R2,WORK             R2=A(WORK)                                   
         ZIC   R3,PFTNKEYS         R3=N'KEY FIELDS                              
         LA    R4,PFTKEYS          SET R4=A(1ST KEY FIELD)                      
         USING KEYD,R4                                                          
         L     R6,ATMPSTOR                                                      
         USING TMPSTORD,R6                                                      
*                                                                               
EXP10    CLI   KEYTYPE,KEYTYCOM    TEST SIMPLY PLACE IMBEDDED COMMA             
         BE    EXP20                                                            
         LR    RF,RA               SET WHERE DATA IS                            
         CLI   KEYTYPE,KEYTYTWA    TWA                                          
         BE    EXP15                                                            
         L     RF,ASTARTSV                                                      
         LA    RF,SYSSPARE                                                      
         CLI   KEYTYPE,KEYTYWS     W/S (SYSSPARE)                               
         BE    EXP15                                                            
         CLI   KEYTYPE,KEYTYCUR    CURSOR LOCATION                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    RF,CURDISP          ASSUME THIS IS A SELECT FIELD                
         AR    RF,RA               RF=A(FLD WHERE CURSOR IS)                    
         BAS   RE,BMPTOROW         BUMP TO FIRST FIELD FOR THIS ROW             
         BNE   PFERR2                                                           
         L     RF,FULL             RETURNS ADDRESS IN FULL                      
*                                                                               
EXP15    AH    RF,KEYDISP          RF=A(DATA)                                   
         ZIC   RE,KEYLEN           RE=L'DATA-1                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)       MOVE TO WORK                                 
         AR    R2,RE               BUMP TO LAST CHARACTER OF FIELD              
*                                                                               
         CLI   0(R2),C' '          SHUFFLE BACK TO 1ST NON-SPACE                
         BH    *+10                                                             
         BCTR  R2,0                                                             
         BCT   RE,*-10                                                          
         LA    R2,1(R2)            BUMP TO 1ST POSITION PAST                    
*                                                                               
         CH    R3,=H'1'            TEST THIS IS LAST KEY FIELD                  
         BE    EXPX                SO FINISH UP                                 
*                                                                               
EXP20    MVI   0(R2),C','          INSERT COMMA BEFORE NEXT FIELD               
         LA    R2,1(R2)            BUMP PAST COMMA TO NEXT POSITION             
         LA    R4,KEYNEXT          BUMP TO NEXT KEY FIELD                       
         BCT   R3,EXP10            AND PROCESS                                  
*                                                                               
EXPX     LA    R3,WORK                                                          
         SR    R2,R3               R2=L'TMPKEY FIELD                            
         CLM   R2,1,=AL1(L'TMPKEY)                                              
         BNH   *+6                                                              
         DC    H'0'                MAKE TMPKEY BIGGER                           
*                                                                               
         STC   R2,TMPKEYH+5        STORE LENGTH IN FIELD HEADER                 
         MVI   TMPKEYH,L'TMPKEY+L'TMPKEYH SET LENGTH OF FIELD                   
         LA    RE,TMPKEYH                                                       
         ST    RE,EFHKEY           TELL GENCON TO USE TMPKEY FIELD              
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     XIT2                                                             
         MVC   TMPKEY(0),WORK      MOVE DATA TO FAKE KEY FIELD                  
*                                                                               
*                                                                               
*                                                                               
BMPTOROW NTR1                      BUMP TO FIRST FIELD IN ROW                   
         LR    R2,RF               R2=A(CURRENT FIELD)                          
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         L     R2,AFRSTREC                                                      
BMPT2    LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE                                                        
         BE    BMPT4                                                            
         ZIC   R1,0(R2)            TRY NEXT FIELD                               
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BNE   BMPT2                                                            
         B     NO2                 RETURN CC NE IF REACHED E-O-S                
*                                                                               
BMPT4    DS    0H                                                               
*                                                                               
         TM    GLSTSTAT,NOSELFLD                                                
         BO    BMPT6                                                            
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               ASSUMING SELECT FIELD -- BUMP PAST           
*                                                                               
BMPT6    DS    0H                                                               
         LA    R2,8(R2)            AND PAST HEADER OF (FIRST) DATA FLD          
         ST    R2,FULL             MATCH-RETURN A(FLD HEADER) IN FULL           
         B     YES2                                                             
         DROP  R6,RA                                                            
*                                                                               
***********************************************************************         
* ROUTINE TO READ/WRITE TEMPSTR PAGES                                           
*                                                                               
* ON ENTRY:    PARAM 1  BYTE  0    BIT SETTINGS/PAGE NUMBER                     
*              PARAM 1  BYTES 1-3  READ/WRITE ADDRESS                           
***********************************************************************         
GETTWA   NTR1                                                                   
         L     RA,ATWA                                                          
         USING T230FFD,RA                                                       
*                                                                               
         MVC   BYTE,0(R1)          BIT SETTINGS/PAGE NUMBER                     
         L     R2,0(R1)            READ/WRITE ADDRESS                           
*                                                                               
         MVC   COMMAND(6),=C'DMWRT '                                            
         TM    BYTE,X'80'          X'80'=1 IS WRITE, ELSE READ                  
         BO    GTWA10                                                           
         MVC   COMMAND(6),=C'DMRDIR'                                            
         TM    BYTE,X'40'          X'40'=1 IS 2304 BYTE TWAS ELSE 6144          
         BNZ   GTWA10                                                           
         MVC   COMMAND(6),=C'DMREAD'                                            
*                                                                               
GTWA10   NI    BYTE,X'0F'          TURN OFF HIGH ORDER BITS                     
*                                                                               
         MVC   DMCB+8(1),BYTE      PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
*                                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TEMPSTR',,(R2),0                         
*                                                                               
         CLI   8(R1),0             IF COULDN'T DO IT, DIE                       
         BE    XIT2                                                             
         DC    H'0'                                                             
         DROP  RA                                                               
*                                                                               
PFERR2   MVI   ERROR,ERINVPFK      INVALID PF KEY                               
         TM    TRNSTAT,NOVALERR    NO ERREX ON VALI RTNS                        
         BZ    *+12                                                             
         OI    TRNSTAT,BASERR      SET FLAG FOR ERROR TO APPL                   
         B     XIT2                                                             
         GOTO1 ERREX               HOPEFULLY, NEVER TO RETURN                   
         B     XIT2                ELSE GENCON WAS SLAVED                       
*                                                                               
DUMYERR2 MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
         L     RA,ATWA             CURSOR TO RECORD FIELD                       
         USING T230FFD,RA                                                       
         LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         GOTO1 ERREX2                                                           
         DROP  RA                                                               
*                                                                               
YES2     SR    RC,RC               SET CC EQ                                    
NO2      LTR   RC,RC               SET CC NEQ                                   
XIT2     XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
*                                                                               
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
         USING T230FFD,RA                                                       
*                                                                               
         MVI   ERROR,0                                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T230FFD+6                                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVOFFC                                                    
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,SVAGYM        AGENCY/MEDIA BYTE                         
         MVC   OFCLMT(4),T230FFD+6                                              
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKTREC                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS                                   
         CLI   0(R1),0                                                          
         JE    EXIT                                                             
*                                                                               
         MVI   ERROR,SCLOCK                                                     
         CLI   SVMACCS,X'FF'                                                    
         BE    *+8                                                              
         MVI   ERROR,NOMKTACC                                                   
         J     EXIT                                                             
*                                                                               
SCLOCK   EQU   249                 SECURITY LOCKOUT                             
NOMKTACC EQU   69                  ACCESS TO MKT NOT AUTH                       
*                                                                               
*                                                                               
*                                                                               
*                                                                               
BLDEQVTB NTR1  BASE=*,LABEL=*                                                   
         MVC   PRESYST,CURSYST                                                  
         MVI   CURSYST,C'C'                                                     
         GOTO1 VALIFAS                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
*                                                                               
         MVI   BYTE,C'N'           TABLE FULL WARNING FLAG                      
*                                                                               
         USING EZSKEY,R4                                                        
         MVC   EZSKEY,SPACES                                                    
         MVC   EZSKTYP,=C'ZS'                                                   
         MVC   EZSKAGY,AGENCY                                                   
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'GENDIR ',KEY,(R4)                    
         B     BLDEQ31                                                          
*                                                                               
BLDEQ30  DS   0H                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ ',=C'GENDIR ',KEY,(R4)                    
*                                                                               
BLDEQ31  CLC   KEY(4),KEYSAVE                                                   
         BNE   BLDEQ50                                                          
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,(R6),WORK             
*                                                                               
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BE    BLDEQ30                                                          
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,02                                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EZSIDEL,R6                                                       
*                                                                               
         MVC   WORK(5),EZSKSTA                                                  
         MVC   WORK+5(5),EZSCALL                                                
*                                                                               
         MVI   BP4,X'01'         ADD RECORD IF NOT FOUND                        
         SAM31                                                                  
         GOTO1 VBINSR31,BSPARS,WORK                                             
         SAM24                                                                  
         OC    BP1,BP1             TEST TABLE FULL                              
         JZ    *+2                                                              
*                                                                               
         LRL   R3,BP3              NUMBER OF RECORDS                            
         CHI   R3,NUMSTAQ*9/10     LESS THAN 10% LEFT?                          
         BL    BLDEQ30             NO - PROCEED                                 
*                                                                               
         CLI   BYTE,C'N'           YES, SEND AUTONOTE                           
         BNE   BLDEQ30                                                          
         MVI   BYTE,C'Y'                                                        
         MVC   MSGTEXT1+17(2),AGENCY                                            
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'MSGTEXT1,MSGTEXT1)                     
         B     BLDEQ30                                                          
*                                                                               
MSGTEXT1 DC    C'AUTONOTE*TZIH: **XX STA EQU TABLE 90% FULL**'                  
*                                                                               
BLDEQ50  DS    0H                                                               
         L     RF,ASTATAB                                                       
         SAM31                                                                  
         MVC   0(4,RF),BP3                                                      
         SAM24                                                                  
*                                                                               
         CLI   TWAMODE,0                                                        
         BNE   BLDEQ60             DON'T SAVE TO WSSVR                          
*                                                                               
         LARL  R3,WSSVAREA                                                      
         USING FAWSSVRD,R3                                                      
*                                                                               
         MVI   FAWSACTN,FAWSUSVE   SAVE IN UTL XA BUFFER                        
*                                                                               
         GOTO1 WSSVR,FAWSSVRD                                                   
         CLI   FAWSRTN,0                                                        
         JNE   *+2                 DIE ON ALL ERRORS                            
*                                                                               
         DROP  R3                                                               
*                                                                               
BLDEQ60  DS    0H                                                               
         MVC   AIO,AIO1                                                         
         CLI   PRESYST,X'00'                                                    
         BE    *+10                                                             
         MVC   CURSYST,PRESYST                                                  
         GOTO1 VALIFAS                                                          
         J     EQXIT                                                            
*                                                                               
*                                                                               
* INITIALIZE SYSTEM DEPENDENT VALUES *                                          
*                                                                               
SYSINIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    RD,SYSRD                                                         
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RE,8(RF)            SYSFACS                                      
         MVC   CALLOV,4(RE)        (CALLOV)                                     
         MVC   ACOMFACS,16(RF)     COMFACS                                      
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A1B'       MSPACK                           
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   MSPACK,0(R1)                                                     
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A1C'       MSUNPK                           
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   MSUNPK,0(R1)                                                     
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A30'       GENCON                           
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   GENCON,0(R1)                                                     
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   WSSVR,CWSSVR-COMFACSD(RF)                                        
*                                                                               
         L     RF,=V(GETMED)                                                    
         A     RF,RELO                                                          
         ST    RF,VGETMED                                                       
*                                                                               
         L     RF,=V(PRTSTA)                                                    
         A     RF,RELO                                                          
         ST    RF,VPRTSTA                                                       
*                                                                               
         L     RF,=V(CLPACK)                                                    
         A     RF,RELO                                                          
         ST    RF,CLPACK                                                        
*                                                                               
         L     RF,=V(CLUNPK)                                                    
         A     RF,RELO                                                          
         ST    RF,CLUNPK                                                        
*                                                                               
         L     RF,=V(BINSRCH)                                                   
         A     RF,RELO                                                          
         ST    RF,VBINSR31                                                      
*                                                                               
         L     RF,=V(DUMMY)                                                     
         A     RF,RELO                                                          
         ST    RF,VDUMMY                                                        
*                                                                               
         LARL  R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
*                                                                               
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
*                                                                               
* SET SYSTEM DEPENDENT VALUES *                                                 
*                                                                               
         MVI   SYSTEM,C'S'         INITIALIZE TO "S" (USED TO BE "Z")           
         MVI   FRSTLAST,C'Y'                                                    
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 4096 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'32'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVC   LWORK,=A(LENWORK)   SET WORK AREA LENGTH                         
         MVC   RCPROG(2),=C'SP'    PREFIX FOR REPORT NO.                        
*                                                                               
         L     R1,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(,R1)                                        
*                                                                               
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         CLI   FAOVSYS,03          NET                                          
         BNE   *+14                                                             
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT NO.                        
         MVI   SYSTEM,C'N'                                                      
         DROP  RE                                                               
*                                                                               
         MVC   SYSPHASE,=X'D9023000'    PRESET FOR SYSTEM CALLOVS               
         LARL  R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         MVI   LRECACT,13                                                       
         LA    R1,SVSTART          SET SAVED STORAGE START                      
         ST    R1,ASTARTSV                                                      
         MVI   NTWA,X'80'          RESTORE LARGE SYSD AREA                      
         MVC   LSVTWA0,=AL2(WRKFBUFR-SYSD) WRK STORAGE UP TO WRKFBUFR           
*                                                                               
         OI    GENSTAT7,GES7DDS    DDS ONLY RECORDS                             
*                                                                               
         XC    UTL,UTL                                                          
         L     RE,SYSPARMS                                                      
         L     RF,4(,RE)           TCBTWA                                       
*                                                                               
* TEST IF OFFLINE - IF TWAVPRNT ZERO, ONLINE *                                  
*                                                                               
         OC    TWAVPRNT-T230FFD(,RF),TWAVPRNT-T230FFD(RF)                       
         BZ    SYS30                                                            
         L     R1,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         L     R1,FAAUTL-FACTSD(RE)                                             
         ST    R1,UTL                                                           
*                                                                               
         CLI   SAVSYS,0                                                         
         BNE   SYS30                                                            
         MVC   SAVSYS,4(R1)                                                     
*                                                                               
SYS30    DS    0H                                                               
*                                                                               
         MVI   CURSYST,C'M'                                                     
*                                                                               
         L     RA,ATWA                                                          
         USING T230FFD,RA                                                       
*                                                                               
         LH    R6,=Y(SVSPARE-T230FFD)                                           
         AR    R6,RA                                                            
         ST    R6,ATMPSTOR                                                      
*                                                                               
         L     R1,SYSPARMS         RF = A(TIOB)                                 
         L     RF,0(R1)                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID                                                       
         CH    R0,=H'12'                                                        
         BNH   *+8                                                              
         SHI   R0,12                                                            
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY                          
         MVC   CURDISP,TIOBCURD    SAVE CURSOR DISPLACEMENT                     
         DROP  RF                                                               
*                                                                               
*=====================================================*                         
* INITIALIZE SECRET                                   *                         
*=====================================================*                         
         LR    RF,RA                                                            
         AHI   RF,SECBLK-T230FFD                                                
         XC    ASECBLK,ASECBLK                                                  
*                                                                               
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    T230FFD+6(2),T230FFD+6   OR HAVE LIMIT ACCESS                    
         BZ    SYS40                                                            
*                                                                               
         ST    RF,ASECBLK                                                       
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SYS40    DS    0H                                                               
         CLI   TWAMODE,0                                                        
         BNE   SYS50                                                            
*                                                                               
* RUNNING ONLINE HERE                                                           
*                                                                               
* GET XA9 BUFFER ADDRESS                                                        
*                                                                               
         L     R1,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(,R1)                                        
         GOTO1 (RF),DMCB,(X'80',DUB),F#XA9ADR                                   
         MVC   ASTATAB,DUB                                                      
*                                                                               
* SET UP WSSVR PARAMETERS                                                       
         LARL  R3,WSSVAREA                                                      
         USING FAWSSVRD,R3                                                      
*                                                                               
*BUFFSIZQ EQU   9*1024              WSSVR BUFFER SIZE, 9K FOR NOW               
BUFFSIZQ EQU   10*1024              WSSVR BUFFER SIZE, 10K                      
*                                                                               
         XC    FAWSSVRD(FAWSSVRL),FAWSSVRD                                      
         MVC   FAWSTOKN,=C'EQST'   TOKEN                                        
         MVC   FAWSADR,ASTATAB     A(XA9 BUFFER)                                
*        MVC   FAWSLEN,=AL2(XAPASSRL-WSLNQ) BUFFER LENGTH                       
         MVC   FAWSLEN,=AL2(BUFFSIZQ-WSLNQ)                                     
*                                                                               
         DROP  R3                                                               
*                                                                               
* SETUP BINSRCH PARAMETERS                                                      
SYS50    DS    0H                                                               
         XC    BSPARS,BSPARS                                                    
         LA    RE,WORK                                                          
         ST    RE,BP1              P1: RECORD AREA                              
         L     RF,ASTATAB                                                       
         AHI   RF,4                FIRST 4 BYTES = NUMBER OF RECORDS            
         ST    RF,BP2              P2: A(BUFFER: XA9/GETMAIN)                   
         MVI   BP4+3,10            P4: L'RECORD                                 
         MVI   BP5+3,5             P5: L'KEY                                    
         LHI   RE,NUMSTAQ                                                       
         STCM  RE,15,BP6           P6: MAX NUMBER OF RECORDS                    
*                                                                               
SYSX     J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
* DDCOMFACS/DDSPOOLD/DDSPLWORKD/SPEZFWORKD/DDCNTRL/CLTHRDD                      
* PRDHDRD/AGYHDRD/STARECD/SPGENEZ/CTGENFILE/SPEZFFFD                            
*                                                                               
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FASSB                                                          
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFWORKD                                                     
       ++INCLUDE SPEZFSYSD                                                      
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DMWRKFK                                                        
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
*                                                                               
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
*                                                                               
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
*                                                                               
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
*                                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
*                                                                               
* EASI FILE DSECTS WORKER INDEX/AGENCY OFFICE/CLIENT/PRODUCT                    
       ++INCLUDE SPGENEZ                                                        
*                                                                               
       ++INCLUDE CTGENFILE                                                      
*                                                                               
       ++INCLUDE SPEZFFFD                                                       
*                                                                               
         ORG   CONHEADH-64                                                      
       ++INCLUDE DDGENTWA                                                       
*                                                                               
       ++INCLUDE SPEZFSAVED                                                     
*                                                                               
* DDCOREQUS                                                                     
       ++INCLUDE DDCOREQUS                                                      
*                                                                               
       ++INCLUDE DDOFFICED                                                      
*                                                                               
       ++INCLUDE SPEZDSCTS                                                      
*                                                                               
LKAGYD   DSECT                                                                  
       ++INCLUDE EZLKAGYBLK                                                     
*                                                                               
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE FAUTL                                                          
*                                                                               
TMPSTORD DSECT                                                                  
TMPKEYH  DS    XL8                 DUMMY KEY FIELD HEADER FOR GENCON            
TMPKEY   DS    CL50                DUMMY KEY FIELD                              
*                                                                               
$$LITER  LOCTR                                                                  
         LTORG                                                                  
*                                                                               
$$CODE   LOCTR                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'146SPEZF00   05/01/18'                                      
         END                                                                    
