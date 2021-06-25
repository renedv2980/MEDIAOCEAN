*          DATA SET RERMP20S   AT LEVEL 112 AS OF 12/15/03                      
*PHASE T81020B,*                                                                
*INCLUDE UPOUT                                                                  
*  CLUDE PRINT                                                                  
*  CLUDE PRNTBL                                                                 
                                                                                
         TITLE 'T81020 - RERMP20 - ETRANS REPORT'                               
                                                                                
***********************************************************************         
*                                                                     *         
*- RERMP20 -- ETRANS REPORT                                           *         
*                    -(FORMERLY LABELED AS "DEMO EDICT CREATE")       *         
*  MOD LOG:                                                           *         
*  --------                                                           *         
*                                                                     *         
*  OCT30/00 -- CHANGED TO ALLOW FOR FILTERING BY INV #S               *         
*               (RANGE AND/OR SERIES)                                 *         
*               NECESSITATED ADDITION OF E1 REC TYPE TO NOTIFY        *         
*               TAPSCAN OF INV STILL ON FILE BUT NOT SENT IN TRANSFER *         
*              ALSO, MODULE REARRANGED TO USE ONLY ONE BASE REGISTER  *         
*              AND RELATIVE BRANCHING                                 *         
*                                                                     *         
*  APR27/01 (BU ) --- TEST VERSION TO DISPLAY OUTPUT                  *         
*                                                                     *         
*  AUG14/01 (JRD) --- DON'T INCLUDE DEBUG PRINTS ONLINE, PHASE LIST   *         
*                     IS TOO LONG.                                    *         
*  AUG22/02 (HQ ) --- CHANGE READ TO CTFILE TO BE NON-UPDATIVE        *         
*                     PER ALAN A.                                     *         
*                                                                     *         
*  DEC12/02 (BU ) --- NEW EDICT CARDS                                 *         
*                                                                     *         
*  FEB06/03 (BU ) --- MODIFY EDICT CARDS FROM *TAPSCAN TO *TVSCAN     *         
*                                                                     *         
*  FEB21/03 (BU ) --- REVISED DEMO LIST (AGAIN)                       *         
*                                                                     *         
*  MAR27/03 (BU ) --- SET HEADER VERSION TO '3' FROM 2.               *         
*                                                                     *         
*  AUG19/03 (SKU) --- HAVE SPECIAL WOOD FOR EDICT                     *         
*                                                                     *         
*  OCT02/03 (BU ) --- HAVE SPECIAL WZPX FOR EDICT                     *         
*                                                                     *         
*  OCT09/03 (BU ) --- HFS PROCESSING: A FLAG HAS BEEN SET IN THE      *         
*                     STATION RECORD FOR THIS, BUT RIGHT NOW, IT'S    *         
*                     A HARD-CODED TABLE                              *         
*                                                                     *         
*  DEC03/03 (BU ) --- HAVE SPECIAL WISH FOR EDICT                     *         
*                 --- HAVE SPECIAL WLFI FOR EDICT                     *         
*                                                                     *         
*  DEC05/03 (BU ) --- HAVE SPECIAL WCVB FOR EDICT                     *         
*                                                                     *         
*  DEC15/03 (BU ) --- HAVE SPECIAL WOKR FOR EDICT                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
                                                                                
         SPACE 2                                                                
T81020   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (WRKLENQ),T81020**,RR=RE,CLEAR=YES                               
         USING T81020,RB                                                        
                                                                                
         LR    R8,RC               ESTABLISH MODULE'S LOCAL STORAGE             
         USING WORKD,R8                                                         
                                                                                
         L     RC,0(R1)            ESTABLISH GENCON WORKAREA                    
         USING GEND,RC                                                          
                                                                                
         L     R7,ASPOOLD          ESTABLISH PRINT CONTROL AREA                 
         USING SPOOLD,R7                                                        
                                                                                
         L     RA,ATWA             ESTABLISH SCREEN                             
         USING CONHEADH-64,RA                                                   
                                                                                
         L     R9,ASYSD            ESTABLISH SYSTEM WORKAREA                    
         USING SYSD,R9                                                          
                                                                                
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
                                                                                
DB       USING DBLOCK,MYDBLOCK     ASSIGN DBLOCK DSECT                          
                                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MAIN0020                                                         
         BRAS  RE,VALREQ                                                        
         B     MAINEXIT                                                         
         SPACE 1                                                                
                                                                                
MAIN0020 EQU   *                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   MAIN0040                                                         
         BRAS  RE,VALREQ                                                        
*                                                                               
*                                                                               
         BRAS  RE,SETHFS           DETERMINE OUTPUT FORMAT                      
         BRAS  RE,SETTAPE                                                       
         BRAS  RE,SETEDICT                                                      
         TM    HFSFLAG,X'80'       OUTPUT IS UNIX FILE?                         
         BNO   MAIN0030            NO                                           
*                                                                               
         GOTO1 HFSPROC,DMCB,(0,0)                                               
*                                  PROCESS HFS FILE FOR 'OPEN'                  
MAIN0030 EQU   *                                                                
*                                                                               
         BRAS  RE,REPMODE                                                       
         B     MAINEXIT                                                         
         SPACE 1                                                                
                                                                                
MAIN0040 CLI   MODE,RUNLAST                                                     
         BNE   MAINEXIT                                                         
*                                                                               
         TM    HFSFLAG,X'80'       OUTPUT IS UNIX FILE?                         
         BO    MAIN00640           YES                                          
*                                  NO  -                                        
         L     R2,=A(FILOUTA)                                                   
         A     R2,RELO                                                          
                                                                                
         CLOSE ((R2),)                                                          
*        GOTO1 =V(PRNTBL),DMCB,=C'CDCB',0(R2),C'DUMP',200,=C'1D'                
         B     MAINEXIT                                                         
MAIN00640 EQU  *                                                                
*                                                                               
         GOTO1 HFSPROC,DMCB,(2,0)                                               
*                                  PROCESS HFS FILE FOR 'CLOSE'                 
         B     MAINEXIT                                                         
MAINEXIT XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*********************************************************************           
*                                                                               
*        SETTAPE ROUTINE  -- OPEN UP THE FILE                                   
*                                                                               
*********************************************************************           
                                                                                
SETTAPE  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTO1 DATCON,DMCB,(5,DUB),(19,WORK) GET TODAYS DATE (JULIAN)           
         MVO   FULL(3),WORK+1(2)                                                
         MVI   FULL,0                                                           
         OI    FULL+2,X'0F'                                                     
         EDIT  (P3,FULL),(3,TDTTIME),FILL=0                                     
         TIME  DEC                                                              
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,DUB,4,=C'TOG'                                   
         MVC   TDTTIME+3(4),DUB    SAVE CURRENT TIME (HHMM)                     
         MVC   TDTTIME+7(2),=C'.S'                                              
         MVC   TDTTIME+9(2),DUB+4  SAVE CURRENT TIME (SS)                       
                                                                                
         MVC   WORK(30),=CL30'SMSROE.D                      '                   
         MVC   WORK+8(11),TDTTIME   SET DAY AND TIME                            
***********************************************************************         
*                                                                               
*   OLD DYNALLOC CALLS NO LONGER USED                                           
*                                                                               
*        GOTO1 =V(DYNALLOC),DMCB,(0,=CL8'FILOUTA'),(0,WORK)                     
         PRINT GEN                                                              
***      GOTO1 =V(DYNALLOC),DMCB,(X'80',=CL8'FILOUTA'),               X         
***            (X'81',DISKSPCE),                                      X         
***            (X'80',WORK)                                                     
***********************************************************************         
         TM    HFSFLAG,X'80'       OPEN HFS FILE?                               
         BO    SETT0100            YES - NO DYNALLOC                            
*                                                                               
         L     RF,TWADCONS                                                      
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         GOTO1 (RF),DMCB,(X'80',=CL8'FILOUTA'),                        X        
               (X'81',DISKSPCE),                                       X        
               (X'80',WORK)                                                     
         PRINT NOGEN                                                            
                                                                                
         L     R4,=A(FILOUTA)                                                   
         A     R4,RELO                                                          
                                                                                
         OPEN  ((R4),(OUTPUT))                                                  
         B     SETT0800                                                         
*                                                                               
SETT0100 EQU   *                                                                
SETT0800 EQU   *                                                                
         L     RE,ATWA                                                          
         MVI   29(RE),2                                                         
         XIT1                                                                   
*&&DO                                                                           
DISKSPCE DC    XL6'000005000005'   SIZE PARAMS FOR DYNALLOC FILE                
*&&                                                                             
DISKSPCE DC    AL3(35,35)                                                       
         EJECT                                                                  
**********************************************************************          
*                                                                               
*        SETEDICT ROUTINE -- SET UP THE EDICT FILE                              
*                                                                               
*QUES:   DOES THE HFS MECHANISM REQUIRE AN EDICT HEADER?  THE SAME              
*             ONE, OR WITH MODIFICATIONS?                                       
*                                                                               
**********************************************************************          
                                                                                
SETEDICT NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTO1 SPOOL,DMCB,(R7)                                                  
                                                                                
         MVI   P,X'40'             *HDR* CARD                                   
         MVC   P+1(L'P-1),P                                                     
         MVC   P+4(5),=C'*HDR*'                                                 
         MVC   P+9(6),=C'EDICT='                                                
                                                                                
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CTIREC,R6                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TWAORIG                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO2                  
*                                  KEY AND KEYSAVE NOT AFFECTED BECAUSE         
         L     R6,AIO2                                                          
         CLC   KEY(L'CTIKEY),0(R6)   IT ISN'T A GENCON READ HIGH                
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
                                                                                
         LA    R6,28(R6)           OFFSET TO FIRST ELEMENT                      
                                                                                
EDICT03  DS    0H                                                               
         CLI   0(R6),0             NOT FOUND, EXIT                              
         BNE   *+6                                                              
         DC    H'0'                MUST BE THERE!                               
                                                                                
         CLI   0(R6),CTDSCELQ                                                   
         BE    EDICT05                                                          
         ZIC   RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                MUST BE THERE!                               
                                                                                
         AR    R6,RF                                                            
         B     EDICT03                                                          
*                                                                               
EDICT05  DS    0H                                                               
                                                                                
***      MVC   P+15(8),=CL8'TAPSCAN '                                           
*                                  BDE VERSION                                  
         MVC   P+15(8),=CL8'*TVSCAN '                                           
*                                  ISDN VERSION                                 
*                                                                               
* SPECIAL HARD CODE TO USE AN ALTERNATE EDICT RECORD                            
*    ADD STATION TO HFSTABL ALSO                                                
*                                                                               
         CLC   =C'WOODT',CSTAT                                                  
         BE    EDICT07                                                          
         CLC   =C'WXSPT',CSTAT                                                  
         BE    EDICT07                                                          
         CLC   =C'WOTVT',CSTAT                                                  
         BE    EDICT07                                                          
         CLC   =C'WISHT',CSTAT                                                  
         BE    EDICT07                                                          
         CLC   =C'WLFIT',CSTAT                                                  
         BE    EDICT07                                                          
         CLC   =C'WCVBT',CSTAT                                                  
         BE    EDICT07                                                          
         CLC   =C'WOKRT',CSTAT     SELTEL                                       
         BE    EDICT07                                                          
         CLC   =C'WZPXT',CSTAT                                                  
         BNE   EDICT10                                                          
*                                                                               
EDICT07  DS    0H                                                               
         MVC   P+15(8),=CL8'*1DOMAIN'                                           
         MVI   P+37,C'H'           132 CHARS WIDE                               
         B     EDICT15                                                          
*                                                                               
EDICT10  DS    0H                                                               
         MVI   P+37,C'D'           132 CHARS WIDE                               
EDICT15  DS    0H                                                               
****     MVC   P+38(16),P+15      FORMATTED DESTINATION NAME                    
         GOTO1 SPOOL,DMCB,(R7)                                                  
                                                                                
                                                                                
         LA    R6,P                                                             
         USING EDICTD,R6                                                        
         MVI   P,C' '              ++DDS TRN CARD                               
         MVC   P+1(L'P-1),P                                                     
         MVC   P(14),=CL14'++DDS REDTRTRN'                                      
         LA    R1,P+15                                                          
         USING EDIRDT,R1                                                        
         MVC   EDIRDTRP,AGENCY                                                  
         MVC   EDIRDTST,CSTAT                                                   
         MVC   EDIRDTSD,STRDATE6                                                
         MVC   EDIRDTED,ENDDATE6                                                
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R7)                                                  
*                                                                               
         MVI   P,C' '              ++DDS TRN CARD                               
         MVC   P+1(L'P-1),P                                                     
*                                                                               
         TM    HFSFLAG,X'80'       HFS OUTPUT FOR THIS RUN?                     
         BO    EDICT20             YES - OUTPUT HSF FORMAT                      
         MVC   P(14),=CL14'++DDS      DSN'                                      
*                                                                               
         MVC   P+15(30),=CL30'SMSROE.D                      '                   
         MVC   P+23(11),TDTTIME                                                 
         B     EDICT40                                                          
EDICT20  EQU   *                                                                
         MVC   P(14),=CL15'++DDS      DSN '                                     
*                             1     2         3                                 
*                             5.7.9.1.3.5.7.9.1.3.5.7.9.                        
         MVC   P+15(17),=CL17'/u/edict/etrans/D'                                
         MVC   P+32(11),TDTTIME                                                 
         LA    RF,28               SET DATA FILE LENGTH                         
         ST    RF,DFLEN                                                         
         MVC   DATAFILE(28),P+15   MOVE FILE TO DATAFILE NAME                   
***      MVC   P+15(09),=CL09'/u/yyun/D'                                        
***      MVC   P+24(11),TDTTIME                                                 
***      LA    RF,20               SET DATA FILE LENGTH                         
***      ST    RF,DFLEN                                                         
***      MVC   DATAFILE(20),P+15   MOVE FILE TO DATAFILE NAME                   
*                                                                               
*   DFLEN MUST AGREE WITH SIZE OF DATAFILE LABEL                                
*                                                                               
EDICT40  EQU   *                                                                
         GOTO1 SPOOL,DMCB,(R7)                                                  
*                                                                               
*        NEW EDICT CARDS PER FRED ROE:  DEC/02                                  
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(32),=C'SUB ETRANS RERMP20 DEMO DOWNLOAD'               
                                                                                
*                                                                               
         GOTO1 SPOOL,DMCB,(R7)                                                  
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(05),=C'FIL D'                                          
         EJECT                                                                  
         MVC   EDISYST+10(07),TDTTIME                                           
*                                  INSERT FILE NAME CREATED,                    
         GOTO1 SPOOL,DMCB,(R7)                                                  
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST+5(04),=C'EXT '                                           
         MVC   EDISYST+9(03),TDTTIME+8                                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R7)                                                  
*                                                                               
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   P+11(32),=C'    ETRANS RERMP20 DEMO DOWNLOAD'                    
         GOTO1 SPOOL,DMCB,(R7)                                                  
*                                                                               
*        NEW EDICT CARDS END                                                    
*                                                                               
EDICTX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  R6                                                               
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                               
*-- OPEN THE FILE?                                                              
*                                                                               
**********************************************************************          
CAR_RET  EQU   X'0D'                                                            
LINE_FD  EQU   X'0A'                                                            
NEW_LINE EQU   X'15'                                                            
BPX1OPNQ EQU   156                                                              
BPX1CLOQ EQU   72                                                               
BPX1WRTQ EQU   220                                                              
**********************************************************************          
HFSPROC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         LA    R4,DATABLOK                                                      
         USING DATA,R4                                                          
*                                                                               
         L     R3,AIO3             SET A(IO AREA3)                              
         USING VARRECD,R3                                                       
*                                                                               
         CLI   0(R1),0             OPEN REQUEST?                                
         BNE   HFSWRITE            NO  - CHECK WRITE                            
*                                                                               
         L     R1,16               CVT - COMMON VECTOR TABLE                    
         L     R1,544(R1)          CSRTABLE                                     
         L     R6,24(R1)           CSR SLOT                                     
         ST    R6,CSRSLOT                                                       
*                                                                               
         XC    S_MODE,S_MODE                                                    
         MVI   S_MODE2,S_IRUSR     USER READ/WRITE, GROUP READ                  
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IROTH                                  
*                                  OTHER READ                                   
         XC    O_FLAGS(OPNF#LENGTH),O_FLAGS                                     
         MVI   O_FLAGS4,O_CREAT+O_RDWR+O_TRUNC                                  
         L     RF,BPX1OPNQ(R6)     ADDRESS OF THE SERVICE BPX1OPN               
         CALL  (15),                 OPEN A FILE  BPX1OPN              X        
               (DFLEN,               INPUT: PATHNAME LENGTH            X        
               DATAFILE,             INPUT: PATHNAME                   X        
               O_FLAGS,              INPUT: ACCESS            BPXYOPNF X        
               S_MODE,               INPUT: MODE    BPXYMODE, BPXYFTYP X        
               RETVAL,               RETURN VALUE:-1 OR FILE DESCRIPTO X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   SAVEFILE                                                         
         LA    RE,DFLEN                                                         
         L     R7,RETVAL                                                        
         L     R8,RETCODE                                                       
         L     R9,RSNCODE                                                       
         DC    H'0'                                                             
SAVEFILE EQU   *                                                                
*                                                                               
         ST    RF,FILEDESC                                                      
         B     HFSP0800            EXIT ROUTINE                                 
         EJECT                                                                  
**********************************************************************          
*                                                                               
*-- WRITE THE FILE?                                                             
*                                                                               
**********************************************************************          
HFSWRITE EQU   *                                                                
         CLI   0(R1),1             WRITE REQUEST?                               
         BNE   HFSCLOSE            NO  - CHECK FOR CLOSE                        
*                                                                               
         L     R5,4(R1)            A(START OF VARIABLE RECORD)                  
         ZICM  RF,0(R5),2          GET RECORD LENGTH FROM CONTROL               
         LR    RE,R5               SET A(START OF RECORD AREA)                  
         AR    RE,RF               ADD LEN TO SET A(END OF RECORD)              
         MVI   0(RE),CAR_RET       INSERT CARRIAGE RETURN                       
         MVI   1(RE),LINE_FD       INSERT LINE FEED                             
         LA    R5,4(R5)            A(START OF DATA)                             
         ST    R5,BUFADDR          SET A(BUFFER ADDRESS) FOR UNIX               
*                                                                               
         L     R6,CSRSLOT          RESET A(CSRSLOT)                             
         SH    RF,=H'2'            RESET RECORD LEN:                            
*                                     - 4 : SUBTRACT VAR CONTROL                
*                                     + 2 : ADD CARRET, LF                      
*                                     NET = - 2                                 
*                                                                               
         ST    RF,BUFLEN                                                        
*                                                                               
         L     RF,BPX1WRTQ(R6)       ADDRESS OF THE SERVICE BPX1WRT             
*                                                                               
*   TEST DUMP                                                                   
**       LA    R0,6                                                             
**       DC    H'0'                                                             
*   TEST DUMP                                                                   
*                                                                               
         CALL  (15),                 WRITE TO A FILE                   X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               BUFADDR,              INPUT: ->BUFFER                   X        
               ALET,                 INPUT: BUFFER ALET                X        
               BUFLEN,               INPUT: NUMBER OF BYTES TO WRIT    X        
               RETVAL,               RETURN VALUE: -1 OR BYTES WRIT    X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
*                                                                               
*   TEST DUMP                                                                   
**       LA    R0,7                                                             
**       DC    H'0'                                                             
*   TEST DUMP                                                                   
*                                                                               
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   HFSP0800            RECORD WRITTEN: EXIT                         
         DC    H'0'                ERROR ON WRITE                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*-- CLOSE THE FILE?                                                             
*                                                                               
**********************************************************************          
HFSCLOSE EQU   *                                                                
         CLI   0(R1),2             CLOSE REQUEST?                               
         BE    HFSC0020            YES -                                        
         DC    H'0'                NO  - UNRECOGNIZED REQUEST                   
HFSC0020 EQU   *                                                                
         L     R6,CSRSLOT          RESET A(CSRSLOT)                             
         L     RF,BPX1CLOQ(R6)     ADDRESS OF THE SERVICE BPX1CLO               
         CALL  (15),                 CLOSE A FILE                      X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               RETVAL,               RETURN VALUE: 0 OR -1             X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
HFSP0800 EQU   *                                                                
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         XIT1                                                                   
         DROP  R3,R4                                                            
**********************************************************************          
*                                                                               
*-- PLIST CALLED BY OPEN/WRITE/CLOSE                                            
*                                                                               
**********************************************************************          
PLIST    CALL  ,(,,,,,,,,,,,,),MF=L                                             
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                               
*-- EDIT THE REQUEST SCREEN                                                     
*                                                                               
**********************************************************************          
                                                                                
SETHFS   NTR1  BASE=*,LABEL=*                                                   
         MVI   HFSFLAG,0           CLEAR HFSFLAG                                
         LA    RF,HFSTAB           SET STATION TABLE                            
SETH0020 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    SETH0800            YES - EXIT                                   
         CLC   CSTAT,0(RF)         STATION IN TABLE?                            
         BE    SETH0040            YES                                          
         LA    RF,LHFSTAB(RF)      NO  - BUMP TO NEXT TABLE ENTRY               
         B     SETH0020            GO BACK FOR NEXT                             
SETH0040 EQU   *                                                                
         OI    HFSFLAG,X'80'       STATION FOUND: SET HFS FLAG                  
         LA    RF,200              SET FIELD VALUES                             
         ST    RF,SIZE                                                          
         XC    ALET,ALET                                                        
SETH0800 EQU   *                                                                
         XIT1                                                                   
HFSTAB   EQU   *                                                                
         DC    C'WOODT'                                                         
LHFSTAB  EQU   *-HFSTAB                                                         
         DC    C'WXSPT'                                                         
         DC    C'WOTVT'                                                         
         DC    C'WZPXT'                                                         
         DC    C'WISHT'                                                         
         DC    C'WLFIT'                                                         
         DC    C'WCVBT'                                                         
         DC    C'WOKRT'                                                         
         DC    X'0000'                                                          
         DS    0H                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
*-- EDIT THE REQUEST SCREEN                                                     
*                                                                               
**********************************************************************          
                                                                                
VALREQ   NTR1  BASE=*,LABEL=*                                                   
         SPACE 1                                                                
         LA    R2,TITSTAH          STATION FIELD                                
         XC    WORK,WORK                                                        
         GOTO1 VALISTA             VALIDATE STATION                             
         CLI   WORK+40,X'40'                                                    
         BNH   *+10                                                             
         MVC   CSTAT+4(1),WORK+40                                               
*                                                                               
*        VALIDATE START DATE                                                    
*                                                                               
         LA    R2,TITSDTH          POINT TO START DATE                          
         XC    STRTOPT,STRTOPT     INIT START DATE OPTIONS                      
         XC    STRTOPTC,STRTOPTC   INIT START DATE OPTIONS                      
*                                                                               
         CLI   5(R2),0             OKAY IF NO START DATE ENTERED                
         BE    VREND                                                            
         CLC   8(3,R2),=C'ALL'     OKAY IF 'ALL' ENTERED                        
         BE    VREND                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK   VALIDATE ENTERED DATE               
*                                                                               
         CLI   DMCB+3,0                                                         
         BNE   VRSTRT1                                                          
         B     DATERR                                                           
*                                                                               
         MVC   STRDATE6,WORK                                                    
*                                                                               
VRSTRT1  GOTO1 DATCON,DMCB,(0,WORK),(3,STRTOPT)  SAVE START DATE                
         GOTO1 DATCON,DMCB,(0,WORK),(2,STRTOPTC) SAVE START DATE                
*                                                                               
*        VALIDATE END DATE                                                      
*                                                                               
VREND    LA    R2,TITEDTH          POINT TO END DATE                            
         MVC   ENDOPT,=X'FFFFFF'   INIT INTERNAL END DATE                       
                                                                                
         CLI   5(R2),0             OKAY IF NO END DATE ENTERED                  
         BE    VRENDX                                                           
         CLC   8(3,R2),=C'ALL'     OKAY IF 'ALL' ENTERED                        
         BE    VRENDX                                                           
                                                                                
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK  VALIDATE ENTERED DATE                
         CLI   DMCB+3,0                                                         
         BNE   VREND1                                                           
         B     DATERR                                                           
                                                                                
         MVC   ENDDATE6,WORK                                                    
                                                                                
VREND1   GOTO1 DATCON,DMCB,(0,WORK),(3,ENDOPT) SAVE END DATE                    
                                                                                
VRENDX   CLC   STRTOPT,ENDOPT      CHECK START LESS THEN END                    
         BH    DATERR                                                           
                                                                                
*        VALIDATE COMPETITIVE STATION                                           
                                                                                
         MVI   ERROR,INVALID                                                    
         MVI   COMPSTA,C'N'        DEFAULT                                      
                                                                                
         LA    R2,TITCSTH                                                       
         CLI   5(R2),0                                                          
         BE    VREQ50                                                           
         MVC   COMPSTA,TITCST                                                   
         CLI   TITCST,C'N'                                                      
         BE    VREQ50                                                           
         CLI   TITCST,C'Y'                                                      
         JNE   ERREND                                                           
                                                                                
VREQ50   BAS   RE,VALINVF                                                       
                                                                                
VALREQX  XIT1                                                                   
                                                                                
DATERR   MVC   RERROR,=AL2(INVDATE) ERROR - INVALID DATE                        
         GOTO1 MYERROR                                                          
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                    *          
*      VALINVF ROUTINE - VALIDATES INVENTORY FILTER FIELDS           *          
*                                                                    *          
**********************************************************************          
                                                                                
VALINVF  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   FLTRFLGS,0           CLEAR FILTER FLAGS                          
                                                                                
                                                                                
*     INIT FILTER HOLDERS AND POINTERS                                          
                                                                                
         LA    RE,RANGETAB                                                      
         ST    RE,RANGEPTR                                                      
         LHI   RF,(RANGETBX-RANGETAB)                                           
         XCEF                                                                   
                                                                                
         LA    RE,INVLIST                                                       
         ST    RE,LISTPTR                                                       
         LHI   RF,(INVLISTX-INVLIST)                                            
         XCEF                                                                   
                                                                                
         MVC   RERROR,=AL2(INVFILT) ERROR - INVALID FILTER                      
*                                                                               
*                                                                               
         CLI   TITINV1H+5,0        IF 1ST INV FILTER EMPTY                      
         BNE   VALIN10                                                          
         XC    TITINV2,TITINV2     THEN ENSURE OTHERS ARE CLEARED,              
         OI    TITINV2H+6,X'80'     TRANSMITTED,                                
         XC    TITINV3,TITINV3                                                  
         OI    TITINV3H+6,X'80'                                                 
         B     VALINVX              AND THEN EXIT                               
*                                  ELSE, BEGIN EXTRACTION                       
VALIN10  LA    R2,TITINV1H                                                      
         BAS   RE,DOINVSCN         SCAN 1ST LINE                                
                                                                                
         BAS   RE,EXTRINV         EXTRACT INVENTORY FILTERS                     
                                                                                
         CLI   TITINV2H+5,0       IF 2ND LINE EMPTY                             
         BNE   VALIN40                                                          
         XC    TITINV3,TITINV3    THEN ENSURE THIRD IS CLEARED,                 
         OI    TITINV3H+6,X'80'    TRANSMITTED                                  
         B     VALIN60             AND FINISH                                   
*                                 ELSE, CONTINUE                                
                                                                                
VALIN40  LA    R2,TITINV2H                                                      
         BAS   RE,DOINVSCN        SCAN 2ND LINE                                 
                                                                                
         BAS   RE,EXTRINV         EXTRACT INVENTORY FILTERS                     
                                                                                
         CLI   TITINV3H+5,0       IF LAST LINE EMPTY,                           
         BE    VALIN60            THEN FINISH UP                                
*                                 ELSE, CONTINUE                                
                                                                                
VALIN50  LA    R2,TITINV3H                                                      
         BAS   RE,DOINVSCN        SCAN 3RD (LAST) LINE                          
                                                                                
         BAS   RE,EXTRINV         EXTRACT INVENTORY NUMBERS                     
                                                                                
VALIN60  L     R1,LISTPTR         CURRENT LIST TABLE POSITION                   
         MVI   0(R1),X'FF'        MARK END OF TABLE                             
         L     R1,RANGEPTR        CURRENT RANGE TABLE POSITION                  
         MVI   0(R1),X'FF'        MARK END OF RANGE TABLE                       
*                                                                               
VALINVX  XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                    *          
*      EXTRINV ROUTINE - VALIDATES, AND EXTRACTS INVENTORY ITEMS     *          
*                        FOR LIST TABLE                              *          
**********************************************************************          
                                                                                
EXTRINV  NTR1  BASE=*,LABEL=*                                                   
         L     R1,LISTPTR           CURRENT FILTER TABLE POSITION               
         LA    R3,FLTRBLOK          SCANNER BLOCK (INV FLTR RESULTS)            
         ZIC   R4,#OFENTS           # OF ENTRIES TO EXTRACT                     
                                                                                
         USING SCANBLKD,R3                                                      
                                                                                
EXTR10   CLI   SC2NDLEN,0           POSSIBLE RANGE?                             
         BE    EXTR15                NO, TEST SINGLE ENTRY                      
         BAS   RE,VALRANGE           YES, CHECK IT OUT                          
         B     EXTR20                AND CONTINUE                               
                                                                                
EXTR15   CLI   SC1STLEN,4                                                       
         JNE   VALERROR             MUST BE A FOUR CHAR INPUT                   
         MVC   0(4,R1),SC1STFLD     MOVE IT INTO LIST                           
         OI    FLTRFLGS,LISTFLG     ENSURE LIST FILTER FLAG IS ON               
         LA    R1,4(R1)             BUMP LIST POINTER                           
                                                                                
EXTR20   LA    R3,L'FLTRBLOK(R3)    BUMP SCAN BLOCK POINTER                     
         BCT   R4,EXTR10            REPEAT FOR ALL ENTRIES                      
         DROP  R3                                                               
                                                                                
         ST    R1,LISTPTR           UPDATE CURRENT FLTR TABLE POSITON           
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                    *          
*      VALRANGE ROUTINE - VALIDATES INVENTORY RANGE FILTER           *          
*                         AND EXTRACTS BOUNDS VALUES                 *          
*                         (R3 -> CURRENT FILTER BLOCK ENTRY)         *          
*                                                                    *          
**********************************************************************          
                                                                                
VALRANGE NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R1,RANGEPTR                                                      
         USING SCANBLKD,R3                                                      
                                                                                
         CLI   SC1STLEN,4           1ST RANGE BOUND IS 4 CHAR?                  
         JNE   VALERROR                                                         
         MVC   0(4,R1),SC1STFLD     YES, MOVE IT IN                             
                                                                                
         CLI   SC2NDLEN,4           2ND RANGE BOUND IS 4 CHAR?                  
         JNE   VALERROR                                                         
         MVC   4(4,R1),SC2NDFLD     YES, MOVE IT IN                             
         DROP  R3                                                               
                                                                                
         CLC   0(4,R1),4(R1)        LOW VALUE < HIGH VALUE?                     
         JNL   VALERROR             NO, ERROR                                   
                                                                                
         OI    FLTRFLGS,RANGEFLG    ENSURE RANGE FLAG IS ON                     
         LA    R1,RANGELEN(R1)      BUMP RANGE TABLE POINTER                    
         ST    R1,RANGEPTR          STORE IT                                    
                                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                    *          
*      DOINVSCN ROUTINE - SCANS INVENTORY FILTER LINES               *          
*        R2 MUST POINT TO SCREEN FLD HEADER OF LINE TO SCAN          *          
*                                                                    *          
**********************************************************************          
                                                                                
DOINVSCN NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,CLRBLK           CLEAR BLOCK FOR SCANNER                      
         GOTO1 SCANNER,DMCB,(R2),FLTRBLOK,C',=,-'                               
         CLI   DMCB+4,0            ERROR FOUND?                                 
         JE    VALERROR                                                         
         MVC   #OFENTS,DMCB+4      STORE # OF ENTRIES                           
                                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                    *          
*      CLRBLK ROUTINE - CLEARS SCANNER BLOCK                         *          
*                                                                    *          
**********************************************************************          
                                                                                
CLRBLK   NTR1  BASE=*,LABEL=*                                                   
         LA    RE,FLTRBLOK                                                      
         LHI   RF,(MAXOPTS*L'FLTRBLOK)                                          
         XCEF                                                                   
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                    *          
*    REPMODE ROUTINE -- EDIT THE REQUEST SCREEN                      *          
*                                                                    *          
**********************************************************************          
                                                                                
REPMODE  NTR1  BASE=*,LABEL=*                                                   
*        GOTO1 =V(PRNTBL),DMCB,=C'REP',AIO3,C'DUMP',700,=C'1D'                  
*                                                                               
         MVI   INVFLGS,0           INIT INVENTORY RECORD FLAGS                  
                                                                                
         L     R5,AIO1                                                          
         USING REINVREC,R5                                                      
*                                                                               
         BAS   RE,BLDHEAD          HEADER RECORD                                
         BAS   RE,BLDDPT           DAYPART RECORD                               
*                                                                               
RPM40    BAS   RE,GTINV            READ INVENTORY RECORD                        
         CLI   KEY,X'FF'                                                        
         BE    RPMEX                                                            
*                                                                               
RPM60    CLI   KEY+24,0                                                         
         BNE   RPM100                                                           
         BAS   RE,BLDINVH          INV HEADER RECORD                            
         B     RPM40                                                            
*                                                                               
RPM100   CLI   KEY+24,X'FF'                                                     
         BNE   RPM200                                                           
         BAS   RE,BLDCOMM          COMMENT RECORD                               
         B     RPM40                                                            
*                                                                               
RPM200   CLI   KEY+24,C'M'                                                      
         BE    RPM40                                                            
         CLI   KEY+24,C'S'                                                      
         BE    RPM40                                                            
         BAS   RE,BLDSRVY          SURVEY RECORD                                
         BAS   RE,BLDOVR           OVERRIDE RECORD                              
         B     RPM40                                                            
*                                                                               
RPMEX    BRAS  RE,BLDTRL           TRAILER                                      
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                               
*        GTINV ROUTINE --READ AND FILTER INVENTORY RECORDS                      
*                                                                               
**********************************************************************          
                                                                                
GTINV    NTR1  BASE=*,LABEL=*                                                   
         LA    R4,KEY                                                           
         USING REINVREC,R4                                                      
                                                                                
         CLI   KEYSAVE,X'12'       FIRST TIME ?                                 
         BE    GTINV100            NO, GET NEXT RECORD                          
                                                                                
         XC    KEY,KEY             YES, BUILD INITIAL KEY                       
                                                                                
         MVI   RINVKTYP,X'12'      INVENTORY TYPE                               
         MVC   RINVKREP,AGENCY     PLACE CURRENT ALPHA CODE                     
         MVC   RINVKSTA,CSTAT      PLACE REQUEST STATION                        
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(17),KEYSAVE     CHECK UP TO STATION                          
         BE    GTINV300                                                         
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     GTINVX                                                           
*                                                                               
*  GET NEXT RECORD                                                              
*                                                                               
                                                                                
GTINV100 GOTO1 SEQ                 GET NEXT RECORD                              
         CLC   KEY(17),KEYSAVE     CHECK UP TO STATION                          
         BE    *+12                                                             
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     GTINVX                                                           
                                                                                
         CLI   RINVKSRC,0          IS RECORD A HEADER                           
         BE    GTINV300             YES, DO HEADER LOGIC                        
                                                                                
         GOTO1 GETREC               NO, GET RECORD                              
         B     GTINVX                   AND EXIT                                
                                                                                
*                                                                               
*  HEADER LOGIC                                                                 
*                                                                               
                                                                                
GTINV300 BAS   RE,CHKFILT          CHK INV FILTERS (SETS CC)                    
         BNE   GTINV400            EXCLUDE                                      
                                                                                
         GOTO1 GETREC              INCLUDE, GET RECORD                          
         B     GTINVX                       AND EXIT                            
                                                                                
*                                                                               
*  BUILD EXCLUSION RECORD AND READ TO NEXT HEADER                               
*                                                                               
GTINV400 MVC   KEYHOLD,KEY         SAVE CURRENT KEY                             
         BAS   RE,BLDEXCL          BUILD THE EXCLUSION RECORD                   
         XC    KEY,KEY                                                          
         MVC   KEY(27),KEYHOLD     RESTORE KEY                                  
                                                                                
GTINV425 ZICM  R5,RINVKSTD,3       TAKE KEY EFFECTIVE DATE                      
         AHI   R5,1                BUMP IT UP A NOTCH                           
         STCM  R5,7,RINVKSTD       PUT BACK IN KEY                              
                                                                                
         XC    KEY+24(3),KEY+24    CLEAR REST OF KEY                            
                                                                                
         GOTO1 HIGH                FIND NEXT PROSPECT                           
                                                                                
GTINV440 CLC   KEY(17),KEYSAVE     MATCH UP TO STATION?                         
         BE    GTINV450            YES, CONTINUE                                
         MVI   KEY,X'FF'           NO, END OF LOOKUP                            
         B     GTINVX                                                           
                                                                                
GTINV450 CLI   RINVKSRC,X'00'      IS HEADER? (SHOULD BE)                       
         BE    GTINV300            YES, DO HEADER LOGIC                         
         B     GTINV425            NO,  TRY AGAIN                               
         DROP  R4                                                               
******                                                                          
*   THE PRECEDING BRANCH SHOULD NOT OCCUR,BECAUSE IT WOULD                      
*   INDICATE THAT THERE ARE CHILD RECORDS (IE: TRACKS, TEXT)                    
*   WITHOUT AN ASSOCIATED HEADER.  IF WE DO SEE ONE THOUGH, WE                  
*   WILL JUST SKIP IT AS OPPOSED TO DUMPING BECAUSE IT WOULD                    
*   BE TOO MUCH TROUBLE TO MANUALLY DELETE THESE EACH TIME.                     
*    !!!!!MUST WRITE A FILE FIX TO DO IT AT SOME POINT                          
*         (11/06/00 -FJD)                                                       
*******                                                                         
                                                                                
                                                                                
GTINVX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*        CHKFILT ROUTINE -- CHECK CURRENT INV HEADER AGAINST FILTERS            
*                           (IF PRESENT) AND RETURN CC SET                      
*                                                                               
**********************************************************************          
                                                                                
CHKFILT  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
                                                                                
****                                                                            
*      CHECK EFFECTIVE DATE FILTERS FIRST (LESS PROCESSING OVERHEAD)            
****                                                                            
                                                                                
*            IF INV EFF STRT DATE LOWER THAN START FILTER,                      
         CLC   RINVKSTD,STRTOPT                                                 
         BL    FILTEXC                  THEN, EXCLUDE THIS INVENTORY            
*                                       ELSE, CONTINUE                          
                                                                                
*            IF INV EFF STRT DATE HIGHER THAN END FILTER,                       
         CLC   RINVKSTD,ENDOPT                                                  
         BH    FILTEXC                  THEN, EXCLUDE THIS INVENTORY            
*                                       ELSE, CONTINUE                          
                                                                                
****                                                                            
*      NOW CHECK FOR INVENTORY NUMBER FILTERS                                   
****                                                                            
                                                                                
         TM    FLTRFLGS,LISTFLG+RANGEFLG ANY INVENTORY# FILTERS?                
         BZ    FILTINC                   NO, INCLUDE THIS ITEM                  
                                                                                
*      CHECK AGAINST THE LIST                                                   
                                                                                
         LA      R3,INVLIST                                                     
CHKFLT10 CLI     0(R3),X'FF'        END OF LIST?                                
         BE      CHKFLT20                YES, CHECK RANGES                      
         CLC     RINVKINV,0(R3)          NO, IS THERE A MATCH IN LIST?          
         BE      FILTINC                   YES, INCLUDE ITEM                    
         LA      R3,4(R3)                  NO, CHECK NEXT ENTRY                 
         B       CHKFLT10                                                       
                                                                                
*      CHECK AGAINST THE RANGE(S)                                               
                                                                                
CHKFLT20 LA      R3,RANGETAB                                                    
CHKFLT40 CLI     0(R3),X'FF'             END OF TABLE?                          
         BE      FILTEXC                 YES, NO MATCH FOUND, EXCLUDE           
         CLC     RINVKINV,0(R3)          CHECK AGAINST ENTRY LOW RANGE          
         BL      CHKFLT60                IF LOWER, CHK WITH NEXT ENTRY          
         CLC     RINVKINV,4(R3)          CHECK AGAINST ENTRY HIGH RANGE         
         BL      FILTINC                 IF LOWER, IT'S WITHIN RANGE            
CHKFLT60 LA      R3,RANGELEN(R3)         ELSE, CHECK NEXT TABLE ENTRY           
         B       CHKFLT40                                                       
                                                                                
FILTEXC  LA      R1,1                    SET CC != (FOR EXCLUDE)                
         B       *+6                                                            
FILTINC  SR      R1,R1                   SET CC  = (FOR INCLUDE)                
         LTR     R1,R1                                                          
         XIT1                                                                   
                                                                                
         DROP    R4                                                             
         LTORG                                                                  
         DROP    RB                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                               
*        BLDHEAD ROUTINE --BUILD THE HEADER RECORD                              
*                                                                               
**********************************************************************          
                                                                                
BLDHEAD  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING HDREC,R3                                                         
         XC    0(250,R3),0(R3)                                                  
                                                                                
         LA    RE,28                                                            
         STCM  RE,3,RECLEN                                                      
                                                                                
         MVC   HDID,=CL2'H1'                                                    
         MVC   HDSRC,=CL4'DDS '                                                 
****     MVC   HDVER,=CL2'02'     VERSION 02 AS OF 11/03/00                     
         MVC   HDVER,=CL2'03'     VERSION 03 AS OF 03/18/03                     
         MVC   HDREP,AGENCY                                                     
         GOTO1 DATCON,DMCB,(5,DUB),(0,HDSVDAT)                                  
         TIME  DEC                                                              
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,DUB,4,=C'TOG'                                   
         MVC   HDSVTIM(2),DUB                                                   
         MVI   HDSVTIM+2,C':'                                                   
         MVC   HDSVTIM+3(2),DUB+2                                               
         MVI   HDSVTIM+5,C':'                                                   
         MVC   HDSVTIM+6(2),DUB+4                                               
         BRAS  RE,WRITREC                                                       
BLDHX    XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
                                                                                
**********************************************************************          
*                                                                               
*        BLDDPT ROUTINE --BUILD THE DAYPART RECORDS                             
*                                                                               
**********************************************************************          
                                                                                
BLDDPT   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,KEY                                                           
         USING RRDPRECD,R5                                                      
*                                                                               
*  READ THE DAYPART RECORDS                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVI   RRDPKTYP,X'3C'                                                   
         MVC   RRDPKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         B     BDP100                                                           
*                                                                               
BDP80    GOTO1 SEQ                                                              
*                                                                               
BDP100   CLC   KEY(26),KEYSAVE                                                  
         BNE   BDPEX                                                            
         MVC   AIO,AIO1                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING DPTREC,R3                                                        
         XC    0(250,R3),0(R3)                                                  
*                                                                               
         LA    RE,27                                                            
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   DPID,=CL2'D1'                                                    
         MVC   DPCODE,RRDPKDPT                                                  
         MVC   DPSNAME(3),RRDPSNAM                                              
         MVC   DPLNAME,RRDPLNAM                                                 
         OI    DPSNAME+3,X'40'                                                  
         OI    DPLNAME+15,X'40'                                                 
         BAS   RE,WRITREC                                                       
         B     BDP80                                                            
*                                                                               
BDPEX    XIT1                                                                   
         DROP  R3,R5                                                            
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*        BLDINVH ROUTINE --BUILD THE INVENTORY RECORD                           
*                                                                               
**********************************************************************          
                                                                                
BLDINVH  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING IMREC,R3                                                         
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    RE,457                                                           
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   IMID,=CL2'I1'                                                    
         MVC   IMSTA,RINVKSTA                                                   
         CLI   IMSTA+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   IMSTA+4,X'40'                                                    
         MVC   IMINVN,RINVKINV                                                  
         MVC   IMDYPTS,RINVDP                                                   
         MVC   IMFLTER,RINVPFLT                                                 
         MVC   IMMARKET,CMKTNAM                                                 
         OC    IMMARKET,SPACES                                                  
         MVC   IMPOWER,RINVKREP                                                 
         MVC   IMCOMPST,COMPSTA    COMP STATION INDICATOR                       
         CLI   RINVTCHG,C'S'                                                    
         BNE   *+10                                                             
         MVC   IMTIMCHG,=CL3'+60'                                               
         CLI   RINVTCHG,C'F'                                                    
         BNE   *+10                                                             
         MVC   IMTIMCHG,=CL3'-60'                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(0,IMEFFS)                              
         OC    IMEFFE,SPACES                                                    
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    BDI100                                                           
         LA    R4,RINVPEFF+2                                                    
         GOTO1 DATCON,DMCB,(2,(R4)),(0,IMEFFE)                                  
*                                                                               
*  PEPARE FOR THE YEAR 2000                                                     
         CLI   IMEFFS,X'FA'                                                     
         BL    BDI50                                                            
         ZIC   RE,IMEFFS                                                        
         SH    RE,=H'10'                                                        
         STCM  RE,1,IMEFFS                                                      
*                                                                               
BDI50    CLI   IMEFFE,X'FA'                                                     
         BL    BDI100                                                           
         ZIC   RE,IMEFFE                                                        
         SH    RE,=H'10'                                                        
         STCM  RE,1,IMEFFE                                                      
*                                                                               
*  DAY/TIME LOGIC                                                               
*                                                                               
BDI100   MVI   IMDAY1,X'40'        BLANK OUT THE FIELD                          
         MVC   IMDAY1+1(175),IMDAY1                                             
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'04',AIO),0                          
         CLI   12(R1),0                                                         
         BE    BDI300              DO AVAIL MOVE                                
*                                                                               
*  FILL DAY/TIME FIELDS FROM REGULART DAY/TIME ELEMENTS                         
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,12(R1)                                                        
         LA    R6,IMDAY1                                                        
BDI200   GOTO1 UNDAY,DMCB,2(R4),WORK              DAY                           
         MVC   0(11,6),WORK                                                     
         GOTO1 UNTIME,DMCB,3(R4),(0,11(R6))       TIME                          
         OC    0(22,R6),SPACES     BLANK FILL THE FIELDS                        
         OC    5(2,R4),5(R4)                                                    
         BNZ   BDI260                                                           
*  IF NO END TIME MOVE A ,B AFTER THE START                                     
         LA    RE,11(R6)                                                        
         LA    RF,11                                                            
BDI220   CLI   0(RE),X'40'                                                      
         BNH   BDI240                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,BDI220                                                        
         DC    H'0'                                                             
BDI240   MVC   0(2,RE),=C',B'                                                   
*                                                                               
BDI260   LA    R6,22(R6)                                                        
*        SEE IF MORE AVAIL ADY TIME ELEMS EXIST                                 
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),2                                                          
         BNE   BDI350                                                           
         B     BDI200                                                           
*                                                                               
*  FILL DAY/TIME FIELDS FROM AVAIL INFO                                         
*                                                                               
BDI300   L     R4,12(R1)                                                        
         LA    R6,IMDAY1                                                        
BDI320   MVC   0(22,R6),2(R4)                                                   
         OC    0(22,R6),=22X'40'                                                
         LA    R6,22(R6)                                                        
*        SEE IF MORE AVAIL ADY TIME ELEMS EXIST                                 
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),4                                                          
         BNE   BDI350                                                           
         B     BDI320                                                           
*                                                                               
*  SAVE DAY TIME FOR TRACKS                                                     
*                                                                               
BDI350   XC    DAYTMHD,DAYTMHD                                                  
         LA    R6,DAYTMHD                                                       
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
*  SAVE DAY TIME                                                                
BDI370   MVC   0(5,R6),2(R4)                                                    
*                                                                               
*        SEE IF MORE DAY TIME ELEMENTS                                          
*                                                                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),2                                                          
         BNE   BDI400                                                           
         B     BDI370                                                           
*                                                                               
*  PROGRAM NAME LOGIC                                                           
*                                                                               
BDI400   MVI   IMPROG1,X'40'       BLANK OUT THE FIELD                          
         MVC   IMPROG1+1(215),IMPROG1                                           
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*  SAVE FIRST PROGRAM LINE FOR THE TRACK RECORDS                                
         L     R4,12(R1)                                                        
*                                                                               
         MVC   SAVPROG,SPACES                                                   
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVPROG(0),2(R4)                                                 
*                                                                               
         LA    R6,IMPROG1                                                       
BDI440   ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     BDI460                                                           
         MVC   0(0,R6),2(R4)                                                    
*                                                                               
BDI460   OC    0(27,R6),SPACES                                                  
         LA    R6,27(R6)                                                        
*        SEE IF MORE PROG NAME ELEMENTS EXIST                                   
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),3                                                          
         BNE   BDIEX                                                            
         B     BDI440                                                           
*                                                                               
BDIEX    BAS   RE,WRITREC                                                       
         XIT1                                                                   
         DROP  R3,R5                                                            
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                               
*        BLDEXCL ROUTINE --BUILD THE EXCLUSION RECORD                           
*                          (TELL TAPSCAN THAT THIS RECORD IS STILL              
*                           ON FILE, BUT NO CHANGES ARE TO BE MADE)             
*                                                                               
**********************************************************************          
                                                                                
BLDEXCL  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO3               BUILD THIS REC IN IO3                      
         LA    R3,4(R3)                                                         
*                                                                               
         L     RE,AIO3               CLEAR IO3 FOR BUILD                        
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         USING EXCREC,R3                                                        
                                                                                
         LA    R5,KEY                WE'VE ONLY GOT THE KEY                     
         USING RINVKEY,R5             (IT'S ALL WE NEED NOW)                    
                                                                                
         LHI   R0,21                 LENGTH OF E1 REC BASE + 4                  
                                                                                
         MVC   EXCID,=CL2'E1'        RECORD ID                                  
         MVC   EXCSTA,RINVKSTA       STATION CALL LETTERS                       
         CLI   EXCSTA+4,C'T'         T FOR 'TV'                                 
         BNE   *+8                                                              
         MVI   EXCSTA+4,X'40'          ELSE, BLANK                              
         MVC   EXCINVN,RINVKINV      INVENTORY NUMBER                           
                                                                                
*             GET EFFECTIVE START DATE FROM KEY                                 
                                                                                
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(0,EXCEFFS)                             
         DROP  R3                                                               
*                                                                               
*        NOW APPEND INFO FOR VARIABLE NUMBER OF BOOKS                           
*                                                                               
         LA    R3,17(R3)             CURRENT END OF EX1 REC                     
                                                                                
BLDEX020 GOTO1 SEQ                                                              
         CLC   KEY(24),KEYSAVE       SAME ITEM/EFF DATE?                        
         BNE   BLDEXWRT               NO, WRITE EX1 RECORD                      
         CLI   RINVKSRC,C'M'         IGNORE MARKET FACT                         
         BE    BLDEX020                                                         
         CLI   RINVKSRC,C'S'         IGNORE STATION FACT                        
         BE    BLDEX020                                                         
         CLI   RINVKSRC,X'FF'        IGNORE "RATIONAL" RECORDS                  
         BE    BLDEX020                                                         
                                                                                
         XC    WORK(20),WORK         NOW DERIVE BOOK INFO FROM SOURCE           
         MVC   WORK+2(1),RINVKSRC                                               
         GOTO1 VGETKSRC,DMCB,(C'K',WORK),WORK+10                                
         CLI   DMCB+4,0               VALID KSRC?                               
         BNE   BLDEX020                  NO, DON'T USE THIS RECORD              
                                                                                
         USING EXCBOOK,R3                                                       
BLDEX060 MVC   EXCSRVY,RINVKBK        BOOK YY/MM                                
         MVC   EXCPRFX,WORK+11        LOOKUP TYPE (BOOK PREFIX)                 
         MVC   EXCBKTYP,WORK+14       BOOK TYPE                                 
         CLI   EXCBKTYP,0             IF NO BOOK TYPE                           
         BNE   *+8                                                              
         MVI   EXCBKTYP,X'40'          THEN, INSERT SPACE CHAR                  
         DROP  R3,R5                                                            
                                                                                
         LA    R3,4(R3)               POINT R3 AT NEW END OF EX1 REC            
         AHI   R0,4                   INCREASE RECORD SIZE HOLDER               
         B     BLDEX020               AND REPEAT LOOP                           
                                                                                
BLDEXWRT STCM  R0,3,RECLEN            STORE ADJUSTED RECORD LENGTH              
         BAS   RE,WRITREC             AND WRITE EX RECORD                       
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*        BLDSRVY ROUTINE --BUILD THE SURVEY RECORD                              
*                                                                               
**********************************************************************          
                                                                                
BLDSRVY  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING SRVREC,R3                                                        
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    RE,750                                                           
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   SRVID,=C'S1'                                                     
         MVC   SRVSTA,RINVKSTA                                                  
         CLI   SRVSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   SRVSTA+4,X'40'                                                   
         MVC   SRVINV,RINVKINV                                                  
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(0,SRVSEFF)                             
         MVC   SRVBOOK,RINVKBK                                                  
                                                                                
         XC    WORK(20),WORK                                                    
         MVC   WORK+2(1),RINVKSRC                                               
         GOTO1 VGETKSRC,DMCB,(C'K',WORK),WORK+10                                
         CLI   DMCB+4,0            VALID KSRC?                                  
         BE    BLDSR060               YES, CONTINUE                             
         B     BLDSRX                 NO, DON'T WRITE RECORD                    
*                                                                               
BLDSR060 MVC   SRVSRC,WORK+10      SERVICE                                      
         MVC   SRVLOOK,WORK+11     LOOKUP TYPE                                  
         MVC   SRVBTYP,WORK+14     BOOK TYPE                                    
         CLI   SRVBTYP,0                                                        
         BNE   *+8                                                              
         MVI   SRVBTYP,X'40'                                                    
*                                                                               
         MVC   SRVPROG,SAVPROG     GET DEFAULT PROGRAM NAME                     
         CLI   RINVKSRC,C'O'       IF PROJECTED                                 
         BE    BLDSR100            US DEFAULT                                   
*                                                                               
         MVC   SRVPROG,SPACES                                                   
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'01',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDSR070                                                         
         L     R4,12(R1)                                                        
         MVC   SRVPROG(16),15(R4)                                               
*                                                                               
BLDSR070 MVC   SRVBRCD,=CL2'PV'                                                 
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'CD',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDSR080                                                         
         L     R4,12(R1)                                                        
         CLI   2(R4),X'40'                                                      
         BE    BLDSR080                                                         
         MVC   SRVBRCD(2),2(R4)                                                 
                                                                                
*                                                                               
BLDSR080 XC    SRVFRBK(9),SRVFRBK                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDSR090                                                         
         L     R4,12(R1)                                                        
         USING RINVFREL,R4                                                      
         MVC   SRVFRBK,RINVFRBK+1                                               
         MVC   SRVFRTP,RINVFRTY                                                 
         MVC   SRVFRFN,RINVFRPR                                                 
         MVC   SRVFRST,RINVFRST                                                 
         DROP  R4                                                               
*                                                                               
BLDSR090 MVC   SRVUPGF,SPACES                                                   
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'05',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDSR100                                                         
         L     R4,12(R1)                                                        
         GOTO1 =V(UPOUT),DMCB,(R4),UPGRADST                                     
         MVC   SRVUPGF,UPGRADST                                                 
         OC    SRVUPGF,SPACES                                                   
*                                                                               
*  FILL DAY/TIME FIELDS FROM REGULART DAY/TIME ELEMENTS                         
*                                                                               
BLDSR100 MVI   SRVDAY1,X'40'       BLANK OUT THE FIELD                          
         MVC   SRVDAY1+1(175),SRVDAY1                                           
                                                                                
         OC    DAYTMHD+1(4),DAYTMHD+1                                           
         BNZ   *+6                                                              
         DC    H'0'                MUST BE 1 DAY TIME                           
         LA    R4,DAYTMHD                                                       
         LA    R6,SRVDAY1                                                       
BLDSR140 XC    WORK(11),WORK                                                    
         GOTO1 UNDAY,DMCB,0(R4),WORK              DAY                           
         MVC   0(11,6),WORK                                                     
         GOTO1 UNTIME,DMCB,1(R4),(0,11(R6))       TIME                          
         OC    0(22,R6),SPACES     BLANK FILL THE FIELDS                        
         LA    R6,22(R6)                                                        
                                                                                
         LA    R4,5(R4)                                                         
         OC    1(4,R4),1(R4)                                                    
         BNZ   BLDSR140                                                         
*                                                                               
* MOVE THE DEMOS OUT                                                            
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         XC    MYDSPILL,MYDSPILL                                                
         MVC   DB.DBFILE,=C'INV'                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVC   DB.DBAREC,AIO                                                    
         L     R1,DB.DBAREC                                                     
         LA    R1,34(R1)           POINT TO FIRST ELEMENT POSITION              
         ST    R1,DB.DBAQUART                                                   
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DB.DBEXTEND                                                
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
*                                                                               
*  MOVE OUT THE STATION LEVEL DEMOS                                             
*                                                                               
         SPACE 1                                                                
         GOTO1 DEMOUT,DMCB,(C'L',STADEMS),MYDBLOCK,AIO2                         
         SPACE 1                                                                
*                                                                               
*  DO THE RATINGS                                                               
*                                                                               
         L     RE,AIO2                                                          
         LA    RF,33                                                            
         LA    R1,STPRAT                                                        
*                                                                               
BLDSR300 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR300                                                      
*                                                                               
*  DO THE IMPS                                                                  
*                                                                               
         LA    RF,34                                                            
         LA    R1,STPIMP                                                        
*                                                                               
BLDSR350 MVC   0(4,R1),0(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   RF,BLDSR350                                                      
*                                                                               
*  DO THE VARIOUS DEMOS                                                         
*                                                                               
         LA    RF,9                                                             
         LA    R1,STHLDRTG                                                      
*                                                                               
BLDSR400 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR400                                                      
*                                                                               
*  MOVE OUT THE MARKET LEVEL DEMOS                                              
*                                                                               
         SPACE 1                                                                
         GOTO1 DEMOUT,DMCB,(C'L',MKTDEMS),MYDBLOCK,AIO2                         
         SPACE 1                                                                
*                                                                               
*  DO THE RATINGS                                                               
*                                                                               
         L     RE,AIO2                                                          
         LA    RF,33                                                            
         LA    R1,MKPRAT                                                        
*                                                                               
BLDSR500 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR500                                                      
*                                                                               
*  DO THE IMPS                                                                  
*                                                                               
         LA    RF,34                                                            
         LA    R1,MKPIMP                                                        
*                                                                               
BLDSR550 MVC   0(4,R1),0(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   RF,BLDSR550                                                      
*                                                                               
*  DO THE VARIOUS DEMOS                                                         
*                                                                               
         LA    RF,9                                                             
         LA    R1,MKHLDRTG                                                      
*                                                                               
BLDSR600 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR600                                                      
*                                                                               
* SET MARKET SHARE VALUES TO 100                                                
*                                                                               
         MVC   MKHLDSHR,=F'1000'                                                
         MVC   MKMTASHR,=F'1000'                                                
         MVC   MKMTBSHR,=F'1000'                                                
*                                                                               
         BAS   RE,WRITREC                                                       
BLDSRX   XIT1                                                                   
         DROP  R3,R5                                                            
         EJECT                                                                  
*********************************************************************           
*                                                                   *           
*        TYPTAB                                                     *           
*                                                                   *           
*********************************************************************           
TYPTAB   DC    X'C1',CL2'MA'      SOURCE = A                                    
         DC    X'09',CL2'MA'                                                    
         DC    X'0A',CL2'MA'                                                    
         DC    X'0B',CL2'MA'                                                    
         DC    X'0C',CL2'MA'                                                    
         DC    X'0D',CL2'MA'                                                    
         DC    X'0E',CL2'MA'                                                    
         DC    X'C2',CL2'MP'      SOURCE = B                                    
         DC    X'11',CL2'MP'                                                    
         DC    X'12',CL2'MP'                                                    
         DC    X'13',CL2'MP'                                                    
         DC    X'14',CL2'MP'                                                    
         DC    X'15',CL2'MP'                                                    
         DC    X'16',CL2'MP'                                                    
         DC    X'C3',CL2'MT'      SOURCE = C                                    
         DC    X'19',CL2'MT'                                                    
         DC    X'1A',CL2'MT'                                                    
         DC    X'1B',CL2'MT'                                                    
         DC    X'1C',CL2'MT'                                                    
         DC    X'1D',CL2'MT'                                                    
         DC    X'1E',CL2'MT'                                                    
         DC    X'C4',CL2'MS'      SOURCE = D                                    
         DC    X'21',CL2'MS'                                                    
         DC    X'22',CL2'MS'                                                    
         DC    X'23',CL2'MS'                                                    
         DC    X'24',CL2'MS'                                                    
         DC    X'25',CL2'MS'                                                    
         DC    X'26',CL2'MS'                                                    
         DC    X'C5',CL2'ME'      SOURCE = E                                    
         DC    X'29',CL2'ME'                                                    
         DC    X'2A',CL2'ME'                                                    
         DC    X'2B',CL2'ME'                                                    
         DC    X'2C',CL2'ME'                                                    
         DC    X'2D',CL2'ME'                                                    
         DC    X'2E',CL2'ME'                                                    
         DC    X'D5',CL2'NA'      SOURCE = N                                    
         DC    X'31',CL2'NA'                                                    
         DC    X'32',CL2'NA'                                                    
         DC    X'33',CL2'NA'                                                    
         DC    X'34',CL2'NA'                                                    
         DC    X'35',CL2'NA'                                                    
         DC    X'36',CL2'NA'                                                    
         DC    X'D6',CL2'NP'      SOURCE = M                                    
         DC    X'39',CL2'NP'                                                    
         DC    X'3A',CL2'NP'                                                    
         DC    X'3B',CL2'NP'                                                    
         DC    X'3C',CL2'NP'                                                    
         DC    X'3D',CL2'NP'                                                    
         DC    X'3E',CL2'NP'                                                    
         DC    X'D7',CL2'NM'      SOURCE = O                                    
         DC    X'41',CL2'NM'                                                    
         DC    X'42',CL2'NM'                                                    
         DC    X'43',CL2'NM'                                                    
         DC    X'44',CL2'NM'                                                    
         DC    X'45',CL2'NM'                                                    
         DC    X'46',CL2'NM'                                                    
         DC    X'D8',CL2'NS'      SOURCE = P                                    
         DC    X'49',CL2'NS'                                                    
         DC    X'4A',CL2'NS'                                                    
         DC    X'4B',CL2'NS'                                                    
         DC    X'4C',CL2'NS'                                                    
         DC    X'4D',CL2'NS'                                                    
         DC    X'4E',CL2'NS'                                                    
         DC    X'D9',CL2'NE'      SOURCE = Q                                    
         DC    X'51',CL2'NE'                                                    
         DC    X'52',CL2'NE'                                                    
         DC    X'53',CL2'NE'                                                    
         DC    X'54',CL2'NE'                                                    
         DC    X'55',CL2'NE'                                                    
         DC    X'56',CL2'NE'                                                    
         DC    X'E3',CL2'SA'      SOURCE = T                                    
         DC    X'59',CL2'SA'                                                    
         DC    X'5A',CL2'SA'                                                    
         DC    X'5B',CL2'SA'                                                    
         DC    X'5C',CL2'SA'                                                    
         DC    X'5D',CL2'SA'                                                    
         DC    X'5E',CL2'SA'                                                    
         DC    X'E4',CL2'SP'      SOURCE = U                                    
         DC    X'61',CL2'SP'                                                    
         DC    X'62',CL2'SP'                                                    
         DC    X'63',CL2'SP'                                                    
         DC    X'64',CL2'SP'                                                    
         DC    X'65',CL2'SP'                                                    
         DC    X'66',CL2'SP'                                                    
         DC    X'E7',CL2'SE'      SOURCE = X                                    
         DC    X'69',CL2'SE'                                                    
         DC    X'6A',CL2'SE'                                                    
         DC    X'6B',CL2'SE'                                                    
         DC    X'6C',CL2'SE'                                                    
         DC    X'6D',CL2'SE'                                                    
         DC    X'6E',CL2'SE'                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                               
*        BLDOVR ROUTINE --BUILD THE OVERRIDE RECORD                             
*                                                                               
**********************************************************************          
                                                                                
BLDOVR   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING OVRREC,R3                                                        
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    RE,33                                                            
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   OVRID,=C'O1'                                                     
         MVC   OVRSTA,RINVKSTA                                                  
         CLI   OVRSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   OVRSTA+4,X'40'                                                   
         MVC   OVRINV,RINVKINV                                                  
         PRINT GEN                                                              
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(0,OVRSEFF)                             
         PRINT NOGEN                                                            
*                                                                               
*  SET THE DBLOCK FOR DEMOCON                                                   
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         XC    MYDSPILL,MYDSPILL                                                
         MVC   DB.DBFILE,=C'INV'                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'U'                                                 
*                                                                               
*  GET OVERRIDE ELEMENTS                                                        
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'DE',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDOVEX                                                          
*                                                                               
         L     R4,12(R1)                                                        
*                                                                               
BLDOV440 CLI   1(R4),12                                                         
         BNE   BLDOV460                                                         
*                                                                               
         XC    DUB,DUB                                                          
         GOTO1 DEMOCON,DMCB,(0,3(R4)),(6,DUB),MYDBLOCK                          
         MVC   OVRCAT,DUB                                                       
         CLI   DUB,C'R'                                                         
         BNE   *+10                                                             
         MVC   OVRCAT,DUB+1                                                     
         OC    OVRCAT,SPACES                                                    
         MVI   OVRVLTYP,C'R'                                                    
         CLI   4(R4),C'R'                                                       
         BE    BLDOV450                                                         
         MVI   OVRVLTYP,C'S'                                                    
         CLI   4(R4),C'S'                                                       
         BE    BLDOV450                                                         
         MVI   OVRVLTYP,C'P'                                                    
         CLI   4(R4),C'P'                                                       
         BE    BLDOV450                                                         
         MVI   OVRVLTYP,C'I'                                                    
BLDOV450 MVC   OVRDEMO,8(R4)                                                    
*                                                                               
         BAS   RE,WRITREC                                                       
*        SEE IF MORE OVERRIDE ELEMENTS EXIST                                    
BLDOV460 ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),X'DE'                                                      
         BE    BLDOV440                                                         
*                                                                               
BLDOVEX  XIT1                                                                   
         DROP  R3,R5                                                            
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                               
*        BLDCOMM ROUTINE --BUILD THE COMMENT RECORD                             
*                                                                               
**********************************************************************          
                                                                                
BLDCOMM  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING CMREC,R3                                                         
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVC   CMID,=CL2'C1'                                                    
         MVC   CMSTA,RINVKSTA                                                   
         CLI   CMSTA+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   CMSTA+4,X'40'                                                    
         MVC   CMINVN,RINVKINV                                                  
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(0,CMEFFS)                              
         EDIT  (2,RINVKTXT),(3,CMTXTN),FILL=0                                   
*  GET FILTER INFORMATION                                                       
         XC    CMBOOK(3),CMBOOK                                                 
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDCM50                                                          
*                                                                               
         L     R6,12(R1)                                                        
         USING RINVFEL,R6                                                       
*                                                                               
         MVC   CMBOOK,RINVFBK                                                   
*  GET BOOK TYPE                                                                
         LA    R1,SVCLST                                                        
BLDCM20  CLC   RINVFBKT,3(R1)                                                   
         BE    BLDCM30                                                          
         LA    R1,L'SVCLST(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   BLDCM20                                                          
         DC    H'0'                                                             
BLDCM30  MVC   CMBKPFX,1(R1)                                                    
         CLI   1(R1),C' '                                                       
         BNE   BLDCM40                                                          
         MVI   CMBKPFX,C'A'                                                     
                                                                                
BLDCM40  MVC   CMBKTYP,RINVFBTP    ATTACH TYPE (IE: H=HISPANIC)                 
         CLI   CMBKTYP,0           NULL?                                        
         BNE   BLDCM50             NO,  KEEP AS IS                              
         MVI   CMBKTYP,C' '        YES, REPLACE WITH SPACE CHAR                 
                                                                                
*                                                                               
*  MOVE TEXT OUT                                                                
*                                                                               
BLDCM50  LA    R4,CMTXT1                                                        
         L     R6,AIO                                                           
         LA    R6,34(R6)           POINT R6 TO FIRST ELEMENT                    
         USING RINVTEL,R6                                                       
*                                                                               
         LA    RF,28               RECORD LENGTH                                
*                                                                               
BLDCM100 CLI   0(R6),1                                                          
         BNE   BLDCMEX                                                          
         XC    0(60,R4),0(R4)      CLEAR THE BUFFER OUT                         
         ZIC   R1,RINVTLEN                                                      
         SH    R1,=H'7'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),RINVTEXT                                                 
         OC    0(60,R4),SPACES     SPACE FILL                                   
         LA    R4,60(R4)                                                        
         LA    RF,60(RF)                                                        
*                                                                               
*  GET NEXT ELEMENT                                                             
*                                                                               
         ZIC   RE,RINVTLEN                                                      
         AR    R6,RE                                                            
         B     BLDCM100                                                         
*                                                                               
BLDCMEX  STCM  RF,3,RECLEN                                                      
         BAS   RE,WRITREC                                                       
         XIT1                                                                   
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                               
*        BLDTRL ROUTINE --BUILD THE TRAILER RECORD                              
*                                                                               
**********************************************************************          
                                                                                
BLDTRL   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING TRREC,R3                                                         
*                                                                               
         LA    RE,17                                                            
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   TRID,=CL2'T1'                                                    
         MVC   TRSTA,CSTAT                                                      
         CLI   TRSTA+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   TRSTA+4,X'40'                                                    
         EDIT  (4,TAPECNT),(6,TRRECCNT),FILL=0                                  
*                                                                               
BLDTREX  BAS   RE,WRITREC                                                       
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                               
*        WRITEREC ROUTINE                                                       
*                                                                               
**********************************************************************          
                                                                                
WRITREC  NTR1  BASE=*,LABEL=*                                                   
         L     RE,TAPECNT          BUMP OUTPUT RECORD COUNTER                   
         LA    RE,1(RE)                                                         
         ST    RE,TAPECNT                                                       
*                                     FILE GENERATION                           
         L     R4,AIO3                                                          
         XC    0(4,R4),0(R4)                                                    
         MVC   0(2,R4),RECLEN                                                   
*                                                                               
*   TEST DUMP                                                                   
***      LA    R0,10                                                            
***      DC    H'0'                                                             
*   TEST DUMP END                                                               
*                                                                               
         TM    HFSFLAG,X'80'       HFS OUTPUT FOR THIS RUN?                     
         BO    WREC0100            YES - OUTPUT UNIX FILE                       
*                                  NO  - OUTPUT TAPE FILE                       
         PRINT GEN                                                              
         L     R2,=A(FILOUTA)                                                   
         A     R2,RELO                                                          
                                                                                
         PUT   (R2),(R4)                                                        
         B     WREC0800                                                         
WREC0100 EQU   *                                                                
*                                                                               
*   TEST DUMP                                                                   
***      LA    R0,4                                                             
***      DC    H'0'                                                             
*   TEST DUMP                                                                   
*                                                                               
         GOTO1 =A(HFSPROC),DMCB,(1,0),(R4)                                      
*                                  WRITE REC TO UNIX FILE                       
*                                                                               
*   TEST DUMP                                                                   
**       LA    R0,5                                                             
**       DC    H'0'                                                             
*   TEST DUMP                                                                   
*                                                                               
         PRINT NOGEN                                                            
WREC0800 EQU   *                                                                
***********                                                                     
*             FOR PRINTED TEST OUTPUT                                           
*                                                                               
**       LH    R5,RECLEN                                                        
**       GOTO1 =V(PRNTBL),DMCB,=C'REC',AIO3,C'DUMP',(R5),=C'1D'                 
*                                                                               
***********                                                                     
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ERREND   GOTO1 ERREX                                                            
VALERROR GOTO1 MYERROR                                                          
*                                                                               
REPFILE  DC    CL8'REPFILE'                                                     
*                                                                               
STADEMS  DC    X'00D97A'           CH2-11                                       
         DC    X'00D97B'           CH6-11                                       
**>>     DC    X'00D941'           WW18+                                        
         DC    X'00D99A'           A35+    (DDS DEMO 154)                       
         DC    X'00D97F'           A2+ (V2+)                                    
**>>     DC    X'00D919'           W12-17                                       
**>>     DC    X'00D91C'           W12-24                                       
         DC    X'00D99D'           A50+    (DDS DEMO 157)                       
         DC    X'00D998'           A35-54  (DDS DEMO 152)                       
         DC    X'00D92D'           W18+                                         
         DC    X'00D929'           W18-34                                       
         DC    X'00D92A'           W18-49                                       
         DC    X'00D947'           W21-49                                       
         DC    X'00D92F'           W25-49                                       
         DC    X'00D930'           W25-54                                       
         DC    X'00D931'           W25-64                                       
         DC    X'00D935'           W35-64                                       
         DC    X'00D95F'           M18+                                         
         DC    X'00D95B'           M18-34                                       
         DC    X'00D95C'           M18-49                                       
         DC    X'00D973'           M21-49                                       
         DC    X'00D961'           M25-49                                       
         DC    X'00D962'           M25-54                                       
         DC    X'00D963'           M25-64                                       
         DC    X'00D967'           M35-64                                       
         DC    X'00D97D'           TN12-17                                      
         DC    X'00D980'           A12-24 (V12-24)                              
         DC    X'00D981'           A12-34 (V12-34)                              
         DC    X'00D991'           A18+                                         
         DC    X'00D98D'           A1834                                        
         DC    X'00D98E'           A1849                                        
         DC    X'00D9BF'           A2149                                        
         DC    X'00D993'           A25-49                                       
         DC    X'00D994'           A25-54                                       
         DC    X'00D995'           A25-64                                       
         DC    X'00D999'           A35-64                                       
*                                                                               
         DC    X'00E37A'           CH2-11                                       
         DC    X'00E37B'           CH6-11                                       
**>>     DC    X'00E341'           WW18+                                        
         DC    X'00E39A'           A35+    (DDS DEMO 154)                       
         DC    X'00E37F'           A2+ (V2+)                                    
**>>     DC    X'00E319'           W12-17                                       
**>>     DC    X'00E31C'           W12-24                                       
         DC    X'00E39D'           A50+    (DDS DEMO 157)                       
         DC    X'00E398'           A35-54  (DDS DEMO 152)                       
         DC    X'00E32D'           W18+                                         
         DC    X'00E329'           W18-34                                       
         DC    X'00E32A'           W18-49                                       
         DC    X'00E347'           W21-49                                       
         DC    X'00E32F'           W25-49                                       
         DC    X'00E330'           W25-54                                       
         DC    X'00E331'           W25-64                                       
         DC    X'00E335'           W35-64                                       
         DC    X'00E35F'           M18+                                         
         DC    X'00E35B'           M18-34                                       
         DC    X'00E35C'           M18-49                                       
         DC    X'00E373'           M21-49                                       
         DC    X'00E361'           M25-49                                       
         DC    X'00E362'           M25-54                                       
         DC    X'00E363'           M25-64                                       
         DC    X'00E367'           M35-64                                       
         DC    X'00E37D'           TN12-17                                      
         DC    X'00E380'           A12-24 (V12-24)                              
         DC    X'00E381'           A12-34 (V12-34)                              
         DC    X'00E391'           A18+                                         
         DC    X'00E38D'           A1834                                        
         DC    X'00E38E'           A1849                                        
         DC    X'00E3BF'           A2149                                        
         DC    X'00E393'           A25-49                                       
         DC    X'00E394'           A25-54                                       
         DC    X'00E395'           A25-64                                       
         DC    X'00E399'           A35-64                                       
*                                                                               
         DC    X'00E301'           THOMES                                       
         DC    X'00D901'           RHOMES                                       
         DC    X'00E201'           SHOMES                                       
         DC    X'00D701'           PHOMES                                       
         DC    X'00D902'           RMETRO-A                                     
         DC    X'00E202'           SMETRO-A                                     
         DC    X'00D702'           PMETRO-A                                     
         DC    X'00D903'           RMETRO-B                                     
         DC    X'00E203'           SMETRO-B                                     
         DC    X'00D703'           PMETRO-B                                     
         DC    X'FF'                                                            
*                                                                               
MKTDEMS  DC    X'00D77A'           CH2-11                                       
         DC    X'00D77B'           CH6-11                                       
***>>    DC    X'00D741'           WW18+                                        
         DC    X'00D79A'           A35+    (DDS DEMO 154)                       
         DC    X'00D77F'           A2+ (V2+)                                    
***>>>   DC    X'00D719'           W12-17                                       
***>>>   DC    X'00D71C'           W12-24                                       
         DC    X'00D79D'           A50+    (DDS DEMO 157)                       
         DC    X'00D798'           A35-54  (DDS DEMO 152)                       
         DC    X'00D72D'           W18+                                         
         DC    X'00D729'           W18-34                                       
         DC    X'00D72A'           W18-49                                       
         DC    X'00D747'           W21-49                                       
         DC    X'00D72F'           W25-49                                       
         DC    X'00D730'           W25-54                                       
         DC    X'00D731'           W25-64                                       
         DC    X'00D735'           W35-64                                       
         DC    X'00D75F'           M18+                                         
         DC    X'00D75B'           M18-34                                       
         DC    X'00D75C'           M18-49                                       
         DC    X'00D773'           M21-49                                       
         DC    X'00D761'           M25-49                                       
         DC    X'00D762'           M25-54                                       
         DC    X'00D763'           M25-64                                       
         DC    X'00D767'           M35-64                                       
         DC    X'00D77D'           TN12-17                                      
         DC    X'00D780'           A12-24 (V12-24)                              
         DC    X'00D781'           A12-34 (V12-34)                              
         DC    X'00D791'           A18+                                         
         DC    X'00D78D'           A1834                                        
         DC    X'00D78E'           A1849                                        
         DC    X'00D7BF'           A2149                                        
         DC    X'00D793'           A25-49                                       
         DC    X'00D794'           A25-54                                       
         DC    X'00D795'           A25-64                                       
         DC    X'00D799'           A35-64                                       
*                                                                               
         DC    X'00D87A'           CH2-11                                       
         DC    X'00D87B'           CH6-11                                       
***>>    DC    X'00D841'           WW18+                                        
         DC    X'00D89A'           A35+    (DDS DEMO 154)                       
         DC    X'00D87F'           A2+ (V2+)                                    
***>>    DC    X'00D819'           W12-17                                       
***>>    DC    X'00D81C'           W12-24                                       
         DC    X'00D89D'           A50+    (DDS DEMO 157)                       
         DC    X'00D898'           A35-54  (DDS DEMO 152)                       
         DC    X'00D82D'           W18+                                         
         DC    X'00D829'           W18-34                                       
         DC    X'00D82A'           W18-49                                       
         DC    X'00D847'           W21-49                                       
         DC    X'00D82F'           W25-49                                       
         DC    X'00D830'           W25-54                                       
         DC    X'00D831'           W25-64                                       
         DC    X'00D835'           W35-64                                       
         DC    X'00D85F'           M18+                                         
         DC    X'00D85B'           M18-34                                       
         DC    X'00D85C'           M18-49                                       
         DC    X'00D873'           M21-49                                       
         DC    X'00D861'           M25-49                                       
         DC    X'00D862'           M25-54                                       
         DC    X'00D863'           M25-64                                       
         DC    X'00D867'           M35-64                                       
         DC    X'00D87D'           TN12-17                                      
         DC    X'00D880'           A12-24 (V12-24)                              
         DC    X'00D881'           A12-34 (V12-34)                              
         DC    X'00D891'           A18+                                         
         DC    X'00D88D'           A1834                                        
         DC    X'00D88E'           A1849                                        
         DC    X'00D8BF'           A2149                                        
         DC    X'00D893'           A25-49                                       
         DC    X'00D894'           A25-54                                       
         DC    X'00D895'           A25-64                                       
         DC    X'00D899'           A35-64                                       
*                                                                               
         DC    X'00D801'           THOMES                                       
         DC    X'00D701'           RHOMES                                       
         DC    X'00D701'           SHOMES                                       
         DC    X'00D701'           PHOMES                                       
         DC    X'00D702'           RMETRO-A                                     
         DC    X'00D702'           SMETRO-A                                     
         DC    X'00D702'           PMETRO-A                                     
         DC    X'00D703'           RMETRO-B                                     
         DC    X'00D703'           SMETRO-B                                     
         DC    X'00D703'           PMETRO-B                                     
         DC    X'FF'                                                            
         EJECT                                                                  
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         SPACE 2                                                                
       ++INCLUDE RESVCTAB                                                       
*                                                                               
DATA     DSECT                                                                  
SAVE     DS    18F                                                              
CSRSLOT  DS    1F                                                               
FILEDESC DS    1F                                                               
RETVAL   DS    1F                                                               
RETCODE  DS    1F                                                               
RSNCODE  DS    1F                                                               
BUFADDR  DS    1F                                                               
BUFLEN   DS    1F                                                               
*                                                                               
VARRECD  DSECT                                                                  
VARLEN   DS    H                  VB RECORD LENGTH                              
         DS    XL2                                                              
VARDATA  DS    C                  DATA AREA                                     
         BPXYFTYP DSECT=NO                                                      
         BPXYOPNF DSECT=NO                                                      
         BPXYMODE DSECT=NO                                                      
*                                                                               
*  LAYOUT FOR TAPE HEADER                                                       
HDREC    DSECT                                                                  
HDID     DS    CL2                 ID (H1)                                      
HDSRC    DS    CL4                 TAPE SOURCE                                  
HDVER    DS    CL2                 VERSION NUMBER                               
HDREP    DS    CL2                 REP CODE                                     
HDSVDAT  DS    CL6                 SAVE DATE                                    
HDSVTIM  DS    CL8                 SAVE TIME                                    
*                                                                               
*  LAYOUT FOR DAYPART RECORD                                                    
DPTREC   DSECT                                                                  
DPID     DS    CL2                 ID (D1)                                      
DPCODE   DS    CL1                 DAYPART CODE                                 
DPSNAME  DS    CL4                 DAYPART SHORT NAME                           
DPLNAME  DS    CL16                DAYPART LONG NAME                            
*                                                                               
*  LAYOUT FOR INVENTORY MASTER                                                  
IMREC    DSECT                                                                  
IMID     DS    CL2                 ID (I1)                                      
IMSTA    DS    CL5                 STATION                                      
IMINVN   DS    CL4                 INVENTORY NUMBER                             
IMEFFS   DS    CL6                 EFFECTIVE START DATE                         
IMEFFE   DS    CL6                 EFFECTIVE END DATE                           
IMDAY1   DS    CL11                DAY 1                                        
IMSTIM1  DS    CL11                TIME 1                                       
IMDAY2   DS    CL11                DAY 2                                        
IMSTIM2  DS    CL11                TIME 2                                       
IMDAY3   DS    CL11                DAY 3                                        
IMSTIM3  DS    CL11                TIME 3                                       
IMDAY4   DS    CL11                DAY 4                                        
IMSTIM4  DS    CL11                TIME 4                                       
IMDAY5   DS    CL11                DAY 5                                        
IMSTIM5  DS    CL11                TIME 5                                       
IMDAY6   DS    CL11                DAY 6                                        
IMSTIM6  DS    CL11                TIME 6                                       
IMDAY7   DS    CL11                DAY 7                                        
IMSTIM7  DS    CL11                TIME 7                                       
IMDAY8   DS    CL11                DAY 8                                        
IMSTIM8  DS    CL11                TIME 8                                       
IMPROG1  DS    CL27                PROGRAM TITLE 1                              
IMPROG2  DS    CL27                PROGRAM TITLE 2                              
IMPROG3  DS    CL27                PROGRAM TITLE 3                              
IMPROG4  DS    CL27                PROGRAM TITLE 4                              
IMPROG5  DS    CL27                PROGRAM TITLE 5                              
IMPROG6  DS    CL27                PROGRAM TITLE 6                              
IMPROG7  DS    CL27                PROGRAM TITLE 7                              
IMPROG8  DS    CL27                PROGRAM TITLE 8                              
IMDYPTS  DS    CL6                 DAYPARTS                                     
IMFLTER  DS    CL6                 FILTERS                                      
IMMARKET DS    CL20                MARKET                                       
IMPOWER  DS    CL2                 AGENCY POWER CODE                            
IMCOMPST DS    CL1                 COMPETITIVE STATION                          
IMTIMCHG DS    CL3                 TIME CHANGE                                  
*                                                                               
*  LAYOUT FOR EXCLUSION RECORD                                                  
EXCREC   DSECT                                                                  
EXCID    DS    CL2                 ID (E1)                                      
EXCSTA   DS    CL5                 STATION                                      
EXCINVN  DS    CL4                 INVENTORY NUMBER                             
EXCEFFS  DS    CL6                 EFFECTIVE START DATE                         
EXCBOOK  DSECT                      MULTI-APPENDED TO EXCL RECORD BASE          
EXCSRVY  DS    XL2                 SURVEY(YYMM)                                 
EXCPRFX  DS    CL1                 BOOK PREFIX                                  
EXCBKTYP DS    CL1                 BOOK TYPE                                    
*                                                                               
*  LAYOUT FOR SURVEY RECORD                                                     
SRVREC   DSECT                                                                  
SRVID    DS    CL2                 ID (S1)                                      
SRVSTA   DS    CL5                 STATION                                      
SRVINV   DS    CL4                 INVENTORY NUMBER                             
SRVSEFF  DS    CL6                 EFFECTIVE START DATE                         
SRVSRC   DS    CL1                 SOURCE                                       
SRVBOOK  DS    CL2                 BOOK                                         
SRVLOOK  DS    CL1                 LOOKUP TYPE (T,P,A,S,E)                      
SRVDAY1  DS    CL11                DAY 1                                        
SRVSTM1  DS    CL11                TIME 1                                       
SRVDAY2  DS    CL11                DAY 2                                        
SRVSTM2  DS    CL11                TIME 2                                       
SRVDAY3  DS    CL11                DAY 3                                        
SRVSTM3  DS    CL11                TIME 3                                       
SRVDAY4  DS    CL11                DAY 4                                        
SRVSTM4  DS    CL11                TIME 4                                       
SRVDAY5  DS    CL11                DAY 5                                        
SRVSTM5  DS    CL11                TIME 5                                       
SRVDAY6  DS    CL11                DAY 6                                        
SRVSTM6  DS    CL11                TIME 6                                       
SRVDAY7  DS    CL11                DAY 7                                        
SRVSTM7  DS    CL11                TIME 7                                       
SRVDAY8  DS    CL11                DAY 8                                        
SRVSTM8  DS    CL11                TIME 8                                       
*  STATION LEVEL DEMOS                                                          
STPRAT   DS    CL66                PEOPLE RATINGS 33X2                          
STPIMP   DS    CL132               PEOPLE IMPS 33X4                             
STTHMS   DS    CL4                 TOTAL HOMES 000'S                            
STHLDRTG DS    CL2                 HOUSEHOLD RATINGS                            
STHLDSHR DS    CL2                 HOUSEHOLD SHARES                             
STHLDHUT DS    CL2                 HOUSEHOLD HUTS                               
STMTARTG DS    CL2                 METRO-A RATINGS                              
STMTASHR DS    CL2                 METRO-A SHARE                                
STMTAHUT DS    CL2                 METRO-A HUTS                                 
STMTBRTG DS    CL2                 METRO-B RATINGS                              
STMTBSHR DS    CL2                 METRO-B SHARE                                
STMTBHUT DS    CL2                 METRO-B HUTS                                 
*  MARKET LEVEL DEMOS                                                           
MKPRAT   DS    CL66                PEOPLE RATINGS 33X2                          
MKPIMP   DS    CL132               PEOPLE IMPS 33X4                             
MKTHMS   DS    CL4                 TOTAL HOMES 000'S                            
MKHLDRTG DS    CL2                 HOUSEHOLD RATINGS                            
MKHLDSHR DS    CL2                 HOUSEHOLD SHARES                             
MKHLDHUT DS    CL2                 HOUSEHOLD HUTS                               
MKMTARTG DS    CL2                 METRO-A RATINGS                              
MKMTASHR DS    CL2                 METRO-A SHARE                                
MKMTAHUT DS    CL2                 METRO-A HUTS                                 
MKMTBRTG DS    CL2                 METRO-B RATINGS                              
MKMTBSHR DS    CL2                 METRO-B SHARE                                
MKMTBHUT DS    CL2                 METRO-B HUTS                                 
*                                                                               
SRVPROG  DS    CL27                PROGRAM INFO                                 
SRVBRCD  DS    CL2                 PROGRAM/BREAK CODE                           
SRVFRBK  DS    CL2                 FROM BOOK                                    
SRVFRTP  DS    CL1                 FROM TYPE                                    
SRVFRFN  DS    CL1                 FROM FUNCTION                                
SRVFRST  DS    CL5                 FROM STATION                                 
SRVUPGF  DS    CL70                UPGRADE FORMULA                              
SRVBTYP  DS    CL1                 BOOK TYPE                                    
*                                                                               
*  LAYOUT FOR OVERRIDE RECORD                                                   
OVRREC   DSECT                                                                  
OVRID    DS    CL2                 ID (O1)                                      
OVRSTA   DS    CL5                 STATION                                      
OVRINV   DS    CL4                 INVENTORY NUMBER                             
OVRSEFF  DS    CL6                 EFFECTIVE START DATE                         
OVRCAT   DS    CL7                 CATEGORY NAME                                
OVRVLTYP DS    CL1                 VALUE TYPE R,I                               
OVRDEMO  DS    CL4                 DEMO AMOUNT                                  
*                                                                               
*  LAYOUT FOR COMMENT RECORD                                                    
CMREC    DSECT                                                                  
CMID     DS    CL2                 RECORD ID (C1)                               
CMSTA    DS    CL5                 STATION                                      
CMINVN   DS    CL4                 INVENTORY NUMBER                             
CMEFFS   DS    CL6                 EFFECTIVE START DATE                         
CMTXTN   DS    CL3                 TEXT NUMBER                                  
CMBOOK   DS    CL2                 BOOK                                         
CMBKPFX  DS    CL1                 BOOK PREFIX                                  
CMBKTYP  DS    CL1                 BOOK TYPE                                    
CMTXT1   DS    CL60                COMMENT TEXT (UP TO 40 LINES)                
*                                                                               
*  LAYOUT FOR TRAILER RECORD                                                    
TRREC    DSECT                                                                  
TRID     DS    CL2                 RECORD ID (T1)                               
TRSTA    DS    CL5                 STATION                                      
TRRECCNT DS    CL6                 RECORD COUNT                                 
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPFFD                                                                      
* DDGENTWA                                                                      
* RERMPWTWA                                                                     
* RERMPD7D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
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
       ++INCLUDE RERMPEDD                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
       ++INCLUDE REGENRDP                                                       
         EJECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
         EJECT                                                                  
       ++INCLUDE EDILINKD                                                       
         EJECT                                                                  
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
***********************************************************************         
*                                                                               
*            SYSSPARE WORK AREA                                                 
*                                                                               
***********************************************************************         
                                                                                
RELO     DS    A                   MODULES' RELOCATION FACTOR                   
                                                                                
TAPECNT  DS    F                   NUMBER OF RECORDS                            
ATAPEDCB DS    F                   ADDRESS OF DCB                               
*                                                                               
STRTOPT  DS    CL3                 START DATE                                   
STRTOPTC DS    CL2                 START DATE COMPRESSED                        
ENDOPT   DS    CL3                 END DATE                                     
SAVPROG  DS    CL27                PROGRAM NAME                                 
COMPSTA  DS    CL1                 COMPETITIVE STATION (Y,N)                    
*                                                                               
STRDATE6 DS    CL6                 6 CHAR START DATE                            
ENDDATE6 DS    CL6                 6 CHAR END DATE                              
*                                                                               
TDTTIME  DS    CL13                DATE AND TIME STAMP                          
*                                                                               
RECLEN   DS    H                   RECORD LENGTH                                
*                                                                               
DAYTMHD  DS    XL40                HEADER DAY TIME INFO                         
*                                                                               
UPGRADST DS    XL132               UPGRADE EXPRESSION HOLD AREA                 
*                                                                               
FLTRFLGS DS    XL1                 INV# RELATED FILTER FLAGS                    
LISTFLG  EQU   X'80'                INVENTORY LIST FILTER                       
RANGEFLG EQU   X'40'                INVENTORY RANGE FILTER                      
                                                                                
                                                                                
                                                                                
                                                                                
KEYSKEL  DS    XL(RINVKSRC-RINVKEY) TEMP KEY SKELETON HOLDER                    
THRUINVQ EQU   (RINVKSTD-RINVKEY)  LENGTH OF KEY TO&INCLUDING INV#              
         EJECT                                                                  
                                                                                
**********************************************************************          
*                                                                               
*     LOCAL WORKING STORAGE                                                     
*                                                                               
**********************************************************************          
                                                                                
WORKD    DSECT                                                                  
DATABLOK DS    50F                 HFS BLOCK SPACE                              
*                                                                               
SIZE     DS    F                   200                                          
ALET     DS    F                   0                                            
DFLEN    DS    F                   DATA FILE LEN: # CHARS IN DATAFILE           
DATAFILE DS    CL28                DATA FILE NAME: LENGTH IS ABOVE              
*                                                                               
MYDBLOCK DS    CL(L'DBLOCK)        ALLOCATED SPACE FOR DBLOCK                   
MYDSPILL DS    CL44                DBLOCK IS BIGGER THAN ADVERTISED             
DBEXTRA1 DS    CL128               DBLOCK EXTRA LENGTH                          
                                                                                
#OFENTS  DS     XL1                                                             
                                                                                
*                                                                               
INVFLGS  DS     XL1                                                             
BLDEXREC EQU    X'80'              NO MATCH ON FILTER, BUILD EXCLUSION          
*                                                                               
HFSFLAG  DS    XL1                                                              
SETHFSON EQU   X'80'               OUTPUT IS HFS                                
                                                                                
KEYHOLD  DS    CL27                                                             
                                                                                
LISTPTR  DS    A                   POINTER TO INVENTORY LIST POSITION           
INVLIST  DS    CL(MAXOPTS*INVLINES*INVLNQ) INV ITEM FILTER LIST                 
         DS    CL1                         SPACE FOR LIST DELIMITER             
INVLISTX EQU   *                                                                
                                                                                
RANGEPTR DS    A                   POINTER TO INV RANGE TABLE POSITION          
RANGETAB DS    33XL(RANGELEN)      INV RANGE FILTER TABLE                       
INVLOW   DS    0CL4                LOW BOUND INVENTORY FILTER                   
INVHIGH  DS    0CL4                UPPERBOUND INVENTORY FILTER                  
         DS    CL1                 SPACE FOR TABLE DELIMITER                    
RANGETBX EQU   *                                                                
RANGELEN EQU   L'INVLOW+L'INVHIGH                                               
                                                                                
FLTRBLOK DS    (MAXOPTS)XL32     FOR SCANNER                                    
MAXOPTS  EQU   11                MAX# OF INV FILTERS PER LINE                   
INVLINES EQU   3                 NUMBER OF INVENTORY FILTER LINES               
INVLNQ   EQU   4                 LENGTH OF AN INVENTORY# IDENTIFIER             
                                                                                
WRKLENQ  EQU   *-WORKD                                                          
                                                                                
         EJECT                                                                  
                                                                                
*********************************************************************           
*                                                                               
* DSECT TO COVER DBLOCK                                                         
*                                                                               
**********************************************************************          
                                                                                
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
                                                                                
                                                                                
                                                                                
                                                                                
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T810FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T810FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
                                                                                
       ++INCLUDE DDSCANBLKD                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'112RERMP20S  12/15/03'                                      
         END                                                                    
