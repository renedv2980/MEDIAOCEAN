*          DATA SET REREP2402E AT LEVEL 052 AS OF 04/28/00                      
*PHASE RE2402E,*                                                                
*INCLUDE RXROUTS                                                                
*INCLUDE RXCNVX                                                                 
         TITLE 'MODULE TO EXTRACT THE KATZ FILE'                                
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* TEMP VERSION WILL BE DELETED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *             
*******************************************************************             
*                                                                               
RE2402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2402,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         SPACE 2                                                                
*--------------------*                                                          
* INITIALIZE PROGRAM *                                                          
*--------------------*                                                          
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                                                               
         L     R2,ATSARDWA         A(TSAR CONTROL BOLOCK AREA)                  
         USING TSARD,R2                                                         
         XC    0(TSARDL,R2),0(R2)                                               
         MVC   TSABUF,AAGGREG      USE AGGREG BUFFER                            
         MVC   TSAREC,=A(LENAGG)                                                
         LA    R0,TSKLENQ          SET KEY LENGTH                               
         STC   R0,TSKEYL                                                        
         LA    R0,L'BUFFREC        SET RECORD LENGTH                            
         STH   R0,TSRECL                                                        
         MVI   TSOFFACT,TSAINI     INITIALIZE BUFFER                            
         GOTO1 ATSAROFF,(R2)                                                    
         MVC   SVTSRBLK,0(R2)      SAVE THE TSAR BLOCK                          
         EJECT                                                                  
*--------------------*                                                          
* READ RECORDS       *                                                          
*--------------------*                                                          
         LA    R5,RECTAB                                                        
         USING RECTABD,R5                                                       
MAIN010  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),RECTTYP                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
MAIN020  DS    0H                                                               
         CLC   KEY(1),RECTTYP                                                   
         BNE   MAIN040             SKIP READ TO NEXT TYPE                       
*                                                                               
         ZIC   RE,RECTRDIS                                                      
         LA    RE,KEY(RE)                                                       
         ICM   RF,15,RECTREPS                                                   
*                                                                               
MAIN022  DS    0H                                                               
         OC    0(2,RF),0(RF)                                                    
         BZ    MAIN030             SKIP READ TO NEXT REP                        
         CLC   0(2,RF),0(RE)                                                    
         BE    MAIN024             PROCESS MATCH                                
         LA    RF,2(RF)                                                         
         B     MAIN022                                                          
*                                                                               
MAIN024  DS    0H                                                               
         LA    RF,RCONREC          JUST SOMEWHERE TO READ                       
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
*                                                                               
         ICM   RF,15,RECTPROC      GO PROCESS IT                                
         BASR  RE,RF                                                            
*                                                                               
MAIN026  DS    0H                                                               
         GOTO1 SEQ                 GET NEXT RECORD                              
         B     MAIN020                                                          
*                                                                               
MAIN030  DS    0H                  SKIP READ TO NEXT REP                        
         ZIC   RE,RECTRDIS                                                      
         LA    RE,KEY(RE)          POINT TO REP CODE                            
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         LA    RF,1(RF)            BUMP REP CODE BY BINARY 1                    
         STCM  RF,3,0(RE)                                                       
*                                                                               
         LA    RE,2(RE)            BUMP PAST REPCODE                            
         LR    RF,RE                                                            
         LA    R0,KEY                                                           
         SR    RF,R0               LENGHT OF REMAINING KEY                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,RE),0(RE)       CLEAR REMAINING KEY                          
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     MAIN020                                                          
*                                                                               
MAIN030  DS    0H                  SKIP TO NEXT RECORD TYPE                     
         LA    R5,RECTABL(R5)                                                   
         CLI   RECTTYP,0                                                        
         BE    MAIN100             EXIT                                         
*                                                                               
         CLC   KEY(1),RECTTYP      NEXT TYPE IN KEY?                            
         BE    MAIN020             YES                                          
         B     MAIN010                                                          
*                                                                               
MAIN100  DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LOAD CONTRACT DOLLAR RECORD                                         *         
*        ROUTINE WILL BUILD BOTH DOLLAR BUCKET RECORDS AND            *         
*        CONTRACT HEADER RECORDS AT THE SAME TIME                     *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADDOL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CON RECORD             
         USING RCONREC,R2                                                       
         XC    RCONKEY,RCONKEY                                                  
*                                                                               
         MVI   RCON8ETP,X'8E'                                                   
         MVC   RCON8ERP,REPALPHA                                                
*                                 CHECK STATION FILTER                          
         CLC   DXU.RXUSTA,SPACES                                                
         JNH   *+10                                                             
         MVC   RCON8EST,DXU.RXUSTA                                              
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             ERROR ON READ HIGH                           
         JNE   NO                                                               
*                                                                               
LDOL0020 TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   IOKEY(RCON8EST-RCONKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         L     R5,=A(STAXREC)                                                   
*                                                                               
         CLC   4(5,R5),RCON8EST-RCONKEY(R2)                                     
         JE    LDOL0030            STATION DIDN'T CHANGE                        
*                                                                               
         OC    4(5,R5),4(R5)       FIRST TIME?                                  
         JZ    LDOL0022            YES                                          
*                                                                               
         CLC   DXU.RXUSTA,SPACES   STATION FILTER?                              
         JH    YES                 YES - WE MUST BE DONE                        
*                                                                               
LDOL0022 DS    0H                                                               
         MVC   WORK(L'IOKEY),0(R2)      SAVE THE CONTRACT KEY                   
K        USING RSTAKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,REPALPHA                                              
         MVC   K.RSTAKSTA,RCON8EST-RCONKEY(R2)                                  
         DROP  K                                                                
*                                                                               
         LA    R2,IO                                                            
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         CLC   IOKEY(L'RSTAKEY),0(R2)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
R        USING RSTAREC,R2                                                       
         MVC   00(2,R5),=X'0248'                                                
         MVC   02(2,R5),REPALPHA    INSERT REP CODE                             
         MVC   04(5,R5),R.RSTAKSTA                                              
         MVC   09(3,R5),R.RSTASTRT                                              
         MVC   12(3,R5),R.RSTAEND                                               
         MVC   15(2,R5),REPALPHA    INSERT REP CODE                             
         MVC   17(20,R5),R.RSTAMKT                                              
         OC    17(20,R5),SPACES  SET X'00' TO SPACES                            
         MVC   37(2,R5),R.RSTAGRUP                                              
         MVC   39(3,R5),R.RSTAAFFL STATION AFFILIATE                            
         MVC   42(2,R5),=C'T '   DEFAULT NY TEAM CODE                           
         MVC   44(2,R5),R.RSTATVB  TVB REGION                                   
         MVC   46(3,R5),R.RSTAOWN  STATION OWNER                                
         MVC   49(1,R5),R.RSTARANK STATION MARKET RANK                          
         MVC   50(2,R5),=C'T '   DEFAULT CH TEAM CODE                           
         XC    52(2,R5),WORK+52  DISP OF LAST STA IN MKT                        
         MVC   68(2,R5),R.RSTACLDT INSERT CLOSE DATE OF STATION                 
         DROP  R                                                                
*                                                                               
         MVC   IOKEY,WORK          RESTORE CONTRACT                             
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             ERROR ON READ HIGH                           
         JNE   NO                                                               
*                                                                               
LDOL0030 DS    0H                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AFILTDOL            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LDOL0080                                                         
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JH    NO                  ERROR ON GETREC                              
         JL    LDOL0080                                                         
*                                                                               
         TM    REPPFLG,X'40'       FILTER BACK BILLING?                         
         JZ    LDOL0032            NO                                           
*                                                                               
R        USING RCONREC,R2                                                       
         CLC   =C'ACC-BB',RCONBUYR                                              
         JE    LDOL0080                                                         
         DROP  R                                                                
*                                                                               
LDOL0032 DS    0H                                                               
*                                                                               
         GOTO1 AINITDOL            INITIALISE EXTRACT BUFFER                    
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (1,=A(STAXREC)),(R6)                                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LDOL0040 DS    0H                                                               
         GOTO1 VREPDOLC,DMCB,DXAXREC,(REPPFLG,(R2)),                   +        
               (2,=A(STAXREC)),(R6)                                             
         CLI   8(R1),FF                                                         
         JE    LDOL0080            NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LDOL0080            DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    LDOL0060            DO NOT CONVERT RECORD                        
*                                                                               
         L     RE,=A(RXRECIDT)                                                  
T        USING RXRECIDT,RE                                                      
X        USING RECOND,RF                                                        
         CLC   X.RECONTYP,T.REPCONQ                                             
         JNE   LDOL0042                                                         
         DROP  X,T                                                              
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,=C'CON'),      *        
               VERSION                                                          
         J     LDOL0044                                                         
*                                                                               
LDOL0042 DS    0H                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,TYPENAME),     *        
               VERSION                                                          
LDOL0044 DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LDOL0060 GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LDOL0040                                                         
*                                                                               
LDOL0080 DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),REPDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDOL0020                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD AGENCY RECORDS                                                           
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADAGY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RAGYREC,R2                                                       
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKTYP,X'1A'                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGY02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RAGYKAGY-RAGYKEY),0(R2)                                    
         JNE   YES                   ALL DONE IF TYPE CHANGES                   
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPAGYC,AINITAGY,AFILTAGY                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGY02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD ADV RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADADV  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RADVREC,R2                                                       
         XC    RADVKEY,RADVKEY                                                  
         MVI   RADVKTYP,X'08'                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LADV02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RADVKADV-RADVKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE CHANGES                     
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPADVC,AINITADV,AFILTADV                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LADV02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD SAL RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADSAL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RSALREC,R2                                                       
         XC    RSALKEY,RSALKEY                                                  
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSAL02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RSALKSAL-RSALKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPSALC,AINITSAL,AFILTSAL                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSAL02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD CTY RECORDS (CONTRACT TYPE)                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADCTY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RCTYREC,R2                                                       
         XC    RCTYKEY,RCTYKEY                                                  
         MVI   RCTYKTYP,X'32'                                                   
         MVC   RCTYKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCTY02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RCTYKCTY-RCTYKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPCTYC,AINITCTY,AFILTCTY                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCTY02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD DCT RECORDS (DEVELOPMENT CONTRACT TYPE)                                  
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADDCT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RDCTREC,R2                                                       
         XC    RDCTKEY,RDCTKEY                                                  
         MVI   RDCTKTYP,X'3B'                                                   
         MVC   RDCTKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDCT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RDCTKCTY-RDCTKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPDCTC,AINITDCT,AFILTDCT                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDCT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD STA RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADSTA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RSTAREC,R2                                                       
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSTA02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RSTAKSTA-RSTAKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPSTAC,AINITSTA,AFILTSTA                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSTA02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD GRP RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADGRP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RGRPREC,R2                                                       
         XC    RGRPKEY,RGRPKEY                                                  
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGRP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RGRPKGRP-RGRPKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPGRPC,AINITGRP,AFILTGRP                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGRP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD DSP RECORDS (DEVELOPMENT SALESPERSON)                                    
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADDSP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RDSPREC,R2                                                       
         XC    RDSPKEY,RDSPKEY                                                  
         MVI   RDSPKTYP,X'3A'                                                   
         MVC   RDSPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDSP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RDSPKSAL-RDSPKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPDSPC,AINITDSP,AFILTDSP                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDSP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD TEM RECORDS (TEAM)                                                       
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADTEM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RTEMREC,R2                                                       
         XC    RTEMKEY,RTEMKEY                                                  
         MVI   RTEMKTYP,X'05'                                                   
         MVC   RTEMKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LTEM02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RTEMKTEM-RTEMKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPTEMC,AINITTEM,AFILTTEM                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTEM02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD PRD RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADPRD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RPRDREC,R2                                                       
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPRD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RPRDKADV-RPRDKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPPRDC,AINITPRD,AFILTPRD                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPRD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD CLS RECORDS (CLASS)                                                      
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADCLS  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RCLSREC,R2                                                       
         XC    RCLSKEY,RCLSKEY                                                  
         MVI   RCLSKTYP,X'0D'                                                   
         MVC   RCLSKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCLS02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RCLSKCLS-RCLSKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPCLSC,AINITCLS,AFILTCLS                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCLS02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD CAT RECORDS (PRODUCT CATEGORY)                                           
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADCAT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RCTGREC,R2                                                       
         XC    RCTGKEY,RCTGKEY                                                  
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCAT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RCTGKCTG-RCTGKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPCATC,AINITCAT,AFILTCAT                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCAT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD OFF RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADOFF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ROFFREC,R2                                                       
         XC    ROFFKEY,ROFFKEY                                                  
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOFF02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(ROFFKOFF-ROFFKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPOFFC,AINITOFF,AFILTOFF                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOFF02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD OWN RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADOWN  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ROWNREC,R2                                                       
         XC    ROWNKEY,ROWNKEY                                                  
         MVI   ROWNKTYP,X'2A'                                                   
         MVC   ROWNKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOWN02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(ROWNKOWN-ROWNKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPOWNC,AINITOWN,AFILTOWN                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOWN02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD MKT RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADMKT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RMKTREC,R2                                                       
         XC    RMKTKEY,RMKTKEY                                                  
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMKT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(RMKTKMKT-RMKTKEY),0(R2)                                    
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPMKTC,AINITMKT,AFILTMKT                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMKT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD MST RECORDS (SEE-ME/READ-ME MASTER/SUB RECORDS                           
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADMST  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RREPREC,R2                                                       
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMST02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(27),0(R2)                                                  
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPMSTC,AINITMST,AFILTMST                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMST02                                                           
         J     YES                                                              
*                                                                               
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD REP RECORDS                                                              
*---------------------------------------------------------------------*         
         SPACE 1                                                                
LOADREP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING RREPREC,R2                                                       
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,REPALPHA                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LREP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLC   IOKEY(27),0(R2)                                                  
         JNE   YES                 ALL DONE IF TYPE/REP CHANGES                 
*                                                                               
         MVC   REPADDR,28(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VREPREPC,AINITREP,AFILTREP                         
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LREP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN LOAD MODE                  *         
* R2 = A(ACCOUNT DIRECTORY RECORD BUFFER)                             *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
***********************************************************************         
         SPACE 1                                                                
ACCLOAD  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               FILTER ROUTINE?                              
         JZ    ALOA02                                                           
         GOTO1 (R5)                FILTER RECORD                                
         JNE   ALOA06              NOT VALID - GET NEXT                         
*                                                                               
ALOA02   GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 (R4)                INITIALISE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6)                                    
         TM    DMCB+8,X'80'                                                     
         JO    ALOA06              ERROR - NO WRITE                             
         CLI   DMCB+8,FF                                                        
         JE    ALOA06              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   ALOA06              CONTROLLER REQUESTS NO WRITE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    ALOA04              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VREPCNVX,DMCB,(C'L',DXAXREC),(1,DXASQLB),(0,TYPENAME),  *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
ALOA04   GOTO1 DXPUT,DMCB,(RF),(R7) UNCONVERTED RECORD TO EXTRACT               
*                                                                               
ALOA06   GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JNE   NO                  TOO MANY IOS                                 
*                                                                               
         MVC   IOKEY(L'RREPKEY),0(R2) READ NEXT RECORD - SEQUENTIAL             
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,REPDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SETUP DATES REQUIRED BY THE PROGRAM                                           
***********************************************************************         
SETDATES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* CURRENT YEAR BROADCAST START                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(9,DXU.RXUCURST),(0,WORK)                           
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+12),(2,CURBST)                              
*                                                                               
* PRIOR YEAR BROADCAST START                                                    
*                                                                               
         GOTO1 =V(ADDAY),DMCB,(C'Y',WORK),(0,WORK),-1                           
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+12),(2,PRIBST)                              
*                                                                               
* CURRENT YEAR BROADCAST END                                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(9,DXU.RXUCURND),(0,WORK)                           
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+18),(2,CURBND)                              
*                                                                               
* PRIOR YEAR BROADCAST END                                                      
*                                                                               
         GOTO1 =V(ADDAY),DMCB,(C'Y',WORK),(0,WORK),-1                           
         GOTO1 =V(GETBROAD),DMCB,WORK,WORK+12                                   
         GOTO1 VDATCON,DMCB,(0,WORK+18),(2,PRIBND)                              
*                                                                               
         L     R2,=V(RXQWK)                                                     
         USING QWKD,R2                                                          
         GOTO1 VDATCON,DMCB,(2,CURBST),(3,QWCURST3)                             
         GOTO1 VDATCON,DMCB,(2,CURBND),(3,QWCURND3)                             
         GOTO1 VDATCON,DMCB,(2,PRIBST),(3,QWPRIST3)                             
         GOTO1 VDATCON,DMCB,(2,PRIBND),(3,QWPRIND3)                             
         DROP  R2                                                               
*                                                                               
         GOTO1 =V(SETUPMON),DMCB,DXUSER                                         
*                                                                               
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REPFILE INITIALISATION                                              *         
***********************************************************************         
         SPACE 1                                                                
REPINIT  NTR1  BASE=*,LABEL=*                                                   
K        USING RREPKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         LA    R2,IO                                                            
         GOTO1 AREADHI                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RREPKEY),0(R2)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   REPADDR,28(R2)      SET DA(RECORD)                               
         GOTO1 AGETIT                                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
R        USING RREPREC,R2                                                       
         MVI   REPPFLG,0                                                        
         CLI   R.RREPPROF+10,C'Y'                                               
         JNE   *+8                                                              
         OI    REPPFLG,X'80'                                                    
         CLI   R.RREPPROF+15,C'B'                                               
         JNE   *+8                                                              
         OI    REPPFLG,X'40'                                                    
         DROP  R                                                                
*                                                                               
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RXRECID                                                                       
***********************************************************************         
         PRINT OFF                                                              
RXRECIDT DS    0C                                                               
       ++INCLUDE RXRECID                                                        
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* BROADCAST DATE BLOCK                                                          
***********************************************************************         
DATEBLK  DS    0C                                                               
CURBST   DS    XL2                 COMPRESSED CURRENT BROADCAST START           
CURBND   DS    XL2                 COMPRESSED CURRENT BROADCAST END             
PRIBST   DS    XL2                 COMPRESSED PRIOR BROADCAST START             
PRIBND   DS    XL2                 COMPRESSED PRIOR BROADCAST END               
         SPACE 3                                                                
*XXXXXXXXXXXXXXXXXXXXXXXXXXXX                                                   
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
         DS    0D                                                               
       ++INCLUDE RGENIO                                                         
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'STORAGE AND DSECTS'                                             
**********************************************************************          
RECTAB   DS    0H                                                               
         DC    X'01',AL1(RREPKREP-RREPKEY),A(ALLREPS),A(PROCREP)                
         DC    X'02',AL1(RSTAKREP-RSTAKEY),A(SUBREPS),A(PROCSTA)                
         DC    X'04',AL1(ROFFKREP-ROFFKEY),A(SUBREPS),A(PROCOFF)                
         DC    X'05',AL1(RTEMKREP-RTEMKEY),A(MASREPS),A(PROCTEM)                
         DC    X'06',AL1(RSALKREP-RSALKEY),A(MASREPS),A(PROCSAL)                
         DC    X'07',AL1(RGRPKREP-RGRPKEY),A(MASREPS),A(PROCGRP)                
         DC    X'08',AL1(RADVKREP-RADVKEY),A(MASREPS),A(PROCADV)                
         DC    X'09',AL1(RPRDKREP-RPRDKEY),A(MASREPS),A(PROCPRD)                
         DC    X'0C',AL1(RCONKREP-RCONKEY),A(SUBREPS),A(PROCCON)                
         DC    X'0D',AL1(RCLAKREP-RCLAKEY),A(MASREPS),A(PROCCLA)                
         DC    X'0F',AL1(RCATKREP-RCATKEY),A(MASREPS),A(PROCCAT)                
         DC    X'1A',AL1(RAGYKREP-RAGYKEY),A(MASREPS),A(PROCAGY)                
         DC    X'2A',AL1(ROWNKREP-ROWNKEY),A(MASREPS),A(PROCOWN)                
         DC    X'2B',AL1(RMKTKREP-RMKTKEY),A(MASREPS),A(PROCMKT)                
         DC    X'32',AL1(RCTYKREP-RCTYKEY),A(MASREPS),A(PROCCTY)                
         DC    X'3A',AL1(RDSPKREP-RDSPKEY),A(MASREPS),A(PROCDSP)                
         DC    X'3B',AL1(RDCTKREP-RDCTKEY),A(MASREPS),A(PROCDCT)                
         DC    X'00'                                                            
*                                                                               
*                --..--..--..--..--..--..--                                     
ALLREPS  DC    C'K3BFCREAKFKUK4K6NDNUQDRSS3'      KATZ RADIO                    
         DC    C'MRAMCQ'                          KATZ TV                       
         DC    C'SZ'                              SELTEL/MILLENIUM              
         DC    X'0000'                                                          
*                --..--..--..--..--..--..                                       
MASREPS  DC    C'K3'                              KATZ RADIO                    
         DC    C'MR'                              KATZ TV                       
         DC    C'SZ'                              SELTEL/MILLENIUM              
         DC    X'0000'                                                          
*                --..--..--..--..--..--..                                       
SUBREPS  DC    C'BFCREAKFKUK4K6NDNUQDRSS3'        KATZ RADIO                    
         DC    C'AMCQ'                            KATZ TV                       
         DC    C'SZ'                              SELTEL/MILLENIUM              
         DC    X'0000'                                                          
*                                                                               
***********************************************************************         
* NON RE-ENTRANT WORKING STORAGE                                                
***********************************************************************         
*********************                                                           
** WORKING STORAGE **                                                           
*********************                                                           
RELO     DS    F                                                                
SAVEREGS DS    11F                                                              
*                                                                               
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
SVTSRBLK DS    CL(TSARDL)                                                       
*                                                                               
****************************                                                    
** TSAR RECORD DEFINITION **                                                    
****************************                                                    
TSKEY    EQU   *                                                                
TSKREP   DS    CL2                 REP CODE                                     
TSKSTA   DS    CL(L'RSTAKSTA)      STATION CALL LETTERS(WHEN NO OWNER)          
TSKLENQ  EQU   *-TSKEY             KEY LENGTH                                   
*                                                                               
TSREC    EQU   *                                                                
TSRDATA  DS    XL70                STATION RECORD DATA                          
TSRLENQ  EQU   *-TSKEY             RECORD LENGTH                                
*                                                                               
         ORG   TSKEY                                                            
TSTAREC  DS    CL(TSRLENQ)                                                      
*                                                                               
RECTABD  DSECT                                                                  
RECTTYP  DS    AL1                 RECORD TYPE                                  
RECTRDIS DS    AL1                 DISPLACEMENT TO REP CODE                     
RECTREPS DS    A                   A(REPCODE TABLE)                             
RECTPROC DS    A                   A(PROCESSING ROUTINE)                        
RECTABL  EQU   *-RECTABD                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052REREP2402E04/28/00'                                      
         END                                                                    
* REREPRGEQA                                                                    
* REREPTSAR                                                                     
* REGENALL1                                                                     
* REREPWORKD                                                                    
* REREPMODES                                                                    
* REXADDRD                                                                      
* DDTSARD                                                                       
* RXUSERD                                                                       
* RXRECD                                                                        
* DDPERVALD                                                                     
* REGENREQ2                                                                     
* DDMASTC                                                                       
         PRINT OFF                                                              
       ++INCLUDE REREPRGEQA                                                     
       ++INCLUDE REREPTSAR                                                      
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE RXUSERD                                                        
       ++INCLUDE RXRECD                                                         
       ++INCLUDE DDPERVALD                                                      
QREC2D   DSECT                                                                  
       ++INCLUDE REGENREQ2                                                      
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
