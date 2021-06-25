*          DATA SET DEDEM09    AT LEVEL 093 AS OF 01/04/11                      
*PHASE T21B09A                                                                  
*INCLUDE BINSR31                                                                
                                                                                
****************************** UPDATE LOG *****************************         
*                                                                     *         
*   DATE  LVL USER DESCRIPTION                                        *         
* ------- --- ---- -------------------------------------------------- *         
* 04NOV00 020 BPOO SUPPORT USERID AUTHORIZATION                       *         
* 02NOV00 019 BPOO MOVE DEM TABLES TO DEM81 PHASE                     *         
* 22SEP00 018 BPOO falink stuff...change screen one line lower        *         
* 20Jun00 017 GLEE Change versioning scheme to support PC vrsn stmp   *         
*                                                                     *         
* 08Mar00 016 GLEE Re-linked for bigger buy record support            *         
*                                                                     *         
* 18Feb00 015 GLEE Force agency="SJ" when looking up program names    *         
*                                                                     *         
* 13Sep99 014 GLEE Set CC upon exiting DEMPROC mode                   *         
*                                                                     *         
* 22Jun99 013 GLEE Made change in LVL=006 applicable to MF & DEM16    *         
*                                                                     *         
* 11Feb99 008 GLEE Implement version (extract) dates for DEM32        *         
*                                                                     *         
* 05Nov98 007 GLEE Support DEM32 request without station input        *         
*                                                                     *         
* 14Oct98 006 GLEE Ignore programs whose program source is PUBLIC     *         
*                                                                     *         
* 20JUL98 003 BPOO SUPPORT FOR DEM32 AND FALINK                       *         
*                                                                               
* 20Nov95 002 GLEE Support for STEREO                                 *         
*                                                                     *         
* 02Nov95 001 GLEE Enhanced program and re-leveled to 001             *         
*                                                                     *         
* 22Nov94 001 GLEE New program to list network/syndicated programs a  *         
*                   station ran for a given book                      *         
***********************************************************************         
T21B09   TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK'                        
DEM09    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DEM9**,RR=RE                                                 
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
         USING DEMTWAD,R8          R8=A(TWA)                                    
         L     R7,AAPWORK                                                       
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
*                                                                               
         ST    RE,RELO09                                                        
         DS    0H                  SET UP ADCONS                                
         LH    R1,=Y(DISPTAB-DEM09)                                             
         LA    R1,DEM09(R1)                                                     
         LA    R0,DISPTABQ                                                      
DEM09_10 SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         LA    RE,DEM09(RE)        RE=A(TABLE)                                  
         SR    RF,RF                                                            
         ICM   RF,3,2(R1)                                                       
         LA    RF,DEMTMPD(RF)      RF-->PLACE TO STORE A(TABLE)                 
         ST    RE,0(RF)                                                         
         LA    R1,L'DISPTAB(R1)                                                 
         BCT   R0,DEM09_10                                                      
*                                                                               
*                                  HANDLE CONTROLLER MODE SETTINGS              
         CLI   APMODE,FORMHEAD     FORMAT HEADLINES & INITIALISE                
         BE    DEMHEAD                                                          
         CLI   APMODE,PROCESS      READ & POST RECORDS TO BUFFER                
         BE    DEMPROC                                                          
         CLI   APMODE,FORMLINE     FORMAT A PRINT A BUFFER RECORD               
         BE    DEMLINE                                                          
*                                                                               
EXITH    DS    0H                  EXIT W/ CC HIGH                              
         LA    R0,1                                                             
         J     EXITCR                                                           
                                                                                
EXITL    DS    0H                  EXIT W/ CC LOW                               
         LHI   R0,-1                                                            
         J     EXITCR                                                           
                                                                                
EXITE    DS    0H                  EXIT W/ CC EQUAL                             
         SR    R0,R0                                                            
         J     EXITCR                                                           
                                                                                
EXITCR   DS    0H                                                               
         AR    R0,R9                                                            
         CR    R0,R9               SET CC                                       
         J     EXIT                 AND EXIT                                    
*                                                                               
YES      SR    R9,R9                                                            
NO       LTR   R9,R9                                                            
EXIT     XMOD1 1                                                                
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (APMODE=FORMHEA+        
               D)'                                                              
***********************************************************************         
*=================== FORMAT HEADLINES & INITIALIZE ===================*         
                                                                                
DEMHEAD  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DEMHD10              GO TO STEREO LOGIC                          
*                                                                               
         LA    RF,BINEND-BINRECD   L'RECORD                                     
                                                                                
         LA    R2,LDNPNAM-LISTD    ASSUME SEQUENCING BY NUMBER                  
         LA    R3,LDNPNUM-LISTD                                                 
         LA    RE,BINNKEYL                                                      
         CLI   OPTLIST,C'A'        TEST LIST IN ALPHA SEQUENCE                  
         BNE   DEMHEAD2             NOPE                                        
         LA    R2,LDAPNAM-LISTD     YES, SEQUENCE BY NAME                       
         LA    R3,LDAPNUM-LISTD                                                 
         LA    RE,BINAKEYL                                                      
                                                                                
DEMHEAD2 DS    0H                  SET BINRCH PARMS                             
         ST    RE,BINLKEY           L'KEY                                       
         MVI   BINDKEY,0            DISPLACEMENT TO KEY                         
         ST    RF,BINLREC           L'RECORD                                    
*                                                                               
         LA    RE,DEMHD1(R2)       FORMAT TITLE - PROGRAM NAME                  
         LA    RF,DEMHD1+39(R2)                                                 
         MVC   0(L'LPNAM,RE),HEADNAM1                                           
         MVC   0(L'LPNAM,RF),HEADNAM1                                           
                                                                                
         LA    RE,DEMHD1(R3)       FORMAT TITLE - PROGRAM NUMBER                
         LA    RF,DEMHD1+39(R3)                                                 
         MVC   0(L'LPNUM,RE),HEADNUM1                                           
         MVC   0(L'LPNUM,RF),HEADNUM1                                           
                                                                                
         LA    RE,DEMHD2(R2)       FORMAT TITLE - UNDERLINES                    
         LA    RF,DEMHD2+39(R2)                                                 
         MVC   0(L'LPNAM,RE),HEADNAM2                                           
         MVC   0(L'LPNAM,RF),HEADNAM2                                           
         LA    RE,DEMHD2(R3)                                                    
         LA    RF,DEMHD2+39(R3)                                                 
         MVC   0(L'LPNUM,RE),HEADNUM2                                           
         MVC   0(L'LPNUM,RF),HEADNUM2                                           
         B     DEMHDX                                                           
                                                                                
                                                                                
DEMHD10  DS    0H                  THIS PART FOR STEREO SESSION ONLY            
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         CLI   TSKEYL,0            SEE IF WE NEED TO SET LENGTHS AGAIN          
         BNE   DEMHD20              NOPE, THEY WERE SET BEFORE                  
                                                                                
         MVI   ANYDATA,C'N'        ASSUME NO DATA                               
         MVI   TSKEYL,TDRNKEYL     ASSUME LISTING IN NUMERIC SEQUENCE           
         CLI   OPTLIST,C'A'                                                     
         BNE   *+8                                                              
         MVI   TSKEYL,TDRAKEYL      WRONG, LIST IN ALPHA SEQUENCE               
                                                                                
         LA    R0,TDRLENQ+2                                                     
         STH   R0,TSRECL                                                        
         DROP  R4                                                               
                                                                                
DEMHD20  DS    0H                                                               
         B     DEMHDX                                                           
                                                                                
                                                                                
DEMHDX   DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (APMODE=PROCESS+        
               )'                                                               
***********************************************************************         
*============== READ DEMO FILES & POST TO BINSRCH BUFFER =============*         
                                                                                
DEMPROC  DS    0H                                                               
                                                                                
         LA    R5,DBLOCK1          INITIALIZE DBLOCK FOR MARKET READS           
         USING DBLOCKD,R5          R5=A(DBLOCK)                                 
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIOAREA1                                                  
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBFUNCT,DBFNC                                                    
*                                                                               
         CLI   NSTAS,0                      IF NO STATIONS ENTERRED,            
         BNE   DMPRC029                                                         
         TM    DEMFLAG1,DF1STERO+DF1DEM32    AND NOT DEM32 SESSION,             
         BNO   DMPRCX                        CAN'T PROCESS ANYTHING             
                                                                                
         L     RE,=V(BINSRCH)                                                   
         A     RE,RELO09                                                        
         ST    RE,VBINSR31                                                      
         L     RE,AFAC                                                          
         ICM   RE,15,CPROTON-COMFACSD(RE)                                       
         STCM  RE,15,VPROTON                                                    
         L     RE,AFAC                                                          
         ICM   RE,15,CPROTOFF-COMFACSD(RE)                                      
         STCM  RE,15,VPROTOFF                                                   
* GRAB WSSVR STORAGE FOR DEM32 TRANSACTION                                      
         L     RF,AFAC                                                          
         ICM   RF,15,CWSSVR-COMFACSD(RF)                                        
         GOTOR (RF),DMCB,C'BINB',('FAWSGMXA',80),0,0                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,8(R1)                                                         
         STCM  RE,15,ABINBIG                                                    
                                                                                
*                                                                               
         MVI   DBFUNCT,DBGETISI                                                 
         MVI   DBSELPR4,X80                                                     
         L     R0,BINATAB          CLEAR A BIG AREA TO STORE STATIONS           
         LH    R1,=Y(L'BINTAB)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
DMPRC029 EQU   *                                                                
*                                                                               
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELBK,BKS                                                      
         MVC   DBBTYPE,BKS+3                                                    
         MVC   DBSELSTA,STAS                                                    
         XC    PROGRAMN,PROGRAMN   CLEAR N'PROGRAMS                             
                                                                                
         DS    0H                  CALL DEMAND TO READ RECORDS                  
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,DEMHOOK,0,0                                 
         DROP  R5                                                               
*                                                                               
         DS    0H                                                               
         CLI   NSTAS,0             IF NO STATIONS ENTERRED,                     
         BNE   DMPRC049                                                         
         MVI   GOSUBN,RPI#          READ & POST PROGRAM INFO HERE               
         GOTO1 AGOSUB                                                           
DMPRC049 EQU   *                                                                
                                                                                
         DS    0H                  POST MESSAGE RECORD                          
         MVI   POSTRTYP,PRTMSG                                                  
         BAS   RE,DEMPOST                                                       
                                                                                
         DS    0H                  POST COLUMN HEADER RECORD                    
         MVI   POSTRTYP,PRTHDR                                                  
         BAS   RE,DEMPOST                                                       
*                                                                               
DMPRCX   DS    0H                                                               
*&&DO                                                                           
         B     EXIT                                                             
*&&                                                                             
         B     EXITE                                                            
         EJECT                                                                  
*======= ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND =======*         
                                                                                
* At entry,                                                                     
*   R5-->DBLOCKD.                                                               
                                                                                
DEMHOOK  DS    0H                                                               
DMHK     NTR1                                                                   
         USING DBLOCKD,R5                                                       
                                                                                
         CLI   DBRECTYP,DBRECPRG   MAKE SURE WE HAVE CORRECT RECD TYPE          
         BE    DMHK200                                                          
         CLI   DBRECTYP,DBRECDEM   MAKE SURE WE HAVE CORRECT RECD TYPE          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DS    0H                  INITIALIZE STORAGE                           
         MVC   PROGNAM,SPACES                                                   
         MVC   PROGNUM,SPACES                                                   
*                                                                               
                                                                                
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'PSOUR',DBLOCK1,WORK                            
         CLC   =C'P ',WORK                                                      
         BE    DMHKX                                                            
*                                                                               
         GOTO1 VDEFINE,MYDMCB,=C'PROGRA',DBLOCK1,PROGNAM                        
         GOTO1 (RF),(R1),=C'TPNO',,PROGNUM                                      
         SR    R0,R0                                                            
         CLC   PROGNUM,=CL8'N/A'                                                
         BE    *+14                                                             
*                                                                               
         PACK  DUB,PROGNUM                                                      
         CVB   R0,DUB                                                           
         STCM  R0,7,PROGBNUM       PROGRAM NUMBER IN BINARY                     
*                                                                               
         MVI   POSTRTYP,PRTPRG     PROGRAM RECORD TO POST                       
         BAS   RE,DEMPOST           AND GO POST IT                              
         B     DMHK300                                                          
         EJECT                                                                  
DMHK200  DS    0H                                                               
         LA    R6,DBKEY            GET & SAVE STATION IN SORTED ORDER           
         USING PIKEY,R6                                                         
*  DONT INCLUDE CABLE STATIONS                                                  
* CONVERT CALL LETTER TO NETWORK NUMERIC ID                                     
         L     RF,AFAC                                                          
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NSICABLE                                               
         ICM   RE,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            R0 HAS LENGTH OF TABLE ENTRY                 
         LR    R1,RE               R1=A(TABLE)                                  
DMHK220  CLI   0(R1),X'FF'         EOT?                                         
         BE    DMHK250                                                          
         USING NSICBLD,R1                                                       
         CLC   NSICCALL,PISTA                                                   
         BE    DMHKX                                                            
         AR    R1,R0                                                            
         B     DMHK220                                                          
         DROP  R1                                                               
                                                                                
*                                                                               
*                                                                               
DMHK250  MVC   WORK+0(L'PISTA),PISTA                                            
         MVC   WORK+5(L'PIDAY+L'PISQH+L'PIEQH),PIDAY                            
*                                                                               
         LA    RE,WORK                                                          
         ST    RE,MYDMCB+00         A(RECORD TO BE ADDED)                       
         GOTO1 VPROTOFF                                                         
         SAM31                                                                  
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    DMHK270                                                          
         MVI   MYDMCB+00,X01        INSERT RECORD IF NOT FOUND                  
         L     RE,BINATAB                                                       
         MVC   MYDMCB+08(4),0(RE)   # OF RECORDS IN TABLE SO FAR                
         LA    RE,4(RE)                                                         
         ST    RE,MYDMCB+04         A(TABLE)                                    
         LA    RE,L'PISTA+L'PIDAY                                               
         ST    RE,MYDMCB+16         L(KEY)                                      
         LA    RE,L'PISQH+L'PIEQH(RE)                                           
         ST    RE,MYDMCB+12         L(RECORD)                                   
         LA    RE,(L'BINTAB/(L'PISTA+L'PIDAY+L'PISQH+L'PIEQH))                  
         ST    RE,MYDMCB+20         MAX # OF RECORDS IN TABLE                   
         B     DMHK280                                                          
*                                                                               
DMHK270  ZICM  RE,ABINBIG,(15)      BIG WSSVR AREA                              
         MVC   MYDMCB+08(4),0(RE)   # OF RECORDS IN TABLE SO FAR                
         LA    RE,4(RE)                                                         
         ST    RE,MYDMCB+04         A(TABLE)                                    
         LA    RE,L'PISTA+L'PIDAY                                               
         ST    RE,MYDMCB+16         L(KEY)                                      
         LA    RE,L'PISQH+L'PIEQH(RE)                                           
         ST    RE,MYDMCB+12         L(RECORD)                                   
         MVI   MYDMCB+12,X01        INSERT RECORD IF NOT FOUND                  
         LHI   RE,8000              8000 RECODS                                 
         ST    RE,MYDMCB+20         MAX # OF RECORDS IN TABLE                   
         SR    RE,RE                                                            
         GOTO1 VBINSR31,MYDMCB                                                  
         TM    MYDMCB,X'80'                                                     
         BZ    *+8                                                              
         NI    MYDMCB,X'7F'                                                     
         ICM   R2,15,MYDMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ZICM  RE,ABINBIG,(15)                                                  
         MVC   0(4,RE),MYDMCB+08   UPDATE # OF RECORDS IN TABLE                 
         B     DMHK290                                                          
*                                                                               
DMHK280  GOTO1 VBINSRCH,MYDMCB                                                  
         ZICM  R2,MYDMCB+01,(7)                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RE,BINATAB                                                       
         MVC   0(4,RE),MYDMCB+08   UPDATE # OF RECORDS IN TABLE                 
                                                                                
DMHK290  CLC   6(L'PISQH,R2),PISQH                                              
         BNH   *+10                                                             
         MVC   6(L'PISQH,R2),PISQH    WANT EARLIEST START QH                    
         CLC   7(L'PIEQH,R2),PIEQH                                              
         BNL   *+10                                                             
         MVC   7(L'PIEQH,R2),PIEQH    WANT LATEST END QH                        
         GOTO1 VPROTON                                                          
         SAM24                                                                  
         DROP  R6                                                               
                                                                                
         B     DMHK300                                                          
                                                                                
DMHK300  DS    0H                  BUMP N'PROGRAMS                              
         LA    R1,1                                                             
         AH    R1,PROGRAMN                                                      
         STH   R1,PROGRAMN                                                      
         MVI   ANYDATA,C'Y'        THERE IS DATA FOR THIS REQUEST               
*                                                                               
DMHKX    DS    0H                  RETURN TO DEMAND                             
         B     EXIT                                                             
                                                                                
         DROP  R5                                                               
         EJECT                                                                  
*============= ROUTINE TO POST A RECORD TO (SORT) BUFFER =============*         
*                                                                               
         DS    0H                                                               
DEMPOST  NTR1                                                                   
         MVI   GOSUBN,DPST#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (APMODE=FORMLIN+        
               E)'                                                              
***********************************************************************         
*========================= FORMAT PRINT LINES ========================*         
                                                                                
DEMLINE  DS    0H                                                               
                                                                                
         MVC   PROGNAM,SPACES                                                   
         XC    PROGBNUM,PROGBNUM                                                
*                                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DML100               GO TO STEREO LOGIC                          
*                                                                               
         L     R5,BINAREC                                                       
         USING BINRECD,R5                                                       
         CLI   BINRTYP,PRTPRG      RECD CONTAINS PROGRAM INFO                   
         BE    DML060                                                           
         CLI   BINRTYP,PRTMSG      RECD CONTAINS INFO FOR MESSAGE               
         BE    DML050                                                           
         CLI   BINRTYP,PRTXFF      (RESERVED FOR DEM00)                         
         BE    DEMLINEX                                                         
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
** DISPLAY MESSAGE **                                                           
*                                                                               
DML050   DS    0H                  DISPLAY # OF PROGRAMS MESSAGE                
         USING BINRECD,R5                                                       
         MVC   PROGRAMN,BINRTYP+1                                               
         BAS   RE,BLDMSG            SCRNLINE=MSG, R0=L'MSG                      
                                                                                
         LA    R1,L'DEMHD3          R1=L(OUTPUT LINE)                           
         LA    RE,DEMHD3            INITIALIZE RE-->OUTPUT AREA                 
         CR    R1,R0                CHECK IF MSG FITS ON ONE LINE               
         BNH   DML055                IT DOESN'T, JUST FIT MAX IN                
         SR    R1,R0                 IT DOES, CENTER IT                         
         SRL   R1,1                                                             
         LA    RE,DEMHD3(R1)        SET A(OUTPUT AREA)                          
         LR    R1,R0                SET L(MESSAGE)                              
                                                                                
DML055   DS    0H                                                               
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),SCRNLINE                                                
         B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
** DISPLAY PROGRAM NUMBER & NAME **                                             
*                                                                               
DML060   DS    0H                                                               
         LA    R2,LDNPNAM-LISTD    ASSUME SEQUENCING BY NUMBER                  
         LA    R3,LDNPNUM-LISTD                                                 
         CLI   OPTLIST,C'A'        TEST LIST IN ALPHA SEQUENCE                  
         BNE   DML070                                                           
         LA    R2,LDAPNAM-LISTD     YES, SEQUENCE BY NAME                       
         LA    R3,LDAPNUM-LISTD                                                 
                                                                                
DML070   DS    0H                                                               
         L     R5,BINAREC                                                       
         BAS   RE,GETPRGI          EXTRACT PROGRAM INFORMATION                  
         LA    R6,LINE1                                                         
         BAS   RE,DEMMOVE          GO FORMAT LINE - 1ST COLUMN                  
                                                                                
         SR    R4,R4                                                            
         ICM   R5,15,BINLAST                                                    
         LA    R5,LINESQ(R5)                                                    
         C     R5,BINSOFAR         ANYTHING TO DISPLAY ON 2ND COLUMN?           
         BNL   DML080               NOPE, SKIP AROUND CODE                      
                                                                                
         M     R4,BINLREC           YES, THEN GET R5-->BINREC                   
         A     R5,BINATAB                                                       
         BAS   RE,GETPRGI          EXTRACT PROGRAM INFORMATION                  
         BNE   DML080               SKIP FORMATTING--NOTHING EXTRACTED          
         LA    R6,LINE1+39                                                      
         BAS   RE,DEMMOVE          GO FORMAT LINE - 2ND COLUMN                  
*                                                                               
DML080   DS    0H                  FUDGE SOME CONTROL NUMBERS                   
         ZIC   R1,NLINE                                                         
         LA    R1,1(R1)                                                         
         CLM   R1,1,MAXLINE                                                     
         BL    DEMLINEX                                                         
         ICM   R1,15,BINLAST                                                    
         LA    R1,LINESQ(R1)                                                    
         STCM  R1,15,BINLAST                                                    
         C     R1,BINSOFAR                                                      
         BL    DEMLINEX                                                         
         L     R1,BINSOFAR                                                      
         BCTR  R1,0                                                             
         STCM  R1,15,BINNEXT                                                    
*                                                                               
DEMLINEX DS    0H                                                               
         B     EXIT                                                             
                                                                                
*                                                                               
** MOVE PROGRAM INFO TO DISPLAY LINE **                                         
*                                                                               
DEMMOVE  NTR1                                                                   
         LA    R4,0(R2,R6)         FORMAT LINE - PROGRAM NAME                   
         MVC   0(L'LPNAM,R4),PROGNAM                                            
                                                                                
         LA    R4,0(R3,R6)         FORMAT LINE - PROGRAM NUMBER                 
         ZICM  R1,PROGBNUM,(7)                                                  
         EDIT  (R1),(L'LPNUM,(R4)),ALIGN=LEFT                                   
*                                                                               
         DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
DML100   DS    0H                  STEREO SESSION - USE SPECIAL FORMAT          
                                                                                
* First record is always a message record telling # of programs.                
                                                                                
         L     R2,ASTIOBUF                                                      
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         SR    R5,R5                                                            
         ICM   R5,7,TSAREC+1                                                    
         DROP  R1                                                               
                                                                                
         LA    R5,2(R5)                                                         
         USING TSDEMRCD,R5                                                      
*                                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32   DEM32                               
         BO    DMLN200                                                          
*                                                                               
         CLI   TDRRTYP,PRTPRG      RECD CONTAINS PROGRAM INFO                   
         BE    DML130                                                           
         CLI   TDRRTYP,PRTMSG      RECD CONTAINS MESSAGE INFO                   
         BE    DML110                                                           
         CLI   TDRRTYP,PRTHDR      OUTPUT COLUMN HEADINGS                       
         BE    DML120                                                           
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
** OUTPUT NUMBER-OF-PROGRAMS MESSAGE                                            
*                                                                               
DML110   DS    0H                                                               
         USING TSDEMRCD,R5                                                      
         MVC   PROGRAMN,TDRRTYP+1                                               
         DROP  R5                                                               
                                                                                
         BAS   RE,BLDMSG           SCRNLINE=MSG, R0=L'MSG                       
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),SCRNLINE                                                
         AR    R2,R1                                                            
                                                                                
         B     DML150                                                           
*                                                                               
** OUTPUT COLUMN HEADER **                                                      
*                                                                               
DML120   DS    0H                                                               
         MVC   0(L'HEADNUM1,R2),HEADNUM1                                        
         LR    R0,R2                                                            
         LA    R2,L'HEADNUM1-1(R2)                                              
         BAS   RE,TRUNCSPC                                                      
         MVI   1(R2),STSPOLST                                                   
         LA    R2,2(R2)                                                         
                                                                                
         MVC   0(L'HEADNAM1,R2),HEADNAM1                                        
         LR    R0,R2                                                            
         LA    R2,L'HEADNAM1-1(R2)                                              
         BAS   RE,TRUNCSPC                                                      
                                                                                
         B     DML150                                                           
*                                                                               
** OUTPUT PROGRAM NUMBER & NAME **                                              
*                                                                               
DML130   DS    0H                                                               
         BAS   RE,GETPRGI                                                       
                                                                                
         DS    0H                  FORMAT PROGRAM NUMBER FIRST                  
         ZICM  R1,PROGBNUM,(7)                                                  
         EDIT  (R1),(L'LPNUM,(R2)),ALIGN=LEFT                                   
         AR    R2,R0                                                            
                                                                                
         MVI   0(R2),STSPOLST      FLD SEPARATOR (LISTS USE SEMICOLON)          
         LA    R2,1(R2)                                                         
                                                                                
         MVC   0(L'LPNAM,R2),PROGNAM                                            
         LR    R0,R2                                                            
         LA    R2,L'LPNAM-1(R2)                                                 
         BAS   RE,TRUNCSPC                                                      
         B     DML150                                                           
                                                                                
                                                                                
DML150   DS    0H                                                               
         MVI   1(R2),STSPIKEY                                                   
         LA    R2,2(R2)                                                         
                                                                                
         S     R2,ASTIOBUF                                                      
         STH   R2,IODATALN                                                      
         B     DEMLINEX                                                         
*                                                                               
DMLN200  DS    0H                                                               
         MVI   GOSUBN,D32DL#                                                    
         GOTO1 AGOSUB                                                           
         B     DEMLINEX                                                         
*                                                                               
                                                                                
                                                                                
* Little routine to help truncate right-padded spaces.                          
* At entry, R0-->1st byte of string,                                            
*           R2-->last byte of string.                                           
* At exit,  R2-->last "printable" character in string.                          
                                                                                
TRUNCSPC DS    0H                                                               
         CR    R0,R2                                                            
         BER   RE                                                               
         CLI   0(R2),C' '                                                       
         BHR   RE                                                               
         BCT   R2,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
*=========================== BUILD MESSAGE ===========================*         
                                                                                
* Routine builds a number-of-programs message in SCRNLINE.                      
* At entry,                                                                     
*   PROGRAMN = the # of programs.                                               
* At exit,                                                                      
*   SCRNLINE contains message,                                                  
*   R0 = length of message.                                                     
                                                                                
         DS    0H                                                               
BLDMSG   NTR1                                                                   
         MVC   SCRNLINE,SPACES                                                  
         LA    R2,SCRNLINE                                                      
*                                                                               
         LH    R1,PROGRAMN                                                      
         EDIT  (R1),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         LA    R2,1(R2)                                                         
                                                                                
         MVC   0(7,R2),=C'program'                                              
         LA    R2,7(R2)                                                         
         CLC   PROGRAMN,=H'1'                                                   
         BNH   *+12                                                             
         MVI   0(R2),C's'                                                       
         LA    R2,1(R2)                                                         
         LA    R2,1(R2)                                                         
                                                                                
         MVC   0(2,R2),=C'in'                                                   
         LA    R2,2+1(R2)                                                       
                                                                                
         MVC   0(5,R2),STAS                                                     
         MVI   4(R2),C'T'                                                       
         LA    R2,5+1(R2)                                                       
                                                                                
         MVC   0(3,R2),=C'for'                                                  
         LA    R2,3+1(R2)                                                       
                                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(2),BKS                                                       
         GOTO1 VDATCON,DMCB,(X'83',DUB),(6,(R2))                                
         ZIC   R0,DMCB+4                                                        
         AR    R2,R0                                                            
*                                                                               
         LA    R0,SCRNLINE                                                      
         SR    R0,R2                                                            
         LPR   R0,R0               PASS LENGTH BACK IN R0                       
*                                                                               
         DS    0H                                                               
         XIT1  REGS=(R0)                                                        
         EJECT                                                                  
*======================== EXTRACT PROGRAM INFO =======================*         
                                                                                
* Routine extracts program name & number from sort record into PROGNAM          
*  and PROGBNUM respectively.                                                   
* At entry,                                                                     
*  R5-->sort record.                                                            
                                                                                
         DS    0H                                                               
GETPRGI  NTR1                                                                   
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    GPGI20               EXTRACT DATA FROM TSAR DEMO RECD            
*                                                                               
         DS    0H                                                               
         USING BINRECD,R5                                                       
         CLI   BINRTYP,PRTPRG      SEE IF RECD CONTAINS PROGRAM INFO            
         BNE   GPGIXN               NO, EXIT NOW                                
                                                                                
         MVC   PROGBNUM,BINNPNUM   VIA NUMERIC SEQUENCE (DEFAULT)               
         MVC   PROGNAM,BINNPNAM                                                 
         CLI   OPTLIST,C'A'                                                     
         BNE   *+16                                                             
         MVC   PROGNAM,BINAPNAM    VIA ALPHA SEQUENCE                           
         MVC   PROGBNUM,BINAPNUM                                                
                                                                                
         B     GPGIXY              GOT PROGRAM INFO, EXIT W/ CC EQUAL           
         DROP  R5                                                               
*                                                                               
GPGI20   DS    0H                  THIS CODE FOR STEREO SESSION ONLY            
         USING TSDEMRCD,R5                                                      
         CLI   TDRRTYP,PRTPRG      SEE IF RECD CONTAINS PROGRAM INFO            
         BNE   GPGIXN               NO, EXIT NOW                                
                                                                                
         MVC   PROGBNUM,TDRNPNUM   VIA NUMERIC SEQUENCE (DEFAULT)               
         MVC   PROGNAM,TDRNPNAM                                                 
         CLI   OPTLIST,C'A'                                                     
         BNE   *+16                                                             
         MVC   PROGNAM,TDRAPNAM    VIA ALPHA SEQUENCE                           
         MVC   PROGBNUM,TDRAPNUM                                                
                                                                                
         B     GPGIXY              GOT PROGRAM INFO, EXIT W/ CC EQUAL           
         DROP  R5                                                               
*                                                                               
GPGIXN   DS    0H                                                               
         B     NO                                                               
GPGIXY   DS    0H                                                               
         B     YES                                                              
*                                                                               
***********************************************************************         
*===================== SUBROUTINE POOL INTERFACE =====================*         
         DS    0H                                                               
GOSUB    NTR1  BASE=*,LABEL=N                                                   
*                                                                               
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'+GO'                                                  
         MVC   3(1,RE),GOSUBN                                                   
         SR    RE,RE                AND CLEAR  RE  JUST TO BE SAFE              
*                                                                               
         MVC   ASUBRTN,ASUBR01                                                  
         B     GOSUBGO                                                          
*                                                                               
GOSUBGO  DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,GOSUBN                                                        
         GOTO1 ASUBRTN,(R1)                                                     
*                                                                               
         DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK'                        
***********************************************************************         
*======================== LITERALS & CONSTANTS =======================*         
         LTORG                                                                  
         SPACE 2                                                                
HEADNAM1 DC    CL(L'LPNAM)'PROGRAM NAME'                                        
HEADNAM2 DC    CL(L'LPNAM)'------------'                                        
HEADNUM1 DC    CL(L'LPNUM)'PROGRAM#'                                            
HEADNUM2 DC    CL(L'LPNUM)'--------'                                            
         SPACE 2                                                                
LINESQ   EQU   (DEMLAST-DEMLN1H)/(L'DEMLN1+L'DEMLN1H)-1                         
***********************************************************************         
         SPACE 2                                                                
         DROP  R7,R8,R9,RB                                                      
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (SUBR01)'               
*********************************************************                       
*=============SUBROUTINE POOL ==========================                        
* AT ENTRY ,                                                                    
* R9 --->DEMWRKD                                                                
* R8 ---> TWA                                                                   
* R7 --->DEMTMPD                                                                
* R1 ---> EQUATED SUBROUTINE NUMBER                                             
SUBR01Q  EQU   (((*-DEM09+4095)/4096)*4096)                                     
         ORG   DEM09+SUBR01Q                                                    
SUBR01   NMOD1 0,**0901**                                                       
         USING DEMWRKD,R9                                                       
         USING DEMTWAD,R8                                                       
         USING DEMTMPD,R7                                                       
*                                                                               
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
*                                                                               
D32DL#   EQU   (R01_01-*)/4+1      DEMLINE PROCESSING FOR DEM32                 
D32PG#   EQU   (R01_02-*)/4+1      DOWNLOAD PROGRAM LIST                        
OKDWNLD# EQU   (R01_03-*)/4+1      OKAY TO DOWNLAD ?                            
GADLD#   EQU   (R01_04-*)/4+1      GET A(DOWNLOAD DATA TABLES)                  
IDB#     EQU   (R01_05-*)/4+1      INITIALIZE DBLOCK                            
RPI#     EQU   (R01_06-*)/4+1      GET PROGRAM INFORMATION                      
DPST#    EQU   (R01_07-*)/4+1      CALLS POST ROUTINE                           
QTM#     EQU   (R01_08-*)/4+1      CONVERT QH TO MILITARY TIME                  
*                                                                               
R01_00   DS    0H                                                               
R01_01   B     D32DEMLN            DEMLINE PROCESSING FOR DEM32                 
R01_02   B     D32DLPG             DOWNLOAD MARKET LIST                         
R01_03   B     OKDOWNLD            OKAY TO DOWNLOAD?                            
R01_04   B     GADLDTAB            GET A(DOWNLOAD DATA TABLE)                   
R01_05   B     INITDBLK            INITIALIZE DBLOCK                            
R01_06   B     RDPRGINF            READ FOR PROGRAM INFORMATION                 
R01_07   B     DMPST               CALLS POST ROUTINE                           
R01_08   B     QHRTOMIL            CONVERT QH TO MILITARY TIME                  
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
YES_STE  SR    R9,R9                                                            
NO_STE   LTR   R9,R9                                                            
XIT_STE  XIT1                                                                   
*********************************************************                       
*-------------------- GET A(DOWNLOAD DATA TABLES) --------------------*         
                                                                                
* Gets address of corresponding download data entry                             
* At entry,                                                                     
*   TMPRCTYP = TSAR demo record type                                            
* At exit,                                                                      
*   ADLDNTRY = A(download data table entry)                                     
                                                                                
GADLDTAB DS    0H                                                               
         ICM   R2,15,ADLDTABS      R2-->DOWNLOAD DATA TABLES                    
         BZ    GADLDX                                                           
         USING DLDTABD,R2                                                       
                                                                                
*                                                                               
GADLD012 DS    0H                                                               
         CLI   DLDTLEN,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DLDTFIL,DBFIL                                                    
         BNE   GADLD018                                                         
         CLC   DLDTSRC,DBSRC                                                    
         BNE   GADLD018                                                         
         CLC   DLDTMED,DBMED                                                    
         BNE   GADLD018                                                         
         CLC   DLDTRTYP,TMPRTYP                                                 
         BNE   GADLD018                                                         
         CLC   DLDTVRSN,D32PCVER                                                
         BH    GADLD018                                                         
         B     GADLD019                                                         
*                                                                               
GADLD018 DS    0H                  BUMP TO NEXT DOWNLOAD DATA ENTRY             
         ZIC   R0,DLDTLEN                                                       
         AR    R2,R0                                                            
         B     GADLD012                                                         
GADLD019 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         B     GADLDX                                                           
                                                                                
*                                                                               
GADLDX   DS    0H                                                               
         ST    R2,ADLDNTRY                                                      
         J     EXIT                                                             
         DROP  R2                                                               
         TITLE 'DEDEM02 - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02--OKD+        
               L#)'                                                             
*------------------------- OKAY TO DOWNLOAD? -------------------------*         
                                                                                
* See if data can be downloaded to PC                                           
* At entry,                                                                     
*   TMPRCTYP = TSAR demo record type                                            
*   FALEMPC  = element map code                                                 
*   FALDMPC  = data    map code                                                 
* At exit,                                                                      
*   CC set to eql if data can be downloaded                                     
*   CC set to neq if otherwise                                                  
                                                                                
OKDOWNLD DS    0H                                                               
         MVI   GOSUBN,GADLD#                                                    
         GOTO1 AGOSUB                                                           
         ICM   R2,15,ADLDNTRY                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                  R2-->DOWNLOAD DATA TABLE ENTRY               
         LA    R3,DLDTFIXL(R2)                                                  
         SR    R0,R0                                                            
*                                                                               
OKDL022  DS    0H                  BUMP TO SECTION FOR ELEMENT MAP CODE         
         CLI   0(R3),0                                                          
         BE    OKDLXN                                                           
         CLC   FALEMPC,1(R3)                                                    
         BE    *+14                                                             
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     OKDL022                                                          
*                                                                               
         LA    R4,3(R3)                                                         
OKDL025  DS    0H                                                               
         OC    0(L'FALDMPC,R4),0(R4)                                            
         BZ    OKDLXN                                                           
         CLC   0(L'FALDMPC,R4),FALDMPC                                          
         BE    *+12                                                             
         LA    R4,2(R4)                                                         
         B     OKDL025                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         B     OKDLXY                                                           
                                                                                
*                                                                               
OKDLXN   DS    0H                                                               
         J     NO                                                               
OKDLXY   DS    0H                                                               
         J     YES                                                              
*********************************************************                       
D32DEMLN  DS    0H                                                              
          USING TSDEMRCD,R5                                                     
          MVC   TMPRTYP,TDRRTYP                                                 
          CLI   TDRRTYP,PRTPRG                                                  
          BE    D32DL100                                                        
          B     D32DLX                                                          
D32DL100  DS    0H                                                              
          MVI   GOSUBN,D32PG#                                                   
          GOTO1 AGOSUB                                                          
D32DLX    B     XIT_STE                                                         
******************* DEM32 DOWNLOAD MARKET LIST *******                          
D32DLPG   DS    0H                                                              
          USING TSDEMRCD,R5                                                     
          MVC   FALEMPC,=Y(FMHPRGS)                                             
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
*                                                                               
          DS    0H                  PROGRAM CODE                                
          MVC   FALDMPC,=Y(FMDPRCD)                                             
          MVC   WORK,SPACES                                                     
          ZICM  R1,TDRNPNUM,(7)                                                 
          EDIT  (R1),(L'LPNUM,WORK),ALIGN=LEFT                                  
          CLI   OPTLIST,C'A'                                                    
          BNE   D32PG30                                                         
          MVC   WORK,SPACES                                                     
          ZICM  R1,TDRAPNUM,(7)                                                 
          EDIT  (R1),(L'LPNUM,WORK),ALIGN=LEFT                                  
D32PG30   LA    R0,WORK                                                         
          LA    R1,L'LPNUM                                                      
          BAS   RE,D32TPAD                                                      
*                                                                               
          DS    0H                  PROGRAM NAME                                
          MVC   FALDMPC,=Y(FMDPRNM)                                             
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRNPNAM),TDRNPNAM  ASSUME LIST BY ALPHA SEQ             
          CLI   OPTLIST,C'A'                                                    
          BNE   D32PG40                                                         
          MVC   WORK(L'TDRAPNAM),TDRAPNAM                                       
D32PG40   LA    R0,WORK                                                         
          LA    R1,14                                                           
          BAS   RE,D32TPAD                                                      
*                                                                               
          B     XIT_STE                                                         
*                                                                               
          DROP  R5                                                              
*                                                                               
D32TPAD   NTR1                                                                  
          ST    R0,ADLDATA                                                      
          ST    R1,LDLDATA                                                      
          MVI   ADMODE,ADMADD                                                   
          GOTO1 ADM32ADD                                                        
          XIT1                                                                  
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (SUBR01--IDB#)'         
*------------------------- INITIALIZE DBLOCK -------------------------*         
                                                                                
* Initializes a DEMO BLOCK in DBLOCK1.                                          
                                                                                
INITDBLK DS    0H                                                               
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
                                                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIOAREA1     I/O AREA                                     
         MVC   DBCOMFCS,AFAC       COMFACS                                      
         MVC   DBFILE,DBFIL        FILE                                         
         MVC   DBSELMED,DBMED      MEDIA                                        
         MVC   DBSELSRC,DBSRC      SOURCE                                       
         MVC   DBSELAGY,AGYALPH    AGENCY ALPHA                                 
         MVC   DBSELBK,BKS         BOOK                                         
                                                                                
         DROP  R5                                                               
*                                                                               
IDBX     DS    0H                                                               
         J     EXIT                                                             
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (SUBR01--RPI#)'         
*-------------------- READ FOR PROGRAM INFORMATION -------------------*         
                                                                                
* This routine should only be called under a DEM32 session.                     
*  Routine goes through BINATAB, where STTN/DAY/SQH/EQH are stored,             
*  and reads the demo records for the STTN/DAY/SQH/EQH for the                  
*  program information.                                                         
                                                                                
RDPRGINF DS    0H                                                               
         MVI   GOSUBN,IDB#         INITIALIZE DBLOCK                            
         GOTO1 AGOSUB                                                           
                                                                                
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBBTYPE,BKS+3                                                    
         MVI   DBSELWKN,X'FF'      WANT ALL WEEKS                               
**       MVI   DBSELWKN,1                                                       
         MVC   DBSELAGY,=C'SJ'     GIVE 'EM ALL PROGRAM NAMES                   
*                                                                               
RPI010   DS    0H                                                               
         L     R3,ASTBUFFR         SET UP DAY/TIME EXTEND LINK                  
         STCM  R3,15,DBEXTEND                                                   
         USING DBXTLD,R3                                                        
         MVC   DBXTLID,=C'DYTM'                                                 
         XC    DBXTLNXT,DBXTLNXT                                                
                                                                                
         L     R2,BINATAB                                                       
         SAM31                                                                  
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BNO   *+8                                                              
         ICM   R2,15,ABINBIG           BIG WSSVR AREA                           
*                                                                               
*                                                                               
         L     R0,0(R2)            R0 = # OF ENTRIES IN TABLE                   
         LA    R2,4(R2)                                                         
         USING SDQTABD,R2                                                       
                                                                                
*                                                                               
RPI020   DS    0H                  START OF READING PROGRAM INFO LOOP           
         MVI   DBXTLIDX,0           RESET INDEX                                 
         MVC   DBSELSTA,SDQSTA      SET STATION                                 
*                                                                               
RPI022   DS    0H                   START OF DAY/TIME EXTEND LOOP               
         ST    R0,COUNTER            SET COUNTER                                
*                                                                               
         ZIC   R1,DBXTLIDX                                                      
         LR    R4,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,DBXTLIDX           UPDATE DAY/TIME INDEX                      
                                                                                
         MH    R4,=Y(L'DBXTLIST)                                                
         LA    R4,DBXTLIST(R4)       R4-->NEXT AVAILABLE DAY/TIME SLOT          
*                                                                               
         MVI   0(R4),0                                                          
         L     R1,ADAYTAB                                                       
RPI026A  CLI   0(R1),X'FF'                                                      
         BE    RPI026X                                                          
         CLC   SDQDAY,1(R1)                                                     
         BE    RPI026C                                                          
         LA    R1,L'DAYTAB(R1)                                                  
         B     RPI026A                                                          
RPI026C  MVC   0(1,R4),2(R1)          SET DAY                                   
RPI026X  EQU   *                                                                
*                                                                               
         MVI   GOSUBN,QTM#            CONVERTING QHRS TO MIL TIMES              
         MVC   TEMPQHR,SDQSQH                                                   
         GOTO1 AGOSUB                                                           
         MVC   1(2,R4),TEMPMTIM        SET START TIME                           
         ZIC   R1,SDQEQH                                                        
         LA    R1,1(R1)                                                         
         STC   R1,TEMPQHR                                                       
         GOTO1 AGOSUB                                                           
         MVC   3(2,R4),TEMPMTIM        AND END TIME                             
*                                                                               
         DS    0H                                                               
         L     R0,COUNTER                                                       
         SH    R0,=H'1'                                                         
         BZ    RPI032X                                                          
         BP    *+8                                                              
         J     EXIT                                                             
****     BP    *+6                                                              
****     DC    H'0'                    DIE IF COUNT IS ZERO                     
         LA    R2,SDQLENQ(R2)                                                   
         CLC   DBSELSTA,SDQSTA                                                  
         BE    RPI022                                                           
RPI032X  EQU   *                                                                
                                                                                
         DROP  R2                                                               
         DROP  R3                                                               
*                                                                               
RPI040   MVC   PROGNAM,SPACES      INITIALIZE PROGRAM VALUES                    
         MVC   PROGNUM,SPACES                                                   
         MVC   HALF,DBSELAGY                                                    
         MVC   DBSELAGY,=C'SJ'                                                  
         SAM24                                                                  
         GOTO1 ASETUSID                                                         
         XC    DBBTYPE,DBBTYPE                                                  
         GOTO1 VDEMAND,DMCB,DBLOCKD,DEMHOOK2,0,0,0,0                            
RPI070   DS    0H                                                               
         L     R0,COUNTER                                                       
         SAM31                                                                  
         BCT   R0,RPI020                                                        
         SAM24                                                                  
         MVC   DBSELAGY,HALF                                                    
                                                                                
*&&DO                                                                           
         CLI   DBSELWKN,4                                                       
         JE    EXIT                                                             
         ZIC   RE,DBSELWKN                                                      
         AHI   RE,1                                                             
         STC   RE,DBSELWKN                                                      
         J     RPI010                                                           
*&&                                                                             
         DS    0H                                                               
         J     EXIT                                                             
                                                                                
         DROP  R5                                                               
         EJECT                                                                  
*======= ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND =======*         
                                                                                
* At entry,                                                                     
*   R5-->DBLOCKD.                                                               
                                                                                
DEMHOOK2 DS    0H                                                               
DHK2     NTR1                                                                   
         DS    0XL(1-(DHK2-DEMHOOK2))                                           
                                                                                
         USING DBLOCKD,R5                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   DBRECTYP,DBRECDEM   MAKE SURE WE HAVE CORRECT RECD TYPE          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
DEMHK210 MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'PROGRA',DBLOCK1,WORK                           
         MVC   PROGNAM,WORK                                                     
*                                                                               
DEMHK211 DS    0H                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'PSOUR',DBLOCK1,WORK                            
         CLC   =C'P ',WORK         IGNORE PUBLIC PROGRAMMING SOURCE             
         BE    DHK2X                                                            
* TP AFFILATION                                                                 
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'AFFL',DBLOCK1,WORK                             
         CLC   =C'ION',WORK         ION                                         
         BE    DHK2X                                                            
         CLC   WORK(5),SPACES      ONLY STATIONS W AFFILATIONS                  
         BNH   DHK2X                                                            
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'MAR',DBLOCK1,WORK                              
         L     RF,AFAC                                                          
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,LPMUPG                                                 
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
         USING LPMUDATD,RE                                                      
DEMHK216 CLC   =X'FFFF',0(RE)      IN TABLE                                     
         BE    DHK2X               ONLY KEEP LPM MARKETS                        
         CLC   LPMUNMKT,WORK                                                    
         BE    DEMHK218                                                         
         AR    RE,R0                                                            
         B     DEMHK216                                                         
*                                                                               
DEMHK218 DS    0H                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'TPNO',DBLOCK1,WORK                             
         CLC   WORK(L'PROGNUM),SPACES                                           
         BE    DHK2X                                                            
         CLC   WORK(L'PROGNUM),=CL8'N/A'                                        
         BE    DHK2X                                                            
         MVC   PROGNUM,WORK                                                     
         PACK  DUB,PROGNUM                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    DHK2X                                                            
         STCM  R0,7,PROGBNUM       PROGRAM NUMBER IN BINARY                     
*&&DO                                                                           
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'PROGRA',DBLOCK1,WORK                           
         MVC   PROGNAM,WORK                                                     
*&&                                                                             
         MVI   POSTRTYP,PRTPRG     PROGRAM RECORD TO POST                       
         MVI   GOSUBN,DPST#                                                     
         GOTO1 AGOSUB               AND GO POST IT                              
                                                                                
*                                                                               
DHK2X    DS    0H                  RETURN TO DEMAND                             
         J     EXIT                                                             
                                                                                
         DROP  R5                                                               
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (SUBR01--DPST#)+        
               '                                                                
*--------------------------- POST TO BUFFER --------------------------*         
                                                                                
* This routine posts data to buffer                                             
                                                                                
DMPST    DS    0H                                                               
         CLI   POSTRTYP,PRTPRG     POST PROGRAM INFO                            
         BE    DMPST100                                                         
         CLI   POSTRTYP,PRTMSG     POST MESSAGE INFO                            
         BE    DMPST50                                                          
         CLI   POSTRTYP,PRTHDR     POST HEADER  INFO                            
         BE    DMPST150                                                         
         DC    H'0'                                                             
*                                                                               
** POSTING MESSAGE INFO **                                                      
*                                                                               
DMPST50  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DMPST60              POST THE STEREO WAY                         
*                                                                               
         DS    0H                                                               
         XC    POSTLINE,POSTLINE                                                
         LA    R5,POSTLINE                                                      
         USING BINRECD,R5                                                       
         MVI   BINRTYP,PRTMSG                                                   
         MVC   BINRTYP+1(2),PROGRAMN                                            
         DROP  R5                                                               
         B     DMPST70                                                          
*                                                                               
DMPST60  DS    0H                  POST MESSAGE FOR STEREO SESSION              
         CLI   ANYDATA,C'Y'         IF THERE WERE NO DATA,                      
         BNE   DMPST79               DON'T POST MSG RECORD                      
*                                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
                                                                                
         MVC   0(2,R2),TSRECL         MOVE IN LENGTH OF RECORD                  
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
         XC    0(TDRLENQ,R2),0(R2)                                              
         MVI   TDRRTYP,PRTMSG                                                   
         MVC   TDRRTYP+1(2),PROGRAMN  MOVE IN # OF PROGRAMS                     
         DROP  R2,R4                                                            
         B     DMPST70                                                          
*                                                                               
DMPST70  DS    0H                                                               
         GOTO1 APOST               POST MESSAGE INFO RECD TO BUFFER             
*                                                                               
DMPST79  DS    0H                                                               
         J     EXIT                                                             
*                                                                               
** POSTING PROGRAM INFO **                                                      
*                                                                               
DMPST100 DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DMPST120             POST THE STEREO WAY                         
*                                                                               
         XC    POSTLINE,POSTLINE                                                
         LA    R5,POSTLINE                                                      
         USING BINRECD,R5                                                       
         MVI   BINRTYP,PRTPRG                                                   
                                                                                
         MVC   BINNPNUM,PROGBNUM                                                
         MVC   BINNPNAM,PROGNAM                                                 
         CLI   OPTLIST,C'A'                                                     
         BNE   *+16                                                             
         MVC   BINAPNAM,PROGNAM                                                 
         MVC   BINAPNUM,PROGBNUM                                                
         DROP  R5                                                               
         B     DMPST140                                                         
*                                                                               
DMPST120 DS    0H                  POST PROGRAM INFO FOR STEREO SESSION         
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
                                                                                
         MVC   0(2,R2),TSRECL                                                   
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
         XC    0(TDRLENQ,R2),0(R2)                                              
         MVI   TDRRTYP,PRTPRG                                                   
                                                                                
         MVC   TDRNPNUM,PROGBNUM                                                
         MVC   TDRNPNAM,PROGNAM                                                 
         CLI   OPTLIST,C'A'                                                     
         BNE   *+16                                                             
         MVC   TDRAPNAM,PROGNAM                                                 
         MVC   TDRAPNUM,PROGBNUM                                                
         DROP  R2,R4                                                            
         B     DMPST140                                                         
*                                                                               
DMPST140 DS    0H                                                               
         GOTO1 APOST               POST PROGRAM INFO RECD TO BUFFER             
*                                                                               
         J     EXIT                                                             
*                                                                               
** POSTING COLUMN HEADER RECORD **                                              
*                                                                               
DMPST150 DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   THIS ONLY APPLIES TO STEREO SESSION          
         BZ    DMPST159                                                         
         CLI   ANYDATA,C'Y'         IF THERE WERE NO DATA,                      
         BNE   DMPST159              DON'T POST HDR RECORD                      
*                                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
                                                                                
         MVC   0(2,R2),TSRECL                                                   
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
         XC    0(TDRLENQ,R2),0(R2)                                              
         MVI   TDRRTYP,PRTHDR                                                   
         DROP  R2,R4                                                            
                                                                                
         DS    0H                                                               
         GOTO1 APOST               POST COLUMN HEADER RECD TO BUFFER            
*                                                                               
DMPST159 DS    0H                                                               
         J     EXIT                                                             
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (SUBR01--QTM#)'         
*-------------------- CONVERT QH TO MILITARY TIME --------------------*         
                                                                                
* At entry,                                                                     
*   TEMPQHR  = input quarter hour.                                              
* At exit,                                                                      
*   TEMPMTIM = output military time.                                            
                                                                                
QHRTOMIL DS    0H                                                               
         SR    R0,R0                                                            
         ZIC   R1,TEMPQHR                                                       
         D     R0,=F'4'                                                         
         LA    R1,5(R1)            BASE = 5AM IF RADIO,                         
         CLI   DBMED,C'R'                                                       
         BE    *+8                                                              
         LA    R1,1(R1)             ELSE, BASE = 6AM                            
         CH    R1,=H'24'           TEST AFTER MIDNIGHT                          
         BL    *+8                                                              
         SH    R1,=H'24'           YES - GO BACK ONE DAY                        
         MH    R1,=H'100'                                                       
         MH    R0,=H'15'                                                        
         AR    R1,R0               R1 CONTAINS MILITARY TIME                    
         OR    R1,R1                                                            
         BNZ   *+8                                                              
         LH    R1,=H'2400'                                                      
         STCM  R1,3,TEMPMTIM                                                    
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (SUBR01--MISC S+        
               TUFF)'                                                           
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'1000'-SUBR01L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,R9,RB                                                      
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (TABLES)'               
***********************************************************************         
*================================ TABLES ==============================*        
                                                                                
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0AL(2+2)                                                         
         DC    AL2(GOSUB-DEM09),AL2(AGOSUB-DEMTMPD)                             
         DC    AL2(SUBR01-DEM09),AL2(ASUBR01-DEMTMPD)                           
         DC    AL2(DAYTAB-DEM09),AL2(ADAYTAB-DEMTMPD)                           
         DC    AL2(DLDATTAB-DEM09),AL2(ADLDTABS-DEMTMPD)                        
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
                                                                                
*                                  TABLE TO CONVERT KEY DAY VALUES              
DAYTAB   DS    0XL(1+1+1+3)                                                     
         DC     X'0',X'10',X'40',C'MON'                                         
         DC     X'0',X'20',X'20',C'TUE'                                         
         DC     X'0',X'30',X'10',C'WED'                                         
         DC     X'0',X'40',X'08',C'THU'                                         
         DC     X'0',X'50',X'04',C'FRI'                                         
         DC     X'0',X'60',X'02',C'SAT'                                         
         DC     X'0',X'70',X'01',C'SUN'                                         
         DC     C'T',X'95',X'7C',C'M-F'                                         
         DC     C'T',X'FF',X'FF',C'VAR'                                         
         DC     X'FF',X'FF',X'FF',C'???'                                        
                                                                                
                                                                                
*                                                                               
DLDATTAB    DS  0D                                                              
*                                                                               
***********************************************************************         
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (DEDEMWRK)'             
***********************************************************************         
*============================== DEDEMWRK =============================*         
       ++INCLUDE DEDEMWRK                                                       
         ORG   APSAVE                                                           
         SPACE 1                                                                
* SAVE STORAGE FOR OVERLAY                                                      
*                                                                               
ANYDATA  DS    CL1                 ANY DATA FOR THIS REQUEST? (Y/N)             
         ORG                                                                    
***********************************************************************         
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (DEDBEXTRAD)'           
***********************************************************************         
*============================= DEDBEXTRAD ============================*         
                                                                                
       ++INCLUDE DEDBEXTRAD                                                     
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
       ++INCLUDE FAWSSVRD                                                       
         EJECT                                                                  
***********************************************************************         
         TITLE 'DEDEM09 - $DEM LIST PROGRAMS FOR A BOOK (MISC DSECTS)'          
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
                                                                                
* DSECT TO COVER TEMPORARY WORKING STORAGE                                      
*                                                                               
DEMTMPD  DSECT                                                                  
MYDMCB   DS    6F                                                               
COUNTER  DS    F                                                                
PROGRAMN DS    H                   N'PROGRAMS FOR BOOK                          
                                                                                
POSTRTYP DS    XL1                 TYPE OF RECORD TO POST                       
PRTMSG   EQU   X'10'                RECD HAS INFO FOR MESSAGE                   
PRTHDR   EQU   X'20'                RECD HAS COLUMN HEADER NAMES                
PRTPRG   EQU   X'30'                RECD HAS PROGRAM INFORMATION                
PRTXFF   EQU   X'FF'                (RESERVED FOR DEM00 USE)                    
PROGBNUM DS    XL3                 PROGRAM NUMBER IN BINARY                     
PROGNUM  DS    XL8                 PROGRAM NUMBER FROM DEFINE                   
PROGNAM  DS    CL16                PROGRAM NAME FROM DEFINE                     
TEMPQHR  DS    XL1                 TEMP STORAGE FOR QUARTER HOUR                
TEMPMTIM DS    XL2                  "      "     "  MILITARY TIME               
TMPRTYP  DS    XL(L'TDRRTYP)       TEMP STORAGE FOR TSAR DEM RECD TYP           
GOSUBN   DS    XL1                 SUB-ROUTINE NUMBER                           
*                                                                               
RELO09   DS    F                                                                
         DS    0A                 *************** ADCONS **************         
AGOSUB   DS    A                   A(SUBROUTINE POOL INTERFACE)                 
ASUBR01  DS    A                   A(SUBR01 POOL INTERFACE)                     
ADAYTAB  DS    A                   A(DAYTAB)                                    
ADLDTABS DS    A                                                                
                                                                                
ADLDNTRY DS    A                                                                
ASUBRTN  DS    A                                                                
                                                                                
ABINBIG  DS    A                                                                
VBINSR31 DS    V                                                                
VPROTOFF DS    V                                                                
VPROTON  DS    V                                                                
SCRNLINE DS    CL(L'DEMHD1)                                                     
         DS    0CL(2000-(*-DEMTMPD)+1)                                          
                                                                                
                                                                                
         EJECT                                                                  
* DSECT TO COVER BINSRCH RECORD                                                 
*                                                                               
BINRECD  DSECT                                                                  
BINKEY   DS    0C                  BINSEARCH KEY                                
BINRTYP  DS    XL1                  RECORD TYPE                                 
                                                                                
BINNKEY  DS    0C                   SORTING IN NUMERIC SEQUENCE                 
BINNPNUM DS    XL3                   PROGRAM NUMBER                             
BINNKEYL EQU   *-BINKEY                                                         
BINNPNAM DS    CL(L'LPNAM)           PROGRAM NAME                               
                                                                                
         ORG   BINNKEY                                                          
BINAKEY  DS    0C                   SORTING IN ALPHA SEQUENCE                   
BINAPNAM DS    CL(L'LPNAM)           PROGRAM NAME                               
BINAKEYL EQU   *-BINKEY                                                         
BINAPNUM DS    XL3                   PROGRAM NUMBER                             
                                                                                
         ORG                                                                    
BINEND   EQU   *                                                                
                                                                                
                                                                                
* DSECT TO COVER TSAR DEMO RECORD                                               
*                                                                               
TSDEMRCD DSECT                                                                  
TDRKEY   DS    0X                  TSAR DEMO RECORD KEY                         
TDRRTYP  DS    XL1                  RECORD TYPE                                 
                                                                                
TDRNKEY  DS    0X                   LIST BY PROGRAM NUMBER SEQUENCE             
TDRNPNUM DS    XL3                   PROGRAM NUMBER                             
TDRNKEYL EQU   *-TDRKEY                                                         
TDRNPNAM DS    CL(L'LPNAM)           PROGRAM NAME                               
         ORG   TDRNKEY                                                          
TDRAKEY  DS    0X                   LIST BY PROGRAM NAME SEQUENCE               
TDRAPNAM DS    CL(L'LPNAM)           PROGRAM NAME                               
TDRAKEYL EQU   *-TDRKEY                                                         
TDRAPNUM DS    XL3                   PROGRAM NUMBER                             
         ORG                                                                    
                                                                                
TDRLENQ  EQU   *-TSDEMRCD                                                       
         EJECT                                                                  
* DSECT TO COVER AN ENTRY ON DISPLAY LINE                                       
*                                                                               
LISTD    DSECT                                                                  
LPNAM    DS    0CL14                                                            
LPNUM    DS    0CL10                                                            
*                                  ALPHA SEQUENCE                               
LDAPNAM  DS    CL(L'LPNAM)                                                      
         DS    CL3                                                              
LDAPNUM  DS    CL(L'LPNUM)                                                      
         ORG   LISTD                                                            
*                                  NUMBER SEQUENCE                              
LDNPNUM  DS    CL(L'LPNUM)                                                      
         DS    CL1                                                              
LDNPNAM  DS    CL(L'LPNAM)                                                      
LISTQ    EQU   *-LISTD                                                          
***********************************************************************         
DLDTABD  DSECT                                                                  
DLDTLEN  DS    XL1                 L(ENTRY)                                     
DLDTFMS  DS    0CL(L'DBFIL+L'DBSRC+L'DBMED)                                     
DLDTFIL  DS     CL(L'DBFIL)         FILE                                        
DLDTSRC  DS     CL(L'DBSRC)         SOURCE                                      
DLDTMED  DS     CL(L'DBMED)         MEDIA                                       
DLDTRTYP DS    XL(L'TDRRTYP)      TSAR DEMO RECORD TYPE                         
DLDTVRSN DS    XL(L'D32PCVER)      STEREO DEM EXTRACT VERSION                   
DLDTFIXL EQU   *-DLDTABD                                                        
                                                                                
DLDTDATA DS    0X                                                               
                                                                                
                                                                                
* DSECT TO COVER STATION/DAY/QHs TABLE                                          
*                                                                               
SDQTABD  DSECT                                                                  
SDQSTA   DS    CL(L'PISTA)         STATION                                      
SDQDAY   DS    XL(L'PIDAY)         DAY                                          
SDQSQH   DS    XL(L'PISQH)         START QH                                     
SDQEQH   DS    XL(L'PIEQH)         END   QH                                     
SDQLENQ  EQU   *-SDQTABD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093DEDEM09   01/04/11'                                      
         END                                                                    
