*          DATA SET DEPREDALY  AT LEVEL 141 AS OF 06/22/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEPREDLA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DDINFO                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
         TITLE 'PRE-PROCESSOR FOR NIELSEN LOCAL DAILIES'                        
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
*                                                                               
DEPREDLY CSECT                                                                  
         NBASE 0,DEPREDLY,=V(REGSAVE),RA                                        
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         MVC   TITLE(27),=C'LOCAL DAILIES PRE-PROCESSOR'                        
         LA    R2,FULL                                                          
         EXTRACT (2),'S',FIELDS=(ASID)                                          
         LA    R1,FULL                                                          
         L     R2,0(R1)                                                         
         LOCASCB ASID=(R2)         GET ASCB ADDRESS INTO R1                     
         L     R2,ASCBASSB-ASCB(R1) R2 = A(ASSB)                                
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
         MVC   TITLE+38(2),=C'(J'                                               
         L     R1,ASSBJSAB-ASSB(R2) R1 = A(JSAB)                                
         MVC   TITLE+40(5),JSABJBID-JSAB+3(R1)  JOB#####                        
         MVI   TITLE+45,C')'                                                    
         SAM24 ,                   SWITCH BACK TO 24-BIT MODE                   
         LA    R2,FULL                                                          
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,FULL                                                          
         MVC   TITLE+30(8),0(R2)   JOBNAME                                      
*                                                                               
READCARD DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         EOF?                                         
         BE    MAIN10                                                           
*                                                                               
         MVC   P(L'CARD),CARD                                                   
         GOTO1 =V(PRINTER)         PRINT PARAMETER CARD                         
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    READCARD            YES: IGNORE                                  
*                                                                               
         CLC   =C'DSPACE=',CARD                                                 
         BNE   CHKDDSIO                                                         
         LARL  RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(,RF),CARD+7   DSPACE=X                           
         B     READCARD                                                         
*                                                                               
CHKDDSIO DS    0H                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   CHKALLOC                                                         
         ICM   RF,15,=V(DDSIO)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(8,RF),CARD+6      OVERRIDE DDSIO PHASE NAME                    
         B     READCARD                                                         
*                                                                               
CHKALLOC DS    0H                                                               
         CLC   =C'ALLOCATE=',CARD                                               
         BNE   CHKTRACE                                                         
         CLI   CARD+9,C'Y'                                                      
         BE    READCARD            DEFAULT IS ALLOCATE=YES                      
         CLI   CARD+9,C'N'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ALLOCFLG,C'N'       ALLOCATE=NO                                  
         B     READCARD                                                         
*                                                                               
CHKTRACE DS    0H                                                               
         CLC   =C'TRACE=',CARD                                                  
         BE    *+6                                                              
         DC    H'0'                INVALID PARAMETER CARD!                      
         CLI   CARD+6,C'N'                                                      
         BE    READCARD            DEFAULT IS NO TRACE                          
         CLI   CARD+6,C'Y'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACEFLG,C'Y'       TRACE=YES                                    
         B     READCARD                                                         
*                                                                               
MAIN10   DS    0H                                                               
         OPEN  (FILOUT,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'A',=C'FILIN1  ')                            
         CLI   DMCB+4,0            IS FILIN1 ALLOCATED VIA DD STMT?             
         BE    DO_NFS              NO                                           
*                                                                               
         MVC   P(36),=C'FILIN1 IS ALLOCATED VIA DD STATEMENT'                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVI   HAVEDATA,C'Y'       YES: WE HAVE SOMETHING TO CONVERT            
         MVI   USINGNFS,C'N'       WE WILL READ VIA NFS                         
         MVI   SKIP_ALL,C'N'       NOT AN "IGNORE ALL FILES" SITUATION          
         B     OPENFILS                                                         
         EJECT                                                                  
*                                                                               
* IF WE GET HERE, THEN THERE IS NO FILIN1 DD STATEMENT IN THE JCL.              
* THAT MEANS THAT WE WILL READ ALL THE FILES IN AN NFS-MOUNTED FOLDER           
* TO GET THE DATA. THE FILENAMES IN THE FOLDER WILL BE IN THE DATASET           
* WITH DDNAME NFSFILES, AND IS PRODUCED BY THE "DDNFSNAMES" PROGRAM             
* (WHICH MUST RUN PRIOR TO THIS PROGRAM).                                       
*                                                                               
* FOR EACH FILE IN THE NFS-MOUNTED FOLDER, WE NEED TO:                          
*  1. DYNAMICALLY ALLOCATE THE FILE                                             
*  2. OPEN IT                                                                   
*  3. READ ITS CONTENTS                                                         
*  4. CLOSE IT                                                                  
*  5. DEALLOCATE THE DDNAME                                                     
*                                                                               
DO_NFS   DS    0H                                                               
         MVC   P(36),=C'FILIN1 WILL BE DYNAMICALLY ALLOCATED'                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVI   HAVEDATA,C'N'       ASSUME MOUNTED FOLDER IS EMPTY               
         OPEN  NFSFILES            CONTAINS FILENAMES IN MOUNTED FOLDER         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FILELOOP DS    0H                                                               
         XC    IOAREA(256),IOAREA                                               
         GET   NFSFILES,IOAREA     GET THE PATHNAME                             
*                                                                               
         MVI   EMPTYNFS,C'N'       THERE'S AT LEAST 1 FILE TO EXAMINE           
         SR    R2,R2                                                            
         ICM   R2,3,IOAREA         RECLEN (FROM RDW)                            
*                                                                               
         MVC   P(10),=C'PATHNAME: '                                             
         LR    RF,R2                                                            
         SHI   RF,1+4              1 FOR EX, 4 FOR RDW                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P+10(0),IOAREA+4    PRINT PATHNAME                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*                                  EXTRACT THE 1ST FILENAME QUALIFIER           
         LA   R1,IOAREA            A(START OF PATHNAME)                         
         AR   R1,R2                R1 = A(JUST PAST END OF RECORD)              
         DO   UNTIL=(CLI,0(R1),EQ,C'/') BUMP BACKWARD TO FILENAME START         
           SHI R1,1                                                             
         ENDDO ,                                                                
         MVC  EMPTYWNM,1(R1)       SAVE NAME IN CASE OF ERROR LATER             
         OC   EMPTYWNM,BLANKS      FORCE UPPERCASE AND TRAILING BLANKS          
         MVC  QUAL1,BLANKS                                                      
         LA   RE,QUAL1             BUMP FORWARD TO FIRST QUALIFIER              
         DO   UNTIL=(CLI,1(R1),EQ,C'_')                                         
           MVC  0(1,RE),1(R1)      BUILD FIRST QUALIFIER IN "QUAL1"             
           LA   R1,1(,R1)                                                       
           LA   RE,1(,RE)                                                       
         ENDDO ,                                                                
*                                                                               
* JAN/2020: NIELSEN IS NOW DELIVERING THE LOCAL DAILIES FILES VIA NDS,          
*   NOT FTP. PART OF THAT CHANGE ENTAILS A NEW *FILENAME* STRUCTURE.            
*   WE IDENTIFY THE FILENAME FLAVOR BY THE FIRST 3 CHARACTERS OF THE            
*   FILENAME. THE FILENAME BEGINS WITH THE LETTERS "NSI" IF AND ONLY            
*   IF IT'S THE NEW FORMAT.                                                     
*                                                                               
         CLC   =C'NSI',QUAL1       JAN/2020: NEW NDS-STYLE FILENAME?            
         BE    NDSNAMES            YES: FILENAME STRUCTURE IS DIFFERENT         
*                                                                               
*----------------------------------------------------------------------         
* THIS CODE APPLIES TO THE "OLD" FTP (PRE-NDS) FILENAME CONVENTION.             
* IT SHOULD ONLY BE NEEDED IF WE EVER NEED TO BACKLOAD OLD DATA. AT             
* SOME POINT, IT CAN BE DISCARDED.                                              
         LA    R1,IOAREA           WE DO NOT WANT TO PROCESS WIRED DATA         
         AR    R1,R2               R1 = A(JUST PAST END OF RECORD)              
         SHI   R1,7                BACK UP TO UNDERSCORE                        
         CLC   =C'_C',0(R1)        E.G.: _T7.TXT                                
         BNE   GET_DATE            IT'S NOT WIRED: DON'T SKIP IT                
         MVC   P(19),=C'IGNORING WIRED DATA'                                    
         GOTO1 =V(PRINTER)                                                      
         B     FILELOOP                                                         
*                                                                               
GET_DATE DS    0H                                                               
         SHI   R1,8                BACK UP TO START OF DATE                     
         MVC   FK_DATE_YYYY,0(R1)  FILENAME DATE IS IN FORMAT YYYYMMDD          
         MVC   FK_DATE_MM,4(R1)                                                 
         MVC   FK_DATE_DD,6(R1)                                                 
*                                                                               
         CLI   0(R1),C'/'          BACK UP TO BEFORE FILENAME START             
         BE    *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
*                                                                               
* IGNORE IMPACT DATA FOR NOW (UNTIL/UNLESS WE DECIDE TO PROCESS IT).            
* AS PER THE NIELSEN SPEC, IMPACT DATA IS DENOTED IN THE "MARKET                
* ABBREVIATION" FIELD IN THE FILENAME.                                          
*   1. DMA FILES HAVE "X" APPENDED (E.G., "NYX")                                
*   2. HISPANIC FILES HAVE "X" PREPENDED (E.G., "XLA")                          
*                                                                               
         IF CLI,3(R1),EQ,C'X',OR,CLI,1(R1),EQ,C'X'                              
           MVC   P(20),=C'IGNORING IMPACT DATA'                                 
           GOTO1 =V(PRINTER)                                                    
           B     FILELOOP          SKIP THIS FILE                               
         ENDIF                                                                  
*                                                                               
         CLI   3(R1),C'H'          HISPANIC MARKET?                             
         BNE   ALLOCATE            NO: DON'T SKIP IT                            
         CLC   =C'LA',1(R1)        LOS ANGELES?                                 
         BE    ALLOCATE                                                         
         MVC   P(22),=C'IGNORING HISPANIC DATA'                                 
         GOTO1 =V(PRINTER)                                                      
         B     FILELOOP                                                         
*----------------------------------------------------------------------         
*                                                                               
NDSNAMES DS   0H                                                                
         SELECT CLC,QUAL1,EQ       FIRST QUALIFIER INDICATES FILETYPE           
*                                                                               
* THESE ARE THE FILETYPES WE EXPECT TO SEE IN THE NFSFILES DATASET:             
*                                                                               
           WHEN (=CL20'NSIDMAB',   NSI TOTAL DMA BROADCAST                      
                 =CL20'NSIDMAC',   NSI TOTAL DMA CABLE                          
                 =CL20'NSIHDMAB',  NSIH (HISPANIC) TOTAL DMA BROADCAST          
                 =CL20'NSIHDMAC')  NSIH (HISPANIC) TOTAL DMA CABLE              
             LA   R1,IOAREA          A(START OF PATHNAME)                       
             DO   UNTIL=(CLI,0(R1),EQ,C'.')  BUMP TO START OF EXTENSION         
               LA   R1,1(,R1)                                                   
             ENDDO ,                                                            
             DO   UNTIL=(CLI,0(R1),EQ,C'_')  BACK UP TO LAST QUALIFIER          
               BCTR R1,0                                                        
             ENDDO ,                                                            
             IF (CLC,=C'X1',EQ,1(R1))  IGNORE IMPACT DATA (FOR NOW)             
               MVC   P(20),=C'IGNORING IMPACT DATA'                             
               GOTO1 =V(PRINTER)                                                
               B     FILELOOP                                                   
             ENDIF ,                                                            
             NEXTWHEN ,            FALL THROUGH TO NEXT "WHEN" MACRO            
*                                                                               
           WHEN (=CL20'NSIDMAB',   NSI TOTAL DMA BROADCAST                      
                 =CL20'NSIDMAC')   NSI TOTAL DMA CABLE                          
             NOPR  R0              (FALL THROUGH AND KEEP FILE)                 
*                                                                               
           WHEN (=CL20'NSIHDMAB',  NSIH (HISPANIC) TOTAL DMA BROADCAST          
                 =CL20'NSIHDMAC')  NSIH (HISPANIC) TOTAL DMA CABLE              
             LA   R1,IOAREA          A(START OF PATHNAME)                       
             DO   UNTIL=(CLI,0(R1),EQ,C'_')  BUMP JUST BEFORE MKT #             
               LA   R1,1(,R1)                                                   
             ENDDO ,                                                            
             IF (CLC,=C'403',NE,1(R1))  ONLY KEEP LOS ANGELES HISPANIC          
               MVC   P(22),=C'IGNORING HISPANIC DATA'                           
               GOTO1 =V(PRINTER)                                                
               B     FILELOOP                                                   
             ENDIF ,                                                            
*                                                                               
* THESE FILETYPES SHOULD BE GETTING FILTERED OUT BY THE DOWNLOAD                
* PROCESS, SO WE ACTUALLY DON'T EXPECT TO SEE THEM HERE, BUT JUST IN            
* CASE WE DO, WE SKIP THEM:                                                     
*                                                                               
           WHEN (=CL20'NSIHWCB',   NSI HARD-WIRED CABLE BROADCAST               
                 =CL20'NSIHWCC')   NSI HARD-WIRED CABLE CABLE                   
             MVC   P(19),=C'IGNORING WIRED DATA'                                
             GOTO1 =V(PRINTER)                                                  
             B     FILELOOP        IGNORE THIS FILE                             
*                                                                               
           WHEN (=CL20'NSITDMAB',  NSI *TEST* FILES                             
                 =CL20'NSITDMAC',                                               
                 =CL20'NSITHWCB',                                               
                 =CL20'NSITHWCC',                                               
                 =CL20'NSIHTDMAB',                                              
                 =CL20'NSIHTDMAC')                                              
             MVC   P(18),=C'IGNORING TEST DATA'                                 
             GOTO1 =V(PRINTER)                                                  
             B     FILELOOP        IGNORE THIS FILE                             
*                                                                               
           OTHRWISE ,                                                           
             MVC   P(24),=C'IGNORING UNEXPECTED DATA'                           
             GOTO1 =V(PRINTER)                                                  
             B     FILELOOP        IGNORE THIS FILE                             
*                                                                               
         ENDSEL ,                                                               
*                                                                               
         LA   R1,IOAREA            A(START OF PATHNAME)                         
         DO   FROM=(R0,3)          BUMP TO THE 3RD UNDERSCORE                   
           DO   UNTIL=(CLI,0(R1),EQ,C'_')                                       
             LA   R1,1(,R1)                                                     
           ENDDO ,                                                              
         ENDDO ,                                                                
         MVC   FK_DATE_YYYY,1(R1) FILENAME DATE IS IN FORMAT YYYY_MM_DD         
         MVC   FK_DATE_MM,6(R1)                                                 
         MVC   FK_DATE_DD,9(R1)                                                 
*                                                                               
ALLOCATE DS    0H                                                               
         MVI   SKIP_ALL,C'N'       NOT AN "IGNORE ALL FILES" SITUATION          
*                                                                               
         CLI   ALLOCFLG,C'Y'                                                    
         BNE   FILELOOP                                                         
         SHI   R2,4                L'RDW                                        
         ICM   R0,15,=X'50000000'  FILETEXT=BINARY,PATHOPTS=ORDONLY             
         CLI   TRACEFLG,C'Y'       TRACE=YES?                                   
         BNE   *+8                                                              
         O     R0,=X'04000000'     WTO *ALL* MESSAGES OUT OF DYNALLOC           
         GOTO1 =V(DYNALLOC),DMCB,(C'H',=C'FILIN1  '),((R2),IOAREA+4),  +        
               (R0)                                                             
         TM    DMCB+8,X'20'        DYNAMIC ALLOCATION FAILED?                   
         BZ    *+6                                                              
         DC    H'0'                YES                                          
         MVI   HAVEDATA,C'Y'       WE HAVE AT LEAST ONE FILE TO CONVERT         
         EJECT                                                                  
*                                                                               
OPENFILS DS    0H                                                               
         CLI   HAVEDATA,C'Y'       IS THERE ANY DATA TO CONVERT?                
         BNE   XBASE               NO                                           
*                                                                               
         OPEN  FILIN1                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   OUTCNT,=P'0'                                                     
         ZAP   BADCNT,=P'0'                                                     
*                                                                               
PRE10    DS    0H                                                               
         LA    RE,IOAREA                                                        
         LHI   RF,RECLENQ                                                       
         XCEF  ,                                                                
*                                                                               
         GET   FILIN1,IOAREA                                                    
         AP    INCNT,=P'1'                                                      
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,IOAREA         R2 = RECORD LENGTH (FROM RDW)                
         CLI   TRACEFLG,C'Y'       TRACE=YES?                                   
         BNE   PRE15               IF YES, PRINT INPUT RECORD                   
         MVC   WORK(15),=C'INPUT RECORD # '                                     
         UNPK  WORK+15(8),INCNT                                                 
         OI    WORK+22,X'F0'                                                    
         GOTO1 =V(PRNTBL),DMCB,(23,WORK),IOAREA,C'DUMP',(R2),=C'1D'             
*                                                                               
PRE15    DS    0H                                                               
         LA    R2,IOAREA+4                                                      
*                                                                               
         CLI   1(R2),C'1'          HEADER RECORD? (C'01', C'11', C'21')         
         BE    *+14                                                             
         CLI   XPECTHDR,C'N'       ARE WE EXPECTING A HEADER NOW?               
         BE    PRE20                                                            
         DC    H'0'                YES: WHERE IS IT ?!?                         
*                                                                               
         L     RE,HDRCOUNT         YES: BUMP COUNTER                            
         AHI   RE,1                                                             
         ST    RE,HDRCOUNT                                                      
         MVI   XPECTHDR,C'N'       HEADER NOT EXPECTED NEXT                     
*                                                                               
PRE20    DS    0H                                                               
         BAS   RE,PROCREC                                                       
         J     PRE10                                                            
         EJECT                                                                  
*                                                                               
EOFIN1   DS    0H                                                               
         CLOSE FILIN1                                                           
         FREEPOOL FILIN1                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   XPECTHDR,C'Y'       NEXT RECORD MUST BE A HEADER                 
*                                                                               
         SR    R0,R0                                                            
         CLI   TRACEFLG,C'Y'       TRACE=YES?                                   
         BNE   *+8                                                              
         ICM   R0,15,=C'INFO'      WTO *ALL* MESSAGES OUT OF DYNALLOC           
         GOTO1 =V(DYNALLOC),DMCB,(C'U',=C'FILIN1  '),(R0)                       
         OC    DMCB+4(4),DMCB+4    SUCCESSFUL DYNAMIC UNALLOCATION?             
         BZ    *+6                                                              
         DC    H'0'                NO                                           
*                                                                               
         ZAP   DUB,OUTCNT                                                       
         AP    DUB,BADCNT                                                       
         CP    INCNT,DUB           WHAT GOES IN MUST COME OUT...!               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CP    INCNT,=P'0'         INPUT FILE WAS EMPTY?                        
         BNE   EOFIN05             NO                                           
         MVC   P(42),=C'WARNING: EMPTY LOCAL DAILIES INPUT FILE!!!'             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',('EMPTYWRQ',EMPTYWRN)                 
*                                                                               
EOFIN05  DS    0H                                                               
         CLI   TRACEFLG,C'Y'       TRACE=YES?                                   
         BNE   EOFIN10                                                          
         MVC   P(65),=C'IN RECS: NN,NNN,NNN   OUT RECS: NN,NNN,NNN   BA+        
               D RECS: NN,NNN,NNN'                                              
         EDIT  INCNT,(10,P+9),ZERO=NOBLANK,COMMAS=YES                           
         EDIT  OUTCNT,(10,P+32),ZERO=NOBLANK,COMMAS=YES                         
         EDIT  BADCNT,(10,P+55),ZERO=NOBLANK,COMMAS=YES                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EOFIN10  DS    0H                                                               
         CLI   USINGNFS,C'Y'                                                    
         BE    FILELOOP                                                         
*                                                                               
DONE     DS    0H                                                               
         CLOSE FILOUT                                                           
*                                                                               
         MVC   P(46),=C'FILOUT IS NOW CLOSED. XXXXXX HEADER RECS READ.'         
         EDIT  HDRCOUNT,(6,P+22),ZERO=NOBLANK                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         IF (CLI,SKIP_ALL,EQ,C'Y'),AND,(CLI,EMPTYNFS,EQ,C'N')                   
           MVC   P(30),=C'ALL FILES LEGITIMATELY SKIPPED'                       
           GOTO1 =V(PRINTER)                                                    
           GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',('NOFILOKQ',NOFILEOK)               
         ENDIF ,                                                                
*                                                                               
         CLI   USINGNFS,C'Y'                                                    
         BNE   XBASE                                                            
         CLOSE NFSFILES                                                         
*                                                                               
XBASE    DS    0H                                                               
         IF (CLC,RETCODE,NE,=F'0')                                              
           MVC P(44),=C'*** ERROR(S) FOUND DURING PRE-PROCESSING ***'           
           GOTO1 =V(PRINTER)                                                    
         ENDIF ,                                                                
*                                                                               
         XBASE RC=RETCODE                                                       
*                                                                               
         EJECT                                                                  
PROCREC  NTR1                                                                   
*                                                                               
       BAS   RE,FORMAT        PARSE THE INPUT RECORD                            
       IF (NE)                IF PARSE FAILED:                                  
*                                                                               
         AP    BADCNT,=P'1'     INCREMENT BAD REC. COUNT & SKIP RECORD          
         MVC   RETCODE,=F'8'    SET BAD RETURN CODE                             
         IF (CLI,TRACEFLG,EQ,C'Y')                                              
           MVC   P(25),=C'SKIPPING BAD INPUT RECORD'                            
           GOTO1 =V(PRINTER)                                                    
         ENDIF ,                                                                
*                                                                               
* INSERT A SORT KEY AHEAD OF EACH OUTPUT RECORD TO GUARANTEE THAT               
* THE FILES ARE PROCESSED IN THE ORDER THAT THE CONVERSIONS (DEDALYI            
* AND DEDAPNI) EXPECT TO SEE THEM. VIZ: FOR EACH MARKET, ALL BROADCAST          
* FILES MUST BE IN CHRONOLOGICAL ORDER, FOLLOWED BY ALL CABLE FILES IN          
* CHRONOLOGICAL ORDER.                                                          
* NOTE THAT THE MARKETS THEMSELVES ARE BEING SORTED BY THEIR MARKET             
* ABBREVIATION, NOT BY MARKET NUMBER. WE DO IT THIS WAY BECAUSE THAT'S          
* HOW IT ALWAYS WORKED WITH THE "OLD" (FTP) FILENAME STRUCTURE, AND             
* IT'S EASY ENOUGH TO SORT IT THAT WAY WITH THE NEW NDS FILENAMES (AND          
* IT MEANS THAT EVERYTHING IS ABSOLUTELY TRANSPARENT TO THE                     
* CONVERSIONS).                                                                 
* (NOTE: THE LOCAL DAILIES SPEC SAYS THAT THE MARKET ABBREVIATION HAS A         
* MAXIMUM LENGTH OF 6, BUT KAREN HOOSE CONFIRMS THAT THIS IS INCORRECT.         
* THE MAXIMUM LENGTH IS ACTUALLY 3.)                                            
*                                                                               
       ELSE ,                 RECORD LOOKS GOOD                                 
*                                                                               
         LA    R3,TEMPREC+4+FILKEYLQ  ADD L'RDW + L'SORTKEY                     
         USING IDAILYD,R3                                                       
         SELECT CLC,0(2,R3),EQ        CHECK THE RECORD TYPE                     
           WHEN (=C'01')              IF MARKET HEADER RECORD (QH):             
             MVC  FK_ABBRV,IDMABBV     MARKET ABBREVIATION                      
             MVC  FK_DSTYP,IDMDSTY     B|C (BROADCAST|CABLE)                    
           WHEN (=C'11')              IF MARKET HEADER RECORD (PGNAM):          
             MVC  FK_ABBRV,IDPHABR     MARKET ABBREVIATION                      
             MVC  FK_DSTYP,IDPHDST     B|C (BROADCAST|CABLE)                    
           WHEN (=C'21')              IF MARKET HEADER RECORD (PGUPD):          
             MVC  FK_ABBRV,IDUHABR     MARKET ABBREVIATION                      
             MVC  FK_DSTYP,IDUHDST     B|C (BROADCAST|CABLE)                    
         ENDSEL ,                                                               
         MVC   TEMPREC+4(FILKEYLQ),FILEKEY  INSERT SORT KEY AFTER RDW           
         DROP  R3                                                               
*                                                                               
         LA    R1,TEMPREC+4+FILKEYLQ   ADD L'RDW + L'SORTKEY                    
         LA    RF,350              LRECL                                        
         BAS   RE,X401F                                                         
*                                                                               
         LA    R6,TEMPREC                                                       
         PUT   FILOUT,(R6)         OUTPUT THE RECORD                            
         AP    OUTCNT,=P'1'                                                     
*                                                                               
       ENDIF ,                                                                  
*                                                                               
EXIT   DS    0H                                                                 
       XIT1                                                                     
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*FORMAT  -  PROCESS RECORDS                                                     
*        R1: FIELD DISPLACEMENT/LENGTH TABLE                                    
*        R2: IOAREA                                                             
*        R3: TEMPREC                                                            
*        R6: UTILITY                                                            
*        RE: UTILITY (LENGTH)                                                   
*                                                                               
* ON EXIT:                                                                      
*  CC EQUAL:     RECORD IS OKAY                                                 
*  CC NOT EQUAL: RECORD IS BAD                                                  
***********************************************************************         
FORMAT   NTR1                                                                   
*                                                                               
         XC    TEMPREC(256),TEMPREC                                             
*                                                                               
         MVI   EORFLAG,C'N'        NOT END-OF-RECORD YET                        
*                                                                               
         LA    R1,DISPTAB          DISPLACEMENT TABLE                           
FMT01    CLC   0(2,R2),0(R1)       CORRECT REC TYPE?                            
         BE    FMT05                                                            
         LH    RE,2(R1)                                                         
         AR    R1,RE                                                            
         CLI   0(R1),X'FF'                                                      
         BE    FMTX                                                             
         B     FMT01                                                            
*                                                                               
FMT05    LA    R1,4(R1)                                                         
         LA    R3,TEMPREC+4+FILKEYLQ   ADD L'RDW + L'SORTKEY                    
*                                                                               
FMT10    DS    0H                                                               
         CLI   0(R1),0             ONE OF THOSE "FOR FUTURE USE" FIELD          
         BNE   FMT12                                                            
         MVI   0(R3),X'40'                                                      
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         B     FMT10                                                            
*                                                                               
FMT12    DS    0H                                                               
         CLI   0(R1),X'FF'         END OF EXPECTED FIELDS?                      
         BE    FMTX                                                             
*                                                                               
         LLC   RE,0(R1)            GET FIELD LENGTH                             
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BLANKS      FILL WITH SPACE FIRST                        
*                                                                               
         LR    RE,R2               GET LENGTH OF ACTUAL DATA                    
         CLI   EORFLAG,C'Y'        BUT IF THERE IS NO MORE...                   
         BE    FMT25               ...JUST LEAVE IT AS BLANKS                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,IOAREA         RF = RECORD LENGTH (FROM RDW)                
         LA    RF,IOAREA(RF)       RF = A(FIRST BYTE AFTER RECORD END)          
FMT15    CLI   0(RE),X'05'                                                      
         BE    FMT20                                                            
         CLI   0(RE),X'0D'                                                      
         BE    *+10                                                             
         CR    RE,RF               END OF REC REACHED (BASED ON LRECL)?         
         BNE   *+12                                                             
         MVI   EORFLAG,C'Y'        END OF RECORD SEEN                           
         B     FMT20                                                            
         LA    RE,1(RE)                                                         
         B     FMT15                                                            
*                                                                               
FMT20    SR    RE,R2               GET LENGTH                                   
         CHI   RE,0                IF LENGTH IS 0                               
         BE    FMT25               LEAVE IT AS SPACE                            
*                                                                               
         LR    R6,R3               R6 = TEMPREC POINTER                         
         SHI   RE,1                                                             
         BM    NO                  BAD RECORD                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R2)                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         NC    WORK(0),ZEROES                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),ZEROES                                                   
         BL    FMT21                                                            
*                                                                               
         LLC   R6,0(R1)            RIGHT JUSTIFY NUMERIC DATA                   
         SR    R6,RE                                                            
         SHI   R6,1                RE IS 1 SHORT                                
         BM    NO                  BAD RECORD                                   
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ZEROES      ZERO FILL NUMERIC DATA                       
         AR    R6,R3                                                            
*                                                                               
FMT21    EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R2)                                                    
         AHI   RE,1                                                             
*                                                                               
FMT25    LA    R2,1(RE,R2)                                                      
         LLC   RE,0(R1)                                                         
         LA    R3,0(RE,R3)                                                      
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   FMT10                                                            
*                                                                               
FMTX     LA    R6,TEMPREC                                                       
         SR    R3,R6                                                            
         STH   R3,TEMPREC                                                       
*                                                                               
         B     YES                 GOOD EXIT                                    
*                                                                               
         ANSR  ,                                                                
         EJECT                                                                  
*                                                                               
******************************************************************              
* X40EF - TURN ON X'40' TO 0(R1) FOR THE LENGTH OF (RF)                         
******************************************************************              
X401F    NTR1                                                                   
*                                                                               
X401F10  DS    0H                                                               
         CHI   RF,256                                                           
         BL    X401F20                                                          
         OC    0(256,R1),BLANKS                                                 
         LA    R1,256(R1)                                                       
         SHI   RF,256                                                           
         B     X401F10                                                          
*                                                                               
X401F20  DS    0H                                                               
         LTR   RF,RF                                                            
         BZ    X401FX                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,R1),BLANKS                                                   
*                                                                               
X401FX   DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  TABLE ALLOCATION                                                             
*                                                                               
* DEIS JAN/2020:                                                                
*  THE TABLES BELOW ARE USED TO PARSE EACH TAB-DELIMITED INPUT                  
*  RECORD, WHILE SIMULTANEOUSLY REFORMATTING EACH RECORD INTO A                 
*  FIXED-WIDTH FORMAT FILE. THAT FILE IS THEN USED AS INPUT INTO                
*  A DFSORT STEP WHICH FOLLOWS THIS PROGRAM (SEE PAN MEMBER                     
*  DEDAILYSRT).                                                                 
*                                                                               
*  DEIS WOULD LIKE TO MAKE A FEW CLARIFYING POINTS:                             
*   1. IF THIS WERE TO BE REWRITTEN, WE SHOULD CONSIDER REMOVING                
*      THE PARSE/BUILD LOGIC FROM THIS PROGRAM AND INCORPORATE IT               
*      INTO AN ICETOOL/DFSORT STEP (WHICH IS WHAT WE DO WITH THE                
*      LOCAL MONTHLIES).                                                        
*   2. THERE ARE FIELDS BELOW MARKED AS "FOR FUTURE USE". THESE                 
*      FIELDS CERTAINLY DO NEED TO BE ACCOUNTED FOR DURING PARSING,             
*      BUT DEIS CANNOT UNDERSTAND WHY THEY NEED TO BE *SAVED*. THIS             
*      PROGRAM INSERTS A SINGLE BLANK IN THE OUTPUT RECORD WHEREVER             
*      THERE IS A "FOR FUTURE USE" FIELD. THOSE COLUMNS ARE ACCOUNTED           
*      FOR IN DEDAILYSRT.                                                       
*   3. IN JAN/2020, DEIS CHANGED ALL OF THE HARD-CODED FIELD LENGTHS            
*      IN THE TABLES BELOW TO USE THE LENGTH ATTRIBUTES OF THE                  
*      DEDAILYD SYMBOLS. AT LEAST THIS MAKES IT A LITTLE MORE OBVIOUS           
*      WHAT EACH FIELD CONTAINS.                                                
*   4. THERE IS NO COMMUNICATION REGARDING THE RECORD *FORMAT* BETWEEN          
*      THIS PROGRAM AND DEDAILYSRT. ANY CHANGES TO THE LOCAL DAILIES            
*      INPUT FILE FORMAT WILL PRESUMABLY REQUIRE CHANGES TO THE TABLES          
*      BELOW, TO DEDAILYD, AND DEDAILYSRT.                                      
******************************************************************              
DISPTAB  DS    0C                                                               
*                                                                               
MHR      DC    C'01',AL2(MHRX-MHR)                                              
         DC    AL1(L'IDMRCDE)                                                   
         DC    AL1(L'IDMVER)                                                    
         DC    AL1(L'IDMMKT)                                                    
         DC    AL1(L'IDMDMA)                                                    
         DC    AL1(L'IDMRANK)                                                   
         DC    AL1(L'IDMGEOI)                                                   
         DC    AL1(L'IDMGEON)                                                   
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDMABBV)                                                   
         DC    AL1(L'IDMMTRD)                                                   
         DC    AL1(L'IDMDOW)                                                    
         DC    AL1(L'IDMRDST)                                                   
         DC    AL1(L'IDMCDST)                                                   
         DC    AL1(L'IDMRSRVC)                                                  
         DC    AL1(L'IDMSAMTY)                                                  
         DC    AL1(L'IDMREPRO)                                                  
         DC    AL1(L'IDMDACC)                                                   
         DC    AL1(L'IDMPLYTY)                                                  
         DC    AL1(L'IDMTINT)                                                   
         DC    AL1(L'IDMCMET)                                                   
         DC    AL1(L'IDMDSTY)                                                   
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              ???                                          
         DC    X'FF'                                                            
MHRX     EQU   *                                                                
*                                                                               
DHR      DC    C'02',AL2(DHRX-DHR)                                              
         DC    AL1(L'IDRRCDE)                                                   
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDRORIG)                                                   
         DC    AL1(L'IDRMKT)                                                    
         DC    AL1(L'IDRSTCDE)                                                  
         DC    AL1(L'IDRCALL)                                                   
         DC    AL1(L'IDRCHAN)                                                   
         DC    AL1(L'IDRAFFIL)                                                  
         DC    AL1(7)              ???                                          
         DC    AL1(7)              ???                                          
         DC    AL1(L'IDRDSGRP)                                                  
         DC    AL1(L'IDRPRENT)                                                  
         DC    AL1(L'IDRSATAL)                                                  
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    X'FF'                                                            
DHRX     EQU   *                                                                
*                                                                               
DEMCAT   DC    C'03',AL2(DEMCATX-DEMCAT)                                        
         DC    AL1(L'IDDRCDE)                                                   
         DC    AL1(L'IDDHHLD)                                                   
         DC    AL1(L'IDDDEM01)                                                  
         DC    AL1(L'IDDDEM02)                                                  
         DC    AL1(L'IDDDEM03)                                                  
         DC    AL1(L'IDDDEM04)                                                  
         DC    AL1(L'IDDDEM05)                                                  
         DC    AL1(L'IDDDEM06)                                                  
         DC    AL1(L'IDDDEM07)                                                  
         DC    AL1(L'IDDDEM08)                                                  
         DC    AL1(L'IDDDEM09)                                                  
         DC    AL1(L'IDDDEM10)                                                  
         DC    AL1(L'IDDDEM11)                                                  
         DC    AL1(L'IDDDEM12)                                                  
         DC    AL1(L'IDDDEM13)                                                  
         DC    AL1(L'IDDDEM14)                                                  
         DC    AL1(L'IDDDEM15)                                                  
         DC    AL1(L'IDDDEM16)                                                  
         DC    AL1(L'IDDDEM17)                                                  
         DC    AL1(L'IDDDEM18)                                                  
         DC    AL1(L'IDDDEM19)                                                  
         DC    AL1(L'IDDDEM20)                                                  
         DC    AL1(L'IDDDEM21)                                                  
         DC    X'FF'                                                            
DEMCATX  EQU   *                                                                
*                                                                               
UNI      DC    C'04',AL2(UNIX-UNI)                                              
         DC    AL1(L'IDURCDE)                                                   
         DC    AL1(L'IDUDTIM)                                                   
         DC    AL1(L'IDUHHLD)                                                   
         DC    AL1(L'IDUDEM01)                                                  
         DC    AL1(L'IDUDEM02)                                                  
         DC    AL1(L'IDUDEM03)                                                  
         DC    AL1(L'IDUDEM04)                                                  
         DC    AL1(L'IDUDEM05)                                                  
         DC    AL1(L'IDUDEM06)                                                  
         DC    AL1(L'IDUDEM07)                                                  
         DC    AL1(L'IDUDEM08)                                                  
         DC    AL1(L'IDUDEM09)                                                  
         DC    AL1(L'IDUDEM10)                                                  
         DC    AL1(L'IDUDEM11)                                                  
         DC    AL1(L'IDUDEM12)                                                  
         DC    AL1(L'IDUDEM13)                                                  
         DC    AL1(L'IDUDEM14)                                                  
         DC    AL1(L'IDUDEM15)                                                  
         DC    AL1(L'IDUDEM16)                                                  
         DC    AL1(L'IDUDEM17)                                                  
         DC    AL1(L'IDUDEM18)                                                  
         DC    AL1(L'IDUDEM19)                                                  
         DC    AL1(L'IDUDEM20)                                                  
         DC    AL1(L'IDUDEM21)                                                  
         DC    X'FF'                                                            
UNIX     EQU   *                                                                
*                                                                               
INTAB    DC    C'05',AL2(INTABX-INTAB)                                          
         DC    AL1(L'IDIRCDE)                                                   
         DC    AL1(L'IDIDTIM)                                                   
         DC    AL1(L'IDIHHLD)                                                   
         DC    AL1(L'IDIDEM01)                                                  
         DC    AL1(L'IDIDEM02)                                                  
         DC    AL1(L'IDIDEM03)                                                  
         DC    AL1(L'IDIDEM04)                                                  
         DC    AL1(L'IDIDEM05)                                                  
         DC    AL1(L'IDIDEM06)                                                  
         DC    AL1(L'IDIDEM07)                                                  
         DC    AL1(L'IDIDEM08)                                                  
         DC    AL1(L'IDIDEM09)                                                  
         DC    AL1(L'IDIDEM10)                                                  
         DC    AL1(L'IDIDEM11)                                                  
         DC    AL1(L'IDIDEM12)                                                  
         DC    AL1(L'IDIDEM13)                                                  
         DC    AL1(L'IDIDEM14)                                                  
         DC    AL1(L'IDIDEM15)                                                  
         DC    AL1(L'IDIDEM16)                                                  
         DC    AL1(L'IDIDEM17)                                                  
         DC    AL1(L'IDIDEM18)                                                  
         DC    AL1(L'IDIDEM19)                                                  
         DC    AL1(L'IDIDEM20)                                                  
         DC    AL1(L'IDIDEM21)                                                  
         DC    X'FF'                                                            
INTABX   EQU   *                                                                
*                                                                               
EXC      DC    C'06',AL2(EXCX-EXC)                                              
         DC    AL1(L'IDXRCDE)                                                   
         DC    AL1(L'IDXTYPE)                                                   
         DC    AL1(L'IDXSDT)                                                    
         DC    AL1(L'IDXEDT)                                                    
         DC    AL1(L'IDXDESC)                                                   
         DC    AL1(L'IDXSTAC)                                                   
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    X'FF'                                                            
EXCX     EQU   *                                                                
*                                                                               
HUTPUT   DC    C'07',AL2(HUTPUTX-HUTPUT)                                        
         DC    AL1(L'IDHRCDE)                                                   
         DC    AL1(L'IDHSTAC)                                                   
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDHSTDT)                                                   
         DC    AL1(L'IDHEXC)                                                    
         DC    AL1(L'IDHHHLD)                                                   
         DC    AL1(L'IDHDEM01)                                                  
         DC    AL1(L'IDHDEM02)                                                  
         DC    AL1(L'IDHDEM03)                                                  
         DC    AL1(L'IDHDEM04)                                                  
         DC    AL1(L'IDHDEM05)                                                  
         DC    AL1(L'IDHDEM06)                                                  
         DC    AL1(L'IDHDEM07)                                                  
         DC    AL1(L'IDHDEM08)                                                  
         DC    AL1(L'IDHDEM09)                                                  
         DC    AL1(L'IDHDEM10)                                                  
         DC    AL1(L'IDHDEM11)                                                  
         DC    AL1(L'IDHDEM12)                                                  
         DC    AL1(L'IDHDEM13)                                                  
         DC    AL1(L'IDHDEM14)                                                  
         DC    AL1(L'IDHDEM15)                                                  
         DC    AL1(L'IDHDEM16)                                                  
         DC    AL1(L'IDHDEM17)                                                  
         DC    AL1(L'IDHDEM18)                                                  
         DC    AL1(L'IDHDEM19)                                                  
         DC    AL1(L'IDHDEM20)                                                  
         DC    AL1(L'IDHDEM21)                                                  
         DC    X'FF'                                                            
HUTPUTX  EQU   *                                                                
*                                                                               
IMP      DC    C'08',AL2(IMPX-IMP)                                              
         DC    AL1(L'IDQRCDE)                                                   
         DC    AL1(L'IDQSTAC)                                                   
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDQSTDT)                                                   
         DC    AL1(L'IDQEXC)                                                    
         DC    AL1(L'IDQHHLD)                                                   
         DC    AL1(L'IDQDEM01)                                                  
         DC    AL1(L'IDQDEM02)                                                  
         DC    AL1(L'IDQDEM03)                                                  
         DC    AL1(L'IDQDEM04)                                                  
         DC    AL1(L'IDQDEM05)                                                  
         DC    AL1(L'IDQDEM06)                                                  
         DC    AL1(L'IDQDEM07)                                                  
         DC    AL1(L'IDQDEM08)                                                  
         DC    AL1(L'IDQDEM09)                                                  
         DC    AL1(L'IDQDEM10)                                                  
         DC    AL1(L'IDQDEM11)                                                  
         DC    AL1(L'IDQDEM12)                                                  
         DC    AL1(L'IDQDEM13)                                                  
         DC    AL1(L'IDQDEM14)                                                  
         DC    AL1(L'IDQDEM15)                                                  
         DC    AL1(L'IDQDEM16)                                                  
         DC    AL1(L'IDQDEM17)                                                  
         DC    AL1(L'IDQDEM18)                                                  
         DC    AL1(L'IDQDEM19)                                                  
         DC    AL1(L'IDQDEM20)                                                  
         DC    AL1(L'IDQDEM21)                                                  
         DC    X'FF'                                                            
IMPX     EQU   *                                                                
*                                                                               
PMHDR    DC    C'11',AL2(PMHDRX-PMHDR)                                          
         DC    AL1(L'IDPHCDE)                                                   
         DC    AL1(L'IDPHMKC)                                                   
         DC    AL1(L'IDPHDMA)                                                   
         DC    AL1(L'IDPHGEO)                                                   
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDPHABR)                                                   
         DC    AL1(L'IDPHDAT)                                                   
         DC    AL1(L'IDPHRTG)                                                   
         DC    AL1(L'IDPHPFI)                                                   
         DC    AL1(L'IDPHDST)                                                   
         DC    X'FF'                                                            
PMHDRX   EQU   *                                                                
*                                                                               
PGM      DC    C'12',AL2(PGMX-PGM)                                              
         DC    AL1(L'IDPNCDE)                                                   
         DC    AL1(L'IDPNSTC)                                                   
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDPNSTDT)                                                  
         DC    AL1(L'IDPNENDT)                                                  
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDPNPGN)                                                   
         DC    AL1(L'IDPNSUB)                                                   
         DC    AL1(L'IDPNACND)                                                  
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDPNSRC)                                                   
         DC    AL1(L'IDPNORIG)                                                  
         DC    AL1(L'IDPNTID)                                                   
         DC    AL1(L'IDPNTNAM)                                                  
         DC    AL1(L'IDPNTEID)                                                  
         DC    AL1(L'IDPNNVID)                                                  
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDPNPRIM)                                                  
         DC    AL1(L'IDPNRPET)                                                  
         DC    AL1(L'IDPNSPC)                                                   
         DC    X'FF'                                                            
PGMX     EQU   *                                                                
*                                                                               
PUHDR    DC    C'21',AL2(PUHDRX-PUHDR)                                          
         DC    AL1(L'IDUHCDE)                                                   
         DC    AL1(L'IDUHMKC)                                                   
         DC    AL1(L'IDUHDMA)                                                   
         DC    AL1(L'IDUHGEO)                                                   
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDUHABR)                                                   
         DC    AL1(L'IDUHRTG)                                                   
         DC    AL1(L'IDUHDST)                                                   
         DC    AL1(L'IDUHDAT)                                                   
         DC    X'FF'                                                            
PUHDRX   EQU   *                                                                
*                                                                               
PU       DC    C'22',AL2(PUX-PU)                                                
         DC    AL1(L'IDPUCDE)                                                   
         DC    AL1(L'IDPUSTC)                                                   
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDPUMDT)                                                   
         DC    AL1(L'IDPUSTDT)                                                  
         DC    AL1(L'IDPUENDT)                                                  
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDPUPGN)                                                   
         DC    AL1(L'IDPUSUB)                                                   
         DC    AL1(L'IDPUACND)                                                  
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDPUSRC)                                                   
         DC    AL1(L'IDPUORIG)                                                  
         DC    AL1(L'IDPUTID)                                                   
         DC    AL1(L'IDPUTNAM)                                                  
         DC    AL1(L'IDPUTEID)                                                  
         DC    AL1(L'IDPUNVID)                                                  
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(0)              FOR FUTURE USE                               
         DC    AL1(L'IDPUPRIM)                                                  
         DC    AL1(L'IDPURPET)                                                  
         DC    AL1(L'IDPUSPC)                                                   
         DC    X'FF'                                                            
PUX      EQU   *                                                                
*                                                                               
FOOTER   DC    C'99',AL2(FOOX-FOOTER)                                           
         DC    AL1(L'IDFRCDE)                                                   
         DC    AL1(L'IDFFOOT)                                                   
         DC    X'FF'                                                            
FOOX     EQU   *                                                                
*                                                                               
         DC    X'FF'               ONLY MATTERS FOR INVALID RECORD TYPE         
         EJECT                                                                  
*                                                                               
* I/O ERROR HANDLER                                                             
*                                                                               
IOERR    DS    0H                                                               
*                                                                               
* EXTRACT SYSTEM ERROR MESSAGES INTO FIELD WORK. SEE IBM MANUAL                 
* "Z/OS DFSMS MACRO INSTRUCTIONS FOR DATA SETS", SECTION ON SYNADAF             
* MACRO, FOR DETAILS. IF WE HAVE A DDNAME, TRY TO EXTRACT A DSN OR              
* PATHNAME AND DISPLAY IT ALONG WITH THE SYNAD MESSAGES.                        
*                                                                               
         SYNADAF ACSMETH=QSAM                                                   
         MVC   WORK,50(R1)         MESSAGE AREA FROM SYNADAF                    
         SYNADRLS                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'I/O ERROR: FORCING USER AB+        
               END.'                                                            
*                                                                               
         CLC   WORK+25(8),BLANKS   DO WE HAVE A DDNAME?                         
         BE    IOERR20             NO                                           
*                                  YES: TRY TO EXTRACT DSN                      
         GOTO1 =V(DDINFO),DMCB,(8,WORK+25),(0,=AL2(DINRTDSN)),0                 
         LTR   RF,RF                                                            
         BNZ   IOERR20             BAD RETURN FROM DDINFO                       
         SR    R2,R2                                                            
         ICM   R2,1,DMCB+8                                                      
         BZ    IOERR20             NO DSN AVAILABLE                             
         L     RE,DMCB+8           A(RETURNED DSN OR PATHNAME)                  
         CLC   =C'...PATH=.SPECIFIED...',0(RE)                                  
         BNE   IOERR10             IT'S NOT A PATHNAME                          
*                                                                               
*                                  TRY TO EXTRACT PATHNAME                      
         GOTO1 =V(DDINFO),DMCB,(8,WORK+25),(0,=AL2(DINRPATH)),0                 
         LTR   RF,RF                                                            
         BNZ   IOERR20             BAD RETURN FROM DDINFO                       
         SR    R2,R2                                                            
         ICM   R2,1,DMCB+8                                                      
         BZ    IOERR20             NO PATHNAME RETURNED                         
         L     RE,DMCB+8           A(RETURNED DSN OR PATHNAME)                  
*                                                                               
IOERR10  DS    0H                                                               
         MVC   OPERMSG(21),=C'FAILURE READING FROM '                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   OPERMSG+21(0),0(RE) PUT PATHNAME INTO CONSOLE MESSAGE            
         AHI   R2,1+21             R2 = L'MESSAGE                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',((R2),OPERMSG)                        
*                                                                               
IOERR20  DS    0H                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'SYNAD ERROR MESSAGES FOLLO+        
               W:'                                                              
         MVC   OPERMSG,BLANKS      BUILD FIRST MESSAGE LINE...                  
         MVC   OPERMSG(59),WORK+18 STARTING AFTER <JOBNAME,STEPNAME,>           
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(59,OPERMSG)                          
         CLI   WORK+77,C'S'        IS THERE A 2ND MESSAGE?                      
         BNE   IOERRXIT                                                         
         MVC   OPERMSG,BLANKS      YES: BUILD IT                                
         MVC   OPERMSG,WORK+94                                                  
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPERMSG,OPERMSG)                   
*                                                                               
IOERRXIT DS    0H                                                               
         ABEND 925                                                              
         EJECT                                                                  
FILIN1   DCB   DDNAME=FILIN1,DSORG=PS,RECFM=VB,MACRF=GM,               X        
               EODAD=EOFIN1,LRECL=350,BLKSIZE=0,SYNAD=IOERR                     
*                                                                               
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=VB,MACRF=PM,LRECL=360               
*                                                                               
* DCB FOR DATASET WITH FILENAMES IN NFS-MOUNTED FOLDER                          
NFSFILES DCB   DDNAME=NFSFILES,DSORG=PS,MACRF=GM,LRECL=256,RECFM=VB,   X        
               EODAD=DONE                                                       
         EJECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
RETCODE  DC    F'0'                PROGRAM RETURN CODE                          
HDRCOUNT DC    F'0'                                                             
INCNT    DC    PL8'0'                                                           
OUTCNT   DC    PL8'0'                                                           
BADCNT   DC    PL8'0'                                                           
DMCB     DS    6F                                                               
QUAL1    DS    CL20                FIRST FILENAME QUALIFIER FIELD               
*                                                                               
FILEKEY  DS    0C                  SORT KEY PREFIX (FOR DEDAILYSRT)             
FK_ABBRV DS    CL3                  MARKET ABBREVIATION                         
FK_DSTYP DS    C                    'B' = BROADCAST, 'C' = CABLE                
FK_DATE  DS    0CL8                 DATA|FILE DATE                              
FK_DATE_YYYY DS CL4                                                             
FK_DATE_MM   DS CL2                                                             
FK_DATE_DD   DS CL2                                                             
FILKEYLQ EQU   *-FILEKEY                                                        
*                                                                               
OPERMSG  DS    CL100               MAXIMUM LENGTH FOR LOGIO                     
WORK     DS    CL256                                                            
HAVEDATA DS    C                                                                
EMPTYNFS DC    C'Y'                'Y' = NFSFILES DATASET IS EMPTY              
SKIP_ALL DC    C'Y'                'Y' = ALL FILES DELIBERATELY SKIPPED         
USINGNFS DC    C'Y'                ASSUME DATA WILL BE READ VIA NFS             
TRACEFLG DC    C'N'                ASSUME TRACE=NO                              
XPECTHDR DC    C'Y'                'Y' IF NEXT RECORD MUST BE A HEADER          
ALLOCFLG DC    C'Y'                'N': DON'T CALL DYNALLOC                     
*                                    (FOR DEBUGGING ONLY!!)                     
EORFLAG  DS    C                   'Y' WHEN END OF INPUT RECORD IS SEEN         
BLANKS   DC    256C' '                                                          
ZEROES   DC    20C'0'                                                           
CARD     DS    CL80                                                             
*                                                                               
EMPTYWRN DC    C'AUTONOTE*USOPSUP:WARNING! EMPTY LOCAL DAILIES INPUT FI+        
               LE '                                                             
EMPTYWNM DS    CL50                PATHNAME                                     
EMPTYWRQ EQU   *-EMPTYWRN          LENGTH OF AUTONOTE WARNING                   
*                                                                               
NOFILEOK DC    C'AUTONOTE*USOPSUP,US-DEMOSTEAM:OK: IGNORING ALL LOCAL D+        
               AILIES INPUT FILES'                                              
NOFILOKQ EQU   *-NOFILEOK          LENGTH OF AUTONOTE WARNING                   
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*IOAREA*'                                                      
IOAREA   DS    CL(RECLENQ)                                                      
*                                                                               
* TEMPREC IS THE OUTPUT RECORD FORMAT. SEE DEDAILYSRT.                          
* TEMPREC+0(4)     = RDW                                                        
* TEMPREC+4(4)     = SORT KEY (SEE FIELD "FILEKEY")                             
* TEMPREC+FILKEYLQ = RECORD DATA                                                
         DS    0D                                                               
         DC    C'*TEMPREC'                                                      
TEMPREC  DS    CL(RECLENQ)                                                      
RECLENQ  EQU   2000                                                             
         EJECT                                                                  
         DS    0D                                                               
         DC    CL16'*****UTL********'                                           
UTL      DC    4X'00',X'0C'        UTL FOR DEMO                                 
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    CL16'*****SSB********'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
         EJECT                                                                  
*******************************************************************             
*        DSECTS                                                   *             
*******************************************************************             
*                                                                               
       ++INCLUDE DEDAILYD                                                       
         EJECT                                                                  
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
         IEFZB4D2                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'141DEPREDALY 06/22/20'                                      
         END                                                                    
