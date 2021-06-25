*          DATA SET DDMAPPER   AT LEVEL 023 AS OF 09/04/96                      
*PHASE MAPPER,*                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PANIC                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'PROCESS A GENFILE INTO SERIES OF SQL COMPATIBLE TABLES'         
         PRINT NOGEN                                                            
         SPACE 1                                                                
MAPPER   START                                                                  
         PRINT NOGEN                                                            
         NBASE WRKX-WRKD,*MAPPER*,RA,WORK=A(WRKCHN),CLEAR=YES,RR=R3             
         USING WRKD,RC                                                          
*                                                                               
         BAS   RE,INIT             INITIALISE VALUES                            
         L     R2,VCPRINT                                                       
         USING DPRINT,R2                                                        
*                                                                               
         BAS   RE,GETCARDS         RESOLVE INPUT CARDS                          
         BL    XBASE               REQUIRED CARDS MISSING - SEE OUTPUT          
*                                                                               
         LR    R1,RC                                                            
         GOTOX =A(RECEQUS)         BUILD RECORD EQUATE FILE                     
         GOTOX =A(FRSTPASS)        BUILD INITIAL GENFILE TABLE                  
         GOTOX =A(SCNDPASS)        RESOLVE GENFILE TABLE & SORT IT              
*                                                                               
         GOTOX AFREMAIN,PLIST,AGENTAB,LGENTAB                                   
*                                                                               
         BAS   RE,INIT2            INITIALISE SECOND SERIES OF TABLES           
         BAS   RE,SORTDUMP         DUMP SORTED FILE TO TABLES                   
*                                                                               
         LR    R1,RC                                                            
         GOTOX =A(PROCFILE)        PROCESS RECORDS FOR WHOLE FILE               
         GOTOX =A(DUMPFILE)        DUMP RESULTS TO DATASETS                     
*                                                                               
         GOTOX AFREMAIN,PLIST,ADSCTAB,LDSCTAB                                   
         GOTOX AFREMAIN,PLIST,ADSTAB,LDSTAB                                     
         GOTOX AFREMAIN,PLIST,AEQUTAB,LEQUTAB                                   
         GOTOX AFREMAIN,PLIST,AIDNTAB,LIDNTAB                                   
         GOTOX AFREMAIN,PLIST,AFILTAB,LFILTAB                                   
*                                                                               
XBASE    XBASE                                                                  
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* FIRST SERIES INITIALISATION                                        *          
**********************************************************************          
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         MVI   SPACE,C' '          FILL OUR SPACE LINE WITH '    '              
         MVC   SPACE+1(L'SPACE-1),SPACE                                         
*                                                                               
         MVC   ADRLST(ADRLSTLQ),VTYPES  SET COMMON VTYPES                       
         MVC   READ(PANCMDLQ),PANCMD                                            
         MVC   MAPFILE(OTHERLQ),OTHERS                                          
         MVC   SRTCMDS(SRTCMDLQ),SORTCMND                                       
*                                                                               
         L     RF,=A(GETMAIN)      SET COMMON ROUTINES                          
         ST    RF,AGETMAIN                                                      
         L     RF,=A(FREEMAIN)                                                  
         ST    RF,AFREMAIN                                                      
*                                                                               
         LA    RF,SKIPTAB          PRINT CONTROL COMMANDS                       
         ST    RF,ASKIPTAB                                                      
*                                                                               
         GOTOX VLOADER,PLIST,=CL8'T00A7D'                                       
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR            LOAD TSAROFF                                 
*                                                                               
         L     R1,=A(TBLRPT)       NUMBER OF 4K PAGES REQUESTED                 
         SLL   R1,12               ROUND UP TO NEAREST 4K PAGE                  
         GOTOX AGETMAIN                                                         
         ST    R0,LGENTAB          R0 = LENGTH OF GENFILE TABLE AREA            
         ST    R1,AGENTAB          R1 = A(GENFILE TABLE AREA)                   
         ST    R1,AGENCUR                                                       
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* SECOND SERIES INITIALISATION                                       *          
**********************************************************************          
         SPACE 1                                                                
INIT2    NTR1  ,                                                                
         L     R1,RCOUNT           NUMBER OF RECORDS IN TABLE                   
         MH    R1,=Y(HEDLENQ)      LENGTH OF A HEDTAB ENTRY                     
         GOTOX AGETMAIN                                                         
         ST    R0,LDSCTAB          R0=LENGTH OF DSECT TABLE AREA                
         ST    R1,ADSCTAB          R1=START OF DSECT TABLE AREA                 
         ST    R1,ADSCCUR                                                       
*                                                                               
         L     R1,DCOUNT           NUMBER OF DATA ENTRIES IN TABLE              
         MH    R1,=Y(DTALENQ)      LENGTH OF A DTATAB ENTRY                     
         GOTOX AGETMAIN                                                         
         ST    R0,LDSTAB           R0=LENGTH OF DS TABLE AREA                   
         ST    R1,ADSTAB           R1=START OF DS TABLE AREA                    
         ST    R1,ADSCUR                                                        
*                                                                               
         L     R1,ECOUNT           NUMBER OF EQUATES IN TABLE                   
         MH    R1,=Y(EQULENQ)      LENGTH OF A EQUTAB ENTRY                     
         GOTOX AGETMAIN                                                         
         ST    R0,LEQUTAB          R0=LENGTH OF EQUATE TABLE AREA               
         ST    R1,AEQUTAB          R1=START OF EQUATE TABLE AREA                
         ST    R1,AEQUCUR                                                       
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* READ IN ALL INPUT CARDS FOR THIS PARSE                             *          
**********************************************************************          
         SPACE 1                                                                
GETCARDS NTR1  ,                                                                
         L     R2,VCPRINT                                                       
         USING DPRINT,R2                                                        
         MVC   P(20),=CL20'SYSIN JCL CARDS:'                                    
         GOTOX VPRINTER                                                         
*                                                                               
GCARD02  GOTOX VCARDS,PLIST,P,=C'RE00'                                          
         CLC   =CL2'/*',P          END OF JCL - CHECK REQUIRED CARDS            
         BE    GCARD10                                                          
         CLI   P,COMMENT           COMMENT CARD - IGNORE IT                     
         BE    GCARD02                                                          
*                                                                               
         MVC   P+INPNMBR(L'INPLINE-INPNMBR),SPACE REMOVE LEVEL STAMP            
*                                                                               
         LA    RF,CARDTBL          RECOGNISED INPUT CARD                        
         USING CARDTBLD,RF                                                      
         XR    R1,R1                                                            
*                                                                               
GCARD04  CLI   0(RF),FF            END OF TABLE?                                
         BE    GCARD08                                                          
         IC    R1,CARDMCH                                                       
         EX    R1,GCRDMCH          TRY TO MATCH INPUT CARD                      
         BE    GCARD06                                                          
         LA    RF,CARDTBLL(RF)                                                  
         B     GCARD04                                                          
*                                                                               
GCRDMCH  CLC   CARDINP(0),P                                                     
*                                                                               
GCARD06  ICM   RF,15,CARDPRC       PROCESS THIS INPUT CARD                      
         BASR  RE,RF                                                            
         GOTOX VPRINTER                                                         
         B     GCARD02                                                          
*                                                                               
GCARD08  GOTOX VPRINTER            INVALID CARD                                 
         MVC   P(L'EINVCARD),EINVCARD                                           
         GOTOX VPRINTER                                                         
         B     GCARD02                                                          
*                                                                               
GCARD10  MVI   P,C' '              RESET PRINT LINE                             
         MVC   P+1(L'P-1),P                                                     
         GOTOX VPRINTER                                                         
         CLC   GENFILE,SPACE       MUST PROVIDE GENFILE NAME TO READ            
         BH    EXITOK                                                           
*                                                                               
         MVC   P(L'EMISBOOK),EMISBOOK                                           
         GOTOX VPRINTER                                                         
         B     EXITL               MISSING GENFILE                              
*                                                                               
         LTORG                                                                  
*                                                                               
SKIPTAB  DC    CL5'SPACE'          CONTROL CHARACTERS - SKIP LINE               
SKIPTBLQ EQU   *-SKIPTAB                                                        
         DC    CL5'EJECT'                                                       
         DC    CL5'PRINT'                                                       
         DC    X'FF'                                                            
*                                                                               
CARDTBL  DC    AL1(07),CL20'GENFILE='                                           
         DC    AL4(CARDGEN)                                                     
         DC    AL1(07),CL20'RECTYPE='                                           
         DC    AL4(CARDREC)                                                     
         DC    X'FF'                                                            
*                                                                               
EINVCARD DC    CL40'*** ERROR - INVALID CARD ***'                               
EMISBOOK DC    CL40'*** ERROR - MISSING GENFILE BOOK ***'                       
         SPACE 2                                                                
**********************************************************************          
* GENFILE= CARD PROCESSING ROUTINE                                   *          
*                                                                    *          
* THIS ROUTINE SETS THE CORRECT GENFILE FOR PANIC TO READ            *          
**********************************************************************          
         SPACE 1                                                                
CARDGEN  NTR1  ,                                                                
         MVC   GENFILE,P+8         SET GENFILE TO BE READ BY PANIC              
         B     EXITOK                                                           
         SPACE 2                                                                
**********************************************************************          
* RECTYPE= CARD PROCESSING ROUTINE                                   *          
*                                                                    *          
* THIS ROUTINE SETS THE CORRECT RECEQUS FOR PANIC TO READ            *          
**********************************************************************          
         SPACE 1                                                                
CARDREC  NTR1  ,                                                                
         MVC   RECTYPE,P+8         SET GENFILE TO BE READ BY PANIC              
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ OUTPUT FROM SORTER AND DUMP TO TABLES                          *         
***********************************************************************         
         SPACE 1                                                                
SORTDUMP NTR1  ,                                                                
*                                                                               
SRDUM02  GOTOX VSORTER,PLIST,GET   GET NEXT SORT RECORD                         
         ICM   R2,15,4(R1)                                                      
         BZ    SRDUMX                                                           
         LA    R2,4(R2)            GO PAST LENGTH                               
*                                                                               
         USING SRTRECD,R2                                                       
         CLI   SRTTYPE,SRTTREC     DSECT                                        
         BE    SRDUM04                                                          
         CLI   SRTTYPE,SRTTDTA     DATA                                         
         BE    SRDUM06                                                          
         CLI   SRTTYPE,SRTTEQU     EQUATE                                       
         BE    SRDUM08                                                          
         CLI   SRTTYPE,SRTTIDNT    IDENTIFIER                                   
         BE    SRDUM10                                                          
*                                                                               
SRDUM04  L     RF,ADSCCUR      *** DSECT TABLE                                  
         USING HEDTABD,RF                                                       
         MVC   HEDNAME,SRTNAME     DSECT NAME                                   
         MVC   HEDNUM,SRTRNUM      DSECT NUMBER                                 
         MVC   HEDIDNT,SRTRID      COUNT OF IDENTIFIERS                         
         MVC   HEDMEMBR,SRTRDSN    NUMBER OF MEMBERS                            
         MVC   HEDDESC,SRTDESC     DESCRIPTION                                  
         ST    RF,ADSCLAST                                                      
         LA    RF,HEDLENQ(RF)      SET NEXT FREE ENTRY                          
         ST    RF,ADSCCUR                                                       
         L     RF,CDSCTAB          INCREMENT MEMBER COUNT                       
         LA    RF,1(RF)                                                         
         ST    RF,CDSCTAB                                                       
         B     SRDUM02                                                          
         DROP  RF                                                               
*                                                                               
SRDUM06  L     RF,ADSCUR       *** DS TABLE                                     
         USING DTATABD,RF                                                       
         MVC   DTANAME,SRTNAME     DATA NAME                                    
         MVC   DTARNUM,SRTRNUM     DSECT NUMBER                                 
         MVC   DTADNUM,SRTDNUM     DS NUMBER                                    
         MVC   DTATYPE,SRTDTYPE    DATA TYPE                                    
         MVC   DTADSPN,SRTDSPN     DISPLACEMENT TO DATA                         
         MVC   DTAMLTN,SRTMLTN     REPEAT COUNT                                 
         MVC   DTALENN,SRTLENN     LENGTH OF DATA                               
         MVC   DTAIDNT,SRTDID      CURRENT IDENTIFIER NUMBER                    
         MVC   DTAMEMBR,SRTDCNT    NUMBER OF ASSOCIATED EQUATES                 
         MVC   DTADESC,SRTDESC     DESCRIPTION                                  
         ST    RF,ADSLAST                                                       
         LA    RF,DTALENQ(RF)                                                   
         ST    RF,ADSCUR                                                        
         L     RF,CDSTAB           INCREMENT MEMBER COUNT                       
         LA    RF,1(RF)                                                         
         ST    RF,CDSTAB                                                        
         B     SRDUM02                                                          
         DROP  RF                                                               
*                                                                               
SRDUM08  L     RF,AEQUCUR      *** EQUATE TABLE                                 
         USING EQUTABD,RF                                                       
         MVC   EQUNAME,SRTNAME     DATA NAME                                    
         MVC   EQURNUM,SRTRNUM     DSECT NUMBER                                 
         MVC   EQUDNUM,SRTDNUM     DS NUMBER                                    
         MVC   EQUENUM,SRTENUM     EQUATE NUMBER                                
         MVC   EQUEQUT,SRTEQUT     EQUATE TYPE                                  
         MVC   EQUEQUL,SRTEQUL     EQUATE LENGTH                                
         MVC   EQUEQUN,SRTEQUN     EQUATE VALUE - NUMERIC                       
         MVC   EQUDESC,SRTDESC                                                  
         ST    RF,AEQULAST                                                      
         LA    RF,EQULENQ(RF)                                                   
         ST    RF,AEQUCUR                                                       
         L     RF,CEQUTAB          INCREMENT MEMBER COUNT                       
         LA    RF,1(RF)                                                         
         ST    RF,CEQUTAB                                                       
         B     SRDUM02                                                          
         DROP  RF                                                               
*                                                                               
SRDUM10  OC    AIDNTAB,AIDNTAB *** IDENTIFIER TABLE                             
         BNZ   SRDUM18             TABLE HAS BEEN INITIALISED                   
*                                                                               
         XR    R3,R3               R3=(TOTAL LINES IN IDENTIFIER TABLE)         
         XC    WORD,WORD           NUMBER OF LINES IN CURRENT DSECT             
         XC    HALF,HALF                                                        
*                                                                               
         L     R5,ADSTAB           SCAN DATA TABLE -                            
         USING DTATABD,R5          CALCULATE NUMBER OF LINES REQUIRED           
         LA    RE,DTALENQ          FOR IDENTIFIER TABLE                         
         L     RF,ADSLAST                                                       
         XR    R0,R0                                                            
         ICM   R0,3,DTARNUM        SET FIRST DSECT NUMBER                       
*                                                                               
SRDUM12  CLM   R0,3,DTARNUM        SAME DSECT AS LAST IDENTIFIER?               
         BE    SRDUM14             YES                                          
*                                                                               
         A     R3,WORD             INCREMENT TOTAL LINES                        
         XC    WORD,WORD           RESET LINE COUNT FOR CURRENT DSECT           
         ICM   R0,3,DTARNUM        SET NEW DSECT NUMBER                         
*                                                                               
SRDUM14  OC    DTAIDNT,DTAIDNT     DS FLAGGED AS AN IDENTIFIER?                 
         BZ    SRDUM16             NO                                           
*                                                                               
         MVC   HALF,DTAMEMBR       NUMBER OF LINES REQUIRED =                   
         ICM   R1,15,WORD          M1*M2*M3...                                  
         BNZ   *+8                 WHERE MN IS THE NUMBER OF MEMBERS            
         LA    R1,1                IDENTIFIER N                                 
         MH    R1,HALF                                                          
         ST    R1,WORD             SAVE NEW TOTAL FOR THIS DSECT                
*                                                                               
SRDUM16  BXLE  R5,RE,SRDUM12       REPEAT FOR WHOLE DATA TABLE                  
         DROP  R5                                                               
*                                                                               
         A     R3,WORD             LAST IDENTIFIER                              
         LR    R1,R3               NUMBER OF RECORDS IN TABLE                   
         MH    R1,=Y(IDNLENQ)      LENGTH OF A HEDTAB ENTRY                     
         GOTOX AGETMAIN                                                         
         ST    R0,LIDNTAB          R0 = L'IDENTIFIER TABLE                      
         ST    R1,AIDNTAB          R1 = A(IDENTIFIER TABLE)                     
         ST    R1,AIDNCUR                                                       
*                                                                               
         BAS   RE,NUMFILL          FILL IN IDENTIFIER MATCH NUMBERS             
*                                                                               
SRDUM18  L     R5,AIDNTAB          FILL IN IDENTIFIER VALUES                    
         USING IDNTABD,R5                                                       
ID1      USING IDNIDD,IDNID1                                                    
ID2      USING IDNIDD,IDNID2                                                    
ID3      USING IDNIDD,IDNID3                                                    
         LA    RE,IDNLENQ                                                       
         L     RF,AIDNLAST                                                      
*                                                                               
SRDUM20  CLC   SRTRNUM,ID1.IDIRNUM                                              
         BNE   SRDUM22                                                          
         CLC   SRTDNUM,ID1.IDIDNUM                                              
         BNE   SRDUM22                                                          
         CLC   SRTENUM,ID1.IDIENUM                                              
         BNE   SRDUM22                                                          
         B     SRDUM26                                                          
*                                                                               
SRDUM22  CLC   SRTRNUM,ID2.IDIRNUM                                              
         BNE   SRDUM24                                                          
         CLC   SRTDNUM,ID2.IDIDNUM                                              
         BNE   SRDUM24                                                          
         CLC   SRTENUM,ID2.IDIENUM                                              
         BNE   SRDUM24                                                          
         B     SRDUM26                                                          
*                                                                               
SRDUM24  CLC   SRTRNUM,ID3.IDIRNUM                                              
         BNE   SRDUM28                                                          
         CLC   SRTDNUM,ID3.IDIDNUM                                              
         BNE   SRDUM28                                                          
         CLC   SRTENUM,ID3.IDIENUM                                              
         BNE   SRDUM28                                                          
*                                                                               
SRDUM26  LA    R1,IDNNAME                                                       
         CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         LA    R0,IDNNAME                                                       
         CR    R1,R0                                                            
         BNH   *+12                                                             
         MVI   0(R1),C'/'                                                       
         LA    R1,1(R1)                                                         
         MVC   0(L'SRTNAME,R1),SRTNAME                                          
         MVC   IDNTYPE,SRTKYEL                                                  
*                                                                               
SRDUM28  BXLE  R5,RE,SRDUM20                                                    
         B     SRDUM02                                                          
         DROP  R5                                                               
*                                                                               
SRDUMX   GOTOX VSORTER,PLIST,END                                                
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* FILL IDNTAB WITH COUNTERS READY FOR EQUATE POPULATION              *          
**********************************************************************          
         SPACE 1                                                                
NUMFILL  NTR1  ,                                                                
         XC    WORD,WORD           LINES REQUIRED FOR THIS ONE DSECT            
         XC    HALF,HALF                                                        
         XR    R3,R3               R3 = TOTAL NUMBER OF LINES REQUIRED          
         MVI   BYTE,C'N'           LAST TIME FLAG                               
*                                                                               
         L     R6,ADSTAB           SCAN DATA TABLE -                            
         USING DTATABD,R6          CALCULATE NUMBER OF LINES REQUIRED           
         LA    R4,DTALENQ          FOR EACH DSECT                               
         L     R5,ADSLAST                                                       
         XR    R0,R0                                                            
         ICM   R0,3,DTARNUM        SET FIRST DSECT NUMBER                       
*                                                                               
NFI02    CLM   R0,3,DTARNUM        SAME DSECT AS LAST IDENTIFIER?               
         BE    NFI26               YES                                          
*                                                                               
         L     R1,AIDNCUR          A(CURRENT TABLE ENTRY)                       
         USING IDNTABD,R1                                                       
ID1      USING IDNIDD,IDNID1                                                    
ID2      USING IDNIDD,IDNID2                                                    
ID3      USING IDNIDD,IDNID3                                                    
*                                                                               
         OC    IDCOUNT,IDCOUNT                                                  
         BNZ   NFI12               DSECT HAS IDENTIFIERS                        
*                                                                               
         L     R2,ADSCTAB                                                       
         LA    RE,HEDLENQ                                                       
         L     RF,ADSCLAST                                                      
*                                                                               
         USING HEDTABD,R2                                                       
NFI04    CLM   R0,3,HEDNUM                                                      
         BE    NFI06                                                            
         BXLE  R2,RE,NFI04                                                      
         DC    H'0'                                                             
*                                                                               
NFI06    LA    RF,NFITAB                                                        
         USING NFITABD,RF                                                       
*                                                                               
NFI08    CLI   0(RF),FF           END OF TABLE                                  
         BE    NFI24                                                            
         CLC   NFINAME,HEDNAME                                                  
         BE    NFI10                                                            
         LA    RF,NFILENQ(RF)                                                   
         B     NFI08                                                            
*                                                                               
NFI10    MVC   IDNNAME,SPACE      SET NAME OF SPECIAL IDENTIFIER                
         MVC   IDNNAME(L'NFINAME),NFINAME                                       
         MVI   IDNTYPE,IDNTKEY                                                  
         MVC   IDNSPEC1,NFITYPE1  SET ACRECTYPES FOR THIS IDENTIFIER            
         MVC   IDNSPEC2,NFITYPE2                                                
*                                                                               
         LH    RF,ICOUNT           SET NUMBER FOR COUNTER                       
         LA    RF,1(RF)                                                         
         STH   RF,ICOUNT                                                        
         STCM  RF,3,IDNINUM                                                     
*                                                                               
         L     RF,CIDNTAB          INCREMENT MEMBER COUNT                       
         LA    RF,1(RF)                                                         
         ST    RF,CIDNTAB                                                       
*                                                                               
         LA    R1,IDNLENQ(R1)      NEXT FREE                                    
         B     NFI24                                                            
         DROP  RF,R2                                                            
*                                                                               
NFI12    STCM  R0,3,IDREC          SAVE RECORD NUMBER                           
         MVC   IDNNAME,SPACE       FILL IN NAME                                 
*                                                                               
         XC    IDM1,IDM1           RESET MAXIMUMS                               
         XC    IDM2,IDM2                                                        
         XC    IDM3,IDM3                                                        
         XC    IDD1,IDD1           RESET DS NUMBERS                             
         XC    IDD2,IDD2                                                        
         XC    IDD3,IDD3                                                        
         LA    RF,1                POPULATE CURRENT COUNTERS                    
         STH   RF,IDC1                                                          
         STH   RF,IDC2                                                          
         STH   RF,IDC3                                                          
*                                                                               
IDLINE   USING DTATABD,RF                                                       
         ICM   RF,15,IO1           SET COUNTER MAXIMUMS                         
         BZ    *+16                                                             
         MVC   IDD1,IDLINE.DTADNUM  SET DATA NUMBER                             
         MVC   IDM1,IDLINE.DTAMEMBR SET DATA MEMBER MAXIMUM                     
*                                                                               
         ICM   RF,15,IO2           SECOND IDENTIFIER?                           
         BZ    *+16                NO                                           
         MVC   IDD2,IDLINE.DTADNUM                                              
         MVC   IDM2,IDLINE.DTAMEMBR                                             
*                                                                               
         ICM   RF,15,IO3           THIRD IDENTIFIER?                            
         BZ    *+16                NO                                           
         MVC   IDD3,IDLINE.DTADNUM                                              
         MVC   IDM3,IDLINE.DTAMEMBR                                             
         DROP  IDLINE                                                           
*                                                                               
NFI14    LH    RF,ICOUNT           SET NUMBER FOR COUNTER                       
         LA    RF,1(RF)                                                         
         STH   RF,ICOUNT                                                        
         STCM  RF,3,IDNINUM                                                     
*                                                                               
         L     RF,CIDNTAB          INCREMENT MEMBER COUNT                       
         LA    RF,1(RF)                                                         
         ST    RF,CIDNTAB                                                       
*                                                                               
         OC    IDM3,IDM3           THIRD IDENTIFIER FOR DSECT?                  
         BZ    NFI18               NO                                           
*                                                                               
         MVC   ID3.IDIRNUM,IDREC   SET #3 RECORD NUMBER                         
         MVC   ID3.IDIDNUM,IDD3    SET #3 DATA NUMBER                           
         MVC   ID3.IDIENUM,IDC3    SET #3 EQUATE NUMBER                         
         MVC   ID2.IDIRNUM,IDREC   SET #2 RECORD NUMBER                         
         MVC   ID2.IDIDNUM,IDD2    SET #2 DATA NUMBER                           
         MVC   ID2.IDIENUM,IDC2    SET #2 EQUATE NUMBER                         
         MVC   ID1.IDIRNUM,IDREC   SET #1 RECORD NUMBER                         
         MVC   ID1.IDIDNUM,IDD1    SET #1 DATA NUMBER                           
         MVC   ID1.IDIENUM,IDC1    SET #1 EQUATE NUMBER                         
*                                                                               
         LH    RF,IDC3             BUMP COUNTER 3                               
         LA    RF,1(RF)                                                         
         STH   RF,IDC3                                                          
         CH    RF,IDM3             DID IT OVERFLOW?                             
         BNH   NFI22               NO - NEXT LINE IN TABLE                      
*                                                                               
         LH    RF,IDC2             OVERFLOW - INCREMENT COUNTER 2               
         LA    RF,1(RF)                                                         
         STH   RF,IDC2                                                          
         CH    RF,IDM2             TEST COUNTER 2 OVERFLOW                      
         BNH   NFI16               NO                                           
*                                                                               
         LH    RF,IDC1             OVERFLOW - INCREMENT COUNTER 1               
         LA    RF,1(RF)                                                         
         STH   RF,IDC1                                                          
         CH    RF,IDM1             TEST COUNTER 1 OVERFLOW                      
         BNH   *+14                NO                                           
         LA    R1,IDNLENQ(R1)      NEXT LINE IN TABLE                           
         B     NFI24               FINISHED FOR DSECT                           
*                                                                               
         LA    RF,1                RESET COUNTER 2                              
         STH   RF,IDC2                                                          
*                                                                               
NFI16    LA    RF,1                RESET COUNTER 3                              
         STH   RF,IDC3                                                          
         B     NFI22               NEXT LINE IN TABLE                           
*                                                                               
NFI18    OC    IDM2,IDM2           SECOND IDENTIFIER FOR DSECT?                 
         BZ    NFI20               NO                                           
         MVC   ID2.IDIRNUM,IDREC   SET #2 RECORD NUMBER                         
         MVC   ID2.IDIDNUM,IDD2    SET #2 DATA NUMBER                           
         MVC   ID2.IDIENUM,IDC2    SET #2 EQUATE NUMBER                         
         MVC   ID1.IDIRNUM,IDREC   SET #1 RECORD NUMBER                         
         MVC   ID1.IDIDNUM,IDD1    SET #1 DATA NUMBER                           
         MVC   ID1.IDIENUM,IDC1    SET #1 EQUATE NUMBER                         
*                                                                               
         LH    RF,IDC2             BUMP COUNTER 2                               
         LA    RF,1(RF)                                                         
         STH   RF,IDC2                                                          
         CH    RF,IDM2             DID IT OVERFLOW?                             
         BNH   NFI22               NO                                           
*                                                                               
         LH    RF,IDC1             OVERFLOW - INCREMENT COUNTER 1               
         LA    RF,1(RF)                                                         
         STH   RF,IDC1                                                          
         CH    RF,IDM1             TEST COUNTER 1 OVERFLOW                      
         BNH   *+14                NO                                           
         LA    R1,IDNLENQ(R1)      NEXT LINE IN TABLE                           
         B     NFI24               FINISHED FOR DSECT                           
*                                                                               
         LA    RF,1                RESET COUNTER 2                              
         STH   RF,IDC2                                                          
         B     NFI22               NEXT LINE                                    
*                                                                               
NFI20    OC    IDM1,IDM1           FIRST IDENTIFIER FOR DSECT?                  
         BNZ   *+6                                                              
         DC    H'0'                HOW DID WE GET HERE?                         
*                                                                               
         MVC   ID1.IDIRNUM,IDREC   SET #1 RECORD NUMBER                         
         MVC   ID1.IDIDNUM,IDD1    SET #1 DATA NUMBER                           
         MVC   ID1.IDIENUM,IDC1    SET #1 EQUATE NUMBER                         
*                                                                               
         OC    IDM2,IDM2           LOWER COUNTER TO CONSIDER?                   
         BNZ   NFI22               YES                                          
*                                                                               
         LH    RF,IDC1             BUMP COUNTER 1                               
         LA    RF,1(RF)                                                         
         STH   RF,IDC1                                                          
         CH    RF,IDM1             TEST COUNTER 1 OVERFLOW                      
         BNH   NFI22               NO - NEXT IN COUNT                           
*                                                                               
         LA    R1,IDNLENQ(R1)      NEXT FREE                                    
         B     NFI24                                                            
*                                                                               
NFI22    LA    R1,IDNLENQ(R1)      NEXT LINE OF TABLE                           
         B     NFI14                                                            
         DROP  R1                                                               
*                                                                               
NFI24    ST    R1,AIDNCUR          SAVE NEXT HIGHEST IDNTAB ENTRY               
         SH    R1,=Y(IDNLENQ)                                                   
         ST    R1,AIDNLAST         SAVE CURRENT HIGHEST REACHED                 
*                                                                               
         CLI   BYTE,C'Y'           LAST TIME?                                   
         BE    NFIX                YES                                          
*                                                                               
         XC    WORD,WORD           RESET LINE COUNT FOR CURRENT DSECT           
         XC    IDCOUNT,IDCOUNT                                                  
         XC    IO1,IO1             RESET DS ADDRESS BUFFERS                     
         XC    IO2,IO2                                                          
         XC    IO3,IO3                                                          
         ICM   R0,3,DTARNUM        SET NEW DSECT NUMBER                         
*                                                                               
NFI26    OC    DTAIDNT,DTAIDNT     DATA ACTS AS AN IDENTIFIER?                  
         BZ    NFI28               NO                                           
*                                                                               
         LH    RF,IDCOUNT          BUMP NUMBER OF IDENTIFIERS                   
         LA    RF,1(RF)                                                         
         STH   RF,IDCOUNT                                                       
*                                                                               
         CLM   RF,3,=AL2(3)        MAX NUMBER OF IDENTIFIERS ALLOWED            
         BNH   *+6                                                              
         DC    H'0'                TOO MANY IDENTIFIERS                         
*                                                                               
         BCTR  RF,0                ZERO BASE & * 4                              
         SLL   RF,2                                                             
         LA    RF,IO1(RF)          INDEX INTO ADDRESS SAVE AREA                 
         ST    R6,0(RF)            SAVE A(THIS DATA LINE)                       
*                                                                               
         MVC   HALF,DTAMEMBR       NUMBER OF LINES REQUIRED =                   
         ICM   R1,15,WORD          M1*M2*M3...                                  
         BNZ   *+8                 WHERE MN IS THE NUMBER OF MEMBERS            
         LA    R1,1                IDENTIFIER N                                 
         MH    R1,HALF                                                          
         ST    R1,WORD             SAVE NEW TOTAL FOR THIS DSECT                
*                                                                               
NFI28    BXLE  R6,R4,NFI02                                                      
         MVI   BYTE,C'Y'                                                        
         B     NFI02                                                            
*                                                                               
NFIX     B     EXITOK                                                           
         DROP  R6                                                               
         SPACE 1                                                                
NFITAB   DC    CL8'CPYRECD',AL1(ACRTCPY,0)          COMPANY                     
         DC    CL8'UNTRECD',AL1(ACRTUNT,0)          UNIT                        
         DC    CL8'LDGRECD',AL1(ACRTLDG,0)          LEDGER                      
         DC    CL8'ACTRECD',AL1(ACRTACTH,ACRTACTL)  ACCOUNT                     
         DC    CL8'OFARECD',AL1(ACRTOFA,0)          OFFICE ACCOUNT              
         DC    CL8'CHDRECD',AL1(ACRTCHDH,0)         CONTRA HEADER               
         DC    CL8'CACRECD',AL1(ACRTCAC,0)          CONTRA ACCOUNT              
         DC    CL8'TRNRECD',AL1(ACRTTRN,ACRTTRNA)   TRANSACTION                 
         DC    CL8'TIMRECD',AL1(ACRTTIM,0)          TIME                        
         DC    XL1'FF'                                                          
         EJECT                                                                  
**********************************************************************          
* EXIT POINTS                                                        *          
**********************************************************************          
         SPACE 1                                                                
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         SPACE 2                                                                
**********************************************************************          
* LITERALS AND CONSTANTS                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
VTYPES   DC    V(ACRECTYP)                                                      
         DC    V(CARDS)                                                         
         DC    V(CPRINT)                                                        
         DC    V(HEXIN)                                                         
         DC    V(HEXOUT)                                                        
         DC    V(LOADER)                                                        
         DC    V(PANIC)                                                         
         DC    V(PRINT)                                                         
         DC    V(PRINTER)                                                       
         DC    V(SORTER)                                                        
         DC    V(SQUASHER)                                                      
*                                                                               
PANCMD   DC    CL10'READ'          PANIC COMMANDS                               
         DC    CL10'CLOSE'                                                      
         DC    CL10'PAN'                                                        
         DC    CL10'DIRECTORY'                                                  
PANCMDLQ EQU   *-PANCMD                                                         
*                                                                               
OTHERS   DS    0X                  OTHER EQUATED VALUES                         
         DC    CL3'*++'            MAPPER COMMAND EQUATE                        
         DC    CL5'*&&&&  '        COUNTRY SPECIFIC CODE SPECIFIERS             
         DC    CL5'*&&&&US'                                                     
         DC    CL5'*&&&&UK'                                                     
         DC    CL8'*&&&&US*&&&&'                                                
         DC    CL8'*&&&&UK*&&&&'                                                
OTHERLQ  EQU   *-OTHERS                                                         
*                                                                               
SORTCMND DC    CL8'PUT'           SORT COMMANDS                                 
         DC    CL8'GET'                                                         
         DC    CL8'END'                                                         
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO DO A GETMAIN CALL                                       *          
*                                                                    *          
* NTRY: R1 = LENGTH OF STORAGE REQUESTED                             *          
* EXIT: R0 = LENGTH OF STORAGE OBTAINED                              *          
*       R1 = A(STORAGE)                                              *          
**********************************************************************          
         SPACE 1                                                                
         DS    0H                                                               
         USING *,RB                                                             
GETMAIN  NTR1  BASE=(RF)                                                        
         SRL   R1,12               ROUND UP STORAGE REQUEST TO NEXT 4K          
         LA    R1,1(R1)                                                         
         LR    R0,R1                                                            
         SLL   R0,12                                                            
         LR    R2,R0               PRESERVE LENGTH REQUESTED                    
*                                                                               
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                GETMAIN FAILED RC IN (RF)                    
*                                                                               
         LR    R0,R2                                                            
         XIT1  REGS=(R0,R1)                                                     
         LTORG                                                                  
         SPACE 2                                                                
**********************************************************************          
* ROUTINE TO DO A FREEMAIN CALL                                      *          
*                                                                    *          
* NTRY: P1 = A(STORAGE TO FREE)                                      *          
*       P2 = L'STORAGE TO FREE                                       *          
**********************************************************************          
         SPACE 1                                                                
         DS    0H                                                               
         USING *,RB                                                             
FREEMAIN NTR1  BASE=(RF)                                                        
         L     R0,4(R1)            R0=L'STORAGE TO FREE                         
         L     R1,0(R1)            R1=A(STORAGE TO FREE)                        
         FREEMAIN RC,A=(1),LV=(0)                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                FREEMAIN FAILED RC IN (RF)                   
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RA,RB,RC                                                         
         DS    0D                                                               
         EJECT                                                                  
***********************************************************************         
* CREATE RECORD TYPE EQUATE TABLE                                     *         
***********************************************************************         
         SPACE 1                                                                
RECEQUS  CSECT                                                                  
         NMOD1 0,RECEQUS                                                        
         LR    RC,R1                                                            
         USING WRKD,RC                                                          
         MVI   INCNTRY,C'N'                                                     
         XC    SORND,SORND                                                      
         OPEN  (REQUFILE,OUTPUT)                                                
*                                                                               
REQU02   GOTOX VPANIC,PLIST,(X'80',READ),PAN,RECTYPE,INPLINE                    
         CLI   8(R1),0             READ NEXT LINE OK?                           
         BNE   REQUX               NO MORE LINES                                
*                                                                               
         MVC   INPLINE+INPNMBR(L'INPLINE-INPNMBR),SPACE                         
*                                  REMOVE LINE/LEVEL STAMP FROM END             
*                                                                               
*&&UK*&& CLC   USONLY1,INPLINE     THIS LINE COUNTRY SPECIFIC?                  
*&&US*&& CLC   UKONLY1,INPLINE     IF SO IGNORE IT                              
         BE    REQU02                                                           
*                                                                               
*&&UK*&& CLC   USONLY,INPLINE      *&& COUNTRY SPECIFIC EQUATES?                
*&&US*&& CLC   UKONLY,INPLINE                                                   
         BNE   *+8                 NO                                           
         MVI   INCNTRY,C'Y'        YES - SET WITHIN COUNTRY CODE                
*                                                                               
         CLC   UKUSOFF,INPLINE     *&& COUNTRY SPECIFIC CODE END                
         BNE   *+8                 NO                                           
         MVI   INCNTRY,C'N'                                                     
*                                                                               
         CLI   INCNTRY,C'Y'        WITHIN COUNTRY SPECIFIC CODE?                
         BE    REQU02              YES                                          
*                                                                               
         CLI   INPLINE,COMMENT     IGNORE COMMENT LINES                         
         BE    REQU02                                                           
*                                                                               
         L     RF,ASKIPTAB         PRINT INSTRUCTIONS (IGNORE THESE)            
REQU04   CLI   0(RF),FF            END OF TABLE                                 
         BE    REQU06              YES                                          
         CLC   0(SKIPTBLQ,RF),INPLINE+INPCOL2                                   
         BE    REQU02              MATCHED - SKIP THIS LINE                     
         LA    RF,SKIPTBLQ(RF)                                                  
         B     REQU04                                                           
*                                                                               
REQU06   LA    R7,SORBLK           OUTPUT RECORD BUILT IN THIS BLOCK            
         LR    R0,R7                                                            
         LH    R1,=Y(SORBLKLQ)                                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACE                                                       
         MVCL  R0,RE               CLEAR OUTPUT LINE TO SPACES                  
*                                                                               
         LA    RF,INPLINE                                                       
         CLI   0(RF),C' '                                                       
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         LR    R0,RF               PRESERVE A(SPACE AFTER NAME)                 
*                                                                               
         LA    RE,INPLINE                                                       
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R7),0(RE)       COPY OUT NAME                                
         LA    R7,1(RF,R7)                                                      
         MVI   0(R7),DEFSEP        INSERT SEPARATOR                             
         LA    R7,1(R7)                                                         
*                                                                               
         LR    RF,R0               RESTORE A(SPACE AFTER NAME)                  
         CLI   0(RF),C' '          LOOK FOR 'EQU' STATEMENT START               
         BH    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         CLI   0(RF),C' '          GO PAST 'EQU' STATEMENT                      
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         LR    RE,RF               SAVE A(VALUE START)                          
*                                                                               
         CLI   0(RF),C' '          GO PAST VALUE STATEMENT                      
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         LR    R0,RF               SAVE A(SPACE AFTER VALUE)                    
*                                                                               
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R7),0(RE)       COPY OUT VALUE                               
         LA    R7,1(RF,R7)                                                      
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         LR    RE,R0                                                            
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
*                                                                               
         LA    RF,INPLINE+L'INPLINE-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
*                                                                               
         SR    RF,RE                                                            
         EX    RF,*+4                                                           
         MVC   0(0,R7),0(RE)       COPY OUT DESCRIPTION                         
         LA    R7,1(RF,R7)                                                      
         MVI   0(R7),DEFSEP                                                     
         MVI   1(R7),DEFSEP        EXTRA SEPARATOR FOR END OF RECORD            
         LA    R7,2(R7)                                                         
*                                                                               
         LA    RF,SORLEN           SET LENGTH                                   
         SR    R7,RF                                                            
         STH   R7,SORLEN                                                        
         PUT   REQUFILE,SORLEN     WRITE RECORD TO FILE                         
         B     REQU02                                                           
*                                                                               
REQUX    GOTOX VPANIC,PLIST,CLOSE,PAN                                           
         CLOSE REQUFILE                                                         
         XMOD1 ,                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
REQUFILE DCB   DDNAME=REQUFILE,DSORG=PS,MACRF=(PM),RECFM=VB,           *        
               BLKSIZE=8200,LRECL=256,BUFNO=2                                   
*                                                                               
         DROP  RB,RC                                                            
         DS    0D                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST PASS PROCESSING FOR FILE                                      *         
* THIS ROUTINE CREATES A TABLE OF RECORDS FOR THE GENFILE REQUESTED   *         
***********************************************************************         
         SPACE 1                                                                
FRSTPASS CSECT                                                                  
         NMOD1 0,FRSTPASS,RA                                                    
         LR    RC,R1                                                            
         USING WRKD,RC                                                          
         MVI   PRCLFLAG,C'N'       RESET PROCESSING FLAG                        
         MVI   INCNTRY,C'N'        RESET COUNTRY SPECIFIC CODE FLAG             
         XR    R0,R0                                                            
*                                                                               
FPASS02  GOTOX VPANIC,PLIST,(X'80',READ),PAN,GENFILE,INPLINE                    
         CLI   8(R1),0             READ NEXT LINE OK?                           
         BNE   FPASS18             NO MORE LINES                                
*                                                                               
         MVC   INPLINE+INPNMBR(L'INPLINE-INPNMBR),SPACE                         
*                                  REMOVE LINE/LEVEL STAMP FROM END             
*                                                                               
         CLC   MAPFILE,INPLINE     MAPPER COMMAND?                              
         BNE   FPASS08             NO                                           
*                                                                               
         LA    RF,MAPTAB           TABLE OF MAPPER COMMANDS                     
         USING MAPTABD,RF                                                       
         XR    R1,R1                                                            
*                                                                               
FPASS04  CLI   MAPLEN,FF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R1,MAPLEN                                                        
         EX    R1,MAPCMP                                                        
         BE    FPASS06                                                          
         LA    RF,MAPLENQ(RF)                                                   
         B     FPASS04                                                          
*                                                                               
MAPCMP   CLC   MAPCMND(0),INPLINE+L'MAPFILE                                     
*                                                                               
FPASS06  ICM   RF,15,MAPADDR                                                    
         BASR  RE,RF                                                            
         B     FPASS08                                                          
*                                                                               
FPASS08  CLI   PRCLFLAG,C'Y'       ALLOWED TO PROCESS THIS LINE?                
         BE    FPASS02             NO                                           
*                                                                               
*&&UK*&& CLC   USONLY1,INPLINE     THIS LINE COUNTRY SPECIFIC?                  
*&&US*&& CLC   UKONLY1,INPLINE     IF SO IGNORE IT                              
         BE    FPASS02                                                          
*                                                                               
*&&UK*&& CLC   USONLY,INPLINE      *&& COUNTRY SPECIFIC EQUATES?                
*&&US*&& CLC   UKONLY,INPLINE                                                   
         BNE   *+8                 NO                                           
         MVI   INCNTRY,C'Y'        YES - SET WITHIN COUNTRY CODE                
*                                                                               
         CLC   UKUSOFF,INPLINE     *&& COUNTRY SPECIFIC CODE END                
         BNE   *+8                 NO                                           
         MVI   INCNTRY,C'N'                                                     
*                                                                               
         CLI   INCNTRY,C'Y'        WITHIN COUNTRY SPECIFIC CODE?                
         BE    FPASS02             YES                                          
*                                                                               
         CLI   INPLINE,COMMENT     IGNORE COMMENT LINES                         
         BE    FPASS02                                                          
*                                                                               
         L     RF,ASKIPTAB         PRINT INSTRUCTIONS (IGNORE THESE)            
FPASS10  CLI   0(RF),FF            END OF TABLE                                 
         BE    FPASS12             YES                                          
         CLC   0(SKIPTBLQ,RF),INPLINE+INPCOL2                                   
         BE    FPASS02             MATCHED - SKIP THIS LINE                     
         LA    RF,SKIPTBLQ(RF)                                                  
         B     FPASS10                                                          
*                                                                               
FPASS12  LA    RF,PROCTAB          TYPES TO PROCESS                             
         USING PROCTABD,RF                                                      
         XR    R1,R1                                                            
*                                                                               
FPASS14  CLI   PROCLEN,FF          END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN TYPE TO PROCESS                      
*                                                                               
         IC    R1,PROCLEN          TRY TO MATCH THIS TYPE                       
         EX    R1,FPASSCMP                                                      
         BE    FPASS16                                                          
         LA    RF,PROCLNQ(RF)                                                   
         B     FPASS14                                                          
*                                                                               
FPASSCMP CLC   PROCCMP(0),INPLINE+INPCOL2                                       
*                                                                               
FPASS16  ICM   RF,15,PROCADDR      A(TYPE HANDLER)                              
         BASR  RE,RF                                                            
         B     FPASS02             PROCESS NEXT LINE                            
         DROP  RF                                                               
*                                                                               
FPASS18  GOTOX VPANIC,PLIST,CLOSE,PAN                                           
         B     FPASSOK                                                          
         EJECT                                                                  
*                                                                               
FPASSL   CLI   *,FF                                                             
         B     FPASSX                                                           
FPASSH   CLI   *,0                                                              
         B     FPASSX                                                           
FPASSOK  CR    RB,RB                                                            
FPASSX   XIT1  ,                                                                
*                                                                               
MAPTAB   DC    AL1(6),CL10'PROCESS',AL4(FPMPRCL)                                
         DC    AL1(9),CL10'IDENTIFIER',AL4(FPMIDNT)                             
         DC    AL1(9),CL10'ASSOCIATED',AL4(FPMASOC)                             
         DC    AL1(3),CL10'TYPE      ',AL4(FPMTYPE)                             
         DC    X'FF'                                                            
*                                                                               
PROCTAB  DC    AL1(4),CL5'DSECT',AL4(FPDSECT)                                   
         DC    AL1(3),CL5'ORG  ',AL4(FPORG)                                     
         DC    AL1(2),CL5'EQU  ',AL4(FPEQU)                                     
         DC    AL1(1),CL5'DS   ',AL4(FPDS)                                      
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* SET PROCESSING LINES ON/OFF                                         *         
***********************************************************************         
         SPACE 1                                                                
FPMPRCL  NTR1                                                                   
         GOTOX VSQUASH,PLIST,INPLINE,L'INPLINE                                  
*                                                                               
         LA    R2,INPLINE                                                       
         CLI   0(R2),C' '          GO PAST MAPPER COMMAND                       
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         LA    R2,1(R2)            AND FOLLOWING SPACE                          
*                                                                               
         LA    RF,PRCLTAB                                                       
         USING PRCLTABD,RF                                                      
         XR    R1,R1                                                            
*                                                                               
FPMSK02  CLI   0(RF),FF            END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN COMMAND                              
*                                                                               
         IC    R1,PRCLLEN                                                       
         EX    R1,FPMPCMP                                                       
         BE    FPMSK04                                                          
         LA    RF,PRCLLENQ(RF)                                                  
         B     FPMSK02                                                          
*                                                                               
FPMPCMP  CLC   PRCLCMND(0),0(R2)                                                
*                                                                               
FPMSK04  MVC   PRCLFLAG,PRCLINS   SET CORRECT FLAG INSTRUCTION                  
         B     FPASSOK                                                          
*                                                                               
PRCLTAB  DC    AL1(1),CL8'ON',C'N'                                              
         DC    AL1(2),CL8'OFF',C'Y'                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
***********************************************************************         
* SET NEXT DS IS AN IDENTIFIER                                        *         
***********************************************************************         
         SPACE 1                                                                
FPMIDNT  NTR1                                                                   
         CLI   PRCLFLAG,C'Y'       ALLOWED TO PROCESS?                          
         BE    FPASSOK             NO                                           
         CLI   INCNTRY,C'Y'        ALLOWED TO PROCESS?                          
         BE    FPASSOK             NO                                           
*                                                                               
         MVI   MAPIDENT,C'Y'                                                    
         B     FPASSOK                                                          
         SPACE 2                                                                
***********************************************************************         
* SET EQUATE IS ASSOCIATED WITH DS NOT IMMEDIATELY PRECEDING IT       *         
***********************************************************************         
         SPACE 1                                                                
FPMASOC  NTR1                                                                   
         CLI   PRCLFLAG,C'Y'       ALLOWED TO PROCESS?                          
         BE    FPASSOK             NO                                           
         CLI   INCNTRY,C'Y'        ALLOWED TO PROCESS?                          
         BE    FPASSOK             NO                                           
*                                                                               
         MVI   MAPASOC,C'Y'        SET DS OVERRIDDEN FOR EQUATE                 
         GOTOX VSQUASH,PLIST,INPLINE,L'INPLINE                                  
*                                                                               
         LA    R2,INPLINE                                                       
         CLI   0(R2),C' '          GO PAST MAPPER COMMAND                       
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         LA    R2,1(R2)            AND FOLLOWING SPACE                          
*                                                                               
         MVC   MAPASOCE,0(R2)      SAVE EQUATED NAME                            
*                                                                               
         L     R5,AGENTAB          FIRST LINE OF TABLE                          
ALL      USING LINTABD,R5                                                       
         XR    RF,RF                                                            
*                                                                               
FPMAS02  ICM   RF,1,ALL.LINLEN     REACHED END OF TABLE?                        
         BZ    FPASSOK             YES - UNRESOLVED EQUATE                      
         CLI   ALL.LINTYPE,LINTDTA                                              
         BNE   FPMAS04                                                          
         CLC   MAPASOCE,ALL.LINNAME  MATCH NAME?                                
         BE    FPMAS06                                                          
*                                                                               
FPMAS04  BXH   R5,RF,FPMAS02                                                    
*                                                                               
FPMAS06  ST    R5,ADSOVER          SET OVERRIDE DS SPACE                        
         XC    MAPASOCE,MAPASOCE                                                
         B     FPASSOK                                                          
         DROP  ALL                                                              
         SPACE 2                                                                
***********************************************************************         
* SET TYPE OF DSECT TO PROCESS                                        *         
***********************************************************************         
         SPACE 1                                                                
FPMTYPE  NTR1                                                                   
         XC    DTYPE,DTYPE                                                      
         GOTOX VSQUASH,PLIST,INPLINE,L'INPLINE                                  
*                                                                               
         LA    R2,INPLINE                                                       
         CLI   0(R2),C' '          GO PAST TYPE COMMAND                         
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         LA    R2,1(R2)            AND FOLLOWING SPACE                          
*                                                                               
         LA    RF,DTYPTAB                                                       
         USING DTYPTABD,RF                                                      
         XR    R1,R1                                                            
*                                                                               
FPTYP02  CLI   0(RF),FF            END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN COMMAND                              
*                                                                               
         IC    R1,DTYPLEN                                                       
         EX    R1,FPTYCMP                                                       
         BE    FPTYP04                                                          
         LA    RF,DTYPLENQ(RF)                                                  
         B     FPTYP02                                                          
*                                                                               
FPTYCMP  CLC   DTYPCMND(0),0(R2)                                                
*                                                                               
FPTYP04  MVC   DTYPE,DTYPINS      SET CORRECT FLAG INSTRUCTION                  
         B     FPASSOK                                                          
*                                                                               
DTYPTAB  DC    AL1(2),CL8'KEY     ',AL1(IDNTKEY)                                
         DC    AL1(6),CL8'ELEMENT ',AL1(IDNTEL)                                 
         DC    X'FF'                                                            
         SPACE 2                                                                
***********************************************************************         
* RECORD TABLE (COVERS DSECTS)                                        *         
***********************************************************************         
         SPACE 1                                                                
FPDSECT  NTR1  ,                                                                
         L     R2,AGENCUR          NEXT EMPTY LINE IN TABLE                     
         USING LINTABD,R2                                                       
         ST    R2,ADSECT           SAVE A(CURRENT DSECT)                        
*                                                                               
         MVC   LINNAME,INPLINE     SET EQUATED NAME                             
         MVI   LINTYPE,LINTREC     SET RECORD HEADER                            
         MVI   LINLEN,LINRLENQ     SET LENGTH                                   
         MVC   LINEKYEL,DTYPE                                                   
*                                                                               
         L     RF,RCOUNT           INCREMENT RECORD COUNT                       
         LA    RF,1(RF)                                                         
         ST    RF,RCOUNT                                                        
         STCM  RF,3,LINRRNUM       SET COUNT IN TABLE                           
*                                                                               
         MVC   LINRDESC,INPLINE+INPCOL4                                         
*                                                                               
         MVI   FNDORG,C'N'         RESET UNRESOLVED ORG POINTER                 
         XC    STAR,STAR           RESET CURRENT DISPLACEMENT POINTER           
         XC    STARHIGH,STARHIGH   RESET HIGHEST DISPLACEMENT POINTER           
*                                                                               
         LA    R2,LINRLENQ(R2)                                                  
         ST    R2,AGENCUR          SET NEXT AVAILABLE SPACE IN TABLE            
         B     FPASSOK                                                          
         EJECT                                                                  
***********************************************************************         
* DATA TABLE (COVERS DS VALUES)                                       *         
*                                                                     *         
* A LINE OF DATA IS COMPRISED OF THE FOLLOWING FIELDS SEPERATED BY    *         
* A VARIABLE NUMBER OF SPACES:                                        *         
*                                                                     *         
* 1. NAME (MAX 8 CHARACTERS)                                          *         
* 2. IDENTIFIER - DS FOR RESERVED SPACE                               *         
* 3.              ANY COMBINATION OF THE FOLLOWING                    *         
*                 1. MULTIPLIER                       (OPTIONAL)      *         
*                 2. DATA TYPE - LENGTH IMPLICIT                      *         
*                 3. DATA LENGTH QUALIFIER            (OPTIONAL)      *         
***********************************************************************         
         SPACE 1                                                                
FPDS     NTR1  ,                                                                
         L     R2,AGENCUR          LOAD CURRENT LINE INFORMATION                
         USING LINTABD,R2                                                       
         ST    R2,ADS              SET A(CURRENT DS)                            
*                                                                               
         MVC   LINNAME,INPLINE     SET EQUATED NAME                             
         MVI   LINTYPE,LINTDTA     SET DATA SPACE                               
         MVI   LINLEN,LINDLENQ     SET DATA LENGTH                              
         MVC   LINEKYEL,DTYPE                                                   
*                                                                               
         MVC   LINDDSPE,SPACE      CLEAR BUFFERS                                
         MVC   LINDMLTE,SPACE                                                   
         MVC   LINDLENE,SPACE                                                   
         MVC   LINDDESC,SPACE                                                   
*                                                                               
         L     R1,DCOUNT           INCREMENT TOTAL DATA ITEM COUNT              
         LA    R1,1(R1)                                                         
         ST    R1,DCOUNT                                                        
         STCM  R1,3,LINDDNUM       SET CURRENT DATA NUMBER                      
*                                                                               
         L     RF,ADSECT                                                        
DSECT    USING LINTABD,RF                                                       
         MVC   LINDRNUM,DSECT.LINRRNUM                                          
*                                                                               
         XR    R1,R1               INCREMENT DSECT DATA ITEM TOTAL              
         ICM   R1,3,DSECT.LINRDSNO                                              
         LA    R1,1(R1)                                                         
         STCM  R1,3,DSECT.LINRDSNO                                              
         DROP  DSECT                                                            
*                                                                               
         CLI   MAPIDENT,C'Y'       IS THIS A RECORD IDENTIFIER?                 
         MVI   MAPIDENT,C'N'                                                    
         BNE   FPDS01              NO                                           
*                                                                               
         OI    LINDFLAG,LINDFID    SET IDENTIFIER                               
         L     RF,ADSECT                                                        
DSECT    USING LINTABD,RF                                                       
         XR    R1,R1               INCREMENT DSECT IDENTIFIER TOTAL             
         ICM   R1,3,DSECT.LINRID                                                
         LA    R1,1(R1)                                                         
         STCM  R1,3,DSECT.LINRID                                                
         STCM  R1,3,LINDID         SET CURRENT DS IDENTIFIER NUMBER             
         DROP  DSECT                                                            
*                                                                               
FPDS01   CLI   FNDORG,C'Y'         TEST ORG STATEMENT FOUND                     
         BE    FPDS02                                                           
         OI    LINDFLAG,LINDFDSP   RESOLVED DISPLACEMENT                        
         L     R1,STAR                                                          
         STCM  R1,3,LINDDSPN       SAVE NUMERIC DISPLACEMENT                    
*                                                                               
FPDS02   LA    R3,INPLINE+INPCOL3  PROCESS SPACE VALUE                          
*                                                                               
         GOTOX EQVAL,PLIST,(R3),LINDMLTN,LINDMLTE                               
         BL    FPDS04              MULTPLIER CANNOT BE RESOLVED                 
*                                                                               
         BE    *+10                MULTIPLIER SET EXPLICITLY                    
         MVC   LINDMLTN,=AL2(1)    DEFAULT MULTIPLIER = 1                       
*                                                                               
         OI    LINDFLAG,LINDFMLT   RESOLVED MULTIPLIER                          
*                                                                               
FPDS04   LR    R3,RF               NEXT SPACE ON LINE RETURNED IN RF            
         LR    R6,R3                                                            
         LA    R5,TYPTAB                                                        
         USING TYPTABD,R5                                                       
         LA    RF,TYPTABLQ                                                      
*                                                                               
FPDS06   CLI   TYPTYPE,FF          END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN DATA TYPE                            
         CLC   TYPTYPE,0(R6)                                                    
         BE    FPDS08              MATCHED DATA TYPE                            
         BXH   R5,RF,FPDS06                                                     
*                                                                               
FPDS08   MVC   LINDDTYP,TYPEQU     SET DATA TYPE EQUATE                         
         LA    R6,1(R6)            BUMP PAST DATA TYPE                          
*                                                                               
         CLI   0(R6),C'L'          LENGTH OVERRIDE FOR TYPE?                    
         BE    FPDS10              YES                                          
*                                                                               
         OI    LINDFLAG,LINDFLEN   SET LENGTH RESOLVED                          
         XR    RF,RF                                                            
         IC    RF,TYPLEN           USE DEFAULT LENGTH FOR DATA TYPE             
         STCM  RF,3,LINDLENN                                                    
         B     FPDS12                                                           
         DROP  R5                                                               
*                                                                               
FPDS10   LA    R6,1(R6)            GO PAST THE 'L' LENGTH MODIFIER              
         GOTOX EQVAL,PLIST,(R6),LINDLENN,LINDLENE                               
         BNE   *+8                                                              
         OI    LINDFLAG,LINDFLEN   RESOLVED LENGTH                              
         LR    R6,RF               NEXT SPACE ON LINE                           
*                                                                               
FPDS12   MVC   LINDDESC,SPACE                                                   
         LA    RF,INPLINE+L'INPLINE-1                                           
*                                                                               
FPDS14   CR    RF,R6               REACHED END OF LINE                          
         BL    FPDS16                                                           
         CLI   0(R6),C' '          GO PAST SPACES BEFORE DESCRIPTION            
         BH    *+12                                                             
         LA    R6,1(R6)                                                         
         B     FPDS14                                                           
*                                                                               
         CLI   0(RF),C' '          STRIP SPACES FROM END                        
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
*                                                                               
         SR    RF,R6               RF=(LENGTH OF DESCRIPTION-1)                 
         BM    FPDS16              NO DESCRIPTION                               
         EX    RF,*+4                                                           
         MVC   LINDDESC(0),0(R6)   COPY IN DESCRIPTION                          
*                                                                               
FPDS16   TM    LINDFLAG,(LINDFDSP+LINDFMLT+LINDFLEN)                            
         BO    *+12                                                             
         MVI   FNDORG,C'Y'         DISPLACEMENT NOT FULLY RESOLVED              
         B     FPDSX                                                            
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,3,LINDLENN                                                    
         XR    R1,R1                                                            
         ICM   R1,3,LINDMLTN                                                    
         MR    RE,R1               LENGTH OF DS IS (MULTIPLIER*LENGTH)          
         A     RF,STAR                                                          
         ST    RF,STAR             SET CURRENT DISPLACEMENT POINTER             
*                                                                               
         C     RF,STARHIGH         PAST HIGHEST POINT REACHED THUS FAR?         
         BNH   *+8                 NO                                           
         ST    RF,STARHIGH                                                      
*                                                                               
FPDSX    LA    R2,LINDLENQ(R2)                                                  
         ST    R2,AGENCUR          GO TO NEXT FREE LINE                         
         B     FPASSOK                                                          
*                                                                               
TYPTAB   DC    C'D',AL1(LINDDT_D),AL1(8) KNOWN DS DATA TYPES                    
         DC    C'F',AL1(LINDDT_F),AL1(4)                                        
         DC    C'V',AL1(LINDDT_V),AL1(4)                                        
         DC    C'A',AL1(LINDDT_A),AL1(4)                                        
         DC    C'H',AL1(LINDDT_H),AL1(2)                                        
         DC    C'S',AL1(LINDDT_S),AL1(2)                                        
         DC    C'X',AL1(LINDDT_X),AL1(1)                                        
         DC    C'C',AL1(LINDDT_C),AL1(1)                                        
         DC    C'P',AL1(LINDDT_P),AL1(1)                                        
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO PROCESS AN EQUATE STATEMENT                             *          
*                                                                    *          
* A LINE OF DATA IS COMPRISED OF THE FOLLOWING FIELDS SEPERATED BY   *          
* A VARIABLE NUMBER OF SPACE:                                        *          
*                                                                    *          
* 1. NAME (MAX 8 CHARACTERS)                                         *          
* 2. IDENTIFIER - EQU FOR EQUATED CONSTANT                           *          
* 3. EQUATE:      1. DATA TYPE                        (OPTIONAL)     *          
*                 2. VALUE - CAN BE FORMED FROM OTHER EQUATES        *          
**********************************************************************          
         SPACE 1                                                                
FPEQU    NTR1  ,                                                                
         L     R2,AGENCUR                                                       
         USING LINTABD,R2                                                       
*                                                                               
         L     RF,ECOUNT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ECOUNT                                                        
*                                                                               
         MVI   LINLEN,LINELENQ     SET EQUATED LENGTH                           
         MVI   LINTYPE,LINTEQU     SET EQUATE TYPE                              
         MVC   LINNAME,INPLINE     SET EQUATED NAME                             
         MVC   LINEKYEL,DTYPE                                                   
*                                                                               
         MVC   LINEDNOE,SPACE      CLEAR EQUATE BUFFERS                         
         MVC   LINEEQUE,SPACE                                                   
         MVC   LINEDESC,SPACE                                                   
*                                                                               
         CLI   MAPASOC,C'Y'        DATA NUMBER OVERRIDDEN?                      
         BE    *+12                YES                                          
         L     RF,ADS              A(CURRENT DS)                                
         B     FPEQU01                                                          
*                                                                               
         OC    MAPASOCE,MAPASOCE   UNRESOLVED OVERRIDE?                         
         BNZ   *+12                YES                                          
         L     RF,ADSOVER          A(OVERRIDE DS)                               
         B     FPEQU01                                                          
*                                                                               
         MVC   LINEDNOE,MAPASOCE   SET OVERRIDE DATA EQUATE                     
         B     FPEQU02                                                          
*                                                                               
DSPAC    USING LINTABD,RF                                                       
FPEQU01  MVC   LINERNUM,DSPAC.LINDRNUM  SET DATA NUMBER                         
         MVC   LINEDNUM,DSPAC.LINDDNUM  SET DATA NUMBER                         
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,DSPAC.LINDEQNO INCREMENT COUNT OF ASSOCIATED EQUS           
         LA    R1,1(R1)                                                         
         STCM  R1,3,DSPAC.LINDEQNO SAVE IT                                      
         STCM  R1,3,LINEENUM       SAVE THIS EQUATE NUMBER COUNT                
         DROP  DSPAC                                                            
*                                                                               
FPEQU02  XC    MAPASOC,MAPASOC     RESET OVERRIDES                              
         XC    MAPASOCE,MAPASOCE                                                
*                                                                               
         GOTOX VSQUASH,PLIST,INPLINE,L'INPLINE                                  
*                                                                               
         LA    R3,INPLINE          GO PAST EQUATED NAME                         
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
         LA    R3,1(R3)            GO PAST SPACE AFTER NAME                     
         CLI   0(R3),C' '          GO PAST EQU STATEMENT                        
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         LA    R3,1(R3)            GO PAST SPACE AFTER EQU                      
*                                                                               
         GOTOX EQUSTR,PLIST,(R3)                                                
         BNE   FPEQ04              NOT AN EQUATE STRING                         
         LR    R3,RF               NEXT FREE ON LINE                            
*                                                                               
         OI    LINEFLAG,LINEFEQU   EQUATE RESOLVED                              
         MVI   LINEEQU,LINE_NUM    DEFAULT EQUATE TYPE                          
         CLI   ESMATCH,C'Y'        STRING HAS ALL SAME TYPE OF EQUATE?          
         BNE   *+10                                                             
         MVC   LINEEQU,ESTYPE      SET EQUATE TYPE TO BE SAME AS STRING         
*                                                                               
         L     RF,ESTHIS                                                        
         STCM  RF,3,LINEEQUN       SET EQUATE VALUE                             
         LA    RF,2                                                             
         CLI   LINEEQUN,0          LENGTH IS EITHER 1 OR 2                      
         BNE   *+8                                                              
         LA    RF,1                                                             
         STCM  RF,1,LINEEQUL       SET EQUATE LENGTH                            
*                                                                               
         B     FPEQ12                                                           
*                                                                               
FPEQ04   LA    RF,EQUTBLE          EQUATED VALUE                                
         USING EQUTBLD,RF                                                       
*                                                                               
FPEQ06   CLI   EQUTYPE,FF          END OF TABLE                                 
         BE    FPEQ10                                                           
         CLC   EQUTYPE,0(R3)                                                    
         BE    FPEQ08                                                           
         LA    RF,EQUTBLLQ(RF)                                                  
         B     FPEQ06                                                           
*                                                                               
FPEQ08   ICM   RF,15,EQUROUT       VALIDATION ROUTINE                           
         BASR  RE,RF                                                            
         LR    R3,RF               NEXT SPACE ON LINE                           
         B     FPEQ12                                                           
*                                                                               
FPEQ10   MVC   LINEEQUE,ESSTR      SAVE EQUATE STRING                           
         B     FPEQ12                                                           
*                                                                               
FPEQ12   LA    R3,1(R3)            NEXT FREE ON LINE                            
         LA    RF,INPLINE+L'INPLINE-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
*                                                                               
         SR    RF,R3               RF=(LENGTH OF DESCRIPTION-1)                 
         BM    FPEQUX                                                           
         EX    RF,*+4                                                           
         MVC   LINEDESC(0),0(R3)                                                
*                                                                               
FPEQUX   LA    R2,LINELENQ(R2)                                                  
         ST    R2,AGENCUR          GO TO NEXT FREE LINE                         
         B     FPASSOK                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
EQUTBLE  DC    C'X',AL1(LINDDT_X),AL4(EQHEX)                                    
         DC    C'C',AL1(LINDDT_C),AL4(EQCHAR)                                   
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* EQUATED CHARACTER VALUE HANDLER                                    *          
**********************************************************************          
         SPACE 1                                                                
EQCHAR   NTR1  ,                                                                
         LA    R3,1(R3)            GO PAST EQUATE TYPE                          
*                                                                               
         CLI   0(R3),C''''         LOOK FOR START DELIMITER                     
         BE    EQC02                                                            
         LR    RF,R3               NO DELIMITER - UNRESOLVED EQUATE             
         MVC   LINEEQUE,ESSTR      SAVE EQUATE STRING                           
         B     EQCX                                                             
*                                                                               
EQC02    LA    R3,1(R3)            GO PAST START DELIMITER                      
         LR    RF,R3                                                            
*                                                                               
         CLI   0(RF),C''''         TEST FOR END DELIMITER                       
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         LA    R0,1(RF)            NEXT CHARACTER AFTER END DELIMITER           
*                                                                               
         OI    LINEFLAG,LINEFEQU   SET EQUATE RESOLVED                          
         MVI   LINEEQU,LINE_CHR    SET CHARACTER EQUATE                         
*                                                                               
         SR    RF,R3                                                            
         STC   RF,LINEEQUL         SAVE LENGTH                                  
*                                                                               
         CLM   RF,1,=AL1(1)        EQUATE LENGTH IS 1?                          
         BNE   EQC04               NO                                           
         ICM   RF,1,0(R3)                                                       
         B     EQC08                                                            
*                                                                               
EQC04    CLM   RF,1,=AL1(2)        EQUATE LENGTH IS 2?                          
         BNE   EQC06               NO                                           
         ICM   RF,3,0(R3)                                                       
         B     EQC08                                                            
*                                                                               
EQC06    DC    H'0'                EQUATE IS LONGER THAN 2 BYTES                
*                                                                               
EQC08    STCM  RF,3,LINEEQUN                                                    
         LR    RF,R0                                                            
         B     EQCX                                                             
*                                                                               
EQCX     CLI   0(RF),C' '          END OF EQUATE                                
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         XIT1  REGS=(RF)                                                        
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
**********************************************************************          
* EQUATED HEX VALUE HANDLER                                          *          
**********************************************************************          
         SPACE 1                                                                
EQHEX    NTR1  ,                                                                
         LA    R3,1(R3)            GO PAST EQUATE TYPE                          
*                                                                               
         CLI   0(R3),C''''         LOOK FOR START DELIMITER                     
         BE    EQH02                                                            
         LR    RF,R3               NO DELIMITER - UNRESOLVED EQUATE             
         MVC   LINEEQUE,ESSTR      SAVE EQUATE STRING                           
         B     EQHX                                                             
*                                                                               
EQH02    LA    R3,1(R3)            GO PAST START DELIMITER                      
         XR    R1,R1                                                            
         LR    R6,R3                                                            
*                                                                               
         CLI   0(R6),C''''         TEST FOR END DELIMITER                       
         BE    *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
*                                                                               
         SR    R6,R3                                                            
         XC    WORK,WORK                                                        
         GOTOX VHEXIN,PLIST,(R3),WORK,(R6)                                      
         ICM   RF,15,12(R1)                                                     
         BNZ   *+10                                                             
         LR    RF,R3                                                            
         B     EQHX                                                             
*                                                                               
         OI    LINEFLAG,LINEFEQU   EQUATE RESOLVED                              
         MVI   LINEEQU,LINE_HEX    HEXADECIMAL EQUATE                           
         STCM  RF,1,LINEEQUL       SET LENGTH OF HEX VALUE                      
*                                                                               
         CLM   RF,1,=AL1(1)        EQUATE LENGTH IS 1?                          
         BNE   EQH04               NO                                           
         ICM   RF,1,WORK                                                        
         B     EQH08                                                            
*                                                                               
EQH04    CLM   RF,1,=AL1(2)        EQUATE LENGTH IS 2?                          
         BNE   EQH06               NO                                           
         ICM   RF,3,WORK                                                        
         B     EQH08                                                            
*                                                                               
EQH06    DC    H'0'                EQUATE IS LONGER THAN 2 BYTES                
*                                                                               
EQH08    STCM  RF,3,LINEEQUN                                                    
         LR    RF,R3                                                            
         B     EQHX                                                             
*                                                                               
EQHX     CLI   0(RF),C' '          END OF EQUATE                                
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         XIT1  REGS=(RF)                                                        
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
**********************************************************************          
* ROUTINE TO PROCESS ORG STATEMENT                                   *          
**********************************************************************          
         SPACE 1                                                                
FPORG    NTR1  ,                                                                
         GOTOX VSQUASH,PLIST,INPLINE,L'INPLINE                                  
         MVI   FNDORG,C'Y'         SET UNPROCESSED ORG POINTER                  
*                                                                               
         LA    R3,INPLINE          BRANCH PAST INITIAL SPACE                    
         CLI   0(R3),C' '                                                       
         BNE   *+8                                                              
         LA    R3,1(R3)                                                         
*                                                                               
         CLI   0(R3),C' '          GO PAST ORG STATEMENT                        
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         LA    R3,1(R3)            AND SPACE AFTER ORG STATEMENT                
*                                                                               
         CLI   0(R3),C' '          CHECK FOR TEXT AFTER ORG STATEMENT           
         BNE   FPORG02                                                          
         MVC   STAR,STARHIGH       RESET POINTER TO HIGHEST REACHED             
         MVI   FNDORG,C'N'         SET PROCESSED ORG POINTER                    
         B     FPASSOK                                                          
*                                                                               
FPORG02  GOTOX EQUSTR,PLIST,(R3)                                                
         BE    *+6                                                              
         DC    H'0'                ORG POINTER CANNOT BE PROCESSED              
*                                                                               
         MVI   FNDORG,C'N'         RESET ORG PROCESSED INDICATOR                
         MVC   STAR,ESTHIS         RESET CURRENT POINTER                        
         B     FPASSOK                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO VALIDATE EITHER A SINGLE EQUATE OR A SINGLE NUMBER      *          
*                                                                    *          
* NTRY P1: A(VALUE ON LINE TO TRY TO VALIDATE)                       *          
*      P2: A(BUFFER TO FILL IF VALID - XL2)                          *          
*      P3: A(BUFFER TO FILL IF NOT VALID - CL30)                     *          
*                                                                    *          
* EXIT CC: EQ : EQUATE/NUMBER RESOLVED                               *          
* EXIT CC: LO : EQUATE/NUMBER NOT RESOLVED                           *          
* EXIT CC: HI : EQUATE/NUMBER NOT FOUND                              *          
**********************************************************************          
         SPACE 1                                                                
EQVAL    NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         LR    R6,R2                                                            
         XR    R1,R1                                                            
*                                                                               
EQVAL02  CLI   0(R6),C'0'          TEST FOR NUMERIC EQUATE VALUE                
         BL    EQVAL04             NON-NUMERIC                                  
         CLI   0(R6),C'9'                                                       
         BH    EQVAL04             NON-NUMERIC                                  
         LA    R1,1(R1)                                                         
         LA    R6,1(R6)                                                         
         B     EQVAL02                                                          
*                                                                               
EQVAL04  LTR   R1,R1               NUMERIC EQUATE?                              
         BZ    EQVAL06             NO - SEE IF EQUATE RESOLVED                  
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,EQPK             PACK NUMERIC EQUATE                          
         CVB   R1,DUB                                                           
         STCM  R1,3,0(R3)                                                       
         B     EQVOK                                                            
*                                                                               
EQPK     PACK  DUB,0(0,R2)                                                      
*                                                                               
EQVAL06  CLI   0(R6),C'('          EQUATED EQUATE VALUE?                        
         BE    EQVAL08             YES                                          
         B     EQVH                NO EQUATED VALUE                             
*                                                                               
EQVAL08  LA    R2,1(R2)            GO PAST OPENING BRACKET                      
         LA    R6,1(R6)                                                         
         XR    R1,R1                                                            
*                                                                               
         MVI   ESLEN,C'N'                                                       
         CLC   =C'L''',0(R2)       WANT LENGTH OF EQUATE?                       
         BNE   EQVAL10             NO                                           
         MVI   ESLEN,C'Y'                                                       
         LA    R2,2(R2)            GO PAST L'MODIFIER                           
         LA    R6,2(R6)                                                         
*                                                                               
EQVAL10  CLI   0(R6),C')'          END OF EQUATED VALUE?                        
         BE    EQVAL12             YES                                          
         LA    R6,1(R6)                                                         
         LA    R1,1(R1)                                                         
         B     EQVAL10                                                          
*                                                                               
EQVAL12  LA    R6,1(R6)            R6 POINTS TO NEXT ON LINE                    
         BCTR  R1,0                                                             
*                                                                               
         L     R5,AGENTAB          FIRST LINE OF TABLE                          
ALL      USING LINTABD,R5                                                       
         XR    RF,RF                                                            
*                                                                               
EQVAL14  ICM   RF,1,ALL.LINLEN     REACHED END OF TABLE?                        
         BZ    EQVAL15             YES - UNRESOLVED EQUATE                      
         EX    R1,EQCMP                                                         
         BE    EQVAL16                                                          
         BXH   R5,RF,EQVAL14                                                    
*                                                                               
EQCMP    CLC   ALL.LINNAME(0),0(R2)                                             
*                                                                               
EQVAL15  EX    R1,*+4                                                           
         MVC   0(0,R4),0(R2)       SAVE DESCRIPTION                             
         B     EQVL                                                             
*                                                                               
EQVAL16  CLI   ALL.LINTYPE,LINTDTA   DS?                                        
         BE    EQVAL18                                                          
         CLI   ALL.LINTYPE,LINTEQU   EQUATE?                                    
         BE    EQVAL22                                                          
         DC    H'0'                                                             
*                                                                               
EQVAL18  CLI   ESLEN,C'Y'          RESOLVE DS VALUES                            
         BE    EQVAL20                                                          
         TM    ALL.LINDFLAG,LINDFDSP                                            
         BZ    EQVL                DISPLACEMENT NOT RESOLVED YET                
         ICM   RF,3,ALL.LINDDSPN                                                
         STCM  RF,3,0(R3)                                                       
         B     EQVOK                                                            
*                                                                               
EQVAL20  TM    ALL.LINDFLAG,LINDFLEN                                            
         BZ    EQVL                LENGTH NOT RESOLVED YET                      
         ICM   RF,3,ALL.LINDLENN                                                
         STCM  RF,3,0(R3)                                                       
         B     EQVOK                                                            
*                                                                               
EQVAL22  TM    ALL.LINEFLAG,LINEFEQU                                            
         BZ    EQVL                EQUATE NOT RESOLVED YET                      
         ICM   RF,3,ALL.LINEEQUN                                                
         STCM  RF,3,0(R3)                                                       
         B     EQVOK                                                            
*                                                                               
EQVL     CLI   *,FF                SET CC LOW                                   
         B     EQVXX                                                            
EQVH     CLI   *,0                 SET CC HIGH                                  
         B     EQVXX                                                            
EQVOK    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EQVXX    LR    RF,R6               SET NEXT FREE ON LINE                        
         XIT1  REGS=(RF)                                                        
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO VALIDATE AN EQUATE STRING                               *          
*                                                                    *          
* NTRY P1: A(STRING)                                                 *          
*                                                                    *          
* EXIT EQVPTR = RESOLVED VALUE                                       *          
*      EQVSTR = STRING IF VALUE CANNOT BE RESOLVED                   *          
*                                                                    *          
* EXIT CC: EQ = EQUATE/NUMBER RESOLVED                               *          
* EXIT CC: LO = EQUATE/NUMBER NOT RESOLVED                           *          
**********************************************************************          
         SPACE 1                                                                
EQUSTR   NTR1  ,                                                                
         L     R3,0(R1)            R3 = A(START OF STRING)                      
*                                                                               
         MVI   ESSIGN,C'+'         DEFAULT SIGN                                 
         MVI   ESTYPEL,C'?'        NOTHING SET                                  
         MVI   ESMATCH,C'Y'        TYPES MATCH                                  
         MVI   ESBRACE,C'N'        NOT WITHIN BRACES                            
         MVC   ESSTR,SPACE                                                      
         XC    ESTHIS,ESTHIS                                                    
*                                                                               
         LR    RF,R3               LOOK FOR END OF STRING                       
         CLI   0(RF),C' '                                                       
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
*                                                                               
         SR    RF,R3               RF = LENGTH OF STRING                        
         BP    *+6                                                              
         DC    H'0'                WHERE IS THE STRING?                         
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ESSTR(0),0(R3)      SAVE STRING                                  
*                                                                               
EQS02    MVI   ESLEN,C'N'          RESET EQUATED LENGTH REQUEST                 
         MVI   ESTYPE,LINE_NUM     SET NUMERIC TYPE                             
         XR    R1,R1                                                            
         LR    R6,R3                                                            
*                                                                               
         CLI   0(R6),C'*'          STRING HAS DISPLACEMENT POINTER?             
         BNE   EQS04               NO                                           
*                                                                               
         LA    R6,1(R6)            GO PAST DISPLACEMENT POINTER                 
         L     RF,STAR             SET PURE DISPLACEMENT                        
         B     EQS30                                                            
*                                                                               
EQS04    CLC   =C'L''',0(R6)       WANT LENGTH OF EQUATE?                       
         BNE   EQS06               NO                                           
*                                                                               
         MVI   ESLEN,C'Y'                                                       
         LA    R3,2(R3)            GO PAST L'MODIFIER                           
         LA    R6,2(R6)                                                         
         B     EQS10               TRY TO MATCH EQUATED NAME                    
*                                                                               
EQS06    CLI   0(R6),C'0'          MATCHING NUMERIC MODIFIER?                   
         BL    EQS08                                                            
         CLI   0(R6),C'9'                                                       
         BH    EQS08                                                            
         LA    R1,1(R1)                                                         
         LA    R6,1(R6)                                                         
         B     EQS06                                                            
*                                                                               
EQS08    LTR   R1,R1               IS THIS PART NUMERIC?                        
         BZ    EQS10               NO                                           
*                                                                               
         BCTR  R1,0                PACK NUMERIC MODIFIER                        
         EX    R1,EQSPACK                                                       
         CVB   RF,DUB              SET MODIFIER IN RF                           
         B     EQS30               PROCESS MODIFIER                             
*                                                                               
EQSPACK  PACK  DUB,0(0,R3)                                                      
*                                                                               
EQS10    CLI   0(R6),C'('          EQUATE ENCLOSED IN BRACES                    
         BNE   EQS12                                                            
         MVI   ESBRACE,C'Y'                                                     
         LA    R3,1(R3)            GO PAST OPENING BRACE                        
         LA    R6,1(R6)                                                         
*                                                                               
EQS12    CLI   0(R6),C' '          END OF STRING                                
         BE    EQS14                                                            
         CLI   0(R6),C')'          CLOSING BRACE                                
         BE    EQS14                                                            
         CLI   0(R6),C'+'          ARITHMETIC MODIFIER?                         
         BE    EQS14                                                            
         CLI   0(R6),C'-'                                                       
         BE    EQS14                                                            
         LA    R1,1(R1)                                                         
         LA    R6,1(R6)                                                         
         B     EQS12                                                            
*                                                                               
EQS14    CLI   ESBRACE,C'Y'        INSIDE BRACES?                               
         BNE   EQS16               NO                                           
         CLI   0(R6),C')'          CLOSING BRACE?                               
         BNE   EQS16               NO                                           
         LA    R6,1(R6)            POINT PAST BRACE                             
         MVI   ESBRACE,C'N'                                                     
*                                                                               
EQS16    L     R5,AGENTAB          FIRST LINE OF TABLE                          
ALL      USING LINTABD,R5                                                       
         XR    RF,RF                                                            
         BCTR  R1,0                R1 HOLDS L' OF STRING TO COMPARE             
*                                                                               
EQS18    ICM   RF,1,ALL.LINLEN     REACHED END OF TABLE?                        
         BZ    EQSL                YES - UNRESOLVED STRING                      
         EX    R1,EQSCMP                                                        
         BE    EQS20                                                            
         BXH   R5,RF,EQS18                                                      
*                                                                               
EQSCMP   CLC   ALL.LINNAME(0),0(R3)                                             
*                                                                               
EQS20    CLI   ALL.LINTYPE,LINTREC   DSECT?                                     
         BE    EQS22                                                            
         CLI   ALL.LINTYPE,LINTDTA   DS?                                        
         BE    EQS24                                                            
         CLI   ALL.LINTYPE,LINTEQU   EQUATE?                                    
         BE    EQS28                                                            
         DC    H'0'                                                             
*                                                                               
EQS22    XR    RF,RF               ZERO LENGTH & DISPLACEMENT                   
         B     EQS30                                                            
*                                                                               
EQS24    CLI   ESLEN,C'Y'          RESOLVE DS LENGTH?                           
         BE    EQS26               YES                                          
         TM    ALL.LINDFLAG,LINDFDSP                                            
         BZ    EQSL                DISPLACEMENT NOT YET RESOLVED                
         ICM   RF,3,ALL.LINDDSPN                                                
         B     EQS30                                                            
*                                                                               
EQS26    TM    ALL.LINDFLAG,LINDFLEN                                            
         BZ    EQSL                LENGTH NOT RESOLVED YET                      
         ICM   RF,3,ALL.LINDLENN                                                
         B     EQS30                                                            
*                                                                               
EQS28    TM    ALL.LINEFLAG,LINEFEQU                                            
         BZ    EQSL                EQUATE NOT RESOLVED YET                      
         ICM   RF,3,ALL.LINEEQUN                                                
         MVC   ESTYPE,ALL.LINEEQU  SET DIFFERENT EQUATE TYPE                    
         B     EQS30                                                            
*                                                                               
EQS30    L     RE,ESTHIS           CURRENT EQUATED STRING VALUE                 
         CLI   ESSIGN,C'-'                                                      
         BNE   *+10                                                             
         SR    RE,RF               IF ESSIGN IS A MINUS                         
         B     *+6                                                              
         AR    RE,RF               IF ESSIGN IS A PLUS                          
         ST    RE,ESTHIS                                                        
*                                                                               
         CLI   ESTYPEL,C'?'        FIRST TIME?                                  
         BNE   *+10                NO                                           
         MVC   ESTYPEL,ESTYPE                                                   
         CLC   ESTYPEL,ESTYPE      TYPES STILL THE SAME                         
         BE    *+8                 YES                                          
         MVI   ESMATCH,C'N'                                                     
*                                                                               
         CLI   0(R6),C' '          LAST PART OF STRING                          
         BE    EQSOK                                                            
         MVC   ESSIGN,0(R6)                                                     
         LA    R3,1(R6)            NEXT START POSITION                          
         CLI   ESSIGN,C'-'                                                      
         BE    EQS02                                                            
         CLI   ESSIGN,C'+'                                                      
         BE    EQS02                                                            
         CLI   ESSIGN,C'('                                                      
         BE    EQS02                                                            
         DC    H'0'                WHAT ARE WE LOOKING AT?                      
*                                                                               
EQSL     LR    RF,R6               LOOK FOR END OF STRING ON LINE               
         CLI   0(RF),C' '                                                       
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         CLI   *,FF                SET CC LOW                                   
         B     EQSX                                                             
*                                                                               
EQSOK    MVC   ESSTR,SPACE         RESET STRING                                 
         LR    RF,R6                                                            
         CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EQSX     XIT1  REGS=(RF)           (RF) = NEXT FREE ON LINE                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PARSE TABLE AND BUILD RECORDS FOR PUTTING TO SORT                   *         
***********************************************************************         
         SPACE 1                                                                
         DROP  RA,RB,RC                                                         
         DS    0H                                                               
SCNDPASS CSECT                                                                  
         NMOD1 0,FRSTPASS,RA                                                    
         LR    RC,R1                                                            
         USING WRKD,RC                                                          
*                                                                               
         GOTOX VSORTER,PLIST,SORTCARD,RECCARD,0                                 
*                                                                               
         L     R2,AGENTAB                                                       
         USING LINTABD,R2                                                       
         LA    R3,SORBLK                                                        
         USING SRTRECD,R3                                                       
*                                                                               
SPTB02   CLI   LINLEN,0            END OF TABLE?                                
         BE    SPTBX                                                            
         CLC   LINNAME,SPACE       IGNORE THOSE WITH NO NAMES                   
         BNH   SPTB22                                                           
         CLI   LINTYPE,LINTREC     IGNORE DSECTS                                
         BE    SPTB12                                                           
         CLI   LINTYPE,LINTDTA                                                  
         BE    SPTB04                                                           
         CLI   LINTYPE,LINTEQU                                                  
         BE    SPTB10                                                           
*                                                                               
SPTB04   TM    LINDFLAG,(LINDFMLT+LINDFLEN+LINDFDSP)                            
         BO    SPTB12              RESOLVED EVERYTHING                          
*                                                                               
         TM    LINDFLAG,LINDFMLT   RESOLVED MULTIPLIER?                         
         BO    SPTB06                                                           
         GOTOX E2RSLV,PLIST,LINDMLTE,AGENTAB,0,0                                
         OI    LINDFLAG,LINDFMLT   RESOLVED THIS VALUE                          
         L     RF,12(R1)                                                        
         STCM  RF,3,LINDMLTN                                                    
         XC    LINDMLTE,LINDMLTE                                                
*                                                                               
SPTB06   TM    LINDFLAG,LINDFDSP   RESOLVED MULTIPLIER?                         
         BO    SPTB08                                                           
         GOTOX E2RSLV,PLIST,LINDDSPE,AGENTAB,0,0                                
         OI    LINDFLAG,LINDFDSP   RESOLVED THIS VALUE                          
         L     RF,12(R1)                                                        
         STCM  RF,3,LINDDSPN                                                    
         XC    LINDDSPE,LINDDSPE                                                
*                                                                               
SPTB08   TM    LINDFLAG,LINDFLEN   RESOLVED LENGTH?                             
         BO    SPTB12                                                           
         GOTOX E2RSLV,PLIST,LINDLENE,AGENTAB,0,0                                
         OI    LINDFLAG,LINDFLEN   RESOLVED THIS VALUE                          
         L     RF,12(R1)                                                        
         STCM  RF,3,LINDLENN                                                    
         XC    LINDLENE,LINDLENE                                                
         B     SPTB12                                                           
*                                                                               
SPTB10   TM    LINEFLAG,LINEFEQU   RESOLVED EQUATE?                             
         BO    SPTB11              YES                                          
         GOTOX E2RSLV,PLIST,LINEEQUE,AGENTAB,0,0                                
*                                                                               
         OI    LINEFLAG,LINEFEQU   RESOLVED THIS VALUE                          
         MVI   LINEEQU,LINE_NUM    DEFAULT TYPE                                 
         ICM   RF,15,8(R1)         DIFFERENT TYPE ALLOCATED?                    
         BZ    *+8                 NO                                           
         STC   RF,LINEEQU                                                       
*                                                                               
         L     RF,12(R1)           NUMERIC VALUE OF EQUATE                      
         STCM  RF,3,LINEEQUN                                                    
         MVI   LINEEQUL,1          DEFAULT LENGTH IS 1                          
         CLI   LINEEQUN,0          LONGER THAN 1?                               
         BNE   *+8                 NO                                           
         MVI   LINEEQUL,2                                                       
*                                                                               
         MVC   LINEEQUE,SPACE      RESET EQUATE VALUE                           
         B     SPTB12                                                           
*                                                                               
SPTB11   CLC   LINEDNOE,SPACE      RESOLVED ASSOCIATED DS?                      
         BNH   SPTB12              YES                                          
*                                                                               
         L     R5,AGENTAB          FIRST LINE OF TABLE                          
ALL      USING LINTABD,R5                                                       
         XR    RF,RF                                                            
*                                                                               
FPMAX02  ICM   RF,1,ALL.LINLEN     REACHED END OF TABLE?                        
         BNZ   *+6                                                              
         DC    H'0'                UNRESOLVED EQUATE                            
         CLI   ALL.LINTYPE,LINTDTA                                              
         BNE   *+14                                                             
         CLC   LINEDNOE,ALL.LINNAME  MATCH NAME?                                
         BE    FPMAX06                                                          
         BXH   R5,RF,FPMAX02                                                    
*                                                                               
FPMAX06  MVC   LINEDNOE,SPACE                                                   
         MVC   LINERNUM,ALL.LINDRNUM  SET RECORD NUMBER                         
         MVC   LINEDNUM,ALL.LINDDNUM  SET DATA NUMBER                           
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,ALL.LINDEQNO   INCREMENT COUNT OF ASSOCIATED EQUS           
         LA    R1,1(R1)                                                         
         STCM  R1,3,ALL.LINDEQNO   SAVE IT                                      
         STCM  R1,3,LINEENUM       SAVE THIS EQUATE NUMBER COUNT                
         DROP  ALL                                                              
*                                                                               
SPTB12   LR    R0,R3               CLEAR BLOCK TO SPACES                        
         LH    R1,=Y(SORBLKLQ)                                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACE                                                       
         MVCL  R0,RE                                                            
*                                                                               
         XC    SORLEN,SORLEN                                                    
         XC    SORND,SORND                                                      
*                                                                               
         LA    RF,PROC2TAB         TYPES TO PROCESS                             
         USING PRO2TABD,RF                                                      
SPTB14   CLI   PRO2CMP,FF          END OF TABLE?                                
         BE    SPTB18                                                           
         CLC   PRO2CMP,LINTYPE                                                  
         BE    SPTB16                                                           
         LA    RF,PRO2LNQ(RF)                                                   
         B     SPTB14                                                           
*                                                                               
SPTB16   ICM   RF,15,PRO2ADDR      A(SECOND PASS LINE HANDLER)                  
         BASR  RE,RF                                                            
         B     SPTB20              PUT TO SORT AND GO TO NEXT LINE              
         DROP  RF                                                               
*                                                                               
SPTB18   DC    H'0'                UNKNOWN EQUATE IN LINE                       
*                                                                               
SPTB20   CLC   SORLEN,=AL2(200)                                                 
         BL    *+6                                                              
         DC    H'0'                                                             
         CLI   SORLEN+200,C' '                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOX VSORTER,PLIST,PUT,SORLEN                                         
*                                                                               
SPTB22   XR    RF,RF                                                            
         IC    RF,LINLEN                                                        
         LA    R2,0(RF,R2)                                                      
         B     SPTB02                                                           
*                                                                               
SPTBX    B     SPASSOK                                                          
*                                                                               
SPASSL   CLI   *,FF                                                             
         B     SPASSX                                                           
SPASSH   CLI   *,0                                                              
         B     SPASSX                                                           
SPASSOK  CR    RB,RB                                                            
SPASSX   XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(4,10,A),FORMAT=BI,WORK=1 '                     
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(500,,,,) '                            
*                                                                               
PROC2TAB DC    AL1(LINTREC)        TABLE TYPES TO PROCESS (PASS 2)              
         DC    AL4(SPDSECT)                                                     
         DC    AL1(LINTDTA)                                                     
         DC    AL4(SPDS)                                                        
         DC    AL1(LINTEQU)                                                     
         DC    AL4(SPEQU)                                                       
         DC    AL1(LINTORG)                                                     
         DC    AL4(SPORG)                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS TABLE ENTRY FOR RECORD HEADER                               *         
***********************************************************************         
         SPACE 1                                                                
SPDSECT  NTR1  ,                                                                
         MVC   SORLEN,=AL2(SRTLENQ+4)                                           
         MVI   SRTTYPE,SRTTREC     SET RECORD TYPE                              
         MVC   SRTRNUM,LINRRNUM    SET RECORD NUMBER                            
         XC    SRTDNUM,SRTDNUM                                                  
         XC    SRTENUM,SRTENUM                                                  
         MVC   SRTNAME,LINNAME                                                  
         MVC   SRTKYEL,LINEKYEL                                                 
         MVC   SRTDESC,LINRDESC                                                 
         MVC   SRTRDSN,LINRDSNO                                                 
         MVC   SRTRID,LINRID                                                    
         XC    SRTDCNT,SRTDCNT                                                  
         XC    SRTDID,SRTDID                                                    
         B     SPASSOK                                                          
         SPACE 2                                                                
***********************************************************************         
* PROCESS TABLE ENTRY FOR DATA LINE                                   *         
***********************************************************************         
         SPACE 1                                                                
SPDS     NTR1  ,                                                                
         TM    LINDFLAG,(LINDFDSP+LINDFMLT+LINDFLEN)                            
         BO    *+6                                                              
         DC    H'0'                UNRESOLVED EQUATED VALUE                     
*                                                                               
         MVC   SORLEN,=AL2(SRTLENQ+4)                                           
         MVI   SRTTYPE,SRTTDTA     SET TYPE                                     
         MVC   SRTRNUM,LINDRNUM    SET RECORD NUMBER                            
         MVC   SRTDNUM,LINDDNUM    SET DS NUMBER                                
         XC    SRTENUM,SRTENUM                                                  
         MVC   SRTNAME,LINNAME     SET DS NAME                                  
         MVC   SRTKYEL,LINEKYEL    SET DSECT TYPE                               
         MVC   SRTDESC,LINDDESC    SET DS DESCRIPTION                           
         XC    SRTRDSN,SRTRDSN                                                  
         XC    SRTRID,SRTRID                                                    
         MVC   SRTDCNT,LINDEQNO                                                 
         MVC   SRTDID,LINDID                                                    
         MVC   SRTDTYPE,LINDDTYP   SET DS TYPE                                  
         MVC   SRTDSPN,LINDDSPN    SET DS START DISPLACEMENT                    
         MVC   SRTMLTN,LINDMLTN    SET DS MULTIPLIER                            
         MVC   SRTLENN,LINDLENN    SET DS LENGTH                                
         B     SPASSOK                                                          
         SPACE 2                                                                
***********************************************************************         
* PROCESS TABLE ENTRY FOR EQUATED VALUE                               *         
***********************************************************************         
         SPACE 1                                                                
SPEQU    NTR1  ,                                                                
         TM    LINEFLAG,(LINEFEQU)                                              
         BO    *+6                                                              
         DC    H'0'                UNRESOLVED EQUATED VALUE                     
*                                                                               
         L     R5,AGENTAB          FIRST LINE OF TABLE                          
ALL      USING LINTABD,R5                                                       
         XR    RF,RF                                                            
*                                                                               
SPEQU02  ICM   RF,1,ALL.LINLEN     REACHED END OF TABLE?                        
         BZ    SPEQU08             YES                                          
         CLI   ALL.LINTYPE,LINTDTA                                              
         BNE   SPEQU04                                                          
         CLC   ALL.LINDRNUM,LINDRNUM                                            
         BNE   SPEQU04                                                          
         CLC   ALL.LINDDNUM,LINDDNUM                                            
         BE    SPEQU06                                                          
*                                                                               
SPEQU04  BXH   R5,RF,SPEQU02                                                    
*                                                                               
SPEQU06  TM    ALL.LINDFLAG,LINDFID  DS IS IDENTIFIER FOR THIS DSECT            
         BZ    SPEQU08                                                          
         OI    LINEFLAG,LINEFID    SET EQUATE IS IDENTIFIER                     
         MVC   SORLEN,=AL2(SRTLENQ+4)                                           
         MVI   SRTTYPE,SRTTIDNT    SET TYPE                                     
         MVC   SRTRNUM,LINERNUM    SET RECORD NUMBER                            
         MVC   SRTDNUM,LINEDNUM    SET DS NUMBER                                
         MVC   SRTENUM,LINEENUM    SET EQUATE NUMBER                            
         MVC   SRTNAME,LINNAME     SET EQU NAME                                 
         MVC   SRTKYEL,LINEKYEL    SET DSECT TYPE                               
         MVC   SRTDESC,LINEDESC    SET EQU DESCRIPTION                          
         XC    SRTRDSN,SRTRDSN                                                  
         XC    SRTRID,SRTRID                                                    
         XC    SRTDCNT,SRTDCNT                                                  
         XC    SRTDID,SRTDID                                                    
         MVC   SRTEQUT,LINEEQU     SET EQUATE TYPE                              
         MVC   SRTEQUL,LINEEQUL    SET EQUATE LENGTH                            
         MVC   SRTEQUN,LINEEQUN    SET EQUATE VALUE                             
         GOTOX VSORTER,PLIST,PUT,SORLEN                                         
         DROP  ALL                                                              
*                                                                               
SPEQU08  MVC   SORLEN,=AL2(SRTLENQ+4)                                           
         MVI   SRTTYPE,SRTTEQU     SET TYPE                                     
         MVC   SRTRNUM,LINERNUM    SET RECORD NUMBER                            
         MVC   SRTDNUM,LINEDNUM    SET DS NUMBER                                
         MVC   SRTENUM,LINEENUM    SET EQUATE NUMBER                            
         MVC   SRTNAME,LINNAME     SET EQU NAME                                 
         MVC   SRTKYEL,LINEKYEL    SET DSECT TYPE                               
         MVC   SRTDESC,LINEDESC    SET EQU DESCRIPTION                          
         MVC   SRTEQUT,LINEEQU     SET EQUATE TYPE                              
         XC    SRTRDSN,SRTRDSN                                                  
         XC    SRTRID,SRTRID                                                    
         XC    SRTDCNT,SRTDCNT                                                  
         XC    SRTDID,SRTDID                                                    
         MVC   SRTEQUL,LINEEQUL    SET EQUATE LENGTH                            
         MVC   SRTEQUN,LINEEQUN    SET EQUATE VALUE                             
         B     SPASSOK                                                          
         SPACE 2                                                                
***********************************************************************         
* PROCESS TABLE ENTRY FOR POINTER REPOSITION                          *         
***********************************************************************         
         SPACE 1                                                                
SPORG    NTR1  ,                                                                
         B     SPASSOK                                                          
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO PARSE THE TABLE TO RESOLVE AN UNKNOWN EQUATE            *          
*                                                                    *          
* NTRY: P1: A(STRING TO RESOLVE)                                     *          
*       P2: A(TABLE OF EQUATED VALUES)                               *          
*                                                                    *          
* EXIT: P3: RESOLVED VALUE                                           *          
*       P4: RESOLVED TYPE                                            *          
**********************************************************************          
         SPACE 1                                                                
E2RSLV   NMOD1 E2WORKLQ,PASSTWO*,CLEAR=YES                                      
         USING E2WORKD,RC                                                       
*                                                                               
         ST    R1,E2CALLR1                                                      
         MVC   E2STRING,0(R1)                                                   
         MVC   E2TABLE,4(R1)                                                    
*                                                                               
         L     R3,E2STRING         R3=STRING TO RESOLVE                         
         MVI   E2BRACE,C'N'        NOT WITHIN BRACES                            
         MVI   E2SIGN,C'+'         DEFAULT SIGN                                 
         MVI   E2TYPEL,C'?'        NOTHING SET                                  
         MVI   E2MATCH,C'Y'        TYPES MATCH                                  
         XC    E2THIS,E2THIS                                                    
*                                                                               
E2STR02  MVI   E2LEN,C'N'          RESET EQUATED LENGTH REQUEST                 
         MVI   E2TYPE,LINE_NUM     SET NUMERIC TYPE                             
         XR    R1,R1                                                            
         LR    R6,R3                                                            
*                                                                               
         CLC   =C'L''',0(R6)       REQUESTED EQUATED LENGTH?                    
         BNE   E2STR04                                                          
         MVI   E2LEN,C'Y'                                                       
         LA    R6,2(R6)            BUMP PAST L'SYMBOL                           
         LA    R3,2(R3)                                                         
*                                                                               
E2STR04  CLI   0(R6),C'0'          SEE IF MODIFIER IS NUMERIC                   
         BL    E2STR06                                                          
         CLI   0(R6),C'9'                                                       
         BH    E2STR06                                                          
         LA    R1,1(R1)                                                         
         LA    R6,1(R6)                                                         
         B     E2STR04                                                          
*                                                                               
E2STR06  LTR   R1,R1               NUMERIC PORTION?                             
         BZ    E2STR08             NO                                           
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,E2PACK                                                        
         CVB   RF,E2DUB                                                         
         B     E2STR36                                                          
*                                                                               
E2PACK   PACK  E2DUB,0(0,R3)       PACK NUMERIC MODIFIER                        
*                                                                               
E2STR08  CLI   0(R6),C'('          EQUATE ENCLOSED WITHIN BRACES?               
         BNE   E2STR10                                                          
         MVI   E2BRACE,C'Y'                                                     
         LA    R3,1(R3)                                                         
         LA    R6,1(R6)                                                         
*                                                                               
E2STR10  CLI   0(R6),C' '          END OF EQUATE                                
         BNH   E2STR12                                                          
         CLI   0(R6),C'+'                                                       
         BE    E2STR12                                                          
         CLI   0(R6),C'-'                                                       
         BE    E2STR12                                                          
         CLI   0(R6),C')'                                                       
         BE    E2STR12                                                          
         LA    R6,1(R6)                                                         
         LA    R1,1(R1)                                                         
         B     E2STR10                                                          
*                                                                               
E2STR12  CLI   E2BRACE,C'Y'        WITHIN PAIR OF BRACES?                       
         BNE   E2STR14             NO                                           
         CLI   0(R6),C')'          CLOSING BRACE OF PAIR                        
         BNE   E2STR14             NO                                           
         MVI   E2BRACE,C'N'                                                     
         LA    R6,1(R6)            POINT PAST CLOSING BRACE                     
*                                                                               
E2STR14  L     R5,E2TABLE                                                       
ALL      USING LINTABD,R5                                                       
         XR    RF,RF                                                            
         BCTR  R1,0                                                             
*                                                                               
E2STR16  ICM   RF,1,ALL.LINLEN     LENGTH OF THIS ENTRY                         
         BZ    E2STR18                                                          
         EX    R1,E2CMP            MATCH?                                       
         BE    E2STR20                                                          
         BXH   R5,RF,E2STR16                                                    
*                                                                               
E2CMP    CLC   ALL.LINNAME(0),0(R3)                                             
*                                                                               
E2STR18  DC    H'0'                WHAT IS THIS?                                
*                                                                               
E2STR20  CLI   ALL.LINTYPE,LINTREC                                              
         BE    E2STR22                                                          
         CLI   ALL.LINTYPE,LINTDTA                                              
         BE    E2STR24                                                          
         CLI   ALL.LINTYPE,LINTEQU                                              
         BE    E2STR32                                                          
*                                                                               
E2STR22  XR    RF,RF               DSECT HAS ZERO LENGTH & DISPLACEMENT         
         B     E2STR36                                                          
*                                                                               
E2STR24  CLI   E2LEN,C'Y'          REQUEST LENGTH?                              
         BE    E2STR28             YES                                          
         TM    ALL.LINDFLAG,LINDFDSP                                            
         BZ    E2STR26                                                          
         ICM   RF,3,ALL.LINDDSPN                                                
         B     E2STR36                                                          
*                                                                               
E2STR26  GOTOX E2RSLV,E2PARM,ALL.LINDDSPE,E2TABLE,0,0                           
         L     RF,12(R1)                                                        
*                                                                               
         OI    ALL.LINDFLAG,LINDFDSP RESOLVED THIS VALUE                        
         STCM  RF,3,ALL.LINDDSPN                                                
         XC    ALL.LINDDSPE,ALL.LINDDSPE                                        
         B     E2STR36                                                          
*                                                                               
E2STR28  TM    ALL.LINDFLAG,LINDFLEN                                            
         BZ    E2STR30                                                          
         ICM   RF,3,ALL.LINDDSPN                                                
         B     E2STR36                                                          
*                                                                               
E2STR30  GOTOX E2RSLV,E2PARM,ALL.LINDLENE,E2TABLE,0,0                           
         L     RF,12(R1)                                                        
*                                                                               
         OI    ALL.LINDFLAG,LINDFLEN RESOLVED THIS VALUE                        
         STCM  RF,3,ALL.LINDLENN                                                
         XC    ALL.LINDLENE,ALL.LINDLENE                                        
         B     E2STR36                                                          
*                                                                               
E2STR32  TM    ALL.LINEFLAG,LINEFEQU                                            
         BZ    E2STR34                                                          
         ICM   RF,3,ALL.LINEEQUN                                                
         MVC   E2TYPE,ALL.LINEEQU                                               
         B     E2STR36                                                          
*                                                                               
E2STR34  GOTOX E2RSLV,E2PARM,ALL.LINEEQUE,E2TABLE,0,0                           
         MVI   ALL.LINEEQU,LINE_NUM                                             
         ICM   RF,15,8(R1)         DIFFERENT TYPE?                              
         BZ    *+12                                                             
         STC   RF,ALL.LINEEQU                                                   
         STC   RF,E2TYPE                                                        
*                                                                               
         OI    ALL.LINEFLAG,LINEFEQU RESOLVED THIS VALUE                        
         L     RF,12(R1)                                                        
         STCM  RF,3,ALL.LINEEQUN                                                
         XC    ALL.LINEEQUE,ALL.LINEEQUE                                        
         B     E2STR36                                                          
*                                                                               
E2STR36  L     RE,E2THIS           CURRENT EQUATE TOTAL                         
         CLI   E2SIGN,C'-'                                                      
         BNE   *+10                                                             
         SR    RE,RF                                                            
         B     *+6                                                              
         AR    RE,RF                                                            
         ST    RE,E2THIS                                                        
*                                                                               
         CLI   E2TYPEL,C'?'        FIRST TIME?                                  
         BNE   *+10                NO                                           
         MVC   E2TYPEL,E2TYPE                                                   
         CLC   E2TYPEL,E2TYPE      TYPES STILL THE SAME                         
         BE    *+8                 YES                                          
         MVI   E2MATCH,C'N'                                                     
*                                                                               
         CLI   0(R6),C' '          END OF EQUATE                                
         BNH   E2STR38                                                          
         MVC   E2SIGN,0(R6)                                                     
         LA    R3,1(R6)                                                         
         B     E2STR02                                                          
*                                                                               
E2STR38  L     R1,E2CALLR1                                                      
         LA    RF,LINE_NUM                                                      
         CLI   E2MATCH,C'Y'                                                     
         BNE   *+8                                                              
         ICM   RF,1,E2TYPE                                                      
         ST    RF,8(R1)                                                         
         L     RF,E2THIS                                                        
         ST    RF,12(R1)                                                        
         XMOD1 ,                                                                
         DROP  ALL                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP TABLES OUT TO DATASETS                                         *         
*                                                                     *         
* TABLES ARE OUTPUT TO THE DATASETS POINTED TO IN THE INPUT JCL AS    *         
* FOLLOWS:                                                            *         
* DSECT TABLE       -> DSCTFILE                                       *         
* DS TABLE          -> DSFILE                                         *         
* EQUATE TABLE      -> EQUFILE                                        *         
* IDENTIFIER TABLE  -> IDNTFILE                                       *         
* ACC FILE TABLE    -> FILEFILE                                       *         
*                                                                     *         
* OUTPUT FORMAT IS DELIMITED. DELIMITER CHARACTER IS IN DEFSEP        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
DUMPFILE CSECT                                                                  
         NMOD1 0,DUMPFILE,RA                                                    
         LR    RC,R1                                                            
         USING WRKD,RC                                                          
         BAS   RE,DUMPDSC          DSECT TABLE                                  
         BAS   RE,DUMPDS           DS TABLE                                     
         BAS   RE,DUMPEQU          EQUATE TABLE                                 
         BAS   RE,DUMPIDN          IDENTIFIER TABLE                             
         BAS   RE,DUMPFIL          FILE TABLE                                   
         XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP DSECT TABLE TO DATASET                                         *         
***********************************************************************         
         SPACE 1                                                                
DUMPDSC  NTR1  ,                                                                
         OPEN  (DSCTFILE,OUTPUT)                                                
         L     R3,ADSCTAB                                                       
         USING HEDTABD,R3                                                       
         LA    R4,HEDLENQ                                                       
         L     R5,ADSCLAST                                                      
         XC    SORND,SORND                                                      
*                                                                               
DDSC02   LA    R7,SORBLK           OUTPUT RECORD BUILT IN THIS BLOCK            
         LR    R0,R7                                                            
         LH    R1,=Y(SORBLKLQ)                                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACE                                                       
         MVCL  R0,RE               CLEAR OUTPUT LINE TO SPACES                  
*                                                                               
         XR    RF,RF               DSECT NUMBER                                 
         ICM   RF,3,HEDNUM                                                      
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         MVC   0(L'HEDNAME,R7),HEDNAME                                          
         LA    R7,L'HEDNAME(R7)    DSECT NAME                                   
         CLI   0(R7),C' '                                                       
         BH    *+10                                                             
         BCTR  R7,0                                                             
         B     *-10                                                             
         MVI   1(R7),DEFSEP                                                     
         LA    R7,2(R7)                                                         
*                                                                               
         XR    RF,RF               IDENTIFIER COUNT                             
         ICM   RF,3,HEDIDNT                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               NUMBER OF MEMBERS                            
         ICM   RF,3,HEDMEMBR                                                    
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         MVC   0(L'HEDDESC,R7),HEDDESC                                          
         LA    R7,L'HEDDESC(R7)    DESCRIPTION                                  
         CLI   0(R7),C' '                                                       
         BH    *+10                                                             
         BCTR  R7,0                                                             
         B     *-10                                                             
         MVI   1(R7),DEFSEP                                                     
         MVI   2(R7),DEFSEP        EXTRA SEPARATOR FOR END OF RECORD            
         LA    R7,3(R7)                                                         
*                                                                               
         LA    RF,SORLEN           SET LENGTH                                   
         SR    R7,RF                                                            
         STH   R7,SORLEN                                                        
*                                                                               
         PUT   DSCTFILE,SORLEN     WRITE RECORD TO FILE                         
         BXLE  R3,R4,DDSC02                                                     
         CLOSE DSCTFILE                                                         
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* DUMP DS FILE TO DATASET                                             *         
***********************************************************************         
         SPACE 1                                                                
DUMPDS   NTR1  ,                                                                
         OPEN  (DSFILE,OUTPUT)                                                  
         L     R3,ADSTAB                                                        
         USING DTATABD,R3                                                       
         LA    R4,DTALENQ                                                       
         L     R5,ADSLAST                                                       
         XC    SORND,SORND                                                      
*                                                                               
DDS02    OC    DTAMLTN,DTAMLTN     IGNORE ZERO MULTIPLIER DATA                  
         BZ    DDS04                                                            
         OC    DTALENN,DTALENN     IGNORE ZERO LENGTH DATA                      
         BZ    DDS04                                                            
*                                                                               
         LA    R7,SORBLK           START OF OUTPUT BLOCK                        
         LR    R0,R7               CLEAR BLOCK TO SPACES                        
         LH    R1,=Y(SORBLKLQ)                                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACE                                                       
         MVCL  R0,RE                                                            
*                                                                               
         XR    RF,RF               DS NUMBER                                    
         ICM   RF,3,DTADNUM                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               NO. OF DSECT ASSOCIATED WITH DS              
         ICM   RF,3,DTARNUM                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         MVC   0(L'DTANAME,R7),DTANAME                                          
         LA    R7,L'DTANAME(R7)    DATA NAME                                    
         CLI   0(R7),C' '                                                       
         BH    *+10                                                             
         BCTR  R7,0                                                             
         B     *-10                                                             
         MVI   1(R7),DEFSEP                                                     
         LA    R7,2(R7)                                                         
*                                                                               
         XR    RF,RF               IDENTIFIER NUMBER                            
         ICM   RF,3,DTAIDNT                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               NUMBER OF MEMBERS                            
         ICM   RF,3,DTAMEMBR                                                    
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         MVC   0(L'DTATYPE,R7),DTATYPE                                          
         MVI   L'DTATYPE(R7),DEFSEP DATA TYPE                                   
         LA    R7,L'DTATYPE+1(R7)                                               
*                                                                               
         XR    RF,RF               DISPLACEMENT TO DATA                         
         ICM   RF,3,DTADSPN                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               DATA MULTIPLIER                              
         ICM   RF,3,DTAMLTN                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               LENGTH OF DATA                               
         ICM   RF,3,DTALENN                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         MVC   0(L'DTADESC,R7),DTADESC                                          
         LA    R7,L'DTADESC(R7)    DESCRIPTION                                  
         CLI   0(R7),C' '                                                       
         BH    *+10                                                             
         BCTR  R7,0                                                             
         B     *-10                                                             
         MVI   1(R7),DEFSEP                                                     
         MVI   2(R7),DEFSEP                                                     
         LA    R7,3(R7)                                                         
*                                                                               
         LA    RF,SORLEN           SET LENGTH                                   
         SR    R7,RF                                                            
         STH   R7,SORLEN                                                        
*                                                                               
         PUT   DSFILE,SORLEN       WRITE RECORD TO FILE                         
DDS04    BXLE  R3,R4,DDS02                                                      
         CLOSE  DSFILE                                                          
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* DUMP EQUATE TABLE TO FILE                                           *         
***********************************************************************         
         SPACE 1                                                                
DUMPEQU  NTR1  ,                                                                
         OPEN  (EQUFILE,OUTPUT)                                                 
         L     R3,AEQUTAB                                                       
         USING EQUTABD,R3                                                       
         LA    R4,EQULENQ                                                       
         L     R5,AEQULAST                                                      
         XC    SORND,SORND                                                      
*                                                                               
DEQU02   LA    R7,SORBLK           START OF OUTPUT BLOCK                        
         LR    R0,R7               CLEAR BLOCK TO SPACES                        
         LH    R1,=Y(SORBLKLQ)                                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACE                                                       
         MVCL  R0,RE                                                            
*                                                                               
         XR    RF,RF               DS NUMBER                                    
         ICM   RF,3,EQUDNUM                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               EQUATE NUMBER                                
         ICM   RF,3,EQUENUM                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         MVC   0(L'EQUNAME,R7),EQUNAME                                          
         LA    R7,L'EQUNAME(R7)    EQUATE NAME                                  
         CLI   0(R7),C' '                                                       
         BH    *+10                                                             
         BCTR  R7,0                                                             
         B     *-10                                                             
         MVI   1(R7),DEFSEP                                                     
         LA    R7,2(R7)                                                         
*                                                                               
         MVC   0(L'EQUEQUT,R7),EQUEQUT                                          
         MVI   L'EQUEQUT(R7),DEFSEP   DATA TYPE                                 
         LA    R7,L'EQUEQUT+1(R7)                                               
*                                                                               
         XR    RF,RF               EQUATE LENGTH                                
         ICM   RF,1,EQUEQUL                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         CLI   EQUEQUT,LINE_NUM    IS THE EQUATE VALUE NUMERIC?                 
         BNE   DEQU04              NO                                           
         XR    RF,RF                                                            
         ICM   RF,3,EQUEQUN                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
         B     DEQU10                                                           
*                                                                               
DEQU04   CLI   EQUEQUT,LINE_CHR    IS THE EQUATE VALUE CHARACTER(S)?            
         BNE   DEQU06              NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,EQUEQUL        GET LENGTH                                   
         BNZ   *+6                                                              
         DC    H'0'                CANNOT HAVE ZERO LENGTH EQUATE               
*                                                                               
         CLM   RF,1,=AL1(2)        LENGTH 1 OR 2 ONLY ALLOWED                   
         BNH   *+6                                                              
         DC    H'0'                EQUATE TOO LONG                              
*                                                                               
         LA    RE,EQUEQUN                                                       
         CLM   RF,1,=AL1(1)        LENGTH 1?                                    
         BNE   *+8                 NO                                           
         LA    RE,EQUEQUN+1                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R7),0(RE)       MOVE IN CHARACTERS                           
         LA    R7,1(RF,R7)                                                      
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
         B     DEQU10                                                           
*                                                                               
DEQU06   CLI   EQUEQUT,LINE_HEX    IS THE EQUATE VALUE HEXADECIMAL?             
         BNE   DEQU08              NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,EQUEQUL        GET LENGTH                                   
         BNZ   *+6                                                              
         DC    H'0'                CANNOT HAVE ZERO LENGTH EQUATE               
*                                                                               
         CLM   RF,1,=AL1(2)        LENGTH 1 OR 2 ONLY ALLOWED                   
         BNH   *+6                                                              
         DC    H'0'                EQUATE TOO LONG                              
*                                                                               
         LA    R0,EQUEQUN                                                       
         CLM   RF,1,=AL1(1)        LENGTH 1?                                    
         BNE   *+8                 NO                                           
         LA    R0,EQUEQUN+1                                                     
*                                                                               
         GOTOX VHEXOUT,PLIST,(R0),(R7),(RF),0                                   
         ICM   RF,15,16(R1)        LENGTH RETURNED HERE                         
         BNZ   *+6                                                              
         DC    H'0'                INVALID HEX                                  
*                                                                               
         AR    R7,RF                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
         B     DEQU10                                                           
*                                                                               
DEQU08   DC    H'0'                INVALID EQUATE TYPE                          
*                                                                               
DEQU10   MVC   0(L'EQUDESC,R7),EQUDESC                                          
         LA    R7,L'EQUDESC(R7)    DESCRIPTION                                  
         CLI   0(R7),C' '                                                       
         BH    *+10                                                             
         BCTR  R7,0                                                             
         B     *-10                                                             
         MVI   1(R7),DEFSEP                                                     
         MVI   2(R7),DEFSEP                                                     
         LA    R7,3(R7)                                                         
*                                                                               
         LA    RF,SORLEN           SET LENGTH                                   
         SR    R7,RF                                                            
         STH   R7,SORLEN                                                        
*                                                                               
         PUT   EQUFILE,SORLEN      WRITE RECORD TO FILE                         
         BXLE  R3,R4,DEQU02                                                     
         CLOSE  EQUFILE                                                         
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* DUMP IDENTIFIER TABLE TO FILE                                       *         
***********************************************************************         
         SPACE 1                                                                
DUMPIDN  NTR1  ,                                                                
         OPEN  (IDNTFILE,OUTPUT)                                                
         L     R3,AIDNTAB      *** IDENTIFER TABLE                              
         USING IDNTABD,R3                                                       
ID1      USING IDNIDD,IDNID1                                                    
ID2      USING IDNIDD,IDNID2                                                    
ID3      USING IDNIDD,IDNID3                                                    
*                                                                               
         LA    R4,IDNLENQ                                                       
         L     R5,AIDNLAST                                                      
         XC    SORLEN,SORLEN                                                    
         XC    SORND,SORND                                                      
*                                                                               
DIDN02   LA    R0,SORBLK           CLEAR BLOCK TO SPACES                        
         LH    R1,=Y(SORBLKLQ)                                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACE                                                       
         MVCL  R0,RE                                                            
*                                                                               
         LA    R7,SORBLK           START OF OUTPUT BLOCK                        
*                                                                               
         MVC   0(L'IDNNAME,R7),IDNNAME                                          
         LA    R7,L'IDNNAME(R7)                                                 
         CLI   0(R7),C' '                                                       
         BH    *+10                                                             
         BCTR  R7,0                                                             
         B     *-10                                                             
         LA    RF,SORBLK                                                        
         CR    R7,RF                                                            
         BH    *+6                                                              
         LR    R7,RF                                                            
*                                                                               
         MVI   1(R7),DEFSEP                                                     
         LA    R7,2(R7)                                                         
         XR    RF,RF               MAPPER NUMBER                                
         ICM   RF,3,IDNINUM                                                     
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               SPECIAL MATCH #1                             
         ICM   RF,1,IDNSPEC1                                                    
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               SPECIAL MATCH #2                             
         ICM   RF,1,IDNSPEC2                                                    
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               DATA NUMBER #1                               
         ICM   RF,3,ID1.IDIDNUM                                                 
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               EQUATE NUMBER #1                             
         ICM   RF,3,ID1.IDIENUM                                                 
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               DATA NUMBER #2                               
         ICM   RF,3,ID2.IDIDNUM                                                 
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               EQUATE NUMBER #2                             
         ICM   RF,3,ID2.IDIENUM                                                 
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               DATA NUMBER #3                               
         ICM   RF,3,ID3.IDIDNUM                                                 
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               EQUATE NUMBER #3                             
         ICM   RF,3,ID3.IDIENUM                                                 
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         MVI   0(R7),DEFSEP        END OF RECORD                                
         LA    R7,1(R7)                                                         
*                                                                               
         LA    RF,SORLEN           SET LENGTH                                   
         SR    R7,RF                                                            
         STH   R7,SORLEN                                                        
*                                                                               
         PUT   IDNTFILE,SORLEN     WRITE RECORD TO FILE                         
         BXLE  R3,R4,DIDN02                                                     
         CLOSE IDNTFILE                                                         
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* DUMP ACCOUNT FILE TO TABLE                                          *         
***********************************************************************         
         SPACE 1                                                                
DUMPFIL  NTR1  ,                                                                
         OPEN  (FILEFILE,OUTPUT)                                                
         L     R3,AFILTAB      *** FILE TABLE                                   
         USING FMTABD,R3                                                        
         LA    R4,FMLENQ                                                        
         L     R5,AFILLAST                                                      
*                                                                               
         XC    SORND,SORND                                                      
         XC    RCOUNT,RCOUNT                                                    
*                                                                               
DFIL02   LA    R6,FMIDN                                                         
         USING FMIDND,R6                                                        
         LA    R8,FMIDNN                                                        
*                                                                               
DFIL04   LA    R0,SORBLK           CLEAR BLOCK TO SPACES                        
         LH    R1,=Y(SORBLKLQ)                                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACE                                                       
         MVCL  R0,RE                                                            
*                                                                               
         LA    R7,SORBLK           START OF OUTPUT BLOCK                        
*                                                                               
         L     RF,RCOUNT           COUNTER USED AS PRIMARY KEY                  
         LA    RF,1(RF)                                                         
         ST    RF,RCOUNT                                                        
*                                                                               
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               RECTYPE NUMBER                               
         ICM   RF,1,FMTYPE                                                      
         EDIT  (RF),(4,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         MVC   0(L'FMUNIT,R7),FMUNIT UNIT                                       
         CLC   0(L'FMUNIT,R7),SPACE                                             
         BH    *+10                                                             
         MVC   0(3,R7),=AL3(NONSPEC)                                            
         LA    R7,3(R7)              FIELD IS 3 CHARS SPACE FILLED              
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         MVC   0(L'FMLGR,R7),FMLGR   LEDGER                                     
         CLC   0(L'FMLGR,R7),SPACE                                              
         BH    *+10                                                             
         MVC   0(3,R7),=AL3(NONSPEC)                                            
         LA    R7,3(R7)              FIELD IS 3 CHARS SPACE FILLED              
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               IDENTIFIER NUMBER                            
         ICM   RF,3,FMIDNNUM                                                    
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               MAXIMUM COUNT                                
         ICM   RF,1,FMIDNMAX                                                    
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
         LA    R7,1(R7)                                                         
*                                                                               
         XR    RF,RF               MINIMUM COUNT                                
         ICM   RF,1,FMIDNMIN                                                    
         EDIT  (RF),(7,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),DEFSEP                                                     
*                                                                               
         MVI   1(R7),DEFSEP        END OF RECORD                                
         LA    R7,1(R7)                                                         
*                                                                               
         LA    RF,SORLEN           SET LENGTH                                   
         SR    R7,RF                                                            
         STH   R7,SORLEN                                                        
         PUT   FILEFILE,SORLEN     WRITE RECORD TO FILE                         
*                                                                               
         LA    R6,FMIDNLQ(R6)      NEXT IDENTIFIER ON LINE                      
         OC    0(FMIDNLQ,R6),0(R6) ANY MORE?                                    
         BZ    DFIL06              NO                                           
         BCT   R8,DFIL04                                                        
*                                                                               
DFIL06   BXLE  R3,R4,DFIL02                                                     
         CLOSE FILEFILE                                                         
         XIT1  ,                                                                
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* OUTPUT FILE DCBS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
DSCTFILE DCB   DDNAME=DSCTFILE,DSORG=PS,MACRF=(PM),RECFM=VB,           *        
               BLKSIZE=8200,LRECL=256,BUFNO=2                                   
DSFILE   DCB   DDNAME=DSFILE,DSORG=PS,MACRF=(PM),RECFM=VB,             *        
               BLKSIZE=8200,LRECL=256,BUFNO=2                                   
EQUFILE  DCB   DDNAME=EQUFILE,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=256,BUFNO=2                                   
IDNTFILE DCB   DDNAME=IDNTFILE,DSORG=PS,MACRF=(PM),RECFM=VB,           *        
               BLKSIZE=8200,LRECL=256,BUFNO=2                                   
FILEFILE DCB   DDNAME=FILEFILE,DSORG=PS,MACRF=(PM),RECFM=VB,           *        
               BLKSIZE=8200,LRECL=256,BUFNO=2                                   
         EJECT                                                                  
***********************************************************************         
* PROCESS INPUT DATA FILE & CREATE MAPPING TABLE                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
PROCFILE CSECT                                                                  
         NMOD1 0,PROCFILE,RA                                                    
         LR    RC,R1                                                            
         USING WRKD,RC                                                          
         PUSH  USING                                                            
         L     R5,VCPRINT                                                       
         USING DPRINT,R5                                                        
         USING SRTFILED,SORBLK                                                  
*                                                                               
         BAS   RE,TSARELS          BUILD TSAR RECORDS                           
*                                                                               
         L     R1,=A(FMMAXN)       NUMBER OF IDENTIFIERS                        
         MH    R1,=Y(FMLENQ)                                                    
         GOTOX AGETMAIN                                                         
         ST    R0,LFILTAB          R0 = LENGTH OF MAPPING TABLE                 
         ST    R1,AFILTAB          R1 = A(MAPPING TABLE AREA)                   
         ST    R1,AFILCUR                                                       
         ST    R1,AFILLAST                                                      
*                                                                               
         GOTOX VSORTER,PLIST,SORTFILE,RECFILE,0                                 
         OPEN  (ACCFILE,INPUT)                                                  
         MVC   P(L'FILE1),FILE1                                                 
         GOTOX VPRINTER                                                         
*                                                                               
PRGET02  GET   ACCFILE,IOL                                                      
*                                                                               
         XR    RF,RF               CLEAR SORT BLOCK TO BINARY 0                 
         XR    RE,RE                                                            
         LH    R1,=Y(SORBLKLQ)                                                  
         LA    R0,SORBLK                                                        
         MVCL  R0,RE                                                            
*                                                                               
         GOTOX VRECTYP,PLIST,(C'D',IO)                                          
         MVC   SFTYPE,0(R1)        SET RECORD TYPE                              
*                                                                               
         L     RF,IOL              LENGTH OF FILE RECORD                        
         SRL   RF,16                                                            
         LA    RF,L'SFTYPE(RF)     ADD LENGTH OF RECORD TYPE                    
         STCM  RF,3,SORLEN                                                      
*                                                                               
         BCTR  RF,0                LENGTH OF FILE RECORD                        
         LR    R1,RF                                                            
         LA    R0,SFREC            MOVE FILE RECORD TO SORT BLOCK               
         LA    RE,IO                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTOX VSORTER,PLIST,PUT,SORLEN                                         
         B     PRGET02                                                          
*                                                                               
PRGET04  CLOSE (ACCFILE)                                                        
         MVC   P(L'FILE2),FILE2                                                 
         GOTOX VPRINTER                                                         
*                                                                               
PRGET06  GOTOX VSORTER,PLIST,GET   GET NEXT SORT RECORD                         
         ICM   R2,15,4(R1)                                                      
         BZ    PRGETX                                                           
*                                                                               
         XR    RF,RF               CLEAR RECORD BUFFER TO X'00'                 
         XR    RE,RE                                                            
         LH    R1,=Y(SORBLKLQ)                                                  
         LA    R0,SORBLK                                                        
         MVCL  R0,RE                                                            
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,0(R2)          GET LENGTH OF SORT RECORD                    
         LR    R1,RF                                                            
         LR    RE,R2                                                            
         LA    R0,SORLEN                                                        
         MVCL  R0,RE                                                            
                                                                                
         LA    R2,SORBLK           PROCESS SORTED ACCOUNT RECORD                
         GOTOX ACCREC,PLIST,(R2)                                                
         B     PRGET06                                                          
*                                                                               
PRGETX   GOTOX VSORTER,PLIST,END   FINISHED WITH SORT                           
         MVC   P(L'FILE3),FILE3                                                 
         GOTOX VPRINTER                                                         
*                                                                               
         L     RF,AFILCUR          ADD LAST ACRECTYPE TO TABLE                  
         MVC   0(FMLENQ,RF),FMBLD                                               
         ST    RF,AFILLAST                                                      
*                                                                               
         XMOD1                                                                  
         DROP  R5                                                               
*                                                                               
FILE1    DC    CL40'ACCOUNT FILE SORT BEGUN'                                    
FILE2    DC    CL40'ACCOUNT FILE SORT COMPLETED'                                
FILE3    DC    CL40'ACCOUNT RECORD PROCESS COMPLETED'                           
*                                                                               
         POP   USING                                                            
         SPACE 2                                                                
**********************************************************************          
* PROCFILE EXIT POINTS                                               *          
**********************************************************************          
         SPACE 1                                                                
PROCXL   CLI   *,FF                SET CC LOW                                   
         B     PROCX                                                            
*                                                                               
PROCXH   CLI   *,0                 SET CC HIGH                                  
         B     PROCX                                                            
*                                                                               
PROCXOK  CR    RB,RB               SET CC EQUAL                                 
*                                                                               
PROCX    XIT1  ,                                                                
         SPACE 2                                                                
***********************************************************************         
* BUILD TSAR SORT RECORDS FOR ELEMENTS                                *         
***********************************************************************         
         USING TSARD,TLDATA                                                     
         USING TLSTD,TLREC                                                      
         SPACE 1                                                                
TSARELS  NTR1  ,                                                                
         L     R1,CIDNTAB          GRAB STORAGE FOR TSAR BUFFER                 
         MH    R1,=Y(TLLENQ)                                                    
         SRL   R1,12                                                            
         LA    R1,1(R1)            ROUND UP TO NEXT 4K                          
         LR    R0,R1                                                            
         SLL   R0,12                                                            
*                                                                               
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                GETMAIN FAILED RC IN (RF)                    
                                                                                
         STCM  R1,15,TSABUF        SET UP A(TSAR BUFFER)                        
         STCM  R0,7,TSAREC+1       SET LENGTH OF BUFFER                         
         MVI   TSKEYL,TLKEYLQ      SET LENGTH OF KEY                            
         MVC   TSRECL,=AL2(TLLENQ) SET LENGTH OF RECORD                         
         MVI   TSOFFACT,TSAINI                                                  
         GOTOX ATSAR,TSARD         INITIALISE TSAR                              
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                INITIALISE ERROR                             
*                                                                               
         L     R3,AIDNTAB          BUILD TSAR RECORDS FOR ELEMENTS              
         USING IDNTABD,R3                                                       
         LA    R4,IDNLENQ                                                       
         L     R5,AIDNLAST                                                      
*                                                                               
TSEL02   CLI   IDNTYPE,IDNTKEY     KEY?                                         
         BE    TSEL04              YES                                          
*                                                                               
         XC    TLREC,TLREC                                                      
         MVC   TLIDN,IDNINUM       SET IDENTIFER NUMBER                         
*                                                                               
         GOTOX TBLD,PLIST,(R3)                                                  
         BL    TSEL04                                                           
*                                                                               
         LA    RF,TLREC                                                         
         ST    RF,TSAREC                                                        
         MVI   TSOFFACT,TSAADD                                                  
         GOTOX ATSAR,TSARD         ADD RECORD TO TSAR                           
         CLI   TSERRS,0                                                         
         BE    TSEL04                                                           
         TM    TSERRS,TSEDUP       DUPLICATE KEY?                               
         BO    TSEL04              IGNORE FOR NOW                               
         DC    H'0'                                                             
*                                                                               
TSEL04   BXLE  R3,R4,TSEL02                                                     
         B     PROCXOK                                                          
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* BUILD TSAR RECORD TO ADD TO BUFFER                                  *         
***********************************************************************         
TBLD     NTR1  ,                                                                
         L     R3,0(R1)                                                         
         USING IDNTABD,R3                                                       
*                                                                               
         OC    IDNID1,IDNID1       FIRST IDENTIFER?                             
         BZ    PROCXL              NO - IGNORE RECORD                           
         LA    R6,IDNID1                                                        
         USING IDNIDD,R6                                                        
*                                                                               
         LA    RF,L'IDIRNUM+L'IDIDNUM                                           
         GOTOX BSRCH,PLIST,ADSTAB,CDSTAB,DTALENQ,0,(RF),IDIRNUM                 
         LR    R4,RF               DS TABLE ENTRY                               
         USING DTATABD,R4                                                       
*                                                                               
         MVC   TLLDSP1,DTADSPN                                                  
*                                                                               
         LA    RF,L'IDIRNUM+L'IDIDNUM+L'IDIENUM                                 
         GOTOX BSRCH,PLIST,AEQUTAB,CEQUTAB,EQULENQ,0,(RF),IDIRNUM               
         LR    R5,RF               EQUATE TABLE ENTRY                           
         USING EQUTABD,R5                                                       
*                                                                               
         MVC   TLLEN1,EQUEQUL      EQUATE LENGTH                                
         MVC   TLELEQU1,EQUEQUN    EQUATE VALUE                                 
*                                                                               
         OC    IDNID2,IDNID2       SECOND IDENTIFIER?                           
         BZ    PROCXOK             NO                                           
         LA    R6,IDNID2                                                        
         USING IDNIDD,R6                                                        
*                                                                               
         LA    RF,L'IDIRNUM+L'IDIDNUM                                           
         GOTOX BSRCH,PLIST,ADSTAB,CDSTAB,DTALENQ,0,(RF),IDIRNUM                 
         LR    R4,RF               DS TABLE ENTRY                               
         USING DTATABD,R4                                                       
*                                                                               
         MVC   TLLDSP2,DTADSPN                                                  
*                                                                               
         LA    RF,L'IDIRNUM+L'IDIDNUM+L'IDIENUM                                 
         GOTOX BSRCH,PLIST,AEQUTAB,CEQUTAB,EQULENQ,0,(RF),IDIRNUM               
         LR    R5,RF               EQUATE TABLE ENTRY                           
         USING EQUTABD,R5                                                       
*                                                                               
         MVC   TLLEN2,EQUEQUL      EQUATE LENGTH                                
         MVC   TLELEQU2,EQUEQUN    EQUATE VALUE                                 
*                                                                               
         OC    IDNID3,IDNID3       THIRD IDENTIFER?                             
         BZ    PROCXOK             NO                                           
         LA    R6,IDNID3                                                        
         USING IDNIDD,R6                                                        
*                                                                               
         LA    RF,L'IDIRNUM+L'IDIDNUM                                           
         GOTOX BSRCH,PLIST,ADSTAB,CDSTAB,DTALENQ,0,(RF),IDIRNUM                 
         LR    R4,RF               DS TABLE ENTRY                               
         USING DTATABD,R4                                                       
*                                                                               
         MVC   TLLDSP3,DTADSPN                                                  
*                                                                               
         LA    RF,L'IDIRNUM+L'IDIDNUM+L'IDIENUM                                 
         GOTOX BSRCH,PLIST,AEQUTAB,CEQUTAB,EQULENQ,0,(RF),IDIRNUM               
         LR    R5,RF               EQUATE TABLE ENTRY                           
         USING EQUTABD,R5                                                       
*                                                                               
         MVC   TLLEN3,EQUEQUL      EQUATE LENGTH                                
         MVC   TLELEQU3,EQUEQUN    EQUATE VALUE                                 
         B     PROCXOK             NO MORE IDENTIFERS                           
         DROP  R3,R4,R5,R6                                                      
         SPACE 2                                                                
***********************************************************************         
* PROCESS AN ACCOUNT FILE RECORD                                      *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING FMTABD,FMBLD                                                     
ACCREC   NTR1  ,                                                                
         L     R2,0(R1)            R2 = RECORD TO PROCESS                       
         USING SRTFILED,R2                                                      
         LA    R4,SFREC                                                         
         USING ACTRECD,R4                                                       
*                                                                               
         CLI   SFTYPE,ACRTHDRA     HEADER/TRAILER RECORDS                       
         BL    *+12                                                             
         CLI   SFTYPE,ACRTTRLB                                                  
         BNH   PROCXOK             YES - IGNORE                                 
*                                                                               
         CLI   SFTYPE,ACRTFEEC     OLD FEE RECORDS                              
         BL    *+12                                                             
         CLI   SFTYPE,ACRTFEEP                                                  
         BNH   PROCXOK             YES - IGNORE                                 
*                                                                               
         MVI   FILEFRST,C'N'       RESET FIRST FOR THIS TYPE FLAG               
         OC    LASTTYPE,LASTTYPE   FIRST TIME IN?                               
         BNZ   *+14                                                             
         MVC   LASTTYPE,SFTYPE     SET FIRST TYPE                               
         MVI   FILEFRST,C'Y'       SET FIRST FOR THIS TYPE FLAG                 
*                                                                               
         CLC   LASTTYPE,SFTYPE     STILL PROCESSING SAME TYPE?                  
         BNE   ACCR02              NO                                           
*                                                                               
         CLI   SFTYPE,ACRTOTHR     PART OF HEIRARCHY?                           
         BH    ACCR04              NO - PROCESS DIRECTLY                        
*                                                                               
         CLI   SFTYPE,ACRTCPY      COMPANY RECORD?                              
         BE    ACCR04              YES - PROCESS DIRECTLY                       
*                                                                               
         CLC   LASTUNIT,SPACE      UNIT SET?                                    
         BH    *+10                YES                                          
         MVC   LASTUNIT,ACTKUNT                                                 
         CLC   LASTLGR,SPACE       LEDGER SET?                                  
         BH    *+10                YES                                          
         MVC   LASTLGR,ACTKLDG                                                  
*                                                                               
         CLC   LASTUNIT,ACTKUNT    UNIT CHANGED?                                
         BNE   ACCR02              YES                                          
         CLC   LASTLGR,ACTKLDG     LEDGER CHANGED?                              
         BNE   ACCR02              YES                                          
         B     ACCR04                                                           
*                                                                               
ACCR02   L     RF,AFILCUR          ADD LAST ACRECTYPE TO TABLE                  
         MVC   0(FMLENQ,RF),FMTABD                                              
         ST    RF,AFILLAST                                                      
         LA    RF,FMLENQ(RF)                                                    
         ST    RF,AFILCUR                                                       
*                                                                               
         XC    FMTABD(FMLENQ),FMTABD                                            
         MVC   LASTTYPE,SFTYPE     SET THIS TYPE                                
         MVC   LASTUNIT,ACTKUNT                                                 
         MVC   LASTLGR,ACTKLDG                                                  
         MVI   FILEFRST,C'Y'       SET FIRST FOR THIS TYPE FLAG                 
*                                                                               
ACCR04   MVC   FMTYPE,SFTYPE       SET RECORD EQUATE FROM SORT RECORD           
         CLI   FMTYPE,ACRTOTHR     PART OF HEIRARCHY?                           
         BH    ACCR06              NO - NO UNIT/LEDGER THEN                     
*                                                                               
         MVC   FMUNIT,ACTKUNT                                                   
         MVC   FMLGR,ACTKLDG                                                    
*                                                                               
ACCR06   LA    R3,FMTABD                                                        
         GOTOX FNDKY,PLIST,(R4),(R3)                                            
         LA    R4,ACTRFST          FIRST ELEMENT ON RECORD                      
*                                                                               
ACCR08   CLI   0(R4),0             END OF RECORD?                               
         BE    ACCR10              YES                                          
*                                                                               
         GOTOX FNDEL,PLIST,(R4),(R3)                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,1(R4)            NEXT ELEMENT                                 
         AR    R4,RF                                                            
         B     ACCR08                                                           
         DROP  R4                                                               
*                                                                               
ACCR10   LA    RF,FMIDN            SET IDENTIFIER COUNT FOR RECORDS             
         USING FMIDND,RF                                                        
         LA    R0,FMIDNN           COUNT OF IDENTIFIERS POSSIBLE                
*                                                                               
ACCR12   CLC   FMIDNMAX,FMIDNREC   MORE THAN PREVIOUS MAX COUNT?                
         BH    *+10                NO                                           
         MVC   FMIDNMAX,FMIDNREC                                                
*                                                                               
         CLI   FILEFRST,C'Y'       FIRST RECORD OF THIS TYPE?                   
         BNE   *+10                                                             
         MVC   FMIDNMIN,FMIDNREC   SET MINIMUM TO NUMBER FOR FIRST TIME         
*                                                                               
         CLC   FMIDNMIN,FMIDNREC   FEWER THAN PREVIOUS MIN COUNT?               
         BL    *+10                NO                                           
         MVC   FMIDNMIN,FMIDNREC                                                
*                                                                               
         XC    FMIDNREC,FMIDNREC   RESET CURRENT COUNT                          
         LA    RF,FMIDNLQ(RF)      NEXT IDENTIFIER                              
         BCT   R0,ACCR12                                                        
         B     PROCXOK                                                          
         DROP  RF                                                               
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* PROCESS SINGLE RECORD AND ADD TO CURRENT FMTABD ENTRY               *         
***********************************************************************         
         SPACE 1                                                                
FNDKY    NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING FMTABD,R3                                                        
*                                                                               
         L     R6,AIDNTAB          ROUTINE TO FIND IDENTIFIER                   
         USING IDNTABD,R6                                                       
         LA    R4,IDNLENQ                                                       
         L     R5,AIDNLAST                                                      
*                                                                               
FNDKY02  CLI   IDNTYPE,IDNTKEY     KEY TYPE?                                    
         BE    *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLC   IDNSPEC1,LASTTYPE   MATCH SPECIALS?                              
         BE    FNDKY06             YES                                          
         CLC   IDNSPEC2,LASTTYPE                                                
         BE    FNDKY06             YES                                          
*                                                                               
         OC    IDNID1,IDNID1       ANY IDENTIFERS?                              
         BZ    FNDKY04             NO                                           
*                                                                               
         GOTOX TRYMTCH,PLIST,(R2),(R6)                                          
         BE    FNDKY06             MATCHED IDENTIFIER                           
*                                                                               
FNDKY04  BXLE  R6,R4,FNDKY02                                                    
         DC    H'0'                NO MATCH FOR THIS KEY                        
*                                                                               
FNDKY06  LA    R0,FMIDNN                                                        
         LA    RF,FMIDN                                                         
         USING FMIDND,RF                                                        
*                                                                               
FNDKY08  OC    FMIDNNUM,FMIDNNUM   END OF KNOWN IDENTIFIERS?                    
         BZ    FNDKY10             YES                                          
         CLC   FMIDNNUM,IDNINUM    MATCH IDENTIFIER?                            
         BE    FNDKY10             YES                                          
         LA    RF,FMIDNLQ(RF)                                                   
         BCT   R0,FNDKY08                                                       
         DC    H'0'                                                             
*                                                                               
FNDKY10  MVC   FMIDNNUM,IDNINUM    SAVE IDENTIFIER NUMBER                       
         XR    R1,R1                                                            
         IC    R1,FMIDNREC         INCREMENT NUMBER FOUND                       
         LA    R1,1(R1)                                                         
         STC   R1,FMIDNREC                                                      
         B     PROCXOK                                                          
         DROP  RF,R6,R3                                                         
         SPACE 2                                                                
***********************************************************************         
* ATTEMPT TO MATCH SINGLE IDENTIFIER ENTRY TO CURRENT RECORD          *         
***********************************************************************         
         SPACE 1                                                                
TRYMTCH  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING IDNTABD,R3                                                       
*                                                                               
         LA    R6,IDNID1           FIRST IDENTIFIER                             
         USING IDNIDD,R6                                                        
*                                                                               
TRYM02   LA    RF,L'IDIRNUM+L'IDIDNUM                                           
         GOTOX BSRCH,PLIST,ADSTAB,CDSTAB,DTALENQ,0,(RF),IDIRNUM                 
*                                                                               
         LR    R4,RF               DS TABLE ENTRY                               
         USING DTATABD,R4                                                       
*                                                                               
         LA    RF,L'IDIRNUM+L'IDIDNUM+L'IDIENUM                                 
         GOTOX BSRCH,PLIST,AEQUTAB,CEQUTAB,EQULENQ,0,(RF),IDIRNUM               
*                                                                               
         LR    R5,RF               EQUATE TABLE ENTRY                           
         USING EQUTABD,R5                                                       
         XR    RF,RF                                                            
         ICM   RF,3,DTADSPN                                                     
         AR    RF,R2               RF POINTS TO THE DS IDENTIFIER               
         XR    R1,R1                                                            
         ICM   R1,1,EQUEQUL        R1 = EQUATE LENGTH                           
         BZ    PROCXL                                                           
         LA    RE,EQUEQUN          RE = A(EQUATE VALUE)                         
         CLM   R1,1,=AL1(1)                                                     
         BNE   *+8                                                              
         LA    RE,1(RE)            EQUATE IS RIGHT ALIGNED                      
*                                                                               
         BCTR  R1,0                TRY TO MATCH THIS IDENTIFIER                 
         EX    R1,TRYCMP                                                        
         BNE   PROCXL                                                           
         B     TRYM04                                                           
*                                                                               
TRYCMP   CLC   0(0,RE),0(RF)                                                    
*                                                                               
TRYM04   LA    R6,IDILENQ(R6)      NEXT IDENTIFER SPACE                         
         LA    RF,IDNID3           LAST IDENTIFIER                              
         CR    RF,R6                                                            
         BL    PROCXOK             MATCHED ALL IDENTIFIERS                      
*                                                                               
         OC    IDIRNUM,IDIRNUM                                                  
         BZ    PROCXOK             NO MORE IDENTIFERS                           
         B     TRYM02                                                           
         DROP  R3,R4,R5,R6                                                      
         SPACE 2                                                                
***********************************************************************         
* PROCESS SINGLE ELEMENT AND ADD TO CURRENT FMTABD ENTRY              *         
***********************************************************************         
         SPACE 1                                                                
FNDEL    NTR1  ,                                                                
         LM    R2,R3,0(R1)         R2 HOLDS A(ELEMENT)                          
         USING FMTABD,R3                                                        
*                                                                               
         XR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         XC    TLREC,TLREC                                                      
         STCM  RF,3,TLELEQU1                                                    
         LA    RF,TLREC                                                         
         ST    RF,TSAREC                                                        
         MVI   TSOFFACT,TSARDH                                                  
         B     *+8                                                              
*                                                                               
FNDEL02  MVI   TSOFFACT,TSANXT                                                  
         GOTOX ATSAR,TSARD         GET RECORD FROM TSAR                         
         TM    TSERRS,TSEEOF                                                    
         BO    PROCXOK                                                          
*                                                                               
         XR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         CLM   RF,3,TLELEQU1       STILL FOR THIS IDENTIFER                     
         BNE   PROCXOK                                                          
*                                                                               
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         OC    TLELEQU1,TLELEQU1   TRY TO MATCH EQUATE #1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,TLLDSP1                                                     
         AR    RF,R2               RF POINTS TO THE DS IDENTIFIER               
         XR    R1,R1                                                            
         ICM   R1,1,TLLEN1         R1 = EQUATE LENGTH                           
         BZ    FNDEL02                                                          
         LA    RE,TLELEQU1         RE = A(EQUATE VALUE)                         
         CLM   R1,1,=AL1(1)                                                     
         BNE   *+8                                                              
         LA    RE,1(RE)            EQUATE IS RIGHT ALIGNED                      
*                                                                               
         BCTR  R1,0                TRY TO MATCH THIS IDENTIFIER                 
         EX    R1,FELCMP                                                        
         BNE   FNDEL02                                                          
*                                                                               
         OC    TLELEQU2,TLELEQU2   TRY TO MATCH EQUATE #2                       
         BZ    FNDEL04             NO EQUATE 2 - MATCHED ALL                    
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,TLLDSP2                                                     
         AR    RF,R2               RF POINTS TO THE DS IDENTIFIER               
         XR    R1,R1                                                            
         ICM   R1,1,TLLEN2         R1 = EQUATE LENGTH                           
         BZ    FNDEL02                                                          
         LA    RE,TLELEQU2         RE = A(EQUATE VALUE)                         
         CLM   R1,1,=AL1(1)                                                     
         BNE   *+8                                                              
         LA    RE,1(RE)            EQUATE IS RIGHT ALIGNED                      
*                                                                               
         BCTR  R1,0                TRY TO MATCH THIS IDENTIFIER                 
         EX    R1,FELCMP                                                        
         BNE   FNDEL02                                                          
*                                                                               
         OC    TLELEQU3,TLELEQU3   TRY TO MATCH EQUATE #3                       
         BZ    FNDEL04             NO EQUATE 3 - MATCHED ALL                    
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,TLLDSP3                                                     
         AR    RF,R2               RF POINTS TO THE DS IDENTIFIER               
         XR    R1,R1                                                            
         ICM   R1,1,TLLEN3         R1 = EQUATE LENGTH                           
         BZ    FNDEL02                                                          
         LA    RE,TLELEQU3         RE = A(EQUATE VALUE)                         
         CLM   R1,1,=AL1(1)                                                     
         BNE   *+8                                                              
         LA    RE,1(RE)            EQUATE IS RIGHT ALIGNED                      
*                                                                               
         BCTR  R1,0                TRY TO MATCH THIS IDENTIFIER                 
         EX    R1,FELCMP                                                        
         BNE   FNDEL02                                                          
         B     FNDEL04                                                          
*                                                                               
FELCMP   CLC   0(0,RE),0(RF)                                                    
*                                                                               
FNDEL04  LA    R0,FMIDNN                                                        
         LA    RF,FMIDN                                                         
         USING FMIDND,RF                                                        
*                                                                               
FNDEL06  OC    FMIDNNUM,FMIDNNUM   END OF KNOWN IDENTIFIERS?                    
         BZ    FNDEL08             YES                                          
         CLC   FMIDNNUM,TLIDN      MATCH IDENTIFIER?                            
         BE    FNDEL08             YES                                          
         LA    RF,FMIDNLQ(RF)                                                   
         BCT   R0,FNDEL06                                                       
         DC    H'0'                                                             
*                                                                               
FNDEL08  MVC   FMIDNNUM,TLIDN      SAVE IDENTIFIER NUMBER                       
         XR    R1,R1                                                            
         IC    R1,FMIDNREC         INCREMENT NUMBER FOUND                       
         LA    R1,1(R1)                                                         
         STC   R1,FMIDNREC                                                      
         B     PROCXOK                                                          
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* BINARY SEARCH FOR REQUESTED TABLE ENTRY                             *         
*                                                                     *         
* NTRY: P1: A(FIRST RECORD IN TABLE)                            (R2)  *         
*       P2: NUMBER OF ENTRIES IN TABLE                          (R3)  *         
*       P3: L' SINGLE ENTRY IN TABLE                            (R4)  *         
*       P4: DISPLACEMENT FROM START OF RECORD OF KEY            (R5)  *         
*       P5: L' KEY                                              (R6)  *         
*       P6: KEY TO FIND                                         (R7)  *         
*                                                                     *         
* EXIT: RF: A(RECORD)          - CC:EQ                                *         
*       RF: A(INSERTION POINT) - CC:NEQ                               *         
***********************************************************************         
         SPACE 1                                                                
BSRCH    NTR1  ,                                                                
         LM    R2,R7,0(R1)                                                      
         BCTR  R6,0                                                             
         LTR   R6,R6               R6 = L'KEY-1                                 
         BNZ   *+6                                                              
         DC    H'0'                CANNOT MATCH ON ZERO LENGTH KEY              
*                                                                               
         XR    R1,R1               R1=LOW                                       
         BCTR  R3,0                R3=HIGH                                      
         LTR   R3,R3               HIGH=NUMBER OF RECORDS-1                     
         BNZ   *+6                                                              
         DC    H'0'                EMPTY TABLE                                  
*                                                                               
BSRCH02  CR    R1,R3               WHILE LOW<=HIGH                              
         BH    BSNO                R1 POINTS TO INSERTION POINT                 
*                                                                               
         LA    R8,0(R1,R3)         R8=MID                                       
         SRL   R8,1                MID=(HIGH+LOW)/2                             
         LR    RF,R8                                                            
         XR    RE,RE                                                            
         MR    RE,R4               R4=L' SINGLE ENTRY                           
         AR    RF,R2               R2=A(START OF TABLE)                         
         AR    RF,R5               R5=DISPLACEMENT TO START OF KEY              
*                                                                               
         EX    R6,BSMTCH           TRY TO MATCH KEY                             
         BE    BSYES               KEY == K(MID)                                
*                                                                               
         BH    BSRCH04             IF KEY < K(MID)                              
         LR    R3,R8               HIGH = MID-1;                                
         BCTR  R3,0                                                             
         B     BSRCH02                                                          
*                                                                               
BSRCH04  LA    R1,1(R8)            ELSE LOW = MID+1                             
         B     BSRCH02                                                          
*                                                                               
BSMTCH   CLC   0(0,R7),0(RF)       COMPARE REQUESTED KEY WITH RECORD            
*                                                                               
BSYES    SR    RF,R5               POINT RF TO START OF RECORD                  
         CR    RB,RB                                                            
         B     BSXIT                                                            
*                                                                               
BSNO     CLI   *,FF                                                             
*                                                                               
BSXIT    XIT1  REGS=(RF)                                                        
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                               *          
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
SORTFILE DC    CL80'SORT FIELDS=(4,43,A),FORMAT=BI,WORK=1 '                     
RECFILE  DC    CL80'RECORD TYPE=V,LENGTH=(2100,,,,) '                           
*                                                                               
ACCFILE  DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=PRGET04,        X        
               RECFM=VB,LRECL=4004,BUFNO=2                                      
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
**********************************************************************          
* EQUATES                                                            *          
**********************************************************************          
         SPACE 1                                                                
TBLRPT   EQU   1000                NUMBER OF 4K PAGES FOR GENTAB                
FMMAXN   EQU   3000                MAX IDENTIFIER COUNT FOR FMTABD              
*                                                                               
COMMENT  EQU   C'*'                FIRST CHARACTER OF A COMMENT CARD            
DEFSEP   EQU   C';'                DEFAULT SEPARATOR FOR OUTPUT                 
NONSPEC  EQU   C'N/S'              NON-SPECIFIED FOR FILE TABLE                 
*                                                                               
INPNMBR  EQU   72                  DISPLACEMENT TO LINE/LEVEL NUMBERS           
INPCOL1  EQU   0                   COLUMN 1 DISPLACEMENT                        
INPCOL2  EQU   9                   COLUMN 2 DISPLACEMENT                        
INPCOL3  EQU   15                  COLUMN 3 DISPLACEMENT                        
INPCOL4  EQU   35                  COLUMN 4 DISPLACEMENT                        
*                                                                               
FF       EQU   X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* ADDITIONAL CSECTS FOR W/S ETC                                      *          
**********************************************************************          
         SPACE 1                                                                
WRKCHN   CSECT                     WORKING STORAGE POOL                         
         DS    20000D                                                           
         SPACE 2                                                                
**********************************************************************          
* DSECTS                                                             *          
**********************************************************************          
         SPACE 1                                                                
WRKD     DSECT                 *** MAIN RC DSECT                                
DUB      DS    D                                                                
WORD     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
ADRLST   DS    0A              *** COMMON ADRESSES                              
VRECTYP  DS    A                                                                
VCARDS   DS    A                                                                
VCPRINT  DS    A                                                                
VHEXIN   DS    A                                                                
VHEXOUT  DS    A                                                                
VLOADER  DS    A                                                                
VPANIC   DS    A                                                                
VPRINT   DS    A                                                                
VPRINTER DS    A                                                                
VSORTER  DS    A                                                                
VSQUASH  DS    A                                                                
ADRLSTLQ EQU   *-ADRLST                                                         
*                                                                               
AGETMAIN DS    A                   A(GETMAIN CALL ROUTINE)                      
AFREMAIN DS    A                   A(FREEMAIN CALL ROUTINE)                     
ATSAR    DS    A                   A(TSAROFF ROUTINE)                           
ASKIPTAB DS    A                   A(PRINTER COMMANDS TABLE)                    
*                                                                               
PLIST    DS    XL24                                                             
*                                                                               
AGENTAB  DS    A                   A(GENFILE TABLE)                             
LGENTAB  DS    A                   L'GENFILE TABLE                              
AGENCUR  DS    A                   CURRENT LINE OF GENFILE TABLE                
ADSECT   DS    A                   A(CURRENT DSECT LINTAB ENTRY)                
ADS      DS    A                   A(CURRENT DS LINTAB ENTRY)                   
ADSOVER  DS    A                   A(OVERRIDE DS LINTAB ENTRY)                  
*                                                                               
RCOUNT   DS    F                   RECORD TOTAL                                 
DCOUNT   DS    F                   DATA SPACE TOTAL                             
ECOUNT   DS    F                   EQUATE TOTAL                                 
ICOUNT   DS    F                   IDENTIFIER TOTAL                             
STAR     DS    F                   CURRENT RECORD POINTER                       
STARHIGH DS    F                   HIGHEST POINT REACHED IN RECORD              
ESTHIS   DS    F                   VALUE WHILE RESOLVING STRING                 
*                                                                               
ADSCTAB  DS    A                   A(DSECT TABLE)                               
LDSCTAB  DS    A                   L'DSECT TABLE                                
CDSCTAB  DS    A                   MEMBER COUNT OF DSECT TABLE                  
ADSCCUR  DS    A                   A(CURRENT DSECT LINE)                        
ADSCLAST DS    A                   A(LAST DSECT LINE)                           
*                                                                               
ADSTAB   DS    A                   A(DS TABLE)                                  
LDSTAB   DS    A                   L'DS TABLE                                   
CDSTAB   DS    A                   MEMBER COUNT OF DS TABLE                     
ADSCUR   DS    A                   A(CURRENT DS LINE)                           
ADSLAST  DS    A                   A(LAST DS LINE)                              
*                                                                               
AEQUTAB  DS    A                   A(EQUATE TABLE)                              
LEQUTAB  DS    A                   L'EQUATE TABLE                               
CEQUTAB  DS    A                   MEMBER COUNT OF EQUATE TABLE                 
AEQULAST DS    A                   A(LAST EQUATE LINE)                          
AEQUCUR  DS    A                   A(CURRENT EQUATE LINE)                       
*                                                                               
AIDNTAB  DS    A                   A(IDENTIFIER TABLE)                          
LIDNTAB  DS    A                   L'IDENTIFIER TABLE                           
CIDNTAB  DS    A                   MEMBER COUNT OF IDENTIFIER TABLE             
AIDNLAST DS    A                   A(LAST IDENTIFIER LINE)                      
AIDNCUR  DS    A                   A(CURRENT IDENTIFIER LINE)                   
*                                                                               
AFILTAB  DS    A                   A(MAPPING TABLE)                             
LFILTAB  DS    A                   L'MAPPING TABLE                              
CFILTAB  DS    A                   MEMBER COUNT OF MAPPING TABLE                
AFILLAST DS    A                   A(LAST MAPING LINE)                          
AFILCUR  DS    A                   A(CURRENT MAPPING LINE)                      
*                                                                               
IO1      DS    A                                                                
IO2      DS    A                                                                
IO3      DS    A                                                                
IO4      DS    A                                                                
IDCOUNT  DS    H                   NUMBER OF IDENTIFIERS FOUND                  
IDREC    DS    H                   DSECT NUMBER FOR IDNTAB BUILD                
IDD1     DS    H                   DS NUMBER FOR IDNTAB #1                      
IDC1     DS    H                   CURRENT EQUATE COUNT #1                      
IDM1     DS    H                   MAXIMUM EQUATE COUNT #1                      
IDD2     DS    H                   DS NUMBER FOR IDNTAB #2                      
IDC2     DS    H                   CURRENT EQUATE COUNT #2                      
IDM2     DS    H                   MAXIMUM EQUATE COUNT #2                      
IDD3     DS    H                   DS NUMBER FOR IDNTAB #3                      
IDC3     DS    H                   CURRENT EQUATE COUNT #3                      
IDM3     DS    H                   MAXIMUM EQUATE COUNT #3                      
IDD4     DS    H                                                                
IDC4     DS    H                                                                
IDM4     DS    H                                                                
*                                                                               
GENFILE  DS    CL10                NAME OF ..GENFILE REQUESTED                  
RECTYPE  DS    CL10                NAME OF ..RECEQUS REQUESTED                  
*                                                                               
READ     DS    CL10                PANIC - READ                                 
CLOSE    DS    CL10                        CLOSE                                
PAN      DS    CL10                        PAN                                  
DIR      DS    CL10                        DIRECTORY                            
*                                                                               
MAPFILE  DS    CL3                 MAPPER COMMAND                               
UKUSOFF  DS    CL5                 COUNTRY SPECIFIC CODE SPECIFIERS             
USONLY   DS    CL5                                                              
UKONLY   DS    CL5                                                              
USONLY1  DS    CL8                                                              
UKONLY1  DS    CL8                                                              
*                                                                               
INPLINE  DS    XL80                                                             
WORK     DS    XL40                                                             
ESSTR    DS    CL40                                                             
SPACE    DS    CL80                SPACE FILLED FOR ALL TO USE                  
*                                                                               
INCNTRY  DS    X                   'Y' IF WITHIN COUNTRY SPECIFIC CODE          
PRCLFLAG DS    X                   'Y' IF LINES TO NOT BE PROCESSED             
DTYPE    DS    X                   USES IDNTYPE EQUATES                         
*                                                                               
FNDORG   DS    X                   'Y' IF UNRESOLVED ORG PRIOR                  
ESSIGN   DS    X                   NEXT SIGN TO APPLY TO STRING                 
ESBRACE  DS    X                   'Y' IF WITHIN BRACES                         
ESLEN    DS    X                   'Y' IF EQUATED LENGTH REQUESTED              
ESTYPE   DS    X                   CURRENT EQUATE TYPE                          
ESTYPEL  DS    X                   PREVIOUS EQUATE TYPE                         
ESMATCH  DS    X                   'Y' IF EQUATE TYPES ALL SAME                 
*                                                                               
MAPIDENT DS    X                   'Y' IF NEXT DS IS RECORD IDENTIFER           
MAPASOC  DS    X                   'Y' IF EQUATE HAS OTHER ASSOCIATION          
MAPASOCE DS    CL8                 EQUATE FOR DATA ASSOCIATION                  
*                                                                               
SRTCMDS  DS    0X                                                               
PUT      DS    CL8                 SORT COMMANDS                                
GET      DS    CL8                                                              
END      DS    CL8                                                              
SRTCMDLQ EQU   *-SRTCMDS                                                        
*                                                                               
FILEFRST DS    X                   FIRST FOR THIS TYPE FLAG                     
LASTTYPE DS    X                   LAST ACRECTYPE FROM SORT RECORD              
LASTUNIT DS    C                   LAST UNIT FROM SORT RECORD                   
LASTLGR  DS    C                   LAST LEDGER FROM SORT RECORD                 
*                                                                               
TLREC    DS    XL(TLLENQ)          TSAR RECORD                                  
FMBLD    DS    XL(FMLENQ)          SINGLE LINE OF FILE RECORDS TABLE            
         DS    0D                                                               
TLDATA   DS    XL(TSARDL)          TSAR BUFFER                                  
*                                                                               
SORLEN   DS    H                   LENGTH                                       
SORND    DS    H                   N/D                                          
SORBLK   DS    XL2100              SORT RECORD AREA                             
SORBLKLQ EQU   *-SORBLK                                                         
*                                                                               
IOL      DS    F                                                                
IO       DS    XL2000                                                           
*                                                                               
WRKX     DS    0C                                                               
*                                                                               
LINTABD  DSECT                 *** EQUATE TABLE LINE ENTRIES                    
LINLEN   DS    X                   LENGTH OF TABLE ENTRY                        
LINTYPE  DS    X                   TYPE (DATA)                                  
LINTREC  EQU   C'R'                RECORD HEADER                                
LINTDTA  EQU   C'D'                DATA                                         
LINTEQU  EQU   C'E'                EQUATED VALUE                                
LINTORG  EQU   C'O'                POINTER REPOSITION (UNRESOLVED)              
*                                                                               
LINNAME  DS    XL8                 EQUATED NAME                                 
LINEKYEL DS    X                   KEY OR ELEMENT (FROM DTYPE)                  
LINVAR   EQU   *                   START OF VARIABLE LENGTH ENTRIES             
*                                                                               
         ORG   LINVAR          *** DSECT VALUES                                 
LINRRNUM DS    XL2                 RECORD NUMBER                                
LINRID   DS    XL2                 COUNT OF IDENTIFIERS FOR THIS DSECT          
LINRDSNO DS    XL2                 COUNT OF DS VALUES FOR THIS DSECT            
LINRDESC DS    XL40                RECORD DESCRIPTION                           
LINRLENQ EQU   *-LINTABD                                                        
*                                                                               
         ORG   LINVAR          *** DS VALUES                                    
LINDRNUM DS    XL2                 RECORD NUMBER                                
LINDDNUM DS    XL2                 DATA NUMBER                                  
LINDID   DS    XL2                 IDENTIFER NUMBER (LINDFID TRUE)              
LINDEQNO DS    XL2                 COUNT OF EQ VALUES FOR THIS RECORD           
LINDFLAG DS    X                   DATA SPACE PARAMETERS RESOLVED               
LINDFDSP EQU   X'80'               RESOLVED DISPLACEMENT                        
LINDFMLT EQU   X'40'               RESOLVED DATA MULTIPLIER                     
LINDFLEN EQU   X'20'               RESOLVED LENGTH                              
LINDFID  EQU   X'10'               DS IS IDENTIFIER FOR THIS RECORD             
*                                                                               
LINDDTYP DS    XL1                 DATA TYPE                                    
LINDDT_D EQU   C'D'                DOUBLEWORD                                   
LINDDT_F EQU   C'F'                FULLWORD                                     
LINDDT_V EQU   C'V'                V-TYPE ADDRESS                               
LINDDT_A EQU   C'A'                ADDRESS                                      
LINDDT_H EQU   C'H'                HALFWORD                                     
LINDDT_S EQU   C'S'                S-TYPE                                       
LINDDT_X EQU   C'X'                HEXADECIMAL                                  
LINDDT_C EQU   C'C'                CHAR                                         
LINDDT_P EQU   C'P'                PACK                                         
*                                                                               
LINDINUM DS    XL2                 IDENTIFIER NUMBER (1-3)                      
LINDDSPN DS    XL2                 DISPLACEMENT (NUMBER)                        
LINDMLTN DS    XL2                 DATA MULTIPLIER (NUMBER)                     
LINDLENN DS    XL2                 DATA LENGTH (NUMBER)                         
LINDDSPE DS    CL30                DISPLACEMENT (EQUATE)                        
LINDMLTE DS    CL30                DATA MULTIPLIER (EQUATE)                     
LINDLENE DS    CL30                DATA LENGTH (EQUATE)                         
*                                                                               
LINDDESC DS    CL40                DESCRIPTION                                  
LINDLENQ EQU   *-LINTABD                                                        
*                                                                               
         ORG   LINVAR          *** EQU VALUES                                   
LINERNUM DS    XL2                 RECORD NUMBER                                
LINEDNUM DS    XL2                 ASSOCIATED DATA NUMBER                       
LINEENUM DS    XL2                 EQUATE NUMBER                                
LINEFLAG DS    XL1                 EQUATE FLAGS                                 
LINEFEQU EQU   X'80'               EQUATE HAS BEEN RESOLVED                     
LINEFID  EQU   X'40'               EQUATE IS ASSOCIATED WITH IDENTIFIER         
*                                                                               
LINEDNOE DS    CL8                 EQUATE FOR OVERRIDDEN DATA NUMBER            
LINEINUM DS    XL2                 IDENTIFIER NUMBER (1-3)                      
LINEINU1 DS    XL2                 IDENTIFIER NUMBER (1-3)                      
*                                                                               
LINEEQU  DS    XL1                 EQUATE TYPE                                  
LINE_CHR EQU   C'C'                CHARACTER                                    
LINE_NUM EQU   C'N'                NUMERIC                                      
LINE_HEX EQU   C'X'                HEXADECIMAL                                  
*                                                                               
LINEEQUL DS    XL1                 EQUATE LENGTH                                
LINEEQUN DS    XL2                 EQUATE VALUE (NUMBER)                        
LINEEQUE DS    CL40                EQUATE VALUE (EQUATE)                        
*                                                                               
LINEDESC DS    CL40                EQUATE DESCRIPTION                           
LINELENQ EQU   *-LINTABD                                                        
*                                                                               
HEDTABD  DSECT                 *** RECORD HEADER TABLE                          
HEDNUM   DS    XL2                 RECORD NUMBER                                
HEDNAME  DS    CL8                 RECORD NAME                                  
HEDIDNT  DS    XL2                 NUMBER OF IDENTIFIERS                        
HEDMEMBR DS    XL2                 NUMBER OF MEMBERS                            
HEDDESC  DS    XL40                RECORD DESCRIPTION                           
HEDLENQ  EQU   *-HEDTABD                                                        
*                                                                               
DTATABD  DSECT                 *** DATA TABLE                                   
DTARNUM  DS    XL2                 RECORD NUMBER FOR DATA                       
DTADNUM  DS    XL2                 UNIQUE DATA NUMBER                           
DTANAME  DS    CL8                 DATA NAME                                    
DTAIDNT  DS    XL2                 IDENTIFIER NUMBER                            
DTAMEMBR DS    XL2                 NUMBER OF MEMBERS                            
DTATYPE  DS    XL1                 DATA TYPE (USES LINDD_ EQUATES)              
DTADSPN  DS    XL2                 DISPLACEMENT TO DATA                         
DTAMLTN  DS    XL2                 REPEAT COUNT                                 
DTALENN  DS    XL2                 LENGTH OF DATA                               
DTADESC  DS    CL40                DESCRIPTION                                  
DTALENQ  EQU   *-DTATABD                                                        
*                                                                               
EQUTABD  DSECT                 *** EQUATE TABLE                                 
EQURNUM  DS    XL2                 RECORD NUMBER FOR EQUATE                     
EQUDNUM  DS    XL2                 DATA NUMBER FOR EQUATE                       
EQUENUM  DS    XL2                 UNIQUE EQUATE NUMBER                         
EQUNAME  DS    CL8                 DATA NAME                                    
EQUEQUT  DS    XL1                 EQUATE TYPE (USES LINE_ EQUATES)             
EQUEQUL  DS    XL1                 EQUATE LENGTH                                
EQUEQUN  DS    XL2                 EQUATE VALUE (NUMBER)                        
EQUDESC  DS    CL40                EQUATE DESCRIPTION                           
EQULENQ  EQU   *-EQUTABD                                                        
*                                                                               
IDNTABD  DSECT                 *** IDENTIFIER TABLE                             
IDNNAME  DS    CL35                IDENTIFIER NAME                              
IDNINUM  DS    XL2                 IDENTIFIER NUMBER                            
IDNSPEC1 DS    XL1                 SPECIAL RECTYPE #1                           
IDNSPEC2 DS    XL1                 SPECIAL RECTYPE #2                           
IDNTYPE  DS    XL1                 IDENTIFIER TYPE                              
IDNTKEY  EQU   C'K'                                                             
IDNTEL   EQU   C'E'                                                             
IDNID1   DS    XL(IDILENQ)                                                      
IDNID2   DS    XL(IDILENQ)                                                      
IDNID3   DS    XL(IDILENQ)                                                      
IDNLENQ  EQU   *-IDNTABD                                                        
*                                                                               
IDNIDD   DSECT                 *** COVERS IDNID1-3                              
IDIRNUM  DS    XL2                 RECORD NUMBER                                
IDIDNUM  DS    XL2                 DATA NUMBER                                  
IDIENUM  DS    XL2                 EQUATE NUMBER                                
IDILENQ  EQU   *-IDNIDD                                                         
*                                                                               
FMTABD   DSECT                                                                  
FMTYPE   DS    XL1                 ACRECTYPE EQUATE                             
FMUNIT   DS    CL1                 UNIT                                         
FMLGR    DS    CL1                 LEDGER                                       
FMIDN    DS    (FMIDNN)XL(FMIDNLQ)                                              
FMIDNN   EQU   45                                                               
FMLENQ   EQU   *-FMTABD                                                         
*                                                                               
FMIDND   DSECT                                                                  
FMIDNNUM DS    XL2                 IDENTIFIER NUMBER                            
FMIDNMAX DS    XL1                 MAXIMUM ON 1 RECORD                          
FMIDNMIN DS    XL1                 MINIMUM ON 1 RECORD                          
FMIDNREC DS    XL1                 NUMBER ON THIS RECORD                        
FMIDNLQ  EQU   *-FMIDND                                                         
*                                                                               
SRTFILED DSECT                                                                  
SFTYPE   DS    XL1                                                              
SFREC    DS    0XL2000                                                          
*                                                                               
TLSTD    DSECT                                                                  
TLELEQU1 DS    XL2                 IDENTIFIER #1                                
TLELEQU2 DS    XL2                 IDENTIFIER #2                                
TLELEQU3 DS    XL2                 IDENTIFIER #3                                
TLKEYLQ  EQU   *-TLSTD                                                          
TLLEN1   DS    XL1                 LENGTH OF IDENTIFIER #1                      
TLLEN2   DS    XL1                 LENGTH OF IDENTIFIER #2                      
TLLEN3   DS    XL1                 LENGTH OF IDENTIFIER #3                      
TLLDSP1  DS    XL2                 DISP TO IDENTIFIER #1                        
TLLDSP2  DS    XL2                 DISP TO IDENTIFIER #2                        
TLLDSP3  DS    XL2                 DISP TO IDENTIFIER #3                        
TLIDN    DS    XL2                 IDENTIFIER NUMBER                            
TLLENQ   EQU   *-TLSTD                                                          
*                                                                               
E2WORKD  DSECT                                                                  
E2DUB    DS    D                                                                
E2CALLR1 DS    A                                                                
E2PARM   DS    6F                                                               
E2TABLE  DS    A                                                                
E2STRING DS    A                                                                
E2THIS   DS    F                                                                
E2BRACE  DS    X                                                                
E2SIGN   DS    X                                                                
E2LEN    DS    X                   LENGTH NOT DISPLACEMENT REQUIRED             
E2TYPE   DS    X                   CURRENT EQUATED DATA TYPE                    
E2TYPEL  DS    X                   PREVIOUS EQUATED DATA TYPE                   
E2MATCH  DS    X                   ALL TYPES MATCH INDICATOR                    
E2WORKLQ EQU   *-E2WORKD                                                        
*                                                                               
SRTRECD  DSECT                                                                  
SRTTYPE  DS    XL1                 TABLE TYPE                                   
SRTTREC  EQU   1                   RECORD                                       
SRTTDTA  EQU   2                   DS                                           
SRTTEQU  EQU   3                   EQU                                          
SRTTIDNT EQU   4                   IDENTIFIER                                   
SRTRNUM  DS    XL2                 RECORD NUMBER                                
SRTDNUM  DS    XL2                 DATA NUMBER                                  
SRTENUM  DS    XL2                 EQUATE NUMBER                                
SRTNAME  DS    CL8                 EQUATED NAME                                 
SRTKYEL  DS    CL1                 KEY OR ELEMENT?                              
SRTDESC  DS    CL40                DESCRIPTION                                  
*                                                                               
SRTRDSN  DS    XL2                 DSECT - COUNT OF DS                          
SRTRID   DS    XL2                 DSECT - COUNT OF IDENTIFIERS                 
*                                                                               
SRTDCNT  DS    XL2                 DS - COUNT OF EQUATES                        
SRTDID   DS    XL2                 DS - CURRENT IDENTIFER COUNT                 
SRTDTYPE DS    XL1                 DATA TYPE (USES LINDD_ EQUATES)              
SRTDSPN  DS    XL2                 DISPLACEMENT TO DATA                         
SRTMLTN  DS    XL2                 REPEAT COUNT                                 
SRTLENN  DS    XL2                 LENGTH OF DATA                               
*                                                                               
SRTEQUT  DS    XL1                 EQUATE TYPE (USES LINE_ EQUATES)             
SRTEQUL  DS    XL1                 EQUATE LENGTH                                
SRTEQUN  DS    XL2                 EQUATE VALUE (NUMBER)                        
SRTLENQ  EQU   *-SRTRECD                                                        
*                                                                               
PRO2TABD DSECT                 *** COVERS PASS 2 PROCESS ROUTINES               
PRO2CMP  DS    XL1                                                              
PRO2ADDR DS    AL4                                                              
PRO2LNQ  EQU   *-PRO2TABD                                                       
*                                                                               
EQUTBLD  DSECT                 *** COVERS EQUATE VALIDATION ROUTINES            
EQUTYPE  DS    C                   EQUATE IDENTIFIER                            
EQUEQU   DS    AL1                 EQUATED EQUATE TYPE FOR TABLE                
EQUROUT  DS    AL4                 EQUATE VALIDATION ROUTINE                    
EQUTBLLQ EQU   *-EQUTBLD                                                        
*                                                                               
TYPTABD  DSECT                 *** COVERS STANDARD DATA TYPE EQUATES            
TYPTYPE  DS    C                   DATA TYPE IDENTIFIER                         
TYPEQU   DS    AL1                 EQUATED DATA TYPE FOR TABLE                  
TYPLEN   DS    AL1                 DEFAULT LENGTH FOR DATA TYPE                 
TYPTABLQ EQU   *-TYPTABD                                                        
*                                                                               
DTYPTABD DSECT                                                                  
DTYPLEN  DS    AL1                 COMMAND LEN-1 FOR COMPARE                    
DTYPCMND DS    CL8                 COMMAND                                      
DTYPINS  DS    CL1                 INSTRUCTION                                  
DTYPLENQ EQU   *-DTYPTABD                                                       
*                                                                               
PROCTABD DSECT                     INFORMATION TYPES TO PROCESS TABLE           
PROCLEN  DS    AL1                 LEN-1 OF INFOMATION TYPE FOR COMPARE         
PROCCMP  DS    CL5                 INFORMATION TYPE IDENTIFIER                  
PROCADDR DS    AL4                 INFORMATION TYPE PROCESSING ROUTINE          
PROCLNQ  EQU   *-PROCTABD                                                       
*                                                                               
PRCLTABD DSECT                                                                  
PRCLLEN  DS    AL1                 COMMAND LEN-1 FOR COMPARE                    
PRCLCMND DS    CL8                 COMMAND                                      
PRCLINS  DS    CL1                 INSTRUCTION                                  
PRCLLENQ EQU   *-PRCLTABD                                                       
*                                                                               
CARDTBLD DSECT                                                                  
CARDMCH  DS    AL1                 LENGTH-1 FOR EXECUTED COMPARE                
CARDINP  DS    CL20                INPUT TO MATCH AGAINST                       
CARDPRC  DS    AL4                 PROCESSING ROUTINE FOR INPUT CARD            
CARDTBLL EQU   *-CARDTBLD                                                       
*                                                                               
MAPTABD DSECT                  *** MAPPER COMMANDS                              
MAPLEN   DS    AL1                 LEN-1 OF INFOMATION TYPE FOR COMPARE         
MAPCMND  DS    CL10                COMMAND TYPE IDENTIFIER                      
MAPADDR  DS    AL4                 COMMAND PROCESSING ROUTINE                   
MAPLENQ  EQU   *-MAPTABD                                                        
*                                                                               
NFITABD  DSECT                                                                  
NFINAME  DS    CL8                                                              
NFITYPE1 DS    CL1                                                              
NFITYPE2 DS    CL1                                                              
NFILENQ  EQU   *-NFITABD                                                        
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 2                                                                
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*ACRECEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*DDTSARD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023DDMAPPER  09/04/96'                                      
         END                                                                    
