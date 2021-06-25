*          DATA SET REREPFY02  AT LEVEL 040 AS OF 11/14/02                      
*PHASE REFY02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREPFY02 - RADIO CODE SEED'                                    
*                                                                               
         PRINT NOGEN                                                            
REFY02   CSECT                                                                  
         NMOD1 0,**FY02**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         USING PLINED,P                                                         
         ST    R5,RELO                                                          
*                                                                               
         L     RF,ADCONLST                                                      
         MVC   AUTL,VUTL-ADCONSD(RF)                                            
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,INIT                                                          
         B     EXIT                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
*        BRAS  RE,BLDACTST         BUILD A LIST OF ACTIVE STATIONS              
         BRAS  RE,BLDCODES         BUILD A LIST OF STATION CODES                
         BRAS  RE,STAPROC          PROCESS STATION RECORDS                      
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     RF,AUTL                                                          
         MVC   SAVSYS,4(RF)                                                     
         MVI   4(RF),X'0A'                                                      
         GOTO1 DATAMGR,DMCB,(0,DMOPEN),CONTROL,CTFLIST,REC,0                    
*                                                                               
         L     RF,AUTL                                                          
         MVC   4(1,RF),SAVSYS                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ IN FILE OF ACTIVE RADIO STATIONS                                         
***********************************************************************         
         SPACE 1                                                                
BLDACTST NTR1  ,                                                                
         L     R0,AACTTAB          CLEAR ACTIVE TABLE                           
         L     R1,=A(ACTTABX-ACTTAB)                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AACTTAB                                                       
         XR    R5,R5                                                            
*                                                                               
BACT02   GET   FILEIN,WORK                                                      
         MVC   0(5,R4),WORK                                                     
         LA    R4,5(R4)                                                         
         AHI   R5,1                                                             
         CHI   R5,ACTTABN                                                       
         BNH   BACT02                                                           
         DC    H'0'                                                             
*                                                                               
BACT04   ST    R5,ACTPAR3          SET RECORD COUNT IN BINPARS                  
         CLOSE FILEIN                                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD A LIST OF VALID STATION CODES FROM CTFILE                     *         
***********************************************************************         
         SPACE 1                                                                
BLDCODES NTR1  ,                                                                
         L     RF,AUTL                                                          
         MVC   SAVSYS,4(RF)                                                     
         MVI   4(RF),X'0A'                                                      
*                                                                               
         L     R0,ACODETAB                                                      
         L     R1,=A(CODETABX-CODETAB)                                          
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R2,REC                                                           
         USING CT99RECD,R2                                                      
         XC    CT99KEY,CT99KEY                                                  
         MVI   CT99KTYP,CT99KTYQ                                                
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,REC,REC,0                             
         B     BCOD04                                                           
*                                                                               
BCOD02   GOTO1 DATAMGR,DMCB,DMRSEQ,CTFILE,REC,REC                               
*                                                                               
BCOD04   CLC   =X'9900',CT99KTYP                                                
         BNE   BCOD10                                                           
*                                                                               
X        USING CODETABD,WORK                                                    
         XC    WORK,WORK                                                        
         MVC   X.CUID,CT99KUID     SAVE UNIQUE ID                               
         MVI   X.CSUFF,0           SET LIVE                                     
*                                                                               
         LA    R3,CT99DATA                                                      
         USING CRCLD,R3                                                         
         CLI   0(R3),X'01'         MUST BE X'01' ELEMENT                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   X.CCALL(4),CRCLCLL    CALL LETTERS                               
         MVC   X.CCALL+4(1),CRCLBND                                             
         MVC   X.CCALLS,X.CCALL                                                 
         MVC   X.CCITY,CRCLCTY                                                  
         MVC   X.CSTATE,CRCLSTE                                                 
         GOTO1 SQUASHER,DMCB,X.CCITY,26                                         
*                                                                               
         LA    R3,CT99DATA                                                      
         USING CRCHD,R3                                                         
         XR    RF,RF                                                            
BCOD06   CLI   CRCHEL,0            FIND CALL HISTORY ELEMENT                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CRCHEL,CRCHELQ      FIND CALL HISTORY ELEMENT                    
         BE    *+12                                                             
         IC    RF,CRCHLN                                                        
         BXH   R3,RF,BCOD06                                                     
*                                                                               
         MVC   X.COLD1C,CRCHHST1                                                
         MVC   X.COLD1D,CRCHHDT1                                                
         MVC   X.COLD2C,CRCHHST2                                                
         MVC   X.COLD2D,CRCHHDT2                                                
         GOTO1 BINSRCH,CODEPARS,(X'01',WORK)                                    
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'              INCREASE TABLE SIZE                            
*                                                                               
         CLC   X.COLD1C,SPACES     OLD FORMAT 1 TO PROCESS?                     
         BNH   BCOD08              NO                                           
         MVC   X.CCALL,X.COLD1C                                                 
         MVI   X.CSUFF,1           SET OLD 1                                    
         GOTO1 BINSRCH,CODEPARS,(X'01',WORK)                                    
         OC    0(4,R1),0(R1)                                                    
         BNZ   BCOD08                                                           
         DC    H'0'              INCREASE TABLE SIZE                            
*                                                                               
BCOD08   CLC   X.COLD2C,SPACES     OLD FORMAT 2 TO PROCESS?                     
         BNH   BCOD02              NO                                           
         MVC   X.CCALL,X.COLD2C                                                 
         MVI   X.CSUFF,2           SET OLD 2                                    
         GOTO1 BINSRCH,CODEPARS,(X'01',WORK)                                    
         OC    0(4,R1),0(R1)                                                    
         BNZ   BCOD02                                                           
         DC    H'0'              INCREASE TABLE SIZE                            
*                                                                               
BCOD10   L     RF,AUTL                                                          
         MVC   4(1,RF),SAVSYS                                                   
         B     EXIT                                                             
         DROP  R2,R3,X                                                          
         EJECT                                                                  
***********************************************************************         
* PROCESS STATION RECORDS FOR THIS AGENCY AS REQUESTED                *         
***********************************************************************         
         SPACE 1                                                                
STAPROC  NTR1  ,                                                                
         XC    KEY,KEY                                                          
X        USING RSTAKEY,KEY                                                      
         XC    X.RSTAKEY,X.RSTAKEY                                              
         MVI   X.RSTAKTYP,X'02'                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     BSTA04                                                           
*                                                                               
BSTA02   GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
*                                                                               
BSTA04   CLI   X.RSTAKTYP,X'02'    TEST SAME KEY TYPE                           
         BNE   EXIT                                                             
         CLI   X.RSTAKSTA+4,C'A'   ONLY RADIO                                   
         BE    *+12                                                             
         CLI   X.RSTAKSTA+4,C'F'                                                
         BNE   BSTA02                                                           
*                                                                               
         LA    RF,REPTAB           MATCH TABLE OF POWER CODES                   
BSTA06   CLI   0(RF),0                                                          
         BE    BSTA02                                                           
         CLC   X.RSTAKREP,0(RF)                                                 
         BE    *+12                                                             
         AHI   RF,2                                                             
         B     BSTA06                                                           
*                                                                               
         MVC   PPC,0(RF)           PRINT OUT POWER CODE                         
*                                                                               
         LHI   R0,X'80'                                                         
         CLI   QOPTION2,C'Y'       RUN FOR UPDATE?                              
         BE    *+6                 YES                                          
         XR    R0,R0                                                            
         GOTO1 DATAMGR,DMCB,((R0),GETREC),REPFILE,KEY+28,REC,DMWORK             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BAD RECORD ON FILE                           
*                                                                               
         LA    R2,REC                                                           
         USING RSTAKEY,R2                                                       
         CLI   RSTACODE,X'01'      HOPE THIS IS OK                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPTION1,C'Y'       FILTER ACTIVE STATIONS ONLY                  
         BE    *+14                NO - DO ALL                                  
         OC    RSTAEND,RSTAEND                                                  
         BNZ   BSTA02              INACTIVE - IGNORE                            
*                                                                               
         MVC   PSTA,X.RSTAKSTA     PRINT OUT STATION CALL LETTERS               
         MVI   PACTV,C'A'                                                       
         OC    RSTAEND,RSTAEND                                                  
         BZ    *+8                 TEST INACTIVE                                
         MVI   PACTV,C'I'                                                       
         MVC   PMKTREP,RSTAMKT     SET MARKET                                   
         OC    PMKTREP,SPACES                                                   
*                                                                               
         MVC   PUID,=CL6'??????'   TRY TO FIND UID                              
         XC    WORK,WORK                                                        
         MVC   WORK(L'RSTAKSTA),RSTAKSTA                                        
         GOTO1 BINSRCH,CODEPARS,(X'02',WORK)                                    
*                                                                               
         L     R4,0(R1)            GET RETURNED RECORD                          
         USING CODETABD,R4                                                      
         CLC   CCALL,X.RSTAKSTA     ANY MATCH?                                  
         BNE   BSTA99                                                           
*                                                                               
         CLI   CSUFF,0             LIVE (ACTIVE) CALL/UID MATCH?                
         BNE   BSTA08              YES                                          
         MVC   PUID,CUID                                                        
         MVC   PMKT,CCITY                                                       
*                                                                               
         CLI   QOPTION2,C'Y'       RUN FOR UPDATE?                              
         BNE   BSTA99              NO                                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',REPFIL),(X'2A',(R2)),0,0                       
*                                                                               
         XC    WORK,WORK                                                        
W        USING RSTAUIEL,WORK                                                    
         MVI   W.RSTAUIEC,X'2A'                                                 
         MVI   W.RSTAUILN,12                                                    
         MVC   W.RSTAUIST,CUID                                                  
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',REPFIL),(R2),WORK,0                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,PUTREC),REPFILE,KEY+28,REC,DMWORK                
         CLI   8(R1),0                                                          
         BE    BSTA99                                                           
         DC    H'0'                BAD RECORD ON PUT                            
*                                                                               
BSTA08   MVC   PCALL,CCALLS                                                     
         MVC   PMKT,CCITY                                                       
         MVC   PUID,SPACES                                                      
         MVC   PNUID,CUID                                                       
         MVC   POLD1C,COLD1C                                                    
         MVC   POLD2C,COLD2C                                                    
*                                                                               
         OC    COLD1D,COLD1D                                                    
         BZ    BSTA10                                                           
         GOTO1 DATCON,DMCB,(3,COLD1D),(13,POLD1D)                               
*                                                                               
BSTA10   OC    COLD1D,COLD1D                                                    
         BZ    BSTA99                                                           
         GOTO1 DATCON,DMCB,(3,COLD2D),(13,POLD2D)                               
*                                                                               
BSTA99   GOTO1 REPORT                                                           
         B     BSTA02                                                           
         EJECT                                                                  
***********************************************************************         
* BINSRCH PARAMETER LISTS                                             *         
***********************************************************************         
         SPACE 1                                                                
ACTPARS  DS    0D                                                               
ACTPAR1  DC    A(0)                A(SEARCH ARG)                                
ACTPAR2  DC    A(ACTTAB)           A(TABLE)                                     
ACTPAR3  DC    F'0'                NUMBER OF RECORDS IN TABLE                   
ACTPAR4  DC    A(L'ACTTAB)         RECORD LENGTH                                
ACTPAR5  DC    AL1(0),AL3(5)       KEY DSPL/LENGTH                              
ACTPAR6  DC    A(ACTTABN)          MAX RECS                                     
*                                                                               
CODEPARS DS    0D                                                               
CODEPAR1 DC    A(0)                A(SEARCH ARG)                                
CODEPAR2 DC    A(CODETAB)          A(TABLE)                                     
CODEPAR3 DC    F'0'                NUMBER OF RECORDS IN TABLE                   
CODEPAR4 DC    A(L'CODETAB)        RECORD LENGTH                                
CODEPAR5 DC    AL1(0),AL3(6)       KEY DSPL/LENGTH                              
CODEPAR6 DC    A(CODETABN)         MAX RECS                                     
         EJECT                                                                  
***********************************************************************         
* DCBS                                                                *         
***********************************************************************         
         SPACE 1                                                                
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=FB,LRECL=24,MACRF=GM,      +        
               EODAD=BACT04                                                     
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
REPTAB   DS    0XL2                                                             
         DC    C'BFK4KUKFEACRS3RSG8J0NU'      KRNY                              
         DC    C'AQMGIFD4CNL7IBUO'            IRNY                              
         DC    C'B3'                          EJOR                              
         DC    X'00'                                                            
*                                                                               
AUTL     DS    F                                                                
RELO     DS    F                   RELOCATION FACTOR                            
         EJECT                                                                  
*                                                                               
SAVSYS   DS    X                                                                
CTFILE   DC    CL8'CTFILE  '                                                    
CONTROL  DC    CL8'CONTROL '                                                    
REPFIL   DC    CL8'REPFILE '                                                    
DMOPEN   DC    CL8'DMOPEN  '                                                    
*                                                                               
VHELLO   DC    V(HELLO)                                                         
BINSRCH  DC    V(BINSRCH)                                                       
ACODETAB DC    A(CODETAB)                                                       
AACTTAB  DC    A(ACTTAB)                                                        
         EJECT                                                                  
CTFLIST  DC    C'NCTFILE X'                                                     
REC      DS    CL4096              I/O AREA                                     
*                                                                               
         DC    CL8'*ACTTAB*'                                                    
ACTTAB   DS    10000XL5                                                         
ACTTABX  EQU   *                                                                
ACTTABN  EQU   (ACTTABX-ACTTAB)/L'ACTTAB                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*CODETAB'                                                    
CODETABN EQU   30000                                                            
CODETAB  DS    (CODETABN)XL(CODETABL)                                           
CODETABX EQU   *                                                                
*                                                                               
CODETABD DSECT                                                                  
CCALL    DS    CL5                 STATION CALL LETTERS                         
CSUFF    DS    X                   SUFFIX                                       
CUID     DS    CL6                 UNIQUE ID                                    
CCALLS   DS    CL5                 STATION CALL LETTERS                         
COLD1C   DS    CL7                 OLD CALL LETTERS 1                           
COLD1D   DS    XL3                 OLD CALL DATE 1                              
COLD2C   DS    CL7                 OLD CALL LETTERS 2                           
COLD2D   DS    XL3                 OLD CALL DATE 2                              
CCITY    DS    CL24                CITY                                         
CSTATE   DS    CL2                 STATE                                        
CODETABL EQU   *-CODETABD                                                       
*                                                                               
*  INCLUDE REGENREPA               REP RECORD                                   
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
PLINED   DSECT                                                                  
PPC      DS    CL2                 POWER CODE                                   
         DS    C                                                                
PSTA     DS    CL5                 REP STATION                                  
         DS    C                                                                
PACTV    DS    C                   ACTIVE/INACTIVE                              
         DS    C                                                                
PUID     DS    CL6                 MATCHING UID                                 
         DS    C                                                                
PMKTREP  DS    CL20                REP MARKET                                   
         DS    C                                                                
PMKT     DS    CL26                M STREET MARKET                              
         DS    C                                                                
PCALL    DS    CL5                 NEW CALL LETTERS                             
         DS    C                                                                
PNUID    DS    XL6                 UID THAT MATCHES THE OLD STUFF               
         DS    C                                                                
POLD1C   DS    CL7                 OLD NAME 1                                   
         DS    C                                                                
POLD1D   DS    CL10                OLD DATE 1                                   
         DS    C                                                                
POLD2C   DS    CL7                 OLD NAME 2                                   
         DS    C                                                                
POLD2D   DS    CL10                OLD DATE 2                                   
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL4008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE CTGENRAD                                                       
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
       EJECT                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040REREPFY02 11/14/02'                                      
         END                                                                    
