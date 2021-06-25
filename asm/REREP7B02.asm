*          DATA SET REREP7B02  AT LEVEL 106 AS OF 05/01/02                      
*PHASE RE7B02A,*                                                                
*INCLUDE PERVERT                                                                
*INCLUDE REGENBUC                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREP7B02 - STATION CONTRACT END DATE LISTER'                   
*********************************************************************           
*                                                                   *           
*        REREP7B02 --- STATION CONTRACT END DATE LIST               *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* FEB13/02 (HQ ) --- FRESH MEAT                                     *           
* FEB28/02 (HQ ) --- ADAPT DOWNLOADABLE FORMAT                      *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
RE7B02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE7B02,R7,RR=RE                                              
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         B     MAIN30              ZERO IS GOOD RETURN                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     MODEEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
*                                  ALL DATA COLLECTION WILL BE                  
*                                     OUT OF THE 'INITIAL' ROUTINE              
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
***      DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
INITIAL  NTR1                                                                   
         LA    R0,TKWORK           CLEAR WORKING STORAGE                        
         LHI   R1,TKWORKLQ         IN PROGRAMS AREA                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R6,P                                                             
         USING PRINLIN,R6                                                       
         MVI   RCSUBPRG,0          SET INITIAL HEADING FOR OFFICE               
         MVI   STOPTEST,C'N'       STOP TEST = N                                
         MVI   PRINLAST,C'N'       PRINT LAST = N                               
*                                                                               
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    INIT0003                                                         
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         GOTO1 REPORT                                                           
         B     INIT0005                                                         
INIT0003 EQU   *                                                                
         BAS   RE,DWNHEAD          DOWNLOAD HEADING                             
*                                                                               
INIT0005 EQU   *                                                                
         XC    KEY,KEY                                                          
K        USING RSTAREC,KEY                                                      
         MVI   K.RSTAKEY,X'02'     READ STATION RECORD                          
         MVC   K.RSTAKREP,RCREPFL  INSERT REP CODE                              
*                                                                               
         GOTO1 HIGH                                                             
         B     E                                                                
INIT0010 EQU   *                                                                
         XC    P,P                                                              
         GOTO1 SEQ                                                              
E        CLC   KEY(RSTAKSTA-RSTAKEY),KEYSAVE                                    
         BNE   INIT0060            NO MORE STATION TO READ                      
*                                                                               
         MVC   SSTATION,K.RSTAKSTA                                              
         MVC   DSTATION(4),K.RSTAKSTA                                           
         CLI   K.RSTAKSTA+4,C' '                                                
         BE    INIT0015                                                         
         CLI   K.RSTAKSTA+4,C'C'                                                
         BE    INIT0010            SKIP ORDERS WITH '-C'                        
*                                                                               
         MVI   DSTATION+4,C'-'                                                  
         MVC   DSTATION+5(1),K.RSTAKSTA+4                                       
         CLI   K.RSTAKSTA+4,C'F'                                                
         BE    INIT0011                                                         
         CLI   K.RSTAKSTA+4,C'A'                                                
         BE    INIT0011                                                         
         B     INIT0015                                                         
INIT0011 MVI   DSTATION+6,C'M'                                                  
INIT0015 GOTO1 GETSTA              GOT STATION RECORD                           
*                                                                               
         LA    R5,RSTAREC                                                       
         MVI   ELCODE,X'10'        CONTRACT END DATE ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   INIT0016            ONLY ORDER WITH CONTRACT END DATE!           
         USING RSTADTEL,R5                                                      
         OC    RSTADTCE,RSTADTCE   ANY CONTRACT END                             
         BZ    INIT0016            NO, NEXT RECORD                              
         B     INIT0017                                                         
INIT0016 CLI   QOPTION1,C'A'       ALL ORDERS?                                  
         BNE   INIT0010            NO                                           
         B     INIT0020                                                         
INIT0017 GOTO1 DATCON,DMCB,(3,RSTADTCE),(8,DCEDATE)                             
*                                                                               
         MVC   WORK(L'RCDATE),RCDATE                                            
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         GOTO1 DATCON,DMCB,(3,RSTADTCE),(0,WORK+6)                              
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6                                     
*                                                                               
         MVC   DMNTHS,=C'EXPIRED'                                               
         LH    R4,DMCB+14        MONTH IN BETWEEN INCLUSIVE                     
         BCTR  R4,0              DECREASE BY ONE                                
         CHI   R4,0                                                             
         BL    INIT0020          EXPIRED                                        
         XC    DMNTHS,DMNTHS     PRINT OUT MONTH DIFFERENCE                     
         EDIT  (R4),(7,DMNTHS),ZERO=NOBLANK                                     
         DROP  R5                                                               
*                                                                               
INIT0020 EQU   *                                                                
         LA    R5,RSTAREC          EXTRA DESCRIPTION ELT                        
         MVI   ELCODE,X'08'        GET A/R INTERFACE CODE                       
         BAS   RE,GETEL                                                         
         BE    X                                                                
         MVC   DARCODE,=CL10'NOT FOUND'                                         
         B     INIT0025                                                         
X        MVC   DARCODE,RSTAOSI-RSTAXXEL(R5)                                     
*                                                                               
INIT0025 EQU   *                                                                
         LA    R5,RSTAREC          GET JOIN DATE                                
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RSTACODE,R5                                                      
         MVI   DSTATUS,C'Y'                                                     
         OC    RSTAEND,RSTAEND     IS THERE A LEAVING DATE?                     
         BZ    *+16                                                             
         CLI   QOPTION2,C'I'       SHOW INACTIVE?                               
         BNE   INIT0010            NO, SKIP ORDER                               
         MVI   DSTATUS,C'N'        YES, NOT ACTIVE                              
*                                                                               
         OC    RSTASTRT,RSTASTRT   ANY JOIN DATE                                
         BZ    INIT0030            NO                                           
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(8,DJDATE)                              
         DROP  R5                                                               
         DROP  K                                                                
         CLC   QASAT,SPACES                                                     
         BNE   INIT0030                                                         
         GOTO1 DATCON,DMCB,(5,QASAT),(0,QASAT)                                  
         MVI   NODATE,C'Y'                                                      
*HAN                                                                            
INIT0030 EQU   *                                                                
         MVC   KEYSAV2,KEY                                                      
         XC    KEY,KEY                                                          
K        USING RCOMREC,KEY                                                      
         MVI   K.RCOMKTYP,X'29'                                                 
         MVC   K.RCOMKREP,RCREPFL                                               
         MVC   K.RCOMKSTA,SSTATION                                              
         GOTO1 HIGH                                                             
         B     INIT0045                                                         
INIT0040 GOTO1 SEQ                 NEXT REC                                     
INIT0045 CLC   KEY(RCOMKOFF-RCOMREC),KEYSAVE                                    
         BNE   INIT0050            NEXT STATION                                 
         LA    R8,KEY                                                           
         USING RCOMKEY,R8                                                       
         OC    RCOMKOFF(L'RCOMKOFF+L'RCOMKADV),RCOMKOFF                         
         BNZ   INIT0040                                                         
         DROP  R8                                                               
         GOTO1 GETCOM                                                           
*                                                                               
         CLI   STOPTEST,C'Y'                                                    
         BE    INIT0048                                                         
INIT0047 EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,QASAT),(3,WORK)                                   
         CLC   WORK(L'RCOMKDAT),RCOMKDAT                                        
         BNH   PASS                EFFECTIVE DATE > FILTER                      
         MVC   TEMPEFF,RCOMKDAT                                                 
         MVC   TEMPCOMM,RCOMRAT1                                                
         MVI   PRINLAST,C'Y'                                                    
         MVI   LASTDONE,C'N'                                                    
         B     INIT0040            NEXT COMMISION RECORD                        
*                                                                               
PASS     MVI   STOPTEST,C'Y'       STOP TEST UNTIL NEW ADV,OFF                  
         MVI   LASTDONE,C'Y'                                                    
INIT0048 EQU   *                                                                
         MVI   PRINLAST,C'N'                                                    
         EDIT  (4,RCOMRAT1),(6,DCOMRATE),FILL=0,ZERO=NOBLANK                    
         OC    RCOMKDAT,RCOMKDAT                                                
         BZ    INIT0049            NO EFFECTIVE DATE?                           
         GOTO1 DATCON,DMCB,(3,RCOMKDAT),(9,DEFFDATE)                            
INIT0049 EQU   *                                                                
         CLI   NODATE,C'Y'                                                      
         BE    INIT0040                                                         
         BAS   RE,LOCALREP                                                      
         DROP  K                                                                
         B     INIT0040            NEXT COMMISION RECORD                        
*                                                                               
INIT0050 EQU   *                                                                
         MVI   STOPTEST,C'N'                                                    
*                                                                               
         CLI   PRINLAST,C'Y'                                                    
         BNE   INIT0055                                                         
         MVI   PRINLAST,C'N'                                                    
         EDIT  (4,TEMPCOMM),(6,DCOMRATE),FILL=0,ZERO=NOBLANK                    
         GOTO1 DATCON,DMCB,(3,TEMPEFF),(9,DEFFDATE)                             
*        BAS   RE,LOCALREP                                                      
INIT0055 EQU   *                                                                
         BAS   RE,LOCALREP                                                      
         MVC   KEY,KEYSAV2         RESTORE KEY                                  
         GOTO1 HIGH                GET BACK TO STATION READ SEQ                 
         B     INIT0010                                                         
*                                                                               
INIT0060 EQU   *                   EXIT                                         
         B     MODEEXIT                                                         
         EJECT                                                                  
MODEEXIT LTR   R0,R0                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
         GETEL    R5,34,ELCODE                                                  
*                                                                               
WORK2    DS    CL256                                                            
         EJECT                                                                  
***>>>                                                                          
**********************************************************************          
* DOWN HEAD - DOWNLOAD HEADING                                                  
**********************************************************************          
DWNHEAD  NTR1                                                                   
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         LA    R1,DWNDEF2          R1 -> DOWNOAD DEFINITION                     
         DROP  R5                                                               
DWNH0010 DS    0H                                                               
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    DWNH0020                                                         
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     DWNH0010                                                         
*                                                                               
DWNH0020 MVC   P+00(07),=C'STATION'                                             
         MVC   P+07(06),=C'ACTIVE'                                              
         MVC   P+13(04),=C'JOIN'                                                
         MVC   P+17(08),=C'CONTRACT'                                            
         MVC   P+25(09),=C'REMAINING'                                           
         MVC   P+34(03),=C'A/R'                                                 
         MVC   P+37(03),=C'EFF'                                                 
         MVC   P+40(04),=C'COMM'                                                
         MVI   LINE,1              NEVER PAGE BREAK                             
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
*                                                                               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         LA    R1,DWNDEF3          R1 -> DOWNOAD DEFINITION                     
         DROP  R5                                                               
DWNH0030 DS    0H                                                               
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    DWNH0040                                                         
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     DWNH0030                                                         
*                                                                               
DWNH0040 MVC   P+13(04),=C'DATE'                                                
         MVC   P+17(08),=C'END DATE'                                            
         MVC   P+25(04),=C'TERM'                                                
         MVC   P+29(04),=C'CODE'                                                
         MVC   P+33(04),=C'DATE'                                                
         MVC   P+37(04),=C'RATE'                                                
         MVI   LINE,1              NEVER PAGE BREAK                             
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
*                                                                               
         XIT1                                                                   
         SPACE 1                                                                
         EJECT                                                                  
***>>>                                                                          
**********************************************************************          
* LOCAL REP - DOWNLOAD HANDLING                                                 
**********************************************************************          
LOCALREP NTR1                                                                   
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BNE   LOCALNO             NO - JUST GOTO REPORT                        
                                                                                
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
                                                                                
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         LA    R1,DWNDEF           R1 -> DOWNOAD DEFINITION                     
         DROP  R5                                                               
                                                                                
LOCAL010 DS    0H                                                               
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LOCAL020                                                         
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LOCAL010                                                         
                                                                                
LOCAL020 DS    0H                                                               
         XC    DWNBUFF,DWNBUFF                                                  
         MVC   DWNBUFF+00(07),P+1                                               
         MVC   DWNBUFF+07(01),P+13                                              
         MVC   DWNBUFF+08(08),P+19                                              
         MVC   DWNBUFF+16(08),P+30                                              
         MVC   DWNBUFF+24(07),P+41                                              
         MVC   DWNBUFF+31(10),P+51                                              
         MVC   DWNBUFF+41(08),P+63                                              
         MVC   DWNBUFF+50(08),P+73                                              
         XC    P,P                                                              
         MVC   P(L'DWNBUFF),DWNBUFF                                             
*                                                                               
         MVI   LINE,1              NEVER PAGE BREAK                             
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
         B     LOCALX                                                           
*                                                                               
LOCALNO  DS    0H                                                               
         GOTO1 REPORT                                                           
                                                                                
LOCALX   DS    0H                                                               
         XIT1                                                                   
         SPACE 1                                                                
         EJECT                                                                  
DWNDEF   DC    C'T',AL1(07)                                                     
         DC    C'T',AL1(01)                                                     
         DC    C'T',AL1(08)                                                     
         DC    C'T',AL1(08)                                                     
         DC    C'T',AL1(07)                                                     
         DC    C'T',AL1(10)                                                     
         DC    C'T',AL1(08)                                                     
         DC    C'N',AL1(08)                                                     
         DC    X'0000'                                                          
         EJECT                                                                  
*                                                                               
DWNDEF2  DC    C'T',AL1(07)                                                     
         DC    C'T',AL1(06)                                                     
         DC    C'T',AL1(04)                                                     
         DC    C'T',AL1(08)                                                     
         DC    C'T',AL1(09)                                                     
         DC    C'T',AL1(03)                                                     
         DC    C'T',AL1(03)                                                     
         DC    C'T',AL1(04)                                                     
         DC    X'0000'                                                          
         EJECT                                                                  
*                                                                               
DWNDEF3  DC    C'T',AL1(07)                                                     
         DC    C'T',AL1(06)                                                     
         DC    C'T',AL1(04)                                                     
         DC    C'T',AL1(08)                                                     
         DC    C'T',AL1(04)                                                     
         DC    C'T',AL1(04)                                                     
         DC    C'T',AL1(04)                                                     
         DC    C'T',AL1(04)                                                     
         DC    X'0000'                                                          
         EJECT                                                                  
*                                                                               
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         B     EXIT                                                             
         EJECT                                                                  
GETCOM   LA    RF,RCOMREC                                                       
         B     LINKFILE                                                         
GETCON   LA    RF,RCONREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETBUY   LA    RF,RBUYREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETADV   LA    RF,RADVREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETSTA   LA    RF,RSTAREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETOFF   LA    RF,ROFFREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETREP   LA    RF,RREPREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         LR    R2,RF                                                            
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*  DATA MANAGER INTERFACE (CHECK ERRORS)                                        
*                                                                               
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION?               
         BZ    DM020                                                            
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         B     MODEEXIT                                                         
*                                                                               
DM010    EQU   *                                                                
         TM    DMINBTS,X'08'       PASS BACK DELETED RECORDS?                   
         BO    MODEEXIT            YES                                          
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    MODEEXIT                                                         
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*                                                                               
*                                                                               
*   ROUTINE TO TRACE DATA MANAGER CALLS                                         
*                                                                               
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,MTRACDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
TKWORK   EQU   *                                                                
SORTREC  DS    0CL32                                                            
SSTATION DS    CL5   +0            STATION CALL LETTER                          
SACTIVE  DS    CL4   +5            ACTIVE/INACTIVE FLAG                         
SJDATE   DS    CL2   +9            STATION JOIN DATE                            
SCEDATE  DS    CL2   +11           STATION CONTRACT END DATE                    
SARCODE  DS    CL10  +13           STATION A/R INTERFACE CODE                   
         DS    CL9   +23           SPARE                                        
SRTRECLN EQU   *-SORTREC                                                        
*                                                                               
RECCOUNT DS    F                                                                
LASTAGYO DS    CL6                                                              
LASTAGY  DS    CL6                                                              
LASTOFF  DS    CL2                                                              
LASTADV  DS    CL4                                                              
ELCODE   DS    X                   ELEMENT CODE FOR GETEL                       
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
AIO      DS    A                   USE CONTRACT RECORD AREA AS IO               
RELO     DS    F                   RELOCATION ADDRESS                           
NODATE   DS    CL1                 FLAG FOR NO EFFECTIVE DATE                   
STOPTEST DS    CL1                 FLAG FOR STOPPING TEST                       
PRINLAST DS    CL1                 FLAG FOR PRINTING LAST                       
LASTDONE DS    CL1                 LAST ONE IS BEFORE EFFECTIVE DATE            
TEMPHOLD DS    0CL10                                                            
TEMPOFF  DS    CL2                 HOLDS PREVIOUS OFFICE                        
TEMPADV  DS    CL4                 HOLDS PREVIOUS ADV                           
TEMPEFF  DS    CL2                 HOLDS PREVIOUS EFF DATE                      
TEMPCOMM DS    F                   HOLDS PREVIOUS COMMISION RATE                
DWNBUFF  DS    CL57                                                             
*                                                                               
TKWORKLQ EQU   *-TKWORK                                                         
*                                                                               
* END OF REINITIALIZED WORKING STORAGE                                          
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8'NREPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
*TL      DC    F'0',X'0A'          FOR CONTROL SYSTEM                           
***<<<                                                                          
*                                                                               
*&&DO                                                                           
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
SORTCARD DC    CL80'SORT FIELDS=(1,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=192'                                   
*&&                                                                             
         SPACE 2                                                                
*                                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENCOM                                                       
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE RESUBREPS                                                      
       ++INCLUDE REGENREQ2                                                      
       ++INCLUDE DDMASTD                                                        
PRINLIN  DSECT                                                                  
         DS    CL1                                                              
DSTATION DS    CL7                 STATION CALL LETTER                          
         DS    CL5                                                              
DSTATUS  DS    CL1                 STATUS FLAG ACTIVE/INACTIVE                  
         DS    CL5                                                              
DJDATE   DS    CL8                 STATION JOIN DATE                            
         DS    CL3                                                              
DCEDATE  DS    CL8                 CONTRACT END DATE                            
         DS    CL3                                                              
DMNTHS   DS    CL7                 MONTH TO CONTRACT END DATE                   
         DS    CL3                                                              
DARCODE  DS    CL10                A/R INTERFACE CODE                           
         DS    CL2                                                              
DEFFDATE DS    CL8                 EFFECTIVE DATE                               
         DS    CL2                                                              
DCOMRATE DS    CL8                 COMMISION RATE                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'106REREP7B02 05/01/02'                                      
         END                                                                    
