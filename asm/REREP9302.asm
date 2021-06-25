*          DATA SET REREP9302  AT LEVEL 067 AS OF 04/22/15                      
*          DATA SET REREP9302  AT LEVEL 066 AS OF 10/12/99                      
*PHASE RE9302A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'REREP9302 - RE9302 - CONTRACT ACTIVE AGY/ADV SCAN'              
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP9302 --- CONTRACT ACTIVE AGY/ADV SCAN                 *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 04OCT99 RHV -- VOILA!                                             *           
*                                                                   *           
*********************************************************************           
*                                                                   *           
*                                                                   *           
*********************************************************************           
RE9202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STOREX-STORED,*RE9302*,R8,RR=R5                                  
         USING STORED,RC                                                        
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
                                                                                
*                                                                               
         CLI   MODE,REQFRST        ONLY MODE SUPPORTED                          
         BE    REQF                                                             
                                                                                
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
NZ       EQU   *                                                                
EXITL    CLI   *,X'FF'             SET CC LOW                                   
         B     EXIT                                                             
Z        EQU   *                                                                
EXITOK   CR    RB,RB               SET CC EQUAL                                 
EXIT     XIT1                                                                   
*                                                                               
**********************************************************************          
* REPORT PARAMETERS:                                                            
*                                                                               
FEDATE   DC    X'C421'  01/01/98   END FLIGHT DATE FILTER                       
*                                                                               
*                                                                               
*                                                                               
STALIST  DS    0CL5                STATION FILTER LIST                          
         DC    CL5'WNEP '                                                       
         DC    X'FF'                                                            
*                                                                               
*&&DO                                                                           
STALIST  DS    0CL5                STATION FILTER LIST                          
         DC    CL5'KRQE '      ALBUQUERQUE        REPC TO REPB                  
         DC    CL5'WDTN '      DAYTON        REPC TO REPB                       
         DC    CL5'KASY '      ALBUQUERQUE        REPC TO REPB                  
         DC    CL5'WNEP '      WILKES/BARRE    REPC TO REPB * ALREADY           
         DC    CL5'WVUE '      NEW ORLEANS        REPC TO REPB                  
         DC    CL5'WFTC '      MINNEAPOLIS          REPC TO REPB                
         DC    CL5'KUSI '      SAN DIEGO        REPC TO REPB                    
         DC    CL5'WAWS '      JACKSONVILLE    REPC TO REPB                     
         DC    CL5'WUTV '      BUFFALO        REPC TO REPB                      
         DC    CL5'KLRT '      LITTLE ROCK        REPC TO REPB                  
         DC    CL5'KASN '      LITTLE ROCK        REPC TO REPB                  
         DC    CL5'KFOR '      OKLAHOMA CITH    REPC TO REPB                    
         DC    CL5'WNAC '      PROVIDENCE        REPC    TO REPB                
         DC    CL5'WHO  '      DES MOINES        REPC TO REPB                   
         DC    CL5'WQAD '      QUAD CITIES        REPC TO REPB                  
         DC    CL5'WTEV '     JACKSONVILLE    REPC TO REPB                      
         DC    CL5'KTBU '     HOUSTON        REPC TO REPB                       
         DC    X'FF'                                                            
*&&                                                                             
**********************************************************************          
* REQFRST - PROCESS DOWNLOAD REPORT REQUEST                                     
**********************************************************************          
REQF     DS    0H                                                               
         GOTOX LOADER,DMCB,=CL8'T00AAC',0                                       
         MVC   REPFACS,4(R1)       EXTERNAL ROUTINE, MISC. SUBROUTINES          
         MVC   RFBLOCK(4),ACOMFACT                                              
         MVC   RFBLOCK+4(2),QREP                                                
*                                                                               
         LA    RE,HOOK                                                          
         ST    RE,HEADHOOK                                                      
*                                                                               
         BAS   RE,GETCON           READ CONTRACTS                               
         BAS   RE,PROCCON          PROCESS CONTRACTS, GENERATE REPORT           
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CKSTA - CHECK STATION AGAINST STATION LIST                                    
*                                                                               
*  IN  - 5 BYTE STAION CALL LETTERS IN WORK                                     
*  OUT - CC EQ: STATION IN LIST                                                 
*        CC NEQ: NOT IN LIST                                                    
**********************************************************************          
CKSTA    NTR1                                                                   
         LA    R2,STALIST                                                       
CKSTA10  DS    0H                                                               
         CLI   0(R2),X'FF'                                                      
         BE    EXITL                                                            
         CLC   0(L'STALIST,R2),WORK                                             
         BE    EXITOK                                                           
         LA    R2,L'STALIST(R2)                                                 
         B     CKSTA10                                                          
**********************************************************************          
* GETCON - READ TAPE FOR CONTRACTS & PASS TO SORTER                             
**********************************************************************          
GETCON   NTR1                                                                   
*                                                                               
* INTIALIZE                                                                     
*                                                                               
         GOTOX =V(SORTER),DMCB,SORTCARD,RECCARD   INITL SORTER                  
*                                                                               
* LOOP THRU CONTRACT RECS                                                       
*                                                                               
         XC    KEY,KEY             INIT FOR FIRST READ                          
         LA    R6,KEY                                                           
         USING RCONREC,R6                                                       
         MVI   RCON8ETP,X'8E'                                                   
         MVC   RCON8ERP,QREP                                                    
*                                                                               
GC100    DS    0H                                                               
         MVC   KEYSAVE(27),KEY                                                  
         GOTOX DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(3),KEYSAVE      KEY TYPE & REP CODE                          
         BNE   EXIT                                                             
*                                                                               
         CLI   RCON8EID,1          TYPE 1 KEY?                                  
         BE    GC120               YES - PROCESS                                
         MVI   RCON8EID,X'FF'      NO - SKIP READ TO                            
         B     GC100               NEXT ROOT KEY                                
*                                                                               
GC120    DS    0H                                                               
         MVC   WORK(5),RCON8EST                                                 
         BAS   RE,CKSTA            STATION IN FILTER TABLE?                     
         BE    GC140               YES - PROCESS                                
         MVC   RCON8EFS,=X'FFFF'   NO - SKIP READ TO                            
         B     GC100               NEXT STATION                                 
*                                                                               
GC140    DS    0H                                                               
         CLC   RCON8EFE,FEDATE     FLT END VS. FILTER EDATE                     
         BNL   GC200               PROCESS                                      
         MVI   RCON8EID,X'FF'      NO - SKIP READ TO                            
         B     GC100               NEXT CONTRACT                                
*                                                                               
GC200    DS    0H                  PASS AGENCY TO SORTER                        
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING SORTD,R3                                                         
         MVI   STYPE,1                                                          
         MVC   SCODE(6),RCON8EAG                                                
         GOTOX =V(SORTER),DMCB,=C'PUT',SORTREC                                  
*                                                                               
         XC    WORK,WORK           PASS ADVERTISER TO SORTER                    
         MVI   STYPE,2                                                          
         MVC   SCODE(4),RCON8EAV                                                
         GOTOX =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         DROP  R3                                                               
*                                                                               
         MVI   RCON8EID,X'FF'      SKIP READ TO                                 
         B     GC100               NEXT CONTRACT                                
**********************************************************************          
* PROCCON - RETRIEVE SORT RECORDS & OUTPUT REPORT                               
**********************************************************************          
PROCCON  NTR1                                                                   
*                                                                               
* RETRIEVE CONTRACTS FROM SORTER                                                
*                                                                               
         XC    WORK,WORK           WORK HOLDS LAST REC FROM SORTER              
*                                                                               
PC050    DS    0H                                                               
         GOTOX =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,DMCB+4                                                     
         BZ    EXIT                                                             
*                                                                               
         USING SORTD,R3                                                         
         LA    R4,P                                                             
         USING DLINE,R4                                                         
*                                                                               
         CLC   0(L'SORTREC,R3),WORK   SAME AS LAST SORTREC?                     
         BE    PC050                  YES - GET NEXT                            
*                                                                               
         CLI   STYPE,1                                                          
         BNE   PC080                                                            
         MVC   DTYPE,=C'AGY'                                                    
         GOTOX (RFAGYOUT,REPFACS),DMCB,SCODE,DCODE,DEXP,RFBLOCK                 
         B     PC100                                                            
PC080    DS    0H                                                               
         CLI   STYPE,2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DTYPE,=C'ADV'                                                    
         MVC   DCODE(4),SCODE                                                   
         GOTOX (RFADVOUT,REPFACS),DMCB,SCODE,DEXP,RFBLOCK                       
*                                                                               
PC100    DS    0H                                                               
         BAS   RE,LOCALREP                                                      
         MVC   WORK(L'SORTREC),0(R3)  SAVE LAST SORTREC?                        
         B     PC050               NEXT CONTRACT                                
         DROP  R3,R4                                                            
**********************************************************************          
* HEAD - COLUMN HEADERS                                                         
**********************************************************************          
HOOK     NTR1                                                                   
         B     EXIT                                                             
**********************************************************************          
* LOCAL REP - DOWNLOAD HANDLING                                                 
**********************************************************************          
LOCALREP NTR1                                                                   
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BNE   LOCALR              NO - JUST PRINT LINE                         
                                                                                
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
                                                                                
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         LA    R1,DEF              R1 -> DOWNOAD DEFINITION                     
         DROP  R5                                                               
                                                                                
LOCAL010 DS    0H                                                               
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LOCAL020                                                         
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LOCAL010                                                         
                                                                                
LOCAL020 DS    0H                                                               
         MVI   LINE,1              NEVER PAGE BREAK                             
*                                                                               
         GOTOX REPORT,DMCB,WORKC,=C'PRINT'                                      
         B     LOCALX                                                           
*                                                                               
LOCALR   GOTOX REPORT                                                           
                                                                                
LOCALX   DS    0H                                                               
         B     EXIT                                                             
                                                                                
DEF      DC    C'T',AL1(03)        TYPE                                         
         DC    C'T',AL1(07)        CODE                                         
         DC    C'T',AL1(36)        EXPANSION                                    
         DC    X'0000'                                                          
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,7,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=7'                                     
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DASHES   DC    80C'-'                                                           
EQUALS   DC    80C'='                                                           
         DS    CL2000              INCREASE PROGRAM SIZE!!!!                    
*                                                                               
PLINE    DSECT                     PRINT LINE DSECT                             
PLEN     EQU   *-PLINE                                                          
*                                                                               
DLINE    DSECT                     DOWNLOAD LINE DSECT                          
DTYPE    DS    CL3                 TYPE AGY/ADV                                 
DCODE    DS    CL7                 AGY/ADV CODE                                 
DEXP     DS    CL36                AGY/ADV EXPANSION                            
DLEN     EQU   *-DLINE                                                          
*                                                                               
SORTD    DSECT         DSECT FOR SORTER REC                                     
SORTREC  DS    0CL7                                                             
STYPE    DS    XL1                 01=AGY   01=ADV                              
SCODE    DS    CL6                 AGY+OFC / ADV CODE                           
*                                                                               
*                                                                               
* WORKING STORAGE                                                               
*                                                                               
STORED   DSECT                                                                  
RELO     DS    A                                                                
REPFACS  DS    A                                                                
CALLOV   DS    A                                                                
ELCODE   DS    X                                                                
RFBLOCK  DS    CL6                 REPFACS PARAM BLOCK                          
         DS    0D                                                               
         DS    F                                                                
IOAREA   DS    CL4096                                                           
STOREX   EQU   *                                                                
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE REPFACSQ                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067REREP9302 04/22/15'                                      
         END                                                                    
