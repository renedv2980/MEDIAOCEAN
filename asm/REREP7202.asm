*          DATA SET REREP7202  AT LEVEL 089 AS OF 07/09/07                      
*PHASE RE7202A                                                                  
***********************************************************************         
* HISTORY:                                                                      
*                                                                               
* 28APR92 (SKU) ADD CATEGORY FIELD                                              
*                                                                               
* 12APR92 (SKU) OPTION TO SHOW ONLY ADVERTISER CODES W/O A CATEGORY             
*               CODE                                                            
*                                                                               
* 08NOV95 (RHV) ADD PRINTING OF KATZ CODES TO REPORT                            
*                                                                     *         
* DEC14/95 (BG )  38 CHANGE REGENALL TO REGENALL1 2K CON              *         
*                                                                     *         
* FEB26/97 (DBU)  ACTIVITY DATE FOR INDIVIDUAL RECORD MUST BE         *         
*                 WITHIN REQUEST DATES FOR RECORD TO BE DISPLAYED     *         
*                                                                   * *         
* JAN28/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* SEP09/98 (AST) --- FILTER ON LOCAL/NATIONAL OFFICES (QOPTION2)    *           
*                                                                   *           
* NOV28/06 (BU ) --- EQUIVALENCE LISTING                            *           
*                                                                   *           
* JUL09/07 (SKU) --- FIX 'BOTH' OPTIONS BUG                         *           
*                                                                   *           
*                          ** END TOMBSTONE **                        *         
***********************************************************************         
         TITLE 'ADVERTISER LISTING PROGRAM'                                     
RE7202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE7202,RR=R5                                                 
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*              CHECK MODE SETTINGS                                              
         SPACE 3                                                                
         MVI   RCSUBPRG,0                                                       
*                                                                               
         CLI   QOPTION2,C'L'                                                    
         BE    *+12                                                             
         CLI   QOPTION2,C'N'                                                    
         BNE   *+8                                                              
         BAS   RE,LOADOFF                                                       
*                                                                               
         CLI   QOPTION2,C' '                                                    
         BE    AD1                                                              
         CLI   QOPTION2,C'B'                                                    
         BE    AD1                                                              
         MVI   RCSUBPRG,1                                                       
         CLI   QOPTION2,C'S'                                                    
         BE    AD1                                                              
         MVI   RCSUBPRG,2                                                       
         SPACE 2                                                                
AD1      CLI   MODE,REQFRST                                                     
         BNE   AD2                                                              
*                                                                               
         MVI   EQUIV,C'N'          REQUEST FOR EQUIVALENT LISTING?              
         CLC   RCREPFL,=C'CV'      ABC?                                         
         BE    AD10010             YES - PERMIT SPECIAL OPTION                  
         CLC   RCREPFL,=C'SJ'      SJR - FOR TESTING?                           
         BE    AD10010             YES - PERMIT SPECIAL OPTION                  
         CLC   RCREPFL,=C'B3'      EJOR- FOR TESTING?                           
         BE    AD10010             YES - PERMIT SPECIAL OPTION                  
         CLC   RCREPFL,=C'B4'      ROB - FOR TESTING?                           
         BNE   AD10020             NO  - DON'T PERMIT SPECIAL OPTION            
AD10010  EQU   *                                                                
         CLC   =C'EQUIV',QUESTOR                                                
         BNE   AD10020             NO                                           
         MVI   EQUIV,C'Y'          YES - SET OPTION                             
*                                                                               
AD10020  EQU   *                                                                
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     ADEXT                                                            
         SPACE 2                                                                
AD2      EQU   *                                                                
         CLI   MODE,REQLAST                                                     
         BNE   AD4                                                              
         CLI   EQUIV,C'Y'          EQUIV LISTING?                               
         BE    ADEXT               YES - FINISHED                               
         BAS   RE,PRINTEM                                                       
         B     ADEXT                                                            
AD4      CLI   MODE,PROCADV                                                     
         BNE   ADEXT                                                            
*                                                                               
         CLI   EQUIV,C'Y'          EQUIV LISTING?                               
         BNE   AD40020             NO                                           
         MVI   RCSUBPRG,3                                                       
         BAS   RE,DOEQUIV                                                       
         B     ADEXT                                                            
AD40020  EQU   *                                                                
         CLI   QOPTION1,C'A'       IF OPT 1, ONLY PRINT ADV W/O                 
         BNE   AD5                 CATEGORY CODES                               
         OC    RADVCATG,RADVCATG   CHECK IF NULLS OR SPACES                     
         BZ    AD5                                                              
         CLC   RADVCATG,SPACES                                                  
         BNE   ADEXT                                                            
*                                                                               
AD5      DS    0H                                                               
         CLC   QSTART(6),SPACES                                                 
         BE    AD7                                                              
         GOTO1 DATCON,DMCB,(2,RADVLCD),(3,DATE)                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(3,STDATE)                                
         GOTO1 DATCON,DMCB,(0,QEND),(3,ENDDATE)                                 
         CLC   STDATE,DATE         IS DATE WITHIN REQUEST DATES?                
         BH    ADEXT               NO - SKIP IT                                 
         CLC   ENDDATE,DATE        IS DATE WITHIN REQUEST DATES?                
         BL    ADEXT               NO - SKIP IT                                 
AD7      EQU   *                                                                
         CLI   QOPTION2,C'L'                                                    
         BE    *+12                                                             
         CLI   QOPTION2,C'N'                                                    
         BNE   AD8                                                              
         BAS   RE,CHKSCP                                                        
         CLI   RTNFLG,0            DISPLAY RECORD?                              
         BNE   ADEXT               NO, SKIP IT                                  
*                                                                               
AD7A     EQU   *                                                                
         CLC   QOFFICE,SPACES                                                   
         BNH   AD8                                                              
         BAS   RE,CHKOFF                                                        
         CLI   RTNFLG,0                                                         
         BNE   ADEXT                                                            
*                                                                               
AD8      EQU   *                                                                
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(4),RADVKADV                                               
         MVC   WORK+7(20),RADVNAME                                              
         BAS   RE,POSTEM                                                        
         CLI   QOPTIONS,C'F'                                                    
         BNE   ADEXT                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(5),RADVKATZ  MOVE KATZ CODE INTO LINE                     
         MVC   WORK+8(20),RADVCITY                                              
         BAS   RE,POSTEM                                                        
         MVC   WORK,SPACES                                                      
         CLC   RADVCATG,SPACES                                                  
         BE    AD10                                                             
         MVC   WORK+7(2),RADVCATG                                               
         BAS   RE,EXPCAT                                                        
AD10     BAS   RE,POSTEM                                                        
         MVC   WORK,SPACES                                                      
         BAS   RE,POSTEM                                                        
         SPACE 2                                                                
ADEXT    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO PRODUCE AN EQUIVALENCY LISTING                        
***********************************************************************         
DOEQUIV  NTR1                                                                   
         SPACE 2                                                                
**       CLC   RADVORIG,SPACES     ANY EQUIVALENCY CODE ENTERED?                
**       BNH   DOEQ0900            NO  - DON'T LIST IT                          
         MVC   P+1(04),RADVKADV    YES - DISPLAY ADVERTISER CODE                
         MVC   P+7(20),RADVNAME    DISPLAY ADVERTISER NAME                      
         OC    RADVORIG,SPACES     OR IN SPACES                                 
         MVC   P+30(5),RADVORIG    DISPLAY EQUIVALENCY CODE                     
         GOTO1 LOCALREP,DMCB,ALLTEXT                                            
DOEQ0900 EQU   *                                                                
         XIT1                                                                   
*                                                                               
ALLTEXT  EQU   *                                                                
         DC    C'O',AL1(01),C'T',AL1(04)   OFFSET / ADV CODE                    
         DC    C'O',AL1(02),C'T',AL1(20)   OFFSET / ADV NAME                    
         DC    C'O',AL1(03),C'T',AL1(05)   OFFSET / ADV EQUIV CODE              
         DC    X'0000'                                                          
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
LOCALREP NTR1                                                                   
*                                                                               
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BNE   LOCALNO                                                          
*                                                                               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         L     R1,0(R1)            R1 -> PASSED DEFINITION LIST                 
LREP10   EQU   *                                                                
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LREP20                                                           
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LREP10                                                           
LREP20   EQU   *                                                                
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
         B     LREPGOOD                                                         
LOCALNO  EQU   *                                                                
         GOTO1 REPORT                                                           
*                                                                               
LREPGOOD EQU   *                                                                
         XIT1                                                                   
***********************************************************************         
*              ROUTINE TO POST TO TABLE                                         
***********************************************************************         
POSTEM   NTR1                                                                   
         SPACE 2                                                                
POST1    LA    R2,PAGETAB                                                       
         LA    R3,192                                                           
         SPACE 2                                                                
POST2    CLI   0(R2),0                                                          
         BE    POST4                                                            
         LA    R2,28(R2)           COLUMN WIDTH INCREASED TO 28 (RHV)           
         BCT   R3,POST2                                                         
         BAS   RE,PRINTEM                                                       
         B     POST1                                                            
         SPACE 2                                                                
POST4    MVC   0(28,R2),WORK                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO PRINT FROM TABLE                                      
***********************************************************************         
PRINTEM  NTR1                                                                   
         MVC   PMWORK(20),WORK                                                  
         LA    R2,PAGETAB                                                       
         LA    R3,48                                                            
         LA    R1,48                                                            
         MH    R1,=H'28'                                                        
         LA    R4,0(R2,R1)                                                      
         LA    R5,0(R4,R1)                                                      
         LA    R6,0(R5,R1)                                                      
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
PRINT2   MVC   P(28),0(R2)                                                      
         MVC   P+28(28),0(R4)                                                   
         MVC   P+56(28),0(R5)                                                   
         MVC   P+84(28),0(R6)                                                   
         GOTO1 REPORT                                                           
         XC    0(28,R2),0(R2)                                                   
         XC    0(28,R4),0(R4)                                                   
         XC    0(28,R5),0(R5)                                                   
         XC    0(28,R6),0(R6)                                                   
         LA    R2,28(R2)                                                        
         LA    R4,28(R4)                                                        
         LA    R5,28(R5)                                                        
         LA    R6,28(R6)                                                        
         BCT   R3,PRINT2                                                        
         MVC   WORK(20),PMWORK                                                  
PRINTEX  EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO EXPAND CATEGORY NAME                                  
***********************************************************************         
EXPCAT   NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R6,IOAREA                                                        
         USING RCTGREC,R6                                                       
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,RADVKREP                                                
         MVC   RCTGKCTG,RADVCATG                                                
         MVC   KEY,IOAREA                                                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EXPCATX                                                          
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK            
         MVC   WORK+10(18),RCTGNAME                                             
*                                                                               
EXPCATX  DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY,0                     
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
*********************************                                               
*                                                                               
* CHECK SCOPE FILTER FOR DISPLAYING ADV RECS                                    
*                                                                               
CHKSCP   NTR1                                                                   
         MVI   RTNFLG,0            SET TO DISPLAY NEXT RECORD                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK         
         LA    R6,RADVREC                                                       
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BNE   CSNX                NO ELEMS, IS NATIONAL                        
         B     CS11                                                             
CS10     BAS   RE,NEXTEL                                                        
         BNE   CSSX                NO MORE ELEMS, SKIP REC                      
*                                                                               
CS11     LA    R1,OFF2TBL          POINT TO TABLE OF OFFICES                    
         B     CSO02                                                            
CSO01    LA    R1,TBENTEQ(R1)      BUMP TO NEXT OFFICE IN TABLE                 
         CLI   0(R1),X'FF' END OF TABLE?                                        
         BNE   CSO02               NO, SKIP                                     
* END OF TABLE, OFFICE MUST BE NATIONAL                                         
         CLI   QOPTION2,C'L'        IF LOCAL FILTER                             
         BNE   CSDX                NO, DISPLAY RECORD                           
         MVI   RTNFLG,1            LOCAL FILTER SET FLAG                        
         B     CS10                CHECK NEXT ELEM                              
CSO02    CLC   0(2,R1),2(R6)       COMPARE OFFICES                              
         BNE   CSO01               CHECK NEXT TABLE ENTRY                       
         CLC   QOPTION2,2(R1)       COMPARE FILTER TO TABLE                     
         BE    CSDX                EQUAL, EXIT W/ FLAG SET TO DISPLAY           
*                                                                               
         B     CS10                CHECK NEXT ELEM                              
*                                                                               
CSNX     DS    0H                  REC HAS NO OFFICES, IS NATIONAL              
         CLI   QOPTION2,C'L'       IS FILTER LOCAL?                             
         BNE   CSDX                NATIONAL/BOTH FILTER, DISPLAY                
*                                  LOCAL FILTER, NATIONAL RECORD, SKIP          
*                                                                               
CSSX     DS    0H                  SKIP THE RECORD                              
         MVI   RTNFLG,1            NO MATCHING SCOPE FOUND                      
*                                  DONT' DISPLAY                                
         B     CSX                                                              
*                                                                               
CSDX     DS    0H                  DISPLAY THE RECORD                           
         MVI   RTNFLG,0                                                         
CSX      B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CHECK OFFICE FILTER FOR DISPLAYING ADV RECS                                   
*                                                                               
CHKOFF   NTR1                                                                   
         MVI   RTNFLG,0            SET TO DISPLAY NEXT RECORD                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK         
         LA    R6,RADVREC                                                       
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BNE   COSX                NO ELEMS, NO OFFICES, SKIP                   
         B     CO11                                                             
CO10     BAS   RE,NEXTEL                                                        
         BNE   COSX                NO MORE ELEMS, SKIP REC                      
*                                                                               
CO11     CLC   QOFFICE,2(R6)        IS OFFICE ELEMENT SAME AS FILTER?           
         BE    CODX                YES, DISPLAY RECORD                          
*                                                                               
         B     CO10                CHECK NEXT ELEM                              
*                                                                               
COSX     DS    0H                  SKIP THE RECORD                              
         MVI   RTNFLG,1            NO MATCHING SCOPE FOUND                      
*                                  DONT' DISPLAY                                
         B     COX                                                              
*                                                                               
CODX     DS    0H                  DISPLAY THE RECORD                           
         MVI   RTNFLG,0                                                         
COX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOAD THE TABLE WITH OFFICES AND SCOPES                                        
*                                                                               
LOADOFF  NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
         LA    RE,OFF2TBL          CLEAR OFFICE TABLE                           
         LA    RF,TBLNEQ                                                        
         XCEF                                                                   
*                                                                               
         LA    R4,OFF2TBL                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'44'           OFFICE2 RECORD                               
         MVC   KEY+23(2),QREP      USE QREP IN REQUEST                          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         B     LO11                                                             
LO10     MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY,0                 
LO11     CLC   KEY(25),KEYSAVE     SAME REP?                                    
         BNE   LOX                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK         
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'10'        GET OFFICE FAX ELEM                          
         BAS   RE,GETEL                                                         
         BNE   LO10                CHECK NEXT RECORD                            
         USING ROFF2FXE,R6                                                      
*                                                                               
         MVI   2(R4),C'N'          ASSUME NATIONAL                              
         TM    ROFF2PRF+1,X'80'    LOCAL?                                       
         BZ    *+8                 NO, SKIP                                     
         MVI   2(R4),C'L'          YES, IT'S LOCAL                              
         LA    R6,IOAREA                                                        
         USING ROFF2KEY,R6                                                      
         MVC   0(2,R4),ROFF2OFF    STORE OFFICE                                 
*                                                                               
         LA    R4,TBENTEQ(R4)      FILL IN NEXT TABLE ENTRY                     
         B     LO10                                                             
*                                                                               
LOX      DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY,0                     
         B     XIT                                                              
         DROP  R6                                                               
***********************                                                         
XIT      XIT1                                                                   
***********************                                                         
         LTORG                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
PMWORK   DS    CL20                                                             
RELO     DS    A                                                                
         DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
DATE     DS    CL3                 LAST CHANGED DATE IN BINARY                  
STDATE   DS    CL3                 FROM: REQUEST DATE                           
ENDDATE  DS    CL3                 TO: REQUEST DATE                             
RTNFLG   DS    CL1                                                              
ELCODE   DS    XL1                                                              
EQUIV    DS    CL1                                                              
SAVEKEY  DS    CL32                                                             
IOAREA   DS    CL1000                                                           
         SPACE 3                                                                
*                                                                               
OFF2TBL  DS    0CL300              100 3-BYTE ENTRIES                           
         DS    CL2                 OFF CODE                                     
         DS    CL1                 OFF SCOPE                                    
*                                                                               
TBENTEQ  EQU   *-OFF2TBL LENGTH OF ONE ENTRY                                    
*                                                                               
         DS    99CL3               REST OF ENTRIES                              
*                                                                               
TBLNEQ   EQU   *-OFF2TBL LENGTH OF TABLE                                        
         DC    X'FF'               END OF TABLE                                 
*                                                                               
PAGETAB  DC    6000X'00'                                                        
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENOFF2                                                      
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089REREP7202 07/09/07'                                      
         END                                                                    
