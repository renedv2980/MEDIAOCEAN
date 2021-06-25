*          DATA SET SPREPFXPWB AT LEVEL 003 AS OF 06/20/97                      
*PHASE SPFX026B                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'SPFX02 - MERGE CABLE STATION PW REC W/ SAME SYSCODE'            
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 MYWORKL,SPFX02,R8,RR=R2,CLEAR=YES                                
         USING MYWORKD,RC                                                       
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
FX       DS    0H                                                               
         BAS   RE,INIT                                                          
*                                                                               
         XC    KEY,KEY                                                          
         XC    CURRSTA,CURRSTA                                                  
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   KEY(L'PWKTYP),=X'0D7A'     PW RECORDS ONLY                       
                                                                                
*                                                                               
FX032    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     FX040                                                            
*                                                                               
FX034    DS    0H                                                               
         GOTO1 SEQ                                                              
         B     FX040                                                            
                                                                                
*                                                                               
FX040    DS    0H                                                               
         LA    R0,1                                                             
         A     R0,CNTDIRRD                                                      
         ST    R0,CNTDIRRD                                                      
                                                                                
*                                                                               
         CLC   KEY(L'PWKTYP),=X'0D7A'                                           
         BNE   FX100                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         XC    TMPKEY,TMPKEY                                                    
PWK1     USING PWFKEY,TMPKEY                                                    
         MVC   PWK1.PWFKEY,KEY                                                  
         TM    PWK1.PWKSTA,X'F0'                                                
         BNO   FX034                                                            
*                                                                               
         NC    PWK1.PWKSTA,CBLSCMSK                                             
*                                                                               
         DS    0H                                                               
         L     R6,ANEWREC                                                       
         CLC   PWK1.PWFKEY,0(R6)                                                
         BE    FX070                                                            
         MVC   CURRSTA,PWK1.PWKSTA                                              
         DROP  PWK1                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         BAS   RE,ADDRECD                                                       
         BAS   RE,BLDSKREC                                                      
                                                                                
*                                                                               
FX070    DS    0H                                                               
         GOTO1 GET                                                              
         LA    R0,1                                                             
         A     R0,CNTFILRD                                                      
         ST    R0,CNTFILRD                                                      
                                                                                
*                                                                               
         DS    0H                                                               
         BAS   RE,MRGRECD          MERGE READ RECD INTO NEW RECD                
*                                                                               
         BAS   RE,DELRECD          DELETE RECORD THAT WAS JUST READ             
                                                                                
*                                                                               
         B     FX034                                                            
                                                                                
*                                                                               
** NO MORE PW RECORDS **                                                        
*                                                                               
FX100    DS    0H                                                               
         BAS   RE,ADDRECD          ADD THE RECORD THAT WAS WORKED ON            
*                                                                               
         B     FX200                                                            
         EJECT                                                                  
FX200    DS    0H                  REPORTING                                    
         MVI   LINE,99              PAGE EJECT                                  
                                                                                
         MVC   P1(25),=C'NUMBER OF DIRECTORY READS'                             
         L     R1,CNTDIRRD                                                      
         EDIT  (R1),(10,P1+30),ZERO=NOBLANK                                     
                                                                                
         MVC   P2(25),=C'NUMBER OF FILE      READS'                             
         L     R1,CNTFILRD                                                      
         EDIT  (R1),(10,P2+30),ZERO=NOBLANK                                     
                                                                                
         MVI   P3,0                                                             
         MVI   P4,0                                                             
                                                                                
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(25),=C'NUMBER OF RECORDS DELETED'                             
         L     R1,CNTRDEL                                                       
         EDIT  (R1),(10,P1+30),ZERO=NOBLANK                                     
                                                                                
         MVC   P2(23),=C'NUMBER OF RECORDS ADDED'                               
         L     R1,CNTRADD                                                       
         EDIT  (R1),(10,P2+30),ZERO=NOBLANK                                     
                                                                                
         MVI   P3,0                                                             
         MVI   P4,0                                                             
                                                                                
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(13),=C'OLD FILE SIZE'                                         
         L     R1,OLDSIZE                                                       
         EDIT  (R1),(10,P1+30),ZERO=NOBLANK,COMMAS=YES                          
                                                                                
         MVC   P2(13),=C'NEW FILE SIZE'                                         
         L     R1,NEWSIZE                                                       
         EDIT  (R1),(10,P2+30),ZERO=NOBLANK,COMMAS=YES                          
                                                                                
         MVC   P3+30(10),=C'----------'                                         
                                                                                
         MVC   P4(19),=C'CHANGE IN FILE SIZE'                                   
         L     R1,NEWSIZE                                                       
         S     R1,OLDSIZE                                                       
         EDIT  (R1),(10,P4+30),ZERO=NOBLANK,COMMAS=YES,FLOAT=-                  
                                                                                
         GOTO1 REPORT                                                           
                                                                                
*                                                                               
FXEND    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*=========================== INITIALIZATION ==========================*         
                                                                                
INIT     NTR1                                                                   
         LH    R1,=Y(NEWREC-MYWORKD)                                            
         LA    R1,MYWORKD(R1)                                                   
         ST    R1,ANEWREC                                                       
                                                                                
*                                                                               
         MVC   CBLSCMSK,=X'FFFF80'   SET CABLE SYSCODE MASK                     
                                                                                
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         MVC   AREC,ADPWREC                                                     
         SR    RF,RF                                                            
         DROP  RF                                                               
                                                                                
*                                                                               
         L     R0,ANEWREC                                                       
         LA    R1,L'NEWREC                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                 CLEAR NEW RECORD AREA                      
                                                                                
*                                                                               
         DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= BUILD SKELETON RECORD =======================*         
                                                                                
* AT ENTRY,                                                                     
*   KEY     = PWFKEY W/ MED/CLT/PRD/EST/MKT                                     
*   CURRSTA = STATION TO BUILD SKELETON RECORD FOR                              
* AT EXIT,                                                                      
*   ANEWREC = A(SKELETON RECORD)                                                
                                                                                
BLDSKREC NTR1                                                                   
         L     R0,ANEWREC                                                       
         LA    R1,L'NEWREC                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR NEW RECORD AREA                        
                                                                                
*                                                                               
         DS    0H                  SET KEY AND LENGTH OF RECORD                 
         L     R6,ANEWREC                                                       
         USING PWRECD,R6                                                        
         MVC   PWFKEY,KEY                                                       
         MVC   PWKSTA,CURRSTA                                                   
         MVI   PWEL,0                                                           
         LA    R0,(PWEL-PWRECD)+1                                               
         STCM  R0,3,PWLEN                                                       
                                                                                
*                                                                               
         DS    0H                  PUT ACTIVITY ELEMENT INTO RECORD             
         XC    MYELEM,MYELEM                                                    
         LA    R3,MYELEM                                                        
         USING ACTVD,R3                                                         
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,TODAYB                                                  
         MVC   ACTVCHDT,TODAYB                                                  
                                                                                
         GOTO1 =V(HELLO),DMCB,(C'P',=C'SPTFILE'),ANEWREC,ACTVD,0,      +        
               RR=RELO                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
                                                                                
*                                                                               
         DROP  R6                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ MERGE RECORD ===========================*         
                                                                                
* TAKES THE STATION-LEVEL PW RECORD READ FROM FILE AND MERGE IT TO              
*  THE NEW RECORD.                                                              
* AT ENTRY,                                                                     
*   AREC  = A(SOURCE RECORD)                                                    
* AT EXIT,                                                                      
*   ANEWREC = A(UPDATED RECORD)                                                 
                                                                                
MRGRECD  NTR1                                                                   
         L     R3,AREC                                                          
                                                                                
*                                                                               
** AGENCY CODE **                                                               
*                                                                               
         DS    0H                                                               
         L     RF,ANEWREC                                                       
         USING PWRECD,RF                                                        
         OC    PWAGYA,PWAGYA                                                    
         BNZ   *+10                                                             
         MVC   PWAGYA,(PWAGYA-PWRECD)(R3)                                       
         DROP  RF                                                               
                                                                                
*                                                                               
** RECORD ELEMENTS **                                                           
*                                                                               
         DS    0H                                                               
         LA    R3,(PWEL-PWRECD)(R3)                                             
                                                                                
*                                                                               
MR022    DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    MRX                                                              
         CLI   0(R3),PWGNELQ                                                    
         BE    MRGN_                                                            
         CLI   0(R3),PWDOLCDQ                                                   
         BE    MRDOL                                                            
         CLI   0(R3),PWBAKDOL                                                   
         BE    MRDOL                                                            
         CLI   0(R3),X'F1'                                                      
         BE    MRBUMP                                                           
                                                                                
*                                                                               
*** PWGNEL ELEMENT ***                                                          
*                                                                               
MRGN_    DS    0H                                                               
         USING PWGNEL,R3                                                        
         GOTO1 =V(HELLO),DMCB,(C'G',=C'SPTFILE'),('PWGNELQ',ANEWREC),  +        
               0,0                                                              
         CLI   DMCB+12,0                                                        
         BE    MRGN_020                                                         
         CLI   DMCB+12,X'06'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                  ELEMENT NOT FOUND                            
         GOTO1 =V(HELLO),DMCB,(C'P',=C'SPTFILE'),ANEWREC,PWGNEL,0               
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     MRGN_030                                                         
                                                                                
*                                                                               
MRGN_020 DS    0H                  ELEMENT FOUND                                
         L     RF,DMCB+12                                                       
         ZIC   R1,1(RF)                                                         
         CLM   R1,1,PWGNLEN                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         BCTR  R1,0                                                             
         EXCLC R1,0(RF),PWGNEL      ELEMENTS MUST MATCH                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     MRGN_030                                                         
                                                                                
*                                                                               
MRGN_030 DS    0H                                                               
         B     MRBUMP                                                           
         DROP  R3                                                               
                                                                                
*                                                                               
*** PWDOLEL AND ITS BACKUP ELEMENTS ***                                         
*                                                                               
MRDOL    DS    0H                                                               
         USING PWDOLEL,R3                                                       
         GOTO1 =V(HELLO),DMCB,(C'G',=C'SPTFILE'),(0(R3),ANEWREC),      +        
               (L'PWDOLWK,PWDOLWK),0                                            
         CLI   DMCB+12,0                                                        
         BE    MRDOL020                                                         
         CLI   DMCB+12,X'06'                                                    
         BE    MRDOL040                                                         
         DC    H'0'                                                             
                                                                                
*                                                                               
MRDOL020 DS    0H                  ELEMENT FOUND                                
         L     R2,DMCB+12           R2-->FOUND ELEMENT                          
*                                                                               
         DS    0H                   ACCUMULATE SPOTS                            
         ICM   R0,15,(PWDOLSPT-PWDOLEL)(R2)                                     
         ICM   R1,15,PWDOLSPT                                                   
         AR    R0,R1                                                            
         STCM  R0,15,(PWDOLSPT-PWDOLEL)(R2)                                     
*                                                                               
         DS    0H                   ACCUMULATE WIM GROSS                        
         ICM   R0,15,(PWDOLWG-PWDOLEL)(R2)                                      
         ICM   R1,15,PWDOLWG                                                    
         AR    R0,R1                                                            
         STCM  R0,15,(PWDOLWG-PWDOLEL)(R2)                                      
*                                                                               
         DS    0H                   ACCUMULATE WIM NET                          
         ICM   R0,15,(PWDOLWN-PWDOLEL)(R2)                                      
         ICM   R1,15,PWDOLWN                                                    
         AR    R0,R1                                                            
         STCM  R0,15,(PWDOLWN-PWDOLEL)(R2)                                      
*                                                                               
         DS    0H                   ACCUMULATE CLT GROSS                        
         ICM   R0,15,(PWDOLCG-PWDOLEL)(R2)                                      
         ICM   R1,15,PWDOLCG                                                    
         AR    R0,R1                                                            
         STCM  R0,15,(PWDOLCG-PWDOLEL)(R2)                                      
*                                                                               
         DS    0H                   ACCUMULATE CLT NET                          
         ICM   R0,15,(PWDOLCN-PWDOLEL)(R2)                                      
         ICM   R1,15,PWDOLCN                                                    
         AR    R0,R1                                                            
         STCM  R0,15,(PWDOLCN-PWDOLEL)(R2)                                      
*                                                                               
         DS    0H                   ACCUMULATE WIM TAX                          
         ICM   R0,15,(PWDOLTAX-PWDOLEL)(R2)                                     
         ICM   R1,15,PWDOLTAX                                                   
         AR    R0,R1                                                            
         STCM  R0,15,(PWDOLTAX-PWDOLEL)(R2)                                     
*                                                                               
         DS    0H                   ACCUMULATE SCPWCLTX                         
         ICM   R0,15,(PWDOLCTX-PWDOLEL)(R2)                                     
         ICM   R1,15,PWDOLCTX                                                   
         AR    R0,R1                                                            
         STCM  R0,15,(PWDOLCTX-PWDOLEL)(R2)                                     
*                                                                               
         B     MRDOL050                                                         
                                                                                
*                                                                               
MRDOL040 DS    0H                  ELEMENT NOT FOUND                            
         GOTO1 =V(HELLO),DMCB,(C'P',=C'SPTFILE'),ANEWREC,PWDOLEL,0              
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     MRDOL050                                                         
                                                                                
*                                                                               
MRDOL050 DS    0H                                                               
         B     MRBUMP                                                           
         DROP  R3                                                               
                                                                                
*                                                                               
*** BUMP TO NEXT ELEMENT ***                                                    
*                                                                               
MRBUMP   DS    0H                                                               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     MR022                                                            
                                                                                
*                                                                               
MRX      DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== DELETE RECORD ===========================*         
                                                                                
* AT ENTRY,                                                                     
*   AREC = A(RECORD TO DELETE)                                                  
*   KEY  = KEY OF RECORD TO DELETE                                              
                                                                                
DELRECD  NTR1                                                                   
         L     R3,AREC                                                          
         OC    0(L'PWFKEY,R3),0(R3)                                             
         BZ    DLRCX                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         OI    (PWCNTL-PWRECD)(R3),X80                                          
         GOTO1 PUT                                                              
*                                                                               
         OI    KEY+L'PWFKEY,X80                                                 
         GOTO1 WRITE                                                            
                                                                                
*                                                                               
** PRINT KEY OF DELETED RECORDS **                                              
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,WORK,18,=C'TOG',0                                
*                                                                               
         LA    RF,WORK                                                          
         LA    R2,P1                                                            
         USING P1DSECT,R2                                                       
         MVC   P1CPTION,=C'DELETE:'                                             
                                                                                
         MVC   P1KEY,0(RF)                                                      
         LA    RF,L'P1KEY(RF)                                                   
                                                                                
         MVC   P1CNTL,0(RF)                                                     
         LA    RF,L'P1CNTL(RF)                                                  
                                                                                
         MVC   P1DSKADD,0(RF)                                                   
         DROP  R2                                                               
*                                                                               
         GOTO1 REPORT                                                           
                                                                                
*                                                                               
** UPDATE COUNTERS **                                                           
*                                                                               
         DS    0H                                                               
         LA    R0,1                                                             
         A     R0,CNTRDEL                                                       
         ST    R0,CNTRDEL                                                       
*                                                                               
         ZICM  R0,(PWLEN-PWRECD)(R3),(3)                                        
         A     R0,OLDSIZE                                                       
         ST    R0,OLDSIZE                                                       
                                                                                
*                                                                               
DLRCX    DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= ADD NEW RECORD TO FILE ======================*         
                                                                                
* AT ENTRY,                                                                     
*   ANEWREC = A(RECORD TO ADD)                                                  
                                                                                
ADDRECD  NTR1                                                                   
         L     R3,ANEWREC                                                       
         OC    0(L'PWFKEY,R3),0(R3)                                             
         BZ    ADRCX                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   TMPKEY,KEY          HOLD ONTO KEY                                
         MVC   KEY(L'PWFKEY),0(R3)                                              
         GOTO1 HIGH                                                             
*                                                                               
         L     RF,ADD                                                           
         CLC   KEY(L'PWFKEY),0(R3)                                              
         BNE   *+8                                                              
         L     RF,PUT                                                           
*                                                                               
         DS    0H                                                               
         L     R0,AREC             HOLD ONTO ADDRESS                            
         ST    R3,AREC                                                          
         GOTO1 (RF)                                                             
         ST    R0,AREC             RESTORE ADDRESS                              
         MVC   KEY,TMPKEY           AND  KEY                                    
         GOTO1 READ                 AND DIRECTORY READ                          
                                                                                
*                                                                               
** PRINT ADDED RECORD **                                                        
*                                                                               
         ZICM  R0,(PWLEN-PWRECD)(R3),(3)                                        
         GOTO1 =V(PRNTBL),DMCB,=C'ADD:',ANEWREC,C'DUMP',(R0),=C'1D',   +        
               RR=RELO                                                          
                                                                                
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
                                                                                
*                                                                               
** UPDATE COUNTERS **                                                           
*                                                                               
         DS    0H                                                               
         LA    R0,1                                                             
         A     R0,CNTRADD                                                       
         ST    R0,CNTRADD                                                       
*                                                                               
         ZICM  R0,(PWLEN-PWRECD)(R3),(3)                                        
         A     R0,NEWSIZE                                                       
         ST    R0,NEWSIZE                                                       
                                                                                
*                                                                               
ADRCX    DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
*                                                                               
         GETEL R3,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
*                                                                               
FXELTAB  DC    AL1(PWDOLCDQ,PWCURCDQ,PWBAKDOL,0)                                
*                                                                               
AGYCOUNT DS    0XL(2+6+4+4+4)                                                   
*  BYTES 1-2   : ALPHA AGENCY CODE                                              
*  BYTES 3-8   : AGENCY ID                                                      
*  BYTES 9-12  : # OF PW MKT-LEVEL RECDS                                        
*  BYTES 13-16 : # OF PW MKTS W/ TAX                                            
*  BYTES 17-20 : # OF PW STATION-LEVEL RECDS                                    
SJCOUNT  DC    CL2'SJ',CL6'SJR   ',AL4(0),AL4(0),AL4(0)                         
TCCOUNT  DC    CL2'TC',CL6'TRC   ',AL4(0),AL4(0),AL4(0)                         
WICOUNT  DC    CL2'WI',CL6'WILA  ',AL4(0),AL4(0),AL4(0)                         
WJCOUNT  DC    CL2'WJ',CL6'WITEST',AL4(0),AL4(0),AL4(0)                         
OTCOUNT  DC    XL2'0000',CL6'OTHERS',AL4(0),AL4(0),AL4(0)                       
***********************************************************************         
                                                                                
                                                                                
         DROP  R8,R9,RA,RB,RC                                                   
         EJECT                                                                  
***********************************************************************         
*========================== RMP18's EQUATES ==========================*         
                                                                                
EOT      EQU   X'00'               END OF TABLE MARKER                          
                                                                                
PKYMKTL  EQU   PWKMKT-PWFKEY+L'PWKMKT                                           
                                                                                
*                                 ********* BIT MANIPULATIONS *********         
XFF      EQU   X'FF'                                                            
X80      EQU   X'80'                                                            
X40      EQU   X'40'                                                            
X20      EQU   X'20'                                                            
X10      EQU   X'10'                                                            
X08      EQU   X'08'                                                            
X04      EQU   X'04'                                                            
X02      EQU   X'02'                                                            
X01      EQU   X'01'                                                            
X00      EQU   X'00'                                                            
                                                                                
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= LOCAL WORKING STORAGE =======================*         
MYWORKD  DSECT                                                                  
                                                                                
*                                 ************* ADDRESSES *************         
ANEWREC  DS    A                                                                
                                                                                
*                                 ************** COUNTERS *************         
OLDSIZE  DS    F                   OLD FILE SIZE                                
NEWSIZE  DS    F                   NEW FILE SIZE                                
CNTDIRRD DS    F                   # OF DIRECTORY READS                         
CNTFILRD DS    F                   # OF FILE      READS                         
CNTRDEL  DS    F                   # OF RECORDS DELETED                         
CNTRADD  DS    F                   # OF RECORDS ADDED                           
                                                                                
*                                 *********** MISCELLANEOUS ***********         
RELO     DS    F                                                                
CURRSTA  DS    XL(L'BSTA)          STATION CURRENTLY BEING PROCESSED            
CBLSCMSK DS    XL(L'BSTA)          MASK FOR CABLE STATION SYSCODE               
ELCODE   DS    XL1                                                              
                                                                                
*                                 ************ TEMP VALUES ************         
TMPKEY   DS    XL(L'KEY)                                                        
                                                                                
*                                 ************** BUFFERS **************         
MYELEM   DS    XL256                                                            
NEWREC   DS    XL2000                                                           
                                                                                
*                                                                               
MYWORKX  EQU   *                                                                
MYWORKL  EQU   MYWORKX-MYWORKD                                                  
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================= PRINT LINE ============================*         
                                                                                
P1DSECT  DSECT                                                                  
P1CPTION DS    CL7                 C'DELETE:'                                   
         DS    CL2                                                              
P1KEY    DS    XL(2*L'PWFKEY)                                                   
         DS    CL1                                                              
P1CNTL   DS    XL(2*L'PWCNTL)                                                   
         DS    CL1                                                              
P1DSKADD DS    XL(2*4)                                                          
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
                                                                                
OWDOLEL  DSECT                     OLD WEEKLY (LOCKED) DOLLAR ELEMENT           
OWDOLCD  DS    X'06'               ELEMENT CODE (X'06')                         
OWDOLCDQ EQU   X'06'                                                            
OWDOLLEN DS    AL1(OWDOLLNQ)       ELEMENT LENGTH                               
OWDOLWK  DS    XL2                 WEEK DATE                                    
OWDOLSPT DS    XL4                 SPOTS                                        
OWDOLWG  DS    XL4                 WIM GROSS (IN PENNIES)                       
OWDOLWN  DS    XL4                 WIM NET    "                                 
OWDOLCG  DS    XL4                 CLT GROSS  "                                 
OWDOLCN  DS    XL4                 CLT NET    "                                 
OWDOLTAX DS    XL4                 TAX        "   (WIMTAX)                      
* FOLLOWING FIELD NON-ZERO IN FIRST WEEK OF EACH MONTH ONLY IN BOTH             
*  THE MKT-LEVEL AND STATION-LEVEL RECORDS.  SINCE THERE IS NO WAY TO           
*  DISTRIBUTE THE BILL OVRD AMOUNT ON A STATION-BY-STATION BASIS, EACH          
*  STATION WILL HAVE THE SAME BILL OVRD $'S AS THE MKT-LEVEL RECORD.            
OWDOLBIL DS    XL4                 BILL OVRD DOLLARS (X'80000000'=$0)           
OWDOLBLD DS    XL2                 ADJUSTMENT BILLING DATE                      
OWDOLLNQ EQU   *-OWDOLEL                                                        
                                                                                
                                                                                
OWCUREL  DSECT                     OLD WEEKLY CURRENT DOLLAR ELEMENT            
OWCURCD  DS    X'07'               ELEMENT CODE (X'07')                         
OWCURCDQ EQU   X'07'                                                            
OWCURLEN DS    AL1(OWCURLNQ)       ELEMENT LENGTH                               
OWCURWK  DS    XL2                 WEEK DATE                                    
OWCURSPT DS    XL4                 SPOTS                                        
OWCURWG  DS    XL4                 WIM GROSS (IN PENNIES)                       
OWCURWN  DS    XL4                 WIM NET    "                                 
OWCURCG  DS    XL4                 CLT GROSS  "                                 
OWCURCN  DS    XL4                 CLT NET    "                                 
OWCURTAX DS    XL4                 TAX        "   (WIMTAX)                      
* FOLLOWING FIELD NON-ZERO IN FIRST WEEK OF MONTH ONLY.                         
OWCURBIL DS    XL4                 BILL OVRD DOLLARS (X'80000000'=$0)           
OWCURBLD DS    XL2                 ADJUSTMENT BILLING DATE                      
OWCURLNQ EQU   *-OWCUREL                                                        
***********************************************************************         
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPFXPWB06/20/97'                                      
         END                                                                    
