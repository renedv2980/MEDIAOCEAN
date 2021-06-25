*          DATA SET SPREPFXPWC AT LEVEL 142 AS OF 12/23/97                      
*PHASE SPFX026C                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'SPFX02 - SEARCH FOR MKT RECS W/ NOT STA LVL RECS'               
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
         XC    MKTKEY,MKTKEY                                                    
         MVI   FLAG,0                                                           
*                                                                               
         DS    0H                                                               
         MVC   KEY(L'PWKTYP),=X'0D7A'     PW RECORDS ONLY                       
*                                                                               
FX010    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY               
         B     FX020                                                            
*                                                                               
FX015    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'SPTDIR',KEY,KEY               
         B     FX020                                                            
                                                                                
*                                                                               
FX020    DS    0H                                                               
         CLI   DMCB+8,0                                                         
         BE    FX020X                                                           
         TM    DMCB+8,X'02'                                                     
         BNZ   FX020X                                                           
         DC    H'0'                                                             
FX020X   EQU   *                                                                
*                                                                               
         LA    R0,1                                                             
         A     R0,CNTDIRRD                                                      
         ST    R0,CNTDIRRD                                                      
*                                                                               
         USING PWRECD,R6                                                        
         LA    R6,KEY                                                           
         CLC   KEY(3),=X'0D7A42'                                                
         BH    FX100                                                            
*        CLC   KEY(L'PWKTYP),=X'0D7A'                                           
*        BNE   FX100                                                            
*                                                                               
         MVC   SVKYCNTL,KEY+L'PWFKEY    SAVE CONTROL BYTE                       
*                                                                               
         OC    MKTKEY,MKTKEY            FIRST READ?                             
         BZ    FX070                     YES, PUT DATES IN TABLE                
         CLC   KEY(PWKSTA-PWFKEY),MKTKEY     SAME MKT?                          
         BE    FX030                    YES,CURREC MUST BE STA REC              
         OC    PWKSTA,PWKSTA            NO, MUST BE MKT REC                     
         BZ    *+6                      MKT REC, OK                             
         DC    H'0'                                                             
* CHECK SUMS IN TABLE AGAINST MKTREC (ROUTINE)                                  
         BAS   RE,CHKSUM                                                        
         B     FX070                    GET THE RECORD&ENTER DATES              
*                                                                               
FX030    DS    0H                                                               
         OC    PWKSTA,PWKSTA            SAMKE MKT, MUST BE STA REC              
         BNZ   *+6                      STA REC, OK                             
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
FX070    DS    0H                                                               
         XCEF  NEWREC,2000                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'SPTFIL',             +        
               KEY+14,ANEWREC,DMWORK                                            
         CLI   DMCB+8,0                                                         
         BE    FX070X                                                           
         TM    DMCB+8,X'02'                                                     
         BNZ   FX070X                                                           
         DC    H'0'                                                             
FX070X   EQU   *                                                                
                                                                                
         LA    R0,1                                                             
         A     R0,CNTFILRD                                                      
         ST    R0,CNTFILRD                                                      
*                                                                               
         DS    0H                                                               
         L     R6,ANEWREC                                                       
         USING PWRECD,R6                                                        
*                                                                               
         CLC   PWFKEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SVKYCNTL,PWCNTL                                                  
         TM    SVKYCNTL,X'80'           DELETE BITS IN SYNC?                    
         BZ    FX075                    YES, OK                                 
         LA    R0,1                                                             
         A     R0,DELPROB                                                       
         ST    R0,DELPROB                                                       
         MVI   FLAG,1                                                           
         BAS   RE,PRNTREC                                                       
         MVI   FLAG,0                   RESTORE FLAG                            
         B     FX015                                                            
*                                                                               
FX075    OC    PWKSTA,PWKSTA            MKT LVL?                                
         BNZ   FX080                    STA LVL, ACCUM VALUES PER WK            
         DROP  R6                                                               
*                                                                               
         MVC   MKTKEY,KEY               MKT LVL, SAVE MKT KEY                   
         BAS   RE,INPTDAT               INPUT DATES INTO TABLE                  
         B     FX085                                                            
*                                                                               
FX080    DS    0H                                                               
         BAS   RE,ACCUMVAL              ACUMM VALUES IN X'06' EL'S              
*                                                                               
FX085    B     FX015                    READ NEXT RECORD                        
*                                                                               
** NO MORE PW RECORDS **                                                        
         EJECT                                                                  
FX100    DS    0H                                                               
*                                                                               
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
         MVC   P1(29),=C'NUMBER OF RECORDS W/ MKT PROB'                         
         L     R1,MKTPROB                                                       
         EDIT  (R1),(10,P1+30),ZERO=NOBLANK                                     
                                                                                
         MVI   P2,0                                                             
         MVI   P3,0                                                             
         MVI   P4,0                                                             
                                                                                
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(29),=C'NUMBER OF RECORDS W/ DEL PROB'                         
         L     R1,DELPROB                                                       
         EDIT  (R1),(10,P1+30),ZERO=NOBLANK                                     
                                                                                
         MVI   P2,0                                                             
         MVI   P3,0                                                             
         MVI   P4,0                                                             
                                                                                
         GOTO1 REPORT                                                           
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
         XCEF  NEWREC,2000                                                      
         XCEF  MKTREC,2000                                                      
         XCEF  DOLWKTAB,600                                                     
*                                                                               
         DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====== MARKET RECORD HAS PROBLEM, PRINT OUT KEY, STATUS, AND D/A =====         
                                                                                
PRNTREC  NTR1                                                                   
         MVC   P1,SPACES                                                        
         USING MYPRNTD,R2                                                       
         LA    R2,P1                                                            
         GOTO1 HEXOUT,DMCB,MKTKEY,PRECAGYM,3,=C'TOG'                            
* SPACE IN BETWEEN                                                              
         MVC   SAVEKEY(13),KEY                                                  
         LA    R4,MKTKEY                                                        
         USING PWFKEY,R4                                                        
         GOTO1 CLUNPK,DMCB,PWKCLT,PCLT                                          
* SPACE IN BETWEEN                                                              
         ZIC   R0,PWKEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
* SPACE IN BETWEEN                                                              
         ZICM  R0,PWKMKT,2                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMKT,DUB                                                         
*                                                                               
* FIND PRODUCT CODE                                                             
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),PWKAGMD                                                 
         MVC   KEY+2(2),PWKCLT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETCLT                                                           
         USING CLTHDR,R6                                                        
         L     R6,ADCLT                                                         
         LA    R1,CLIST                                                         
PR10     CLC   3(1,R1),PWKPRD            PRD OF SAVED KEY?                      
         BE    PR20                                                             
         LA    R1,4(R1)                                                         
         LA    R0,CCLTIFC                                                       
         CR    R1,R0                    END OF CLIST?                           
         BL    PR10                                                             
         DC    H'0'                     PRD NOT FOUND, DIE                      
*                                                                               
PR20     MVC   MYSVPRD,0(R1)            SAVE PROD MNEMONIC                      
         MVC   PPRD,0(R1)              PRINT PROD MNEMONIC                      
* SPACE                                                                         
* FIND ESTIMATE DATE                                                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),PWKAGMD                                                 
         MVC   KEY+2(2),PWKCLT                                                  
         MVC   KEY+4(3),MYSVPRD                                                 
         MVC   KEY+7(1),PWKEST                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
*                                                                               
         USING ESTHDR,R6                                                        
         L     R6,ADEST                                                         
         MVC   PSTDATE,ESTART                                                   
         MVC   PENDATE,EEND                                                     
*                                                                               
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY               
*                                                                               
         CLI   FLAG,1                   IS IT A DELETE ERROR?                   
         BNE   *+10                                                             
         MVC   PRECAGYM(4),=C'*DEL'    DELETE BIT ERROR MESSAGE                 
*&&DO                                                                           
         GOTO1 HEXOUT,DMCB,MKTKEY,WORK,18,=C'TOG'                               
         LA    R2,P1                                                            
         LA    R3,WORK                                                          
                                                                                
         MVC   0(2*L'PWFKEY,R2),0(R3)   PW KEY                                  
         LA    R2,2*L'PWFKEY+1(R2)                                              
         LA    R3,2*L'PWFKEY(R3)                                                
         CLI   FLAG,1                   IS IT A DELETE ERROR?                   
         BNE   *+10                                                             
         MVC   0(4,R2),=C'*DEL'         DELETE BIT ERROR MESSAGE                
                                                                                
         MVC   0(2*1,R2),0(R3)          CONTROL BYTE                            
         LA    R2,2*1+1(R2)                                                     
         LA    R3,2*1(R3)                                                       
                                                                                
         MVC   0(2*4,R2),0(R3)          DISK ADDRESS                            
*                                                                               
         XC    SORTLEN,SORTLEN                                                  
         XC    SORTBUFF,SORTBUFF                                                
         MVC   SORTLEN(2),=H'104'                                               
         MVC   SORTBUFF(L'PSTDATE),PSTDATE                                      
         MVC   SORTBUFF+6(L'PENDATE),PENDATE                                    
         MVC   SORTBUFF+12(L'PRECAGYM),PRECAGYM                                 
         MVC   SORTBUFF+18(L'PCLT),PCLT                                         
         MVC   SORTBUFF+21(L'PPRD),PPRD                                         
         MVC   SORTBUFF+24(L'PEST),PEST                                         
         MVC   SORTBUFF+27(L'PMKT),PMKT                                         
*        MVC   SORTBUFF,P1                                                      
*&&                                                                             
*                                                                               
         GOTO1 REPORT                                                           
         DROP  R2,R6,R4                                                         
*                                                                               
         CLI   FLAG,1                   DELETE ERROR?                           
         BE    PRX                      YES, EXIT                               
*                                                                               
         LA    R0,1                                                             
         A     R0,MKTPROB                                                       
         ST    R0,MKTPROB          INCREMENT COUNT OF RECS W/ PROBLEM           
         DS    0H                                                               
PRX      B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====== INPUT DATES FROM MKT LVL RECORD INTO TABLE ====================         
                                                                                
INPTDAT  NTR1                                                                   
* CLEAR TABLE                                                                   
         XCEF  DOLWKTAB,600                                                     
         USING TABLED,R3                                                        
         L     R6,ANEWREC                                                       
         LA    R3,DOLWKTAB                                                      
*                                                                               
         USING PWDOLEL,R6                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         B     ID15                                                             
ID10     BAS   RE,NEXTEL                                                        
ID15     BNE   ID100                                                            
         MVC   DOLWKD,PWDOLWK                                                   
         LA    R3,L'DOLWKTAB(R3)        PNT TO NEXT TABLE ENTRY                 
         CLI   0(R3),X'FF'              END OF TABLE?                           
         BNE   ID10                     NO, CHECK NEXT ELEM                     
*                                                                               
ID100    MVI   0(R3),X'FF'              END TABLE/END OF ELEM'S                 
         XCEF  MKTREC,2000                                                      
         LA    R0,MKTREC                                                        
         LA    R1,L'MKTREC                                                      
         L     RE,ANEWREC                                                       
         LA    RF,L'NEWREC                                                      
         MVCL  R0,RE                                                            
         B     EXIT                                                             
         DROP  R3,R6                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====== ACCUMULATE DOLLAR VALUES FROM STATION LVL RECORDS =============         
ACCUMVAL NTR1                                                                   
         CLI   DOLWKTAB,X'DD'           ERROR FLAG?                             
         BE    ACCX                     YES, DON'T SUM THIS STA REC             
*                                                                               
ACC30    DS    0H                                                               
         USING TABLED,R3                                                        
         LA    R3,DOLWKTAB                                                      
         L     R6,ANEWREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         B     ACC45                                                            
*                                                                               
ACC40    LA    R3,DOLWKTAB                                                      
         BAS   RE,NEXTEL                                                        
ACC45    BNE   ACCX                                                             
ACC50    CLI   0(R3),X'FF'              END OF TABLE?                           
         BNE   ACC60                                                            
*                                                                               
         USING PWDOLEL,R6                                                       
         OC    PWDOLSPT(36),PWDOLSPT    SHOULD BE NULLS                         
         BZ    ACC40                    NULLS, OK                               
         MVI   DOLWKTAB,X'DD'           ERROR CODE                              
         B     ACCX                                                             
*                                                                               
ACC60    CLC   PWDOLWK,DOLWKD                                                   
         BNE   ACC90                                                            
*                                                                               
         ZICM  RE,PWDOLSPT,4                                                    
         ZICM  R1,DOLSPTD,4                                                     
         AR    R1,RE                                                            
         STCM  R1,15,DOLSPTD                                                    
*                                                                               
         ZICM  RE,PWDOLWG,4                                                     
         ZICM  R1,DOLWGD,4                                                      
         AR    R1,RE                                                            
         STCM  R1,15,DOLWGD                                                     
*                                                                               
         ZICM  RE,PWDOLWN,4                                                     
         ZICM  R1,DOLWND,4                                                      
         AR    R1,RE                                                            
         STCM  R1,15,DOLWND                                                     
*                                                                               
         ZICM  RE,PWDOLCG,4                                                     
         ZICM  R1,DOLCGD,4                                                      
         AR    R1,RE                                                            
         STCM  R1,15,DOLCGD                                                     
*                                                                               
         ZICM  RE,PWDOLCN,4                                                     
         ZICM  R1,DOLCND,4                                                      
         AR    R1,RE                                                            
         STCM  R1,15,DOLCND                                                     
*                                                                               
         ZICM  RE,PWDOLTAX,4                                                    
         ZICM  R1,DOLTAXD,4                                                     
         AR    R1,RE                                                            
         STCM  R1,15,DOLTAXD                                                    
*                                                                               
         ZICM  RE,PWDOLCTX,4                                                    
         ZICM  R1,DOLCTXD,4                                                     
         AR    R1,RE                                                            
         STCM  R1,15,DOLCTXD                                                    
         DROP  R3,R6                                                            
*                                                                               
         B     ACC40                    GET NEXT ELEM                           
*                                                                               
ACC90    LA    R3,L'DOLWKTAB(R3)                                                
         B     ACC50                    CHECK NEXT TABLE ENTRY                  
*                                                                               
ACCX     B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====== COMPARE DOL VALUES IN MKT REC'S WITH SUMS IN TABLE ============         
CHKSUM   NTR1                                                                   
         CLI   DOLWKTAB,X'DD'           BAD RECORD?                             
         BE    CKS55                                                            
         CLI   DOLWKTAB,X'FF'                                                   
         BE    CKSX                                                             
*                                                                               
         USING TABLED,R3                                                        
         LA    R3,DOLWKTAB                                                      
         LA    R6,MKTREC                                                        
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         B     CKS45                                                            
*                                                                               
CKS40    LA    R3,DOLWKTAB                                                      
         BAS   RE,NEXTEL                                                        
CKS45    BNE   CKSX                                                             
CKS50    CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                     DATE IN STA NOT IN MKT, DIE             
*                                                                               
         USING PWDOLEL,R6                                                       
         CLC   PWDOLWK,DOLWKD                                                   
         BNE   CKS70                                                            
         CLC   PWDOLSPT,DOLSPTD                                                 
         BNE   CKS55                                                            
         CLC   PWDOLWG,DOLWGD                                                   
         BNE   CKS55                                                            
         CLC   PWDOLWN,DOLWND                                                   
         BNE   CKS55                                                            
         CLC   PWDOLCG,DOLCGD                                                   
         BNE   CKS55                                                            
         CLC   PWDOLCN,DOLCND                                                   
         BNE   CKS55                                                            
         CLC   PWDOLTAX,DOLTAXD                                                 
         BNE   CKS55                                                            
         CLC   PWDOLCTX,DOLCTXD                                                 
         BNE   CKS55                                                            
         DROP  R6,R3                                                            
*                                                                               
         B     CKS40                    CHECK NEXT ELEM                         
*                                                                               
CKS55    BAS   RE,PRNTREC               PRINT BAD RECORD                        
         B     CKSX                                                             
*                                                                               
CKS70    LA    R3,L'DOLWKTAB(R3)                                                
         B     CKS50                                                            
*                                                                               
CKSX     B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         EJECT                                                                  
***********************************************************************         
*======================= LOCAL WORKING STORAGE =======================*         
MYWORKD  DSECT                                                                  
                                                                                
*                                 ************* ADDRESSES *************         
ANEWREC  DS    A                                                                
*                                                                               
*                                 **** SORTER'S CARDS ****                      
*                                                                               
*                                 ************** COUNTERS *************         
CNTDIRRD DS    F                   # OF DIRECTORY READS                         
CNTFILRD DS    F                   # OF FILE      READS                         
MKTPROB  DS    F                   # OF MKT RECS WITH UNEQ 06 PROB              
DELPROB  DS    F                   # OF RECS W/ DELETE BIT PROB                 
*                                 *********** MISCELLANEOUS ***********         
RELO     DS    F                                                                
CBLSCMSK DS    XL(L'BSTA)          MASK FOR CABLE STATION SYSCODE               
ELCODE   DS    XL1                                                              
FLAG     DS    XL1                      USE FOR ERROR CODES                     
MKTERR   EQU   X'00'                                                            
DELERR   EQU   X'01'                                                            
SVKYCNTL DS    XL1                      SAVE CONTROL BYTE OF DIR PTR            
MYSVPRD  DS    CL3                      SAVED PRODUCT                           
SVESTART DS    CL6                      SAVED EST DATES                         
SVEEND   DS    CL6                                                              
SAVEKEY  DS    XL13                                                             
MKTKEY   DS    XL18                                                             
*                                                                               
*ORTLEN  DS    XL4                                                              
*ORTBUFF DS    CL100                                                            
*                                       DOLWK TABLE FOR X'06' ELEM'S            
DOLWKTAB DS    20XL30                   ENOUGH FOR 20 WK'S                      
TABEND   DC    X'FF'                    END OF TABLE                            
TABLNEQ  EQU   TABEND-DOLWKTAB                                                  
*                                                                               
NEWREC   DS    XL2000                                                           
MKTREC   DS    XL2000                                                           
*                                                                               
MYWORKX  EQU   *                                                                
MYWORKL  EQU   MYWORKX-MYWORKD                                                  
*                                                                               
* TABLE DSECT                                                                   
TABLED   DSECT                                                                  
DOLWKD   DS    XL2                                                              
DOLSPTD  DS    XL4                                                              
DOLWGD   DS    XL4                                                              
DOLWND   DS    XL4                                                              
DOLCGD   DS    XL4                                                              
DOLCND   DS    XL4                                                              
DOLTAXD  DS    XL4                                                              
DOLCTXD  DS    XL4                                                              
*                                                                               
MYPRNTD  DSECT                                                                  
PSTDATE  DS    CL6                                                              
         DS    CL1                                                              
PENDATE  DS    CL6                                                              
         DS    CL1                                                              
PRECAGYM DS    CL6                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL1                                                              
PRLNEQ   EQU   *-PSTDATE                                                        
*                                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                               
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'142SPREPFXPWC12/23/97'                                      
         END                                                                    
